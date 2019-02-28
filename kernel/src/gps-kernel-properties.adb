------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2019, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Ada.Strings.Fixed;

with GNAT.OS_Lib;                use GNAT.OS_Lib;

with GNATCOLL.JSON;
with GNATCOLL.Traces;            use GNATCOLL.Traces;
with GNATCOLL.Projects;          use GNATCOLL.Projects;
with GNATCOLL.Scripts;           use GNATCOLL.Scripts;
with GNATCOLL.VFS;               use GNATCOLL.VFS;
with GNATCOLL.SQL;               use GNATCOLL.SQL;
with GNATCOLL.SQL.Exec;          use GNATCOLL.SQL.Exec;
with GNATCOLL.SQL.Sqlite;

with GPS.Intl;                   use GPS.Intl;
with GPS.Kernel.Hooks;           use GPS.Kernel.Hooks;
with GPS.Kernel.Scripts;         use GPS.Kernel.Scripts;
with Default_Preferences;        use Default_Preferences;

with GPS.Kernel.Properties.Database;

package body GPS.Kernel.Properties is

   use Properties_Indefinite_Hashed_Maps;

   Me   : constant Trace_Handle := Create ("GPS.KERNEL.PROPERTIES");
   Dump : constant Trace_Handle := Create
     ("TESTSUITE.DUMP_PROPERTIES", Off);

   Store_Properties_On_The_Fly : Boolean_Preference;

   function Get_Properties_Filename
     (Kernel : access Kernel_Handle_Record'Class;
      Dump   : Boolean := False)
      return Virtual_File;
   --  Return the filename to use when saving the persistent properties for the
   --  current project

   procedure Set_Resource_Property
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      Key      : String;
      Name     : String;
      Property : Property_Description);
   --  Set property for any kind of resource. This is the internal
   --  implementation of Set_*_Property.

   procedure Remove_Resource_Property
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Key    : String;
      Name   : String);
   --  Remove property for any kind of resource

   procedure File_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handles script commands for properties

   procedure Project_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handles script commands for properties

   procedure Properties_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handles script commands for properties

   -- Database routines --

   type Dummy_Writer_Record is new Writer_Record with null record;

   overriding procedure Get_Value
     (Self     : not null access Dummy_Writer_Record;
      Key      : String;
      Name     : String;
      Property : out Property_Record'Class;
      Found    : out Boolean);

   overriding procedure Get_Values
     (Self     : not null access Dummy_Writer_Record;
      Name     : String;
      Property : in out Property_Record'Class;
      Callback : access procedure
        (Key : String; Property : in out Property_Record'Class)) is null;

   overriding procedure Insert
     (Self     : not null access Dummy_Writer_Record;
      Key      : String;
      Name     : String;
      Property : Property_Description) is null;

   overriding procedure Update
     (Self     : not null access Dummy_Writer_Record;
      Key      : String;
      Name     : String;
      Property : Property_Description) is null;

   overriding procedure Remove
     (Self : not null access Dummy_Writer_Record;
      Key  : String;
      Name : String) is null;

   overriding procedure Dump_Database
     (Self : not null access Dummy_Writer_Record) is null;

   procedure Store_Properties;
   --  Stores properties in DB

   procedure Split
     (Value : String;
      Key   : out Unbounded_String;
      Name  : out Unbounded_String);
   --  Splits Value into Key and Name

   type On_Pref_Changed is new Preferences_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference);

   -------------------
   -- SQLite_Writer --
   -------------------

   package SQLite_Writer is

      type SQLite_Writer_Record (Kernel : access Kernel_Handle_Record'Class) is
        new Writer_Record with private;
      type SQLite_Writer is access all SQLite_Writer_Record'Class;

      overriding procedure Get_Value
        (Self     : not null access SQLite_Writer_Record;
         Key      : String;
         Name     : String;
         Property : out Property_Record'Class;
         Found    : out Boolean);

      overriding procedure Get_Values
        (Self     : not null access SQLite_Writer_Record;
         Name     : String;
         Property : in out Property_Record'Class;
         Callback : access procedure
           (Key : String; Property : in out Property_Record'Class));

      overriding procedure Insert
        (Self     : not null access SQLite_Writer_Record;
         Key      : String;
         Name     : String;
         Property : Property_Description);

      overriding procedure Update
        (Self     : not null access SQLite_Writer_Record;
         Key      : String;
         Name     : String;
         Property : Property_Description);

      overriding procedure Remove
        (Self : not null access SQLite_Writer_Record;
         Key  : String;
         Name : String);

      overriding procedure Dump_Database
        (Self   : not null access SQLite_Writer_Record);

      function Constructor
        (Kernel : access Kernel_Handle_Record'Class)
         return Writer;

   private
      type SQLite_Writer_Record (Kernel : access Kernel_Handle_Record'Class) is
        new Writer_Record with record
         DB : GNATCOLL.SQL.Exec.Database_Description;
         --  The description of the database we are currently connected to.
         --  This must not be freed while where exists connections to this
         --  database, since the connections have a pointer to this descr.

         Connection : GNATCOLL.SQL.Exec.Database_Connection;
         --  A thread-specific access to a database
      end record;

      overriding procedure Finalize   (Self : in out SQLite_Writer_Record);

      procedure Init
        (Self : not null access SQLite_Writer_Record;
         File : Virtual_File);

      procedure Create_Database
        (Self : not null access SQLite_Writer_Record);
      --  Create the database tables and initial contents.

      procedure Create_If_Needed
        (Self        : not null access SQLite_Writer_Record;
         Key         : String;
         Name        : String;
         Resource_Id : out Integer;
         Property_Id : out Integer);
      --  Insert new value of Property and Resource if not exsist in DB
      --  and return Primary keys

      procedure Delete_If_No_Used
        (Self        : not null access SQLite_Writer_Record;
         Resource_Id : Integer;
         Property_Id : Integer);
      --  Delete unused Property and Resource

      function Get_Id
        (Self  : not null access SQLite_Writer_Record;
         Query : Prepared_Statement;
         Name  : String)
         return Integer;

      procedure Get_Ids
        (Self        : not null access SQLite_Writer_Record;
         Key         : String;
         Name        : String;
         Resource_Id : out Integer;
         Property_Id : out Integer);
      --  Retrive Ids of Property and Resource

      procedure Insert_Or_Update
        (Self     : not null access SQLite_Writer_Record;
         Key      : String;
         Name     : String;
         Property : Property_Description);
      --  Insert or Update Value. Behavior depends on Query.

   end SQLite_Writer;

   procedure Free is
     new Ada.Unchecked_Deallocation (Writer_Record'Class, Writer);

   Current_Writer : Writer := new Dummy_Writer_Record;

   -----------------------------------
   -- Open_Persistent_Properties_DB --
   -----------------------------------

   function Open_Persistent_Properties_DB
     (Kernel : access Kernel_Handle_Record'Class)
     return GPS.Properties.Writer
   is
      Object : constant Writer := SQLite_Writer.Constructor (Kernel);
   begin
      Store_Properties_On_The_Fly :=
        Kernel.Get_Preferences.Create_Invisible_Pref
          (Name    => "Store_Properties_On_The_Fly",
           Label   => "Store properties on the fly",
           Doc     => -"Whether to store the properties database on disk " &
             " after each change.",
           Default => False);

      Preferences_Changed_Hook.Add (new On_Pref_Changed);

      if Object /= null then
         Free (Current_Writer);
         Current_Writer := Object;
      end if;

      if Current_Writer = null then
         Current_Writer := new Dummy_Writer_Record;
      end if;

      return GPS.Properties.Writer (Current_Writer);
   end Open_Persistent_Properties_DB;

   ----------------------
   -- Store_Properties --
   ----------------------

   procedure Store_Properties
   is
      C         : Cursor;
      Key, Name : Unbounded_String;

   begin
      C := First (All_Properties);
      while Has_Element (C) loop
         if Element (C).Persistent
           and then Element (C).Modified
         then
            Split (Properties_Indefinite_Hashed_Maps.Key (C), Key, Name);
            if Key /= Null_Unbounded_String
              and then Name /= Null_Unbounded_String
            then
               if Element (C).Value /= null then
                  Current_Writer.Update
                    (To_String (Key), To_String (Name), Element (C).all);
               else
                  Current_Writer.Remove (To_String (Key), To_String (Name));
               end if;
            end if;

            Element (C).Modified := False;
         end if;

         Next (C);
      end loop;
   end Store_Properties;

   ------------------------------------
   -- Close_Persistent_Properties_DB --
   ------------------------------------

   procedure Close_Persistent_Properties_DB
     (Kernel : access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Kernel);

   begin
      if not Store_Properties_On_The_Fly.Get_Pref then
         Store_Properties;
      end if;

      Free (Current_Writer);
   end Close_Persistent_Properties_DB;

   ---------------------------
   -- Set_Resource_Property --
   ---------------------------

   procedure Set_Resource_Property
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      Key      : String;
      Name     : String;
      Property : Property_Description)
   is
      pragma Unreferenced (Kernel);

      Descr     : Property_Description_Access;
      Prop      : Property_Description_Access;
      New_Value : Boolean := True;
      C         : Cursor;
   begin
      Prop := new Property_Description'(Property);
      Prop.Modified := True;

      C := Find (All_Properties, Key & Sep & Name);
      if Has_Element (C) then
         Descr     := Element (C);
         New_Value := Descr.Value = null;
         Replace_Element (All_Properties, C, Prop);
         Free (Descr);

      else
         Insert (All_Properties, Key & Sep & Name, Prop);
      end if;

      if Property.Persistent
        and then Store_Properties_On_The_Fly.Get_Pref
      then
         if Property.Value /= null then
            if New_Value then
               Current_Writer.Insert (Key, Name, Property);
            else
               Current_Writer.Update (Key, Name, Property);
            end if;

         elsif not New_Value then
            --  The property had a value but it is deleted now,
            --  so property is deleted also
            Current_Writer.Remove (Key, Name);
         end if;
      end if;
   end Set_Resource_Property;

   ------------------------------
   -- Remove_Resource_Property --
   ------------------------------

   procedure Remove_Resource_Property
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Key    : String;
      Name   : String)
   is
      pragma Unreferenced (Kernel);

      Descr : Property_Description_Access;
      C     : Cursor := Find (All_Properties, Key & Sep & Name);

   begin
      if not Has_Element (C) then
         return;
      end if;

      Descr := Element (C);
      if Descr.Value /= null
        and then Descr.Persistent
      then
         if Store_Properties_On_The_Fly.Get_Pref then
            Current_Writer.Remove (Key, Name);
            Delete (All_Properties, C);
            Free (Descr);

         else
            Clear (Descr);
            Descr.Modified := True;
         end if;
      end if;
   end Remove_Resource_Property;

   ------------------
   -- Set_Property --
   ------------------

   procedure Set_Property
     (Kernel     : access GPS.Kernel.Kernel_Handle_Record'Class;
      Key        : String;
      Name       : String;
      Property   : access Property_Record'Class;
      Persistent : Boolean := False) is
   begin
      Set_Resource_Property
        (Kernel,
         Key, Name,
         Property_Description'(Value      => Property_Access (Property),
                               Persistent => Persistent,
                               Modified   => True));
   end Set_Property;

   ---------------------
   -- Remove_Property --
   ---------------------

   procedure Remove_Property
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Key    : String;
      Name   : String) is
   begin
      Remove_Resource_Property (Kernel, Key, Name);
   end Remove_Property;

   ------------------
   -- Set_Property --
   ------------------

   procedure Set_Property
     (Kernel      : access GPS.Kernel.Kernel_Handle_Record'Class;
      File       : GNATCOLL.VFS.Virtual_File;
      Name       : String;
      Property   : access Property_Record'Class;
      Persistent : Boolean := False) is
   begin
      Set_Property
        (Kernel, To_String (File), Name, Property, Persistent);
   end Set_Property;

   ------------------
   -- Set_Property --
   ------------------

   procedure Set_Property
     (Kernel     : access GPS.Kernel.Kernel_Handle_Record'Class;
      Project    : Project_Type;
      Name       : String;
      Property   : access Property_Record'Class;
      Persistent : Boolean := False) is
   begin
      Set_Property
        (Kernel, To_String (Project), Name, Property, Persistent);
   end Set_Property;

   ---------------------
   -- Remove_Property --
   ---------------------

   procedure Remove_Property
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File;
      Name   : String) is
   begin
      Remove_Property (Kernel, To_String (File), Name);
   end Remove_Property;

   ---------------------
   -- Remove_Property --
   ---------------------

   procedure Remove_Property
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      Project : Project_Type;
      Name    : String) is
   begin
      Remove_Property (Kernel, To_String (Project), Name);
   end Remove_Property;

   -----------------------------
   -- Get_Properties_Filename --
   -----------------------------

   function Get_Properties_Filename
     (Kernel : access Kernel_Handle_Record'Class;
      Dump   : Boolean := False)
      return Virtual_File is
   begin
      --  We are using the .gps directory, but that would mean we have to keep
      --  in database information for files that do not belong to the current
      --  project.

      return Create_From_Dir
        (Get_Home_Dir (Kernel), "properties" &
           (if Dump then ".dump" else ".db"));
   end Get_Properties_Filename;

   ----------------------
   -- Reset_Properties --
   ----------------------

   procedure Reset_Properties
     (Kernel : access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Kernel);

      C         : Cursor := First (All_Properties);
      Descr     : Property_Description_Access;
      Key, Name : Unbounded_String;
   begin
      while Has_Element (C) loop
         Descr := Element (C);
         if Store_Properties_On_The_Fly.Get_Pref then
            Split (Properties_Indefinite_Hashed_Maps.Key (C), Key, Name);
            Current_Writer.Remove (To_String (Key), To_String (Name));
            Free (Descr);
         else
            Clear (Descr);
            Descr.Modified := True;
         end if;

         Next (C);
      end loop;

      if Store_Properties_On_The_Fly.Get_Pref then
         Clear (All_Properties);
      end if;
   end Reset_Properties;

   ----------------------------
   -- Set_Language_From_File --
   ----------------------------

   procedure Set_Language_From_File
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      Filename : GNATCOLL.VFS.Virtual_File;
      Language : String := "")
   is
      Prop : String_Property_Access;
   begin
      if Filename = No_File then
         --  This shouldn't happen. But add the test for safety, to make sure
         --  we are not persisting a language for the empty file.
         return;
      end if;

      if Language = "" then
         Remove_Property (Kernel, Filename, "language");
      else
         Prop := new String_Property'(Value => new String'(Language));
         Set_Property (Kernel, Filename, "language", Prop, Persistent => True);
      end if;
   end Set_Language_From_File;

   --------------------------
   -- File_Command_Handler --
   --------------------------

   procedure File_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel     : constant Kernel_Handle := Get_Kernel (Data);
      File_Class : constant Class_Type := New_Class (Kernel, "File");
      Inst       : constant Class_Instance := Nth_Arg (Data, 1, File_Class);
      File       : constant Virtual_File := Get_Data (Inst);

      Name       : aliased constant String := "name";
      Value      : aliased constant String := "value";
      Persistent : aliased constant String := "persistent";

      Found      : Boolean;
      Prop       : String_Property_Access;
      Prop2      : aliased String_Property;
   begin
      if Command = "set_property" then
         Name_Parameters (Data, (2 => Name'Unchecked_Access,
                                 3 => Value'Unchecked_Access,
                                 4 => Persistent'Unchecked_Access));
         Prop := new String_Property'(Value => new String'(Nth_Arg (Data, 3)));
         Set_Property
           (Kernel,
            File,
            Name       => Nth_Arg (Data, 2),
            Property   => Prop,
            Persistent => Nth_Arg (Data, 4, False));

      elsif Command = "get_property" then
         Name_Parameters (Data, (2 => Name'Unchecked_Access));
         Get_Property
           (Property => Prop2,
            File     => File,
            Name     => Nth_Arg (Data, 2),
            Found    => Found);

         if not Found then
            Set_Error_Msg (Data, -"Property not found");

         elsif Prop2.Value = null then
            Set_Return_Value (Data, String'(""));

         else
            Set_Return_Value (Data, Prop2.Value.all);
         end if;

      else
         Name_Parameters (Data, (2 => Name'Unchecked_Access));
         Remove_Property
           (Kernel => Kernel,
            File   => File,
            Name   => Nth_Arg (Data, 2));
      end if;
   end File_Command_Handler;

   -----------------------------
   -- Project_Command_Handler --
   -----------------------------

   procedure Project_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Project    : constant Project_Type  := Get_Data (Data, 1);
      Name       : aliased constant String := "name";
      Value      : aliased constant String := "value";
      Persistent : aliased constant String := "persistent";

      Found      : Boolean;
      Prop       : String_Property_Access;
      Prop2      : aliased String_Property;
   begin
      if Command = "set_property" then
         Name_Parameters (Data, (2 => Name'Unchecked_Access,
                                 3 => Value'Unchecked_Access,
                                 4 => Persistent'Unchecked_Access));
         Prop := new String_Property'(Value => new String'(Nth_Arg (Data, 3)));
         Set_Property
           (Get_Kernel (Data),
            Project,
            Name       => Nth_Arg (Data, 2),
            Property   => Prop,
            Persistent => Nth_Arg (Data, 4, False));

      elsif Command = "get_property" then
         Name_Parameters (Data, (2 => Name'Unchecked_Access));
         Get_Property
           (Property => Prop2,
            Project     => Project,
            Name     => Nth_Arg (Data, 2),
            Found    => Found);

         if not Found then
            Set_Error_Msg (Data, -"Property not found");

         elsif Prop2.Value = null then
            Set_Return_Value (Data, String'(""));

         else
            Set_Return_Value (Data, Prop2.Value.all);
         end if;

      else
         Name_Parameters (Data, (2 => Name'Unchecked_Access));
         Remove_Property
           (Kernel   => Get_Kernel (Data),
            Project  => Project,
            Name     => Nth_Arg (Data, 2));
      end if;
   end Project_Command_Handler;

   --------------------------------
   -- Properties_Command_Handler --
   --------------------------------

   procedure Properties_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      pragma Unreferenced (Data);
   begin
      --  This procedure was left for testing purposes. It is no nessesary
      --  to use it in other cases because properties are stored on fly.

      if Command = "save_persistent_properties" then
         if Active (Dump) then
            if not Store_Properties_On_The_Fly.Get_Pref then
               Store_Properties;
            end if;

            Current_Writer.Dump_Database;
         end if;
      end if;
   end Properties_Command_Handler;

   ------------------------------
   -- Register_Script_Commands --
   ------------------------------

   procedure Register_Script_Commands
     (Kernel : access Kernel_Handle_Record'Class)
   is
      File_Class    : constant Class_Type := New_Class (Kernel, "File");
      Project_Class : constant Class_Type := New_Class (Kernel, "Project");
   begin
      Register_Command
        (Kernel, "set_property",
         Minimum_Args => 2,
         Maximum_Args => 3,
         Class        => File_Class,
         Handler      => File_Command_Handler'Access);
      Register_Command
        (Kernel, "get_property",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Class        => File_Class,
         Handler      => File_Command_Handler'Access);
      Register_Command
        (Kernel, "remove_property",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Class        => File_Class,
         Handler      => File_Command_Handler'Access);

      Register_Command
        (Kernel, "set_property",
         Minimum_Args => 2,
         Maximum_Args => 3,
         Class        => Project_Class,
         Handler      => Project_Command_Handler'Access);
      Register_Command
        (Kernel, "get_property",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Class        => Project_Class,
         Handler      => Project_Command_Handler'Access);
      Register_Command
        (Kernel, "remove_property",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Class        => Project_Class,
         Handler      => Project_Command_Handler'Access);

      Register_Command
        (Kernel, "save_persistent_properties",
         Handler => Properties_Command_Handler'Access);
   end Register_Script_Commands;

   ---------------
   -- Get_Value --
   ---------------

   overriding procedure Get_Value
     (Self     : not null access Dummy_Writer_Record;
      Key      : String;
      Name     : String;
      Property : out Property_Record'Class;
      Found    : out Boolean)
   is
      pragma Unreferenced (Self, Key, Name, Property);
   begin
      Found := False;
   end Get_Value;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference)
   is
      pragma Unreferenced (Self, Kernel);
   begin
      if Pref = Preference (Store_Properties_On_The_Fly)
        and then Store_Properties_On_The_Fly.Get_Pref
      then
         Store_Properties;
      end if;
   end Execute;

   -----------
   -- Split --
   -----------

   procedure Split
     (Value : String;
      Key   : out Unbounded_String;
      Name  : out Unbounded_String)
   is
      Idx : Integer;
   begin
      Key  := Null_Unbounded_String;
      Name := Null_Unbounded_String;
      Idx  := Ada.Strings.Fixed.Index (Value, Sep);

      if Idx in Value'Range then
         Key  := To_Unbounded_String (Value (Value'First .. Idx - 1));
         Name := To_Unbounded_String (Value (Idx + Sep'Length .. Value'Last));
      end if;
   end Split;

   -------------------
   -- SQLite_Writer --
   -------------------

   package body SQLite_Writer is

      use GPS.Kernel.Properties.Database;

      Schema_Version : constant Integer := 1;
      --  The current version of the database schema.

      -- Queries --

      Query_Get_Resource_Id : constant Prepared_Statement :=
        Prepare
          (SQL_Select
             (Fields => Resources.Id,
              From   => Resources,
              Where  => Resources.Name = Text_Param (1),
              Limit  => 1),
           On_Server => True,
           Use_Cache => True,
           Name      => "get_resource_id");
      --  Retrieve the id of resource

      Query_Insert_Resource : constant Prepared_Statement :=
        Prepare
          (SQL_Insert
             ((Resources.Name = Text_Param (1))),
           On_Server => True, Name => "insert_resource");
      --  Insert new resource

      Query_Delete_Resource : constant Prepared_Statement :=
        Prepare
          (SQL_Delete
             (From  => Resources,
              Where => Resources.Id = Integer_Param (1)),
           On_Server => True, Name => "delete_resource");
      --  Delete resource

      Query_Get_Property_Id : constant Prepared_Statement :=
        Prepare
          (SQL_Select
             (Fields => Database.Properties.Id,
              From   => Database.Properties,
              Where  => Database.Properties.Name = Text_Param (1),
              Limit  => 1),
           On_Server => True,
           Use_Cache => True,
           Name      => "get_property_id");
      --  Retrieve the id of resource

      Query_Insert_Property : constant Prepared_Statement :=
        Prepare
          (SQL_Insert
             ((Database.Properties.Name = Text_Param (1))),
           On_Server => True, Name => "insert_property");
      --  Insert new property

      Query_Delete_Property : constant Prepared_Statement :=
        Prepare
          (SQL_Delete
             (From  => Database.Properties,
              Where => Database.Properties.Id = Integer_Param (1)),
           On_Server => True, Name => "delete_property");
      --  Delete property

      Query_Is_Resource_In_Use : constant Prepared_Statement :=
        Prepare
          (SQL_Select
             (Fields => Items.Resource,
              From   => Items,
              Where  => Items.Resource = Integer_Param (1),
              Limit  => 1),
           On_Server => True, Name => "is_resource_in_use");
      --  Retrive record if some value use resource

      Query_Is_Property_In_Use : constant Prepared_Statement :=
        Prepare
          (SQL_Select
             (Fields => Items.Property,
              From   => Items,
              Where  => Items.Property = Integer_Param (1),
              Limit  => 1),
           On_Server => True, Name => "is_property_in_use");
      --  Retrive record if some value use property

      Query_Insert_Value : constant Prepared_Statement :=
        Prepare
          (SQL_Insert
             ((Items.Item = Text_Param (1))
              & (Items.Resource = Integer_Param (2))
              & (Items.Property = Integer_Param (3))),
           On_Server => True, Name => "insert_item");
      --  Insert new value into database

      Query_Get_Value : constant Prepared_Statement :=
        Prepare
          (SQL_Select
             (Items.Item,
              From => Items,
              Where => Items.Resource = Integer_Param (1)
              and Items.Property = Integer_Param (2)),
           On_Server => True,
           Use_Cache => True,
           Name      => "get_item");
      --  Retrive value

      Query_Get_Values : constant Prepared_Statement :=
        Prepare
          (SQL_Select
             ((Items.Item & Resources.Name),
              From => Items & Resources,
              Where => Items.Property = Integer_Param (1)
              and Items.Resource = Resources.Id),
           On_Server => True,
           Use_Cache => True,
           Name      => "get_items");
      --  Retrive values

      Query_Update_Value : constant Prepared_Statement :=
        Prepare
          (SQL_Update
             (Set   => Items.Item = Text_Param (1),
              Table => Items,
              Where => Items.Resource = Integer_Param (2)
              and Items.Property = Integer_Param (3)),
           On_Server => True, Name => "update_item");

      Query_Delete_Value : constant Prepared_Statement :=
        Prepare
          (SQL_Delete
             (From  => Items,
              Where => Items.Resource = Integer_Param (1)
              and Items.Property = Integer_Param (2)),
           On_Server => True, Name => "delete_item");
      --  Delete value

      -----------------
      -- Constructor --
      -----------------

      function Constructor
        (Kernel : access Kernel_Handle_Record'Class)
         return Writer
      is
         File : constant Virtual_File := Get_Properties_Filename (Kernel);
      begin
         Trace (Me, "Open " & File.Display_Full_Name);

         if not Create (File.Dir_Name).Is_Writable
           or else
             (File.Is_Regular_File and then not File.Is_Writable)
         then
            Trace (Me, "Properties file is not writable " &
                     File.Display_Full_Name);
            return null;
         end if;

         return W : constant Writer := new SQLite_Writer_Record (Kernel) do
            SQLite_Writer (W).Init (File);
         end return;

      exception
         when E : others => Trace (Me, E);
            return null;
      end Constructor;

      ---------------------
      -- Create_Database --
      ---------------------

      procedure Create_Database
        (Self : not null access SQLite_Writer_Record)
      is
         Transaction : Transaction_Controller (Self.Connection);
      begin
         Create_Database (Self.Connection);
         Self.Connection.Execute
           ("PRAGMA user_version=" & Schema_Version'Img & ";");
      end Create_Database;

      ----------------------
      -- Create_If_Needed --
      ----------------------

      procedure Create_If_Needed
        (Self        : not null access SQLite_Writer_Record;
         Key         : String;
         Name        : String;
         Resource_Id : out Integer;
         Property_Id : out Integer)
      is
         procedure Create
           (Query : Prepared_Statement;
            PK    : SQL_Field_Integer;
            Name  : String;
            Id    : in out Integer);

         ------------
         -- Create --
         ------------

         procedure Create
           (Query : Prepared_Statement;
            PK    : SQL_Field_Integer;
            Name  : String;
            Id    : in out Integer) is
         begin
            if Id < 0 then
               Id := Self.Connection.Insert_And_Get_PK
                 (Query, Params => (1 => +Name), PK => PK);
            end if;
         end Create;

      begin
         Self.Get_Ids (Key, Name, Resource_Id, Property_Id);

         Create (Query_Insert_Resource, Resources.Id, Key, Resource_Id);
         Create
           (Query_Insert_Property, Database.Properties.Id, Name, Property_Id);
      end Create_If_Needed;

      -----------------------
      -- Delete_If_No_Used --
      -----------------------

      procedure Delete_If_No_Used
        (Self        : not null access SQLite_Writer_Record;
         Resource_Id : Integer;
         Property_Id : Integer)
      is
         procedure Delete
           (In_Use_Query : Prepared_Statement;
            Delete_Query : Prepared_Statement;
            Id           : Natural);

         ------------
         -- Delete --
         ------------

         procedure Delete
           (In_Use_Query : Prepared_Statement;
            Delete_Query : Prepared_Statement;
            Id           : Natural)
         is
            Cursor : Forward_Cursor;
         begin
            Cursor.Fetch (Self.Connection, In_Use_Query, Params => (1 => +Id));

            if not Cursor.Has_Row then
               Self.Connection.Execute (Delete_Query, Params => (1 => +Id));
            end if;
         end Delete;

      begin
         if Resource_Id /= -1 then
            Delete
              (Query_Is_Resource_In_Use, Query_Delete_Resource, Resource_Id);
         end if;

         if Property_Id /= -1 then
            Delete
              (Query_Is_Property_In_Use, Query_Delete_Property, Property_Id);
         end if;
      end Delete_If_No_Used;

      -------------------
      -- Dump_Database --
      -------------------

      overriding procedure Dump_Database
        (Self : not null access SQLite_Writer_Record)
      is
         File : Writable_File := Write_File
           (Get_Properties_Filename (Self.Kernel, True));

         Get_Values : constant Prepared_Statement :=
           Prepare
             (SQL_Select
                (Items.Item & Items.Resource & Items.Property,
                 From => Items),
              On_Server => True, Name => "get_items");

         Get_Resource : constant Prepared_Statement :=
           Prepare
             (SQL_Select
                (Fields => Resources.Name,
                 From   => Resources,
                 Where  => Resources.Id = Integer_Param (1),
                 Limit  => 1),
              On_Server => True, Name => "get_resource_name");

         Get_Property : constant Prepared_Statement :=
           Prepare
             (SQL_Select
                (Fields => Database.Properties.Name,
                 From   => Database.Properties,
                 Where  => Database.Properties.Id = Integer_Param (1),
                 Limit  => 1),
              On_Server => True, Name => "get_property_name");

         function Get
           (Query : Prepared_Statement;
            Id    : Integer)
            return String;
         --  Retrieve record with Id and return result's first column as string

         ---------
         -- Get --
         ---------

         function Get
           (Query : Prepared_Statement;
            Id    : Integer)
            return String
         is
            Cursor : Forward_Cursor;
         begin
            Cursor.Fetch (Self.Connection, Query, Params => (1 => +Id));

            if Cursor.Has_Row then
               return Cursor.Value (0);
            else
               return "";
            end if;
         end Get;

         R : Forward_Cursor;
      begin
         R.Fetch (Self.Connection, Get_Values);

         while R.Has_Row loop
            Write
              (File,
               Get (Get_Resource, R.Integer_Value (1)) & "@" &
                 Get (Get_Property, R.Integer_Value (2)) & ":" &
                 R.Value (0) & ASCII.LF);

            R.Next;
         end loop;

         Close (File);
      end Dump_Database;

      ----------
      -- Init --
      ----------

      procedure Init
        (Self : not null access SQLite_Writer_Record;
         File : Virtual_File)
      is
         R      : Forward_Cursor;
         Create : Boolean;
      begin
         Self.DB := GNATCOLL.SQL.Sqlite.Setup (+File.Full_Name.all, True);

         Self.Connection := Self.DB.Build_Connection;

         R.Fetch (Self.Connection, "PRAGMA user_version;");
         Create := not Self.Connection.Success
           or else R.Integer_Value (0) /= Schema_Version;

         Self.Connection.Execute ("PRAGMA mmap_size=268435456;");

         if Create then
            Self.Create_Database;
         end if;
         Self.Connection.Automatic_Transactions (False);
      end Init;

      ------------
      -- Insert --
      ------------

      overriding procedure Insert
        (Self     : not null access SQLite_Writer_Record;
         Key      : String;
         Name     : String;
         Property : Property_Description) is
      begin
         Self.Insert_Or_Update (Key, Name, Property);
      end Insert;

      ----------------------
      -- Insert_Or_Update --
      ----------------------

      procedure Insert_Or_Update
        (Self     : not null access SQLite_Writer_Record;
         Key      : String;
         Name     : String;
         Property : Property_Description)
      is
         use GNATCOLL.JSON;

         Transaction : Transaction_Controller (Self.Connection);
         Resource_Id : Integer;
         Property_Id : Integer;
         Value       : constant GNATCOLL.JSON.JSON_Value :=
           Property.Value.Store;
         Exist       : Boolean;
      begin
         if Value = JSON_Null then
            return;
         end if;

         Self.Create_If_Needed (Key, Name, Resource_Id, Property_Id);

         declare
            Cursor : Forward_Cursor;
         begin
            Cursor.Fetch
              (Self.Connection,
               Query_Get_Value,
               Params =>
                 (1 => +Resource_Id,
                  2 => +Property_Id));

            Exist := Cursor.Has_Row;
         end;

         if Exist then
            Self.Connection.Execute
              (Query_Update_Value,
               Params => (1 => +String'(GNATCOLL.JSON.Write (Value)),
                          2 => +Resource_Id,
                          3 => +Property_Id));

         else
            Self.Connection.Execute
              (Query_Insert_Value,
               Params => (1 => +String'(GNATCOLL.JSON.Write (Value)),
                          2 => +Resource_Id,
                          3 => +Property_Id));
         end if;
      end Insert_Or_Update;

      --------------
      -- Finalize --
      --------------

      overriding procedure Finalize (Self : in out SQLite_Writer_Record) is
      begin
         if Active (Dump) then
            Self.Dump_Database;
         end if;

         Free (Self.Connection);
         Free (Self.DB);

      exception
         when E : others => Trace (Me, E);
      end Finalize;

      ------------
      -- Get_Id --
      ------------

      function Get_Id
        (Self  : not null access SQLite_Writer_Record;
         Query : Prepared_Statement;
         Name  : String)
         return Integer
      is
         Cursor : Forward_Cursor;
      begin
         Cursor.Fetch (Self.Connection, Query, Params => (1 => +Name));

         if Cursor.Has_Row then
            return Cursor.Integer_Value (0);
         else
            return -1;
         end if;
      end Get_Id;

      -------------
      -- Get_Ids --
      -------------

      procedure Get_Ids
        (Self        : not null access SQLite_Writer_Record;
         Key         : String;
         Name        : String;
         Resource_Id : out Integer;
         Property_Id : out Integer) is
      begin
         Resource_Id := Self.Get_Id (Query_Get_Resource_Id, Key);
         Property_Id := Self.Get_Id (Query_Get_Property_Id, Name);
      end Get_Ids;

      ---------------
      -- Get_Value --
      ---------------

      overriding procedure Get_Value
        (Self     : not null access SQLite_Writer_Record;
         Key      : String;
         Name     : String;
         Property : out Property_Record'Class;
         Found    : out Boolean)
      is
         Transaction : Transaction_Controller (Self.Connection);
         Resource_Id : Integer;
         Property_Id : Integer;
         Cursor      : Forward_Cursor;
      begin
         Found := False;
         Self.Get_Ids (Key, Name, Resource_Id, Property_Id);

         if Resource_Id < 0
           or else Property_Id < 0
         then
            return;
         end if;

         Cursor.Fetch
           (Self.Connection,
            Query_Get_Value,
            Params =>
              (1 => +Resource_Id,
               2 => +Property_Id));

         if Cursor.Has_Row then
            Property.Restore (GNATCOLL.JSON.Read (Cursor.Value (0)), Found);
         end if;
      end Get_Value;

      ----------------
      -- Get_Values --
      ----------------

      overriding procedure Get_Values
        (Self     : not null access SQLite_Writer_Record;
         Name     : String;
         Property : in out Property_Record'Class;
         Callback : access procedure
           (Key : String; Property : in out Property_Record'Class))
      is
         Transaction : Transaction_Controller (Self.Connection);
         Property_Id : constant Integer := Self.Get_Id
           (Query_Get_Property_Id, Name);
         Cursor      : Forward_Cursor;
         Valid       : Boolean;
      begin
         if Property_Id = -1 then
            return;
         end if;

         Cursor.Fetch
           (Self.Connection,
            Query_Get_Values,
            Params => (1 => +Property_Id));

         while Cursor.Has_Row loop
            Property.Restore (GNATCOLL.JSON.Read (Cursor.Value (0)), Valid);
            if Valid then
               Callback (Cursor.Value (1), Property);
            end if;
            Cursor.Next;
         end loop;
      end Get_Values;

      ------------
      -- Remove --
      ------------

      overriding procedure Remove
        (Self : not null access SQLite_Writer_Record;
         Key  : String;
         Name : String)
      is
         Transaction : Transaction_Controller (Self.Connection);
         Resource_Id : Integer;
         Property_Id : Integer;

      begin
         Self.Get_Ids (Key, Name, Resource_Id, Property_Id);

         if Resource_Id /= -1
           and then Property_Id /= -1
         then
            Self.Connection.Execute
              (Query_Delete_Value,
               Params => (1 => +Resource_Id, 2 => +Property_Id));
         end if;

         if Resource_Id /= -1
           or else Property_Id /= -1
         then
            Self.Delete_If_No_Used (Resource_Id, Property_Id);
         end if;
      end Remove;

      ------------
      -- Update --
      ------------

      overriding procedure Update
        (Self     : not null access SQLite_Writer_Record;
         Key      : String;
         Name     : String;
         Property : Property_Description) is
      begin
         Self.Insert_Or_Update (Key, Name, Property);
      end Update;

   end SQLite_Writer;

end GPS.Kernel.Properties;
