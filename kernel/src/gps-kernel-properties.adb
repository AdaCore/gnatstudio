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
with GNATCOLL.Projects;          use GNATCOLL.Projects;
with GNATCOLL.Scripts;           use GNATCOLL.Scripts;
with GNATCOLL.VFS;               use GNATCOLL.VFS;

with GPS.Intl;                   use GPS.Intl;
with GPS.Kernel.Hooks;           use GPS.Kernel.Hooks;
with GPS.Kernel.Scripts;         use GPS.Kernel.Scripts;
with Default_Preferences;        use Default_Preferences;

with GPS.Kernel.Properties.File_Writer;

package body GPS.Kernel.Properties is

   use Properties_Indefinite_Hashed_Maps;

   Store_Properties_On_The_Fly : Boolean_Preference;

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
      Object : constant Writer := Writer (File_Writer.Constructor (Kernel));
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

end GPS.Kernel.Properties;
