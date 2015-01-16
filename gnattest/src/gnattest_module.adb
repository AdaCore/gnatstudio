------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2011-2015, AdaCore                     --
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

with Ada.Calendar;
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;

with Commands.GNATTest;
with Commands.Interactive;
with Glib.Object;                       use Glib.Object;

with GNAT.Calendar.Time_IO;

with GPS.Kernel;                        use GPS.Kernel;
with GPS.Kernel.Actions;                use GPS.Kernel.Actions;
with GPS.Kernel.Contexts;               use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;                  use GPS.Kernel.Hooks;
with GPS.Kernel.Messages.Simple;
with GPS.Kernel.Modules;                use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;             use GPS.Kernel.Modules.UI;
with GPS.Kernel.Project;

with Gtk.Handlers;
with Gtk.Menu;
with Gtk.Menu_Item;

with Input_Sources.File;
with Sax.Readers;
with Sax.Attributes;
with Src_Editor_Box;
with Src_Editor_Buffer;
with Unicode.CES;
with Xref;                              use Xref;

package body GNATTest_Module is

   type GNATTest_Module_Record is new Module_ID_Record with null record;
   GNATTest_Module_ID   : Module_ID;
   GNATTest_Module_Name : constant String := "GNATTest_Support";

   type Harness_Project_Filter is new GPS.Kernel.Action_Filter_Record
     with null record;

   overriding function Filter_Matches_Primitive
     (Filter  : access Harness_Project_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean;

   type Non_Harness_Project_Filter is new GPS.Kernel.Action_Filter_Record
     with null record;

   overriding function Filter_Matches_Primitive
     (Filter  : access Non_Harness_Project_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean;

   type Create_Harness_Project_Filter is new GPS.Kernel.Action_Filter_Record
     with null record;

   overriding function Filter_Matches_Primitive
     (Filter  : access Create_Harness_Project_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean;

   type Harness_Project_Exists_Filter is new GPS.Kernel.Action_Filter_Record
     with null record;

   overriding function Filter_Matches_Primitive
     (Filter  : access Harness_Project_Exists_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean;

   type Go_To_Tested_Filter is
     new GPS.Kernel.Action_Filter_Record with null record;

   overriding function Filter_Matches_Primitive
     (Filter  : access Go_To_Tested_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean;

   type Package_Declaration_Filter is
     new GPS.Kernel.Action_Filter_Record with null record;

   overriding function Filter_Matches_Primitive
     (Filter  : access Package_Declaration_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean;

   type Submenu_Factory_Record is
     new GPS.Kernel.Modules.UI.Submenu_Factory_Record with null record;

   overriding procedure Append_To_Menu
     (Factory : access Submenu_Factory_Record;
      Context : GPS.Kernel.Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);

   function Get_Mapping_File
     (Project : GNATCOLL.Projects.Project_Type)
     return String;

   type On_Project_Changed is new Simple_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Project_Changed;
      Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class);

   type Source_Entity is record
      Source_File      : Unbounded_String;
      Test_Unit        : Unbounded_String;
      Subprogram_Name  : Unbounded_String;
      Line             : Natural := 0;
      Column           : Natural := 0;
      Test_Case_Name   : Unbounded_String;
      Test_Case_Line   : Natural := 0;
      Test_Case_Column : Natural := 0;
   end record;

   function "<" (Left, Right : Source_Entity) return Boolean;

   type Test_Entity is record
      File_Name        : Unbounded_String;
      Line             : Natural;
      Column           : Natural;
      Stamp            : Ada.Calendar.Time;
   end record;

   package Source_Entity_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Source_Entity,
      Element_Type => Test_Entity);

   package Test_Entity_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Unbounded_String,
      Element_Type => Source_Entity);

   type Modes is (Monolith, Separates);

   type Mapping_File is new Sax.Readers.Reader with record
      Mode         : Modes;
      Last_Source  : Source_Entity;
      Source_Map   : Source_Entity_Maps.Map;
      Test_Map     : Test_Entity_Maps.Map;
      First_Test   : Boolean;
      First_Tested : Boolean;
      Setup        : Test_Entity;
      Teardown     : Test_Entity;
   end record;

   overriding procedure Start_Element
     (Self          : in out Mapping_File;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "";
      Atts          : Sax.Attributes.Attributes'Class);

   type Show_Not_Implemented_Tests_Command_Type is
     new Commands.Interactive.Interactive_Command with null record;

   overriding function Execute
     (Command : access Show_Not_Implemented_Tests_Command_Type;
      Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type;

   Map : Mapping_File;

   function Find_In_Map
     (File_Name : GNATCOLL.VFS.Virtual_File)
     return Test_Entity_Maps.Cursor;

   function Tested_Subprogram_Name
     (Context : GPS.Kernel.Selection_Context)
     return String;

   type Menu_Data is record
      Entity : Test_Entity;
      Kernel : GPS.Kernel.Kernel_Handle;
   end record;

   package Test_Entity_CB is new Gtk.Handlers.User_Callback
     (Gtk.Menu_Item.Gtk_Menu_Item_Record, Menu_Data);

   procedure Test_Entity_Callback
     (Widget    : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class;
      User_Data : Menu_Data);

   function Harness_Project_Exists
     (Project : GNATCOLL.Projects.Project_Type)
      return Boolean;

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Source_Entity) return Boolean is
   begin
      if Left.Source_File = Right.Source_File then
         if Left.Line = Right.Line then
            return Left.Test_Case_Name < Right.Test_Case_Name;
         else
            return Left.Line < Right.Line;
         end if;
      else
         return Left.Source_File < Right.Source_File;
      end if;
   end "<";

   --------------------
   -- Append_To_Menu --
   --------------------

   overriding procedure Append_To_Menu
     (Factory : access Submenu_Factory_Record;
      Context : GPS.Kernel.Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      pragma Unreferenced (Factory);

      Item : Gtk.Menu_Item.Gtk_Menu_Item;

      Entity : constant Root_Entity'Class := Get_Entity (Context);

      Declaration : constant General_Entity_Declaration :=
        Get_Declaration (Entity);

      Lookup : Source_Entity;
      Cursor : Source_Entity_Maps.Cursor;
   begin
      Lookup.Source_File := To_Unbounded_String
        (Declaration.Loc.File.Display_Base_Name);

      Lookup.Line := Declaration.Loc.Line;

      Cursor := Map.Source_Map.Floor (Lookup);

      if Source_Entity_Maps.Has_Element (Cursor) then
         Cursor := Source_Entity_Maps.Next (Cursor);
      else
         Cursor := Map.Source_Map.First;
      end if;

      while Source_Entity_Maps.Has_Element (Cursor) loop
         declare
            Found : constant Source_Entity := Source_Entity_Maps.Key (Cursor);
         begin

            exit when Found.Source_File /= Lookup.Source_File or
              Found.Line /= Lookup.Line;

            Gtk.Menu_Item.Gtk_New
              (Item, "Go to " & To_String (Found.Test_Case_Name));

            Menu.Append (Item);

            Test_Entity_CB.Connect
              (Item,
               Gtk.Menu_Item.Signal_Activate,
               Test_Entity_Callback'Access,
               (Entity => Source_Entity_Maps.Element (Cursor),
                Kernel => GPS.Kernel.Get_Kernel (Context)));

            Source_Entity_Maps.Next (Cursor);
         end;
      end loop;

   end Append_To_Menu;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Show_Not_Implemented_Tests_Command_Type;
      Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type
   is
      pragma Unreferenced (Command);
      use type Ada.Calendar.Time;

      Flags    : constant GPS.Kernel.Messages.Message_Flags :=
        (GPS.Kernel.Messages.Locations => True,
         others                        => False);

      Category : constant String := "GNATtest";
      Kernel   : constant Kernel_Handle := Get_Kernel (Context.Context);
      Messages : constant GPS.Kernel.Messages.Messages_Container_Access :=
        GPS.Kernel.Messages.Get_Messages_Container (Kernel);
      Cursor   : Source_Entity_Maps.Cursor := Map.Source_Map.First;
   begin
      GPS.Kernel.Messages.Get_Messages_Container (Kernel).Remove_Category
        (Category, Flags);

      while Source_Entity_Maps.Has_Element (Cursor) loop
         declare
            use type GNATCOLL.VFS.Filesystem_String;
            Key  : constant Source_Entity := Source_Entity_Maps.Key (Cursor);
            Item : constant Test_Entity := Source_Entity_Maps.Element (Cursor);
            File : constant GNATCOLL.VFS.Virtual_File := GPS.Kernel.Create
              (+To_String (Key.Source_File), Kernel);
            Test : constant GNATCOLL.VFS.Virtual_File := GPS.Kernel.Create
              (+To_String (Item.File_Name), Kernel);
         begin
            if Test.File_Time_Stamp = Item.Stamp then
               GPS.Kernel.Messages.Simple.Create_Simple_Message
                 (Container => Messages,
                  Category  => Category,
                  File      => File,
                  Line      => Key.Line,
                  Column    => Basic_Types.Visible_Column_Type (Key.Column),
                  Text      => "Unimplemented " &
                    To_String (Key.Test_Case_Name) & " " &
                    To_String (Key.Subprogram_Name),
                  Weight    => 1,
                  Flags     => Flags);
            end if;

            Source_Entity_Maps.Next (Cursor);
         end;
      end loop;

      return Commands.Success;
   end Execute;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Harness_Project_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      declare
         Project : constant GNATCOLL.Projects.Project_Type
           := GPS.Kernel.Project.Get_Project (GPS.Kernel.Get_Kernel (Context));

         Value : constant String := Get_Mapping_File (Project);
      begin
         return Value /= "";
      end;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Non_Harness_Project_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      declare
         Project : constant GNATCOLL.Projects.Project_Type
           := GPS.Kernel.Project.Get_Project (GPS.Kernel.Get_Kernel (Context));

         Value : constant String := Get_Mapping_File (Project);
      begin
         return Value = "";
      end;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Create_Harness_Project_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      if not Has_Project_Information (Context) then
         return False;
      end if;

      declare
         Project : constant GNATCOLL.Projects.Project_Type
            := Project_Information (Context);

         Value : constant String := Get_Mapping_File (Project);
      begin
         return Value = "" and then
           not Harness_Project_Exists (Project);
      end;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Harness_Project_Exists_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      if not Has_Project_Information (Context) then
         return False;
      end if;

      declare
         Project : constant GNATCOLL.Projects.Project_Type
           := Project_Information (Context);
      begin
         return Harness_Project_Exists (Project);
      end;
   end Filter_Matches_Primitive;

   ----------------------------
   -- Harness_Project_Exists --
   ----------------------------

   function Harness_Project_Exists
     (Project : GNATCOLL.Projects.Project_Type)
      return Boolean
   is
      use type GNATCOLL.VFS.Filesystem_String;

      Name  : constant GNATCOLL.Projects.Attribute_Pkg_String
        := GNATCOLL.Projects.Build ("GNATtest", "Harness_Dir");

      Value : constant String := Project.Attribute_Value
        (Name, Default => "gnattest/harness");

      Object_Dir : constant GNATCOLL.VFS.Virtual_File
        := Project.Object_Dir;

      Harness_Dir : constant GNATCOLL.VFS.Virtual_File
        := GNATCOLL.VFS.Create_From_Base (+Value, Object_Dir.Full_Name);

      Harness_Project : constant GNATCOLL.VFS.Virtual_File
        := Harness_Dir.Create_From_Dir ("test_driver.gpr");
   begin
      return Value /= "" and then Harness_Project.Is_Regular_File;
   end Harness_Project_Exists;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Go_To_Tested_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      if Has_File_Information (Context) then
         return Test_Entity_Maps.Has_Element
             (Find_In_Map (File_Information (Context)));
      else
         return False;
      end if;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Package_Declaration_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
      use type GNATCOLL.Projects.Unit_Parts;
      File : GNATCOLL.VFS.Virtual_File;
   begin
      if Has_File_Information (Context) then
         File := File_Information (Context);

         if File.Is_Regular_File then
            --  Safe to use the first matching info, since the language will
            --  most likely be the same in all projects anyway.

            declare
               Info : constant GNATCOLL.Projects.File_Info'Class :=
                 GNATCOLL.Projects.File_Info'Class
                   (GPS.Kernel.Project.Get_Registry
                      (GPS.Kernel.Get_Kernel (Context)).Tree.Info_Set (File)
                    .First_Element);
            begin

               return Info.Language = "ada" and then
                 Info.Unit_Part = GNATCOLL.Projects.Unit_Spec;
            end;
         end if;
      end if;

      return False;
   end Filter_Matches_Primitive;

   -----------------
   -- Find_Tested --
   -----------------

   procedure Find_Tested
     (File_Name       : GNATCOLL.VFS.Virtual_File;
      Unit_Name       : out Ada.Strings.Unbounded.Unbounded_String;
      Subprogram_Name : out Ada.Strings.Unbounded.Unbounded_String;
      Line            : out Natural;
      Column          : out Basic_Types.Visible_Column_Type)
   is
      Cursor : constant Test_Entity_Maps.Cursor := Find_In_Map (File_Name);
   begin
      if Test_Entity_Maps.Has_Element (Cursor) then
         Unit_Name := Test_Entity_Maps.Element (Cursor).Source_File;
         Subprogram_Name := Test_Entity_Maps.Element (Cursor).Subprogram_Name;
         Line := Test_Entity_Maps.Element (Cursor).Line;
         Column := Basic_Types.Visible_Column_Type
           (Test_Entity_Maps.Element (Cursor).Column);
      else
         Unit_Name := Ada.Strings.Unbounded.Null_Unbounded_String;
         Subprogram_Name := Ada.Strings.Unbounded.Null_Unbounded_String;
         Line := 0;
         Column := 0;
      end if;
   end Find_Tested;

   ---------------
   -- Find_Test --
   ---------------

   function Find_In_Map
     (File_Name : GNATCOLL.VFS.Virtual_File)
     return Test_Entity_Maps.Cursor
   is
      Item : constant Unbounded_String :=
        To_Unbounded_String (String (File_Name.Base_Name));
   begin
      return Map.Test_Map.Find (Item);
   end Find_In_Map;

   ----------------------
   -- Get_Mapping_File --
   ----------------------

   function Get_Mapping_File
     (Project : GNATCOLL.Projects.Project_Type)
     return String
   is
      Name  : constant GNATCOLL.Projects.Attribute_Pkg_String
        := GNATCOLL.Projects.Build ("GNATtest", "GNATtest_Mapping_File");
   begin
      return Project.Attribute_Value (Name);
   end Get_Mapping_File;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Project_Changed;
      Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Self);
      Project : constant GNATCOLL.Projects.Project_Type :=
         GPS.Kernel.Project.Get_Project (Kernel);
      Map_File_Name : constant String := Get_Mapping_File (Project);
      File        : Input_Sources.File.File_Input;
   begin
      Map.Source_Map.Clear;
      Map.Test_Map.Clear;

      if Map_File_Name /= "" then
         Input_Sources.File.Open (Map_File_Name, File);
         Map.Parse (File);
         Input_Sources.File.Close (File);
      end if;
   end Execute;

   ---------------
   -- Open_File --
   ---------------

   procedure Open_File
     (Kernel          : GPS.Kernel.Kernel_Handle;
      Project         : GNATCOLL.Projects.Project_Type;
      Unit_Name       : String;
      Line            : Natural;
      Column          : Basic_Types.Visible_Column_Type;
      Subprogram_Name : String := "")
   is
      File  : constant GNATCOLL.VFS.Virtual_File := GPS.Kernel.Create
        (GNATCOLL.VFS.Filesystem_String (Unit_Name), Kernel);

   begin
      Src_Editor_Box.Go_To_Closest_Match
        (Kernel      => Kernel,
         Filename    => File,
         Project     => Project,
         Line        => Src_Editor_Buffer.Editable_Line_Type (Line),
         Column      => Column,
         Entity_Name => Subprogram_Name);
   end Open_File;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      use Commands.GNATTest;
      Filter : Action_Filter;
   begin
      GNATTest_Module_ID := new GNATTest_Module_Record;

      Register_Module
        (Module      => GNATTest_Module_ID,
         Kernel      => Kernel,
         Module_Name => GNATTest_Module_Name,
         Priority    => Default_Priority);

      Filter := new Harness_Project_Filter;
      Register_Filter (Kernel, Filter, "Harness project");

      GPS.Kernel.Actions.Register_Action
        (Kernel      => Kernel,
         Name        => "Show not implemented tests",
         Command     => new Show_Not_Implemented_Tests_Command_Type,
         Filter      => Filter);

      Filter := new Non_Harness_Project_Filter;
      Register_Filter (Kernel, Filter, "Non harness project");

      Filter := new Create_Harness_Project_Filter;
      Register_Filter (Kernel, Filter, "Create harness project");

      Filter := new Harness_Project_Exists_Filter;
      Register_Filter (Kernel, Filter, "Harness project exists");

      Filter := new Package_Declaration_Filter;
      Register_Filter (Kernel, Filter, "Library package declaration");

      Filter := new Go_To_Tested_Filter;
      Register_Filter (Kernel, Filter, "Tested exists");

      Register_Contextual_Submenu
        (Kernel   => Kernel,
         Name     => "GNATtest",
         Label    => "GNATtest",
         Filter   => GPS.Kernel.Lookup_Filter (Kernel, "Entity is subprogram"),
         Submenu  => new Submenu_Factory_Record,
         Ref_Item => "Coverage");

      Register_Action
        (Kernel      => Kernel,
         Name        => "go to tested procedure",
         Command     => new Go_To_Tested_Command_Type,
         Filter      => Filter);
      Register_Contextual_Menu
        (Kernel      => Kernel,
         Name        => "Goto tested subprogram",
         Action      => "go to tested procedure",
         Label       => "GNATtest/Go to %C",
         Custom      => Tested_Subprogram_Name'Access,
         Ref_Item    => "GNATtest",
         Add_Before  => False);

      Project_View_Changed_Hook.Add (new On_Project_Changed);
   end Register_Module;

   -------------------
   -- Start_Element --
   -------------------

   overriding procedure Start_Element
     (Self          : in out Mapping_File;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "";
      Atts          : Sax.Attributes.Attributes'Class)
   is
      pragma Unreferenced (Namespace_URI);
      pragma Unreferenced (Qname);

      function To_Integer (Name : String) return Integer;
      function To_Time (Name : String) return Ada.Calendar.Time;
      function Get_Attribute (Name : String) return String;
      --  Return value of attribute with given Name or empty string if no such

      procedure Add_Setup_Teardown;

      procedure Add_Setup_Teardown is
      begin
         Self.Last_Source.Test_Case_Name := To_Unbounded_String ("test setup");
         Self.Source_Map.Include (Self.Last_Source, Self.Setup);

         Self.Last_Source.Test_Case_Name :=
           To_Unbounded_String ("test teardown");
         Self.Source_Map.Include (Self.Last_Source, Self.Teardown);

         if Self.First_Tested then
            Self.Test_Map.Include (Self.Teardown.File_Name, Self.Last_Source);
            Self.First_Tested := False;
         end if;
      end Add_Setup_Teardown;

      function Get_Attribute (Name : String) return String is
         Index : constant Integer := Atts.Get_Index (Name);
      begin
         if Index < 0 then
            return "";
         else
            return Atts.Get_Value (Index);
         end if;
      end Get_Attribute;

      function To_Integer (Name : String) return Integer is
      begin
         return Integer'Value (Atts.Get_Value (Name));
      end To_Integer;

      Null_Time : constant Ada.Calendar.Time := Ada.Calendar.Time_Of
        (Year    => Ada.Calendar.Year_Number'First,
         Month   => Ada.Calendar.Month_Number'First,
         Day     => Ada.Calendar.Day_Number'First);

      function To_Time (Name : String) return Ada.Calendar.Time is
         Image : constant String := Get_Attribute (Name);
      begin
         if Image /= "" and Image /= "modified" then
            return GNAT.Calendar.Time_IO.Value (Image);
         end if;

         return Null_Time;
      end To_Time;
   begin
      if Local_Name = "tests_mapping" then
         if Get_Attribute ("mode") = "monolith" then
            Self.Mode := Monolith;
         else
            Self.Mode := Separates;
         end if;
      elsif Local_Name = "unit" then
         Self.Last_Source.Source_File :=
           To_Unbounded_String (Atts.Get_Value ("source_file"));

      elsif Local_Name = "test_unit" then
         Self.Last_Source.Test_Unit :=
           To_Unbounded_String (Atts.Get_Value ("target_file"));
         Self.First_Tested := True;

      elsif Local_Name = "tested" then
         Self.Last_Source.Subprogram_Name :=
           To_Unbounded_String (Atts.Get_Value ("name"));
         Self.Last_Source.Line := To_Integer ("line");
         Self.Last_Source.Column := To_Integer ("column");
         Self.Last_Source.Test_Case_Name := To_Unbounded_String ("test case");
         Self.Last_Source.Test_Case_Line := 0;
         Self.Last_Source.Test_Case_Column := 0;
         Self.First_Test := True;

      elsif Local_Name = "test_case" then
         Self.Last_Source.Test_Case_Name :=
           To_Unbounded_String (Atts.Get_Value ("name"));
         Self.Last_Source.Test_Case_Line := To_Integer ("line");
         Self.Last_Source.Test_Case_Column := To_Integer ("column");

      elsif Local_Name = "setup" then
         Self.Setup.File_Name :=
              To_Unbounded_String (Atts.Get_Value ("file"));
         Self.Setup.Line := To_Integer ("line");
         Self.Setup.Column := To_Integer ("column");
         Self.Setup.Stamp := Null_Time;

      elsif Local_Name = "teardown" then
         Self.Teardown.File_Name :=
              To_Unbounded_String (Atts.Get_Value ("file"));
         Self.Teardown.Line := To_Integer ("line");
         Self.Teardown.Column := To_Integer ("column");
         Self.Setup.Stamp := Null_Time;

      elsif Local_Name = "test" then
         declare
            Target : Test_Entity;
         begin
            Target.File_Name :=
              To_Unbounded_String (Atts.Get_Value ("file"));
            Target.Line := To_Integer ("line");
            Target.Column := To_Integer ("column");
            Target.Stamp := To_Time ("timestamp");

            Self.Source_Map.Include (Self.Last_Source, Target);
            Self.Test_Map.Include (Target.File_Name, Self.Last_Source);

            if Self.First_Test then
               Add_Setup_Teardown;
               Self.First_Test := False;
            end if;
         end;
      end if;
   end Start_Element;

   --------------------------
   -- Test_Entity_Callback --
   --------------------------

   procedure Test_Entity_Callback
     (Widget    : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class;
      User_Data : Menu_Data)
   is
      pragma Unreferenced (Widget);
   begin
      Open_File
        (User_Data.Kernel,
         Project   => GNATCOLL.Projects.No_Project,  --  will use any project
         Unit_Name => To_String (User_Data.Entity.File_Name),
         Line   => User_Data.Entity.Line,
         Column => Basic_Types.Visible_Column_Type (User_Data.Entity.Column));
   end Test_Entity_Callback;

   ----------------------------
   -- Tested_Subprogram_Name --
   ----------------------------

   function Tested_Subprogram_Name
     (Context : GPS.Kernel.Selection_Context)
     return String is

      Cursor : constant Test_Entity_Maps.Cursor :=
        Find_In_Map (File_Information (Context));
   begin
      if Test_Entity_Maps.Has_Element (Cursor) then
         return GPS.Kernel.Modules.UI.Emphasize
           (To_String (Test_Entity_Maps.Element (Cursor).Subprogram_Name));
      else
         return "";
      end if;
   end Tested_Subprogram_Name;

end GNATTest_Module;
