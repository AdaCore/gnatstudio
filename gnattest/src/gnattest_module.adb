-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2011, AdaCore                    --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------
with Ada.Containers.Ordered_Maps;

with Commands.GNATTest;

with Glib.Object;                       use Glib.Object;

with GNATCOLL.Projects;
with GNATCOLL.Symbols;
with GNATCOLL.VFS;

with GPS.Kernel;                        use GPS.Kernel;
with GPS.Kernel.Actions;
with GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;
with GPS.Kernel.Modules;                use GPS.Kernel.Modules;
with GPS.Kernel.Project;

with Input_Sources.File;
with Sax.Readers;
with Sax.Attributes;
with Unicode.CES;
with Commands.Interactive;

package body GNATTest_Module is

   use Ada.Strings.Unbounded;

   type GNATTest_Module_Record is new Module_ID_Record with null record;
   GNATTest_Module_ID   : Module_ID;
   GNATTest_Module_Name : constant String := "GNATTest_Support";

   type Harness_Project_Filter is new GPS.Kernel.Action_Filter_Record
     with null record;

   overriding function Filter_Matches_Primitive
     (Filter  : access Harness_Project_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean;

   type Non_Harness_Project_Filter is new Harness_Project_Filter
     with null record;

   overriding function Filter_Matches_Primitive
     (Filter  : access Non_Harness_Project_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean;

   type Harness_Project_Exists_Filter is new GPS.Kernel.Action_Filter_Record
     with null record;

   overriding function Filter_Matches_Primitive
     (Filter  : access Harness_Project_Exists_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean;

   type Go_To_Test_Filter is new GPS.Kernel.Action_Filter_Record with record
      To_Test : Boolean;
   end record;

   overriding function Filter_Matches_Primitive
     (Filter  : access Go_To_Test_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean;

   function Get_Mapping_File
     (Project : GNATCOLL.Projects.Project_Type)
     return String;

   procedure On_Project_Changed
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);

   type Entity_Name is record
      Unit_Name       : Unbounded_String;
      Subprogram_Name : Unbounded_String;
      Line            : Natural;
      Column          : Natural;
   end record;

   function "<" (Left, Right : Entity_Name) return Boolean;

   package Entity_Name_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Entity_Name,
      Element_Type => Entity_Name);

   function Find_In_Map
     (Entity  : Entities.Entity_Information;
      To_Test : Boolean)
     return Entity_Name_Maps.Cursor;

   type Mapping_File is new Sax.Readers.Reader with record
      Source_File : Unbounded_String;
      Target_File : Unbounded_String;
      Test_Map    : Entity_Name_Maps.Map;
      Tested_Map  : Entity_Name_Maps.Map;
   end record;

   overriding procedure Start_Element
     (Self          : in out Mapping_File;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "";
      Atts          : Sax.Attributes.Attributes'Class);

   Map : Mapping_File;

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Entity_Name) return Boolean is
   begin
      return Left.Unit_Name < Right.Unit_Name or else
        (Left.Unit_Name = Right.Unit_Name and
           Left.Subprogram_Name < Right.Subprogram_Name);
   end "<";

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Harness_Project_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      if not GPS.Kernel.Contexts.Has_Project_Information (Context) then
         return False;
      end if;

      declare
         Project : constant GNATCOLL.Projects.Project_Type
            := GPS.Kernel.Contexts.Project_Information (Context);

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
      Context : GPS.Kernel.Selection_Context) return Boolean is
   begin
      if GPS.Kernel.Contexts.Has_Project_Information (Context) then
         return not Harness_Project_Filter (Filter.all)'Access
           .Filter_Matches_Primitive (Context);
      else
         return False;
      end if;
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
      if not GPS.Kernel.Contexts.Has_Project_Information (Context) then
         return False;
      end if;

      declare
         use type GNATCOLL.VFS.Filesystem_String;

         Project : constant GNATCOLL.Projects.Project_Type
           := GPS.Kernel.Contexts.Project_Information (Context);

         Name  : constant GNATCOLL.Projects.Attribute_Pkg_String
           := GNATCOLL.Projects.Build ("GNATtest", "Harness_Dir");

         Value : constant String := Project.Attribute_Value (Name);

         Project_Path : constant GNATCOLL.VFS.Virtual_File
           := Project.Project_Path;

         Harness_Dir : constant GNATCOLL.VFS.Virtual_File
           := GNATCOLL.VFS.Create_From_Base (+Value, Project_Path.Dir_Name);

         Harness_Project : constant GNATCOLL.VFS.Virtual_File
           := Harness_Dir.Create_From_Dir ("test_driver.gpr");
      begin
         return Value /= "" and then Harness_Project.Is_Regular_File;
      end;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Go_To_Test_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean
   is
      use type Entities.Entity_Information;
      Entity : Entities.Entity_Information;
   begin
      if GPS.Kernel.Contexts.Has_Entity_Name_Information (Context) then
         Entity := GPS.Kernel.Contexts.Get_Entity (Context);
         return Entity /= null
           and then Entities.Is_Subprogram (Entity)
           and then Entity_Name_Maps.Has_Element
             (Find_In_Map (Entity, Filter.To_Test));
      else
         return False;
      end if;
   end Filter_Matches_Primitive;

   ----------
   -- Find --
   ----------

   procedure Find
     (Entity          : Entities.Entity_Information;
      To_Test         : Boolean;
      Unit_Name       : out Ada.Strings.Unbounded.Unbounded_String;
      Subprogram_Name : out Ada.Strings.Unbounded.Unbounded_String;
      Line            : out Natural;
      Column          : out Basic_Types.Visible_Column_Type)
   is
      Cursor : constant Entity_Name_Maps.Cursor :=
        Find_In_Map (Entity, To_Test);
   begin
      if Entity_Name_Maps.Has_Element (Cursor) then
         Unit_Name := Entity_Name_Maps.Element (Cursor).Unit_Name;
         Subprogram_Name := Entity_Name_Maps.Element (Cursor).Subprogram_Name;
         Line := Entity_Name_Maps.Element (Cursor).Line;
         Column := Basic_Types.Visible_Column_Type
           (Entity_Name_Maps.Element (Cursor).Column);
      else
         Unit_Name := Ada.Strings.Unbounded.Null_Unbounded_String;
         Subprogram_Name := Ada.Strings.Unbounded.Null_Unbounded_String;
         Line := 0;
         Column := 0;
      end if;
   end Find;

   ---------------
   -- Find_Test --
   ---------------

   function Find_In_Map
     (Entity  : Entities.Entity_Information;
      To_Test : Boolean)
     return Entity_Name_Maps.Cursor
   is
      use Entities;
      Item : Entity_Name;
   begin
      Item.Unit_Name := To_Unbounded_String
        (String (Get_Filename (Get_Declaration_Of (Entity).File).Base_Name));

      --  Entity.Get_Declaration_Of.File.Get_Filename.Base_Name));
      Item.Subprogram_Name := To_Unbounded_String
        (GNATCOLL.Symbols.Get (Get_Name (Entity)).all);

      if To_Test then
         return Map.Test_Map.Find (Item);
      else
         return Map.Tested_Map.Find (Item);
      end if;
   end Find_In_Map;

   ----------------------
   -- Get_Mapping_File --
   ----------------------

   function Get_Mapping_File
     (Project : GNATCOLL.Projects.Project_Type)
     return String
   is
      Name  : constant GNATCOLL.Projects.Attribute_Pkg_String
        := GNATCOLL.Projects.Build ("GNATtest", "GNATTest_Mapping_File");
   begin
      return Project.Attribute_Value (Name);
   end Get_Mapping_File;

   ------------------------
   -- On_Project_Changed --
   ------------------------

   procedure On_Project_Changed
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Project : constant GNATCOLL.Projects.Project_Type
        := GPS.Kernel.Project.Get_Project (Kernel);
      Map_File_Name : constant String := Get_Mapping_File (Project);
      File        : Input_Sources.File.File_Input;
   begin
      if Map_File_Name /= "" then
         Input_Sources.File.Open (Map_File_Name, File);
         Map.Parse (File);
         Input_Sources.File.Close (File);
      end if;
   end On_Project_Changed;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      use Commands.GNATTest;
      Filter : Action_Filter;

      Go_To_Test_Command : constant Go_To_Test_Command_Access :=
        new Go_To_Test_Command_Type'(Commands.Interactive.Interactive_Command
                                     with To_Test => True);
      Go_To_Tested_Command : constant Go_To_Test_Command_Access :=
        new Go_To_Test_Command_Type'(Commands.Interactive.Interactive_Command
                                     with To_Test => False);
   begin
      GNATTest_Module_ID := new GNATTest_Module_Record;

      Register_Module
        (Module                  => GNATTest_Module_ID,
         Kernel                  => Kernel,
         Module_Name             => GNATTest_Module_Name,
         Priority                => Default_Priority);

      Filter := new Harness_Project_Filter;
      Register_Filter (Kernel, Filter, "Harness project");

      Filter := new Non_Harness_Project_Filter;
      Register_Filter (Kernel, Filter, "Non harness project");

      Filter := new Harness_Project_Exists_Filter;
      Register_Filter (Kernel, Filter, "Harness project exists");

      Filter := new Go_To_Test_Filter'
        (GPS.Kernel.Action_Filter_Record with To_Test => True);
      Register_Filter (Kernel, Filter, "Test exists");

      GPS.Kernel.Actions.Register_Action
        (Kernel      => Kernel,
         Name        => "go to test procedure",
         Command     => Go_To_Test_Command,
         Filter      => Filter);

      Filter := new Go_To_Test_Filter'
        (GPS.Kernel.Action_Filter_Record with To_Test => False);
      Register_Filter (Kernel, Filter, "Tested exists");

      GPS.Kernel.Actions.Register_Action
        (Kernel      => Kernel,
         Name        => "go to tested procedure",
         Command     => Go_To_Tested_Command,
         Filter      => Filter);

      GPS.Kernel.Hooks.Add_Hook
        (Kernel,
         GPS.Kernel.Project_View_Changed_Hook,  --  Project_Changed_Hook,
         GPS.Kernel.Hooks.Wrapper (On_Project_Changed'Access),
         "gnattest.project_view_changed");

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

      function To_Integer (Name : String) return Integer is
      begin
         return Integer'Value (Atts.Get_Value (Name));
      end To_Integer;
   begin
      if Local_Name = "unit" then
         Self.Source_File :=
           To_Unbounded_String (Atts.Get_Value ("source_file"));
         Self.Target_File :=
           To_Unbounded_String (Atts.Get_Value ("target_file"));
      elsif Local_Name = "pair" then
         declare
            Source : Entity_Name;
            Target : Entity_Name;
         begin
            Source.Unit_Name := Self.Source_File;
            Target.Unit_Name := Self.Target_File;
            Source.Subprogram_Name :=
              To_Unbounded_String (Atts.Get_Value ("tested"));
            Target.Subprogram_Name :=
              To_Unbounded_String (Atts.Get_Value ("test"));
            Source.Line := To_Integer ("tested_line");
            Source.Column := To_Integer ("tested_column");
            Target.Line := To_Integer ("test_line");
            Target.Column := To_Integer ("test_column");
            Self.Test_Map.Include (Source, Target);
            Self.Tested_Map.Include (Target, Source);
         end;
      end if;
   end Start_Element;

end GNATTest_Module;
