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

with Glib.Object;                       use Glib.Object;

with GNATCOLL.Projects;
with GNATCOLL.VFS;

with GPS.Kernel;                        use GPS.Kernel;
with GPS.Kernel.Contexts;
with GPS.Kernel.Modules;                use GPS.Kernel.Modules;

package body GNATTest_Module is

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

         Name  : constant GNATCOLL.Projects.Attribute_Pkg_String
           := GNATCOLL.Projects.Build ("GNATtest", "GNATTest_Mapping_File");

         Value : constant String := Project.Attribute_Value (Name);
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

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Filter : Action_Filter;
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
   end Register_Module;

end GNATTest_Module;
