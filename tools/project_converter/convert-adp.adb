-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2009-2010, AdaCore              --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Projects;                  use Projects;
with Adp_Converter;             use Adp_Converter;
with GNATCOLL.Projects;         use GNATCOLL.Projects;
with GNATCOLL.VFS;              use GNATCOLL.VFS;

package body Convert.Adp is

   -----------------------------
   -- Convert_From_Adp_To_Gpr --
   -----------------------------

   procedure Convert_From_Adp_To_Gpr
     (Adp_Filename : String;
      Spec_Extension, Body_Extension : GNAT.OS_Lib.String_Access)
   is
      Tree     : constant Project_Tree_Access := new Project_Tree;
      Project  : Project_Type;
      Registry : constant Project_Registry_Access := Create (Tree);
      Tmp      : Boolean;
      pragma Unreferenced (Tmp);
   begin
      Tree.Load_Empty_Project;

      Project := Tree.Create_Project
        (Name => Base_Name (Adp_Filename, ".adp"),
         Path => Create (+Dir_Name (Adp_Filename)));
      --  Set_Paths_Type (Project, Absolute);
      Convert_Adp_File (+Adp_Filename,
                        Registry       => Registry.all,
                        Project        => Project,
                        Spec_Extension => Spec_Extension.all,
                        Body_Extension => Body_Extension.all);
      Tmp := Project.Save;
   end Convert_From_Adp_To_Gpr;
end Convert.Adp;
