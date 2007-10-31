-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2006-2007, AdaCore                 --
--                                                                   --
-- GPS is Free  software;  you can redistribute it and/or modify  it --
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

--  <description>
--  This package provides the coverage specific graphical user interface
--  subprograms for Code Analysis Module use
--  </description>

with GNAT.Traces;
with Glib;
with GPS.Kernel;    use GPS.Kernel;
with GPS.Intl;      use GPS.Intl;
with VFS;           use VFS;
with Language;      use Language;
with Traces;        use Traces;
with Code_Analysis; use Code_Analysis;

package Coverage_GUI is

   Gcov_Extension_Cst : constant String := ".gcov";
   --  Constant String that represents the extension of GCOV files.

   CodeAnalysis_Cst   : constant String := "CodeAnalysis";
   Coverage_Category  : constant Glib.UTF8_String := -"Uncovered lines";

   Binary_Coverage_Trace : constant Debug_Handle :=
                             Create ("BINARY_COVERAGE_MODE", GNAT.Traces.On);
   Binary_Coverage_Mode  : Boolean;
   --  Boolean that allows to determine wether we are in binary coverage mode
   --  or not, if true no line execution coverage count will be displayed.

   procedure Add_Gcov_Project_Info
     (Kernel   : Kernel_Handle;
      Prj_Node : Project_Access);
   --  Try to load Gcov information for every files of the given project.

   procedure Add_Gcov_File_Info
     (Kernel       : Kernel_Handle;
      Src_File     : VFS.Virtual_File;
      Cov_File     : VFS.Virtual_File;
      Project_Node : Project_Access);
   --  Add into the corresponding code_analysis nodes the coverage info
   --  provided by the given-gcov-file parsing.

   procedure Clear_Project_Locations
     (Kernel       : Kernel_Handle;
      Project_Node : Project_Access);
   --  Remove from the Locations view the uncovered lines of each files of the
   --  given Project_Node.
   --  Does nothing if the uncovered lines are not listed there.

   procedure Clear_File_Locations
     (Kernel    : Kernel_Handle;
      File_Node : Code_Analysis.File_Access);
   --  Remove from the Locations view the uncovered lines of the given
   --  File_Node.
   --  Does nothing if the uncovered lines aren't listed there.

   procedure Clear_Subprogram_Locations
     (Kernel    : Kernel_Handle;
      File_Node : Code_Analysis.File_Access;
      Subp_Node : Subprogram_Access);
   --  Remove from the Locations view the uncovered lines of the given
   --  subp_node.
   --  Does nothing if the uncovered lines aren't listed there.

   procedure Add_File_Coverage_Annotations
     (Kernel    : Kernel_Handle;
      File_Node : Code_Analysis.File_Access);
   --  Add the coverage annotation columns to the corresponding src_editor.

   procedure Remove_File_Coverage_Annotations
     (Kernel    : Kernel_Handle;
      File_Node : Code_Analysis.File_Access);
   --  Removes coverage annotations of src_editor of the given file.

   procedure List_File_Uncovered_Lines
     (Kernel    : Kernel_Handle;
      File_Node : Code_Analysis.File_Access);
   --  Add to the Locations view the not covered lines of the given File_Node.

   procedure List_Project_Uncovered_Lines
     (Kernel       : Kernel_Handle;
      Project_Node : Project_Access);
   --  Add to the location view the not covered lines of the given Project.

   procedure Add_Project_Coverage_Annotations
     (Kernel : Kernel_Handle; Project_Node : Project_Access);
   --  Add coverage annotations of the src_editors of the files of the project.

   procedure Remove_Project_Coverage_Annotations
     (Kernel : Kernel_Handle; Project_Node : Project_Access);
   --  Removes coverage annotations from the src_editors of the project files.

   procedure List_Subprogram_Uncovered_Lines
     (Kernel    : Kernel_Handle;
      File_Node : Code_Analysis.File_Access;
      Subp_Node : Subprogram_Access);
   --  Add to the Locations view the not covered lines of the given Subprogram.

   procedure Show_All_Coverage_Information
     (Kernel   : Kernel_Handle;
      Projects : Code_Analysis_Tree);
   --  List uncovered lines and add coverage annotations for every projects of
   --  the given code analysis instance.

   procedure Hide_All_Coverage_Information
     (Kernel   : Kernel_Handle;
      Projects : Code_Analysis_Tree);
   --  Remove from the Locations view the listed uncovered lines of each files
   --  of each loaded projects.
   --  Does nothing if the lines are not listed in.
   --  Remove every coverage annotations of opened source file editors.

   function Find_Gcov_File
     (Kernel  : Kernel_Handle;
      Source  : VFS.Virtual_File) return VFS.Virtual_File;
   --  Return the gcov file associated with Source.

   function Have_Gcov_Info
     (Projects : Code_Analysis_Tree;
      Context  : Selection_Context) return Boolean;
   --  Verify that contextual Project and/or file if any, have associated
   --  coverage information in their corresponding node of the analysis tree.

end Coverage_GUI;
