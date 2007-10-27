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
--  This package provides a user level code coverage API
--  </description>

with GNAT.OS_Lib;            use GNAT.OS_Lib;
with Gtk.Tree_Store;         use Gtk.Tree_Store;
with Gtk.Tree_Model;         use Gtk.Tree_Model;
with Language.Tree.Database; use Language.Tree.Database;
with Code_Analysis;          use Code_Analysis;

package Code_Coverage is

   procedure Set_Error
     (File_Node  : Code_Analysis.File_Access;
      Error_Code : Coverage_Status);
   --  Sets a coverage data with Error_Code for Status to the given File_Node

   procedure Add_File_Info
     (File_Node     : Code_Analysis.File_Access;
      File_Contents : String_Access);
   --  Parse the File_Contents and fill the File_Node with gcov info
   --  And set Line_Count and Covered_Lines

   procedure Get_Runs_Info_From_File
     (File_Contents : String_Access;
      Prj_Runs      : out Positive;
      Have_Runs     : out Boolean);
   --  Reads and returns in the given .gcov file contents the number of
   --  execution(s) of the binary file produce by the analyzed sources
   --  This information is contained in every .gcov files
   --  Raise Bad_Gcov_File if this information is not found.

   procedure Add_Subprogram_Info
     (File_Node : Code_Analysis.File_Access;
      Data_File : Structured_File_Access);
   --  Add the subprogram nodes of the given file node, and compute it coverage
   --  information

   procedure Compute_Project_Coverage (Project_Node : Project_Access);
   --  Compute the node coverage information of the single given project from
   --  the coverage information of its File children

   procedure Dump_Node_Coverage (Coverage : Coverage_Access);
   --  Dump to the standard output coverage information stored in a
   --  Code_Analysis. Coverage of the types before Line, ie the tree nodes

   procedure Dump_Line_Coverage (Coverage : Coverage_Access);
   --  Dump to the standard output coverage information stored
   --  in a Code_Analysis. Coverage record of the Line type

   procedure Dump_Subp_Coverage (Coverage : Coverage_Access);
   --  Dump to the standard output coverage information stored in a
   --  Code_Analysis. Coverage of the Subprogram nodes, ie with extra Called

   procedure Dump_Prj_Coverage (Coverage : Coverage_Access);
   --  Dump to the standard output coverage information stored in a
   --  Code_Analysis. Coverage of the Project nodes, ie with extra Runs if any.

   function Line_Coverage_Info
     (Coverage : Coverage_Access;
      Bin_Mode : Boolean := False) return String_Access;
   --  Return a String_Access pointing on a message describing the coverage
   --  state of the line from which the Coverage record had been extracted
   --  If Bin_Mode is True, then the returned messages can only be between
   --  (covered | not covered)

   procedure Fill_Iter
     (Tree_Store : in out Gtk_Tree_Store;
      Iter       : in out Gtk_Tree_Iter;
      Coverage   : Coverage_Access;
      Bin_Mode   : Boolean := False);
   --  Fill the Gtk_Tree_Store with the given coverage record
   --  If Bin_Mode is True, then the coverage messages will only be between
   --  (covered | not covered)

end Code_Coverage;
