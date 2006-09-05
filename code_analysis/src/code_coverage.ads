-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2006                         --
--                              AdaCore                              --
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

with Code_Analysis;   use Code_Analysis;

with GNAT.OS_Lib;     use GNAT.OS_Lib;
with Gtk.Tree_Store;  use Gtk.Tree_Store;
with Gtk.Tree_Model;  use Gtk.Tree_Model;

package Code_Coverage is

   procedure Read_Gcov_Info
     (File_Node     : File_Access;
      File_Contents : String_Access;
      Line_Count    : out Natural;
      Covered_Lines : out Natural);
   --  Parse the File_Contents and fill the File_Node with gcov info
   --  And set Line_Count and Covered_Lines

   procedure Compute_Project_Coverage (Project_Node : in out Project_Access);
   --  Compute the nodes coverage informations of the single given project from
   --  the coverage informations of its File children

   procedure Dump_Node_Coverage (Coverage : Coverage_Access);
   --  Currently dump to the standard output coverage information stored
   --  in a Code_Analysis.Coverage for the types before Line, ie the tree nodes

   procedure Dump_Line_Coverage (Coverage : Coverage_Access);
   --  Currently dump to the standard output coverage information stored
   --  in a Code_Analysis.Coverage record for the Line type

   procedure Dump_Subp_Coverage (Coverage : Coverage_Access);
   --  Currently dump to the standard output coverage information stored in a
   --  Code_Analysis.Coverage for the Subprogram nodes, ie with extra Called

   function Line_Coverage_Info (Coverage : Coverage_Access)
                                return String_Access;
   --  Returns a String_Access pointing on a message describing the coverage
   --  state of the line from which the Coverage record had been extracted

   procedure Fill_Iter
     (Tree_Store : in out Gtk_Tree_Store;
      Iter       : in out Gtk_Tree_Iter;
      Coverage   : Coverage_Access);
   --  Fill the Gtk_Tree_Store with the given coverage record

end Code_Coverage;
