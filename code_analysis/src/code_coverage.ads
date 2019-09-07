------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2006-2019, AdaCore                     --
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

--  <description>
--  This package provides a user level code coverage API
--  </description>

with GNAT.Strings;                    use GNAT.Strings;

with GNATCOLL.Projects;               use GNATCOLL.Projects;

with Gtk.Tree_Store;                  use Gtk.Tree_Store;
with Gtk.Tree_Model;                  use Gtk.Tree_Model;

with Code_Analysis;                   use Code_Analysis;
with Language.Abstract_Language_Tree; use Language.Abstract_Language_Tree;
with XML_Utils;                       use XML_Utils;

package Code_Coverage is

   procedure Set_Error
     (File_Node  : Code_Analysis.File_Access;
      Error_Code : File_Coverage_Status);
   --  Sets a coverage data with Error_Code for Status to the given File_Node

   procedure Get_Runs_Info_From_File
     (File_Contents : GNAT.Strings.String_Access;
      Prj_Runs      : out Positive;
      Have_Runs     : out Boolean);
   --  Reads and returns in the given .gcov file contents the number of
   --  execution(s) of the binary file produced by the analyzed sources.
   --  This information is contained in every .gcov files.
   --  Have_Runs is set to False if this information is not found.

   procedure Add_Subprogram_Info
     (File_Node : Code_Analysis.File_Access;
      Tree      : not null access Semantic_Tree'Class);
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

   procedure XML_Dump_Coverage (Coverage : Coverage_Access; Loc : Node_Ptr);
   --  Add to Loc the coverage attributes that Coverage may contain
   --  This procedure handles all the Coverage_Access'Class

   procedure XML_Parse_Coverage
     (Coverage : in out Coverage_Access;
      Loc      : Node_Ptr);
   --  Get from Loc the coverage attributes that Coverage should contain
   --  This procedure handles all the Coverage_Access'Class

   function First_Project_With_Coverage_Data
     (Projects : Code_Analysis_Tree) return Project_Type;
   --  Return the 1st project that contains coverage data from the given
   --  analysis.
   --  Return No_Project if no project contains such data.

   procedure Fill_Iter
     (Tree_Store : Gtk_Tree_Store;
      Iter       : Gtk_Tree_Iter;
      Coverage   : Coverage_Access;
      Bin_Mode   : Boolean := False);
   --  Fill the Gtk_Tree_Store with the given coverage record
   --  If Bin_Mode is True, then the coverage messages will only be between
   --  (covered | not covered)

end Code_Coverage;
