-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2007, AdaCore                   --
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
--  This package provides the subprograms needed by the XML dumping
--  of a Code_Analysis tree structure on the standard output.
--  </description>

with Glib.Xml_Int;  use Glib.Xml_Int;
with Code_Analysis; use Code_Analysis;
with Projects;      use Projects;

package Code_Analysis_XML is

   procedure Dump_XML
     (Projects : Code_Analysis_Tree;
      Parent   : Node_Ptr);
   --  Starts a dominos calling to the xml dumping subprograms
   --  following the Code_Analysis tree structure.

   procedure Parse_XML
     (Project  : Project_Type;
      Projects : Code_Analysis_Tree;
      Child    : in out Node_Ptr);
   --  Starts a dominos calling to the xml parsing subprograms
   --  to fill the Code_Analysis tree structure.

private

   procedure Dump_Project
     (Prj_Node : Project_Access;
      Parent   : Node_Ptr);
   --  Add to Parent an XML child nammed Project, and call Dump_File for each
   --  file it contains.

   procedure Parse_Project
     (Prj_Node : Project_Access;
      Parent   : Node_Ptr);
   --  Load from an XML child tagged "Project", the information needed to fill
   --  a Code_Analysis Project_Node.
   --  Then, call Parse_File on each XML children.

   procedure Dump_File
     (File_Node : Code_Analysis.File_Access; Parent : Node_Ptr);
   --  Add to Parent an XML child nammed File, and call Dump_File for each
   --  file it contains.

   procedure Parse_File
     (File_Node : Code_Analysis.File_Access;
      Parent    : Node_Ptr);
   --  Load from an XML child tagged "File", the information needed to fill
   --  a Code_Analysis File_Node, its Subprogram and Line nodes.

   procedure Dump_Subprogram
     (Subp_Node : Subprogram_Access; Parent : Node_Ptr);
   --  Add to Parent an XML child nammed Subprogram, and call Dump_File for
   --  each file it contains.

   procedure Dump_Line (Line_Node : Code_Analysis.Line; Parent : Node_Ptr);
   --  Add to Parent an XML child nammed Line, and call Dump_File for each
   --  file it contains.

end Code_Analysis_XML;
