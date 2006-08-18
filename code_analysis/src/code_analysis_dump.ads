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
--  This package provides the subprograms needed by the text dumping
--  of a Code_Analysis tree structure on the standard output
--  </description>

with Code_Analysis; use Code_Analysis;

package Code_Analysis_Dump is

   procedure Dump_Text (Projects : Code_Analysis_Tree);
   --  Starts a dominos calling to the text dumping subprograms
   --  following the Code_Analysis tree structure

   procedure Dump_Line (Line_Node : Line_Access);
   --  Dump in text format a Line ending-node by standard output

private

   procedure Dump_Project (Cursor : Project_Maps.Cursor);
   --  Dump in text format a Project node by standard output

   procedure Dump_File (Cursor : File_Maps.Cursor);
   --  Dump in text format a File node by standard output

   procedure Dump_Subprogram (Cursor : Subprogram_Maps.Cursor);
   --  Dump in text format a Subprogram node by standard output

end Code_Analysis_Dump;
