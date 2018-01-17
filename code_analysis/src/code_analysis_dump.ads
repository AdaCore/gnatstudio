------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2006-2018, AdaCore                     --
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
--  This package provides the subprograms needed by the text dumping
--  of a Code_Analysis tree structure on the standard output
--  </description>

with Code_Analysis; use Code_Analysis;

package Code_Analysis_Dump is

   procedure Dump_Text (Projects : Code_Analysis_Tree);
   --  Starts a dominos calling to the text dumping subprograms
   --  following the Code_Analysis tree structure

   procedure Dump_Line (Line_Node : Line);
   --  Dump in text format a Line ending-node by standard output

private

   procedure Dump_Project (Cursor : Project_Maps.Cursor);
   --  Dump in text format a Project node by standard output

   procedure Dump_File (Cursor : File_Maps.Cursor);
   --  Dump in text format a File node by standard output

   procedure Dump_Subprogram (Cursor : Subprogram_Maps.Cursor);
   --  Dump in text format a Subprogram node by standard output

end Code_Analysis_Dump;
