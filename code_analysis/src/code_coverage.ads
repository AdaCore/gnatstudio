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

with Code_Analysis; use Code_Analysis;

with GNAT.OS_Lib; use GNAT.OS_Lib;

package Code_Coverage is

   function Get_Project_From_File (F_I : File_Id) return Project_Id;
   --  Currently returns a preset name of project

   procedure Add_Subprograms
     (F_A : Code_Analysis.File_Access; File_Contents : String_Access);
   --  Find subprograms in a gcov formated output file, and add them to the
   --  given File node.

   procedure Add_Lines
     (S_A : Subprogram_Access; File_Contents : String_Access);
   --  Find lines in a gcov formated output file, and add them to the
   --  given Subprogram node, with the corresponding coverage info.

   procedure Dump_Coverage (C_A : Coverage_Access);
   --  Currently dump to the standard output coverage informations stored
   --  in a Code_Analysis.Coverage record

   procedure Dump_Line_Coverage (C_A : Coverage_Access);
   --  Currently dump to the standard output coverage informations stored
   --  in a Code_Analysis.Coverage record for the Line type

end Code_Coverage;
