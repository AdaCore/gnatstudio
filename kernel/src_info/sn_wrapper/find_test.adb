-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2002                         --
--                            ACT-Europe                             --
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

with SN,
     SN.DB_Structures,
     SN.Find_Fns,
     DB_API,
     Ada.Text_IO,
     Ada.Strings.Unbounded;

use  SN,
     SN.DB_Structures,
     SN.Find_Fns,
     DB_API,
     Ada.Text_IO,
     Ada.Strings.Unbounded;

procedure Find_Test is
   DB  : DB_File;
   Tab : GV_Table;
begin
   Open (DB, "db/test.gv");
   Tab := Find (DB, "a");
   Put_Line ("Found: " & Tab.Buffer (Tab.Name.First .. Tab.Name.Last) & ", "
      & Tab.Buffer (Tab.File_Name.First .. Tab.File_Name.Last));
   Tab := Find (DB, "a", Point'(1, 4));
   Put_Line ("Found: " & Tab.Buffer (Tab.Name.First .. Tab.Name.Last) & ", "
      & Tab.Buffer (Tab.File_Name.First .. Tab.File_Name.Last));
   Tab := Find (DB, "c");
   Close (DB);
end Find_Test;
