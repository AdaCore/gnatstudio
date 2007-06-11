-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
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

with Language; use Language;

package Syntax_Diff is

   type Diff_Kind is (Added, Removed, Moved, Profile_Changed);

   type Result_Record;
   type Result_Link is access all Result_Record;
   type Result_Record (Kind : Diff_Kind) is record
      Construct     : Construct_Access;
      Category      : Language_Category;
      New_Construct : Construct_Access;
      Next          : Result_Link;
   end record;

   function Syntax_Diff
     (Old_Constructs, New_Constructs : Construct_List) return Result_Link;
   --  Build a list of syntax differences between Old_Construct and
   --  New_Construct

   procedure Print_Results (Results : Result_Link);
   --  Print the list of results to stdout

end Syntax_Diff;
