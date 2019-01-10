------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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
