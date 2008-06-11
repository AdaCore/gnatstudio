-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2008, AdaCore                    --
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

--  This package implements a diff algorithm for strings.

package String_Diff is

   type Diff_State is (Equal, Added, Removed);
   --  See description in package Diffing

   type Diff_Callback is access procedure
     (Old_Obj, New_Obj : Character; State : Diff_State);
   --  See description in package Diffing

   procedure Diff
     (Old_String : String;
      New_String : String;
      Callback   : Diff_Callback);
   --  Compute a diff between Old_String and New_String.
   --  See description in package Diffing.

end String_Diff;
