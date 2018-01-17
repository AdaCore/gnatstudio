------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2008-2018, AdaCore                     --
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
