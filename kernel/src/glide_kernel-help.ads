-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package provides generic Help facilities.

package Glide_Kernel.Help is

   function Search
     (Kernel         : access Kernel_Handle_Record'Class;
      Text           : String;
      Case_Sensitive : Boolean := True;
      Forward        : Boolean := True;
      Regular        : Boolean := False) return Boolean;
   --  Search in the current help file the first occurrence of Text.
   --  Return True if Text was found.

   function Search_Next
     (Kernel : access Kernel_Handle_Record'Class) return Boolean;
   --  Search in the current help file the next occurrence.

   procedure Register_Module;
   --  Register the module in the list

end Glide_Kernel.Help;
