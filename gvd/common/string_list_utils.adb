-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2002                         --
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

package body String_List_Utils is

   use String_List;

   ----------------------
   -- Copy_String_List --
   ----------------------

   function Copy_String_List
     (S : in String_List.List) return String_List.List
   is
      Result : String_List.List;
      Temp   : List_Node := First (S);

   begin
      while Temp /= Null_Node loop
         Append (Result, Data (Temp));
         Temp := Next (Temp);
      end loop;

      return Result;
   end Copy_String_List;

   -----------------
   -- String_Free --
   -----------------

   procedure String_Free (S : in out String) is
      pragma Unreferenced (S);
   begin
      null;
   end String_Free;

end String_List_Utils;
