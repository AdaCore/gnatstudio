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

package body Codefix is

   ------------
   -- Assign --
   ------------

   procedure Assign (This : in out String_Access; Value : String) is
      Garbage : String_Access := This;
      --  Used to prevent usage like 'Assign (Str, Str.all)'
   begin
      This := new String'(Value);
      Free (Garbage);
   end Assign;

   ------------
   -- Assign --
   ------------

   procedure Assign (This : in out String_Access; Value : String_Access) is
      Garbage : String_Access := This;
      --  Used to prevent usage like 'Assign (Str, Str)'
   begin
      This := Clone (Value);
      Free (Garbage);
   end Assign;

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line (File : File_Type; This : in out String_Access) is
      Len    : Natural;
      Buffer : String (1 .. 2048);
   begin
      --  We can't read lines longer than 2048 characters. Doesn't seem worth
      --  it anyway, since we are dealing with source files.
      Get_Line (File, Buffer, Len);
      Assign (This,  Buffer (1 .. Len));
   end Get_Line;

   -----------
   -- Clone --
   -----------

   function Clone (This : String_Access) return String_Access is
   begin
      if This = null then
         return null;
      else
         return new String'(This.all);
      end if;
   end Clone;

end Codefix;
