-----------------------------------------------------------------------
--                 Odd - The Other Display Debugger                  --
--                                                                   --
--                         Copyright (C) 2000                        --
--                 Emmanuel Briot and Arnaud Charlet                 --
--                                                                   --
-- Odd is free  software;  you can redistribute it and/or modify  it --
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

package body Debugger.Jdb.Java is

   use Language;

   ----------------
   -- Parse_Type --
   ----------------

   procedure Parse_Type
     (Lang     : access Jdb_Java_Language;
      Type_Str : String;
      Entity   : String;
      Index    : in out Natural;
      Result   : out Generic_Type_Access) is
   begin
      raise Unexpected_Type;
   end Parse_Type;

   -----------------
   -- Parse_Value --
   -----------------

   procedure Parse_Value
     (Lang       : access Jdb_Java_Language;
      Type_Str   : String;
      Index      : in out Natural;
      Result     : in out Generic_Values.Generic_Type_Access;
      Repeat_Num : out Positive)
   is
   begin
      raise Program_Error;
   end Parse_Value;

   -----------------
   -- Thread_List --
   -----------------

   function Thread_List (Lang : access Jdb_Java_Language) return String is
   begin
      return "threads";
   end Thread_List;
   
   -----------------------
   -- Parse_Thread_List --
   -----------------------

   function Parse_Thread_List
     (Lang   : access Jdb_Java_Language;
      Output : String) return Thread_Information_Array
   is
      Result : Thread_Information_Array (1 .. 0);
   begin
      return Result;
   end Parse_Thread_List;

end Debugger.Jdb.Java;
