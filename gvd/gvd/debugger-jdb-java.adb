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
     (Lang     : Jdb_Java_Language;
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
     (Lang       : Jdb_Java_Language;
      Type_Str   : String;
      Index      : in out Natural;
      Result     : in out Generic_Values.Generic_Type_Access;
      Repeat_Num : out Positive)
   is
   begin
      raise Program_Error;
   end Parse_Value;

   ----------------------
   -- Parse_Array_Type --
   ----------------------

   procedure Parse_Array_Type
     (Lang      : Jdb_Java_Language;
      Type_Str  : String;
      Entity    : String;
      Index     : in out Natural;
      Result    : out Generic_Type_Access)
   is
   begin
      raise Program_Error;
   end Parse_Array_Type;

   -----------------------
   -- Parse_Record_Type --
   -----------------------

   procedure Parse_Record_Type
     (Lang      : Jdb_Java_Language;
      Type_Str  : String;
      Entity    : String;
      Index     : in out Natural;
      Result    : out Generic_Type_Access;
      End_On    : String)
   is
   begin
      raise Program_Error;
   end Parse_Record_Type;

   -----------------------
   -- Parse_Array_Value --
   -----------------------

   procedure Parse_Array_Value
     (Lang     : Jdb_Java_Language;
      Type_Str : String;
      Index    : in out Natural;
      Result   : in out Array_Type_Access)
   is
   begin
      raise Program_Error;
   end Parse_Array_Value;

end Debugger.Jdb.Java;
