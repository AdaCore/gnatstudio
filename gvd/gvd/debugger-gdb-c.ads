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

--  This package defines the object Gdb_C_Language that provides support
--  for the language C in Gdb.
--
--  See language.ads for a complete spec.

with Language.Debugger.C;
with Items.Arrays;

package Debugger.Gdb.C is

   type Gdb_C_Language is new
     Language.Debugger.C.C_Language with private;

   procedure Parse_Type
     (Lang     : access Gdb_C_Language;
      Type_Str : String;
      Entity   : String;
      Index    : in out Natural;
      Result   : out Items.Generic_Type_Access);

   procedure Parse_Value
     (Lang       : access Gdb_C_Language;
      Type_Str   : String;
      Index      : in out Natural;
      Result     : in out Items.Generic_Type_Access;
      Repeat_Num : out Positive);

   function Break_Exception
     (Debugger  : access Gdb_C_Language;
      Name      : String  := "";
      Unhandled : Boolean := False) return String;

   function Thread_List (Lang : access Gdb_C_Language) return String;

   function Thread_Switch
     (Lang   : access Gdb_C_Language;
      Thread : Natural) return String;

   function Parse_Thread_List
     (Lang   : access Gdb_C_Language;
      Output : String) return Language.Thread_Information_Array;

   procedure Parse_Array_Type
     (Lang      : access Gdb_C_Language;
      Type_Str  : String;
      Entity    : String;
      Index     : in out Natural;
      Start_Of_Dim : in Natural;
      Result    : out Items.Generic_Type_Access);

   procedure Parse_Record_Type
     (Lang      : access Gdb_C_Language;
      Type_Str  : String;
      Entity    : String;
      Index     : in out Natural;
      Is_Union  : Boolean;
      Result    : out Items.Generic_Type_Access;
      End_On    : String);
   --  End_On is ignored in the C implementation.

   procedure Parse_Array_Value
     (Lang     : access Gdb_C_Language;
      Type_Str : String;
      Index    : in out Natural;
      Result   : in out Items.Arrays.Array_Type_Access);

   function Set_Variable
     (Lang     : access Gdb_C_Language;
      Var_Name : String;
      Value    : String)
     return String;

   function Get_Language_Context
     (Lang : access Gdb_C_Language) return Language.Debugger.Language_Context;

private

   type Gdb_C_Language is new
     Language.Debugger.C.C_Language with null record;

end Debugger.Gdb.C;
