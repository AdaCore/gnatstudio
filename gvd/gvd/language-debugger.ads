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

with Debugger; use Debugger;
with Items;
with Items.Arrays;

package Language.Debugger is

   type Language_Debugger is abstract new Language_Root with private;
   type Language_Debugger_Access is access all Language_Debugger'Class;

   procedure Set_Debugger
     (The_Language : access Language_Debugger;
      The_Debugger : Debugger_Access);
   --  Set the debugger associated with a language.

   function Get_Debugger
     (The_Language : access Language_Debugger) return Debugger_Access;
   --  Return the debugger associate with a language.

   procedure Parse_Type
     (Lang     : access Language_Debugger;
      Type_Str : String;
      Entity   : String;
      Index    : in out Natural;
      Result   : out Items.Generic_Type_Access) is abstract;
   --  Parse the type of Entity.
   --  Type_Str should contain the type as returned by the debugger.
   --  Entity is used to get the type of the fields or array items.

   procedure Parse_Value
     (Lang       : access Language_Debugger;
      Type_Str   : String;
      Index      : in out Natural;
      Result     : in out Items.Generic_Type_Access;
      Repeat_Num : out Positive) is abstract;
   --  Parse the value of an entity, for the Ada language.
   --  Type_Str should contain the value, as returned by the debugger itself.
   --  Repeat_Num is the number of times the item is repeated in the output.

   procedure Parse_Array_Type
     (Lang      : access Language_Debugger;
      Type_Str  : String;
      Entity    : String;
      Index     : in out Natural;
      Start_Of_Dim : in Natural;
      Result    : out Items.Generic_Type_Access) is abstract;
   --  Parse the description of an array type.
   --  Index should point at the opening character of the array in Type_Str
   --  (ie "array " in gdb Ada, or "int [4]" in gdb C).
   --  Start_Of_Dim should point to the beginning of the definition of the
   --  dimensions ("[4]" in the above example)

   procedure Parse_Record_Type
     (Lang      : access Language_Debugger;
      Type_Str  : String;
      Entity    : String;
      Index     : in out Natural;
      Is_Union  : Boolean;
      Result    : out Items.Generic_Type_Access;
      End_On    : String) is abstract;
   --  Parse the type describing a record.
   --  Index should pointer after the initial "record ", and the record is
   --  assumed to end on a string like End_On.
   --  This function is also used to parse the variant part of a record.
   --  If Is_Union is True, then a union type is created instead of a record
   --  type.

   procedure Parse_Array_Value
     (Lang     : access Language_Debugger;
      Type_Str : String;
      Index    : in out Natural;
      Result   : in out Items.Arrays.Array_Type_Access) is abstract;
   --  Parse the value of an array.

   type Language_Context (Record_Field_Length : Positive) is record
      Record_Start : Character;
      --  Character that starts the display of record values

      Record_End   : Character;
      --  Character that ends the display of record values

      Array_Start  : Character;
      --  Character that starts the display of array values

      Array_End    : Character;
      --  Character that ends the display of array values

      Record_Field : String (1 .. Record_Field_Length);
      --  how are record field names separated from their values
   end record;
   --  Description of some strings used in the parsing of the output for
   --  some given languages.


   function Get_Language_Context (Lang : access Language_Debugger)
                                 return Language_Context is abstract;
   --  Return the context to use for a specific language

private
   type Language_Debugger is abstract new Language_Root with record
      The_Debugger : Debugger_Access;
   end record;
end Language.Debugger;
