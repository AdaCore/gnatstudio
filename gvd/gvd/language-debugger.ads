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

--  The default implementation of some of these subprograms raises
--  Program_Error since they require a debugger session. However, since we
--  need to be able to declare some instances of Language_Debugger_Ada, for
--  instance, we need non-abstract subprograms.

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
      Result   : out Items.Generic_Type_Access);
   --  Parse the type of Entity.
   --  Type_Str should contain the type as returned by the debugger.
   --  Entity is used to get the type of the fields or array items.
   --
   --  The default implementation raises Program_Error (See comment above)

   procedure Parse_Value
     (Lang       : access Language_Debugger;
      Type_Str   : String;
      Index      : in out Natural;
      Result     : in out Items.Generic_Type_Access;
      Repeat_Num : out Positive);
   --  Parse the value of an entity, for the Ada language.
   --  Type_Str should contain the value, as returned by the debugger itself.
   --  Repeat_Num is the number of times the item is repeated in the output.
   --
   --  The default implementation raises Program_Error (See comment above)

   procedure Parse_Array_Type
     (Lang      : access Language_Debugger;
      Type_Str  : String;
      Entity    : String;
      Index     : in out Natural;
      Start_Of_Dim : in Natural;
      Result    : out Items.Generic_Type_Access);
   --  Parse the description of an array type.
   --  Index should point at the opening character of the array in Type_Str
   --  (ie "array " in gdb Ada, or "int [4]" in gdb C).
   --  Start_Of_Dim should point to the beginning of the definition of the
   --  dimensions ("[4]" in the above example)
   --
   --  The default implementation raises Program_Error (See comment above)

   procedure Parse_Record_Type
     (Lang      : access Language_Debugger;
      Type_Str  : String;
      Entity    : String;
      Index     : in out Natural;
      Is_Union  : Boolean;
      Result    : out Items.Generic_Type_Access;
      End_On    : String);
   --  Parse the type describing a record.
   --  Index should pointer after the initial "record ", and the record is
   --  assumed to end on a string like End_On.
   --  This function is also used to parse the variant part of a record.
   --  If Is_Union is True, then a union type is created instead of a record
   --  type.
   --
   --  The default implementation raises Program_Error (See comment above)

   procedure Parse_Array_Value
     (Lang     : access Language_Debugger;
      Type_Str : String;
      Index    : in out Natural;
      Result   : in out Items.Arrays.Array_Type_Access);
   --  Parse the value of an array.
   --
   --  The default implementation raises Program_Error (See comment above)

   function Set_Variable
     (Lang     : access Language_Debugger;
      Var_Name : String;
      Value    : String)
     return String;
   --  Return the command to use to set a variable, depending on a language
   --  specific language.

   type Language_Debugger_Context (Record_Field_Length : Positive) is record
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
   --  Language/Debugger specific syntax description.
   --  This describes the how variable types and values are output
   --  by the debugger, and these are used by the common parsing
   --  functions.
   --  Note that this is different from Language.Language_Context,
   --  since the fields depend on the debugger.

   function Get_Language_Debugger_Context
     (Lang : access Language_Debugger) return Language_Debugger_Context;
   --  Return the language/Debugger context.
   --  The default implementation raises Program_Error (See comment above)

private
   type Language_Debugger is abstract new Language_Root with record
      The_Debugger : Debugger_Access;
   end record;
end Language.Debugger;
