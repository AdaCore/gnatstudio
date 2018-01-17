------------------------------------------------------------------------------
--                                   GPS                                    --
--                                                                          --
--                     Copyright (C) 2000-2018, AdaCore                     --
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

with Debugger;             use Debugger;
with GVD.Variables.Types;

package Language.Debugger is

   type Language_Debugger is abstract new Language_Root with private;
   type Language_Debugger_Access is access all Language_Debugger'Class;

   overriding function Keywords
     (Lang : access Language_Debugger) return Strings.String_Access;
   --  Function used for all debugger languages as this is not used. This
   --  version always returns null.

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
      Result   : out GVD.Variables.Types.GVD_Type_Holder) is abstract;
   --  Parse the type of Entity.
   --  Type_Str should contain the type as returned by the debugger.
   --  Entity is used to get the type of the fields or array items.

   procedure Parse_Value
     (Lang       : access Language_Debugger;
      Type_Str   : String;
      Index      : in out Natural;
      Result     : in out GVD.Variables.Types.GVD_Type_Holder;
      Repeat_Num : out Positive) is abstract;
   --  Parse the value of an entity, for a given language.
   --  Type_Str should contain the value, as returned by the debugger itself.
   --  Repeat_Num is the number of times the item is repeated in the output.

   procedure Parse_Array_Type
     (Lang         : access Language_Debugger;
      Type_Str     : String;
      Entity       : String;
      Index        : in out Natural;
      Start_Of_Dim : Natural;
      Result       : out GVD.Variables.Types.GVD_Type_Holder) is abstract;
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
      Result    : out GVD.Variables.Types.GVD_Type_Holder;
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
      Result   : in out GVD.Variables.Types.GVD_Type_Holder) is abstract;
   --  Parse the value of an array.

   function Set_Variable
     (Lang     : access Language_Debugger;
      Var_Name : String;
      Value    : String) return String is abstract;
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
     (Lang : access Language_Debugger)
      return Language_Debugger_Context is abstract;
   --  Return the language/Debugger context.

private
   type Language_Debugger is abstract new Language_Root with record
      The_Debugger : Debugger_Access;
   end record;
end Language.Debugger;
