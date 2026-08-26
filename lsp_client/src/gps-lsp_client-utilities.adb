------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2019-2026, AdaCore                  --
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

with Ada.Strings.Fixed;
with Interfaces;

with GPS.Kernel.Preferences;

with Basic_Types;
with Case_Handling;
with Language;         use Language;
with LSP.Enumerations; use LSP.Enumerations;
with URIs;

with VSS.Characters;
with VSS.JSON;
with VSS.JSON.Streams;
with VSS.Strings;
with VSS.Strings.Conversions;
with VSS.Unicode;

package body GPS.LSP_Client.Utilities is

   procedure Append_JSON
     (Result : in out LSP.Structures.LSPAny; Value : GNATCOLL.JSON.JSON_Value);
   --  Append the JSON events representing Value to Result.

   function UTF16_Encoded_Length
     (Item : VSS.Characters.Virtual_Character)
      return VSS.Unicode.UTF16_Code_Unit_Count;
   --  Returns length of the given Unicode character in UTF-16 encoding form
   --  in UTF-16 code units.

   function To_LSP_Keyword_Casing
     (Casing : Case_Handling.Casing_Type) return KeywordCasingKind;
   --  Convert GS casing preference into LSP keyword casing capability

   function To_LSP_Identifier_Casing
     (Casing : Case_Handling.Casing_Type) return IdentifierCasingKind;
   --  Convert GS casing preference into LSP identifier casing capability

   ------------
   -- To_URI --
   ------------

   function To_URI
     (Item : GNATCOLL.VFS.Virtual_File) return LSP.Structures.DocumentUri is
   begin
      return
        (VSS.Strings.Conversions.To_Virtual_String
           (URIs.Conversions.From_File (Item.Display_Full_Name))
         with null record);
   end To_URI;

   ---------------------
   -- To_Virtual_File --
   ---------------------

   function To_Virtual_File
     (Item : LSP.Structures.DocumentUri) return GNATCOLL.VFS.Virtual_File
   is
      File : constant String :=
        URIs.Conversions.To_File
          (VSS.Strings.Conversions.To_UTF_8_String
             (VSS.Strings.Virtual_String (Item)),
           Normalize => not GPS.Kernel.Preferences.Trusted_Mode.Get_Pref);
   begin
      --  Call Create_From_UTF8 to guess the proper filesystem encoding
      --  from the UTF8 string returned by the protocol.
      return GNATCOLL.VFS.Create_From_UTF8 (File);
   end To_Virtual_File;

   ------------------------------
   -- LSP_Position_To_Location --
   ------------------------------

   function LSP_Position_To_Location
     (Editor   : GPS.Editors.Editor_Buffer'Class;
      Position : LSP.Structures.Position)
      return GPS.Editors.Editor_Location'Class
   is
      use type VSS.Unicode.UTF16_Code_Unit_Offset;

      U16 : VSS.Unicode.UTF16_Code_Unit_Count := 0;
      C   : VSS.Characters.Virtual_Character'Base;

   begin
      return
         Result : GPS.Editors.Editor_Location'Class :=
           Editor.New_Location_At_Line
             (Basic_Types.Editable_Line_Type (Position.line + 1))
      do
         loop
            exit when
              U16 >= VSS.Unicode.UTF16_Code_Unit_Count (Position.character);

            C := Result.Get_Char;
            Result.Forward_Character;
            U16 := @ + UTF16_Encoded_Length (C);
         end loop;
      end return;
   end LSP_Position_To_Location;

   ------------------------------
   -- Location_To_LSP_Position --
   ------------------------------

   function Location_To_LSP_Position
     (Location : GPS.Editors.Editor_Location'Class)
      return LSP.Structures.Position
   is
      use type VSS.Unicode.UTF16_Code_Unit_Offset;

   begin
      if Location.Line = 0 then
         raise Program_Error
           with "Special lines can't be converted to LSP.Structures.Position";
      else
         declare
            U16 : VSS.Unicode.UTF16_Code_Unit_Count := 0;
            C   : VSS.Characters.Virtual_Character'Base;
            Aux : GPS.Editors.Editor_Location'Class :=
              Location.Buffer.New_Location_At_Line
                (Basic_Types.Editable_Line_Type (Location.Line));

         begin
            loop
               exit when Aux.Line_Offset = Location.Line_Offset;

               C := Aux.Get_Char;
               Aux.Forward_Character;
               U16 := @ + UTF16_Encoded_Length (C);
            end loop;

            return
              (line      => Natural (Location.Line - 1),
               character => Natural (U16));
         end;
      end if;
   end Location_To_LSP_Position;

   --------------------------
   -- To_Language_Category --
   --------------------------

   function To_Language_Category
     (K : LSP.Enumerations.SymbolKind; Is_Procedure : Boolean := False)
      return Language.Language_Category is

   begin
      case K is
         when Module                                       =>
            return Cat_Package;

         when Namespace                                    =>
            return Cat_With;

         when A_Package                                    =>
            return Cat_Package;

         when Class | Enum | An_Interface                  =>
            return Cat_Type;

         when Struct                                       =>
            return Cat_Structure;

         when Method                                       =>
            return Cat_Function;

         when A_Function                                   =>
            return (if Is_Procedure then Cat_Procedure else Cat_Function);

         when Property                                     =>
            return Cat_Pragma;

         when Field                                        =>
            return Cat_Field;

         when Constructor                                  =>
            return Cat_Constructor;

         when A_Constant                                   =>
            return Cat_Constant;

         when Variable | LSP.Enumerations.String .. Object =>
            return Cat_Variable;

         when others                                       =>
            return Cat_Unknown;
      end case;
   end To_Language_Category;

   -----------------------------
   -- To_Construct_Visibility --
   -----------------------------

   function To_Construct_Visibility
     (V : LSP.Enumerations.AlsVisibility) return Language.Construct_Visibility
   is
   begin
      case V is
         when Als_Public    =>
            return Visibility_Public;

         when Als_Protected =>
            return Visibility_Protected;

         when Als_Private   =>
            return Visibility_Private;
      end case;
   end To_Construct_Visibility;

   ----------------------------
   -- Get_Formatting_Options --
   ----------------------------

   function Get_Formatting_Options
     (Kernel : GPS.Kernel.Kernel_Handle; File : GNATCOLL.VFS.Virtual_File)
      return LSP.Structures.FormattingOptions
   is
      use GPS.Kernel.Preferences;
      Lang         : constant Language.Language_Access :=
        Kernel.Get_Language_Handler.Get_Language_From_File (File);
      Params       : Indent_Parameters;
      Indent_Style : Indentation_Kind;
      --  Max line include the last character for GNATformat thus the -1
      Max_Line     : constant Integer := Highlight_Column.Get_Pref - 1;
   begin
      Get_Indentation_Parameters (Lang, Params, Indent_Style);
      return
        LSP.Structures.FormattingOptions'
          (tabSize                          => Params.Indent_Level,
           insertSpaces                     => not Params.Use_Tabs,
           trimTrailingWhitespace           =>
             (Is_Set => True, Value => Strip_Blanks.Get_Pref /= Never),
           insertFinalNewline               =>
             (Is_Set => True, Value => False),
           trimFinalNewlines                =>
             (Is_Set => True, Value => Strip_Lines.Get_Pref /= Never),
           gnatKeywordCasing                =>
             (Is_Set => True,
              Value  => To_LSP_Keyword_Casing (Params.Reserved_Casing)),
           gnatIdentifierCasing             =>
             (Is_Set => True,
              Value  => To_LSP_Identifier_Casing (Params.Identifier_Casing)),
           gnatFormatMaxSize                =>
             (Is_Set => True, Value => Max_Line),
           gnatFormatContinuationLineIndent =>
             (Is_Set => True, Value => Params.Indent_Continue));
   end Get_Formatting_Options;

   --------------------------
   -- UTF16_Encoded_Length --
   --------------------------

   function UTF16_Encoded_Length
     (Item : VSS.Characters.Virtual_Character)
      return VSS.Unicode.UTF16_Code_Unit_Count
   is
      use type VSS.Unicode.Code_Point;

   begin
      return
        (if VSS.Characters.Virtual_Character'Pos (Item)
           <= VSS.Unicode.Code_Point (VSS.Unicode.UTF16_Code_Unit'Last)
         then 1
         else 2);
   end UTF16_Encoded_Length;

   ---------------------------
   -- To_LSP_Keyword_Casing --
   ---------------------------

   function To_LSP_Keyword_Casing
     (Casing : Case_Handling.Casing_Type) return KeywordCasingKind is
   begin
      case Casing is
         when Case_Handling.Upper =>
            return LSP.Enumerations.Upper;

         when Case_Handling.Lower =>
            return LSP.Enumerations.Lower;

         when others              =>
            return LSP.Enumerations.Keep;
      end case;
   end To_LSP_Keyword_Casing;

   ------------------------------
   -- To_LSP_Identifier_Casing --
   ------------------------------

   function To_LSP_Identifier_Casing
     (Casing : Case_Handling.Casing_Type) return IdentifierCasingKind is
   begin
      case Casing is
         when Case_Handling.Upper =>
            return LSP.Enumerations.Upper;

         when Case_Handling.Lower =>
            return LSP.Enumerations.Lower;

         when Case_Handling.Mixed =>
            return LSP.Enumerations.Mixed;

         when others              =>
            return LSP.Enumerations.Keep;
      end case;
   end To_LSP_Identifier_Casing;

   -----------------
   -- Append_JSON --
   -----------------

   procedure Append_JSON
     (Result : in out LSP.Structures.LSPAny; Value : GNATCOLL.JSON.JSON_Value)
   is
      use type GNATCOLL.JSON.JSON_Value_Type;

      procedure On_Field (Name : String; Field : GNATCOLL.JSON.JSON_Value);

      --------------
      -- On_Field --
      --------------

      procedure On_Field (Name : String; Field : GNATCOLL.JSON.JSON_Value) is
      begin
         Result.Append
           (VSS.JSON.Streams.JSON_Stream_Element'
              (Kind     => VSS.JSON.Streams.Key_Name,
               Key_Name => VSS.Strings.Conversions.To_Virtual_String (Name)));
         Append_JSON (Result, Field);
      end On_Field;

   begin
      case Value.Kind is
         when GNATCOLL.JSON.JSON_Null_Type    =>
            Result.Append
              (VSS.JSON.Streams.JSON_Stream_Element'
                 (Kind => VSS.JSON.Streams.Null_Value));

         when GNATCOLL.JSON.JSON_Boolean_Type =>
            Result.Append
              (VSS.JSON.Streams.JSON_Stream_Element'
                 (Kind          => VSS.JSON.Streams.Boolean_Value,
                  Boolean_Value => GNATCOLL.JSON.Get (Value)));

         when GNATCOLL.JSON.JSON_Int_Type     =>
            declare
               V : constant Long_Long_Integer := GNATCOLL.JSON.Get (Value);
               S : constant String :=
                 Ada.Strings.Fixed.Trim
                   (Long_Long_Integer'Image (V), Ada.Strings.Left);

            begin
               Result.Append
                 (VSS.JSON.Streams.JSON_Stream_Element'
                    (Kind         => VSS.JSON.Streams.Number_Value,
                     Number_Value =>
                       (Kind          => VSS.JSON.JSON_Integer,
                        String_Value  =>
                          VSS.Strings.Conversions.To_Virtual_String (S),
                        Integer_Value => Interfaces.Integer_64 (V))));
            end;

         when GNATCOLL.JSON.JSON_Float_Type   =>
            declare
               V : constant Float := GNATCOLL.JSON.Get (Value);
               S : constant String :=
                 Ada.Strings.Fixed.Trim (Float'Image (V), Ada.Strings.Left);

            begin
               Result.Append
                 (VSS.JSON.Streams.JSON_Stream_Element'
                    (Kind         => VSS.JSON.Streams.Number_Value,
                     Number_Value =>
                       (Kind         => VSS.JSON.JSON_Float,
                        String_Value =>
                          VSS.Strings.Conversions.To_Virtual_String (S),
                        Float_Value  => Interfaces.IEEE_Float_64 (V))));
            end;

         when GNATCOLL.JSON.JSON_String_Type  =>
            Result.Append
              (VSS.JSON.Streams.JSON_Stream_Element'
                 (Kind         => VSS.JSON.Streams.String_Value,
                  String_Value =>
                    VSS.Strings.Conversions.To_Virtual_String
                      (String'(GNATCOLL.JSON.Get (Value)))));

         when GNATCOLL.JSON.JSON_Array_Type   =>
            declare
               Arr : constant GNATCOLL.JSON.JSON_Array :=
                 GNATCOLL.JSON.Get (Value);

            begin
               Result.Append
                 (VSS.JSON.Streams.JSON_Stream_Element'
                    (Kind => VSS.JSON.Streams.Start_Array));

               for J in 1 .. GNATCOLL.JSON.Length (Arr) loop
                  Append_JSON (Result, GNATCOLL.JSON.Get (Arr, J));
               end loop;

               Result.Append
                 (VSS.JSON.Streams.JSON_Stream_Element'
                    (Kind => VSS.JSON.Streams.End_Array));
            end;

         when GNATCOLL.JSON.JSON_Object_Type  =>
            Result.Append
              (VSS.JSON.Streams.JSON_Stream_Element'
                 (Kind => VSS.JSON.Streams.Start_Object));

            GNATCOLL.JSON.Map_JSON_Object (Value, On_Field'Access);

            Result.Append
              (VSS.JSON.Streams.JSON_Stream_Element'
                 (Kind => VSS.JSON.Streams.End_Object));
      end case;
   end Append_JSON;

   ----------------
   -- To_LSP_Any --
   ----------------

   function To_LSP_Any
     (Value : GNATCOLL.JSON.JSON_Value) return LSP.Structures.LSPAny
   is
      Result : LSP.Structures.LSPAny;

   begin
      Append_JSON (Result, Value);

      return Result;
   end To_LSP_Any;

   ------------------
   -- From_LSP_Any --
   ------------------

   function From_LSP_Any
     (Value : LSP.Structures.LSPAny) return GNATCOLL.JSON.JSON_Value
   is
      Index : Positive := 1;

      function Build return GNATCOLL.JSON.JSON_Value;
      --  Consume one value starting at Value (Index), advancing Index past
      --  it, and return the corresponding GNATCOLL.JSON value.

      -----------
      -- Build --
      -----------

      function Build return GNATCOLL.JSON.JSON_Value is
         use type VSS.JSON.Streams.JSON_Stream_Element_Kind;

         Item : constant VSS.JSON.Streams.JSON_Stream_Element := Value (Index);

      begin
         Index := Index + 1;

         case Item.Kind is
            when VSS.JSON.Streams.Null_Value    =>
               return GNATCOLL.JSON.Create;

            when VSS.JSON.Streams.Boolean_Value =>
               return GNATCOLL.JSON.Create (Item.Boolean_Value);

            when VSS.JSON.Streams.Number_Value  =>
               case Item.Number_Value.Kind is
                  when VSS.JSON.JSON_Integer =>
                     return
                       GNATCOLL.JSON.Create
                         (Long_Long_Integer (Item.Number_Value.Integer_Value));

                  when others                =>
                     return
                       GNATCOLL.JSON.Create
                         (Float (Item.Number_Value.Float_Value));
               end case;

            when VSS.JSON.Streams.String_Value  =>
               return
                 GNATCOLL.JSON.Create
                   (VSS.Strings.Conversions.To_UTF_8_String
                      (Item.String_Value));

            when VSS.JSON.Streams.Start_Array   =>
               declare
                  Result : GNATCOLL.JSON.JSON_Array;

               begin
                  while Value (Index).Kind /= VSS.JSON.Streams.End_Array loop
                     Result.Append (Build);
                  end loop;

                  Index := Index + 1;

                  return GNATCOLL.JSON.Create (Result);
               end;

            when VSS.JSON.Streams.Start_Object  =>
               declare
                  Result : constant GNATCOLL.JSON.JSON_Value :=
                    GNATCOLL.JSON.Create_Object;

               begin
                  while Value (Index).Kind /= VSS.JSON.Streams.End_Object loop
                     declare
                        Key : constant VSS.Strings.Virtual_String :=
                          Value (Index).Key_Name;

                     begin
                        Index := Index + 1;
                        Result.Set_Field
                          (VSS.Strings.Conversions.To_UTF_8_String (Key),
                           Build);
                     end;
                  end loop;

                  Index := Index + 1;

                  return Result;
               end;

            when others                         =>
               return GNATCOLL.JSON.Create;
         end case;
      end Build;

   begin
      if Value.Is_Empty then
         return GNATCOLL.JSON.Create;
      end if;

      return Build;
   end From_LSP_Any;

end GPS.LSP_Client.Utilities;
