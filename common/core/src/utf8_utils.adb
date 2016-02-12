------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2016, AdaCore                     --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Decode_UTF8_String;
with Config;

package body UTF8_Utils is

   Locale_To_UTF_8 : Iconv_T;
   UTF_8_To_Locale : Iconv_T;
   Latin1_To_UTF_8 : Iconv_T;
   UTF_8_To_UTF_32 : Iconv_T;
   Is_Opened       : Boolean := False;

   procedure Open;
   --  Initialize internal data if not yet initialized

   ----------
   -- Open --
   ----------

   procedure Open is
   begin
      if not Is_Opened then
         Locale_To_UTF_8 := Iconv_Open
           (To_Code => UTF8, From_Code => Config.Default_Charset);
         UTF_8_To_Locale := Iconv_Open
           (From_Code => UTF8, To_Code => Config.Default_Charset);
         Latin1_To_UTF_8 := Iconv_Open
           (From_Code => ISO_8859_1, To_Code => UTF8);
         UTF_8_To_UTF_32 := Iconv_Open
           (From_Code => UTF8, To_Code => UTF32);
         Is_Opened := True;
      end if;
   end Open;

   ---------------------
   -- Unknown_To_UTF8 --
   ---------------------

   procedure Unknown_To_UTF8
     (Input   : String;
      Output  : out GNAT.Strings.String_Access;
      Success : out Boolean) is
   begin
      Open;
      Output := null;
      Success := True;

      --  First check if the string is already UTF-8
      if Validate (UTF_8_To_UTF_32, Input) then
         --  The string is UTF-8, nothing to do
         return;
      end if;

      --  The string is not valid UTF-8, assume it is encoded using the locale.

      if Validate (Locale_To_UTF_8, Input) then
         Output := new String'(Iconv (Locale_To_UTF_8, Input));
      else
         Success := False;
      end if;
   end Unknown_To_UTF8;

   function Unknown_To_UTF8
     (Input   : String;
      Success : access Boolean) return UTF8_String
   is
      Output : GNAT.Strings.String_Access;
   begin

      Unknown_To_UTF8 (Input, Output, Success.all);

      if Success.all then
         if Output = null then
            return Input;
         else
            declare
               S : constant String := Output.all;
            begin
               Free (Output);
               return S;
            end;
         end if;

      else
         return "";
      end if;
   end Unknown_To_UTF8;

   function Unknown_To_UTF8
     (Input : String) return UTF8_String
   is
      Success : aliased Boolean;
      S       : constant String := Unknown_To_UTF8 (Input, Success'Access);
   begin
      if Success then
         return S;
      else
         return "<could not convert to UTF8>";
      end if;
   end Unknown_To_UTF8;

   --------------------
   -- UTF8_To_Locale --
   --------------------

   function UTF8_To_Locale (Input : UTF8_String) return String is
   begin
      Open;
      return Iconv (UTF_8_To_Locale, Input);
   end UTF8_To_Locale;

   --------------------
   -- Locale_To_UTF8 --
   --------------------

   function Locale_To_UTF8 (Input : String) return UTF8_String is
   begin
      Open;
      return Iconv (Locale_To_UTF_8, Input);
   end Locale_To_UTF8;

   -------------------
   -- UTF8_Get_Char --
   -------------------

   function UTF8_Get_Char (Input : UTF8_String) return Wide_Wide_Character is
      Next   : Positive := Input'First;
      Result : Wide_Wide_Character;
   begin
      GNAT.Decode_UTF8_String.Decode_Wide_Wide_Character
        (Input, Next, Result);
      return Result;
   end UTF8_Get_Char;

   --------------------
   -- UTF8_Next_Char --
   --------------------

   function UTF8_Next_Char
     (Str : UTF8_String; Index : Positive) return Positive
   is
      Byte : constant Character := Str (Index);
   begin
      case Byte is
         when Character'Val (16#C0#) .. Character'Val (16#DF#) =>
            return Index + 2;
         when Character'Val (16#E0#) .. Character'Val (16#EF#) =>
            return Index + 3;
         when Character'Val (16#F0#) .. Character'Val (16#F7#) =>
            return Index + 4;
         when Character'Val (16#F8#) .. Character'Val (16#FB#) =>
            return Index + 5;
         when Character'Val (16#FC#) .. Character'Val (16#FD#) =>
            return Index + 6;
         when others =>
            return Index + 1;
      end case;
   end UTF8_Next_Char;

   --------------------
   -- UTF8_Next_Char --
   --------------------

   function UTF8_Next_Char
     (Str : UTF8_Unbounded_String; Index : Positive) return Positive
   is
      Byte : constant Character := Element (Str, Index);
   begin
      case Byte is
         when Character'Val (16#C0#) .. Character'Val (16#DF#) =>
            return Index + 2;
         when Character'Val (16#E0#) .. Character'Val (16#EF#) =>
            return Index + 3;
         when Character'Val (16#F0#) .. Character'Val (16#F7#) =>
            return Index + 4;
         when Character'Val (16#F8#) .. Character'Val (16#FB#) =>
            return Index + 5;
         when Character'Val (16#FC#) .. Character'Val (16#FD#) =>
            return Index + 6;
         when others =>
            return Index + 1;
      end case;
   end UTF8_Next_Char;

   --------------------
   -- UTF8_Prev_Char --
   --------------------

   function UTF8_Prev_Char
     (Str : UTF8_String; Index : Natural) return Natural
   is
      Result : Integer := Index - 1;
   begin
      if Index not in Str'Range then
         return Index;
      end if;

      while Result in Str'Range and then
        Str (Result) in Character'Val (16#80#) .. Character'Val (16#BF#)
      loop
         Result := Result - 1;
      end loop;

      return Result;
   end UTF8_Prev_Char;

   ---------------------
   -- Latin_1_To_UTF8 --
   ---------------------

   function Latin_1_To_UTF8 (Input : String) return UTF8_String is
   begin
      Open;
      return Iconv (Latin1_To_UTF_8, Input);
   end Latin_1_To_UTF8;

   --------------
   -- Validate --
   --------------

   function Validate
     (Object : Iconv_T; Input : Byte_Sequence) return Boolean
   is
      Output       : Byte_Sequence (1 .. 4096);
      Input_Index  : Positive := Input'First;
      Output_Index : Positive := Output'First;
      Result       : Iconv_Result;
   begin
      if Input = "" then
         return True;
      end if;

      loop
         Iconv (Object, Input, Input_Index, Output, Output_Index, Result);

         case Result is
            when Invalid_Multibyte_Sequence | Incomplete_Multibyte_Sequence =>
               return False;
            when Success =>
               return True;
            when Full_Buffer =>
               --  Continue convertion by rewriting output buffer
               Output_Index := Output'First;
         end case;
      end loop;
   end Validate;

   --------------------
   -- Validate_UTF_8 --
   --------------------

   function Validate_UTF_8 (Input : Byte_Sequence) return Boolean is
   begin
      Open;

      return Validate (UTF_8_To_UTF_32, Input);
   end Validate_UTF_8;

   ---------------------
   -- Column_To_Index --
   ---------------------

   function Column_To_Index
     (Buffer : UTF8_String; Column : Character_Offset_Type) return Natural
   is
      Result : Positive := Buffer'First;
   begin
      if Column <= 0 then
         return 0;
      end if;

      for J in 2 .. Column  loop
         Result := UTF8_Next_Char (Buffer, Result);
      end loop;

      return Result;
   end Column_To_Index;

   -----------------
   -- UTF8_Length --
   -----------------

   function UTF8_Length (Item : UTF8_String) return Natural is
      Result : Natural := 0;
      Index  : Positive := Item'First;

   begin
      while Index <= Item'Last loop
         Result := Result + 1;
         Index := UTF8_Next_Char (Item, Index);
      end loop;

      return Result;
   end UTF8_Length;

end UTF8_Utils;
