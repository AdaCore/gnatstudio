------------------------------------------------------------------------------
--                     XML/Ada - An XML suite for Ada95                     --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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

with Unicode.CES;        use Unicode.CES;
with Unicode.CES.Utf32;  use Unicode.CES.Utf32;
with Unicode.CES.Utf16;  use Unicode.CES.Utf16;
with Unicode.CES.Utf8;   use Unicode.CES.Utf8;
with GNATCOLL.Mmap;          use GNATCOLL.Mmap;
with GNAT.OS_Lib;        use GNAT.OS_Lib;

package body Input_Sources.Mmap is

   ----------
   -- Open --
   ----------

   procedure Open (Filename : Filesystem_String; Input : out Mmap_Input) is
      BOM    : Bom_Type;
   begin
      Input.File := Open_Read (+Filename);
      Read (Input.File);
      Input.Buffer := Data (Input.File);

      Read_Bom
        (String (Input.Buffer (1 .. Integer (Length (Input.File)))),
         Input.Prolog_Size, BOM);
      case BOM is
         when Utf32_LE =>
            Set_Encoding (Input, Utf32_LE_Encoding);
         when Utf32_BE =>
            Set_Encoding (Input, Utf32_BE_Encoding);
         when Utf16_LE =>
            Set_Encoding (Input, Utf16_LE_Encoding);
         when Utf16_BE =>
            Set_Encoding (Input, Utf16_BE_Encoding);
         when Ucs4_BE | Ucs4_LE | Ucs4_2143 | Ucs4_3412 =>
            raise Invalid_Encoding;
         when Utf8_All | Unknown =>
            Set_Encoding (Input, Utf8_Encoding);
      end case;

      Input.Index := 1 + Input.Prolog_Size;

      --  Base file name should be used as the public Id
      Set_Public_Id (Input, +Filename);
      Set_System_Id (Input, +Filename);
   end Open;

   -----------
   -- Close --
   -----------

   overriding procedure Close (Input : in out Mmap_Input) is
   begin
      Close (Input.File);
      Input_Sources.Close (Input_Source (Input));
      Input.Index := Natural'Last;
   end Close;

   ---------------
   -- Next_Char --
   ---------------

   overriding procedure Next_Char
     (From : in out Mmap_Input;
      C    : out Unicode.Unicode_Char) is
   begin
      From.Es.Read
        (String (From.Buffer (From.Index .. From.Index + 6)), From.Index, C);
      C := From.Cs.To_Unicode (C);
   end Next_Char;

   ---------
   -- Eof --
   ---------

   overriding function Eof (From : Mmap_Input) return Boolean is
   begin
      return GNATCOLL.Mmap.File_Size (From.Index) > Length (From.File);
   end Eof;

   -------------------
   -- Set_System_Id --
   -------------------

   overriding procedure Set_System_Id
     (Input : in out Mmap_Input; Id : Byte_Sequence) is
   begin
      if Is_Absolute_Path (Id) then
         Set_System_Id (Input_Source (Input), Id);
      else
         Set_System_Id
           (Input_Source (Input),
            Normalize_Pathname (Id, Resolve_Links => False));
      end if;
   end Set_System_Id;

end Input_Sources.Mmap;
