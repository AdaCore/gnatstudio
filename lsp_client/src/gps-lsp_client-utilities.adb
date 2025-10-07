------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2019-2025, AdaCore                  --
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

with VSS.Strings.Conversions;

with URIs;
with GPS.Kernel.Preferences;

with Language;     use Language;
with LSP.Messages; use LSP.Messages;
with LSP.Types;

with VSS.Characters;
with VSS.Unicode;

package body GPS.LSP_Client.Utilities is

   function UTF16_Encoded_Length
     (Item : VSS.Characters.Virtual_Character)
      return VSS.Unicode.UTF16_Code_Unit_Count;
   --  Returns length of the given Unicode character in UTF-16 encoding form
   --  in UTF-16 code units.

   ------------
   -- To_URI --
   ------------

   function To_URI
     (Item : GNATCOLL.VFS.Virtual_File) return LSP.Messages.DocumentUri is
   begin
      return
        LSP.Types.To_LSP_URI
          (VSS.Strings.Conversions.To_Virtual_String
             (URIs.Conversions.From_File (Item.Display_Full_Name)));
   end To_URI;

   ---------------------
   -- To_Virtual_File --
   ---------------------

   function To_Virtual_File
     (Item : LSP.Messages.DocumentUri) return GNATCOLL.VFS.Virtual_File
   is
      File : constant String := URIs.Conversions.To_File
        (LSP.Types.To_UTF_8_String (Item),
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
      Position : LSP.Messages.Position)
      return GPS.Editors.Editor_Location'Class
   is
      use type LSP.Types.Line_Number;
      use type VSS.Unicode.UTF16_Code_Unit_Offset;

      U16 : VSS.Unicode.UTF16_Code_Unit_Count := 0;
      C   : VSS.Characters.Virtual_Character'Base;

   begin
      return Result : GPS.Editors.Editor_Location'Class :=
        Editor.New_Location_At_Line (Natural (Position.line + 1))
      do
         loop
            exit when U16 >= Position.character;

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
      return LSP.Messages.Position
   is
      use type VSS.Unicode.UTF16_Code_Unit_Offset;

   begin
      if Location.Line = 0 then
         raise Program_Error
           with "Special lines can't be converted to LSP.Messages.Position";
      else
         declare
            U16 : VSS.Unicode.UTF16_Code_Unit_Count := 0;
            C   : VSS.Characters.Virtual_Character'Base;
            Aux : GPS.Editors.Editor_Location'Class :=
              Location.Buffer.New_Location_At_Line
                (Natural (Location.Line));

         begin
            loop
               exit when Aux.Line_Offset = Location.Line_Offset;

               C := Aux.Get_Char;
               Aux.Forward_Character;
               U16 := @ + UTF16_Encoded_Length (C);
            end loop;

            return
              (line      => LSP.Types.Line_Number (Location.Line - 1),
               character => LSP.Types.UTF_16_Index (U16));
         end;
      end if;
   end Location_To_LSP_Position;

   --------------------------
   -- To_Language_Category --
   --------------------------

   function To_Language_Category
     (K            : LSP.Messages.SymbolKind;
      Is_Procedure : Boolean := False)
      return Language.Language_Category
   is

   begin
      case K is
         when Module                        => return Cat_Package;
         when Namespace                     => return Cat_With;
         when A_Package                     => return Cat_Package;
         when Class | Enum | An_Interface   => return Cat_Type;
         when Struct                        => return Cat_Structure;
         when Method                        => return Cat_Function;
         when A_Function                    => return (if Is_Procedure
                                                       then Cat_Procedure
                                                       else Cat_Function);
         when Property                      => return Cat_Pragma;
         when Field                         => return Cat_Field;
         when Constructor                   => return Cat_Constructor;
         when A_Constant                    => return Cat_Constant;
         when Variable |
              LSP.Messages.String .. Object => return Cat_Variable;
         when others                        => return Cat_Unknown;
      end case;
   end To_Language_Category;

   -----------------------------
   -- To_Construct_Visibility --
   -----------------------------

   function To_Construct_Visibility
     (V : LSP.Messages.Als_Visibility)
      return Language.Construct_Visibility
   is
   begin
      case V is
         when Als_Public    => return Visibility_Public;
         when Als_Protected => return Visibility_Protected;
         when Als_Private   => return Visibility_Private;
      end case;
   end To_Construct_Visibility;

   ----------------------------
   -- Get_Formatting_Options --
   ----------------------------

   function Get_Formatting_Options
     (Kernel : GPS.Kernel.Kernel_Handle;
      File   : GNATCOLL.VFS.Virtual_File)
      return LSP.Messages.FormattingOptions
   is
      use GPS.Kernel.Preferences;
      Lang         : constant Language.Language_Access :=
        Kernel.Get_Language_Handler.Get_Language_From_File (File);
      Params       : Indent_Parameters;
      Indent_Style : Indentation_Kind;
      Max_Line     : constant Integer := Highlight_Column.Get_Pref;
   begin
      Get_Indentation_Parameters (Lang, Params, Indent_Style);
      return LSP.Messages.FormattingOptions'
        (tabSize                          =>
           LSP.Types.LSP_Number (Params.Indent_Level),
         insertSpaces                     =>  not Params.Use_Tabs,
         trimTrailingWhitespace           =>
           (Is_Set => True, Value => Strip_Blanks.Get_Pref /= Never),
         insertFinalNewline               =>
           (Is_Set => True, Value => False),
         trimFinalNewlines                =>
           (Is_Set => True, Value => Strip_Lines.Get_Pref /= Never),
         gnatFormatMaxSize                =>
           (Is_Set => True,
            Value  => LSP.Types.LSP_Number (Max_Line)),
         gnatFormatContinuationLineIndent =>
           (Is_Set => True,
            Value  => LSP.Types.LSP_Number (Params.Indent_Continue)));
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
         then 1 else 2);
   end UTF16_Encoded_Length;

end GPS.LSP_Client.Utilities;
