------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2003-2019, AdaCore                     --
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

--  Win32 version of this file

with GNATCOLL.VFS; use GNATCOLL.VFS;

with System;              use System;
with Win32_Printing_Defs; use Win32_Printing_Defs;
with GNATCOLL.Traces;              use GNATCOLL.Traces;
with Ada.Exceptions;      use Ada.Exceptions;
with Ada.Text_IO;
with Pango.Font;
with GPS.Kernel.Preferences;
with Pango.Enums;
with Glib;

with Src_Editor_Buffer;

package body Src_Printing.Win32_Printer is

   use Src_Editor_Buffer;

   Me : constant Trace_Handle :=  Create ("Src_Printing.Win32_Printer");

   function Noncollated_Copies (PD : PrintDlg) return INT;
   --  Controller for collating loops. If the user selects collating, returns
   --  1; otherwise returns the number of copies requested.

   function Collated_Copies (PD : PrintDlg) return INT;
   --  Controller for collating loops. If the user selects collating, returns
   --  the number of copies requested; otherwise returns 1.

   function First_Page_Selected (PD : PrintDlg; Min : INT) return INT;
   --  Return the number of the first page to print.  If the user selects
   --  specific pages, returns that selection; otherwise returns the value
   --  of Min (presumably 1).

   function Last_Page_Selected (PD : PrintDlg; Max : INT) return INT;
   --  Return the number of the last page to print.  If the user selects
   --  specific pages, returns that selection; otherwise returns the value
   --  of Max (presumably the total number of pages).

   procedure Initialize_Print_Dialog (PD : out PrintDlg);
   --  Prepare data structure for printer selection and configuration dialog

   type Margin_Offsets is record
      Left   : INT;
      Right  : INT;
      Top    : INT;
      Bottom : INT;
   end record;
   --  The offsets from the physical page representing the user's margin
   --  selections, taking into account the fact that the printer cannot usually
   --  print to the very edge of the page.

   procedure Get_Margins (Printer : HDC; Offsets : out Margin_Offsets);
   --  Determine the offsets in pixels from the edges of the page.
   --  The user's values are from the edge of the page so we have to
   --  subtract the margin offsets that the printer inserts due to the
   --  fact that it cannot print to the very edge of the paper.

   function Foundary
     (Font_Name  : String;
      Font_Size  : Positive; -- in points
      Bold       : Boolean;
      Italicized : Boolean;
      Printer    : HDC) return HFONT;
   --  Create a logical font with the indciated chacteristics indicated for the
   --  specified printer.

   procedure Print
     (Editor     : Source_Editor_Box;
      Font_Name  : String;
      Font_Size  : Positive;
      From_Line  : Editable_Line_Type;
      To_Line    : Editable_Line_Type);
   --  Print buffer or part of it

   procedure Print_Header
     (File_Name : String;
      Alt_Font  : HFONT;
      Printer   : HDC);
   --  Print the file name at the top of the page with the font indicated.

   procedure Print_Body
     (From_Line      : Editable_Line_Type;
      This_Page      : INT;
      Lines_Per_Page : INT;
      Line_Height    : INT;
      Chars_Per_Line : INT;
      Total_Lines    : INT;
      Offsets        : Margin_Offsets;
      Buffer         : Source_Buffer;
      Printer        : HDC);
   --  Print the center section of the page between the header & footer

   procedure Print_Footer (This_Page : INT; Alt_Font : HFONT; Printer : HDC);
   --  Print the page number at the bottom of the page with the font indicated.

   function Text_Width (Input : String; Printer : HDC) return INT;
   --  Return the length of the input string in pixels using the current font
   --  for the indicated printer.

   function Centered (Input : String;  Printer : HDC) return INT;
   --  Return the location of the input string centered on the page
   --  with the current font of the selected printer, in pixel units.

   ------------
   -- Create --
   ------------

   function Create return Printer is
   begin
      return (Abstract_Printer with null record);
   end Create;

   -----------
   -- Print --
   -----------

   overriding procedure Print
     (This       : Printer;
      Editor     : Src_Editor_Box.Source_Editor_Box;
      From       : Editable_Line_Type := 1;
      To         : Editable_Line_Type := Editable_Line_Type'Last)
   is
      pragma Unreferenced (This);

      Source_Font      : constant Pango.Font.Pango_Font_Description :=
        GPS.Kernel.Preferences.View_Fixed_Font.Get_Pref;
      Source_Font_Name : constant String :=
        Pango.Font.Get_Family (Source_Font);
      Source_Font_Size : constant Glib.Gint :=
        Pango.Enums.To_Pixels (Pango.Font.Get_Size (Source_Font));
      Last_Line        : constant Editable_Line_Type :=
        Editable_Line_Type (Editor.Get_Last_Line);
   begin
      Print
        (Editor,
         Source_Font_Name,
         Positive (Source_Font_Size),
         From,
         Editable_Line_Type'Min (Last_Line, To));
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print
     (Editor     : Source_Editor_Box;
      Font_Name  : String;
      Font_Size  : Positive;
      From_Line  : Editable_Line_Type;
      To_Line    : Editable_Line_Type)
   is
      PD             : PrintDlg;
      Document       : DOCINFO;
      Success        : Boolean;
      Old_Mode       : INT;
      Printer_TM     : TEXTMETRIC;
      New_Font       : HFONT;
      Old_Font       : HFONT;
      Banner_Font    : HFONT;
      Line_Height    : INT;
      Chars_Per_Line : INT;
      Lines_Per_Page : INT;
      Total_Pages    : INT;
      Start_Page     : INT;
      End_Page       : INT;
      Actual_Width   : INT;
      Actual_Height  : INT;
      Result         : BOOL;
      Status         : INT;
      Offsets        : Margin_Offsets;

      Buffer         : constant Source_Buffer := Get_Buffer (Editor);

      File_Name      : constant Virtual_File := Get_Filename (Editor);

      Document_Name  : constant String := +File_Name.Full_Name & ASCII.NUL;

      Total_Lines    : constant INT := INT (To_Line - From_Line + 1);

      pragma Unreferenced (Status, Result);

   begin
      Document.cbSize := (Document'Size + Storage_Unit - 1) / Storage_Unit;
      Document.lpszOutput := Null_Address;
      Document.lpszDocName := Document_Name'Address;

      Initialize_Print_Dialog (PD);

      if not PrintDlg_func (PD'Address) then
         return;
      end if;
      --  Note: several of the values within PD may be changed by dialog

      --  This font selection should come from Prefs!
      Banner_Font := Foundary ("Arial", 8, False, True, PD.DC);

      --  Tell the printer to use points for measurements
      Status := SetMapMode (PD.DC, MM_TEXT);

      --  Create a logical font for the selected printer
      New_Font := Foundary (Font_Name, Font_Size, False, False, PD.DC);

      --  Make the background opaque instead of transparent (as necessary)
      Old_Mode := SetBkMode (PD.DC, OPAQUE);

      --  Tell the printer to use our font
      --  This must happen before we compute lines per page and total pages!
      Old_Font := SelectObject (PD.DC, New_Font);

      --  Now that we've selected the printer we can compute the offsets
      Get_Margins (PD.DC, Offsets);

      --  Query printer to determine how the page is laid out with new font
      Result := GetTextMetrics (PD.DC, Printer_TM'Address);

      Actual_Width := GetDeviceCaps (PD.DC, HORZRES)
                      - Offsets.Left - Offsets.Right;

      Chars_Per_Line := (Actual_Width + INT (Printer_TM.tmAveCharWidth) - 1) /
                        INT (Printer_TM.tmAveCharWidth);
      --  NB: the maximum char width for proportional fonts can be huge
      --  so we use the average width; no diff for nonproportional fonts

      Line_Height := INT (Printer_TM.tmHeight + Printer_TM.tmExternalLeading);

      Actual_Height := GetDeviceCaps (PD.DC, VERTRES)
        - Offsets.Top - Offsets.Bottom;

      Lines_Per_Page := Actual_Height / Line_Height;

      Total_Pages := (Total_Lines + Lines_Per_Page - 1) / Lines_Per_Page;

      --  See if user selected a starting page, one otherwise
      Start_Page := First_Page_Selected (PD, 1);

      --  See if user selected an ending page, Total_Pages otherwise
      End_Page := Last_Page_Selected (PD, Total_Pages);

      Success := True;
      --  NB : Logical page and line numbers start at zero, unlike the
      --  line numbers in the source buffer.
      --  The key to these loops is that Paginated_Line_Number repeats
      --  iterating over the same range when the same page must be printed
      --  due to collating, such that we get the same lines each iteration.

      if StartDoc (PD.DC, Document'Address) > 0 then
         Outermost : for Collated in 1 .. Collated_Copies (PD) loop
            for This_Page in Start_Page - 1 .. End_Page - 1 loop
               for Noncollated in 1 .. Noncollated_Copies (PD) loop
                  if StartPage (PD.DC) < 0 then
                     Success := False;
                     exit Outermost;
                  end if;

                  --  We have to call this, here, for the selected font to
                  --  be used consistently across Windows versions...
                  New_Font := SelectObject (PD.DC, New_Font);

                  Print_Header
                    (+File_Name.Full_Name, Banner_Font, PD.DC);
                  Print_Body
                    (From_Line,
                     This_Page,
                     Lines_Per_Page,
                     Line_Height,
                     Chars_Per_Line,
                     Total_Lines,
                     Offsets,
                     Buffer,
                     PD.DC);
                  Print_Footer (This_Page + 1, Banner_Font, PD.DC);

                  if EndPage (PD.DC) < 0 then
                     Success := False;
                     exit Outermost;
                  end if;
               end loop;
            end loop;
         end loop Outermost;

      else
         Success := False;
      end if;

      if Success then
         Status := EndDoc (PD.DC);
      end if;

      --  Go back to the previous settings so that subsequent
      --  jobs don't inherit our settings

      Old_Font := SelectObject (PD.DC, Old_Font);
      Old_Mode := SetBkMode (PD.DC, Old_Mode);
      Result := DeleteDC (PD.DC);

   exception
      when Error : others =>
         Old_Font := SelectObject (PD.DC, Old_Font);
         Old_Mode := SetBkMode (PD.DC, Old_Mode);
         Result := DeleteDC (PD.DC);

         Trace (Me,
                "Unexpected exception: " & Exception_Information (Error));
   end Print;

   ------------------
   -- Print_Header --
   ------------------

   procedure Print_Header
     (File_Name : String;
      Alt_Font  : HFONT;
      Printer   : HDC)
   is
      Prior_Font : HFONT;
      pragma Warnings (Off, Prior_Font);
      Result     : BOOL;
      pragma Unreferenced (Result);

   begin
      Prior_Font := SelectObject (Printer, Alt_Font);

      Result := TextOut
        (Printer,
         Centered (File_Name, Printer),
         0,
         File_Name'Address,
         File_Name'Length);

      Prior_Font := SelectObject (Printer, Prior_Font);
   end Print_Header;

   ------------------
   -- Print_Footer --
   ------------------

   procedure Print_Footer (This_Page : INT; Alt_Font : HFONT; Printer : HDC) is
      Printer_TM  : TEXTMETRIC;
      Line_Height : INT;
      Page_Height : INT;
      Last_Line   : INT;
      Prior_Font  : HFONT;
      pragma Warnings (Off, Prior_Font);
      Banner      : constant String := "Page" & INT'Image (This_Page);
      Result      : BOOL;
      pragma Unreferenced (Result);

   begin
      Prior_Font := SelectObject (Printer, Alt_Font);
      Result     := GetTextMetrics (Printer, Printer_TM'Address);

      Line_Height := INT (Printer_TM.tmHeight + Printer_TM.tmExternalLeading);
      Page_Height := GetDeviceCaps (Printer, VERTRES);
      Last_Line   := Page_Height - Line_Height;
      --  Note that the last line position depends on the size of this font

      Result := TextOut
        (Printer,
         Centered (Banner, Printer),
         Last_Line,
         Banner'Address,
         Banner'Length);

      Prior_Font := SelectObject (Printer, Prior_Font);
   end Print_Footer;

   ----------------
   -- Print_Body --
   ----------------

   procedure Print_Body
     (From_Line      : Editable_Line_Type;
      This_Page      : INT;
      Lines_Per_Page : INT;
      Line_Height    : INT;
      Chars_Per_Line : INT;
      Total_Lines    : INT;
      Offsets        : Margin_Offsets;
      Buffer         : Source_Buffer;
      Printer        : HDC)
   is
      Actual_Line_Number    : Editable_Line_Type;
      Actual_Line_Image     : String (1 .. 5);
      Paginated_Line_Number : INT;
      Result                : BOOL;
      pragma Unreferenced (Result);

      package Line_Number_IO is new
        Ada.Text_IO.Integer_IO (Editable_Line_Type);

   begin
      for Line_Num in 0 .. Lines_Per_Page - 1 loop
         Paginated_Line_Number :=
           Lines_Per_Page * This_Page + Line_Num;

         exit when Paginated_Line_Number > Total_Lines - 1;

         Actual_Line_Number := Editable_Line_Type (Paginated_Line_Number) +
           From_Line;

         Line_Number_IO.Put (Actual_Line_Image, Actual_Line_Number);

         declare
            Line_String : Src_String := Get_String_At_Line
              (Buffer, Actual_Line_Number);
            Content     : constant String := Actual_Line_Image & ": " &
              To_String (Line_String);
         begin
            Free (Line_String);

            Result := TextOut
              (Printer,
               Offsets.Left,
               (Line_Height * Line_Num) + Offsets.Top,
               Content'Address,
               INT'Min (Content'Length, Chars_Per_Line));
         end;
      end loop;
   end Print_Body;

   --------------
   -- Centered --
   --------------

   function Centered (Input : String;  Printer : HDC) return INT is
      Page_Width  : constant INT := GetDeviceCaps (Printer, PHYSICALWIDTH);
      Page_Center : constant INT := Page_Width / 2;
      Offset      : constant INT := GetDeviceCaps (Printer, PHYSICALOFFSETX);
      Input_Width : constant INT := Text_Width (Input, Printer);

   begin
      return Page_Center - Offset - (Input_Width / 2);
   end Centered;

   ----------------
   -- Text_Width --
   ----------------

   function Text_Width (Input : String; Printer : HDC) return INT is
      Output : Win32_Printing_Defs.SIZE;
      Result : BOOL;
      pragma Unreferenced (Result);

   begin
      Result := GetTextExtentPoint
        (Printer, Input'Address, Input'Length, Output'Address);
      return INT (Output.cx);
   end Text_Width;

   ------------------------
   -- Noncollated_Copies --
   ------------------------

   function Noncollated_Copies (PD : PrintDlg) return INT is
   begin
      if (PD.Flags and PD_COLLATE) /= 0 then
         return 1;
      else
         return INT (PD.nCopies);
      end if;
   end Noncollated_Copies;

   ---------------------
   -- Collated_Copies --
   ---------------------

   function Collated_Copies (PD : PrintDlg) return INT is
   begin
      if (PD.Flags and PD_COLLATE) /= 0 then
         return INT (PD.nCopies);
      else
         return 1;
      end if;
   end Collated_Copies;

   --------------
   -- Foundary --
   --------------

   function Foundary
     (Font_Name  : String;
      Font_Size  : Positive;
      Bold       : Boolean;
      Italicized : Boolean;
      Printer    : HDC) return HFONT
   is
      Logical_Font : LOGFONT;
      Font_Id      : constant String := Font_Name & ASCII.NUL;
   begin
      Logical_Font.lfHeight := LONG
        (-MulDiv (INT (Font_Size), GetDeviceCaps (Printer, LOGPIXELSY), 72));
      --  The above formula is from the Microsoft Developers Network docs

      Logical_Font.lfWidth := 0;
      Logical_Font.lfEscapement := 0;
      Logical_Font.lfOrientation := 0;

      if Bold then
         Logical_Font.lfWeight := FW_BOLD;
      else
         Logical_Font.lfWeight := FW_DONTCARE;
      end if;

      if Italicized then
         Logical_Font.lfItalic := 1;
      else
         Logical_Font.lfItalic := 0;
      end if;

      Logical_Font.lfUnderline := 0;
      Logical_Font.lfStrikeOut := 0;
      Logical_Font.lfCharSet := 0;
      Logical_Font.lfOutPrecision := OUT_TT_PRECIS;
      Logical_Font.lfClipPrecision := CLIP_DEFAULT_PRECIS;
      Logical_Font.lfQuality := DEFAULT_QUALITY;
      Logical_Font.lfPitchAndFamily := DEFAULT_PITCH or FF_SWISS;

      declare
         Result : LPSTR;
         pragma Unreferenced (Result);
      begin
         Result := lstrcpy (Logical_Font.lfFaceName'Address, Font_Id'Address);
      end;

      return CreateFontIndirect (Logical_Font'Address);
   end Foundary;

   -----------------------------
   -- Initialize_Print_Dialog --
   -----------------------------

   procedure Initialize_Print_Dialog (PD : out PrintDlg) is
   begin
      PD.lStructSize := PrintDlg_Size;

      --  Use null for hwndOwner so the collating option works interactively

      PD.hwndOwner           := Null_Address;
      PD.hDevMode            := Null_Address;
      PD.hDevNames           := Null_Address;
      PD.DC                  := Null_Address;
      --  PD.DC is non-null on return because we specify the flag PD_RETURNDC

      PD.Flags               := PD_RETURNDC or PD_ALLPAGES or PD_COLLATE or
                                PD_USEDEVMODECOPIES or PD_NOSELECTION or
                                PD_HIDEPRINTTOFILE;
      PD.nFromPage           := 16#FFFF#;
      PD.nToPage             := 16#FFFF#;
      PD.nMinPage            := 0;
      PD.nMaxPage            := 16#FFFF#;
      PD.nCopies             := 1;
      PD.Instance            := Null_Address;
      PD.lCustData           := 0;
      PD.lpfnPrintHook       := null;
      PD.lpfnSetupHook       := null;
      PD.lpPrintTemplateName := Null_Address;
      PD.lpSetupTemplateName := Null_Address;
      PD.hPrintTemplate      := Null_Address;
      PD.hSetupTemplate      := Null_Address;
   end Initialize_Print_Dialog;

   -------------------------
   -- First_Page_Selected --
   -------------------------

   function First_Page_Selected (PD : PrintDlg; Min : INT) return INT is
   begin
      if (PD.Flags and PD_PAGENUMS) /= 0 then
         return INT'Max (INT (PD.nFromPage), Min);
      else
         return Min;
      end if;
   end First_Page_Selected;

   ------------------------
   -- Last_Page_Selected --
   ------------------------

   function Last_Page_Selected (PD : PrintDlg; Max : INT) return INT is
   begin
      if (PD.Flags and PD_PAGENUMS) /= 0 then
         return INT'Min (INT (PD.nToPage), Max);
      else
         return Max;
      end if;
   end Last_Page_Selected;

   -----------------
   -- Get_Margins --
   -----------------

   procedure Get_Margins (Printer : HDC; Offsets : out Margin_Offsets) is
      --  Requested margins are decimal values specifying (fractions of) inches
      --  eventually these will come from the user via Prefs
      Left   : constant Float := 0.75; -- inches
      Right  : constant Float := 0.5; -- inches
      Top    : constant Float := 0.5; -- inches
      Bottom : constant Float := 0.5; -- inches

      --  Pixels per inch across the width of the page
      PPI_Width : constant Float := Float
        (GetDeviceCaps (Printer, LOGPIXELSX));

      --  Pixels per inch across the height of the page
      PPI_Height : constant Float := Float
        (GetDeviceCaps (Printer, LOGPIXELSY));

      --  Offset of the printable area from the left side of the physical page
      Printable_Offset_Left : constant INT :=
        GetDeviceCaps (Printer, PHYSICALOFFSETX);

      --  Offset of the printable area from the top of the physical page
      Printable_Offset_Top : constant INT :=
        GetDeviceCaps (Printer, PHYSICALOFFSETY);

      Right_Side_Gap : constant INT :=
        GetDeviceCaps (Printer, PHYSICALWIDTH) -
        Printable_Offset_Left -
        GetDeviceCaps (Printer, HORZRES);

      Bottom_Gap : constant INT :=
        GetDeviceCaps (Printer, PHYSICALHEIGHT) -
        Printable_Offset_Top -
        GetDeviceCaps (Printer, VERTRES);

   begin
      Offsets.Left   := INT ((Left * PPI_Width) + 0.5) - Printable_Offset_Left;
      Offsets.Right  := INT ((Right * PPI_Width) + 0.5) - Right_Side_Gap;
      Offsets.Top    := INT ((Top * PPI_Height) + 0.5) - Printable_Offset_Top;
      Offsets.Bottom := INT ((Bottom * PPI_Height) + 0.5) - Bottom_Gap;
   end Get_Margins;

end Src_Printing.Win32_Printer;
