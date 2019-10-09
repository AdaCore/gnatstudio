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

--  Win32 API definitions related to printing text

with System;
with Interfaces.C;

package Win32_Printing_Defs is

   type    BYTE     is new Interfaces.C.unsigned_char;   --  windef.h
   type    USHORT   is new Interfaces.C.unsigned_short;  --  windef.h
   subtype WORD     is USHORT;                           --  windef.h
   type    ULONG    is new Interfaces.C.unsigned_long;   --  windef.h
   subtype DWORD    is ULONG;                            --  windef.h
   type    LONG     is new Interfaces.C.long;            --  winnt.h
   subtype LPARAM   is LONG;                             --  windef.h
   type    INT      is new Interfaces.C.int;             --  windef.h
   type    UINT     is new Interfaces.C.unsigned;        --  windef.h
   subtype WPARAM   is UINT;                             --  windef.h
   subtype CHAR     is Interfaces.C.char;                --  winnt.h
   type  CHAR_Array is array (Natural range <>) of aliased CHAR;

   type BOOL is new Boolean;                            --  windef.h
   pragma Convention (C, BOOL);

   subtype HANDLE is System.Address;             --  winnt.h :144

   subtype HWND      is HANDLE;                  --  windef.h :178
   subtype HFONT     is HANDLE;                  --  windef.h :195
   subtype HGLOBAL   is HANDLE;                  --  windef.h :169
   subtype HINSTANCE is HANDLE;                  --  windef.h :199
   subtype HDC       is HANDLE;                  --  windef.h :191
   subtype HGDIOBJ   is HANDLE;                  --  windef.h :183

   subtype PCCH   is System.Address;             --  winnt.h
   subtype LPCSTR is PCCH;                       --  winnt.h
   subtype LPSTR  is PCCH;                       --  winnt.h

   type LPPRINTHOOKPROC is access function
     (Wnd    : HWND;
      nCode  : UINT;
      Param1 : WPARAM;
      Param2 : LPARAM) return UINT;              --  commdlg.h:425
   pragma Convention (Stdcall, LPPRINTHOOKPROC);

   type LPSETUPHOOKPROC is access function
     (Wnd    : HWND;
      nCode  : UINT;
      Param1 : WPARAM;
      Param2 : LPARAM) return UINT;              --  commdlg.h:426
   pragma Convention (Stdcall, LPSETUPHOOKPROC);

   type PRINTDLGA is record                      --  commdlg.h:428
      lStructSize         : DWORD;               --  commdlg.h:429
      hwndOwner           : HWND;                --  commdlg.h:430
      hDevMode            : HGLOBAL;             --  commdlg.h:431
      hDevNames           : HGLOBAL;             --  commdlg.h:432
      DC                  : HDC;                 --  commdlg.h:433
      Flags               : DWORD;               --  commdlg.h:434
      nFromPage           : WORD;                --  commdlg.h:435
      nToPage             : WORD;                --  commdlg.h:436
      nMinPage            : WORD;                --  commdlg.h:437
      nMaxPage            : WORD;                --  commdlg.h:438
      nCopies             : WORD;                --  commdlg.h:439
      Instance            : HINSTANCE;           --  commdlg.h:440
      lCustData           : LPARAM;              --  commdlg.h:441
      lpfnPrintHook       : LPPRINTHOOKPROC;     --  commdlg.h:442
      lpfnSetupHook       : LPSETUPHOOKPROC;     --  commdlg.h:443
      lpPrintTemplateName : LPCSTR;              --  commdlg.h:444
      lpSetupTemplateName : LPCSTR;              --  commdlg.h:445
      hPrintTemplate      : HGLOBAL;             --  commdlg.h:446
      hSetupTemplate      : HGLOBAL;             --  commdlg.h:447
   end record;
   pragma Convention (C, PRINTDLGA);

   subtype PrintDlg is PRINTDLGA;                          --  commdlg.h:475

   subtype Print_Dialog_Access is System.Address;

   PD_ALLPAGES                  : constant := 16#0#;         --  commdlg.h:491
   PD_SELECTION                 : constant := 16#1#;         --  commdlg.h:492
   PD_PAGENUMS                  : constant := 16#2#;         --  commdlg.h:493
   PD_NOSELECTION               : constant := 16#4#;         --  commdlg.h:494
   PD_NOPAGENUMS                : constant := 16#8#;         --  commdlg.h:495
   PD_COLLATE                   : constant := 16#10#;        --  commdlg.h:496
   PD_PRINTTOFILE               : constant := 16#20#;        --  commdlg.h:497
   PD_PRINTSETUP                : constant := 16#40#;        --  commdlg.h:498
   PD_NOWARNING                 : constant := 16#80#;        --  commdlg.h:499
   PD_RETURNDC                  : constant := 16#100#;       --  commdlg.h:500
   PD_RETURNIC                  : constant := 16#200#;       --  commdlg.h:501
   PD_RETURNDEFAULT             : constant := 16#400#;       --  commdlg.h:502
   PD_SHOWHELP                  : constant := 16#800#;       --  commdlg.h:503
   PD_ENABLEPRINTHOOK           : constant := 16#1000#;      --  commdlg.h:504
   PD_ENABLESETUPHOOK           : constant := 16#2000#;      --  commdlg.h:505
   PD_ENABLEPRINTTEMPLATE       : constant := 16#4000#;      --  commdlg.h:506
   PD_ENABLESETUPTEMPLATE       : constant := 16#8000#;      --  commdlg.h:507
   PD_ENABLEPRINTTEMPLATEHANDLE : constant := 16#10000#;     --  commdlg.h:508
   PD_ENABLESETUPTEMPLATEHANDLE : constant := 16#20000#;     --  commdlg.h:509
   PD_USEDEVMODECOPIES          : constant := 16#40000#;     --  commdlg.h:510
   PD_DISABLEPRINTTOFILE        : constant := 16#80000#;     --  commdlg.h:511
   PD_HIDEPRINTTOFILE           : constant := 16#100000#;    --  commdlg.h:512
   PD_NONETWORKBUTTON           : constant := 16#200000#;    --  commdlg.h:513

   OUT_TT_PRECIS       : constant := 4;             --  wingdi.h:789
   CLIP_DEFAULT_PRECIS : constant := 0;             --  wingdi.h:795
   DEFAULT_QUALITY     : constant := 0;             --  wingdi.h:803
   DEFAULT_PITCH       : constant := 0;             --  wingdi.h:807
   FF_SWISS            : constant := 32;            --  wingdi.h:824

   function PrintDlg_func (lppd : Print_Dialog_Access) return BOOL;
   --  commdlg.h:482
   pragma Import (Stdcall, PrintDlg_func, "PrintDlgA");

   function SetMapMode (dc : HDC; fnmapMode : INT) return INT;
   --  wingdi.h:2210
   pragma Import (Stdcall, SetMapMode, "SetMapMode");

   function EnableWindow (Wnd : HWND; bEnable : BOOL) return BOOL;
   --  winuser.h:3197
   pragma Import (Stdcall, EnableWindow, "EnableWindow");

   function GetDeviceCaps (dc : HDC; iCapability : INT) return INT;
   --  wingdi.h:2037
   pragma Import (Stdcall, GetDeviceCaps, "GetDeviceCaps");

   type TEXTMETRICA is record                    --  wingdi.h:522
      tmHeight           : LONG;                 --  wingdi.h:524
      tmAscent           : LONG;                 --  wingdi.h:525
      tmDescent          : LONG;                 --  wingdi.h:526
      tmInternalLeading  : LONG;                 --  wingdi.h:527
      tmExternalLeading  : LONG;                 --  wingdi.h:528
      tmAveCharWidth     : LONG;                 --  wingdi.h:529
      tmMaxCharWidth     : LONG;                 --  wingdi.h:530
      tmWeight           : LONG;                 --  wingdi.h:531
      tmOverhang         : LONG;                 --  wingdi.h:532
      tmDigitizedAspectX : LONG;                 --  wingdi.h:533
      tmDigitizedAspectY : LONG;                 --  wingdi.h:534
      tmFirstChar        : BYTE;                 --  wingdi.h:535
      tmLastChar         : BYTE;                 --  wingdi.h:536
      tmDefaultChar      : BYTE;                 --  wingdi.h:537
      tmBreakChar        : BYTE;                 --  wingdi.h:538
      tmItalic           : BYTE;                 --  wingdi.h:539
      tmUnderlined       : BYTE;                 --  wingdi.h:540
      tmStruckOut        : BYTE;                 --  wingdi.h:541
      tmPitchAndFamily   : BYTE;                 --  wingdi.h:542
      tmCharSet          : BYTE;                 --  wingdi.h:543
   end record;
   pragma Convention (C, TEXTMETRICA);

   subtype TEXTMETRIC is TEXTMETRICA;               --  wingdi.h:574

   subtype TEXTMETRIC_Access is System.Address;   --  wingdi.h:1785

   type LOGFONTA is record                       --  wingdi.h:714
      lfHeight         : LONG;                   --  wingdi.h:716
      lfWidth          : LONG;                   --  wingdi.h:717
      lfEscapement     : LONG;                   --  wingdi.h:718
      lfOrientation    : LONG;                   --  wingdi.h:719
      lfWeight         : LONG;                   --  wingdi.h:720
      lfItalic         : BYTE;                   --  wingdi.h:721
      lfUnderline      : BYTE;                   --  wingdi.h:722
      lfStrikeOut      : BYTE;                   --  wingdi.h:723
      lfCharSet        : BYTE;                   --  wingdi.h:724
      lfOutPrecision   : BYTE;                   --  wingdi.h:725
      lfClipPrecision  : BYTE;                   --  wingdi.h:726
      lfQuality        : BYTE;                   --  wingdi.h:727
      lfPitchAndFamily : BYTE;                   --  wingdi.h:728
      lfFaceName       : CHAR_Array (0 .. 31);   --  wingdi.h:729
   end record;
   pragma Convention (C, LOGFONTA);

   subtype LOGFONT is LOGFONTA;

   subtype LOGFONT_Access is System.Address;

   FF_DONTCARE : constant := 0; --  font family
   FW_DONTCARE : constant := 0; --  font weight
   FW_BOLD     : constant := 700;

   function CreateFontIndirect (lplf : LOGFONT_Access) return HFONT;
   --  wingdi.h:1840
   pragma Import (Stdcall, CreateFontIndirect, "CreateFontIndirectA");

   HORZRES         : constant := 8;             --  wingdi.h:1160
   VERTRES         : constant := 10;            --  wingdi.h:1161
   LOGPIXELSX      : constant := 88;            --  wingdi.h:1180
   LOGPIXELSY      : constant := 90;            --  wingdi.h:1181
   PHYSICALWIDTH   : constant := 110;
   PHYSICALHEIGHT  : constant := 111;
   PHYSICALOFFSETX : constant := 112;           -- wingdi.h:1192
   PHYSICALOFFSETY : constant := 113;           -- wingdi.h:1193

   OPAQUE : constant := 2;             --  wingdi.h:1050

   MM_TEXT : constant := 1;             --  wingdi.h:1066

   function MulDiv
     (nNumber : INT;
      nNumerator : INT;
      nDenominator : INT) return INT;           --  winbase.h :2604
   pragma Import (Stdcall, MulDiv, "MulDiv");

   function SelectObject (dc : HDC; gdiobj : HGDIOBJ) return HGDIOBJ;
   --  wingdi.h:2198
   pragma Import (Stdcall, SelectObject, "SelectObject");

   function SetBkMode (dc : HDC; fnBkMode : INT) return INT;
   --  wingdi.h:2201
   pragma Import (Stdcall, SetBkMode, "SetBkMode");

   function GetTextMetrics (dc : HDC;  lptm : TEXTMETRIC_Access) return BOOL;
   --  wingdi.h:2286
   pragma Import (Stdcall, GetTextMetrics, "GetTextMetricsA");

   type DOCINFOA is record                       --  wingdi.h:2376
      cbSize      : INT;                         --  wingdi.h:2377
      lpszDocName : LPCSTR;                      --  wingdi.h:2378
      lpszOutput  : LPCSTR;                      --  wingdi.h:2379
   end record;
   subtype DOCINFO is DOCINFOA;

   subtype DOCINFOA_Access is System.Address;    --  wingdi.h:2395

   function StartDoc (dc : HDC;  lpdi : DOCINFOA_Access) return INT;
   --  wingdi.h:2395
   pragma Import (Stdcall, StartDoc, "StartDocA");

   function StartPage (dc : HDC) return INT;     --  wingdi.h:2403
   pragma Import (Stdcall, StartPage, "StartPage");

   function EndPage (dc : HDC) return INT;       --  wingdi.h:2404
   pragma Import (Stdcall, EndPage, "EndPage");

   function EndDoc (dc : HDC) return INT;        --  wingdi.h:2402
   pragma Import (Stdcall, EndDoc, "EndDoc");

   function TextOut
     (dc       : HDC;
      nXStart  : INT;
      nYStart  : INT;
      lpstring : LPCSTR;
      cbString : INT) return BOOL;               --  wingdi.h:2436
   pragma Import (Stdcall, TextOut, "TextOutA");

   function DeleteDC (dc : HDC) return BOOL;     --  wingdi.h:1891
   pragma Import (Stdcall, DeleteDC, "DeleteDC");

   function lstrcpy
     (lpString1 : LPSTR;
      lpString2 : LPCSTR) return LPSTR;          --  winbase.h :3012
   pragma Import (Stdcall, lstrcpy, "lstrcpyA");

   type SIZE is record
      cx, cy : LONG;
   end record;

   subtype SIZE_Access is System.Address;

   function GetTextExtentPoint
     (dc     : HDC;
      Input  : LPSTR;
      Length : INT;
      Result : SIZE_Access) return BOOL;
   pragma Import (Stdcall, GetTextExtentPoint, "GetTextExtentPoint32A");

end Win32_Printing_Defs;
