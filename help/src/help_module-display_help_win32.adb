------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2017, AdaCore                     --
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

with System;

separate (Help_Module)
procedure Display_Help
  (Kernel    : access Kernel_Handle_Record'Class;
   URL       : String)
is
   File_Protocol : constant String := "file://";

--     HTML_Browser : constant String :=
--       GPS.Kernel.Preferences.Html_Browser.Get_Pref;
--
--     Browsers : constant Browser_List :=
--       (new String'("start"),
--        new String'("firefox %u -new-tab"),
--        new String'("mozilla"),
--        new String'("galeon"),
--        new String'("netscape"),
--        new String'("opera -newpage %u"),
--        new String'("nautilus"),
--        new String'("konqueror"),
--        new String'("open"));

   function Shell_Open (File : String) return Boolean;
   --  Open file with the associated command registered under Windows.
   --  Return True in case of success.

   procedure Display_Help_Internal (URL : String);
   --  Display help file, and insert message in kernel console.

   ----------------
   -- Shell_Open --
   ----------------

   function Shell_Open (File : String) return Boolean is

      SW_SHOW : constant := 5;  --  winuser.h:206

      function ShellExecute
        (Hwnd         : System.Address;
         Lpoperation  : String;
         Lpfile       : String;
         Lpparameters : System.Address := System.Null_Address;
         Lpdirectory  : System.Address := System.Null_Address;
         Nshowcmd     : Integer := SW_SHOW) return Long_Integer;
      pragma Import (Stdcall, ShellExecute, "ShellExecuteA");

      Result : Long_Integer;
   begin
      Result := ShellExecute
        (System.Null_Address, "open" & ASCII.NUL, File & ASCII.NUL);

      if Result <= 32 then
         Trace (Me, "ShellExecute failed:" & Long_Integer'Image (Result));
         return False;
      else
         return True;
      end if;
   end Shell_Open;

   ---------------------------
   -- Display_Help_Internal --
   ---------------------------

   procedure Display_Help_Internal (URL : String) is
   begin
      if Shell_Open (URL) then
         Insert
           (Kernel, -"Using default browser to view " & URL, Mode => Info);
      else
         Insert
           (Kernel, -"Could not display help file " & URL, Mode => Error);
      end if;
   end Display_Help_Internal;

begin
   if URL'Length > File_Protocol'Length
     and then URL (URL'First .. URL'First + File_Protocol'Length - 1)
       = File_Protocol
   then
      --  Strip file:// part, since it causes troubles on some configurations

      Display_Help_Internal
        (URL (URL'First + File_Protocol'Length .. URL'Last));
   else
      Display_Help_Internal (URL);
   end if;
end Display_Help;
