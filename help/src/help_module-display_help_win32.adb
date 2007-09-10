-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2001-2007, AdaCore                 --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with System;

separate (Help_Module)
procedure Display_Help
  (Kernel    : access Kernel_Handle_Record'Class;
   URL       : String)
is
   File_Protocol : constant String := "file://";

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

   begin
      return ShellExecute
        (System.Null_Address, "open" & ASCII.NUL, File & ASCII.NUL) > 32;
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
