-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2001-2006                      --
--                              AdaCore                              --
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
   function Shell_Open (File : String) return Boolean;
   --  Open file with the associated command registered under Windows.
   --  Return True in case of success.

   ----------------
   -- Shell_Open --
   ----------------

   function Shell_Open (File : String) return Boolean is

      SW_SHOW : constant := 5;  --  winuser.h:206

      function ShellExecute
        (Hwnd         : System.Address;
         Lpoperation  : System.Address;
         Lpfile       : System.Address;
         Lpparameters : System.Address;
         Lpdirectory  : System.Address;
         Nshowcmd     : Integer) return Long_Integer;
      pragma Import (Stdcall, ShellExecute, "ShellExecuteA");

      C_Open : aliased constant String := "open" & ASCII.NUL;
      C_File : aliased constant String := File & ASCII.NUL;

   begin
      return ShellExecute
        (System.Null_Address,
         C_Open'Address,
         C_File'Address,
         System.Null_Address,
         System.Null_Address,
         SW_SHOW) > 32;
   end Shell_Open;

begin
   if Shell_Open (URL) then
      Insert
        (Kernel, -"Using default browser to view " & URL, Mode => Info);
   else
      Insert
        (Kernel, -"Could not display help file " & URL, Mode => Error);
   end if;
end Display_Help;
