-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2005                       --
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

with GPS.Kernel.Preferences;

separate (Help_Module)
procedure Display_Help
  (Kernel    : access Kernel_Handle_Record'Class;
   Help_File : VFS.Virtual_File)
is
   HTML_Browser : constant String :=
     Get_Pref (Kernel, GPS.Kernel.Preferences.Html_Browser);
   Args         : Argument_List_Access;

   type Cst_String_Access is access constant String;
   type Browser_List is array (Natural range <>) of Cst_String_Access;

   Browsers : constant Browser_List :=
     (new String'("firefox"),
      new String'("mozilla"),
      new String'("galeon"),
      new String'("netscape"),
      new String'("opera"),
      new String'("nautilus"),
      new String'("konqueror"));

   function Launch_Browser
     (Browser : String;
      Args    : Argument_List := (1 .. 0 => null)) return Boolean;
   --  Launch the given browser.
   --  Return True in case of success, false otherwise.

   function Launch_Browser
     (Browser : String;
      Args    : Argument_List := (1 .. 0 => null)) return Boolean
   is
      Cmd     : GNAT.OS_Lib.String_Access;
      File    : GNAT.OS_Lib.String_Access;
      Process : Process_Id;

   begin
      Cmd := Locate_Exec_On_Path (Browser);

      if Cmd = null then
         return False;
      else
         File := new String'(Full_Name (Help_File, True).all);
         Process := Non_Blocking_Spawn (Cmd.all, Args & (1 => File));
         Free (Cmd);
         Insert
           (Kernel, (-"Launching ") & Browser & (-" to view ") & File.all,
            Mode => Info);
         Free (File);

         return Process /= Invalid_Pid;
      end if;
   end Launch_Browser;

begin
   if not Is_Regular_File (Help_File) then
      Insert (Kernel,
              Full_Name (Help_File).all & (-": File not found"),
              Mode => Error);
   end if;

   if HTML_Browser = "" then
      for J in Browsers'Range loop
         if Launch_Browser (Browsers (J).all) then
            return;
         end if;
      end loop;

      Insert (Kernel, -"No HTML browser specified", Mode => Error);
      return;
   end if;

   Args := Argument_String_To_List (HTML_Browser);

   if not Launch_Browser
     (Unprotect (Protect (Args (Args'First).all, False)),
      Args (Args'First + 1 .. Args'Last))
   then
      Insert
        (Kernel,
         -"Could not launch HTML browser: " & Args (Args'First).all,
         Mode => Error);
   end if;

   Free (Args);
end Display_Help;
