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
   URL : constant String := "file://" & Full_Name (Help_File, True).all;

   HTML_Browser : constant String :=
     Get_Pref (Kernel, GPS.Kernel.Preferences.Html_Browser);

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


   function Get_Command (Browser : String) return Argument_List_Access;
   --  return the command to execute to open the specific browser. Parameter
   --  substitution takes place, and the URL is appended if necessary.
   --  It is the responsability of the caller to free the result value

   -----------------
   -- Get_Command --
   -----------------

   function Get_Command (Browser : String) return Argument_List_Access is
      Found_URL : Boolean := False;

      function Substitute_Parameters
        (Param : String; Quoted : Boolean) return String;
      --  Substitute %u... in the command, and check whether the name of the
      --  file appeared at least once

      ---------------------------
      -- Substitute_Parameters --
      ---------------------------

      function Substitute_Parameters
        (Param : String; Quoted : Boolean) return String
      is
         pragma Unreferenced (Quoted);
      begin
         if Param = "u" then
            Found_URL := True;
            return URL;
         end if;
         raise Invalid_Substitution;
      end Substitute_Parameters;

      Result : constant String :=
        Substitute (Browser, '%', Substitute_Parameters'Unrestricted_Access);
      Args : Argument_List_Access;
   begin
      if Found_URL then
         Args := Argument_String_To_List (Result);
      else
         Args := Argument_String_To_List (Browser & " " & URL);
      end if;

      --  Unprotect all arguments, since that is why the shell would normally
      --  do and browser might not be expecting the initial quotes

      for A in Args'Range loop
         declare
            Arg : constant String := Unprotect (Args (A).all);
         begin
            Free (Args (A));
            Args (A) := new String'(Arg);
         end;
      end loop;
      return Args;
   end Get_Command;

   --------------------
   -- Launch_Browser --
   --------------------

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
      Process : Process_Id;
   begin
      Cmd := Locate_Exec_On_Path (Browser);

      if Cmd = null then
         return False;
      else
         Process := Non_Blocking_Spawn (Cmd.all, Args);
         Free (Cmd);

         Insert
           (Kernel, (-"Launching ") & Browser & (-" to view ") & URL,
            Mode => Info);

         Trace (Me, "Launching external browser with " & Browser & "--");
         for A in Args'Range loop
            Trace (Me, "Args (" & A'Img & ")=" & Args (A).all & "--");
         end loop;

         return Process /= Invalid_Pid;
      end if;
   end Launch_Browser;

   Args         : Argument_List_Access;

begin
   if not Is_Regular_File (Help_File) then
      Insert (Kernel,
              Full_Name (Help_File).all & (-": File not found"),
              Mode => Error);
   end if;

   if HTML_Browser = "" then
      Args := new Argument_List'(1 => new String'(URL));
      for J in Browsers'Range loop
         if Launch_Browser (Browsers (J).all, Args.all) then
            Free (Args);
            return;
         end if;
      end loop;

      Free (Args);
      Insert (Kernel, -"No HTML browser specified", Mode => Error);
      return;
   end if;

   Args := Get_Command (HTML_Browser);

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
