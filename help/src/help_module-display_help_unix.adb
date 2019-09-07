------------------------------------------------------------------------------
--                               GNAT Studio                                --
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

with GPS.Kernel.Preferences; use GPS.Kernel.Preferences;
with GNATCOLL.Templates;     use GNATCOLL.Templates;
with GNATCOLL.Traces;        use GNATCOLL.Traces;
with String_Utils;           use String_Utils;

separate (Help_Module)
procedure Display_Help
  (Kernel    : access Kernel_Handle_Record'Class;
   URL       : String)
is

   type Cst_String_Access is access constant String;
   type Browser_List is array (Natural range <>) of Cst_String_Access;

   HTML_Browser : constant String :=
     GPS.Kernel.Preferences.Html_Browser.Get_Pref;

   Browsers : constant Browser_List :=
     (new String'("xdg-open %u"),
      new String'("firefox %u -new-tab"),
      new String'("mozilla"),
      new String'("galeon"),
      new String'("netscape"),
      new String'("opera -newpage %u"),
      new String'("nautilus"),
      new String'("konqueror"),
      new String'("open"));

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

      Result : constant String := Substitute
        (Str       => Browser,
         Delimiter => '%',
         Callback  => Substitute_Parameters'Unrestricted_Access);
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
            Arg : constant String := String_Utils.Unprotect (Args (A).all);
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
      Cmd     : GNAT.Strings.String_Access;
      Process : Process_Id;
   begin
      Cmd := Locate_Exec_On_Path (Browser);

      if Cmd = null then
         return False;
      else
         Kernel.Get_Environment.Apply_Users_Environment;
         Process := Non_Blocking_Spawn (Cmd.all, Args);
         Kernel.Get_Environment.Apply_GPS_Environment;

         Insert
           (Kernel, (-"Launching ") & Browser & (-" to view ") & URL,
            Mode => Info);

         Trace (Me, "Launching external browser with " & Cmd.all & "--");
         for A in Args'Range loop
            Trace (Me, "Args (" & A'Img & ")=" & Args (A).all & "--");
         end loop;

         Free (Cmd);
         return Process /= Invalid_Pid;
      end if;
   end Launch_Browser;

   Args         : Argument_List_Access;

begin
   if HTML_Browser = "" then
      for J in Browsers'Range loop
         Args := Get_Command (Browsers (J).all);
         if Launch_Browser
            (Args (Args'First).all, Args (Args'First + 1 .. Args'Last))
         then
            Free (Args);
            return;
         end if;
         Free (Args);
      end loop;

      Insert (Kernel, -"No HTML browser specified", Mode => Error);
      Trace (Me, "Couldn't start browser");
      return;
   end if;

   Args := Get_Command (HTML_Browser);

   if not Launch_Browser
     (String_Utils.Unprotect
        (String_Utils.Protect (Args (Args'First).all, False)),
      Args (Args'First + 1 .. Args'Last))
   then
      Insert
        (Kernel,
         -"Could not launch HTML browser: " & Args (Args'First).all,
         Mode => Error);
   end if;

   Free (Args);
end Display_Help;
