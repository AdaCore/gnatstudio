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

with GVD.Preferences;

separate (Help_Module)
procedure Display_Help
  (Kernel    : access Kernel_Handle_Record'Class;
   Help_File : VFS.Virtual_File)
is
   Args    : Argument_List_Access;
   File    : GNAT.OS_Lib.String_Access;
   Cmd     : GNAT.OS_Lib.String_Access;
   Process : Process_Id;
   Browser : constant String :=
     Get_Pref (Kernel, GVD.Preferences.Html_Browser);

begin
   if not Is_Regular_File (Help_File) then
      Insert (Kernel,
              Full_Name (Help_File).all & (-": File not found"),
              Mode => Error);
   end if;

   if Browser = "" then
      --  ??? look for the following:
      --  firefox, mozilla, galeon, netscape, opera, nautilus, konqueror

      Insert (Kernel, -"No HTML browser specified", Mode => Error);
      return;
   end if;

   Args := Argument_String_To_List (Browser);
   Cmd := Locate_Exec_On_Path
            (Unprotect (Protect (Args (Args'First).all, False)));

   if Cmd = null then
      Insert
        (Kernel,
         -"Could not locate HTML browser on path: "
         & Args (Args'First).all,
         Mode => Error);

   else
      File := new String'(Full_Name (Help_File, True).all);
      Process :=
        Non_Blocking_Spawn
          (Cmd.all, Args (Args'First + 1 .. Args'Last) & (1 => File));

      if Process = Invalid_Pid then
         Insert
           (Kernel,
            -"Could not launch HTML browser: " & Args (Args'First).all,
            Mode => Error);
      end if;

      Free (Cmd);
      Free (File);
   end if;

   Free (Args);
end Display_Help;
