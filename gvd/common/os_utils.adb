-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
--                             ACT-Europe                            --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with GNAT.Case_Util;       use GNAT.Case_Util;
with GNAT.Expect;          use GNAT.Expect;
pragma Warnings (Off);
with GNAT.Expect.TTY;      use GNAT.Expect.TTY;
pragma Warnings (On);
with Interfaces.C.Strings; use Interfaces.C.Strings;
with String_Utils;         use String_Utils;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

package body OS_Utils is

   OpenVMS_Host : Boolean := False;

   -----------------
   -- Get_Tmp_Dir --
   -----------------

   function Get_Tmp_Dir return String is
      S : String_Access;
   begin
      --  Try in the following order:
      --  TMPDIR env var
      --  TEMP env var
      --  system specific function (GetWindows () & "\Temp" under Windows)
      --  ??? not implemented yet
      --  if the above failed, fall back to /tmp

      S := Getenv ("TMPDIR");

      if S.all /= "" then
         declare
            Val : constant String := Name_As_Directory (S.all);
         begin
            Free (S);
            return Val;
         end;
      end if;

      Free (S);
      S := Getenv ("TEMP");

      if S.all /= "" then
         declare
            Val : constant String := Name_As_Directory (S.all);
         begin
            Free (S);
            return Val;
         end;
      end if;

      Free (S);
      return "/tmp/";
   end Get_Tmp_Dir;

   -------------------------
   -- Executable_Location --
   -------------------------

   type chars_ptr_ptr is access all chars_ptr;

   Argv : chars_ptr_ptr;
   pragma Import (C, Argv, "gnat_argv");

   function Executable_Location return String is
      Exec_Name : constant String := Value (Argv.all);

      function Get_Install_Dir (S : String) return String;
      --  S is the executable name preceeded by the absolute or relative
      --  path, e.g. "c:\usr\bin\gcc.exe" or "..\bin\gcc". Returns the absolute
      --  or relative directory where "bin" lies (in the example "C:\usr"
      --  or ".."). If the executable is not a "bin" directory, return "".

      ---------------------
      -- Get_Install_Dir --
      ---------------------

      function Get_Install_Dir (S : String) return String is
         Exec      : String  := S;
         Path_Last : Integer := 0;

      begin
         for J in reverse Exec'Range loop
            if Is_Directory_Separator (Exec (J)) then
               Path_Last := J - 1;
               exit;
            end if;
         end loop;

         if Path_Last >= Exec'First + 2 then
            To_Lower (Exec (Path_Last - 2 .. Path_Last));
         end if;

         if Path_Last < Exec'First + 2
           or else Exec (Path_Last - 2 .. Path_Last) /= "bin"
           or else (Path_Last - 3 >= Exec'First
                    and then not Is_Directory_Separator (Exec (Path_Last - 3)))
         then
            return "";
         end if;

         return Exec (Exec'First .. Path_Last - 4);
      end Get_Install_Dir;

   --  Beginning of Executable_Location

   begin
      --  First determine if a path prefix was placed in front of the
      --  executable name.

      for J in reverse Exec_Name'Range loop
         if Is_Directory_Separator (Exec_Name (J)) then
            return Get_Install_Dir (Exec_Name);
         end if;
      end loop;

      --  If you are here, the user has typed the executable name with no
      --  directory prefix.

      return Get_Install_Dir (GNAT.OS_Lib.Locate_Exec_On_Path (Exec_Name).all);
   end Executable_Location;

   ----------------------------
   -- Is_Directory_Separator --
   ----------------------------

   function Is_Directory_Separator (C : Character) return Boolean is
   begin
      --  In addition to the default directory_separator allow the '/' to
      --  act as separator since this is allowed in MS-DOS, Windows 95/NT,
      --  and OS2 ports. On VMS, the situation is more complicated because
      --  there are two characters to check for.

      return
        C = Directory_Separator
          or else C = '/'
          or else (OpenVMS_Host
                    and then (C = ']' or else C = ':'));
   end Is_Directory_Separator;

   ------------------
   -- Max_Path_Len --
   ------------------

   Max_Path : Integer;
   pragma Import (C, Max_Path, "max_path_len");
   --  Take advantage of max_path_len defined in the GNAT run time.

   function Max_Path_Len return Natural is
   begin
      return Max_Path;
   end Max_Path_Len;

   ---------------
   -- Read_File --
   ---------------

   function Read_File (File : String) return String_Access is
      FD     : File_Descriptor := Invalid_FD;
      Buffer : String_Access;
      Length : Integer;

   begin
      FD := Open_Read (File, Fmode => Binary);

      if FD = Invalid_FD then
         return null;
      end if;

      Length := Integer (File_Length (FD));
      Buffer := new String (1 .. Length);
      Length := Read (FD, Buffer.all'Address, Length);
      Close (FD);
      return Buffer;
   end Read_File;

   ----------------------
   -- Set_OpenVMS_Host --
   ----------------------

   procedure Set_OpenVMS_Host (Setting : Boolean := True) is
   begin
      OpenVMS_Host := Setting;
   end Set_OpenVMS_Host;

   -----------
   -- Spawn --
   -----------

   procedure Spawn
     (Program_Name : String;
      Args         : Argument_List;
      Idle         : Idle_Callback;
      Success      : out Boolean)
   is
      Fd     : TTY_Process_Descriptor;
      Result : Expect_Match;

   begin
      Non_Blocking_Spawn (Fd, Program_Name, Args, Err_To_Out  => True);
      Success := True;

      loop
         Idle.all;
         Expect (Fd, Result, ".+", Timeout => 50);
      end loop;

   exception
      when Invalid_Process =>
         Success := False;

      when Process_Died =>
         Close (Fd);
   end Spawn;

   ------------------------
   -- Make_Dir_Recursive --
   ------------------------

   procedure Make_Dir_Recursive (Name : String) is
      Last : Natural := Name'First + 1;
   begin
      --  Strictly inferior to ignore '/' at the end of Name
      while Last < Name'Last loop
         while Last <= Name'Last
           and then Name (Last) /= Directory_Separator
           and then Name (Last) /= '/'
         loop
            Last := Last + 1;
         end loop;

         if not Is_Directory (Name (Name'First .. Last - 1)) then
            GNAT.Directory_Operations.Make_Dir (Name (Name'First .. Last - 1));
         end if;

         Last := Last + 1;
      end loop;
   end Make_Dir_Recursive;

end OS_Utils;
