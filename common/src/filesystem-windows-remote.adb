-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2006-2008, AdaCore             --
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

pragma Warnings (Off);
with GNAT.Expect.TTY.Remote; use GNAT.Expect.TTY.Remote;
pragma Warnings (On);
with GNAT.OS_Lib;            use GNAT.OS_Lib;
with GNAT.Regpat;            use GNAT.Regpat;

with Basic_Types;
with VFS;

package body Filesystem.Windows.Remote is

   --------------
   -- Home_Dir --
   --------------

   function Home_Dir
     (FS   : Remote_Windows_Filesystem_Record;
      Host : String) return String
   is
      Args   : GNAT.OS_Lib.Argument_List :=
                 (1 => new String'("echo"),
                  2 => new String'("%HOME%"));
      Output : String_Access;
      Status : Boolean;

   begin
      Sync_Execute (Host, Args, Output, Status);
      Basic_Types.Free (Args);

      if Status then
         declare
            Result : constant String := Output.all;
         begin
            Free (Output);
            return Result;
         end;

      else
         return Get_Root (FS, "");
      end if;
   end Home_Dir;

   ---------------------
   -- Is_Regular_File --
   ---------------------

   function Is_Regular_File
     (FS              : Remote_Windows_Filesystem_Record;
      Host            : String;
      Local_Full_Name : String) return Boolean
   is
      pragma Unreferenced (FS);
      --  Redirect stderr to stdout for synchronisation purpose
      --  (stderr is asynchronous on windows)
      Args   : GNAT.OS_Lib.Argument_List :=
                 (new String'("dir"),
                  new String'("/a-d"),
                  new String'("""" & Local_Full_Name & """"),
                  new String'("2>&1"));
      Status : Boolean;

   begin
      Sync_Execute (Host, Args, Status);
      Basic_Types.Free (Args);
      return Status;
   end Is_Regular_File;

   ---------------
   -- Read_File --
   ---------------

   function Read_File
     (FS              : Remote_Windows_Filesystem_Record;
      Host            : String;
      Local_Full_Name : String) return GNAT.Strings.String_Access
   is
      pragma Unreferenced (FS);
      Args   : GNAT.OS_Lib.Argument_List :=
                 (new String'("type"),
                  new String'("""" & Local_Full_Name & """"));
      Status : Boolean;
      Output : String_Access;

   begin
      Sync_Execute (Host, Args, Output, Status);
      Basic_Types.Free (Args);
      return Output;
   end Read_File;

   ------------
   -- Delete --
   ------------

   function Delete
     (FS              : Remote_Windows_Filesystem_Record;
      Host            : String;
      Local_Full_Name : String) return Boolean
   is
      pragma Unreferenced (FS);
      Args  : GNAT.OS_Lib.Argument_List :=
                (new String'("erase"),
                 new String'("/f"),
                 new String'("""" & Local_Full_Name & """"),
                 new String'("2>&1"));
      Status : Boolean;

   begin
      Sync_Execute (Host, Args, Status);
      Basic_Types.Free (Args);
      return Status;
   end Delete;

   -----------------
   -- Is_Writable --
   -----------------

   function Is_Writable
     (FS              : Remote_Windows_Filesystem_Record;
      Host            : String;
      Local_Full_Name : String) return Boolean
   is
      --  ??? return always false for now: the Write function is not yet
      --  operational. Uncomment the following code as soon as the write
      --  function is ready.
      pragma Unreferenced (FS, Host, Local_Full_Name);
--        pragma Unreferenced (FS);
--        Args : GNAT.OS_Lib.Argument_List :=
--          (new String'("dir"),
--           new String'("/a-r"),
--           new String'("""" & Local_Full_Name & """"));
--        Status : Boolean;

   begin
--        Sync_Execute (Host, Args, Status);
--        Basic_Types.Free (Args);
--        return Status;
      return False;
   end Is_Writable;

   ----------------------
   -- Is_Symbolic_Link --
   ----------------------

   function Is_Symbolic_Link
     (FS              : Remote_Windows_Filesystem_Record;
      Host            : String;
      Local_Full_Name : String) return Boolean
   is
      pragma Unreferenced (FS, Host, Local_Full_Name);
   begin
      --  No symbolic link on windows
      return False;
   end Is_Symbolic_Link;

   ------------------
   -- Is_Directory --
   ------------------

   function Is_Directory
     (FS              : Remote_Windows_Filesystem_Record;
      Host            : String;
      Local_Full_Name : String) return Boolean
   is
      pragma Unreferenced (FS);
      Args : GNAT.OS_Lib.Argument_List :=
               (new String'("dir"),
                new String'("/ad"),
                new String'("""" & Local_Full_Name & """"),
                new String'("2>&1"));
      Status : Boolean;

   begin
      Sync_Execute (Host, Args, Status);
      Basic_Types.Free (Args);
      return Status;
   end Is_Directory;

   ---------------------
   -- File_Time_Stamp --
   ---------------------

   function File_Time_Stamp
     (FS              : Remote_Windows_Filesystem_Record;
      Host            : String;
      Local_Full_Name : String) return Ada.Calendar.Time
   is
      pragma Unreferenced (FS);
      Regexp  : constant Pattern_Matcher
        := Compile ("(\d\d\d\d[-]\d\d[-]\d\d)\s+(\d\d:\d\d:\d\d[.]\d+)\s+");
      Args    : GNAT.OS_Lib.Argument_List :=
                  (new String'("ls"),
                   new String'("-l"),
                   new String'("--time-style=full-iso"),
                   new String'("""" & Local_Full_Name & """"),
                   new String'("2>"),
                   new String'("/dev/null"));
      Status  : Boolean;
      Matched : Match_Array (0 .. 2);
      Output  : String_Access;
      Year    : Natural;
      Month   : Natural;
      Day     : Natural;
      Hour    : Natural;
      Minute  : Natural;
      Second  : Ada.Calendar.Day_Duration;
      use type Ada.Calendar.Day_Duration;

   begin
--
--        declare
--           Reg_Query_Args : constant Argument_List :=
--             (new String'("reg"),
--              new String'("query"),
--              new String'("""HKCU\Control Panel\International"""),
--              new String'("/v"),
--              new String'("sShortDate"));
--           Reg_Match
--        begin
--           Sync_Execute (Host, Reg_Query_Args, Output, Status);
--           Basic_Types.Free (Reg_Query_Args);
--           Match ("REG_SZ\s+([dMy]+)[^dMy]*([dMy]+)[^dMy]*([dMy]+)$",
--        end;

      Sync_Execute (Host, Args, Output, Status);
      Basic_Types.Free (Args);

      if Status then
         Match (Regexp, Output.all, Matched);

         if Matched (0) = No_Match then
            return VFS.No_Time;
         end if;
         Year := Natural'Value
           (Output (Matched (1).First .. Matched (1).First + 4));
         Month := Natural'Value
           (Output (Matched (1).First + 6 .. Matched (1).First + 7));
         Day := Natural'Value
           (Output (Matched (1).First + 9 .. Matched (1).First + 10));
         Hour := Natural'Value
           (Output (Matched (2).First .. Matched (2).First + 1));
         Minute := Natural'Value
           (Output (Matched (2).First + 3 .. Matched (2).First + 4));
         Second := Ada.Calendar.Day_Duration'Value
           (Output (Matched (2).First + 6 .. Matched (2).Last));
         Second := Second
           + (60.0 * Ada.Calendar.Day_Duration (Minute))
           + (3600.0 * Ada.Calendar.Day_Duration (Hour));
         return Ada.Calendar.Time_Of (Year, Month, Day, Second);
      end if;

      return VFS.No_Time;
   end File_Time_Stamp;

   -----------
   -- Write --
   -----------

   procedure Write
     (FS              : Remote_Windows_Filesystem_Record;
      Host            : String;
      Local_Full_Name : String;
      Temporary_File  : String;
      Append          : Boolean := False)
   is
      pragma Unreferenced (FS, Host, Local_Full_Name, Temporary_File, Append);
   begin
      null;
      --  ??? TODO !!!!!!!!!!!!!!!!!!!!!!!!!!
   end Write;

   ------------------
   -- Set_Writable --
   ------------------

   procedure Set_Writable
     (FS              : Remote_Windows_Filesystem_Record;
      Host            : String;
      Local_Full_Name : String;
      Writable        : Boolean)
   is
      pragma Unreferenced (FS);
      Args   : GNAT.OS_Lib.Argument_List :=
                 (new String'("attrib"),
                  new String'("-r"),
                  new String'("""" & Local_Full_Name & """"),
                  new String'("2>&1"));
      Status : Boolean;
      pragma Unreferenced (Status);

   begin
      if not Writable then
         Args (2).all := "+r";
      end if;

      Sync_Execute (Host, Args, Status);
      Basic_Types.Free (Args);
   end Set_Writable;

   ------------------
   -- Set_Readable --
   ------------------

   procedure Set_Readable
     (FS              : Remote_Windows_Filesystem_Record;
      Host            : String;
      Local_Full_Name : String;
      Readable        : Boolean)
   is
      pragma Unreferenced (FS, Host, Local_Full_Name, Readable);
   begin
      null;
   end Set_Readable;

   ------------------------
   -- Get_Logical_Drives --
   ------------------------

   procedure Get_Logical_Drives
     (FS     : Remote_Windows_Filesystem_Record;
      Host   : String;
      Buffer : in out String;
      Len    :    out Integer)
   is
      pragma Unreferenced (FS);
      Status : Boolean;
   begin
      Len := Buffer'First - 1;

      for Drive in Character'('C') .. Character'('Z') loop
         declare
            Args : GNAT.OS_Lib.Argument_List :=
              (new String'("vol"),
               new String'(Drive & ":"),
               new String'("2>&1"));
         begin
            Sync_Execute (Host, Args, Status);
            Basic_Types.Free (Args);
         end;

         if Status then
            Len := Len + 3;
            Buffer (Len - 2 .. Len) := Drive & ":" & ASCII.NUL;
         end if;
      end loop;
   end Get_Logical_Drives;

   --------------
   -- Make_Dir --
   --------------

   function Make_Dir
     (FS             : Remote_Windows_Filesystem_Record;
      Host           : String;
      Local_Dir_Name : String) return Boolean
   is
      pragma Unreferenced (FS);
      Args   : GNAT.OS_Lib.Argument_List :=
                 (new String'("mkdir"),
                  new String'("""" & Local_Dir_Name & """"),
                  new String'("2>&1"));
      Status : Boolean;

   begin
      Sync_Execute (Host, Args, Status);
      Basic_Types.Free (Args);
      return Status;
   end Make_Dir;

   ----------------
   -- Remove_Dir --
   ----------------

   function Remove_Dir
     (FS             : Remote_Windows_Filesystem_Record;
      Host           : String;
      Local_Dir_Name : String;
      Recursive      : Boolean) return Boolean
   is
      pragma Unreferenced (FS);
      Args   : GNAT.OS_Lib.Argument_List :=
                 (1 => new String'("rmdir"),
                  2 => new String'("/q"),
                  3 => new String'("""" & Local_Dir_Name & """"),
                  4 => new String'("2>&1"));
      Status : Boolean;

   begin
      if Recursive then
         Free (Args (2));
         Args (2) := new String'("/q/s");
      end if;

      Sync_Execute (Host, Args, Status);
      Basic_Types.Free (Args);
      return Status;
   end Remove_Dir;

   --------------
   -- Read_Dir --
   --------------

   function Read_Dir
     (FS             : Remote_Windows_Filesystem_Record;
      Host           : String;
      Local_Dir_Name : String;
      Dirs_Only      : Boolean := False;
      Files_Only     : Boolean := False) return GNAT.Strings.String_List
   is
      pragma Unreferenced (FS);
      function Create_Args return GNAT.OS_Lib.Argument_List;
      --  Return dir arguments following the Dirs_Only and Files_Only arguments

      -----------------
      -- Create_Args --
      -----------------

      function Create_Args return GNAT.OS_Lib.Argument_List is
      begin
         if Dirs_Only then
            return (new String'("dir"),
                    new String'("/ad"),
                    new String'("/b"),
                    new String'(Local_Dir_Name),
                    new String'("2>&1"));
         elsif Files_Only then
            return (new String'("dir"),
                    new String'("/a-d"),
                    new String'("/b"),
                    new String'(Local_Dir_Name),
                    new String'("2>&1"));
         else
            return (new String'("dir"),
                    new String'("/b"),
                    new String'(Local_Dir_Name),
                    new String'("2>&1"));
         end if;
      end Create_Args;

      Args     : GNAT.OS_Lib.Argument_List := Create_Args;
      Status   : Boolean;
      Output   : String_Access;
      Regexp   : constant Pattern_Matcher :=
                   Compile ("^(.+)$", Multiple_Lines);
      Matched  : Match_Array (0 .. 1);
      Index    : Natural;
      Nb_Files : Natural;

   begin
      Sync_Execute (Host, Args, Output, Status);
      Basic_Types.Free (Args);

      if Status then
         Index    := Output'First;
         Nb_Files := 0;

         while Index <= Output'Last loop
            Match (Regexp, Output.all, Matched, Index);
            exit when Matched (0) = No_Match;
            Index := Matched (1).Last + 1;

            if Output (Matched (1).First .. Matched (1).Last) /= "."
              and then Output (Matched (1).First .. Matched (1).Last) /= ".."
            then
               Nb_Files := Nb_Files + 1;
            end if;
         end loop;

         declare
            List     : String_List (1 .. Nb_Files);
            File_Idx : Natural;
         begin
            Index    := Output'First;
            File_Idx := List'First;

            while Index /= Output'Last loop
               Match (Regexp, Output.all, Matched, Index);
               exit when Matched (0) = No_Match;
               Index := Matched (1).Last + 1;

               if Output (Matched (1).First .. Matched (1).Last) /= "."
                 and then Output (Matched (1).First .. Matched (1).Last)
                           /= ".."
               then
                  List (File_Idx) := new String'
                    (Output (Matched (1).First .. Matched (1).Last));
                  File_Idx := File_Idx + 1;
               end if;
            end loop;

            return List;
         end;
      end if;

      return (1 .. 0 => null);
   end Read_Dir;

   ------------
   -- Rename --
   ------------

   function Rename
     (FS              : Remote_Windows_Filesystem_Record;
      Host            : String;
      From_Local_Name : String;
      To_Local_Name   : String) return Boolean
   is
      pragma Unreferenced (FS);
      Args  : GNAT.OS_Lib.Argument_List :=
                (new String'("ren"),
                 new String'("""" & From_Local_Name & """"),
                 new String'("""" & To_Local_Name & """"),
                 new String'("2>&1"));
      Status : Boolean;
   begin
      Sync_Execute (Host, Args, Status);
      Basic_Types.Free (Args);
      return Status;
   end Rename;

   ----------
   -- Copy --
   ----------

   function Copy
     (FS              : Remote_Windows_Filesystem_Record;
      Host            : String;
      From_Local_Name : String;
      To_Local_Name   : String) return Boolean
   is
      pragma Unreferenced (FS);
      Args  : GNAT.OS_Lib.Argument_List :=
                (new String'("copy"),
                 new String'("""" & From_Local_Name & """"),
                 new String'("""" & To_Local_Name & """"),
                 new String'("2>&1"));
      Status : Boolean;
   begin
      Sync_Execute (Host, Args, Status);
      Basic_Types.Free (Args);
      return Status;
   end Copy;

   ----------------
   -- Change_Dir --
   ----------------

   function Change_Dir
     (FS             : Remote_Windows_Filesystem_Record;
      Host           : String;
      Local_Dir_Name : String) return Boolean
   is
      Args  : GNAT.OS_Lib.Argument_List :=
                (new String'("cd"),
                 new String'("""" & Local_Dir_Name & """"));
      Status : Boolean;
      pragma Unreferenced (FS);
   begin
      Sync_Execute (Host, Args, Status);
      Basic_Types.Free (Args);
      return Status;
   end Change_Dir;

end Filesystem.Windows.Remote;
