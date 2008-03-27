-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                   Copyright (C) 2001-2008, AdaCore                --
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
with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with Filesystem;                use Filesystem;
with Filesystem.Queries;        use Filesystem.Queries;
with GNAT.Calendar;             use GNAT.Calendar;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with String_Utils;              use String_Utils;
with OS_Utils;                  use OS_Utils;
with VFS;                       use VFS;
with Traces; use Traces;

package body File_Utils is
   Me : constant Debug_Handle := Create ("File_Utils");

   ------------------------
   -- Get_Logical_Drives --
   ------------------------

   procedure Get_Logical_Drive_Strings
     (Buffer : out String;
      Len    : out Natural)
   is
      function Internal
        (Buffer : System.Address;
         Length : Integer) return Integer;
      pragma Import (C, Internal, "__gps_get_logical_drive_strings");

   begin
      Len := Internal (Buffer'Address, Buffer'Length);
   end Get_Logical_Drive_Strings;

   --------------------------
   -- Subdirectories_Count --
   --------------------------

   function Subdirectories_Count (Directory : String) return Integer is
      function C_Subdirectories_Count (Dir : String) return Integer;
      pragma Import (C, C_Subdirectories_Count, "__gps_subdirectories_count");

   begin
      return C_Subdirectories_Count (Directory & ASCII.NUL);
   end Subdirectories_Count;

   --------------------------
   -- Read_Files_From_Dirs --
   --------------------------

   function Read_Files_From_Dirs
     (Dirs : String) return File_Array_Access
   is
      Dir          : Dir_Type;
      File         : String (1 .. 1024);
      File_Last    : Natural;
      Tmp          : File_Array_Access;
      Result       : File_Array_Access := new File_Array (1 .. 128);
      Result_Index : Natural := Result'First;
      Iter         : Path_Iterator := Start (Dirs);

   begin
      while not At_End (Dirs, Iter) loop
         declare
            Normalized_Dir : constant String := Name_As_Directory
              (Current (Dirs, Iter));
         begin
            if Normalized_Dir /= "" then
               Open (Dir, Normalized_Dir);

               loop
                  Read (Dir, File, File_Last);
                  exit when File_Last = 0;

                  if Is_Regular_File
                    (Normalized_Dir & File (File'First .. File_Last))
                  then
                     --  We just need one more item in the array, so doubling
                     --  is more than enough, we do not need to loop until the
                     --  length is correct

                     if Result_Index > Result'Last then
                        Tmp := Result;
                        Result := new File_Array (1 .. Result'Last * 2);
                        Result (1 .. Tmp'Last) := Tmp.all;
                        Unchecked_Free (Tmp);
                     end if;

                     Result (Result_Index) := Create
                       (Full_Filename =>
                          Normalized_Dir & File (File'First .. File_Last));
                     Result_Index := Result_Index + 1;
                  end if;
               end loop;

               Close (Dir);
            end if;

         exception
            when Directory_Error =>
               null;
         end;

         Iter := Next (Dirs, Iter);
      end loop;

      Tmp        := Result;
      Result     := new File_Array (1 .. Result_Index - 1);
      Result.all := Tmp (1 .. Result_Index - 1);
      Unchecked_Free (Tmp);

      return Result;
   end Read_Files_From_Dirs;

   -----------------------
   -- Is_Case_Sensitive --
   -----------------------

   function Is_Case_Sensitive
     (Server : Server_Type := GPS_Server) return Boolean
   is
      function Internal return Integer;
      pragma Import (C, Internal, "__gnat_get_file_names_case_sensitive");
   begin
      if Is_Local (Server) then
         return Boolean'Val (Internal);
      else
         return Is_Case_Sensitive
           (Get_Filesystem (Get_Nickname (Server)));
      end if;
   end Is_Case_Sensitive;

   ----------------
   -- File_Equal --
   ----------------

   function File_Equal
     (File1, File2 : String;
      Server : Server_Type := GPS_Server) return Boolean is
   begin
      return Equal
        (File1, File2, Case_Sensitive => Is_Case_Sensitive (Server));
   end File_Equal;

   ------------------
   -- To_File_Name --
   ------------------

   function To_File_Name (Name : String) return String is
      Result : String (1 .. Name'Length) := To_Lower (Name);
   begin
      for J in Result'First .. Result'Last loop
         if Result (J) = '.' then
            Result (J) := '-';
         end if;
      end loop;

      return Result;
   end To_File_Name;

   ----------------------
   -- To_Host_Pathname --
   ----------------------

   function To_Host_Pathname (Path : String) return String is
      Cygdrv : constant String := "cygdrive";
   begin
      if GNAT.OS_Lib.Directory_Separator = '/' then
         return Path;
      end if;

      --  Replace /cygdrive/x/ by x:\

      if Path'Length > Cygdrv'Length + 3
        and then Is_Directory_Separator (Path (Path'First))
        and then Path (Path'First + 1 .. Path'First + Cygdrv'Length) = Cygdrv
        and then Is_Directory_Separator (Path (Path'First + Cygdrv'Length + 1))
        and then Is_Directory_Separator (Path (Path'First + Cygdrv'Length + 3))
      then
         return
            Path (Path'First + Cygdrv'Length + 2) & ":\" &
            Path (Path'First + Cygdrv'Length + 4 .. Path'Last);
      else
         return Path;
      end if;
   end To_Host_Pathname;

   ----------------------
   -- To_Unix_Pathname --
   ----------------------

   function To_Unix_Pathname (Path : String) return String is
      Result : String (Path'Range);
   begin
      if GNAT.OS_Lib.Directory_Separator = '/' then
         return Path;
      end if;

      for J in Result'Range loop
         if Path (J) = GNAT.OS_Lib.Directory_Separator then
            Result (J) := '/';
         else
            Result (J) := Path (J);
         end if;
      end loop;

      return Result;
   end To_Unix_Pathname;

   -------------
   -- Shorten --
   -------------

   function Shorten
     (Path    : String;
      Max_Len : Natural := 40) return String
   is
      Len : constant Natural := Path'Length;
   begin
      if Len <= Max_Len then
         return Path;
      else
         declare
            Prefix       : constant String  := "[...]";
            Search_Start : constant Natural
              := Path'Last - Max_Len + Prefix'Length;
            New_Start    : Natural;
         begin
            if Search_Start > Path'Last then
               --  Max_Len < Prefix'Length
               --  Shorten anyway, but might give a strange result
               return Path (Path'Last - Max_Len .. Path'Last);
            end if;

            New_Start := Index (Path (Search_Start .. Path'Last), "/");

            if New_Start = 0 and New_Start not in Path'Range then
               --  Shorten anyway (but it might not make sense)
               New_Start := Search_Start;
            end if;

            return (Prefix & Path (New_Start .. Path'Last));
         end;
      end if;
   end Shorten;

   --------------------
   -- Suffix_Matches --
   --------------------

   function Suffix_Matches
     (File_Name : String; Suffix : String) return Boolean
   is
      pragma Suppress (All_Checks);
   begin
      --  This version is slightly faster than checking
      --     return Tail (File_Name, Suffix'Length) = Suffix;
      --  which needs a function returning a string.

      if File_Name'Length < Suffix'Length then
         return False;
      end if;

      --  Do the loop in reverse, since it likely that Suffix starts with '.'
      --  In the GPS case, it is also often the case that suffix starts with
      --  '.ad' for Ada extensions
      for J in reverse Suffix'Range loop
         if File_Name (File_Name'Last + J - Suffix'Last) /= Suffix (J) then
            return False;
         end if;
      end loop;

      return True;
   end Suffix_Matches;

   -----------------------
   -- Name_As_Directory --
   -----------------------

   function Name_As_Directory
     (Name  : String;
      Style : GNAT.Directory_Operations.Path_Style :=
        GNAT.Directory_Operations.System_Default) return String
   is
      Dir : constant String := Format_Pathname (Name, Style);

   begin
      if Dir = "" then
         return "";

      elsif Style = UNIX
        and then Dir (Dir'Last) /= '/'
      then
         return Dir & '/';

      elsif Style = DOS
        and then Dir (Dir'Last) /= '\'
      then
         return Dir & '\';

      elsif Style = System_Default
        and then Dir (Dir'Last) /= Dir_Separator
      then
         return Dir & Dir_Separator;

      else
         return Dir;
      end if;
   end Name_As_Directory;

   ------------------------
   -- Relative_Path_Name --
   ------------------------

   function Relative_Path_Name
     (File_Name, Base_Name : String;
      Server    : Server_Type := GPS_Server) return String
   is
      Base       : constant String := Name_As_Directory
        (Normalize_Pathname (Base_Name, Resolve_Links => False));
      File       : constant String :=
        Normalize_Pathname (File_Name, Resolve_Links => False);
      Level      : Natural := 0;
      Base_End   : Natural := Base'Last;
      Length     : Natural;
      Parent_Dir : constant String := ".." & Directory_Separator;

   begin
      if File_Equal (File, Base, Server)
        or else File_Equal (File, Base (Base'First .. Base'Last - 1), Server)
      then
         return ".";
      end if;

      --  Do not use >=, since if only '/' is the common part, we want to use
      --  an absolute path instead.

      while Base_End > Base'First loop
         Length := Base_End - Base'First + 1;

         if File'Length >= Length
           and then File_Equal
             (File (File'First .. File'First + Length - 1),
              Base (Base'First .. Base_End),
              Server)
         then
            return (Level * Parent_Dir) & File
              (File'First + Length .. File'Last);

         --  Else try without the last directory separator
         elsif File'Length = Length - 1
           and then File_Equal
             (File, Base (Base'First .. Base_End - 1), Server)
         then
            return (Level * Parent_Dir) & File
              (File'First + Length .. File'Last);
         end if;

         --  Look for the parent directory.
         Level := Level + 1;
         loop
            Base_End := Base_End - 1;
            exit when Base_End < Base'First
              or else Base (Base_End) = Directory_Separator;
         end loop;
      end loop;

      return File;
   end Relative_Path_Name;

   -----------
   -- Start --
   -----------

   function Start (Path : String) return Path_Iterator is
   begin
      return Next (Path, (First => 0, Last => Path'First - 1));
   end Start;

   ----------
   -- Next --
   ----------

   function Next (Path : String; Iter : Path_Iterator) return Path_Iterator is
      Pos : Natural := Iter.Last + 1;
   begin
      while Pos <= Path'Last
        and then Path (Pos) /= Path_Separator
      loop
         Pos := Pos + 1;
      end loop;

      return (First => Iter.Last + 1, Last => Pos);
   end Next;

   -------------
   -- Current --
   -------------

   function Current (Path : String; Iter : Path_Iterator) return String is
   begin
      if Iter.First <= Path'Last then
         return Path (Iter.First .. Iter.Last - 1);
      else
         return "";
      end if;
   end Current;

   ------------
   -- At_End --
   ------------

   function At_End (Path : String; Iter : Path_Iterator) return Boolean is
   begin
      return Iter.First > Path'Last;
   end At_End;

   -----------------------------
   -- Is_Absolute_Path_Or_URL --
   -----------------------------

   function Is_Absolute_Path_Or_URL (Name : String) return Boolean is
      Index : Natural;
   begin
      if Is_Absolute_Path (Name) then
         return True;
      end if;

      Index := Name'First;
      while Index <= Name'Last - 3
        and then Name (Index) /= ':'
      loop
         Index := Index + 1;
      end loop;

      return Index <= Name'Last - 3
        and then Name (Index .. Index + 2) = "://";
   end Is_Absolute_Path_Or_URL;

   ---------------------
   -- File_Time_Stamp --
   ---------------------

   function File_Time_Stamp (File : String) return Ada.Calendar.Time is
      T      : constant OS_Time := File_Time_Stamp (File);
      Year   : Year_Type;
      Month  : Month_Type;
      Day    : Day_Type;
      Hour   : Hour_Type;
      Minute : Minute_Type;
      Second : Second_Type;
   begin
      GM_Split (T, Year, Month, Day, Hour, Minute, Second);

      if T = Invalid_Time then
         return VFS.No_Time;
      end if;

      return GNAT.Calendar.Time_Of
        (Year   => Year,
         Month  => Month,
         Day    => Day,
         Hour   => Hour,
         Minute => Minute,
         Second => Second);
   end File_Time_Stamp;

   ------------------
   -- Find_On_Path --
   ------------------

   function Find_On_Path
     (Base_Name : String; Path : String) return VFS.Virtual_File
   is
      Iter : Path_Iterator := Start (Path);
   begin
      while not At_End (Path, Iter) loop
         declare
            S : constant String := Name_As_Directory (Current (Path, Iter))
              & Base_Name;
         begin
            Trace (Me, "MANU Testing " & S);
            if Is_Regular_File (S) then
               return Create (Full_Filename => S);
            end if;
         end;
         Iter := Next (Path, Iter);
      end loop;
      return VFS.No_File;
   end Find_On_Path;

end File_Utils;
