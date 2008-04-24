-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2006-2008, AdaCore           --
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

with Ada.Directories;           use Ada.Directories;
with Ada.Unchecked_Deallocation;
with Basic_Types;               use Basic_Types;
with GNATCOLL.Mmap;             use GNATCOLL.Mmap;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with File_Utils;                use File_Utils;
with String_Utils;              use String_Utils;

package body Filesystem is

   ----------------
   -- Is_Subtree --
   ----------------

   function Is_Subtree
     (FS        : Filesystem_Record;
      Directory : String;
      Full_Path : String) return Boolean is
   begin
      --  Path length shall be greater or equal to directory length
      if Directory'Length > Full_Path'Length then
         return False;
      end if;

      --  Do not try to compare last character: on VMS, you will compare
      --  a closing bracket with a dot (disk:[path] with disk:[path.subpath])
      return Equal
        (Full_Path (Full_Path'First .. Full_Path'First + Directory'Length - 1),
         Directory,
         Is_Case_Sensitive (Filesystem_Record'Class (FS)));
   end Is_Subtree;

   --------------------
   -- File_Extension --
   --------------------

   function File_Extension
     (FS   : Filesystem_Record;
      Path : String) return String
   is
      pragma Unreferenced (FS);
   begin
      for J in reverse Path'Range loop
         if Path (J) = '.' then
            return Path (J .. Path'Last);
         end if;
      end loop;
      return "";
   end File_Extension;

   ------------
   -- Concat --
   ------------

   function Concat
     (FS   : Filesystem_Record;
      Root : String;
      Sub  : String) return String
   is
      pragma Unreferenced (FS);
   begin
      return Root & Sub;
   end Concat;

   ---------------------------
   -- Multi_Unit_Index_Char --
   ---------------------------

   function Multi_Unit_Index_Char
     (FS : Filesystem_Record) return Character
   is
      pragma Unreferenced (FS);
   begin
      return '~';
   end Multi_Unit_Index_Char;

   ---------------
   -- Base_Name --
   ---------------

   function Base_Name
     (FS     : Filesystem_Record;
      Path   : String;
      Suffix : String := "") return String is
   begin
      for J in reverse Path'Range loop
         if Path (J) = GNAT.OS_Lib.Directory_Separator then
            if Equal (Path (Path'Last - Suffix'Length + 1 .. Path'Last),
                      Suffix,
                      Is_Case_Sensitive (Filesystem_Record'Class (FS))) then
               return Path (J + 1 .. Path'Last - Suffix'Length);
            else
               return Path (J + 1 .. Path'Last);
            end if;
         end if;
      end loop;

      return Path;
   end Base_Name;

   -------------------
   -- Base_Dir_Name --
   -------------------

   function Base_Dir_Name
     (FS  : Filesystem_Record;
      Path : String) return String is
   begin
      if Path'Length > 1 and then Path (Path'Last) = Directory_Separator then
         return Base_Name (FS, Path (Path'First .. Path'Last - 1));
      else
         return Base_Name (FS, Path);
      end if;
   end Base_Dir_Name;

   --------------
   -- Dir_Name --
   --------------

   function Dir_Name
     (FS   : Filesystem_Record;
      Path : String) return String
   is
      pragma Unreferenced (FS);
   begin
      for J in reverse Path'Range loop
         if Path (J) = Directory_Separator then
            return Path (Path'First .. J);
         end if;
      end loop;

      return "";
   end Dir_Name;

   ----------------
   -- Get_Parent --
   ----------------

   function Get_Parent
     (FS   : Filesystem_Record;
      Path : String) return String is
   begin
      if Path (Path'Last) = Directory_Separator then
         return Dir_Name (FS, Path (Path'First .. Path'Last - 1));
      else
         return Dir_Name (FS, Path);
      end if;
   end Get_Parent;

   ----------------------
   -- Ensure_Directory --
   ----------------------

   function Ensure_Directory
     (FS   : Filesystem_Record;
      Path : String) return String
   is
      pragma Unreferenced (FS);
   begin
      if Path'Length = 0 or else Path (Path'Last) /= Directory_Separator then
         return Path & Directory_Separator;
      else
         return Path;
      end if;
   end Ensure_Directory;

   ---------------
   -- Normalize --
   ---------------

   function Normalize
     (FS   : Filesystem_Record;
      Path : String) return String
   is
      Last_Dir : Natural;
   begin
      Last_Dir := Path'First;

      for J in Path'Range loop
         if Path (J) = Directory_Separator then
            if J < Path'Last - 3
              and then Path (J .. J + 3) =
                 Directory_Separator & ".." & Directory_Separator
            then
               return Normalize (FS, Path (Path'First .. Last_Dir) &
                                     Path (J + 3 .. Path'Last));

            elsif J < Path'Last - 2
              and then Path (J .. J + 2) =
                 Directory_Separator & '.' & Directory_Separator
            then
               return Normalize (FS, Path (Path'First .. J) &
                                     Path (J + 2 .. Path'Last));
            end if;

            Last_Dir := J;
         end if;
      end loop;

      return Path;
   end Normalize;

   --------------
   -- Home_Dir --
   --------------

   function Home_Dir
     (FS   : Filesystem_Record;
      Host : String) return String
   is
      HOME : GNAT.Strings.String_Access := Getenv ("HOME");
   begin
      if HOME = null then
         return Get_Root (Filesystem_Record'Class (FS), Host);
      else
         declare
            Result : constant String := HOME.all;
         begin
            Free (HOME);
            return Result;
         end;
      end if;
   end Home_Dir;

   ---------------------
   -- Is_Regular_File --
   ---------------------

   function Is_Regular_File
     (FS              : Filesystem_Record;
      Host            : String;
      Local_Full_Name : String) return Boolean
   is
      pragma Unreferenced (FS, Host);
   begin
      return Is_Regular_File (Local_Full_Name);
   end Is_Regular_File;

   ---------------
   -- Read_File --
   ---------------

   function Read_File
     (FS              : Filesystem_Record;
      Host            : String;
      Local_Full_Name : String) return GNAT.Strings.String_Access
   is
      pragma Unreferenced (FS, Host);
   begin
      return Read_Whole_File (Local_Full_Name);
   end Read_File;

   ------------
   -- Delete --
   ------------

   function Delete
     (FS              : Filesystem_Record;
      Host            : String;
      Local_Full_Name : String) return Boolean
   is
      pragma Unreferenced (FS, Host);
      Success : Boolean;
   begin
      Delete_File (Local_Full_Name, Success);
      return Success;
   end Delete;

   -----------------
   -- Is_Writable --
   -----------------

   function Is_Writable
     (FS              : Filesystem_Record;
      Host            : String;
      Local_Full_Name : String) return Boolean
   is
      pragma Unreferenced (FS, Host);
   begin
      return Is_Writable_File (Local_Full_Name);
   end Is_Writable;

   ------------------
   -- Is_Directory --
   ------------------

   function Is_Directory
     (FS              : Filesystem_Record;
      Host            : String;
      Local_Full_Name : String) return Boolean
   is
      pragma Unreferenced (FS, Host);
   begin
      return Is_Directory (Local_Full_Name);
   end Is_Directory;

   ---------------------
   -- File_Time_Stamp --
   ---------------------

   function File_Time_Stamp
     (FS              : Filesystem_Record;
      Host            : String;
      Local_Full_Name : String)
      return Ada.Calendar.Time
   is
      pragma Unreferenced (FS, Host);
   begin
      return File_Utils.File_Time_Stamp (Local_Full_Name);
   end File_Time_Stamp;

   -----------
   -- Write --
   -----------

   procedure Write
     (FS              : Filesystem_Record;
      Host            : String;
      Local_Full_Name : String;
      Temporary_File  : String;
      Append          : Boolean := False)
   is
      Success : Boolean;
      pragma Unreferenced (FS, Host, Success);
   begin
      if Append then
         Copy_File (Temporary_File, Pathname => Local_Full_Name,
                    Success => Success,
                    Mode => GNAT.OS_Lib.Append);
      else
         Copy_File (Temporary_File, Pathname => Local_Full_Name,
                    Success => Success,
                    Mode => Overwrite);
      end if;
   end Write;

   ------------------
   -- Set_Writable --
   ------------------

   procedure Set_Writable
     (FS              : Filesystem_Record;
      Host            : String;
      Local_Full_Name : String;
      Writable        : Boolean)
   is
      pragma Unreferenced (FS, Host);
   begin
      if Writable then
         Set_Writable (Local_Full_Name);
      else
         Set_Read_Only (Local_Full_Name);
      end if;
   end Set_Writable;

   ------------------
   -- Set_Readable --
   ------------------

   procedure Set_Readable
     (FS              : Filesystem_Record;
      Host            : String;
      Local_Full_Name : String;
      Readable        : Boolean)
   is
      pragma Unreferenced (FS, Host);
      procedure Internal (File : String; Set : Integer);
      pragma Import (C, Internal, "__gps_set_readable");

   begin
      Internal (Local_Full_Name & ASCII.NUL, Boolean'Pos (Readable));
   end Set_Readable;

   ----------------
   -- Remove_Dir --
   ----------------

   function Remove_Dir
     (FS             : Filesystem_Record;
      Host           : String;
      Local_Dir_Name : String;
      Recursive      : Boolean) return Boolean
   is
      pragma Unreferenced (FS, Host);
   begin
      Remove_Dir (Local_Dir_Name, Recursive);
      return True;
   exception
      when Directory_Error =>
         return False;
   end Remove_Dir;

   --------------
   -- Read_Dir --
   --------------

   function Read_Dir
     (FS             : Filesystem_Record;
      Host           : String;
      Local_Dir_Name : String;
      Dirs_Only      : Boolean := False;
      Files_Only     : Boolean := False) return GNAT.Strings.String_List
   is
      pragma Unreferenced (FS, Host);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (GNAT.Strings.String_List, String_List_Access);

      Nb_Files  : Natural := 0;
      Tmp       : String_List_Access;
      F_Array   : String_List_Access;
      Search    : Search_Type;
      Ent       : Directory_Entry_Type;
      Filter    : Filter_Type := (others => True);

   begin
      if not Is_Directory (Local_Dir_Name) then
         return (1 .. 0 => null);
      end if;

      if Dirs_Only then
         Filter := (Directory => True, others => False);
      elsif Files_Only then
         Filter (Directory) := False;
      end if;

      Start_Search
        (Search,
         Directory => Local_Dir_Name,
         Pattern   => "",
         Filter    => Filter);

      --  let's start with 128 items
      F_Array := new GNAT.Strings.String_List (1 .. 128);

      while More_Entries (Search) loop
         Get_Next_Entry (Search, Ent);

         declare
            Simple : constant String := Simple_Name (Ent);
         begin
            if Simple /= "."
              and then Simple /= ".."
            then
               Nb_Files := Nb_Files + 1;

               --  Array too small, let's double it
               if Nb_Files > F_Array'Last then
                  Tmp := F_Array;
                  F_Array := new GNAT.Strings.String_List (1 .. Tmp'Last * 2);
                  F_Array (1 .. Tmp'Last) := Tmp.all;
                  Unchecked_Free (Tmp);
               end if;

               F_Array (Nb_Files) := new String'(Simple);
            end if;
         end;
      end loop;

      End_Search (Search);

      Tmp := F_Array;

      declare
         F_Array : GNAT.Strings.String_List (1 .. Nb_Files);
      begin
         F_Array := Tmp (1 .. Nb_Files);
         Unchecked_Free (Tmp);
         return F_Array;
      end;

   exception
      when others =>
         End_Search (Search);
         return (1 .. 0 => null);
   end Read_Dir;

   --------------
   -- Make_Dir --
   --------------

   function Make_Dir
     (FS             : Filesystem_Record;
      Host           : String;
      Local_Dir_Name : String) return Boolean
   is
      pragma Unreferenced (FS, Host);
   begin
      Create_Path (Local_Dir_Name);
      return True;
   exception
      when others =>
         return False;
   end Make_Dir;

   ------------
   -- Rename --
   ------------

   function Rename
     (FS              : Filesystem_Record;
      Host            : String;
      From_Local_Name : String;
      To_Local_Name   : String) return Boolean
   is
      pragma Unreferenced (FS, Host);
      Success : Boolean;
   begin
      Rename_File (From_Local_Name, To_Local_Name, Success);
      return Success;
   end Rename;

   ----------
   -- Copy --
   ----------

   function Copy
     (FS              : Filesystem_Record;
      Host            : String;
      From_Local_Name : String;
      To_Local_Name   : String) return Boolean
   is
      pragma Unreferenced (FS, Host);
   begin
      Ada.Directories.Copy_File (From_Local_Name, To_Local_Name);
      return True;
   exception
      when others =>
         return False;
   end Copy;

   --------------
   -- Copy_Dir --
   --------------

   function Copy_Dir
     (FS              : Filesystem_Record;
      Host            : String;
      From_Local_Name : String;
      To_Local_Name   : String) return Boolean
   is
      From   : constant String :=
        Ensure_Directory (Filesystem_Record'Class (FS), From_Local_Name);
      Files_Array : String_List  :=
        Read_Dir (Filesystem_Record'Class (FS), Host, From);
      Target : constant String :=
        Ensure_Directory (Filesystem_Record'Class (FS), To_Local_Name);
   begin
      if not Is_Directory (Filesystem_Record'Class (FS), Host, Target)
        and then
          not Make_Dir (Filesystem_Record'Class (FS), Host, Target)
      then
         Basic_Types.Free (Files_Array);
         return False;
      end if;

      for F in Files_Array'Range loop
         if not Copy
           (Filesystem_Record'Class (FS),
            Host,
            From & Files_Array (F).all,
            Target)
         then
            Basic_Types.Free (Files_Array);
            return False;
         end if;
      end loop;

      Basic_Types.Free (Files_Array);

      return True;

   exception
      when others =>
         Basic_Types.Free (Files_Array);
         return False;
   end Copy_Dir;

   ----------------------
   -- Is_Symbolic_Link --
   ----------------------

   function Is_Symbolic_Link
     (FS              : Filesystem_Record;
      Host            : String;
      Local_Full_Name : String) return Boolean
   is
      pragma Unreferenced (FS, Host);
   begin
      return Is_Symbolic_Link (Local_Full_Name);
   end Is_Symbolic_Link;

   ----------------
   -- Change_Dir --
   ----------------

   function Change_Dir
     (FS             : Filesystem_Record;
      Host           : String;
      Local_Dir_Name : String) return Boolean
   is
      pragma Unreferenced (FS, Host);
   begin
      GNAT.Directory_Operations.Change_Dir (Local_Dir_Name);
      return True;
   exception
      when others =>
         return False;
   end Change_Dir;

end Filesystem;
