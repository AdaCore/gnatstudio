-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
--                            ACT-Europe                             --
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
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with String_Utils;              use String_Utils;
with VFS;                       use VFS;

package body File_Utils is

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
      pragma Import (C, Internal, "__gnat_get_logical_drive_strings");

   begin
      Len := Internal (Buffer'Address, Buffer'Length);
   end Get_Logical_Drive_Strings;

   --------------------------
   -- Subdirectories_Count --
   --------------------------

   function Subdirectories_Count (Directory : String) return Integer is
      function C_Subdirectories_Count (Dir : String) return Integer;
      pragma Import (C, C_Subdirectories_Count, "__gnat_subdirectories_count");

   begin
      return C_Subdirectories_Count (Directory & ASCII.NUL);
   end Subdirectories_Count;

   --------------------------
   -- Read_Files_From_Dirs --
   --------------------------

   function Read_Files_From_Dirs
     (Dirs : String) return File_Array_Access
   is
      First, Last  : Natural := Dirs'First;
      Dir          : Dir_Type;
      File         : String (1 .. 1024);
      File_Last    : Natural;
      Tmp          : File_Array_Access;
      Result       : File_Array_Access := new File_Array (1 .. 128);
      Result_Index : Natural := Result'First;

   begin
      --  Avoid case where Dirs starts with a path separator

      if First <= Dirs'Last and then Dirs (First) = Path_Separator then
         First := First + 1;
      end if;

      while First <= Dirs'Last loop
         Last := First + 1;

         while Last <= Dirs'Last
           and then Dirs (Last) /= Path_Separator
         loop
            Last := Last + 1;
         end loop;

         if Dirs (First .. Last - 1) /= "" then
            declare
               Normalized_Dir : constant String := Name_As_Directory
                 (Dirs (First .. Last - 1));
            begin
               Open (Dir, Dirs (First .. Last - 1));

               loop
                  Read (Dir, File, File_Last);
                  exit when File_Last = 0;

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
               end loop;

               Close (Dir);

            exception
               when Directory_Error =>
                  null;
            end;
         end if;

         First := Last + 1;
      end loop;

      Tmp := Result;
      Result := new File_Array (1 .. Result_Index - 1);
      Result.all := Tmp (1 .. Result_Index - 1);
      Unchecked_Free (Tmp);

      return Result;
   end Read_Files_From_Dirs;

   ----------------------------------
   -- Filenames_Are_Case_Sensitive --
   ----------------------------------

   function Filenames_Are_Case_Sensitive return Boolean is
      function Internal return Integer;
      pragma Import (C, Internal, "__gnat_get_file_names_case_sensitive");
   begin
      return Boolean'Val (Internal);
   end Filenames_Are_Case_Sensitive;

   ----------------
   -- File_Equal --
   ----------------

   function File_Equal (File1, File2 : String) return Boolean is
   begin
      if Filenames_Are_Case_Sensitive then
         return File1 = File2;
      else
         return Case_Insensitive_Equal (File1, File2);
      end if;
   end File_Equal;

   ------------------
   -- Set_Writable --
   ------------------

   procedure Set_Writable
     (File     : VFS.Virtual_File;
      Writable : Boolean)
   is
      procedure Internal (File : String; Set : Integer);
      pragma Import (C, Internal, "__gnat_set_writable");

   begin
      Internal (Locale_Full_Name (File) & ASCII.NUL, Boolean'Pos (Writable));
   end Set_Writable;

   ------------------
   -- Set_Readable --
   ------------------

   procedure Set_Readable
     (File     : String;
      Readable : Boolean)
   is
      procedure Internal (File : String; Set : Integer);
      pragma Import (C, Internal, "__gnat_set_readable");

   begin
      Internal (File & ASCII.NUL, Boolean'Pos (Readable));
   end Set_Readable;

end File_Utils;
