------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2018, AdaCore                     --
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

with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Directories;           use Ada.Directories;

with GNAT.Case_Util;
with GNAT.Directory_Operations; use GNAT, GNAT.Directory_Operations;

with GNATCOLL.VFS;              use GNATCOLL.VFS;
with GNATCOLL.VFS_Utils;        use GNATCOLL.VFS_Utils;

with Config;

package body OS_Utils is

   ---------------------
   -- Create_Tmp_File --
   ---------------------

   function Create_Tmp_File return GNATCOLL.VFS.Virtual_File is
      Current_Dir : constant Virtual_File := Get_Current_Dir;
      Temp_Dir    : constant Virtual_File := Get_Tmp_Directory;
      Fd          : File_Descriptor;
      Base        : String_Access;

   begin
      Change_Dir (Temp_Dir);
      Create_Temp_File (Fd, Base);
      Close (Fd);

      declare
         Result : constant Filesystem_String := +Base.all;
      begin
         Free (Base);
         Change_Dir (Current_Dir);

         return Create_From_Dir (Temp_Dir, Result);
      end;
   end Create_Tmp_File;

   ------------------
   -- Max_Path_Len --
   ------------------

   Max_Path : Integer;
   pragma Import (C, Max_Path, "max_path_len");
   --  Take advantage of max_path_len defined in the GNAT run time

   function Max_Path_Len return Natural is
   begin
      return Max_Path;
   end Max_Path_Len;

   ------------------------
   -- Make_Dir_Recursive --
   ------------------------

   procedure Make_Dir_Recursive (Name : Virtual_File) is
      Parent : constant Virtual_File := Get_Parent (Name);

   begin
      if Parent = No_File or else Is_Directory (Name) then
         return;
      else
         Make_Dir_Recursive (Parent);
         Name.Make_Dir;
      end if;
   end Make_Dir_Recursive;

   --------------
   -- New_Line --
   --------------

   EOL : aliased constant String := (1 => ASCII.LF);

   procedure New_Line (File : File_Descriptor; Count : Natural := 1) is
      N : Integer;
      pragma Unreferenced (N);
   begin
      for J in 1 .. Count loop
         N := Write (File, EOL'Address, EOL'Length);
      end loop;
   end New_Line;

   ---------
   -- Put --
   ---------

   procedure Put (File : File_Descriptor; Str : String) is
      N : Integer;
      pragma Unreferenced (N);
   begin
      if Str'Length /= 0 then
         N := Write (File, Str (Str'First)'Address, Str'Length);
      end if;
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (File : File_Descriptor; Str : String) is
      N : Integer;
      pragma Unreferenced (N);
   begin
      if Str'Length /= 0 then
         N := Write (File, Str (Str'First)'Address, Str'Length);
      end if;

      N := Write (File, EOL'Address, EOL'Length);
   end Put_Line;

   --------------------
   -- Is_Cygwin_Path --
   --------------------

   function Is_Cygwin_Path (Path : Filesystem_String) return Boolean is
      Cygdrive : constant Filesystem_String := "/cygdrive/";
   begin
      return Path'Length > Cygdrive'Length + 1
        and then
      Path (Path'First .. Path'First + Cygdrive'Length - 1) = Cygdrive
        and then Is_Letter (Path (Path'First + Cygdrive'Length))
        and then Path (Path'First + Cygdrive'Length + 1) = '/';
   end Is_Cygwin_Path;

   ---------------------
   -- Format_Pathname --
   ---------------------

   function Format_Pathname
     (Path  : Filesystem_String;
      Style : Path_Style := System_Default) return Filesystem_String
   is
      use type Config.Host_Type;

      function Cygwin_To_Dos
        (Path : Filesystem_String) return Filesystem_String;
      --  Convert the /cygdrive/<drive>/ prefix to the DOS <drive>:\ equivalent
      --  and convert a forward slashes to backward slashes.

      -------------------
      -- Cygwin_To_Dos --
      -------------------

      function Cygwin_To_Dos
        (Path : Filesystem_String) return Filesystem_String
      is
         Cygdrive : constant String := "/cygdrive/";
      begin
         if Is_Cygwin_Path (Path) then
            return +(Case_Util.To_Upper (Path (Path'First + Cygdrive'Length)) &
              ":") & Path (Path'First + Cygdrive'Length + 1 .. Path'Last);
         else
            return Path;
         end if;
      end Cygwin_To_Dos;

   begin
      case Style is
         when UNIX =>
            return Format_Pathname
              (Path,
               Directory_Operations.Path_Style'Val (Path_Style'Pos (Style)));

         when System_Default =>
            if Config.Host = Config.Windows then
               --  If we are running on Windows, then make sure that a
               --  conversion to the System_Default does convert from Cygwin
               --  PATH.
               return Format_Pathname
                 (Cygwin_To_Dos (Path), Directory_Operations.DOS);

            else
               return Format_Pathname
                 (Path,
                  Directory_Operations.Path_Style'Val
                    (Path_Style'Pos (Style)));
            end if;

         when DOS =>
            declare
               Result : constant Filesystem_String := Cygwin_To_Dos (Path);
            begin
               return Format_Pathname (Result, Directory_Operations.DOS);
            end;

         when Cygwin =>
            declare
               Result : constant Filesystem_String :=
                          Format_Pathname (Path, Directory_Operations.UNIX);
            begin
               if Result'Length > 2
                 and then (Result (Result'First) in 'A' .. 'Z'
                           or else Result (Result'First) in 'a' .. 'z')
                 and then Result (Result'First + 1) = ':'
                 and then Result (Result'First + 2) = '/'
               then
                  return +("/cygdrive/" &
                    Case_Util.To_Upper (Result (Result'First))) &
                    Result (Result'First + 2 .. Result'Last);
               else
                  return Result;
               end if;
            end;
      end case;
   end Format_Pathname;

   --------------------------
   -- Normalize_To_OS_Case --
   --------------------------

   function Normalize_To_OS_Case
     (Full_Name : Filesystem_String) return Filesystem_String
   is

      use type Config.Host_Type;

      function Norm
        (Dir    : String;
         Name   : String;
         Filter : Filter_Type)
         return String;
      --  Normalize Name using OS casing and do the same recusivelly for full
      --  pathname in Dir.

      ----------
      -- Norm --
      ----------

      function Norm
        (Dir    : String;
         Name   : String;
         Filter : Filter_Type)
         return String
      is
         L_Name  : String := Name;
         Search  : Search_Type;
         Entries : Directory_Entry_Type;
      begin
         Case_Util.To_Lower (L_Name);

         Start_Search (Search, Dir, "*", Filter);

         Main_Loop : loop
            loop
               --  Catch exceptions (such as Access_Denied) which could
               --  occur on files that are not accessible.
               begin
                  if More_Entries (Search) then
                     exit;
                  else
                     exit Main_Loop;
                  end if;
               exception
                  when others =>
                     null;
               end;
            end loop;

            Get_Next_Entry (Search, Entries);

            declare
               S_Name : String := Simple_Name (Entries);
            begin
               Case_Util.To_Lower (S_Name);

               if L_Name = S_Name then
                  End_Search (Search);

                  if Dir'Length = 3 then
                     if Dir (Dir'First + 1 .. Dir'First + 2) = ":\" then
                        return Compose
                          (Case_Util.To_Upper (Dir (Dir'First)) & ":\",
                           Simple_Name (Entries));

                     else
                        return Compose (Dir, Simple_Name (Entries));
                     end if;

                  else
                     return Compose
                       (Norm (Containing_Directory (Dir),
                              Simple_Name (Dir),
                              (Directory => True, others => False)),
                        Simple_Name (Entries));
                  end if;
               end if;
            end;
         end loop Main_Loop;

         --  Not found, return Full_Name
         End_Search (Search);
         return Compose (Dir, Name);
      end Norm;

   begin
      if Config.Host = Config.Windows
        and then (Is_Regular_File (Full_Name)
                  or else Is_Directory (Full_Name))
      then
         return +Norm
           (Ada.Directories.Full_Name (Containing_Directory (+Full_Name)),
            Simple_Name (+Full_Name), (others => True));
      else
         return Full_Name;
      end if;

   exception
      when Name_Error | Use_Error | Status_Error =>
         return Full_Name;
   end Normalize_To_OS_Case;

   ---------------
   -- Strip_Exe --
   ---------------

   function Strip_Exe (Name : String) return String is
      use type Config.Host_Type;
   begin
      if Config.Host = Config.Windows
        and then Name'Length > 4
        and then Name (Name'Last - 3 .. Name'Last) = ".exe"
      then
         return Name (Name'First .. Name'Last - 4);
      else
         return Name;
      end if;
   end Strip_Exe;

end OS_Utils;
