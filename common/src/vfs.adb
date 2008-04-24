-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2003-2008, AdaCore                  --
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

--  This module implements a virtual file system.
--  It gives access to local and remote files, abstract file system specific
--  features (case sensitivity),...

with Ada.Calendar;              use Ada.Calendar;
with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with Ada.Strings.Hash;
with Ada.Characters.Handling;

with GNAT.Case_Util;            use GNAT.Case_Util;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.Heap_Sort;            use GNAT.Heap_Sort;
with GNAT.OS_Lib;               use GNAT.OS_Lib;

with Glib.Unicode;              use Glib.Unicode;

with Filesystem;                use Filesystem;
with Filesystem.Queries;        use Filesystem.Queries;
with File_Utils;                use File_Utils;
with OS_Utils;                  use OS_Utils;
with String_Utils;              use String_Utils;

package body VFS is

   package SLU renames String_List_Utils;

   --  ??? The various calls in Locale_To_UTF8, which is in the Gtk library,
   --  make it risky to use any function of this package before Gtk.Init has
   --  been called.
   --  This will be fixed when we stop relying on Gtk+ for the UTF8
   --  conversions.

   Temporary_Dir : constant String := Get_Tmp_Dir;
   --  Cache the name of the temporary directory

   Empty_String : constant Cst_UTF8_String_Access := new String'("");

   procedure Ensure_Normalized (File : Virtual_File);
   --  Ensure that the information for the normalized file have been correctly
   --  computed.

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Contents_Record, Contents_Access);

   Cygdrive : constant String := "/cygdrive/";
   --  This package supports the standard Cygwin naming convention. A filename
   --  starting with /cygdrive/ will be properly converted to the DOS
   --  equivalent.

   function Native_Filename (Filename : String) return String;
   pragma Inline (Native_Filename);
   --  Routine that will translate a "foreign" filename convention to the
   --  proper native one. This is currently used only for the standard Cygwin
   --  convention.

   ---------------------
   -- Native_Filename --
   ---------------------

   function Native_Filename (Filename : String) return String is
   begin
      if Filename'Length > Cygdrive'Length + 2
        and then
          Filename (Filename'First .. Filename'First + Cygdrive'Length - 1)
          = Cygdrive
        and then
          (Filename (Filename'First + Cygdrive'Length) in 'a' .. 'z'
           or else Filename (Filename'First + Cygdrive'Length) in 'A' .. 'Z')
      then
         return OS_Utils.Format_Pathname (Filename, Style => DOS);

      else
         return Filename;
      end if;
   end Native_Filename;

   --------------
   -- Is_Local --
   --------------

   function Is_Local (File : Virtual_File) return Boolean is
   begin
      if File.Value /= null then
         return File.Value.Server.all = "";
      end if;

      return True;
   end Is_Local;

   ---------
   -- "=" --
   ---------

   function "=" (File1, File2 : Virtual_File) return Boolean is
   begin
      if File1.Value = null then
         return File2.Value = null;

      elsif File2.Value = null then
         return False;

      elsif File1.Value = File2.Value then
         return True;

      elsif File1.Value.Server.all /= File2.Value.Server.all then
         return False;

      else
         Ensure_Normalized (File1);
         Ensure_Normalized (File2);
         return Equal
           (File2.Value.Normalized_Full.all,
            File1.Value.Normalized_Full.all,
            File1.Get_Filesystem.Is_Case_Sensitive);
      end if;
   end "=";

   ------------
   -- Create --
   ------------

   function Create (Full_Filename : UTF8_String) return Virtual_File is

   begin
      return Create (Host => "", Full_Filename => Full_Filename);
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Host          : UTF8_String;
      Full_Filename : UTF8_String) return Virtual_File
   is
      Valid       : Boolean;
      Invalid_Pos : Natural;
      Last        : Natural;
      File        : Virtual_File;
   begin
      UTF8_Validate (Full_Filename, Valid, Invalid_Pos);

      if not Valid then
         Last := Invalid_Pos - 1;
      else
         Last := Full_Filename'Last;
      end if;

      while Last >= Full_Filename'First loop
         exit when Full_Filename (Last) /= ASCII.LF
           and then Full_Filename (Last) /= ASCII.CR;
         Last := Last - 1;
      end loop;

      File :=
        (Ada.Finalization.Controlled with
         Value => new Contents_Record'
           (Server          => new String'(Host),
            Ref_Count       => 1,
            Full_Name       => new String'
              (Native_Filename (Full_Filename (Full_Filename'First .. Last))),
            Normalized_Full => null,
            Dir_Name        => null,
            Kind            => Unknown));

      if File.Full_Name.all = "" then
         Ensure_Directory (File);
      end if;

      return File;
   end Create;

   function Create (Files : SLU.String_List.List) return File_Array is
      File_Array : VFS.File_Array (1 .. SLU.String_List.Length (Files));
      Iter       : SLU.String_List.List_Node;
   begin
      Iter := SLU.String_List.First (Files);

      for K in File_Array'Range loop
         File_Array (K) := Create (SLU.String_List.Data (Iter));
         Iter := SLU.String_List.Next (Iter);
      end loop;

      return File_Array;
   end Create;

   ---------------------
   -- Create_From_Dir --
   ---------------------

   function Create_From_Dir
     (Dir       : Virtual_File;
      Base_Name : UTF8_String) return Virtual_File is
   begin
      Ensure_Directory (Dir);
      return Create
        (Get_Host (Dir),
         Dir.Get_Filesystem.Concat (Dir.Full_Name.all, Base_Name));
   end Create_From_Dir;

   ----------------------
   -- Create_From_Base --
   ----------------------

   function Create_From_Base (Base_Name : UTF8_String) return Virtual_File is
   begin
      if Base_Name /= "" then
         return (Ada.Finalization.Controlled with
                 Value => new Contents_Record'
                 (Server          => new String'(""),
                  Ref_Count       => 1,
                  Full_Name       => new String'(Base_Name),
                  Normalized_Full => null,
                  Dir_Name        => null,
                  Kind            => Unknown));
      else
         return No_File;
      end if;
   end Create_From_Base;

   -----------------------
   -- Ensure_Normalized --
   -----------------------

   procedure Ensure_Normalized (File : Virtual_File) is
   begin
      if File.Value.Normalized_Full = null then
         --  If the user didn't create a file with a full name, no need to
         --  spend time now trying to find the file.

         if not File.Get_Filesystem.Is_Absolute_Path
           (File.Value.Full_Name.all)
         then
            File.Value.Normalized_Full :=
              new UTF8_String'(File.Value.Full_Name.all);

         else
            --  Normalize_Pathname with link resolution is not needed, since
            --     we want to keep links anyway (they have been checked for
            --     the application's source files, and do not matter for other
            --     files on the system).

            File.Value.Normalized_Full := new UTF8_String'
              (File.Get_Filesystem.Normalize (File.Value.Full_Name.all));
         end if;
      end if;
   end Ensure_Normalized;

   ---------------
   -- Base_Name --
   ---------------

   function Base_Name
     (File : Virtual_File; Suffix : String := "") return Glib.UTF8_String is
   begin
      if File.Value = null
        or else File.Value.Full_Name = null
      then
         return "";
      end if;

      return File.Get_Filesystem.Base_Name
        (File.Value.Full_Name.all, Suffix);
   end Base_Name;

   -------------------
   -- Base_Dir_Name --
   -------------------

   function Base_Dir_Name (File : Virtual_File) return Glib.UTF8_String is
   begin
      if File.Value = null
        or else File.Value.Full_Name = null
      then
         return "";
      end if;

      return File.Get_Filesystem.Base_Dir_Name (File.Value.Full_Name.all);
   end Base_Dir_Name;

   ---------------
   -- Full_Name --
   ---------------

   function Full_Name
     (File : Virtual_File; Normalize : Boolean := False)
      return Cst_UTF8_String_Access is
   begin
      if File.Value = null then
         return Empty_String;

      elsif Normalize then
         Ensure_Normalized (File);
         return Cst_UTF8_String_Access (File.Value.Normalized_Full);

      else
         return Cst_UTF8_String_Access (File.Value.Full_Name);
      end if;
   end Full_Name;

   --------------------
   -- Full_Name_Hash --
   --------------------

   function Full_Name_Hash
     (Key : Virtual_File) return Ada.Containers.Hash_Type is
   begin
      if Is_Case_Sensitive (Get_Filesystem (Key)) then
         return Ada.Strings.Hash (Full_Name (Key, True).all);
      else
         return Ada.Strings.Hash
           (Ada.Characters.Handling.To_Lower (Full_Name (Key, True).all));
      end if;
   end Full_Name_Hash;

   ---------------
   -- Host_Name --
   ---------------

   function Get_Host (File : Virtual_File) return UTF8_String is
   begin
      if File.Value = null then
         return "";
      else
         return File.Value.Server.all;
      end if;
   end Get_Host;

   --------------
   -- Dir_Name --
   --------------

   function Dir_Name (File : Virtual_File) return Cst_UTF8_String_Access is
   begin
      if File.Value = null then
         return Empty_String;

      else
         if File.Value.Dir_Name = null then
            File.Value.Dir_Name :=
              new UTF8_String'
                (File.Get_Filesystem.Dir_Name (File.Full_Name.all));
         end if;
         return Cst_UTF8_String_Access (File.Value.Dir_Name);
      end if;
   end Dir_Name;

   ---------
   -- Dir --
   ---------

   function Dir (File : Virtual_File) return Virtual_File is
      The_Dir      : Virtual_File;
      The_Dir_Name : constant String := File.Dir_Name.all;
   begin
      if The_Dir_Name = "" then
         return VFS.No_File;
      else
         The_Dir := Create (Get_Host (File), The_Dir_Name);
         return The_Dir;
      end if;
   end Dir;

   ----------------------
   -- Locale_Full_Name --
   ----------------------

   function Locale_Full_Name (File : Virtual_File) return String is
   begin
      if File.Value = null then
         return "";
      else
         --  ??? This is not cached, should it ?
         return File.Full_Name.all;
      end if;
   end Locale_Full_Name;

   ----------------------
   -- Locale_Base_Name --
   ----------------------

   function Locale_Base_Name (File : Virtual_File) return String is
   begin
      return File.Base_Name;
   end Locale_Base_Name;

   ---------------------
   -- Locale_Dir_Name --
   ---------------------

   function Locale_Dir_Name (File : Virtual_File) return String is
   begin
      return File.Dir_Name.all;
   end Locale_Dir_Name;

   --------------------
   -- Unchecked_Free --
   --------------------

   procedure Unchecked_Free (Arr : in out File_Array_Access) is
      procedure Internal is new Ada.Unchecked_Deallocation
        (File_Array, File_Array_Access);
   begin
      Internal (Arr);
   end Unchecked_Free;

   ---------------------
   -- Is_Regular_File --
   ---------------------

   function Is_Regular_File (File : Virtual_File) return Boolean is
   begin
      return File.Value /= null
        and then File.Get_Filesystem.Is_Regular_File
          (File.Value.Server.all, File.Locale_Full_Name);
   end Is_Regular_File;

   ------------
   -- Rename --
   ------------

   procedure Rename
     (File      : Virtual_File;
      Full_Name : String;
      Success   : out Boolean) is
   begin
      Success := File.Get_Filesystem.Rename
        (File.Value.Server.all,
         File.Locale_Full_Name,
         Full_Name);
   end Rename;

   ----------
   -- Copy --
   ----------

   procedure Copy
     (File        : Virtual_File;
      Target_Name : String;
      Success     : out Boolean) is
   begin
      Success := File.Get_Filesystem.Copy
        (File.Value.Server.all,
         File.Locale_Full_Name,
         Target_Name);
   end Copy;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (File    : Virtual_File;
      Success : out Boolean) is
   begin
      Success := File.Get_Filesystem.Delete
        (File.Value.Server.all,
         File.Locale_Full_Name);

      if Success then
         File.Value.Kind := Unknown;
      end if;
   end Delete;

   -----------------
   -- Is_Writable --
   -----------------

   function Is_Writable (File : Virtual_File) return Boolean is
   begin
      return File.Value /= null
        and then File.Get_Filesystem.Is_Writable
          (File.Value.Server.all, File.Locale_Full_Name);
   end Is_Writable;

   ------------------
   -- Is_Directory --
   ------------------

   function Is_Directory (VF : Virtual_File) return Boolean is
      Ret : Boolean;
   begin
      if VF.Value = null then
         return False;

      elsif VF.Value.Kind = Directory then
         return True;

      elsif VF.Value.Kind = File then
         return False;

      else
         Ret := VF.Get_Filesystem.Is_Directory
           (VF.Value.Server.all,
            VF.Locale_Full_Name);
      end if;

      if Ret then
         VF.Value.Kind := Directory;
         Ensure_Directory (VF);
      else
         VF.Value.Kind := File;
      end if;

      return Ret;
   end Is_Directory;

   ----------------------
   -- Is_Symbolic_Link --
   ----------------------

   function Is_Symbolic_Link (File : Virtual_File) return Boolean is
   begin
      return File.Value /= null
        and then File.Get_Filesystem.Is_Symbolic_Link
          (File.Value.Server.all, File.Locale_Full_Name);
   end Is_Symbolic_Link;

   ----------------------
   -- Is_Absolute_Path --
   ----------------------

   function Is_Absolute_Path (File : Virtual_File) return Boolean is
   begin
      return File.Get_Filesystem.Is_Absolute_Path (File.Locale_Full_Name);
   end Is_Absolute_Path;

   --------------------
   -- File_Extension --
   --------------------

   function File_Extension (File : Virtual_File) return UTF8_String is
   begin
      return File.Get_Filesystem.File_Extension (File.Full_Name.all);
   end File_Extension;

   ---------------
   -- Read_File --
   ---------------

   function Read_File (File : Virtual_File) return String_Access is
   begin
      if File.Value = null then
         return null;

      elsif File.Value.Kind = Directory then
         return null;

      else
         return File.Get_Filesystem.Read_File
           (File.Value.Server.all,
            File.Locale_Full_Name);
      end if;
   end Read_File;

   ----------------
   -- Write_File --
   ----------------

   function Write_File
     (File   : Virtual_File;
      Append : Boolean := False) return Writable_File
   is
      Tmp : GNAT.Strings.String_Access := null;
      Fd  : File_Descriptor;
   begin
      if File.Value = null then
         return Invalid_File;

      else
         declare
            Current_Dir : constant String := Get_Current_Dir;
            Base        : String_Access;
         begin
            --  ??? Append is now handled

            Change_Dir (Temporary_Dir);
            Create_Temp_File (Fd, Base);
            Tmp := new String'(Name_As_Directory (Temporary_Dir) & Base.all);
            Free (Base);
            Change_Dir (Current_Dir);
         end;

--        else
--           if Append and then Is_Regular_File (File.Locale_Full_Name) then
--              Fd := Open_Read_Write (Locale_Full_Name (File), Binary);
--              Lseek (Fd, 0, Seek_End);
--           else
--              Fd := Create_File (Locale_Full_Name (File), Binary);
--           end if;
      end if;

      if Fd = Invalid_FD then
         Free (Tmp);
         return Invalid_File;
      else
         return (File => File, FD => Fd, Filename => Tmp, Append => Append);
      end if;
   end Write_File;

   -----------
   -- Write --
   -----------

   procedure Write
     (File : in out Writable_File;
      Str  : String)
   is
      Written : aliased Natural;

   begin
      Written := Write (File.FD, Str'Address, Str'Length);

      --  ??? Should raise an exception if we couldn't write all the bytes
   end Write;

   -----------
   -- Close --
   -----------

   procedure Close (File : in out Writable_File) is
      Success : Boolean;
      pragma Unreferenced (Success);
   begin
      File.File.Get_Filesystem.Write
        (File.File.Value.Server.all,
         File.File.Value.Full_Name.all,
         File.Filename.all,
         Append => File.Append);
      Delete_File (File.Filename.all, Success);
      Free (File.Filename);

      Close (File.FD);
   end Close;

   ------------------
   -- Set_Writable --
   ------------------

   procedure Set_Writable (File : VFS.Virtual_File; Writable : Boolean) is
   begin
      File.Get_Filesystem.Set_Writable
        (File.Value.Server.all,
         File.Locale_Full_Name,
         Writable);
   end Set_Writable;

   ------------------
   -- Set_Readable --
   ------------------

   procedure Set_Readable (File : VFS.Virtual_File; Readable : Boolean) is
   begin
      File.Get_Filesystem.Set_Readable
        (File.Value.Server.all,
         File.Locale_Full_Name,
         Readable);
   end Set_Readable;

   ---------------------
   -- File_Time_Stamp --
   ---------------------

   function File_Time_Stamp
     (File : Virtual_File) return Ada.Calendar.Time is
   begin
      return File.Get_Filesystem.File_Time_Stamp
        (File.Value.Server.all,
         File.Locale_Full_Name);
   end File_Time_Stamp;

   ---------------------
   -- Get_Current_Dir --
   ---------------------

   function Get_Current_Dir return Virtual_File is
      File : Virtual_File;
   begin
      File := Create (GNAT.Directory_Operations.Get_Current_Dir);
      File.Value.Kind := Directory;
      return File;
   end Get_Current_Dir;

   ----------------------
   -- Ensure_Directory --
   ----------------------

   procedure Ensure_Directory (Dir : Virtual_File) is
      Full    : String_Access;
   begin
      if Dir.Value /= null
        and then Dir.Value.Full_Name = null
      then
         declare
            Dir_Path : constant String :=
              Dir.Get_Filesystem.Ensure_Directory
                (Dir.Full_Name.all);
         begin
            if Dir_Path /= Dir.Value.Full_Name.all then
               Full := new String'(Dir_Path);
               Free (Dir.Value.Full_Name);
               Dir.Value.Full_Name := Full;

               if Dir.Value.Normalized_Full /= null then
                  Free (Dir.Value.Normalized_Full);
                  Ensure_Normalized (Dir);
               end if;

               if Dir.Value.Dir_Name /= null then
                  Free (Dir.Value.Dir_Name);
                  Dir.Value.Dir_Name := new String'(Full.all);
               end if;
            end if;
         end;
      end if;
   end Ensure_Directory;

   --------------
   -- Get_Root --
   --------------

   function Get_Root (File : Virtual_File) return Virtual_File is
   begin
      return Create
        (Host          => File.Value.Server.all,
         Full_Filename => File.Get_Filesystem.Get_Root (File.Full_Name.all));
   end Get_Root;

   ----------------
   -- Get_Parent --
   ----------------

   function Get_Parent (Dir : Virtual_File) return Virtual_File is
   begin
      if Dir.Value = null
        or else Dir.Value.Full_Name = null
        or else Dir.Value.Full_Name.all = ""
      then
         return No_File;
      end if;

      return Create
        (Dir.Get_Host, Dir.Get_Filesystem.Get_Parent (Dir.Full_Name.all));
   end Get_Parent;

   -------------
   -- Sub_Dir --
   -------------

   function Sub_Dir
     (Dir : Virtual_File; Name : UTF8_String) return Virtual_File
   is
      New_Dir : Virtual_File;
   begin
      Ensure_Directory (Dir);
      New_Dir := Create
        (Dir.Value.Server.all,
         Dir.Get_Filesystem.Concat (Dir.Full_Name.all, Name));
      Ensure_Directory (New_Dir);

      if Is_Directory (New_Dir) then
         return New_Dir;
      else
         return No_File;
      end if;
   end Sub_Dir;

   ----------------
   -- Change_Dir --
   ----------------

   procedure Change_Dir (Dir : Virtual_File) is
      Success : Boolean;
      pragma Unreferenced (Success);
   begin
      if Dir.Value = null then
         Raise_Exception (VFS_Directory_Error'Identity, "Dir is No_File");
      end if;

      Success := Dir.Get_Filesystem.Change_Dir
        (Dir.Value.Server.all, Full_Name (Dir).all);
   end Change_Dir;

   --------------
   -- Make_Dir --
   --------------

   procedure Make_Dir (Dir : Virtual_File) is
      Result : Boolean;
   begin
      if Dir.Value = null then
         Raise_Exception (VFS_Directory_Error'Identity, "Dir is No_File");
      end if;

      --  If Dir already exists and is a directory, then return
      if Is_Directory (Dir) then
         return;
      end if;

      Result := Dir.Get_Filesystem.Make_Dir
        (Dir.Value.Server.all,
         Dir.Value.Full_Name.all);

      if not Result then
         Raise_Exception
           (VFS_Directory_Error'Identity, "Dir cannot be created");
      end if;

      Dir.Value.Kind := Directory;

   exception
      when E : GNAT.Directory_Operations.Directory_Error =>
         Raise_Exception
           (VFS_Directory_Error'Identity, Exception_Message (E));
   end Make_Dir;

   ----------
   -- Read --
   ----------

   function Read_Dir
     (Dir    : in Virtual_File;
      Filter : Read_Dir_Filter := All_Files) return File_Array_Access
   is
      F_Array   : File_Array_Access;
      Tmp_File  : Virtual_File;

   begin
      if Dir.Value = null then
         Raise_Exception (VFS_Directory_Error'Identity, "Dir is No_File");
      end if;

      Ensure_Directory (Dir);

      if not Is_Directory (Dir) then
         Raise_Exception
           (VFS_Directory_Error'Identity, "Dir is not a directory");
      end if;

      declare
         List : GNAT.Strings.String_List :=
           Dir.Get_Filesystem.Read_Dir
             (Dir.Value.Server.all,
              Dir.Value.Full_Name.all,
              Dirs_Only  => Filter = Dirs_Only,
              Files_Only => Filter = Files_Only);
      begin
         F_Array := new File_Array (1 .. List'Length);

         for J in List'Range loop
            Tmp_File := Dir.Create_From_Dir (List (J).all);
            Free (List (J));

            case Filter is
               when Dirs_Only =>
                  Tmp_File.Value.Kind := Directory;

               when Files_Only =>
                  Tmp_File.Value.Kind := File;

               when others =>
                  null;

            end case;

            F_Array (F_Array'First + J - List'First) := Tmp_File;
         end loop;
      end;

      if F_Array /= null then
         return F_Array;
      else
         return null;
      end if;

   exception
      when E : GNAT.Directory_Operations.Directory_Error =>
         Raise_Exception (VFS_Directory_Error'Identity,
                          Exception_Message (E));
   end Read_Dir;

   --------------
   -- Open_Dir --
   --------------

   function Open_Dir (Dir : Virtual_File) return Virtual_Dir is
      VDir : Virtual_Dir;
   begin
      if Dir.Value = null then
         return Invalid_Dir;
      end if;

      VDir.File := Dir;
      VDir.Files_List := Read_Dir (Dir);

      if VDir.Files_List /= null then
         VDir.Current := VDir.Files_List'First - 1;
      end if;

      Dir.Value.Kind := Directory;
      return VDir;

   exception
      when VFS_Directory_Error =>
         return Invalid_Dir;
   end Open_Dir;

   ----------
   -- Read --
   ----------

   procedure Read
     (VDir : in out Virtual_Dir;
      File :    out Virtual_File) is
   begin
      if VDir.Files_List /= null
        and then VDir.Current < VDir.Files_List'Last
      then
         VDir.Current := VDir.Current + 1;
         File := VDir.Files_List (VDir.Current);
      else
         File := No_File;
      end if;
   end Read;

   -----------
   -- Close --
   -----------

   procedure Close (VDir : in out Virtual_Dir) is
   begin
      if VDir.Files_List /= null then
         Unchecked_Free (VDir.Files_List);
      end if;

      VDir := Invalid_Dir;
   end Close;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Value : in out Contents_Access) is
   begin
      if Value /= null then
         Value.Ref_Count := Value.Ref_Count - 1;

         if Value.Ref_Count = 0 then
            Free (Value.Server);
            Free (Value.Full_Name);
            Free (Value.Dir_Name);
            Free (Value.Normalized_Full);
            Unchecked_Free (Value);
         end if;
      end if;
   end Finalize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (File : in out Virtual_File) is
   begin
      Finalize (File.Value);
   end Finalize;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (File : in out Virtual_File) is
   begin
      if File.Value /= null then
         File.Value.Ref_Count := File.Value.Ref_Count + 1;
      end if;
   end Adjust;

   ---------
   -- "<" --
   ---------

   function "<" (File1, File2 : Virtual_File) return Boolean is
      C1, C2         : Character;
      Ind1, Ind2     : Integer;
      Case_Sensitive : Boolean;
   begin
      if File1 = File2 then
         return False;

      elsif File1.Value = null then
         return True;

      elsif File2.Value = null then
         return False;

      else
         Case_Sensitive := File1.Get_Filesystem.Is_Case_Sensitive
           and then File2.Get_Filesystem.Is_Case_Sensitive;

         Ensure_Normalized (File1);
         Ensure_Normalized (File2);

         if Case_Sensitive then
            return File1.Value.Normalized_Full.all
              < File2.Value.Normalized_Full.all;
         else
            Ind1 := File1.Value.Normalized_Full'First;
            Ind2 := File2.Value.Normalized_Full'First;

            for C in 1 .. File1.Value.Normalized_Full'Length loop
               if Ind2 > File2.Value.Normalized_Full'Last then
                  return False;
               end if;

               C1 := To_Lower (File1.Value.Normalized_Full (Ind1));
               C2 := To_Lower (File2.Value.Normalized_Full (Ind2));

               if C1 < C2 then
                  return True;
               elsif C1 > C2 then
                  return False;
               end if;

               Ind1 := Ind1 + 1;
               Ind2 := Ind2 + 1;
            end loop;

            return False;
         end if;
      end if;
   end "<";

   --------------------
   -- Get_Filesystem --
   --------------------

   function Get_Filesystem
     (File : Virtual_File) return Filesystem_Record'Class is
   begin
      return Get_Filesystem (File.Value.Server.all);
   end Get_Filesystem;

   ---------------
   -- Is_Parent --
   ---------------

   function Is_Parent (Parent, Child : Virtual_File) return Boolean is
   begin
      if Parent.Value = null or Child.Value = null then
         return False;
      elsif Parent.Value.Server.all /= Child.Value.Server.all then
         return False;
      end if;

      Ensure_Normalized (Parent);
      Ensure_Normalized (Child);

      if Parent.Value.Normalized_Full'Length >
        Child.Value.Normalized_Full'Length
      then
         return False;
      end if;

      return Parent.Get_Filesystem.Is_Subtree
        (Parent.Value.Normalized_Full.all,
         Child.Value.Normalized_Full.all);
   end Is_Parent;

   ----------
   -- Sort --
   ----------

   procedure Sort (Files : in out File_Array) is
      --  ??? Right now, this sorts only on the full name. Do we want to
      --  provide other choices for sorting ?

      procedure Xchg (Op1, Op2 : Natural);
      --  Exchanges two items in the array

      function Lt (Op1, Op2 : Natural) return Boolean;
      --  Return True if the first item is to be sorted before the second

      ----------
      -- Xchg --
      ----------

      procedure Xchg (Op1, Op2 : Natural) is
         Buffer : Virtual_File;
      begin
         Buffer := Files (Files'First - 1 + Op1);
         Files (Files'First - 1 + Op1) := Files (Files'First - 1 + Op2);
         Files (Files'First - 1 + Op2) := Buffer;
      end Xchg;

      --------
      -- Lt --
      --------

      function Lt (Op1, Op2 : Natural) return Boolean is
      begin
         return Files (Files'First - 1 + Op1) <
           Files (Files'First - 1 + Op2);
      end Lt;

   begin
      Sort (Files'Length, Xchg'Unrestricted_Access, Lt'Unrestricted_Access);
   end Sort;

end VFS;
