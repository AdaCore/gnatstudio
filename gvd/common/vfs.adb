-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2003                         --
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

--  This module implements a virtual file system.
--  It gives access to local and remote files, abstract file system specific
--  features (case sensitivity),...

with Glib;                       use Glib;
with Glib.Convert;               use Glib.Convert;
with GNAT.Directory_Operations;  use GNAT.Directory_Operations;
with Ada.Unchecked_Deallocation;
with GNAT.OS_Lib;                use GNAT.OS_Lib;
with File_Utils;                 use File_Utils;
with OS_Utils;                   use OS_Utils;
with GNAT.Case_Util;             use GNAT.Case_Util;
with GNAT.Heap_Sort;             use GNAT.Heap_Sort;
with Interfaces.C.Strings;       use Interfaces.C.Strings;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System;

package body VFS is

   Empty_String : constant Cst_UTF8_String_Access := new String'("");

   procedure Ensure_Normalized (File : Virtual_File);
   --  Ensure that the information for the normalized file have been correctly
   --  computed.

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Contents_Record, Contents_Access);

   ---------
   -- "=" --
   ---------

   function "=" (File1, File2 : Virtual_File) return Boolean is
   begin
      if File1.Value = null then
         return File2.Value = null;
      elsif File2.Value = null then
         return False;
      else
         Ensure_Normalized (File1);
         Ensure_Normalized (File2);

         return File2.Value.Normalized_Full.all =
           File1.Value.Normalized_Full.all;
      end if;
   end "=";

   ------------
   -- Create --
   ------------

   function Create (Full_Filename : UTF8_String) return Virtual_File is
   begin
      return (Ada.Finalization.Controlled with
              Value => new Contents_Record'
                (Ref_Count       => 1,
                 Full_Name       => new String'(Full_Filename),
                 Normalized_Full => null,
                 Dir_Name        => null,
                 Base_Name       => null));
   end Create;

   ----------------------
   -- Create_From_Base --
   ----------------------

   function Create_From_Base (Base_Name : UTF8_String) return Virtual_File is
   begin
      return (Ada.Finalization.Controlled with
                Value => new Contents_Record'
                (Ref_Count       => 1,
                 Full_Name       => new String'(Base_Name),
                 Normalized_Full => null,
                 Dir_Name        => null,
                 Base_Name       => new String'(Base_Name)));
   end Create_From_Base;

   -----------------------
   -- Ensure_Normalized --
   -----------------------

   procedure Ensure_Normalized (File : Virtual_File) is
   begin
      if File.Value.Normalized_Full = null then
         --  If the user didn't create a file with a full name, no need to
         --  spend time now trying to find the file.
         if not Is_Absolute_Path (File.Value.Full_Name.all) then
            File.Value.Normalized_Full :=
              new UTF8_String'(File.Value.Full_Name.all);
         else
            File.Value.Normalized_Full := new UTF8_String'
              (Locale_To_UTF8
                 (To_Host_Pathname
                    (Normalize_Pathname
                       (Locale_From_UTF8 (File.Value.Full_Name.all),
                        Resolve_Links => True))));
            if not File_Utils.Filenames_Are_Case_Sensitive then
               To_Lower (File.Value.Normalized_Full.all);
            end if;
         end if;
      end if;
   end Ensure_Normalized;

   ---------------
   -- Base_Name --
   ---------------

   function Base_Name
     (File   : Virtual_File;
      Suffix : String := "") return Cst_UTF8_String_Access is
   begin
      if File.Value = null then
         return Empty_String;
      else
         if File.Value.Base_Name = null then
            File.Value.Base_Name := new UTF8_String'
              (Base_Name (File.Value.Full_Name.all, Suffix));
         end if;
         return Cst_UTF8_String_Access (File.Value.Base_Name);
      end if;
   end Base_Name;

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
              new UTF8_String'(Dir_Name (File.Value.Full_Name.all));
         end if;
         return Cst_UTF8_String_Access (File.Value.Dir_Name);
      end if;
   end Dir_Name;

   ---------------
   -- Read_File --
   ---------------

   function Read_File (File : Virtual_File) return UTF8_String_Access is
   begin
      if File.Value = null then
         return null;
      else
         --  ??? Should be enhanced for remote access
         return Read_File (Full_Name (File).all);
      end if;
   end Read_File;

   ------------
   -- Delete --
   ------------

   procedure Delete (File : Virtual_File) is
      Success : Boolean;
   begin
      Delete_File (Locale_Full_Name (File), Success);
   end Delete;

   ----------------------
   -- Locale_Full_Name --
   ----------------------

   function Locale_Full_Name (File : Virtual_File) return String is
   begin
      if File.Value = null then
         return "";
      else
         --  ??? This is not cached, should it ?
         return Locale_From_UTF8 (File.Value.Full_Name.all);
      end if;
   end Locale_Full_Name;

   ----------------------
   -- Locale_Base_Name --
   ----------------------

   function Locale_Base_Name (File : Virtual_File) return String is
      Base : constant String := Base_Name (File).all;
   begin
      return Locale_From_UTF8 (Base);
   end Locale_Base_Name;

   ---------------------
   -- Is_Regular_File --
   ---------------------

   function Is_Regular_File (File : Virtual_File) return Boolean is
   begin
      return File.Value /= null
        and then Is_Regular_File (Locale_Full_Name (File));
   end Is_Regular_File;

   --------------------
   -- Unchecked_Free --
   --------------------

   procedure Unchecked_Free (Arr : in out File_Array_Access) is
      procedure Internal is new Ada.Unchecked_Deallocation
        (File_Array, File_Array_Access);
   begin
      Internal (Arr);
   end Unchecked_Free;

   -----------------
   -- Is_Writable --
   -----------------

   function Is_Writable (File : Virtual_File) return Boolean is
   begin
      return Is_Writable_File (Locale_Full_Name (File));
   end Is_Writable;

   ------------------
   -- Is_Directory --
   ------------------

   function Is_Directory (File : Virtual_File) return Boolean is
   begin
      return Is_Directory (Locale_Full_Name (File));
   end Is_Directory;

   ----------------------
   -- Is_Absolute_Path --
   ----------------------

   function Is_Absolute_Path (File : Virtual_File) return Boolean is
   begin
      return Is_Absolute_Path (Full_Name (File).all);
   end Is_Absolute_Path;

   --------------------
   -- File_Extension --
   --------------------

   function File_Extension (File : Virtual_File) return UTF8_String is
   begin
      return File_Extension (Full_Name (File).all);
   end File_Extension;

   ----------------
   -- Write_File --
   ----------------

   function Write_File (File : Virtual_File) return Writable_File is
   begin
      --  ??? Should we first delete the file ?
      return (File => File,
              FD   => Create_File (Locale_Full_Name (File), Binary));
   end Write_File;

   -----------
   -- Write --
   -----------

   procedure Write
     (File : in out Writable_File;
      Str : UTF8_String;
      As_UTF8 : Boolean := True)
   is
      function To_Address is new Ada.Unchecked_Conversion
        (chars_ptr, System.Address);
      Written : aliased Natural;
      Read    : aliased Natural;
      S       : chars_ptr;
   begin
      if As_UTF8 then
         S := Locale_From_UTF8
           (Str,
            Read'Access,
            Written'Access);
         Written := Write (File.FD, To_Address (S), Written);
         Free (S);
      else
         Written := Write (File.FD, Str'Address, Str'Length);
      end if;

      --  ??? Should raise an exception if we couldn't write all the bytes.
   end Write;

   -----------
   -- Close --
   -----------

   procedure Close (File : in out Writable_File) is
   begin
      Close (File.FD);
   end Close;

   ---------------------
   -- File_Time_Stamp --
   ---------------------

   function File_Time_Stamp (File : Virtual_File) return GNAT.OS_Lib.OS_Time is
   begin
      return File_Time_Stamp (Locale_Full_Name (File));
   end File_Time_Stamp;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (File : in out Virtual_File) is
   begin
      if File.Value /= null then
         File.Value.Ref_Count := File.Value.Ref_Count - 1;

         if File.Value.Ref_Count = 0 then
            Free (File.Value.Full_Name);
            Free (File.Value.Base_Name);
            Free (File.Value.Dir_Name);
            Free (File.Value.Normalized_Full);
            Unchecked_Free (File.Value);
         end if;
      end if;
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

   ----------
   -- Sort --
   ----------

   procedure Sort (Files : in out File_Array) is
      --  ??? Right now, this sorts only on the full name. Do we want to
      --  provide other choices for sorting ?

      procedure Xchg (Op1, Op2 : Natural);
      --  Exchanges two items in the array.

      function Lt (Op1, Op2 : Natural) return Boolean;
      --  Return True if the first item is to be sorted before the second.

      procedure Xchg (Op1, Op2 : Natural) is
         Buffer : Virtual_File;
      begin
         Buffer := Files (Files'First - 1 + Op1);
         Files (Files'First - 1 + Op1) := Files (Files'First - 1 + Op2);
         Files (Files'First - 1 + Op2) := Buffer;
      end Xchg;

      function Lt (Op1, Op2 : Natural) return Boolean is
      begin
         --  ??? What about case sensitivity ?
         --  Do we lower-case file names automatically if we are on a
         --  case-insensitive system ? would it be interesting to do so ?
         --  We need to decide on this and then either implement the suggestion
         --  above, or else do a case-insensitive compare just below.
         --  (Or maybe write a "Case_Insensitive_Lt" for more efficiency).

         return
           Files (Files'First - 1 + Op2).Value /= null
            and then Files (Files'First - 1 + Op1).Value /= null
            and then Files (Files'First - 1 + Op1).Value.Full_Name.all
              < Files (Files'First - 1 + Op2).Value.Full_Name.all;
      end Lt;
   begin
      Sort (Files'Length, Xchg'Unrestricted_Access, Lt'Unrestricted_Access);
   end Sort;

end VFS;
