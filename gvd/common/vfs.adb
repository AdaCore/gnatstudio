-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                   Copyright (C) 2003                              --
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
with String_Utils;               use String_Utils;
with GNAT.Case_Util;             use GNAT.Case_Util;
with Traces;                     use Traces;

package body VFS is

   Me : constant Debug_Handle := Create ("VFS");

   type Contents_Record is record
      Ref_Count       : Natural := 1;
      --  This is a reference counter. When it reaches 0, the memory is
      --  freed

      Full_Name        : String_Access;
      Normalized_Full  : String_Access;
   end record;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Contents_Record, Contents_Access);

   procedure Ensure_Normalized (File : Virtual_File);
   --  Ensure that the information for the normalized file have been correctly
   --  computed.

   ------------
   -- Create --
   ------------

   function Create (Full_Filename : UTF8_String) return Virtual_File is
   begin
      return (Ada.Finalization.Controlled with
              Value => new Contents_Record'
                (Ref_Count       => 1,
                 Full_Name       => new String'(Full_Filename),
                 Normalized_Full => null));
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
                 Normalized_Full => null));
   end Create_From_Base;

   -----------------------
   -- Ensure_Normalized --
   -----------------------

   procedure Ensure_Normalized (File : Virtual_File) is
   begin
      if File.Value /= null
        and then File.Value.Normalized_Full = null
      then
         File.Value.Normalized_Full := new UTF8_String'
           (Locale_To_UTF8 (To_Host_Pathname (Normalize_Pathname
                              (Locale_From_UTF8 (File.Value.Full_Name.all),
                                                 Resolve_Links => True))));
         if not File_Utils.Filenames_Are_Case_Sensitive then
            To_Lower (File.Value.Normalized_Full.all);
         end if;
      end if;
   end Ensure_Normalized;

   ---------------
   -- Base_Name --
   ---------------

   function Base_Name
     (File   : Virtual_File;
      Suffix : String := "") return UTF8_String is
   begin
      if File.Value = null then
         return "";
      else
         return Base_Name (File.Value.Full_Name.all, Suffix);
      end if;
   end Base_Name;

   ---------------
   -- Full_Name --
   ---------------

   function Full_Name
     (File : Virtual_File; Normalize : Boolean := False) return UTF8_String is
   begin
      if File.Value = null then
         return "";

      elsif Normalize then
         Ensure_Normalized (File);
         return File.Value.Normalized_Full.all;

      else
         return File.Value.Full_Name.all;
      end if;
   end Full_Name;

   --------------
   -- Dir_Name --
   --------------

   function Dir_Name (File : Virtual_File) return UTF8_String is
   begin
      if File.Value = null then
         return "";
      else
         return Dir_Name (File.Value.Full_Name.all);
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
         return Read_File (Full_Name (File));
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

   ---------
   -- "=" --
   ---------

   function "=" (File1, File2 : Virtual_File) return Boolean is
   begin
      Ensure_Normalized (File1);
      Ensure_Normalized (File2);

      if File1.Value = null then
         return File2.Value = null;
      elsif File2.Value = null then
         return File1.Value = null;
      else
         return File_Utils.File_Equal
           (File1.Value.Normalized_Full.all, File2.Value.Normalized_Full.all);
      end if;
   end "=";

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
      Base : constant String := Base_Name (File);
   begin
      return Locale_From_UTF8 (Base);
   end Locale_Base_Name;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (File : in out Virtual_File) is
   begin
      if File.Value /= null then
         File.Value.Ref_Count := File.Value.Ref_Count - 1;

         if File.Value.Ref_Count = 0 then
            Free (File.Value.Full_Name);
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

   ---------------------
   -- Is_Regular_File --
   ---------------------

   function Is_Regular_File (File : Virtual_File) return Boolean is
   begin
      return File /= No_File and then Is_Regular_File (Full_Name (File));
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


end VFS;
