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

with Glib;          use Glib;
with GNAT.OS_Lib;
with Ada.Finalization;

package VFS is

   subtype UTF8_String_Access is GNAT.OS_Lib.String_Access;
   type Cst_UTF8_String_Access is access constant Glib.UTF8_String;

   type Virtual_File is private;
   No_File : constant Virtual_File;

   type File_Array is array (Natural range <>) of Virtual_File;
   type File_Array_Access is access File_Array;
   procedure Unchecked_Free (Arr : in out File_Array_Access);

   function Create (Full_Filename : UTF8_String) return Virtual_File;
   --  Return a file, given its full filename.
   --  The latter can be found, for source files, through the functions in
   --  projects-registry.ads.
   --  The user must call Destroy on the result

   function Create_From_Base (Base_Name : UTF8_String) return Virtual_File;
   --  Return a file, given its base name.
   --  The full name will never be computable. Consider using Projects.Create
   --  if you know to which project the file belongs. Also consider using
   --  Glide_Kernel.Create
   --
   --  ??? Currently, this does the same thing as create, but it is
   --  preferable to distinguish both cases just in case.

   function Is_Regular_File (File : Virtual_File) return Boolean;
   --  Whether File corresponds to an actual file on the disk.
   --  This also works for remote files.

   function "=" (File1, File2 : Virtual_File) return Boolean;
   --  Overloading of the standard operator

   function "<" (File1, File2 : Virtual_File) return Boolean;
   --  Compare two files, possibly case insensitively on file systems that
   --  require this.

   function Base_Name
     (File   : Virtual_File; Suffix : String := "") return Glib.UTF8_String;
   --  Return the base name of the file.

   function Full_Name
     (File : Virtual_File; Normalize : Boolean := False)
      return Cst_UTF8_String_Access;
   --  Return the full path to File
   --  If Normalize is True, the file name is first normalized, and links
   --  are resolved on systems where it applies.
   --  The returned value can be used to recreate a Virtual_File instance.
   --  If file names are case insensitive, the normalized name will always
   --  be all lower cases.

   function File_Extension (File : Virtual_File) return UTF8_String;
   --  Return the extension of the file, or the empty string if there is no
   --  extension. This extension includes the last dot and all the following
   --  characters;

   function Dir_Name (File : Virtual_File) return Cst_UTF8_String_Access;
   --  Return the directory name for File

   function Read_File (File : Virtual_File) return UTF8_String_Access;
   --  Return the contents of an entire file.
   --  If the file cannot be found, return null.
   --  The caller is responsible for freeing the returned memory.
   --  This works transparently for remote files

   procedure Delete (File : Virtual_File);
   --  Remove file from the disk. This also works for remote files

   function Is_Writable (File : Virtual_File) return Boolean;
   --  Return True if File is writable

   function Is_Directory (File : Virtual_File) return Boolean;
   --  Return True if File is in fact a directory

   function Is_Absolute_Path (File : Virtual_File) return Boolean;
   --  Return True if File contains an absolute path name, False if it only
   --  contains the base name or a relative name.

   function File_Time_Stamp (File : Virtual_File) return GNAT.OS_Lib.OS_Time;
   --  Return the timestamp for this file

--   procedure Define_Translation
--     (Host_Dir : String; Remote_Dir : String);
   --  Defines a translation for file names: any occurrence of Host_Dir at
   --  the beginning of the file name will be replaced by Remote_Dir.

   procedure Sort (Files : in out File_Array);
   --  Sort the array of files, in the order given by the full names.

   -------------------
   -- Writing files --
   -------------------
   --  Writing is more complex than reading, since generally the whole buffer
   --  to write down is not available immediately, but the user wants to be
   --  able to write characters in a series of calls.
   --  The interface in this package will also support remote files. In this
   --  case, writing the small chunks is done in a temporary file, which is
   --  sent to the remote host only when the file is closed.

   type Writable_File is private;

   function Write_File (File : Virtual_File) return Writable_File;
   --  Open File for writting. The returned handler can be used for writting.
   --  You must close it, otherwise the file will not actually be written in
   --  some cases

   procedure Write
     (File : in out Writable_File;
      Str : UTF8_String;
      As_UTF8 : Boolean := True);
   --  Write a string to File. The required encoding for the string depends
   --  on As_UTF8.

   procedure Close (File : in out Writable_File);
   --  Closes File, and write the file to disk

   ---------------------
   -- Locale encoding --
   ---------------------
   --  The following functions return their result encoded in the locale
   --  charset, suitable for calls to the C library and low-level manipulations
   --  of files.

   function Locale_Full_Name (File : Virtual_File) return String;
   --  Same as Full_Name

   function Locale_Base_Name (File : Virtual_File) return String;
   --  Same as Base_Name

private
   --  This type is implemented as a controlled type, to ease the memory
   --  management (so that we can have gtk+ callbacks that take a Virtual
   --  File in argument, without caring who has to free the memory).
   --  Other solutions (using Name_Id to store the strings for instance) do
   --  not work properly, since the functions above cannot modify File
   --  itself, although they do compute some information lazily).

   type Contents_Record is record
      Ref_Count        : Natural := 1;
      Full_Name        : GNAT.OS_Lib.String_Access;
      Normalized_Full  : GNAT.OS_Lib.String_Access;
      Dir_Name         : GNAT.OS_Lib.String_Access;
   end record;
   type Contents_Access is access Contents_Record;

   type Virtual_File is new Ada.Finalization.Controlled with record
      Value : Contents_Access;
   end record;

   procedure Finalize (File : in out Virtual_File);
   procedure Adjust (File : in out Virtual_File);

   type Writable_File is record
      File : Virtual_File;
      FD   : GNAT.OS_Lib.File_Descriptor := GNAT.OS_Lib.Invalid_FD;
   end record;

   No_File : constant Virtual_File :=
     (Ada.Finalization.Controlled with Value => null);
end VFS;
