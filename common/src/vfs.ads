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

with Ada.Calendar;
with Ada.Finalization;
with Ada.Containers;

with GNAT.Calendar;
with GNAT.OS_Lib;
with GNAT.Strings;

with Glib;               use Glib;

with String_List_Utils;  use String_List_Utils;

package VFS is

   VFS_Directory_Error : exception;

   No_Time : constant Ada.Calendar.Time := GNAT.Calendar.Time_Of
     (GNAT.OS_Lib.GM_Year (GNAT.OS_Lib.Invalid_Time),
      GNAT.OS_Lib.GM_Month (GNAT.OS_Lib.Invalid_Time),
      GNAT.OS_Lib.GM_Day (GNAT.OS_Lib.Invalid_Time),
      GNAT.OS_Lib.GM_Hour (GNAT.OS_Lib.Invalid_Time),
      GNAT.OS_Lib.GM_Minute (GNAT.OS_Lib.Invalid_Time),
      GNAT.OS_Lib.GM_Second (GNAT.OS_Lib.Invalid_Time));

   subtype UTF8_String_Access is GNAT.Strings.String_Access;
   type Cst_UTF8_String_Access is access constant Glib.UTF8_String;

   type Virtual_File is tagged private;
   No_File        : constant Virtual_File;
   Local_Root_Dir : constant Virtual_File;

   type Virtual_File_Access is access constant Virtual_File;

   type File_Array is array (Positive range <>) of Virtual_File;
   type File_Array_Access is access all File_Array;
   procedure Unchecked_Free (Arr : in out File_Array_Access);

   Empty_File_Array : constant File_Array;

   function Create (Full_Filename : UTF8_String) return Virtual_File;
   --  Return a file, given its full filename.
   --  The latter can be found, for source files, through the functions in
   --  projects-registry.ads.

   function Create
     (Host          : UTF8_String;
      Full_Filename : UTF8_String) return Virtual_File;
   --  Return a file, given its full filename and host name.
   --  The latter can be found, for source files, through the functions in
   --  projects-registry.ads.

   function Create_From_Dir
     (Dir       : Virtual_File;
      Base_Name : UTF8_String) return Virtual_File;
   --  Creates a file from its directory and base name

   function Create_From_Base (Base_Name : UTF8_String) return Virtual_File;
   --  Return a file, given its base name.
   --  The full name will never be computable. Consider using Projects.Create
   --  if you know to which project the file belongs. Also consider using
   --  GPS.Kernel.Create
   --
   --  ??? Currently, this does the same thing as create, but it is
   --  preferable to distinguish both cases just in case.

   function Create (Files : String_List.List) return File_Array;
   --  Returns a File_Array out of a string list of file names

   function Is_Local (File : Virtual_File) return Boolean;
   --  Tell if the file is local

   function Is_Regular_File (File : Virtual_File) return Boolean;
   --  Whether File corresponds to an actual file on the disk.
   --  This also works for remote files.

   function "=" (File1, File2 : Virtual_File) return Boolean;
   --  Overloading of the standard operator

   function "<" (File1, File2 : Virtual_File) return Boolean;
   --  Compare two files, possibly case insensitively on file systems that
   --  require this.

   function Is_Parent (Parent, Child : Virtual_File) return Boolean;
   --  Compare Parent and Child directory and determines if Parent contains
   --  Child directory

   function Base_Name
     (File : Virtual_File; Suffix : String := "") return Glib.UTF8_String;
   --  Return the base name of the file

   function Base_Dir_Name
     (File : Virtual_File) return Glib.UTF8_String;
   --  Return the base name of the directory or the file

   function Full_Name
     (File : Virtual_File; Normalize : Boolean := False)
      return Cst_UTF8_String_Access;
   --  Return the full path to File.
   --  If Normalize is True, the file name is first normalized, note that links
   --  are not resolved there.
   --  The returned value can be used to recreate a Virtual_File instance.
   --  If file names are case insensitive, the normalized name will always
   --  be all lower cases.

   function Full_Name_Hash
     (Key : Virtual_File) return Ada.Containers.Hash_Type;
   --  Return a Hash_Type computed from the full name of the given VFS.
   --  Could be used to instantiate an Ada 2005 container that uses a VFS as
   --  key and requires a hash function.

   function File_Extension (File : Virtual_File) return UTF8_String;
   --  Return the extension of the file, or the empty string if there is no
   --  extension. This extension includes the last dot and all the following
   --  characters.

   function Get_Host (File : Virtual_File) return UTF8_String;
   --  Returns the host containing the file. If the host is the localhost,
   --  the empty string is returned.

   function Dir_Name (File : Virtual_File) return Cst_UTF8_String_Access;
   --  Return the directory name for File. This includes any available
   --  on the protocol, so that relative files names are properly found.

   function Dir (File : Virtual_File) return Virtual_File;
   --  Return the virtual file corresponding to the directory of the file

   procedure Rename
     (File      : Virtual_File;
      Full_Name : String;
      Success   : out Boolean);
   --  Rename a file or directory. This does not work for remote files

   procedure Copy
     (File        : Virtual_File;
      Target_Name : String;
      Success     : out Boolean);
   --  Copy a file or directory. This does not work for remote files

   procedure Delete (File : Virtual_File; Success : out Boolean);
   --  Remove file from the disk. This also works for remote files

   function Is_Writable (File : Virtual_File) return Boolean;
   --  Return True if File is writable

   function Is_Directory (VF : Virtual_File) return Boolean;
   --  Return True if File is in fact a directory

   function Is_Symbolic_Link (File : Virtual_File) return Boolean;
   --  Return True if File is a symbolic link

   function Is_Absolute_Path (File : Virtual_File) return Boolean;
   --  Return True if File contains an absolute path name, False if it only
   --  contains the base name or a relative name.

   procedure Set_Writable (File : VFS.Virtual_File; Writable : Boolean);
   --  If Writable is True, make File writable, otherwise make File unwritable

   procedure Set_Readable (File : VFS.Virtual_File; Readable : Boolean);
   --  If Readable is True, make File readable, otherwise make File unreadable

   function File_Time_Stamp (File : Virtual_File) return Ada.Calendar.Time;
   --  Return the timestamp for this file. This is GMT time, not local time.
   --  Note: we do not return GNAT.OS_Lib.OS_Time, since the latter cannot be
   --  created by anyone, and is just a private type.
   --  If the file doesn't exist, No_Time is returned.

   procedure Sort (Files : in out File_Array);
   --  Sort the array of files, in the order given by the full names

   -------------------
   -- Reading files --
   -------------------

   function Read_File (File : Virtual_File) return GNAT.Strings.String_Access;
   --  Return the contents of an entire file, encoded with the locale encoding.
   --  If the file cannot be found, return null.
   --  The caller is responsible for freeing the returned memory.
   --  This works transparently for remote files

   --------------------------
   -- Directory operations --
   --------------------------

   function Get_Current_Dir return Virtual_File;

   procedure Ensure_Directory (Dir : Virtual_File);
   --  Ensures that the file is a directory: add directory separator if
   --  needed.

   function Get_Root (File : Virtual_File) return Virtual_File;
   --  returns root directory of the file

   function Get_Parent (Dir : Virtual_File) return Virtual_File;
   --  return the parent directory if it exists, else No_File is returned

   function Sub_Dir
     (Dir : Virtual_File; Name : UTF8_String) return Virtual_File;
   --  returns sub directory Name if it exists, else No_File is returned

   procedure Change_Dir (Dir : Virtual_File);
   --  Changes working directory. Raises Directory_Error if Dir_Name does not
   --  exist or is not a readable directory

   procedure Make_Dir (Dir : Virtual_File);
   --  Create a new directory named Dir_Name. Raises Directory_Error if
   --  Dir_Name cannot be created.

   type Read_Dir_Filter is (All_Files, Dirs_Only, Files_Only);

   function Read_Dir
     (Dir    : Virtual_File;
      Filter : Read_Dir_Filter := All_Files) return File_Array_Access;
   --  Reads all entries from the directory and returns a File_Array containing
   --  those entries, according to filter. The list of files returned
   --  includes directories in systems providing a hierarchical directory
   --  structure, including . (the current directory) and .. (the parent
   --  directory) in systems providing these entries.

   type Virtual_Dir is private;

   Invalid_Dir : constant Virtual_Dir;

   function Open_Dir (Dir : Virtual_File) return Virtual_Dir;
   --  Opens for reading a file

   procedure Read (VDir : in out Virtual_Dir; File : out Virtual_File);
   --  Returns next file or No_File is no file is left for current directory

   procedure Close (VDir : in out Virtual_Dir);
   --  Closes the Virtual_Dir

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

   Invalid_File : constant Writable_File;
   --  Used when a file couldn't be open

   function Write_File
     (File   : Virtual_File;
      Append : Boolean := False) return Writable_File;
   --  Open File for writing. The returned handler can be used for writting.
   --  You must close it, otherwise the file will not actually be written in
   --  some cases. If Append is True then writting will be done at the end of
   --  the file if the file exists otherwise the file is created.
   --  Return Invalid_File is the file couldn't be open for writing

   procedure Write
     (File : in out Writable_File;
      Str  : String);
   --  Write a string to File. The contents of Str are written as-is

   procedure Close (File : in out Writable_File);
   --  Closes File, and write the file to disk.
   --  Use_Error is raised if the file could not be saved.

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

   function Locale_Dir_Name (File : Virtual_File) return String;
   --  Same as Dir_Name

private
   --  This type is implemented as a controlled type, to ease the memory
   --  management (so that we can have gtk+ callbacks that take a Virtual
   --  File in argument, without caring who has to free the memory).
   --  Other solutions (using Name_Id to store the strings for instance) do
   --  not work properly, since the functions above cannot modify File
   --  itself, although they do compute some information lazily).

   type File_Type is
     (Unknown,
      --  File is not determined
      File,
      --  Regular file
      Directory
      --  Directory
      );

   type Contents_Record is record
      Server          : GNAT.Strings.String_Access;
      Ref_Count       : Natural := 1;
      Full_Name       : GNAT.Strings.String_Access;
      Normalized_Full : GNAT.Strings.String_Access;
      Dir_Name        : GNAT.Strings.String_Access;
      Kind            : File_Type := Unknown;
   end record;
   --  Start_Of_Path is the index in Full_Name where the remote path starts.
   --  For local files, this is Full_Name'First, for remote file it points
   --  after the protocol description.

   type Contents_Access is access Contents_Record;

   type Virtual_File is new Ada.Finalization.Controlled with record
      Value : Contents_Access;
   end record;

   pragma Finalize_Storage_Only (Virtual_File);
   procedure Finalize (File : in out Virtual_File);
   procedure Adjust (File : in out Virtual_File);

   type Writable_File is record
      File     : Virtual_File;
      FD       : GNAT.OS_Lib.File_Descriptor := GNAT.OS_Lib.Invalid_FD;
      Filename : GNAT.Strings.String_Access;
      Append   : Boolean;
   end record;

   Invalid_File : constant Writable_File :=
     ((Ada.Finalization.Controlled with Value => null),
      GNAT.OS_Lib.Invalid_FD, null, False);

   type Virtual_Dir is record
      File       : Virtual_File;
      Files_List : File_Array_Access;
      Current    : Natural;
   end record;

   No_File : constant Virtual_File :=
     (Ada.Finalization.Controlled with Value => null);

   Empty_File_Array : constant File_Array :=
                        File_Array'(1 .. 0 => No_File);

   Local_Root_Dir : constant Virtual_File :=
     (Ada.Finalization.Controlled with Value => new Contents_Record'(
        Server          => new String'(""),
        Ref_Count       => 1,
        Full_Name       => new String'(1 => GNAT.OS_Lib.Directory_Separator),
        Normalized_Full => new String'(1 => GNAT.OS_Lib.Directory_Separator),
        Dir_Name        => new String'(1 => GNAT.OS_Lib.Directory_Separator),
        Kind            => Directory));

   Invalid_Dir : constant Virtual_Dir :=
     ((Ada.Finalization.Controlled with Value => null),
      null,
      0);

   procedure Finalize (Value : in out Contents_Access);
   --  Internal version of Finalize

end VFS;
