-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003-2005                       --
--                              AdaCore                              --
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
with GNAT.Calendar;
with GNAT.OS_Lib;

with Glib;               use Glib;
with Glib.Values;
with Remote_Connections;

package VFS is

   VFS_Directory_Error : exception;

   No_Time : constant Ada.Calendar.Time := GNAT.Calendar.Time_Of
     (GNAT.OS_Lib.GM_Year (GNAT.OS_Lib.Invalid_Time),
      GNAT.OS_Lib.GM_Month (GNAT.OS_Lib.Invalid_Time),
      GNAT.OS_Lib.GM_Day (GNAT.OS_Lib.Invalid_Time),
      GNAT.OS_Lib.GM_Hour (GNAT.OS_Lib.Invalid_Time),
      GNAT.OS_Lib.GM_Minute (GNAT.OS_Lib.Invalid_Time),
      GNAT.OS_Lib.GM_Second (GNAT.OS_Lib.Invalid_Time));

   subtype UTF8_String_Access is GNAT.OS_Lib.String_Access;
   type Cst_UTF8_String_Access is access constant Glib.UTF8_String;

   type Virtual_File is private;
   No_File : constant Virtual_File;
   Local_Root_Dir : constant Virtual_File;

   type Virtual_File_Access is access constant Virtual_File;

   type File_Array is array (Natural range <>) of Virtual_File;
   type File_Array_Access is access File_Array;
   procedure Unchecked_Free (Arr : in out File_Array_Access);

   function Create (Full_Filename : UTF8_String) return Virtual_File;
   --  Return a file, given its full filename.
   --  The latter can be found, for source files, through the functions in
   --  projects-registry.ads.

   function Create_From_Base (Base_Name : UTF8_String) return Virtual_File;
   --  Return a file, given its base name.
   --  The full name will never be computable. Consider using Projects.Create
   --  if you know to which project the file belongs. Also consider using
   --  GPS.Kernel.Create
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

   function Is_Parent (Parent, Child : Virtual_File) return Boolean;
   --  Compare Parent and Child directory and determines if Parent contains
   --  Child directory

   function Base_Name
     (File : Virtual_File; Suffix : String := "") return Glib.UTF8_String;
   --  Return the base name of the file

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

   function Get_Host (File : Virtual_File) return UTF8_String;
   --  Returns the host containing the file. If the host is the localhost,
   --  the empty string is returned.

   function Dir_Name (File : Virtual_File) return Cst_UTF8_String_Access;
   --  Return the directory name for File. This includes any available
   --  on the protocol, so that relative files names are properly found.

   function Dir (File : Virtual_File) return Virtual_File;
   --  Return the virtual file corresponding to the directory of the file.

   procedure Delete (File    : in     Virtual_File;
                     Success :    out Boolean);
   --  Remove file from the disk. This also works for remote files

   function Is_Writable (File : Virtual_File) return Boolean;
   --  Return True if File is writable

   function Is_Directory (File : Virtual_File) return Boolean;
   --  Return True if File is in fact a directory

   function Is_Absolute_Path (File : Virtual_File) return Boolean;
   --  Return True if File contains an absolute path name, False if it only
   --  contains the base name or a relative name.

   procedure Set_Writable (File : VFS.Virtual_File; Writable : Boolean);
   --  If Writable is True, make File writable, otherwise make File unwritable.

   procedure Set_Readable (File : VFS.Virtual_File; Readable : Boolean);
   --  If Readable is True, make File readable, otherwise make File unreadable.

   function File_Time_Stamp
     (File : Virtual_File) return Ada.Calendar.Time;
   --  Return the timestamp for this file.
   --  Note: we do not return GNAT.OS_Lib.OS_Time, since the latter cannot be
   --  created by anyone, and is just a private type.
   --  If the file doesn't exist, No_Time is returned.

--   procedure Define_Translation
--     (Host_Dir : String; Remote_Dir : String);
   --  Defines a translation for file names: any occurrence of Host_Dir at
   --  the beginning of the file name will be replaced by Remote_Dir.

   procedure Sort (Files : in out File_Array);
   --  Sort the array of files, in the order given by the full names

   -------------------
   -- Reading files --
   -------------------

   function Read_File (File : Virtual_File) return GNAT.OS_Lib.String_Access;
   --  Return the contents of an entire file, encoded with the locale encoding.
   --  If the file cannot be found, return null.
   --  The caller is responsible for freeing the returned memory.
   --  This works transparently for remote files

   --------------------------
   -- Directory operations --
   --------------------------

   function Get_Current_Dir return Virtual_File;

   procedure Ensure_Directory (Dir : Virtual_File);
   --  Ensures that the file is a directory : adds directory separator if
   --  needed.

   function Get_Root (File : Virtual_File) return Virtual_File;
   --  returns root directory on remote host

   function Sub_Dir (Dir : Virtual_File; Name : UTF8_String)
                     return Virtual_File;
   --  returns sub directory Name if it exists, else No_File is returned.

   procedure Change_Dir (Dir : Virtual_File);
   --  Changes working directory. Raises Directory_Error if Dir_Name does not
   --  exist or is not a readable directory

   procedure Make_Dir (Dir : Virtual_File);
   --  Create a new directory named Dir_Name. Raises Directory_Error if
   --  Dir_Name cannot be created.

   procedure Make_Dir_Recursive (Dir : Virtual_File);
   --  Create recursively a new directory named Dir_Name. Raises
   --  Directory_Error if Dir_Name cannot be created.

   procedure Remove_Dir (Dir : Virtual_File;
                         Recursive : Boolean := False);
   --  Remove the directory named Dir_Name. If Recursive is set to True, then
   --  Remove_Dir removes all the subdirectories and files that are in
   --  Dir_Name. Raises Directory_Error if Dir_Name cannot be removed.

   function Read_Dir (Dir : in Virtual_File) return File_Array_Access;
   --  Reads all entries from the directory and returns a File_Array containing
   --  those entries. The list of files returned
   --  includes directories in systems providing a hierarchical directory
   --  structure, including . (the current directory) and .. (the parent
   --  directory) in systems providing these entries.

   type Virtual_Dir is private;

   Invalid_Dir : constant Virtual_Dir;

   function Open_Dir (Dir : in Virtual_File) return Virtual_Dir;
   --  Opens for reading a file.

   procedure Read (VDir : in out Virtual_Dir;
                   File :    out Virtual_File);
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
   --  Used when a file couldn't be open.

   function Write_File (File : Virtual_File) return Writable_File;
   --  Open File for writing. The returned handler can be used for writting.
   --  You must close it, otherwise the file will not actually be written in
   --  some cases
   --  Return Invalid_File is the file couldn't be open for writing

   procedure Write
     (File    : in out Writable_File;
      Str     : UTF8_String;
      As_UTF8 : Boolean := True);
   --  Write a string to File. The required encoding for the string depends
   --  on As_UTF8: if As_UTF8 is True, the file is converted to Locale before
   --  writing. If As_UTF8 is False, no conversion is performed.

   procedure Close (File : in out Writable_File);
   --  Closes File, and write the file to disk.
   --  Use_Error is raised if the file could not be saved

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

   -------------
   -- Gvalues --
   -------------
   --  The following subprograms are provided to encapsulate a virtual file
   --  in a GValue.

   procedure Set_File (Value : in out Glib.Values.GValue; File : Virtual_File);
   --  Store File into Value

   function Get_File (Value : Glib.Values.GValue) return Virtual_File;
   --  Retrieve the file stored in Value

   function Get_Virtual_File_Type return Glib.GType;
   --  Return the gtype to use for virtual files

private
   --  This type is implemented as a controlled type, to ease the memory
   --  management (so that we can have gtk+ callbacks that take a Virtual
   --  File in argument, without caring who has to free the memory).
   --  Other solutions (using Name_Id to store the strings for instance) do
   --  not work properly, since the functions above cannot modify File
   --  itself, although they do compute some information lazily).

   type Contents_Record is record
      Connection      : Remote_Connections.Remote_Connection;
      Start_Of_Path   : Integer;
      Ref_Count       : Natural := 1;
      Full_Name       : GNAT.OS_Lib.String_Access;
      Normalized_Full : GNAT.OS_Lib.String_Access;
      Dir_Name        : GNAT.OS_Lib.String_Access;
   end record;
   --  Start_Of_Path is the index in Full_Name where the remote path starts.
   --  For local files, this is Full_Name'First, for remote file it points
   --  after the protocol description.

   type Contents_Access is access Contents_Record;

   type Virtual_File is new Ada.Finalization.Controlled with record
      Value : Contents_Access;
   end record;

   procedure Finalize (File : in out Virtual_File);
   procedure Adjust (File : in out Virtual_File);

   type Readable_File is record
      File     : Virtual_File;
   end record;

   type Writable_File is record
      File     : Virtual_File;
      FD       : GNAT.OS_Lib.File_Descriptor := GNAT.OS_Lib.Invalid_FD;
      Filename : GNAT.OS_Lib.String_Access;
   end record;

   No_File : constant Virtual_File :=
     (Ada.Finalization.Controlled with Value => null);

   Local_Root_Dir : constant Virtual_File :=
     (Ada.Finalization.Controlled with Value => new Contents_Record'(
        Connection      => null,
        Start_Of_Path   => 1,
        Ref_Count       => 1,
        Full_Name       => new String'(1 => GNAT.OS_Lib.Directory_Separator),
        Normalized_Full => new String'(1 => GNAT.OS_Lib.Directory_Separator),
        Dir_Name        => new String'(1 => GNAT.OS_Lib.Directory_Separator)));

   Invalid_File : constant Writable_File :=
     ((Ada.Finalization.Controlled with Value => null),
      GNAT.OS_Lib.Invalid_FD, null);

   type Virtual_Dir is record
      File       : Virtual_File;
      Files_List : File_Array_Access;
      Current    : Natural;
   end record;

   Invalid_Dir : constant Virtual_Dir :=
     ((Ada.Finalization.Controlled with Value => null),
      null,
      0);

end VFS;
