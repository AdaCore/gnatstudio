-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2002                         --
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

with Interfaces.C.Strings;
with GNAT.OS_Lib; use GNAT.OS_Lib;

package DB_API is

   type DB_File is private;
   --  Database file descriptor.

   type Cursor_Position is (First, Last, By_Key);
   --  Cursor positions for Set_Cursor.

   type Cursor_Movement is (Prev, Next, Next_By_Key);
   --  Cursor movements fro Get_Pair

   for Cursor_Position use (First => 1, Last => 2, By_Key => 3);
   for Cursor_Movement use (Prev  => 4, Next => 5, Next_By_Key => 6);
   for Cursor_Position'Size use Interfaces.C.int'Size;
   for Cursor_Movement'Size use Interfaces.C.int'Size;

   Field_Sep : constant Character := Character'Val (1);
   --  Standard field separator

   type Integer_Array is array (0 .. 15) of Integer;

   type CSF is record
      Num_Of_Fields : Integer;
      Fields        : Integer_Array;
   end record;

   --  CSF stands for "Character Separated Fields".
   --  Represents key or data value ("CSF" in term of SN team).
   --  Value can contain 0 or more string fields.
   --  Each field can be retrieved by Get_Field function.
   --  ("data base thang" in terms of SN team).

   type Pair is record
      Key    : Interfaces.C.Strings.chars_ptr;
      Data   : Interfaces.C.Strings.chars_ptr;
      DBI    : Integer;
   end record;
   --  Type for key/data pair retrieved from database by Get_Pair
   --  operation.
   No_Pair : constant Pair;

   function "=" (P1, P2 : Pair) return Boolean;
   --  Redefine the standard "=" operator for pairs

   procedure Open
     (DB         : out DB_File;
      File_Names : String_List_Access;
      Success    : out Boolean);
   --  Opens specified file as database file.
   --  Sets cursor to the first key/data pair in database (see
   --  Set_Cursor).
   --  Success holds True upon success and False otherwise

   function Dup (DB : DB_File) return DB_File;
   --  Reopens DB file and returns new DB file descriptor.
   --  Throws DB_Error if DB file was not opened.

   function Is_Open (DB : DB_File) return Boolean;
   pragma Inline (Is_Open);
   --  Returns True if given DB ws successfully open,
   --  False otherwise.

   procedure Close (DB : in out DB_File; Success : out Boolean);
   --  Closes specified DB file and frees underlying resources.
   --  Ignores uninitialized DB. Returns status in Success

   procedure Set_Cursor
     (DB          : DB_File;
      Position    : Cursor_Position;
      Key         : String  := "";
      Exact_Match : Boolean := True);
   --  Sets cursor position in given database.
   --  If Position = First, then cursor is set to the first key/data
   --  pair in database, Key is ignored.
   --  If Position = Last, then cursor is set to the last key/data
   --  pair in database, Key is ignored.
   --  If Position = By_Key, then cursor is set to the first key/data
   --  pair, where key is:
   --    The first key equals to specified Key, if Exact_Match is True.
   --    The smallest one greater than or equals to specified Key, if
   --    Exact_Match is False.
   --  Throws DB_Error if DB was not opened.
   --  Note: only one cursor per DB file can be used (use Dup for
   --  using two or more cursors simultaneously).

   procedure Release_Cursor (DB : DB_File);
   --  Releases resources allocated before by Set_Cursor.
   --  Does nothing if cursor was not set.

   procedure Get_Pair
     (DB       : DB_File;
      Movement : Cursor_Movement := Next;
      Result   : out Pair);
   --  Gets key/data pair from database from cursor position and
   --  changes cursor position to:
   --    Next key/data pair, if Movement is Next.
   --    Previous key/data pair, if Movement is Prev.
   --    Next key/data pair, where key is satisfying conditions set by
   --    Set_Cursor operations, if Movement is Next_By_Key.
   --  Returns null if there are no key/data pairs more.
   --  If Movement is Next_By_Key and cursor was not set by Set_Cursor
   --  with Position = By_Key then Get_Pair returns null;
   --  Throws DB_Error if DB was not opened or error occurred.

   procedure CSF_Init (Str : Interfaces.C.Strings.chars_ptr; Result : out CSF);
   --  Parses the string Str for later analysis through Get_Field

   function Get_Field_Count (The_CSF : CSF) return Natural;
   pragma Inline (Get_Field_Count);
   --  Returns number of fields in specified CSF.

   DB_Error             : exception;
   DB_Close_Error       : exception;
   Index_Out_Of_Range   : exception;

private
   type DB_File_Record is null record;
   type DB_File is access DB_File_Record;
   pragma Convention (C, DB_File);

   function Error_Message (DB : DB_File) return String;
   --  Return string describing the last error for given DB

   pragma Convention (C, CSF);
   pragma Convention (C, Pair);

   No_Pair : constant Pair := (Interfaces.C.Strings.Null_Ptr,
                               Interfaces.C.Strings.Null_Ptr,
                               -1);

   pragma Import (C, CSF_Init, "csf_init");
end DB_API;
