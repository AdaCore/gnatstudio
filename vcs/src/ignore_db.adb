-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2006                         --
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

with Ada.Strings.Fixed;          use Ada.Strings.Fixed; use Ada.Strings;
with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with GNAT.OS_Lib;                use GNAT;
with GNAT.Strings;

with String_Hash;

package body Ignore_Db is

   use type GNAT.Strings.String_Access;

   --  Table of files

   Not_Null : constant GNAT.Strings.String_Access := new String'("");

   procedure No_Free (Str : in out GNAT.Strings.String_Access);

   package Files_Table is new String_Hash
     (GNAT.Strings.String_Access, No_Free, null);
   use Files_Table;

   type File_Table_Access is access String_Hash_Table.HTable;

   --  Table of directory containing ignore file

   procedure Free is new Ada.Unchecked_Deallocation
     (String_Hash_Table.HTable, File_Table_Access);

   package Dir_Table is new String_Hash (File_Table_Access, Free, null);

   DB : Dir_Table.String_Hash_Table.HTable;

   function Load (Ignore_File : String) return File_Table_Access;
   --  Load Ignore_File and store the values into DB. Returns the new table
   --  created.

   -----------------
   -- Ignore_File --
   -----------------

   function Ignore_File
     (VCS  : VCS_Access;
      File : Virtual_File)
      return Boolean
   is
      Ign_File : constant String :=
                   Dir_Name (File).all & Ignore_Filename (VCS);
      Files    : File_Table_Access :=
                   Dir_Table.String_Hash_Table.Get (DB, Ign_File);
   begin
      if Files = null
        and then OS_Lib.Is_Regular_File (Ign_File)
      then
         Files := Load (Ign_File);
      end if;

      if Files /= null then
         return String_Hash_Table.Get
           (Files.all, Base_Name (File)) /= null;
      else
         return False;
      end if;
   end Ignore_File;

   ----------
   -- Load --
   ----------

   function Load (Ignore_File : String) return File_Table_Access is
      Files  : constant File_Table_Access := new String_Hash_Table.HTable;
      File   : File_Type;
      Buffer : String (1 .. 256);
      Last   : Natural;
   begin
      --  Add all files read from Ignore_File

      Open (File, In_File, Ignore_File);

      while not End_Of_File (File) loop
         Get_Line (File, Buffer, Last);
         String_Hash_Table.Set
           (Files.all, Trim (Buffer (1 .. Last), Both), Not_Null);
      end loop;

      Close (File);

      --  Register this table

      Dir_Table.String_Hash_Table.Set (DB, Ignore_File, Files);

      return Files;
   end Load;

   -------------
   -- No_Free --
   -------------

   procedure No_Free (Str : in out GNAT.Strings.String_Access) is
      pragma Unreferenced (Str);
   begin
      null;
   end No_Free;

end Ignore_Db;
