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

--  This package provides a facility to create a bi-associative table
--  of strings.

with String_Hash;
with GNAT.OS_Lib; use GNAT.OS_Lib;

package Basic_Mapper is

   type File_Mapper is limited private;
   type File_Mapper_Access is access File_Mapper;

   function Get_Other_Text
     (Mapper : File_Mapper_Access;
      Text   : String) return String;
   --  Return the file that has been entered as corresponding to
   --  Text in Mapper, or "" if no such entry was found.

   procedure Add_Entry
     (Mapper : in out File_Mapper_Access;
      Text_1 : String;
      Text_2 : String);
   --  Add a couple of corresponding strings in Mapper.

   procedure Remove_Entry
     (Mapper : in out File_Mapper_Access;
      Text   : String);
   --  Remove all the entries containing Text on either table
   --  from Mapping.

   procedure Save_Mapper
     (Mapper    : File_Mapper_Access;
      File_Name : String);
   --  Save Mapper to file File_Name.

   procedure Load_Mapper
     (Mapper    : out File_Mapper_Access;
      File_Name : String);
   --  Load Mapper from file File_Name.

   procedure Free (Mapper : in out File_Mapper_Access);
   --  Free the memory occupied by Mapper and its components.
   --  On exit, Mapper is null.

private

   No_Element : constant String_Access := null;

   package Double_String_Table is new String_Hash (String_Access, No_Element);

   type File_Mapper is limited record
      Table_1 : Double_String_Table.String_Hash_Table.HTable;
      Table_2 : Double_String_Table.String_Hash_Table.HTable;
   end record;

end Basic_Mapper;
