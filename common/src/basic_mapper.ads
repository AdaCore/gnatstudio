-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2002                         --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with String_Hash;
with GNAT.OS_Lib;   use GNAT.OS_Lib;

package Basic_Mapper is

   type File_Mapper is limited private;
   type File_Mapper_Access is access File_Mapper;

   function Get_Other_File
     (Mapper    : File_Mapper_Access;
      File_Name : String) return String;

   procedure Add_Entry
     (Mapper      : in out File_Mapper_Access;
      File_Name_1 : String;
      File_Name_2 : String);

   procedure Save_Mapper
     (Mapper      : File_Mapper_Access;
      File_Name   : String);

   procedure Load_Mapper
     (Mapper      : out File_Mapper_Access;
      File_Name   : String);

private

   No_Element : constant String_Access := null;

   package Double_String_Table is new String_Hash (String_Access, No_Element);

   type File_Mapper is limited record
      Table_1 : Double_String_Table.String_Hash_Table.HTable;
      Table_2 : Double_String_Table.String_Hash_Table.HTable;
   end record;

end Basic_Mapper;
