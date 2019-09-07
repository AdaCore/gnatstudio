------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2003-2019, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

--  This package provides a facility to create a bi-associative table
--  of strings.

with String_Hash;
with GNAT.OS_Lib;  use GNAT.OS_Lib;
with GNATCOLL.VFS; use GNATCOLL.VFS;

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
   --  Add a couple of corresponding strings in Mapper

   procedure Remove_Entry
     (Mapper : File_Mapper_Access;
      Text   : String);
   --  Remove all the entries containing Text on either table
   --  from Mapping.

   procedure Save_Mapper
     (Mapper    : File_Mapper_Access;
      File_Name : Virtual_File);
   --  Save Mapper to file File_Name

   procedure Load_Mapper
     (Mapper    : out File_Mapper_Access;
      File_Name : Virtual_File);
   --  Load Mapper from file File_Name

   procedure Empty_Mapper (Mapper : out File_Mapper_Access);
   --  Return an empty mapper

   procedure Free (Mapper : in out File_Mapper_Access);
   --  Free the memory occupied by Mapper and its components.
   --  On exit, Mapper is null.

private

   No_Element : constant String_Access := null;

   procedure False_Free (X : in out String_Access);
   --  False Free procedure.
   --  The actual freeing occurs automatically, since the elements for one
   --  table are the keys for another table, and vice versa.

   package Double_String_Table is
     new String_Hash (String_Access, False_Free, No_Element);

   type File_Mapper is limited record
      Table_1 : Double_String_Table.String_Hash_Table.Instance;
      Table_2 : Double_String_Table.String_Hash_Table.Instance;
   end record;

end Basic_Mapper;
