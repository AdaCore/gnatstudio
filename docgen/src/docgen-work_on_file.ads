-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
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

--  This package is the part of the Docgen tool responsable for the
--  processing of the program structure information for the source file
--  list passed by the procedure Docgen.

with Glide_Kernel;
with String_Hash;

package Docgen.Work_On_File is

   procedure Free_All_Tree (X : in out Scope_Tree);
   --  Subprogram called in order to free the memory catched by each element
   --  of the following hash table.

   package Tree_Htable is new String_Hash
     (Data_Type => Scope_Tree,
      Free_Data => Free_All_Tree,
      Null_Ptr  => Null_Scope_Tree);
   use Tree_Htable.String_Hash_Table;
   --  For each ali file, its scope tree is created one time and then it's
   --  stored in the hash table for all the process. In fact, a scope tree
   --  can be used by several files.

   procedure Process_Files
     (B                : Backend_Handle;
      Source_File_List : in out Docgen.Type_Source_File_List.List;
      Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
      Options          : Docgen.All_Options;
      Doc_Suffix       : String;
      Converter        : Docgen.Doc_Subprogram_Type);
   --  Process all files from Source_File_List, and generate their
   --  documentation. Converter indicates what format the documentation should
   --  be generated in.
   --
   --  ??? Doc_Suffix suffix should be a primitive operation of the converter

end Docgen.Work_On_File;
