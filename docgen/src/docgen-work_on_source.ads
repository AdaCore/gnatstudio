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

--  This package provided with the entity lists from the package
--  Docgen.Work_On_File will parse the source files in order to get
--  the missing information, like the entity headers and their
--  descriptions.

--  The three procedures Process_Unit_Index, Process_Subprogram_Index,
--  and Process_Type_Index will generate the index doc pages by calling for
--  each entity the subprogram from an output package (like Docgen.Html_Output
--  or Docgen.Texi_Output).

--  The procedure Process_Source provided with all the list for the current
--  source file, will call some private procedures of this package to
--  create the documentation of each entity type. It is here, where the
--  order of the entity types in the final documentation is set. The output
--  formats creating one doc file for each source file (like HTML) and
--  the ones creating only one file for all source files must be process
--  differently. There is also a different manner of processing spec files
--  and body files. The private functions used will call the subprogram from
--  an output package (like Docgen.Html_Output or Docgen.Texi_Output).

with GNAT.OS_Lib;
with VFS;
with Glide_Kernel;

package Docgen.Work_On_Source is

   procedure Process_Source
     (B                 : Backend_Handle;
      Kernel            : access Glide_Kernel.Kernel_Handle_Record'Class;
      Doc_File          : File_Type;
      Next_Package      : GNAT.OS_Lib.String_Access;
      Prev_Package      : GNAT.OS_Lib.String_Access;
      Source_File_List  : in out Type_Source_File_List.List;
      Source_Filename   : VFS.Virtual_File;
      Package_Name      : String;
      Entity_List       : in out Type_Entity_List.List;
      List_Ref_In_File  : in out List_Reference_In_File.List;
      Process_Body_File : Boolean;
      LI_Unit           : LI_File_Ptr;
      Options           : All_Options;
      Converter         : Docgen.Doc_Subprogram_Type;
      Doc_Directory     : String;
      Doc_Suffix        : String);
   --  With the data from the lists, the source file and the config file,
   --  create the Strings for the output.
   --  The order of the procedure calls can't be changed here
   --  without changing the order in texi_output!
   --  Change also: Doc_TEXI_Subtitle !!!

   procedure Process_Unit_Index
     (B                : Backend_Handle;
      Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
      Source_File_List : Docgen.Type_Source_File_List.List;
      Entity_List      : in out Type_Entity_List.List;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Options          : Docgen.All_Options;
      Converter        : Docgen.Doc_Subprogram_Type;
      Doc_Directory    : String;
      Doc_Suffix       : String);
   --  Create the index file for the packages

   procedure Process_Subprogram_Index
     (B                     : Backend_Handle;
      Kernel                : access Glide_Kernel.Kernel_Handle_Record'Class;
      Subprogram_Index_List : Docgen.Type_Entity_List.List;
      Private_Subprogram_Index_List : in out Type_Entity_List.List;
      Entity_List           : in out Type_Entity_List.List;
      List_Ref_In_File      : in out List_Reference_In_File.List;
      Options               : Docgen.All_Options;
      Converter             : Docgen.Doc_Subprogram_Type;
      Doc_Directory         : String;
      Doc_Suffix            : String);
   --  Create the index file for the subprograms

   procedure Process_Type_Index
     (B                : Backend_Handle;
      Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
      Type_Index_List  : Docgen.Type_Entity_List.List;
      Private_Type_Index_List : in out Type_Entity_List.List;
      Entity_List      : in out Type_Entity_List.List;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Options          : All_Options;
      Converter        : Doc_Subprogram_Type;
      Doc_Directory    : String;
      Doc_Suffix       : String);
   --  Create the index file for the types

   procedure Process_Tagged_Type_Index
     (B                      : Backend_Handle;
      Kernel                 : access Glide_Kernel.Kernel_Handle_Record'Class;
      Tagged_Type_Index_List : Docgen.Type_List_Tagged_Element.List;
      Private_Tagged_Types_List : in out Type_List_Tagged_Element.List;
      Entity_List            : in out Type_Entity_List.List;
      List_Ref_In_File       : in out List_Reference_In_File.List;
      Source_File_List       : in out Type_Source_File_List.List;
      Options                : All_Options;
      Converter              : Doc_Subprogram_Type;
      Doc_Directory          : String;
      Doc_Suffix             : String);
   --  Create the index file for the tagged types

end Docgen.Work_On_Source;
