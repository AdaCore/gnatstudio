-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2005                       --
--                             AdaCore                               --
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
--  and Process_Type_Index will generate the index doc pages by calling
--  the appropriate docgen backend.

--  The procedure Process_Source provided with all the list for the current
--  source file, will call some private procedures of this package to
--  create the documentation of each entity type. It is here where the
--  order of the entity types in the final documentation is set. The output can
--  be made of one doc file per source file (like HTML) or one file for all
--  source files.
--  There is also a different manner of processing spec files
--  and body files. The private functions used will call backend subprograms.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Docgen.Backend;
with VFS;

package Docgen.Work_On_Source is

   procedure Process_Source
     (B                         : access Docgen.Backend.Backend'Class;
      Kernel                    : access Kernel_Handle_Record'Class;
      Result                    : in out Unbounded_String;
      Source_File_List          : in out Type_Source_File_Table.HTable;
      Source_Filename           : VFS.Virtual_File;
      Source_Is_Spec            : Boolean;
      Unit_Name                 : String;
      Entity_List               : in out Type_Entity_List.List;
      List_Ref_In_File          : in out List_Reference_In_File.List;
      Tagged_Types_List         : in out List_Entity_Information.List;
      Private_Tagged_Types_List : in out List_Entity_Information.List;
      Options                   : All_Options;
      Level                     : in out Natural);
   --  With the data from the lists, the source file and the config file,
   --  create the Strings for the output.
   --  The order of the procedure calls can't be changed here
   --  without changing the order in texi_output!
   --  Change also Doc_TEXI_Subtitle !!!
   --  Source_File_List : list of all files that must be processed by docgen.
   --  Source_Filename  : current file processed.
   --  Package_Name     : name of the current package. For this subprogram,
   --  it's always the name of the main package which is defined.
   --  Entity_List      : list of entities in the current file.
   --  List_Ref_In_File : list of references in the current file.
   --  Tagged_Types_List: list of public tagged types.
   --  Private_Tagged_Types_List: list of private tagged types.
   --  Options          : options set by the preferences.
   --  Process_Body_File: indicate if bofy files must be processed.
   --  ???  This last parameter is redundant because Options indicate it.
   --  Level            : the level of the current package. By default, the
   --  level of the package file is 1, then this level is increased by 1 at
   --  each inner package

   procedure Process_Unit_Index
     (B                : access Docgen.Backend.Backend'Class;
      Kernel           : access Kernel_Handle_Record'Class;
      Source_File_List : Docgen.Type_Source_File_Table.HTable;
      Options          : Docgen.All_Options;
      Level            : in out Natural);
   --  Create the index file for the packages

   procedure Process_Subprogram_Index
     (B                             : access Docgen.Backend.Backend'Class;
      Kernel                        : access Kernel_Handle_Record'Class;
      Subprogram_Index_List         : Docgen.Type_Entity_List.List;
      Private_Subprogram_Index_List : Type_Entity_List.List;
      Source_File_List              : Type_Source_File_Table.HTable;
      Options                       : Docgen.All_Options);
   --  Create the index file for the subprograms
   --  Subprogram_Index_List         : list of public subprograms.
   --  Private_Subprogram_Index_List : list of private subprograms.

   procedure Process_Type_Index
     (B                       : access Docgen.Backend.Backend'Class;
      Kernel                  : access Kernel_Handle_Record'Class;
      Type_Index_List         : Docgen.Type_Entity_List.List;
      Private_Type_Index_List : Type_Entity_List.List;
      Source_File_List        : Type_Source_File_Table.HTable;
      Options                 : All_Options);
   --  Create the index file for the types.
   --  Type_Index_List         : list of public types.
   --  Private_Type_Index_List : list of private types.

   procedure Process_Tagged_Type_Index
     (B                         : access Docgen.Backend.Backend'Class;
      Kernel                    : access Kernel_Handle_Record'Class;
      Tagged_Type_Index_List    : List_Entity_Information.List;
      Private_Tagged_Types_List : List_Entity_Information.List;
      Source_File_List          : Type_Source_File_Table.HTable;
      Options                   : All_Options);
   --  Create the index file for the tagged types.
   --  Tagged_Type_Index_List    : list of public tagged types.
   --  Private_Tagged_Types_List : list of private tagged types.

end Docgen.Work_On_Source;
