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

with Ada.Text_IO;               use Ada.Text_IO;
with Basic_Types;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Ada.Characters.Handling;   use Ada.Characters.Handling;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Basic_Types;
with Language;                  use Language;
with Language_Handlers;         use Language_Handlers;
with Src_Info.Queries;          use Src_Info.Queries;
with VFS;                       use VFS;
with Glide_Kernel.Project;      use Glide_Kernel, Glide_Kernel.Project;
with Projects.Registry;         use Projects.Registry;
with Traces;                    use Traces;
with Ada.Exceptions;            use Ada.Exceptions;

package body Docgen.Work_On_Source is

   Me : constant Debug_Handle := Create ("Docgen-work_on_source");

   package TSFL renames Type_Source_File_List;
   package TEL renames Type_Entity_List;

   procedure Process_Source_Spec
     (B                  : Backend_Handle;
      Kernel             : access Kernel_Handle_Record'Class;
      Doc_File           : File_Type;
      Source_File_List   : in out Type_Source_File_List.List;
      Source_Filename    : VFS.Virtual_File;
      Package_Name       : String;
      Package_Info       : Entity_Information;
      Entity_List        : in out Type_Entity_List.List;
      List_Ref_In_File   : in out List_Reference_In_File.List;
      Tagged_Types_List  : in out Type_List_Tagged_Element.List;
      Private_Tagged_Types_List : in out Type_List_Tagged_Element.List;
      Process_Body_File  : Boolean;
      LI_Unit            : LI_File_Ptr;
      Options            : All_Options;
      Converter          : Docgen.Doc_Subprogram_Type;
      Doc_Directory      : String;
      Doc_Suffix         : String;
      Level              : in out Natural;
      File_Text          : GNAT.OS_Lib.String_Access;
      Tree_Package       : Scope_Tree;
      Parsed_List        : in out Construct_List);
   --  It's in charge of processing packages, types, variables, subprograms,
   --  exceptions, entries. It can be called recursively for inner package.
   --  Source_File_List : list of all files that must be processed by docgen.
   --  Source_Filename  : current file processed.
   --  Package_Name     : name of the current package. The first time, it's
   --  the name of the main package which is defined in the file. Then, in the
   --  case of inner package, this subprogram is called recursively and this
   --  field is the name of the inner package.
   --  Package_Info     : Entity_Information of the current package.
   --  Tree_Package     : scope tree of entities in the current package (it
   --  contains only declarations)
   --  File_Text        : source code of the current package.
   --  Entity_List      : list of entities in the current file.
   --  List_Ref_In_File : list of references in the current file.
   --  Tagged_Types_List: list of public tagged types.
   --  Private_Tagged_Types_List: list of private tagged types.
   --  Options          : options set by the preferences.
   --  Process_Body_File: indicate if bofy files must be processed.
   --  ???  This last parameter is redondant because Options indicate it.
   --  Converter        : used to indicate the subprogram used in order to
   --  start making the output (currently, it's Launch_Doc_Create).
   --  Level            : the level of the current package. By default, the
   --  level of the package file is 1, then this level is increased by 1 at
   --  each inner package

   procedure Process_One_Body_File
     (B                : Backend_Handle;
      Kernel           : access Kernel_Handle_Record'Class;
      Doc_File         : File_Type;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Source_File      : Virtual_File;
      File_Text        : GNAT.OS_Lib.String_Access;
      LI_Unit          : LI_File_Ptr;
      Source_File_List : in out Type_Source_File_List.List;
      Options          : All_Options;
      Converter        : Docgen.Doc_Subprogram_Type;
      Doc_Directory    : String;
      Doc_Suffix       : String;
      Level            : in out Natural);
   --  Will pass the information about the body file to the output
   --  subprogram. This is the only subprogram working on the contents
   --  of the body source files.

   procedure Process_Open_File
     (B                : Backend_Handle;
      Kernel           : access Kernel_Handle_Record'Class;
      Doc_File         : File_Type;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Package_File     : Virtual_File;
      Next_Package     : GNAT.OS_Lib.String_Access;
      Prev_Package     : GNAT.OS_Lib.String_Access;
      Package_Name     : String;
      Source_File_List : in out Type_Source_File_List.List;
      Options          : All_Options;
      Converter        : Docgen.Doc_Subprogram_Type;
      Doc_Directory    : String;
      Doc_Suffix       : String;
      Level            : in out Natural);
   --  Is always the first subprogram to be called, as it creates the
   --  very beginning of the documentation by calling the output
   --  subprogram

   procedure Process_Close_File
     (B                : Backend_Handle;
      Kernel           : access Kernel_Handle_Record'Class;
      Doc_File         : File_Type;
      List_Ref_In_File : in out List_Reference_In_File.List;
      File_Name        : Virtual_File;
      Options          : All_Options;
      Converter        : Docgen.Doc_Subprogram_Type;
      Doc_Directory    : String;
      Doc_Suffix       : String;
      Level            : in out Natural);
   --  Is always the last subprogram to be called, as it creates the
   --  very end of the documentation by calling the output subprogram

   procedure Process_Package_Description
     (B                : Backend_Handle;
      Kernel           : access Kernel_Handle_Record'Class;
      Doc_File         : File_Type;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Package_Name     : String;
      Text             : String;
      Options          : All_Options;
      Converter        : Doc_Subprogram_Type;
      Doc_Directory    : String;
      Doc_Suffix       : String;
      Level            : in out Natural);
   --  Extracts all the comment lines of the source file which are at the
   --  beginning of it. Empty lines are ignored, the procedure stops when
   --  first command is found. This information will be passed to the
   --  output subprogram

   procedure Process_With_Clause
     (B                : Backend_Handle;
      Kernel           : access Kernel_Handle_Record'Class;
      Doc_File         : File_Type;
      Parsed_List      : in out Construct_List;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Source_Filename  : VFS.Virtual_File;
      Package_Name     : String;
      File_Text        : GNAT.OS_Lib.String_Access;
      LI_Unit          : LI_File_Ptr;
      Source_File_List : in out Type_Source_File_List.List;
      Options          : All_Options;
      Converter        : Doc_Subprogram_Type;
      Doc_Directory    : String;
      Doc_Suffix       : String;
      Level            : in out Natural);
   --  Will process the lines at the beginning of the source file
   --  starting with "with" and pass them to the output subprogram

   procedure Process_Description
     (B                 : Backend_Handle;
      Kernel            : access Kernel_Handle_Record'Class;
      Doc_File          : File_Type;
      List_Ref_In_File  : in out List_Reference_In_File.List;
      Options           : All_Options;
      Converter         : Doc_Subprogram_Type;
      Doc_Directory     : String;
      Doc_Suffix        : String;
      Level             : in out Natural;
      Comment           : String);
   --  Processes comments after the source code (or before: it depends on
   --  preferences)

   procedure Process_Packages
     (B                         : Backend_Handle;
      Kernel                    : access Kernel_Handle_Record'Class;
      Doc_File                  : File_Type;
      Parsed_List               : in out Construct_List;
      Entity_List               : in out Type_Entity_List.List;
      List_Ref_In_File          : in out List_Reference_In_File.List;
      Tagged_Types_List         : in out Type_List_Tagged_Element.List;
      Private_Tagged_Types_List : in out Type_List_Tagged_Element.List;
      Source_Filename           : VFS.Virtual_File;
      Package_Name              : String;
      Package_Information       : Entity_Information;
      File_Text                 : GNAT.OS_Lib.String_Access;
      LI_Unit                   : LI_File_Ptr;
      Source_File_List          : in out Type_Source_File_List.List;
      Options                   : All_Options;
      Converter                 : Doc_Subprogram_Type;
      Doc_Directory             : String;
      Doc_Suffix                : String;
      Private_Entity            : Boolean;
      Display_Private           : in out Boolean;
      Level                     : in out Natural;
      Tree_Package              : Scope_Tree;
      Process_Body_File         : Boolean);
   --  Will process renamed and instantiated packages and pass
   --  them to the output subprogram.
   --  Display_Private : indicate if we print the "Private" header.
   --  Private_Entity  : indicate if it must process private/public entities
   --  contained in Entity_List.

   procedure Process_Package_Open
     (B                 : Backend_Handle;
      Kernel            : access Kernel_Handle_Record'Class;
      Doc_File          : File_Type;
      List_Ref_In_File  : in out List_Reference_In_File.List;
      Converter         : Doc_Subprogram_Type;
      Doc_Directory     : String;
      Doc_Suffix        : String;
      Level             : in out Natural;
      Data_Package      : in out Doc_Info);
   --  For inner packages: generates the output "package ... is"

   procedure Process_Package_Close
     (B                 : Backend_Handle;
      Kernel            : access Kernel_Handle_Record'Class;
      Doc_File          : File_Type;
      List_Ref_In_File  : in out List_Reference_In_File.List;
      Converter         : Doc_Subprogram_Type;
      Doc_Directory     : String;
      Doc_Suffix        : String;
      Level             : in out Natural;
      Data_Package      : in out Doc_Info);
   --  For inner packages: generates the output "end ...;"

   procedure Process_Vars
     (B                : Backend_Handle;
      Kernel           : access Kernel_Handle_Record'Class;
      Doc_File         : File_Type;
      Parsed_List      : in out Construct_List;
      Entity_List      : in out Type_Entity_List.List;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Source_Filename  : VFS.Virtual_File;
      Package_Name     : String;
      Package_Info     : Entity_Information;
      File_Text        : GNAT.OS_Lib.String_Access;
      LI_Unit          : LI_File_Ptr;
      Source_File_List : in out Type_Source_File_List.List;
      Options          : All_Options;
      Converter        : Doc_Subprogram_Type;
      Doc_Directory    : String;
      Doc_Suffix       : String;
      Private_Entity   : Boolean;
      Display_Private  : in out Boolean;
      Level            : in out Natural;
      Tree_Package     : Scope_Tree);
   --  Called by Process_Source to work on the constants
   --  and named numbers and pass each of them to the output subprogram
   --  Display_Private : indicate if we print the "Private" header.
   --  Private_Entity  : indicate if it must process private/public entities
   --  contained in Entity_List.

   procedure Process_Exceptions
     (B                : Backend_Handle;
      Kernel           : access Kernel_Handle_Record'Class;
      Doc_File         : File_Type;
      Parsed_List      : in out Construct_List;
      Entity_List      : in out Type_Entity_List.List;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Source_Filename  : VFS.Virtual_File;
      Package_Name     : String;
      Package_Info     : Entity_Information;
      File_Text        : GNAT.OS_Lib.String_Access;
      LI_Unit          : LI_File_Ptr;
      Source_File_List : in out Type_Source_File_List.List;
      Options          : All_Options;
      Converter        : Doc_Subprogram_Type;
      Doc_Directory    : String;
      Doc_Suffix       : String;
      Private_Entity   : Boolean;
      Display_Private  : in out Boolean;
      Level            : in out Natural;
      Tree_Package     : Scope_Tree);
   --  Called by Process_Source to work on the exceptions and
   --  pass each of them to the output subprogram
   --  Display_Private : indicate if we print the "Private" header.
   --  Private_Entity  : indicate if it must process private/public entities
   --  contained in Entity_List.

   procedure Process_Entries
     (B                 : Backend_Handle;
      Kernel            : access Kernel_Handle_Record'Class;
      Doc_File          : File_Type;
      Parsed_List       : in out Construct_List;
      Entity_List       : in out Type_Entity_List.List;
      List_Ref_In_File  : in out List_Reference_In_File.List;
      Source_Filename   : VFS.Virtual_File;
      Process_Body_File : Boolean;
      Package_Name      : String;
      Package_Info      : Entity_Information;
      File_Text         : GNAT.OS_Lib.String_Access;
      LI_Unit           : LI_File_Ptr;
      Source_File_List  : in out Type_Source_File_List.List;
      Options           : All_Options;
      Converter         : Doc_Subprogram_Type;
      Doc_Directory     : String;
      Doc_Suffix        : String;
      Private_Entity    : Boolean;
      Display_Private   : in out Boolean;
      Level             : in out Natural;
      Tree_Package      : Scope_Tree);
   --  Called by Process_Source to work on the entires and entry
   --  families and pass each of them to the output subprogram
   --  Display_Private : indicate if we print the "Private" header.
   --  Private_Entity  : indicate if it must process private/public entities
   --  contained in Entity_List.

   procedure Process_References
     (B                 : Backend_Handle;
      Kernel            : access Kernel_Handle_Record'Class;
      Doc_File          : File_Type;
      Source_File_List  : in out Type_Source_File_List.List;
      List_Ref_In_File  : in out List_Reference_In_File.List;
      Options           : All_Options;
      Converter         : Doc_Subprogram_Type;
      Doc_Directory     : String;
      Doc_Suffix        : String;
      Level             : in out Natural;
      Entity_Info       : Entity_List_Information);
   --  For one subprogram: processes the output of the callgraph when this
   --  option is choosen.

   procedure Process_Subprograms
     (B                  : Backend_Handle;
      Kernel             : access Kernel_Handle_Record'Class;
      Doc_File           : File_Type;
      Parsed_List        : in out Construct_List;
      Entity_List        : in out Type_Entity_List.List;
      List_Ref_In_File   : in out List_Reference_In_File.List;
      Source_Filename    : VFS.Virtual_File;
      Process_Body_File  : Boolean;
      Package_Name       : String;
      Package_Info       : Entity_Information;
      File_Text          : GNAT.OS_Lib.String_Access;
      LI_Unit            : LI_File_Ptr;
      Source_File_List   : in out Type_Source_File_List.List;
      Options            : All_Options;
      Converter          : Doc_Subprogram_Type;
      Doc_Directory      : String;
      Doc_Suffix         : String;
      Private_Entity     : Boolean;
      Display_Private    : in out Boolean;
      Level              : in out Natural;
      Tree_Package       : Scope_Tree);
   --  Called by Process_Source to work on the subprograms and
   --  pass each of them to the output subprogram
   --  Private_Entity indicates if it must process private or public entites
   --  Display_Private is used to indicate if it's necessary to print the
   --  "Private" header

   procedure Process_One_Family
     (B                : Backend_Handle;
      Kernel           : access Kernel_Handle_Record'Class;
      Doc_File         : File_Type;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Family           : Type_List_Tagged_Element.List;
      Source_File_List : in out Type_Source_File_List.List;
      Entity           : Entity_Information;
      Level            : in out Natural;
      Options          : All_Options;
      Converter        : Doc_Subprogram_Type;
      Doc_Directory    : String;
      Doc_Suffix       : String);
   --  For one tagged type: indicates its parents and children.

   procedure Process_Types
     (B                         : Backend_Handle;
      Kernel                    : access Kernel_Handle_Record'Class;
      Doc_File                  : File_Type;
      Parsed_List               : in out Construct_List;
      Entity_List               : in out Type_Entity_List.List;
      List_Ref_In_File          : in out List_Reference_In_File.List;
      Tagged_Types_List         : in out Type_List_Tagged_Element.List;
      Private_Tagged_Types_List : in out Type_List_Tagged_Element.List;
      Source_Filename           : VFS.Virtual_File;
      Package_Name              : String;
      Package_Info              : Entity_Information;
      File_Text                 : GNAT.OS_Lib.String_Access;
      LI_Unit                   : LI_File_Ptr;
      Source_File_List          : in out Type_Source_File_List.List;
      Options                   : All_Options;
      Converter                 : Doc_Subprogram_Type;
      Doc_Directory             : String;
      Doc_Suffix                : String;
      Private_Entity            : Boolean;
      Display_Private           : in out Boolean;
      Level                     : in out Natural;
      Tree_Package              : Scope_Tree);
   --  Called by Process_Source to work on the types and
   --  pass each of them to the output subprogram
   --  Display_Private : indicate if we print the "Private" header.
   --  Private_Entity  : indicate if it must process private/public entities
   --  contained in Entity_List.

   procedure Process_Header
     (B                 : Backend_Handle;
      Kernel            : access Kernel_Handle_Record'Class;
      Doc_File          : File_Type;
      Entity_List       : in out Type_Entity_List.List;
      List_Ref_In_File  : in out List_Reference_In_File.List;
      Source_Filename   : VFS.Virtual_File;
      Package_Name      : String;
      Package_File      : VFS.Virtual_File;
      Process_Body_File : Boolean;
      Options           : All_Options;
      Converter         : Doc_Subprogram_Type;
      Doc_Directory     : String;
      Doc_Suffix        : String;
      Level             : in out Natural);
   --  Will call the output subprogram to create the header of
   --  the package. This is NOT the same as Process_Open_File,
   --  if TexInfo doc is created, the file is opened only once,
   --  but the Header has to be set in front of each package.

   procedure Process_Header_Private
     (B                 : Backend_Handle;
      Kernel            : access Kernel_Handle_Record'Class;
      Doc_File          : File_Type;
      List_Ref_In_File  : in out List_Reference_In_File.List;
      Options           : All_Options;
      Converter         : Doc_Subprogram_Type;
      Doc_Directory     : String;
      Doc_Suffix        : String;
      Level             : in out Natural);
   --  Adds title "Private: " when private entities are required.

   procedure Process_Header_Packages
     (B                 : Backend_Handle;
      Kernel            : access Kernel_Handle_Record'Class;
      Doc_File          : File_Type;
      List_Ref_In_File  : in out List_Reference_In_File.List;
      Options           : All_Options;
      Converter         : Doc_Subprogram_Type;
      Doc_Directory     : String;
      Doc_Suffix        : String;
      Package_Name      : String;
      Level             : in out Natural);
   --  Adds title "Packages".

   procedure Process_Header_Vars
     (B                 : Backend_Handle;
      Kernel            : access Kernel_Handle_Record'Class;
      Doc_File          : File_Type;
      List_Ref_In_File  : in out List_Reference_In_File.List;
      Options           : All_Options;
      Converter         : Doc_Subprogram_Type;
      Doc_Directory     : String;
      Doc_Suffix        : String;
      Package_Name      : String;
      Level             : in out Natural);
   --  Adds title "Constants and Named Numbers"

   procedure Process_Header_Types
     (B                 : Backend_Handle;
      Kernel            : access Kernel_Handle_Record'Class;
      Doc_File          : File_Type;
      List_Ref_In_File  : in out List_Reference_In_File.List;
      Options           : All_Options;
      Converter         : Doc_Subprogram_Type;
      Doc_Directory     : String;
      Doc_Suffix        : String;
      Package_Name      : String;
      Level             : in out Natural);
   --  Adds title "Types"

   procedure Process_Header_Entries
     (B                 : Backend_Handle;
      Kernel            : access Kernel_Handle_Record'Class;
      Doc_File          : File_Type;
      List_Ref_In_File  : in out List_Reference_In_File.List;
      Options           : All_Options;
      Converter         : Doc_Subprogram_Type;
      Doc_Directory     : String;
      Doc_Suffix        : String;
      Package_Name      : String;
      Level             : in out Natural);
   --  Adds title "Entries"

   procedure Process_Header_Subprograms
     (B                 : Backend_Handle;
      Kernel            : access Kernel_Handle_Record'Class;
      Doc_File          : File_Type;
      List_Ref_In_File  : in out List_Reference_In_File.List;
      Options           : All_Options;
      Converter         : Doc_Subprogram_Type;
      Doc_Directory     : String;
      Doc_Suffix        : String;
      Package_Name      : String;
      Level             : in out Natural);
   --  Adds title "Subprograms"

   procedure Process_Header_Exceptions
     (B                 : Backend_Handle;
      Kernel            : access Kernel_Handle_Record'Class;
      Doc_File          : File_Type;
      List_Ref_In_File  : in out List_Reference_In_File.List;
      Options           : All_Options;
      Converter         : Doc_Subprogram_Type;
      Doc_Directory     : String;
      Doc_Suffix        : String;
      Package_Name      : String;
      Level             : in out Natural);
   --  Adds title "Exceptions"

   procedure Process_Footer
     (B                : Backend_Handle;
      Kernel           : access Kernel_Handle_Record'Class;
      Doc_File         : File_Type;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Package_File     : Virtual_File;
      Options          : All_Options;
      Converter        : Doc_Subprogram_Type;
      Doc_Directory    : String;
      Doc_Suffix       : String;
      Level            : in out Natural);
   --  Will call the output subprogram to create the footer of
   --  the package. This is NOT the same as Process_Close_File,
   --  if TexInfo doc is created, the file is closed only once,
   --  but the Footer has to be set behind each package.

   function Extract_Comment
     (File_Text           : String;
      Line                : Natural;
      Header_Lines        : Natural;
      Package_Description : Boolean;
      Options             : All_Options) return GNAT.OS_Lib.String_Access;
   --  Get the doc comments from the source file. The File_Text gives the
   --  String where to search, Line is the line number of the entity and
   --  Header_Lines says how many lines takes the header of the entity.
   --  Within Options it can be chosen, if the comments are placed
   --  below or above the entity header.
   --  If Package_Description is set, empty lines between the comment lines
   --  will be ignored, the direction of the processing is always the same
   --  and it stops when the first command is found.

   function Remove_Space (Text : String)
                          return GNAT.OS_Lib.String_Access;
   --  Removes spaces which start a string.

   function Entity_Defined_In_Package
     (Entity_Info       : Entity_Information;
      Package_Container : Entity_Information;
      Tree_Package      : Scope_Tree) return Boolean;
   --  Determines if an entity is defined in a package.

   function Package_Contain_Entity
     (Package_Entity : Entity_Information;
      Entity_List    : Type_Entity_List.List;
      Tree_Package   : Scope_Tree) return Boolean;
   --  Determines if there's entities defined in a the package

   function Line_Is_Comment
     (Line : String) return Boolean;
   --  Return true, if the first chars of the line are "--"

   function Line_Is_Empty
     (Line : String) return Boolean;
   --  Return true, if there is no text in this line

   function Is_Ignorable_Comment
     (Comment_Line : String) return Boolean;
   --  Return true, if the comment line starts with a "--!"
   --  It must be sure, that Comment_List is a comment line!

   function Kill_Prefix
     (Comment_Line : String) return String;
   --  Return the comment line without the "--" in front

   function Get_Whole_Header
     (File_Text   : String;
      Parsed_List : Construct_List;
      Entity_Name : String;
      Entity_Line : Natural) return GNAT.OS_Lib.String_Access;
   --  Return the Header of the entity. If no header was found,
   --  null will be returned.

   function Remove_Indent
     (Text  : GNAT.OS_Lib.String_Access;
      Space : Natural) return GNAT.OS_Lib.String_Access;
   --  The header returned by Get_Whole_Header contains indent spaces (except
   --  those of the first line). This function removes those spaces.

   function Get_Line_From_String
     (Text    : String;
      Line_Nr : Natural) return String;
   --  Return the wished Line from the String

   --------------------
   -- Process_Source --
   --------------------

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
      Tagged_Types_List : in out Type_List_Tagged_Element.List;
      Private_Tagged_Types_List : in out Type_List_Tagged_Element.List;
      Process_Body_File : Boolean;
      LI_Unit           : LI_File_Ptr;
      Options           : All_Options;
      Converter         : Docgen.Doc_Subprogram_Type;
      Doc_Directory     : String;
      Doc_Suffix        : String;
      Level             : in out Natural;
      All_Scope_Tree    : in out Tree_Htable.String_Hash_Table.HTable)
   is
      use TEL;
      use type Basic_Types.String_Access;
      File_Text          : GNAT.OS_Lib.String_Access;
      Tree_Package       : Scope_Tree;
      Entity_Node        : Type_Entity_List.List_Node;
      Found_Main_Package : Boolean;
      Parsed_List        : Construct_List;
   begin
      File_Text := Read_File (Source_Filename);
      --  All the file is stored in a string

      if File_Text = null then
         --  This is a non existing file
         return;
      end if;

      Process_Open_File
        (B,
         Kernel,
         Doc_File,
         List_Ref_In_File,
         Source_Filename,
         Next_Package,
         Prev_Package,
         Package_Name,
         Source_File_List,
         Options,
         Converter,
         Doc_Directory,
         Doc_Suffix,
         Level);
      Process_Header
        (B,
         Kernel,
         Doc_File,
         Entity_List,
         List_Ref_In_File,
         Source_Filename,
         Package_Name,
         Source_Filename,
         Process_Body_File,
         Options,
         Converter,
         Doc_Directory,
         Doc_Suffix,
         Level);

      --  Different ways of process for spec and body files
      if Is_Spec_File (Kernel, Source_Filename) then
         if not TEL.Is_Empty (Entity_List) then

            --  Try to get the scope tree associated with the ali file
            Tree_Package
              := Tree_Htable.String_Hash_Table.Get
                (All_Scope_Tree,
                 Base_Name (Get_LI_Filename (LI_Unit)));
            --  First time, we met the ali file, we build the scope tree
            --  and store it in the hash table during all the process of
            --  docgen
            if Tree_Package = Null_Scope_Tree then
               Tree_Package :=
                 Create_Tree (LI_Unit);
                  Tree_Htable.String_Hash_Table.Set
                 (All_Scope_Tree,
                  Base_Name (Get_LI_Filename (LI_Unit)),
                  Tree_Package);
            end if;

            --  Build of the scope tree.
            --  True means that only declarations are inserted into the tree
            --  (references don't needed).
            --  Tree_Package := Create_Tree (LI_Unit, True);

            --  Parse the source file and create the Parsed_List
            Parse_Constructs
              (Get_Language_From_File
                 (Get_Language_Handler (Kernel),
                  Source_Filename),
               File_Text.all,
               Parsed_List);

            --  Research of the entity of the main package
            Found_Main_Package := False;
            Entity_Node := TEL.First (Entity_List);
            while Entity_Node /= TEL.Null_Node loop
               if
               --  Check if the entity is a package
                 TEL.Data (Entity_Node).Kind = Package_Entity
                 and then
               --  the main package itself
                 To_Lower (TEL.Data (Entity_Node).Name.all) =
                 To_Lower (Package_Name)
               then
                  Found_Main_Package := True;
                  exit;
               end if;
               Entity_Node := TEL.Next (Entity_Node);
            end loop;

            if Found_Main_Package then
               Process_Package_Description
                 (B, Kernel, Doc_File,
                  List_Ref_In_File,
                  Package_Name, File_Text.all, Options,
                  Converter, Doc_Directory, Doc_Suffix,
                  Level);
               Process_With_Clause
                 (B,
                  Kernel,
                  Doc_File,
                  Parsed_List,
                  List_Ref_In_File,
                  Source_Filename,
                  Package_Name,
                  File_Text,
                  LI_Unit,
                  Source_File_List,
                  Options,
                  Converter, Doc_Directory, Doc_Suffix,
                  Level);

               --  Process types, variables, subprograms, entries, exceptions,
               --  packages (recursive calls for inner packages)
               Process_Source_Spec
                 (B,
                  Kernel,
                  Doc_File,
                  Source_File_List,
                  Source_Filename,
                  Package_Name,
                  TEL.Data (Entity_Node).Entity,
                  Entity_List,
                  List_Ref_In_File,
                  Tagged_Types_List,
                  Private_Tagged_Types_List,
                  Process_Body_File,
                  LI_Unit,
                  Options,
                  Converter,
                  Doc_Directory,
                  Doc_Suffix,
                  Level,
                  File_Text,
                  Tree_Package,
                  Parsed_List);
            end if;
            Free (Parsed_List);
            --  Free (Tree_Package);
         end if;
      else
         Process_One_Body_File
           (B,
            Kernel,
            Doc_File,
            List_Ref_In_File,
            Source_Filename,
            File_Text,
            LI_Unit,
            Source_File_List,
            Options,
            Converter, Doc_Directory, Doc_Suffix,
            Level);
      end if;

      Process_Footer
        (B, Kernel,
         Doc_File,
         List_Ref_In_File,
         Source_Filename,
         Options,
         Converter,
         Doc_Directory,
         Doc_Suffix,
         Level);
      Process_Close_File
        (B,
         Kernel,
         Doc_File,
         List_Ref_In_File,
         Source_Filename,
         Options,
         Converter,
         Doc_Directory,
         Doc_Suffix,
         Level);
      Free (File_Text);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Process_Source;

   -------------------------
   -- Process_Source_Spec --
   -------------------------

   procedure Process_Source_Spec
     (B                  : Backend_Handle;
      Kernel             : access Kernel_Handle_Record'Class;
      Doc_File           : File_Type;
      Source_File_List   : in out Type_Source_File_List.List;
      Source_Filename    : VFS.Virtual_File;
      Package_Name       : String;
      Package_Info       : Entity_Information;
      Entity_List        : in out Type_Entity_List.List;
      List_Ref_In_File   : in out List_Reference_In_File.List;
      Tagged_Types_List  : in out Type_List_Tagged_Element.List;
      Private_Tagged_Types_List : in out Type_List_Tagged_Element.List;
      Process_Body_File  : Boolean;
      LI_Unit            : LI_File_Ptr;
      Options            : All_Options;
      Converter          : Docgen.Doc_Subprogram_Type;
      Doc_Directory      : String;
      Doc_Suffix         : String;
      Level              : in out Natural;
      File_Text          : GNAT.OS_Lib.String_Access;
      Tree_Package       : Scope_Tree;
      Parsed_List        : in out Construct_List)
   is
      use Type_Entity_List;
      Display_Private  : Boolean := False;
   begin
      --  The 6 following calls have the value "False" for the
      --  parameter Private_Entity. So, only public entities are
      --  processed
      Process_Packages
        (B,
         Kernel,
         Doc_File,
         Parsed_List,
         Entity_List,
         List_Ref_In_File,
         Tagged_Types_List,
         Private_Tagged_Types_List,
         Source_Filename,
         Package_Name,
         Package_Info,
         File_Text,
         LI_Unit,
         Source_File_List,
         Options,
         Converter, Doc_Directory, Doc_Suffix, False, Display_Private,
         Level,
         Tree_Package,
         Process_Body_File);
      Process_Vars
        (B,
         Kernel,
         Doc_File,
         Parsed_List,
         Entity_List,
         List_Ref_In_File,
         Source_Filename,
         Package_Name,
         Package_Info,
         File_Text,
         LI_Unit,
         Source_File_List,
         Options,
         Converter, Doc_Directory, Doc_Suffix, False, Display_Private,
         Level,
         Tree_Package);
      Process_Exceptions
        (B,
         Kernel,
         Doc_File,
         Parsed_List,
         Entity_List,
         List_Ref_In_File,
         Source_Filename,
         Package_Name,
         Package_Info,
         File_Text,
         LI_Unit,
         Source_File_List,
         Options,
         Converter, Doc_Directory, Doc_Suffix, False, Display_Private,
         Level,
         Tree_Package);
      Process_Types
        (B,
         Kernel,
         Doc_File,
         Parsed_List,
         Entity_List,
         List_Ref_In_File,
         Tagged_Types_List,
         Private_Tagged_Types_List,
         Source_Filename,
         Package_Name,
         Package_Info,
         File_Text,
         LI_Unit,
         Source_File_List,
         Options,
         Converter, Doc_Directory, Doc_Suffix, False, Display_Private,
         Level,
         Tree_Package);
      Process_Entries
        (B,
         Kernel,
         Doc_File,
         Parsed_List,
         Entity_List,
         List_Ref_In_File,
         Source_Filename,
         Process_Body_File,
         Package_Name,
         Package_Info,
         File_Text,
         LI_Unit,
         Source_File_List,
         Options,
         Converter, Doc_Directory, Doc_Suffix, False, Display_Private,
         Level,
         Tree_Package);
      Process_Subprograms
        (B,
         Kernel,
         Doc_File,
         Parsed_List,
         Entity_List,
         List_Ref_In_File,
         Source_Filename,
         Process_Body_File,
         Package_Name,
         Package_Info,
         File_Text,
         LI_Unit,
         Source_File_List,
         Options,
         Converter, Doc_Directory, Doc_Suffix, False, Display_Private,
         Level,
         Tree_Package);

      if Options.Show_Private then
         --  Private entities are displayed. Hence, the value "True" is
         --  given to the parameter Private_Entity
         Process_Packages
           (B,
            Kernel,
            Doc_File,
            Parsed_List,
            Entity_List,
            List_Ref_In_File,
            Tagged_Types_List,
            Private_Tagged_Types_List,
            Source_Filename,
            Package_Name,
            Package_Info,
            File_Text,
            LI_Unit,
            Source_File_List,
            Options,
            Converter, Doc_Directory, Doc_Suffix, True, Display_Private,
            Level,
            Tree_Package,
            Process_Body_File);
         Process_Vars
           (B,
            Kernel,
            Doc_File,
            Parsed_List,
            Entity_List,
            List_Ref_In_File,
            Source_Filename,
            Package_Name,
            Package_Info,
            File_Text,
            LI_Unit,
            Source_File_List,
            Options,
            Converter, Doc_Directory, Doc_Suffix, True, Display_Private,
            Level,
            Tree_Package);
         Process_Exceptions
           (B,
            Kernel,
            Doc_File,
            Parsed_List,
            Entity_List,
            List_Ref_In_File,
            Source_Filename,
            Package_Name,
            Package_Info,
            File_Text,
            LI_Unit,
            Source_File_List,
            Options,
            Converter, Doc_Directory, Doc_Suffix, True, Display_Private,
            Level,
            Tree_Package);
         Process_Types
           (B,
            Kernel,
            Doc_File,
            Parsed_List,
            Entity_List,
            List_Ref_In_File,
            Tagged_Types_List,
            Private_Tagged_Types_List,
            Source_Filename,
            Package_Name,
            Package_Info,
            File_Text,
            LI_Unit,
            Source_File_List,
            Options,
            Converter, Doc_Directory, Doc_Suffix, True, Display_Private,
            Level,
            Tree_Package);
         Process_Entries
           (B,
            Kernel,
            Doc_File,
            Parsed_List,
            Entity_List,
            List_Ref_In_File,
            Source_Filename,
            Process_Body_File,
            Package_Name,
            Package_Info,
            File_Text,
            LI_Unit,
            Source_File_List,
            Options,
            Converter, Doc_Directory, Doc_Suffix, True, Display_Private,
            Level,
            Tree_Package);
         Process_Subprograms
           (B,
            Kernel,
            Doc_File,
            Parsed_List,
            Entity_List,
            List_Ref_In_File,
            Source_Filename,
            Process_Body_File,
            Package_Name,
            Package_Info,
            File_Text,
            LI_Unit,
            Source_File_List,
            Options,
            Converter, Doc_Directory, Doc_Suffix, True, Display_Private,
            Level,
            Tree_Package);
      end if;
   end Process_Source_Spec;

   -----------------------
   -- Process_Open_File --
   -----------------------

   procedure Process_Open_File
     (B                : Backend_Handle;
      Kernel           : access Kernel_Handle_Record'Class;
      Doc_File         : File_Type;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Package_File     : Virtual_File;
      Next_Package     : GNAT.OS_Lib.String_Access;
      Prev_Package     : GNAT.OS_Lib.String_Access;
      Package_Name     : String;
      Source_File_List : in out Type_Source_File_List.List;
      Options          : All_Options;
      Converter        : Docgen.Doc_Subprogram_Type;
      Doc_Directory    : String;
      Doc_Suffix       : String;
      Level            : in out Natural)
   is
      Data_Open : Doc_Info :=
        (Open_Info,
         Doc_Info_Options  => Options,
         Doc_File_List     => Source_File_List,
         Open_Title        => new String'(Package_Name),
         Open_File         => Package_File,
         Open_Package_Next => Next_Package,
         Open_Package_Prev => Prev_Package,
         Doc_LI_Unit       => No_LI_File);
   begin
      Converter (B,
                 Kernel,
                 Doc_File,
                 List_Ref_In_File,
                 Data_Open,
                 Doc_Directory,
                 Doc_Suffix,
                 Level);
      Free (Data_Open.Open_Title);
   end Process_Open_File;

   ------------------------
   -- Process_Close_File --
   ------------------------

   procedure Process_Close_File
     (B                : Backend_Handle;
      Kernel           : access Kernel_Handle_Record'Class;
      Doc_File         : File_Type;
      List_Ref_In_File : in out List_Reference_In_File.List;
      File_Name        : Virtual_File;
      Options          : All_Options;
      Converter        : Docgen.Doc_Subprogram_Type;
      Doc_Directory    : String;
      Doc_Suffix       : String;
      Level            : in out Natural)
   is
      Data_Close : Doc_Info :=
        (Close_Info,
         Doc_Info_Options => Options,
         Doc_LI_Unit      => No_LI_File,
         Doc_File_List    => TSFL.Null_List,
         Close_File_Name  => File_Name);

   begin
      Converter (B, Kernel, Doc_File,
                 List_Ref_In_File,
                 Data_Close, Doc_Directory, Doc_Suffix,
                 Level);
   end Process_Close_File;

   ---------------------------
   -- Process_One_Body_File --
   ---------------------------

   procedure Process_One_Body_File
     (B                : Backend_Handle;
      Kernel           : access Kernel_Handle_Record'Class;
      Doc_File         : File_Type;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Source_File      : Virtual_File;
      File_Text        : GNAT.OS_Lib.String_Access;
      LI_Unit          : LI_File_Ptr;
      Source_File_List : in out Type_Source_File_List.List;
      Options          : All_Options;
      Converter        : Docgen.Doc_Subprogram_Type;
      Doc_Directory    : String;
      Doc_Suffix       : String;
      Level            : in out Natural)
   is
      Data_Line : Doc_Info :=
        (Body_Line_Info,
         Doc_Info_Options => Options,
         Doc_LI_Unit      => LI_Unit,
         Body_Text        => File_Text,
         Body_File        => Source_File,
         Doc_File_List    => Source_File_List);
   begin
      Converter (B, Kernel, Doc_File,
                 List_Ref_In_File,
                 Data_Line, Doc_Directory, Doc_Suffix,
                 Level);
   end Process_One_Body_File;

   ------------------------
   -- Process_Unit_Index --
   ------------------------

   procedure Process_Unit_Index
     (B                : Backend_Handle;
      Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
      Source_File_List : Docgen.Type_Source_File_List.List;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Options          : Docgen.All_Options;
      Converter        : Docgen.Doc_Subprogram_Type;
      Doc_Directory    : String;
      Doc_Suffix       : String;
      Level            : in out Natural)
   is
      use TSFL;
      Source_Filename  : Virtual_File;
      Package_Name     : String_Access;
      Source_File_Node : Type_Source_File_List.List_Node;
      Index_File       : File_Type;
      Data_Package     : Doc_Info (Info_Type => Unit_Index_Info);
      Data_Item        : Doc_Info (Info_Type => Index_Item_Info);
      Data_End         : Doc_Info (Info_Type => End_Of_Index_Info);

      package TSFL renames Type_Source_File_List;

      One_Ready        : Integer;
      --  how many files already examined BEFORE the loop

      Doc_File_Name    : constant String := "index_unit";

   begin
      Create
        (Index_File, Out_File, Doc_Directory & Doc_File_Name & Doc_Suffix);

      if not TSFL.Is_Empty (Source_File_List) then
         One_Ready := 0;
         Source_File_Node := TSFL.First (Source_File_List);
         Source_Filename  := TSFL.Data (Source_File_Node).File_Name;

         --  if first body file, take the next one, which must be spec file
         if not Is_Spec_File (Kernel, Source_Filename) then
            Source_File_Node := TSFL.Next (Source_File_Node);
            One_Ready := 1;
         end if;

         Data_Package := Doc_Info'
           (Unit_Index_Info,
            Doc_Info_Options     => Options,
            Doc_LI_Unit          => No_LI_File,
            Doc_File_List        => TSFL.Null_List,
            Unit_Project_Name    => Get_Root_Project (Get_Registry (Kernel)),
            Unit_Index_File_Name => new String'(Doc_File_Name),
            Unit_File_List       => Source_File_List);

         --  Create the upper part of the unit index
         Converter
           (B, Kernel, Index_File,
            List_Ref_In_File,
            Data_Package, Doc_Directory, Doc_Suffix,
            Level);
         Free (Data_Package.Unit_Index_File_Name);

         for J in 1 .. Type_Source_File_List.Length (Source_File_List) -
           One_Ready
         loop
            Source_Filename := TSFL.Data (Source_File_Node).File_Name;

            --  Add unit, but only if from a spec file
            if Is_Spec_File (Kernel, Source_Filename) then
               Package_Name := TSFL.Data (Source_File_Node).Package_Name;
               Data_Item := Doc_Info'
                 (Index_Item_Info,
                  Doc_Info_Options => Options,
                  Doc_LI_Unit      => No_LI_File,
                  Doc_File_List    => TSFL.Null_List,
                  Item_Name        => Package_Name,
                  Item_File        => Source_Filename,
                  Item_Line        => First_File_Line,
                  Item_Doc_File    => new String'
                    (Base_Name
                       (Get_Doc_File_Name
                          (Source_Filename, Doc_Directory, Doc_Suffix))));
               Converter
                 (B, Kernel, Index_File,
                  List_Ref_In_File,
                  Data_Item, Doc_Directory, Doc_Suffix,
                  Level);
               Free (Data_Item.Item_Doc_File);
            end if;

            Source_File_Node := TSFL.Next (Source_File_Node);
         end loop;
      end if;

      Data_End := Doc_Info'
        (End_Of_Index_Info,
         Doc_Info_Options => Options,
         Doc_LI_Unit      => No_LI_File,
         Doc_File_List    => TSFL.Null_List,
         End_Index_Title  => new String'("End of Index"));
      Converter
        (B, Kernel, Index_File,
         List_Ref_In_File,
         Data_End, Doc_Directory, Doc_Suffix,
         Level);

      Free (Data_End.End_Index_Title);
      Close (Index_File);
   end Process_Unit_Index;

   ------------------------------
   -- Process_Subprogram_Index --
   ------------------------------

   procedure Process_Subprogram_Index
     (B                             : Backend_Handle;
      Kernel                        : access Kernel_Handle_Record'Class;
      Subprogram_Index_List         : Type_Entity_List.List;
      Private_Subprogram_Index_List : in out Type_Entity_List.List;
      List_Ref_In_File              : in out List_Reference_In_File.List;
      Options                       : All_Options;
      Converter                     : Doc_Subprogram_Type;
      Doc_Directory                 : String;
      Doc_Suffix                    : String;
      Level                         : in out Natural)
   is
      use TEL;
      Source_Filename       : Virtual_File;
      Subprogram_Index_Node : Type_Entity_List.List_Node;
      Index_File            : File_Type;
      Data_Public           : Doc_Info (Info_Type => Public_Index_Info);
      Data_Private          : Doc_Info (Info_Type => Private_Index_Info);
      Data_Subprogram       : Doc_Info (Info_Type => Subprogram_Index_Info);
      Data_Item             : Doc_Info (Info_Type => Index_Item_Info);
      Data_End              : Doc_Info (Info_Type => End_Of_Index_Info);
      Doc_File_Name         : constant String := "index_sub";
   begin
      Create
        (Index_File, Out_File, Doc_Directory & Doc_File_Name & Doc_Suffix);

      Data_Subprogram := Doc_Info'
        (Subprogram_Index_Info,
         Doc_Info_Options           => Options,
         Doc_LI_Unit                => No_LI_File,
         Doc_File_List              => TSFL.Null_List,
         Subprogram_Index_File_Name => new String'(Doc_File_Name));
      Converter
        (B, Kernel, Index_File,
         List_Ref_In_File,
         Data_Subprogram, Doc_Directory, Doc_Suffix,
         Level);
      Free (Data_Subprogram.Subprogram_Index_File_Name);

      if not TEL.Is_Empty (Subprogram_Index_List) then

         if Options.Show_Private then
            --  Title "Public" is set
            Data_Public := Doc_Info'
              (Public_Index_Info,
               Doc_Info_Options           => Options,
               Doc_LI_Unit                => No_LI_File,
               Doc_File_List              => TSFL.Null_List,
               Public_Index_Title         => new String'("Public:"));
            Converter
              (B, Kernel, Index_File,
               List_Ref_In_File,
               Data_Public, Doc_Directory, Doc_Suffix,
               Level);
            Free (Data_Public.Public_Index_Title);
         end if;

         --  Public subprograms are printed
         Subprogram_Index_Node := TEL.First (Subprogram_Index_List);
         while Subprogram_Index_Node /= TEL.Null_Node loop
            Source_Filename := Get_Declaration_File_Of
              (TEL.Data (Subprogram_Index_Node).Entity);
            Data_Item := Doc_Info'
              (Index_Item_Info,
               Doc_Info_Options => Options,
               Doc_LI_Unit      => No_LI_File,
               Doc_File_List    => TSFL.Null_List,
               Item_Name        => new String'
                 (Get_Name (TEL.Data (Subprogram_Index_Node).Entity)),
               Item_File        => Source_Filename,
               Item_Line        => Get_Declaration_Line_Of
                 (TEL.Data (Subprogram_Index_Node).Entity),
               Item_Doc_File    => new String'
                 (Base_Name
                    (Get_Doc_File_Name
                       (Source_Filename, Doc_Directory, Doc_Suffix))));
            Converter
              (B, Kernel, Index_File,
               List_Ref_In_File,
               Data_Item, Doc_Directory, Doc_Suffix,
               Level);
            Free (Data_Item.Item_Doc_File);
            Free (Data_Item.Item_Name);

            Subprogram_Index_Node := TEL.Next (Subprogram_Index_Node);
         end loop;
      end if;

      if Options.Show_Private
        and then
          not TEL.Is_Empty (Private_Subprogram_Index_List) then
         --  Title "Private" is set.
         Data_Private := Doc_Info'
           (Private_Index_Info,
            Doc_Info_Options           => Options,
            Doc_LI_Unit                => No_LI_File,
            Doc_File_List              => TSFL.Null_List,
            Private_Index_Title        => new String'("Private:"));
         Converter
           (B, Kernel, Index_File,
            List_Ref_In_File,
            Data_Private, Doc_Directory, Doc_Suffix,
            Level);
         Free (Data_Private.Private_Index_Title);

         --  Private subprograms are printed.
         Subprogram_Index_Node := TEL.First (Private_Subprogram_Index_List);
         while Subprogram_Index_Node /= TEL.Null_Node loop
            Source_Filename := Get_Declaration_File_Of
              (TEL.Data (Subprogram_Index_Node).Entity);
            Data_Item := Doc_Info'
              (Index_Item_Info,
               Doc_Info_Options => Options,
               Doc_LI_Unit      => No_LI_File,
               Doc_File_List    => TSFL.Null_List,
               Item_Name        => new String'
                 (Get_Name (TEL.Data (Subprogram_Index_Node).Entity)),
               Item_File        => Source_Filename,
               Item_Line        => Get_Declaration_Line_Of
                 (TEL.Data (Subprogram_Index_Node).Entity),
               Item_Doc_File    => new String'
                 (Base_Name
                    (Get_Doc_File_Name
                       (Source_Filename, Doc_Directory, Doc_Suffix))));
            Converter
              (B, Kernel, Index_File,
               List_Ref_In_File,
               Data_Item, Doc_Directory, Doc_Suffix,
               Level);
            Free (Data_Item.Item_Doc_File);
            Free (Data_Item.Item_Name);

            Subprogram_Index_Node := TEL.Next (Subprogram_Index_Node);
         end loop;
      end if;

      Data_End := Doc_Info'
        (End_Of_Index_Info,
         Doc_Info_Options => Options,
         Doc_LI_Unit      => No_LI_File,
         Doc_File_List    => TSFL.Null_List,
         End_Index_Title  => new String'("End of Index"));
      Converter
        (B, Kernel, Index_File,
         List_Ref_In_File,
         Data_End, Doc_Directory, Doc_Suffix,
         Level);

      Free (Data_End.End_Index_Title);
      Close (Index_File);
   end Process_Subprogram_Index;

   ------------------------
   -- Process_Type_Index --
   ------------------------

   procedure Process_Type_Index
     (B                       : Backend_Handle;
      Kernel                  : access Kernel_Handle_Record'Class;
      Type_Index_List         : Docgen.Type_Entity_List.List;
      Private_Type_Index_List : in out Type_Entity_List.List;
      List_Ref_In_File        : in out List_Reference_In_File.List;
      Options                 : All_Options;
      Converter               : Doc_Subprogram_Type;
      Doc_Directory           : String;
      Doc_Suffix              : String;
      Level                   : in out Natural)
   is
      use TEL;
      Type_Index_Node : Type_Entity_List.List_Node;
      Index_File      : File_Type;
      Data_Private    : Doc_Info (Info_Type => Private_Index_Info);
      Data_Public     : Doc_Info (Info_Type => Public_Index_Info);
      Data_Type       : Doc_Info (Info_Type => Type_Index_Info);
      Data_Item       : Doc_Info (Info_Type => Index_Item_Info);
      Data_End        : Doc_Info (Info_Type => End_Of_Index_Info);
      Doc_File_Name   : constant String := "index_type";
   begin
      Create
        (Index_File, Out_File, Doc_Directory & Doc_File_Name & Doc_Suffix);
      Data_Type := Doc_Info'
        (Type_Index_Info,
         Doc_Info_Options     => Options,
         Doc_LI_Unit          => No_LI_File,
         Doc_File_List        => TSFL.Null_List,
         Type_Index_File_Name => new String'(Doc_File_Name));
      Converter (B, Kernel, Index_File,
                 List_Ref_In_File,
                 Data_Type, Doc_Directory, Doc_Suffix,
                 Level);
      Free (Data_Type.Type_Index_File_Name);

      if not TEL.Is_Empty (Type_Index_List) then

         if Options.Show_Private then
            --  Title "Public" is set.
            Data_Public := Doc_Info'
              (Public_Index_Info,
               Doc_Info_Options   => Options,
               Doc_LI_Unit        => No_LI_File,
               Doc_File_List      => TSFL.Null_List,
               Public_Index_Title => new String'("Public:"));
            Converter
              (B, Kernel, Index_File,
               List_Ref_In_File,
               Data_Public, Doc_Directory, Doc_Suffix,
               Level);
            Free (Data_Public.Public_Index_Title);
         end if;

         --  Public types are printed
         Type_Index_Node := TEL.First (Type_Index_List);
         while Type_Index_Node /= TEL.Null_Node loop
            Data_Item := Doc_Info'
              (Index_Item_Info,
               Doc_Info_Options => Options,
               Doc_LI_Unit      => No_LI_File,
               Doc_File_List    => TSFL.Null_List,
               Item_Name        => new String'
                 (Get_Name (TEL.Data (Type_Index_Node).Entity)),
               Item_File        =>
                 Get_Declaration_File_Of (TEL.Data (Type_Index_Node).Entity),
               Item_Line        => Get_Declaration_Line_Of
                 (TEL.Data (Type_Index_Node).Entity),
               Item_Doc_File    => new String'
                 (Base_Name
                    (Get_Doc_File_Name (Get_Declaration_File_Of
                                          (TEL.Data (Type_Index_Node).Entity),
                                        Doc_Directory,
                                        Doc_Suffix))));
            Converter
              (B, Kernel, Index_File,
               List_Ref_In_File,
               Data_Item, Doc_Directory, Doc_Suffix,
               Level);
            Free (Data_Item.Item_Doc_File);
            Free (Data_Item.Item_Name);

            Type_Index_Node := TEL.Next (Type_Index_Node);
         end loop;
      end if;

      if Options.Show_Private
        and then
          not TEL.Is_Empty (Private_Type_Index_List) then
         --  Title "Private" is set.
         Data_Private := Doc_Info'
           (Private_Index_Info,
            Doc_Info_Options           => Options,
            Doc_LI_Unit                => No_LI_File,
            Doc_File_List              => TSFL.Null_List,
            Private_Index_Title        => new String'("Private:"));
         Converter
           (B, Kernel, Index_File,
            List_Ref_In_File,
            Data_Private, Doc_Directory, Doc_Suffix,
            Level);
         Free (Data_Private.Private_Index_Title);

         --  Private types are printed
         Type_Index_Node := TEL.First (Private_Type_Index_List);
         while Type_Index_Node /= TEL.Null_Node loop
            Data_Item := Doc_Info'
              (Index_Item_Info,
               Doc_Info_Options => Options,
               Doc_LI_Unit      => No_LI_File,
               Doc_File_List    => TSFL.Null_List,
               Item_Name        => new String'
                 (Get_Name (TEL.Data (Type_Index_Node).Entity)),
               Item_File        =>
                 Get_Declaration_File_Of (TEL.Data (Type_Index_Node).Entity),
               Item_Line        => Get_Declaration_Line_Of
                 (TEL.Data (Type_Index_Node).Entity),
               Item_Doc_File    => new String'
                 (Base_Name
                    (Get_Doc_File_Name
                       (Get_Declaration_File_Of
                          (TEL.Data (Type_Index_Node).Entity),
                        Doc_Directory,
                        Doc_Suffix))));
            Converter
              (B, Kernel, Index_File,
               List_Ref_In_File,
               Data_Item, Doc_Directory, Doc_Suffix,
               Level);
            Free (Data_Item.Item_Doc_File);
            Free (Data_Item.Item_Name);

            Type_Index_Node := TEL.Next (Type_Index_Node);
         end loop;
      end if;

      Data_End := Doc_Info'
        (End_Of_Index_Info,
         Doc_Info_Options => Options,
         Doc_LI_Unit      => No_LI_File,
         Doc_File_List    => TSFL.Null_List,
         End_Index_Title  => new String'("End of Index"));
      Converter
        (B, Kernel, Index_File,
         List_Ref_In_File,
         Data_End, Doc_Directory, Doc_Suffix,
         Level);

      Free (Data_End.End_Index_Title);
      Close (Index_File);
   end Process_Type_Index;

   -------------------------------
   -- Process_Tagged_Type_Index --
   -------------------------------

   procedure Process_Tagged_Type_Index
     (B                         : Backend_Handle;
      Kernel                    : access Kernel_Handle_Record'Class;
      Tagged_Type_Index_List    : Docgen.Type_List_Tagged_Element.List;
      Private_Tagged_Types_List : in out Type_List_Tagged_Element.List;
      List_Ref_In_File          : in out List_Reference_In_File.List;
      Source_File_List          : in out Type_Source_File_List.List;
      Options                   : All_Options;
      Converter                 : Doc_Subprogram_Type;
      Doc_Directory             : String;
      Doc_Suffix                : String;
      Level                     : in out Natural)
   is
      use Type_List_Tagged_Element;
      use List_Entity_Handle;
      Tagged_Type_Index_Node : Type_List_Tagged_Element.List_Node;
      Index_File             : File_Type;
      Data_Public            : Doc_Info (Info_Type => Public_Index_Info);
      Data_Private           : Doc_Info (Info_Type => Private_Index_Info);
      Data_Tagged_Type       : Doc_Info (Info_Type => Tagged_Type_Index_Info);
      Data_Item              : Doc_Info (Info_Type => Index_Tagged_Type_Item);
      Data_End               : Doc_Info (Info_Type => End_Of_Index_Info);
      Doc_File_Name          : constant String := "index_tagged_type";
      Parent_Node            : List_Entity_Handle.List_Node;
      Child_Node             : List_Entity_Handle.List_Node;
      Tag_Elem               : Tagged_Element;

   begin
      Create
        (Index_File, Out_File, Doc_Directory & Doc_File_Name & Doc_Suffix);
      Data_Tagged_Type := Doc_Info'
        (Tagged_Type_Index_Info,
         Doc_Info_Options     => Options,
         Doc_LI_Unit          => No_LI_File,
         Doc_File_List        => TSFL.Null_List,
         Tagged_Type_Index_File_Name => new String'(Doc_File_Name));
      Converter (B, Kernel, Index_File,
                 List_Ref_In_File,
                 Data_Tagged_Type, Doc_Directory, Doc_Suffix,
                 Level);
      Free (Data_Tagged_Type.Tagged_Type_Index_File_Name);

      if not Type_List_Tagged_Element.Is_Empty (Tagged_Type_Index_List) then
         --  Work on public tagged types
         if Options.Show_Private then
            --  Title "Public" is set.
            Data_Public := Doc_Info'
              (Public_Index_Info,
               Doc_Info_Options           => Options,
               Doc_LI_Unit                => No_LI_File,
               Doc_File_List              => TSFL.Null_List,
               Public_Index_Title         => new String'("Public:"));
            Converter
              (B, Kernel, Index_File,
               List_Ref_In_File,
               Data_Public, Doc_Directory, Doc_Suffix,
               Level);
            Free (Data_Public.Public_Index_Title);
         end if;

         Tagged_Type_Index_Node :=
           Type_List_Tagged_Element.First (Tagged_Type_Index_List);

         while Tagged_Type_Index_Node /= Type_List_Tagged_Element.Null_Node
         loop
            Tag_Elem := Type_List_Tagged_Element.Data (Tagged_Type_Index_Node);

            if Tag_Elem.Me /= null and then Tag_Elem.Print_Me then
               --  Print the tagged type itself (only if the field Print_Me
               --  is True)
               --  Print_Me is false if the tagged type isn't define in the
               --  processed files.
               Data_Item := Doc_Info'
                 (Index_Tagged_Type_Item,
                  Doc_Info_Options => Options,
                  Doc_LI_Unit      => No_LI_File,
                  Doc_File_List    => TSFL.Null_List,
                  Doc_Tagged_Type  => Tag_Elem.Me.all,
                  Doc_Family       => Main,
                  Directory        => new String'(Doc_Directory),
                  Suffix           => new String'(Doc_Suffix));
               Converter
                 (B, Kernel, Index_File,
                  List_Ref_In_File,
                  Data_Item, Doc_Directory, Doc_Suffix,
                  Level);
               Free (Data_Item.Directory);
               Free (Data_Item.Suffix);

               if Tag_Elem.Number_Of_Parents > 0 then
                  --  There is at least one parent

                  Parent_Node := List_Entity_Handle.First
                    (Tag_Elem.My_Parents);

                  while Parent_Node /= List_Entity_Handle.Null_Node loop
                     if List_Entity_Handle.Data (Parent_Node) /= null then
                        if Source_File_In_List
                          (Source_File_List,
                           Get_Declaration_File_Of
                             (List_Entity_Handle.Data (Parent_Node).all))
                        then
                           --  Linkage is possible
                           Data_Item := Doc_Info'
                             (Index_Tagged_Type_Item,
                              Doc_Info_Options => Options,
                              Doc_LI_Unit      => No_LI_File,
                              Doc_File_List    => TSFL.Null_List,
                              Doc_Tagged_Type  =>
                                List_Entity_Handle.Data (Parent_Node).all,
                              Doc_Family       => Parent_With_Link,
                              Directory        => new String'(Doc_Directory),
                              Suffix           => new String'(Doc_Suffix));
                        else
                           --  No link for this parent
                           Data_Item := Doc_Info'
                             (Index_Tagged_Type_Item,
                              Doc_Info_Options => Options,
                              Doc_LI_Unit      => No_LI_File,
                              Doc_File_List    => TSFL.Null_List,
                              Doc_Tagged_Type  =>
                                List_Entity_Handle.Data (Parent_Node).all,
                              Doc_Family       => Parent_Without_Link,
                              Directory        => new String'(Doc_Directory),
                              Suffix           => new String'(Doc_Suffix));
                        end if;
                        Converter
                          (B, Kernel, Index_File,
                           List_Ref_In_File,
                           Data_Item, Doc_Directory, Doc_Suffix,
                           Level);
                        Free (Data_Item.Directory);
                        Free (Data_Item.Suffix);
                     end if;

                     Parent_Node
                       := List_Entity_Handle.Next (Parent_Node);
                  end loop;

               else
                  --  There's no parent
                  Data_Item := Doc_Info'
                    (Index_Tagged_Type_Item,
                     Doc_Info_Options => Options,
                     Doc_LI_Unit      => No_LI_File,
                     Doc_File_List    => TSFL.Null_List,
                     Doc_Tagged_Type  => No_Entity_Information,
                     Doc_Family       => No_Parent,
                     Directory        => new String'(Doc_Directory),
                     Suffix           => new String'(Doc_Suffix));
                  Converter
                    (B, Kernel, Index_File,
                     List_Ref_In_File,
                     Data_Item, Doc_Directory, Doc_Suffix,
                     Level);
                  Free (Data_Item.Directory);
                  Free (Data_Item.Suffix);
               end if;

               if Tag_Elem.Number_Of_Children > 0 then
                  --  There is at least one child

                  Child_Node := List_Entity_Handle.First
                    (Tag_Elem.My_Children);

                  while Child_Node /= List_Entity_Handle.Null_Node loop
                     if List_Entity_Handle.Data (Child_Node) /= null then
                        if Source_File_In_List
                          (Source_File_List, Get_Declaration_File_Of
                             (List_Entity_Handle.Data (Child_Node).all))
                        then
                           --  Linkage is possible
                           Data_Item := Doc_Info'
                             (Index_Tagged_Type_Item,
                              Doc_Info_Options => Options,
                              Doc_LI_Unit      => No_LI_File,
                              Doc_File_List    => TSFL.Null_List,
                              Doc_Tagged_Type  =>
                                List_Entity_Handle.Data
                                  (Child_Node).all,
                              Doc_Family       => Child_With_Link,
                              Directory        => new String'(Doc_Directory),
                              Suffix           => new String'(Doc_Suffix));
                        else
                           --  No link for this child
                           Data_Item := Doc_Info'
                             (Index_Tagged_Type_Item,
                              Doc_Info_Options => Options,
                              Doc_LI_Unit      => No_LI_File,
                              Doc_File_List    => TSFL.Null_List,
                              Doc_Tagged_Type  =>
                                List_Entity_Handle.Data
                                  (Child_Node).all,
                              Doc_Family       => Child_Without_Link,
                              Directory        => new String'(Doc_Directory),
                              Suffix           => new String'(Doc_Suffix));
                        end if;
                        Converter
                          (B, Kernel, Index_File,
                           List_Ref_In_File,
                           Data_Item, Doc_Directory, Doc_Suffix,
                           Level);
                        Free (Data_Item.Directory);
                        Free (Data_Item.Suffix);
                     end if;

                     Child_Node
                       := List_Entity_Handle.Next (Child_Node);
                  end loop;

               else
                  --  There's no child
                  Data_Item := Doc_Info'
                    (Index_Tagged_Type_Item,
                     Doc_Info_Options => Options,
                     Doc_LI_Unit      => No_LI_File,
                     Doc_File_List    => TSFL.Null_List,
                     Doc_Tagged_Type  => No_Entity_Information,
                     Doc_Family       => No_Child,
                     Directory        => new String'(Doc_Directory),
                     Suffix           => new String'(Doc_Suffix));
                  Converter
                    (B, Kernel, Index_File,
                     List_Ref_In_File,
                     Data_Item, Doc_Directory, Doc_Suffix,
                     Level);
                  Free (Data_Item.Directory);
                  Free (Data_Item.Suffix);
               end if;
            end if;

            Tagged_Type_Index_Node :=
              Type_List_Tagged_Element.Next (Tagged_Type_Index_Node);
         end loop;
      end if;

      --  Private tagged types are printed
      if Options.Show_Private
        and then
          not Type_List_Tagged_Element.Is_Empty
            (Private_Tagged_Types_List) then

         Data_Private := Doc_Info'
           (Private_Index_Info,
            Doc_Info_Options           => Options,
            Doc_LI_Unit                => No_LI_File,
            Doc_File_List              => TSFL.Null_List,
            Private_Index_Title        => new String'("Private:"));
         Converter
           (B, Kernel, Index_File,
            List_Ref_In_File,
            Data_Private, Doc_Directory, Doc_Suffix,
            Level);
         Free (Data_Private.Private_Index_Title);

         Tagged_Type_Index_Node :=
           Type_List_Tagged_Element.First (Private_Tagged_Types_List);

         while Tagged_Type_Index_Node /= Type_List_Tagged_Element.Null_Node
         loop
            Tag_Elem := Type_List_Tagged_Element.Data (Tagged_Type_Index_Node);

            if Tag_Elem.Me /= null and then Tag_Elem.Print_Me then
               --  Print the tagged type itself (only if the field Print_Me
               --  is True)

               Data_Item := Doc_Info'
                 (Index_Tagged_Type_Item,
                  Doc_Info_Options => Options,
                  Doc_LI_Unit      => No_LI_File,
                  Doc_File_List    => TSFL.Null_List,
                  Doc_Tagged_Type  => Tag_Elem.Me.all,
                  Doc_Family       => Main,
                  Directory        => new String'(Doc_Directory),
                  Suffix           => new String'(Doc_Suffix));
               Converter
                 (B, Kernel, Index_File,
                  List_Ref_In_File,
                  Data_Item, Doc_Directory, Doc_Suffix,
                  Level);
               Free (Data_Item.Directory);
               Free (Data_Item.Suffix);

               if Tag_Elem.Number_Of_Parents > 0 then
                  --  There is at least one parent

                  Parent_Node := List_Entity_Handle.First
                    (Tag_Elem.My_Parents);

                  while Parent_Node /= List_Entity_Handle.Null_Node loop
                     if List_Entity_Handle.Data (Parent_Node) /= null then
                        if Source_File_In_List
                          (Source_File_List,
                           Get_Declaration_File_Of
                             (List_Entity_Handle.Data (Parent_Node).all))
                        then
                           --  Linkage is possible
                           Data_Item := Doc_Info'
                             (Index_Tagged_Type_Item,
                              Doc_Info_Options => Options,
                              Doc_LI_Unit      => No_LI_File,
                              Doc_File_List    => TSFL.Null_List,
                              Doc_Tagged_Type  =>
                                List_Entity_Handle.Data (Parent_Node).all,
                              Doc_Family       => Parent_With_Link,
                              Directory        => new String'(Doc_Directory),
                              Suffix           => new String'(Doc_Suffix));
                        else
                           --  No link for this parent
                           Data_Item := Doc_Info'
                             (Index_Tagged_Type_Item,
                              Doc_Info_Options => Options,
                              Doc_LI_Unit      => No_LI_File,
                              Doc_File_List    => TSFL.Null_List,
                              Doc_Tagged_Type  =>
                                List_Entity_Handle.Data (Parent_Node).all,
                              Doc_Family       => Parent_Without_Link,
                              Directory        => new String'(Doc_Directory),
                              Suffix           => new String'(Doc_Suffix));
                        end if;
                        Converter
                          (B, Kernel, Index_File,
                           List_Ref_In_File,
                           Data_Item, Doc_Directory, Doc_Suffix,
                           Level);
                        Free (Data_Item.Directory);
                        Free (Data_Item.Suffix);
                     end if;

                     Parent_Node
                       := List_Entity_Handle.Next (Parent_Node);
                  end loop;

               else
                  --  There's no parent
                  Data_Item := Doc_Info'
                    (Index_Tagged_Type_Item,
                     Doc_Info_Options => Options,
                     Doc_LI_Unit      => No_LI_File,
                     Doc_File_List    => TSFL.Null_List,
                     Doc_Tagged_Type  => No_Entity_Information,
                     Doc_Family       => No_Parent,
                     Directory        => new String'(Doc_Directory),
                     Suffix           => new String'(Doc_Suffix));
                  Converter
                    (B, Kernel, Index_File,
                     List_Ref_In_File,
                     Data_Item, Doc_Directory, Doc_Suffix,
                     Level);
                  Free (Data_Item.Directory);
                  Free (Data_Item.Suffix);
               end if;

               if Tag_Elem.Number_Of_Children > 0 then
                  --  There is at least one child

                  Child_Node := List_Entity_Handle.First
                    (Tag_Elem.My_Children);

                  while Child_Node /= List_Entity_Handle.Null_Node loop
                     if List_Entity_Handle.Data (Child_Node) /= null then
                        if Source_File_In_List
                          (Source_File_List, Get_Declaration_File_Of
                             (List_Entity_Handle.Data (Child_Node).all))
                        then
                           --  Linkage is possible
                           Data_Item := Doc_Info'
                             (Index_Tagged_Type_Item,
                              Doc_Info_Options => Options,
                              Doc_LI_Unit      => No_LI_File,
                              Doc_File_List    => TSFL.Null_List,
                              Doc_Tagged_Type  =>
                                List_Entity_Handle.Data
                                  (Child_Node).all,
                              Doc_Family       => Child_With_Link,
                              Directory        => new String'(Doc_Directory),
                              Suffix           => new String'(Doc_Suffix));
                        else
                           --  No link for this child
                           Data_Item := Doc_Info'
                             (Index_Tagged_Type_Item,
                              Doc_Info_Options => Options,
                              Doc_LI_Unit      => No_LI_File,
                              Doc_File_List    => TSFL.Null_List,
                              Doc_Tagged_Type  =>
                                List_Entity_Handle.Data
                                  (Child_Node).all,
                              Doc_Family       => Child_Without_Link,
                              Directory        => new String'(Doc_Directory),
                              Suffix           => new String'(Doc_Suffix));
                        end if;
                        Converter
                          (B, Kernel, Index_File,
                           List_Ref_In_File,
                           Data_Item, Doc_Directory, Doc_Suffix,
                           Level);
                        Free (Data_Item.Directory);
                        Free (Data_Item.Suffix);
                     end if;

                     Child_Node
                       := List_Entity_Handle.Next (Child_Node);
                  end loop;

               else
                  --  There's no child
                  Data_Item := Doc_Info'
                    (Index_Tagged_Type_Item,
                     Doc_Info_Options => Options,
                     Doc_LI_Unit      => No_LI_File,
                     Doc_File_List    => TSFL.Null_List,
                     Doc_Tagged_Type  => No_Entity_Information,
                     Doc_Family       => No_Child,
                     Directory        => new String'(Doc_Directory),
                     Suffix           => new String'(Doc_Suffix));
                  Converter
                    (B, Kernel, Index_File,
                     List_Ref_In_File,
                     Data_Item, Doc_Directory, Doc_Suffix,
                     Level);
                  Free (Data_Item.Directory);
                  Free (Data_Item.Suffix);
               end if;
            end if;

            Tagged_Type_Index_Node :=
              Type_List_Tagged_Element.Next (Tagged_Type_Index_Node);
         end loop;
      end if;

      Data_End := Doc_Info'
        (End_Of_Index_Info,
         Doc_Info_Options => Options,
         Doc_LI_Unit      => No_LI_File,
         Doc_File_List    => TSFL.Null_List,
         End_Index_Title  => new String'("End of Index"));
      Converter (B, Kernel, Index_File,
                 List_Ref_In_File,
                 Data_End, Doc_Directory, Doc_Suffix,
                 Level);

      Free (Data_End.End_Index_Title);
      Close (Index_File);
   end Process_Tagged_Type_Index;

   --------------------
   -- Process_Header --
   --------------------

   procedure Process_Header
     (B                 : Backend_Handle;
      Kernel            : access Kernel_Handle_Record'Class;
      Doc_File          : File_Type;
      Entity_List       : in out Type_Entity_List.List;
      List_Ref_In_File  : in out List_Reference_In_File.List;
      Source_Filename   : VFS.Virtual_File;
      Package_Name      : String;
      Package_File      : Virtual_File;
      Process_Body_File : Boolean;
      Options           : All_Options;
      Converter         : Doc_Subprogram_Type;
      Doc_Directory     : String;
      Doc_Suffix        : String;
      Level             : in out Natural)
   is
      use TEL;
      Find_Header : Boolean := False;
      Declar_Line : Natural := First_File_Line;
      Entity_Node : Type_Entity_List.List_Node;
      Data_Header : Doc_Info :=
        (Header_Info,
         Doc_Info_Options => Options,
         Doc_LI_Unit      => No_LI_File,
         Doc_File_List    => TSFL.Null_List,
         Header_Package   => new String'(Package_Name),
         Header_File      => Package_File,
         Header_Line      => Declar_Line,
         Header_Link      => Process_Body_File);
      --           Header_Link      =>
      --           (Process_Body_File and
      --           (Other_File_Name (Kernel, Source_Filename) /= No_File));
      --  If a file x.ads (resp. x.adb) has no file x. adb (resp. x.ads),
      --  no link is made

      --  ??? currently, there's a bug with Other_File_Name. When it will be
      --  fixed, those commented lines will avoid making a link.
   begin
      if not TEL.Is_Empty (Entity_List) then
         Entity_Node := TEL.First (Entity_List);

         while Entity_Node /= TEL.Null_Node loop
            if TEL.Data (Entity_Node).Kind = Package_Entity
              and then
                TEL.Data (Entity_Node).Name /= null
                and then
                To_Lower (TEL.Data (Entity_Node).Name.all) =
              To_Lower (Package_Name)
            then
               --  It's a library level package declaration
               if Get_Declaration_File_Of (TEL.Data (Entity_Node).Entity)
                 = Source_Filename
               then
                  --  Clauses with may be above the declaration
                  --  of the main package
                  Declar_Line := Get_Declaration_Line_Of
                    (TEL.Data (Entity_Node).Entity);
               else
                  Declar_Line := Get_Line
                    (TEL.Data (Entity_Node).Line_In_Body);
               end if;

               Find_Header := True;
            end if;

            exit when Find_Header;

            Entity_Node := TEL.Next (Entity_Node);
         end loop;
      end if;

      Data_Header.Header_Line := Declar_Line;
      Converter (B, Kernel, Doc_File,
                 List_Ref_In_File,
                 Data_Header, Doc_Directory, Doc_Suffix,
                 Level);
      Free (Data_Header.Header_Package);
   end Process_Header;

   ------------------------------
   --  Process_Header_Private  --
   ------------------------------

   procedure Process_Header_Private
     (B                 : Backend_Handle;
      Kernel            : access Kernel_Handle_Record'Class;
      Doc_File          : File_Type;
      List_Ref_In_File  : in out List_Reference_In_File.List;
      Options           : All_Options;
      Converter         : Doc_Subprogram_Type;
      Doc_Directory     : String;
      Doc_Suffix        : String;
      Level             : in out Natural)
   is
      Data_Header_Private : Doc_Info :=
        (Header_Private_Info,
         Doc_Info_Options => Options,
         Doc_LI_Unit      => No_LI_File,
         Doc_File_List    => TSFL.Null_List,
         Header_Title     => new String'("Private:"));
   begin
      Converter (B, Kernel, Doc_File,
                 List_Ref_In_File,
                 Data_Header_Private, Doc_Directory, Doc_Suffix,
                 Level);
   end Process_Header_Private;

   --------------------
   -- Process_Footer --
   --------------------

   procedure Process_Footer
     (B                : Backend_Handle;
      Kernel           : access Kernel_Handle_Record'Class;
      Doc_File         : File_Type;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Package_File     : Virtual_File;
      Options          : All_Options;
      Converter        : Doc_Subprogram_Type;
      Doc_Directory    : String;
      Doc_Suffix       : String;
      Level            : in out Natural)
   is
      Data_Footer : Doc_Info :=
        (Footer_Info,
         Doc_Info_Options => Options,
         Doc_LI_Unit      => No_LI_File,
         Doc_File_List    => TSFL.Null_List,
         Footer_Title     => new String'("Docgen"),
         Footer_File      => Package_File);
   begin
      Converter (B, Kernel, Doc_File,
                 List_Ref_In_File,
                 Data_Footer, Doc_Directory, Doc_Suffix,
                 Level);
      Free (Data_Footer.Footer_Title);
   end Process_Footer;

   ---------------------------------
   -- Process_Package_Description --
   ---------------------------------

   procedure Process_Package_Description
     (B                : Backend_Handle;
      Kernel           : access Kernel_Handle_Record'Class;
      Doc_File         : File_Type;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Package_Name     : String;
      Text             : String;
      Options          : All_Options;
      Converter        : Doc_Subprogram_Type;
      Doc_Directory    : String;
      Doc_Suffix       : String;
      Level            : in out Natural)
   is
      Data_Subtitle     : Doc_Info (Info_Type => Subtitle_Info);
      Data_Package      : Doc_Info (Info_Type => Package_Desc_Info);
      Description_Found : Boolean;
      Start_Found       : Boolean;
      Line              : Natural;
      Max_Lines         : constant Natural := Count_Lines (Text);
      Description       : GNAT.OS_Lib.String_Access;
      Temp_Line         : GNAT.OS_Lib.String_Access;
   begin
      --  Try to find the first line of the description of the package
      --  if something else is found than a comment line => no description
      Description_Found := False;
      Start_Found       := False;
      Line              := 1;

      while not Start_Found and Line < Max_Lines + 1 loop
         Temp_Line := new String'(Get_Line_From_String (Text, Line));
         if Line_Is_Comment (Temp_Line.all) then
            Description_Found := True;
            Start_Found       := True;

         elsif not Line_Is_Empty (Temp_Line.all) then
            Start_Found := True;
         else
            Line := Line + 1;
         end if;
         Free (Temp_Line);
      end loop;

      if Description_Found then
         Data_Subtitle := Doc_Info'
           (Subtitle_Info,
            Doc_Info_Options => Options,
            Doc_LI_Unit      => No_LI_File,
            Doc_File_List    => TSFL.Null_List,
            Subtitle_Name    => new String'("Description"),
            Subtitle_Kind    => Package_Desc_Info,
            Subtitle_Package => new String'(Package_Name));
         Converter
           (B, Kernel, Doc_File,
            List_Ref_In_File,
            Data_Subtitle, Doc_Directory, Doc_Suffix,
            Level);
         Description := Extract_Comment (Text, Line, 0, True, Options);
         Data_Package := Doc_Info'
           (Package_Desc_Info,
            Doc_Info_Options         => Options,
            Doc_LI_Unit              => No_LI_File,
            Doc_File_List            => TSFL.Null_List,
            Package_Desc_Description => Description);
         Converter
           (B, Kernel, Doc_File,
            List_Ref_In_File,
            Data_Package, Doc_Directory, Doc_Suffix,
            Level);
         Free (Description);
      end if;
   end Process_Package_Description;

   --------------------------
   -- Process_With_Clauses --
   --------------------------

   procedure Process_With_Clause
     (B                : Backend_Handle;
      Kernel           : access Kernel_Handle_Record'Class;
      Doc_File         : File_Type;
      Parsed_List      : in out Construct_List;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Source_Filename  : VFS.Virtual_File;
      Package_Name     : String;
      File_Text        : GNAT.OS_Lib.String_Access;
      LI_Unit          : LI_File_Ptr;
      Source_File_List : in out Type_Source_File_List.List;
      Options          : All_Options;
      Converter        : Doc_Subprogram_Type;
      Doc_Directory    : String;
      Doc_Suffix       : String;
      Level            : in out Natural)
   is
      Data_Subtitle      : Doc_Info (Info_Type => Subtitle_Info);
      Data_With          : Doc_Info (Info_Type => With_Info);
      Old_Line, New_Line : GNAT.OS_Lib.String_Access;
      Final              : GNAT.OS_Lib.String_Access;
      Parse_Node         : Construct_Access;
      Parsed_List_End    : Boolean;
      First_With_Line    : Natural;
   begin
      New_Line        := new String'("");
      --  "  " replaced by "". It avoids to print the header Dependencies
      --  if there isn't such clause
      Parse_Node      := Parsed_List.First;
      Parsed_List_End := False;
      First_With_Line := 0;

      while not Parsed_List_End loop
         if Parse_Node.Category = Cat_With then
            Old_Line := New_Line;
            New_Line := new String'
              (New_Line.all & ASCII.LF &
               File_Text (Parse_Node.Sloc_Start.Index ..
                          Parse_Node.Sloc_End.Index));
            Free (Old_Line);

            if First_With_Line = 0 then
               First_With_Line := Parse_Node.Sloc_Start.Line;
            end if;
         end if;

         if Parse_Node = Parsed_List.Last
           or else Parse_Node.Category = Cat_Procedure
           or else Parse_Node.Category = Cat_Function
         then
            Parsed_List_End := True;
         else
            Parse_Node := Parse_Node.Next;
         end if;
      end loop;

      if New_Line'Length > 0 then
         Data_Subtitle := Doc_Info'
           (Subtitle_Info,
            Doc_Info_Options => Options,
            Doc_LI_Unit      => No_LI_File,
            Doc_File_List    => TSFL.Null_List,
            Subtitle_Name    => new String'("Dependencies"),
            Subtitle_Kind    => With_Info,
            Subtitle_Package => new String'(Package_Name));
         Converter
           (B, Kernel, Doc_File,
            List_Ref_In_File,
            Data_Subtitle, Doc_Directory, Doc_Suffix,
            Level);

         Final := new String'(New_Line.all
                                (New_Line.all'First + 1 ..
                                   New_Line.all'Last));
         --  the "+1" avoids the first ASCII.LF in New_Line
         Free (New_Line);

         Data_With := Doc_Info'
           (With_Info,
            Doc_Info_Options => Options,
            Doc_LI_Unit      => LI_Unit,
            Doc_File_List    => Source_File_List,
            With_Header_Line => First_With_Line,
            With_File        => Source_Filename,
            With_Header      => Final);
         Converter (B, Kernel, Doc_File,
                    List_Ref_In_File,
                    Data_With, Doc_Directory, Doc_Suffix,
                    Level);

         Free (Data_Subtitle.Subtitle_Name);
         Free (Data_Subtitle.Subtitle_Package);
      end if;

      Free (Final);
   end Process_With_Clause;

   -----------------------------
   -- Process_Header_Packages --
   -----------------------------

   procedure Process_Header_Packages
     (B                 : Backend_Handle;
      Kernel            : access Kernel_Handle_Record'Class;
      Doc_File          : File_Type;
      List_Ref_In_File  : in out List_Reference_In_File.List;
      Options           : All_Options;
      Converter         : Doc_Subprogram_Type;
      Doc_Directory     : String;
      Doc_Suffix        : String;
      Package_Name      : String;
      Level             : in out Natural)
   is
      Data_Subtitle     : Doc_Info (Info_Type => Subtitle_Info);
   begin
      Data_Subtitle := Doc_Info'
        (Subtitle_Info,
         Doc_Info_Options => Options,
         Doc_LI_Unit      => No_LI_File,
         Doc_File_List    => TSFL.Null_List,
         Subtitle_Name    => new String'("Packages"),
         Subtitle_Kind    => Package_Info,
         Subtitle_Package => new String'(Package_Name));
      Converter (B,
                 Kernel,
                 Doc_File,
                 List_Ref_In_File,
                 Data_Subtitle,
                 Doc_Directory,
                 Doc_Suffix,
                 Level);
      Free (Data_Subtitle.Subtitle_Name);
      Free (Data_Subtitle.Subtitle_Package);
   end Process_Header_Packages;

   --------------------------
   -- Process_Package_Open --
   --------------------------

   procedure Process_Package_Open
     (B                 : Backend_Handle;
      Kernel            : access Kernel_Handle_Record'Class;
      Doc_File          : File_Type;
      List_Ref_In_File  : in out List_Reference_In_File.List;
      Converter         : Doc_Subprogram_Type;
      Doc_Directory     : String;
      Doc_Suffix        : String;
      Level             : in out Natural;
      Data_Package      : in out Doc_Info) is
   begin
      Converter (B,
                 Kernel,
                 Doc_File,
                 List_Ref_In_File,
                 Data_Package,
                 Doc_Directory,
                 Doc_Suffix,
                 Level);
   end Process_Package_Open;

   ---------------------------
   -- Process_Package_Close --
   ---------------------------

   procedure Process_Package_Close
     (B                 : Backend_Handle;
      Kernel            : access Kernel_Handle_Record'Class;
      Doc_File          : File_Type;
      List_Ref_In_File  : in out List_Reference_In_File.List;
      Converter         : Doc_Subprogram_Type;
      Doc_Directory     : String;
      Doc_Suffix        : String;
      Level             : in out Natural;
      Data_Package      : in out Doc_Info) is
   begin
      Converter (B,
                 Kernel,
                 Doc_File,
                 List_Ref_In_File,
                 Data_Package,
                 Doc_Directory,
                 Doc_Suffix,
                 Level);
   end Process_Package_Close;

   -------------------------
   -- Process_Description --
   -------------------------

   procedure Process_Description
     (B                 : Backend_Handle;
      Kernel            : access Kernel_Handle_Record'Class;
      Doc_File          : File_Type;
      List_Ref_In_File  : in out List_Reference_In_File.List;
      Options           : All_Options;
      Converter         : Doc_Subprogram_Type;
      Doc_Directory     : String;
      Doc_Suffix        : String;
      Level             : in out Natural;
      Comment           : String)
   is
      Data_Description : Doc_Info :=
        (Description_Info,
         Doc_Info_Options => Options,
         Doc_LI_Unit      => No_LI_File,
         Doc_File_List    => TSFL.Null_List,
         Description      => new String'(Comment));
   begin
      Converter (B, Kernel, Doc_File,
                 List_Ref_In_File,
                 Data_Description, Doc_Directory, Doc_Suffix,
                 Level);
   end Process_Description;

   ----------------------
   -- Process_Packages --
   ----------------------

   procedure Process_Packages
     (B                         : Backend_Handle;
      Kernel                    : access Kernel_Handle_Record'Class;
      Doc_File                  : File_Type;
      Parsed_List               : in out Construct_List;
      Entity_List               : in out Type_Entity_List.List;
      List_Ref_In_File          : in out List_Reference_In_File.List;
      Tagged_Types_List         : in out Type_List_Tagged_Element.List;
      Private_Tagged_Types_List : in out Type_List_Tagged_Element.List;
      Source_Filename           : VFS.Virtual_File;
      Package_Name              : String;
      Package_Information       : Entity_Information;
      File_Text                 : GNAT.OS_Lib.String_Access;
      LI_Unit                   : LI_File_Ptr;
      Source_File_List          : in out Type_Source_File_List.List;
      Options                   : All_Options;
      Converter                 : Doc_Subprogram_Type;
      Doc_Directory             : String;
      Doc_Suffix                : String;
      Private_Entity            : Boolean;
      Display_Private           : in out Boolean;
      Level                     : in out Natural;
      Tree_Package              : Scope_Tree;
      Process_Body_File         : Boolean)
   is
      use TEL;
      Entity_Node       : Type_Entity_List.List_Node;
      Entity_Node_Prec  : Type_Entity_List.List_Node;
      Description       : GNAT.OS_Lib.String_Access;
      Header            : GNAT.OS_Lib.String_Access;
      Header_Temp       : GNAT.OS_Lib.String_Access;
      Name              : GNAT.OS_Lib.String_Access;
      First_Already_Set : Boolean;
      Entity_Info       : Entity_List_Information_Handle;
      Data_Package            : Doc_Info (Info_Type => Package_Info);
      Data_Package_Open_Close : Doc_Info (Info_Type
                                            => Package_Info_Open_Close);
   begin
      if not TEL.Is_Empty (Entity_List) then
         First_Already_Set := False;
         Entity_Node := TEL.First (Entity_List);
         Entity_Node_Prec := TEL.Null_Node;

         while Entity_Node /= TEL.Null_Node loop
            if
            --  Private and public entities are processed separately
              TEL.Data (Entity_Node).Is_Private = Private_Entity
              and then
            --  Check if the entity is a package
              TEL.Data (Entity_Node).Kind = Package_Entity
            --  but NOT the package itself
              and then
                To_Lower (TEL.Data (Entity_Node).Name.all)
              /= To_Lower (Package_Name)
            --  check if defined in this file, the others used only for bodys!
              and then Get_Declaration_File_Of
                (TEL.Data (Entity_Node).Entity) = Source_Filename
              and then
            --  The entity is defined in the current package
              Entity_Defined_In_Package (TEL.Data (Entity_Node).Entity,
                                         Package_Information,
                                         Tree_Package)
            then
               if Package_Contain_Entity (TEL.Data (Entity_Node).Entity,
                                          Entity_List,
                                          Tree_Package) then
                  --  Entities are declared in the current package
                  Header_Temp := Get_Whole_Header
                    (File_Text.all,
                     Parsed_List,
                     Get_Name (TEL.Data (Entity_Node).Entity),
                     Get_Declaration_Line_Of (TEL.Data (Entity_Node).Entity));

                  if Header_Temp /= null then
                     --  Indent spaces are deleted
                     Header := Remove_Indent (Header_Temp,
                                              Level * Get_Indent (B.all));
                     Free (Header_Temp);

                     if TEL.Data (Entity_Node).Is_Private and then
                       not Display_Private
                     then
                        --  Print title "private" required and not done
                        Process_Header_Private (B,
                                                Kernel,
                                                Doc_File,
                                                List_Ref_In_File,
                                                Options,
                                                Converter,
                                                Doc_Directory,
                                                Doc_Suffix,
                                                Level);
                        Display_Private := True;
                     end if;

                     --  Check if the subtitle "Packages" has already been
                     --  set.
                     if not First_Already_Set then
                        Process_Header_Packages (B,
                                                 Kernel,
                                                 Doc_File,
                                                 List_Ref_In_File,
                                                 Options,
                                                 Converter,
                                                 Doc_Directory,
                                                 Doc_Suffix,
                                                 Package_Name,
                                                 Level);
                        First_Already_Set := True;
                     end if;

                     Description := Extract_Comment
                       (File_Text.all,
                        Get_Declaration_Line_Of
                          (TEL.Data (Entity_Node).Entity),
                        Count_Lines (Header.all),
                        False,
                        Options);

                     --  We save in an Entity_Information the current package
                     --  because it must be removed from Entity_List
                     Entity_Info := new Entity_List_Information'
                       (Clone (TEL.Data (Entity_Node), True));
                     Name := new String'("package "
                                         & Get_Name (Entity_Info.all.Entity)
                                         & " is");

                     Data_Package_Open_Close := Doc_Info'
                       (Package_Info_Open_Close,
                        Doc_Info_Options          => Options,
                        Doc_LI_Unit               => LI_Unit,
                        Doc_File_List             => Source_File_List,
                        Package_Open_Close_Entity => Entity_Info.all,
                        Package_Open_Close_Header => Name,
                        Package_Open_Close_Header_Line
                          => Get_Declaration_Line_Of (Entity_Info.all.Entity));

                     Type_Entity_List.Remove_Nodes
                       (Entity_List,
                        Entity_Node_Prec,
                        Entity_Node);

                     Process_Package_Open
                       (B,
                        Kernel,
                        Doc_File,
                        List_Ref_In_File,
                        Converter,
                        Doc_Directory,
                        Doc_Suffix,
                        Level,
                        Data_Package_Open_Close);

                     Free (Name);

                     Level := Level + 1;
                     --  Recursive call in order to deal with entity defined
                     --  in the current package.
                     Process_Source_Spec
                       (B,
                        Kernel,
                        Doc_File,
                        Source_File_List,
                        Source_Filename,
                        Entity_Info.all.Name.all,
                        Entity_Info.all.Entity,
                        Entity_List,
                        List_Ref_In_File,
                        Tagged_Types_List,
                        Private_Tagged_Types_List,
                        Process_Body_File,
                        LI_Unit,
                        Options,
                        Converter,
                        Doc_Directory,
                        Doc_Suffix,
                        Level,
                        File_Text,
                        Tree_Package,
                        Parsed_List);
                     Level := Level - 1;

                     Name := new String'("end "
                                         & Get_Name (Entity_Info.all.Entity));

                     Data_Package_Open_Close := Doc_Info'
                       (Package_Info_Open_Close,
                        Doc_Info_Options          => Options,
                        Doc_LI_Unit               => LI_Unit,
                        Doc_File_List             => Source_File_List,
                        Package_Open_Close_Entity => Entity_Info.all,
                        Package_Open_Close_Header => Name,
                        Package_Open_Close_Header_Line
                          => Get_Declaration_Line_Of (Entity_Info.all.Entity));
                     Process_Package_Close
                       (B,
                        Kernel,
                        Doc_File,
                        List_Ref_In_File,
                        Converter,
                        Doc_Directory,
                        Doc_Suffix,
                        Level,
                        Data_Package_Open_Close);
                     if Description.all /= "" then
                        Process_Description
                          (B,
                           Kernel,
                           Doc_File,
                           List_Ref_In_File,
                           Options,
                           Converter,
                           Doc_Directory,
                           Doc_Suffix,
                           Level,
                           Description.all);
                     end if;
                     Free (Name);
                     Free (Entity_Info.all);
                     Free (Header);
                     Free (Description);

                     if Entity_Node_Prec = TEL.Null_Node then
                        Entity_Node := TEL.First (Entity_List);
                     else
                        Entity_Node := Next (Entity_Node_Prec);
                     end if;
                  else
                     Entity_Node_Prec := Entity_Node;
                     Entity_Node := TEL.Next (Entity_Node);
                  end if;
               else
                  --  The current package doesn't contain any declarations of
                  --  entities.
                  Header_Temp := Get_Whole_Header
                    (File_Text.all,
                     Parsed_List,
                     Get_Name (TEL.Data (Entity_Node).Entity),
                     Get_Declaration_Line_Of (TEL.Data (Entity_Node).Entity));

                  if Header_Temp /= null then
                     --  Indent spaces are deleted
                     Header := Remove_Indent (Header_Temp,
                                              Level * Get_Indent (B.all));
                     Free (Header_Temp);

                     if TEL.Data (Entity_Node).Is_Private and then
                       not Display_Private
                     then
                        --  It's the first time we met a private entity
                        --  and we work on the private part, so we put the
                        --  title "Private"
                        Process_Header_Private (B,
                                                Kernel,
                                                Doc_File,
                                                List_Ref_In_File,
                                                Options,
                                                Converter,
                                                Doc_Directory,
                                                Doc_Suffix,
                                                Level);
                        Display_Private := True;
                     end if;

                     --  Check if the subtitle "Packages" has already been
                     --  set.
                     if not First_Already_Set then
                        Process_Header_Packages (B,
                                                 Kernel,
                                                 Doc_File,
                                                 List_Ref_In_File,
                                                 Options,
                                                 Converter,
                                                 Doc_Directory,
                                                 Doc_Suffix,
                                                 Package_Name,
                                                 Level);
                        First_Already_Set := True;
                     end if;

                     Description := Extract_Comment
                       (File_Text.all,
                        Get_Declaration_Line_Of
                          (TEL.Data (Entity_Node).Entity),
                        Count_Lines (Header.all),
                        False,
                        Options);

                     Data_Package := Doc_Info'
                       (Package_Info,
                        Doc_Info_Options    => Options,
                        Doc_LI_Unit         => LI_Unit,
                        Doc_File_List       => Source_File_List,
                        Package_Entity      => TEL.Data (Entity_Node),
                        Package_Header      => Header,
                        Package_Header_Line => Get_Declaration_Line_Of
                          (TEL.Data (Entity_Node).Entity));
                     Converter (B,
                                Kernel,
                                Doc_File,
                                List_Ref_In_File,
                                Data_Package,
                                Doc_Directory,
                                Doc_Suffix,
                                Level);
                     if Description.all /= "" then
                        Process_Description
                          (B,
                           Kernel,
                           Doc_File,
                           List_Ref_In_File,
                           Options,
                           Converter,
                           Doc_Directory,
                           Doc_Suffix,
                           Level,
                           Description.all);
                     end if;
                     Free (Header);
                     Free (Description);

                     Type_Entity_List.Remove_Nodes
                       (Entity_List,
                        Entity_Node_Prec,
                        Entity_Node);

                     if Entity_Node_Prec = TEL.Null_Node then
                        Entity_Node := TEL.First (Entity_List);
                     else
                        Entity_Node := TEL.Next (Entity_Node_Prec);
                     end if;

                  else
                     Entity_Node_Prec := Entity_Node;
                     Entity_Node      := TEL.Next (Entity_Node);
                  end if;
               end if;
            else
               Entity_Node_Prec := Entity_Node;
               Entity_Node := TEL.Next (Entity_Node);
            end if;
         end loop;
      end if;
   end Process_Packages;

   -------------------------
   -- Process_Header_Vars --
   -------------------------

   procedure Process_Header_Vars
     (B                 : Backend_Handle;
      Kernel            : access Kernel_Handle_Record'Class;
      Doc_File          : File_Type;
      List_Ref_In_File  : in out List_Reference_In_File.List;
      Options           : All_Options;
      Converter         : Doc_Subprogram_Type;
      Doc_Directory     : String;
      Doc_Suffix        : String;
      Package_Name      : String;
      Level             : in out Natural)
   is
      Data_Subtitle     : Doc_Info (Info_Type => Subtitle_Info);
   begin
      Data_Subtitle := Doc_Info'
        (Subtitle_Info,
         Doc_Info_Options => Options,
         Doc_LI_Unit      => No_LI_File,
         Doc_File_List    => TSFL.Null_List,
         Subtitle_Name    => new String'("Constants and Named Numbers"),
         Subtitle_Kind    => Var_Info,
         Subtitle_Package => new String'(Package_Name));
      Converter (B,
                 Kernel,
                 Doc_File,
                 List_Ref_In_File,
                 Data_Subtitle,
                 Doc_Directory,
                 Doc_Suffix,
                 Level);
      Free (Data_Subtitle.Subtitle_Name);
      Free (Data_Subtitle.Subtitle_Package);
   end Process_Header_Vars;

   ------------------
   -- Process_Vars --
   ------------------

   procedure Process_Vars
     (B                : Backend_Handle;
      Kernel           : access Kernel_Handle_Record'Class;
      Doc_File         : File_Type;
      Parsed_List      : in out Construct_List;
      Entity_List      : in out Type_Entity_List.List;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Source_Filename  : VFS.Virtual_File;
      Package_Name     : String;
      Package_Info     : Entity_Information;
      File_Text        : GNAT.OS_Lib.String_Access;
      LI_Unit          : LI_File_Ptr;
      Source_File_List : in out Type_Source_File_List.List;
      Options          : All_Options;
      Converter        : Doc_Subprogram_Type;
      Doc_Directory    : String;
      Doc_Suffix       : String;
      Private_Entity   : Boolean;
      Display_Private  : in out Boolean;
      Level            : in out Natural;
      Tree_Package     : Scope_Tree)
   is
      use TEL;
      use type Basic_Types.String_Access;
      Entity_Node       : Type_Entity_List.List_Node;
      Entity_Node_Prec  : Type_Entity_List.List_Node;
      Description       : GNAT.OS_Lib.String_Access;
      Header            : GNAT.OS_Lib.String_Access;
      Header_Temp       : GNAT.OS_Lib.String_Access;
      Data_Var          : Doc_Info (Info_Type => Var_Info);
      First_Already_Set : Boolean;
      Delete_Node       : Boolean;
   begin
      if not TEL.Is_Empty (Entity_List) then
         First_Already_Set := False;
         Entity_Node := TEL.First (Entity_List);
         Entity_Node_Prec := TEL.Null_Node;

         while Entity_Node /= TEL.Null_Node loop

            Delete_Node       := False;
            if
            --  Private and public entities are processed separately
              TEL.Data (Entity_Node).Is_Private = Private_Entity
              and then
            --  Check if the entity is a variable
              TEL.Data (Entity_Node).Kind = Var_Entity
            --  Check if defined in this file, the others used only for bodys!
              and then Get_Declaration_File_Of
                (TEL.Data (Entity_Node).Entity) = Source_Filename
              and then
            --  The entity is defined in the current package
              Entity_Defined_In_Package (TEL.Data (Entity_Node).Entity,
                                         Package_Info,
                                         Tree_Package)
            then
               Header_Temp := Get_Whole_Header
                 (File_Text.all,
                  Parsed_List,
                  Get_Name (TEL.Data (Entity_Node).Entity),
                  Get_Declaration_Line_Of (TEL.Data (Entity_Node).Entity));

               --  Check if it was an entity with its own header
               if Header_Temp /= null then
                  --  Indent spaces are deleted
                  Header := Remove_Indent (Header_Temp,
                                           Level * Get_Indent (B.all));
                  Free (Header_Temp);

                  if TEL.Data (Entity_Node).Is_Private and then
                    not Display_Private
                  then
                     --  It's the first time we met a private entity
                     --  and we work on the private part, so we put the
                     --  title "Private"
                     Process_Header_Private (B,
                                             Kernel,
                                             Doc_File,
                                             List_Ref_In_File,
                                             Options,
                                             Converter,
                                             Doc_Directory,
                                             Doc_Suffix,
                                             Level);
                     Display_Private := True;
                  end if;

                  --  Check if the subtitle "Constand and Named Numbers:"
                  --  has been set already.
                  if not First_Already_Set then
                     Process_Header_Vars (B,
                                          Kernel,
                                          Doc_File,
                                          List_Ref_In_File,
                                          Options,
                                          Converter,
                                          Doc_Directory,
                                          Doc_Suffix,
                                          Package_Name,
                                          Level);
                     First_Already_Set := True;
                  end if;

                  Description := Extract_Comment
                    (File_Text.all,
                     Get_Declaration_Line_Of (TEL.Data (Entity_Node).Entity),
                     Count_Lines (Header.all),
                     False,
                     Options);

                  Data_Var := Doc_Info'
                    (Var_Info,
                     Doc_Info_Options => Options,
                     Doc_LI_Unit      => LI_Unit,
                     Doc_File_List    => Source_File_List,
                     Var_Entity       => TEL.Data (Entity_Node),
                     Var_Header       => Header,
                     Var_Header_Line  => Get_Declaration_Line_Of
                       (TEL.Data (Entity_Node).Entity));
                  Converter (B,
                             Kernel,
                             Doc_File,
                             List_Ref_In_File,
                             Data_Var,
                             Doc_Directory,
                             Doc_Suffix,
                             Level);
                  if Description.all /= "" then
                     Process_Description
                       (B,
                        Kernel,
                        Doc_File,
                        List_Ref_In_File,
                        Options,
                        Converter,
                        Doc_Directory,
                        Doc_Suffix,
                        Level,
                        Description.all);
                  end if;
                  Delete_Node := True;
                  Free (Header);
                  Free (Description);
               end if;
            end if;

            if Delete_Node then
               Type_Entity_List.Remove_Nodes
                 (Entity_List,
                  Entity_Node_Prec,
                  Entity_Node);

               if Entity_Node_Prec = TEL.Null_Node then
                  Entity_Node := TEL.First (Entity_List);
               else
                  Entity_Node := TEL.Next (Entity_Node_Prec);
               end if;
            else
               Entity_Node_Prec := Entity_Node;
               Entity_Node := TEL.Next (Entity_Node);
            end if;
         end loop;
      end if;
   end Process_Vars;

   -------------------------------
   -- Process_Header_Exceptions --
   -------------------------------

   procedure Process_Header_Exceptions
     (B                 : Backend_Handle;
      Kernel            : access Kernel_Handle_Record'Class;
      Doc_File          : File_Type;
      List_Ref_In_File  : in out List_Reference_In_File.List;
      Options           : All_Options;
      Converter         : Doc_Subprogram_Type;
      Doc_Directory     : String;
      Doc_Suffix        : String;
      Package_Name      : String;
      Level             : in out Natural)
   is
      Data_Subtitle     : Doc_Info (Info_Type => Subtitle_Info);
   begin
      Data_Subtitle := Doc_Info'
        (Subtitle_Info,
         Doc_Info_Options => Options,
         Doc_LI_Unit      => No_LI_File,
         Doc_File_List    => TSFL.Null_List,
         Subtitle_Name    => new String'("Exceptions"),
         Subtitle_Kind    => Exception_Info,
         Subtitle_Package => new String'(Package_Name));
      Converter (B,
                 Kernel,
                 Doc_File,
                 List_Ref_In_File,
                 Data_Subtitle,
                 Doc_Directory,
                 Doc_Suffix,
                 Level);
      Free (Data_Subtitle.Subtitle_Name);
      Free (Data_Subtitle.Subtitle_Package);
   end Process_Header_Exceptions;

   ------------------------
   -- Process_Exceptions --
   ------------------------

   procedure Process_Exceptions
     (B                : Backend_Handle;
      Kernel           : access Kernel_Handle_Record'Class;
      Doc_File         : File_Type;
      Parsed_List      : in out Construct_List;
      Entity_List      : in out Type_Entity_List.List;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Source_Filename  : VFS.Virtual_File;
      Package_Name     : String;
      Package_Info     : Entity_Information;
      File_Text        : GNAT.OS_Lib.String_Access;
      LI_Unit          : LI_File_Ptr;
      Source_File_List : in out Type_Source_File_List.List;
      Options          : All_Options;
      Converter        : Doc_Subprogram_Type;
      Doc_Directory    : String;
      Doc_Suffix       : String;
      Private_Entity   : Boolean;
      Display_Private  : in out Boolean;
      Level            : in out Natural;
      Tree_Package     : Scope_Tree)
   is
      use TEL;
      Entity_Node       : Type_Entity_List.List_Node;
      Entity_Node_Prec  : Type_Entity_List.List_Node;
      Description       : GNAT.OS_Lib.String_Access;
      Header            : GNAT.OS_Lib.String_Access;
      Header_Temp       : GNAT.OS_Lib.String_Access;
      Data_Exception    : Doc_Info (Info_Type => Exception_Info);
      First_Already_Set : Boolean;
      Delete_Node       : Boolean;

   begin
      if not TEL.Is_Empty (Entity_List) then
         First_Already_Set := False;
         Entity_Node := TEL.First (Entity_List);
         Entity_Node_Prec := TEL.Null_Node;

         while Entity_Node /= TEL.Null_Node loop
            Delete_Node := False;

            if
            --  Private and public entities are processed separately
              TEL.Data (Entity_Node).Is_Private = Private_Entity
              and then
            --  Check if the entity is an exception
              TEL.Data (Entity_Node).Kind = Exception_Entity
            --  Check if defined in this file, the others used only for bodys!
              and then Get_Declaration_File_Of
                (TEL.Data (Entity_Node).Entity) = Source_Filename
              and then
            --  The entity is defined in the current package
              Entity_Defined_In_Package (TEL.Data (Entity_Node).Entity,
                                         Package_Info,
                                         Tree_Package)
            then
               Header_Temp := Get_Whole_Header
                 (File_Text.all,
                  Parsed_List,
                  Get_Name (TEL.Data (Entity_Node).Entity),
                  Get_Declaration_Line_Of (TEL.Data (Entity_Node).Entity));

               --  Check if it was a entity with its own header

               if Header_Temp /= null then
                  --  Indent spaces are deleted
                  Header := Remove_Indent (Header_Temp,
                                           Level * Get_Indent (B.all));
                  Free (Header_Temp);

                  if TEL.Data (Entity_Node).Is_Private and then
                    not Display_Private
                  then
                     --  It's the first time we met a private entity
                     --  and we work on the private part, so we put the
                     --  title "Private"
                     Process_Header_Private (B,
                                             Kernel,
                                             Doc_File,
                                             List_Ref_In_File,
                                             Options,
                                             Converter,
                                             Doc_Directory,
                                             Doc_Suffix,
                                             Level);
                     Display_Private := True;
                  end if;

                  --  Check if the subtitle "Exceptions:" has been set already.
                  if not First_Already_Set then
                     Process_Header_Exceptions (B,
                                                Kernel,
                                                Doc_File,
                                                List_Ref_In_File,
                                                Options,
                                                Converter,
                                                Doc_Directory,
                                                Doc_Suffix,
                                                Package_Name,
                                                Level);
                     First_Already_Set := True;
                  end if;

                  Description := Extract_Comment
                    (File_Text.all,
                     Get_Declaration_Line_Of (TEL.Data (Entity_Node).Entity),
                     Count_Lines (Header.all),
                     False,
                     Options);

                  Data_Exception := Doc_Info'
                    (Exception_Info,
                     Doc_Info_Options      => Options,
                     Doc_LI_Unit           => LI_Unit,
                     Doc_File_List         => Source_File_List,
                     Exception_Entity      => TEL.Data (Entity_Node),
                     Exception_Header      => Header,
                     Exception_Header_Line => Get_Declaration_Line_Of
                       (TEL.Data (Entity_Node).Entity));
                  Converter (B,
                             Kernel,
                             Doc_File,
                             List_Ref_In_File,
                             Data_Exception,
                             Doc_Directory,
                             Doc_Suffix,
                             Level);
                  if Description.all /= "" then
                     Process_Description
                       (B,
                        Kernel,
                        Doc_File,
                        List_Ref_In_File,
                        Options,
                        Converter,
                        Doc_Directory,
                        Doc_Suffix,
                        Level,
                        Description.all);
                  end if;
                  Delete_Node := True;
                  Free (Header);
                  Free (Description);
               end if;
            end if;

            if Delete_Node then
               Type_Entity_List.Remove_Nodes
                 (Entity_List,
                  Entity_Node_Prec,
                  Entity_Node);

               if Entity_Node_Prec = TEL.Null_Node then
                  Entity_Node := TEL.First (Entity_List);
               else
                  Entity_Node := TEL.Next (Entity_Node_Prec);
               end if;
            else
               Entity_Node_Prec := Entity_Node;
               Entity_Node := TEL.Next (Entity_Node);
            end if;
         end loop;
      end if;
   end Process_Exceptions;

   --------------------------
   -- Process_Header_Types --
   --------------------------

   procedure Process_Header_Types
     (B                 : Backend_Handle;
      Kernel            : access Kernel_Handle_Record'Class;
      Doc_File          : File_Type;
      List_Ref_In_File  : in out List_Reference_In_File.List;
      Options           : All_Options;
      Converter         : Doc_Subprogram_Type;
      Doc_Directory     : String;
      Doc_Suffix        : String;
      Package_Name      : String;
      Level             : in out Natural)
   is
      Data_Subtitle     : Doc_Info (Info_Type => Subtitle_Info);
   begin
      Data_Subtitle := Doc_Info'
        (Subtitle_Info,
         Doc_Info_Options => Options,
         Doc_LI_Unit      => No_LI_File,
         Doc_File_List    => TSFL.Null_List,
         Subtitle_Name    => new String'("Types"),
         Subtitle_Kind    => Type_Info,
         Subtitle_Package => new String'(Package_Name));
      Converter (B,
                 Kernel,
                 Doc_File,
                 List_Ref_In_File,
                 Data_Subtitle,
                 Doc_Directory,
                 Doc_Suffix,
                 Level);
      Free (Data_Subtitle.Subtitle_Name);
      Free (Data_Subtitle.Subtitle_Package);
   end Process_Header_Types;

   ------------------------
   -- Process_One_Family --
   ------------------------

   procedure Process_One_Family
     (B                : Backend_Handle;
      Kernel           : access Kernel_Handle_Record'Class;
      Doc_File         : File_Type;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Family           : Type_List_Tagged_Element.List;
      Source_File_List : in out Type_Source_File_List.List;
      Entity           : Entity_Information;
      Level            : in out Natural;
      Options          : All_Options;
      Converter        : Doc_Subprogram_Type;
      Doc_Directory    : String;
      Doc_Suffix       : String)
   is
      use Type_List_Tagged_Element;
      use List_Entity_Handle;
      Tagged_Type_Node : Type_List_Tagged_Element.List_Node;
      Tag_Elem         : Tagged_Element;
      Found            : Boolean := False;
      Data             : Doc_Info (Info_Type => Tagged_Type_Info);
   begin
      if not Type_List_Tagged_Element.Is_Empty (Family) then
         Tagged_Type_Node := Type_List_Tagged_Element.First (Family);

         --  Research of the tagged type in the list
         while Tagged_Type_Node /= Type_List_Tagged_Element.Null_Node loop
            Tag_Elem := Type_List_Tagged_Element.Data (Tagged_Type_Node);

            if Tag_Elem.Me /= null
              and then
                Is_Equal (Tag_Elem.Me.all, Entity)
              and then
                Tag_Elem.Print_Me
            --  Print the tagged type itself (only if the field Print_Me
            --  is True)
            --  Print_Me is false if the tagged type isn't define in the
            --  processed files.
            then
               Data := Doc_Info'
                 (Tagged_Type_Info,
                  Doc_Info_Options => Options,
                  Doc_LI_Unit      => No_LI_File,
                  Doc_File_List    => TSFL.Null_List,
                  Tagged_Entity    => Tag_Elem,
                  Tagged_Source_File_List => Source_File_List,
                  Tagged_Directory => new String'(Doc_Directory),
                  Tagged_Suffix    => new String'(Doc_Suffix));
               Converter
                 (B, Kernel, Doc_File,
                  List_Ref_In_File,
                  Data,
                  Doc_Directory, Doc_Suffix,
                  Level);
               Free (Data.Tagged_Directory);
               Free (Data.Tagged_Suffix);
               Found := True;
            end if;
            exit when Found;
            Tagged_Type_Node
              := Type_List_Tagged_Element.Next (Tagged_Type_Node);
         end loop;
      end if;
   end Process_One_Family;

   -------------------
   -- Process_Types --
   -------------------

   procedure Process_Types
     (B                         : Backend_Handle;
      Kernel                    : access Kernel_Handle_Record'Class;
      Doc_File                  : File_Type;
      Parsed_List               : in out Construct_List;
      Entity_List               : in out Type_Entity_List.List;
      List_Ref_In_File          : in out List_Reference_In_File.List;
      Tagged_Types_List         : in out Type_List_Tagged_Element.List;
      Private_Tagged_Types_List : in out Type_List_Tagged_Element.List;
      Source_Filename           : VFS.Virtual_File;
      Package_Name              : String;
      Package_Info              : Entity_Information;
      File_Text                 : GNAT.OS_Lib.String_Access;
      LI_Unit                   : LI_File_Ptr;
      Source_File_List          : in out Type_Source_File_List.List;
      Options                   : All_Options;
      Converter                 : Doc_Subprogram_Type;
      Doc_Directory             : String;
      Doc_Suffix                : String;
      Private_Entity            : Boolean;
      Display_Private           : in out Boolean;
      Level                     : in out Natural;
      Tree_Package              : Scope_Tree)
   is
      use TEL;
      Entity_Node        : Type_Entity_List.List_Node;
      Entity_Node_Prec   : Type_Entity_List.List_Node;
      Description        : GNAT.OS_Lib.String_Access;
      Header             : GNAT.OS_Lib.String_Access;
      Header_Temp        : GNAT.OS_Lib.String_Access;
      Data_Type          : Doc_Info (Info_Type => Type_Info);
      First_Already_Set  : Boolean;
      Delete_Node        : Boolean;
      In_Current_Package : Boolean := False;
   begin
      if not TEL.Is_Empty (Entity_List) then
         First_Already_Set := False;
         Entity_Node      := TEL.First (Entity_List);
         Entity_Node_Prec := TEL.Null_Node;

         while Entity_Node /= TEL.Null_Node loop
            Delete_Node := False;

            --  We search if the type is declared in the current package
            if TEL.Data (Entity_Node).Public_Declaration
              = No_Entity_Information
            then
               In_Current_Package
                 := Entity_Defined_In_Package (TEL.Data (Entity_Node).Entity,
                                               Package_Info,
                                               Tree_Package);
            else
               --  Public entity wich contains private fields.
               --  In this case, the name of the entity appears in two
               --  elements of Entity_List. One element refers to the
               --  public declaration and the second element refers the
               --  private part. For this last element the field Line_In_Body
               --  refers to the code in the private part and the field
               --  Public_Declaration leads us to the public declaration.
               --  We need this public declaration because only this one
               --  appears in the scope tree.
               In_Current_Package := Entity_Defined_In_Package
                 (TEL.Data (Entity_Node).Public_Declaration,
                  Package_Info,
                  Tree_Package);
            end if;

            if
            --  Private and public entities are processed separately
              TEL.Data (Entity_Node).Is_Private = Private_Entity
              and then
            --  Check if the entity is a type
              TEL.Data (Entity_Node).Kind = Type_Entity
            --  Check if defined in this file (the rest of entities
            --  only for the body documentation)
              and then Get_Declaration_File_Of
                (TEL.Data (Entity_Node).Entity) = Source_Filename
              and then
            --  The entity is declared in the current package
              In_Current_Package
            then
               Header_Temp := Get_Whole_Header
                 (File_Text.all,
                  Parsed_List,
                  Get_Name (TEL.Data (Entity_Node).Entity),
                  Get_Declaration_Line_Of (TEL.Data (Entity_Node).Entity));

               --  Check if it was a entity with its own header
               if Header_Temp /= null then
                  --  Indent spaces are deleted
                  Header := Remove_Indent (Header_Temp,
                                           Level * Get_Indent (B.all));
                  Free (Header_Temp);

                  if TEL.Data (Entity_Node).Is_Private and then
                    not Display_Private
                  then
                     --  It's the first time we met a private entity
                     --  and we work on the private part, so we put the
                     --  title "Private"
                     Process_Header_Private (B,
                                             Kernel,
                                             Doc_File,
                                             List_Ref_In_File,
                                             Options,
                                             Converter,
                                             Doc_Directory,
                                             Doc_Suffix,
                                             Level);
                     Display_Private := True;
                  end if;

                  --  Check if the subtitle "Types:" has to be set.
                  if not First_Already_Set then
                     Process_Header_Types (B,
                                           Kernel,
                                           Doc_File,
                                           List_Ref_In_File,
                                           Options,
                                           Converter,
                                           Doc_Directory,
                                           Doc_Suffix,
                                           Package_Name,
                                           Level);
                     First_Already_Set := True;
                  end if;

                  Description := Extract_Comment
                    (File_Text.all,
                     Get_Declaration_Line_Of (TEL.Data (Entity_Node).Entity),
                     Count_Lines (Header.all),
                     False,
                     Options);

                  Data_Type := Doc_Info'
                    (Type_Info,
                     Doc_Info_Options => Options,
                     Doc_LI_Unit      => LI_Unit,
                     Doc_File_List    => Source_File_List,
                     Type_Entity      => TEL.Data (Entity_Node),
                     Type_Header      => Header,
                     Type_Header_Line => Get_Declaration_Line_Of
                       (TEL.Data (Entity_Node).Entity));
                  Converter (B,
                             Kernel,
                             Doc_File,
                             List_Ref_In_File,
                             Data_Type,
                             Doc_Directory,
                             Doc_Suffix,
                             Level);
                  if Description.all /= "" then
                     Process_Description
                       (B,
                        Kernel,
                        Doc_File,
                        List_Ref_In_File,
                        Options,
                        Converter,
                        Doc_Directory,
                        Doc_Suffix,
                        Level,
                        Description.all);
                  end if;

                  if Options.Tagged_Types
                    and then
                      (Get_Kind (TEL.Data (Entity_Node).Entity).Kind
                         = Record_Kind
                        --  ??? In Ada, tagged type are classified as Record
                        --  It must be improved (see also comments in
                        --  docgen-work_on_file.adb)
                       or else Get_Kind (TEL.Data (Entity_Node).Entity).Kind
                         = Class
                       or else Get_Kind (TEL.Data (Entity_Node).Entity).Kind
                         = Class_Wide)
                  --  it's a tagged type
                  then
                     if Private_Entity then
                        --  List of private tagged types
                        Process_One_Family
                          (B,
                           Kernel,
                           Doc_File,
                           List_Ref_In_File,
                           Private_Tagged_Types_List,
                           Source_File_List,
                           TEL.Data (Entity_Node).Entity,
                           Level,
                           Options,
                           Converter,
                           Doc_Directory,
                           Doc_Suffix);
                     else
                        --  List of public tagged types
                        Process_One_Family
                          (B,
                           Kernel,
                           Doc_File,
                           List_Ref_In_File,
                           Tagged_Types_List,
                           Source_File_List,
                           TEL.Data (Entity_Node).Entity,
                           Level,
                           Options,
                           Converter,
                           Doc_Directory,
                           Doc_Suffix);
                     end if;
                  end if;

                  Delete_Node := True;
                  Free (Header);
                  Free (Description);
               end if;
            end if;

            if Delete_Node then
               Type_Entity_List.Remove_Nodes
                 (Entity_List,
                  Entity_Node_Prec,
                  Entity_Node);

               if Entity_Node_Prec = TEL.Null_Node then
                  Entity_Node := TEL.First (Entity_List);
               else
                  Entity_Node := TEL.Next (Entity_Node_Prec);
               end if;
            else
               Entity_Node_Prec := Entity_Node;
               Entity_Node := TEL.Next (Entity_Node);
            end if;
         end loop;
      end if;
   end Process_Types;

   ----------------------------
   -- Process_Header_Entries --
   ----------------------------

   procedure Process_Header_Entries
     (B                 : Backend_Handle;
      Kernel            : access Kernel_Handle_Record'Class;
      Doc_File          : File_Type;
      List_Ref_In_File  : in out List_Reference_In_File.List;
      Options           : All_Options;
      Converter         : Doc_Subprogram_Type;
      Doc_Directory     : String;
      Doc_Suffix        : String;
      Package_Name      : String;
      Level             : in out Natural)
   is
      Data_Subtitle     : Doc_Info (Info_Type => Subtitle_Info);
   begin
      Data_Subtitle := Doc_Info'
        (Subtitle_Info,
         Doc_Info_Options => Options,
         Doc_LI_Unit      => No_LI_File,
         Doc_File_List    => TSFL.Null_List,
         Subtitle_Name    =>
         new String'("Tasks, Entries and Entry Families"),
         Subtitle_Kind    => Entry_Info,
         Subtitle_Package => new String'(Package_Name));
      Converter (B,
                 Kernel,
                 Doc_File,
                 List_Ref_In_File,
                 Data_Subtitle,
                 Doc_Directory,
                 Doc_Suffix,
                 Level);
      Free (Data_Subtitle.Subtitle_Name);
      Free (Data_Subtitle.Subtitle_Package);
   end Process_Header_Entries;

   ---------------------
   -- Process_Entries --
   ---------------------

   procedure Process_Entries
     (B                  : Backend_Handle;
      Kernel             : access Kernel_Handle_Record'Class;
      Doc_File           : File_Type;
      Parsed_List        : in out Construct_List;
      Entity_List        : in out Type_Entity_List.List;
      List_Ref_In_File   : in out List_Reference_In_File.List;
      Source_Filename    : VFS.Virtual_File;
      Process_Body_File  : Boolean;
      Package_Name       : String;
      Package_Info       : Entity_Information;
      File_Text          : GNAT.OS_Lib.String_Access;
      LI_Unit            : LI_File_Ptr;
      Source_File_List   : in out Type_Source_File_List.List;
      Options            : All_Options;
      Converter          : Doc_Subprogram_Type;
      Doc_Directory      : String;
      Doc_Suffix         : String;
      Private_Entity     : Boolean;
      Display_Private    : in out Boolean;
      Level              : in out Natural;
      Tree_Package       : Scope_Tree)
   is
      use TEL;
      Entity_Node       : Type_Entity_List.List_Node;
      Entity_Node_Prec  : Type_Entity_List.List_Node;
      Description       : GNAT.OS_Lib.String_Access;
      Header            : GNAT.OS_Lib.String_Access;
      Header_Temp       : GNAT.OS_Lib.String_Access;
      Data_Entry        : Doc_Info (Info_Type => Entry_Info);
      First_Already_Set : Boolean;
      Delete_Node       : Boolean;
   begin
      if not TEL.Is_Empty (Entity_List) then
         First_Already_Set := False;
         Entity_Node := TEL.First (Entity_List);
         Entity_Node_Prec := TEL.Null_Node;

         while Entity_Node /= TEL.Null_Node loop
            Delete_Node := False;

            if
            --  Private and public entities are processed separately
              TEL.Data (Entity_Node).Is_Private = Private_Entity
              and then
            --  Check if the entity is a entry or entry family
              TEL.Data (Entity_Node).Kind = Entry_Entity
            --  Check if defined in this file (the rest of
            --  entities only for the body documentation)
              and then Get_Declaration_File_Of
                (TEL.Data (Entity_Node).Entity) = Source_Filename
              and then
            --  The entity is defined in the current package
              Entity_Defined_In_Package (TEL.Data (Entity_Node).Entity,
                                         Package_Info,
                                         Tree_Package)
            then
               Header_Temp := Get_Whole_Header
                 (File_Text.all,
                  Parsed_List,
                  Get_Name (TEL.Data (Entity_Node).Entity),
                  Get_Declaration_Line_Of (TEL.Data (Entity_Node).Entity));

               --  Check if it was a entity with its own header

               if Header_Temp /= null then
                  --  Indent spaces are deleted
                  Header := Remove_Indent (Header_Temp,
                                           Level * Get_Indent (B.all));
                  Free (Header_Temp);

                  if TEL.Data (Entity_Node).Is_Private and then
                    not Display_Private
                  then
                     --  It's the first time we met a private entity
                     --  and we work on the private part, so we put the
                     --  title "Private"
                     Process_Header_Private (B,
                                             Kernel,
                                             Doc_File,
                                             List_Ref_In_File,
                                             Options,
                                             Converter,
                                             Doc_Directory,
                                             Doc_Suffix,
                                             Level);
                     Display_Private := True;
                  end if;

                  --  Check if the subtitle has to be set.
                  if not First_Already_Set then
                     Process_Header_Entries (B,
                                             Kernel,
                                             Doc_File,
                                             List_Ref_In_File,
                                             Options,
                                             Converter,
                                             Doc_Directory,
                                             Doc_Suffix,
                                             Package_Name,
                                             Level);
                     First_Already_Set := True;
                  end if;

                  Description := Extract_Comment
                    (File_Text.all,
                     Get_Declaration_Line_Of (TEL.Data (Entity_Node).Entity),
                     Count_Lines (Header.all),
                     False,
                     Options);

                  Data_Entry := Doc_Info'
                    (Entry_Info,
                     Doc_Info_Options  => Options,
                     Doc_LI_Unit       => LI_Unit,
                     Doc_File_List     => Source_File_List,
                     Entry_Entity      => TEL.Data (Entity_Node),
                     Entry_Link        => Process_Body_File,
                     Entry_Header      => Header,
                     Entry_Header_Line => Get_Declaration_Line_Of
                       (TEL.Data (Entity_Node).Entity));
                  Converter (B,
                             Kernel,
                             Doc_File,
                             List_Ref_In_File,
                             Data_Entry,
                             Doc_Directory,
                             Doc_Suffix,
                             Level);
                  if Description.all /= "" then
                     Process_Description
                       (B,
                        Kernel,
                        Doc_File,
                        List_Ref_In_File,
                        Options,
                        Converter,
                        Doc_Directory,
                        Doc_Suffix,
                        Level,
                        Description.all);
                  end if;
                  Delete_Node := True;
                  Free (Header);
                  Free (Description);
               end if;
            end if;

            if Delete_Node then
               Type_Entity_List.Remove_Nodes
                 (Entity_List,
                  Entity_Node_Prec,
                  Entity_Node);

               if Entity_Node_Prec = TEL.Null_Node then
                  Entity_Node := TEL.First (Entity_List);
               else
                  Entity_Node := TEL.Next (Entity_Node_Prec);
               end if;
            else
               Entity_Node_Prec := Entity_Node;
               Entity_Node := TEL.Next (Entity_Node);
            end if;
         end loop;
      end if;
   end Process_Entries;

   --------------------------------
   -- Process_Header_Subprograms --
   --------------------------------

   procedure Process_Header_Subprograms
     (B                 : Backend_Handle;
      Kernel            : access Kernel_Handle_Record'Class;
      Doc_File          : File_Type;
      List_Ref_In_File  : in out List_Reference_In_File.List;
      Options           : All_Options;
      Converter         : Doc_Subprogram_Type;
      Doc_Directory     : String;
      Doc_Suffix        : String;
      Package_Name      : String;
      Level             : in out Natural)
   is
      Data_Subtitle     : Doc_Info (Info_Type => Subtitle_Info);
   begin
      Data_Subtitle := Doc_Info'
        (Subtitle_Info,
         Doc_Info_Options => Options,
         Doc_LI_Unit      => No_LI_File,
         Doc_File_List    => TSFL.Null_List,
         Subtitle_Name    => new String'("Subprograms"),
         Subtitle_Kind    => Subprogram_Info,
         Subtitle_Package => new String'(Package_Name));
      Converter
        (B,
         Kernel,
         Doc_File,
         List_Ref_In_File,
         Data_Subtitle,
         Doc_Directory,
         Doc_Suffix,
         Level);
      Free (Data_Subtitle.Subtitle_Name);
      Free (Data_Subtitle.Subtitle_Package);
   end Process_Header_Subprograms;

   ------------------------
   -- Process_References --
   ------------------------

   procedure Process_References
     (B                 : Backend_Handle;
      Kernel            : access Kernel_Handle_Record'Class;
      Doc_File          : File_Type;
      Source_File_List  : in out Type_Source_File_List.List;
      List_Ref_In_File  : in out List_Reference_In_File.List;
      Options           : All_Options;
      Converter         : Doc_Subprogram_Type;
      Doc_Directory     : String;
      Doc_Suffix        : String;
      Level             : in out Natural;
      Entity_Info       : Entity_List_Information)
   is
      Data              : Doc_Info (Info_Type => References_Info);
   begin
      Data := Doc_Info'
        (References_Info,
         Doc_Info_Options            => Options,
         Doc_LI_Unit                 => No_LI_File,
         Doc_File_List               => TSFL.Null_List,
         References_Entity           => Entity_Info,
         References_Source_File_List => Source_File_List,
         References_Directory        => new String'(Doc_Directory),
         References_Suffix           => new String'(Doc_Suffix));
      Converter
        (B,
         Kernel,
         Doc_File,
         List_Ref_In_File,
         Data,
         Doc_Directory, Doc_Suffix,
         Level);
   end Process_References;

   -------------------------
   -- Process_Subprograms --
   -------------------------

   procedure Process_Subprograms
     (B                  : Backend_Handle;
      Kernel             : access Kernel_Handle_Record'Class;
      Doc_File           : File_Type;
      Parsed_List        : in out Construct_List;
      Entity_List        : in out Type_Entity_List.List;
      List_Ref_In_File   : in out List_Reference_In_File.List;
      Source_Filename    : VFS.Virtual_File;
      Process_Body_File  : Boolean;
      Package_Name       : String;
      Package_Info       : Entity_Information;
      File_Text          : GNAT.OS_Lib.String_Access;
      LI_Unit            : LI_File_Ptr;
      Source_File_List   : in out Type_Source_File_List.List;
      Options            : All_Options;
      Converter          : Doc_Subprogram_Type;
      Doc_Directory      : String;
      Doc_Suffix         : String;
      Private_Entity     : Boolean;
      Display_Private    : in out Boolean;
      Level              : in out Natural;
      Tree_Package       : Scope_Tree)
   is
      use TEL;
      Entity_Node       : Type_Entity_List.List_Node;
      Entity_Node_Prec  : Type_Entity_List.List_Node;
      Description       : GNAT.OS_Lib.String_Access;
      Header            : GNAT.OS_Lib.String_Access;
      Header_Temp       : GNAT.OS_Lib.String_Access;
      Data_Subprogram   : Doc_Info (Info_Type => Subprogram_Info);
      First_Already_Set : Boolean;
      Delete_Node       : Boolean;
   begin
      if not TEL.Is_Empty (Entity_List) then
         First_Already_Set := False;
         Entity_Node := TEL.First (Entity_List);
         Entity_Node_Prec := TEL.Null_Node;

         while Entity_Node /= TEL.Null_Node loop
            Delete_Node       := False;

            if
            --  Private and public entities are processed separately
              TEL.Data (Entity_Node).Is_Private = Private_Entity
              and then
            --  Check if the entity is a procedure or a function
              TEL.Data (Entity_Node).Kind = Subprogram_Entity
            --  Check if defined in this file (the rest of
            --  entities only for the body documentation)
              and then Get_Declaration_File_Of
                (TEL.Data (Entity_Node).Entity) = Source_Filename
              and then
            --  The entity is defined in the current package
              Entity_Defined_In_Package (TEL.Data (Entity_Node).Entity,
                                         Package_Info,
                                         Tree_Package)
            then
               Header_Temp := Get_Whole_Header
                 (File_Text.all,
                  Parsed_List,
                  Get_Name (TEL.Data (Entity_Node).Entity),
                  Get_Declaration_Line_Of (TEL.Data (Entity_Node).Entity));

               --  Check if it was a entity with its own header
               if Header_Temp /= null then
                  --  Indent spaces are deleted
                  Header := Remove_Indent (Header_Temp,
                                           Level * Get_Indent (B.all));
                  Free (Header_Temp);

                  if TEL.Data (Entity_Node).Is_Private and then
                    not Display_Private
                  then
                     --  It's the first time we met a private entity
                     --  and we work on the private part, so we put the
                     --  title "Private"
                     Process_Header_Private (B,
                                             Kernel,
                                             Doc_File,
                                             List_Ref_In_File,
                                             Options,
                                             Converter,
                                             Doc_Directory,
                                             Doc_Suffix,
                                             Level);
                     Display_Private := True;
                  end if;

                  --  Check if the subtitle "Subprograms:" has to be set.
                  if not First_Already_Set then
                     Process_Header_Subprograms (B,
                                                 Kernel,
                                                 Doc_File,
                                                 List_Ref_In_File,
                                                 Options,
                                                 Converter,
                                                 Doc_Directory,
                                                 Doc_Suffix,
                                                 Package_Name,
                                                 Level);
                     First_Already_Set := True;
                  end if;

                  Description := Extract_Comment
                    (File_Text.all,
                     Get_Declaration_Line_Of (TEL.Data (Entity_Node).Entity),
                     Count_Lines (Header.all),
                     False,
                     Options);

                  Data_Subprogram := Doc_Info'
                    (Subprogram_Info,
                     Doc_Info_Options       => Options,
                     Doc_LI_Unit            => LI_Unit,
                     Doc_File_List          => Source_File_List,
                     Subprogram_Entity      => TEL.Data (Entity_Node),
                     Subprogram_Link        => Process_Body_File,
                     Subprogram_List        => Entity_List,
                     Subprogram_Header      => Header,
                     Subprogram_Header_Line =>
                       Get_Declaration_Line_Of
                         (TEL.Data (Entity_Node).Entity));

                  Converter (B,
                             Kernel,
                             Doc_File,
                             List_Ref_In_File,
                             Data_Subprogram,
                             Doc_Directory,
                             Doc_Suffix,
                             Level);
                  if Description.all /= "" then

                     Process_Description
                       (B,
                        Kernel,
                        Doc_File,
                        List_Ref_In_File,
                        Options,
                        Converter,
                        Doc_Directory,
                        Doc_Suffix,
                        Level,
                        Description.all);
                  end if;

                  if Options.References then
                     --  Callgraph is processed
                     Process_References
                       (B,
                        Kernel,
                        Doc_File,
                        Source_File_List,
                        List_Ref_In_File,
                        Options,
                        Converter,
                        Doc_Directory,
                        Doc_Suffix,
                        Level,
                        TEL.Data (Entity_Node));
                  end if;

                  Delete_Node := True;
                  Free (Header);
                  Free (Description);
               end if;
            end if;

            if Delete_Node then

               Type_Entity_List.Remove_Nodes
                 (Entity_List,
                  Entity_Node_Prec,
                  Entity_Node);

               if Entity_Node_Prec = TEL.Null_Node then
                  Entity_Node := TEL.First (Entity_List);
               else
                  Entity_Node := TEL.Next (Entity_Node_Prec);
               end if;
            else
               Entity_Node_Prec := Entity_Node;
               Entity_Node := TEL.Next (Entity_Node);
            end if;
         end loop;
      end if;
   end Process_Subprograms;

   -------------------------------
   -- Entity_Defined_In_Package --
   -------------------------------

   function Entity_Defined_In_Package
     (Entity_Info       : Entity_Information;
      Package_Container : Entity_Information;
      Tree_Package      : Scope_Tree) return Boolean
   is
      Temp_Entity : Entity_Information;
      Temp_Node   : Scope_Tree_Node;
      Temp        : Scope_Tree_Node;
   begin
      --  Research of Entity_Info in the scope tree
      Temp := Find_Entity_Scope (Tree_Package, Entity_Info);
      if Temp /= Null_Scope_Tree_Node then
         --  We get the entity in wich Entity_Info is declared
         Temp_Node := Get_Parent (Temp);
         if Temp_Node /= Null_Scope_Tree_Node then
            Temp_Entity := Get_Entity (Temp_Node);
            if Temp_Entity /= No_Entity_Information then
               if Is_Equal (Temp_Entity, Package_Container) then
                  --  Entity_Info is declared in the given package
                  Free (Temp_Entity);
                  return True;
               end if;
               Free (Temp_Entity);
            end if;
         end if;
      end if;
      return False;
   end Entity_Defined_In_Package;

   ----------------------------
   -- Package_Contain_Entity --
   ----------------------------

   function Package_Contain_Entity
     (Package_Entity : Entity_Information;
      Entity_List    : Type_Entity_List.List;
      Tree_Package   : Scope_Tree) return Boolean
   is
      use TEL;
      Entity_Node : Type_Entity_List.List_Node;
   begin
      if not TEL.Is_Empty (Entity_List) then
         Entity_Node := TEL.First (Entity_List);
         while Entity_Node /= TEL.Null_Node loop
            if Entity_Defined_In_Package (TEL.Data (Entity_Node).Entity,
                                          Package_Entity,
                                          Tree_Package)
            then
               --  The given package contains at least one declaration of an
               --  element of Entity_List
               return True;
            end if;
            Entity_Node := TEL.Next (Entity_Node);
         end loop;
      end if;
      return False;
   end Package_Contain_Entity;

   ---------------------
   -- Line_Is_Comment --
   ---------------------

   function Line_Is_Comment (Line : String) return Boolean is
   begin
      --  ??? Should be language insensitive
      for J in Line'First .. Line'Last - 1 loop
         if Line (J) = '-' and Line (J + 1) = '-' then
            return True;
         elsif Line (J) /= ' '
           and Line (J) /= ASCII.HT
           and Line (J) /= ASCII.LF
           and Line (J) /= ASCII.CR
         then
            return False;
         end if;
      end loop;

      return False;
   end Line_Is_Comment;

   -------------------
   -- Line_Is_Empty --
   -------------------

   function Line_Is_Empty (Line : String) return Boolean is
   begin
      for J in Line'First .. Line'Last loop
         if    Line (J) /= ' '
           and Line (J) /= ASCII.HT
           and Line (J) /= ASCII.LF
           and Line (J) /= ASCII.CR
         then
            return False;
         end if;
      end loop;

      return True;
   end Line_Is_Empty;

   --------------------------
   -- Is_Ignorable_Comment --
   --------------------------

   function Is_Ignorable_Comment (Comment_Line : String) return Boolean is
   begin
      --  ??? Should be language-insensitive
      for J in Comment_Line'First .. Comment_Line'Last - 2 loop
         if Comment_Line (J .. J + 1) = "--" then
            return Comment_Line (J + 2) = '!';
         end if;
      end loop;

      return False;
   end Is_Ignorable_Comment;

   -----------------
   -- Kill_Prefix --
   -----------------

   function Kill_Prefix (Comment_Line : String) return String is
      J : Natural := Comment_Line'First;
   begin
      --  ??? Should be language insensitive
      while Comment_Line (J) /= '-' and then Comment_Line (J + 1) /= '-' loop
         J := J + 1;
      end loop;

      return Comment_Line (J + 3 .. Comment_Line'Last);
   end Kill_Prefix;

   ---------------------
   -- Extract_Comment --
   ---------------------

   function Extract_Comment
     (File_Text           : String;
      Line                : Natural;
      Header_Lines        : Natural;
      Package_Description : Boolean;
      Options             : All_Options) return GNAT.OS_Lib.String_Access
   is
      New_Line, Old_Line  : GNAT.OS_Lib.String_Access;
      Result_Line         : GNAT.OS_Lib.String_Access;
      Temp                : GNAT.OS_Lib.String_Access;
      J                   : Natural;
   begin
      Result_Line := new String'("");

      --  The comments are under the header of the entity

      if (not Options.Comments_Above) or else Package_Description then
         J := Line + Header_Lines;
      else
         --  The comments are above the header of the entity
         J := Line - 1;
      end if;

      New_Line := new String'
        (Get_Line_From_String (File_Text, J));

      while Line_Is_Comment (New_Line.all) loop
         if (not Options.Comments_Above) or Package_Description then
            J := J + 1;

            if not (Options.Ignorable_Comments and then
                      Is_Ignorable_Comment (New_Line.all)) then
               if Package_Description then
                  Result_Line := new String'(Result_Line.all & New_Line.all);
               else
                  Temp := Remove_Space (Kill_Prefix (New_Line.all));
                  Result_Line := new String'(Result_Line.all
                                             & Temp.all);
               end if;
            end if;
         else
            J := J - 1;

            if not (Options.Ignorable_Comments and then
                      Is_Ignorable_Comment (New_Line.all)) then
               if Package_Description then
                  Result_Line := new String'(New_Line.all &
                    Result_Line.all);
               else
                  Temp := Remove_Space (Kill_Prefix (New_Line.all));
                  Result_Line := new String'(Temp.all
                                             & Result_Line.all);
                  Free (Temp);
               end if;
            end if;
         end if;

         Old_Line := New_Line;
         New_Line := new String'(Get_Line_From_String (File_Text, J));
         Free (Old_Line);
      end loop;

      Free (New_Line);
      return Result_Line;
   end Extract_Comment;

   ------------------
   -- Remove_Space --
   ------------------

   function Remove_Space (Text : String)
                          return GNAT.OS_Lib.String_Access
   is
      Result_Line : GNAT.OS_Lib.String_Access;
      I           : Natural;
   begin
      I := Text'First;
      while I <= Text'Last and then Text (I) = ' ' loop
         I := I + 1;
      end loop;
      if I <= Text'Last then
         Result_Line := new String'(Text (I .. Text'Last));
         return Result_Line;
      else
         return new String'("");
      end if;
   end Remove_Space;

   --------------------------
   -- Get_Line_From_String --
   --------------------------

   function Get_Line_From_String
     (Text    : String;
      Line_Nr : Natural) return String
   is
      Lines, Index_Start, Index_End : Natural;
   begin
      Lines       := 1;
      Index_Start := 1;

      if Line_Nr > 1 then
         while Index_Start < Text'Length and Lines < Line_Nr loop
            if Text (Index_Start) = ASCII.LF then
               Lines := Lines + 1;
            end if;

            Index_Start := Index_Start + 1;
         end loop;
      end if;

      Index_End := Index_Start;

      while Index_End < Text'Length and then Text (Index_End) /=  ASCII.LF loop
         Index_End := Index_End + 1;
      end loop;

      return Text (Index_Start .. Index_End);
   end Get_Line_From_String;

   ------------------------
   --  Get_Whole_Header  --
   ------------------------

   function Get_Whole_Header
     (File_Text   : String;
      Parsed_List : Construct_List;
      Entity_Name : String;
      Entity_Line : Natural) return GNAT.OS_Lib.String_Access
   is
      use type Basic_Types.String_Access;
      Parse_Node : Construct_Access := Parsed_List.First;
      Header     : GNAT.OS_Lib.String_Access;
   begin
      while Parse_Node /= null loop
         if Parse_Node.Name /= null then

            if Parse_Node.Name.all (Parse_Node.Name.all'First) = '"'
              and then
                Parse_Node.Name.all (Parse_Node.Name.all'Last) = '"'
              and then
                Parse_Node.Name.all'First + 1 <=
                  Parse_Node.Name.all'Last - 1
            then
               --  This case happens when operators are overridden
               --  It's necessary to look after " in Parse_Node.Name.all
               --  because Parse_Construct returns the name with " (eg. "<")
               --  whereas functions which permit to build the list of entities
               --  return the name without " (eg. <).

               if To_Lower (Parse_Node.Name.all
                              (Parse_Node.Name.all'First + 1
                                 .. Parse_Node.Name.all'Last - 1))
                   = Entity_Name
                   and then Parse_Node.Sloc_Start.Line = Entity_Line
               then
                  Header := new String'(File_Text
                                          (Parse_Node.Sloc_Start.Index ..
                                             Parse_Node.Sloc_End.Index));
                  return Header;
               end if;
            else

               if To_Lower (Parse_Node.Name.all) = Entity_Name
                 and then Parse_Node.Sloc_Start.Line = Entity_Line
               then
                  Header := new String'(File_Text
                                          (Parse_Node.Sloc_Start.Index ..
                                             Parse_Node.Sloc_End.Index));
                  return Header;
               end if;
            end if;
         end if;
         Parse_Node := Parse_Node.Next;
      end loop;
      return null;
   end Get_Whole_Header;

   -------------------
   -- Remove_Indent --
   -------------------

   function Remove_Indent
     (Text  : GNAT.OS_Lib.String_Access;
      Space : Natural) return GNAT.OS_Lib.String_Access
   is
      Temp     : GNAT.OS_Lib.String_Access;
      Temp_Bis : GNAT.OS_Lib.String_Access;
      Old_I    : Natural;
      I        : Natural;
      Stop     : Boolean;
   begin
      if Text /= null then
         Old_I := Text.all'First;
         I     := Text.all'First;
         Temp_Bis := new String'("");
         while I <= Text.all'Last loop
            if Text.all (I) = ASCII.LF then
               Temp := new String'(Temp_Bis.all & Text.all (Old_I .. I));
               Free (Temp_Bis);
               Temp_Bis := new String'(Temp.all);
               Free (Temp);

               Stop := False;
               for J in I + 1 .. I + Space loop
                  if J <= Text.all'Last
                    and then
                      Text.all (J) /= ' '
                  then
                     --  If the indentation is bad
                     Old_I := J;
                     I := Old_I;
                     Stop := True;
                  end if;
                  exit when Stop;
               end loop;
               if not Stop then
                  Old_I := I + Space + 1;
                  I := Old_I;
               end if;
            else
               I := I + 1;
            end if;
         end loop;
         Temp := new String'(Temp_Bis.all & Text.all (Old_I .. I - 1));
         Free (Temp_Bis);
         Temp_Bis := new String'(Temp.all);
         Free (Temp);

         return Temp_Bis;
      else
         return null;
      end if;
   end Remove_Indent;

end Docgen.Work_On_Source;
