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

with Basic_Types;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Basic_Types;
with Language;                  use Language;
with Language_Handlers;         use Language_Handlers;
with Entities.Queries;          use Entities.Queries;
with VFS;                       use VFS;
with Glide_Kernel.Project;      use Glide_Kernel, Glide_Kernel.Project;
with Projects.Registry;         use Projects.Registry;
with Traces;                    use Traces;
with Ada.Strings.Unbounded;
with Ada.Exceptions;            use Ada.Exceptions;
with Projects;                  use Projects;
with String_Utils;              use String_Utils;
with Docgen.Backend;            use Docgen.Backend;
with Language;                  use Language;

package body Docgen.Work_On_Source is

   package TEL renames Type_Entity_List;

   procedure Process_Source_Spec
     (B                         : access Docgen.Backend.Backend'Class;
      Kernel                    : access Kernel_Handle_Record'Class;
      Doc_File                  : File_Descriptor;
      Source_File_List          : in out Type_Source_File_Table.HTable;
      Source_Filename           : VFS.Virtual_File;
      Package_Name              : String;
      Package_Info              : Entity_Information;
      Entity_List               : in out Type_Entity_List.List;
      List_Ref_In_File          : in out List_Reference_In_File.List;
      Tagged_Types_List         : in out List_Entity_Information.List;
      Private_Tagged_Types_List : in out List_Entity_Information.List;
      Options                   : All_Options;
      Level                     : in out Natural;
      File_Text                 : GNAT.OS_Lib.String_Access;
      Parsed_List               : in out Construct_List);
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
   --  Level            : the level of the current package. By default, the
   --  level of the package file is 1, then this level is increased by 1 at
   --  each inner package

   procedure Process_One_Body_File
     (B                : access Docgen.Backend.Backend'Class;
      Kernel           : access Kernel_Handle_Record'Class;
      Doc_File         : File_Descriptor;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Source_File      : Virtual_File;
      File_Text        : GNAT.OS_Lib.String_Access;
      Source_File_List : in out Type_Source_File_Table.HTable;
      Options          : All_Options;
      Level            : in out Natural);
   --  Will pass the information about the body file to the output
   --  subprogram. This is the only subprogram working on the contents
   --  of the body source files.

   procedure Process_Open_File
     (B                : access Docgen.Backend.Backend'Class;
      Kernel           : access Kernel_Handle_Record'Class;
      Doc_File         : File_Descriptor;
      Package_Name     : String);
   --  Is always the first subprogram to be called, as it creates the
   --  very beginning of the documentation by calling the output
   --  subprogram

   procedure Process_Package_Description
     (B                : access Docgen.Backend.Backend'Class;
      Kernel           : access Kernel_Handle_Record'Class;
      Doc_File         : File_Descriptor;
      Text             : String;
      Options          : All_Options;
      Language         : Language_Access;
      Level            : in out Natural);
   --  Extracts all the comment lines of the source file which are at the
   --  beginning of it. Empty lines are ignored, the procedure stops when
   --  first command is found. This information will be passed to the
   --  output subprogram

   procedure Process_With_Clause
     (B                : access Docgen.Backend.Backend'Class;
      Kernel           : access Kernel_Handle_Record'Class;
      Doc_File         : File_Descriptor;
      Parsed_List      : in out Construct_List;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Source_Filename  : VFS.Virtual_File;
      File_Text        : GNAT.OS_Lib.String_Access;
      Source_File_List : in out Type_Source_File_Table.HTable;
      Options          : All_Options;
      Level            : in out Natural);
   --  Will process the lines at the beginning of the source file
   --  starting with "with" and pass them to the output subprogram

   procedure Process_Description
     (B                 : access Docgen.Backend.Backend'Class;
      Kernel            : access Kernel_Handle_Record'Class;
      Doc_File          : File_Descriptor;
      Level             : in out Natural;
      Comment           : String);
   --  Processes comments after the source code (or before: it depends on
   --  preferences)

   procedure Process_Packages
     (B                         : access Docgen.Backend.Backend'Class;
      Kernel                    : access Kernel_Handle_Record'Class;
      Doc_File                  : File_Descriptor;
      Parsed_List               : in out Construct_List;
      Entity_List               : in out Type_Entity_List.List;
      List_Ref_In_File          : in out List_Reference_In_File.List;
      Tagged_Types_List         : in out List_Entity_Information.List;
      Private_Tagged_Types_List : in out List_Entity_Information.List;
      Source_Filename           : VFS.Virtual_File;
      Package_Name              : String;
      Package_Information       : Entity_Information;
      File_Text                 : GNAT.OS_Lib.String_Access;
      Source_File_List          : in out Type_Source_File_Table.HTable;
      Options                   : All_Options;
      Private_Entity            : Boolean;
      Display_Private           : in out Boolean;
      Level                     : in out Natural);
   --  Will process renamed and instantiated packages and pass
   --  them to the output subprogram.
   --  Display_Private : indicate if we print the "Private" header.
   --  Private_Entity  : indicate if it must process private/public entities
   --  contained in Entity_List.

   procedure Process_Vars
     (B                : access Docgen.Backend.Backend'Class;
      Kernel           : access Kernel_Handle_Record'Class;
      Doc_File         : File_Descriptor;
      Parsed_List      : in out Construct_List;
      Entity_List      : in out Type_Entity_List.List;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Source_Filename  : VFS.Virtual_File;
      Package_Info     : Entity_Information;
      File_Text        : GNAT.OS_Lib.String_Access;
      Source_File_List : in out Type_Source_File_Table.HTable;
      Options          : All_Options;
      Private_Entity   : Boolean;
      Display_Private  : in out Boolean;
      Level            : in out Natural);
   --  Called by Process_Source to work on the constants
   --  and named numbers and pass each of them to the output subprogram
   --  Display_Private : indicate if we print the "Private" header.
   --  Private_Entity  : indicate if it must process private/public entities
   --  contained in Entity_List.

   procedure Process_Exceptions
     (B                : access Docgen.Backend.Backend'Class;
      Kernel           : access Kernel_Handle_Record'Class;
      Doc_File         : File_Descriptor;
      Parsed_List      : in out Construct_List;
      Entity_List      : in out Type_Entity_List.List;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Source_Filename  : VFS.Virtual_File;
      Package_Info     : Entity_Information;
      File_Text        : GNAT.OS_Lib.String_Access;
      Source_File_List : in out Type_Source_File_Table.HTable;
      Options          : All_Options;
      Private_Entity   : Boolean;
      Display_Private  : in out Boolean;
      Level            : in out Natural);
   --  Called by Process_Source to work on the exceptions and
   --  pass each of them to the output subprogram
   --  Display_Private : indicate if we print the "Private" header.
   --  Private_Entity  : indicate if it must process private/public entities
   --  contained in Entity_List.

   procedure Process_Entries
     (B                 : access Docgen.Backend.Backend'Class;
      Kernel            : access Kernel_Handle_Record'Class;
      Doc_File          : File_Descriptor;
      Parsed_List       : in out Construct_List;
      Entity_List       : in out Type_Entity_List.List;
      List_Ref_In_File  : in out List_Reference_In_File.List;
      Source_Filename   : VFS.Virtual_File;
      Package_Info      : Entity_Information;
      File_Text         : GNAT.OS_Lib.String_Access;
      Source_File_List  : in out Type_Source_File_Table.HTable;
      Options           : All_Options;
      Private_Entity    : Boolean;
      Display_Private   : in out Boolean;
      Level             : in out Natural);
   --  Called by Process_Source to work on the entires and entry
   --  families and pass each of them to the output subprogram
   --  Display_Private : indicate if we print the "Private" header.
   --  Private_Entity  : indicate if it must process private/public entities
   --  contained in Entity_List.

   procedure Process_Calls_References
     (B                 : access Docgen.Backend.Backend'Class;
      Kernel            : access Kernel_Handle_Record'Class;
      Options           : All_Options;
      Doc_File          : File_Descriptor;
      Info              : Entity_Information;
      Source_File_List  : Type_Source_File_Table.HTable;
      Level             : in out Natural);
   procedure Process_Caller_References
     (B                 : access Docgen.Backend.Backend'Class;
      Kernel            : access Kernel_Handle_Record'Class;
      Options           : All_Options;
      Doc_File          : File_Descriptor;
      Info              : Entity_Information;
      Source_File_List  : Type_Source_File_Table.HTable;
      Level             : in out Natural);
   --  For one subprogram: processes the output of the callgraph when this
   --  option is choosen.

   procedure Process_Subprograms
     (B                  : access Docgen.Backend.Backend'Class;
      Kernel             : access Kernel_Handle_Record'Class;
      Doc_File           : File_Descriptor;
      Parsed_List        : in out Construct_List;
      Entity_List        : in out Type_Entity_List.List;
      List_Ref_In_File   : in out List_Reference_In_File.List;
      Source_Filename    : VFS.Virtual_File;
      Package_Info       : Entity_Information;
      File_Text          : GNAT.OS_Lib.String_Access;
      Source_File_List   : in out Type_Source_File_Table.HTable;
      Options            : All_Options;
      Private_Entity     : Boolean;
      Display_Private    : in out Boolean;
      Level              : in out Natural);
   --  Called by Process_Source to work on the subprograms and
   --  pass each of them to the output subprogram
   --  Private_Entity indicates if it must process private or public entites
   --  Display_Private is used to indicate if it's necessary to print the
   --  "Private" header

   procedure Process_One_Family
     (B                : access Docgen.Backend.Backend'Class;
      Kernel           : access Kernel_Handle_Record'Class;
      Doc_File         : File_Descriptor;
      Family           : List_Entity_Information.List;
      Source_File_List : in out Type_Source_File_Table.HTable;
      Entity           : Entity_Information;
      Level            : in out Natural);
   --  For one tagged type: indicates its parents and children.

   procedure Process_Types
     (B                         : access Docgen.Backend.Backend'Class;
      Kernel                    : access Kernel_Handle_Record'Class;
      Doc_File                  : File_Descriptor;
      Parsed_List               : in out Construct_List;
      Entity_List               : in out Type_Entity_List.List;
      List_Ref_In_File          : in out List_Reference_In_File.List;
      Tagged_Types_List         : in out List_Entity_Information.List;
      Private_Tagged_Types_List : in out List_Entity_Information.List;
      Source_Filename           : VFS.Virtual_File;
      Package_Info              : Entity_Information;
      File_Text                 : GNAT.OS_Lib.String_Access;
      Source_File_List          : in out Type_Source_File_Table.HTable;
      Options                   : All_Options;
      Private_Entity            : Boolean;
      Display_Private           : in out Boolean;
      Level                     : in out Natural);
   --  Called by Process_Source to work on the types and
   --  pass each of them to the output subprogram
   --  Display_Private : indicate if we print the "Private" header.
   --  Private_Entity  : indicate if it must process private/public entities
   --  contained in Entity_List.

   procedure Process_Header
     (B                 : access Docgen.Backend.Backend'Class;
      Kernel            : access Kernel_Handle_Record'Class;
      Doc_File          : File_Descriptor;
      Entity_List       : in out Type_Entity_List.List;
      Source_Filename   : VFS.Virtual_File;
      Package_Name      : String;
      Package_File      : VFS.Virtual_File;
      Options           : All_Options);
   --  Will call the output subprogram to create the header of
   --  the package. This is NOT the same as Process_Open_File,
   --  if TexInfo doc is created, the file is opened only once,
   --  but the Header has to be set in front of each package.

   procedure Process_Header_Private
     (B                 : access Docgen.Backend.Backend'Class;
      Kernel            : access Kernel_Handle_Record'Class;
      Doc_File          : File_Descriptor;
      Level             : in out Natural);
   --  Adds title "Private: " when private entities are required.

   procedure Process_Header_Packages
     (B                 : access Docgen.Backend.Backend'Class;
      Kernel            : access Kernel_Handle_Record'Class;
      Doc_File          : File_Descriptor;
      Level             : in out Natural);
   --  Adds title "Packages".

   procedure Process_Header_Vars
     (B                 : access Docgen.Backend.Backend'Class;
      Kernel            : access Kernel_Handle_Record'Class;
      Doc_File          : File_Descriptor;
      Level             : in out Natural);
   --  Adds title "Constants and Named Numbers"

   procedure Process_Header_Types
     (B                 : access Docgen.Backend.Backend'Class;
      Kernel            : access Kernel_Handle_Record'Class;
      Doc_File          : File_Descriptor;
      Level             : in out Natural);
   --  Adds title "Types"

   procedure Process_Header_Entries
     (B                 : access Docgen.Backend.Backend'Class;
      Kernel            : access Kernel_Handle_Record'Class;
      Doc_File          : File_Descriptor;
      Level             : in out Natural);
   --  Adds title "Entries"

   procedure Process_Header_Subprograms
     (B                 : access Docgen.Backend.Backend'Class;
      Kernel            : access Kernel_Handle_Record'Class;
      Doc_File          : File_Descriptor;
      Level             : in out Natural);
   --  Adds title "Subprograms"

   procedure Process_Header_Exceptions
     (B                 : access Docgen.Backend.Backend'Class;
      Kernel            : access Kernel_Handle_Record'Class;
      Doc_File          : File_Descriptor;
      Level             : in out Natural);
   --  Adds title "Exceptions"

   procedure Process_Footer
     (B                : access Docgen.Backend.Backend'Class;
      Kernel           : access Kernel_Handle_Record'Class;
      Doc_File         : File_Descriptor);
   --  Will call the output subprogram to create the footer of
   --  the package. This is NOT the same as Doc_Close
   --  if TexInfo doc is created, the file is closed only once,
   --  but the Footer has to be set behind each package.

   function Entity_Defined_In_Package
     (Entity_Info       : Entity_Information;
      Package_Container : Entity_Information) return Boolean;
   --  Determines if an entity is defined in a package.

   function Package_Contain_Entity
     (Package_Entity : Entity_Information;
      Entity_List    : Type_Entity_List.List) return Boolean;
   --  Determines if there's entities defined in a the package

--     function Is_Ignorable_Comment
--       (Comment_Line : String) return Boolean;
   --  Return true, if the comment line starts with a "--!"
   --  It must be sure, that Comment_List is a comment line!

   procedure Get_Whole_Header
     (File_Text    : String;
      Parsed_List  : Construct_List;
      Entity_Name  : String;
      Entity_Line  : Natural;
      Header_Start : out Natural;
      Header_End   : out Natural);
   --  Return the Header of the entity. If no header was found, the empty
   --  string will be returned.
   --  The header is File_Text (Header_Start .. Header_End)

   procedure Remove_Indent
     (Text       : String;
      Space      : Natural;
      Clean_Text : out GNAT.OS_Lib.String_Access;
      Line_Count : out Natural);
   --  The header returned by Get_Whole_Header contains indent spaces (except
   --  those of the first line). This function removes those spaces.

   --------------------
   -- Process_Source --
   --------------------

   procedure Process_Source
     (B                         : access Docgen.Backend.Backend'Class;
      Kernel                    : access
        Glide_Kernel.Kernel_Handle_Record'Class;
      Doc_File                  : File_Descriptor;
      Source_File_List          : in out Type_Source_File_Table.HTable;
      Source_Filename           : VFS.Virtual_File;
      Source_Is_Spec            : Boolean;
      Package_Name              : String;
      Entity_List               : in out Type_Entity_List.List;
      List_Ref_In_File          : in out List_Reference_In_File.List;
      Tagged_Types_List         : in out List_Entity_Information.List;
      Private_Tagged_Types_List : in out List_Entity_Information.List;
      Options                   : All_Options;
      Level                     : in out Natural)
   is
      use TEL;
      use type Basic_Types.String_Access;
      File_Text          : GNAT.OS_Lib.String_Access;
      Entity_Node        : Type_Entity_List.List_Node;
      Found_Main_Package : Boolean;
      Parsed_List        : Construct_List;
      Entity             : TEL.Data_Access;

   begin
      File_Text := Read_File (Source_Filename);
      --  All the file is stored in a string

      if File_Text = null then
         --  This is a non existing file
         return;
      end if;

      Process_Open_File (B, Kernel, Doc_File, Package_Name);
      Process_Header
        (B,
         Kernel,
         Doc_File,
         Entity_List,
         Source_Filename,
         Package_Name,
         Source_Filename,
         Options);

      --  Different processing for spec and body files

      if Source_Is_Spec then
         if not TEL.Is_Empty (Entity_List) then
            --  Parse the source file and create the Parsed_List

            Parse_Constructs
              (Get_Language_From_File
                 (Get_Language_Handler (Kernel), Source_Filename),
               File_Text.all,
               Parsed_List);

            --  Find the the main package entity

            Found_Main_Package := False;
            Entity_Node := TEL.First (Entity_List);

            while Entity_Node /= TEL.Null_Node loop
               Entity := TEL.Data_Ref (Entity_Node);

               if Entity.Kind = Package_Entity
                 and then Get_Full_Name (Entity.Entity) = Package_Name
               then
                  Found_Main_Package := True;
                  exit;
               end if;

               Entity_Node := TEL.Next (Entity_Node);
            end loop;

            if Found_Main_Package then
               Process_Package_Description
                 (B, Kernel, Doc_File,
                  File_Text.all, Options,
                  Get_Language_From_File
                    (Get_Language_Handler (Kernel), Source_Filename),
                  Level);
               Process_With_Clause
                 (B,
                  Kernel,
                  Doc_File,
                  Parsed_List,
                  List_Ref_In_File,
                  Source_Filename,
                  File_Text,
                  Source_File_List,
                  Options,
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
                  Entity.Entity,
                  Entity_List,
                  List_Ref_In_File,
                  Tagged_Types_List,
                  Private_Tagged_Types_List,
                  Options,
                  Level,
                  File_Text,
                  Parsed_List);
            end if;

            Free (Parsed_List);
         end if;
      else
         Process_One_Body_File
           (B,
            Kernel,
            Doc_File,
            List_Ref_In_File,
            Source_Filename,
            File_Text,
            Source_File_List,
            Options, Level);
      end if;

      Process_Footer (B, Kernel, Doc_File);
      Doc_Close (B, Kernel, Doc_File);
      Free (File_Text);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Process_Source;

   -------------------------
   -- Process_Source_Spec --
   -------------------------

   procedure Process_Source_Spec
     (B                         : access Docgen.Backend.Backend'Class;
      Kernel                    : access Kernel_Handle_Record'Class;
      Doc_File                  : File_Descriptor;
      Source_File_List          : in out Type_Source_File_Table.HTable;
      Source_Filename           : VFS.Virtual_File;
      Package_Name              : String;
      Package_Info              : Entity_Information;
      Entity_List               : in out Type_Entity_List.List;
      List_Ref_In_File          : in out List_Reference_In_File.List;
      Tagged_Types_List         : in out List_Entity_Information.List;
      Private_Tagged_Types_List : in out List_Entity_Information.List;
      Options                   : All_Options;
      Level                     : in out Natural;
      File_Text                 : GNAT.OS_Lib.String_Access;
      Parsed_List               : in out Construct_List)
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
         Source_File_List,
         Options,
         False, Display_Private,
         Level);
      Process_Vars
        (B,
         Kernel,
         Doc_File,
         Parsed_List,
         Entity_List,
         List_Ref_In_File,
         Source_Filename,
         Package_Info,
         File_Text,
         Source_File_List,
         Options,
         False, Display_Private,
         Level);
      Process_Exceptions
        (B,
         Kernel,
         Doc_File,
         Parsed_List,
         Entity_List,
         List_Ref_In_File,
         Source_Filename,
         Package_Info,
         File_Text,
         Source_File_List,
         Options,
         False, Display_Private,
         Level);
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
         Package_Info,
         File_Text,
         Source_File_List,
         Options,
         False, Display_Private,
         Level);
      Process_Entries
        (B,
         Kernel,
         Doc_File,
         Parsed_List,
         Entity_List,
         List_Ref_In_File,
         Source_Filename,
         Package_Info,
         File_Text,
         Source_File_List,
         Options,
         False, Display_Private,
         Level);
      Process_Subprograms
        (B,
         Kernel,
         Doc_File,
         Parsed_List,
         Entity_List,
         List_Ref_In_File,
         Source_Filename,
         Package_Info,
         File_Text,
         Source_File_List,
         Options,
         False, Display_Private,
         Level);

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
            Source_File_List,
            Options,
            True, Display_Private,
            Level);
         Process_Vars
           (B,
            Kernel,
            Doc_File,
            Parsed_List,
            Entity_List,
            List_Ref_In_File,
            Source_Filename,
            Package_Info,
            File_Text,
            Source_File_List,
            Options,
            True, Display_Private,
            Level);
         Process_Exceptions
           (B,
            Kernel,
            Doc_File,
            Parsed_List,
            Entity_List,
            List_Ref_In_File,
            Source_Filename,
            Package_Info,
            File_Text,
            Source_File_List,
            Options,
            True, Display_Private,
            Level);
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
            Package_Info,
            File_Text,
            Source_File_List,
            Options,
            True, Display_Private,
            Level);
         Process_Entries
           (B,
            Kernel,
            Doc_File,
            Parsed_List,
            Entity_List,
            List_Ref_In_File,
            Source_Filename,
            Package_Info,
            File_Text,
            Source_File_List,
            Options,
            True, Display_Private,
            Level);
         Process_Subprograms
           (B,
            Kernel,
            Doc_File,
            Parsed_List,
            Entity_List,
            List_Ref_In_File,
            Source_Filename,
            Package_Info,
            File_Text,
            Source_File_List,
            Options,
            True, Display_Private,
            Level);
      end if;
   end Process_Source_Spec;

   -----------------------
   -- Process_Open_File --
   -----------------------

   procedure Process_Open_File
     (B            : access Docgen.Backend.Backend'Class;
      Kernel       : access Kernel_Handle_Record'Class;
      Doc_File     : File_Descriptor;
      Package_Name : String) is
   begin
      Doc_Open (B, Kernel, Doc_File, Open_Title => Package_Name);
   end Process_Open_File;

   ---------------------------
   -- Process_One_Body_File --
   ---------------------------

   procedure Process_One_Body_File
     (B                : access Docgen.Backend.Backend'Class;
      Kernel           : access Kernel_Handle_Record'Class;
      Doc_File         : File_Descriptor;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Source_File      : Virtual_File;
      File_Text        : GNAT.OS_Lib.String_Access;
      Source_File_List : in out Type_Source_File_Table.HTable;
      Options          : All_Options;
      Level            : in out Natural) is
   begin
      Doc_Body_Line (B, Kernel, Doc_File, List_Ref_In_File, Source_File_List,
                     Options, Level,
                     Body_Text => File_Text.all,
                     Body_File => Source_File);
   end Process_One_Body_File;

   ------------------------
   -- Process_Unit_Index --
   ------------------------

   procedure Process_Unit_Index
     (B                : access Docgen.Backend.Backend'Class;
      Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
      Source_File_List : Docgen.Type_Source_File_Table.HTable;
      Options          : Docgen.All_Options;
      Level            : in out Natural)
   is
      use Type_Source_File_Table;
      Source_File_Node : Type_Source_File_Table.Iterator;
      Index_File       : File_Descriptor;
      Doc_File_Name    : constant String := "index_unit";

      use Source_File_Arrays;

      Sources : Source_File_Arrays.Instance;
      Info    : Source_File_Information;

   begin
      Index_File := Create_File
        (Get_Doc_Directory (B, Kernel) & Doc_File_Name & Get_Extension (B),
         Binary);

      Get_First (Source_File_List, Source_File_Node);
      while Get_Element (Source_File_Node) /= No_Source_File_Information loop
         Append (Sources, Get_Key (Source_File_Node));
         Get_Next (Source_File_List, Source_File_Node);
      end loop;

      Entities.Sort (Sources, Unit_Name);

      if Length (Sources) /= 0 then
         Doc_Unit_Index
           (B, Kernel, Index_File,
            Source_File_List, Options, Level,
            Doc_Directory => Get_Doc_Directory (B, Kernel));
      end if;

      for S in Source_File_Arrays.First .. Last (Sources) loop
         Info := Get (Source_File_List, Sources.Table (S));

         if Info.Is_Spec then
            Doc_Index_Item (B, Kernel, Index_File,
                            Name      => Get_Unit_Name (Sources.Table (S)),
                            Item_File => Sources.Table (S),
                            Line      => First_File_Line,
                            Doc_File  => Info.Doc_File_Name.all);
         end if;
      end loop;

      Source_File_Arrays.Free (Sources);

      Doc_End_Of_Index (B, Kernel, Index_File);
      Close (Index_File);
   end Process_Unit_Index;

   ------------------------------
   -- Process_Subprogram_Index --
   ------------------------------

   procedure Process_Subprogram_Index
     (B                             : access Docgen.Backend.Backend'Class;
      Kernel                        : access Kernel_Handle_Record'Class;
      Subprogram_Index_List         : Type_Entity_List.List;
      Private_Subprogram_Index_List : in out Type_Entity_List.List;
      Source_File_List              : Type_Source_File_Table.HTable;
      Options                       : All_Options)
   is
      Index_File    : File_Descriptor;
      Doc_File_Name : constant String := "index_sub";

      procedure Process_List
        (List : Type_Entity_List.List; Public : Boolean);

      ------------------
      -- Process_List --
      ------------------

      procedure Process_List
        (List : Type_Entity_List.List; Public : Boolean)
      is
         use TEL;
         Source                : Source_File;
         Subprogram_Index_Node : Type_Entity_List.List_Node;
         Entity                : TEL.Data_Access;
         Info                  : Source_File_Information;
      begin
         if not TEL.Is_Empty (List) then
            if Options.Show_Private then
               if Public then
                  Doc_Public_Index (B, Kernel, Index_File, Title => "Public:");
               else
                  Doc_Private_Index
                    (B, Kernel, Index_File, Title => "Private:");
               end if;
            end if;

            Subprogram_Index_Node := TEL.First (List);

            while Subprogram_Index_Node /= TEL.Null_Node loop
               Entity := TEL.Data_Ref (Subprogram_Index_Node);
               Source := Get_File (Get_Declaration_Of (Entity.Entity));
               Info   := Type_Source_File_Table.Get (Source_File_List, Source);
               Doc_Index_Item
                 (B, Kernel, Index_File,
                  Name      => Get_Name (Entity.Entity).all,
                  Item_File => Source,
                  Line      => Get_Line (Get_Declaration_Of (Entity.Entity)),
                  Doc_File  => Info.Doc_File_Name.all);

               Subprogram_Index_Node := TEL.Next (Subprogram_Index_Node);
            end loop;
         end if;
      end Process_List;

   begin
      Index_File := Create_File
        (Get_Doc_Directory (B, Kernel) & Doc_File_Name & Get_Extension (B),
         Binary);

      Doc_Subprogram_Index (B, Kernel, Index_File, Options);

      Process_List (Subprogram_Index_List, Public => True);

      if Options.Show_Private then
         Process_List (Private_Subprogram_Index_List, Public => False);
      end if;

      Doc_End_Of_Index (B, Kernel, Index_File);
      Close (Index_File);
   end Process_Subprogram_Index;

   ------------------------
   -- Process_Type_Index --
   ------------------------

   procedure Process_Type_Index
     (B                       : access Docgen.Backend.Backend'Class;
      Kernel                  : access Kernel_Handle_Record'Class;
      Type_Index_List         : Docgen.Type_Entity_List.List;
      Private_Type_Index_List : in out Type_Entity_List.List;
      Source_File_List        : Type_Source_File_Table.HTable;
      Options                 : All_Options)
   is
      Index_File      : File_Descriptor;

      procedure Process_List
        (List : Type_Entity_List.List; Public : Boolean);

      ------------------
      -- Process_List --
      ------------------

      procedure Process_List
        (List : Type_Entity_List.List; Public : Boolean)
      is
         Type_Index_Node : Type_Entity_List.List_Node;
         Entity          : TEL.Data_Access;
         File            : Source_File;
         Info            : Source_File_Information;
         use TEL;
      begin
         if not TEL.Is_Empty (List) then
            if Options.Show_Private then
               if Public then
                  Doc_Public_Index (B, Kernel, Index_File, Title => "Public:");
               else
                  Doc_Private_Index (B, Kernel, Index_File, "Private:");
               end if;
            end if;
         end if;

         --  Public types are printed
         Type_Index_Node := TEL.First (List);

         while Type_Index_Node /= TEL.Null_Node loop
            Entity := TEL.Data_Ref (Type_Index_Node);
            File := Get_File (Get_Declaration_Of (Entity.Entity));
            Info := Type_Source_File_Table.Get (Source_File_List, File);
            Doc_Index_Item
              (B, Kernel, Index_File,
               Name => Get_Name (Entity.Entity).all,
               Item_File => File,
               Line      => Get_Line (Get_Declaration_Of (Entity.Entity)),
               Doc_File  => Info.Doc_File_Name.all);
            Type_Index_Node := TEL.Next (Type_Index_Node);
         end loop;
      end Process_List;

      Doc_File_Name   : constant String := "index_type";
   begin
      Index_File := Create_File
        (Get_Doc_Directory (B, Kernel) & Doc_File_Name & Get_Extension (B),
         Binary);
      Doc_Type_Index (B, Kernel, Index_File, Options);

      Process_List (Type_Index_List, Public => True);

      if Options.Show_Private then
         Process_List (Private_Type_Index_List, Public => False);
      end if;

      Doc_End_Of_Index (B, Kernel, Index_File);
      Close (Index_File);
   end Process_Type_Index;

   -------------------------------
   -- Process_Tagged_Type_Index --
   -------------------------------

   procedure Process_Tagged_Type_Index
     (B                         : access Docgen.Backend.Backend'Class;
      Kernel                    : access Kernel_Handle_Record'Class;
      Tagged_Type_Index_List    : Docgen.List_Entity_Information.List;
      Private_Tagged_Types_List : in out List_Entity_Information.List;
      Source_File_List          : in out Type_Source_File_Table.HTable;
      Options                   : All_Options)
   is
      Index_File : File_Descriptor;

      procedure Process_Parents  (Info : Entity_Information);
      procedure Process_Children (Info : Entity_Information);
      --  Output references to the parents and children of a tagged type

      procedure Process_Type (Entity : Entity_Information);
      --  Process a tagged type itself

      procedure Process_List
        (List : List_Entity_Information.List; Public : Boolean);
      --  Process a list of tagged types

      ---------------------
      -- Process_Parents --
      ---------------------

      procedure Process_Parents (Info : Entity_Information) is
         Parents   : constant Entity_Information_Array :=
                     Get_Parent_Types (Info);
      begin
         if Parents'Length /= 0 then
            for P in Parents'Range loop
               if Source_File_In_List
                 (Source_File_List,
                  Get_File (Get_Declaration_Of (Parents (P))))
               then
                  Doc_Index_Tagged_Type
                    (B, Kernel, Index_File, Source_File_List, Parents (P),
                     Parent_With_Link);

               else
                  Doc_Index_Tagged_Type
                    (B, Kernel, Index_File, Source_File_List, Parents (P),
                     Parent_Without_Link);
               end if;
            end loop;

         else
            Doc_Index_Tagged_Type
              (B, Kernel, Index_File, Source_File_List, null, No_Parent);
         end if;
      end Process_Parents;

      ----------------------
      -- Process_Children --
      ----------------------

      procedure Process_Children (Info : Entity_Information) is
         Child  : Child_Type_Iterator;
         Son    : Entity_Information;
      begin
         Get_Child_Types (Iter => Child, Entity => Info);
         if At_End (Child) then
            Doc_Index_Tagged_Type
              (B, Kernel, Index_File, Source_File_List, null, No_Child);
         end if;

         while not At_End (Child) loop
            Son := Get (Child);
            if Son /= null then
               if Source_File_In_List
                 (Source_File_List,
                  Get_File (Get_Declaration_Of (Son)))
               then
                  Doc_Index_Tagged_Type
                    (B, Kernel, Index_File, Source_File_List,
                     Son, Child_With_Link);
               else
                  Doc_Index_Tagged_Type
                    (B, Kernel, Index_File, Source_File_List,
                     Son, Child_Without_Link);
               end if;
            end if;
            Next (Child);
         end loop;
         Destroy (Child);
      end Process_Children;

      ------------------
      -- Process_Type --
      ------------------

      procedure Process_Type (Entity : Entity_Information) is
      begin
         if Source_File_In_List
           (Source_File_List, Get_File (Get_Declaration_Of (Entity)))
         then
            Doc_Index_Tagged_Type
              (B, Kernel, Index_File, Source_File_List, Entity, Main);
            Process_Parents (Entity);
            Process_Children (Entity);
         end if;
      end Process_Type;

      ------------------
      -- Process_List --
      ------------------

      procedure Process_List
        (List : List_Entity_Information.List; Public : Boolean)
      is
         use List_Entity_Information;
         Node         : List_Entity_Information.List_Node;
      begin
         if not List_Entity_Information.Is_Empty (List) then
            if Options.Show_Private then
               if Public then
                  Doc_Public_Index (B, Kernel, Index_File, "Public:");
               else
                  Doc_Private_Index (B, Kernel, Index_File, "Private:");
               end if;
            end if;

            Node := List_Entity_Information.First (List);

            while Node /= List_Entity_Information.Null_Node loop
               Process_Type (List_Entity_Information.Data (Node));
               Node := List_Entity_Information.Next (Node);
            end loop;
         end if;
      end Process_List;
      Doc_File_Name          : constant String := "index_tagged_type";

   begin
      Index_File := Create_File
        (Get_Doc_Directory (B, Kernel) & Doc_File_Name & Get_Extension (B),
         Binary);
      Doc_Tagged_Type_Index (B, Kernel, Index_File);

      Process_List (Tagged_Type_Index_List, Public => True);

      if Options.Show_Private then
         Process_List (Private_Tagged_Types_List, Public => False);
      end if;

      Doc_End_Of_Index (B, Kernel, Index_File);
      Close (Index_File);
   end Process_Tagged_Type_Index;

   --------------------
   -- Process_Header --
   --------------------

   procedure Process_Header
     (B                 : access Docgen.Backend.Backend'Class;
      Kernel            : access Kernel_Handle_Record'Class;
      Doc_File          : File_Descriptor;
      Entity_List       : in out Type_Entity_List.List;
      Source_Filename   : VFS.Virtual_File;
      Package_Name      : String;
      Package_File      : Virtual_File;
      Options           : All_Options)
   is
      use TEL;
      Declar_Line : Natural := First_File_Line;
      Entity_Node : Type_Entity_List.List_Node;
      Entity      : TEL.Data_Access;
   begin
      if not TEL.Is_Empty (Entity_List) then
         Entity_Node := TEL.First (Entity_List);

         while Entity_Node /= TEL.Null_Node loop
            Entity := TEL.Data_Ref (Entity_Node);

            if Entity.Kind = Package_Entity
              and then Get_Name (Entity.Entity).all = Package_Name
            then
               --  It's a library level package declaration
               if Get_Filename (Get_File (Get_Declaration_Of (Entity.Entity)))
                 = Source_Filename
               then
                  --  Clauses with may be above the declaration
                  --  of the main package
                  Declar_Line := Get_Line (Get_Declaration_Of (Entity.Entity));
               else
                  Declar_Line := Get_Line (Entity.Line_In_Body);
               end if;

               exit;
            end if;

            Entity_Node := TEL.Next (Entity_Node);
         end loop;
      end if;

      Doc_Header (B, Kernel, Doc_File,
                  Header_File    => Package_File,
                  Header_Package => Package_Name,
                  Header_Line    => Declar_Line,
                  Header_Link    => Options.Process_Body_Files
                     and then Other_File_Base_Name
                       (Get_Project_From_File
                         (Project_Registry (Get_Registry (Kernel).all),
                          Source_Filename),
                        Source_Filename) /= Full_Name (Source_Filename).all);
   end Process_Header;

   ------------------------------
   --  Process_Header_Private  --
   ------------------------------

   procedure Process_Header_Private
     (B                 : access Docgen.Backend.Backend'Class;
      Kernel            : access Kernel_Handle_Record'Class;
      Doc_File          : File_Descriptor;
      Level             : in out Natural) is
   begin
      Doc_Header_Private
        (B, Kernel, Doc_File,
         Header_Title => "Private:",
         Level        => Level);
   end Process_Header_Private;

   --------------------
   -- Process_Footer --
   --------------------

   procedure Process_Footer
     (B        : access Docgen.Backend.Backend'Class;
      Kernel   : access Kernel_Handle_Record'Class;
      Doc_File : File_Descriptor) is
   begin
      Doc_Footer (B, Kernel, Doc_File);
   end Process_Footer;

   ---------------------------------
   -- Process_Package_Description --
   ---------------------------------

   procedure Process_Package_Description
     (B        : access Docgen.Backend.Backend'Class;
      Kernel   : access Kernel_Handle_Record'Class;
      Doc_File : File_Descriptor;
      Text     : String;
      Options  : All_Options;
      Language : Language_Access;
      Level    : in out Natural)
   is
      pragma Unreferenced (Options);

      Description : GNAT.OS_Lib.String_Access;
      Start_Line  : Natural := Text'First;
      End_Line    : Natural;

   begin
      Skip_Blanks (Text, Start_Line);
      End_Line := Start_Line;

      Skip_To_Current_Comment_Block_End
        (Get_Language_Context (Language).all, Text, End_Line, True);

      Description := new String'
        (Text (Line_Start (Text, Start_Line) .. Line_End (Text, End_Line)));

      Doc_Subtitle
        (B, Kernel, Doc_File, Level, Subtitle_Name => "Description");

      Doc_Package_Desc
        (B, Kernel, Doc_File, Level, Description => Description.all);
      Free (Description);
   end Process_Package_Description;

   --------------------------
   -- Process_With_Clauses --
   --------------------------

   procedure Process_With_Clause
     (B                : access Docgen.Backend.Backend'Class;
      Kernel           : access Kernel_Handle_Record'Class;
      Doc_File         : File_Descriptor;
      Parsed_List      : in out Construct_List;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Source_Filename  : VFS.Virtual_File;
      File_Text        : GNAT.OS_Lib.String_Access;
      Source_File_List : in out Type_Source_File_Table.HTable;
      Options          : All_Options;
      Level            : in out Natural)
   is
      Old_Line, New_Line : GNAT.OS_Lib.String_Access;
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
         Doc_Subtitle (B, Kernel, Doc_File, Level,
                       Subtitle_Name => "Dependencies");
         Doc_With (B, Kernel, Doc_File, List_Ref_In_File, Source_File_List,
                   Options,
                   Level,
                   With_Header_Line => First_With_Line,
                   With_File        => Source_Filename,
                   With_Header      =>
                     New_Line (New_Line'First + 1 .. New_Line'Last));
         --  the "+1" avoids the first ASCII.LF in New_Line
      end if;

      Free (New_Line);
   end Process_With_Clause;

   -----------------------------
   -- Process_Header_Packages --
   -----------------------------

   procedure Process_Header_Packages
     (B                 : access Docgen.Backend.Backend'Class;
      Kernel            : access Kernel_Handle_Record'Class;
      Doc_File          : File_Descriptor;
      Level             : in out Natural) is
   begin
      Doc_Subtitle (B, Kernel, Doc_File, Level, Subtitle_Name => "Packages");
   end Process_Header_Packages;

   -------------------------
   -- Process_Description --
   -------------------------

   procedure Process_Description
     (B                 : access Docgen.Backend.Backend'Class;
      Kernel            : access Kernel_Handle_Record'Class;
      Doc_File          : File_Descriptor;
      Level             : in out Natural;
      Comment           : String) is
   begin
      Doc_Description (B, Kernel, Doc_File, Level, Description => Comment);
   end Process_Description;

   ----------------------
   -- Process_Packages --
   ----------------------

   procedure Process_Packages
     (B                         : access Docgen.Backend.Backend'Class;
      Kernel                    : access Kernel_Handle_Record'Class;
      Doc_File                  : File_Descriptor;
      Parsed_List               : in out Construct_List;
      Entity_List               : in out Type_Entity_List.List;
      List_Ref_In_File          : in out List_Reference_In_File.List;
      Tagged_Types_List         : in out List_Entity_Information.List;
      Private_Tagged_Types_List : in out List_Entity_Information.List;
      Source_Filename           : VFS.Virtual_File;
      Package_Name              : String;
      Package_Information       : Entity_Information;
      File_Text                 : GNAT.OS_Lib.String_Access;
      Source_File_List          : in out Type_Source_File_Table.HTable;
      Options                   : All_Options;
      Private_Entity            : Boolean;
      Display_Private           : in out Boolean;
      Level                     : in out Natural)
   is
      use TEL;
      Entity_Node             : Type_Entity_List.List_Node;
      Entity_Node_Prec        : Type_Entity_List.List_Node;
      Description             : GNAT.OS_Lib.String_Access;
      Header                  : GNAT.OS_Lib.String_Access;
      Header_Lines            : Natural;
      Header_Start, Header_End : Natural;
      First_Already_Set       : Boolean;
      Entity                  : TEL.Data_Access;
      Info                    : Entity_Information;

   begin
      if not TEL.Is_Empty (Entity_List) then
         First_Already_Set := False;
         Entity_Node := TEL.First (Entity_List);
         Entity_Node_Prec := TEL.Null_Node;

         while Entity_Node /= TEL.Null_Node loop
            Entity := TEL.Data_Ref (Entity_Node);

            if Entity.Is_Private = Private_Entity
              and then Entity.Kind = Package_Entity
              and then Get_Name (Entity.Entity).all /= Package_Name
              and then Source_Filename =
                Get_Filename (Get_File (Get_Declaration_Of (Entity.Entity)))
              and then Entity_Defined_In_Package
                (Entity.Entity, Package_Information)
            then
               if Package_Contain_Entity (Entity.Entity, Entity_List) then
                  --  Entities are declared in the current package
                  Get_Whole_Header
                    (File_Text.all,
                     Parsed_List,
                     Get_Name (Entity.Entity).all,
                     Get_Line (Get_Declaration_Of (Entity.Entity)),
                     Header_Start, Header_End);

                  if Header_Start <= Header_End then
                     Header_Lines := Count_Lines
                       (File_Text (Header_Start .. Header_End));

                     if Entity.Is_Private and then not Display_Private then
                        --  Print title "private" required and not done
                        Process_Header_Private (B, Kernel, Doc_File, Level);
                        Display_Private := True;
                     end if;

                     --  Check if the subtitle "Packages" has already been
                     --  set.

                     if not First_Already_Set then
                        Process_Header_Packages (B, Kernel, Doc_File, Level);
                        First_Already_Set := True;
                     end if;

                     Description := new String'
                       (Entities.Get_Documentation
                          (Entity.Entity,
                           File_Text.all));

                     --  We save in an Entity_Information the current package
                     --  because it must be removed from Entity_List

                     Info := Entity.Entity;
                     Ref (Info);

                     Type_Entity_List.Remove_Nodes
                       (Entity_List,
                        Entity_Node_Prec,
                        Entity_Node);
                     Doc_Package_Open_Close
                       (B,
                        Kernel,
                        Doc_File,
                        List_Ref_In_File,
                        Source_File_List,
                        Options,
                        Level,
                        Entity => Info,
                        Header => "package " & Get_Name (Info).all & " is");
                     Level := Level + 1;

                     --  Recursive call in order to deal with entity defined
                     --  in the current package.

                     Process_Source_Spec
                       (B,
                        Kernel,
                        Doc_File,
                        Source_File_List,
                        Source_Filename,
                        Get_Name (Info).all,
                        Info,
                        Entity_List,
                        List_Ref_In_File,
                        Tagged_Types_List,
                        Private_Tagged_Types_List,
                        Options,
                        Level,
                        File_Text,
                        Parsed_List);
                     Level := Level - 1;

                     Doc_Package_Open_Close
                       (B,
                        Kernel,
                        Doc_File,
                        List_Ref_In_File,
                        Source_File_List,
                        Options,
                        Level,
                        Entity => Info,
                        Header => "end " & Get_Name (Info).all);

                     if Description.all /= "" then
                        Process_Description
                          (B,
                           Kernel,
                           Doc_File,
                           Level,
                           Description.all);
                     end if;

                     Unref (Info);
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

                  Get_Whole_Header
                    (File_Text.all,
                     Parsed_List,
                     Get_Name (Entity.Entity).all,
                     Get_Line (Get_Declaration_Of (Entity.Entity)),
                     Header_Start, Header_End);

                  if Header_Start <= Header_End then
                     Remove_Indent
                       (File_Text (Header_Start .. Header_End),
                        Level * Get_Indent (B.all),
                        Header, Header_Lines);

                     if Entity.Is_Private and then not Display_Private then
                        Process_Header_Private (B, Kernel, Doc_File, Level);
                        Display_Private := True;
                     end if;

                     if not First_Already_Set then
                        Process_Header_Packages (B, Kernel, Doc_File, Level);
                        First_Already_Set := True;
                     end if;

                     Description := new String'
                       (Entities.Get_Documentation
                          (Entity.Entity,
                           File_Text.all));

                     Doc_Package
                       (B, Kernel, Doc_File,
                        List_Ref_In_File, Source_File_List, Options, Level,
                        Package_Entity => Entity.Entity,
                        Package_Header => Header.all);

                     if Description.all /= "" then
                        Process_Description
                          (B, Kernel, Doc_File, Level, Description.all);
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
     (B                 : access Docgen.Backend.Backend'Class;
      Kernel            : access Kernel_Handle_Record'Class;
      Doc_File          : File_Descriptor;
      Level             : in out Natural) is
   begin
      Doc_Subtitle (B, Kernel, Doc_File, Level, Subtitle_Name => "Constants");
   end Process_Header_Vars;

   ------------------
   -- Process_Vars --
   ------------------

   procedure Process_Vars
     (B                : access Docgen.Backend.Backend'Class;
      Kernel           : access Kernel_Handle_Record'Class;
      Doc_File         : File_Descriptor;
      Parsed_List      : in out Construct_List;
      Entity_List      : in out Type_Entity_List.List;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Source_Filename  : VFS.Virtual_File;
      Package_Info     : Entity_Information;
      File_Text        : GNAT.OS_Lib.String_Access;
      Source_File_List : in out Type_Source_File_Table.HTable;
      Options          : All_Options;
      Private_Entity   : Boolean;
      Display_Private  : in out Boolean;
      Level            : in out Natural)
   is
      use TEL;
      use type Basic_Types.String_Access;
      Entity_Node       : Type_Entity_List.List_Node;
      Entity_Node_Prec  : Type_Entity_List.List_Node;
      Description       : GNAT.OS_Lib.String_Access;
      Header            : GNAT.OS_Lib.String_Access;
      Header_Lines      : Natural;
      Header_Start, Header_End : Natural;
      First_Already_Set : Boolean;
      Delete_Node       : Boolean;
      Entity            : TEL.Data_Access;

   begin
      if not TEL.Is_Empty (Entity_List) then
         First_Already_Set := False;
         Entity_Node := TEL.First (Entity_List);
         Entity_Node_Prec := TEL.Null_Node;

         while Entity_Node /= TEL.Null_Node loop
            Entity := TEL.Data_Ref (Entity_Node);
            Delete_Node := False;

            if Entity.Is_Private = Private_Entity
              and then Entity.Kind = Var_Entity
              and then Source_Filename =
                Get_Filename (Get_File (Get_Declaration_Of (Entity.Entity)))
              and then Entity_Defined_In_Package (Entity.Entity, Package_Info)
            then
               Get_Whole_Header
                 (File_Text.all,
                  Parsed_List,
                  Get_Name (Entity.Entity).all,
                  Get_Line (Get_Declaration_Of (Entity.Entity)),
                  Header_Start, Header_End);

               --  Check if it was an entity with its own header

               if Header_Start <= Header_End then
                  Remove_Indent
                    (File_Text (Header_Start .. Header_End),
                     Level * Get_Indent (B.all),
                     Header, Header_Lines);

                  if Entity.Is_Private and then not Display_Private then
                     --  It's the first time we met a private entity
                     --  and we work on the private part, so we put the
                     --  title "Private"

                     Process_Header_Private (B, Kernel, Doc_File, Level);
                     Display_Private := True;
                  end if;

                  --  Check if the subtitle "Constand and Named Numbers:"
                  --  has been set already.

                  if not First_Already_Set then
                     Process_Header_Vars (B, Kernel, Doc_File, Level);
                     First_Already_Set := True;
                  end if;

                  Description := new String'
                    (Entities.Get_Documentation
                       (Entity.Entity,
                        File_Text.all));

                  Doc_Var
                    (B, Kernel, Doc_File, List_Ref_In_File,
                     Source_File_List, Options, Level,
                     Entity => Entity.Entity, Header => Header.all);

                  if Description.all /= "" then
                     Process_Description
                       (B, Kernel, Doc_File, Level, Description.all);
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
     (B                 : access Docgen.Backend.Backend'Class;
      Kernel            : access Kernel_Handle_Record'Class;
      Doc_File          : File_Descriptor;
      Level             : in out Natural) is
   begin
      Doc_Subtitle (B, Kernel, Doc_File, Level, Subtitle_Name => "Exceptions");
   end Process_Header_Exceptions;

   ------------------------
   -- Process_Exceptions --
   ------------------------

   procedure Process_Exceptions
     (B                : access Docgen.Backend.Backend'Class;
      Kernel           : access Kernel_Handle_Record'Class;
      Doc_File         : File_Descriptor;
      Parsed_List      : in out Construct_List;
      Entity_List      : in out Type_Entity_List.List;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Source_Filename  : VFS.Virtual_File;
      Package_Info     : Entity_Information;
      File_Text        : GNAT.OS_Lib.String_Access;
      Source_File_List : in out Type_Source_File_Table.HTable;
      Options          : All_Options;
      Private_Entity   : Boolean;
      Display_Private  : in out Boolean;
      Level            : in out Natural)
   is
      use TEL;
      Entity_Node       : Type_Entity_List.List_Node;
      Entity_Node_Prec  : Type_Entity_List.List_Node;
      Description       : GNAT.OS_Lib.String_Access;
      Header            : GNAT.OS_Lib.String_Access;
      Header_Lines      : Natural;
      Header_Start, Header_End : Natural;
      First_Already_Set : Boolean;
      Delete_Node       : Boolean;
      Entity            : TEL.Data_Access;

   begin
      if not TEL.Is_Empty (Entity_List) then
         First_Already_Set := False;
         Entity_Node := TEL.First (Entity_List);
         Entity_Node_Prec := TEL.Null_Node;

         while Entity_Node /= TEL.Null_Node loop
            Delete_Node := False;
            Entity := TEL.Data_Ref (Entity_Node);

            if Entity.Is_Private = Private_Entity
              and then Entity.Kind = Exception_Entity
              and then Source_Filename =
                Get_Filename (Get_File (Get_Declaration_Of (Entity.Entity)))
              and then Entity_Defined_In_Package (Entity.Entity, Package_Info)
            then
               Get_Whole_Header
                 (File_Text.all,
                  Parsed_List,
                  Get_Name (Entity.Entity).all,
                  Get_Line (Get_Declaration_Of (Entity.Entity)),
                  Header_Start, Header_End);

               --  Check if it was a entity with its own header

               if Header_Start <= Header_End then
                  Remove_Indent
                    (File_Text (Header_Start .. Header_End),
                     Level * Get_Indent (B.all),
                     Header, Header_Lines);

                  if Entity.Is_Private and then not Display_Private then
                     Process_Header_Private (B, Kernel, Doc_File, Level);
                     Display_Private := True;
                  end if;

                  --  Check if the subtitle "Exceptions:" has been set already.

                  if not First_Already_Set then
                     Process_Header_Exceptions (B, Kernel, Doc_File, Level);
                     First_Already_Set := True;
                  end if;

                  Description := new String'
                    (Entities.Get_Documentation
                       (Entity.Entity,
                        File_Text.all));

                  Doc_Exception
                    (B, Kernel, Doc_File,
                     List_Ref_In_File, Source_File_List, Options, Level,
                     Entity => Entity.Entity,
                     Header => Header.all);

                  if Description.all /= "" then
                     Process_Description
                       (B, Kernel, Doc_File, Level, Description.all);
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
     (B                 : access Docgen.Backend.Backend'Class;
      Kernel            : access Kernel_Handle_Record'Class;
      Doc_File          : File_Descriptor;
      Level             : in out Natural) is
   begin
      Doc_Subtitle (B, Kernel, Doc_File, Level, Subtitle_Name => "Types");
   end Process_Header_Types;

   ------------------------
   -- Process_One_Family --
   ------------------------

   procedure Process_One_Family
     (B                : access Docgen.Backend.Backend'Class;
      Kernel           : access Kernel_Handle_Record'Class;
      Doc_File         : File_Descriptor;
      Family           : List_Entity_Information.List;
      Source_File_List : in out Type_Source_File_Table.HTable;
      Entity           : Entity_Information;
      Level            : in out Natural)
   is
      use List_Entity_Information;
      Node             : List_Entity_Information.List_Node;
      Tagged_Entity    : Entity_Information;

   begin
      if not List_Entity_Information.Is_Empty (Family) then
         Node := List_Entity_Information.First (Family);

         while Node /= List_Entity_Information.Null_Node loop
            Tagged_Entity := List_Entity_Information.Data (Node);

            if Tagged_Entity = Entity
              and then Source_File_In_List
                (Source_File_List, Get_File (Get_Declaration_Of (Entity)))
            then
               Doc_Tagged_Type
                 (B, Kernel, Doc_File, Source_File_List, Level, Entity);
               exit;
            end if;
            Node := List_Entity_Information.Next (Node);
         end loop;
      end if;
   end Process_One_Family;

   -------------------
   -- Process_Types --
   -------------------

   procedure Process_Types
     (B                         : access Docgen.Backend.Backend'Class;
      Kernel                    : access Kernel_Handle_Record'Class;
      Doc_File                  : File_Descriptor;
      Parsed_List               : in out Construct_List;
      Entity_List               : in out Type_Entity_List.List;
      List_Ref_In_File          : in out List_Reference_In_File.List;
      Tagged_Types_List         : in out List_Entity_Information.List;
      Private_Tagged_Types_List : in out List_Entity_Information.List;
      Source_Filename           : VFS.Virtual_File;
      Package_Info              : Entity_Information;
      File_Text                 : GNAT.OS_Lib.String_Access;
      Source_File_List          : in out Type_Source_File_Table.HTable;
      Options                   : All_Options;
      Private_Entity            : Boolean;
      Display_Private           : in out Boolean;
      Level                     : in out Natural)
   is
      use TEL;
      Entity_Node        : Type_Entity_List.List_Node;
      Entity_Node_Prec   : Type_Entity_List.List_Node;
      Description        : GNAT.OS_Lib.String_Access;
      Header             : GNAT.OS_Lib.String_Access;
      Header_Lines       : Natural;
      Header_Start, Header_End : Natural;
      First_Already_Set  : Boolean;
      Delete_Node        : Boolean;
      Entity             : TEL.Data_Access;
      Kind               : E_Kinds;
      Info               : Entity_Information;

   begin
      if not TEL.Is_Empty (Entity_List) then
         First_Already_Set := False;
         Entity_Node      := TEL.First (Entity_List);
         Entity_Node_Prec := TEL.Null_Node;

         while Entity_Node /= TEL.Null_Node loop
            Delete_Node := False;
            Entity := TEL.Data_Ref (Entity_Node);

            --  We search if the type is declared in the current package
            if Entity.Public_Declaration = null then
               Info := Entity.Entity;
            else
               --  Public entity which contains private fields.
               --  In this case, the name of the entity appears in two
               --  elements of Entity_List. One element refers to the
               --  public declaration and the second element refers the
               --  private part. For this last element the field Line_In_Body
               --  refers to the code in the private part and the field
               --  Public_Declaration leads us to the public declaration.
               --  We need this public declaration because only this one
               --  appears in the scope tree.

               Info := Entity.Public_Declaration;
            end if;

            if Entity.Is_Private = Private_Entity
              and then Entity.Kind = Type_Entity
              and then Source_Filename =
                Get_Filename (Get_File (Get_Declaration_Of (Entity.Entity)))
              and then Entity_Defined_In_Package (Info, Package_Info)
            then
               Get_Whole_Header
                 (File_Text.all,
                  Parsed_List,
                  Get_Name (Entity.Entity).all,
                  Get_Line (Get_Declaration_Of (Entity.Entity)),
                  Header_Start, Header_End);

               --  Check if it was a entity with its own header

               if Header_Start <= Header_End then
                  Remove_Indent
                    (File_Text (Header_Start .. Header_End),
                     Level * Get_Indent (B.all),
                     Header, Header_Lines);

                  if Entity.Is_Private and then not Display_Private then
                     --  It's the first time we met a private entity
                     --  and we work on the private part, so we put the
                     --  title "Private"

                     Process_Header_Private (B, Kernel, Doc_File, Level);
                     Display_Private := True;
                  end if;

                  --  Check if the subtitle "Types:" has to be set.

                  if not First_Already_Set then
                     Process_Header_Types (B, Kernel, Doc_File, Level);
                     First_Already_Set := True;
                  end if;

                  Description := new String'
                    (Entities.Get_Documentation
                       (Entity.Entity,
                        File_Text.all));

                  Doc_Type
                    (B, Kernel, Doc_File, List_Ref_In_File, Source_File_List,
                     Options, Level,
                     Entity => Entity.Entity,
                     Header => Header.all);

                  if Description.all /= "" then
                     Process_Description
                       (B, Kernel, Doc_File, Level, Description.all);
                  end if;

                  Kind := Get_Kind (Entity.Entity).Kind;

                  if Options.Tagged_Types
                     --  ??? In Ada, tagged type are classified as Record
                     --  It must be improved (see also comments in
                     --  docgen-work_on_file.adb)
                    and then (Kind = Record_Kind
                              or else Kind = Class
                              or else Kind = Class_Wide)
                  then
                     --  it's a tagged type

                     if Private_Entity then
                        --  List of private tagged types

                        Process_One_Family
                          (B,
                           Kernel,
                           Doc_File,
                           Private_Tagged_Types_List,
                           Source_File_List,
                           Entity.Entity,
                           Level);
                     else
                        --  List of public tagged types

                        Process_One_Family
                          (B,
                           Kernel,
                           Doc_File,
                           Tagged_Types_List,
                           Source_File_List,
                           Entity.Entity,
                           Level);
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
     (B                 : access Docgen.Backend.Backend'Class;
      Kernel            : access Kernel_Handle_Record'Class;
      Doc_File          : File_Descriptor;
      Level             : in out Natural) is
   begin
      Doc_Subtitle (B, Kernel, Doc_File, Level,
                    Subtitle_Name => "Tasks, Entries and Entry Families");
   end Process_Header_Entries;

   ---------------------
   -- Process_Entries --
   ---------------------

   procedure Process_Entries
     (B                  : access Docgen.Backend.Backend'Class;
      Kernel             : access Kernel_Handle_Record'Class;
      Doc_File           : File_Descriptor;
      Parsed_List        : in out Construct_List;
      Entity_List        : in out Type_Entity_List.List;
      List_Ref_In_File   : in out List_Reference_In_File.List;
      Source_Filename    : VFS.Virtual_File;
      Package_Info       : Entity_Information;
      File_Text          : GNAT.OS_Lib.String_Access;
      Source_File_List   : in out Type_Source_File_Table.HTable;
      Options            : All_Options;
      Private_Entity     : Boolean;
      Display_Private    : in out Boolean;
      Level              : in out Natural)
   is
      use TEL;
      Entity_Node       : Type_Entity_List.List_Node;
      Entity_Node_Prec  : Type_Entity_List.List_Node;
      Description       : GNAT.OS_Lib.String_Access;
      Header            : GNAT.OS_Lib.String_Access;
      Header_Lines      : Natural;
      Header_Start, Header_End : Natural;
      First_Already_Set : Boolean;
      Delete_Node       : Boolean;
      Entity            : TEL.Data_Access;

   begin
      if not TEL.Is_Empty (Entity_List) then
         First_Already_Set := False;
         Entity_Node := TEL.First (Entity_List);
         Entity_Node_Prec := TEL.Null_Node;

         while Entity_Node /= TEL.Null_Node loop
            Entity := TEL.Data_Ref (Entity_Node);
            Delete_Node := False;

            if Entity.Is_Private = Private_Entity
              and then Entity.Kind = Entry_Entity
              and then Source_Filename =
                Get_Filename (Get_File (Get_Declaration_Of (Entity.Entity)))
              and then Entity_Defined_In_Package (Entity.Entity, Package_Info)
            then
               Get_Whole_Header
                 (File_Text.all,
                  Parsed_List,
                  Get_Name (Entity.Entity).all,
                  Get_Line (Get_Declaration_Of (Entity.Entity)),
                  Header_Start, Header_End);

               --  Check if it was a entity with its own header

               if Header_Start <= Header_End then
                  Remove_Indent
                    (File_Text (Header_Start .. Header_End),
                     Level * Get_Indent (B.all),
                     Header, Header_Lines);

                  if Entity.Is_Private and then not Display_Private then
                     Process_Header_Private (B, Kernel, Doc_File, Level);
                     Display_Private := True;
                  end if;

                  --  Check if the subtitle has to be set.

                  if not First_Already_Set then
                     Process_Header_Entries (B, Kernel, Doc_File, Level);
                     First_Already_Set := True;
                  end if;

                  Description := new String'
                    (Entities.Get_Documentation
                       (Entity.Entity,
                        File_Text.all));

                  Doc_Entry
                    (B, Kernel, Doc_File, List_Ref_In_File,
                     Source_File_List, Options, Level,
                     Entity => Entity.Entity,
                     Header => Header.all);

                  if Description.all /= "" then
                     Process_Description
                       (B, Kernel, Doc_File, Level, Description.all);
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
     (B                 : access Docgen.Backend.Backend'Class;
      Kernel            : access Kernel_Handle_Record'Class;
      Doc_File          : File_Descriptor;
      Level             : in out Natural) is
   begin
      Doc_Subtitle (B, Kernel, Doc_File, Level, Subtitle_Name => "Subprogams");
   end Process_Header_Subprograms;

   -------------------------------
   -- Process_Caller_References --
   -------------------------------

   procedure Process_Caller_References
     (B                 : access Docgen.Backend.Backend'Class;
      Kernel            : access Kernel_Handle_Record'Class;
      Options           : All_Options;
      Doc_File          : File_Descriptor;
      Info              : Entity_Information;
      Source_File_List  : Type_Source_File_Table.HTable;
      Level             : in out Natural)
   is
      Caller         : Entity_Information;
      Reference_Iter : Entity_Reference_Iterator;
      Callers        : Entity_Information_Arrays.Instance;
   begin
      Find_All_References (Iter => Reference_Iter, Entity => Info);
      while not At_End (Reference_Iter) loop
         if Get (Reference_Iter) /= No_Entity_Reference then
            Caller := Get_Caller (Get (Reference_Iter));
            if Caller /= null and then Is_Subprogram (Caller) then
               Entity_Information_Arrays.Append (Callers, Caller);
            end if;
         end if;
         Next (Reference_Iter);
      end loop;
      Destroy (Reference_Iter);

      Doc_Caller_References
        (B,
         Kernel            => Kernel,
         File              => Doc_File,
         Options           => Options,
         Level             => Level,
         Callers           => Callers,
         Processed_Sources => Source_File_List);
      Entity_Information_Arrays.Free (Callers);
   end Process_Caller_References;

   ------------------------------
   -- Process_Calls_References --
   ------------------------------

   procedure Process_Calls_References
     (B                 : access Docgen.Backend.Backend'Class;
      Kernel            : access Kernel_Handle_Record'Class;
      Options           : All_Options;
      Doc_File          : File_Descriptor;
      Info              : Entity_Information;
      Source_File_List  : Type_Source_File_Table.HTable;
      Level             : in out Natural)
   is
      Call_Iter      : Calls_Iterator := Get_All_Called_Entities (Info);
      Entity         : Entity_Information;
      Calls          : Entity_Information_Arrays.Instance;
   begin
      while not At_End (Call_Iter) loop
         Entity := Get (Call_Iter);
         if Is_Subprogram (Entity) then
            Entity_Information_Arrays.Append (Calls, Entity);
         end if;
         Next (Call_Iter);
      end loop;
      Destroy (Call_Iter);

      Doc_Calls_References
        (B,
         Kernel            => Kernel,
         File              => Doc_File,
         Options           => Options,
         Level             => Level,
         Calls             => Calls,
         Processed_Sources => Source_File_List);
      Entity_Information_Arrays.Free (Calls);
   end Process_Calls_References;

   -------------------------
   -- Process_Subprograms --
   -------------------------

   procedure Process_Subprograms
     (B                  : access Docgen.Backend.Backend'Class;
      Kernel             : access Kernel_Handle_Record'Class;
      Doc_File           : File_Descriptor;
      Parsed_List        : in out Construct_List;
      Entity_List        : in out Type_Entity_List.List;
      List_Ref_In_File   : in out List_Reference_In_File.List;
      Source_Filename    : VFS.Virtual_File;
      Package_Info       : Entity_Information;
      File_Text          : GNAT.OS_Lib.String_Access;
      Source_File_List   : in out Type_Source_File_Table.HTable;
      Options            : All_Options;
      Private_Entity     : Boolean;
      Display_Private    : in out Boolean;
      Level              : in out Natural)
   is
      use TEL;
      Entity_Node       : Type_Entity_List.List_Node;
      Entity_Node_Prec  : Type_Entity_List.List_Node;
      Description       : GNAT.OS_Lib.String_Access;
      Header            : GNAT.OS_Lib.String_Access;
      Header_Lines      : Natural;
      Header_Start      : Natural;
      Header_End        : Natural;
      First_Already_Set : Boolean;
      Delete_Node       : Boolean;
      Entity            : TEL.Data_Access;

   begin
      if not TEL.Is_Empty (Entity_List) then
         First_Already_Set := False;
         Entity_Node := TEL.First (Entity_List);
         Entity_Node_Prec := TEL.Null_Node;

         while Entity_Node /= TEL.Null_Node loop
            Entity := TEL.Data_Ref (Entity_Node);
            Delete_Node := False;

            if Entity.Is_Private = Private_Entity
              and then Entity.Kind = Subprogram_Entity
              and then Source_Filename =
                Get_Filename (Get_File (Get_Declaration_Of (Entity.Entity)))
              and then Entity_Defined_In_Package (Entity.Entity, Package_Info)
            then
               Get_Whole_Header
                 (File_Text.all,
                  Parsed_List,
                  Get_Name (Entity.Entity).all,
                  Get_Line (Get_Declaration_Of (Entity.Entity)),
                  Header_Start, Header_End);

               --  Check if it was an entity with its own header

               if Header_Start <= Header_End then
                  Remove_Indent
                    (File_Text (Header_Start .. Header_End),
                     Level * Get_Indent (B.all),
                     Header, Header_Lines);

                  if Entity.Is_Private and then not Display_Private then
                     Process_Header_Private (B, Kernel, Doc_File, Level);
                     Display_Private := True;
                  end if;

                  --  Check if the subtitle "Subprograms:" has to be set.

                  if not First_Already_Set then
                     Process_Header_Subprograms (B, Kernel, Doc_File, Level);
                     First_Already_Set := True;
                  end if;

                  Description := new String'
                    (Entities.Get_Documentation
                       (Entity.Entity,
                        File_Text.all));

                  Doc_Subprogram
                    (B, Kernel, Doc_File,
                     List_Ref_In_File, Source_File_List, Options, Level,
                     Entity => Entity.all, Header => Header.all);

                  if Description.all /= "" then
                     Process_Description
                       (B, Kernel, Doc_File, Level, Description.all);
                  end if;

                  if Options.References then
                     --  Callgraph is processed

                     Process_Caller_References
                       (B                => B,
                        Kernel           => Kernel,
                        Options          => Options,
                        Doc_File         => Doc_File,
                        Info             => Entity.Entity,
                        Source_File_List => Source_File_List,
                        Level            => Level);
                     Process_Calls_References
                       (B                => B,
                        Kernel           => Kernel,
                        Options          => Options,
                        Doc_File         => Doc_File,
                        Info             => Entity.Entity,
                        Source_File_List => Source_File_List,
                        Level            => Level);
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
      Package_Container : Entity_Information) return Boolean
   is
      Entity : Entity_Information := Entity_Info;
   begin
      while Entity /= null
        and then Entity /= Package_Container
      loop
         Entity := Get_Caller (Declaration_As_Reference (Entity));
      end loop;

      return Entity /= null;
   end Entity_Defined_In_Package;

   ----------------------------
   -- Package_Contain_Entity --
   ----------------------------

   function Package_Contain_Entity
     (Package_Entity : Entity_Information;
      Entity_List    : Type_Entity_List.List) return Boolean
   is
      use TEL;
      Entity_Node : Type_Entity_List.List_Node;
   begin
      if not TEL.Is_Empty (Entity_List) then
         Entity_Node := TEL.First (Entity_List);

         while Entity_Node /= TEL.Null_Node loop
            if Entity_Defined_In_Package
              (TEL.Data_Ref (Entity_Node).Entity, Package_Entity)
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

   --------------------------
   -- Is_Ignorable_Comment --
   --------------------------

--     function Is_Ignorable_Comment (Comment_Line : String) return Boolean is
--     begin
--        --  ??? Should be language-insensitive
--        for J in Comment_Line'First .. Comment_Line'Last - 2 loop
--           if Comment_Line (J .. J + 1) = "--" then
--              return Comment_Line (J + 2) = '!';
--           end if;
--        end loop;
--
--        return False;
--     end Is_Ignorable_Comment;

   ------------------------
   --  Get_Whole_Header  --
   ------------------------

   procedure Get_Whole_Header
     (File_Text    : String;
      Parsed_List  : Construct_List;
      Entity_Name  : String;
      Entity_Line  : Natural;
      Header_Start : out Natural;
      Header_End   : out Natural)
   is
      use type Basic_Types.String_Access;
      Parse_Node : Construct_Access := Parsed_List.First;
   begin
      while Parse_Node /= null loop
         if Parse_Node.Sloc_Start.Line = Entity_Line
           and then Parse_Node.Name /= null
         then
            if Parse_Node.Name (Parse_Node.Name'First) = '"'
              and then Parse_Node.Name (Parse_Node.Name'Last) = '"'
              and then Parse_Node.Name'First + 1 <= Parse_Node.Name'Last - 1
            then
               --  This case happens when operators are overridden
               --  It's necessary to look after " in Parse_Node.Name.all
               --  because Parse_Construct returns the name with " (eg. "<")
               --  whereas functions which permit to build the list of entities
               --  return the name without " (eg. <).

               if To_Lower
                 (Parse_Node.Name
                    (Parse_Node.Name'First + 1 .. Parse_Node.Name'Last - 1)) =
                   Entity_Name
               then
                  Header_Start := Parse_Node.Sloc_Start.Index;
                  Header_End   := Parse_Node.Sloc_End.Index;
                  return;
               end if;

            elsif To_Lower (Parse_Node.Name.all) = Entity_Name then
               Header_Start := Parse_Node.Sloc_Start.Index;
               Header_End   := Parse_Node.Sloc_End.Index;
               return;
            end if;
         end if;
         Parse_Node := Parse_Node.Next;
      end loop;

      Header_Start := File_Text'First;
      Header_End   := Header_Start - 1;
   end Get_Whole_Header;

   -------------------
   -- Remove_Indent --
   -------------------

   procedure Remove_Indent
     (Text       : String;
      Space      : Natural;
      Clean_Text : out GNAT.OS_Lib.String_Access;
      Line_Count : out Natural)
   is
      use Ada.Strings.Unbounded;

      Result   : Unbounded_String;
      Old_J    : Natural := Text'First;
      J        : Natural := Text'First;
      Stop     : Boolean;

   begin
      Line_Count := 1;
      while J <= Text'Last loop
         if Text (J) = ASCII.LF then
            Line_Count := Line_Count + 1;
            Append (Result, Text (Old_J .. J));
            Stop := False;

            for K in J + 1 .. Natural'Min (J + Space, Text'Last) loop
               if Text (K) /= ' ' then
                  --  If the indentation is bad
                  Old_J := K;
                  J := Old_J;
                  Stop := True;
                  exit;
               end if;
            end loop;

            if not Stop then
               Old_J := J + Space + 1;
               J := Old_J;
            end if;

         else
            J := J + 1;
         end if;
      end loop;

      Append (Result, Text (Old_J .. J - 1));
      Clean_Text := new String'(To_String (Result));
   end Remove_Indent;

end Docgen.Work_On_Source;
