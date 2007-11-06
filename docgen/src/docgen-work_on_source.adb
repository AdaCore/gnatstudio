-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2001-2007, AdaCore                 --
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

with Ada.Characters.Handling;   use Ada.Characters.Handling;
with GNAT.OS_Lib;
with GNAT.Strings;

with Glib.Convert;              use Glib.Convert;

with Doc_Utils;                 use Doc_Utils;
with Docgen.Backend;            use Docgen.Backend;
with Entities.Queries;          use Entities.Queries;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with Language;                  use Language;
with Language_Handlers;         use Language_Handlers;
with OS_Utils;                  use OS_Utils;
with Projects;                  use Projects;
with Projects.Registry;         use Projects.Registry;
with String_Utils;              use String_Utils;
with Traces;                    use Traces;
with VFS;                       use VFS;

package body Docgen.Work_On_Source is

   use type GNAT.Strings.String_Access;

   package TEL renames Type_Entity_List;

   procedure Process_Source_Spec
     (B                         : access Docgen.Backend.Backend'Class;
      Kernel                    : access Kernel_Handle_Record'Class;
      Result                    : in out Unbounded_String;
      Source_File_List          : in out Type_Source_File_Table.HTable;
      Source_Filename           : VFS.Virtual_File;
      Unit_Name                 : String;
      Unit_Info                 : Entity_Information;
      Main_Unit                 : Boolean;
      Entity_List               : in out Type_Entity_List.List;
      List_Ref_In_File          : in out List_Reference_In_File.List;
      Tagged_Types_List         : in out List_Entity_Information.List;
      Private_Tagged_Types_List : in out List_Entity_Information.List;
      Options                   : All_Options;
      Level                     : in out Natural;
      File_Text                 : GNAT.Strings.String_Access;
      Parsed_List               : in out Construct_List);
   --  It's in charge of processing packages, types, variables, subprograms,
   --  exceptions, entries. It can be called recursively for inner package.
   --  Source_File_List : list of all files that must be processed by docgen.
   --  Source_Filename  : current file processed.
   --  Unit_Name        : name of the current unit. The first time, it's
   --  the name of the main unit which is defined in the file. Then, in the
   --  case of an inner package, this subprogram is called recursively and this
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
      Result           : in out Unbounded_String;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Source_File      : Virtual_File;
      File_Text        : GNAT.Strings.String_Access;
      Source_File_List : Type_Source_File_Table.HTable;
      Options          : All_Options;
      Level            : Natural);
   --  Will pass the information about the body file to the output
   --  subprogram. This is the only subprogram working on the contents
   --  of the body source files.

   procedure Process_Open_File
     (B            : access Docgen.Backend.Backend'Class;
      Kernel       : access Kernel_Handle_Record'Class;
      Result       : in out Unbounded_String;
      Unit_Name    : String);
   --  Is always the first subprogram to be called, as it creates the
   --  very beginning of the documentation by calling the output
   --  subprogram

   procedure Process_File_Description
     (B        : access Docgen.Backend.Backend'Class;
      Kernel   : access Kernel_Handle_Record'Class;
      Result   : in out Unbounded_String;
      Text     : String;
      Options  : All_Options;
      Language : Language_Access;
      Level    : Natural);
   --  Extracts all the comment lines of the source file which are at the
   --  beginning of it. Empty lines are ignored, the procedure stops when
   --  first command is found. This information will be passed to the
   --  output subprogram

   procedure Process_With_Clause
     (B                : access Docgen.Backend.Backend'Class;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      Parsed_List      : in out Construct_List;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Source_Filename  : VFS.Virtual_File;
      File_Text        : GNAT.Strings.String_Access;
      Source_File_List : in out Type_Source_File_Table.HTable;
      Options          : All_Options;
      Level            : Natural);
   --  Will process the lines at the beginning of the source file
   --  starting with "with" and pass them to the output subprogram

   procedure Process_Description
     (B       : access Docgen.Backend.Backend'Class;
      Kernel  : access Kernel_Handle_Record'Class;
      Result  : in out Unbounded_String;
      Level   : Natural;
      Comment : String);
   --  Processes comments after the source code (or before: it depends on
   --  preferences)

   procedure Process_Packages
     (B                         : access Docgen.Backend.Backend'Class;
      Kernel                    : access Kernel_Handle_Record'Class;
      Result                    : in out Unbounded_String;
      Parsed_List               : in out Construct_List;
      Entity_List               : in out Type_Entity_List.List;
      List_Ref_In_File          : in out List_Reference_In_File.List;
      Tagged_Types_List         : in out List_Entity_Information.List;
      Private_Tagged_Types_List : in out List_Entity_Information.List;
      Source_Filename           : VFS.Virtual_File;
      Package_Name              : String;
      Package_Information       : Entity_Information;
      Main_Unit                 : Boolean;
      File_Text                 : GNAT.Strings.String_Access;
      Source_File_List          : in out Type_Source_File_Table.HTable;
      Options                   : All_Options;
      Private_Entity            : Boolean;
      Display_Private           : in out Boolean;
      Level                     : in out Natural);
   --  Will process renamed and instantiated packages and pass
   --  them to the output subprogram.
   --  Display_Private : indicates whether the "Private" header should be
   --                    displayed
   --  Private_Entity  : indicates whether private/public entities
   --                    contained in Entity_List must be processed

   procedure Process_Vars
     (B                : access Docgen.Backend.Backend'Class;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      Parsed_List      : in out Construct_List;
      Entity_List      : in out Type_Entity_List.List;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Source_Filename  : VFS.Virtual_File;
      Package_Info     : Entity_Information;
      File_Text        : GNAT.Strings.String_Access;
      Source_File_List : in out Type_Source_File_Table.HTable;
      Options          : All_Options;
      Private_Entity   : Boolean;
      Display_Private  : in out Boolean;
      Level            : Natural);
   --  Called by Process_Source to work on the constants
   --  and named numbers and pass each of them to the output subprogram
   --  Display_Private : indicate if we print the "Private" header.
   --  Private_Entity  : indicate if it must process private/public entities
   --  contained in Entity_List.

   procedure Process_Exceptions
     (B                : access Docgen.Backend.Backend'Class;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      Parsed_List      : in out Construct_List;
      Entity_List      : in out Type_Entity_List.List;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Source_Filename  : VFS.Virtual_File;
      Package_Info     : Entity_Information;
      File_Text        : GNAT.Strings.String_Access;
      Source_File_List : in out Type_Source_File_Table.HTable;
      Options          : All_Options;
      Private_Entity   : Boolean;
      Display_Private  : in out Boolean;
      Level            : Natural);
   --  Called by Process_Source to work on the exceptions and
   --  pass each of them to the output subprogram
   --  Display_Private : indicate if we print the "Private" header.
   --  Private_Entity  : indicate if it must process private/public entities
   --  contained in Entity_List.

   procedure Process_Entries
     (B                : access Docgen.Backend.Backend'Class;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      Parsed_List      : in out Construct_List;
      Entity_List      : in out Type_Entity_List.List;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Source_Filename  : VFS.Virtual_File;
      Package_Info     : Entity_Information;
      File_Text        : GNAT.Strings.String_Access;
      Source_File_List : in out Type_Source_File_Table.HTable;
      Options          : All_Options;
      Private_Entity   : Boolean;
      Display_Private  : in out Boolean;
      Level            : Natural);
   --  Called by Process_Source to work on the entires and entry
   --  families and pass each of them to the output subprogram
   --  Display_Private : indicate if we print the "Private" header.
   --  Private_Entity  : indicate if it must process private/public entities
   --  contained in Entity_List.

   procedure Process_Calls_References
     (B                : access Docgen.Backend.Backend'Class;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      Options          : All_Options;
      Info             : Entity_Information;
      Source_File_List : Type_Source_File_Table.HTable;
      Level            : Natural);
   procedure Process_Caller_References
     (B                : access Docgen.Backend.Backend'Class;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      Options          : All_Options;
      Info             : Entity_Information;
      Source_File_List : Type_Source_File_Table.HTable;
      Level            : Natural);
   --  For one subprogram: processes the output of the callgraph when this
   --  option is choosen.

   procedure Process_Subprograms
     (B                : access Docgen.Backend.Backend'Class;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      Parsed_List      : in out Construct_List;
      Entity_List      : in out Type_Entity_List.List;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Source_Filename  : VFS.Virtual_File;
      Unit_Info        : Entity_Information;
      File_Text        : GNAT.Strings.String_Access;
      Source_File_List : in out Type_Source_File_Table.HTable;
      Options          : All_Options;
      Private_Entity   : Boolean;
      Display_Private  : in out Boolean;
      Level            : Natural);
   --  Called by Process_Source to work on the subprograms and
   --  pass each of them to the output subprogram
   --  Private_Entity indicates if it must process private or public entites
   --  Display_Private is used to indicate if it's necessary to print the
   --  "Private" header

   procedure Process_One_Family
     (B                : access Docgen.Backend.Backend'Class;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      Family           : List_Entity_Information.List;
      Source_File_List : in out Type_Source_File_Table.HTable;
      Entity           : Entity_Information;
      Level            : Natural);
   --  For one tagged type: indicates its parents and children.

   procedure Process_Types
     (B                         : access Docgen.Backend.Backend'Class;
      Kernel                    : access Kernel_Handle_Record'Class;
      Result                    : in out Unbounded_String;
      Parsed_List               : in out Construct_List;
      Entity_List               : in out Type_Entity_List.List;
      List_Ref_In_File          : in out List_Reference_In_File.List;
      Tagged_Types_List         : in out List_Entity_Information.List;
      Private_Tagged_Types_List : in out List_Entity_Information.List;
      Source_Filename           : VFS.Virtual_File;
      Package_Info              : Entity_Information;
      File_Text                 : GNAT.Strings.String_Access;
      Source_File_List          : in out Type_Source_File_Table.HTable;
      Options                   : All_Options;
      Private_Entity            : Boolean;
      Display_Private           : in out Boolean;
      Level                     : Natural);
   --  Called by Process_Source to work on the types and
   --  pass each of them to the output subprogram
   --  Display_Private : indicate if we print the "Private" header.
   --  Private_Entity  : indicate if it must process private/public entities
   --  contained in Entity_List.

   procedure Process_Header
     (B               : access Docgen.Backend.Backend'Class;
      Kernel          : access Kernel_Handle_Record'Class;
      Result          : in out Unbounded_String;
      Entity_List     : Type_Entity_List.List;
      Source_Filename : VFS.Virtual_File;
      Package_Name    : String;
      Package_File    : VFS.Virtual_File;
      Options         : All_Options);
   --  Will call the output subprogram to create the header of
   --  the package. This is NOT the same as Process_Open_File,
   --  if TexInfo doc is created, the file is opened only once,
   --  but the Header has to be set in front of each package.

   procedure Process_Header_Private
     (B      : access Docgen.Backend.Backend'Class;
      Kernel : access Kernel_Handle_Record'Class;
      Result : in out Unbounded_String;
      Level  : Natural);
   --  Adds title "Private: " when private entities are required.

   procedure Process_Header_Packages
     (B      : access Docgen.Backend.Backend'Class;
      Kernel : access Kernel_Handle_Record'Class;
      Result : in out Unbounded_String;
      Level  : Natural);
   --  Adds title "Packages".

   procedure Process_Header_Vars
     (B      : access Docgen.Backend.Backend'Class;
      Kernel : access Kernel_Handle_Record'Class;
      Result : in out Unbounded_String;
      Level  : Natural);
   --  Adds title "Constants and Named Numbers"

   procedure Process_Header_Types
     (B      : access Docgen.Backend.Backend'Class;
      Kernel : access Kernel_Handle_Record'Class;
      Result : in out Unbounded_String;
      Level  : Natural);
   --  Adds title "Types"

   procedure Process_Header_Entries
     (B      : access Docgen.Backend.Backend'Class;
      Kernel : access Kernel_Handle_Record'Class;
      Result : in out Unbounded_String;
      Level  : Natural);
   --  Adds title "Entries"

   procedure Process_Header_Subprograms
     (B      : access Docgen.Backend.Backend'Class;
      Kernel : access Kernel_Handle_Record'Class;
      Result : in out Unbounded_String;
      Level  : Natural);
   --  Adds title "Subprograms"

   procedure Process_Header_Exceptions
     (B      : access Docgen.Backend.Backend'Class;
      Kernel : access Kernel_Handle_Record'Class;
      Result : in out Unbounded_String;
      Level  : Natural);
   --  Adds title "Exceptions"

   procedure Process_Footer
     (B      : access Docgen.Backend.Backend'Class;
      Kernel : access Kernel_Handle_Record'Class;
      Result : in out Unbounded_String);
   --  Will call the output subprogram to create the footer of
   --  the package. This is NOT the same as Doc_Close
   --  if TexInfo doc is created, the file is closed only once,
   --  but the Footer has to be set behind each package.

   function Entity_Defined_In_Package
     (Entity_Info       : Entity_Information;
      Package_Container : Entity_Information)
      return Boolean;
   --  Determine whether an entity is defined in a package.

   function Package_Contains_Entity_From_List
     (Package_Entity : Entity_Information;
      Entity_List    : Type_Entity_List.List)
      return Boolean;
   pragma Unreferenced (Package_Contains_Entity_From_List);
   --  Determine whether at least one entity from a list is defined in
   --  a package.

--     function Is_Ignorable_Comment
--       (Comment_Line : String) return Boolean;
   --  Return true, if the comment line starts with a "--!"
   --  It must be sure, that Comment_List is a comment line!

   function Get_Whole_Header
     (File_Text      : String;
      Parsed_List    : Construct_List;
      Entity         : Entity_List_Information;
      Case_Sensitive : Boolean)
      return String;
   --  Return the header of the declarative part of an entity.

   procedure Remove_Indent
     (Text       : String;
      Clean_Text : out GNAT.Strings.String_Access;
      Line_Count : out Natural);
   --  The header returned by Get_Whole_Header contains leading spaces. We
   --  the first line is well indented.
   --  Leading spaces are removed from the first line.
   --  Let's say N spaces have been removed.
   --  For the following lines, if there are at least N leading spaces,
   --  N leading spaces are removed, otherwise, the line is kept as is.

   --------------------
   -- Process_Source --
   --------------------

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
      Level                     : in out Natural)
   is
      use TEL;
      File_Text            : GNAT.Strings.String_Access;
      Entity_Node          : Type_Entity_List.List_Node;
      Found_Main_Unit      : Boolean;
      Parsed_List          : Construct_List;
      Entity               : TEL.Data_Access;

   begin
      File_Text := Read_File (Source_Filename);
      --  The whole file is stored in a string

      if File_Text = null then
         --  This is a non existing file
         return;
      end if;

      Process_Open_File (B, Kernel, Result, Unit_Name);
      Process_Header
        (B, Kernel, Result,
         Entity_List,
         Source_Filename,
         Unit_Name,
         Source_Filename,
         Options);

      --  Different processing for spec and body files
      if Source_Is_Spec then
         --  ??? Entity_List is always empty for files not compiled
         --  (no ALI file generated). This is true for glu_h.ads for example.

         if not TEL.Is_Empty (Entity_List) then
            --  Parse the source file and create the Parsed_List

            Parse_Constructs
              (Get_Language_From_File
                 (Get_Language_Handler (Kernel), Source_Filename),
               Locale_To_UTF8 (File_Text.all),
               Parsed_List);

            --  Find the main unit

            Found_Main_Unit := False;
            Entity_Node := TEL.First (Entity_List);

            while Entity_Node /= TEL.Null_Node loop
               Entity := TEL.Data_Ref (Entity_Node);

               if To_Lower (Get_Full_Name (Entity.Entity)) =
                 To_Lower (Unit_Name)
               then
                  Found_Main_Unit := True;
                  exit;
               end if;

               Entity_Node := TEL.Next (Entity_Node);
            end loop;

            if Found_Main_Unit then
               Process_File_Description
                 (B, Kernel, Result,
                  File_Text.all, Options,
                  Get_Language_From_File
                    (Get_Language_Handler (Kernel), Source_Filename),
                  Level);

               Process_With_Clause
                 (B, Kernel, Result,
                  Parsed_List,
                  List_Ref_In_File,
                  Source_Filename,
                  File_Text,
                  Source_File_List,
                  Options,
                  Level);

               --  Process types, variables, subprograms, entries,
               --  exceptions, packages (recursive calls for inner packages)

               Process_Source_Spec
                 (B, Kernel, Result,
                  Source_File_List,
                  Source_Filename,
                  Unit_Name,
                  Entity.Entity,
                  True,
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
           (B, Kernel, Result,
            List_Ref_In_File,
            Source_Filename,
            File_Text,
            Source_File_List,
            Options, Level);
      end if;

      Process_Footer (B, Kernel, Result);
      Doc_Close (B, Kernel, Result);
      GNAT.Strings.Free (File_Text);

   exception
      when E : others => Trace (Exception_Handle, E);
   end Process_Source;

   -------------------------
   -- Process_Source_Spec --
   -------------------------

   procedure Process_Source_Spec
     (B                         : access Docgen.Backend.Backend'Class;
      Kernel                    : access Kernel_Handle_Record'Class;
      Result                    : in out Unbounded_String;
      Source_File_List          : in out Type_Source_File_Table.HTable;
      Source_Filename           : VFS.Virtual_File;
      Unit_Name                 : String;
      Unit_Info                 : Entity_Information;
      Main_Unit                 : Boolean;
      Entity_List               : in out Type_Entity_List.List;
      List_Ref_In_File          : in out List_Reference_In_File.List;
      Tagged_Types_List         : in out List_Entity_Information.List;
      Private_Tagged_Types_List : in out List_Entity_Information.List;
      Options                   : All_Options;
      Level                     : in out Natural;
      File_Text                 : GNAT.Strings.String_Access;
      Parsed_List               : in out Construct_List)
   is
      use Type_Entity_List;
      Display_Private : Boolean := False;
   begin
      --  The 6 following calls have the value "False" for the
      --  parameter Private_Entity. So, only public entities are
      --  processed
      Process_Packages
        (B, Kernel, Result,
         Parsed_List,
         Entity_List,
         List_Ref_In_File,
         Tagged_Types_List,
         Private_Tagged_Types_List,
         Source_Filename,
         Unit_Name,
         Unit_Info,
         Main_Unit,
         File_Text,
         Source_File_List,
         Options,
         False, Display_Private,
         Level);
      Process_Vars
        (B, Kernel, Result,
         Parsed_List,
         Entity_List,
         List_Ref_In_File,
         Source_Filename,
         Unit_Info,
         File_Text,
         Source_File_List,
         Options,
         False, Display_Private,
         Level);
      Process_Exceptions
        (B, Kernel, Result,
         Parsed_List,
         Entity_List,
         List_Ref_In_File,
         Source_Filename,
         Unit_Info,
         File_Text,
         Source_File_List,
         Options,
         False, Display_Private,
         Level);
      Process_Types
        (B, Kernel, Result,
         Parsed_List,
         Entity_List,
         List_Ref_In_File,
         Tagged_Types_List,
         Private_Tagged_Types_List,
         Source_Filename,
         Unit_Info,
         File_Text,
         Source_File_List,
         Options,
         False, Display_Private,
         Level);
      Process_Entries
        (B, Kernel, Result,
         Parsed_List,
         Entity_List,
         List_Ref_In_File,
         Source_Filename,
         Unit_Info,
         File_Text,
         Source_File_List,
         Options,
         False, Display_Private,
         Level);
      Process_Subprograms
        (B, Kernel, Result,
         Parsed_List,
         Entity_List,
         List_Ref_In_File,
         Source_Filename,
         Unit_Info,
         File_Text,
         Source_File_List,
         Options,
         False, Display_Private,
         Level);

      if Options.Show_Private then
         --  Private entities are displayed. Hence, the value "True" is
         --  given to the parameter Private_Entity
         Process_Packages
           (B, Kernel, Result,
            Parsed_List,
            Entity_List,
            List_Ref_In_File,
            Tagged_Types_List,
            Private_Tagged_Types_List,
            Source_Filename,
            Unit_Name,
            Unit_Info,
            Main_Unit,
            File_Text,
            Source_File_List,
            Options,
            True, Display_Private,
            Level);
         Process_Vars
           (B, Kernel, Result,
            Parsed_List,
            Entity_List,
            List_Ref_In_File,
            Source_Filename,
            Unit_Info,
            File_Text,
            Source_File_List,
            Options,
            True, Display_Private,
            Level);
         Process_Exceptions
           (B, Kernel, Result,
            Parsed_List,
            Entity_List,
            List_Ref_In_File,
            Source_Filename,
            Unit_Info,
            File_Text,
            Source_File_List,
            Options,
            True, Display_Private,
            Level);
         Process_Types
           (B, Kernel, Result,
            Parsed_List,
            Entity_List,
            List_Ref_In_File,
            Tagged_Types_List,
            Private_Tagged_Types_List,
            Source_Filename,
            Unit_Info,
            File_Text,
            Source_File_List,
            Options,
            True, Display_Private,
            Level);
         Process_Entries
           (B, Kernel, Result,
            Parsed_List,
            Entity_List,
            List_Ref_In_File,
            Source_Filename,
            Unit_Info,
            File_Text,
            Source_File_List,
            Options,
            True, Display_Private,
            Level);
         Process_Subprograms
           (B, Kernel, Result,
            Parsed_List,
            Entity_List,
            List_Ref_In_File,
            Source_Filename,
            Unit_Info,
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
      Result       : in out Unbounded_String;
      Unit_Name    : String) is
   begin
      Doc_Open (B, Kernel, Result, Open_Title => Unit_Name);
   end Process_Open_File;

   ---------------------------
   -- Process_One_Body_File --
   ---------------------------

   procedure Process_One_Body_File
     (B                : access Docgen.Backend.Backend'Class;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Source_File      : Virtual_File;
      File_Text        : GNAT.Strings.String_Access;
      Source_File_List : Type_Source_File_Table.HTable;
      Options          : All_Options;
      Level            : Natural) is
   begin
      Doc_Body
        (B, Kernel, Result, List_Ref_In_File, Source_File_List,
         Options, Level,
         Body_Text => File_Text.all,
         Body_File => Source_File);
   end Process_One_Body_File;

   ------------------------
   -- Process_Unit_Index --
   ------------------------

   procedure Process_Unit_Index
     (B                : access Docgen.Backend.Backend'Class;
      Kernel           : access Kernel_Handle_Record'Class;
      Source_File_List : Docgen.Type_Source_File_Table.HTable;
      Options          : Docgen.All_Options;
      Level            : Natural)
   is
      use GNAT.OS_Lib;
      use Type_Source_File_Table;

      Source_File_Node : Type_Source_File_Table.Iterator;
      Index_File       : File_Descriptor;
      Doc_File_Name    : constant String := "index_unit";

      use Source_File_Arrays;

      Sources : Source_File_Arrays.Instance;
      Info             : Source_File_Information;
      Result           : Unbounded_String;

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
           (B, Kernel, Result,
            Source_File_List, Options, Level,
            Doc_Directory => Get_Doc_Directory (B, Kernel));
      end if;

      for S in Source_File_Arrays.First .. Last (Sources) loop
         Info := Get (Source_File_List, Sources.Table (S));

         if Info.Is_Spec then
            Doc_Index_Item
              (B, Kernel, Result,
               Name      => Get_Unit_Name (Sources.Table (S)),
               Item_File => Sources.Table (S),
               Line      => First_File_Line,
               Doc_File  => Info.Doc_File_Name.all);
         end if;
      end loop;

      Source_File_Arrays.Free (Sources);

      Doc_End_Of_Index (B, Kernel, Result);

      --  Write result to file

      Put_Line (Index_File, To_String (Result));
      Close (Index_File);
   end Process_Unit_Index;

   ------------------------------
   -- Process_Subprogram_Index --
   ------------------------------

   procedure Process_Subprogram_Index
     (B                             : access Docgen.Backend.Backend'Class;
      Kernel                        : access Kernel_Handle_Record'Class;
      Subprogram_Index_List         : Type_Entity_List.List;
      Private_Subprogram_Index_List : Type_Entity_List.List;
      Source_File_List              : Type_Source_File_Table.HTable;
      Options                       : All_Options)
   is
      use GNAT.OS_Lib;

      Doc_File_Name : constant String := "index_sub";
      Index_File    : File_Descriptor;
      Result        : Unbounded_String;

      procedure Process_List
        (List   : Type_Entity_List.List;
         Public : Boolean);

      ------------------
      -- Process_List --
      ------------------

      procedure Process_List
        (List   : Type_Entity_List.List;
         Public : Boolean)
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
                  Doc_Public_Index (B, Kernel, Result, Title => "Public:");
               else
                  Doc_Private_Index
                    (B, Kernel, Result, Title => "Private:");
               end if;
            end if;

            Subprogram_Index_Node := TEL.First (List);

            while Subprogram_Index_Node /= TEL.Null_Node loop
               Entity := TEL.Data_Ref (Subprogram_Index_Node);
               Source := Get_File (Get_Declaration_Of (Entity.Entity));
               Info   := Type_Source_File_Table.Get (Source_File_List, Source);
               Doc_Index_Item
                 (B, Kernel, Result,
                  Name      => Get_Name (Entity.Entity).all,
                  Item_File => Source,
                  Line      => Get_Line (Get_Declaration_Of (Entity.Entity)),
                  Doc_File  => Info.Doc_File_Name.all);

               Subprogram_Index_Node := TEL.Next (Subprogram_Index_Node);
            end loop;
         end if;
      end Process_List;

   begin
      Doc_Subprogram_Index (B, Kernel, Result, Options);

      Process_List (Subprogram_Index_List, Public => True);

      if Options.Show_Private then
         Process_List (Private_Subprogram_Index_List, Public => False);
      end if;

      Doc_End_Of_Index (B, Kernel, Result);

      --  Write result into index file

      Index_File := Create_File
        (Get_Doc_Directory (B, Kernel) & Doc_File_Name & Get_Extension (B),
         Binary);

      Put_Line (Index_File, To_String (Result));

      Close (Index_File);
   end Process_Subprogram_Index;

   ------------------------
   -- Process_Type_Index --
   ------------------------

   procedure Process_Type_Index
     (B                       : access Docgen.Backend.Backend'Class;
      Kernel                  : access Kernel_Handle_Record'Class;
      Type_Index_List         : Docgen.Type_Entity_List.List;
      Private_Type_Index_List : Type_Entity_List.List;
      Source_File_List        : Type_Source_File_Table.HTable;
      Options                 : All_Options)
   is
      use GNAT.OS_Lib;

      Doc_File_Name : constant String := "index_type";
      Index_File    : File_Descriptor;
      Result        : Unbounded_String;

      procedure Process_List
        (List : Type_Entity_List.List; Public : Boolean);

      ------------------
      -- Process_List --
      ------------------

      procedure Process_List
        (List   : Type_Entity_List.List;
         Public : Boolean)
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
                  Doc_Public_Index (B, Kernel, Result, Title => "Public:");
               else
                  Doc_Private_Index (B, Kernel, Result, "Private:");
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
              (B, Kernel, Result,
               Name => Get_Name (Entity.Entity).all,
               Item_File => File,
               Line      => Get_Line (Get_Declaration_Of (Entity.Entity)),
               Doc_File  => Info.Doc_File_Name.all);
            Type_Index_Node := TEL.Next (Type_Index_Node);
         end loop;
      end Process_List;

   begin
      Doc_Type_Index (B, Kernel, Result, Options);

      Process_List (Type_Index_List, Public => True);

      if Options.Show_Private then
         Process_List (Private_Type_Index_List, Public => False);
      end if;

      Doc_End_Of_Index (B, Kernel, Result);

      --  Write result into index file

      Index_File := Create_File
        (Get_Doc_Directory (B, Kernel) & Doc_File_Name & Get_Extension (B),
         Binary);
      Put_Line (Index_File, To_String (Result));
      Close (Index_File);
   end Process_Type_Index;

   -------------------------------
   -- Process_Tagged_Type_Index --
   -------------------------------

   procedure Process_Tagged_Type_Index
     (B                         : access Docgen.Backend.Backend'Class;
      Kernel                    : access Kernel_Handle_Record'Class;
      Tagged_Type_Index_List    : Docgen.List_Entity_Information.List;
      Private_Tagged_Types_List : List_Entity_Information.List;
      Source_File_List          : Type_Source_File_Table.HTable;
      Options                   : All_Options)
   is
      use GNAT.OS_Lib;

      Doc_File_Name : constant String := "index_tagged_type";
      Index_File    : File_Descriptor;
      Result        : Unbounded_String;

      procedure Process_Parents_Or_Children
        (Members             : Entity_Information_Array;
         Member_With_Link    : Family_Type;
         Member_Without_Link : Family_Type;
         No_Member           : Family_Type);
      procedure Process_Parents  (Info : Entity_Information);
      procedure Process_Children (Info : Entity_Information);
      --  Output references to the parents and children of a tagged type

      procedure Process_Type (Entity : Entity_Information);
      --  Process a tagged type itself

      procedure Process_List
        (List   : List_Entity_Information.List;
         Public : Boolean);
      --  Process a list of tagged types

      ---------------------
      -- Process_Parents --
      ---------------------

      procedure Process_Parents_Or_Children
        (Members             : Entity_Information_Array;
         Member_With_Link    : Family_Type;
         Member_Without_Link : Family_Type;
         No_Member           : Family_Type) is
      begin
         if Members'Length /= 0 then
            for M in Members'Range loop
               if Source_File_In_List
                 (Source_File_List,
                  Get_File (Get_Declaration_Of (Members (M))))
               then
                  Doc_Index_Tagged_Type
                    (B, Kernel, Result, Source_File_List, Members (M),
                     Member_With_Link);
               else
                  Doc_Index_Tagged_Type
                    (B, Kernel, Result, Source_File_List, Members (M),
                     Member_Without_Link);
               end if;
            end loop;

         else
            Doc_Index_Tagged_Type
              (B, Kernel, Result, Source_File_List, null, No_Member);
         end if;
      end Process_Parents_Or_Children;

      ---------------------
      -- Process_Parents --
      ---------------------

      procedure Process_Parents (Info : Entity_Information) is
         Parents : constant Entity_Information_Array :=
                     Get_Parent_Types (Info);
      begin
         Process_Parents_Or_Children
           (Parents, Parent_With_Link, Parent_Without_Link, No_Parent);
      end Process_Parents;

      ----------------------
      -- Process_Children --
      ----------------------

      procedure Process_Children (Info : Entity_Information) is
         Iter        : Children_Iterator := Get_Child_Types (Info);
         Nb_Children : Natural := 0;
      begin
         while not At_End (Iter) loop
            if Get (Iter) /= null then
               Nb_Children := Nb_Children + 1;
            end if;

            Next (Iter);
         end loop;

         declare
            Children : Entity_Information_Array (1 .. Nb_Children);
         begin
            Nb_Children := 0;
            Iter := Get_Child_Types (Info);

            while not At_End (Iter) loop
               if Get (Iter) /= null then
                  Nb_Children := Nb_Children + 1;
                  Children (Nb_Children) := Get (Iter);
               end if;

               Next (Iter);
            end loop;

            Process_Parents_Or_Children
              (Children, Child_With_Link, Child_Without_Link, No_Child);
         end;
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
              (B, Kernel, Result, Source_File_List, Entity, Main);
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
         Node : List_Entity_Information.List_Node;
      begin
         if not List_Entity_Information.Is_Empty (List) then
            if Options.Show_Private then
               if Public then
                  Doc_Public_Index (B, Kernel, Result, "Public:");
               else
                  Doc_Private_Index (B, Kernel, Result, "Private:");
               end if;
            end if;

            Node := List_Entity_Information.First (List);

            while Node /= List_Entity_Information.Null_Node loop
               Process_Type (List_Entity_Information.Data (Node));
               Node := List_Entity_Information.Next (Node);
            end loop;
         end if;
      end Process_List;

   begin
      Doc_Tagged_Type_Index (B, Kernel, Result);

      Process_List (Tagged_Type_Index_List, Public => True);

      if Options.Show_Private then
         Process_List (Private_Tagged_Types_List, Public => False);
      end if;

      Doc_End_Of_Index (B, Kernel, Result);

      --  Write result to index file

      Index_File := Create_File
        (Get_Doc_Directory (B, Kernel) & Doc_File_Name & Get_Extension (B),
         Binary);
      Put_Line (Index_File, To_String (Result));
      Close (Index_File);
   end Process_Tagged_Type_Index;

   --------------------
   -- Process_Header --
   --------------------

   procedure Process_Header
     (B               : access Docgen.Backend.Backend'Class;
      Kernel          : access Kernel_Handle_Record'Class;
      Result          : in out Unbounded_String;
      Entity_List     : Type_Entity_List.List;
      Source_Filename : VFS.Virtual_File;
      Package_Name    : String;
      Package_File    : Virtual_File;
      Options         : All_Options)
   is
      pragma Unreferenced (Entity_List, Package_Name);
      use TEL;
--        Declar_Line : Natural := First_File_Line;
--        Entity_Node : Type_Entity_List.List_Node;
--        Entity      : TEL.Data_Access;
   begin
--        if not TEL.Is_Empty (Entity_List) then
--           Entity_Node := TEL.First (Entity_List);
--
--           while Entity_Node /= TEL.Null_Node loop
--              Entity := TEL.Data_Ref (Entity_Node);
--
--              if Entity.Kind = Package_Entity
--                and then Get_Name (Entity.Entity).all = Package_Name
--              then
--                 --  It's a library level package declaration
--              if Get_Filename (Get_File (Get_Declaration_Of (Entity.Entity)))
--                   = Source_Filename
--                 then
--                    --  Clauses with may be above the declaration
--                    --  of the main package
--                Declar_Line := Get_Line (Get_Declaration_Of (Entity.Entity));
--                 else
--                    Declar_Line := Get_Line (Entity.Line_In_Body);
--                 end if;
--
--                 exit;
--              end if;
--
--              Entity_Node := TEL.Next (Entity_Node);
--           end loop;
--        end if;

      Doc_Header (B, Kernel, Result,
                  Header_File    => Package_File,
                  Header_Package => Base_Name (Source_Filename), --  ???
--                    Header_Line    => Declar_Line,
                  Header_Line    => 1,
                  Header_Link    => Options.Process_Body_Files
                     and then Other_File_Base_Name
                       (Get_Project_From_File
                         (Project_Registry (Get_Registry (Kernel).all),
                          Source_Filename),
                        Source_Filename) /= Base_Name (Source_Filename));
   end Process_Header;

   ------------------------------
   --  Process_Header_Private  --
   ------------------------------

   procedure Process_Header_Private
     (B      : access Docgen.Backend.Backend'Class;
      Kernel : access Kernel_Handle_Record'Class;
      Result : in out Unbounded_String;
      Level  : Natural) is
   begin
      Doc_Header_Private
        (B, Kernel, Result,
         Header_Title => "Private:",
         Level        => Level);
   end Process_Header_Private;

   --------------------
   -- Process_Footer --
   --------------------

   procedure Process_Footer
     (B      : access Docgen.Backend.Backend'Class;
      Kernel : access Kernel_Handle_Record'Class;
      Result : in out Unbounded_String) is
   begin
      Doc_Footer (B, Kernel, Result);
   end Process_Footer;

   ---------------------------------
   -- Process_Package_Description --
   ---------------------------------

   procedure Process_File_Description
     (B        : access Docgen.Backend.Backend'Class;
      Kernel   : access Kernel_Handle_Record'Class;
      Result   : in out Unbounded_String;
      Text     : String;
      Options  : All_Options;
      Language : Language_Access;
      Level    : Natural)
   is
      pragma Unreferenced (Options);

      Description : GNAT.Strings.String_Access;
      Start_Line  : Natural := Text'First;
      End_Line    : Natural := Start_Line;

   begin
      Skip_Blanks (Text, Start_Line);
      End_Line := Start_Line;
      Skip_To_Current_Comment_Block_End
        (Get_Language_Context (Language).all, Text, End_Line, True);

      if End_Line > Start_Line then
         --  Text actually begins with comments
         Description := new String'
           (Text (Line_Start (Text, Start_Line) .. Line_End (Text, End_Line)));
      else
         Description := new String'("No description available");
      end if;

      Doc_Subtitle
        (B, Kernel, Result, Level, Subtitle_Name => "Description");

      Doc_Package_Desc
        (B, Kernel, Result, Level, Description => Description.all);
      GNAT.Strings.Free (Description);
   end Process_File_Description;

   -------------------------
   -- Process_With_Clause --
   -------------------------

   procedure Process_With_Clause
     (B                : access Docgen.Backend.Backend'Class;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      Parsed_List      : in out Construct_List;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Source_Filename  : VFS.Virtual_File;
      File_Text        : GNAT.Strings.String_Access;
      Source_File_List : in out Type_Source_File_Table.HTable;
      Options          : All_Options;
      Level            : Natural)
   is
      Parse_Node         : Construct_Access := Parsed_List.First;
      First_With_Line    : Natural := 0;
      Start_Index        : Natural := 0;
      End_Index          : Natural;
   begin
      while Parse_Node.Category = Cat_With loop
         if First_With_Line = 0 then
            Start_Index := Parse_Node.Sloc_Start.Index;
            First_With_Line := Parse_Node.Sloc_Start.Line;
         end if;
         End_Index := Parse_Node.Sloc_End.Index;

         exit when Parse_Node = Parsed_List.Last;
         Parse_Node := Parse_Node.Next;
      end loop;

      if Start_Index > 0 then
         Doc_Subtitle
           (B, Kernel, Result, Level,
            Subtitle_Name => "Dependencies");
         Doc_With
           (B, Kernel, Result,
            List_Ref_In_File, Source_File_List,
            Options, Level,
            With_Header_Line => First_With_Line,
            With_File        => Source_Filename,
            With_Header      => File_Text (Start_Index .. End_Index));
      end if;
   end Process_With_Clause;

   -----------------------------
   -- Process_Header_Packages --
   -----------------------------

   procedure Process_Header_Packages
     (B      : access Docgen.Backend.Backend'Class;
      Kernel : access Kernel_Handle_Record'Class;
      Result : in out Unbounded_String;
      Level  : Natural) is
   begin
      Doc_Subtitle (B, Kernel, Result, Level, Subtitle_Name => "Packages");
   end Process_Header_Packages;

   -------------------------
   -- Process_Description --
   -------------------------

   procedure Process_Description
     (B       : access Docgen.Backend.Backend'Class;
      Kernel  : access Kernel_Handle_Record'Class;
      Result  : in out Unbounded_String;
      Level   : Natural;
      Comment : String) is
   begin
      Doc_Description (B, Kernel, Result, Level, Description => Comment);
   end Process_Description;

   ----------------------
   -- Process_Packages --
   ----------------------

   procedure Process_Packages
     (B                         : access Docgen.Backend.Backend'Class;
      Kernel                    : access Kernel_Handle_Record'Class;
      Result                    : in out Unbounded_String;
      Parsed_List               : in out Construct_List;
      Entity_List               : in out Type_Entity_List.List;
      List_Ref_In_File          : in out List_Reference_In_File.List;
      Tagged_Types_List         : in out List_Entity_Information.List;
      Private_Tagged_Types_List : in out List_Entity_Information.List;
      Source_Filename           : VFS.Virtual_File;
      Package_Name              : String;
      Package_Information       : Entity_Information;
      Main_Unit                 : Boolean;
      File_Text                 : GNAT.Strings.String_Access;
      Source_File_List          : in out Type_Source_File_Table.HTable;
      Options                   : All_Options;
      Private_Entity            : Boolean;
      Display_Private           : in out Boolean;
      Level                     : in out Natural)
   is
      pragma Unreferenced (Package_Name);
      use TEL;
      Case_Sensitive    : constant Boolean :=
                             Get_Language_Context
                               (Get_Language_From_File
                                 (Get_Language_Handler (Kernel),
                                  Source_Filename)).Case_Sensitive;
      Entity_Node       : Type_Entity_List.List_Node;
      Description       : GNAT.Strings.String_Access;
      First_Already_Set : Boolean := False;
      Entity            : TEL.Data_Access;
      Renamed           : Entity_Information;

   begin
      if Entity_List /= TEL.Null_List
        and then not TEL.Is_Empty (Entity_List)
      then
         First_Already_Set := False;
         Entity_Node       := TEL.First (Entity_List);

         loop
            Entity := TEL.Data_Ref (Entity_Node);

            if not Entity.Processed
              and then Entity.Is_Private = Private_Entity
              and then Entity.Kind = Package_Entity
              and then Source_Filename =
                Get_Filename (Get_File (Get_Declaration_Of (Entity.Entity)))
              and then
                ((Main_Unit
                  and then Entity.Entity.all = Package_Information.all)
                 or else
                   (not Main_Unit
                    and then
                      (Get_Name
                         (Get_Caller
                            (Declaration_As_Reference
                               (Entity.Entity))) /= null
                       and then
                         Get_Name
                           (Get_Caller
                              (Declaration_As_Reference (Entity.Entity))).all =
                         Get_Name (Package_Information).all
                       and then Entity_Defined_In_Package
                         (Entity.Entity, Package_Information))))
            then
               Entity.Processed := True;

               if Entity.Is_Private and then not Display_Private then
                  --  Print title "private" required and not done
                  Process_Header_Private (B, Kernel, Result, Level);
                  Display_Private := True;
               end if;

               --  Check if the subtitle "Packages" has already been
               --  set.

               if not First_Already_Set then -- ??? To be renamed.
                  Process_Header_Packages (B, Kernel, Result, Level);
                  First_Already_Set := True;
               end if;

               Renamed := Renaming_Of (Entity.Entity);

               if Renamed = null then
                  Doc_Package_Open_Close
                    (B, Kernel, Result,
                     List_Ref_In_File,
                     Source_File_List,
                     Options,
                     Level,
                     Entity => Entity.Entity,
                     Header =>
                       "package " & Get_Name (Entity.Entity).all & " is");

                  Description := new String'
                    (Get_Documentation
                       (Get_Language_Handler (Kernel),
                        Entity.Entity,
                        File_Text.all));

                  Level := Level + 1;

                  if Description.all /= "" then
                     Doc_Subtitle
                       (B, Kernel, Result, Level,
                        Subtitle_Name => "Description");
                     Process_Description
                       (B, Kernel, Result,
                        Level,
                        Description.all);
                  end if;

                  --  Recursive call in order to deal with entities defined
                  --  in the current package.

                  Process_Source_Spec
                    (B, Kernel, Result,
                     Source_File_List,
                     Source_Filename,
                     Get_Name (Entity.Entity).all,
                     Entity.Entity,
                     False,
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
                    (B, Kernel, Result,
                     List_Ref_In_File,
                     Source_File_List,
                     Options,
                     Level,
                     Entity => Entity.Entity,
                     Header => "end " & Get_Name (Entity.Entity).all & ";");
               else
                  --  The current entity is a package that renames another one.
                  declare
                     Whole_Header : constant String :=
                       Get_Whole_Header
                         (File_Text.all,
                          Parsed_List,
                          Entity.all,
                          Case_Sensitive);
                     Header       : GNAT.Strings.String_Access;
                     Header_Lines : Natural;
                  begin
                     --  Check if it was an entity with its own header

                     if Whole_Header /= "" then
                        Remove_Indent (Whole_Header, Header, Header_Lines);

                        Description := new String'
                          (Get_Documentation
                             (Get_Language_Handler (Kernel),
                              Entity.Entity,
                              File_Text.all));

                        Doc_Package
                          (B, Kernel, Result,
                           List_Ref_In_File,
                           Source_File_List,
                           Options,
                           Level,
                           Entity.Entity,
                           Header.all);

                        if Description.all /= "" then
                           Doc_Subtitle
                             (B, Kernel, Result, Level,
                              Subtitle_Name => "Description");
                           Process_Description
                             (B, Kernel, Result,
                              Level,
                              Description.all);
                        end if;

                        GNAT.Strings.Free (Header);
                        GNAT.Strings.Free (Description);
                     end if;
                  end;
               end if;
            end if;

            exit when Entity_Node = TEL.Last (Entity_List);

            Entity_Node := TEL.Next (Entity_Node);
         end loop;
      end if;
   exception
      when E : others => Trace (Exception_Handle, E);
   end Process_Packages;

   -------------------------
   -- Process_Header_Vars --
   -------------------------

   procedure Process_Header_Vars
     (B      : access Docgen.Backend.Backend'Class;
      Kernel : access Kernel_Handle_Record'Class;
      Result : in out Unbounded_String;
      Level  : Natural) is
   begin
      Doc_Subtitle (B, Kernel, Result, Level, Subtitle_Name => "Constants");
   end Process_Header_Vars;

   ------------------
   -- Process_Vars --
   ------------------

   procedure Process_Vars
     (B                : access Docgen.Backend.Backend'Class;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      Parsed_List      : in out Construct_List;
      Entity_List      : in out Type_Entity_List.List;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Source_Filename  : VFS.Virtual_File;
      Package_Info     : Entity_Information;
      File_Text        : GNAT.Strings.String_Access;
      Source_File_List : in out Type_Source_File_Table.HTable;
      Options          : All_Options;
      Private_Entity   : Boolean;
      Display_Private  : in out Boolean;
      Level            : Natural)
   is
      use TEL;
      Case_Sensitive    : constant Boolean :=
                             Get_Language_Context
                               (Get_Language_From_File
                                 (Get_Language_Handler (Kernel),
                                  Source_Filename)).Case_Sensitive;
      Entity_Node       : Type_Entity_List.List_Node;
      Description       : GNAT.Strings.String_Access;
      Header            : GNAT.Strings.String_Access;
      Header_Lines      : Natural;
      First_Already_Set : Boolean;
      Entity            : TEL.Data_Access;

   begin
      if not TEL.Is_Empty (Entity_List) then
         First_Already_Set := False;
         Entity_Node := TEL.First (Entity_List);

         while Entity_Node /= TEL.Null_Node loop
            Entity := TEL.Data_Ref (Entity_Node);

            if not Entity.Processed
              and then Entity.Is_Private = Private_Entity
              and then Entity.Kind = Var_Entity
              and then Source_Filename =
                Get_Filename (Get_File (Get_Declaration_Of (Entity.Entity)))
              and then Entity_Defined_In_Package (Entity.Entity, Package_Info)
            then
               Entity.Processed := True;

               declare
                  Whole_Header : constant String :=
                    Get_Whole_Header
                      (File_Text.all,
                       Parsed_List,
                       Entity.all,
                       Case_Sensitive);
               begin
               --  Check if it was an entity with its own header

                  if Whole_Header /= "" then
                     Remove_Indent (Whole_Header, Header, Header_Lines);

                     if Entity.Is_Private and then not Display_Private then
                        --  It's the first time we met a private entity
                        --  and we work on the private part, so we put the
                        --  title "Private"

                        Process_Header_Private (B, Kernel, Result, Level);
                        Display_Private := True;
                     end if;

                     --  Check if the subtitle "Constand and Named Numbers:"
                     --  has been set already.

                     if not First_Already_Set then
                        Process_Header_Vars (B, Kernel, Result, Level);
                        First_Already_Set := True;
                     end if;

                     Description := new String'
                       (Get_Documentation
                          (Get_Language_Handler (Kernel), Entity.Entity,
                           File_Text.all));

                     Doc_Var
                       (B, Kernel, Result, List_Ref_In_File,
                        Source_File_List, Options, Level,
                        Entity => Entity.Entity, Header => Header.all);

                     if Description.all /= "" then
                        Process_Description
                          (B, Kernel, Result, Level, Description.all);
                     end if;

                     GNAT.Strings.Free (Header);
                     GNAT.Strings.Free (Description);
                  end if;
               end;
            end if;

            exit when Entity_Node = TEL.Last (Entity_List);

            Entity_Node := TEL.Next (Entity_Node);
         end loop;
      end if;
   end Process_Vars;

   -------------------------------
   -- Process_Header_Exceptions --
   -------------------------------

   procedure Process_Header_Exceptions
     (B      : access Docgen.Backend.Backend'Class;
      Kernel : access Kernel_Handle_Record'Class;
      Result : in out Unbounded_String;
      Level  : Natural) is
   begin
      Doc_Subtitle (B, Kernel, Result, Level, Subtitle_Name => "Exceptions");
   end Process_Header_Exceptions;

   ------------------------
   -- Process_Exceptions --
   ------------------------

   procedure Process_Exceptions
     (B                : access Docgen.Backend.Backend'Class;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      Parsed_List      : in out Construct_List;
      Entity_List      : in out Type_Entity_List.List;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Source_Filename  : VFS.Virtual_File;
      Package_Info     : Entity_Information;
      File_Text        : GNAT.Strings.String_Access;
      Source_File_List : in out Type_Source_File_Table.HTable;
      Options          : All_Options;
      Private_Entity   : Boolean;
      Display_Private  : in out Boolean;
      Level            : Natural)
   is
      use TEL;
      Case_Sensitive    : constant Boolean :=
                             Get_Language_Context
                               (Get_Language_From_File
                                 (Get_Language_Handler (Kernel),
                                  Source_Filename)).Case_Sensitive;
      Entity_Node       : Type_Entity_List.List_Node;
      Description       : GNAT.Strings.String_Access;
      Header            : GNAT.Strings.String_Access;
      Header_Lines      : Natural;
      First_Already_Set : Boolean;
      Entity            : TEL.Data_Access;

   begin
      if not TEL.Is_Empty (Entity_List) then
         First_Already_Set := False;
         Entity_Node := TEL.First (Entity_List);

         while Entity_Node /= TEL.Null_Node loop
            Entity := TEL.Data_Ref (Entity_Node);

            if not Entity.Processed
              and then Entity.Is_Private = Private_Entity
              and then Entity.Kind = Exception_Entity
              and then Source_Filename =
                Get_Filename (Get_File (Get_Declaration_Of (Entity.Entity)))
              and then Entity_Defined_In_Package (Entity.Entity, Package_Info)
            then
               Entity.Processed := True;

               declare
                  Whole_Header : constant String :=
                    Get_Whole_Header
                      (File_Text.all,
                       Parsed_List,
                       Entity.all,
                       Case_Sensitive);
               begin
                  --  Check if it was a entity with its own header

                  if Whole_Header /= "" then
                     Remove_Indent (Whole_Header, Header, Header_Lines);

                     if Entity.Is_Private and then not Display_Private then
                        Process_Header_Private (B, Kernel, Result, Level);
                        Display_Private := True;
                     end if;

                     --  Check if the subtitle "Exceptions:" has been set
                     --  already.

                     if not First_Already_Set then
                        Process_Header_Exceptions (B, Kernel, Result, Level);
                        First_Already_Set := True;
                     end if;

                     Description := new String'
                       (Get_Documentation
                          (Get_Language_Handler (Kernel), Entity.Entity,
                           File_Text.all));

                     Doc_Exception
                       (B, Kernel, Result,
                        List_Ref_In_File, Source_File_List, Options, Level,
                        Entity => Entity.Entity,
                        Header => Header.all);

                     if Description.all /= "" then
                        Process_Description
                          (B, Kernel, Result, Level, Description.all);
                     end if;

                     GNAT.Strings.Free (Header);
                     GNAT.Strings.Free (Description);
                  end if;
               end;
            end if;

            exit when Entity_Node = TEL.Last (Entity_List);

            Entity_Node := TEL.Next (Entity_Node);
         end loop;
      end if;
   end Process_Exceptions;

   --------------------------
   -- Process_Header_Types --
   --------------------------

   procedure Process_Header_Types
     (B      : access Docgen.Backend.Backend'Class;
      Kernel : access Kernel_Handle_Record'Class;
      Result : in out Unbounded_String;
      Level  : Natural) is
   begin
      Doc_Subtitle (B, Kernel, Result, Level, Subtitle_Name => "Types");
   end Process_Header_Types;

   ------------------------
   -- Process_One_Family --
   ------------------------

   procedure Process_One_Family
     (B                : access Docgen.Backend.Backend'Class;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      Family           : List_Entity_Information.List;
      Source_File_List : in out Type_Source_File_Table.HTable;
      Entity           : Entity_Information;
      Level            : Natural)
   is
      use List_Entity_Information;
      Node          : List_Entity_Information.List_Node;
      Tagged_Entity : Entity_Information;

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
                 (B, Kernel, Result, Source_File_List, Level, Entity);
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
      Result                    : in out Unbounded_String;
      Parsed_List               : in out Construct_List;
      Entity_List               : in out Type_Entity_List.List;
      List_Ref_In_File          : in out List_Reference_In_File.List;
      Tagged_Types_List         : in out List_Entity_Information.List;
      Private_Tagged_Types_List : in out List_Entity_Information.List;
      Source_Filename           : VFS.Virtual_File;
      Package_Info              : Entity_Information;
      File_Text                 : GNAT.Strings.String_Access;
      Source_File_List          : in out Type_Source_File_Table.HTable;
      Options                   : All_Options;
      Private_Entity            : Boolean;
      Display_Private           : in out Boolean;
      Level                     : Natural)
   is
      use TEL;
      Case_Sensitive    : constant Boolean :=
                             Get_Language_Context
                               (Get_Language_From_File
                                 (Get_Language_Handler (Kernel),
                                  Source_Filename)).Case_Sensitive;
      Entity_Node       : Type_Entity_List.List_Node;
      Description       : GNAT.Strings.String_Access;
      Header            : GNAT.Strings.String_Access;
      Header_Lines      : Natural;
      First_Already_Set : Boolean;
      Entity            : TEL.Data_Access;
      Kind              : E_Kinds;
      Info              : Entity_Information;

   begin
      if not TEL.Is_Empty (Entity_List) then
         First_Already_Set := False;
         Entity_Node      := TEL.First (Entity_List);

         while Entity_Node /= TEL.Null_Node loop
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

            if not Entity.Processed
              and then Entity.Is_Private = Private_Entity
              and then Entity.Kind = Type_Entity
              and then Source_Filename =
                Get_Filename (Get_File (Get_Declaration_Of (Entity.Entity)))
              and then Entity_Defined_In_Package (Info, Package_Info)
            then
               Entity.Processed := True;

               declare
                  Whole_Header : constant String :=
                    Get_Whole_Header
                      (File_Text.all,
                       Parsed_List,
                       Entity.all,
                       Case_Sensitive);
               begin
                  --  Check if it was a entity with its own header

                  if Whole_Header /= "" then
                     Remove_Indent (Whole_Header, Header, Header_Lines);

                     if Entity.Is_Private and then not Display_Private then
                        --  It's the first time we met a private entity
                        --  and we work on the private part, so we put the
                        --  title "Private"

                        Process_Header_Private (B, Kernel, Result, Level);
                        Display_Private := True;
                     end if;

                     --  Check if the subtitle "Types:" has to be set.

                     if not First_Already_Set then
                        Process_Header_Types (B, Kernel, Result, Level);
                        First_Already_Set := True;
                     end if;

                     Description := new String'
                       (Get_Documentation
                          (Get_Language_Handler (Kernel), Entity.Entity,
                           File_Text.all));

                     Doc_Type
                       (B, Kernel, Result,
                        List_Ref_In_File, Source_File_List, Options, Level,
                        Entity => Entity.Entity,
                        Header => Header.all);

                     if Description.all /= "" then
                        Process_Description
                          (B, Kernel, Result, Level, Description.all);
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
                             (B, Kernel, Result,
                              Private_Tagged_Types_List,
                              Source_File_List,
                              Entity.Entity,
                              Level);
                        else
                           --  List of public tagged types

                           Process_One_Family
                             (B, Kernel, Result,
                              Tagged_Types_List,
                              Source_File_List,
                              Entity.Entity,
                              Level);
                        end if;
                     end if;

                     GNAT.Strings.Free (Header);
                     GNAT.Strings.Free (Description);
                  end if;
               end;
            end if;

            exit when Entity_Node = TEL.Last (Entity_List);

            Entity_Node := TEL.Next (Entity_Node);
         end loop;
      end if;
   end Process_Types;

   ----------------------------
   -- Process_Header_Entries --
   ----------------------------

   procedure Process_Header_Entries
     (B      : access Docgen.Backend.Backend'Class;
      Kernel : access Kernel_Handle_Record'Class;
      Result : in out Unbounded_String;
      Level  : Natural) is
   begin
      Doc_Subtitle (B, Kernel, Result, Level,
                    Subtitle_Name => "Tasking");
   end Process_Header_Entries;

   ---------------------
   -- Process_Entries --
   ---------------------

   procedure Process_Entries
     (B                : access Docgen.Backend.Backend'Class;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      Parsed_List      : in out Construct_List;
      Entity_List      : in out Type_Entity_List.List;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Source_Filename  : VFS.Virtual_File;
      Package_Info     : Entity_Information;
      File_Text        : GNAT.Strings.String_Access;
      Source_File_List : in out Type_Source_File_Table.HTable;
      Options          : All_Options;
      Private_Entity   : Boolean;
      Display_Private  : in out Boolean;
      Level            : Natural)
   is
      use TEL;
      Case_Sensitive    : constant Boolean :=
                             Get_Language_Context
                               (Get_Language_From_File
                                 (Get_Language_Handler (Kernel),
                                  Source_Filename)).Case_Sensitive;
      Entity_Node       : Type_Entity_List.List_Node;
      Description       : GNAT.Strings.String_Access;
      Header            : GNAT.Strings.String_Access;
      Header_Lines      : Natural;
      First_Already_Set : Boolean;
      Entity            : TEL.Data_Access;

   begin
      if not TEL.Is_Empty (Entity_List) then
         First_Already_Set := False;
         Entity_Node := TEL.First (Entity_List);

         while Entity_Node /= TEL.Null_Node loop
            Entity := TEL.Data_Ref (Entity_Node);

            if not Entity.Processed
              and then Entity.Is_Private = Private_Entity
              and then Entity.Kind = Entry_Entity
              and then Source_Filename =
                Get_Filename (Get_File (Get_Declaration_Of (Entity.Entity)))
              and then Entity_Defined_In_Package (Entity.Entity, Package_Info)
            then
               Entity.Processed := True;

               declare
                  Whole_Header : constant String :=
                    Get_Whole_Header
                      (File_Text.all,
                       Parsed_List,
                       Entity.all,
                       Case_Sensitive);
               begin
                  --  Check if it was a entity with its own header

                  if Whole_Header /= "" then
                     Remove_Indent (Whole_Header, Header, Header_Lines);

                     if Entity.Is_Private and then not Display_Private then
                        Process_Header_Private (B, Kernel, Result, Level);
                        Display_Private := True;
                     end if;

                     --  Check if the subtitle has to be set.

                     if not First_Already_Set then
                        Process_Header_Entries (B, Kernel, Result, Level);
                        First_Already_Set := True;
                     end if;

                     Description := new String'
                       (Get_Documentation
                          (Get_Language_Handler (Kernel),
                           Entity.Entity, File_Text.all));

                     Doc_Entry
                       (B, Kernel, Result, List_Ref_In_File,
                        Source_File_List, Options, Level,
                        Entity => Entity.Entity,
                        Header => Header.all);

                     if Description.all /= "" then
                        Process_Description
                          (B, Kernel, Result, Level, Description.all);
                     end if;

                     GNAT.Strings.Free (Header);
                     GNAT.Strings.Free (Description);
                  end if;
               end;
            end if;

            exit when Entity_Node = TEL.Last (Entity_List);

            Entity_Node := TEL.Next (Entity_Node);
         end loop;
      end if;
   end Process_Entries;

   --------------------------------
   -- Process_Header_Subprograms --
   --------------------------------

   procedure Process_Header_Subprograms
     (B      : access Docgen.Backend.Backend'Class;
      Kernel : access Kernel_Handle_Record'Class;
      Result : in out Unbounded_String;
      Level  : Natural) is
   begin
      Doc_Subtitle (B, Kernel, Result, Level, Subtitle_Name => "Subprograms");
   end Process_Header_Subprograms;

   -------------------------------
   -- Process_Caller_References --
   -------------------------------

   procedure Process_Caller_References
     (B                : access Docgen.Backend.Backend'Class;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      Options          : All_Options;
      Info             : Entity_Information;
      Source_File_List : Type_Source_File_Table.HTable;
      Level            : Natural)
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
         Result            => Result,
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
     (B                : access Docgen.Backend.Backend'Class;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      Options          : All_Options;
      Info             : Entity_Information;
      Source_File_List : Type_Source_File_Table.HTable;
      Level            : Natural)
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
         Result            => Result,
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
     (B                : access Docgen.Backend.Backend'Class;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      Parsed_List      : in out Construct_List;
      Entity_List      : in out Type_Entity_List.List;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Source_Filename  : VFS.Virtual_File;
      Unit_Info        : Entity_Information;
      File_Text        : GNAT.Strings.String_Access;
      Source_File_List : in out Type_Source_File_Table.HTable;
      Options          : All_Options;
      Private_Entity   : Boolean;
      Display_Private  : in out Boolean;
      Level            : Natural)
   is
      use TEL;
      Case_Sensitive    : constant Boolean :=
                            Get_Language_Context
                              (Get_Language_From_File
                                 (Get_Language_Handler (Kernel),
                                  Source_Filename)).Case_Sensitive;
      Entity_Node       : Type_Entity_List.List_Node;
      Description       : GNAT.Strings.String_Access;
      Header            : GNAT.Strings.String_Access;
      Header_Lines      : Natural;
      First_Already_Set : Boolean;
      Entity            : TEL.Data_Access;

   begin
      if not TEL.Is_Empty (Entity_List) then
         First_Already_Set := False;
         Entity_Node       := TEL.First (Entity_List);

         while Entity_Node /= TEL.Null_Node loop
            Entity := TEL.Data_Ref (Entity_Node);

            if not Entity.Processed
              and then Entity.Is_Private = Private_Entity
              and then Entity.Kind = Subprogram_Entity
              and then Source_Filename =
                Get_Filename (Get_File (Get_Declaration_Of (Entity.Entity)))
              and then
                (Entity_Defined_In_Package (Entity.Entity, Unit_Info)
                 or else Unit_Info.all = Entity.Entity.all)
            --  The subprogram must either belong to the package currently
            --  processed or be the main unit itself in order to be processed.
            then
               Entity.Processed := True;

               declare
                  Whole_Header : constant String :=
                    Get_Whole_Header
                      (File_Text.all,
                       Parsed_List,
                       Entity.all,
                       Case_Sensitive);
               begin
                  --  Check if it was an entity with its own header

                  if Whole_Header /= "" then

                     Remove_Indent (Whole_Header, Header, Header_Lines);

                     if Entity.Is_Private and then not Display_Private then
                        Process_Header_Private (B, Kernel, Result, Level);
                        Display_Private := True;
                     end if;

                     --  Check if the subtitle "Subprograms:" has to be set.

                     if not First_Already_Set then
                        Process_Header_Subprograms (B, Kernel, Result, Level);
                        First_Already_Set := True;
                     end if;

                     Description := new String'
                       (Get_Documentation
                          (Get_Language_Handler (Kernel),
                           Entity.Entity, File_Text.all));

                     Doc_Subprogram
                       (B, Kernel, Result,
                        List_Ref_In_File, Source_File_List, Options, Level,
                        Entity => Entity.all, Header => Header.all);

                     if Description.all /= "" then
                        Process_Description
                          (B, Kernel, Result, Level, Description.all);
                     end if;

                     if Options.References then
                        --  Callgraph is processed

                        Process_Caller_References
                          (B                => B,
                           Kernel           => Kernel,
                           Result           => Result,
                           Options          => Options,
                           Info             => Entity.Entity,
                           Source_File_List => Source_File_List,
                           Level            => Level);
                        Process_Calls_References
                          (B                => B,
                           Kernel           => Kernel,
                           Result           => Result,
                           Options          => Options,
                           Info             => Entity.Entity,
                           Source_File_List => Source_File_List,
                           Level            => Level);
                     end if;

                     GNAT.Strings.Free (Header);
                     GNAT.Strings.Free (Description);
                  end if;
               end;
            end if;

            exit when Entity_Node = TEL.Last (Entity_List);

            Entity_Node := TEL.Next (Entity_Node);
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

   ---------------------------------------
   -- Package_Contains_Entity_From_List --
   ---------------------------------------

   function Package_Contains_Entity_From_List
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
   end Package_Contains_Entity_From_List;

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

   function Get_Whole_Header
     (File_Text      : String;
      Parsed_List    : Construct_List;
      Entity         : Entity_List_Information;
      Case_Sensitive : Boolean)
      return String
   is
      Parse_Node : Construct_Access := Parsed_List.First;
   begin
      while Parse_Node /= null loop
         if Parse_Node.Sloc_Start.Line =
           Get_Line (Get_Declaration_Of (Entity.Entity))
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

               if Equal
                 (Parse_Node.Name
                    (Parse_Node.Name'First + 1 .. Parse_Node.Name'Last - 1),
                  Get_Name (Entity.Entity).all,
                  Case_Sensitive)
               then
                  return File_Text
                    (Parse_Node.Sloc_Start.Index .. Parse_Node.Sloc_End.Index);
               end if;

            elsif Equal
              (Parse_Node.Name.all, Get_Name (Entity.Entity).all,
               Case_Sensitive)
              or else Equal
                (Parse_Node.Name.all, Get_Full_Name (Entity.Entity),
                 Case_Sensitive)
            then
               return File_Text
                 (Line_Start (File_Text, Parse_Node.Sloc_Start.Index) ..
                    Parse_Node.Sloc_End.Index);
            end if;
         end if;
         Parse_Node := Parse_Node.Next;
      end loop;

      return "";
   end Get_Whole_Header;

   -------------------
   -- Remove_Indent --
   -------------------

   procedure Remove_Indent
     (Text       : String;
      Clean_Text : out GNAT.Strings.String_Access;
      Line_Count : out Natural)
   is
      Result            : Unbounded_String := Null_Unbounded_String;
      J                 : Natural := Text'First;
      Indent            : Natural := 0;
      End_Of_First_Line : Natural;
      End_Of_Line       : Natural;

   begin
      Line_Count := 0;
      End_Of_First_Line := Line_End (Text, Text'First);

      --  We try to figure out how much spaces ther are before the actual
      --  text.
      while J < End_Of_First_Line
        and then Text (J) = ' '
      loop
         J := J + 1;
      end loop;
      Indent := J - Text'First;

      J := Text'First;
      loop
         Line_Count := Line_Count + 1;

         if Line_End (Text, J) = Line_End (Text, Next_Line (Text, J)) then
            --  Last line
            End_Of_Line := Text'Last;
         else
            End_Of_Line := Next_Line (Text, J) - 1;
         end if;

         if Indent > 0 and then J + Indent < Text'Last
           and then Text (J .. J + Indent - 1) =
           Text (Text'First .. Text'First + Indent - 1)
         then
            --  We remove as many blanks as there are one the first line before
            --  the first non blank character
            Append (Result,  Text (J + Indent .. End_Of_Line));
         else
            --  Bad formatting ==> put the line as is to avoid skipping
            --  characters.
            --  ??? a better solution would be to figure out the right number
            --  of blanks to be added.
            Append (Result, Text (J .. End_Of_Line));
         end if;

         exit when Line_End (Text, J) = Line_End (Text, Next_Line (Text, J));

         J := Next_Line (Text, J);
      end loop;

      Clean_Text := new String'(To_String (Result));

   end Remove_Indent;

end Docgen.Work_On_Source;
