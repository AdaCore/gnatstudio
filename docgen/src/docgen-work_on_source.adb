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
with Language.Ada;              use Language.Ada;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Basic_Types;
with Language;                  use Language;
with Src_Info.Queries;          use Src_Info.Queries;
with VFS;                       use VFS;
with Glide_Kernel.Project;      use Glide_Kernel, Glide_Kernel.Project;
with Projects.Registry;         use Projects.Registry;

package body Docgen.Work_On_Source is

   package TSFL renames Type_Source_File_List;
   package TEL renames Type_Entity_List;

   procedure Process_One_Body_File
     (Kernel           : access Kernel_Handle_Record'Class;
      Doc_File         : File_Type;
      Source_File      : Virtual_File;
      File_Text        : GNAT.OS_Lib.String_Access;
      LI_Unit          : LI_File_Ptr;
      Source_File_List : in out Type_Source_File_List.List;
      Options          : All_Options;
      Converter        : Docgen.Doc_Subprogram_Type;
      Doc_Directory    : String;
      Doc_Suffix       : String);
   --  Will pass the information about the body file to the output
   --  subprogram. This is the only subprogram working on the contents
   --  of the body source files.

   procedure Process_Open_File
     (Kernel           : access Kernel_Handle_Record'Class;
      Doc_File         : File_Type;
      Package_File     : Virtual_File;
      Next_Package     : GNAT.OS_Lib.String_Access;
      Prev_Package     : GNAT.OS_Lib.String_Access;
      Package_Name     : String;
      Source_File_List : in out Type_Source_File_List.List;
      Options          : All_Options;
      Converter        : Docgen.Doc_Subprogram_Type;
      Doc_Directory    : String;
      Doc_Suffix       : String);
   --  is always the first subprogram to be called, as it creates the
   --  very beginning of the documentation by calling the output
   --  subprogram

   procedure Process_Close_File
     (Kernel        : access Kernel_Handle_Record'Class;
      Doc_File      : File_Type;
      File_Name     : Virtual_File;
      Options       : All_Options;
      Converter     : Docgen.Doc_Subprogram_Type;
      Doc_Directory : String;
      Doc_Suffix    : String);
   --  is always the last subprogram to be called, as it creates the
   --  very end of the documentation by calling the output subprogram

   procedure Process_Package_Description
     (Kernel        : access Kernel_Handle_Record'Class;
      Doc_File      : File_Type;
      Package_Name  : String;
      Text          : String;
      Options       : All_Options;
      Converter     : Doc_Subprogram_Type;
      Doc_Directory : String;
      Doc_Suffix    : String);
   --  extracts all the comment lines of the source file which are at the
   --  beginning of it. Empty lines are ignored, the procedure stops when
   --  first command is found. This information will be passed to the
   --  output subprogram

   procedure Process_With_Clause
     (Kernel           : access Kernel_Handle_Record'Class;
      Doc_File         : File_Type;
      Source_Filename  : VFS.Virtual_File;
      Package_Name     : String;
      Parsed_List      : Construct_List;
      File_Text        : GNAT.OS_Lib.String_Access;
      LI_Unit          : LI_File_Ptr;
      Source_File_List : in out Type_Source_File_List.List;
      Options          : All_Options;
      Converter        : Doc_Subprogram_Type;
      Doc_Directory    : String;
      Doc_Suffix       : String);
   --  will process the lines at the beginning of the source file
   --  starting with "with" and pass them to the output subprogram

   procedure Process_Packages
     (Kernel           : access Kernel_Handle_Record'Class;
      Doc_File         : File_Type;
      Entity_List      : in out Type_Entity_List.List;
      Source_Filename  : VFS.Virtual_File;
      Package_Name     : String;
      Parsed_List      : Construct_List;
      File_Text        : GNAT.OS_Lib.String_Access;
      LI_Unit          : LI_File_Ptr;
      Source_File_List : in out Type_Source_File_List.List;
      Options          : All_Options;
      Converter        : Doc_Subprogram_Type;
      Doc_Directory    : String;
      Doc_Suffix       : String);
   --  will process renamed and instantiated packages and pass
   --  them to the output subprogram

   procedure Process_Vars
     (Kernel           : access Kernel_Handle_Record'Class;
      Doc_File         : File_Type;
      Entity_List      : in out Type_Entity_List.List;
      Source_Filename  : VFS.Virtual_File;
      Package_Name     : String;
      Parsed_List      : Construct_List;
      File_Text        : GNAT.OS_Lib.String_Access;
      LI_Unit          : LI_File_Ptr;
      Source_File_List : in out Type_Source_File_List.List;
      Options          : All_Options;
      Converter        : Doc_Subprogram_Type;
      Doc_Directory    : String;
      Doc_Suffix       : String);
   --  called by Process_Source to work on the constants
   --  and named numbers and pass each of them to the output subprogram

   procedure Process_Exceptions
     (Kernel           : access Kernel_Handle_Record'Class;
      Doc_File         : File_Type;
      Entity_List      : in out Type_Entity_List.List;
      Source_Filename  : VFS.Virtual_File;
      Package_Name     : String;
      Parsed_List      : Construct_List;
      File_Text        : GNAT.OS_Lib.String_Access;
      LI_Unit          : LI_File_Ptr;
      Source_File_List : in out Type_Source_File_List.List;
      Options          : All_Options;
      Converter        : Doc_Subprogram_Type;
      Doc_Directory    : String;
      Doc_Suffix       : String);
   --  called by Process_Source to work on the exceptions and
   --  pass each of them to the output subprogram

   procedure Process_Entries
     (Kernel             : access Kernel_Handle_Record'Class;
      Doc_File           : File_Type;
      Entity_List        : in out Type_Entity_List.List;
      Source_Filename    : VFS.Virtual_File;
      Process_Body_File  : Boolean;
      Package_Name       : String;
      Parsed_List        : Construct_List;
      File_Text          : GNAT.OS_Lib.String_Access;
      LI_Unit            : LI_File_Ptr;
      Source_File_List   : in out Type_Source_File_List.List;
      Options            : All_Options;
      Converter          : Doc_Subprogram_Type;
      Doc_Directory      : String;
      Doc_Suffix         : String);
   --  called by Process_Source to work on the entires and entry
   --  families and pass each of them to the output subprogram

   procedure Process_Subprograms
     (Kernel             : access Kernel_Handle_Record'Class;
      Doc_File           : File_Type;
      Entity_List        : in out Type_Entity_List.List;
      Source_Filename    : VFS.Virtual_File;
      Process_Body_File  : Boolean;
      Package_Name       : String;
      Parsed_List        : Construct_List;
      File_Text          : GNAT.OS_Lib.String_Access;
      LI_Unit            : LI_File_Ptr;
      Source_File_List   : in out Type_Source_File_List.List;
      Options            : All_Options;
      Converter          : Doc_Subprogram_Type;
      Doc_Directory      : String;
      Doc_Suffix         : String);
   --  called by Process_Source to work on the subprograms and
   --  pass each of them to the output subprogram

   procedure Process_Types
     (Kernel           : access Kernel_Handle_Record'Class;
      Doc_File         : File_Type;
      Entity_List      : in out Type_Entity_List.List;
      Source_Filename  : VFS.Virtual_File;
      Package_Name     : String;
      Parsed_List      : Construct_List;
      File_Text        : GNAT.OS_Lib.String_Access;
      LI_Unit          : LI_File_Ptr;
      Source_File_List : in out Type_Source_File_List.List;
      Options          : All_Options;
      Converter        : Doc_Subprogram_Type;
      Doc_Directory    : String;
      Doc_Suffix       : String);
   --  called by Process_Source to work on the types and
   --  pass each of them to the output subprogram

   procedure Process_Header
     (Kernel            : access Kernel_Handle_Record'Class;
      Doc_File          : File_Type;
      Package_Name      : String;
      Package_File      : VFS.Virtual_File;
      Process_Body_File : Boolean;
      Options           : All_Options;
      Converter         : Doc_Subprogram_Type;
      Doc_Directory     : String;
      Doc_Suffix        : String);
   --  will call the output subprogram to create the header of
   --  the package. This is NOT the same as Process_Open_File,
   --  if TexInfo doc is created, the file is opened only once,
   --  but the Header has to be set in front of each package.

   procedure Process_Footer
     (Kernel        : access Kernel_Handle_Record'Class;
      Doc_File      : File_Type;
      Package_File  : Virtual_File;
      Options       : All_Options;
      Converter     : Doc_Subprogram_Type;
      Doc_Directory : String;
      Doc_Suffix    : String);
   --  will call the output subprogram to create the footer of
   --  the package. This is NOT the same as Process_Close_File,
   --  if TexInfo doc is created, the file is closed only once,
   --  but the Footer has to be set behind each package.

   function Extract_Comment
     (File_Text           : String;
      Line                : Natural;
      Header_Lines        : Natural;
      Package_Description : Boolean;
      Options             : All_Options) return GNAT.OS_Lib.String_Access;
   --  get the doc comments from the source file. The File_Text gives the
   --  String where to search, Line is the line number of the entity and
   --  Header_Lines says how many lines takes the header of the entity.
   --  Within Options it can be chosen, if the comments are placed
   --  below or above the entity header.
   --  If Package_Description is set, empty lines between the comment lines
   --  will be ignored, the direction of the processing is always the same
   --  and it stops when the first command is found.

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

   function Get_Line_From_String
     (Text    : String;
      Line_Nr : Natural) return String;
   --  Return the wished Line from the String

   --------------------
   -- Process_Source --
   --------------------

   procedure Process_Source
     (Kernel             : access Kernel_Handle_Record'Class;
      Doc_File           : File_Type;
      Next_Package       : GNAT.OS_Lib.String_Access;
      Prev_Package       : GNAT.OS_Lib.String_Access;
      Source_File_List   : in out Type_Source_File_List.List;
      Source_Filename    : VFS.Virtual_File;
      Package_Name       : String;
      Entity_List        : in out Type_Entity_List.List;
      Process_Body_File  : Boolean;
      LI_Unit            : LI_File_Ptr;
      Options            : All_Options;
      Converter          : Docgen.Doc_Subprogram_Type;
      Doc_Directory      : String;
      Doc_Suffix         : String)
   is
      File_Text   : GNAT.OS_Lib.String_Access;
      Parsed_List : Construct_List;

   begin
      --  Parse the source file and create the Parsed_List

      File_Text := Read_File (Source_Filename);

      if File_Text = null then
         --  This is a non existing file
         return;
      end if;

      Process_Open_File
        (Kernel,
         Doc_File,
         Source_Filename,
         Next_Package,
         Prev_Package,
         Package_Name,
         Source_File_List,
         Options,
         Converter,
         Doc_Directory,
         Doc_Suffix);
      Process_Header
        (Kernel,
         Doc_File,
         Package_Name,
         Source_Filename,
         Process_Body_File,
         Options,
         Converter,
         Doc_Directory,
         Doc_Suffix);

      --  Different ways of process for spec and body files

      if Is_Spec_File (Kernel, Source_Filename) then
         --  ??? Need to call the appropriate kernel service to retrieve
         --  the right language automatically.

         Parse_Constructs (Ada_Lang, File_Text.all, Parsed_List);
         Sort_List_Name (Entity_List);

         --  ??? The order of the following procedure calls can't be changed
         --  without changing the order in texi_output.
         --  See Doc_TEXI_Subtitle

         Process_Package_Description
           (Kernel, Doc_File, Package_Name, File_Text.all, Options,
            Converter, Doc_Directory, Doc_Suffix);
         Process_With_Clause
           (Kernel,
            Doc_File,
            Source_Filename,
            Package_Name,
            Parsed_List,
            File_Text,
            LI_Unit,
            Source_File_List,
            Options,
            Converter, Doc_Directory, Doc_Suffix);
         Process_Packages
           (Kernel,
            Doc_File,
            Entity_List,
            Source_Filename,
            Package_Name,
            Parsed_List,
            File_Text,
            LI_Unit,
            Source_File_List,
            Options,
            Converter, Doc_Directory, Doc_Suffix);
         Process_Vars
           (Kernel,
            Doc_File,
            Entity_List,
            Source_Filename,
            Package_Name,
            Parsed_List,
            File_Text,
            LI_Unit,
            Source_File_List,
            Options,
            Converter, Doc_Directory, Doc_Suffix);
         Process_Exceptions
           (Kernel,
            Doc_File,
            Entity_List,
            Source_Filename,
            Package_Name,
            Parsed_List,
            File_Text,
            LI_Unit,
            Source_File_List,
            Options,
            Converter, Doc_Directory, Doc_Suffix);
         Process_Types
           (Kernel,
            Doc_File,
            Entity_List,
            Source_Filename,
            Package_Name,
            Parsed_List,
            File_Text,
            LI_Unit,
            Source_File_List,
            Options,
            Converter, Doc_Directory, Doc_Suffix);
         Process_Entries
           (Kernel,
            Doc_File,
            Entity_List,
            Source_Filename,
            Process_Body_File,
            Package_Name,
            Parsed_List,
            File_Text,
            LI_Unit,
            Source_File_List,
            Options,
            Converter, Doc_Directory, Doc_Suffix);
         Process_Subprograms
           (Kernel,
            Doc_File,
            Entity_List,
            Source_Filename,
            Process_Body_File,
            Package_Name,
            Parsed_List,
            File_Text,
            LI_Unit,
            Source_File_List,
            Options,
            Converter, Doc_Directory, Doc_Suffix);
         Free (Parsed_List);

      else
         Process_One_Body_File
           (Kernel,
            Doc_File,
            Source_Filename,
            File_Text,
            LI_Unit,
            Source_File_List,
            Options,
            Converter, Doc_Directory, Doc_Suffix);
      end if;

      Process_Footer
        (Kernel, Doc_File, Source_Filename, Options,
         Converter, Doc_Directory, Doc_Suffix);
      Process_Close_File
        (Kernel, Doc_File, Source_Filename, Options,
         Converter, Doc_Directory, Doc_Suffix);
      Free (File_Text);
   end Process_Source;

   -----------------------
   -- Process_Open_File --
   -----------------------

   procedure Process_Open_File
     (Kernel           : access Kernel_Handle_Record'Class;
      Doc_File         : File_Type;
      Package_File     : Virtual_File;
      Next_Package     : GNAT.OS_Lib.String_Access;
      Prev_Package     : GNAT.OS_Lib.String_Access;
      Package_Name     : String;
      Source_File_List : in out Type_Source_File_List.List;
      Options          : All_Options;
      Converter        : Docgen.Doc_Subprogram_Type;
      Doc_Directory    : String;
      Doc_Suffix       : String)
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
      Converter (Kernel, Doc_File, Data_Open, Doc_Directory, Doc_Suffix);
      Free (Data_Open.Open_Title);
   end Process_Open_File;

   ------------------------
   -- Process_Close_File --
   ------------------------

   procedure Process_Close_File
     (Kernel        : access Kernel_Handle_Record'Class;
      Doc_File      : File_Type;
      File_Name     : Virtual_File;
      Options       : All_Options;
      Converter     : Docgen.Doc_Subprogram_Type;
      Doc_Directory : String;
      Doc_Suffix    : String)
   is
      Data_Close : Doc_Info :=
        (Close_Info,
         Doc_Info_Options => Options,
         Doc_LI_Unit      => No_LI_File,
         Doc_File_List    => TSFL.Null_List,
         Close_File_Name  => File_Name);

   begin
      Converter (Kernel, Doc_File, Data_Close, Doc_Directory, Doc_Suffix);
   end Process_Close_File;

   ---------------------------
   -- Process_One_Body_File --
   ---------------------------

   procedure Process_One_Body_File
     (Kernel           : access Kernel_Handle_Record'Class;
      Doc_File         : File_Type;
      Source_File      : Virtual_File;
      File_Text        : GNAT.OS_Lib.String_Access;
      LI_Unit          : LI_File_Ptr;
      Source_File_List : in out Type_Source_File_List.List;
      Options          : All_Options;
      Converter        : Docgen.Doc_Subprogram_Type;
      Doc_Directory    : String;
      Doc_Suffix       : String)
   is
      Data_Line : Doc_Info :=
        (Body_Line_Info,
         Doc_Info_Options => Options,
         Doc_LI_Unit      => LI_Unit,
         Body_Text        => File_Text,
         Body_File        => Source_File,
         Doc_File_List    => Source_File_List);

   begin
      Converter (Kernel, Doc_File, Data_Line, Doc_Directory, Doc_Suffix);
   end Process_One_Body_File;

   ------------------------
   -- Process_Unit_Index --
   ------------------------

   procedure Process_Unit_Index
     (Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
      Source_File_List : Docgen.Type_Source_File_List.List;
      Options          : Docgen.All_Options;
      Converter        : Docgen.Doc_Subprogram_Type;
      Doc_Directory    : String;
      Doc_Suffix       : String)
   is
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
           (Kernel, Index_File, Data_Package, Doc_Directory, Doc_Suffix);
         Free (Data_Package.Unit_Index_File_Name);

         for J in 1 .. Type_Source_File_List.Length (Source_File_List) -
           One_Ready
         loop
            Source_Filename := TSFL.Data (Source_File_Node).File_Name;

            --  Add unit, but only if from a spec file
            --  ??? Doesn't work if we are generating only a spec

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
                 (Kernel, Index_File, Data_Item, Doc_Directory, Doc_Suffix);
               Free (Data_Item.Item_Doc_File);
            end if;

            Source_File_Node := TSFL.Next (Source_File_Node);
         end loop;
      end if;

      Data_End := Doc_Info'
        (End_Of_Index_Info,
         Doc_Info_Options => Options,
         Doc_LI_Unit => No_LI_File,
         Doc_File_List => TSFL.Null_List,
         End_index_Title => new String'("End of Index"));
      Converter (Kernel, Index_File, Data_End, Doc_Directory, Doc_Suffix);

      Free (Data_End.End_Index_Title);
      Close (Index_File);
   end Process_Unit_Index;

   ------------------------------
   -- Process_Subprogram_Index --
   ------------------------------

   procedure Process_Subprogram_Index
     (Kernel                : access Kernel_Handle_Record'Class;
      Subprogram_Index_List : Type_Entity_List.List;
      Options               : All_Options;
      Converter             : Doc_Subprogram_Type;
      Doc_Directory         : String;
      Doc_Suffix            : String)
   is
      Source_Filename       : Virtual_File;
      Subprogram_Index_Node : Type_Entity_List.List_Node;
      Index_File       : File_Type;
      Data_Subprogram  : Doc_Info (Info_Type => Subprogram_Index_Info);
      Data_Item        : Doc_Info (Info_Type => Index_Item_Info);
      Data_End         : Doc_Info (Info_Type => End_Of_Index_Info);

      Doc_File_Name    : constant String := "index_sub";
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
        (Kernel, Index_File, Data_Subprogram, Doc_Directory, Doc_Suffix);
      Free (Data_Subprogram.Subprogram_Index_File_Name);

      if not TEL.Is_Empty (Subprogram_Index_List) then
         Subprogram_Index_Node := TEL.First (Subprogram_Index_List);

         for J in 1 .. Type_Entity_List.Length (Subprogram_Index_List) loop
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
              (Kernel, Index_File, Data_Item, Doc_Directory, Doc_Suffix);
            Free (Data_Item.Item_Doc_File);
            Free (Data_Item.Item_Name);

            Subprogram_Index_Node := TEL.Next (Subprogram_Index_Node);
         end loop;

      end if;

      Data_End := Doc_Info'
        (End_Of_Index_Info,
         Doc_Info_Options => Options,
         Doc_LI_Unit => No_LI_File,
         Doc_File_List => TSFL.Null_List,
         End_Index_Title => new String'("End of Index"));
      Converter (Kernel, Index_File, Data_End, Doc_Directory, Doc_Suffix);

      Free (Data_End.End_Index_Title);

      Close (Index_File);
   end Process_Subprogram_Index;

   ------------------------
   -- Process_Type_Index --
   ------------------------

   procedure Process_Type_Index
     (Kernel          : access Kernel_Handle_Record'Class;
      Type_Index_List : Docgen.Type_Entity_List.List;
      Options         : All_Options;
      Converter       : Doc_Subprogram_Type;
      Doc_Directory   : String;
      Doc_Suffix      : String)
   is
      Type_Index_Node : Type_Entity_List.List_Node;
      Index_File      : File_Type;
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
      Converter (Kernel, Index_File, Data_Type, Doc_Directory, Doc_Suffix);
      Free (Data_Type.Type_Index_File_Name);

      if not TEL.Is_Empty (Type_Index_List) then
         Type_Index_Node := TEL.First (Type_Index_List);

         for J in 1 .. Type_Entity_List.Length (Type_Index_List) loop
            Data_Item := Doc_Info'
              (Index_Item_Info,
               Doc_Info_Options => Options,
               Doc_LI_Unit => No_LI_File,
               Doc_File_List => TSFL.Null_List,
               Item_Name     => new String'
                 (Get_Name (TEL.Data (Type_Index_Node).Entity)),
               Item_File     =>
                 Get_Declaration_File_Of (TEL.Data (Type_Index_Node).Entity),
               Item_Line     => Get_Declaration_Line_Of
                 (TEL.Data (Type_Index_Node).Entity),
               Item_Doc_File => new String'
                 (Base_Name
                    (Get_Doc_File_Name
                       (Get_Declaration_File_Of
                          (TEL.Data (Type_Index_Node).Entity),
                        Doc_Directory,
                        Doc_Suffix))));
            Converter
              (Kernel, Index_File, Data_Item, Doc_Directory, Doc_Suffix);
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
      Converter (Kernel, Index_File, Data_End, Doc_Directory, Doc_Suffix);

      Free (Data_End.End_Index_Title);
      Close (Index_File);
   end Process_Type_Index;

   --------------------
   -- Process_Header --
   --------------------

   procedure Process_Header
     (Kernel            : access Kernel_Handle_Record'Class;
      Doc_File          : File_Type;
      Package_Name      : String;
      Package_File      : Virtual_File;
      Process_Body_File : Boolean;
      Options           : All_Options;
      Converter         : Doc_Subprogram_Type;
      Doc_Directory     : String;
      Doc_Suffix        : String)
   is
      Data_Header : Doc_Info :=
        (Header_Info,
         Doc_Info_Options => Options,
         Doc_LI_Unit    => No_LI_File,
         Doc_File_List  => TSFL.Null_List,
         Header_Package => new String'(Package_Name),
         Header_File    => Package_File,
         Header_Line    => First_File_Line,
         Header_Link    => Process_Body_File);

   begin
      Converter (Kernel, Doc_File, Data_Header, Doc_Directory, Doc_Suffix);
      Free (Data_Header.Header_Package);
   end Process_Header;

   --------------------
   -- Process_Footer --
   --------------------

   procedure Process_Footer
     (Kernel        : access Kernel_Handle_Record'Class;
      Doc_File      : File_Type;
      Package_File  : Virtual_File;
      Options       : All_Options;
      Converter     : Doc_Subprogram_Type;
      Doc_Directory : String;
      Doc_Suffix    : String)
   is
      Data_Footer : Doc_Info :=
        (Footer_Info,
         Doc_Info_Options => Options,
         Doc_LI_Unit => No_LI_File,
         Doc_File_List => TSFL.Null_List,
         Footer_Title => new String'("Docgen"),
         Footer_File  => Package_File);

   begin
      Converter (Kernel, Doc_File, Data_Footer, Doc_Directory, Doc_Suffix);
      Free (Data_Footer.Footer_Title);
   end Process_Footer;

   ---------------------------------
   -- Process_Package_Description --
   ---------------------------------

   procedure Process_Package_Description
     (Kernel        : access Kernel_Handle_Record'Class;
      Doc_File      : File_Type;
      Package_Name  : String;
      Text          : String;
      Options       : All_Options;
      Converter     : Doc_Subprogram_Type;
      Doc_Directory : String;
      Doc_Suffix    : String)
   is
      Data_Subtitle : Doc_Info (Info_Type => Subtitle_Info);
      Data_Package  : Doc_Info (Info_Type => Package_Desc_Info);

      Description_Found, Start_Found : Boolean;
      Line                           : Natural;
      Max_Lines                      : constant Natural :=
        Count_Lines (Text);
      Description                    : GNAT.OS_Lib.String_Access;

   begin
      --  Try to find the first line of the description of the package
      --  if something else is found than a comment line => no description

      Description_Found := False;
      Start_Found       := False;
      Line              := 1;

      while not Start_Found and Line < Max_Lines + 1 loop
         if Line_Is_Comment (Get_Line_From_String (Text, Line)) then
            Description_Found := True;
            Start_Found       := True;

         elsif not Line_Is_Empty
           (Get_Line_From_String (Text, Line))
         then
            Start_Found := True;
         else
            Line := Line + 1;
         end if;
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
           (Kernel, Doc_File, Data_Subtitle, Doc_Directory, Doc_Suffix);
         Description := Extract_Comment (Text, Line, 0, True, Options);
         Data_Package := Doc_Info'
           (Package_Desc_Info,
            Doc_Info_Options => Options,
            Doc_LI_Unit      => No_LI_File,
            Doc_File_List    => TSFL.Null_List,
            Package_Desc_Description => Description);
         Converter (Kernel, Doc_File, Data_Package, Doc_Directory, Doc_Suffix);
         Free (Description);
      end if;
   end Process_Package_Description;

   --------------------------
   -- Process_With_Clauses --
   --------------------------

   procedure Process_With_Clause
     (Kernel           : access Kernel_Handle_Record'Class;
      Doc_File         : File_Type;
      Source_Filename  : VFS.Virtual_File;
      Package_Name     : String;
      Parsed_List      : Construct_List;
      File_Text        : GNAT.OS_Lib.String_Access;
      LI_Unit          : LI_File_Ptr;
      Source_File_List : in out Type_Source_File_List.List;
      Options          : All_Options;
      Converter        : Doc_Subprogram_Type;
      Doc_Directory    : String;
      Doc_Suffix       : String)
   is
      Data_Subtitle      : Doc_Info (Info_Type => Subtitle_Info);
      Data_With          : Doc_Info (Info_Type => With_Info);

      Old_Line, New_Line : GNAT.OS_Lib.String_Access;
      Parse_Node         : Construct_Access;
      Parsed_List_End    : Boolean;
      First_With_Line    : Natural;

   begin
      New_Line        := new String'("  ");
      Parse_Node      := Parsed_List.First;
      Parsed_List_End := False;
      First_With_Line := 0;

      --  ??? Exception if no parsed entities found: later

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
           (Kernel, Doc_File, Data_Subtitle, Doc_Directory, Doc_Suffix);
      end if;

      Data_With := Doc_Info'
        (With_Info,
         Doc_Info_Options => Options,
         Doc_LI_Unit      => LI_Unit,
         Doc_File_List    => Source_File_List,
         With_Header_Line => First_With_Line,
         With_File        => Source_Filename,
         With_Header      => New_Line);
      Converter (Kernel, Doc_File, Data_With, Doc_Directory, Doc_Suffix);

      Free (Data_Subtitle.Subtitle_Name);
      Free (Data_Subtitle.Subtitle_Package);
      Free (New_Line);
   end Process_With_Clause;

   ----------------------
   -- Process_Packages --
   ----------------------

   procedure Process_Packages
     (Kernel           : access Kernel_Handle_Record'Class;
      Doc_File         : File_Type;
      Entity_List      : in out Type_Entity_List.List;
      Source_Filename  : VFS.Virtual_File;
      Package_Name     : String;
      Parsed_List      : Construct_List;
      File_Text        : GNAT.OS_Lib.String_Access;
      LI_Unit          : LI_File_Ptr;
      Source_File_List : in out Type_Source_File_List.List;
      Options          : All_Options;
      Converter        : Doc_Subprogram_Type;
      Doc_Directory    : String;
      Doc_Suffix       : String)
   is
      Entity_Node     : Type_Entity_List.List_Node;
      Description     : GNAT.OS_Lib.String_Access;
      Header          : GNAT.OS_Lib.String_Access;
      Data_Subtitle   : Doc_Info (Info_Type => Subtitle_Info);
      Data_Package    : Doc_Info (Info_Type => Package_Info);

      First_Already_Set : Boolean;

   begin
      if not TEL.Is_Empty (Entity_List) then
         First_Already_Set := False;
         Data_Subtitle := Doc_Info'
           (Subtitle_Info,
            Doc_Info_Options => Options,
            Doc_LI_Unit => No_LI_File,
            Doc_File_List => TSFL.Null_List,
            Subtitle_Name => new String'("Packages"),
            Subtitle_Kind => Package_Info,
            Subtitle_Package => new String'(Package_Name));
         Entity_Node := TEL.First (Entity_List);

         for J in 1 .. TEL.Length (Entity_List) loop
            --  Check if the entity is a package

            if TEL.Data (Entity_Node).Kind = Package_Entity
            --  but NOT the package itself
              and then To_Lower (TEL.Data (Entity_Node).Name.all) /=
                To_Lower (Package_Name)
            --  check if defined in this file, the others used only for bodys!
              and then Get_Declaration_File_Of
                (TEL.Data (Entity_Node).Entity) = Source_Filename
            then
               Header := Get_Whole_Header
                 (File_Text.all,
                  Parsed_List,
                  Get_Name (TEL.Data (Entity_Node).Entity),
                  Get_Declaration_Line_Of (TEL.Data (Entity_Node).Entity));

               if Header /= null then
                  --  Check if the subtitle has been set already.
                  --  Can't be set before the "if"

                  if not First_Already_Set then
                     Converter (Kernel, Doc_File, Data_Subtitle,
                                Doc_Directory, Doc_Suffix);
                     First_Already_Set := True;
                  end if;

                  Description := Extract_Comment
                    (File_Text.all,
                     Get_Declaration_Line_Of (TEL.Data (Entity_Node).Entity),
                     Count_Lines (Header.all),
                     False,
                     Options);

                  Data_Package := Doc_Info'
                    (Package_Info,
                     Doc_Info_Options    => Options,
                     Doc_LI_Unit         => LI_Unit,
                     Doc_File_List       => Source_File_List,
                     Package_Entity      => TEL.Data (Entity_Node),
                     Package_Description => Description,
                     Package_Header      => Header,
                     Package_Header_Line => Get_Declaration_Line_Of
                       (TEL.Data (Entity_Node).Entity));
                  Converter (Kernel, Doc_File, Data_Package,
                             Doc_Directory, Doc_Suffix);
               end if;
            end if;

            Entity_Node := TEL.Next (Entity_Node);
            Free (Description);
            Free (Header);
         end loop;

         Free (Data_Subtitle.Subtitle_Name);
         Free (Data_Subtitle.Subtitle_Package);
      end if;
   end Process_Packages;

   ------------------
   -- Process_Vars --
   ------------------

   procedure Process_Vars
     (Kernel           : access Kernel_Handle_Record'Class;
      Doc_File         : File_Type;
      Entity_List      : in out Type_Entity_List.List;
      Source_Filename  : VFS.Virtual_File;
      Package_Name     : String;
      Parsed_List      : Construct_List;
      File_Text        : GNAT.OS_Lib.String_Access;
      LI_Unit          : LI_File_Ptr;
      Source_File_List : in out Type_Source_File_List.List;
      Options          : All_Options;
      Converter        : Doc_Subprogram_Type;
      Doc_Directory    : String;
      Doc_Suffix       : String)
   is
      Entity_Node       : Type_Entity_List.List_Node;
      Description       : GNAT.OS_Lib.String_Access;
      Header            : GNAT.OS_Lib.String_Access;
      Data_Subtitle     : Doc_Info (Info_Type => Subtitle_Info);
      Data_Var          : Doc_Info (Info_Type => Var_Info);
      First_Already_Set : Boolean;

   begin
      if not TEL.Is_Empty (Entity_List) then
         First_Already_Set := False;
         Data_Subtitle := Doc_Info'
           (Subtitle_Info,
            Doc_Info_Options => Options,
            Doc_LI_Unit      => No_LI_File,
            Doc_File_List    => TSFL.Null_List,
            Subtitle_Name    => new String'("Constants and Named Numbers"),
            Subtitle_Kind    => Var_Info,
            Subtitle_Package => new String'(Package_Name));
         Entity_Node := TEL.First (Entity_List);

         for J in 1 .. TEL.Length (Entity_List) loop
            --  Check if the entity is a variable

            if TEL.Data (Entity_Node).Kind = Var_Entity
            --  Check if defined in this file, the others used only for bodys!
              and then Get_Declaration_File_Of
                (TEL.Data (Entity_Node).Entity) = Source_Filename
            then
               Header := Get_Whole_Header
                 (File_Text.all,
                  Parsed_List,
                  Get_Name (TEL.Data (Entity_Node).Entity),
                  Get_Declaration_Line_Of (TEL.Data (Entity_Node).Entity));

               --  Check if it was a entity with its own header

               if Header /= null then
                  --  Check if the subtitle "Constand and Named Numbers:"
                  --  has been set already.
                  --  Can't be set before the "if"

                  if not First_Already_Set then
                     Converter (Kernel, Doc_File, Data_Subtitle,
                                Doc_Directory, Doc_Suffix);
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
                     Doc_LI_Unit     => LI_Unit,
                     Doc_File_List   => Source_File_List,
                     Var_Entity      => TEL.Data (Entity_Node),
                     Var_Description => Description,
                     Var_Header      => Header,
                     Var_Header_Line => Get_Declaration_Line_Of
                       (TEL.Data (Entity_Node).Entity));
                  Converter (Kernel, Doc_File, Data_Var,
                             Doc_Directory, Doc_Suffix);
               end if;
            end if;

            Entity_Node := TEL.Next (Entity_Node);
            Free (Description);
            Free (Header);
         end loop;

         Free (Data_Subtitle.Subtitle_Name);
         Free (Data_Subtitle.Subtitle_Package);
      end if;
   end Process_Vars;

   ------------------------
   -- Process_Exceptions --
   ------------------------

   procedure Process_Exceptions
     (Kernel           : access Kernel_Handle_Record'Class;
      Doc_File         : File_Type;
      Entity_List      : in out Type_Entity_List.List;
      Source_Filename  : VFS.Virtual_File;
      Package_Name     : String;
      Parsed_List      : Construct_List;
      File_Text        : GNAT.OS_Lib.String_Access;
      LI_Unit          : LI_File_Ptr;
      Source_File_List : in out Type_Source_File_List.List;
      Options          : All_Options;
      Converter        : Doc_Subprogram_Type;
      Doc_Directory    : String;
      Doc_Suffix       : String)
   is
      Entity_Node     : Type_Entity_List.List_Node;
      Description     : GNAT.OS_Lib.String_Access;
      Header          : GNAT.OS_Lib.String_Access;
      Data_Subtitle   : Doc_Info (Info_Type => Subtitle_Info);
      Data_Exception  : Doc_Info (Info_Type => Exception_Info);
      First_Already_Set : Boolean;

   begin
      if not TEL.Is_Empty (Entity_List) then
         First_Already_Set := False;
         Data_Subtitle := Doc_Info'
           (Subtitle_Info,
            Doc_Info_Options => Options,
            Doc_LI_Unit => No_LI_File,
            Doc_File_List => TSFL.Null_List,
            Subtitle_Name => new String'("Exceptions"),
            Subtitle_Kind => Exception_Info,
            Subtitle_Package => new String'(Package_Name));
         Entity_Node := TEL.First (Entity_List);

         for J in 1 .. TEL.Length (Entity_List) loop

            --  Check if the entity is an exception

            if TEL.Data (Entity_Node).Kind = Exception_Entity
            --  Check if defined in this file, the others used only for bodys!
              and then Get_Declaration_File_Of
                (TEL.Data (Entity_Node).Entity) = Source_Filename
            then
               Header := Get_Whole_Header
                 (File_Text.all,
                  Parsed_List,
                  Get_Name (TEL.Data (Entity_Node).Entity),
                  Get_Declaration_Line_Of (TEL.Data (Entity_Node).Entity));

               --  Check if it was a entity with its own header

               if Header /= null then
                  --  Check if the subtitle "Exceptions:" has been set already.
                  --  Can't be set before the "if"

                  if not First_Already_Set then
                     Converter (Kernel, Doc_File, Data_Subtitle,
                                Doc_Directory, Doc_Suffix);
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
                     Exception_Description => Description,
                     Exception_Header      => Header,
                     Exception_Header_Line => Get_Declaration_Line_Of
                       (TEL.Data (Entity_Node).Entity));
                  Converter (Kernel, Doc_File, Data_Exception,
                             Doc_Directory, Doc_Suffix);
               end if;
            end if;

            Entity_Node := TEL.Next (Entity_Node);
            Free (Description);
            Free (Header);
         end loop;

         Free (Data_Subtitle.Subtitle_Name);
         Free (Data_Subtitle.Subtitle_Package);
      end if;
   end Process_Exceptions;

   -------------------
   -- Process_Types --
   -------------------

   procedure Process_Types
     (Kernel           : access Kernel_Handle_Record'Class;
      Doc_File         : File_Type;
      Entity_List      : in out Type_Entity_List.List;
      Source_Filename  : VFS.Virtual_File;
      Package_Name     : String;
      Parsed_List      : Construct_List;
      File_Text        : GNAT.OS_Lib.String_Access;
      LI_Unit          : LI_File_Ptr;
      Source_File_List : in out Type_Source_File_List.List;
      Options          : All_Options;
      Converter        : Doc_Subprogram_Type;
      Doc_Directory    : String;
      Doc_Suffix       : String)
   is
      Entity_Node     : Type_Entity_List.List_Node;
      Description     : GNAT.OS_Lib.String_Access;
      Header          : GNAT.OS_Lib.String_Access;
      Data_Subtitle   : Doc_Info (Info_Type => Subtitle_Info);
      Data_Type       : Doc_Info (Info_Type => Type_Info);

      First_Already_Set : Boolean;

   begin
      if not TEL.Is_Empty (Entity_List) then
         First_Already_Set := False;
         Data_Subtitle := Doc_Info'
           (Subtitle_Info,
            Doc_Info_Options => Options,
            Doc_LI_Unit => No_LI_File,
            Doc_File_List => TSFL.Null_List,
            Subtitle_Name => new String'("Types"),
            Subtitle_Kind => Type_Info,
            Subtitle_Package => new String'(Package_Name));
         Entity_Node := TEL.First (Entity_List);

         for J in 1 .. TEL.Length (Entity_List) loop
            --  Check if the entity is a type

            if TEL.Data (Entity_Node).Kind = Type_Entity
            --  Check if defined in this file (the rest of entities
            --  only for the body documentation)
              and then Get_Declaration_File_Of
                (TEL.Data (Entity_Node).Entity) = Source_Filename
            then
               Header := Get_Whole_Header
                 (File_Text.all,
                  Parsed_List,
                  Get_Name (TEL.Data (Entity_Node).Entity),
                  Get_Declaration_Line_Of (TEL.Data (Entity_Node).Entity));

               --  Check if it was a entity with its own header

               if Header /= null then
                  --  Check if still the subtitle "Types:" has to be set.
                  --  Can't be set before the "if"

                  if not First_Already_Set then
                     Converter (Kernel, Doc_File, Data_Subtitle,
                                Doc_Directory, Doc_Suffix);
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
                     Type_Description => Description,
                     Type_Header => Header,
                     Type_Header_Line => Get_Declaration_Line_Of
                       (TEL.Data (Entity_Node).Entity));
                  Converter (Kernel, Doc_File, Data_Type,
                             Doc_Directory, Doc_Suffix);
               end if;
            end if;

            Entity_Node := TEL.Next (Entity_Node);
            Free (Description);
            Free (Header);
         end loop;
      end if;

      Free (Data_Subtitle.Subtitle_Name);
      Free (Data_Subtitle.Subtitle_Package);
   end Process_Types;

   ---------------------
   -- Process_Entries --
   ---------------------

   procedure Process_Entries
     (Kernel             : access Kernel_Handle_Record'Class;
      Doc_File           : File_Type;
      Entity_List        : in out Type_Entity_List.List;
      Source_Filename    : VFS.Virtual_File;
      Process_Body_File  : Boolean;
      Package_Name       : String;
      Parsed_List        : Construct_List;
      File_Text          : GNAT.OS_Lib.String_Access;
      LI_Unit            : LI_File_Ptr;
      Source_File_List   : in out Type_Source_File_List.List;
      Options            : All_Options;
      Converter          : Doc_Subprogram_Type;
      Doc_Directory      : String;
      Doc_Suffix         : String)
   is
      Entity_Node       : Type_Entity_List.List_Node;
      Description       : GNAT.OS_Lib.String_Access;
      Header            : GNAT.OS_Lib.String_Access;
      Data_Subtitle     : Doc_Info (Info_Type => Subtitle_Info);
      Data_Entry        : Doc_Info (Info_Type => Entry_Info);
      First_Already_Set : Boolean;

   begin
      if not TEL.Is_Empty (Entity_List) then
         First_Already_Set := False;
         Data_Subtitle := Doc_Info'
           (Subtitle_Info,
            Doc_Info_Options => Options,
            Doc_LI_Unit => No_LI_File,
            Doc_File_List => TSFL.Null_List,
            Subtitle_Name => new String'("Tasks, Entries and Entry Families"),
            Subtitle_Kind => Entry_Info,
            Subtitle_Package => new String'(Package_Name));
         Entity_Node := TEL.First (Entity_List);

         for J in 1 .. TEL.Length (Entity_List) loop
            --  Check if the entity is a entry or entry family

            if TEL.Data (Entity_Node).Kind = Entry_Entity
            --  Check if defined in this file (the rest of
            --  entities only for the body documentation)
              and then Get_Declaration_File_Of
                (TEL.Data (Entity_Node).Entity) = Source_Filename
            then
               Header := Get_Whole_Header
                 (File_Text.all,
                  Parsed_List,
                  Get_Name (TEL.Data (Entity_Node).Entity),
                  Get_Declaration_Line_Of (TEL.Data (Entity_Node).Entity));

               --  Check if it was a entity with its own header

               if Header /= null then
                  --  Check if still the subtitle has to be set.
                  --  Can be set before the "if"

                  if not First_Already_Set then
                     Converter (Kernel, Doc_File, Data_Subtitle,
                                Doc_Directory, Doc_Suffix);
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
                     Entry_Description => Description,
                     Entry_Link        => Process_Body_File,
                     Entry_Header      => Header,
                     Entry_Header_Line => Get_Declaration_Line_Of
                       (TEL.Data (Entity_Node).Entity));
                  Converter (Kernel, Doc_File, Data_Entry,
                             Doc_Directory, Doc_Suffix);
               end if;
            end if;

            Entity_Node := TEL.Next (Entity_Node);
            Free (Description);
            Free (Header);
         end loop;
      end if;

      Free (Data_Subtitle.Subtitle_Name);
      Free (Data_Subtitle.Subtitle_Package);
   end Process_Entries;

   -------------------------
   -- Process_Subprograms --
   -------------------------

   procedure Process_Subprograms
     (Kernel             : access Kernel_Handle_Record'Class;
      Doc_File           : File_Type;
      Entity_List        : in out Type_Entity_List.List;
      Source_Filename    : VFS.Virtual_File;
      Process_Body_File  : Boolean;
      Package_Name       : String;
      Parsed_List        : Construct_List;
      File_Text          : GNAT.OS_Lib.String_Access;
      LI_Unit            : LI_File_Ptr;
      Source_File_List   : in out Type_Source_File_List.List;
      Options            : All_Options;
      Converter          : Doc_Subprogram_Type;
      Doc_Directory      : String;
      Doc_Suffix         : String)
   is
      Entity_Node       : Type_Entity_List.List_Node;
      Description       : GNAT.OS_Lib.String_Access;
      Header            : GNAT.OS_Lib.String_Access;
      Data_Subtitle     : Doc_Info (Info_Type => Subtitle_Info);
      Data_Subprogram   : Doc_Info (Info_Type => Subprogram_Info);
      First_Already_Set : Boolean;

   begin
      if not TEL.Is_Empty (Entity_List) then
         First_Already_Set := False;
         Data_Subtitle := Doc_Info'
           (Subtitle_Info,
            Doc_Info_Options => Options,
            Doc_LI_Unit      => No_LI_File,
            Doc_File_List    => TSFL.Null_List,
            Subtitle_Name    => new String'("Subprograms"),
            Subtitle_Kind    => Subprogram_Info,
            Subtitle_Package => new String'(Package_Name));
         Entity_Node := TEL.First (Entity_List);

         for J in 1 .. TEL.Length (Entity_List) loop
            --  Check if the entity is a procedure or a function

            if TEL.Data (Entity_Node).Kind = Subprogram_Entity
            --  Check if defined in this file (the rest of
            --  entities only for the body documentation)
              and then Get_Declaration_File_Of
                (TEL.Data (Entity_Node).Entity) = Source_Filename
            then
               --  Check if still the subtitle "Subprograms:"
               --  has to be set. Can be set before the "if"

               if not First_Already_Set then
                  Converter
                    (Kernel, Doc_File, Data_Subtitle,
                     Doc_Directory, Doc_Suffix);
                  First_Already_Set := True;
               end if;

               Header := Get_Whole_Header
                 (File_Text.all,
                  Parsed_List,
                  Get_Name (TEL.Data (Entity_Node).Entity),
                  Get_Declaration_Line_Of (TEL.Data (Entity_Node).Entity));

               --  Check if it was a entity with its own header

               if Header /= null then
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
                     Subprogram_Description => Description,
                     Subprogram_Link        => Process_Body_File,
                     Subprogram_List        => Entity_List,
                     Subprogram_Header      => Header,
                     Subprogram_Header_Line =>
                      Get_Declaration_Line_Of (TEL.Data (Entity_Node).Entity));

                  Converter (Kernel, Doc_File, Data_Subprogram,
                             Doc_Directory, Doc_Suffix);
               end if;
            end if;

            Entity_Node := TEL.Next (Entity_Node);
            Free (Description);
            Free (Header);
         end loop;
      end if;

      Free (Data_Subtitle.Subtitle_Name);
      Free (Data_Subtitle.Subtitle_Package);
   end Process_Subprograms;

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
         then return False;
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

   -----------------------
   --  Extract_Comment  --
   -----------------------

   function Extract_Comment
     (File_Text           : String;
      Line                : Natural;
      Header_Lines        : Natural;
      Package_Description : Boolean;
      Options             : All_Options) return GNAT.OS_Lib.String_Access
   is
      New_Line, Old_Line  : GNAT.OS_Lib.String_Access;
      Result_Line         : GNAT.OS_Lib.String_Access;
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
                  Result_Line := new String'(Result_Line.all & ' ' &
                    Kill_Prefix (New_Line.all));
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
                  Result_Line := new String'((Kill_Prefix (New_Line.all)) &
                    ' ' & Result_Line.all);
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
   begin
      --  ??? Exception if no parsed entities found: later

      while Parse_Node /= null loop
         if Parse_Node.Name /= null
           and then To_Lower (Parse_Node.Name.all) = To_Lower (Entity_Name)
           and then Parse_Node.Sloc_Start.Line = Entity_Line
         then
            return new String'
              (File_Text (Parse_Node.Sloc_Start.Index ..
                            Parse_Node.Sloc_End.Index));
         end if;

         Parse_Node := Parse_Node.Next;
      end loop;

      return null;
   end Get_Whole_Header;

end Docgen.Work_On_Source;
