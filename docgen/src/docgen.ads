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

--  This package defines the types and subprograms used by the other
--  Docgen packages. There are the three types needed for the lists,
--  the type All_Options, the Doc_Subprogram_Type and
--  the type Doc_Info, which will be used to destinguish, which
--  entity type should be processed by the output procedure.

with Ada.Text_IO;               use Ada.Text_IO;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with List_Utils;                use List_Utils;
with Src_Info;                  use Src_Info;
with Src_Info.Queries;          use Src_Info.Queries;
with Glide_Kernel;              use Glide_Kernel;
with VFS;
with Generic_List;
with Projects;

package Docgen is

   Max_Line_Length     : constant Natural := 160;
   First_File_Line     : constant Natural := 1;
   No_Body_Line_Needed : constant Natural := 0;

   type Source_File_Information is record
      File_Name        : VFS.Virtual_File;
      Package_Name     : GNAT.OS_Lib.String_Access;
      Other_File_Found : Boolean;
   end record;
   --  Description of a source file for which documentation should be
   --  generated.

   procedure Free (X : in out Source_File_Information);
   --  Free the information associated with X

   package Type_Source_File_List is
     new Generic_List (Source_File_Information);

   function Compare_Elements (X, Y : Source_File_Information) return Boolean;
   procedure Sort_List_Name is
     new Sort (Type_Source_File_List, "<" => Compare_Elements);
   --  Sort elements by name (BUT: the spec file in front of body file)

   type Reference_List_Information is record
      Entity          : Src_Info.Queries.Entity_Information;
      Set_Link        : Boolean;   --  if False, no link will be set
   end record;
   --  List of references for an entity: this is the list of subprograms
   --  calling or called by the entity.

   procedure Free (X : in out Reference_List_Information);
   --  Free the information associated with X

   package Type_Reference_List is
     new Generic_List (Reference_List_Information);

   function Compare_Elements_Column
     (X, Y : Reference_List_Information) return Boolean;
   function Compare_Elements_Name
     (X, Y : Reference_List_Information) return Boolean;
   procedure Sort_List_Column is
     new Sort (Type_Reference_List, "<" => Compare_Elements_Column);
   --  Sort the list by column

   procedure Sort_List_Name is
     new Sort (Type_Reference_List, "<" => Compare_Elements_Name);
   --  Sort the list by name

   type Entity_Handle is access Src_Info.Queries.Entity_Information;

   type Reference_In_File is record
      Name         : GNAT.OS_Lib.String_Access;
      Line         : Natural;
      Column       : Natural;
      Entity       : Entity_Handle;
   end record;
   --  Record used to save a reference in a file
   --  Entity is the declaration of the reference

   procedure Free (X : in out Reference_In_File);
   --  Free the information associated with X

   package List_Reference_In_File is
     new Generic_List (Reference_In_File);
   --  List used to record the references of a file

   function Compare_Elements_By_Line_And_Column
     (X, Y : Reference_In_File) return Boolean;

   procedure Sort_List_By_Line_And_Column is
     new Sort (List_Reference_In_File,
               "<" => Compare_Elements_By_Line_And_Column);
   --  Sort the list by line and column

   procedure Free (X : in out Src_Info.Queries.Entity_Information);

   package List_Entity_In_File is
     new Generic_List (Src_Info.Queries.Entity_Information);
   --  List used to record the declaration of references of a file

   type Entity_Type is
     (Subprogram_Entity,
      Exception_Entity,
      Type_Entity,
      Var_Entity,
      Package_Entity,
      Entry_Entity,
      Other_Entity);
   --  A simplified list of possible entity types

   type Entity_List_Information is record
      Kind              : Entity_Type;
      Name              : GNAT.OS_Lib.String_Access;
      Entity            : Src_Info.Queries.Entity_Information;
      Is_Private        : Boolean;
      --  The following items won't be used in the index lists
      Line_In_Body      : Src_Info.File_Location;
      --  Calls_List, Called_List: only used for subprograms
      Calls_List        : Type_Reference_List.List;
      Called_List       : Type_Reference_List.List;
   end record;
   --  Description of an entity

   function Clone
     (Entity : Entity_List_Information) return Entity_List_Information;
   --  Return a deep-copy of Entity.
   --  Entity can be freed without impacting the copy

   procedure Free (X : in out Entity_List_Information);
   --  Free the memory associated with X

   package Type_Entity_List is new Generic_List (Entity_List_Information);

   function Compare_Elements_Name
     (X, Y : Entity_List_Information) return Boolean;
   function Compare_Elements_Line
     (X, Y : Entity_List_Information) return Boolean;
   function Compare_Elements_Column
     (X, Y : Entity_List_Information) return Boolean;

   procedure Sort_List_Line   is
     new Sort (Type_Entity_List, "<" => Compare_Elements_Line);
   --  Sort list by line

   procedure Sort_List_Column is
     new Sort (Type_Entity_List, "<" => Compare_Elements_Column);
   --  Sort list by column

   procedure Sort_List_Name   is
     new Sort (Type_Entity_List, "<" => Compare_Elements_Name);
   --  Sort the entities in alphabetical order by name,
   --  BUT all public entites stand in front of the private

   type Info_Types is
     (Open_Info, Close_Info,
      Subtitle_Info, Exception_Info, Type_Info,
      Subprogram_Info, Header_Info, Footer_Info,
      With_Info, Package_Desc_Info,
      Unit_Index_Info, Type_Index_Info,
      Subprogram_Index_Info, End_Of_Index_Info,
      Index_Item_Info, Body_Line_Info, Var_Info,
      Package_Info, Entry_Info);
   --  The structure used in the type Doc_Info.

   type Type_Api_Doc is (HTML, TEXI);
   --  Type of documentation that can be generated
   for Type_Api_Doc'Size use Integer'Size;

   type All_Options is record
      Type_Of_File  : Type_Api_Doc := HTML;
      --  Type of the documentation
      Process_Body_Files   : Boolean := False;
      --  Create also the body documentation
      Ignorable_Comments   : Boolean := False;
      --  Ignore all comments with "--!"
      Comments_Above       : Boolean := False;
      --  Doc comments for entities above the header
      Show_Private         : Boolean := False;
      --  Show also private entities
      References           : Boolean := False;
      --  True if the program should search for the references
      --  Adding information like "subprogram called by..."
      One_Doc_File         : Boolean := False;
      --  Used for TexInfo: True, if the project.texi file should be
      --  build and the package files should be included there later.
      Link_All             : Boolean := False;
      --  Should links be created to entities whose declaration files
      --  aren't being processed
   end record;

   --  The data structure used to pass the information
   --  to the procedure which is defined
   --  as the Doc_Subprogram_Type (see below) to
   --  be the only procedure to be defined
   --  when a new output format should be added
   type Doc_Info (Info_Type : Info_Types) is
      record
         Doc_Info_Options          : All_Options;
         Doc_LI_Unit               : LI_File_Ptr;
         Doc_File_List             : Type_Source_File_List.List;

         case Info_Type is
               --  Used at the very beginning of the file
            when Open_Info =>
               Open_Title                    : GNAT.OS_Lib.String_Access;
               Open_File                     : VFS.Virtual_File;
               Open_Package_Next             : GNAT.OS_Lib.String_Access;
               Open_Package_Prev             : GNAT.OS_Lib.String_Access;

               --  Used at the end of the file
            when Close_Info =>
               Close_File_Name               : VFS.Virtual_File;

               --  Used to start an entity information
            when Header_Info =>
               Header_Package                : GNAT.OS_Lib.String_Access;
               Header_File                   : VFS.Virtual_File;
               Header_Link                   : Boolean;
               Header_Line                   : Natural;

               --  Used to finish an entity information
            when Footer_Info =>
               Footer_Title                  : GNAT.OS_Lib.String_Access;
               Footer_File                   : VFS.Virtual_File;

               --  Used to add a subtitle to the information file
            when Subtitle_Info =>
               Subtitle_Name                 : GNAT.OS_Lib.String_Access;
               Subtitle_Package              : GNAT.OS_Lib.String_Access;
               Subtitle_Kind                 : Info_Types;

            when With_Info =>
               With_Header                   : GNAT.OS_Lib.String_Access;
               With_File                     : VFS.Virtual_File;
               With_Header_Line              : Natural;

            when Package_Info =>
               Package_Entity                : Entity_List_Information;
               Package_Header                : GNAT.OS_Lib.String_Access;
               Package_Header_Line           : Natural;
               Package_Description           : GNAT.OS_Lib.String_Access;

               --  Used to add the package description
            when Package_Desc_Info =>
               Package_Desc_Description      : GNAT.OS_Lib.String_Access;

               --  Used to add a constant and named numbers
            when Var_Info =>
               Var_Entity                    : Entity_List_Information;
               Var_Header                    : GNAT.OS_Lib.String_Access;
               Var_Header_Line               : Natural;
               Var_Description               : GNAT.OS_Lib.String_Access;

               --  Used to add an exception info to the information file
            when Exception_Info =>
               Exception_Entity              : Entity_List_Information;
               Exception_Header              : GNAT.OS_Lib.String_Access;
               Exception_Header_Line              : Natural;
               Exception_Description         : GNAT.OS_Lib.String_Access;

               --  Used to add a type info to the information file
            when Type_Info =>
               Type_Entity                   : Entity_List_Information;
               Type_Header                   : GNAT.OS_Lib.String_Access;
               Type_Header_Line              : Natural;
               Type_Description              : GNAT.OS_Lib.String_Access;

               --  Used to add an entry info to the information file
            when Entry_Info =>
               Entry_Entity                  : Entity_List_Information;
               Entry_Header                  : GNAT.OS_Lib.String_Access;
               Entry_Header_Line             : Natural;
               Entry_Description             : GNAT.OS_Lib.String_Access;
               Entry_Link                    : Boolean;

               --  Used to add a subprogram info to the information file
            when Subprogram_Info =>
               Subprogram_Entity             : Entity_List_Information;
               Subprogram_Header             : GNAT.OS_Lib.String_Access;
               Subprogram_Header_Line        : Natural;
               Subprogram_Description        : GNAT.OS_Lib.String_Access;
               Subprogram_Link               : Boolean;
               Subprogram_List               : Type_Entity_List.List;

               --  Used to start the package index file
            when Unit_Index_Info =>
               --  The list of the files
               Unit_File_List                : Type_Source_File_List.List;
               --  The name doc file name without the suffix
               Unit_Index_File_Name          : GNAT.OS_Lib.String_Access;
               Unit_Project_Name             : Projects.Project_Type;

               --  Used to start the subprogram index file
            when Subprogram_Index_Info =>
               --  The doc file name without the suffix
               Subprogram_Index_File_Name    : GNAT.OS_Lib.String_Access;

               --  Used to start the type index file
            when Type_Index_Info =>
               --  The name doc file name without the suffix
               Type_Index_File_Name          : GNAT.OS_Lib.String_Access;

               --  Used to finish all 3 kinds of index files
            when End_Of_Index_Info =>
               End_Index_Title               : GNAT.OS_Lib.String_Access;

               --  Used to add items to all 3 kindes of index files
            when Index_Item_Info =>
               Item_Name                     : GNAT.OS_Lib.String_Access;
               Item_File                     : VFS.Virtual_File;
               Item_Line                     : Natural;
               Item_Doc_File                 : GNAT.OS_Lib.String_Access;

               --  Used to pass the information of one line in the body file
            when Body_Line_Info =>
               Body_File                     : VFS.Virtual_File;
               Body_Text                     : GNAT.OS_Lib.String_Access;
         end case;
      end record;
   --  The data structure used to pass the information to the procedure which
   --  is defined as the Doc_Subprogram_Type (see below) to be the only
   --  procedure to be defined when a new output format should be added.

   package Docgen_Backend is

      type Backend is abstract tagged private;

      type Backend_Handle is access all Backend'Class;

      procedure Initialize (B : access Backend; Text : String) is abstract;
      --  Initialize the private fields before starting the documentation
      --  process.

      procedure Launch_Doc_Create
        (B                : Backend_Handle;
         Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
         File             : in Ada.Text_IO.File_Type;
         Entity_List      : in out Type_Entity_List.List;
         List_Ref_In_File : in out List_Reference_In_File.List;
         Info             : in out Docgen.Doc_Info;
         Doc_Directory    : String;
         Doc_Suffix       : String);
      --  This method is transmited by a pointer whose type is
      --     Doc_Subprogram_Type. In its body, it calls Doc_Create.

      --  ??? This approach seems over complicated and could probably be
      --  simplified using standard OOP.
      --  Transmitted via a pointer of type Doc_Subprogram_Type.
      --  Call Doc_Create in its body.

      procedure Doc_Create
        (B                : access Backend;
         Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
         File             : in Ada.Text_IO.File_Type;
         Entity_List      : in out Type_Entity_List.List;
         List_Ref_In_File : in out List_Reference_In_File.List;
         Info             : in out Docgen.Doc_Info;
         Doc_Directory    : String;
         Doc_Suffix       : String) is abstract;
      --  Doc_Create starts the process which make the documentation
      --  for one file.

      procedure Format_Comment
        (B           : access Backend;
         File        : Ada.Text_IO.File_Type;
         Text        : String;
         Start_Index : Natural;
         Start_line  : Natural;
         End_Index   : Natural;
         End_Line    : Natural;
         Entity_Line : Natural) is abstract;
      --  Format text as a comment

      procedure Format_Keyword
        (B           : access Backend;
         File        : Ada.Text_IO.File_Type;
         Text        : String;
         Start_Index : Natural;
         Start_line  : Natural;
         End_Index   : Natural;
         End_Line    : Natural;
         Entity_Line : Natural) is abstract;
      --  Format text as a keyword

      procedure Format_String
        (B           : access Backend;
         File        : Ada.Text_IO.File_Type;
         Text        : String;
         Start_Index : Natural;
         Start_line  : Natural;
         End_Index   : Natural;
         End_Line    : Natural;
         Entity_Line : Natural) is abstract;
      --  Format text as a string (between two ")

      procedure Format_Character
        (B           : access Backend;
         File        : Ada.Text_IO.File_Type;
         Text        : String;
         Start_Index : Natural;
         Start_line  : Natural;
         End_Index   : Natural;
         End_Line    : Natural;
         Entity_Line : Natural) is abstract;
      --  Format text as a character (between two ')

      procedure Format_Identifier
        (B                : access Backend;
         Entity_List      : in out Type_Entity_List.List;
         List_Ref_In_File   : in out List_Reference_In_File.List;
         Start_Index      : Natural;
         Start_Line       : Natural;
         Start_Column     : Natural;
         End_Index        : Natural;
         End_Line         : Natural;
         Kernel           : access Kernel_Handle_Record'Class;
         File             : Ada.Text_IO.File_Type;
         LI_Unit          : LI_File_Ptr;
         Text             : String;
         File_Name        : VFS.Virtual_File;
         Entity_Line      : Natural;
         Line_In_Body     : Natural;
         Source_File_List : Type_Source_File_List.List;
         Link_All         : Boolean;
         Is_Body          : Boolean;
         Process_Body     : Boolean;
         Info             : Doc_Info) is abstract;
      --  Format text as an identifier


      procedure Format_File
        (B                : access Backend'Class;
         Kernel           : access Kernel_Handle_Record'Class;
         File             : Ada.Text_IO.File_Type;
         Entity_List      : in out Type_Entity_List.List;
         List_Ref_In_File : in out List_Reference_In_File.List;
         LI_Unit          : LI_File_Ptr;
         Text             : String;
         File_Name        : VFS.Virtual_File;
         Entity_Line      : Natural;
         Line_In_Body     : Natural;
         Source_File_List : Type_Source_File_List.List;
         Link_All         : Boolean;
         Is_Body          : Boolean;
         Process_Body     : Boolean;
         Info             : Doc_Info);
      --  Generate documentation for a type of code in the file
      --  (eg. packages, subprograms, exceptions ...). All tokens of this text
      --  are analysed by Parse_Entities.
      --  Parse_Entities calls Callback which reads the
      --  nature of the token and calls the appropriate subprogram on it
      --  (Format_String, Format_Character, Format_Comment, Format_Keyword,
      --  Format_Identifier).
      --  Line_In_Body is used only for subprograms to create not regular
      --  links (in this case it is not the line number of the declaration
      --  which is needed, but the line of the definition in the body.
      --  If Do_Check_Pack is set, the procedure will check if a link
      --  should be set to First_Package_Line or link it to its declaration
      --  line. ??? What is Do_Check_Pack

      procedure Format_Link
        (B                : access Backend;
         Start_Index      : Natural;
         Start_Line       : Natural;
         Start_Column     : Natural;
         End_Index        : Natural;
         Kernel           : access Kernel_Handle_Record'Class;
         File             : Ada.Text_IO.File_Type;
         LI_Unit          : LI_File_Ptr;
         Text             : String;
         File_Name        : VFS.Virtual_File;
         Entity_Line      : Natural;
         Line_In_Body     : Natural;
         Source_File_List : Type_Source_File_List.List;
         Link_All         : Boolean;
         Is_Body          : Boolean;
         Process_Body     : Boolean;
         Loc_End          : Natural;
         Loc_Start        : Natural;
         Entity_Info      : Entity_Information) is abstract;
      --  Generate a link for the element Entity_Info on its declaration.
      --  Even if the format of the documentation doesn't use links, it's
      --  necessary to override Format_Link with an empty body.

      procedure Finish
        (B           : access Backend;
         File        : Ada.Text_IO.File_Type;
         Text        : String;
         Entity_Line : Natural) is abstract;
      --  Terminate processing of a block of code which has been analysed
      --  by Parse_Entities + Callback + Format_xxx

      function Get_Extension (B : access Backend) return String is abstract;
      --  Return the extension of doc files (eg. ".htm" for an instance of
      --  object Backend_HTML).

      function Get_Doc_Directory
        (B      : access Backend;
         Kernel : Kernel_Handle) return String is abstract;
      --  Return the path which must contain the documentation (eg.
      --  "/..../gps/glide/obj/html/" for an instance of object
      --  Backend_HTML).

      function Get_Last_Index (B : Backend'Class) return Natural;
      function Get_Last_Line (B : Backend'Class) return Natural;
      procedure Set_Last_Index (B : in out Backend'Class; Value : Natural);
      procedure Set_Last_Line (B : in out Backend'Class; Value : Natural);
      --  Getters ans setters of the 2 private fields.
      --  ??? What are these two private fields used for

   private

      type Backend is abstract tagged record
         Last_Index : Natural;
         Last_Line  : Natural;
      end record;

   end Docgen_Backend;

   use Docgen.Docgen_Backend;
   type Doc_Subprogram_Type is access procedure
     (B                : Backend_Handle;
      Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
      File             : in Ada.Text_IO.File_Type;
      Entity_List      : in out Type_Entity_List.List;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Info             : in out Doc_Info;
      Doc_Directory    : String;
      Doc_Suffix       : String);
   --  The procedure to define for each new output format

   procedure Format_All_Link
     (B                : access Backend'Class;
      Entity_List      : in out Type_Entity_List.List;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Start_Index      : Natural;
      Start_Line       : Natural;
      Start_Column     : Natural;
      End_Index        : Natural;
      Kernel           : access Kernel_Handle_Record'Class;
      File             : Ada.Text_IO.File_Type;
      LI_Unit          : LI_File_Ptr;
      Text             : String;
      File_Name        : VFS.Virtual_File;
      Entity_Line      : Natural;
      Line_In_Body     : Natural;
      Source_File_List : Type_Source_File_List.List;
      Link_All         : Boolean;
      Is_Body          : Boolean;
      Process_Body     : Boolean;
      Info             : Doc_Info);
   --  This procedure is used by formats of documentation like html to
   --  create links for each entity of the file File_Name on their
   --  own declaration. It's called by the method Format_Identifier of
   --  a child instance of a Backend object (eg. Backend_HTML).
   --  This process is done in Docgen because for each entity we must search
   --  for its declaration in all concerned files: this work is
   --  independant of the choosen format of documentation.

   function Count_Lines (Line : String) return Natural;
   --  Return the number of lines in the String

   function Count_Points (Text : String) return Natural;
   --  Return the number of point in the given string

   function Get_Doc_File_Name
     (Source_Filename : VFS.Virtual_File;
      Source_Path     : String;
      Doc_Suffix      : String) return String;
   --  Return a string with the name for the new doc file:
   --  first the doc path is added in front of the created name
   --  then the "." in front of the suffix is replaced by "_",
   --  so that a new output format suffix can be added

   function Source_File_In_List
     (Source_File_List : Type_Source_File_List.List;
      Name             : VFS.Virtual_File) return Boolean;
   --  Return true if the file is found in the source file list

   function Spec_Suffix
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File   : VFS.Virtual_File) return String;
   --  Return the spec suffix, without the "." in front, corresponding to
   --  the given file name. This given file can be a body or a spec.
   --  As using Other_File_Name this function works for all suffix's.
   --  ??? Wrong, due to call to Is_Spec_File

   function Body_Suffix
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File   : VFS.Virtual_File) return String;
   --  Return the body suffix, without the "." in front, corresponding to
   --  the given file name. This given file can be a body or a spec.
   --  As using Other_File_Name this function works for all suffix's.

   function Is_Spec_File
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File   : VFS.Virtual_File) return Boolean;
   --  Return whether the File is a Spec file

end Docgen;
