-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
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

with Ada.Text_IO;               use Ada.Text_IO;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Language;                  use Language;

package Docgen.Work_On_Source is

   package GOL renames GNAT.OS_Lib;

   procedure Process_Source
     (Doc_File           : File_Type;
      First_File         : Boolean;
      Last_File          : Boolean;
      Next_Package       : GNAT.OS_Lib.String_Access;
      Prev_Package       : GNAT.OS_Lib.String_Access;
      Source_File_List   : in out Type_Source_File_List.List;
      Source_Filename    : String;
      Package_Name       : String;
      Def_In_Line        : Integer;
      Entity_List        : in out Type_Entity_List.List;
      Process_Body_File  : Boolean;
      Options            : All_Options);
   --  with the data from the lists, the soucre file and the config file,
   --  create the Strings for the output

   procedure Process_Unit_Index
     (Source_File_List : Type_Source_File_List.List;
      Options          : All_Options);
   --  creates the index file for the packages

   procedure Process_Subprogram_Index
     (Subprogram_Index_List : Type_Entity_List.List;
      Options               : All_Options);
   --  creates the index file for the subprograms

   procedure Process_Type_Index
     (Type_Index_List : Type_Entity_List.List;
      Options          : All_Options);
   --  creates the index file for the types

   function Get_Doc_File_Name
     (Source_Filename : String;
      Source_Path     : String;
      Doc_Suffix      : String) return String;
   --  returns a string with the name for the new doc file:
   --  first the doc path is added in front of the created name
   --  then the "." in front of the suffix is replaced by "_",
   --  so that a new output format suffix can be added

private

   procedure Process_One_Body_File
     (Doc_File           : File_Type;
      Source_File        : String;
      Entity_List        : Type_Entity_List.List;
      File_Text          : GNAT.OS_Lib.String_Access;
      Options            : All_Options);
   --  will pass the information about the body file to the output
   --  subprogram. This is the only subprogram working on the contents
   --  of the body source files.

   procedure Process_Open_File
     (Doc_File         : File_Type;
      Package_File     : String;
      Source_File_List : in out Type_Source_File_List.List;
      Options          : All_Options);
   --  is always the first subprogram to be called, as it creates the
   --  very beginning of the documentation by calling the output
   --  subprogram

   procedure Process_Close_File
     (Doc_File      : File_Type;
      Options       : All_Options);
   --  is always the last subprogram to be called, as it creates the
   --  very end of the documentation by calling the output subprogram

   procedure Process_Package_Description
     (Doc_File        : File_Type;
      Package_Name    : String;
      Text            : String;
      Options         : All_Options);
   --  extracts all the comment lines of the source file which are at the
   --  beginning of it. Empty lines are ignored, the procedure stops when
   --  first command is found. This information will be passed to the
   --  output subprogram

   procedure Process_With_Clause
     (Doc_File        : File_Type;
      Entity_List     : in out Type_Entity_List.List;
      Source_Filename : String;
      Package_Name    : String;
      Parsed_List     : Construct_List;
      File_Text       : GNAT.OS_Lib.String_Access;
      Options         : All_Options);
   --  will process the lines at the beginning of the source file
   --  starting with "with" and pass them to the output subprogram

   procedure Process_Packages
     (Doc_File        : File_Type;
      Entity_List     : in out Type_Entity_List.List;
      Source_Filename : String;
      Package_Name    : String;
      Parsed_List     : Construct_List;
      File_Text       : GNAT.OS_Lib.String_Access;
      Options         : All_Options);
   --  will process renamed and instantiated packages and pass
   --  them to the output subprogram

   procedure Process_Vars
     (Doc_File        : File_Type;
      Entity_List     : in out Type_Entity_List.List;
      Source_Filename : String;
      Package_Name    : String;
      Parsed_List     : Construct_List;
      File_Text       : GNAT.OS_Lib.String_Access;
      Options         : All_Options);
   --  called by Process_Source to work on the constants
   --  and named numbers and pass each of them to the output subprogram

   procedure Process_Exceptions
     (Doc_File        : File_Type;
      Entity_List     : in out Type_Entity_List.List;
      Source_Filename : String;
      Package_Name    : String;
      Parsed_List     : Construct_List;
      File_Text       : GNAT.OS_Lib.String_Access;
      Options         : All_Options);
   --  called by Process_Source to work on the exceptions and
   --  pass each of them to the output subprogram

   procedure Process_Subprograms
     (Doc_File           : File_Type;
      Entity_List        : in out Type_Entity_List.List;
      Source_Filename    : String;
      Process_Body_File  : Boolean;
      Package_Name       : String;
      Parsed_List        : Construct_List;
      File_Text          : GNAT.OS_Lib.String_Access;
      Options            : All_Options);
   --  called by Process_Source to work on the subprograms and
   --  pass each of them to the output subprogram

   procedure Process_Types
     (Doc_File        : File_Type;
      Entity_List     : in out Type_Entity_List.List;
      Source_Filename : String;
      Package_Name    : String;
      Parsed_List     : Construct_List;
      File_Text       : GNAT.OS_Lib.String_Access;
      Options         : All_Options);
   --  called by Process_Source to work on the types and
   --  pass each of them to the output subprogram

   procedure Process_Header
     (Doc_File           : File_Type;
      Package_Name       : String;
      Next_Package       : GNAT.OS_Lib.String_Access;
      Prev_Package       : GNAT.OS_Lib.String_Access;
      Def_In_Line        : Integer;
      Package_File       : String;
      Process_Body_File  : Boolean;
      Options            : All_Options);
   --  will call the output subprogram to create the header of
   --  the package. This is NOT the same as Process_Open_File,
   --  if TexInfo doc is created, the file is opened only once,
   --  but the Header has to be set in front of each package.

   procedure Process_Footer
     (Doc_File      : File_Type;
      Package_File  : String;
      Options       : All_Options);
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
   --  returns true, if the first chars of the line are "--"

   function Line_Is_Empty
     (Line : String) return Boolean;
   --  returns true, if there is no text in this line

   function Is_Ignorable_Comment
     (Comment_Line : String) return Boolean;
   --  returns true, if the comment line starts with a "--!"
   --  It must be sure, that Comment_List is a comment line!

   function Kill_Prefix
     (Comment_Line : String) return String;
   --  returns the comment line without the "--" in front

   function Get_Whole_Header
     (File_Text   : String;
      Parsed_List : Construct_List;
      Entity_Name : String;
      Entity_Line : Natural) return GNAT.OS_Lib.String_Access;
   --  returns the Header of the entity

   function Get_Line_From_String
     (Text    : String;
      Line_Nr : Natural) return String;
   --  returns the wished Line from the String

end Docgen.Work_On_Source;
