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

with Ada.Text_IO;               use Ada.Text_IO;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Doc_Types;                 use Doc_Types;

package Work_On_Source is

   package GOL renames GNAT.OS_Lib;
   package ASU renames Ada.Strings.Unbounded;

   type Grecord is (a, b);

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

   function Get_Line_From_File
     (File_Name : String;
      Line      : Natural) return String;
   --  returns the Line in the file

   function Get_Doc_File_Name
     (Source_Filename : String;
      Source_Path     : String;
      Doc_Suffix      : String) return String;
   --  returns a string with the name for the new doc file:
   --  first the doc path is added in front of the created name
   --  then the "." in front of the suffix is replaced by "_",
   --  so that a new output format suffix can be added

private

   procedure Process_Open_File
     (Doc_File          : File_Type;
      Package_File      : String;
      Source_File_List  : in out Type_Source_File_List.List;
      Options           : All_Options);

   procedure Process_Close_File
     (Doc_File      : File_Type;
      Package_File  : String;
      Options       : All_Options);

   procedure Process_One_Body_File
     (Doc_File           : File_Type;
      Source_File        : String;
      Entity_List        : Type_Entity_List.List;
      Options            : All_Options);
   --  processes a body file: will take each line from the source file
   --  and give it to the output procedure

   procedure Process_Package_Description
     (Doc_File        : File_Type;
      Source_Filename : String;
      Package_Name    : String;
      Options         : All_Options);
   --  will process the comment lines at the beginning of the file

   procedure Process_With_Clause
     (Doc_File        : File_Type;
      Entity_List     : in out Type_Entity_List.List;
      Source_Filename : String;
      Package_Name    : String;
      Options         : All_Options);
   --  will process the lines at the beginning of the file
   --  starting with "with"

   procedure Process_Packages
     (Doc_File        : File_Type;
      Entity_List     : in out Type_Entity_List.List;
      Source_Filename : String;
      Package_Name    : String;
      Options         : All_Options);
   --  well process renamed and instantiated packages

   procedure Process_Vars
     (Doc_File        : File_Type;
      Entity_List     : in out Type_Entity_List.List;
      Source_Filename : String;
      Package_Name    : String;
      Options         : All_Options);
   --  called by Process_Source to work on the constants
   --  and named numbers

   procedure Process_Exceptions
     (Doc_File        : File_Type;
      Entity_List     : in out Type_Entity_List.List;
      Source_Filename : String;
      Package_Name    : String;
      Options         : All_Options);
   --  called by Process_Source to work on the exceptions

   procedure Process_Subprograms
     (Doc_File           : File_Type;
      Entity_List        : in out Type_Entity_List.List;
      Source_Filename    : String;
      Process_Body_File  : Boolean;
      Package_Name       : String;
      Options            : All_Options);
   --  called by Process_Source to work on the subprograms

   procedure Process_Types
     (Doc_File        : File_Type;
      Entity_List     : in out Type_Entity_List.List;
      Source_Filename : String;
      Package_Name    : String;
      Options         : All_Options);
   --  called by Process_Source to work on the types

   procedure Process_Header
     (Doc_File           : File_Type;
      Package_Name       : String;
      Next_Package       : GNAT.OS_Lib.String_Access;
      Prev_Package       : GNAT.OS_Lib.String_Access;
      Def_In_Line        : Integer;
      Package_File       : String;
      Process_Body_File  : Boolean;
      Options            : All_Options);

   procedure Process_Footer
     (Doc_File      : File_Type;
      Package_File  : String;
      Options       : All_Options);

   function Extract_Comment
     (File_Name           : String;
      Line                : Natural;
      Header_Lines        : Natural;
      Package_Description : Boolean;
      Options             : All_Options) return String;
   --  get the doc comments from the source file

   function Exception_Renames
     (File_Name : String;
      Line      : Natural) return Unbounded_String;
   --  check if the exception renames another one, in that case
   --  return the rest of the line

   procedure Go_To_Line
     (File : File_Type;
      Line : Natural);
   --  a primitive procedure to move some lines forward in the opened file

   function Line_Is_Comment
     (Line : String) return Boolean;
   --  returns true, if the first chars of the line are "--"

   function Line_Is_Empty
     (Line : String) return Boolean;
   --  returns true, if there is no text in this line

   function Is_Ignorable_Comment
     (Comment_Line : String) return Boolean;
   --  returns true, if the comment line starts with a "--!"

   function Kill_Prefix
     (Comment_Line : String) return String;
   --  returns the comment line without the "--" in front

end Work_On_Source;
