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

--  This package defines the types and subprograms used by the other
--  Docgen packages. There are the three types needed for the lists,
--  the type All_Options, the Doc_Subprogram_Type and
--  the type Doc_Info, which will be used to destinguish, which
--  entity type should be processed by the output procedure.

with Ada.Text_IO;               use Ada.Text_IO;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with List_Utils;                use List_Utils;
with Src_Info;                  use Src_Info;
with Generic_List;

package Docgen is

   Max_Line_Length     : constant Natural := 160;
   First_File_Line     : constant Natural := 1;
   No_Body_Line_Needed : constant Natural := 0;

   type Source_File_Information is record
      File_Name        : GNAT.OS_Lib.String_Access;
      Prj_File_Name    : GNAT.OS_Lib.String_Access;
      Package_Name     : GNAT.OS_Lib.String_Access;
      Other_File_Found : Boolean;
   end record;
   --  the structures for the list of the source files
   procedure Free (X : in out Source_File_Information);
   package Type_Source_File_List is
     new Generic_List (Source_File_Information);
   function Compare_Elements (X, Y : Source_File_Information) return Boolean;
   procedure Sort_List_Name is
     new Sort (Type_Source_File_List, "<" => Compare_Elements);
   --  sort elements by name (BUT: the spec file in front of body file)

   type Reference_List_Information is record
      File_Name       : GNAT.OS_Lib.String_Access;
      Set_Link        : Boolean;   --  if False, no link will be set
      Column          : Natural;
      Line            : Positive;
      Subprogram_Name : GNAT.OS_Lib.String_Access;
      --  the structures for the list of reference entities
      --  used in type Entity_List_Information.
      --  This one will be used to save the positions,
      --  where a subprogram is called or which is called by him.
   end record;
   procedure Free (X : in out Reference_List_Information);
   package Type_Reference_List is
     new Generic_List (Reference_List_Information);
   function Compare_Elements_Column
     (X, Y : Reference_List_Information) return Boolean;
   function Compare_Elements_Name
     (X, Y : Reference_List_Information) return Boolean;
   procedure Sort_List_Column is
     new Sort (Type_Reference_List, "<" => Compare_Elements_Column);
   --  sort the list by column
   procedure Sort_List_Name is
     new Sort (Type_Reference_List, "<" => Compare_Elements_Name);
   --  sort the list by name


   type Entity_Type is (Subprogram_Entity,
                        Exception_Entity,
                        Type_Entity,
                        Var_Entity,
                        Package_Entity,
                        Entry_Entity,
                        Other_Entity);
   --  a simplfied list of possible entity types

   type Entity_List_Information is record
      Kind            : Entity_Type;
      Name            : GNAT.OS_Lib.String_Access;
      Short_Name      : GNAT.OS_Lib.String_Access;
      File_Name       : GNAT.OS_Lib.String_Access;
      Column          : Natural;
      Line            : Integer;
      Is_Private      : Boolean;
      --  The following items won't be used in the index lists
      Line_In_Body    : Natural;  --  if 0, no link to be set!
      Calls_List      : Type_Reference_List.List;
      Called_List     : Type_Reference_List.List;
   end record;
   --  the structures for the list of entities
   procedure Free (X : in out Entity_List_Information);
   package Type_Entity_List is new Generic_List (Entity_List_Information);
   function Compare_Elements_Name
     (X, Y : Entity_List_Information) return Boolean;
   function Compare_Elements_Line
     (X, Y : Entity_List_Information) return Boolean;
   function Compare_Elements_Column
     (X, Y : Entity_List_Information) return Boolean;
   procedure Sort_List_Line   is
     new Sort (Type_Entity_List, "<" => Compare_Elements_Line);
   --  sort list by line
   procedure Sort_List_Column is
     new Sort (Type_Entity_List, "<" => Compare_Elements_Column);
   --  sort list by column
   procedure Sort_List_Name   is
     new Sort (Type_Entity_List, "<" => Compare_Elements_Name);
   --  sort the entities in alphabetical order by name,
   --  BUT all public entites stand in front of the private

   type Info_Types is (Open_Info, Close_Info,
                       Subtitle_Info, Exception_Info, Type_Info,
                       Subprogram_Info, Header_Info, Footer_Info,
                       With_Info, Package_Desc_Info,
                       Unit_Index_Info, Type_Index_Info,
                       Subprogram_Index_Info, End_Of_Index_Info,
                       Index_Item_Info, Body_Line_Info, Var_Info,
                       Package_Info, Entry_Info);
   --  the structure used in the type Doc_Info.

   type Doc_Info;

   type Doc_Subprogram_Type is access
        procedure (File      : in Ada.Text_IO.File_Type;
                   Text_Type : in out Doc_Info);
   --  the procedure to define for each new output format


   type All_Options is record
      Doc_Subprogram       : Doc_Subprogram_Type;
      --  subprogram to use in the output package
      Doc_Suffix           : GNAT.OS_Lib.String_Access;
      --  the suffix of the output file
      Process_Body_Files   : Boolean := False;
      --  create also the body documentation?
      Info_Output          : Boolean := False;
      --  show more information
      Ignorable_Comments   : Boolean := False;
      --  ignore all comments with "--!"
      Comments_Above       : Boolean := False;
      --  doc comments for entities above the header
      Show_Private         : Boolean := False;
      --  show also private entities
      Doc_Directory        : GNAT.OS_Lib.String_Access;
      --  where should the doc files be created
      References           : Boolean := False;
      --  True if the program should search for the references
      --  adding information like "subprogram called by..."
      One_Doc_File         : Boolean := False;
      --  used for TexInfo: True, if the project.texi file should be
      --  build and the package files should be included there later.
      Project_Name         : GNAT.OS_Lib.String_Access;
      --  The name of the main project
      Link_All             : Boolean := False;
      --  Should links be created to entities whose declaration files
      --  aren't being processed
   end record;
   --  the type containing all the information which can be
   --  set by using the opions of the command line

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
               --  used to at the very beginning of the file
            when Open_Info =>
               Open_Title                    : GNAT.OS_Lib.String_Access;
               Open_File                     : GNAT.OS_Lib.String_Access;
               Open_Package_Next             : GNAT.OS_Lib.String_Access;
               Open_Package_Prev             : GNAT.OS_Lib.String_Access;

               --  used at the end of the file
            when Close_Info =>
               Close_File_Name               : GNAT.OS_Lib.String_Access;

               --  used to start an entity information
            when Header_Info =>
               Header_Package                : GNAT.OS_Lib.String_Access;
               Header_File                   : GNAT.OS_Lib.String_Access;
               Header_Link                   : Boolean;
               Header_Line                   : Natural;

               --  used to finish an entity information
            when Footer_Info =>
               Footer_Title                  : GNAT.OS_Lib.String_Access;
               Footer_File                   : GNAT.OS_Lib.String_Access;

               --  used to add a subtitle to the information file
            when Subtitle_Info =>
               Subtitle_Name                 : GNAT.OS_Lib.String_Access;
               Subtitle_Package              : GNAT.OS_Lib.String_Access;
               Subtitle_Kind                 : Info_Types;

            when With_Info =>
               With_Header                   : GNAT.OS_Lib.String_Access;
               With_File                     : GNAT.OS_Lib.String_Access;
               With_Header_Line              : Natural;

            when Package_Info =>
               Package_Entity                : Entity_List_Information;
               Package_Header                : GNAT.OS_Lib.String_Access;
               Package_Header_Line           : Natural;
               Package_Description           : GNAT.OS_Lib.String_Access;

               --  used to add the package description
            when Package_Desc_Info =>
               Package_Desc_Description      : GNAT.OS_Lib.String_Access;

               --  used to add a constant and named numbers
            when Var_Info =>
               Var_Entity                    : Entity_List_Information;
               Var_Header                    : GNAT.OS_Lib.String_Access;
               Var_Header_Line               : Natural;
               Var_Description               : GNAT.OS_Lib.String_Access;

               --  used to add an exception info to the information file
            when Exception_Info =>
               Exception_Entity              : Entity_List_Information;
               Exception_Header              : GNAT.OS_Lib.String_Access;
               Exception_Header_Line              : Natural;
               Exception_Description         : GNAT.OS_Lib.String_Access;

               --  used to add a type info to the information file
            when Type_Info =>
               Type_Entity                   : Entity_List_Information;
               Type_Header                   : GNAT.OS_Lib.String_Access;
               Type_Header_Line              : Natural;
               Type_Description              : GNAT.OS_Lib.String_Access;

               --  used to add an entry info to the information file
            when Entry_Info =>
               Entry_Entity                  : Entity_List_Information;
               Entry_Header                  : GNAT.OS_Lib.String_Access;
               Entry_Header_Line             : Natural;
               Entry_Description             : GNAT.OS_Lib.String_Access;
               Entry_Link                    : Boolean;

               --  used to add a subprogram info to the information file
            when Subprogram_Info =>
               Subprogram_Entity             : Entity_List_Information;
               Subprogram_Header             : GNAT.OS_Lib.String_Access;
               Subprogram_Header_Line        : Natural;
               Subprogram_Description        : GNAT.OS_Lib.String_Access;
               Subprogram_Link               : Boolean;
               Subprogram_List               : Type_Entity_List.List;

               --  used to start the package index file
            when Unit_Index_Info =>
               --  the list of the files
               Unit_File_List                : Type_Source_File_List.List;
               --  the name doc file name without the suffix
               Unit_Index_File_Name          : GNAT.OS_Lib.String_Access;
               Unit_Project_Name             : GNAT.OS_Lib.String_Access;

               --  used to start the subprogram index file
            when Subprogram_Index_Info =>
               --  the doc file name without the suffix
               Subprogram_Index_File_Name    : GNAT.OS_Lib.String_Access;

               --  used to start the type index file
            when Type_Index_Info =>
               --  the name doc file name without the suffix
               Type_Index_File_Name          : GNAT.OS_Lib.String_Access;

               --  used to finish all 3 kinds of index files
            when End_Of_Index_Info =>
               End_Index_Title               : GNAT.OS_Lib.String_Access;

               --  used to add items to all 3 kindes of index files
            when Index_Item_Info =>
               Item_Name                     : GNAT.OS_Lib.String_Access;
               Item_File                     : GNAT.OS_Lib.String_Access;
               Item_Line                     : Natural;
               Item_Doc_File                 : GNAT.OS_Lib.String_Access;

               --  used to pass the information of one line in the body file
            when Body_Line_Info =>
               Body_File                     : GNAT.OS_Lib.String_Access;
               Body_Text                     : GNAT.OS_Lib.String_Access;
            when others => null;  --  exception later
         end case;
      end record;
   --  The data structure used to pass the information
   --  to the procedure which is defined
   --  as the Doc_Subprogram_Type (see below) to
   --  be the only procedure to be defined
   --  when a new output format should be added

   function Count_Lines
     (Line    : String) return Natural;
   --  returns the number of lines in the String

   function Get_String_Index
     (Type_Str  : String;
      Index     : Natural;
      Substring : String) return Natural;
   --  returns the index of the substring in the Type_Str.
   --  The search starts at position Index,
   --  if no position is found, return 0.

   function Get_Doc_File_Name
     (Source_Filename : String;
      Source_Path     : String;
      Doc_Suffix      : String) return String;
   --  returns a string with the name for the new doc file:
   --  first the doc path is added in front of the created name
   --  then the "." in front of the suffix is replaced by "_",
   --  so that a new output format suffix can be added

   function Source_File_In_List
     (Source_File_List : Type_Source_File_List.List;
      Name             : String) return Boolean;
   --  returns true if the file is found in the source file list

   function Count_Points (Text : String) return Natural;
   --  returns the number of point in the given string

   function Spec_Suffix (Name_Of_File : String) return String;
   --  return the spec suffix, without the "." in front, corresponding to
   --  the given file name. This given file can be a body or a spec.
   --  As using Other_File_Name this function works for all suffix's.

   function Body_Suffix (Name_Of_File : String) return String;
   --  return the body suffix, without the "." in front, corresponding to
   --  the given file name. This given file can be a body or a spec.
   --  As using Other_File_Name this function works for all suffix's.

   function File_Name_Without_Suffix
     (Name_Of_File : String) return String;
   --  return the file name without the point and the suffix at the end
   --  ??? This is a duplicate of Base_Name

   function Is_Spec_File (Name_Of_File : String) return Boolean;
   --  returns True, if the File is a Spec file
   --  ??? This function is not the final implementation !!!

   function Other_File_Name (Name_Of_File : String) return String;
   --  Return the full path name to the other file associated with
   --  Name_Of_File (the spec if Name_Of_File is a body or separate,
   --  the body if Nme_Of_File is the spec).
   --  The empty string is returned if the file wasn't found (and error
   --  messages are printed to the console appropriately).
   --  ??? This function is not the final implementation !!!

end Docgen;
