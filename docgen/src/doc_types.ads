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
with List_Utils;                use List_Utils;
with Generic_List;

package Doc_Types is

   Max_Line_Length : constant Natural := 160;

   Dummy_Exception : exception;

   --  the structures for the list of the source files
   type Source_File_Information is record
      File_Name        : GNAT.OS_Lib.String_Access;
      Prj_File_Name    : GNAT.OS_Lib.String_Access;
      Package_Name     : GNAT.OS_Lib.String_Access;
      Def_In_Line      : Integer;
      Other_File_Found : Boolean;
   end record;
   procedure Free (X : in out Source_File_Information);
   package Type_Source_File_List is
     new Generic_List (Source_File_Information);
   function Compare_Elements (X, Y : Source_File_Information) return Boolean;
   procedure Sort_List_Name is
     new Sort (Type_Source_File_List, "<" => Compare_Elements);

   --  the structures for the list of reference entities
   --  used in type Entity_List_Information
   --  this one will be used to save the positions,
   --  where a subprogram is called
   type Reference_List_Information is record
      File_Name       : GNAT.OS_Lib.String_Access;
      File_Found      : Boolean;
      Column          : Natural;
      Line            : Positive;
   end record;
   procedure Free (X : in out Reference_List_Information);
   package Type_Reference_List is
     new Generic_List (Reference_List_Information);
   function Compare_Elements_Column
     (X, Y : Reference_List_Information) return Boolean;
   procedure Sort_List_Column is
     new Sort (Type_Reference_List, "<" => Compare_Elements_Column);

   type Entity_Type is (Procedure_Entity,
                        Function_Entity,
                        Exception_Entity,
                        Type_Entity,
                        Var_Entity,
                        Package_Entity,
                        Other_Entity);

   --  the structures for the list of entities
   type Entity_List_Information is record
      --  these are items will be used by every entity
      --  except when used in Doc_Info with Body_Line_Info (see below)
      Kind            : Entity_Type;
      Name            : GNAT.OS_Lib.String_Access;
      Short_Name      : GNAT.OS_Lib.String_Access;
      File_Name       : GNAT.OS_Lib.String_Access;
      File_Found      : Boolean;
      Column          : Natural;
      Line            : Integer;
      Is_Private      : Boolean;
      --  the following items can be used if necessery
      Header          : GNAT.OS_Lib.String_Access;
      Header_Lines    : Natural;
      Line_In_Body    : Positive;
      Ref_List        : Type_Reference_List.List;
   end record;
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
   procedure Sort_List_Column is
     new Sort (Type_Entity_List, "<" => Compare_Elements_Column);
   procedure Sort_List_Name   is
     new Sort (Type_Entity_List, "<" => Compare_Elements_Name);
   --  Sort_List_Name sorts the entities in alphabetical order,
   --  BUT all public entites stand in front of the private

   type Info_Types is (Open_Info, Close_Info,
                       Subtitle_Info, Exception_Info, Type_Info,
                       Subprogram_Info, Header_Info, Footer_Info,
                       With_Info, Package_Desc_Info,
                       Unit_Index_Info, Type_Index_Info,
                       Subprogram_Index_Info, End_Of_Index_Info,
                       Index_Item_Info, Body_Line_Info, Var_Info,
                       Package_Info);


   --  the data structure used to pass the information
   --  to the procedure which is defined
   --  as the Doc_Subprogram_Type (see below) to
   --  be the only procedure to be defined
   --  when a new output format should be added
   type Doc_Info (Info_Type : Info_Types) is
      record
         case Info_Type is

               --  used to at the very beginning of the file
            when Open_Info =>
               Open_Title                    : GNAT.OS_Lib.String_Access;
               Open_File                     : GNAT.OS_Lib.String_Access;
               Open_Package_List             : Type_Source_File_List.List;

               --  used at the end of the file
            when Close_Info =>
               Close_Title                   : GNAT.OS_Lib.String_Access;

               --  used to start an entity information
            when Header_Info =>
               Header_Package                : GNAT.OS_Lib.String_Access;
               Header_File                   : GNAT.OS_Lib.String_Access;
               Header_Link                   : Boolean;
               Header_Line                   : Integer;
               Header_Package_Next           : GNAT.OS_Lib.String_Access;
               Header_Package_Prev           : GNAT.OS_Lib.String_Access;

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
               With_Lines                    : GNAT.OS_Lib.String_Access;
               With_List                     : Type_Entity_List.List;
               With_File                     : GNAT.OS_Lib.String_Access;

            when Package_Info =>
               Package_Entity                : Entity_List_Information;
               Package_Description           : GNAT.OS_Lib.String_Access;
               Package_List                  : Type_Entity_List.List;

               --  used to add the package description
            when Package_Desc_Info =>
               Package_Desc_Description      : GNAT.OS_Lib.String_Access;

               --  used to add a constant and named numbers
            when Var_Info =>
               Var_Entity                    : Entity_List_Information;
               Var_Description               : GNAT.OS_Lib.String_Access;
               Var_List                      : Type_Entity_List.List;

               --  used to add an exception info to the information file
            when Exception_Info =>
               Exception_Entity              : Entity_List_Information;
               Exception_Description         : GNAT.OS_Lib.String_Access;
               Exception_List                : Type_Entity_List.List;

               --  used to add a type info to the information file
            when Type_Info =>
               Type_Entity                   : Entity_List_Information;
               Type_Description              : GNAT.OS_Lib.String_Access;
               Type_List                     : Type_Entity_List.List;

               --  used to add a subprogram info to the information file
            when Subprogram_Info =>
               Subprogram_Entity             : Entity_List_Information;
               Subprogram_Description        : GNAT.OS_Lib.String_Access;
               Subprogram_Link               : Boolean;
               Subprogram_List               : Type_Entity_List.List;

               --  used to start the package index file
            when Unit_Index_Info =>
               --  the first file to show (for ex. by index.html)
               First_File                    : GNAT.OS_Lib.String_Access;
               --  to create the index.html if necessary
               Doc_Directory                 : GNAT.OS_Lib.String_Access;

               --  used to start the subprogram index file
            when Subprogram_Index_Info =>
               First_Dummy                   : GNAT.OS_Lib.String_Access;

               --  used to start the type index file
            when Type_Index_Info =>
               Second_Dummy                  : GNAT.OS_Lib.String_Access;

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
               Body_Text                     : GNAT.OS_Lib.String_Access;
               Body_File                     : GNAT.OS_Lib.String_Access;
               Body_List                     : Type_Entity_List.List;
            when others => null;  --  exception later
         end case;
      end record;

   --  the procedure to define for each new output format
   type Doc_Subprogram_Type is access
        procedure (File      : in Ada.Text_IO.File_Type;
                   Text_Type : in out Doc_Info);

   type All_Options is record
      Doc_Subprogram       : Doc_Subprogram_Type;
      --  subprogram to use in the output package
      Doc_Suffix           : GNAT.OS_Lib.String_Access;
      --  the suffix of the output file
      Doc_One_File         : Boolean := False;
      --  should all the packages be documented in only one file
      Process_Body_Files   : Boolean := False;
      --  create also the body documentation?
      Info_Output          : Boolean := False;
      --  show more information
      Ignorable_Comments   : Boolean := False;
      --  ignore all comments with "--!"
      Comments_Under       : Boolean := False;
      --  doc comments for entities under the header
      Show_Private         : Boolean := False;
      --  show also private entities
      Doc_Directory        : GNAT.OS_Lib.String_Access;
      --  where should the doc files be created
      References           : Boolean := False;
      --  True if the program should search for the references
      --  adding information like "subprogram called by..."
   end record;

   function Count_Lines
     (Line    : String) return Natural;
   --  returns the number of lines in the String

   function Get_String_Index
     (Type_Str  : String;
      Index     : Natural;
      Substring : String) return Natural;
   --  returns the index of the substring in the Type_Str
   --  the search starts at position Index
   --  if no position is found, return 0

end Doc_Types;
