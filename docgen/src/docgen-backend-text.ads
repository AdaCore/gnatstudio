-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                         Copyright (C) 2005                        --
--                              AdaCore                              --
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

--  This package contains the spec of the text backend. It inherits
--  from the abstract backend object (see docgen.ads). It's responsible
--  for all textual documentation.

with VFS; use VFS;

package Docgen.Backend.Text is

   type Backend is new Docgen.Backend.Backend with null record;

   type Backend_Handle is access all Backend'Class;

   procedure Initialize (B : access Backend; Text : String);
   --  Initialize the private fields before the documentation process

   procedure Doc_Open
     (B          : access Backend;
      Kernel     : access Kernel_Handle_Record'Class;
      Result     : in out Unbounded_String;
      Open_Title : String);

   procedure Doc_Close
     (B      : access Backend;
      Kernel : access Kernel_Handle_Record'Class;
      Result : in out Unbounded_String);

   procedure Doc_Header
     (B              : access Backend;
      Kernel         : access Kernel_Handle_Record'Class;
      Result         : in out Unbounded_String;
      Header_File    : Virtual_File;
      Header_Package : String;
      Header_Line    : Natural;
      Header_Link    : Boolean);

   procedure Doc_Header_Private
     (B            : access Backend;
      Kernel       : access Kernel_Handle_Record'Class;
      Result       : in out Unbounded_String;
      Header_Title : String;
      Level        : Natural);

   procedure Doc_Footer
     (B      : access Backend;
      Kernel : access Kernel_Handle_Record'Class;
      Result : in out Unbounded_String);

   procedure Doc_Subtitle
     (B             : access Backend;
      Kernel        : access Kernel_Handle_Record'Class;
      Result        : in out Unbounded_String;
      Level         : Natural;
      Subtitle_Name : String);

   procedure Doc_With
     (B                : access Backend;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Source_File_List : Type_Source_File_Table.HTable;
      Options          : All_Options;
      Level            : Natural;
      With_Header      : String;
      With_File        : VFS.Virtual_File;
      With_Header_Line : Natural);

   procedure Doc_Package
     (B                : access Backend;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Source_File_List : Type_Source_File_Table.HTable;
      Options          : All_Options;
      Level            : Natural;
      Package_Entity   : Entity_Information;
      Package_Header   : String);

   procedure Doc_Package_Open_Close
     (B                : access Backend;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Source_File_List : Type_Source_File_Table.HTable;
      Options          : All_Options;
      Level            : Natural;
      Entity           : Entity_Information;
      Header           : String);

   procedure Doc_Package_Desc
     (B           : access Backend;
      Kernel      : access Kernel_Handle_Record'Class;
      Result      : in out Unbounded_String;
      Level       : Natural;
      Description : String);

   procedure Doc_Var
     (B                : access Backend;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Source_File_List : Type_Source_File_Table.HTable;
      Options          : All_Options;
      Level            : Natural;
      Entity           : Entity_Information;
      Header           : String);

   procedure Doc_Exception
     (B                : access Backend;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Source_File_List : Type_Source_File_Table.HTable;
      Options          : All_Options;
      Level            : Natural;
      Entity           : Entity_Information;
      Header           : String);

   procedure Doc_Type
     (B                : access Backend;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Source_File_List : Type_Source_File_Table.HTable;
      Options          : All_Options;
      Level            : Natural;
      Entity           : Entity_Information;
      Header           : String);

   procedure Doc_Entry
     (B                : access Backend;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Source_File_List : Type_Source_File_Table.HTable;
      Options          : All_Options;
      Level            : Natural;
      Entity           : Entity_Information;
      Header           : String);

   procedure Doc_Subprogram
     (B                : access Backend;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Source_File_List : Type_Source_File_Table.HTable;
      Options          : All_Options;
      Level            : Natural;
      Entity           : Entity_List_Information;
      Header           : String);

   procedure Doc_Caller_References
     (B                 : access Backend;
      Kernel            : access Kernel_Handle_Record'Class;
      Result            : in out Unbounded_String;
      Options           : All_Options;
      Level             : Natural;
      Callers           : Entities.Entity_Information_Arrays.Instance;
      Processed_Sources : Type_Source_File_Table.HTable);

   procedure Doc_Calls_References
     (B                 : access Backend;
      Kernel            : access Kernel_Handle_Record'Class;
      Result            : in out Unbounded_String;
      Options           : All_Options;
      Level             : Natural;
      Calls             : Entities.Entity_Information_Arrays.Instance;
      Processed_Sources : Type_Source_File_Table.HTable);

   procedure Doc_Tagged_Type
     (B                : access Backend;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      Source_File_List : Type_Source_File_Table.HTable;
      Level            : Natural;
      Entity           : Entity_Information);

   procedure Doc_Unit_Index
     (B                : access Backend;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      Source_File_List : Type_Source_File_Table.HTable;
      Options          : All_Options;
      Level            : Natural;
      Doc_Directory    : String);

   procedure Doc_Subprogram_Index
     (B       : access Backend;
      Kernel  : access Kernel_Handle_Record'Class;
      Result  : in out Unbounded_String;
      Options : All_Options);

   procedure Doc_Type_Index
     (B       : access Backend;
      Kernel  : access Kernel_Handle_Record'Class;
      Result  : in out Unbounded_String;
      Options : All_Options);

   procedure Doc_Tagged_Type_Index
     (B      : access Backend;
      Kernel : access Kernel_Handle_Record'Class;
      Result : in out Unbounded_String);

   procedure Doc_Private_Index
     (B      : access Backend;
      Kernel : access Kernel_Handle_Record'Class;
      Result : in out Unbounded_String;
      Title  : String);

   procedure Doc_Public_Index
     (B      : access Backend;
      Kernel : access Kernel_Handle_Record'Class;
      Result : in out Unbounded_String;
      Title  : String);

   procedure Doc_End_Of_Index
     (B      : access Backend;
      Kernel : access Kernel_Handle_Record'Class;
      Result : in out Unbounded_String);

   procedure Doc_Index_Tagged_Type
     (B                : access Backend;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      Source_File_List : Type_Source_File_Table.HTable;
      Entity           : Entity_Information;
      Family           : Family_Type);

   procedure Doc_Index_Item
     (B         : access Backend;
      Kernel    : access Kernel_Handle_Record'Class;
      Result    : in out Unbounded_String;
      Name      : String;
      Item_File : Entities.Source_File;
      Line      : Natural;
      Doc_File  : String);

   procedure Doc_Body
     (B                : access Backend;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Source_File_List : Type_Source_File_Table.HTable;
      Options          : All_Options;
      Level            : Natural;
      Body_File        : VFS.Virtual_File;
      Body_Text        : String);

   procedure Doc_Description
     (B           : access Backend;
      Kernel      : access Kernel_Handle_Record'Class;
      Result      : in out Unbounded_String;
      Level       : Natural;
      Description : String);

   procedure Format_Comment
     (B           : access Backend;
      Kernel      : access Kernel_Handle_Record'Class;
      Result      : in out Unbounded_String;
      Text        : String;
      Sloc_Start  : Source_Location;
      Sloc_End    : Source_Location;
      Entity_Line : Natural);
   --  Generate a comment in Text format

   procedure Format_Keyword
     (B           : access Backend;
      Kernel      : access Kernel_Handle_Record'Class;
      Result      : in out Unbounded_String;
      Text        : String;
      Sloc_Start  : Source_Location;
      Sloc_End    : Source_Location;
      Entity_Line : Natural);
   --  Generate a keyword in Text format

   procedure Format_String
     (B           : access Backend;
      Kernel      : access Kernel_Handle_Record'Class;
      Result      : in out Unbounded_String;
      Text        : String;
      Sloc_Start  : Source_Location;
      Sloc_End    : Source_Location;
      Entity_Line : Natural);
   --  Generate a string (between two ") in Text format

   procedure Format_Character
     (B           : access Backend;
      Kernel      : access Kernel_Handle_Record'Class;
      Result      : in out Unbounded_String;
      Text        : String;
      Sloc_Start  : Source_Location;
      Sloc_End    : Source_Location;
      Entity_Line : Natural);
   --  Generate a character (between two ') in Text format

   procedure Format_Identifier
     (B                : access Backend;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Text             : String;
      Sloc_Start       : Source_Location;
      Sloc_End         : Source_Location;
      File_Name        : VFS.Virtual_File;
      Entity_Line      : Natural;
      Line_In_Body     : Natural;
      Source_File_List : Type_Source_File_Table.HTable;
      Link_All         : Boolean;
      Is_Body          : Boolean;
      Process_Body     : Boolean;
      Level            : Natural;
      Indent           : Natural);
   --  Generate an identifier in Text format

   procedure Format_Link
     (B                : access Backend;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      Text             : String;
      Sloc_Start       : Source_Location;
      Sloc_End         : Source_Location;
      File_Name        : VFS.Virtual_File;
      Entity_Line      : Natural;
      Line_In_Body     : Natural;
      Source_File_List : Type_Source_File_Table.HTable;
      Link_All         : Boolean;
      Is_Body          : Boolean;
      Process_Body     : Boolean;
      Loc_End          : Natural;
      Loc_Start        : Natural;
      Entity_Info      : Entity_Information;
      Entity_Abstract  : in out Boolean);
   --  Generate a link for the element Entity_Info on its declaration

   procedure Finish
     (B           : access Backend;
      Kernel      : access Kernel_Handle_Record'Class;
      Result      : in out Unbounded_String;
      Text        : String;
      Entity_Line : Natural);
   --  Terminate the handling of a set of code which has been analysed
   --  by Parse_Entities + Callback + Format_xxx

   function Get_Doc_Directory
     (B      : access Backend;
      Kernel : access Kernel_Handle_Record'Class) return String;
   --  Return the path which must contains the documentation, e.g:
   --  "/.../gps/obj/text/"

end Docgen.Backend.Text;
