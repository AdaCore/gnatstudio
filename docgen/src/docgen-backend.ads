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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Docgen_Registry;       use Docgen_Registry;
with Language;              use Language;

package Docgen.Backend is

   type Backend
     (Output_Description : Output_Description_Access)
   is abstract tagged private;

   type Backend_Handle is access all Backend'Class;

   procedure Initialize
     (B : access Backend; Text : String) is abstract;
   --  Initialize the private fields before starting the documentation
   --  process.

   procedure Doc_Open
     (B          : access Backend;
      Kernel     : access Kernel_Handle_Record'Class;
      Result     : in out Unbounded_String;
      Open_Title : String) is abstract;
   --  Called each time a new file is created

   procedure Doc_Close
     (B      : access Backend;
      Kernel : access Kernel_Handle_Record'Class;
      Result : in out Unbounded_String) is abstract;
   --  Called each time a file is closed

   procedure Doc_Header
     (B              : access Backend;
      Kernel         : access Kernel_Handle_Record'Class;
      Result         : in out Unbounded_String;
      Header_File    : VFS.Virtual_File;
      Header_Package : String;
      Header_Line    : Natural;
      Header_Link    : Boolean) is abstract;

   procedure Doc_Header_Private
     (B            : access Backend;
      Kernel       : access Kernel_Handle_Record'Class;
      Result       : in out Unbounded_String;
      Header_Title : String;
      Level        : Natural) is abstract;

   procedure Doc_Footer
     (B      : access Backend;
      Kernel : access Kernel_Handle_Record'Class;
      Result : in out Unbounded_String) is abstract;
   --  Called when we finished processing an entity

   procedure Doc_Subtitle
     (B             : access Backend;
      Kernel        : access Kernel_Handle_Record'Class;
      Result        : in out Unbounded_String;
      Level         : Natural;
      Subtitle_Name : String) is abstract;
   --  Add a subtitle for the entity type to the documentation

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
      With_Header_Line : Natural) is abstract;
   --  With clauses for imported packages

   procedure Doc_Package
     (B                : access Backend;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Source_File_List : Type_Source_File_Table.HTable;
      Options          : All_Options;
      Level            : Natural;
      Package_Entity   : Entity_Information;
      Package_Header   : String) is abstract;
   --  Used to add a package info to the information file

   procedure Doc_Package_Open_Close
     (B                : access Backend;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Source_File_List : Type_Source_File_Table.HTable;
      Options          : All_Options;
      Level            : Natural;
      Entity           : Entity_Information;
      Header           : String) is abstract;
   --  Used to add a header or a footer for an inner package

   procedure Doc_Package_Desc
     (B           : access Backend;
      Kernel      : access Kernel_Handle_Record'Class;
      Result      : in out Unbounded_String;
      Level       : Natural;
      Description : String) is abstract;
   --  Used to add a package description

   procedure Doc_Var
     (B                : access Backend;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Source_File_List : Type_Source_File_Table.HTable;
      Options          : All_Options;
      Level            : Natural;
      Entity           : Entity_Information;
      Header           : String) is abstract;
   --  Used to add a constant or a named number

   procedure Doc_Exception
     (B                : access Backend;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Source_File_List : Type_Source_File_Table.HTable;
      Options          : All_Options;
      Level            : Natural;
      Entity           : Entity_Information;
      Header           : String) is abstract;
   --  Used to add an exception info to the information file

   procedure Doc_Type
     (B                : access Backend;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Source_File_List : Type_Source_File_Table.HTable;
      Options          : All_Options;
      Level            : Natural;
      Entity           : Entity_Information;
      Header           : String) is abstract;
   --  Used to add type information

   procedure Doc_Entry
     (B                : access Backend;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Source_File_List : Type_Source_File_Table.HTable;
      Options          : All_Options;
      Level            : Natural;
      Entity           : Entity_Information;
      Header           : String) is abstract;

   procedure Doc_Subprogram
     (B                : access Backend;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Source_File_List : Type_Source_File_Table.HTable;
      Options          : All_Options;
      Level            : Natural;
      Entity           : Entity_List_Information;
      Header           : String) is abstract;
   --  Add subprogram information

   procedure Doc_Caller_References
     (B                 : access Backend;
      Kernel            : access Kernel_Handle_Record'Class;
      Result            : in out Unbounded_String;
      Options           : All_Options;
      Level             : Natural;
      Callers           : Entities.Entity_Information_Arrays.Instance;
      Processed_Sources : Type_Source_File_Table.HTable) is abstract;
   --  Add the list of callers for a given entity

   procedure Doc_Calls_References
     (B                 : access Backend;
      Kernel            : access Kernel_Handle_Record'Class;
      Result            : in out Unbounded_String;
      Options           : All_Options;
      Level             : Natural;
      Calls             : Entities.Entity_Information_Arrays.Instance;
      Processed_Sources : Type_Source_File_Table.HTable) is abstract;
   --  Add the list of entities called by the current entity

   procedure Doc_Tagged_Type
     (B                : access Backend;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      Source_File_List : Type_Source_File_Table.HTable;
      Level            : Natural;
      Entity           : Entity_Information) is abstract;
   --  Add the list of parent and children for a tagged type

   procedure Doc_Unit_Index
     (B                : access Backend;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      Source_File_List : Type_Source_File_Table.HTable;
      Options          : All_Options;
      Level            : Natural;
      Doc_Directory    : String) is abstract;
   --  Start the output the Unit index file

   procedure Doc_Subprogram_Index
     (B       : access Backend;
      Kernel  : access Kernel_Handle_Record'Class;
      Result  : in out Unbounded_String;
      Options : All_Options) is abstract;
   --  Start the output of the subprogram index file

   procedure Doc_Type_Index
     (B       : access Backend;
      Kernel  : access Kernel_Handle_Record'Class;
      Result  : in out Unbounded_String;
      Options : All_Options) is abstract;
   --  Start the output of the type index file

   procedure Doc_Tagged_Type_Index
     (B      : access Backend;
      Kernel : access Kernel_Handle_Record'Class;
      Result : in out Unbounded_String) is abstract;
   --  Start the output of the index for tagged types

   procedure Doc_Private_Index
     (B      : access Backend;
      Kernel : access Kernel_Handle_Record'Class;
      Result : in out Unbounded_String;
      Title  : String) is abstract;
   --  Start a new section in the current index for private entities

   procedure Doc_Public_Index
     (B      : access Backend;
      Kernel : access Kernel_Handle_Record'Class;
      Result : in out Unbounded_String;
      Title  : String) is abstract;
   --  Start a new section in the current index for public entities

   procedure Doc_End_Of_Index
     (B      : access Backend;
      Kernel : access Kernel_Handle_Record'Class;
      Result : in out Unbounded_String) is abstract;
   --  Terminate the current index file

   procedure Doc_Index_Tagged_Type
     (B                : access Backend;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      Source_File_List : Type_Source_File_Table.HTable;
      Entity           : Entity_Information;
      Family           : Family_Type) is abstract;
   --  Add items to the tagged types index file

   procedure Doc_Index_Item
     (B         : access Backend;
      Kernel    : access Kernel_Handle_Record'Class;
      Result    : in out Unbounded_String;
      Name      : String;
      Item_File : Entities.Source_File;
      Line      : Natural;
      Doc_File  : String) is abstract;
   --  Add items to one of the index files (units, types and subprograms)

   procedure Doc_Body
     (B                : access Backend;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Source_File_List : Type_Source_File_Table.HTable;
      Options          : All_Options;
      Level            : Natural;
      Body_File        : VFS.Virtual_File;
      Body_Text        : String) is abstract;
   --  Process a whole body

   procedure Doc_Description
     (B           : access Backend;
      Kernel      : access Kernel_Handle_Record'Class;
      Result      : in out Unbounded_String;
      Level       : Natural;
      Description : String) is abstract;
   --  Pass comments written after or before the current entity

   procedure Format_Comment
     (B           : access Backend;
      Kernel      : access Kernel_Handle_Record'Class;
      Result      : in out Unbounded_String;
      Text        : String;
      Sloc_Start  : Source_Location;
      Sloc_End    : Source_Location;
      Entity_Line : Natural) is abstract;
   --  Format text as a comment

   procedure Format_Keyword
     (B           : access Backend;
      Kernel      : access Kernel_Handle_Record'Class;
      Result      : in out Unbounded_String;
      Text        : String;
      Sloc_Start  : Source_Location;
      Sloc_End    : Source_Location;
      Entity_Line : Natural) is abstract;
   --  Format text as a keyword

   procedure Format_String
     (B           : access Backend;
      Kernel      : access Kernel_Handle_Record'Class;
      Result      : in out Unbounded_String;
      Text        : String;
      Sloc_Start  : Source_Location;
      Sloc_End    : Source_Location;
      Entity_Line : Natural) is abstract;
   --  Format text as a string (between two ")

   procedure Format_Character
     (B           : access Backend;
      Kernel      : access Kernel_Handle_Record'Class;
      Result      : in out Unbounded_String;
      Text        : String;
      Sloc_Start  : Source_Location;
      Sloc_End    : Source_Location;
      Entity_Line : Natural) is abstract;
   --  Format text as a character (between two ')

   procedure Format_Identifier
     (B                : access Backend;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Text             : String;
      Sloc_Start  : Source_Location;
      Sloc_End    : Source_Location;
      File_Name        : VFS.Virtual_File;
      Entity_Line      : Natural;
      Line_In_Body     : Natural;
      Source_File_List : Type_Source_File_Table.HTable;
      Link_All         : Boolean;
      Is_Body          : Boolean;
      Process_Body     : Boolean;
      Level            : Natural;
      Indent           : Natural) is abstract;
   --  Format text as an identifier.
   --  Level : number of indentation levels.
   --  Indent: value of one indentation level.
   --  Those 2 parameters are used to associate identifiers returned by
   --  parse_entities in format_file (see docgen.adb) with a reference
   --  contained in the list made in Process_One_File
   --  (see docgen-work_on_file.adb).

   procedure Format_Code
     (B                : access Backend'Class;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Text             : String;
      File_Name        : VFS.Virtual_File;
      Entity_Line      : Natural;
      Line_In_Body     : Natural;
      Is_Body          : Boolean;
      Options          : All_Options;
      Source_File_List : Type_Source_File_Table.HTable;
      Level            : Natural;
      Indent           : Natural);
   --  Generate documentation for a type of code in the file
   --  (eg. packages, subprograms, exceptions ...). All tokens of this text
   --  are analysed by Parse_Entities.
   --  Parse_Entities calls Callback which reads the nature of the token
   --  and calls the appropriate subprogram on it (Format_String,
   --  Format_Character, Format_Comment, Format_Keyword, Format_Identifier)
   --  File        : current file processed.
   --  File_Name   : name of current file.
   --  Text        : String which must be processed.
   --  Info        : indicate which type of code must be processed. See
   --  definition of Doc_Info and Info_Types above.
   --  Entity_Line : for an reference, it contains the line in file.
   --  Used for links.
   --  List_Ref_In_File : list of the references in the current file.
   --  Line_In_Body     : for subprograms it's used to create not regular
   --  links (in this case it is not the line number of the declaration
   --  which is needed, but the line of the definition in the body). For
   --  public type which has private fields, it's used to make a link from
   --  private part to public part.
   --  Is_Body      : indicate if the current file is a spec/body file.
   --  Level, indent: same comments as those made for Format_Identifier.

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
      Entity_Abstract  : in out Boolean) is abstract;
   --  Generate a link for the element Entity_Info on its declaration.
   --  Even if the format of the documentation doesn't use links, it's
   --  necessary to override Format_Link with an empty body.
   --  Entity_Info : entity which defines the reference. It may be pointed
   --  by a link.
   --  Entity_Abstract : indicate if this entity is abstract. The value is
   --  set in Get_Declaration (e.g. see docgen_backend_html.adb)

   procedure Finish
     (B           : access Backend;
      Kernel      : access Kernel_Handle_Record'Class;
      Result      : in out Unbounded_String;
      Text        : String;
      Entity_Line : Natural) is abstract;
   --  Terminate processing of a block of code which has been analysed
   --  by Parse_Entities + Callback + Format_xxx

   function Get_Extension (B : access Backend'Class) return String;
   --  Returns the extension of the doc files generated by the backend

   function Get_Doc_Directory
     (B      : access Backend;
      Kernel : access Kernel_Handle_Record'Class) return String is abstract;
   --  Return the path which must contain the documentation (eg.
   --  "/.../gps/obj/html/" for an instance of object
   --  Backend_HTML).

   function Get_Last_Index (B : Backend'Class) return Natural;
   function Get_Last_Line (B : Backend'Class) return Natural;
   function Get_Indent (B : Backend'Class) return Natural;
   procedure Set_Last_Index (B : in out Backend'Class; Value : Natural);
   procedure Set_Last_Line (B : in out Backend'Class; Value : Natural);
   --  Getters ans setters of the private fields.
   --  Having private fields is a way to get rid of global variable.
   --  Fields used for formatting the text. There are moving bounds
   --  in a string.

   procedure Format_All_Link
     (B                : access Backend'Class;
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
   --  This procedure is used by formats of documentation like HTML to
   --  create links for each entity of the file File_Name on their
   --  own declaration. It's called by the method Format_Identifier of
   --  a child instance of a Backend object.
   --  Level, indent: same comments as those made for Format_Identifier.

private

   type Backend
     (Output_Description : Output_Description_Access)
   is abstract tagged record
      Indent     : Natural := 3;
      --  Number of space which correspond to one step of indentation
      --  ??? Use Get_Pref instead
      Last_Index : Natural;
      Last_Line  : Natural;
   end record;

end Docgen.Backend;
