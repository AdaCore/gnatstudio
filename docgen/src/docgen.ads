-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2004                       --
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
--  Docgen packages. There are the two types needed for the lists,
--  the type All_Options, the type Doc_Info, which will be used to destinguish
--  which entity type should be processed by the output procedure.

with GNAT.OS_Lib;               use GNAT.OS_Lib;
with List_Utils;                use List_Utils;
with Entities;                  use Entities;
with Glide_Kernel;              use Glide_Kernel;
with VFS;
with Generic_List;
with HTables;

package Docgen is

   Max_Line_Length     : constant Natural := 160;
   First_File_Line     : constant Natural := 1;
   No_Body_Line_Needed : constant Natural := 0;

   type Source_File_Information is record
      Package_Name  : GNAT.OS_Lib.String_Access;
      Doc_File_Name : GNAT.OS_Lib.String_Access;
      --  The base name of the output file that contains the documentation for
      --  this source file.
      Is_Spec       : Boolean;
   end record;
   No_Source_File_Information : constant Source_File_Information :=
     (Package_Name => null, Doc_File_Name => null, Is_Spec => False);
   --  Description of a source file for which documentation should be
   --  generated.

   procedure Free (X : in out Source_File_Information);
   --  Free the information associated with X

   type HTable_Header is new Natural range 0 .. 3000;
   function Hash (Key : Entities.Source_File) return HTable_Header;
   package Type_Source_File_Table is new HTables.Simple_HTable
     (Header_Num   => HTable_Header,
      Element      => Source_File_Information,
      No_Element   => No_Source_File_Information,
      Free_Element => Free,
      Key          => Entities.Source_File,
      Hash         => Hash,
      Equal        => Entities."=");

   type Reference_List_Information is record
      Entity          : Entity_Information;
      Set_Link        : Boolean;
      --  if False, no link will be set
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
     new List_Utils.Sort (Type_Reference_List, "<" => Compare_Elements_Column);
   --  Sort the list by column

   procedure Sort_List_Name is
     new List_Utils.Sort (Type_Reference_List, "<" => Compare_Elements_Name);
   --  Sort the list by name

   procedure Duplicate
     (List_Out : in out Type_Reference_List.List;
      List_In : in Type_Reference_List.List);
   --  Makes a deep copy of the list List_In into the list List_Out

   type Reference_In_File is record
      Line         : Natural;
      Column       : Natural;
      Entity       : Entity_Information;
   end record;
   --  Record used to save a reference in a file.
   --  Line   : line of the reference in this file.
   --  Column : column of the reference in this file.
   --  Entity : pointer on the declaration of the reference.

   procedure Free (X : in out Reference_In_File);
   --  Free the information associated with X.

   package List_Reference_In_File is
     new Generic_List (Reference_In_File);
   --  List used to record the references of a file.

   function Compare_Elements_By_Line_And_Column
     (X, Y : Reference_In_File) return Boolean;

   procedure Sort_List_By_Line_And_Column is
     new List_Utils.Sort (List_Reference_In_File,
               "<" => Compare_Elements_By_Line_And_Column);
   --  Sort the list by line and column.

   procedure Free (X : in out Entity_Information);

   package List_Entity_In_File is
     new Generic_List (Entity_Information);
   --  List used to record the declaration of references of a file.

   type Entity_Type is
     (Entry_Entity,
      Exception_Entity,
      Package_Entity,
      Subprogram_Entity,
      Type_Entity,
      Var_Entity,
      Other_Entity);
   --  A simplified list of possible entity types

   type Entity_List_Information is record
      Kind                : Entity_Type;
      Entity              : Entity_Information;
      Is_Private          : Boolean;
      Line_In_Body        : File_Location;
      Public_Declaration  : Entity_Information := null;
   end record;
   --  Description of an entity.
   --  Kind   : Simplified type of the entity.
   --  Entity : Pointer on the current entity.
   --  Public_Declaration : when a public type has at least one private field,
   --  we need 2 Entity_List_Information: one for the public type itself and
   --  one in order to generate doc for the private part. This last element
   --  need to have a "pointer" on the public declaration. For all other
   --  entities (subprograms, exceptions, types without private fields ...),
   --  the field Public_Declaration has the value null.
   --  Line_In_Body : for types with private fields, it refers to the public
   --  declaration.

   type Entity_List_Information_Handle is access Entity_List_Information;

   function Clone
     (Entity     : Entity_List_Information) return Entity_List_Information;
   --  Return a deep-copy of Entity.
   --  Entity can be freed without impacting the copy

   procedure Free (X : in out Entity_List_Information);
   --  Free the memory associated with X.

   package Type_Entity_List is new Generic_List (Entity_List_Information);

   function Compare_Elements_Name
     (X, Y : Entity_List_Information) return Boolean;
   function Compare_Elements_Line
     (X, Y : Entity_List_Information) return Boolean;
   function Compare_Elements_Column
     (X, Y : Entity_List_Information) return Boolean;

   procedure Sort_List_Line   is
     new List_Utils.Sort (Type_Entity_List, "<" => Compare_Elements_Line);
   --  Sort list by line.

   procedure Sort_List_Column is
     new List_Utils.Sort (Type_Entity_List, "<" => Compare_Elements_Column);
   --  Sort list by column.

   procedure Sort_List_Name   is
     new List_Utils.Sort (Type_Entity_List, "<" => Compare_Elements_Name);
   --  Sort the entities in alphabetical order by name,
   --  BUT all public entites stand in front of the private.

   package List_Entity_Information is new Generic_List (Entity_Information);

   procedure Sort_List_Name   is
     new List_Utils.Sort (List_Entity_Information, "<" => Entities."<");

   type Type_Api_Doc is (HTML, TEXI);
   --  Type of documentation that can be generated.
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
      Tagged_Types : Boolean := False;
      --  Create a list with all tagged types declared in the list of
      --  files we are processing. For each tagged types we indicate
      --  his parent and his children (if they exist)
   end record;

   type Family_Type is
     (Main, No_Parent, No_Child,
      Parent_With_Link, Parent_Without_Link,
      Child_With_Link, Child_Without_Link);
   --  Used when tagged types are listed. It's a field of the record Doc_Info
   --  (see below) in the case Info_Type=Index_Tagged_Type_Item. It indicates
   --  which item (the tagged type itself, its parent, one of its child)
   --  is put in the index and also if we can link this item to its
   --  declaration

   package Docgen_Backend is

      type Backend is abstract tagged private;
      type Backend_Handle is access all Backend'Class;

      procedure Initialize (B : access Backend; Text : String) is abstract;
      --  Initialize the private fields before starting the documentation
      --  process.

      procedure Doc_Open
        (B          : access Backend;
         Kernel     : access Glide_Kernel.Kernel_Handle_Record'Class;
         File       : File_Descriptor;
         Open_Title : String) is abstract;
      --  Called each time a new file is created

      procedure Doc_Close
        (B      : access Backend;
         Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
         File   : File_Descriptor) is abstract;
      --  Called each time a file is closed

      procedure Doc_Header
        (B              : access Backend;
         Kernel         : access Glide_Kernel.Kernel_Handle_Record'Class;
         File           : File_Descriptor;
         Header_File    : VFS.Virtual_File;
         Header_Package : String;
         Header_Line    : Natural;
         Header_Link    : Boolean) is abstract;

      procedure Doc_Header_Private
        (B                : access Backend;
         Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
         File             : File_Descriptor;
         Header_Title     : String;
         Level            : Natural) is abstract;

      procedure Doc_Footer
        (B                : access Backend;
         Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
         File             : File_Descriptor) is abstract;
      --  Called when we finished processing an entity.

      procedure Doc_Subtitle
        (B                : access Backend;
         Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
         File             : File_Descriptor;
         Level            : Natural;
         Subtitle_Name    : String) is abstract;
      --  Add a subtitle for the entity type to the documentation

      procedure Doc_With
        (B                : access Backend;
         Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
         File             : File_Descriptor;
         List_Ref_In_File : in out List_Reference_In_File.List;
         Source_File_List : Type_Source_File_Table.HTable;
         Options          : All_Options;
         Level            : Natural;
         With_Header      : String;
         With_File        : VFS.Virtual_File;
         With_Header_Line : Natural) is abstract;
      --  With clauses for imported packages

      procedure Doc_Package
        (B                   : access Backend;
         Kernel              : access Glide_Kernel.Kernel_Handle_Record'Class;
         File                : in File_Descriptor;
         List_Ref_In_File    : in out List_Reference_In_File.List;
         Source_File_List    : Type_Source_File_Table.HTable;
         Options             : All_Options;
         Level               : Natural;
         Package_Entity      : Entity_Information;
         Package_Header      : String) is abstract;
      --  Used to add a package info to the information file

      procedure Doc_Package_Open_Close
        (B                 : access Backend;
         Kernel            : access Glide_Kernel.Kernel_Handle_Record'Class;
         File              : in File_Descriptor;
         List_Ref_In_File  : in out List_Reference_In_File.List;
         Source_File_List  : Type_Source_File_Table.HTable;
         Options           : All_Options;
         Level             : Natural;
         Entity            : Entity_Information;
         Header            : String) is abstract;
      --  Used to add a header or a gooter for an inner package

      procedure Doc_Package_Desc
        (B           : access Backend;
         Kernel      : access Glide_Kernel.Kernel_Handle_Record'Class;
         File        : File_Descriptor;
         Level       : Natural;
         Description : String) is abstract;
      --  Used to add a package description

      procedure Doc_Var
        (B                : access Backend;
         Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
         File             : in File_Descriptor;
         List_Ref_In_File : in out List_Reference_In_File.List;
         Source_File_List : Type_Source_File_Table.HTable;
         Options          : All_Options;
         Level            : Natural;
         Entity           : Entity_Information;
         Header           : String) is abstract;
      --  Used to add a constant or a named number

      procedure Doc_Exception
        (B                : access Backend;
         Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
         File             : File_Descriptor;
         List_Ref_In_File : in out List_Reference_In_File.List;
         Source_File_List : Type_Source_File_Table.HTable;
         Options          : All_Options;
         Level            : Natural;
         Entity           : Entity_Information;
         Header           : String) is abstract;
      --  Used to add an exception info to the information file

      procedure Doc_Type
        (B                : access Backend;
         Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
         File             : File_Descriptor;
         List_Ref_In_File : in out List_Reference_In_File.List;
         Source_File_List : Type_Source_File_Table.HTable;
         Options          : All_Options;
         Level            : Natural;
         Entity           : Entity_Information;
         Header           : String) is abstract;
      --  Used to add type information

      procedure Doc_Entry
        (B                : access Backend;
         Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
         File             : File_Descriptor;
         List_Ref_In_File : in out List_Reference_In_File.List;
         Source_File_List : Type_Source_File_Table.HTable;
         Options          : All_Options;
         Level            : Natural;
         Entity           : Entity_Information;
         Header           : String) is abstract;

      procedure Doc_Subprogram
        (B                : access Backend;
         Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
         File             : File_Descriptor;
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
         File              : File_Descriptor;
         Options           : All_Options;
         Level             : Natural;
         Callers           : Entities.Entity_Information_Arrays.Instance;
         Processed_Sources : Type_Source_File_Table.HTable) is abstract;
      --  Add the list of callers for a given entity

      procedure Doc_Calls_References
        (B                 : access Backend;
         Kernel            : access Kernel_Handle_Record'Class;
         File              : File_Descriptor;
         Options           : All_Options;
         Level             : Natural;
         Calls             : Entities.Entity_Information_Arrays.Instance;
         Processed_Sources : Type_Source_File_Table.HTable) is abstract;
      --  Add the list of entities called by the current entity

      procedure Doc_Tagged_Type
        (B                : access Backend;
         Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
         File             : File_Descriptor;
         Source_File_List : Type_Source_File_Table.HTable;
         Level            : Natural;
         Entity           : Entity_Information) is abstract;
      --  Add the list of parent and children for a tagged type

      procedure Doc_Unit_Index
        (B                : access Backend;
         Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
         File             : File_Descriptor;
         Source_File_List : Type_Source_File_Table.HTable;
         Options          : All_Options;
         Level            : Natural;
         Doc_Directory    : String) is abstract;
      --  Start the output the Unit index file

      procedure Doc_Subprogram_Index
        (B       : access Backend;
         Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class;
         File    : File_Descriptor;
         Options : All_Options) is abstract;
      --  Start the output of the subprogram index file

      procedure Doc_Type_Index
        (B                : access Backend;
         Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
         File             : File_Descriptor;
         Options          : All_Options) is abstract;
      --  Start the output of the type index file

      procedure Doc_Tagged_Type_Index
        (B      : access Backend;
         Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
         File   : File_Descriptor) is abstract;
      --  Start the output of the index for tagged types

      procedure Doc_Private_Index
        (B                : access Backend;
         Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
         File             : File_Descriptor;
         Title            : String) is abstract;
      --  Start a new section in the current index for private entities

      procedure Doc_Public_Index
        (B                : access Backend;
         Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
         File             : File_Descriptor;
         Title            : String) is abstract;
      --  Start a new section in the current index for public entities

      procedure Doc_End_Of_Index
        (B                : access Backend;
         Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
         File             : File_Descriptor) is abstract;
      --  Terminate the current index file

      procedure Doc_Index_Tagged_Type
        (B                : access Backend;
         Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
         File             : File_Descriptor;
         Source_File_List : Type_Source_File_Table.HTable;
         Entity           : Entity_Information;
         Family           : Family_Type) is abstract;
      --  Add items to the tagged types index file

      procedure Doc_Index_Item
        (B                : access Backend;
         Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
         File             : File_Descriptor;
         Name             : String;
         Item_File        : Entities.Source_File;
         Line             : Natural;
         Doc_File         : String) is abstract;
      --  Add items to one of the index files (units, types and subprograms)

      procedure Doc_Body_Line
        (B                : access Backend;
         Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
         File             : File_Descriptor;
         List_Ref_In_File : in out List_Reference_In_File.List;
         Source_File_List : Type_Source_File_Table.HTable;
         Options          : All_Options;
         Level            : Natural;
         Body_File        : VFS.Virtual_File;
         Body_Text        : String) is abstract;
      --  Process one line of the body file

      procedure Doc_Description
        (B                : access Backend;
         Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
         File             : File_Descriptor;
         Level            : Natural;
         Description      : String) is abstract;
      --  Pass comments written after or before the current entity

      procedure Format_Comment
        (B           : access Backend;
         File        : File_Descriptor;
         Text        : String;
         Start_Index : Natural;
         Start_line  : Natural;
         End_Index   : Natural;
         End_Line    : Natural;
         Entity_Line : Natural) is abstract;
      --  Format text as a comment

      procedure Format_Keyword
        (B           : access Backend;
         File        : File_Descriptor;
         Text        : String;
         Start_Index : Natural;
         Start_line  : Natural;
         End_Index   : Natural;
         End_Line    : Natural;
         Entity_Line : Natural) is abstract;
      --  Format text as a keyword

      procedure Format_String
        (B           : access Backend;
         File        : File_Descriptor;
         Text        : String;
         Start_Index : Natural;
         Start_line  : Natural;
         End_Index   : Natural;
         End_Line    : Natural;
         Entity_Line : Natural) is abstract;
      --  Format text as a string (between two ")

      procedure Format_Character
        (B           : access Backend;
         File        : File_Descriptor;
         Text        : String;
         Start_Index : Natural;
         Start_line  : Natural;
         End_Index   : Natural;
         End_Line    : Natural;
         Entity_Line : Natural) is abstract;
      --  Format text as a character (between two ')

      procedure Format_Identifier
        (B                   : access Backend;
         List_Ref_In_File    : in out List_Reference_In_File.List;
         Start_Index         : Natural;
         Start_Line          : Natural;
         Start_Column        : Natural;
         End_Index           : Natural;
         End_Line            : Natural;
         Kernel              : access Kernel_Handle_Record'Class;
         File                : File_Descriptor;
         Text                : String;
         File_Name           : VFS.Virtual_File;
         Entity_Line         : Natural;
         Line_In_Body        : Natural;
         Source_File_List    : Type_Source_File_Table.HTable;
         Link_All            : Boolean;
         Is_Body             : Boolean;
         Process_Body        : Boolean;
         Level               : Natural;
         Indent              : Natural) is abstract;
      --  Format text as an identifier.
      --  Level : number of indentation levels.
      --  Indent: value of one indentation level.
      --  Those 2 parameters are used to associate identifiers returned by
      --  parse_entities in format_file (see docgen.adb) with a reference
      --  contained in the list made in Process_One_File
      --  (see docgen-work_on_File.adb).

      procedure Format_Code
        (B                : access Backend'Class;
         Kernel           : access Kernel_Handle_Record'Class;
         File             : File_Descriptor;
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
      --  Process_Body : indicate if the option "process body files" is chosen
      --  in the preferences.
      --  Link_All     : indicate if the option "create all links" is chosen
      --  in the preferences.
      --  Level, indent: same comments as those made for Format_Identifier.

      procedure Format_Link
        (B                : access Backend;
         Start_Index      : Natural;
         Start_Line       : Natural;
         Start_Column     : Natural;
         End_Index        : Natural;
         Kernel           : access Kernel_Handle_Record'Class;
         File             : File_Descriptor;
         Text             : String;
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
         File        : File_Descriptor;
         Text        : String;
         Entity_Line : Natural) is abstract;
      --  Terminate processing of a block of code which has been analysed
      --  by Parse_Entities + Callback + Format_xxx

      function Get_Extension
        (B : access Backend) return String is abstract;
      --  Return the extension of doc files (eg. ".htm" for an instance of
      --  object Backend_HTML).

      function Get_Doc_Directory
        (B      : access Backend;
         Kernel : access Kernel_Handle_Record'Class) return String is abstract;
      --  Return the path which must contain the documentation (eg.
      --  "/..../gps/glide/obj/html/" for an instance of object
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
   private

      type Backend is abstract tagged record
         Indent     : Natural := 3;
         --  Number of space which correspond to one step of indentation
         --  ??? Use Get_Pref instead
         Last_Index : Natural;
         Last_Line  : Natural;
      end record;

   end Docgen_Backend;

   procedure Format_All_Link
     (B                   : access Docgen_Backend.Backend'Class;
      List_Ref_In_File    : in out List_Reference_In_File.List;
      Start_Index         : Natural;
      Start_Line          : Natural;
      Start_Column        : Natural;
      End_Index           : Natural;
      Kernel              : access Kernel_Handle_Record'Class;
      File                : File_Descriptor;
      Text                : String;
      File_Name           : VFS.Virtual_File;
      Entity_Line         : Natural;
      Line_In_Body        : in out Natural;
      Source_File_List    : Type_Source_File_Table.HTable;
      Link_All            : Boolean;
      Is_Body             : Boolean;
      Process_Body        : Boolean;
      Level               : Natural;
      Indent              : Natural);
   --  This procedure is used by formats of documentation like html to
   --  create links for each entity of the file File_Name on their
   --  own declaration. It's called by the method Format_Identifier of
   --  a child instance of a Backend object (eg. Backend_HTML).
   --  This process is done in Docgen because for each entity we must search
   --  for its declaration in all concerned files: this work is
   --  independant of the choosen format of documentation.
   --  Level, indent: same comments as those made for Format_Identifier.

   function Count_Lines (Line : String) return Natural;
   --  Return the number of lines in the String

   function Count_Points (Text : String) return Natural;
   --  Return the number of point in the given string

   function Get_Doc_File_Name
     (Source_Filename : VFS.Virtual_File;
      Doc_Suffix      : String) return String;
   --  Return a string with the base name for the new doc file:

   function Source_File_In_List
     (Source_File_List : Type_Source_File_Table.HTable;
      File             : Entities.Source_File) return Boolean;
   --  Return true if the file is found in the source file list

   function Is_Spec_File
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File   : VFS.Virtual_File) return Boolean;
   --  Return whether the File is a Spec file

end Docgen;
