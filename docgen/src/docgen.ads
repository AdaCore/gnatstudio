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
--  Docgen packages. There are the two types needed for the lists,
--  the type All_Options, the type Doc_Info, which will be used to destinguish
--  which entity type should be processed by the output procedure.

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
     new Sort (Type_Reference_List, "<" => Compare_Elements_Column);
   --  Sort the list by column

   procedure Sort_List_Name is
     new Sort (Type_Reference_List, "<" => Compare_Elements_Name);
   --  Sort the list by name

   procedure Duplicate
     (List_Out : in out Type_Reference_List.List;
      List_In : in Type_Reference_List.List);
   --  Makes a deep copy of the list List_In into the list List_Out

   type Entity_Handle is access Src_Info.Queries.Entity_Information;

   type Reference_In_File is record
      Name         : GNAT.OS_Lib.String_Access;
      Line         : Natural;
      Column       : Natural;
      Entity       : Entity_Handle;
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
     new Sort (List_Reference_In_File,
               "<" => Compare_Elements_By_Line_And_Column);
   --  Sort the list by line and column.

   procedure Free (X : in out Src_Info.Queries.Entity_Information);

   package List_Entity_In_File is
     new Generic_List (Src_Info.Queries.Entity_Information);
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
      Name                : GNAT.OS_Lib.String_Access;
      Entity              : Src_Info.Queries.Entity_Information;
      Is_Private          : Boolean;
      --  The following items won't be used in the index lists
      Line_In_Body        : Src_Info.File_Location;
      Calls_List          : Type_Reference_List.List;
      Called_List         : Type_Reference_List.List;
      Public_Declaration  : Src_Info.Queries.Entity_Information
        := No_Entity_Information;
   end record;
   --  Description of an entity.
   --  Kind   : Simplified type of the entity.
   --  Name   : Name of the entity.
   --  Entity : Pointer on the current entity.
   --  Calls_List, Called_List : only used for subprograms. Calls_List
   --  contains the list of the subprograms called by the current entity.
   --  Called_List contains the list of the subprograms which call current
   --  entity.
   --  Public_Declaration : when a public type has at least one private field,
   --  we need 2 Entity_List_Information: one for the public type itself and
   --  one in order to generate doc for the private part. This last element
   --  need to have a "pointer" on the public declaration. For all other
   --  entities (subprograms, exceptions, types without private fields ...),
   --  the field Public_Declaration has the value No_Entity_Information.
   --  Line_In_Body : for types with private fields, it refers to the public
   --  declaration.

   type Entity_List_Information_Handle is access Entity_List_Information;

   function Clone
     (Entity     : Entity_List_Information;
      Copy_Lists : Boolean) return Entity_List_Information;
   --  Return a deep-copy of Entity.
   --  Entity can be freed without impacting the copy
   --  If Copy_List is true, it copies also the lists used for the call graph
   --  Calls_List and Called_List.

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
     new Sort (Type_Entity_List, "<" => Compare_Elements_Line);
   --  Sort list by line.

   procedure Sort_List_Column is
     new Sort (Type_Entity_List, "<" => Compare_Elements_Column);
   --  Sort list by column.

   procedure Sort_List_Name   is
     new Sort (Type_Entity_List, "<" => Compare_Elements_Name);
   --  Sort the entities in alphabetical order by name,
   --  BUT all public entites stand in front of the private.

   procedure Free (X : in out Entity_Handle);

   package List_Entity_Handle is
     new Generic_List (Entity_Handle);

   function Compare_Name
     (X, Y : Entity_Handle) return Boolean;

   procedure Sort_List_Name   is
     new Sort (List_Entity_Handle, "<" => Compare_Name);

   type Tagged_Element is record
      Me                   : Entity_Handle;
      My_Parents           : List_Entity_Handle.List;
      My_Children          : List_Entity_Handle.List;
      Number_Of_Parents    : Natural;
      Number_Of_Children   : Natural;
      Print_Me             : Boolean;
   end record;
   --  Represent a tagged type.
   --  Me         : Pointer on the tagged type itself.
   --  My_Parents : list of pointers on the entities which are parents of Me.
   --  My_Children: list of pointers on the entities which are children of Me.
   --  Print_Me   : indicates that the tagged type is defined in the
   --  processed files, it must be printed in the specific index file.

   type Tagged_Element_Handle is access Tagged_Element;

   procedure Free (X : in out Tagged_Element);

   package Type_List_Tagged_Element is new Generic_List (Tagged_Element);
   --  Contains tagged types which are declared in the list of files we are
   --  processing

   function Compare_Tagged_Name
     (X, Y : Tagged_Element) return Boolean;

   procedure Sort_List_Name   is
     new Sort (Type_List_Tagged_Element, "<" => Compare_Tagged_Name);
   --  Sort the tagged types in alphabetical order by name,

   function Find_In_List
     (List : List_Entity_Handle.List;
      Info : Entity_Information) return Entity_Handle;
   --  Search if Info is in List. Return a pointer on Info in List if success.

   procedure Add_Child
     (List   : in out Type_List_Tagged_Element.List;
      Target : Entity_Handle;
      Patch  : Entity_Handle);
   --  For a current tagged type: update of the list of children with the
   --  local child in the current file.
   --  Target : tagged type which is updated.
   --  Patch  : child entity added the list My_Children of the Target.

   procedure Add_Parent
     (List   : in out Type_List_Tagged_Element.List;
      Target : Entity_Handle;
      Patch  : Entity_Handle);
   --  For a current tagged type: update of the list of parents with the
   --  local parent in the current file.
   --  Target : tagged type which is updated.
   --  Patch  : parent entity added the list My_Parents of the Target.

   procedure Must_Print_Tagged_Type
     (List : in out Type_List_Tagged_Element.List;
      Target : in Entity_Handle);
   --  Updates the field Print_Me if necessary.
   --  Target : tagged type which is updated.

   type Info_Types is
     (Open_Info,
      Close_Info,
      Header_Info,
      Subtitle_Info,
      Header_Private_Info,
      With_Info,
      Package_Desc_Info,
      Description_Info,
      Entry_Info,
      Exception_Info,
      Package_Info,
      Subprogram_Info,
      Type_Info,
      Var_Info,
      Package_Info_Open_Close,
      References_Info,
      Tagged_Type_Info,
      Body_Line_Info,

      Unit_Index_Info,
      Type_Index_Info,
      Tagged_Type_Index_Info,
      Subprogram_Index_Info,
      --  The 4 fiels above are used to create the header of the index
      --  file (units, types, subprograms or tagged types) and also to build
      --  links (if allowed by the format) on the other index files
      Private_Index_Info,
      --  Used to print "Private" in the index file before all the
      --  private types/subprograms
      Public_Index_Info,
      --  Used to print "Public" in the index file before all the
      --  private types/subprograms
      End_Of_Index_Info,
      --  Used to close the index file ( of units, types, subprograms or
      --  tagged types)
      Index_Tagged_Type_Item,
      --  Used to print a tagged type in the specific index file
      Index_Item_Info,
      --  Used to print a type/unit/subprogram in the specific index file
      Footer_Info);
   --  Structure used in the type Doc_Info.
   --  Open_Info :
   --  used possibly to create a header for the doc file (E.g. in html,
   --  <head>...</head>).
   --  Close_Info :
   --  used to close the output of the doc file (E.g. in html </body></html>).
   --  Header_Info   : used to put a title the doc file.
   --  Subtitle_Info : used to put a subtitle the doc file.
   --  Header_Private_Info :
   --  used to put the subtitle "Private" into the doc file before private
   --  part when the option "Show private" is chosen.
   --  With_Info :
   --  used to process the output of imported packages (clauses "with ...")
   --  Package_Desc_Info :
   --  used to print the description of the current file which is written
   --  before the source code.
   --  Description_Info :
   --  used to print the comments given for an entity (type, subprogram,
   --  exception...).
   --  Entry_Info, Exception_Info, Package_Info, Subprogram_Info, Type_Info,
   --  Var_Info :
   --  used to print source code of entity (exception, subprogram, variable,
   --  type, entry, package).
   --  Package_Info_Open_Close :
   --  used to print either the header or the footer when processing an inner
   --  package. "package X is" or "end X".
   --  References_Info :
   --  used to print the subprogram callgraph of the current subprogram
   --  entity.
   --  Tagged_Type_Info :
   --  used to print the list of children and parents of the current type
   --  entity.
   --  Body_Line_Info : used to print body files.

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

   type Doc_Info_Base is tagged record
      Doc_Info_Options : All_Options;
      Doc_LI_Unit      : LI_File_Ptr;
      Doc_File_List    : Type_Source_File_List.List;
   end record;

   type Doc_Info_Open is new Doc_Info_Base with record
      Open_Title        : GNAT.OS_Lib.String_Access;
      Open_File         : VFS.Virtual_File;
      Open_Package_Next : GNAT.OS_Lib.String_Access;
      Open_Package_Prev : GNAT.OS_Lib.String_Access;
   end record;
   --  Used at the very beginning of the file

   type Doc_Info_Close is new Doc_Info_Base with record
      Close_File_Name : VFS.Virtual_File;
   end record;
   --  Used at the end of the file

   type Doc_Info_Header is new Doc_Info_Base with record
      Header_Package : GNAT.OS_Lib.String_Access;
      Header_File    : VFS.Virtual_File;
      Header_Link    : Boolean;
      Header_Line    : Natural;
   end record;
   --  Used to start an entity information

   type Doc_Info_Header_Private is new Doc_Info_Base with record
      Header_Title : GNAT.OS_Lib.String_Access;
   end record;
   --  Used to start the private part of the file

   type Doc_Info_Footer is new Doc_Info_Base with record
      Footer_Title : GNAT.OS_Lib.String_Access;
      Footer_File  : VFS.Virtual_File;
   end record;
   --  Used to finish an entity information

   type Doc_Info_Subtitle is new Doc_Info_Base with record
      Subtitle_Name    : GNAT.OS_Lib.String_Access;
      Subtitle_Package : GNAT.OS_Lib.String_Access;
      Subtitle_Kind    : Info_Types;
   end record;
   --  Used to add a subtitle to the information file

   type Doc_Info_With is new Doc_Info_Base with record
      With_Header      : GNAT.OS_Lib.String_Access;
      With_File        : VFS.Virtual_File;
      With_Header_Line : Natural;
   end record;
   --  With clauses for imported packages

   type Doc_Info_Package is new Doc_Info_Base with record
      Package_Entity      : Entity_List_Information;
      Package_Header      : GNAT.OS_Lib.String_Access;
      Package_Header_Line : Natural;
   end record;
   --  Used to add a package info to the information file

   type Doc_Info_Package_Open_Close is new Doc_Info_Base with record
      Package_Open_Close_Entity      : Entity_List_Information;
      Package_Open_Close_Header      : GNAT.OS_Lib.String_Access;
      Package_Open_Close_Header_Line : Natural;
   end record;
   --  Used to add a header or a footer of an inner package

   type Doc_Info_Package_Desc is new Doc_Info_Base with record
      Package_Desc_Description : GNAT.OS_Lib.String_Access;
   end record;
   --  Used to add the package description

   type Doc_Info_Var is new Doc_Info_Base with record
      Var_Entity      : Entity_List_Information;
      Var_Header      : GNAT.OS_Lib.String_Access;
      Var_Header_Line : Natural;
   end record;
   --  Used to add a constant and named numbers

   type Doc_Info_Exception is new Doc_Info_Base with record
      Exception_Entity      : Entity_List_Information;
      Exception_Header      : GNAT.OS_Lib.String_Access;
      Exception_Header_Line : Natural;
   end record;
   --  Used to add an exception info to the information file

   type Doc_Info_Type is new Doc_Info_Base with record
      Type_Entity      : Entity_List_Information;
      Type_Header      : GNAT.OS_Lib.String_Access;
      Type_Header_Line : Natural;
   end record;
   --  Used to add a type info to the information file

   type Doc_Info_Entry is new Doc_Info_Base with record
      Entry_Entity      : Entity_List_Information;
      Entry_Header      : GNAT.OS_Lib.String_Access;
      Entry_Header_Line : Natural;
      Entry_Link        : Boolean;
   end record;
   --  Used to add an entry info to the information file

   type Doc_Info_Subprogram is new Doc_Info_Base with record
      Subprogram_Entity      : Entity_List_Information;
      Subprogram_Header      : GNAT.OS_Lib.String_Access;
      Subprogram_Header_Line : Natural;
      Subprogram_Link        : Boolean;
      Subprogram_List        : Type_Entity_List.List;
   end record;
   --  Used to add a subprogram info to the information file

   type Doc_Info_References is new Doc_Info_Base with record
      References_Entity           : Entity_List_Information;
      References_Source_File_List : Type_Source_File_List.List;
      References_Directory        : GNAT.OS_Lib.String_Access;
      References_Suffix           : GNAT.OS_Lib.String_Access;
   end record;
   --  Used to add the callgraph of a subprogram

   type Doc_Info_Tagged_Type is new Doc_Info_Base with record
      Tagged_Entity           : Tagged_Element;
      Tagged_Source_File_List : Type_Source_File_List.List;
      Tagged_Directory        : GNAT.OS_Lib.String_Access;
      Tagged_Suffix           : GNAT.OS_Lib.String_Access;
   end record;
   --  Used to list parents and children of a tagged type

   type Doc_Info_Unit_Index is new Doc_Info_Base with record
      Unit_File_List       : Type_Source_File_List.List;
      --  The name doc file name without the suffix
      Unit_Index_File_Name : GNAT.OS_Lib.String_Access;
      Unit_Project_Name    : Projects.Project_Type;
   end record;
   --  Used to start the package index file

   type Doc_Info_Subprogram_Index is new Doc_Info_Base with record
      Subprogram_Index_File_Name : GNAT.OS_Lib.String_Access;
      --  The doc file name without the suffix
   end record;
   --  Used to start the subprogram index file

   type Doc_Info_Type_Index is new Doc_Info_Base with record
      Type_Index_File_Name : GNAT.OS_Lib.String_Access;
      --  The name doc file name without the suffix
   end record;
   --  Used to start the type index file

   type Doc_Info_Tagged_Type_Index is new Doc_Info_Base with record
      Tagged_Type_Index_File_Name   : GNAT.OS_Lib.String_Access;
   end record;
   --  The doc file name without the suffix

   type Doc_Info_Private_Index is new Doc_Info_Base with record
      Private_Index_Title : GNAT.OS_Lib.String_Access;
   end record;
   --  Used to start private types/subprograms in the index frame

   type Doc_Info_Public_Index is new Doc_Info_Base with record
      Public_Index_Title : GNAT.OS_Lib.String_Access;
   end record;
   --  Used to start public types/subprograms in the index frame

   type Doc_Info_End_Of_Index is new Doc_Info_Base with record
      End_Index_Title : GNAT.OS_Lib.String_Access;
   end record;
   --  Used to finish all 4 kinds of index files

   type Doc_Info_Index_Tagged_Type is new Doc_Info_Base with record
      Doc_Tagged_Type : Entity_Information;
      Doc_Family      : Family_Type;
      Directory       : GNAT.OS_Lib.String_Access;
      Suffix          : GNAT.OS_Lib.String_Access;
   end record;
   --  Used to add items to the tagged types index files

   type Doc_Info_Index_Item is new Doc_Info_Base with record
      Item_Name     : GNAT.OS_Lib.String_Access;
      Item_File     : VFS.Virtual_File;
      Item_Line     : Natural;
      Item_Doc_File : GNAT.OS_Lib.String_Access;
   end record;
   --  Used to add items to 3 kinds of index files (units, types
   --  and subprograms index)

   type Doc_Info_Body_Line is new Doc_Info_Base with record
      Body_File : VFS.Virtual_File;
      Body_Text : GNAT.OS_Lib.String_Access;
   end record;
   --  Used to pass the information of one line in the body file

   type Doc_Info_Description is new Doc_Info_Base with record
      Description : GNAT.OS_Lib.String_Access;
   end record;
   --  Used to pass the comments written after or before of the
   --  source code

   package Docgen_Backend is

      type Backend is abstract tagged private;
      type Backend_Handle is access all Backend'Class;

      procedure Initialize (B : access Backend; Text : String) is abstract;
      --  Initialize the private fields before starting the documentation
      --  process.

      procedure Doc_Open
        (B      : access Backend;
         Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
         File   : File_Descriptor;
         Info   : in out Docgen.Doc_Info_Open) is abstract;
      --  Called each time a new file is created

      procedure Doc_Close
        (B      : access Backend;
         Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
         File   : File_Descriptor;
         Info   : in out Docgen.Doc_Info_Close) is abstract;
      --  Called each time a file is closed

      procedure Doc_Header
        (B                : access Backend;
         Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
         File             : File_Descriptor;
         Info             : in out Docgen.Doc_Info_Header) is abstract;

      procedure Doc_Header_Private
        (B                : access Backend;
         Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
         File             : File_Descriptor;
         Info             : in out Docgen.Doc_Info_Header_Private;
         Level            : Natural) is abstract;

      procedure Doc_Footer
        (B                : access Backend;
         Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
         File             : File_Descriptor;
         Info             : in out Docgen.Doc_Info_Footer) is abstract;

      procedure Doc_Subtitle
        (B      : access Backend;
         Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
         File   : File_Descriptor;
         Info   : in out Docgen.Doc_Info_Subtitle;
         Level  : Natural) is abstract;
      --  Add a subtitle for the entity type to the documentation

      procedure Doc_With
        (B                : access Backend;
         Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
         File             : File_Descriptor;
         List_Ref_In_File : in out List_Reference_In_File.List;
         Info             : in out Docgen.Doc_Info_With;
         Level            : Natural) is abstract;

      procedure Doc_Package
        (B                : access Backend;
         Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
         File             : File_Descriptor;
         List_Ref_In_File : in out List_Reference_In_File.List;
         Info             : in out Docgen.Doc_Info_Package;
         Level            : Natural) is abstract;

      procedure Doc_Package_Open_Close
        (B                : access Backend;
         Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
         File             : File_Descriptor;
         List_Ref_In_File : in out List_Reference_In_File.List;
         Info             : in out Docgen.Doc_Info_Package_Open_Close;
         Level            : Natural) is abstract;

      procedure Doc_Package_Desc
        (B      : access Backend;
         Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
         File   : File_Descriptor;
         Info   : in out Docgen.Doc_Info_Package_Desc;
         Level  : Natural) is abstract;

      procedure Doc_Var
        (B                : access Backend;
         Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
         File             : File_Descriptor;
         List_Ref_In_File : in out List_Reference_In_File.List;
         Info             : in out Docgen.Doc_Info_Var;
         Level            : Natural) is abstract;

      procedure Doc_Exception
        (B                : access Backend;
         Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
         File             : File_Descriptor;
         List_Ref_In_File : in out List_Reference_In_File.List;
         Info             : in out Docgen.Doc_Info_Exception;
         Level            : Natural) is abstract;

      procedure Doc_Type
        (B                : access Backend;
         Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
         File             : File_Descriptor;
         List_Ref_In_File : in out List_Reference_In_File.List;
         Info             : in out Docgen.Doc_Info_Type;
         Level            : Natural) is abstract;

      procedure Doc_Entry
        (B                : access Backend;
         Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
         File             : File_Descriptor;
         List_Ref_In_File : in out List_Reference_In_File.List;
         Info             : in out Docgen.Doc_Info_Entry;
         Level            : Natural) is abstract;

      procedure Doc_Subprogram
        (B                : access Backend;
         Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
         File             : File_Descriptor;
         List_Ref_In_File : in out List_Reference_In_File.List;
         Info             : in out Docgen.Doc_Info_Subprogram;
         Level            : Natural) is abstract;

      procedure Doc_References
        (B                : access Backend;
         Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
         File             : File_Descriptor;
         Info             : in out Docgen.Doc_Info_References;
         Level            : Natural) is abstract;

      procedure Doc_Tagged_Type
        (B                : access Backend;
         Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
         File             : File_Descriptor;
         Info             : in out Docgen.Doc_Info_Tagged_Type;
         Level            : Natural) is abstract;

      procedure Doc_Unit_Index
        (B                : access Backend;
         Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
         File             : File_Descriptor;
         Info             : in out Docgen.Doc_Info_Unit_Index;
         Level            : Natural;
         Doc_Directory    : String;
         Doc_Suffix       : String) is abstract;

      procedure Doc_Subprogram_Index
        (B      : access Backend;
         Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
         File   : File_Descriptor;
         Info   : in out Docgen.Doc_Info_Subprogram_Index) is abstract;

      procedure Doc_Type_Index
        (B                : access Backend;
         Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
         File             : File_Descriptor;
         Info             : in out Docgen.Doc_Info_Type_Index) is abstract;

      procedure Doc_Tagged_Type_Index
        (B      : access Backend;
         Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
         File   : File_Descriptor;
         Info   : in out Docgen.Doc_Info_Tagged_Type_Index) is abstract;

      procedure Doc_Private_Index
        (B                : access Backend;
         Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
         File             : File_Descriptor;
         Info             : in out Docgen.Doc_Info_Private_Index) is abstract;

      procedure Doc_Public_Index
        (B                : access Backend;
         Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
         File             : File_Descriptor;
         Info             : in out Docgen.Doc_Info_Public_Index) is abstract;

      procedure Doc_End_Of_Index
        (B                : access Backend;
         Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
         File             : File_Descriptor;
         Info             : in out Docgen.Doc_Info_End_Of_Index) is abstract;

      procedure Doc_Index_Tagged_Type
        (B      : access Backend;
         Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
         File   : File_Descriptor;
         Info   : in out Docgen.Doc_Info_Index_Tagged_Type) is abstract;

      procedure Doc_Index_Item
        (B                : access Backend;
         Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
         File             : File_Descriptor;
         Info             : in out Docgen.Doc_Info_Index_Item) is abstract;

      procedure Doc_Body_Line
        (B                : access Backend;
         Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
         File             : File_Descriptor;
         List_Ref_In_File : in out List_Reference_In_File.List;
         Info             : in out Docgen.Doc_Info_Body_Line;
         Level            : Natural) is abstract;

      procedure Doc_Description
        (B                : access Backend;
         Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
         File             : File_Descriptor;
         Info             : in out Docgen.Doc_Info_Description;
         Level            : Natural) is abstract;

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
         LI_Unit             : LI_File_Ptr;
         Text                : String;
         File_Name           : VFS.Virtual_File;
         Entity_Line         : Natural;
         Line_In_Body        : Natural;
         Source_File_List    : Type_Source_File_List.List;
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

      procedure Format_File
        (B                : access Backend'Class;
         Kernel           : access Kernel_Handle_Record'Class;
         File             : File_Descriptor;
         List_Ref_In_File : in out List_Reference_In_File.List;
         Text             : String;
         File_Name        : VFS.Virtual_File;
         Entity_Line      : Natural;
         Line_In_Body     : Natural;
         Is_Body          : Boolean;
         Info             : Doc_Info_Base'Class;
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
         Kernel : Kernel_Handle) return String is abstract;
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
      LI_Unit             : LI_File_Ptr;
      Text                : String;
      File_Name           : VFS.Virtual_File;
      Entity_Line         : Natural;
      Line_In_Body        : in out Natural;
      Source_File_List    : Type_Source_File_List.List;
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
