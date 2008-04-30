-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2006                       --
--                             AdaCore                               --
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

with Basic_Types;
with GNAT.Strings;
with List_Utils;                use List_Utils;
with Entities;                  use Entities;
with GPS.Kernel;                use GPS.Kernel;
with GNATCOLL.VFS;
with Generic_List;
with HTables;

package Docgen is

   Max_Line_Length     : constant Natural := 160;
   First_File_Line     : constant Natural := 1;
   No_Body_Line_Needed : constant Natural := 0;

   type Docgen_Error_Reporter_Record is new Entities.File_Error_Reporter_Record
   with record
      Kernel : Kernel_Handle;
      --  The GPS Kernel.

      Called : Boolean := False;
      --  Set to true when Error is called.
   end record;
   procedure Error
     (Report : in out Docgen_Error_Reporter_Record;
      File   : Source_File);
   --  See inherited documentation.

   type Source_File_Information is record
      Unit_Name     : GNAT.Strings.String_Access;
      Doc_File_Name : GNAT.Strings.String_Access;
      --  The base name of the output file that contains the documentation for
      --  this source file.
      Is_Spec       : Boolean;
   end record;
   No_Source_File_Information : constant Source_File_Information :=
                                  (Unit_Name     => null,
                                   Doc_File_Name => null,
                                   Is_Spec       => False);
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
      Column       : Basic_Types.Visible_Column_Type;
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
      Processed           : Boolean := False;
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
     (Entity : Entity_List_Information) return Entity_List_Information;
   --  Return a deep-copy of Entity.
   --  Entity can be freed without impacting the copy

   procedure Free (X : in out Entity_List_Information);
   --  Free the memory associated with X.

   package Type_Entity_List is new
     Generic_List (Entity_List_Information, Free);

   function Compare_Elements_Name
     (X, Y : Entity_List_Information) return Boolean;
   function Compare_Elements_Line
     (X, Y : Entity_List_Information) return Boolean;
   function Compare_Elements_Column
     (X, Y : Entity_List_Information) return Boolean;

   procedure Sort_List_Line is
     new List_Utils.Sort (Type_Entity_List, "<" => Compare_Elements_Line);
   --  Sort list by line.

   procedure Sort_List_Column is
     new List_Utils.Sort (Type_Entity_List, "<" => Compare_Elements_Column);
   --  Sort list by column.

   procedure Sort_List_Name is
     new List_Utils.Sort (Type_Entity_List, "<" => Compare_Elements_Name);
   --  Sort the entities in alphabetical order by name,
   --  BUT all public entites stand in front of the private.

   package List_Entity_Information is new Generic_List (Entity_Information);

   procedure Sort_List_Name is
     new List_Utils.Sort (List_Entity_Information, "<" => Entities."<");

   type All_Options is record
      Process_Body_Files : Boolean := False;
      --  Create also the body documentation
      Ignorable_Comments : Boolean := False;
      --  Ignore all comments with "--!"
      Show_Private       : Boolean := False;
      --  Show also private entities
      References         : Boolean := False;
      --  True if the program should search for the references
      --  Adding information like "subprogram called by..."
      Link_All           : Boolean := False;
      --  Should links be created to entities whose declaration files
      --  aren't being processed
      Tagged_Types       : Boolean := False;
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

   function Count_Lines (Line : String) return Natural;
   --  Return the number of lines in the String

   function Count_Points (Text : String) return Natural;
   --  Return the number of point in the given string

   function Get_Doc_File_Name
     (Source_Filename : GNATCOLL.VFS.Virtual_File;
      Doc_Suffix      : String) return String;
   --  Return a string with the base name for the new doc file:

   function Source_File_In_List
     (Source_File_List : Type_Source_File_Table.HTable;
      File             : Entities.Source_File) return Boolean;
   --  Return true if the file is found in the source file list

   function Is_Spec_File
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File) return Boolean;
   --  Return whether the File is a Spec file

end Docgen;
