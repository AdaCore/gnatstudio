-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2003                         --
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

with Ada.Calendar;
with GNAT.OS_Lib;
with HTables;
with Tries;
with VFS;
with Dynamic_Arrays;

--  This package contains the list of all files and entities used in the
--  current project.
--  Some notes about reference counting: this structure provides reference
--  counting for all the public structures. However, this reference counting is
--  reserved for users of these structures and not used internally.

package Entities is

   No_Time : constant Ada.Calendar.Time := Ada.Calendar.Time_Of
     (Ada.Calendar.Year_Number'First,
      Ada.Calendar.Month_Number'First,
      Ada.Calendar.Day_Number'First);

   -----------------------
   -- Entities_Database --
   -----------------------

   type Entities_Database is private;

   function Create return Entities_Database;
   --  Return a new empty database

   procedure Destroy (Db : in out Entities_Database);
   --  Free the memory occupied by Db

   ------------
   -- E_Kind --
   ------------

   type E_Kinds is
     (Overloaded_Entity,
      --  This special kind of entity is used for overloaded symbols that
      --  couldn't be resolved by the parser. See the comment at the beginning
      --  of the private part for a more complete explanation.

      Unresolved_Entity,
      --  This special kind indicates that we do not know the exact kind of
      --  entity. This can happen for instance in C, in the following case:
      --     typedef old_type new_type;
      --  but old_type is defined nowhere in the closure of the include files.

      Access_Kind,
      Array_Kind,
      Boolean_Kind,
      Class_Wide,
      Class,
      Decimal_Fixed_Point,
      Entry_Or_Entry_Family,
      Enumeration_Literal,
      Enumeration_Kind,
      Exception_Entity,
      Floating_Point,
      Label_On_Block,
      Label_On_Loop,
      Label_On_Statement,
      Modular_Integer,
      Named_Number,
      Function_Or_Operator,
      Package_Kind,
      Procedure_Kind,
      Ordinary_Fixed_Point,
      Private_Type,
      Protected_Kind,
      Record_Kind,
      Signed_Integer,
      String_Kind,
      Task_Kind);
   --  The entity kind (sorted by alphabetical order).
   --
   --  Note that Boolean is treated in a special way: it is treated as
   --  Boolean_Type/Object, rather than as an Enumeration_Type/Object.

   type E_Kind is record
      Kind        : E_Kinds;
      Is_Generic  : Boolean;
      Is_Type     : Boolean;
      Is_Abstract : Boolean;
   end record;
   --  Description for the type of an entity.
   --  Kind describes its general family.
   --  Is_Generic is set to true if this is a generic entity (or a template in
   --  the C++ case).
   --  Is_Type is true if this is a type, instead of an instance of a type.

   Unresolved_Entity_Kind : constant E_Kind :=
     (Unresolved_Entity, False, False, False);

   -------------
   -- LI_File --
   -------------

   type LI_File_Record is tagged private;
   type LI_File is access LI_File_Record'Class;

   function Get_LI_Filename (LI : LI_File) return VFS.Virtual_File;
   --  Return the name of the file

   procedure Unref (LI : in out LI_File);
   procedure Ref   (LI : LI_File);
   --  Change reference counting for the file. When it reaches 0, the memory
   --  is freed.

   function Get_Or_Create
     (Db        : Entities_Database;
      File      : VFS.Virtual_File;
      Timestamp : Ada.Calendar.Time := No_Time) return LI_File;
   --  Get (or create) a new entry for File in the database. If an entry
   --  already exists, it is returned, and the timestamp is updated if the
   --  parameter is not New_Name.
   --  You need to Ref the entry if you intend to keep it in a separate
   --  structure.

   -----------------
   -- Source_File --
   -----------------

   type Source_File_Record is tagged private;
   type Source_File is access Source_File_Record'Class;

   function Get_Filename (File : Source_File) return VFS.Virtual_File;
   --  Return the name of the file file

   procedure Unref (F : in out Source_File);
   procedure Ref   (F : Source_File);
   --  Change reference counting for the file. When it reaches 0, the memory
   --  is freed.

   function Get_Or_Create
     (Db        : Entities_Database;
      File      : VFS.Virtual_File;
      LI        : LI_File;
      Timestamp : Ada.Calendar.Time := No_Time) return Source_File;
   --  Get or create a Source_File corresponding to File.
   --  If there is already an entry for it in the database, the corresponding
   --  Source_File is returned, and the timestamp is adjusted if the
   --  parameter is not No_Time. Otherwise, a new entry is added.
   --  You need to Ref the entry if you intend to keep it in a separate
   --  structure.
   --  The file is automatically added to the list of files for that LI.

   procedure Add_Depends_On
     (File : Source_File; Depends_On  : Source_File);
   --  Add a new dependency to File. No check is done to ensure the dependency
   --  is not already listed. File is automatically added to the list of
   --  files that Depends_On imports.

   procedure Reset (File : Source_File);
   --  Indicate that the parsed contents of File is no longer valid. All
   --  associated cross-references are removed from the table.

   -------------------
   -- File_Location --
   -------------------

   type File_Location is record
      File   : Source_File;
      Line   : Natural;
      Column : Natural;
   end record;
   No_File_Location : constant File_Location := (null, 0, 0);

   ------------------------
   -- Entity_Information --
   ------------------------

   type Entity_Information_Record is tagged private;
   type Entity_Information is access Entity_Information_Record'Class;

   procedure Unref (Entity : in out Entity_Information);
   procedure Ref   (Entity : Entity_Information);
   --  Change reference counting for the file. When it reaches 0, the memory
   --  is freed.

   function Get_Or_Create
     (Name   : String;
      File   : Source_File;
      Line   : Natural;
      Column : Natural) return Entity_Information;
   --  Get an existing or create a new declaration for an entity. File, Line
   --  and column are the location of irs declaration.

   procedure Set_Kind (Entity : Entity_Information; Kind : E_Kind);
   procedure Set_End_Of_Scope
     (Entity : Entity_Information; Location : File_Location);
   procedure Set_Is_Renaming_Of
     (Entity : Entity_Information; Renaming_Of : Entity_Information);
   --  Override some information for the entity.

   procedure Add_Reference
     (Entity : Entity_Information; Location : File_Location);
   --  Add a new reference to the entity. No Check is done whether this
   --  reference already exists.

   procedure Set_Type_Of
     (Entity : Entity_Information; Is_Of_Type : Entity_Information);
   --  Specifies the type of a variable. If Entity is a type, this also
   --  registers it as a child of Is_Of_Type for faster lookup.

   procedure Add_Primitive_Subprogram
     (Entity : Entity_Information; Primitive : Entity_Information);
   --  Add a new primitive operation to Entity

   procedure Set_Pointed_Type
     (Entity : Entity_Information; Points_To : Entity_Information);
   --  For an access type, indicates which type it points to

   procedure Set_Returned_Type
     (Entity : Entity_Information; Returns : Entity_Information);
   --  Stores the type returned by a subprogram

   ----------------
   -- Scope_Tree --
   ----------------

   type Scope_Tree is private;


private

   ----------------
   -- Scope_Tree --
   ----------------

   type Scope_Tree_Node;
   type Scope_Tree is access Scope_Tree_Node;
   type Scope_Tree_Node is record
      Sibling  : Scope_Tree;
      Parent   : Scope_Tree;
      Entity   : Entity_Information;
      --  The entity addressed by this node

      Contents : Scope_Tree;
      --  The first entity referenced under that node.

      Location : File_Location;
      --  The precise location we are talking about. If
      --  Location = Entity.Declaration, we are in a node describing the
      --  declaration of an entity.
   end record;

   procedure Destroy (Tree : in out Scope_Tree);
   --  Free the memory occupied by the scope tree

   -----------------------------
   -- Entity_Information_List --
   -----------------------------

   package Entity_Information_Arrays is new Dynamic_Arrays
     (Data                    => Entity_Information,
      Table_Multiplier        => 1,
      Table_Minimum_Increment => 10,
      Table_Initial_Size      => 5);
   subtype Entity_Information_List is Entity_Information_Arrays.Instance;
   Null_Entity_Information_List : constant Entity_Information_List :=
     Entity_Information_Arrays.Empty_Instance;

   function Find
     (List   : Entity_Information_List; Loc : File_Location)
      return Entity_Information;
   --  Return entity declared at Loc, or null if there is no such entity

   ------------------------
   -- File_Location_List --
   ------------------------

   package File_Location_Arrays is new Dynamic_Arrays
     (Data                    => File_Location,
      Table_Multiplier        => 1,
      Table_Minimum_Increment => 10,
      Table_Initial_Size      => 5);
   subtype File_Location_List is File_Location_Arrays.Instance;
   Null_File_Location_List : constant File_Location_List :=
     File_Location_Arrays.Empty_Instance;

   ------------------------
   -- Entity_Information --
   ------------------------

   type Entity_Information_Record is tagged record
      Name                  : GNAT.OS_Lib.String_Access;
      Kind                  : E_Kind;

      Declaration           : File_Location;
      --  The location of the declaration for this entity.

      End_Of_Scope          : File_Location;
      --  The location at which the declaration of this entity ends. This is
      --  used for all entites that contain other entities (records, C++
      --  classes, packages,...)

      Parent_Types          : Entity_Information_List;
      Pointed_Type          : Entity_Information;
      Returned_Type         : Entity_Information;
      Primitive_Op_Of       : Entity_Information;
      --  These contain information for parent types, supertypes, pointed
      --  types, type of entity contained in an array or returned type for
      --  a function.
      --  It also contains a pointer to the class for which this entity is
      --  a primitive operation
      --  ??? These could be collapsed into a single list, depending on the
      --  kind of the current entity.

      Rename                : Entity_Information;
      --  The entity that this one renames.

      Primitive_Subprograms : Entity_Information_List;

      Child_Types           : Entity_Information_List;
      --  All the types derives from this one.

      References            : File_Location_List;
      --  All the references to this entity in the parsed files

      Ref_Count             : Natural := 1;
      --  The reference count for this entity. When it reaches 0, the entity
      --  is released from memory.
   end record;

   --------------------
   -- Entities_Table --
   --------------------

   type Entity_Information_List_Access is access Entity_Information_List;

   function Get_Name (D : Entity_Information) return GNAT.OS_Lib.String_Access;
   function Get_Name
     (D : Entity_Information_List_Access) return GNAT.OS_Lib.String_Access;
   --  Return the name of the first entity in the list

   procedure Destroy (D : in out Entity_Information_List_Access);

   package Entities_Tries is new Tries
     (Data_Type => Entity_Information_List_Access,
      No_Data   => null,
      Get_Index => Get_Name,
      Free      => Destroy);
   --  Each node in the tree contains all the entities with the same name.

   procedure Add (Entities         : in out Entities_Tries.Trie_Tree;
                  Entity           : Entity_Information;
                  Check_Duplicates : Boolean);
   --  Add a new entity, if not already there, to D

   procedure Remove  (D : in out Entities_Tries.Trie_Tree;
                      E : Entity_Information);
   --  Remove the information for a specific entity from the table.

   ----------------------
   -- Source_File_List --
   ----------------------

   package Source_File_Arrays is new Dynamic_Arrays
     (Data                    => Source_File,
      Table_Multiplier        => 1,
      Table_Minimum_Increment => 10,
      Table_Initial_Size      => 5);
   subtype Source_File_List is Source_File_Arrays.Instance;
   Null_Source_File_List : constant Source_File_List :=
     Source_File_Arrays.Empty_Instance;

   -----------------
   -- Source_File --
   -----------------

   type Source_File_Record is tagged record
      Timestamp    : Ada.Calendar.Time := No_Time;
      --  The timestamp of the file at the time it was parsed. This is left
      --  to No_Time if the file has never been parsed.

      Name        : VFS.Virtual_File;

      Entities    : Entities_Tries.Trie_Tree;
      --  All the entities defined in the source file

      Depends_On  : Source_File_List;
      Depended_On : Source_File_List;
      --  The list of dependencies on or from this file

      Scope       : Scope_Tree;
      --  The scope tree for this file. This is created on-demand the first
      --  time it is needed.

      LI          : LI_File;
      --  The LI file used to parse the file

      All_Entities : Entities_Tries.Trie_Tree;
      --  The list of all entities referenced in the file, and that are defined
      --  in other files.
      --  ??? This could be computed by traversing all the files in Depends_On,
      --  and check whether their entities have references in the current file.

      Ref_Count   : Integer := 0;
      Is_Valid    : Boolean;
      --  The reference counter. If Is_Valid is True, this indicates that the
      --  information for that file is not up-to-date and not available.
   end record;

   -----------------
   -- Files_Table --
   -----------------

   type HTable_Header is new Natural range 0 .. 3000;
   function Hash (Key : VFS.Virtual_File) return HTable_Header;

   package Files_HTable is new HTables.Simple_HTable
     (Header_Num   => HTable_Header,
      Element      => Source_File,
      Free_Element => Unref,
      No_Element   => null,
      Key          => VFS.Virtual_File,
      Hash         => Hash,
      Equal        => VFS."=");

   -------------
   -- LI_File --
   -------------

   type LI_File_Record is tagged record
      Db           : Entities_Database;

      Name      : VFS.Virtual_File;
      Timestamp : Ada.Calendar.Time := No_Time;

      Files     : Source_File_List;
      --  All the files for which xref is provided by this LI_File.

      Ref_Count : Natural := 1;
      --  The reference counter
   end record;

   --------------
   -- LI_Table --
   --------------

   package LI_HTable is new HTables.Simple_HTable
     (Header_Num   => HTable_Header,
      Element      => LI_File,
      Free_Element => Unref,
      No_Element   => null,
      Key          => VFS.Virtual_File,
      Hash         => Hash,
      Equal        => VFS."=");

   -----------------------
   -- Entities_Database --
   -----------------------

   type Entities_Database_Record is record
      Entities : Entities_Tries.Trie_Tree;
      Files    : Files_HTable.HTable;
      LIs      : LI_HTable.HTable;
   end record;
   type Entities_Database is access Entities_Database_Record;

end Entities;
