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

with HTables;
with Types;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Projects;
with Language;
with Language_Handlers;
with VFS;
with Ada.Calendar;

package Src_Info is

   -------------------------------
   -- Library Information files --
   -------------------------------

   type LI_File_Ptr is private;
   --  A handle to a structure containing all the semantic information
   --  concerning a given unit.

   No_LI_File : constant LI_File_Ptr;
   --  A null LI_File_Ptr.

   function Is_Incomplete (Source_Info : LI_File_Ptr) return Boolean;
   --  Return True if the given Source_Info has not been built using
   --  the corresponding LI file and hence can not be used for performing
   --  searches on this LI_File.

   type LI_File_List is private;
   --  A list of LI_File_Ptr.

   procedure Reset (LIFL : in out LI_File_List);
   --  Reset the given list of LI_File_Ptr and deallocate all the
   --  memory allocated for this list. The new list becomes empty.

   function Locate
     (List        : LI_File_List;
      LI_Filename : VFS.Virtual_File) return LI_File_Ptr;
   --  Return a pointer to the LI_File whose filename is LI_Filename.
   --  Return No_LI_File if no such LI_File is found.

   function Locate_From_Source
     (List            : LI_File_List;
      Source_Filename : VFS.Virtual_File) return LI_File_Ptr;
   --  Return a pointer to the LI_File which has a source file named
   --  Source_Filename. Return No_LI_File if not found.
   --  Note that the path to the file is ignored during the search, only
   --  the basename is taken into account.

   function Get_LI_Filename (LI : LI_File_Ptr) return VFS.Virtual_File;
   --  Return the name of the LI file associated with LI

   -----------------------
   --  File information --
   -----------------------

   type Internal_File is private;
   --  Information on a file and its unit. This information can be stored for
   --  later usage, and remains valid even when the LI file is parsed again.

   function Make_Source_File
     (Source_Filename : VFS.Virtual_File;
      Handler         : access Language_Handlers.Language_Handler_Record'Class;
      Project         : Projects.Project_Type) return Internal_File;
   --  Converts from a source filename to a File_Info structure.
   --  The returned result will need to be destroyed.
   --
   --  Project should be the project to which Source_Filename belongs (so that
   --  both the naming scheme and the search path are correct).

   function Get_Source_Filename (File : Internal_File) return VFS.Virtual_File;
   --  Return the Filename for the given File.

   function Get_Unit_Part
     (Lib_Info : LI_File_Ptr; File : VFS.Virtual_File)
      return Projects.Unit_Part;
   --  Return the type of File (a body, a spec or a separate).
   --  See also Projects.Registry.Get_Unit_Part_From_Filename if you are
   --  working with filenames that don't have a matching LI_File.

   function Get_Unit_Name
     (Lib_Info : LI_File_Ptr; File : VFS.Virtual_File) return String;
   --  Return the unit name, when applicable, for the file. If the language of
   --  the file doesn't support the concept of units, the empty string is
   --  returned.

   ----------------------------
   -- Dependency Information --
   ----------------------------

   type Dependency_Info is private;
   --  Information about a dependency.

   function Get_Depends_From_Spec (Dep : Dependency_Info) return Boolean;
   --  Return True if the given Dep is an explicit dependency from the
   --  specificiations part.

   function Get_Depends_From_Body (Dep : Dependency_Info) return Boolean;
   --  Return True if the given Dep is an explicit dependency from the
   --  implementation part.

   --------------------------
   --  LI handler iterator --
   --------------------------

   type LI_Handler_Iterator is abstract tagged private;
   type LI_Handler_Iterator_Access is access all LI_Handler_Iterator'Class;
   --  An iterator to generate the LI database for a set of source files.
   --  See the functions Generate_LI_For_Source and Generate_LI_For_Project
   --  for the factories used to create this type.
   --  See the private part for some subprograms that can be helpful when
   --  writting your own iterators.

   procedure Free (LI : in out LI_Handler_Iterator_Access);
   --  Free the memory associated with the handler, and destroy the iterator.

   procedure Continue
     (Iterator : in out LI_Handler_Iterator;
      Finished : out Boolean) is abstract;
   --  This function should move to the next source file that has been
   --  analyzed, provided the previous file is fully parsed.
   --  If the files are analyzed by external processes, the call to
   --  Generate_LI_For_Project would for instance start the external process
   --  for the first file, and when Next is called, it should check that the
   --  first process has finished executing before processing the next file.
   --
   --  If an extra phase needs to be done after parsing all the source files,
   --  it should also be done as a result of a call to Next.
   --
   --  Nothing needs to be done if the previous source file hasn't been fully
   --  analyzed yet.
   --
   --  Finished should be True if the Iterator has finished regenerating the
   --  database. The memory used for the iterator should be freed at the same
   --  time.

   procedure Destroy (Iterator : in out LI_Handler_Iterator);
   --  Free the memory used by the list of source files.

   -----------------
   -- LI handlers --
   -----------------

   type LI_Handler_Record is abstract tagged limited null record;
   type LI_Handler is access all LI_Handler_Record'Class;
   --  General type to handle and generate Library Information data (for
   --  cross-references, and the various queries for the browsers).
   --  Derived types should be created for all the languages supported.

   procedure Destroy (Handler : in out LI_Handler_Record);
   procedure Destroy (Handler : in out LI_Handler);
   --  Free the memory occupied by Handler. By default, this does nothing

   procedure Create_Or_Complete_LI
     (Handler         : access LI_Handler_Record;
      File            : in out LI_File_Ptr;
      Source_Filename : VFS.Virtual_File;
      List            : LI_File_List;
      Project         : Projects.Project_Type) is abstract;
   --  Find the LI file for Source_Filename, or create one if there is none
   --  yet.
   --  On calling this subprogram, File is always null, and must be looked for
   --  in List. However, this search is done differently depending on handler.
   --  If an LI file was found in list, but was either incomplete or no longer
   --  up-to-date, it must be parsed again.
   --  Source_Filename should contain the full path to the source file.
   --  This subprogram garantees that the returned File has been fully parsed.

   function LI_Filename_From_Source
     (Handler         : access LI_Handler_Record;
      Source_Filename : VFS.Virtual_File;
      Project         : Projects.Project_Type)
      return VFS.Virtual_File is abstract;
   --  Return the name of the Library Information file associated with
   --  Source_Filename. In Ada, there is one such file per source
   --  file. However, in some other languages, all source files might be
   --  associated with the same LI file.
   --  This name is used as an index into the LI_File_List.

   function Case_Insensitive_Identifiers
     (Handler : access LI_Handler_Record) return Boolean is abstract;
   --  Return True if the language associated with Handler is case-insensitive.
   --  Note that for case insensitive languages, the identifier names must be
   --  storer in lower cases in the LI structure.

   procedure Parse_All_LI_Information
     (Handler      : access LI_Handler_Record;
      List         : LI_File_List;
      In_Directory : String;
      Project      : Projects.Project_Type) is abstract;
   --  Parse all the existing LI information in the directory In_Directory, and
   --  store it in the internal structures. No recompilation or parsing of the
   --  sources needs to be done in general.

   function Generate_LI_For_Source
     (Handler       : access LI_Handler_Record;
      Root_Project  : Projects.Project_Type;
      File_Project  : Projects.Project_Type;
      Full_Filename : VFS.Virtual_File) return LI_Handler_Iterator'Class
   is abstract;
   --  Generate the LI information for a given file. In Ada, this means
   --  recompiling the file so as to generate the corresponding ALI
   --  file. However, for some languages the database can only be regenerated
   --  by parsing all the files, and so nothing should be done in this
   --  subprogram.
   --  Note that only the database on the disk needs to be regenerated, not the
   --  LI structures themselves, which will be done by Create_Or_Complete_LI.

   function Generate_LI_For_Project
     (Handler      : access LI_Handler_Record;
      Root_Project : Projects.Project_Type;
      Project      : Projects.Project_Type;
      Recursive    : Boolean := False)
      return LI_Handler_Iterator'Class is abstract;
   --  Generate the LI information for all the source files in Project (and all
   --  its imported projects if Recursive is True).
   --  This function should do as few work as possible, and the iterator will
   --  be called until all the files are processed.
   --  Note that only the database on the disk needs to be regenerated, not the
   --  LI structures themselves, which will be done by Create_Or_Complete_LI.

   ------------------
   --  Constructs  --
   ------------------

   procedure Parse_File_Constructs
     (Handler      : access LI_Handler_Record;
      Root_Project : Projects.Project_Type;
      Languages    : access Language_Handlers.Language_Handler_Record'Class;
      File_Name    : VFS.Virtual_File;
      Result       : out Language.Construct_List);
   --  Build a Construct_List, either using the src_info tools (like SN)
   --  or a language parser.

   --------------
   -- Entities --
   --------------

   type E_Reference is private;
   No_Reference : constant E_Reference;

   type File_Location is private;
   Null_File_Location : constant File_Location;
   Predefined_Entity_Location : constant File_Location;

   function Get_Location (Ref : E_Reference) return File_Location;
   --  Return the location for a declaration or a reference.

   function Get_File   (Location : File_Location) return VFS.Virtual_File;
   function Get_Line   (Location : File_Location) return Positive;
   function Get_Column (Location : File_Location) return Natural;
   --  Return the fields of a location

   function Is_Read_Reference  (Ref : E_Reference) return Boolean;
   function Is_Write_Reference (Ref : E_Reference) return Boolean;
   --  Return true if this is a read-only or write reference to an entity.
   --  It is possible that none of the two return True for some special
   --  entities.

   ------------
   -- E_Kind --
   ------------
   --  This type is exported only for use in the src_info hierarchy. In fact,
   --  it will eventually be replaced by a tagged type hierarchy.

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

   Unresolved_Entity_Kind : constant E_Kind;
   Overloaded_Entity_Kind : constant E_Kind;
   --  Similar to Unresolved_Entity above

   function Type_To_Object (Kind : E_Kind) return E_Kind;
   --  Return a new E_Kind that is an object of the type Kind. For instance,
   --  given a Access_Kind type, it returns an Access_Kind object. In some
   --  cases (subprograms,...) this doesn't apply and will return Kind itself.

   function Kind_To_String (Kind : E_Kind) return String;
   --  Return a printable string for the entity kind.
   --  It is the responsability of the caller to translate the string through
   --  calls to Glide_Intl."-"

   type E_Scope is (Global_Scope, Local_Scope, Class_Static, Static_Local);
   --  The scope of an entity. The values have the following meaning:
   --     - Global_Entity: publicly visible entity in a top level library.
   --     - Local_Entity: an entity that does not satisfy the conditions
   --       to be a Global_Entity.

private

   --  In the following declarations, some abbreviations have been used
   --  to reduce the length the type names:
   --    - Entity       -> E
   --    - Information  -> Info
   --    - Library      -> Lib
   --    - Library_Info -> LI


   --  Overloaded symbols
   --  ==================
   --
   --  The internal structure defined in this package can handle overloaded
   --  entities that could not be resolved completely by the parser, generally
   --  for lack of semantic analysis. In that case, it is up to GPS to ask the
   --  user, when doing a cross-reference for instance, which declaration he
   --  wants to jump to.
   --  Given the following C program (in file.c),
   --       /* line 1 */  void f (int);
   --       /* line 2 */  void f (char);
   --       /* line 3 */  f (0);
   --       /* line 4 */  f ('a');
   --  and if the parser couldn't resolve the calls on line 3 and 4, the
   --  following entries need to be generated in the structure:
   --
   --    - One E_Declaration_Info in the list of declarations for file.c, with
   --      the following contents:
   --          (Declarations => E_Declaration'(Name => "f",
   --                                          Kind   => Non_Generic_Procedure,
   --                                          Location =>  <line 1>,
   --                                          ...),
   --           References   => <empty list>)
   --
   --    - A similar E_Declaration info is generated for the function f on line
   --      2.
   --
   --    - An additional entry is generated for all the references, with the
   --      following contents:
   --          (Declarations => E_Declaration'(Name => "f",
   --                                          Kind => Overloaded_Entity,
   --                                          <other fields left to default>),
   --           References => ( (Location => <line 3>),
   --                           (Location => <line 4>))
   --
   --  When processing any query on the structure, GPS will encounter the
   --  special entity kind Overloaded_Entity. It then has to check all the
   --  entities with a matching name that are found either in the file file.c
   --  or in one of its include files.


   --  Renaming entities
   --  =================
   --
   --  Some languages, like Ada, have the capacity to provide name aliases for
   --  other entities, possibly entities defined in other packages or
   --  namespaces. This renaming can happen for subprograms, variables,
   --  packages,...
   --
   --  This structure handles this through a special field in E_Declaration,
   --  that points to the renamed entity.
   --
   --  For instance, in the following code:
   --        procedure Bar;
   --        procedure Foo renames Bar;
   --  There are two E_Declaration_Info added to the list of entities for the
   --  current source file.
   --
   --   - One E_Declaration_Info for Bar
   --   - One E_Declaration_Info for Foo, with the field Renames pointing to
   --     the location of the declaration of Bar (file, line, column).


   --  C's typedef and Ada subtypes
   --  ============================
   --
   --  Typedefs in C are equivalent to Ada subtypes. Therefore, they should be
   --  implemented by using the Parent_Location field of E_Declaration, to
   --  point to the declaration of the old type.
   --
   --        typedef old_type new_type;   //  line 10 in "current_file"
   --
   --  would result in two E_Declaration records created for the current file:
   --
   --       Old_Type_Decl : E_Declaration'(Name => "old_type",
   --                                      Location => ("current_file", 10, 9),
   --                                      Kind => ...);
   --       New_Type_Decl : E_Declaration'
   --                          (Name => "new_type",
   --                           Parent_Location => ("current_file", 10, 9),
   --                           ...);
   --
   --   When a predefined type is derived from (for instance
   --   "typdef int my_int"), then the Parent_Location should be
   --   Predefined_Entity_Location. We lose the information for the name of the
   --   parent type, but this isn't required currently by GPS. If needed, a new
   --   field will be added to E_Declaration to store the name of the
   --   predefined parent type.


   --  Ada records and C structs
   --  =========================
   --
   --  These types, along with several others like Ada enumerations or C
   --  unions, group several fields into one single type. Therefore, GPS needs
   --  to be able to know what the various field components are, for instance
   --  to display them in the class browser.
   --
   --  To save space, no explicit list is saved in the E_Declaration
   --  structure. But the information can easily be recreated from the Location
   --  and the End_Of_Scope fields, since all the type declarations that appear
   --  between these two locations belong to the record.
   --
   --  For instance, given the following C code:
   --       struct S {    // line 1
   --          int f;     // line 2
   --       }             // line 3
   --
   --  The E_Declaration for "S" would be:
   --       E_Declaration'
   --           (Name         => "S",
   --            Kind         => Record_Type,
   --            Location     => ("current_file", 1, 8),
   --            End_Of_Scope => (("current_file", 3, 1), End_Of_Spec),
   --            ...)
   --
   --  and then one for "f" is:
   --       E_Declaration'
   --           (Name     => "f",
   --            Kind     => Modular_Integer_Type,
   --            Location => ("current_file", 2, 8),
   --            ...);


   --  C++ classes and Ada tagged types
   --  ================================
   --
   --  Such types are associated with a Record_Type E_Kind (at least in Java,
   --  C++ and Ada). However, some extra information is needed to find the
   --  attributes and the methods of such a type.
   --
   --  The attributes are found with the same attribute used for Ada records
   --  and C structs.
   --
   --  The primitive operations are stored in E_Declaration in a field
   --  Primitive_Operations.

   --  C++ templates
   --  =============
   --
   --  C++ class templates have Generic_Class kind.

   type Reference_Kind is
     (Reference,
      Modification,
      Instantiation_Reference,
      Body_Entity,
      Completion_Of_Private_Or_Incomplete_Type,
      Discriminant,
      Type_Extension,
      Implicit,
      Primitive_Operation,
      Overriding_Primitive_Operation,
      With_Line,
      Label,
      Subprogram_In_Parameter,
      Subprogram_In_Out_Parameter,
      Subprogram_Out_Parameter,
      Subprogram_Access_Parameter,
      Formal_Generic_Parameter,
      Parent_Package,
      End_Of_Spec,
      End_Of_Body);
   --  The kind of reference to an entity. They have the following meaning:
   --    - Reference: The entity is used
   --    - Modification: The value of the entity is changed
   --    - Instantiation_Reference: Reference to the instantiation of a
   --      generic.
   --    - Body_Entity: Used for spec entities that are repeated in a body,
   --      including the unit name itself, and the formals in the case of
   --      a subprogram. Also used for entry-names in accept statements.
   --    - Completion_Of_Private_Or_Incomplete_Type: Used to mark the
   --      completion of a private type or incomplete type
   --    - type_Extension: Used to mark the reference as the entity from
   --      which a tagged type is extended.
   --    - Implicit: Used to identify a reference to the entity in a generic
   --      actual or in a default in a call.
   --    - Label: Used for cases where the name of the entity appears in
   --      syntactic constructs only, but doesn't impact the code, for instance
   --      in "end Foo;" constructs in Ada.
   --    - End_Of_Spec: Used to identify the end of the following constructs.
   --      Block statement, loop statement, package specification, task
   --      definition, protected definition, record definition.
   --    - End_Of_Body: Used to identify the end of the following constructs.
   --      Subprogram body, package body, task body, entry body, protected
   --      body, accept statement.
   --    - Primitive_Operation: used for primitive operations of tagged types
   --      (in Ada), or for methods (in C++). It possibly points to inherited
   --      methods in the parent type.
   --    - Overriding_Primitive_Operation is used for primitive operations
   --      that override one of the inherited operations from the parent (for
   --      instance A derives from B and both define the operation foo() with
   --      the same profile, foo() will be marked as an overriding primitive
   --      operation for B.
   --    - Subprogram_*_Parameter: for a subprogram declaration, references all
   --      its parameters, along with their passing mode ("in", "in out", ...)
   --    - Formal_Generic_Parameter: for a generic, reference its format
   --      parameters.
   --    - Parent_Package: for a child Ada package, reference its parent. This
   --      parent, in turn, references its own parent package.
   --    - Discriminant: points to the declaration of the discriminants for
   --      this type.

   type Reference_Kind_To_Boolean_Map is array (Reference_Kind) of Boolean;

   Is_End_Reference : constant Reference_Kind_To_Boolean_Map :=
     (Reference                                => False,
      Instantiation_Reference                  => False,
      Modification                             => False,
      Body_Entity                              => False,
      Completion_Of_Private_Or_Incomplete_Type => False,
      Type_Extension                           => False,
      Implicit                                 => False,
      Label                                    => False,
      Primitive_Operation                      => False,
      Discriminant                             => False,
      Overriding_Primitive_Operation           => False,
      With_Line                                => False,
      Subprogram_In_Parameter                  => False,
      Subprogram_In_Out_Parameter              => False,
      Subprogram_Out_Parameter                 => False,
      Subprogram_Access_Parameter              => False,
      Formal_Generic_Parameter                 => False,
      Parent_Package                           => False,
      End_Of_Spec                              => True,
      End_Of_Body                              => True);
   --  True if the matching entity indicates an end-of-scope (end of subprogram
   --  declaration, end of record definition, ...)

   Is_Start_Reference : constant Reference_Kind_To_Boolean_Map :=
     (Reference                                => False,
      Instantiation_Reference                  => False,
      Modification                             => False,
      Body_Entity                              => True,
      Completion_Of_Private_Or_Incomplete_Type => True,
      Type_Extension                           => False,
      Implicit                                 => False,
      Label                                    => False,
      Primitive_Operation                      => False,
      Discriminant                             => False,
      Overriding_Primitive_Operation           => False,
      With_Line                                => False,
      Subprogram_In_Parameter                  => False,
      Subprogram_In_Out_Parameter              => False,
      Subprogram_Out_Parameter                 => False,
      Subprogram_Access_Parameter              => False,
      Formal_Generic_Parameter                 => False,
      Parent_Package                           => False,
      End_Of_Spec                              => False,
      End_Of_Body                              => False);
   --  True if the matching entity indicates an start-of-scope (start of
   --  subprogram declaration, start of record definition, ...)

   Is_Real_Reference : constant Reference_Kind_To_Boolean_Map :=
     (Reference                                => True,
      Instantiation_Reference                  => True,
      Modification                             => True,
      Body_Entity                              => True,
      Completion_Of_Private_Or_Incomplete_Type => True,
      Type_Extension                           => True,
      Implicit                                 => False,
      Label                                    => True,
      Primitive_Operation                      => False,
      Discriminant                             => False,
      Overriding_Primitive_Operation           => False,
      With_Line                                => True,
      Subprogram_In_Parameter                  => False,
      Subprogram_In_Out_Parameter              => False,
      Subprogram_Out_Parameter                 => False,
      Subprogram_Access_Parameter              => False,
      Formal_Generic_Parameter                 => False,
      Parent_Package                           => False,
      End_Of_Spec                              => False,
      End_Of_Body                              => False);
   --  True if the name of the entity really appears at that location in the
   --  file (for instance, a primitive operation doesn't point directly to the
   --  entity, but only provides more information about the entity).

   Read_Reference : constant Reference_Kind_To_Boolean_Map :=
     (Reference                                => True,
      Instantiation_Reference                  => True,
      Modification                             => False,
      Body_Entity                              => True,
      Completion_Of_Private_Or_Incomplete_Type => True,
      Type_Extension                           => True,
      Implicit                                 => False,
      Label                                    => True,
      Primitive_Operation                      => False,
      Discriminant                             => False,
      Overriding_Primitive_Operation           => False,
      With_Line                                => True,
      Subprogram_In_Parameter                  => False,
      Subprogram_In_Out_Parameter              => False,
      Subprogram_Out_Parameter                 => False,
      Subprogram_Access_Parameter              => False,
      Formal_Generic_Parameter                 => False,
      Parent_Package                           => False,
      End_Of_Spec                              => False,
      End_Of_Body                              => False);
   --  True if the reference is a read-only reference to the entity

   Write_Reference : constant Reference_Kind_To_Boolean_Map :=
     (Reference                                => False,
      Instantiation_Reference                  => False,
      Modification                             => True,
      Body_Entity                              => False,
      Completion_Of_Private_Or_Incomplete_Type => False,
      Type_Extension                           => False,
      Implicit                                 => False,
      Label                                    => False,
      Primitive_Operation                      => False,
      Discriminant                             => False,
      Overriding_Primitive_Operation           => False,
      With_Line                                => False,
      Subprogram_In_Parameter                  => False,
      Subprogram_In_Out_Parameter              => False,
      Subprogram_Out_Parameter                 => False,
      Subprogram_Access_Parameter              => False,
      Formal_Generic_Parameter                 => False,
      Parent_Package                           => False,
      End_Of_Spec                              => False,
      End_Of_Body                              => False);
   --  True if the reference is a write reference to the entity

   Unresolved_Entity_Kind : constant E_Kind :=
     (Unresolved_Entity, False, False, False);
   Overloaded_Entity_Kind : constant E_Kind :=
     (Overloaded_Entity, False, False, False);

   type LI_File_Constrained;
   type LI_File_Ptr is access LI_File_Constrained;

   No_LI_File : constant LI_File_Ptr := null;

   type Source_File is record
      LI              : LI_File_Ptr;
      Part            : Projects.Unit_Part;
      Source_Filename : String_Access;
      --  Allocated only when Part is set to Unit_Separate. Set to null
      --  otherwise.
   end record;
   --  A source file is represented by two or three elements:
   --    - its LI_File
   --    - its unit part
   --    - its file name when it is a separate, so that it can be found in the
   --      LI file
   --  Note that the reason we have both Part and Source_Filename is so that we
   --  don't have to always allocate memory for the file name for the two
   --  default files allowed per LI_File.

   No_Source_File : constant Source_File :=
     (LI              => null,
      Part            => Projects.Unit_Spec,
      Source_Filename => null);
   --  To check that a Source_File is not null, a quick and good enough
   --  check is to verify that Source_File.LI is not null.

   function "=" (Left, Right : Source_File) return Boolean;
   --  A redefined equality function that compares the Unit_Name values, not
   --  the access value.

   type File_Location is record
      File   : Source_File;
      Line   : Positive;
      Column : Natural;
   end record;
   --  A location in a source file.
   --  Column might be null for generic instantiations, or if the LI handler
   --  doesn't know how to handle the column.
   --  Note that a tabulation character in the source file should count as 8
   --  columns in this structure.

   Null_File_Location : constant File_Location :=
     (File   => No_Source_File,
      Line   => 1,
      Column => 1);
   --  To verify that a File_Location is not null, a quick and good enough
   --  check is to verify that File_Location.File.LI is not null.
   --  See function Is_File_Location which performs this check.

   Predefined_Entity_Location : constant File_Location :=
     (File   => No_Source_File,
      Line   => 2,
      Column => 2);
   --  This location is used to represent the declaration of predefined
   --  entities ("Integer" in Ada, "int" in C, ...).
   --  The fields Line and Column are chosen so that this constant is different
   --  from Null_File_Location.

   type Parent_Kind is
     (Pointed_Type, Parent_Type, Container_Type, Returned_Type);
   --  The type of an entity in Parent_Location:
   --     - Pointed_Type: for access types, this means that the location is
   --       in fact the designed type by the access. This is also the contents
   --       type for an array.
   --     - Parent_Type: for type, this is one of the parent types. This also
   --       contains the type of objects.
   --     - Container_Type: for subtypes, point to the parent type. For
   --       enumeration literals, point to the enumeration type. For objects
   --       and components, point to the type.
   --     - Returned_Type: for subprograms, the type of the returned data
   --  We need this to distinguish between the kinds, since for instance an
   --  access type might either have a pointed type, or be a subtype of another
   --  access type.

   type File_Location_Node;
   type File_Location_List is access File_Location_Node;
   type File_Location_Node is record
      Value                  : File_Location;
      Kind                   : Parent_Kind;
      Predefined_Entity_Name : Types.Name_Id;
      Next                   : File_Location_List;
   end record;
   --  Predefined_Entity_Name is set when Value is Predefined_Entity_Location

   function "=" (Left, Right : File_Location) return Boolean;
   --  A redefined equality function that compares uses the redefined equality
   --  for the source file field.
   pragma Inline ("=");

   type E_Reference is record
      Location : File_Location;
      Kind     : Reference_Kind;
   end record;
   --  A reference to an entity.

   No_Reference : constant E_Reference :=
     (Location => Null_File_Location,
      Kind     => Reference);

   type E_Reference_Access is access all E_Reference;

   type E_Reference_Node;
   type E_Reference_List is access E_Reference_Node;
   type E_Reference_Node is record
      Value : E_Reference;
      Next  : E_Reference_List;
   end record;
   --  E_Reference_List is a chained list of E_Reference objects.
   --  E_Reference_Node is a node of this list.

   type E_Declaration is record
      Name            : String_Access;
      --  Name of the entity. For case-insensitive languages, this must be all
      --  lower-cases.

      Location        : File_Location;
      Kind            : E_Kind;

      Parent_Location : File_Location_List;
      --  Note that multiple parents can be set in the case of C++ classes and
      --  multiple inheritance.

      Scope           : E_Scope;
      End_Of_Scope    : E_Reference := No_Reference;
      --  The position at which the body of the entity finishes. It doesn't
      --  correspond to an actualy cross-reference for the entity. Its Kind
      --  should one for which Is_End_Reference (Kind) is True.

      Rename          : File_Location := Null_File_Location;
      --  Location of the declaration that this one renames. See the
      --  explanation about renaming above.

      Primitive_Subprograms : E_Reference_List;
      --  <specific to Ada tagged types or C++ classes>
      --  This fields points to the list of methods or primitive subprograms
      --  for a class, possibly in other LI files if the operation was
      --  inherited and not overloaded. The Kind of the reference should be
      --  Primitive_Operation.
      --  This list should be empty in most cases, except in the file that
      --  contains the declaration of the tagged type (other files that simply
      --  reference the type do not need to set this field, since they do not
      --  necessarily have all the necessary information anyway).
   end record;
   --  All the information about an entity declaration.
   --  ??? Note that, in order to save a little bit of memory space,
   --  End_Of_Scope could be defined as a pointer to E_Reference instead of
   --  as an E_Reference structure, because most entities do not have end
   --  of scope info (because this is not relevant for these entities).
   --  This has not been done yet because it adds a little bit of
   --  complexity to the implementation (in terms of memory management).

   No_Declaration : constant E_Declaration :=
     (Name                  => null,
      Location              => Null_File_Location,
      Kind                  => Unresolved_Entity_Kind,
      Parent_Location       => null,
      Scope                 => Local_Scope,
      End_Of_Scope          => No_Reference,
      Rename                => Null_File_Location,
      Primitive_Subprograms => null);

   type E_Declaration_Access is access all E_Declaration;

   type E_Declaration_Info is record
      Declaration : E_Declaration;
      References  : E_Reference_List;
   end record;
   --  All the information associated to a given entity declaration, which
   --  is the information about the declaration itself, and the references
   --  to this declaration.

   No_Declaration_Info : constant E_Declaration_Info := (No_Declaration, null);

   type E_Declaration_Info_Node;
   type E_Declaration_Info_List is access E_Declaration_Info_Node;
   type E_Declaration_Info_Node is record
      Value : E_Declaration_Info;
      Next  : E_Declaration_Info_List;
   end record;
   --  E_Declaration_Info_List is a chained list of E_Declaration_Info.
   --  E_Declaration_Info_Node is a node of this list.

   type Internal_File is record
      File_Name : VFS.Virtual_File;
      LI_Name   : VFS.Virtual_File;
   end record;
   --  The information associated with a source file, and that remains valid
   --  even when the LI file is parsed again.

   function To_Timestamp
     (Str : Types.Time_Stamp_Type) return Ada.Calendar.Time;
   --  Convert the string to an internal timestamp.

   type File_Info is record
      Unit_Name         : String_Access;
      --  ??? Should be only for languages where it makes sense, for instance
      --  in derived type. Can be left to null otherwise.

      Source_Filename   : String_Access;
      Cached_File       : VFS.Virtual_File;
      File_Timestamp    : Ada.Calendar.Time;
      Original_Filename : String_Access;
      Original_Line     : Positive;
      Declarations      : E_Declaration_Info_List;
   end record;
   --  The information associated to a source file.
   --  Directory_Name is set on demand only. In fact, you should only access it
   --  through Get_Directory_Name.
   --  This structure might be mostly empty in case we haven't parsed the LI
   --  file associated with the source file yet. The record might have just
   --  been created because another file depended on this one.
   --  However, even in that case, the File_Timestamp might have been set if we
   --  looked for the Directory_Name.
   --
   --  ??? It is possible to optimize a bit the memory usage by allocating
   --  the Source_Filename only when the naming of the unit is not
   --  following the standard naming scheme. The Source_Filename could
   --  then be computed from the unit name. This is not done for the
   --  moment because we would like to avoid language specific notions
   --  in the data structures defined.

   type File_Info_Ptr is access File_Info;

   type File_Info_Ptr_Node;
   type File_Info_Ptr_List is access File_Info_Ptr_Node;
   type File_Info_Ptr_Node is record
      Value : File_Info_Ptr;
      Next  : File_Info_Ptr_List;
   end record;
   --  File_Info_Ptr_List is a chained list of File_Info_Ptr.
   --  File_Info_Ptr_Node is a node of this list.
   --
   --  Note that we defined a list of File_Info_Ptr as opposed to a list of
   --  File_Info records because it will provide a more homegeneous interface
   --  inside LI_File record between the Spec_Info, Body_Info and Separate_Info
   --  fields which all contain File_Info_Ptr types.

   type Dependency_Info is record
      Depends_From_Spec : Boolean;
      Depends_From_Body : Boolean;
   end record;
   --  Information about where a dependency between two units comes from.

   type Dependency_File_Info is record
      File              : Source_File;
      Dep_Info          : Dependency_Info;
      Declarations      : E_Declaration_Info_List;
   end record;
   --  the information about a file on which a source file depends.

   type Dependency_File_Info_Node;
   type Dependency_File_Info_List is access Dependency_File_Info_Node;
   type Dependency_File_Info_Node is record
      Value : Dependency_File_Info;
      Next  : Dependency_File_Info_List;
   end record;
   --  Dependency_File_Info_List is a chained list of Dependency_File_Info.
   --  Dependency_File_Info_Node is a node of this list.

   No_Dependencies : constant Dependency_File_Info_List := null;

   type LI_File (Parsed : Boolean := False) is record
      Handler         : LI_Handler;
      --  The handler used to create this LI_File.

      LI_Filename     : VFS.Virtual_File;
      LI_Filename_Key : String_Access;
      Spec_Info       : File_Info_Ptr;
      Body_Info       : File_Info_Ptr;
      Separate_Info   : File_Info_Ptr_List;
      LI_Timestamp    : Ada.Calendar.Time;
      Project         : Projects.Project_Type;

      case Parsed is
         when True =>
            Compilation_Errors_Found : Boolean;
            Dependencies_Info        : Dependency_File_Info_List;
         when False =>
            null;
      end case;
   end record;
   --  All the information about a compilation unit.
   --  Note that this structure is referenced from other structures, and
   --  hence should never be deallocated, except when the entire LI File
   --  Tree (All LI_File object, with all the data pointed by these objects)
   --  is destroyed.

   type LI_File_Constrained is record
      LI : LI_File;
   end record;
   --  Use this record, instead of an LI_File when pointing to it, since
   --  otherwise we cannot change the discriminant of LI_File dynamically when
   --  needed (see ARM 3.10.9).

   type LI_File_Node;
   type LI_File_Node_Ptr is access LI_File_Node;
   type LI_File_Node is record
      Value : LI_File_Ptr;
      Next  : LI_File_Node_Ptr;
   end record;
   --  A structure used to instantiate the HTables.Static_HTable package, to
   --  build a hash-table of LI_File_Ptr objects. The Key will be the
   --  LI_Filename.
   --
   --  Note: The presence of the Next pointer makes it possible to build a
   --  chained list. However, this is not the intent of this type, which
   --  explains why it has not been named with the "_List" suffix. Define a
   --  different type if a chained list of LI_File_Ptr objects is needed.

   type LI_File_HTable_Index is range 1 .. 1_024;
   --  A range type that will be used in a hash-table storing all known
   --  LI_File objects. The upper bound value is empiric, and will probably
   --  need to be tuned for better performance.

   procedure Set_Next (E : LI_File_Node_Ptr; Next : LI_File_Node_Ptr);
   pragma Inline (Set_Next);
   --  Set the element after E in the chained-list to be Next.

   function Next (E : LI_File_Node_Ptr) return LI_File_Node_Ptr;
   pragma Inline (Next);
   --  Return a pointer to the LI_File_Node following given one.

   function Get_LI_Filename (E : LI_File_Node_Ptr) return VFS.Virtual_File;
   pragma Inline (Get_LI_Filename);
   --  return the filename of the LI_File pointed by E.

   function Get_LI_Filename_Key (E : LI_File_Node_Ptr) return String_Access;
   pragma Inline (Get_LI_Filename_Key);
   --  return the basename of E. Intended for use in the hash table

   function Hash (F : String_Access) return LI_File_HTable_Index;
   pragma Inline (Hash);
   --  Hash function.

   function Equal (F1, F2 : String_Access) return Boolean;
   pragma Inline (Equal);
   --  Return True if F1.all = F2.all.

   package LI_File_HTable is
     new HTables.Static_HTable
       (Header_Num => LI_File_HTable_Index,
        Elmt_Ptr   => LI_File_Node_Ptr,
        Null_Ptr   => null,
        Set_Next   => Set_Next,
        Next       => Next,
        Key        => String_Access,
        Get_Key    => Get_LI_Filename_Key,
        Hash       => Hash,
        Equal      => Equal);
   --  A hash-table of LI_File_Ptr objects. There will always be at most
   --  one element per key (that is one unit per unit name).
   --
   --  Using the Simple_HTable would simplify a bit the handling of this
   --  hash-table. This option has not been taken because it would then cause
   --  the duplication of the key (the unit name): the key is already embedded
   --  in the Node_Info structure. This causes a little bit of code duplication
   --  (we almost rewrite HTables.Simple_HTable, ie 120 SLOCs), but potentially
   --  saves a fair bit of memory.

   type LI_HTable_Access is access LI_File_HTable.HTable;

   type LI_File_List is record
      Table : LI_HTable_Access;
   end record;
   --  The list of LI_File is implemented as a hash-table rather than
   --  a plain chained list to improve the lookup performances.

   --------------------------
   --  LI handler iterator --
   --------------------------

   type LI_Handler_Iterator is abstract tagged record
      Source_Files : VFS.File_Array_Access;
      Current_File : Integer := 0;
   end record;

   procedure Compute_Sources
     (Iterator  : in out LI_Handler_Iterator'Class;
      Project   : Projects.Project_Type;
      Recursive : Boolean;
      Languages : Projects.Name_Id_Array);
   --  Compute the list of source files that will need to be analyzed by the
   --  iterator. Elements from this list can be read using
   --  Current_Source_File. Only the files belonging to Language will be
   --  parsed.
   --
   --  This subprogram is provided as a help when writting your own iterators.

   procedure Compute_Sources
     (Iterator    : in out LI_Handler_Iterator'Class;
      Source_File : VFS.Virtual_File);
   --  The list of files will be a single file.

   function Current_Source_File
     (Iterator : LI_Handler_Iterator'Class) return VFS.Virtual_File;
   --  Return the full path name to the next source file that needs to be
   --  analyzed by the iterator.
   --  The empty string "" is returned if there are no more source files.

   procedure Next_Source_File (Iterator : in out LI_Handler_Iterator'Class);
   --  Move to the next source file.

   -----------------------------
   -- LI_File_HTable services --
   -----------------------------

   --  This section provides a set of services to help manipulate the
   --  hash-table provided by LI_File_HTable in a more Src_Info-oriented
   --  manor. They are defined as private so that only childs of this package
   --  (which are expected to provide high-level services that hide the
   --  manipulation of such a list) have access to them. All other units will
   --  rely on the services provided by these child package to manipulate these
   --  lists.

   procedure Add
     (HT   : in out LI_File_HTable.HTable;
      LIFP : LI_File_Ptr);
   --  Saves the given LI_File_Ptr in the hash-table. If a LI_File_Ptr with
   --  the same unit name is already stored, then nothing is done and success
   --  is set to False (Rationale: the LI_File structure already stored might
   --  still be pointed-to by a Source_File, so we do not want to destroy such
   --  object lightly).

   procedure Reset (HT : in out LI_File_HTable.HTable);
   --  Destroys all LI_File_Ptr objects (and all memory allocated by these
   --  objects), and reset the hash-table.

   function Get
     (HT : LI_File_HTable.HTable; LI_Filename : String) return LI_File_Ptr;
   --  Return a pointer to the LI_File whose filename is LI_Filename.
   --  Return No_LI_File if no such LI_File is found.

   ---------------------------
   -- Non-exported services --
   ---------------------------

   --  This section defines a set of services that will help in the management
   --  of a list of LI_File. They are defined as private so that only childs
   --  of this package (which are expected to provide high-level services that
   --  hide the manipulation of such a list) have access to them. All other
   --  units will rely on the services provided by these child package to
   --  manipulate these lists.

   function Is_File_Location (Location : in File_Location) return Boolean;
   pragma Inline (Is_File_Location);
   --  Returns True if the given file location value has been set. It is
   --  faster than comparing the Location against Null_File_Location.

   function Get_Source_Filename (File : Source_File) return VFS.Virtual_File;
   --  Returns the source filename of the given file.
   --  Note that this function is merely a shortcut to
   --       File.Unit.Spec/Body/Separate_Info.Source_Filename.all
   --  and does not perform any check before accessing these fields. The
   --  caller should make sure that the information is accessible before
   --  invoking this function.

   function Get_File_Info (SF : Source_File) return File_Info_Ptr;
   --  Return an access to the File_Info associated to the given Source_File.
   --  This is basically the opposite of Make_Source_File.

   procedure Destroy (LIF : in out LI_File);
   --  Deallocate recursively the data contained in the given LI_File.

   procedure Destroy (LIFP : in out LI_File_Ptr);
   --  Deallocate recursively the LI_File_Ptr. Has no effect if LIFP is null.

   procedure Destroy (SF : in out Source_File);
   --  Deallocate the memory used by the given Source_File.

   procedure Destroy (FL : in out File_Location);
   --  Deallocate the memory used by the given File_Location.

   procedure Destroy (FL : in out File_Location_List);
   --  Deallocate the memory used by the given File_Location_List.

   procedure Destroy (ER : in out E_Reference);
   --  Destroy the memory used by the given E_Reference.

   procedure Destroy (ERL : in out E_Reference_List);
   --  Deallocate recursively the given E_Reference_List.

   procedure Destroy (ED : in out E_Declaration);
   --  Deallocate recursively the data contained in the E_Declaration.

   procedure Destroy (EDI : in out E_Declaration_Info);
   --  Deallocate recursively the data contained in the E_Declaration_Info.

   procedure Destroy (EDIL : in out E_Declaration_Info_List);
   --  Deallocate recursively the given E_Declaration_List.

   procedure Destroy (FI : in out File_Info);
   --  Deallocate recursively the data contained in the File_Info.

   procedure Destroy (FIP : in out File_Info_Ptr);
   --  Deallocate recursively the File_Info_Ptr. Do nothing if FIP is null.

   procedure Destroy (FIPL : in out File_Info_Ptr_List);
   --  Deallocate recursively the File_Info_Ptr_List. Do nothing if FIPL is
   --  null.

   procedure Destroy (DFI : in out Dependency_File_Info);
   --  Deallocate recursively the data contained in the Dependency_File_Info.

   procedure Destroy (DFIL : in out Dependency_File_Info_List);
   --  Deallocate recursively the given Dependency_File_Info_List.

   procedure Destroy (LIFNP : in out LI_File_Node_Ptr);
   --  Deallocated the given LI_File_Node_Ptr (and all objects allocated
   --  by this structure).
   --
   --  Note that only the Node pointed by LIFNP is deallocated, and not
   --  all the following ones.

   function Copy (SF : Source_File) return Source_File;
   --  Return a deep-copy of the given Source_File. The copy should be
   --  deallocated after use.

end Src_Info;
