with HTables;
with Types;
with GNAT.OS_Lib; use GNAT.OS_Lib;

package Src_Info is

   type LI_File_Ptr is private;
   --  A handle to a structure containing all the semantic information
   --  concerning a given unit.

   type LI_File_List is private;
   --  A list of LI_File_Ptr.

   procedure Reset (LIFL : in out LI_File_List);
   --  Reset the given list of LI_File_Ptr and deallocate all the
   --  memory allocated for this list. The new list becomes empty.

private

   --  In the following declarations, some abbreviations have been used
   --  to reduce the length the type names:
   --    - Entity       -> E
   --    - Information  -> Info
   --    - Library      -> Lib
   --    - Library_Info -> LI

   type E_Kind is
     (Access_Object,
      Access_Type,
      Array_Object,
      Array_Type,
      Boolean_Object,
      Boolean_Type,
      Class_Wide_Object,
      Class_Wide_Type,
      Decimal_Fixed_Point_Object,
      Decimal_Fixed_Point_Type,
      Entry_Or_Entry_Family,
      Enumeration_Literal,
      Enumeration_Object,
      Enumeration_Type,
      Exception_Entity,
      Floating_Point_Object,
      Floating_Point_Type,
      Generic_Function_Or_Operator,
      Generic_Package,
      Generic_Procedure,
      Label_On_Block,
      Label_On_Loop,
      Label_On_Statement,
      Modular_Integer_Object,
      Modular_Integer_Type,
      Named_Number,
      Non_Generic_Function_Or_Operator,
      Non_Generic_Package,
      Non_Generic_Procedure,
      Ordinary_Fixed_Point_Object,
      Ordinary_Fixed_Point_Type,
      Private_Type,
      --  ??? Not sure yet if this type will remain or not. Normally, the kind
      --  ??? for private types is set to the type of the parent, but this
      --  ??? is not transitive. There is an ongoing discussion on 9119-006
      --  ??? where it is suggested to make this rule transitive, in which
      --  ??? case this enum value should be removed.
      Protected_Object,
      Protected_Type,
      Record_Object,
      Record_Type,
      Signed_Integer_Object,
      Signed_Integer_Type,
      String_Object,
      String_Type,
      Task_Object,
      Task_Type);
   --  The entity kind (sorted by alphabeticall order).
   --
   --  Note that Boolean is treated in a special way: it is treated as
   --  Boolean_Type/Object, rather than as an Enumeration_Type/Object.

   type Reference_Kind is
     (Reference,
      Modification,
      Body_Entity,
      Completion_Of_Private_Or_Incomplete_Type,
      Type_Extension,
      Implicit,
      End_Of_Spec,
      End_Of_Spec_With_Label,
      End_Of_Body,
      End_Of_Body_With_Label);
   --  The kind of reference to an entity. They have the following meaning:
   --    - Reference: The entity is used
   --    - Modification: The value of the entity is changed
   --    - Body_Entity: Used for spec entities that are repeated in a body,
   --      including the unit name itself, and the formals in the case of
   --      a subprogram. Also used for entry-names in accept statements.
   --    - Completion_Of_Private_Or_Incomplete_Type: Used to mark the
   --      completion of a private type or incomplete type
   --    - type_Extension: Used to mark the reference as the entity from
   --      which a tagged type is extended.
   --    - Implicit: Used to identify a reference to the entity in a generic
   --      actual or in a default in a call.
   --    - End_Of_Spec: Used to identify the end of the following constructs.
   --      Block statement, loop statement, package specification, task
   --      definition, protected definition, record definition.
   --    - End_Of_Spec_With_Label: Identical to End_Of_Spec except that a
   --      label is attached to the contruct.
   --    - End_Of_Body: Used to identify the end of the following constructs.
   --      Subprogram body, package body, task body, entry body, protected
   --      body, accept statement.
   --    - End_Of_Body_With_Label: Identical to End_Of_Body, except that a
   --      label is attached to the construct.

   type Unit_Part is (Unit_Spec, Unit_Body, Unit_Separate);
   --  A unit is usally composed of two parts: the spec and the body.
   --    - Unit_Spec represents package/subprogram/generic declarations
   --    - Unit_Body represents package/subprogram/generic bodies and subunits.

   type E_Scope is (Global_Scope, Local_Scope);
   --  The scope of an entity. The values have the following meaning:
   --     - Global_Entity: publicly visible entity in a top level library.
   --     - Local_Entity: an entity that does not satisfy the conditions
   --       to be a Global_Entity.

   type LI_File;
   type LI_File_Ptr is access LI_File;

   type Source_File is record
      LI        : LI_File_Ptr;
      Part      : Unit_Part;
      Unit_Name : String_Access;
      --  Allocated only when Part is set to Unit_Separate. Set to null
      --  otherwise.
   end record;
   --  A source file is represented by two or three elements:
   --    - its LI_File
   --    - its unit part
   --    - its unit name when it is a separate

   No_Source_File : constant Source_File :=
     (LI        => null,
      Part      => Unit_Spec,
      Unit_Name => null);
   --  To check that a Source_File is not null, a quick and good enough
   --  check is to verify that Source_File.LI is not null.

   function "=" (Left, Right : Source_File) return Boolean;
   --  A redefined equality function that compares the Unit_Name values, not
   --  the access value.

   type File_Location is record
      File   : Source_File;
      Line   : Positive;
      Column : Positive;
   end record;
   --  A location in a source file.

   Null_File_Location : constant File_Location :=
     (File => No_Source_File,
      Line => 1,
      Column => 1);
   --  To verify that a File_Location is not null, a quick and good enough
   --  check is to verify that File_Location.File.LI is not null.
   --  See function Is_File_Location which performs this check.

   function "=" (Left, Right : File_Location) return Boolean;
   --  A redefined equality function that compares uses the redefined equality
   --  for the source file field.

   type E_Reference is record
      Location : File_Location;
      Kind     : Reference_Kind;
   end record;
   --  A reference to an entity.

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
      Location        : File_Location;
      Kind            : E_Kind;
      Parent_Location : File_Location;
      Parent_Kind     : E_Kind;
      Scope           : E_Scope;
      End_Of_Scope    : E_Reference;
   end record;
   --  All the information about an entity declaration.
   --  ??? Note that, in order to save a little bit of memory space,
   --  ??? End_Of_Scope could defined as a pointer to E_Reference instead of
   --  ??? as an E_Reference structure, because most entities do not have end
   --  ??? of scope info (because this is not relevant for these entities).
   --  ??? This has not been done yet because it adds a little bit of
   --  ??? complexity to the implementation (in terms of memory management).

   type E_Declaration_Info is record
      Declaration : E_Declaration;
      References  : E_Reference_List;
   end record;
   --  All the information associated to a given entity declaration, which
   --  is the information about the declaration itself, and the references
   --  to this declaration.

   type E_Declaration_Info_Node;
   type E_Declaration_Info_List is access E_Declaration_Info_Node;
   type E_Declaration_Info_Node is record
      Value : E_Declaration_Info;
      Next  : E_Declaration_Info_List;
   end record;
   --  E_Declaration_Info_List is a chained list of E_Declaration_Info.
   --  E_Declaration_Info_Node is a node of this list.

   type File_Info is record
      Unit_Name         : String_Access;
      Source_Filename   : String_Access;
      File_Timestamp    : Types.Time_Stamp_Type;
      Original_Filename : String_Access;
      Original_Line     : Positive;
      Declarations      : E_Declaration_Info_List;
   end record;
   --  The information associated to a source file.
   --  ??? It is possible to optimize a bit the memory usage by allocating
   --  ??? the Source_Filename only when the naming of the unit is not
   --  ??? following the standard naming scheme. The Source_Filename could
   --  ??? then be computed from the unit name. This is not done for the
   --  ??? moment because we would like to avoid language specific notions
   --  ??? in the data structures defined

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

   type Dependency_File_Info is record
      File              : Source_File;
      File_Timestamp    : Types.Time_Stamp_Type;
      Depends_From_Spec : Boolean;
      Depends_From_Body : Boolean;
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

   type LI_File (Parsed : Boolean := False) is record
      LI_Filename   : String_Access;
      Spec_Info     : File_Info_Ptr;
      Body_Info     : File_Info_Ptr;
      Separate_Info : File_Info_Ptr_List;
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

   function Get_LI_Filename (E : LI_File_Node_Ptr) return String_Access;
   pragma Inline (Get_LI_Filename);
   --  return the filename of the LI_File pointed by E.

   function Hash (F : String_Access) return LI_File_HTable_Index;
   pragma Inline (Hash);
   --  Hash function.

   function Equal (F1, F2 : String_Access) return Boolean;
   pragma Inline (Equal);
   --  Return True if F1.all = F2.all.

   package LI_File_HTable is
     new HTables.Static_HTable
       (Header_Num => LI_File_HTable_Index,
        Element => LI_File_Node,
        Elmt_Ptr => LI_File_Node_Ptr,
        Null_Ptr => null,
        Set_Next => Set_Next,
        Next => Next,
        Key => String_Access,
        Get_Key => Get_LI_Filename,
        Hash => Hash,
        Equal => Equal);
   --  A hash-table of LI_File_Ptr objects. There will always be at most
   --  one element per key (that is one unit per unit name).
   --
   --  Using the Simple_HTable would simplify a bit the handling of this
   --  hash-table. This option has not been taken because it would then cause
   --  the duplication of the key (the unit name): the key is already embedded
   --  in the Node_Info structure. This causes a little bit of code duplication
   --  (we almost rewrite HTables.Simple_HTable, ie 120 SLOCs), but potentially
   --  saves a fair bit of memory.

   type LI_File_List is record
      Table : LI_File_HTable.HTable;
   end record;
   --  The list of LI_File is implemented as a hash-table rather than
   --  a plain chained list to improve the lookup performances.

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
     (HT      : in out LI_File_HTable.HTable;
      LIFP    : LI_File_Ptr;
      Success : out Boolean);
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
   --  Return a pointer to the LI_File which filename is LI_Filename.
   --  Return null if no such LI_File is found.

   procedure Get_First
     (HT : in out LI_File_HTable.HTable; Result : out LI_File_Ptr);
   --  Returns null if the hash-table is empty, otherwise returns one
   --  non specified LI_File_Ptr. There is no guarantee that 2 calls to
   --  this function will return the same element.

   procedure Get_Next
     (HT : in out LI_File_HTable.HTable; Result : out LI_File_Ptr);
   --  Returns a non-specified LI_File_Ptr that has not been returned
   --  by the same function since the last call to Get_First or null if
   --  there is no such element. If there is no call to 'Set' in between
   --  Get_Next calls, all the elements of the Htable will be traversed.

   ---------------------------
   -- Non-exported services --
   ---------------------------

   --  this section defines a set of services that will help in the management
   --  of a list of LI_File. They are defined as private so that only childs
   --  of this package (which are expected to provide high-level services that
   --  hide the manipulation of such a list) have access to them. All other
   --  units will rely on the services provided by these child package to
   --  manipulate these lists.

   function Is_File_Location (Location : in File_Location) return Boolean;
   pragma Inline (Is_File_Location);
   --  Returns True if the given file location value has been set. It is
   --  faster than comparing the Location against Null_File_Location.

   function Get_Source_Filename (File : Source_File) return String;
   --  Returns the source filename of the given file.
   --  Note that this function is merely a shortcut to
   --       File.Unit.Spec/Body/Separate_Info.Source_Filename.all
   --  and does not perform any check before accessing these fields. The
   --  caller should make sure that the information is accessible before
   --  invoking this function.

   function Get_File_Info (SF : Source_File) return File_Info_Ptr;
   --  Return an access to the File_Info associated to the given Source_File.

   procedure Destroy (LIF : in out LI_File);
   --  Deallocate recursively the data contained in the given LI_File.

   procedure Destroy (LIFP : in out LI_File_Ptr);
   --  Deallocate recursively the LI_File_Ptr. Has no effect if LIFP is null.

   procedure Destroy (SF : in out Source_File);
   --  Deallocate the memory used by the given Source_File.

   procedure Destroy (FL : in out File_Location);
   --  Deallocate the memory used by the given File_Location.

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

