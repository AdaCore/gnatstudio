with SN; use SN;
with SN.DB_Structures; use SN.DB_Structures;

private package Src_Info.Type_Utils is

   --  This package is private because it is using some declarations
   --  which are in the private section of Src_Info. This package is
   --  specific to C and C++ types.

   type CType_Description is
      record
         Kind              : E_Kind;
         IsVolatile        : Boolean := False;
         IsConst           : Boolean := False;
         IsTemplate        : Boolean := False;
         Parent_Point      : Point   := Invalid_Point;
         Parent_Filename   : SN.String_Access;
         Ancestor_Point    : Point   := Invalid_Point;
         Ancestor_Filename : SN.String_Access;
         Builtin_Name      : SN.String_Access;
         Is_Typedef        : Boolean := False;
      end record;
   --  Contains C type description. Used in these procedures:
   --    Type_Name_To_Kind
   --    Original_Type
   --    Find_Class
   --
   --  Parent_xxx: the same as Ancestor_xxx (see below) but used for
   --  all other entities except typedefs. For builtin types Parent_Point is
   --  undefined (Invalid_Point).
   --
   --  Ancestor_xxx: location of the closest typedef in the chain of
   --  typedefs. Used only for typedefs. For builtin types Ancestor_Point is
   --  Predefined_Point
   --
   --  Builtin_Name is set for builtin type occurences. Also it is used
   --  (compared with null) in resolving typedef parent type (see
   --  Sym_T_Handler).
   --
   --  Is_Typedef is True if type is declared via typedef clause, False
   --  otherwise.

   type Type_To_Object_Array is array (E_Kind) of E_Kind;
   --  type for array that maps E_Kind type entities into
   --  object entities

   Type_To_Object : Type_To_Object_Array :=
     (Access_Type               => Access_Object,
      Array_Type                => Array_Object,
      Boolean_Type              => Boolean_Object,
      Class_Wide_Type           => Class_Wide_Object,
      Decimal_Fixed_Point_Type  => Decimal_Fixed_Point_Object,
      Enumeration_Type          => Enumeration_Object,
      Modular_Integer_Type      => Modular_Integer_Object,
      Protected_Type            => Protected_Object,
      Record_Type               => Record_Object,
      Ordinary_Fixed_Point_Type => Ordinary_Fixed_Point_Object,
      Signed_Integer_Type       => Signed_Integer_Object,
      String_Type               => String_Object,
      Task_Type                 => Task_Object,
      Private_Type              => Private_Type,      -- ??? what kind
      Unresolved_Entity         => Unresolved_Entity, -- ??? for object?
      others                    => Overloaded_Entity);
   --  This array establishes relation between E_Kind type entities
   --  and object entities

   procedure Builtin_Type_To_Kind
     (Type_Name : in String;
      Desc      : out CType_Description;
      Success   : out Boolean);
   --  Attempts to convert string into E_Kind assuming that the string
   --  is a builtin C type. If conversion fails returns False

   procedure Type_Name_To_Kind
     (Type_Name : in String;
      Desc      : out CType_Description;
      Success   : out Boolean);
   --  Attempts to convert type name into E_Kind.
   --  At the moment searches up for
   --  the name in the class, typedef, enum tables.

   procedure Find_Original_Type
     (Type_Name : String;
      Desc      : out CType_Description;
      Success   : out Boolean);
   --  Gets E_Kind of original type for specified typedef type.
   --  Sets Success to True if type found and fills Desc structure
   --  with appropriate information.
   --  Also supports Unresolved_Entity for such situations:
   --    typedef old_type new_type;
   --  where old_type is unresolved type.

   procedure Find_Class
     (Type_Name : in String;
      Desc      : in out CType_Description;
      Class_Def : out CL_Table;
      Success   : out Boolean);
   --  Finds class and stores information about it in the
   --  Desc and Class_Def arguments
   --  Success returns error status

   procedure Find_Union
     (Type_Name : in String;
      Desc      : in out CType_Description;
      Union_Def : out UN_Table;
      Success   : out Boolean);
   --  Finds union and stores information about it in the
   --  Desc and Union_Def arguments
   --  Success returns error status

   procedure Find_Enum
     (Type_Name : in String;
      Desc      : in out CType_Description;
      Enum_Def  : out E_Table;
      Success   : out Boolean);
   --  Finds enum and stores information about it in the
   --  Desc and Enum_Def arguments
   --  Success returns error status

   procedure Free (Desc : in out CType_Description);
   --  Frees memory used by access fields in given Desc structure.

   function Cmp_Arg_Types
     (Buffer_A, Buffer_B     : SN.String_Access;
      Args_A, Args_B         : DB_Structures.Segment_Vector.Node_Access)
      return Boolean;
   --  checks to see if argument types are the same

   function Cmp_Prototypes
     (Buffer_A, Buffer_B     : SN.String_Access;
      Args_A, Args_B         : DB_Structures.Segment_Vector.Node_Access;
      Ret_Type_A, Ret_Type_B : Segment)
      return Boolean;
   --  checks to see if function prototypes are the same

end Src_Info.Type_Utils;
