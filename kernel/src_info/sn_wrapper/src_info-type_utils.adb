with GNAT.Regpat; use GNAT.Regpat;
with HTables;

package body Src_Info.Type_Utils is

   Function_Type_Pat : constant Pattern_Matcher
                     := Compile ("\(([^\)]+)\)\s*\(\)\s*$");
   --  Regexp used to cut functional type definition
   --  example: int (*[]) ()
   --                ^^^ this is cut

   Template_Type_Pat : constant Pattern_Matcher
                     := Compile ("^([^<\s]+)\s*<");
   --  Regexp to find plain class name in the templatized
   --  name.

   --------------------------------
   -- Type hash table defintions --
   --------------------------------

   Type_Table_Size : constant := 113;

   type String_Hash_Table_Range is range 1 .. Type_Table_Size;

   function Type_Hash_Function (Key : SN.String_Access)
      return String_Hash_Table_Range;
   --  string hash function wrapper

   ------------------------
   -- Type_Hash_Function --
   ------------------------

   function Type_Hash_Function (Key : SN.String_Access)
      return String_Hash_Table_Range
   is
      function String_Hash_Function is
         new HTables.Hash (String_Hash_Table_Range);
      pragma Inline (String_Hash_Function);
   begin
      return String_Hash_Function (Key.all);
   end Type_Hash_Function;

   function Type_Equal_Function (K1, K2 : SN.String_Access)
      return Boolean;

   function Type_Equal_Function (K1, K2 : SN.String_Access)
      return Boolean
   is
   begin
      return K1.all = K2.all;
   end Type_Equal_Function;

   type Type_Parse_State is (Incomplete, Complete, Unknown);

   type Typedef_Entry is
      record
         Key   : SN.String_Access;
         State : Type_Parse_State;
      end record;

   package String_Hash_Table is new HTables.Simple_HTable
     (Header_Num => String_Hash_Table_Range,
      Element    => Typedef_Entry,
      Key        => SN.String_Access,
      No_Element => (null, Unknown),
      Hash       => Type_Hash_Function,
      Equal      => Type_Equal_Function);

   use  String_Hash_Table;

   Module_Typedefs : HTable;
   --  global table for types defined in the file
   --  key = type name
   --  data = whether this type has been successfully identified

   procedure Free (HT : in out HTable);
   --  Frees Module_Typedefs

   ----------
   -- Free --
   ----------

   procedure Free (HT : in out HTable) is
      Elmt : Typedef_Entry;
      Key  : SN.String_Access;
   begin
      Get_First (HT, Elmt);
      loop
         exit when Elmt.Key = null;
         Key := Elmt.Key;
         Remove (HT, Key);
         Free (Key);
         Get_Next (HT, Elmt);
      end loop;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Desc : in out CType_Description) is
   begin
      if Desc.Parent_Point /= Invalid_Point then
         Free_String (Desc.Parent_Filename);
      end if;
      if Desc.Ancestor_Point /= Invalid_Point then
         Free_String (Desc.Ancestor_Filename);
      end if;
      if Desc.Builtin_Name /= null then
         Free_String (Desc.Builtin_Name);
      end if;
   end Free;

   --------------------------
   -- Builtin_Type_To_Kind --
   --------------------------

   procedure Builtin_Type_To_Kind
     (Type_Name : in String;
      Desc      : out CType_Description;
      Success   : out Boolean) is
   begin
      if Type_Name = "char"          or Type_Name = "signed char"
         or Type_Name = "int"        or Type_Name = "signed int"
         or Type_Name = "long"       or Type_Name = "signed long"
         or Type_Name = "long long"  or Type_Name = "signed long long"
         or Type_Name = "short"      or Type_Name = "signed short"
      then
         Desc.Kind := Signed_Integer_Type;
         Desc.Builtin_Name := new String' (Type_Name);
         Success := True;
      elsif Type_Name = "unsigned char"
         or Type_Name = "unsigned int"
         or Type_Name = "unsigned long"
         or Type_Name = "unsigned long long"
         or Type_Name = "unsigned short"
      then
         Desc.Kind         := Modular_Integer_Type;
         Desc.Builtin_Name := new String' (Type_Name);
         Success           := True;
      else
         Success := False;
      end if;
   end Builtin_Type_To_Kind;

   -----------------------
   -- Type_Name_To_Kind --
   -----------------------

   procedure Type_Name_To_Kind
     (Type_Name : in String;
      Desc      : out CType_Description;
      Success   : out Boolean)
   is
      Matches      : Match_Array (0 .. 1);
      Volatile_Str : constant String := "volatile ";
      Const_Str    : constant String := "const ";
   begin
      Success             := False;

      --  check for leading volatile/const modifier
      if Type_Name'Length > Volatile_Str'Length
        and then Type_Name (Type_Name'First ..
                     Type_Name'First + Volatile_Str'Length - 1)
                     = Volatile_Str
      then -- volatile modifier
         Type_Name_To_Kind (Type_Name
            (Type_Name'First + Volatile_Str'Length .. Type_Name'Last),
            Desc, Success);
         Desc.IsVolatile := True;
         return;
      end if;

      if Type_Name'Length > Const_Str'Length
         and then Type_Name (Type_Name'First ..
                     Type_Name'First + Const_Str'Length - 1)
                     = Const_Str
      then -- const modifier
         Type_Name_To_Kind (Type_Name
            (Type_Name'First + Const_Str'Length .. Type_Name'Last),
            Desc, Success);
         Desc.IsConst := True;
         return;
      end if;

      --  first try builtin type
      Builtin_Type_To_Kind (Type_Name, Desc, Success);
      if Success then
         if Desc.Ancestor_Point = Invalid_Point then -- was not set yet
            Desc.Ancestor_Point := Predefined_Point;
         end if;
         return;
      end if;

      if Type_Name (Type_Name'Last) = '*'
         or Type_Name (Type_Name'Last) = '&' then
         Success      := True;
         Desc.Kind    := Access_Type;
         return;
      end if;

      if Type_Name (Type_Name'Last) = ')' then
         --  function pointer?
         Match (Function_Type_Pat, Type_Name, Matches);
         if Matches (0) = No_Match then
            Success := False;
            return;
         end if;
         Type_Name_To_Kind (
            Type_Name (Matches (1).First ..  Matches (1).Last),
            Desc, Success);
         return;
      end if;

      if Type_Name (Type_Name'Last) = ']' then
         --  array
         Success      := True;
         Desc.Kind    := Array_Type;
         return;
      end if;

      --  look in typedefs
      Original_Type (Type_Name, Desc, Success);
      if Success then -- original type found
         return;
      end if;

      --  look in classes
      if Is_Open (SN_Table (CL)) then
         declare
            Class_Def  : CL_Table;
         begin
            Find_Class (Type_Name, Desc, Class_Def, Success);
            if Success then
               Free (Class_Def);
               return;
            end if;
         end;
      end if;

      --  look in unions
      if Is_Open (SN_Table (UN)) then
         declare
            Union_Def  : UN_Table;
         begin
            Find_Union (Type_Name, Desc, Union_Def, Success);
            if Success then
               Free (Union_Def);
               return;
            end if;
         end;
      end if;

      --  look in enums
      if Is_Open (SN_Table (E)) then
         declare
            Enum_Def : E_Table;
         begin
            Find_Enum (Type_Name, Desc, Enum_Def, Success);
            if Success then
               Free (Enum_Def);
               return;
            end if;
         end;
      end if;

      --  when everything else failed
      Success := False;
   end Type_Name_To_Kind;

   ------------------------
   -- Find_Original_Type --
   ------------------------

   procedure Find_Original_Type
     (Type_Name : String;
      Desc      : out CType_Description;
      Success   : out Boolean)
   is
      Typedef   : T_Table;
      HTTypedef : Typedef_Entry;
      Key       : SN.String_Access;
      Seek_Key  : SN.String_Access;
   begin

      Success := False;

      if not Is_Open (SN_Table (T)) then
         --  typedef table does not exist
         return;
      end if;

      Typedef   := Find (SN_Table (T), Type_Name);

      Key := new String'(Type_Name);
      Set (Module_Typedefs, Key, (Key, Incomplete));
      --  add this type as an unidentified one

      --  lookup left side of the typedef in our type
      --  hash table
      Seek_Key  := new String'
         (Typedef.Buffer (Typedef.Original.First .. Typedef.Original.Last));
      HTTypedef := Get (Module_Typedefs, Seek_Key);
      Free (Seek_Key);

      if Desc.Is_Typedef = True
         and then Desc.Ancestor_Point = Invalid_Point
      then -- was not set yet
         Desc.Ancestor_Point    := Typedef.Start_Position;
         Desc.Ancestor_Filename := new String' (Typedef.Buffer (
                       Typedef.File_Name.First .. Typedef.File_Name.Last));
      end if;

      Desc.Is_Typedef := True;

      if HTTypedef.State = Incomplete then -- loop detected
         Desc.Kind := Unresolved_Entity;
         if Desc.Parent_Point = Invalid_Point then
            Desc.Parent_Point    := Typedef.Start_Position;
            Desc.Parent_Filename := new String' (Typedef.Buffer (
                       Typedef.File_Name.First .. Typedef.File_Name.Last));
         end if;
         Success   := True;
         Free (Typedef);
         return;
      end if;

      Type_Name_To_Kind (Typedef.Buffer (
              Typedef.Original.First .. Typedef.Original.Last),
              Desc, Success);

      if Success then
         --  parent type found (E_Kind is resolved)
         Desc.Parent_Point    := Typedef.Start_Position;
         Desc.Parent_Filename := new String'(Typedef.Buffer (
                    Typedef.File_Name.First .. Typedef.File_Name.Last));
         Free (Typedef);
         Success := True;
         Set (Module_Typedefs, Key, (Key, Complete));
         return;
      end if;

      --  original type not found, but typedef clause present
      Desc.Kind := Unresolved_Entity;
      Success := True;

      Free (Typedef);

   exception
      when  DB_Error |   -- non-existent table
            Not_Found => -- missed, fall thru'
         null;
   end Original_Type;

   ----------------
   -- Find_Class --
   ----------------

   procedure Find_Class
     (Type_Name : in String;
      Desc      : in out CType_Description;
      Class_Def : out CL_Table;
      Success   : out Boolean)
   is
      Matches      : Match_Array (0 .. 1);
   begin
      Success := False;
      Match (Template_Type_Pat, Type_Name, Matches);
      if Matches (0) /= No_Match then
         Class_Def := Find (SN_Table (CL), Type_Name
            (Matches (1).First .. Matches (1).Last));
         Desc.IsTemplate := True;
      else
         Class_Def := Find (SN_Table (CL), Type_Name);
      end if;

      Desc.Parent_Point    := Class_Def.Start_Position;
      Desc.Parent_Filename := new String' (Class_Def.Buffer (
                    Class_Def.File_Name.First .. Class_Def.File_Name.Last));

      if Desc.Ancestor_Point = Invalid_Point then -- was not set yet
         Desc.Ancestor_Point    := Class_Def.Start_Position;
         Desc.Ancestor_Filename := new String' (Class_Def.Buffer (
                       Class_Def.File_Name.First .. Class_Def.File_Name.Last));
      end if;

      Desc.Kind := Record_Type;
      Success := True;
   exception
      when  DB_Error |   -- non-existent table
            Not_Found => -- missed, fall thru'
         null;
   end Find_Class;

   ----------------
   -- Find_Union --
   ----------------

   procedure Find_Union
     (Type_Name : in String;
      Desc      : in out CType_Description;
      Union_Def : out UN_Table;
      Success   : out Boolean)
   is
      Matches      : Match_Array (0 .. 1);
   begin
      Success := False;
      Match (Template_Type_Pat, Type_Name, Matches);
      if Matches (0) /= No_Match then
         Union_Def := Find (SN_Table (UN), Type_Name
            (Matches (1).First .. Matches (1).Last));
         Desc.IsTemplate := True;
      else
         Union_Def := Find (SN_Table (UN), Type_Name);
      end if;

      Desc.Parent_Point    := Union_Def.Start_Position;
      Desc.Parent_Filename := new String' (Union_Def.Buffer (
                    Union_Def.File_Name.First .. Union_Def.File_Name.Last));

      if Desc.Ancestor_Point = Invalid_Point then -- was not set yet
         Desc.Ancestor_Point    := Union_Def.Start_Position;
         Desc.Ancestor_Filename := new String' (Union_Def.Buffer (
                       Union_Def.File_Name.First .. Union_Def.File_Name.Last));
      end if;

      Desc.Kind := Record_Type;
      Success := True;
   exception
      when  DB_Error |   -- non-existent table
            Not_Found => -- missed, fall thru'
         null;
   end Find_Union;

   ---------------
   -- Find_Enum --
   ---------------

   procedure Find_Enum
     (Type_Name : in String;
      Desc      : in out CType_Description;
      Enum_Def  : out E_Table;
      Success   : out Boolean)
   is
      Matches      : Match_Array (0 .. 1);
   begin
      Success := False;
      Match (Template_Type_Pat, Type_Name, Matches);
      if Matches (0) /= No_Match then
         Enum_Def := Find (SN_Table (E), Type_Name
            (Matches (1).First .. Matches (1).Last));
         Desc.IsTemplate := True;
      else
         Enum_Def := Find (SN_Table (E), Type_Name);
      end if;

      Desc.Parent_Point    := Enum_Def.Start_Position;
      Desc.Parent_Filename := new String'
        (Enum_Def.Buffer
          (Enum_Def.File_Name.First .. Enum_Def.File_Name.Last));

      if Desc.Ancestor_Point = Invalid_Point then -- was not set yet
         Desc.Ancestor_Point    := Enum_Def.Start_Position;
         Desc.Ancestor_Filename := new String'
           (Enum_Def.Buffer
             (Enum_Def.File_Name.First .. Enum_Def.File_Name.Last));
      end if;

      Desc.Kind := Enumeration_Type;
      Success := True;
   exception
      when  DB_Error |   -- non-existent table
            Not_Found => -- missed, fall thru'
         null;
   end Find_Enum;

   -------------------
   -- Cmp_Arg_Types --
   -------------------

   function Cmp_Arg_Types
     (Buffer_A, Buffer_B     : SN.String_Access;
      Args_A, Args_B         : DB_Structures.Segment_Vector.Node_Access)
      return Boolean
   is
      use DB_Structures.Segment_Vector;
      Ptr_A : Segment_Vector.Node_Access := Args_A;
      Ptr_B : Segment_Vector.Node_Access := Args_B;
   begin
      if (Ptr_A = null and then Ptr_B /= null and then Ptr_B.Next = null
         and then Buffer_B (Ptr_B.Data.First .. Ptr_B.Data.Last) = "void")
         or else (Ptr_B = null and then Ptr_A /= null
         and then Ptr_A.Next = null
         and then Buffer_A (Ptr_A.Data.First .. Ptr_A.Data.Last) = "void")
      then
         return True;
      end if;
      while Ptr_A /= null and then Ptr_B /= null loop
         if Buffer_A (Ptr_A.Data.First .. Ptr_A.Data.Last)
            /= Buffer_B (Ptr_B.Data.First .. Ptr_B.Data.Last) then
            return False;
         end if;
         Ptr_A := Ptr_A.Next;
         Ptr_B := Ptr_B.Next;
      end loop;

      return Ptr_A = null and then Ptr_B = null;
   end Cmp_Arg_Types;

   --------------------
   -- Cmp_Prototypes --
   --------------------

   function Cmp_Prototypes
     (Buffer_A, Buffer_B     : SN.String_Access;
      Args_A, Args_B         : DB_Structures.Segment_Vector.Node_Access;
      Ret_Type_A, Ret_Type_B : Segment)
      return Boolean
   is
   begin
      return Cmp_Arg_Types (Buffer_A, Buffer_B, Args_A, Args_B)
         and then Buffer_A (Ret_Type_A.First .. Ret_Type_A.Last)
            = Buffer_B (Ret_Type_B.First .. Ret_Type_B.Last);
   end Cmp_Prototypes;

end Src_Info.Type_Utils;
