-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------
with Ada.Text_IO;       use Ada.Text_IO;
with GNAT.Regpat;       use GNAT.Regpat;

with Prj;
with Prj_API;
with Src_Info;          use Src_Info;
with Src_Info.LI_Utils; use Src_Info.LI_Utils;

with DB_API;            use DB_API;

with SN;                use SN;
with SN.DB_Structures;  use SN.DB_Structures;
with SN.Find_Fns;       use SN.Find_Fns;
with SN.Browse;
with HTables;

with File_Buffer;
with Ada.Unchecked_Deallocation;

package body Src_Info.CPP is

   type SN_Table_Array is array (Table_Type) of DB_File;

   ----------------
   --  SN_Table  --
   ----------------

   SN_Table : SN_Table_Array;

   ----------------------
   --  Global_LI_File  --
   ----------------------

   Global_LI_File : LI_File_Ptr;
   Global_LI_File_List : LI_File_List;

   procedure Free is new Ada.Unchecked_Deallocation
         (String, SN.String_Access);

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

   ------------------------
   -- Global_CPP_Handler --
   ------------------------

   Global_CPP_Handler : constant CPP_LI_Handler :=
     new CPP_LI_Handler_Record;

   --------------------
   -- Symbol_Handler --
   --------------------

   type Symbol_Handler is access procedure (Sym : FIL_Table);

   procedure Sym_Default_Handler (Sym : FIL_Table);
   procedure Sym_GV_Handler      (Sym : FIL_Table);
   procedure Sym_FD_Handler      (Sym : FIL_Table);
   procedure Sym_FU_Handler      (Sym : FIL_Table);
   procedure Sym_E_Handler       (Sym : FIL_Table);
   procedure Sym_EC_Handler      (Sym : FIL_Table);
   procedure Sym_T_Handler       (Sym : FIL_Table);
   procedure Sym_CL_Handler      (Sym : FIL_Table);
   procedure Sym_UN_Handler      (Sym : FIL_Table);
   procedure Sym_IV_Handler      (Sym : FIL_Table);
   procedure Sym_IU_Handler      (Sym : FIL_Table);
   procedure Sym_MA_Handler      (Sym : FIL_Table);

   ---------------------
   -- Symbol_Handlers --
   ---------------------

   Symbol_Handlers : array (Symbol_Type) of Symbol_Handler :=
     (GV     => Sym_GV_Handler'Access,
      FD     => Sym_FD_Handler'Access,
      FU     => Sym_FU_Handler'Access,
      E      => Sym_E_Handler'Access,
      EC     => Sym_EC_Handler'Access,
      T      => Sym_T_Handler'Access,
      CL     => Sym_CL_Handler'Access,
      UN     => Sym_UN_Handler'Access,
      MA     => Sym_MA_Handler'Access,
      IV     => Sym_IV_Handler'Access,
      IU     => Sym_IU_Handler'Access,
      others => Sym_Default_Handler'Access);

   ------------------
   --  To_Handler  --
   ------------------

   type To_Handler is access procedure (Ref : TO_Table);

   procedure Fu_To_Gv_Handler (Ref : TO_Table);

   -------------------
   --  To_Handlers  --
   -------------------

   Fu_To_Handlers : array (Symbol_Type) of To_Handler :=
     (GV     => Fu_To_Gv_Handler'Access,
      others => null);

   function Ext (S : String) return String;
   --  Used to fill Table_Type_To_Ext array

   ---------
   -- Ext --
   ---------

   function Ext (S : String) return String is
      R : String (1 .. 3) := ASCII.NUL & ASCII.NUL & ASCII.NUL;
   begin
      R (S'First .. S'Last) := S;
      return R;
   end Ext;
   pragma Inline (Ext);

   -----------------------
   -- Table_Type_To_Ext --
   -----------------------

   Table_Type_To_Ext : array (Table_Type) of String (1 .. 3) :=
     (FIL    => Ext ("fil"),
      F      => Ext ("f"),
      FD     => Ext ("fd"),
      FU     => Ext ("fu"),
      T      => Ext ("t"),
      CL     => Ext ("cl"),
      GV     => Ext ("gv"),
      E      => Ext ("e"),
      EC     => Ext ("ec"),
      TO     => Ext ("to"),
      IV     => Ext ("iv"),
      SN_IN  => Ext ("in"),
      UN     => Ext ("un"),
      others => Ext (""));

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

   procedure Open_DB_Files
     (DB_Prefix : in String);

   procedure Close_DB_Files;

   procedure Process_File
     (Source_Filename : in String);

   -----------------------
   -- CType_Description --
   -----------------------

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
   --  Parent_xxx: For typedefs Parent_xxx is the location of the
   --  last type definition in the chain.
   --  For enum, class definitions it is the location of the
   --  declaration itself.
   --  For builtin types this location is empty (invalid)
   --
   --  Ancestor_xxx: location of the closest typedef in the chain of
   --  typedefs. Used only for typedefs.
   --
   --  Builtin_Name is set for builtin type occurences. Also it is used
   --  (compared with null) in resolving typedef parent type (see
   --  Sym_T_Handler).
   --
   --  Is_Typedef is True if type is declared via typedef clause, False
   --  otherwise.

   procedure Free (Desc : in out CType_Description);
   --  Frees memory used by access fields in given Desc structure.

   --  Debugging utils
   procedure Info (Msg : String); -- print info message
   procedure Warn (Msg : String); -- print warning message
   procedure Fail (Msg : String); -- print error message
   pragma Inline (Info, Warn, Fail);

   -------------------
   -- Open_DB_Files --
   -------------------

   procedure Open_DB_Files
     (DB_Prefix : in String)
   is
   begin
      for Table in Table_Type loop
         if Table_Type_To_Ext (Table)(1) /= ASCII.NUL then
            declare
               File_Name : String :=
                 DB_Prefix & "." & Table_Type_To_Ext (Table);
            begin
               Open (SN_Table (Table), File_Name);
            exception
               when others =>
               --  could not open table, ignore this error
                  Warn (Table_Type'Image (Table)
                        & " table does not exist");
            end;
         end if;
      end loop;
   end Open_DB_Files;


   --------------------
   -- Close_DB_Files --
   --------------------

   procedure Close_DB_Files is
   begin
      for Table in Table_Type loop
         begin
            Close (SN_Table (Table));
         exception
            when DB_Close_Error => null; -- ignore it
         end;
      end loop;
   end Close_DB_Files;


   ------------------
   -- Process_File --
   ------------------

   procedure Process_File
     (Source_Filename : in String)
   is
      P : Pair_Ptr;
   begin
      File_Buffer.Init (Source_Filename);
      Set_Cursor (SN_Table (FIL),
                  Position => By_Key,
                  Key => Source_Filename & Field_Sep,
                  Exact_Match => False);

      loop -- iterate thru all symbols for specified file
         P := Get_Pair (SN_Table (FIL), Next_By_Key);
         exit when P = null;

         declare
            Sym : FIL_Table := Parse_Pair (P.all);
         begin
            --  apply corresponding symbol handler
            Symbol_Handlers (Sym.Symbol)(Sym);
            Free (Sym);
         end;

         Free (P);
      end loop;
      File_Buffer.Done;
      Free (Module_Typedefs);
   end Process_File;

   ---------------------------
   -- Create_Or_Complete_LI --
   ---------------------------

   procedure Create_Or_Complete_LI
     (Handler                : access CPP_LI_Handler_Record;
      File                   : in out LI_File_Ptr;
      Source_Filename        : String;
      List                   : in out LI_File_List;
      Project                : Prj.Project_Id;
      Predefined_Source_Path : String;
      Predefined_Object_Path : String)
   is
      pragma Unreferenced (Handler);
      pragma Unreferenced (Predefined_Source_Path);
      pragma Unreferenced (Predefined_Object_Path);

      SN_Dir      : String :=
        Prj_API.Object_Path (Project, Recursive => False)
        & Browse.DB_Dir_Name;
      --  SN project directory

   begin
      --  run cbrowser
      Browse.Browse (Source_Filename, SN_Dir, "cbrowser");

      --  update .to and .by tables
      Browse.Generate_Xrefs (SN_Dir);

      Open_DB_Files
        (SN_Dir & Directory_Separator & Browse.DB_File_Name);

      Global_LI_File := File;
      Global_LI_File_List := List;

      Process_File (Source_Filename);

      List := Global_LI_File_List;
      File := Global_LI_File;

      Global_LI_File := No_LI_File;

      Close_DB_Files;
   end Create_Or_Complete_LI;

   ------------------------------
   -- Parse_All_LI_Information --
   ------------------------------

   procedure Parse_All_LI_Information
     (Handler                : access CPP_LI_Handler_Record;
      List                   : in out LI_File_List;
      In_Directory           : String;
      Project                : Prj.Project_Id;
      Predefined_Source_Path : String;
      Predefined_Object_Path : String)
   is
      pragma Unreferenced (Handler);
      pragma Unreferenced (Predefined_Source_Path);
      pragma Unreferenced (Predefined_Object_Path);
      pragma Unreferenced (List);
      pragma Unreferenced (In_Directory);
      pragma Unreferenced (Project);
   begin
      null;
   end Parse_All_LI_Information;

   ----------------------------------
   -- Case_Insensitive_Identifiers --
   ----------------------------------

   function Case_Insensitive_Identifiers
         (Handler : access CPP_LI_Handler_Record) return Boolean
   is
      pragma Unreferenced (Handler);
   begin
      return False;
   end Case_Insensitive_Identifiers;

   -----------------------------
   -- LI_Filename_From_Source --
   -----------------------------

   function LI_Filename_From_Source
     (Handler                : access CPP_LI_Handler_Record;
      Source_Filename        : String;
      Project                : Prj.Project_Id;
      Predefined_Source_Path : String)
      return String
   is
      pragma Unreferenced (Handler);
      pragma Unreferenced (Predefined_Source_Path);
      pragma Unreferenced (Project);
   begin
      return Source_Filename;
   end LI_Filename_From_Source;

   procedure Builtin_Type_To_Kind
     (Type_Name : in String;
      Desc : out CType_Description; Success : out Boolean);
   --  Attempts to convert string into E_Kind assuming that the string
   --  is a builtin C type. If conversion fails returns False

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

   procedure Type_Name_To_Kind
     (Type_Name : in String; Desc : out CType_Description;
      Success : out Boolean);
   --  Attempts to convert type name into E_Kind.
   --  At the moment searches up for
   --  the name in the class, typedef, enum tables.

   Function_Type_Pat : constant Pattern_Matcher
                     := Compile ("\(([^\)]+)\)\s*\(\)\s*$");
   --  Regexp used to cut functional type definition
   --  example: int (*[]) ()
   --                ^^^ this is cut

   Template_Type_Pat : constant Pattern_Matcher
                     := Compile ("^([^<\s]+)\s*<");
   --  Regexp to find plain class name in the templatized
   --  name.

   procedure Original_Type
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
         --  Info ("volatile ");
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
         --  Info ("const ");
         Type_Name_To_Kind (Type_Name
            (Type_Name'First + Const_Str'Length .. Type_Name'Last),
            Desc, Success);
         Desc.IsConst := True;
         return;
      end if;

      --  first try builtin type
      Builtin_Type_To_Kind (Type_Name, Desc, Success);
      if Success then
         --  Info ("builtin type");
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
         --  Info ("functional ");
         Type_Name_To_Kind (
            Type_Name (Matches (1).First ..  Matches (1).Last),
            Desc, Success);
         return;
      end if;

      if Type_Name (Type_Name'Last) = ']' then
         --  array
         Success      := True;
         Desc.Kind    := Array_Type;
         --  Info ("pointer");
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
            Enum_Def := Find (SN_Table (E), Type_Name);
            Desc.Kind := Enumeration_Type;
            Desc.Parent_Point    := Enum_Def.Start_Position;
            Desc.Parent_Filename := new String' (Enum_Def.Buffer (
                    Enum_Def.File_Name.First .. Enum_Def.File_Name.Last));

            if Desc.Ancestor_Point = Invalid_Point then -- was not set yet
               Desc.Ancestor_Point    := Enum_Def.Start_Position;
               Desc.Ancestor_Filename := new String' (Enum_Def.Buffer (
                     Enum_Def.File_Name.First .. Enum_Def.File_Name.Last));
            end if;

            Free (Enum_Def);
            Success := True;
            return;
         exception
            when  DB_Error |   -- non-existent table
                  Not_Found => -- missed, fall thru'
               null;
         end;
      end if;

      --  when everything else failed
      Success := False;
   end Type_Name_To_Kind;

   -------------------
   -- Original_Type --
   -------------------

   procedure Original_Type
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

      --  typedef found, it is time to set Is_Typedef
      Desc.Is_Typedef := True;

      --  lookup left side of the typedef in our type
      --  hash table
      Seek_Key  := new String'
         (Typedef.Buffer (Typedef.Original.First .. Typedef.Original.Last));
      HTTypedef := Get (Module_Typedefs, Seek_Key);
      Free (Seek_Key);

      if HTTypedef.State = Incomplete then -- loop detected
         Desc.Kind := Unresolved_Entity;

         --  Set parent type to ancestor type
         Desc.Parent_Point := Desc.Ancestor_Point;
         if Desc.Ancestor_Filename /= null then
            --  we need a copy here
            Desc.Parent_Filename := new String' (Desc.Ancestor_Filename.all);
         end if;

         Success   := True;
         Free (Typedef);
         return;
      end if;

      Type_Name_To_Kind (Typedef.Buffer (
              Typedef.Original.First .. Typedef.Original.Last),
              Desc, Success);

      if Desc.Ancestor_Point = Invalid_Point then -- was not set yet
         Desc.Ancestor_Point    := Typedef.Start_Position;
         Desc.Ancestor_Filename := new String' (Typedef.Buffer (
                       Typedef.File_Name.First .. Typedef.File_Name.Last));
      end if;

      if Success then
         --  parent type found (E_Kind is resolved)
         Desc.Parent_Point     := Typedef.Start_Position;
         Desc.Parent_Filename := new String'(Typedef.Buffer (
                    Typedef.File_Name.First .. Typedef.File_Name.Last));

         Free (Typedef);
         Success := True;
         Set (Module_Typedefs, Key, (Key, Complete));
         return;
      end if;

      --  original type not found
      if Desc.Is_Typedef then
         --  but typedef clause present
         Desc.Kind := Unresolved_Entity;
         Success := True;
      end if;

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
      when  DB_Error | -- non-existent table
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
      when  DB_Error | -- non-existent table
            Not_Found => -- missed, fall thru'
         null;
   end Find_Union;

   ----------
   -- Free --
   ----------
   procedure Free (Desc : in out CType_Description) is
   begin
      if Desc.Parent_Point /= Invalid_Point then
         Free (Desc.Parent_Filename);
      end if;
      if Desc.Ancestor_Point /= Invalid_Point then
         Free (Desc.Ancestor_Filename);
      end if;
      if Desc.Builtin_Name /= null then
         Free (Desc.Builtin_Name);
      end if;
   end Free;

   procedure Info (Msg : String) is
   begin
      Put_Line ("[I] " & Msg);
   end Info;

   procedure Warn (Msg : String) is
   begin
      Put_Line ("[W] " & Msg);
   end Warn;

   procedure Fail (Msg : String) is
   begin
      Put_Line ("[E] " & Msg);
   end Fail;

   procedure Refer_Type
     (Type_Name          : in String;
      Type_Decl          : in Point;
      Reference_Filename : in String;
      Reference_Point    : in Point);
   --  Adds reference object into Global_LI_File if
   --  type Type_Name already exists in the tree.
   --
   --  Type_Name, Type_Decl - name and position of
   --  the type declared in the Global_LI_File
   --  Reference_Filename and Reference_Point are
   --  location that refers to the type

   ----------------
   -- Refer_Type --
   ----------------
   procedure Refer_Type
     (Type_Name          : in String;
      Type_Decl          : in Point;
      Reference_Filename : in String;
      Reference_Point    : in Point) is
      Type_Decl_Info     : E_Declaration_Info_List;
   begin
      Type_Decl_Info := Find_Declaration
        (Global_LI_File, Type_Name, "", Type_Decl);

      Insert_Reference
        (Declaration_Info     => Type_Decl_Info,
         File                 => Global_LI_File,
         Source_Filename      => Reference_Filename,
         Location             => Reference_Point,
         Kind                 => Reference);
   exception
      when Declaration_Not_Found => -- ignore
         null;
   end Refer_Type;


   --------------
   -- Handlers --
   --------------

   procedure Sym_Default_Handler (Sym : FIL_Table) is separate;
   procedure Sym_GV_Handler      (Sym : FIL_Table) is separate;
   procedure Sym_FD_Handler      (Sym : FIL_Table) is separate;
   procedure Sym_FU_Handler      (Sym : FIL_Table) is separate;
   procedure Sym_E_Handler       (Sym : FIL_Table) is separate;
   procedure Sym_EC_Handler      (Sym : FIL_Table) is separate;
   procedure Sym_T_Handler       (Sym : FIL_Table) is separate;
   procedure Sym_CL_Handler      (Sym : FIL_Table) is separate;
   procedure Sym_MA_Handler      (Sym : FIL_Table) is separate;
   procedure Sym_IV_Handler      (Sym : FIL_Table) is separate;
   procedure Sym_UN_Handler      (Sym : FIL_Table) is separate;
   procedure Sym_IU_Handler      (Sym : FIL_Table) is separate;

   procedure Fu_To_Gv_Handler    (Ref : TO_Table) is separate;

end Src_Info.CPP;

