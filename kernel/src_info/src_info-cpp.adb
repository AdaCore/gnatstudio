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
   procedure Sym_FU_Handler      (Sym : FIL_Table);
   procedure Sym_E_Handler       (Sym : FIL_Table);
   procedure Sym_EC_Handler      (Sym : FIL_Table);
   procedure Sym_T_Handler       (Sym : FIL_Table);
   procedure Sym_CL_Handler      (Sym : FIL_Table);
   procedure Sym_IV_Handler      (Sym : FIL_Table);
   procedure Sym_MA_Handler      (Sym : FIL_Table);

   ---------------------
   -- Symbol_Handlers --
   ---------------------

   Symbol_Handlers : array (Symbol_Type) of Symbol_Handler :=
     (GV     => Sym_GV_Handler'Access,
      FU     => Sym_FU_Handler'Access,
      E      => Sym_E_Handler'Access,
      EC     => Sym_EC_Handler'Access,
      T      => Sym_T_Handler'Access,
      CL     => Sym_CL_Handler'Access,
      MA     => Sym_MA_Handler'Access,
      IV     => Sym_IV_Handler'Access,
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
      FU     => Ext ("fu"),
      T      => Ext ("t"),
      CL     => Ext ("cl"),
      GV     => Ext ("gv"),
      E      => Ext ("e"),
      EC     => Ext ("ec"),
      TO     => Ext ("to"),
      IV     => Ext ("iv"),
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
         Kind            : E_Kind;
         IsVolatile      : Boolean;
         IsConst         : Boolean;
         IsTemplate      : Boolean;
         Parent_Point    : Point;
         Parent_Filename : SN.String_Access;
      end record;

   procedure Free (Desc : in out CType_Description);
   --  Frees Parent_Filename if any

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
      Desc : out CType_Description; Success : out Boolean) is
   begin
      if Type_Name = "char"          or Type_Name = "signed char"
         or Type_Name = "int"        or Type_Name = "signed int"
         or Type_Name = "long"       or Type_Name = "signed long"
         or Type_Name = "long long"  or Type_Name = "signed long long"
         or Type_Name = "short"      or Type_Name = "signed short"
      then
         Desc.Kind := Signed_Integer_Type;
         Success := True;
      elsif Type_Name = "unsigned char"
         or Type_Name = "unsigned int"
         or Type_Name = "unsigned long"
         or Type_Name = "unsigned long long"
         or Type_Name = "unsigned short"
      then
         Desc.Kind := Modular_Integer_Type;
         Success := True;
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
                     := Compile ("^([^<\s]+)\s*<(.*)>$");
   --  Regexp to find plain class name in the templatized
   --  name.

   procedure Original_Type
     (Type_Name : String;
      Desc      : out CType_Description;
      Success   : out Boolean);
   --  Gets E_Kind of original type for specified typedef type.
   --  Sets Success to True if type found and fills Desc structure
   --  with appropriate information.

   procedure Find_Class
     (Type_Name : in String;
      Desc      : in out CType_Description;
      Class_Def : out CL_Table;
      Success   : out Boolean);
   --  Finds class and stores information about it in the
   --  Desc and Class_Def arguments
   --  Success returns error status

   -----------------------
   -- Type_Name_To_Kind --
   -----------------------

   procedure Type_Name_To_Kind
     (Type_Name : in String;
      Desc      : out CType_Description;
      Success   : out Boolean)
   is
      Matches      : Match_Array (1 .. 1);
      Volatile_Str : constant String := "volatile ";
      Const_Str    : constant String := "const ";
   begin
      Desc.IsVolatile   := False;
      Desc.IsConst      := False;
      Desc.IsTemplate   := False;
      Desc.Parent_Point := Invalid_Point;
      Success           := False;

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

      if Type_Name (Type_Name'Last) = '*' then
         Success      := True;
         Desc.Kind    := Access_Type;
         return;
      end if;

      if Type_Name (Type_Name'Last) = ')' then
         --  function pointer?
         Match (Function_Type_Pat, Type_Name, Matches);
         if Matches (1) = No_Match then
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

      --  look in enums
      if Is_Open (SN_Table (E)) then
         declare
            Enum_Def : E_Table;
         begin
            Enum_Def := Find (SN_Table (E), Type_Name);
            Desc.Kind := Enumeration_Type;
            Desc.Parent_Point    := Enum_Def.Start_Position;
            Desc.Parent_Filename := new String'(Enum_Def.Buffer (
                    Enum_Def.File_Name.First .. Enum_Def.File_Name.Last));
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
   begin

      Success := False;

      if not Is_Open (SN_Table (T)) then
         --  typedef table does not exist
         return;
      end if;

      Typedef   := Find (SN_Table (T), Type_Name);
      Type_Name_To_Kind (Typedef.Buffer (
              Typedef.Original.First .. Typedef.Original.Last),
              Desc, Success);
      if Success then
         Desc.Parent_Point     := Typedef.Start_Position;
         Desc.Parent_Filename := new String'(Typedef.Buffer (
                    Typedef.File_Name.First .. Typedef.File_Name.Last));
         Free (Typedef);
         Success := True;
         return;
      end if;

      Free (Typedef);

      --  original type not found
      return;

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
      Matches      : Match_Array (1 .. 2);
   begin
      Success := False;
      Match (Template_Type_Pat, Type_Name, Matches);
      if Matches (1) /= No_Match and Matches (2) /= No_Match then
         Class_Def := Find (SN_Table (CL), Type_Name
            (Matches (1).First .. Matches (1).Last));
         Desc.IsTemplate := True;
      else
         Class_Def := Find (SN_Table (CL), Type_Name);
      end if;

      Desc.Parent_Point    := Class_Def.Start_Position;
      Desc.Parent_Filename := new String'(Class_Def.Buffer (
                    Class_Def.File_Name.First .. Class_Def.File_Name.Last));

      Desc.Kind := Record_Type;
      Success := True;
   exception
      when  DB_Error | -- non-existent table
            Not_Found => -- missed, fall thru'
         null;
   end Find_Class;

   ----------
   -- Free --
   ----------
   procedure Free (Desc : in out CType_Description) is
      procedure Free is new Ada.Unchecked_Deallocation
         (String, SN.String_Access);
   begin
      if Desc.Parent_Point /= Invalid_Point then
         Free (Desc.Parent_Filename);
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

   --------------
   -- Handlers --
   --------------

   procedure Sym_Default_Handler (Sym : FIL_Table) is separate;
   procedure Sym_GV_Handler      (Sym : FIL_Table) is separate;
   procedure Sym_FU_Handler      (Sym : FIL_Table) is separate;
   procedure Sym_E_Handler       (Sym : FIL_Table) is separate;
   procedure Sym_EC_Handler      (Sym : FIL_Table) is separate;
   procedure Sym_T_Handler       (Sym : FIL_Table) is separate;
   procedure Sym_CL_Handler      (Sym : FIL_Table) is separate;
   procedure Sym_MA_Handler      (Sym : FIL_Table) is separate;
   procedure Sym_IV_Handler      (Sym : FIL_Table) is separate;

   procedure Fu_To_Gv_Handler    (Ref : TO_Table) is separate;

end Src_Info.CPP;

