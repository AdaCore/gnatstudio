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

package body Src_Info.CPP is

   type SN_Table_Array is array (Table_Type) of DB_File;

   procedure Open_DB_Files
     (DB_Prefix : in String);

   procedure Close_DB_Files;

   procedure Process_File
     (Source_Filename : in String);

   --------------------
   -- Symbol_Handler --
   --------------------

   type Symbol_Handler is access procedure (Sym      : FIL_Table);

   procedure Sym_Default_Handler (Sym      : FIL_Table);
   --  This is default handler for symbols, which are not registered
   --  in Symbols_Handlers.

   procedure Sym_GV_Handler (Sym      : FIL_Table);
   procedure Sym_FU_Handler (Sym      : FIL_Table);

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

   type CType_Description is
      record
         Kind         : E_Kind;
         IsVolatile   : Boolean;
         IsConst      : Boolean;
      end record;

   ----------------
   --  SN_Table  --
   ----------------

   SN_Table : SN_Table_Array;

   ----------------------
   --  Global_LI_File  --
   ----------------------

   Global_LI_File : LI_File_Ptr;

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
       others => Ext (""));

   ---------------------
   -- Symbol_Handlers --
   ---------------------

   Symbol_Handlers : array (Symbol_Type) of Symbol_Handler :=
      (GV       => Sym_GV_Handler'Access,
       FU       => Sym_FU_Handler'Access,
       others   => Sym_Default_Handler'Access);

   ------------------------
   -- Global_CPP_Handler --
   ------------------------

   Global_CPP_Handler : constant CPP_LI_Handler :=
     new CPP_LI_Handler_Record;

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
               File_Name : String := DB_Prefix & "." &
                  Table_Type_To_Ext (Table);
            begin
               Open (SN_Table (Table), File_Name);
            exception
               when others =>
                  Put_Line ("Warning: could not open " & "'" &
                            File_Name & "'");
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
      pragma Unreferenced (List);
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
      Process_File (Source_Filename);
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
   --  Attempts to convert type name into E_Kind. Searches up for
   --  the name in the class, typedef, etc. tables.

   Function_Type_Pat : constant Pattern_Matcher
                     := Compile ("\(([^\)]+)\)\s*\(\)\s*$");
   --  Regexp used to cut functional type definition
   --  example: int (*[]) ()
   --                ^^^ this is cut

   -----------------------
   -- Type_Name_To_Kind --
   -----------------------

   procedure Type_Name_To_Kind
     (Type_Name : in String;
      Desc : out CType_Description;
      Success : out Boolean)
   is
      Matches      : Match_Array (1 .. 1);
      Volatile_Str : constant String := "volatile ";
      Const_Str    : constant String := "const ";
   begin
      Desc.IsVolatile := False;
      Desc.IsConst    := False;

      --  check for leading volatile/const modifier
      if Type_Name'Length > Volatile_Str'Length
        and then Type_Name (Type_Name'First ..
                     Type_Name'First + Volatile_Str'Length - 1)
                     = Volatile_Str
      then -- volatile modifier
         --  Put_Line ("volatile ");
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
         --  Put_Line ("const ");
         Type_Name_To_Kind (Type_Name
            (Type_Name'First + Const_Str'Length .. Type_Name'Last),
            Desc, Success);
         Desc.IsConst := True;
         return;
      end if;

      --  first try builtin type
      Builtin_Type_To_Kind (Type_Name, Desc, Success);
      if Success then
         --  Put_Line ("builtin type");
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
         --  Put_Line ("functional ");
         Type_Name_To_Kind (
            Type_Name (Matches (1).First ..  Matches (1).Last),
            Desc, Success);
         return;
      end if;

      if Type_Name (Type_Name'Last) = ']' then
         --  array
         Success      := True;
         Desc.Kind    := Array_Type;
         --  Put_Line ("pointer");
         return;
      end if;

      --  look in typedefs
      declare
         Typedef   : T_Table;
      begin
         Typedef   := Find (SN_Table (T), Type_Name);
         Type_Name_To_Kind (Typedef.Buffer (
                 Typedef.Original.First .. Typedef.Original.Last),
                 Desc, Success);
         if Success then
            Free (Typedef);
            Success := True;
            return;
         end if;
         Free (Typedef);
         --  This is a typedef but base type is not found :(
         Success := False;
         return;
      exception
         when  DB_Error |   -- non-existent table
               Not_Found => -- missed, fall thru'
            null;
      end;

      --  loop in classes
      declare
         Class_Def : CL_Table;
      begin
         Class_Def := Find (SN_Table (CL), Type_Name);
         Free (Class_Def);
         Desc.Kind := Record_Type;
         Success := True;
         return;
      exception
         when  DB_Error |   -- non-existent table
               Not_Found => -- missed, fall thru'
            null;
      end;
      --  when everything else failed
      Success := False;
   end Type_Name_To_Kind;

   -------------------------
   -- Sym_Default_Handler --
   -------------------------

   procedure Sym_Default_Handler
     (Sym : FIL_Table)
   is
      --  pragma Unreferenced (Sym);
   begin
      Put_Line ("Sym_Default_Hanlder ("
        & Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last) & ")");
      null;
   end Sym_Default_Handler;

   --------------------
   -- Sym_GV_Handler --
   --------------------

   procedure Sym_GV_Handler
     (Sym : FIL_Table)
   is
      Desc       : CType_Description;
      Var        : GV_Table;
      Success    : Boolean;
      tmp_ptr    : E_Declaration_Info_List;
      Attributes : SN_Attributes;
      Scope      : E_Scope := Global_Scope;
   begin
      Put_Line ("Sym_GV_Handler ("
                & Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last)
                & ")");

      --  Lookup variable type
      Var := Find (SN_Table (GV), Sym.Buffer
         (Sym.Identifier.First .. Sym.Identifier.Last));
      Type_Name_To_Kind (Var.Buffer
         (Var.Value_Type.First .. Var.Value_Type.Last), Desc, Success);

      if not Success then
         Free (Var);
         return; -- type not found, ignore errors
      end if;

      Attributes := SN_Attributes (Var.Attributes);

      if (Attributes and SN_STATIC) = SN_STATIC then
         Scope := Static_Local;
      end if;

      Insert_Declaration
        (Handler           => LI_Handler (Global_CPP_Handler),
         File              => Global_LI_File,
         Symbol_Name       =>
           Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last),
         Source_Filename   =>
           Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
         Location          => Sym.Start_Position,
         Kind              => Desc.Kind,
         Scope             => Scope,
         Declaration_Info  => tmp_ptr);

      Free (Var);
   exception
      when  DB_Error |   -- non-existent table
            Not_Found => -- no such variable
         null;           -- ignore error
   end Sym_GV_Handler;

   --------------------
   -- Sym_FU_Handler --
   --------------------

   procedure Sym_FU_Handler
     (Sym : FIL_Table)
   is
      tmp_ptr : E_Declaration_Info_List;
   begin

      Put_Line ("Sym_FU_Hanlder ("
        & Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last)
        & ")");

      Insert_Declaration
        (Handler           => LI_Handler (Global_CPP_Handler),
         File              => Global_LI_File,
         Symbol_Name       =>
           Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last),
         Source_Filename   =>
           Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
         Location          => Sym.Start_Position,
         Kind              => Non_Generic_Procedure,
         Scope             => Global_Scope,
         Declaration_Info  => tmp_ptr);

   end Sym_FU_Handler;

   --------------------
   -- Sym_FU_Handler --
   --------------------
--    procedure Sym_FU_Handler (Sym : DB.FIL_Table) is
--       P : Pair_Ptr;
--   S : String := Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last);
--       Class : Unbounded_String := To_Unbounded_String ("#");
--       Comment : Unbounded_String;
--       --  FU_Tab : FU_Table;
--       MDecl  : MD_Table;
--       Sym_Type : String_Access :=
--             new String'(ASCII.NUL & ASCII.NUL & ASCII.NUL);
--       tmp_int : Integer;
--       tmp_ptr : E_Declaration_Info_List;
--    begin
--       --  Handler for FU (function) symbol.
--  --      HTML.Highlight (Sym, HTML.Font_Color_Blue);
--       --  Method implementation specific handling goes here
--       if Sym.Symbol = MI then
--          Comment := To_Unbounded_String (
--                "method " & Sym.Buffer (Sym.Class.First .. Sym.Class.Last)
--                & "::" &
--                Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last));
--          begin
--             MDecl := Find (SN_Table (MD),
--                 Sym.Buffer (Sym.Class.First .. Sym.Class.Last),
--                 Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last));
--             Comment := Make_Method_Comment (MDecl);
--             Insert_Declaration
--             HTML.Insert_Xref (Sym.Highlight_Start_Position,
--                Sym.Highlight_End_Position,
--           MDecl.Buffer (MDecl.File_Name.First .. MDecl.File_Name.Last),
--                MDecl.Start_Position,
--                To_String (Comment));
--             Insert_Declaration
--               (Handler           => LI_Handler (Global_CPP_Handler),
--                File              => File,
--                Symbol_Name       =>
--                  Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last),
--               Source_Filename   =>
--                   Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
--                Location          => Sym.Start_Position,
--                Kind              => Non_Generic_Procedure,
--                Scope             => Global_Scope,
--                Declaration_Info  => tmp_ptr);
--             Free (MDecl);
--          exception
--             when DB_Error | Not_Found =>
--                null;
--          end;
--       else
--          Comment := To_Unbounded_String ("global function definition: " &
--                Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last));
--          HTML.Highlight (Sym, HTML.Title, To_String (Comment));
--       end if;
--
--       tmp_int := 1;
--       To_String (Sym.Symbol, Sym_Type, tmp_int);
--       Set_Cursor
--         (SN_Table (TO),
--          Position => By_Key,
--          Key => To_String (Class) & Field_Sep & S &
--                            Field_Sep &
--                            Sym_Type.all,
--                            Exact_Match => False);
--
--       loop
--          P := Get_Pair (SN_Table (TO), Next_By_Key);
--          exit when P = null;
--
--          declare
--             --  FIXME:
--             --  SN has only line number in .to table. So, to get exact
--             --  position we are getting that line from source file and
--             --  calling corresponding handler for EVERY
--             --  occurrence of that symbol in that line.
--             Ref      : DB.TO_Table := DB.Parse_Pair (P.all);
--
--           Src_Line : String := File_Buffer.Get_Line (Ref.Position.Line);
--             PRef     : DB.TO_Table;  -- Ref with exact position
--             P        : Natural := Src_Line'First; -- index to start from
--             S        : String  :=
--                   Ref.Buffer (Ref.Referred_Symbol_Name.First
--                                     .. Ref.Referred_Symbol_Name.Last);
--             Matches  : Match_Array (0 .. 0);
--             Pat      : Pattern_Matcher := Compile ("\b" & S & "\b");
--          begin
--             loop
--                Match (Pat, Src_Line (P .. Src_Line'Last), Matches);
--                exit when Matches (0) = No_Match or Ref.Symbol = Undef;
--                P := Matches (0).Last + 1;
--                PRef := Ref;
--                --  conversion to column
--              PRef.Position.Column := Matches (0).First - Src_Line'First;
--                Fu_To_Handlers (Ref.Referred_Symbol)(PRef);
--             end loop;
--             Free (Ref);
--          end;
--
--          Free (P);
--       end loop;
--
--    exception
--       when DB_Error => null; -- non-existent table .to
--
--    end Sym_FU_Handler;

end Src_Info.CPP;

