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

with Prj;
with Prj_API;

with SN,
     SN.DB_Structures,
     SN.Find_Fns,
     SN.Browse,
     Src_Info,
     Ada.Text_IO,
     DB_API,
     Src_Info.LI_Utils;

use  SN,
     SN.DB_Structures,
     SN.Find_Fns,
     Src_Info,
     Ada.Text_IO,
     DB_API,
     Src_Info.LI_Utils;

package body Src_Info.CPP is
   type SN_Table_Array is array (Table_Type) of DB_File;

   --------------------
   -- Symbol_Handler --
   --------------------
   type Symbol_Handler is access procedure (Sym : FIL_Table;
                           File : in out LI_File_Ptr;
                           SN_Table : in out SN_Table_Array);

   function Ext (S : String) return String;
   procedure Sym_Default_Handler (Sym : FIL_Table; File : in out
                            LI_File_Ptr; SN_Table : in out SN_Table_Array);
   procedure Open_DB_Files (SN_Table : in out SN_Table_Array;
                            DB_Prefix : in String);
   procedure Close_DB_Files (SN_Table : in out SN_Table_Array);
   procedure Process_File (Source_Filename : in String;
                           File : in out LI_File_Ptr;
                           SN_Table : in out SN_Table_Array);

   procedure Sym_GV_Handler (Sym : FIL_Table; File : in out LI_File_Ptr;
                           SN_Table : in out SN_Table_Array);
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
       T      => Ext ("t"),
       CL     => Ext ("cl"),
       GV     => Ext ("gv"),
       others => Ext (""));


   Symbol_Handlers : array (Symbol_Type) of Symbol_Handler :=
      (GV       => Sym_GV_Handler'Access,
       others   => Sym_Default_Handler'Access);

   Global_CPP_Handler : constant CPP_LI_Handler := null;

   -------------------
   -- Open_DB_Files --
   -------------------
   procedure Open_DB_Files (SN_Table : in out SN_Table_Array;
                            DB_Prefix : in String) is
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
   procedure Close_DB_Files (SN_Table : in out SN_Table_Array) is
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
   procedure Process_File (Source_Filename : in String;
                           File : in out LI_File_Ptr;
                           SN_Table : in out SN_Table_Array) is
      P : Pair_Ptr;
   begin
      Set_Cursor
        (SN_Table (FIL),
         Position => By_Key,
         Key => Source_Filename & Field_Sep,
         Exact_Match => False);

      loop -- iterate thru all symbols for specified file
         P := Get_Pair (SN_Table (FIL), Next_By_Key);
         exit when P = null;

         declare
            Sym : FIL_Table := Parse_Pair (P.all);
         begin
            Symbol_Handlers (Sym.Symbol)(Sym, File, SN_Table);
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
      SN_Table    : SN_Table_Array;
      SN_Dir      : String := Prj_API.Object_Path
        (Project, Recursive => False) & Browse.DB_Dir_Name;
   begin
      Browse.Browse (Source_Filename, SN_Dir, "cbrowser");
      Browse.Generate_Xrefs (SN_Dir);
      Open_DB_Files
        (SN_Table,
         SN_Dir & Directory_Separator & Browse.DB_File_Name);

      Process_File (Source_Filename, File, SN_Table);

      Close_DB_Files (SN_Table);
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

   -------------------------
   -- Sym_Default_Handler --
   -------------------------
   procedure Sym_Default_Handler (Sym : FIL_Table;
                           File : in out LI_File_Ptr;
                           SN_Table : in out SN_Table_Array) is
      --  pragma Unreferenced (Sym);
      pragma Unreferenced (File);
      pragma Unreferenced (SN_Table);
   begin
      Put_Line ("Sym_Default_Hanlder ("
         & Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last) & ")");
      null;
   end Sym_Default_Handler;

   procedure Builtin_Type_To_Kind (Type_Name : in String; Kind : out E_Kind;
      Success : out Boolean);
   --  Attempts to convert string into E_Kind assuming that the string
   --  is a builtin C type. If conversion fails returns False

   --------------------------
   -- Builtin_Type_To_Kind --
   --------------------------
   procedure Builtin_Type_To_Kind (Type_Name : in String; Kind : out E_Kind;
      Success : out Boolean) is
   begin
      if Type_Name = "char"          or Type_Name = "signed char"
         or Type_Name = "int"        or Type_Name = "signed int"
         or Type_Name = "long"       or Type_Name = "signed long"
         or Type_Name = "long long"  or Type_Name = "signed long long"
         or Type_Name = "short"      or Type_Name = "signed short" then
         Kind := Signed_Integer_Type;
         Success := True;
      elsif Type_Name = "unsigned char"
         or Type_Name = "unsigned int"
         or Type_Name = "unsigned long"
         or Type_Name = "unsigned long long"
         or Type_Name = "unsigned short" then
         Kind := Modular_Integer_Type;
         Success := True;
      else
         Success := False;
      end if;
   end Builtin_Type_To_Kind;

   procedure Type_Name_To_Kind (SN_Table : in out SN_Table_Array;
      Type_Name : in String; Kind : out E_Kind; Success : out Boolean);
   --  Attempts to convert type name into E_Kind. Searches up for
   --  the name in the class, typedef, etc. tables.

   -----------------------
   -- Type_Name_To_Kind --
   -----------------------
   procedure Type_Name_To_Kind (SN_Table : in out SN_Table_Array;
      Type_Name : in String; Kind : out E_Kind; Success : out Boolean) is
   begin
      --  first try builtin type
      Builtin_Type_To_Kind (Type_Name, Kind, Success);
      if Success then
         return;
      end if;

      if Type_Name (Type_Name'Last) = '*' then
         Success := True;
         Kind    := Access_Type;
      end if;

      if Type_Name (Type_Name'Last) = ')' then
         --  function pointer
         Success := True;
         Kind    := Access_Type;
      end if;

      if Type_Name (Type_Name'Last) = ']' then
         --  array
         Success := True;
         Kind    := Array_Type;
      end if;

      --  look in typedefs
      declare
         Typedef   : T_Table;
      begin
         Typedef   := Find (SN_Table (T), Type_Name);
         Type_Name_To_Kind (SN_Table, Typedef.Buffer (
                 Typedef.Original.First .. Typedef.Original.Last),
                 Kind, Success);
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
         Kind := Record_Type;
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

   --------------------
   -- Sym_GV_Handler --
   --------------------
   procedure Sym_GV_Handler (Sym : FIL_Table;
                           File : in out LI_File_Ptr;
                           SN_Table : in out SN_Table_Array) is
      Kind    : E_Kind;
      Var     : GV_Table;
      Success : Boolean;
      tmp_ptr : E_Declaration_Info_List;
   begin
      Put_Line ("Sym_GV_Handler (" &
                Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last) &
                ")");
      --  Lookup variable type
      Var := Find (SN_Table (GV), Sym.Buffer
         (Sym.Identifier.First .. Sym.Identifier.Last));
      Type_Name_To_Kind (SN_Table, Var.Buffer
         (Var.Value_Type.First .. Var.Value_Type.Last), Kind, Success);
      if not Success then
         Free (Var);
         return; -- type not found, ignore errors
      end if;
      Insert_Declaration (
         Handler => LI_Handler (Global_CPP_Handler),
         File => File,
         Symbol_Name =>
            Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last),
         Source_Filename =>
            Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
         Location => Sym.Start_Position,
         Kind => Kind,
         Scope => Global_Scope,
         Declaration_Info => tmp_ptr
      );
      Free (Var);
   exception
      when  DB_Error |   -- non-existent table
            Not_Found => -- no such variable
         null;           -- ignore error
   end Sym_GV_Handler;
end Src_Info.CPP;

