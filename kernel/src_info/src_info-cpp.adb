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
     SN.Browse,
     Ada.Text_IO,
     DB_API;

use  SN,
     SN.DB_Structures,
     Ada.Text_IO,
     DB_API;

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
       others => Ext (""));


   Symbol_Handlers : array (Symbol_Type) of Symbol_Handler :=
      (others   => Sym_Default_Handler'Access);


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
      pragma Unreferenced (Project);
      pragma Unreferenced (Predefined_Object_Path);
      SN_Table : SN_Table_Array;
      SN_Dir   : String := Prj_API.Object_Path
        (Project, Recursive => False) & Browse.DB_Dir_Name;
   begin
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
      pragma Unreferenced (Sym);
      pragma Unreferenced (File);
      pragma Unreferenced (SN_Table);
   begin
      null;
   end Sym_Default_Handler;
end Src_Info.CPP;
