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
with Src_Info;             use Src_Info;
with Src_Info.LI_Utils;    use Src_Info.LI_Utils;
with Src_Info.Type_Utils;  use Src_Info.Type_Utils;

with DB_API;            use DB_API;

with SN;                use SN;
with SN.DB_Structures;  use SN.DB_Structures;
with SN.Find_Fns;       use SN.Find_Fns;
with SN.Browse;

with File_Buffer;

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
   procedure Sym_CON_Handler     (Sym : FIL_Table);
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
   procedure Sym_MD_Handler      (Sym : FIL_Table);

   ---------------------
   -- Symbol_Handlers --
   ---------------------

   Symbol_Handlers : array (Symbol_Type) of Symbol_Handler :=
     (GV     => Sym_GV_Handler'Access,
      CON    => Sym_CON_Handler'Access,
      FD     => Sym_FD_Handler'Access,
      FU     => Sym_FU_Handler'Access,
      E      => Sym_E_Handler'Access,
      EC     => Sym_EC_Handler'Access,
      T      => Sym_T_Handler'Access,
      CL     => Sym_CL_Handler'Access,
      UN     => Sym_UN_Handler'Access,
      MA     => Sym_MA_Handler'Access,
      MI     => Sym_FU_Handler'Access,
      MD     => Sym_MD_Handler'Access,
      IV     => Sym_IV_Handler'Access,
      IU     => Sym_IU_Handler'Access,
      others => Sym_Default_Handler'Access);

   ------------------
   --  To_Handler  --
   ------------------

   type To_Handler is access procedure (Ref : TO_Table);

   procedure Fu_To_Gv_Handler  (Ref : TO_Table);
   procedure Fu_To_Fu_Handler  (Ref : TO_Table);
   procedure Fu_To_Con_Handler (Ref : TO_Table);
   procedure Fu_To_E_Handler   (Ref : TO_Table);
   procedure Fu_To_Ec_Handler  (Ref : TO_Table);
   procedure Fu_To_Ma_Handler  (Ref : TO_Table);
   procedure Fu_To_Mi_Handler  (Ref : TO_Table);
   procedure Fu_To_Cl_Handler  (Ref : TO_Table);
   procedure Fu_To_T_Handler   (Ref : TO_Table);
   procedure Fu_To_Un_Handler  (Ref : TO_Table);

   -------------------
   --  To_Handlers  --
   -------------------

   Fu_To_Handlers : array (Symbol_Type) of To_Handler :=
     (GV     => Fu_To_Gv_Handler'Access,
      FU     => Fu_To_Fu_Handler'Access,
      CON    => Fu_To_Con_Handler'Access,
      E      => Fu_To_E_Handler'Access,
      EC     => Fu_To_Ec_Handler'Access,
      MA     => Fu_To_Ma_Handler'Access,
      MI     => Fu_To_Mi_Handler'Access,
      CL     => Fu_To_Cl_Handler'Access,
      T      => Fu_To_T_Handler'Access,
      UN     => Fu_To_Un_Handler'Access,
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
      MI     => Ext ("mi"),
      MD     => Ext ("md"),
      SN_IN  => Ext ("in"),
      UN     => Ext ("un"),
      MA     => Ext ("ma"),
      CON    => Ext ("con"),
      others => Ext (""));

   procedure Open_DB_Files
     (DB_Prefix : in String);

   procedure Close_DB_Files;

   procedure Process_File
     (Source_Filename : in String);

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
         exception
            when others =>
               Free (Sym);
               raise;
         end;

         Free (P);
      end loop;
      File_Buffer.Done;
      Free (Module_Typedefs);
   exception
      when others   => -- unexpected exception
         Free (P);
         File_Buffer.Done;
         Free (Module_Typedefs);
         --  ??? Here we probably want to report the unexpected exception
         --  and continue to work further, but currently we reraise that
         --  exception
         raise;
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
      Reference_Point    : in Point;
      Kind               : in Reference_Kind := Reference);
   --  Adds reference object into Global_LI_File if
   --  type Type_Name already exists in the tree.
   --
   --  Type_Name, Type_Decl - name and position of
   --  the type declared in the Global_LI_File
   --  Reference_Filename and Reference_Point are
   --  location that refers to the type
   --
   --  Kind is a kind of a reference.

   ----------------
   -- Refer_Type --
   ----------------
   procedure Refer_Type
     (Type_Name          : in String;
      Type_Decl          : in Point;
      Reference_Filename : in String;
      Reference_Point    : in Point;
      Kind               : in Reference_Kind := Reference)
   is
      Type_Decl_Info     : E_Declaration_Info_List;
   begin
      Type_Decl_Info := Find_Declaration
        (File         => Global_LI_File,
         Symbol_Name  => Type_Name,
         Location     => Type_Decl);

      Insert_Reference
        (Declaration_Info     => Type_Decl_Info,
         File                 => Global_LI_File,
         Source_Filename      => Reference_Filename,
         Location             => Reference_Point,
         Kind                 => Kind);
   exception
      when Declaration_Not_Found => -- ignore
         null;
   end Refer_Type;

   function Find_First_Forward_Declaration
     (Buffer       : SN.String_Access;
      Name         : Segment;
      Filename     : Segment;
      Return_Type  : Segment;
      Arg_Types    : Segment_Vector.Node_Access)
      return E_Declaration_Info_List;
   --  Attempts to find the first forward declaration
   --  for the function. Returns null if not found or
   --  forward declaration is in another file which
   --  has not been yet processed

   function Find_First_Forward_Declaration
     (Buffer       : SN.String_Access;
      Class_Name   : Segment;
      Name         : Segment;
      Filename     : Segment;
      Return_Type  : Segment;
      Arg_Types    : Segment_Vector.Node_Access)
      return E_Declaration_Info_List;
   --  Attempts to find the first forward declaration
   --  for the method. Returns null if not found or
   --  forward declaration is in another file which
   --  has not been yet processed

   ------------------------------------
   -- Find_First_Forward_Declaration --
   ------------------------------------
   function Find_First_Forward_Declaration
     (Buffer       : SN.String_Access;
      Class_Name   : Segment;
      Name         : Segment;
      Filename     : Segment;
      Return_Type  : Segment;
      Arg_Types    : Segment_Vector.Node_Access)
      return E_Declaration_Info_List
   is
      P            : Pair_Ptr;
      MD_Tab       : MD_Table;
      MD_Tab_Tmp   : MD_Table;
      First_MD_Pos : Point := Invalid_Point;
   begin

      if not Is_Open (SN_Table (MD)) then
         return null; -- .md table does not exist
      end if;

      --  First we have to find the first forward declaration
      --  that corresponds to our method, that is prototypes
      --  should be the same
      Set_Cursor
        (SN_Table (MD),
         By_Key,
         Buffer (Class_Name.First .. Class_Name.Last) & Field_Sep
            & Buffer (Name.First .. Name.Last) & Field_Sep,
         False);

      loop
         P := Get_Pair (SN_Table (MD), Next_By_Key);
         if P = null then -- no fwd decls at all
            return null;
         end if;
         MD_Tab := Parse_Pair (P.all);
         Free (P);
         --  Update position of the first forward declaration
         exit when (MD_Tab.Buffer (MD_Tab.File_Name.First ..
                                   MD_Tab.File_Name.Last)
            = Buffer (Filename.First .. Filename.Last))
            and then Cmp_Prototypes
              (MD_Tab.Buffer,
               Buffer,
               MD_Tab.Arg_Types,
               Arg_Types,
               MD_Tab.Return_Type,
               Return_Type);
         Free (MD_Tab);
      end loop;

      --  now find the first declaration in the file
      Set_Cursor
        (SN_Table (MD),
         By_Key,
         Buffer (Class_Name.First .. Class_Name.Last) & Field_Sep
            & Buffer (Name.First .. Name.Last) & Field_Sep,
         False);

      loop
         P := Get_Pair (SN_Table (MD), Next_By_Key);
         exit when P = null;
         MD_Tab_Tmp := Parse_Pair (P.all);
         Free (P);
         --  Update position of the first forward declaration
         if MD_Tab.Buffer (MD_Tab.File_Name.First .. MD_Tab.File_Name.Last)
            = MD_Tab_Tmp.Buffer (MD_Tab_Tmp.File_Name.First ..
                                 MD_Tab_Tmp.File_Name.Last)
            and then Cmp_Prototypes
              (MD_Tab.Buffer,
               MD_Tab_Tmp.Buffer,
               MD_Tab.Arg_Types,
               MD_Tab_Tmp.Arg_Types,
               MD_Tab.Return_Type,
               MD_Tab_Tmp.Return_Type)
            and then ((First_MD_Pos = Invalid_Point)
            or else MD_Tab_Tmp.Start_Position < First_MD_Pos) then
            First_MD_Pos := MD_Tab_Tmp.Start_Position;
         end if;
         Free (MD_Tab_Tmp);
      end loop;

      pragma Assert (First_MD_Pos /= Invalid_Point); -- DB inconsistency?
      return Find_Declaration
        (File        => Global_LI_File,
         Symbol_Name => Buffer (Name.First .. Name.Last),
         Class_Name  => Buffer (Class_Name.First .. Class_Name.Last),
         Location    => First_MD_Pos);

   exception
      when DB_Error | Declaration_Not_Found =>
         return null;
   end Find_First_Forward_Declaration;

   ------------------------------------
   -- Find_First_Forward_Declaration --
   ------------------------------------
   function Find_First_Forward_Declaration
     (Buffer       : SN.String_Access;
      Name         : Segment;
      Filename     : Segment;
      Return_Type  : Segment;
      Arg_Types    : Segment_Vector.Node_Access)
      return E_Declaration_Info_List
   is
      P            : Pair_Ptr;
      FD_Tab       : FD_Table;
      FD_Tab_Tmp   : FD_Table;
      First_FD_Pos : Point := Invalid_Point;
      Match        : Boolean;
   begin

      if not Is_Open (SN_Table (FD)) then
         return null; -- .md table does not exist
      end if;

      --  First we have to find the first forward declaration
      --  that corresponds to our method, that is prototypes
      --  should be the same.
      --  Prototype is searched only in the same file, no
      --  attempts to link .h and .cpp files are undertaken
      Set_Cursor
        (SN_Table (FD),
         By_Key,
         Buffer (Name.First .. Name.Last) & Field_Sep,
         False);

      loop
         P := Get_Pair (SN_Table (FD), Next_By_Key);
         if P = null then -- no fwd decls at all
            return null;
         end if;
         FD_Tab := Parse_Pair (P.all);
         Free (P);
         --  Update position of the first forward declaration
         exit when (FD_Tab.Buffer (FD_Tab.File_Name.First ..
                                 FD_Tab.File_Name.Last)
               = Buffer (Filename.First .. Filename.Last))
            and then Cmp_Prototypes
              (FD_Tab.Buffer,
               Buffer,
               FD_Tab.Arg_Types,
               Arg_Types,
               FD_Tab.Return_Type,
               Return_Type);
         Free (FD_Tab);
      end loop;

      --  now find the first declaration in the file
      Set_Cursor
        (SN_Table (FD),
         By_Key,
         Buffer (Name.First .. Name.Last) & Field_Sep,
         False);

      loop
         P := Get_Pair (SN_Table (FD), Next_By_Key);
         exit when P = null;
         FD_Tab_Tmp := Parse_Pair (P.all);
         Free (P);
         --  Update position of the first forward declaration
         Match := FD_Tab.Buffer (FD_Tab.File_Name.First ..
                                 FD_Tab.File_Name.Last)
                = FD_Tab_Tmp.Buffer (FD_Tab_Tmp.File_Name.First ..
                                     FD_Tab_Tmp.File_Name.Last);
         Match := Match and then Cmp_Prototypes
           (FD_Tab.Buffer,
            FD_Tab_Tmp.Buffer,
            FD_Tab.Arg_Types,
            FD_Tab_Tmp.Arg_Types,
            FD_Tab.Return_Type,
            FD_Tab_Tmp.Return_Type);

         if (Match and then First_FD_Pos = Invalid_Point)
            or else FD_Tab_Tmp.Start_Position < First_FD_Pos then
            First_FD_Pos := FD_Tab_Tmp.Start_Position;
         end if;
         Free (FD_Tab_Tmp);
      end loop;

      pragma Assert (First_FD_Pos /= Invalid_Point); -- DB inconsistency?
      return Find_Declaration
        (File        => Global_LI_File,
         Symbol_Name => Buffer (Name.First .. Name.Last),
         Location    => First_FD_Pos);

   exception
      when DB_Error | Declaration_Not_Found =>
         return null;
   end Find_First_Forward_Declaration;

   --------------
   -- Handlers --
   --------------

   procedure Sym_Default_Handler (Sym : FIL_Table) is separate;
   procedure Sym_GV_Handler      (Sym : FIL_Table) is separate;
   procedure Sym_CON_Handler     (Sym : FIL_Table) is separate;
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
   procedure Sym_MD_Handler      (Sym : FIL_Table) is separate;

   procedure Fu_To_Gv_Handler    (Ref : TO_Table) is separate;
   procedure Fu_To_Fu_Handler    (Ref : TO_Table) is separate;
   procedure Fu_To_Con_Handler   (Ref : TO_Table) is separate;
   procedure Fu_To_E_Handler     (Ref : TO_Table) is separate;
   procedure Fu_To_Ec_Handler    (Ref : TO_Table) is separate;
   procedure Fu_To_Ma_Handler    (Ref : TO_Table) is separate;
   procedure Fu_To_Mi_Handler    (Ref : TO_Table) is separate;
   procedure Fu_To_Cl_Handler    (Ref : TO_Table) is separate;
   procedure Fu_To_T_Handler     (Ref : TO_Table) is separate;
   procedure Fu_To_Un_Handler    (Ref : TO_Table) is separate;

   ---------
   -- Add --
   ---------
   procedure Add
     (HT      : in out LI_File_List;
      LIFP    : LI_File_Ptr;
      Success : out Boolean) is
   begin
      Add (HT.Table, LIFP, Success);
   end Add;

   ------------------------------
   --  Convert_Filename_To_LI  --
   ------------------------------

   function Convert_Filename_To_LI (Filename : String) return String is
   begin
      return Src_Info.LI_Utils.Convert_Filename_To_LI (Filename);
   end Convert_Filename_To_LI;

end Src_Info.CPP;

