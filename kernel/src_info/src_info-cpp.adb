-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2002                            --
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

with Ada.Exceptions;            use Ada.Exceptions;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Ada.Text_IO;               use Ada.Text_IO;

with Prj;
with Prj_API;              use Prj_API;
with Src_Info;             use Src_Info;
with Src_Info.LI_Utils;    use Src_Info.LI_Utils;
with Src_Info.Prj_Utils;   use Src_Info.Prj_Utils;
with Src_Info.Type_Utils;  use Src_Info.Type_Utils;

with DB_API;            use DB_API;

with SN;                use SN;
with SN.DB_Structures;  use SN.DB_Structures;
with SN.Find_Fns;       use SN.Find_Fns;
with SN.Browse;
with SN.Xref_Pools;     use SN.Xref_Pools;

with Traces;  use Traces;
with String_Utils; use String_Utils;
with Prj_API;
with Snames; use Snames;

package body Src_Info.CPP is

   DBIMP    : constant String := "dbimp";
   --  SN database engine

   CBrowser : constant String := "cbrowser";
   --  SN C and C++ parser

   Info_Stream : constant Debug_Handle := Create ("CPP.Info");
   Warn_Stream : constant Debug_Handle := Create ("CPP.Warn");
   Fail_Stream : constant Debug_Handle := Create ("CPP.Fail");

   --------------------
   -- Symbol_Handler --
   --------------------

   type Symbol_Handler is access procedure
     (Sym              : FIL_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : in out LI_File_List;
      Project_View     : Prj.Project_Id;
      Module_Type_Defs : Module_Typedefs_List);

   procedure Sym_Default_Handler
     (Sym     : FIL_Table;
      Handler : access CPP_LI_Handler_Record'Class;
      File    : in out LI_File_Ptr;
      List    : in out LI_File_List;
      Project_View     : Prj.Project_Id;
      Module_Type_Defs : Module_Typedefs_List);
   procedure Sym_GV_Handler
     (Sym     : FIL_Table;
      Handler : access CPP_LI_Handler_Record'Class;
      File    : in out LI_File_Ptr;
      List    : in out LI_File_List;
      Project_View     : Prj.Project_Id;
      Module_Type_Defs : Module_Typedefs_List);
   procedure Sym_CON_Handler
     (Sym     : FIL_Table;
      Handler : access CPP_LI_Handler_Record'Class;
      File    : in out LI_File_Ptr;
      List    : in out LI_File_List;
      Project_View     : Prj.Project_Id;
      Module_Type_Defs : Module_Typedefs_List);
   procedure Sym_FD_Handler
     (Sym     : FIL_Table;
      Handler : access CPP_LI_Handler_Record'Class;
      File    : in out LI_File_Ptr;
      List    : in out LI_File_List;
      Project_View     : Prj.Project_Id;
      Module_Type_Defs : Module_Typedefs_List);
   procedure Sym_FU_Handler
     (Sym     : FIL_Table;
      Handler : access CPP_LI_Handler_Record'Class;
      File    : in out LI_File_Ptr;
      List    : in out LI_File_List;
      Project_View     : Prj.Project_Id;
      Module_Type_Defs : Module_Typedefs_List);
   procedure Sym_E_Handler
     (Sym     : FIL_Table;
      Handler : access CPP_LI_Handler_Record'Class;
      File    : in out LI_File_Ptr;
      List    : in out LI_File_List;
      Project_View     : Prj.Project_Id;
      Module_Type_Defs : Module_Typedefs_List);
   procedure Sym_EC_Handler
     (Sym     : FIL_Table;
      Handler : access CPP_LI_Handler_Record'Class;
      File    : in out LI_File_Ptr;
      List    : in out LI_File_List;
      Project_View     : Prj.Project_Id;
      Module_Type_Defs : Module_Typedefs_List);
   procedure Sym_T_Handler
     (Sym     : FIL_Table;
      Handler : access CPP_LI_Handler_Record'Class;
      File    : in out LI_File_Ptr;
      List    : in out LI_File_List;
      Project_View     : Prj.Project_Id;
      Module_Type_Defs : Module_Typedefs_List);
   procedure Sym_CL_Handler
     (Sym     : FIL_Table;
      Handler : access CPP_LI_Handler_Record'Class;
      File    : in out LI_File_Ptr;
      List    : in out LI_File_List;
      Project_View     : Prj.Project_Id;
      Module_Type_Defs : Module_Typedefs_List);
   procedure Sym_UN_Handler
     (Sym     : FIL_Table;
      Handler : access CPP_LI_Handler_Record'Class;
      File    : in out LI_File_Ptr;
      List    : in out LI_File_List;
      Project_View     : Prj.Project_Id;
      Module_Type_Defs : Module_Typedefs_List);
   procedure Sym_IV_Handler
     (Sym     : FIL_Table;
      Handler : access CPP_LI_Handler_Record'Class;
      File    : in out LI_File_Ptr;
      List    : in out LI_File_List;
      Project_View     : Prj.Project_Id;
      Module_Type_Defs : Module_Typedefs_List);
   procedure Sym_IU_Handler
     (Sym     : FIL_Table;
      Handler : access CPP_LI_Handler_Record'Class;
      File    : in out LI_File_Ptr;
      List    : in out LI_File_List;
      Project_View     : Prj.Project_Id;
      Module_Type_Defs : Module_Typedefs_List);
   procedure Sym_MA_Handler
     (Sym     : FIL_Table;
      Handler : access CPP_LI_Handler_Record'Class;
      File    : in out LI_File_Ptr;
      List    : in out LI_File_List;
      Project_View     : Prj.Project_Id;
      Module_Type_Defs : Module_Typedefs_List);
   procedure Sym_MD_Handler
     (Sym     : FIL_Table;
      Handler : access CPP_LI_Handler_Record'Class;
      File    : in out LI_File_Ptr;
      List    : in out LI_File_List;
      Project_View     : Prj.Project_Id;
      Module_Type_Defs : Module_Typedefs_List);

   ---------------------
   -- Symbol_Handlers --
   ---------------------

   Symbol_Handlers : constant array (Symbol_Type) of Symbol_Handler :=
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

   type To_Handler is access procedure
     (Ref     : TO_Table;
      Handler : access CPP_LI_Handler_Record'Class;
      File    : in out LI_File_Ptr;
      List    : in out LI_File_List;
      Project_View     : Prj.Project_Id;
      Module_Type_Defs : Module_Typedefs_List);

   procedure Fu_To_Gv_Handler
     (Ref     : TO_Table;
      Handler : access CPP_LI_Handler_Record'Class;
      File    : in out LI_File_Ptr;
      List    : in out LI_File_List;
      Project_View     : Prj.Project_Id;
      Module_Type_Defs : Module_Typedefs_List);
   procedure Fu_To_Fu_Handler
     (Ref     : TO_Table;
      Handler : access CPP_LI_Handler_Record'Class;
      File    : in out LI_File_Ptr;
      List    : in out LI_File_List;
      Project_View     : Prj.Project_Id;
      Module_Type_Defs : Module_Typedefs_List);
   procedure Fu_To_Con_Handler
     (Ref     : TO_Table;
      Handler : access CPP_LI_Handler_Record'Class;
      File    : in out LI_File_Ptr;
      List    : in out LI_File_List;
      Project_View     : Prj.Project_Id;
      Module_Type_Defs : Module_Typedefs_List);
   procedure Fu_To_E_Handler
     (Ref     : TO_Table;
      Handler : access CPP_LI_Handler_Record'Class;
      File    : in out LI_File_Ptr;
      List    : in out LI_File_List;
      Project_View     : Prj.Project_Id;
      Module_Type_Defs : Module_Typedefs_List);
   procedure Fu_To_Ec_Handler
     (Ref     : TO_Table;
      Handler : access CPP_LI_Handler_Record'Class;
      File    : in out LI_File_Ptr;
      List    : in out LI_File_List;
      Project_View     : Prj.Project_Id;
      Module_Type_Defs : Module_Typedefs_List);
   procedure Fu_To_Ma_Handler
     (Ref     : TO_Table;
      Handler : access CPP_LI_Handler_Record'Class;
      File    : in out LI_File_Ptr;
      List    : in out LI_File_List;
      Project_View     : Prj.Project_Id;
      Module_Type_Defs : Module_Typedefs_List);
   procedure Fu_To_Mi_Handler
     (Ref     : TO_Table;
      Handler : access CPP_LI_Handler_Record'Class;
      File    : in out LI_File_Ptr;
      List    : in out LI_File_List;
      Project_View     : Prj.Project_Id;
      Module_Type_Defs : Module_Typedefs_List);
   procedure Fu_To_Cl_Handler
     (Ref     : TO_Table;
      Handler : access CPP_LI_Handler_Record'Class;
      File    : in out LI_File_Ptr;
      List    : in out LI_File_List;
      Project_View     : Prj.Project_Id;
      Module_Type_Defs : Module_Typedefs_List);
   procedure Fu_To_T_Handler
     (Ref     : TO_Table;
      Handler : access CPP_LI_Handler_Record'Class;
      File    : in out LI_File_Ptr;
      List    : in out LI_File_List;
      Project_View     : Prj.Project_Id;
      Module_Type_Defs : Module_Typedefs_List);
   procedure Fu_To_Un_Handler
     (Ref     : TO_Table;
      Handler : access CPP_LI_Handler_Record'Class;
      File    : in out LI_File_Ptr;
      List    : in out LI_File_List;
      Project_View     : Prj.Project_Id;
      Module_Type_Defs : Module_Typedefs_List);

   -------------------
   --  To_Handlers  --
   -------------------

   Fu_To_Handlers : constant array (Symbol_Type) of To_Handler :=
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

   Table_Type_To_Ext : constant array (Table_Type) of String (1 .. 3) :=
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
     (DB_Prefix : in String;
      SN_Table  : out SN_Table_Array);

   procedure Close_DB_Files (SN_Table : in out SN_Table_Array);

   procedure Find_Or_Create_Class
     (Handler         : access CPP_LI_Handler_Record'Class;
      CL_Tab          : CL_Table;
      Source_Filename : String;
      Decl_Info       : out E_Declaration_Info_List;
      File            : in out LI_File_Ptr;
      List            : in out LI_File_List;
      Project_View     : Prj.Project_Id;
      Module_Type_Defs : Module_Typedefs_List);
   --  Attempts to find class declaration among this Handler.File declarations
   --  and dependency declarations. If not found, creates it.
   --  Return pointer to the created/found declaration or null
   --  on error

   procedure Process_File
     (Full_Filename : String;
      Handler       : access CPP_LI_Handler_Record'Class;
      File          : in out LI_File_Ptr;
      Project_View  : Prj.Project_Id;
      List_Of_Files : in out LI_File_List);
   --  Process the SN databases to create the LI structure for
   --  Source_Filename. Source_Filename is the name of the file as it appears
   --  in the sources (for instance, if we have #include "dir/file.h", then
   --  Source_Filename is "dir/file.h". Full_Filename is the full path to the
   --  physical file on the disk.

   --  Debugging utils
   procedure Info (Msg : String); -- print info message
   procedure Warn (Msg : String); -- print warning message
   procedure Fail (Msg : String); -- print error message
   pragma Inline (Info, Warn, Fail);

   function Get_SN_Dir (Project : Prj.Project_Id) return String;
   pragma Inline (Get_SN_Dir);

   procedure Refer_Type
     (Type_Name          : String;
      Type_Decl          : Point;
      File               : in out LI_File_Ptr;
      Reference_Point    : Point;
      Kind               : Reference_Kind := Reference);
   --  Adds reference object into Handler.File if
   --  type Type_Name already exists in the tree.
   --
   --  Type_Name, Type_Decl - name and position of
   --  the type declared in the Handler.File
   --  Reference_Filename and Reference_Point are
   --  location that refers to the type
   --
   --  Kind is a kind of a reference.

   function Get_Method_Kind
     (Handler                 : access CPP_LI_Handler_Record'Class;
      Class_Name, Return_Type : String)
      return E_Kind;
   --  Returns method E_Kind after investigation of its return
   --  type and template parameters of the class


   procedure Find_First_Forward_Declaration
     (Buffer       : in String_Access;
      Class_Name   : in Segment;
      Name         : in Segment;
      Filename     : in String;
      Return_Type  : in Segment;
      Arg_Types    : in Segment_Vector.Node_Access;
      Handler      : access CPP_LI_Handler_Record'Class;
      File         : in out LI_File_Ptr;
      List         : in out LI_File_List;
      Project_View : Prj.Project_Id;
      Module_Type_Defs : Module_Typedefs_List;
      Decl_Info    : out E_Declaration_Info_List);
   --  Attempts to find/create the first forward declaration
   --  for the method. Returns null if not found

   procedure Find_First_Forward_Declaration
     (Buffer       : in String_Access;
      Name         : in Segment;
      Filename     : in String;
      Return_Type  : in Segment;
      Arg_Types    : in Segment_Vector.Node_Access;
      Handler      : access CPP_LI_Handler_Record'Class;
      File         : in out LI_File_Ptr;
      List         : in out LI_File_List;
      Decl_Info    : out E_Declaration_Info_List);
   --  Attempts to find/create the first forward declaration
   --  for the function. Returns null if not found

   ----------------------------
   -- Generate_LI_For_Source --
   ----------------------------

   function Generate_LI_For_Source
     (Handler       : access CPP_LI_Handler_Record;
      Root_Project  : Prj.Project_Id;
      File_Project  : Prj.Project_Id;
      Full_Filename : String) return LI_Handler_Iterator'Class
   is
      pragma Unreferenced (Handler);
      pragma Unreferenced (Root_Project);
      pragma Unreferenced (File_Project);
      pragma Unreferenced (Full_Filename);
      HI : CPP_LI_Handler_Iterator;
   begin
      HI.State := Done;
      return HI;
   end Generate_LI_For_Source;

   -----------------------------
   -- Generate_LI_For_Project --
   -----------------------------

   function Generate_LI_For_Project
     (Handler       : access CPP_LI_Handler_Record;
      Root_Project  : Prj.Project_Id;
      Project       : Prj.Project_Id;
      Recursive     : Boolean := False)
      return LI_Handler_Iterator'Class
   is
      pragma Unreferenced (Root_Project);
      Tmp_File : File_Type;
      HI       : CPP_LI_Handler_Iterator;
      Success  : Boolean;
      Xref_File_Name : String_Access;
      Num_Source_Files : Natural := 0;
   begin
      --  Prepare the list of files
      Trace (Info_Stream, "Computing the C and C++ sources list");
      Compute_Sources
        (HI, Project, Recursive,
         Languages => (1 => Name_C, 2 => Name_C_Plus_Plus));

      HI.Handler := CPP_LI_Handler (Handler);
      HI.List_Filename := new String' (Handler.DB_Dir.all & "gps_list");

      --  Create the list of files that need to be analyzed.
      Create (Tmp_File, Out_File, Name => HI.List_Filename.all);

      loop
         declare
            File : constant String := Current_Source_File (HI);
         begin
            exit when File = "";

            --  Start processing next file
            --  File needs to be processed if:
            --  1. Its xref file is invalid (just created)
            --  2. Source is newer than xref file

            Xref_File_Name := Xref_Filename_For
              (File, Handler.DB_Dir.all, Handler.Xrefs);

            if not Is_Xref_Valid (File, Handler.Xrefs)
              or else To_Timestamp (File_Time_Stamp (File)) >
                To_Timestamp (File_Time_Stamp
                   (Handler.DB_Dir.all & Xref_File_Name.all))
            then
               Num_Source_Files := Num_Source_Files + 1;

               Set_Valid (File, True, Handler.Xrefs);

               --  Remove the current xref file if it exists, since
               --  cbrowser opens it in append mode.

               if Is_Regular_File
                 (Handler.DB_Dir.all & Xref_File_Name.all)
               then
                  Delete_File
                    (Handler.DB_Dir.all & Xref_File_Name.all, Success);
               end if;

               Put_Line
                 (Tmp_File, "@" & Handler.DB_Dir.all & Xref_File_Name.all);
               Put_Line (Tmp_File, File);
            end if;
         end;
         Next_Source_File (HI);
      end loop;

      Close (Tmp_File);

      if Num_Source_Files > 0 then
         Close_DB_Files (Handler.SN_Table);
         SN.Browse.Browse
           (File_Name     => HI.List_Filename.all,
            DB_Directory  => Handler.DB_Dir.all,
            DBIMP_Path    => Handler.DBIMP_Path.all,
            Cbrowser_Path => Handler.CBrowser_Path.all,
            PD            => HI.PD);
         HI.State := Analyze_Files;
      else
         HI.State := Done;
      end if;
      return HI;
   end Generate_LI_For_Project;

   --------------
   -- Continue --
   --------------

   procedure Continue
     (Iterator : in out CPP_LI_Handler_Iterator;
      Finished : out Boolean)
   is
      Process_Alive  : Boolean := False;
      Success        : Boolean;
   begin
      Finished := False;

      case Iterator.State is
         when Done =>
            null;
            --  see below.

         when Analyze_Files =>
            --  If we haven't finished the first phase, keep waiting.
            Browse.Is_Alive (Iterator.PD, Process_Alive);
            if Process_Alive then
               return;
            end if;

            Trace (Info_Stream, "Starting the dbimp process");

            --  All files processed, start generating of xrefs
            Iterator.State := Process_Xrefs;
            Browse.Generate_Xrefs
              (DB_Directory  => Iterator.Handler.DB_Dir.all,
               DBIMP_Path    => Iterator.Handler.DBIMP_Path.all,
               Temp_Name     => Iterator.Tmp_Filename,
               PD            => Iterator.PD);

         when Process_Xrefs =>
            --  If we haven't finished the second phase, keep waiting.
            Browse.Is_Alive (Iterator.PD, Process_Alive);
            if Process_Alive then
               return;
            end if;

            Iterator.State := Done;

            Open_DB_Files
              (Iterator.Handler.DB_Dir.all & Browse.DB_File_Name,
               Iterator.Handler.SN_Table);
      end case;

      if Iterator.State = Done then
         Trace (Info_Stream, "dbimp process is finished");
         Delete_File (Iterator.Tmp_Filename, Success);
         Delete_File (Iterator.List_Filename.all, Success);
         Free (Iterator.List_Filename);
         Finished := True;
      end if;
   end Continue;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Iterator : in out CPP_LI_Handler_Iterator) is
   begin
      Destroy (LI_Handler_Iterator (Iterator));
      Save (Iterator.Handler.Xrefs,
            Iterator.Handler.DB_Dir.all & Browse.Xref_Pool_Filename);
   end Destroy;

   ----------------
   -- Get_SN_Dir --
   ----------------

   function Get_SN_Dir (Project : Prj.Project_Id) return String is
   begin
      return Name_As_Directory
        (Prj_API.Object_Path (Project, Recursive => False))
        & Name_As_Directory (Browse.DB_Dir_Name);
   end Get_SN_Dir;

   -------------------
   -- Open_DB_Files --
   -------------------

   procedure Open_DB_Files
     (DB_Prefix : in String;
      SN_Table  : out SN_Table_Array) is
   begin
      for Table in Table_Type loop
         if Table_Type_To_Ext (Table)(1) /= ASCII.NUL then
            declare
               File_Name : constant String :=
                 DB_Prefix & "." & Table_Type_To_Ext (Table);
            begin
               Open (SN_Table (Table), File_Name);
            exception
               when others => null;
               --  could not open table, ignore this error
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

   procedure Process_File
     (Full_Filename : String;
      Handler       : access CPP_LI_Handler_Record'Class;
      File          : in out LI_File_Ptr;
      Project_View     : Prj.Project_Id;
      List_Of_Files : in out LI_File_List)
   is
      P               : Pair_Ptr;
      Module_Typedefs : Src_Info.Type_Utils.Module_Typedefs_List;
   begin
      if not Is_Open (Handler.SN_Table (FIL)) then
         --  .fil table does not exist, no data available
         Fail (".fil table does not exist: is SN DB generated already?");
         return;
      end if;

      Init (Module_Typedefs);
      Set_Cursor (Handler.SN_Table (FIL),
                  Position    => By_Key,
                  Key         => Full_Filename & Field_Sep,
                  Exact_Match => False);

      loop -- iterate thru all symbols for specified file
         P := Get_Pair (Handler.SN_Table (FIL), Next_By_Key);
         exit when P = null;

         declare
            Sym : FIL_Table := Parse_Pair (P.all);
         begin
            --  apply corresponding symbol handler
            Symbol_Handlers (Sym.Symbol)
              (Sym, Handler, File, List_Of_Files, Project_View,
               Module_Typedefs);
            Free (Sym);
         exception
            when others =>
               Free (Sym);
               raise;
         end;

         Free (P);
      end loop;

      Free (Module_Typedefs);
   exception
      when others   => -- unexpected exception
         Free (P);
         Free (Module_Typedefs);
         --  ??? Here we probably want to report the unexpected exception
         --  and continue to work further, but currently we reraise that
         --  exception
         raise;
   end Process_File;

   ---------------------
   -- Set_Executables --
   ---------------------

   function Set_Executables
     (Handler : access CPP_LI_Handler_Record) return String is
   begin
      Free (Handler.DBIMP_Path);
      Free (Handler.CBrowser_Path);

      Handler.DBIMP_Path    := Locate_Exec_On_Path (DBIMP);
      if Handler.DBIMP_Path = null then
         return DBIMP
           & " not found on the path. C/C++ browsing is not available";
      end if;

      Handler.CBrowser_Path := Locate_Exec_On_Path (CBrowser);
      if Handler.CBrowser_Path = null then
         Free (Handler.DBIMP_Path);
         return CBrowser
           & " not found on the path. C/C++ browsing is not available";
      end if;

      return "";
   end Set_Executables;

   -----------
   -- Reset --
   -----------

   procedure Reset
     (Handler : access CPP_LI_Handler_Record'Class;
      Project : Prj.Project_Id)
   is
      Dir : constant String := Name_As_Directory (Get_SN_Dir (Project));
   begin
      --  Reset the previous contents
      if Handler.DB_Dir = null
        or else Handler.DB_Dir.all /= Dir
      then
         if Handler.DB_Dir /= null then
            Free (Handler.Xrefs);
            Free (Handler.DB_Dir);
         end if;

         Handler.DB_Dir := new String' (Dir);
         Load (Handler.Xrefs, Handler.DB_Dir.all & Browse.Xref_Pool_Filename);

         --  Check that DB_Directory exists. If not, create it.
         if not Is_Directory (Handler.DB_Dir.all) then
            Make_Dir (Handler.DB_Dir.all);
         end if;
      end if;
      Close_DB_Files (Handler.SN_Table);
      Open_DB_Files
        (Handler.DB_Dir.all & Browse.DB_File_Name, Handler.SN_Table);
   end Reset;

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
      pragma Unreferenced (Predefined_Source_Path);
      pragma Unreferenced (Predefined_Object_Path);

      Full_Filename : constant String := Find_File
        (Source_Filename, Include_Path (Project, Recursive => True),
         Predefined_Path => "");

   begin
      if Full_Filename = "" then
         Warn ("File not found: " & Source_Filename);
         return;
      end if;

      --  check timestamps for the parsed file
      if File /= No_LI_File and then File.LI.Parsed then
         if To_Timestamp (File_Time_Stamp (Full_Filename)) <=
            File.LI.LI_Timestamp
         then
            return;
         end if;
         --  File is parsed, but not up-to-date. Destroy
         --  internals of the File to make sure we won't get
         --  duplicate references
         Destroy (File.LI.Body_Info.Declarations);
         File.LI.Body_Info.Declarations := null;
         Destroy (File.LI.Dependencies_Info);
         File.LI.Dependencies_Info := null;
      end if;

      Trace (Info_Stream, "Create_Or_Complete_LI " & Full_Filename);

      if File = No_LI_File then
         Create_Stub_For_File
           (LI            => File,
            Handler       => Handler,
            List          => List,
            Full_Filename => Full_Filename,
            Parsed        => True);
      end if;
      Convert_To_Parsed (File, Update_Timestamp => True);

      Process_File (Full_Filename, Handler, File, Project, List);

      Save (Handler.Xrefs,
            Handler.DB_Dir.all & Browse.Xref_Pool_Filename);
   exception
      when E : others =>
         Trace (Warn_Stream, "Unexpected exception: "
                & Exception_Information (E));
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
      pragma Unreferenced (Predefined_Source_Path);
      Full_Filename : constant String := Find_File
        (Source_Filename, Include_Path (Project, Recursive => True),
         Predefined_Path => "");
      Xref_Pool_Filename : constant String :=
        Handler.DB_Dir.all & Browse.Xref_Pool_Filename;

   begin
      if Full_Filename = "" then
         return "";
      end if;

      declare
         Xref_Filename : constant String := Xref_Filename_For
           (Full_Filename, Handler.DB_Dir.all, Handler.Xrefs).all;
      begin
         Save (Handler.Xrefs, Xref_Pool_Filename);
         return Xref_Filename;
      end;
   end LI_Filename_From_Source;

   ----------
   -- Info --
   ----------

   procedure Info (Msg : String) is
   begin
      Trace (Info_Stream, Msg);
   end Info;

   ----------
   -- Warn --
   ----------

   procedure Warn (Msg : String) is
   begin
      Trace (Warn_Stream, Msg);
   end Warn;

   ----------
   -- Fail --
   ----------

   procedure Fail (Msg : String) is
   begin
      Trace (Fail_Stream, Msg);
   end Fail;

   ----------------
   -- Refer_Type --
   ----------------

   procedure Refer_Type
     (Type_Name          : String;
      Type_Decl          : Point;
      File               : in out LI_File_Ptr;
      Reference_Point    : Point;
      Kind               : Reference_Kind := Reference)
   is
      Type_Decl_Info     : E_Declaration_Info_List;
   begin
      Type_Decl_Info := Find_Declaration
        (File         => File,
         Symbol_Name  => Type_Name,
         Location     => Type_Decl);

      if Type_Decl_Info /= null then
         Insert_Reference
           (Declaration_Info     => Type_Decl_Info,
            File                 => File,
            Location             => Reference_Point,
            Kind                 => Kind);
      end if;
   end Refer_Type;

   --------------------------
   -- Find_Or_Create_Class --
   --------------------------

   procedure Find_Or_Create_Class
     (Handler         : access CPP_LI_Handler_Record'Class;
      CL_Tab          : CL_Table;
      Source_Filename : String;
      Decl_Info       : out E_Declaration_Info_List;
      File            : in out LI_File_Ptr;
      List            : in out LI_File_List;
      Project_View     : Prj.Project_Id;
      Module_Type_Defs : Module_Typedefs_List)
   is
      Sym       : FIL_Table;
   begin
      if Source_Filename
         = CL_Tab.Buffer (CL_Tab.File_Name.First .. CL_Tab.File_Name.Last)
      then -- this class should be declared in the current file
         Decl_Info := Find_Declaration
           (File         => File,
            Symbol_Name  => CL_Tab.Buffer
              (CL_Tab.Name.First .. CL_Tab.Name.Last),
            Location     => CL_Tab.Start_Position);

         if Decl_Info = null then
            Sym.Buffer         := CL_Tab.Buffer;
            Sym.File_Name      := CL_Tab.File_Name;
            Sym.Start_Position := CL_Tab.Start_Position;
            Sym.Identifier     := CL_Tab.Name;
            Sym_CL_Handler
              (Sym, Handler, File, List, Project_View, Module_Type_Defs);

            Decl_Info := Find_Declaration
              (File         => File,
               Symbol_Name  => CL_Tab.Buffer
                 (CL_Tab.Name.First .. CL_Tab.Name.Last),
               Location     => CL_Tab.Start_Position);

            if Decl_Info = null then
               Fail ("We've just called Sym_CL_Handler but it has"
                       & " not created the declaration. Strange...");
            end if;
         end if;

      else -- this class should be declared as a dependency
         Decl_Info := Find_Dependency_Declaration
           (File         => File,
            Symbol_Name  => CL_Tab.Buffer
              (CL_Tab.Name.First .. CL_Tab.Name.Last),
            Location     => CL_Tab.Start_Position);

         if Decl_Info = null then
            Insert_Dependency_Declaration
              (Handler            => Handler,
               File               => File,
               List               => List,
               Symbol_Name        => CL_Tab.Buffer
                 (CL_Tab.Name.First .. CL_Tab.Name.Last),
               Referred_Filename  => CL_Tab.Buffer
                 (CL_Tab.File_Name.First .. CL_Tab.File_Name.Last),
               Location           => CL_Tab.Start_Position,
               Kind               => Record_Type,
               Scope              => Global_Scope,
               Declaration_Info   => Decl_Info);
            Set_End_Of_Scope
              (Decl_Info,
               Decl_Info.Value.Declaration.Location.File.LI,
               CL_Tab.End_Position,
               End_Of_Spec);
         end if;
      end if;
   end Find_Or_Create_Class;

   ---------------------
   -- Get_Method_Kind --
   ---------------------
   function Get_Method_Kind
     (Handler                 : access CPP_LI_Handler_Record'Class;
      Class_Name, Return_Type : String)
      return E_Kind is
      Class_Def          : CL_Table;
      Is_Template         : Boolean := False;
   begin
      begin -- check if this class is template
         Class_Def := Find (Handler.SN_Table (CL), Class_Name);
         Is_Template := Class_Def.Template_Parameters.First
            < Class_Def.Template_Parameters.Last;
         Free (Class_Def);
      exception
         when DB_Error | Not_Found =>
            null;
      end;
      if Return_Type = "void" then
         if Is_Template then
            return Generic_Procedure;
         else
            return Non_Generic_Procedure;
         end if;
      else
         if Is_Template then
            return Generic_Function_Or_Operator;
         else
            return Non_Generic_Function_Or_Operator;
         end if;
      end if;
   end Get_Method_Kind;

   ------------------------------------
   -- Find_First_Forward_Declaration --
   ------------------------------------

   procedure Find_First_Forward_Declaration
     (Buffer           : in String_Access;
      Class_Name       : in Segment;
      Name             : in Segment;
      Filename         : in String;
      Return_Type      : in Segment;
      Arg_Types        : in Segment_Vector.Node_Access;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : in out LI_File_List;
      Project_View     : Prj.Project_Id;
      Module_Type_Defs : Module_Typedefs_List;
      Decl_Info        : out E_Declaration_Info_List)
   is
      P            : Pair_Ptr;
      MD_Tab       : MD_Table;
      MD_Tab_Tmp   : MD_Table;
      First_MD_Pos : Point := Invalid_Point;
      CL_Tab       : CL_Table;
   begin
      Decl_Info := null;
      if not Is_Open (Handler.SN_Table (MD)) then
         return; -- .md table does not exist
      end if;

      --  First we have to find the first forward declaration
      --  that corresponds to our method, that is prototypes
      --  should be the same
      Set_Cursor
        (Handler.SN_Table (MD),
         By_Key,
         Buffer (Class_Name.First .. Class_Name.Last) & Field_Sep
            & Buffer (Name.First .. Name.Last) & Field_Sep,
         False);

      loop
         P := Get_Pair (Handler.SN_Table (MD), Next_By_Key);
         if P = null then -- no fwd decls at all
            return;
         end if;
         MD_Tab := Parse_Pair (P.all);
         Free (P);
         --  Update position of the first forward declaration
         exit when Cmp_Prototypes
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
        (Handler.SN_Table (MD),
         By_Key,
         Buffer (Class_Name.First .. Class_Name.Last) & Field_Sep
            & Buffer (Name.First .. Name.Last) & Field_Sep,
         False);

      loop
         P := Get_Pair (Handler.SN_Table (MD), Next_By_Key);
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

      Assert (Fail_Stream, First_MD_Pos /= Invalid_Point, "DB inconsistency");
      if Filename
         = MD_Tab.Buffer  (MD_Tab.File_Name.First .. MD_Tab.File_Name.Last)
      then -- work with declarations in the same file
         Decl_Info := Find_Declaration
           (File            => File,
            Symbol_Name     => Buffer (Name.First .. Name.Last),
            Class_Name      => Buffer (Class_Name.First .. Class_Name.Last),
            Location        => First_MD_Pos);

         if Decl_Info = null then
            Warn ("Someone needs function "
               & Buffer (Name.First .. Name.Last) & " before its declaration");
         end if;

      else -- work with dependency declarations
         Decl_Info := Find_Dependency_Declaration
           (File        => File,
            Symbol_Name => Buffer (Name.First .. Name.Last),
            Class_Name  => Buffer (Class_Name.First .. Class_Name.Last),
            Filename    => MD_Tab.Buffer
               (MD_Tab.File_Name.First .. MD_Tab.File_Name.Last),
            Location    => First_MD_Pos);

         if Decl_Info = null then
            begin -- create class declaration if needed
               CL_Tab := Find
                 (Handler.SN_Table (CL),
                  Buffer (Class_Name.First .. Class_Name.Last));
               Find_Or_Create_Class
                 (Handler,
                  CL_Tab,
                  Filename,
                  Decl_Info,
                  File,
                  List,
                  Project_View,
                  Module_Type_Defs);
               Free (CL_Tab);
            exception
               when Not_Found =>
                  null;
            end;
            Insert_Dependency_Declaration
              (Handler            => Handler,
               File               => File,
               List               => List,
               Symbol_Name        => Buffer (Name.First .. Name.Last),
               Referred_Filename  => MD_Tab.Buffer
                  (MD_Tab.File_Name.First .. MD_Tab.File_Name.Last),
               Location           => First_MD_Pos,
               Kind               => Get_Method_Kind
                 (Handler,
                  Buffer (Class_Name.First .. Class_Name.Last),
                  Buffer (Return_Type.First .. Return_Type.Last)),
               Scope              => Global_Scope,
               Declaration_Info   => Decl_Info);
         end if;

      end if;
      Free (MD_Tab);
   exception
      when DB_Error => null;
   end Find_First_Forward_Declaration;

   ------------------------------------
   -- Find_First_Forward_Declaration --
   ------------------------------------

   procedure Find_First_Forward_Declaration
     (Buffer       : in String_Access;
      Name         : in Segment;
      Filename     : in String;
      Return_Type  : in Segment;
      Arg_Types    : in Segment_Vector.Node_Access;
      Handler      : access CPP_LI_Handler_Record'Class;
      File         : in out LI_File_Ptr;
      List         : in out LI_File_List;
      Decl_Info    : out E_Declaration_Info_List)
   is
      P            : Pair_Ptr;
      FD_Tab       : FD_Table;
      FD_Tab_Tmp   : FD_Table;
      First_FD_Pos : Point := Invalid_Point;
      Match        : Boolean;
      Target_Kind  : E_Kind;
   begin
      if not Is_Open (Handler.SN_Table (FD)) then
         return; -- .fd table does not exist
      end if;

      --  First we have to find the first forward declaration
      --  that corresponds to our function, that is prototypes
      --  should be the same.
      Set_Cursor
        (Handler.SN_Table (FD),
         By_Key,
         Buffer (Name.First .. Name.Last) & Field_Sep,
         False);

      loop
         P := Get_Pair (Handler.SN_Table (FD), Next_By_Key);
         if P = null then -- no fwd decls at all
            return;
         end if;
         FD_Tab := Parse_Pair (P.all);
         Free (P);
         --  Update position of the first forward declaration
         exit when Cmp_Prototypes
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
        (Handler.SN_Table (FD),
         By_Key,
         Buffer (Name.First .. Name.Last) & Field_Sep,
         False);

      loop
         P := Get_Pair (Handler.SN_Table (FD), Next_By_Key);
         exit when P = null;
         FD_Tab_Tmp := Parse_Pair (P.all);
         Free (P);
         --  Update position of the first forward declaration
         Match :=
            FD_Tab.Buffer (FD_Tab.File_Name.First .. FD_Tab.File_Name.Last)
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

      Assert (Fail_Stream, First_FD_Pos /= Invalid_Point, "DB inconsistency");

      if Filename
         = FD_Tab.Buffer  (FD_Tab.File_Name.First .. FD_Tab.File_Name.Last)
      then -- work with declarations in the same file
         Decl_Info := Find_Declaration
           (File            => File,
            Symbol_Name     => Buffer (Name.First .. Name.Last),
            Location        => First_FD_Pos);

         if Decl_Info = null then
            Warn ("Someone needs function "
               & Buffer (Name.First .. Name.Last) & " before its declaration");
         end if;

      else -- work with dependency declarations
         Decl_Info := Find_Dependency_Declaration
           (File        => File,
            Symbol_Name => Buffer (Name.First .. Name.Last),
            Filename    => FD_Tab.Buffer
               (FD_Tab.File_Name.First .. FD_Tab.File_Name.Last),
            Location    => First_FD_Pos);

         if Decl_Info = null then
            if Buffer (Return_Type.First .. Return_Type.Last) = "void" then
               Target_Kind := Non_Generic_Procedure;
            else
               Target_Kind := Non_Generic_Function_Or_Operator;
            end if;

            Insert_Dependency_Declaration
              (Handler            => Handler,
               File               => File,
               List               => List,
               Symbol_Name        => Buffer (Name.First .. Name.Last),
               Referred_Filename  => FD_Tab.Buffer
                  (FD_Tab.File_Name.First .. FD_Tab.File_Name.Last),
               Location           => First_FD_Pos,
               Kind               => Target_Kind,
               Scope              => Global_Scope,
               Declaration_Info   => Decl_Info);
         end if;
      end if;
      Free (FD_Tab);
   exception
      when DB_Error => null;
   end Find_First_Forward_Declaration;

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

   ------------------------
   --  Fu_To_Cl_Handler  --
   ------------------------

   procedure Fu_To_Cl_Handler
     (Ref              : TO_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : in out LI_File_List;
      Project_View     : Prj.Project_Id;
      Module_Type_Defs : Module_Typedefs_List)
   is
      pragma Unreferenced (Project_View, Module_Type_Defs);
      Class_Desc : CType_Description;
      Class_Def  : CL_Table;
      Success    : Boolean;
      Decl_Info  : E_Declaration_Info_List;
      Ref_Id     : constant String := Ref.Buffer
        (Ref.Referred_Symbol_Name.First .. Ref.Referred_Symbol_Name.Last);

   begin
      --  Info ("Fu_To_Cl_Handler: " & Ref_Id);

      Find_Class
        (Type_Name      => Ref_Id,
         SN_Table       => Handler.SN_Table,
         Desc           => Class_Desc,
         Class_Def      => Class_Def,
         Success        => Success);

      if not Success then
         Fail ("unable to find class " & Ref_Id);
         return;
      end if;

      if Ref.Buffer (Ref.File_Name.First .. Ref.File_Name.Last) /=
          Class_Def.Buffer (Class_Def.File_Name.First ..
                            Class_Def.File_Name.Last)
      then
         Decl_Info := Find_Dependency_Declaration
           (File        => File,
            Symbol_Name => Class_Def.Buffer
              (Class_Def.Name.First .. Class_Def.Name.Last),
            Kind        => Record_Type,
            Location    => Class_Def.Start_Position,
            Filename    => Class_Def.Buffer
              (Class_Def.File_Name.First .. Class_Def.File_Name.Last));

         if Decl_Info = null then
            Insert_Dependency_Declaration
              (Handler            => Handler,
               File               => File,
               List               => List,
               Symbol_Name        => Class_Def.Buffer
                 (Class_Def.Name.First .. Class_Def.Name.Last),
               Referred_Filename  => Class_Def.Buffer
                 (Class_Def.File_Name.First .. Class_Def.File_Name.Last),
               Location           => Class_Def.Start_Position,
               Kind               => Record_Type,
               Scope              => Global_Scope,
               Declaration_Info   => Decl_Info);
         end if;

      else
         Decl_Info := Find_Declaration
           (File        => File,
            Symbol_Name => Class_Def.Buffer
              (Class_Def.Name.First .. Class_Def.Name.Last),
            Kind        => Record_Type,
            Location    => Class_Def.Start_Position);

         if Decl_Info = null then
            Insert_Declaration
              (Handler            => Handler,
               File               => File,
               List               => List,
               Symbol_Name        => Class_Def.Buffer
                 (Class_Def.Name.First .. Class_Def.Name.Last),
               Source_Filename    => Ref.Buffer
                 (Ref.File_Name.First .. Ref.File_Name.Last),
               Location           => Class_Def.Start_Position,
               Kind               => Record_Type,
               Scope              => Global_Scope,
               Declaration_Info   => Decl_Info);
         end if;
      end if;

      Insert_Reference
        (File              => File,
         Declaration_Info  => Decl_Info,
         Location          => Ref.Position,
         Kind              => Reference);
      Free (Class_Def);
      Free (Class_Desc);
   end Fu_To_Cl_Handler;

   ------------------------
   -- Fu_To_Con_Handler  --
   ------------------------

   procedure Fu_To_Con_Handler
     (Ref     : TO_Table;
      Handler : access CPP_LI_Handler_Record'Class;
      File    : in out LI_File_Ptr;
      List    : in out LI_File_List;
      Project_View     : Prj.Project_Id;
      Module_Type_Defs : Module_Typedefs_List)
   is
      Ref_Kind     : Reference_Kind;
      Decl_Info    : E_Declaration_Info_List;
      Var          : CON_Table;
      Desc         : CType_Description;
      Success      : Boolean;
      Scope        : E_Scope := Global_Scope;
      Attributes   : SN_Attributes;
      Sym          : FIL_Table;

   begin
      --  Info ("Fu_To_Con_Handler: "
      --        & Ref.Buffer (Ref.Referred_Symbol_Name.First ..
      --              Ref.Referred_Symbol_Name.Last));

      --  we need declaration's location
      Var := Find (Handler.SN_Table (CON),
         Ref.Buffer (Ref.Referred_Symbol_Name.First ..
                     Ref.Referred_Symbol_Name.Last));

      --  Find declaration
      if Xref_Filename_For
         (Var.Buffer (Var.File_Name.First .. Var.File_Name.Last),
          Handler.DB_Dir.all,
          Handler.Xrefs).all = Get_LI_Filename (File)
      then
         Decl_Info := Find_Declaration
           (File                    => File,
            Symbol_Name             =>
              Ref.Buffer (Ref.Referred_Symbol_Name.First ..
                            Ref.Referred_Symbol_Name.Last),
            Location                => Var.Start_Position);

         if Decl_Info = null then
            Sym.Buffer         := Var.Buffer;
            Sym.Identifier     := Var.Name;
            Sym.Start_Position := Var.Start_Position;
            Sym.File_Name      := Var.File_Name;
            Sym_CON_Handler
              (Sym, Handler, File, List, Project_View, Module_Type_Defs);

            Decl_Info := Find_Declaration
              (File                    => File,
               Symbol_Name             =>
                 Ref.Buffer (Ref.Referred_Symbol_Name.First ..
                               Ref.Referred_Symbol_Name.Last),
               Location                => Var.Start_Position);

            if Decl_Info = null then
               Fail ("Failed to create CON declaration");
               Free (Var);
               return;
            end if;
         end if;

      else -- another file
         Decl_Info := Find_Dependency_Declaration
           (File                    => File,
            Symbol_Name             =>
              Ref.Buffer (Ref.Referred_Symbol_Name.First ..
                            Ref.Referred_Symbol_Name.Last),
            Filename                =>
              Var.Buffer (Var.File_Name.First .. Var.File_Name.Last),
            Location                => Var.Start_Position);

         if Decl_Info = null then
            --  dep decl does not yet exist Collect information about the
            --  variable: type, scope, location of type declaration...
            Type_Name_To_Kind
              (Var.Buffer (Var.Declared_Type.First .. Var.Declared_Type.Last),
               Handler.SN_Table,
               Module_Type_Defs,
               Desc,
               Success);
            if not Success then -- unknown type
               Free (Var);
               return;
            end if;

            Attributes := SN_Attributes (Var.Attributes);

            if (Attributes and SN_STATIC) = SN_STATIC then
               Scope := Static_Local;
            end if;

            if Desc.Parent_Point = Invalid_Point then
               Insert_Dependency_Declaration
                 (Handler           => Handler,
                  File              => File,
                  List              => List,
                  Symbol_Name       =>
                    Var.Buffer (Var.Name.First .. Var.Name.Last),
                  Location          => Var.Start_Position,
                  Kind              => Type_To_Object (Desc.Kind),
                  Scope             => Scope,
                  Referred_Filename =>
                    Var.Buffer (Var.File_Name.First .. Var.File_Name.Last),
                  Declaration_Info  => Decl_Info);
            else
               Insert_Dependency_Declaration
                 (Handler           => Handler,
                  File              => File,
                  List              => List,
                  Symbol_Name       =>
                    Var.Buffer (Var.Name.First .. Var.Name.Last),
                  Location          => Var.Start_Position,
                  Kind              => Type_To_Object (Desc.Kind),
                  Scope             => Scope,
                  Referred_Filename =>
                    Var.Buffer (Var.File_Name.First .. Var.File_Name.Last),
                  Parent_Location   => Desc.Parent_Point,
                  Parent_Filename   => Desc.Parent_Filename.all,
                  Declaration_Info  => Decl_Info);
            end if;
            Free (Desc);
         end if;
      end if;
      Free (Var);

      if Ref.Buffer (Ref.Access_Type.First) = 'r' then
         Ref_Kind := Reference;
      else
         Ref_Kind := Modification;
      end if;

      Insert_Reference
        (Declaration_Info        => Decl_Info,
         File                    => File,
         Location                => Ref.Position,
         Kind                    => Ref_Kind);
   exception
      when Not_Found  | DB_Error => -- ignore
         Fail ("unable to find constant " &
               Ref.Buffer (Ref.Referred_Symbol_Name.First ..
                           Ref.Referred_Symbol_Name.Last));
   end Fu_To_Con_Handler;

   ------------------------
   --  Fu_To_E_Handler  --
   ------------------------

   procedure Fu_To_E_Handler
     (Ref              : TO_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : in out LI_File_List;
      Project_View     : Prj.Project_Id;
      Module_Type_Defs : Module_Typedefs_List)
   is
      pragma Unreferenced (Project_View, Module_Type_Defs);
      Ref_Id : constant String := Ref.Buffer
        (Ref.Referred_Symbol_Name.First .. Ref.Referred_Symbol_Name.Last);
      Enum_Desc : CType_Description;
      Enum_Def  : E_Table;
      Success    : Boolean;
      Decl_Info  : E_Declaration_Info_List;

   begin
      --  Info ("Fu_To_E_Handler: " & Ref_Id);

      Find_Enum
        (Type_Name      => Ref_Id,
         SN_Table       => Handler.SN_Table,
         Desc           => Enum_Desc,
         Enum_Def       => Enum_Def,
         Success        => Success);

      if not Success then
         Fail ("unable to find enum " & Ref_Id);
         return;
      end if;

      if not (Ref.Buffer (Ref.File_Name.First .. Ref.File_Name.Last)
         = Enum_Def.Buffer (Enum_Def.File_Name.First ..
                            Enum_Def.File_Name.Last))
      then
         Decl_Info := Find_Dependency_Declaration
           (File        => File,
            Symbol_Name => Enum_Def.Buffer
              (Enum_Def.Name.First .. Enum_Def.Name.Last),
            Kind        => Enumeration_Type,
            Location    => Enum_Def.Start_Position,
            Filename    => Enum_Def.Buffer
              (Enum_Def.File_Name.First .. Enum_Def.File_Name.Last));

         if Decl_Info = null then
            Insert_Dependency_Declaration
              (Handler            => Handler,
               File               => File,
               List               => List,
               Symbol_Name        => Enum_Def.Buffer
                 (Enum_Def.Name.First .. Enum_Def.Name.Last),
               Referred_Filename  => Enum_Def.Buffer
                 (Enum_Def.File_Name.First .. Enum_Def.File_Name.Last),
               Location           => Enum_Def.Start_Position,
               Kind               => Enumeration_Type,
               Scope              => Global_Scope,
               Declaration_Info   => Decl_Info);
         end if;

      else
         Decl_Info := Find_Declaration
           (File        => File,
            Symbol_Name => Enum_Def.Buffer
              (Enum_Def.Name.First .. Enum_Def.Name.Last),
            Kind        => Enumeration_Type,
            Location    => Enum_Def.Start_Position);

         if Decl_Info = null then
            Insert_Declaration
              (Handler            => Handler,
               File               => File,
               List               => List,
               Symbol_Name        => Enum_Def.Buffer
                 (Enum_Def.Name.First .. Enum_Def.Name.Last),
               Source_Filename    => Ref.Buffer
                 (Ref.File_Name.First .. Ref.File_Name.Last),
               Location           => Enum_Def.Start_Position,
               Kind               => Enumeration_Type,
               Scope              => Global_Scope,
               Declaration_Info   => Decl_Info);
         end if;
      end if;

      Insert_Reference
        (File              => File,
         Declaration_Info  => Decl_Info,
         Location          => Ref.Position,
         Kind              => Reference);
      Free (Enum_Def);
      Free (Enum_Desc);
   end Fu_To_E_Handler;

   ------------------------
   --  Fu_To_Ec_Handler  --
   ------------------------

   procedure Fu_To_Ec_Handler
     (Ref              : TO_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : in out LI_File_List;
      Project_View     : Prj.Project_Id;
      Module_Type_Defs : Module_Typedefs_List)
   is
      pragma Unreferenced (Project_View, Module_Type_Defs);
      Decl_Info  : E_Declaration_Info_List;
      Enum_Const : EC_Table;
      Ref_Id     : constant String := Ref.Buffer
        (Ref.Referred_Symbol_Name.First .. Ref.Referred_Symbol_Name.Last);

   begin
      --  Info ("Fu_To_EC_Handler: " & Ref_Id);

      Enum_Const := Find (Handler.SN_Table (EC), Ref_Id);

      --  Find declaration
      if Xref_Filename_For
         (Enum_Const.Buffer
            (Enum_Const.File_Name.First .. Enum_Const.File_Name.Last),
          Handler.DB_Dir.all,
          Handler.Xrefs).all = Get_LI_Filename (File)
      then
         Decl_Info := Find_Declaration
           (File                    => File,
            Symbol_Name             => Ref_Id,
            Location                => Enum_Const.Start_Position);

         if Decl_Info = null then
            Insert_Declaration
              (Handler           => Handler,
               File              => File,
               List              => List,
               Symbol_Name       => Ref_Id,
               Source_Filename   =>
                 Ref.Buffer (Ref.File_Name.First .. Ref.File_Name.Last),
               Location          => Enum_Const.Start_Position,
               Kind              => Enumeration_Literal,
               Scope             => Global_Scope,
               Declaration_Info  => Decl_Info);
         end if;

      else -- another file
         Decl_Info := Find_Dependency_Declaration
           (File                    => File,
            Symbol_Name             => Ref_Id,
            Filename                => Enum_Const.Buffer
              (Enum_Const.File_Name.First .. Enum_Const.File_Name.Last),
            Location                => Enum_Const.Start_Position);

         if Decl_Info = null then
            Insert_Dependency_Declaration
              (Handler           => Handler,
               File              => File,
               List              => List,
               Symbol_Name       => Ref_Id,
               Location          => Enum_Const.Start_Position,
               Kind              => Enumeration_Literal,
               Scope             => Global_Scope,
               Referred_Filename => Enum_Const.Buffer
                 (Enum_Const.File_Name.First .. Enum_Const.File_Name.Last),
               Declaration_Info  => Decl_Info);
         end if;
      end if;
      Free (Enum_Const);

      Insert_Reference
        (Declaration_Info        => Decl_Info,
         File                    => File,
         Location                => Ref.Position,
         Kind                    => Reference);

   exception
      when DB_Error | Not_Found =>
         Fail ("unable to find enumeration constant " & Ref_Id);
   end Fu_To_Ec_Handler;

   ------------------------
   --  Fu_To_Fu_Handler  --
   ------------------------

   procedure Fu_To_Fu_Handler
     (Ref              : TO_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : in out LI_File_List;
      Project_View     : Prj.Project_Id;
      Module_Type_Defs : Module_Typedefs_List)
   is
      pragma Unreferenced (Project_View, Module_Type_Defs);
      P              : Pair_Ptr;
      Fn             : FU_Table;
      Fn_Tmp         : FU_Table;
      Decl_Info      : E_Declaration_Info_List;
      Overloaded     : Boolean := False;
      Forward_Declared : Boolean := False;
      No_Body        : Boolean := True;
      Kind           : E_Kind;
      FDecl          : FD_Table;
      FDecl_Tmp      : FD_Table;
      Ref_Id         : constant String := Ref.Buffer
        (Ref.Referred_Symbol_Name.First .. Ref.Referred_Symbol_Name.Last);
      Buffer   : String_Access;
      Filename       : Segment;
      Return_Type    : Segment;
      Start_Position : Point;

   begin
      --  Info ("Fu_To_Fu_Handler: " & Ref_Id);

      if Is_Open (Handler.SN_Table (FD)) then
         Set_Cursor (Handler.SN_Table (FD), By_Key, Ref_Id, False);

         loop
            P := Get_Pair (Handler.SN_Table (FD), Next_By_Key);
            exit when P = null;
            FDecl_Tmp := Parse_Pair (P.all);
            Free (P);
            if not Forward_Declared then
               FDecl := FDecl_Tmp;
               Forward_Declared := True;
            else
               Overloaded := not Cmp_Arg_Types -- skip multiple fwd decls
                  (FDecl.Buffer,
                   FDecl_Tmp.Buffer,
                   FDecl.Arg_Types,
                   FDecl_Tmp.Arg_Types);
               Free (FDecl_Tmp);
               exit when Overloaded;
            end if;
         end loop;
      end if;

      if not Overloaded then
         --  Forward declarations may be overloaded by inline implementations
         --  this is what we check here. If no forward declaration was found
         --  above we search for a suitable function body
         Set_Cursor (Handler.SN_Table (FU), By_Key, Ref_Id, False);

         loop
            P := Get_Pair (Handler.SN_Table (FU), Next_By_Key);
            exit when P = null;
            Fn_Tmp := Parse_Pair (P.all);
            Free (P);
            if not Forward_Declared and No_Body then
               --  No forward decls, but we found the first function
               --  with the same name
               Fn      := Fn_Tmp;
               No_Body := False;
            elsif not Forward_Declared and not No_Body then
               --  No forward decls and we found one more function body
               --  with the same name
               Overloaded := True;
               Free (Fn_Tmp);
            elsif Forward_Declared and No_Body then
               --  We have found some forward declaration, but no body
               --  is yet found. Do we have overloading here?
               Overloaded := not Cmp_Arg_Types
                  (Fn_Tmp.Buffer,
                   FDecl.Buffer,
                   Fn_Tmp.Arg_Types,
                   FDecl.Arg_Types);
               if not Overloaded then -- we found the body!
                  No_Body := False;
                  Fn      := Fn_Tmp;
               else
                  Free (Fn_Tmp); -- it's not our body, but it's overloading
               end if;
            else -- Forward_Declared and not No_Body
               --  We have found forward declaration and corresponding body
               --  all other bodies should be overloading functions
               Overloaded := True;
               Free (Fn_Tmp);
            end if;
            exit when Overloaded;
         end loop;
      end if;

      if not Forward_Declared and No_Body then
         Fail ("Can't find either forward declaration or body for " & Ref_Id);
         return;
      end if;

      if not Overloaded then
         Assert (Fail_Stream, Forward_Declared or not No_Body,
           "Hey, what's up?");
         if Forward_Declared then
            Buffer         := FDecl.Buffer;
            Filename       := FDecl.File_Name;
            Start_Position := FDecl.Start_Position;
            Return_Type    := FDecl.Return_Type;
         else
            Buffer         := Fn.Buffer;
            Filename       := Fn.File_Name;
            Start_Position := Fn.Start_Position;
            Return_Type    := Fn.Return_Type;
         end if;

         if Buffer (Return_Type.First .. Return_Type.Last) = "void" then
            Kind := Non_Generic_Procedure;
         else
            Kind := Non_Generic_Function_Or_Operator;
         end if;
         --  this is a function defined in the current file
         --  it may be either forward declared or implemented
         --  right away
         if Forward_Declared then
            Find_First_Forward_Declaration
              (FDecl.Buffer,
               FDecl.Name,
               Ref.Buffer (Ref.File_Name.First .. Ref.File_Name.Last),
               FDecl.Return_Type,
               FDecl.Arg_Types,
               Handler,
               File,
               List,
               Decl_Info);
         else -- when only body is available
            Decl_Info := Find_Declaration
              (File        => File,
               Symbol_Name => Fn.Buffer (Fn.Name.First .. Fn.Name.Last),
               Location    => Fn.Start_Position);
         end if;

         if Decl_Info = null then
            --  function is used before
            --  declaration. Create forward declaration
            Insert_Declaration
              (Handler            => Handler,
               File               => File,
               List               => List,
               Symbol_Name        => Ref_Id,
               Source_Filename    => Ref.Buffer
                 (Ref.File_Name.First .. Ref.File_Name.Last),
               Location           => Ref.Position,
               Kind               => Kind,
               Scope              => Global_Scope,
               Declaration_Info   => Decl_Info);
         end if;

      else  --  overloaded entity
            --  have we already declared it?
         Assert (Warn_Stream, File /= null,
                 "Fu_To_Fu_Handler, File not created yet");

         Decl_Info := Find_Declaration
           (File        => File,
            Symbol_Name => Ref_Id,
            Kind        => Overloaded_Entity);

         if Decl_Info = null then
            Decl_Info := new E_Declaration_Info_Node'
              (Value => (Declaration => No_Declaration,
                         References  => null),
               Next => File.LI.Body_Info.Declarations);
            Decl_Info.Value.Declaration.Name := new String'(Ref_Id);
            Decl_Info.Value.Declaration.Kind := Overloaded_Entity;
            File.LI.Body_Info.Declarations := Decl_Info;
         end if;
      end if;

      if Forward_Declared then
         Free (FDecl);
      end if;

      if not No_Body then
         Free (Fn);
      end if;

      Insert_Reference
        (Decl_Info,
         File,
         Ref.Position,
         Reference);
   exception
      when Not_Found  | DB_Error => -- ignore
         Fail ("unable to find function " & Ref_Id);
      return;
   end Fu_To_Fu_Handler;

   ------------------------
   --  Fu_To_Gv_Handler  --
   ------------------------

   procedure Fu_To_Gv_Handler
     (Ref     : TO_Table;
      Handler : access CPP_LI_Handler_Record'Class;
      File    : in out LI_File_Ptr;
      List    : in out LI_File_List;
      Project_View     : Prj.Project_Id;
      Module_Type_Defs : Module_Typedefs_List)
   is
      Ref_Kind     : Reference_Kind;
      Decl_Info    : E_Declaration_Info_List;
      Var          : GV_Table;
      Desc         : CType_Description;
      Success      : Boolean;
      Scope        : E_Scope := Global_Scope;
      Attributes   : SN_Attributes;
      Ref_Id       : constant String := Ref.Buffer
        (Ref.Referred_Symbol_Name.First .. Ref.Referred_Symbol_Name.Last);
      Sym          : FIL_Table;
   begin
      --  Info ("Fu_To_GV_Handler: " & Ref_Id);

      --  we need declaration's location
      Var := Find (Handler.SN_Table (GV), Ref_Id);

      --  Find declaration
      if Xref_Filename_For
         (Var.Buffer (Var.File_Name.First .. Var.File_Name.Last),
          Handler.DB_Dir.all,
          Handler.Xrefs).all = Get_LI_Filename (File)
      then
         Decl_Info := Find_Declaration
           (File                    => File,
            Symbol_Name             => Ref_Id,
            Location                => Var.Start_Position);

         if Decl_Info = null then
            Info ("Forward reference to the variable: " & Ref_Id);
            Sym.Buffer         := Var.Buffer;
            Sym.Identifier     := Var.Name;
            Sym.Start_Position := Var.Start_Position;
            Sym.File_Name      := Var.File_Name;
            Sym_GV_Handler
              (Sym, Handler, File, List, Project_View, Module_Type_Defs);

            Decl_Info := Find_Declaration
              (File                    => File,
               Symbol_Name             => Ref_Id,
               Location                => Var.Start_Position);

            if Decl_Info = null then
               Fail ("unable to create declaration for global variable "
                       & Ref_Id);
               Free (Var);
               return;
            end if;
         end if;

      else -- another file
         Decl_Info := Find_Dependency_Declaration
           (File                    => File,
            Symbol_Name             => Ref_Id,
            Filename                =>
              Var.Buffer (Var.File_Name.First .. Var.File_Name.Last),
            Location                => Var.Start_Position);

         if Decl_Info = null then
            --  Collect information about the variable:
            --  type, scope, location of type declaration...
            Type_Name_To_Kind
              (Var.Buffer
                 (Var.Value_Type.First .. Var.Value_Type.Last),
               Handler.SN_Table,
               Module_Type_Defs,
               Desc,
               Success);
            if not Success then -- unknown type
               Free (Var);
               return;
            end if;

            Attributes := SN_Attributes (Var.Attributes);

            if (Attributes and SN_STATIC) = SN_STATIC then
               Scope := Static_Local;
            end if;

            if Desc.Parent_Point = Invalid_Point then
               Insert_Dependency_Declaration
                 (Handler           => Handler,
                  File              => File,
                  List              => List,
                  Symbol_Name       =>
                    Var.Buffer (Var.Name.First .. Var.Name.Last),
                  Location          => Var.Start_Position,
                  Kind              => Type_To_Object (Desc.Kind),
                  Scope             => Scope,
                  Referred_Filename =>
                    Var.Buffer (Var.File_Name.First .. Var.File_Name.Last),
                  Declaration_Info  => Decl_Info);
            else
               Insert_Dependency_Declaration
                 (Handler           => Handler,
                  File              => File,
                  List              => List,
                  Symbol_Name       =>
                    Var.Buffer (Var.Name.First .. Var.Name.Last),
                  Location          => Var.Start_Position,
                  Kind              => Type_To_Object (Desc.Kind),
                  Scope             => Scope,
                  Referred_Filename =>
                    Var.Buffer (Var.File_Name.First .. Var.File_Name.Last),
                  Parent_Location   => Desc.Parent_Point,
                  Parent_Filename   => Desc.Parent_Filename.all,
                  Declaration_Info  => Decl_Info);
            end if;
            Free (Desc);
         end if;
      end if;
      Free (Var);

      if Ref.Buffer (Ref.Access_Type.First) = 'r' then
         Ref_Kind := Reference;
      else
         Ref_Kind := Modification;
      end if;


      Insert_Reference
        (Declaration_Info        => Decl_Info,
         File                    => File,
         Location                => Ref.Position,
         Kind                    => Ref_Kind);
   exception
      when Not_Found  | DB_Error => -- ignore
         Fail ("unable to find global variable " & Ref_Id);
   end Fu_To_Gv_Handler;

   ------------------------
   --  Fu_To_Ma_Handler  --
   ------------------------

   procedure Fu_To_Ma_Handler
     (Ref              : TO_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : in out LI_File_List;
      Project_View     : Prj.Project_Id;
      Module_Type_Defs : Module_Typedefs_List)
   is
      pragma Unreferenced (Project_View, Module_Type_Defs);
      Macro  : MA_Table;
      Ref_Id : constant String := Ref.Buffer
        (Ref.Referred_Symbol_Name.First .. Ref.Referred_Symbol_Name.Last);
      Decl_Info : E_Declaration_Info_List;
   begin
      if not Is_Open (Handler.SN_Table (MA)) then
         --  .ma table does not exist
         return;
      end if;

      --  Info ("Fu_To_Ma: " & Ref_Id);

      Macro := Find (Handler.SN_Table (MA), Ref_Id);

      if Xref_Filename_For
         (Macro.Buffer (Macro.File_Name.First .. Macro.File_Name.Last),
          Handler.DB_Dir.all,
          Handler.Xrefs).all = Get_LI_Filename (File)
      then
         --  look for declaration in current file
         Decl_Info := Find_Declaration
           (File        => File,
            Symbol_Name => Ref_Id,
            Location    => Macro.Start_Position);

         if Decl_Info = null then
            Insert_Declaration
              (Handler            => Handler,
               File               => File,
               List               => List,
               Symbol_Name        => Ref_Id,
               Source_Filename    => Ref.Buffer
                 (Ref.File_Name.First .. Ref.File_Name.Last),
               Location           => Macro.Start_Position,
               Kind               => Unresolved_Entity,
               Scope              => Global_Scope,
               Declaration_Info   => Decl_Info);
         end if;

      else
         --  look for dependency declaration
         Decl_Info := Find_Dependency_Declaration
           (File                 => File,
            Symbol_Name          => Ref_Id,
            Filename             => Macro.Buffer
              (Macro.File_Name.First .. Macro.File_Name.Last),
            Location             => Macro.Start_Position);

         if Decl_Info = null then
            Insert_Dependency_Declaration
              (Handler           => Handler,
               File              => File,
               List              => List,
               Symbol_Name       => Ref_Id,
               Location          => Macro.Start_Position,
               Kind              => Unresolved_Entity,
               Scope             => Global_Scope,
               Referred_Filename => Macro.Buffer
                 (Macro.File_Name.First .. Macro.File_Name.Last),
               Declaration_Info  => Decl_Info);
         end if;
      end if;

      Insert_Reference
        (Declaration_Info     => Decl_Info,
         File                 => File,
         Location             => Ref.Position,
         Kind                 => Reference);

      Free (Macro);

   exception
      when DB_Error | Not_Found =>
         Fail ("unable to find macro " & Ref_Id);
   end Fu_To_Ma_Handler;

   ------------------------
   --  Fu_To_Mi_Handler  --
   ------------------------

   procedure Fu_To_Mi_Handler
     (Ref     : TO_Table;
      Handler : access CPP_LI_Handler_Record'Class;
      File    : in out LI_File_Ptr;
      List    : in out LI_File_List;
      Project_View     : Prj.Project_Id;
      Module_Type_Defs : Module_Typedefs_List)
   is
      P             : Pair_Ptr;
      Fn            : MI_Table;
      MDecl         : MD_Table;
      MDecl_Tmp     : MD_Table;
      Decl_Info     : E_Declaration_Info_List;
      Overloaded    : Boolean := False;
      Init          : Boolean := True;
      Pure_Virtual  : Boolean := False;
      Kind          : E_Kind;
      Ref_Id        : constant String := Ref.Buffer
        (Ref.Referred_Symbol_Name.First .. Ref.Referred_Symbol_Name.Last);
      Ref_Class     : constant String := Ref.Buffer
        (Ref.Referred_Class.First .. Ref.Referred_Class.Last);
      Attributes    : SN_Attributes;
   begin
      --  Info ("Fu_To_Mi_Handler: " & Ref_Id);

      Set_Cursor
        (Handler.SN_Table (MD),
         By_Key,
         Ref_Class & Field_Sep & Ref_Id & Field_Sep,
         False);

      loop
         P := Get_Pair (Handler.SN_Table (MD), Next_By_Key);
         exit when P = null;
         MDecl_Tmp := Parse_Pair (P.all);
         Free (P);
         if Init then
            Init  := False;
            MDecl := MDecl_Tmp;
         else
            Overloaded := not Cmp_Arg_Types -- skip multiple fws decls
              (MDecl_Tmp.Buffer,
               MDecl.Buffer,
               MDecl_Tmp.Arg_Types,
               MDecl.Arg_Types);
            Free (MDecl_Tmp);
            exit when Overloaded;
         end if;
      end loop;

      if Init then -- declaration for the referred method not found
         --  ??? We should handle this situation in a special way:
         Fail ("unable to find method " & Ref_Class & "::" & Ref_Id);
         return;
      end if;

      --  Once we have found the declaration(s) we may try to look up
      --  implementation as well
      if not Overloaded then
         Set_Cursor
           (Handler.SN_Table (MI),
            By_Key,
            Ref_Class & Field_Sep & Ref_Id & Field_Sep,
            False);

         Init := True;
         loop
            P := Get_Pair (Handler.SN_Table (MI), Next_By_Key);
            exit when P = null;
            Fn := Parse_Pair (P.all);
            Free (P);
            Init := False;
            exit when Cmp_Arg_Types
              (MDecl.Buffer,
               Fn.Buffer,
               MDecl.Arg_Types,
               Fn.Arg_Types);
            Init := True;
            Free (Fn);
         end loop;

         if Init then -- implementation for the referred method not found
            --  this must be a pure virtual method
            Attributes := SN_Attributes (MDecl.Attributes);
            if (Attributes and SN_PUREVIRTUAL) /= SN_PUREVIRTUAL then
               Fail ("failed to locate method implementation, but it is not"
                  & " an abstract one: " & Ref_Class & "::" & Ref_Id);
               Free (MDecl);
               return;
            end if;
            Pure_Virtual := True;
         else
            Free (Fn);
         end if;

         Kind := Get_Method_Kind
           (Handler,
            Ref_Class,
            MDecl.Buffer (MDecl.Return_Type.First .. MDecl.Return_Type.Last));

         Find_First_Forward_Declaration
           (MDecl.Buffer,
            MDecl.Class,
            MDecl.Name,
            Ref.Buffer (Ref.File_Name.First .. Ref.File_Name.Last),
            MDecl.Return_Type,
            MDecl.Arg_Types,
            Handler,
            File,
            List,
            Project_View,
            Module_Type_Defs,
            Decl_Info);

         if Decl_Info = null then
            --  method is used before
            --  declaration. Create forward declaration
            Insert_Declaration
              (Handler            => Handler,
               File               => File,
               List               => List,
               Symbol_Name        => Ref_Id,
               Source_Filename    => Ref.Buffer
                 (Ref.File_Name.First .. Ref.File_Name.Last),
               Location           => Ref.Position,
               Kind               => Kind,
               Scope              => Global_Scope,
               Declaration_Info   => Decl_Info);
         end if;

      else -- overloaded entity
         --  have we already declared it?
         declare
            Class_Def : CL_Table;
         begin
            Class_Def := Find (Handler.SN_Table (CL), Ref_Class);
            --  ??? what to do when several classes with one name are available
            --  what about unions?

            Decl_Info := Find_Declaration
              (File        => File,
               Symbol_Name => Ref_Id,
               Class_Name  => Ref_Class,
               Kind        => Overloaded_Entity,
               Location    => Class_Def.Start_Position);

            if Decl_Info = null then
               Decl_Info := new E_Declaration_Info_Node'
                 (Value =>
                    (Declaration => No_Declaration,
                     References => null),
                  Next => File.LI.Body_Info.Declarations);
               Decl_Info.Value.Declaration.Name := new String'(Ref_Id);
               Decl_Info.Value.Declaration.Kind := Overloaded_Entity;
               Decl_Info.Value.Declaration.Location.Line :=
                  Class_Def.Start_Position.Line;
               Decl_Info.Value.Declaration.Location.File :=
                  (LI              => File,
                   Part            => Unit_Body,
                   Source_Filename =>
                      new String'(Get_LI_Filename (File)));
               Decl_Info.Value.Declaration.Location.Column :=
                  Class_Def.Start_Position.Column;
               File.LI.Body_Info.Declarations := Decl_Info;
            end if;

            Free (Class_Def);

         exception
            when DB_Error | Not_Found =>
               Fail ("Failed to lookup class " & Ref_Class
                  & " for method " & Ref_Id);
               Free (MDecl);
               return;
         end;
      end if;
      Free (MDecl);

      Insert_Reference
        (Decl_Info,
         File,
         Ref.Position,
         Reference);
   exception
      when Not_Found  | DB_Error => -- ignore
         Fail ("unable to find method " & Ref_Class & "::" & Ref_Id);
         return;
   end Fu_To_Mi_Handler;

   ------------------------
   --  Fu_To_T_Handler  --
   ------------------------

   procedure Fu_To_T_Handler
     (Ref     : TO_Table;
      Handler : access CPP_LI_Handler_Record'Class;
      File    : in out LI_File_Ptr;
      List    : in out LI_File_List;
      Project_View     : Prj.Project_Id;
      Module_Type_Defs : Module_Typedefs_List)
   is
      pragma Unreferenced (Project_View);
      Typedef   : T_Table;
      Ref_Id    : constant String := Ref.Buffer
        (Ref.Referred_Symbol_Name.First .. Ref.Referred_Symbol_Name.Last);
      Decl_Info : E_Declaration_Info_List;
      Desc      : CType_Description;
      Success   : Boolean := False;
   begin
      if not Is_Open (Handler.SN_Table (T)) then
         --  .t table does not exist
         return;
      end if;

      --  Info ("Fu_To_T: " & Ref_Id);

      Typedef := Find (Handler.SN_Table (T), Ref_Id);

      if Xref_Filename_For
         (Typedef.Buffer (Typedef.File_Name.First .. Typedef.File_Name.Last),
          Handler.DB_Dir.all,
          Handler.Xrefs).all = Get_LI_Filename (File)
      then
         --  look for declaration in current file
         Decl_Info := Find_Declaration
           (File        => File,
            Symbol_Name => Ref_Id,
            Location    => Typedef.Start_Position);

         if Decl_Info = null then
            --  Declaration for type is not created yet
            Find_Original_Type
              (Ref_Id,
               Handler.SN_Table,
               Module_Type_Defs,
               Desc, Success);

            if not Success then
               Fail ("unable to find type for typedef " & Ref_Id);
               Free (Desc);
               Free (Typedef);
               return;
            end if;

            if Desc.Ancestor_Point = Invalid_Point then
               --  unknown parent
               Insert_Declaration
                 (Handler           => Handler,
                  File              => File,
                  List              => List,
                  Symbol_Name       => Ref_Id,
                  Source_Filename   => Ref.Buffer
                    (Ref.File_Name.First .. Ref.File_Name.Last),
                  Location          => Typedef.Start_Position,
                  Kind              => Desc.Kind,
                  Scope             => Global_Scope,
                  Declaration_Info  => Decl_Info);
            elsif Desc.Ancestor_Point = Predefined_Point then
               --  typedef for builin type
               Insert_Declaration
                 (Handler           => Handler,
                  File              => File,
                  List              => List,
                  Symbol_Name       => Ref_Id,
                  Source_Filename   => Ref.Buffer
                    (Ref.File_Name.First .. Ref.File_Name.Last),
                  Location          => Typedef.Start_Position,
                  Parent_Location   => Predefined_Point,
                  Kind              => Desc.Kind,
                  Scope             => Global_Scope,
                  Declaration_Info  => Decl_Info);
            else
               --  parent type found
               Insert_Declaration
                 (Handler           => Handler,
                  File              => File,
                  List              => List,
                  Symbol_Name       => Ref_Id,
                  Source_Filename   => Ref.Buffer
                    (Ref.File_Name.First .. Ref.File_Name.Last),
                  Location          => Typedef.Start_Position,
                  Parent_Location   => Desc.Ancestor_Point,
                  Parent_Filename   => Desc.Ancestor_Filename.all,
                  Kind              => Desc.Kind,
                  Scope             => Global_Scope,
                  Declaration_Info  => Decl_Info);
            end if;
         end if;

      else
         --  look for dependency declaration
         Decl_Info := Find_Dependency_Declaration
           (File                 => File,
            Symbol_Name          => Ref_Id,
            Filename             => Typedef.Buffer
              (Typedef.File_Name.First .. Typedef.File_Name.Last),
            Location             => Typedef.Start_Position);

         if Decl_Info = null then
            Find_Original_Type
              (Ref_Id,
               Handler.SN_Table,
               Module_Type_Defs,
               Desc,
               Success);

            if not Success then
               Fail ("unable to find type for typedef " & Ref_Id);
               Free (Desc);
               Free (Typedef);
               return;
            end if;

            if Desc.Ancestor_Point = Invalid_Point then
               --  unknown parent
               Insert_Dependency_Declaration
                 (Handler           => Handler,
                  File              => File,
                  List              => List,
                  Symbol_Name       => Ref_Id,
                  Location          => Typedef.Start_Position,
                  Kind              => Desc.Kind,
                  Scope             => Global_Scope,
                  Referred_Filename => Typedef.Buffer
                    (Typedef.File_Name.First .. Typedef.File_Name.Last),
                  Declaration_Info  => Decl_Info);
            elsif Desc.Ancestor_Point = Predefined_Point then
               --  typedef for builtin type
               Insert_Dependency_Declaration
                 (Handler           => Handler,
                  File              => File,
                  List              => List,
                  Symbol_Name       => Ref_Id,
                  Location          => Typedef.Start_Position,
                  Parent_Location   => Predefined_Point,
                  Kind              => Desc.Kind,
                  Scope             => Global_Scope,
                  Referred_Filename => Typedef.Buffer
                    (Typedef.File_Name.First .. Typedef.File_Name.Last),
                  Declaration_Info  => Decl_Info);
            else
               --  parent type found
               Insert_Dependency_Declaration
                 (Handler           => Handler,
                  File              => File,
                  List              => List,
                  Symbol_Name       => Ref_Id,
                  Location          => Typedef.Start_Position,
                  Parent_Location   => Desc.Ancestor_Point,
                  Parent_Filename   => Desc.Ancestor_Filename.all,
                  Kind              => Desc.Kind,
                  Scope             => Global_Scope,
                  Referred_Filename => Typedef.Buffer
                    (Typedef.File_Name.First .. Typedef.File_Name.Last),
                  Declaration_Info  => Decl_Info);
            end if;
         end if;
      end if;

      Insert_Reference
        (Declaration_Info     => Decl_Info,
         File                 => File,
         Location             => Ref.Position,
         Kind                 => Reference);

      Free (Typedef);

   exception
      when DB_Error | Not_Found  =>
         Fail ("unable to find typedef " & Ref_Id);
   end Fu_To_T_Handler;

   ------------------------
   --  Fu_To_Un_Handler  --
   ------------------------

   procedure Fu_To_Un_Handler
     (Ref     : TO_Table;
      Handler : access CPP_LI_Handler_Record'Class;
      File    : in out LI_File_Ptr;
      List    : in out LI_File_List;
      Project_View     : Prj.Project_Id;
      Module_Type_Defs : Module_Typedefs_List)
   is
      pragma Unreferenced (Project_View, Module_Type_Defs);
      Ref_Id : constant String := Ref.Buffer
        (Ref.Referred_Symbol_Name.First .. Ref.Referred_Symbol_Name.Last);
      Union_Desc : CType_Description;
      Union_Def  : UN_Table;
      Success    : Boolean;
      Decl_Info  : E_Declaration_Info_List;

   begin
      --  Info ("Fu_To_Un_Handler: " & Ref_Id);

      Find_Union
        (Type_Name      => Ref_Id,
         SN_Table       => Handler.SN_Table,
         Desc           => Union_Desc,
         Union_Def      => Union_Def,
         Success        => Success);

      if not Success then
         Fail ("unable to find union " & Ref_Id);
         return;
      end if;

      if not (Ref.Buffer (Ref.File_Name.First .. Ref.File_Name.Last)
         = Union_Def.Buffer (Union_Def.File_Name.First ..
                             Union_Def.File_Name.Last))
      then
         Decl_Info := Find_Dependency_Declaration
           (File        => File,
            Symbol_Name => Union_Def.Buffer
              (Union_Def.Name.First .. Union_Def.Name.Last),
            Kind        => Record_Type,
            Location    => Union_Def.Start_Position,
            Filename    => Union_Def.Buffer
              (Union_Def.File_Name.First .. Union_Def.File_Name.Last));

         if Decl_Info = null then
            Insert_Dependency_Declaration
              (Handler            => Handler,
               File               => File,
               List               => List,
               Symbol_Name        => Union_Def.Buffer
                 (Union_Def.Name.First .. Union_Def.Name.Last),
               Referred_Filename  => Union_Def.Buffer
                 (Union_Def.File_Name.First .. Union_Def.File_Name.Last),
               Location           => Union_Def.Start_Position,
               Kind               => Record_Type,
               Scope              => Global_Scope,
               Declaration_Info   => Decl_Info);
         end if;

      else
         Decl_Info := Find_Declaration
           (File        => File,
            Symbol_Name => Union_Def.Buffer
              (Union_Def.Name.First .. Union_Def.Name.Last),
            Kind        => Record_Type,
            Location    => Union_Def.Start_Position);

         if Decl_Info = null then
            Insert_Declaration
              (Handler            => Handler,
               File               => File,
               List               => List,
               Symbol_Name        => Union_Def.Buffer
                 (Union_Def.Name.First .. Union_Def.Name.Last),
               Source_Filename    => Ref.Buffer
                 (Ref.File_Name.First .. Ref.File_Name.Last),
               Location           => Union_Def.Start_Position,
               Kind               => Record_Type,
               Scope              => Global_Scope,
               Declaration_Info   => Decl_Info);
         end if;
      end if;

      Insert_Reference
        (File              => File,
         Declaration_Info  => Decl_Info,
         Location          => Ref.Position,
         Kind              => Reference);
      Free (Union_Def);
      Free (Union_Desc);
   end Fu_To_Un_Handler;

   --------------------
   -- Sym_CL_Handler --
   --------------------
   --  Note: this handler is called from many different functions

   procedure Sym_CL_Handler
     (Sym     : FIL_Table;
      Handler : access CPP_LI_Handler_Record'Class;
      File    : in out LI_File_Ptr;
      List    : in out LI_File_List;
      Project_View     : Prj.Project_Id;
      Module_Type_Defs : Module_Typedefs_List)
   is
      pragma Unreferenced (Project_View, Module_Type_Defs);
      Decl_Info  : E_Declaration_Info_List;
      Desc       : CType_Description;
      Class_Def  : CL_Table;
      Success    : Boolean;
      P          : Pair_Ptr;
      Super      : IN_Table;
      Super_Def  : CL_Table;
      Super_Desc : CType_Description;
   begin
      --  Info ("Sym_CL_Hanlder: """
      --        & Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last)
      --        & """");

      Find_Class
        (Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last),
         Handler.SN_Table,
         Desc,
         Class_Def,
         Success);

      if not Success then
         Warn ("Class not found: "
               & Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last));
         return;
      end if;

      Insert_Declaration
        (Handler               => Handler,
         File                  => File,
         List                  => List,
         Symbol_Name           =>
           Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last),
         Source_Filename       =>
           Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
         Location              => Sym.Start_Position,
         Kind                  => Record_Type,
         Scope                 => Global_Scope,
         End_Of_Scope_Location => Class_Def.End_Position,
         Declaration_Info      => Decl_Info);

      --  Adjust EOS reference kind
      Decl_Info.Value.Declaration.End_Of_Scope.Kind := End_Of_Spec;

      --  Find all the base classes for this one
      Set_Cursor
        (Handler.SN_Table (SN_IN),
         By_Key,
         --  Use name from Class_Def for it does not hold <> when
         --  template class is encountered
         Class_Def.Buffer (Class_Def.Name.First .. Class_Def.Name.Last)
            & Field_Sep,
         False);

      loop
         P := Get_Pair (Handler.SN_Table (SN_IN), Next_By_Key);
         exit when P = null;
         Super := Parse_Pair (P.all);
         --  Lookup base class definition to find its precise location
         Find_Class
           (Super.Buffer (Super.Base_Class.First .. Super.Base_Class.Last),
            Handler.SN_Table,
            Super_Desc,
            Super_Def,
            Success);
         if Success then -- if found, add it to parent list
            Add_Parent
              (Decl_Info,
               Handler => CPP_LI_Handler (Handler),
               List => List,
               Parent_Filename => Super_Def.Buffer
                 (Super_Def.File_Name.First .. Super_Def.File_Name.Last),
               Parent_Location => Super_Def.Start_Position);
            Free (Super_Desc);
            Free (Super_Def);
         end if;
         Free (Super);
         Free (P);
      end loop;

      Free (Desc);
      Free (Class_Def);
   exception
      when DB_Error => -- something went wrong, ignore it
         null;
   end Sym_CL_Handler;

   ---------------------
   -- Sym_CON_Handler --
   ---------------------
   --  NOTE: this handler is called from fu-to-con handler as well!!!

   procedure Sym_CON_Handler
     (Sym     : FIL_Table;
      Handler : access CPP_LI_Handler_Record'Class;
      File    : in out LI_File_Ptr;
      List    : in out LI_File_List;
      Project_View     : Prj.Project_Id;
      Module_Type_Defs : Module_Typedefs_List)
   is
      pragma Unreferenced (Project_View);
      Desc              : CType_Description;
      Var               : GV_Table;
      Success           : Boolean;
      Decl_Info         : E_Declaration_Info_List;
      Attributes        : SN_Attributes;
      Scope             : E_Scope := Global_Scope;
   begin
      --  Info ("Sym_CON_Handler: """
      --        & Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last)
      --        & """");

      if not Is_Open (Handler.SN_Table (CON)) then
         --  CON table does not exist, nothing to do ...
         return;
      end if;

      --  Lookup variable type
      Var := Find (Handler.SN_Table (CON), Sym.Buffer
         (Sym.Identifier.First .. Sym.Identifier.Last));
      Type_Name_To_Kind
        (Var.Buffer
           (Var.Value_Type.First .. Var.Value_Type.Last),
         Handler.SN_Table,
         Module_Type_Defs,
         Desc,
         Success);

      if not Success then -- type not found
         --  ?? Is ot OK to set E_Kind to Unresolved_Entity for global
         --  variables with unknown type?
         Desc.Kind := Unresolved_Entity;
      end if;

      Attributes := SN_Attributes (Var.Attributes);

      if (Attributes and SN_STATIC) = SN_STATIC then
         Scope := Static_Local;
      end if;

      if Desc.Parent_Point = Invalid_Point then
         Insert_Declaration
           (Handler           => Handler,
            File              => File,
            List              => List,
            Symbol_Name       =>
              Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last),
            Source_Filename   =>
              Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
            Location          => Sym.Start_Position,
            Kind              => Type_To_Object (Desc.Kind),
            Scope             => Scope,
            Declaration_Info  => Decl_Info);
      else
         Insert_Declaration
           (Handler           => Handler,
            File              => File,
            List              => List,
            Symbol_Name       =>
              Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last),
            Source_Filename   =>
              Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
            Location          => Sym.Start_Position,
            Kind              => Type_To_Object (Desc.Kind),
            Scope             => Scope,
            Parent_Location   => Desc.Parent_Point,
            Parent_Filename   => Desc.Parent_Filename.all,
            Declaration_Info  => Decl_Info);

            --  add reference to the type of this variable
         if Desc.IsTemplate then
            --  template specialization
            Refer_Type
              (Var.Buffer (Var.Value_Type.First .. Var.Value_Type.Last),
               Desc.Parent_Point,
               File,
               Sym.Start_Position,
               Instantiation_Reference);
         else
            --  default reference kind
            Refer_Type
              (Var.Buffer (Var.Value_Type.First .. Var.Value_Type.Last),
               Desc.Parent_Point,
               File,
               Sym.Start_Position);
         end if;

      end if;

      Free (Var);
      Free (Desc);
   exception
      when  DB_Error |   -- non-existent table
            Not_Found => -- no such variable
         null;           -- ignore error
   end Sym_CON_Handler;

   -------------------------
   -- Sym_Default_Handler --
   -------------------------
   --  This is default handler for symbols, which are not registered
   --  in Symbols_Handlers.

   procedure Sym_Default_Handler
     (Sym     : FIL_Table;
      Handler : access CPP_LI_Handler_Record'Class;
      File    : in out LI_File_Ptr;
      List    : in out LI_File_List;
      Project_View     : Prj.Project_Id;
      Module_Type_Defs : Module_Typedefs_List)
   is
      pragma Unreferenced
        (Sym, Handler, File, List, Project_View, Module_Type_Defs);
   begin
      --  Info ("Sym_Default_Hanlder: """
      --        & Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last)
      --        & """ : " & Symbol_Type'Image (Sym.Symbol));
      null;
   end Sym_Default_Handler;

   --------------------
   -- Sym_E_Handler --
   --------------------

   procedure Sym_E_Handler
     (Sym     : FIL_Table;
      Handler : access CPP_LI_Handler_Record'Class;
      File    : in out LI_File_Ptr;
      List    : in out LI_File_List;
      Project_View     : Prj.Project_Id;
      Module_Type_Defs : Module_Typedefs_List)
   is
      pragma Unreferenced (Project_View, Module_Type_Defs);
      Decl_Info : E_Declaration_Info_List;
      E_Id      : constant String := Sym.Buffer
        (Sym.Identifier.First .. Sym.Identifier.Last);

   begin
      --  Info ("Sym_E_Hanlder: """ & E_Id & """");

      Insert_Declaration
        (Handler           => Handler,
         File              => File,
         List              => List,
         Symbol_Name       => E_Id,
         Source_Filename   =>
           Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
         Location          => Sym.Start_Position,
         Kind              => Enumeration_Type,
         Scope             => Global_Scope,
         Declaration_Info  => Decl_Info);
   end Sym_E_Handler;

   --------------------
   -- Sym_EC_Handler --
   --------------------

   procedure Sym_EC_Handler
     (Sym     : FIL_Table;
      Handler : access CPP_LI_Handler_Record'Class;
      File    : in out LI_File_Ptr;
      List    : in out LI_File_List;
      Project_View     : Prj.Project_Id;
      Module_Type_Defs : Module_Typedefs_List)
   is
      pragma Unreferenced (Project_View, Module_Type_Defs);
      Decl_Info : E_Declaration_Info_List;
      Ec_Id     : constant String := Sym.Buffer
        (Sym.Identifier.First ..  Sym.Identifier.Last);
      Desc      : CType_Description;
      Has_Enum  : Boolean := False;

   begin
      --  Info ("Sym_EC_Hanlder: '" & Ec_Id & "'");

      --  looking for enum, which contains given enum constant (EC)
      if Is_Open (Handler.SN_Table (EC))
        and then Is_Open (Handler.SN_Table (E))
      then
         declare
            EC_Def : EC_Table := Find
              (Handler.SN_Table (EC), Ec_Id, Sym.Start_Position);
            E_Def  : E_Table;
         begin
            Find_Enum
              (EC_Def.Buffer
                 (EC_Def.Enumeration_Name.First ..
                  EC_Def.Enumeration_Name.Last),
               Handler.SN_Table, Desc, E_Def, Has_Enum);
            Free (E_Def);
            Free (EC_Def);
         exception
            when DB_Error | Not_Found => -- ignore
               Free (E_Def);
               Free (EC_Def);
         end;
      end if;

      if Has_Enum then -- corresponding enumeration found
         Insert_Declaration
           (Handler           => Handler,
            File              => File,
            List              => List,
            Symbol_Name       => Ec_Id,
            Source_Filename   =>
              Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
            Location          => Sym.Start_Position,
            Kind              => Enumeration_Literal,
            Parent_Location   => Desc.Parent_Point,
            Parent_Filename   => Desc.Parent_Filename.all,
            Scope             => Global_Scope,
            Declaration_Info  => Decl_Info);
      else
         Fail ("could not find enum for '" & Ec_Id & "'");
         Insert_Declaration
           (Handler           => Handler,
            File              => File,
            List              => List,
            Symbol_Name       => Ec_Id,
            Source_Filename   =>
              Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
            Location          => Sym.Start_Position,
            Kind              => Enumeration_Literal,
            Scope             => Global_Scope,
            Declaration_Info  => Decl_Info);
      end if;
   end Sym_EC_Handler;

   --------------------
   -- Sym_FD_Handler --
   --------------------

   procedure Sym_FD_Handler
     (Sym     : FIL_Table;
      Handler : access CPP_LI_Handler_Record'Class;
      File    : in out LI_File_Ptr;
      List    : in out LI_File_List;
      Project_View     : Prj.Project_Id;
      Module_Type_Defs : Module_Typedefs_List)
   is
      pragma Unreferenced (Project_View, Module_Type_Defs);
      Target_Kind  : E_Kind;
      Decl_Info    : E_Declaration_Info_List;
      P            : Pair_Ptr;
      First_FD_Pos : Point := Invalid_Point;
      FD_Tab       : FD_Table;
      FD_Tab_Tmp   : FD_Table;
      FU_Tab       : FU_Table;
      Attributes   : SN_Attributes;
      Is_Static    : Boolean;
      Match        : Boolean;
      FU_File      : LI_File_Ptr;
   begin
      --  Info ("Sym_FD_Hanlder: """
      --        & Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last)
      --        & """");

      --  Find this symbol
      FD_Tab := Find
        (Handler.SN_Table (FD),
         Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last),
         Sym.Start_Position,
         Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last));

      Attributes := SN_Attributes (FD_Tab.Attributes);
      Is_Static  := (Attributes and SN_STATIC) = SN_STATIC;

      Set_Cursor
        (Handler.SN_Table (FD),
         By_Key,
         Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last) & Field_Sep,
         False);

      loop
         P := Get_Pair (Handler.SN_Table (FD), Next_By_Key);
         exit when P = null;
         FD_Tab_Tmp := Parse_Pair (P.all);
         Free (P);
         --  Update position of the first forward declaration
         --  We have to compare prototypes of all global functions
         --  if this is a global function, or only local (static)
         --  ones if this is a static function
         Match := True;
         if Is_Static then
            Match := Match and
               Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last)
               = FD_Tab_Tmp.Buffer (FD_Tab_Tmp.File_Name.First ..
                                    FD_Tab_Tmp.File_Name.Last);
         end if;

         Match := Match and Cmp_Prototypes
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

      Assert (Fail_Stream, First_FD_Pos /= Invalid_Point, "DB inconsistency");

      if FD_Tab.Buffer (FD_Tab.Return_Type.First ..
                        FD_Tab.Return_Type.Last) = "void" then
         Target_Kind := Non_Generic_Procedure;
      else
         Target_Kind := Non_Generic_Function_Or_Operator;
      end if;

      Decl_Info := Find_Declaration
        (File        => File,
         Symbol_Name => Sym.Buffer
            (Sym.Identifier.First .. Sym.Identifier.Last),
         Location    => First_FD_Pos);

      if Decl_Info = null then
         Insert_Declaration
           (Handler           => Handler,
            File              => File,
            List              => List,
            Symbol_Name       =>
              Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last),
            Source_Filename   =>
              Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
            Location          => First_FD_Pos,
            Kind              => Target_Kind,
            Scope             => Global_Scope,
            Declaration_Info  => Decl_Info);
      end if;

      --  for all subsequent declarations, add reference to the first decl
      if Sym.Start_Position /= First_FD_Pos then
         Insert_Reference
           (Decl_Info,
            File,
            Sym.Start_Position,
            Reference);
      else -- for the first declaration we lookup body
         Set_Cursor
           (Handler.SN_Table (FU),
            By_Key,
            Sym.Buffer
               (Sym.Identifier.First .. Sym.Identifier.Last) & Field_Sep,
            False);
         Match := False;
         loop
            P := Get_Pair (Handler.SN_Table (FU), Next_By_Key);
            exit when P = null;
            FU_Tab := Parse_Pair (P.all);
            Free (P);
            Match := Cmp_Prototypes
               (FD_Tab.Buffer,
                FU_Tab.Buffer,
                FD_Tab.Arg_Types,
                FU_Tab.Arg_Types,
                FD_Tab.Return_Type,
                FU_Tab.Return_Type);
            exit when Match;
            Free (FU_Tab);
         end loop;
         if Match -- we found the body
            and then FU_Tab.Buffer (FU_Tab.File_Name.First ..
                                    FU_Tab.File_Name.Last)
               /= FD_Tab.Buffer (FD_Tab.File_Name.First ..
                                 FD_Tab.File_Name.Last)
               --  and it is in another file
         then
            FU_File := Locate_From_Source
               (List,
                FU_Tab.Buffer
                   (FU_Tab.File_Name.First .. FU_Tab.File_Name.Last));
            if FU_File = No_LI_File then
               Create_Stub_For_File
                 (LI            => FU_File,
                  Handler       => Handler,
                  List          => List,
                  Full_Filename => FU_Tab.Buffer
                     (FU_Tab.File_Name.First .. FU_Tab.File_Name.Last));
            end if;
            Insert_Reference
              (Decl_Info,
               FU_File,
               FU_Tab.Start_Position,
               Body_Entity);
            Set_End_Of_Scope (Decl_Info, FU_File, FU_Tab.End_Position);
         end if;
         if Match then
            Free (FU_Tab);
         end if;
      end if;
      Free (FD_Tab);
   exception
      when DB_Error | Not_Found =>
         Fail ("unable to find function " &
               Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last));
   end Sym_FD_Handler;

   --------------------
   -- Sym_FU_Handler --
   --------------------

   procedure Sym_FU_Handler
     (Sym     : FIL_Table;
      Handler : access CPP_LI_Handler_Record'Class;
      File    : in out LI_File_Ptr;
      List    : in out LI_File_List;
      Project_View     : Prj.Project_Id;
      Module_Type_Defs : Module_Typedefs_List)
   is
      Decl_Info      : E_Declaration_Info_List := null;
      Target_Kind    : E_Kind;

      P              : Pair_Ptr;
      FU_Tab         : FU_Table;
      MI_Tab         : MI_Table;
      Start_Position : constant Point := Sym.Start_Position;
      Body_Position  : Point := Invalid_Point;
      End_Position   : Point;
      Is_Template     : Boolean := False;
      Ref            : TO_Table;
      Our_Ref        : Boolean;

      Fu_Id          : constant String := Sym.Buffer
        (Sym.Identifier.First .. Sym.Identifier.Last);

   begin
      --  Info ("Sym_FU_Hanlder: """
      --        & Sym.Buffer (Sym.Class.First .. Sym.Class.Last) & "."
      --        & Fu_Id
      --        & """");

      if Sym.Symbol = MI then
         declare
            Class_Def       : CL_Table;
            Class_Decl_Info : E_Declaration_Info_List;
            Ref             : E_Reference_List;
         begin
            MI_Tab := Find (Handler.SN_Table (MI),
                Sym.Buffer (Sym.Class.First .. Sym.Class.Last),
                Fu_Id,
                Sym.Start_Position,
                Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last));
            begin -- check if this class is template
               Class_Def := Find
                 (Handler.SN_Table (CL),
                  Sym.Buffer (Sym.Class.First .. Sym.Class.Last));
               Is_Template := Class_Def.Template_Parameters.First
                  < Class_Def.Template_Parameters.Last;
               --  We want to add a reference to the class we belong to
               --  but there may be already a reference created in the
               --  MD handler. If MD and MI instances are the same we
               --  don't need to duplicate references
               Find_Or_Create_Class
                  (Handler,
                   Class_Def,
                   Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
                   Class_Decl_Info,
                   File,
                   List,
                   Project_View,
                   Module_Type_Defs);
               if Class_Decl_Info /= null then
                  Ref := Class_Decl_Info.Value.References;
                  loop
                     exit when Ref = null
                        or else (Get_LI_Filename (Ref.Value.Location.File.LI)
                             = Get_LI_Filename (File)
                           and then Ref.Value.Location.Line
                              = Sym.Start_Position.Line
                           and then Ref.Value.Location.Column
                              = Sym.Start_Position.Column
                           and then Ref.Value.Kind = Reference);
                     Ref := Ref.Next;
                  end loop;
                  if Ref = null then
                     Insert_Reference
                       (Class_Decl_Info,
                        File,
                        (Sym.Start_Position.Line, 0),
                        --  we don't know the precise position of the class
                        --  name, so set the column to "anywhere"
                        Reference);
                  end if;
               end if;
               Free (Class_Def);
            exception
               when DB_Error | Not_Found =>
                  null;
            end;
            if MI_Tab.Buffer (MI_Tab.Return_Type.First ..
                              MI_Tab.Return_Type.Last) = "void" then
               if Is_Template then
                  Target_Kind := Generic_Procedure;
               else
                  Target_Kind := Non_Generic_Procedure;
               end if;
            else
               if Is_Template then
                  Target_Kind := Generic_Function_Or_Operator;
               else
                  Target_Kind := Non_Generic_Function_Or_Operator;
               end if;
            end if;
            End_Position := MI_Tab.End_Position;
         exception
            when DB_Error | Not_Found =>
               Fail ("unable to find method "
                     & Sym.Buffer (Sym.Class.First .. Sym.Class.Last) & "."
                     & Fu_Id);
               return;
         end;
      else
         begin
            FU_Tab := Find
              (Handler.SN_Table (FU),
               Fu_Id,
               Sym.Start_Position,
               Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last));
            if FU_Tab.Buffer (FU_Tab.Return_Type.First ..
                              FU_Tab.Return_Type.Last) = "void" then
               Target_Kind := Non_Generic_Procedure;
            else
               Target_Kind := Non_Generic_Function_Or_Operator;
            end if;
            End_Position := FU_Tab.End_Position;
         exception
            when DB_Error | Not_Found =>
               Fail ("unable to find function " & Fu_Id);
               return;
         end;
      end if;

      --  Detect forward declaration. If there are many declarations
      --  we should not try do interpret them, 'cause it may be
      --  overloading.
      --  If exist only one, Start_Position
      --  should point to it and we have to add Body_Entity reference
      --  Otherwise Start_Position should point directly to the body.
      --  We should also try to find GPS declaration created during
      --  FD processing and not create new declaration.
      if Sym.Symbol = MI then
         Find_First_Forward_Declaration
           (MI_Tab.Buffer,
            MI_Tab.Class,
            MI_Tab.Name,
            Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
            MI_Tab.Return_Type,
            MI_Tab.Arg_Types,
            Handler,
            File,
            List,
            Project_View,
            Module_Type_Defs,
            Decl_Info);
         if Decl_Info /= null then -- Body_Entity is inserted only w/ fwd decl
            Body_Position := Sym.Start_Position;
         end if;
      else
         --  Try to find forward declaration
         Find_First_Forward_Declaration
           (FU_Tab.Buffer,
            FU_Tab.Name,
            Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
            FU_Tab.Return_Type,
            FU_Tab.Arg_Types,
            Handler,
            File,
            List,
            Decl_Info);
         if Decl_Info /= null then -- Body_Entity is inserted only w/ fwd decl
            Body_Position := Sym.Start_Position;
         end if;
      end if;

      if Decl_Info = null then
         Insert_Declaration
           (Handler               => Handler,
            File                  => File,
            List                  => List,
            Symbol_Name           => Fu_Id,
            Source_Filename       =>
              Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
            Location              => Start_Position,
            Kind                  => Target_Kind,
            Scope                 => Global_Scope,
            End_Of_Scope_Location => End_Position,
            Declaration_Info      => Decl_Info);
      else
         Set_End_Of_Scope (Decl_Info, File, End_Position);
      end if;

      if Body_Position /= Invalid_Point then
         Insert_Reference
           (Decl_Info,
            File,
            Body_Position,
            Body_Entity);
      end if;

      --  Declaration inserted. Now we need to check the body for usage
      --  of objects of all kinds

      Set_Cursor
        (Handler.SN_Table (TO),
         Position => By_Key,
         Key => Sym.Buffer (Sym.Class.First .. Sym.Class.Last) & Field_Sep &
                Fu_Id &
                Field_Sep & To_String (Sym.Symbol),
         Exact_Match => False);

      loop
         P := Get_Pair (Handler.SN_Table (TO), Next_By_Key);
         exit when P = null;

         begin
            Ref := Parse_Pair (P.all);
            if Sym.Symbol = MI then
               Our_Ref := Cmp_Arg_Types
                 (Ref.Buffer,
                  MI_Tab.Buffer,
                  Ref.Caller_Argument_Types,
                  MI_Tab.Arg_Types);
            else
               Our_Ref := Cmp_Arg_Types
                 (Ref.Buffer,
                  FU_Tab.Buffer,
                  Ref.Caller_Argument_Types,
                  FU_Tab.Arg_Types);
            end if;

            if Our_Ref
              and then Fu_To_Handlers (Ref.Referred_Symbol) /= null
            then
               Fu_To_Handlers (Ref.Referred_Symbol)
                 (Ref, Handler, File, List, Project_View, Module_Type_Defs);
            end if;
            Free (Ref);

         exception
            when others =>
               --  unexpected exception in Fu_To_XX handler
               Free (Ref);
               --  ??? Probably we want to report this exception and
               --  continue to work further, but now we reraise that
               --  exception
               raise;
         end;

         Free (P);
      end loop;

      if Sym.Symbol = MI then
         Free (MI_Tab);
      else
         Free (FU_Tab);
      end if;

   exception
      when DB_Error => null; -- non-existent table .to, ignore it
   end Sym_FU_Handler;

   --------------------
   -- Sym_GV_Handler --
   --------------------
   --  NOTE: this handler is called from fu-to-gv handler as well

   procedure Sym_GV_Handler
     (Sym     : FIL_Table;
      Handler : access CPP_LI_Handler_Record'Class;
      File    : in out LI_File_Ptr;
      List    : in out LI_File_List;
      Project_View     : Prj.Project_Id;
      Module_Type_Defs : Module_Typedefs_List)
   is
      pragma Unreferenced (Project_View);
      Desc              : CType_Description;
      Var               : GV_Table;
      Success           : Boolean;
      Decl_Info         : E_Declaration_Info_List;
      Attributes        : SN_Attributes;
      Scope             : E_Scope := Global_Scope;

   begin
      --  Info ("Sym_GV_Handler: """
      --        & Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last)
      --        & """");

      if not Is_Open (Handler.SN_Table (GV)) then
         --  GV table does not exist, nothing to do ...
         return;
      end if;

      --  Lookup variable type
      Var := Find (Handler.SN_Table (GV), Sym.Buffer
         (Sym.Identifier.First .. Sym.Identifier.Last));

      Type_Name_To_Kind
        (Var.Buffer
           (Var.Value_Type.First .. Var.Value_Type.Last),
         Handler.SN_Table,
         Module_Type_Defs,
         Desc,
         Success);

      if not Success then -- type not found
         --  ?? Is ot OK to set E_Kind to Unresolved_Entity for global vars
         --  with unknown type?
         Desc.Kind := Unresolved_Entity;
      end if;

      Attributes := SN_Attributes (Var.Attributes);

      if (Attributes and SN_STATIC) = SN_STATIC then
         Scope := Static_Local;
      end if;

      if Desc.Parent_Point = Invalid_Point then
         Insert_Declaration
           (Handler           => Handler,
            File              => File,
            List              => List,
            Symbol_Name       =>
              Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last),
            Source_Filename   =>
              Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
            Location          => Sym.Start_Position,
            Kind              => Type_To_Object (Desc.Kind),
            Scope             => Scope,
            Declaration_Info  => Decl_Info);
      else
         Insert_Declaration
           (Handler           => Handler,
            File              => File,
            List              => List,
            Symbol_Name       =>
              Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last),
            Source_Filename   =>
              Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
            Location          => Sym.Start_Position,
            Kind              => Type_To_Object (Desc.Kind),
            Scope             => Scope,
            Parent_Location   => Desc.Parent_Point,
            Parent_Filename   => Desc.Parent_Filename.all,
            Declaration_Info  => Decl_Info);

         --  add reference to the type of this variable
         if Desc.IsTemplate then
            --  template specialization
            Refer_Type
              (Var.Buffer (Var.Value_Type.First .. Var.Value_Type.Last),
               Desc.Parent_Point,
               File,
               Sym.Start_Position,
               Instantiation_Reference);
         else
            --  default reference kind
            Refer_Type
              (Var.Buffer (Var.Value_Type.First .. Var.Value_Type.Last),
               Desc.Parent_Point,
               File,
               Sym.Start_Position);
         end if;
      end if;

      Free (Var);
      Free (Desc);
   exception
      when  DB_Error |   -- non-existent table
            Not_Found => -- no such variable
         null;           -- ignore error
   end Sym_GV_Handler;

   --------------------
   -- Sym_IU_Handler --
   --------------------

   procedure Sym_IU_Handler
     (Sym     : FIL_Table;
      Handler : access CPP_LI_Handler_Record'Class;
      File    : in out LI_File_Ptr;
      List    : in out LI_File_List;
      Project_View     : Prj.Project_Id;
      Module_Type_Defs : Module_Typedefs_List)
   is
      pragma Unreferenced (Module_Type_Defs);

      --  ??? We shouldn't use Base_Name below, but should allow find file to
      --  recognize directories in the name.
      Full_Included : constant String := Find_File
        (Base_Name (Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last)),
         Include_Path (Project_View, Recursive => True),
         Predefined_Path => "");

   begin
      --  Info ("Sym_IU_Handler: """
      --        & Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last)
      --        & " depends on " & Full_Included
      --        & """");
      if Full_Included = "" then
         Info ("File not found on path: "
               & Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last));
         Insert_Dependency
           (Handler           => Handler,
            File              => File,
            List              => List,
            Referred_Filename => Sym.Buffer
              (Sym.Identifier.First .. Sym.Identifier.Last));
      else
         Insert_Dependency
           (Handler           => Handler,
            File              => File,
            List              => List,
            Referred_Filename => Full_Included);
      end if;
   end Sym_IU_Handler;

   --------------------
   -- Sym_IV_Handler --
   --------------------

   procedure Sym_IV_Handler
     (Sym     : FIL_Table;
      Handler : access CPP_LI_Handler_Record'Class;
      File    : in out LI_File_Ptr;
      List    : in out LI_File_List;
      Project_View     : Prj.Project_Id;
      Module_Type_Defs : Module_Typedefs_List)
   is
      pragma Unreferenced (Project_View);
      Inst_Var        : IV_Table;
      Decl_Info       : E_Declaration_Info_List;
      Success         : Boolean;
      Desc            : CType_Description;
   begin
      --  Info ("Sym_IV_Handler: """
      --        & Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last)
      --        & """");

      if not Is_Open (Handler.SN_Table (IV)) then
         --  IV table does not exist, nothing to do ...
         return;
      end if;

      --  Lookup instance variable
      Inst_Var := Find
        (Handler.SN_Table (IV),
         Sym.Buffer (Sym.Class.First .. Sym.Class.Last),
         Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last));

      --  Determine its type
      Type_Name_To_Kind
        (Inst_Var.Buffer
           (Inst_Var.Value_Type.First .. Inst_Var.Value_Type.Last),
         Handler.SN_Table,
         Module_Type_Defs,
         Desc,
         Success);

      if not Success then -- failed to determine type
         --  if the variable belongs to a template, the unknown type
         --  may be template parameter. Check it.
         --  TODO Here we should parse class template arguments and
         --  locate the type in question. Not implemented yet
         Desc.Kind           := Private_Type;
         Desc.IsVolatile     := False;
         Desc.IsConst        := False;
         Desc.Parent_Point   := Invalid_Point;
         Desc.Ancestor_Point := Invalid_Point;
         Desc.Builtin_Name   := null;
         --  Free (Inst_Var);
         --  return;
      end if;

      if Desc.Parent_Point = Invalid_Point then
         Insert_Declaration
           (Handler           => Handler,
            File              => File,
            List              => List,
            Symbol_Name       =>
              Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last),
            Source_Filename   =>
              Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
            Location          => Sym.Start_Position,
            Kind              => Type_To_Object (Desc.Kind),
            Scope             => Local_Scope,
            Declaration_Info  => Decl_Info);
      else
         Insert_Declaration
           (Handler           => Handler,
            File              => File,
            List              => List,
            Symbol_Name       =>
              Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last),
            Source_Filename   =>
              Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
            Location          => Sym.Start_Position,
            Kind              => Desc.Kind,
            Scope             => Local_Scope,
            Parent_Location   => Desc.Parent_Point,
            Parent_Filename   => Desc.Parent_Filename.all,
            Declaration_Info  => Decl_Info);

            --  add reference to the type of this field
         Refer_Type
           (Inst_Var.Buffer
              (Inst_Var.Value_Type.First .. Inst_Var.Value_Type.Last),
            Desc.Parent_Point,
            File,
            Sym.Start_Position);
      end if;

      Free (Desc);
      Free (Inst_Var);
   exception
      when  DB_Error |   -- non-existent table
            Not_Found => -- no such variable
         null;           -- ignore error
   end Sym_IV_Handler;

   --------------------
   -- Sym_MA_Handler --
   --------------------

   procedure Sym_MA_Handler
     (Sym     : FIL_Table;
      Handler : access CPP_LI_Handler_Record'Class;
      File    : in out LI_File_Ptr;
      List    : in out LI_File_List;
      Project_View     : Prj.Project_Id;
      Module_Type_Defs : Module_Typedefs_List)
   is
      pragma Unreferenced (Project_View, Module_Type_Defs);
      tmp_ptr    : E_Declaration_Info_List;
   begin
      --  Info ("Sym_MA_Handler: """
      --        & Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last)
      --        & """");

      Insert_Declaration
        (Handler           => Handler,
         File              => File,
         List              => List,
         Symbol_Name       =>
           Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last),
         Source_Filename   =>
           Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
         Location          => Sym.Start_Position,
         Kind              => Unresolved_Entity,
         Scope             => Global_Scope,
         Declaration_Info  => tmp_ptr);
   end Sym_MA_Handler;

   --------------------
   -- Sym_MD_Handler --
   --------------------

   procedure Sym_MD_Handler
     (Sym     : FIL_Table;
      Handler : access CPP_LI_Handler_Record'Class;
      File    : in out LI_File_Ptr;
      List    : in out LI_File_List;
      Project_View     : Prj.Project_Id;
      Module_Type_Defs : Module_Typedefs_List)
   is
      Target_Kind  : E_Kind;
      Decl_Info    : E_Declaration_Info_List;
      P            : Pair_Ptr;
      First_MD_Pos : Point := Invalid_Point;
      MD_Tab       : MD_Table;
      MI_Tab       : MI_Table;
      MD_Tab_Tmp   : MD_Table;
      Is_Template  : Boolean := False;
      Found        : Boolean;
      MI_File      : LI_File_Ptr;
      use DB_Structures.Segment_Vector;

   begin
      --  Info ("Sym_MD_Hanlder: """
      --      & Sym.Buffer (Sym.Class.First .. Sym.Class.Last) & "::"
      --      & Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last) & " "
      --      & Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last)
      --      & """");

      --  Find this symbol
      MD_Tab := Find
        (Handler.SN_Table (MD),
         Sym.Buffer (Sym.Class.First .. Sym.Class.Last),
         Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last),
         Sym.Start_Position,
         Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last));

      Set_Cursor
        (Handler.SN_Table (MD),
         By_Key,
         Sym.Buffer (Sym.Class.First .. Sym.Class.Last) & Field_Sep
         & Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last)
         & Field_Sep,
         False);

      loop
         P := Get_Pair (Handler.SN_Table (MD), Next_By_Key);
         exit when P = null;
         MD_Tab_Tmp := Parse_Pair (P.all);
         Free (P);
         --  Update position of the first forward declaration
         if Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last)
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

      Assert (Fail_Stream, First_MD_Pos /= Invalid_Point, "DB inconsistency");

      declare
         Class_Def          : CL_Table;
         Class_Decl_Info    : E_Declaration_Info_List;
      begin -- check if this class is template
         Class_Def := Find
           (Handler.SN_Table (CL),
            Sym.Buffer (Sym.Class.First .. Sym.Class.Last));
         Is_Template := Class_Def.Template_Parameters.First
            < Class_Def.Template_Parameters.Last;
         --  Add reference to the class we belong to
         Find_Or_Create_Class
           (Handler,
            Class_Def,
            Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
            Class_Decl_Info, File, List, Project_View, Module_Type_Defs);
         if Class_Decl_Info /= null then
            Insert_Reference
              (Class_Decl_Info,
               File,
               (Sym.Start_Position.Line, 0),
               --  we don't know the precise position of the class
               --  name, so set the column to "anywhere"
               Reference);
         end if;
         Free (Class_Def);
      exception
         when DB_Error | Not_Found =>
            null;
      end;

      if MD_Tab.Buffer (MD_Tab.Return_Type.First ..
                        MD_Tab.Return_Type.Last) = "void" then
         if Is_Template then
            Target_Kind := Generic_Procedure;
         else
            Target_Kind := Non_Generic_Procedure;
         end if;
      else
         if Is_Template then
            Target_Kind := Generic_Function_Or_Operator;
         else
            Target_Kind := Non_Generic_Function_Or_Operator;
         end if;
      end if;

      Decl_Info := Find_Declaration
        (File        => File,
         Symbol_Name => Sym.Buffer
            (Sym.Identifier.First .. Sym.Identifier.Last),
         Location    => First_MD_Pos);

      if Decl_Info = null then
         Insert_Declaration
           (Handler           => Handler,
            File              => File,
            List              => List,
            Symbol_Name       =>
               Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last),
            Source_Filename   =>
              Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
            Location          => First_MD_Pos,
            Kind              => Target_Kind,
            Scope             => Global_Scope,
            Declaration_Info  => Decl_Info);
      end if;

      --  for all subsequent declarations, add reference to the first decl
      if Sym.Start_Position /= First_MD_Pos then
         Insert_Reference
           (Declaration_Info => Decl_Info,
            File             => File,
            Location         => Sym.Start_Position,
            Kind             => Reference);
      else -- for the first declaration we lookup body
         Set_Cursor
           (Handler.SN_Table (MI),
            By_Key,
            Sym.Buffer (Sym.Class.First .. Sym.Class.Last) & Field_Sep
               & Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last)
               & Field_Sep,
            False);
         Found := False;
         loop
            P := Get_Pair (Handler.SN_Table (MI), Next_By_Key);
            exit when P = null;
            MI_Tab := Parse_Pair (P.all);
            Free (P);
            Found := Cmp_Prototypes
               (MD_Tab.Buffer,
                MI_Tab.Buffer,
                MD_Tab.Arg_Types,
                MI_Tab.Arg_Types,
                MD_Tab.Return_Type,
                MI_Tab.Return_Type);
            exit when Found;
            Free (MI_Tab);
         end loop;
         if Found -- we found the body
            and then MI_Tab.Buffer (MI_Tab.File_Name.First ..
                                    MI_Tab.File_Name.Last)
               /= MD_Tab.Buffer (MD_Tab.File_Name.First ..
                                 MD_Tab.File_Name.Last)
               --  and it is in another file
         then
            MI_File := Locate_From_Source
               (List,
                MI_Tab.Buffer
                   (MI_Tab.File_Name.First .. MI_Tab.File_Name.Last));
            if MI_File = No_LI_File then
               Create_Stub_For_File
                 (LI            => MI_File,
                  Handler       => Handler,
                  List          => List,
                  Full_Filename => MI_Tab.Buffer
                     (MI_Tab.File_Name.First .. MI_Tab.File_Name.Last));
            end if;
            if MI_File /= File
               or else MD_Tab.Start_Position /= MI_Tab.Start_Position then
               Insert_Reference
                 (Decl_Info,
                  MI_File,
                  MI_Tab.Start_Position,
                  Body_Entity);
            end if;
            Set_End_Of_Scope (Decl_Info, MI_File, MI_Tab.End_Position);
         end if;
         if Found then
            Free (MI_Tab);
         end if;
      end if;
      Free (MD_Tab);
   exception
      when DB_Error | Not_Found =>
         Fail ("unable to find method " &
               Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last));
   end Sym_MD_Handler;

   --------------------
   -- Sym_T_Handler --
   --------------------

   procedure Sym_T_Handler
     (Sym     : FIL_Table;
      Handler : access CPP_LI_Handler_Record'Class;
      File    : in out LI_File_Ptr;
      List    : in out LI_File_List;
      Project_View     : Prj.Project_Id;
      Module_Type_Defs : Module_Typedefs_List)
   is
      pragma Unreferenced (Project_View);
      Decl_Info  : E_Declaration_Info_List;
      Desc       : CType_Description;
      Success    : Boolean;
      Identifier : constant String :=
        Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last);

   begin
      --  Info ("Sym_T_Hanlder: """ & Identifier & """");

      if not Is_Open (Handler.SN_Table (T)) then
         --  .t table does not exist, nothing to do
         return;
      end if;

      --  find original type for given typedef
      Find_Original_Type
        (Identifier,
         Handler.SN_Table,
         Module_Type_Defs,
         Desc,
         Success);

      if Success then
         --  we know E_Kind for original type
         --  Ancestor_Point and Ancestor_Filename has information about
         --  parent type (do not mess with Parent_xxx in CType_Description)

         if Desc.Ancestor_Point = Invalid_Point then
            --  unknown parent
            Insert_Declaration
              (Handler           => Handler,
               File              => File,
               List              => List,
               Symbol_Name       => Identifier,
               Source_Filename   =>
                 Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
               Location          => Sym.Start_Position,
               Kind              => Desc.Kind,
               Scope             => Global_Scope,
               Declaration_Info  => Decl_Info);

         elsif Desc.Ancestor_Point = Predefined_Point then
            --  parent type is builtin: set parent location to predefined one
            --  ??? Builtin_Name is not used anywhere. We should
            --  use it (e.g. for a field like Predefined_Type_Name)
            Insert_Declaration
              (Handler           => Handler,
               File              => File,
               List              => List,
               Symbol_Name       => Identifier,
               Source_Filename   =>
                 Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
               Location          => Sym.Start_Position,
               Parent_Location   => Predefined_Point,
               Kind              => Desc.Kind,
               Scope             => Global_Scope,
               Declaration_Info  => Decl_Info);

         else
            --  Set parent location to ancestor location
            Insert_Declaration
              (Handler           => Handler,
               File              => File,
               List              => List,
               Symbol_Name       => Identifier,
               Source_Filename   =>
                 Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
               Location          => Sym.Start_Position,
               Parent_Filename   => Desc.Ancestor_Filename.all,
               Parent_Location   => Desc.Ancestor_Point,
               Kind              => Desc.Kind,
               Scope             => Global_Scope,
               Declaration_Info  => Decl_Info);

         end if;

      else
         --  could not get E_Kind for the original type
         Fail ("unable to find type for typedef " & Identifier);
      end if;

      Free (Desc);
   end Sym_T_Handler;

   --------------------
   -- Sym_UN_Handler --
   --------------------

   procedure Sym_UN_Handler
     (Sym     : FIL_Table;
      Handler : access CPP_LI_Handler_Record'Class;
      File    : in out LI_File_Ptr;
      List    : in out LI_File_List;
      Project_View     : Prj.Project_Id;
      Module_Type_Defs : Module_Typedefs_List)
   is
      pragma Unreferenced (Project_View, Module_Type_Defs);
      Decl_Info : E_Declaration_Info_List;
      Desc      : CType_Description;
      Union_Def : UN_Table;
      Success   : Boolean;

   begin
      --  Info ("Sym_UN_Hanlder: """
      --        & Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last)
      --        & """");

      Find_Union
        (Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last),
         Handler.SN_Table,
         Desc,
         Union_Def,
         Success);

      if Success then
         Insert_Declaration
           (Handler               => Handler,
            File                  => File,
            List                  => List,
            Symbol_Name           =>
              Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last),
            Source_Filename       =>
              Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
            Location              => Sym.Start_Position,
            Kind                  => Record_Type,
            Scope                 => Global_Scope,
            End_Of_Scope_Location => Union_Def.End_Position,
            Declaration_Info      => Decl_Info);

         Insert_Reference
           (Declaration_Info      => Decl_Info,
            File                  => File,
            Location              => Union_Def.End_Position,
            Kind                  => End_Of_Spec);

         Free (Desc);
         Free (Union_Def);
      end if;
   end Sym_UN_Handler;

   ----------------
   -- Get_DB_Dir --
   ----------------

   function Get_DB_Dir (Handler : access CPP_LI_Handler_Record)
      return String is
   begin
      return Handler.DB_Dir.all;
   end Get_DB_Dir;

   ---------------
   -- Get_Xrefs --
   ---------------

   function Get_Xrefs (Handler : access CPP_LI_Handler_Record)
      return SN.Xref_Pools.Xref_Pool is
   begin
      return Handler.Xrefs;
   end Get_Xrefs;

end Src_Info.CPP;

