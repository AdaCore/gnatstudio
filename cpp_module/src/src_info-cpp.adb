-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2002-2003                       --
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

with Projects;             use Projects;
with Projects.Registry;    use Projects.Registry;
with Src_Info;             use Src_Info;
with Src_Info.LI_Utils;    use Src_Info.LI_Utils;
with Src_Info.Type_Utils;  use Src_Info.Type_Utils;

with DB_API;            use DB_API;

with SN;                use SN;
with SN.DB_Structures;  use SN.DB_Structures;
with SN.Find_Fns;       use SN.Find_Fns;
with SN.Browse;
with SN.Xref_Pools;     use SN.Xref_Pools;

with Types; use Types;
with Traces;  use Traces;
with String_Utils; use String_Utils;
with Snames; use Snames;
with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;
with VFS; use VFS;

package body Src_Info.CPP is

   DBIMP    : constant String := "dbimp";
   --  SN database engine

   CBrowser : constant String := "cbrowser";
   --  SN C and C++ parser

   Info_Stream : constant Debug_Handle := Create ("CPP.Info");
   Warn_Stream : constant Debug_Handle := Create ("CPP.Warn");
   Fail_Stream : constant Debug_Handle := Create ("CPP.Fail");

   Enumeration_Kind_Entity : constant E_Kind :=
     (Enumeration_Kind,
      Is_Type     => True,
      Is_Generic  => False,
      Is_Abstract => False);
   Non_Generic_Class : constant E_Kind :=
     (Class,
      Is_Type     => True,
      Is_Generic  => False,
      Is_Abstract => False);

   procedure Insert_Dependency
     (Handler              : access Src_Info.CPP.CPP_LI_Handler_Record'Class;
      File                 : in out LI_File_Ptr;
      List                 : LI_File_List;
      Referred_Filename    : VFS.Virtual_File);
   --  Create a new dependency, from the files described in File to the source
   --  file Referred_Filename.

   procedure Insert_Dependency_Declaration
     (Handler               : access Src_Info.CPP.CPP_LI_Handler_Record'Class;
      File                  : in out LI_File_Ptr;
      List                  : LI_File_List;
      Symbol_Name           : String;
      Referred_Filename     : VFS.Virtual_File;
      Location              : SN.Point;
      Parent_Filename       : VFS.Virtual_File := VFS.No_File;
      Parent_Location       : SN.Point := SN.Invalid_Point;
      Kind                  : E_Kind;
      Scope                 : E_Scope;
      End_Of_Scope_Location : SN.Point := SN.Invalid_Point;
      Rename_Location       : SN.Point := SN.Invalid_Point;
      Declaration_Info      : out E_Declaration_Info_List);
   --  Inserts new dependency declaration with specified parameters
   --  to given LI structure tree.
   --  Throws Parent_Not_Available exception if LI_Structure for the
   --  file with parent is not created yet.
   --  (Parent_Filename, Parent_Location) is the location of the declaration
   --  for the parent entity, if available.

   procedure Insert_Declaration
     (File                  : in out LI_File_Ptr;
      List                  : LI_File_List;
      Symbol_Name           : String;
      Location              : SN.Point;
      Parent_Filename       : VFS.Virtual_File := VFS.No_File;
      Parent_Location       : SN.Point := SN.Invalid_Point;
      Kind                  : E_Kind;
      Scope                 : E_Scope;
      End_Of_Scope_Location : SN.Point := SN.Invalid_Point;
      Rename_Location       : SN.Point := SN.Invalid_Point;
      Declaration_Info      : out E_Declaration_Info_List);
   --  Insert a new entity declaration in File. File is created if needed, as
   --  well as the entry for Source_Filename (Body_Part).
   --  The newly created declaration is returned in Declaration_Info.
   --  (Parent_Filename, Parent_Location) points to the declaration of the
   --  parent entity, when available (classes, subtypes, ...), and should be
   --  left to the default value if not available.
   --
   --  ??? Rename_Location is currently ignored.
   --
   --  This subprogram raises Parent_Not_Available if the LI_Structure for the
   --  parent entity could not be found.
   --  ??? Shouldn't we create a stub LI file for the parent instead.

   procedure Insert_Declaration
     (D_Ptr                   : in out E_Declaration_Info_List;
      File                    : LI_File_Ptr;
      List                    : LI_File_List;
      Symbol_Name             : String;
      Location                : SN.Point;
      Parent_Filename         : VFS.Virtual_File := VFS.No_File;
      Parent_Location         : SN.Point := SN.Invalid_Point;
      Kind                    : E_Kind;
      Scope                   : E_Scope;
      End_Of_Scope_Location   : SN.Point := SN.Invalid_Point;
      Rename_Location         : SN.Point := SN.Invalid_Point);
   --  Inserts declaration into specified E_Declaration_Info_List

   procedure Add_Parent
     (Declaration_Info : in out E_Declaration_Info_List;
      Handler          : Src_Info.CPP.CPP_LI_Handler;
      List             : LI_File_List;
      Parent_Filename  : VFS.Virtual_File;
      Parent_Location  : SN.Point);
   --  Add a new parent entity to the list of parents for
   --  Declaration_Info. This is mostly used for multiple-inheritance.

   procedure Create_Stub_For_File
     (LI            : out LI_File_Ptr;
      Handler       : access Src_Info.CPP.CPP_LI_Handler_Record'Class;
      List          : LI_File_List;
      Full_Filename : VFS.Virtual_File);
   --  Create a stub LI file for Full_Filename, if there is no matching LI file
   --  in List.
   --  If Parsed is True, the LI file will be considered as already parsed,
   --  even though no entity will be declared for it.

   procedure Insert_Dependency
     (Handler              : access Src_Info.CPP.CPP_LI_Handler_Record'Class;
      File                 : in out LI_File_Ptr;
      List                 : LI_File_List;
      Referred_Filename    : VFS.Virtual_File;
      Referred_LI          : out LI_File_Ptr;
      Dep_Ptr              : out Dependency_File_Info_List);
   --  Same as the procedure with the same name, but also returns the newly
   --  inserted dependency.

   -----------------------
   -- Insert_Dependency --
   -----------------------

   procedure Insert_Dependency
     (Handler              : access Src_Info.CPP.CPP_LI_Handler_Record'Class;
      File                 : in out LI_File_Ptr;
      List                 : LI_File_List;
      Referred_Filename    : VFS.Virtual_File)
   is
      Dep_Ptr : Dependency_File_Info_List;
      Referred_LI : LI_File_Ptr;
   begin
      Insert_Dependency
        (Handler, File, List, Referred_Filename, Referred_LI, Dep_Ptr);
   end Insert_Dependency;

   -----------------------
   -- Insert_Dependency --
   -----------------------

   procedure Insert_Dependency
     (Handler              : access Src_Info.CPP.CPP_LI_Handler_Record'Class;
      File                 : in out LI_File_Ptr;
      List                 : LI_File_List;
      Referred_Filename    : VFS.Virtual_File;
      Referred_LI          : out LI_File_Ptr;
      Dep_Ptr              : out Dependency_File_Info_List)
   is
      Set_Contents : Boolean := False;
   begin
      --  Now we are searching through common list of LI_Files and
      --  trying to locate file with given name. If not found we are
      --  inserting new dependency

      Create_Stub_For_File
        (LI            => Referred_LI,
         Handler       => CPP_LI_Handler (Handler),
         List          => List,
         Full_Filename => Referred_Filename);

      Assert (Fail_Stream, File.LI.Body_Info.Source_Filename.all /=
              Base_Name (Referred_Filename),
              "Can't insert dependency, LI file "
              & Base_Name (Referred_LI.LI.LI_Filename)
              & " is already for file "
              & Full_Name (Referred_Filename));

      --  Is this a first dependencies info in this file?

      if File.LI.Dependencies_Info = null then
         File.LI.Dependencies_Info := new Dependency_File_Info_Node;
         Dep_Ptr := File.LI.Dependencies_Info;
         Set_Contents := True;

      else
         --  Try to locate Dependency_File_Info with given Source_Filename

         Dep_Ptr := File.LI.Dependencies_Info;

         while Get_Source_Filename (Dep_Ptr.Value.File) /=
           Referred_Filename
         loop
            if Dep_Ptr.Next = null then
               --  Unable to find suitable Dependency_File_Info.
               --  Creating a new one.

               Dep_Ptr.Next := new Dependency_File_Info_Node;
               Dep_Ptr := Dep_Ptr.Next;
               Set_Contents := True;
               exit;
            end if;
            Dep_Ptr := Dep_Ptr.Next;
         end loop;
      end if;

      --  Creating new Dependency_File_Info_Node object

      if Set_Contents then
         Dep_Ptr.all :=
           (Value => (File         => (LI              => Referred_LI,
                                       Part            => Unit_Body,
                                       Source_Filename => null),
                      Dep_Info     => (Depends_From_Spec => False,
                                       Depends_From_Body => True),
                      Declarations => null),
            Next  => null);
      end if;
   end Insert_Dependency;

   -----------------------------------
   -- Insert_Dependency_Declaration --
   -----------------------------------

   procedure Insert_Dependency_Declaration
     (Handler               : access Src_Info.CPP.CPP_LI_Handler_Record'Class;
      File                  : in out LI_File_Ptr;
      List                  : LI_File_List;
      Symbol_Name           : String;
      Referred_Filename     : VFS.Virtual_File;
      Location              : Point;
      Parent_Filename       : VFS.Virtual_File := VFS.No_File;
      Parent_Location       : Point := Invalid_Point;
      Kind                  : E_Kind;
      Scope                 : E_Scope;
      End_Of_Scope_Location : Point := Invalid_Point;
      Rename_Location       : Point := Invalid_Point;
      Declaration_Info      : out E_Declaration_Info_List)
   is
      D_Ptr       : E_Declaration_Info_List;
      Dep_Ptr     : Dependency_File_Info_List;
      Referred_LI : LI_File_Ptr;

   begin
      Insert_Dependency
        (Handler           => Handler,
         File              => File,
         List              => List,
         Referred_Filename => Referred_Filename,
         Referred_LI       => Referred_LI,
         Dep_Ptr           => Dep_Ptr);

      --  Now Dep_Ptr points to valid Dependency_File_Info_Node object
      --  Inserting new declaration

      if Dep_Ptr.Value.Declarations = null then
         --  This is a first declaration for this Dependency_File_Info

         Dep_Ptr.Value.Declarations := new E_Declaration_Info_Node;
         Dep_Ptr.Value.Declarations.Next := null;
         D_Ptr := Dep_Ptr.Value.Declarations;

      else
         --  Inserting to the end of the declaration's list

         D_Ptr := Dep_Ptr.Value.Declarations;

         loop
            if D_Ptr.Value.Declaration.Location.Line = Location.Line
              and then D_Ptr.Value.Declaration.Location.Line = Location.Line
            then
               D_Ptr.Value.Declaration := No_Declaration;
               exit;
            end if;

            if D_Ptr.Next = null then
               D_Ptr.Next := new E_Declaration_Info_Node;
               D_Ptr.Next.Next := null;
               D_Ptr := D_Ptr.Next;

               exit;
            end if;

            D_Ptr := D_Ptr.Next;
         end loop;
      end if;

      Insert_Declaration
        (D_Ptr,
         Referred_LI,
         List,
         Symbol_Name,
         Location,
         Parent_Filename,
         Parent_Location,
         Kind,
         Scope,
         End_Of_Scope_Location,
         Rename_Location);
      Declaration_Info := D_Ptr;
   end Insert_Dependency_Declaration;

   ------------------------
   -- Insert_declaration --
   ------------------------

   procedure Insert_Declaration
     (File                    : in out LI_File_Ptr;
      List                    : LI_File_List;
      Symbol_Name             : String;
      Location                : Point;
      Parent_Filename         : VFS.Virtual_File := VFS.No_File;
      Parent_Location         : Point := Invalid_Point;
      Kind                    : E_Kind;
      Scope                   : E_Scope;
      End_Of_Scope_Location   : Point := Invalid_Point;
      Rename_Location         : Point := Invalid_Point;
      Declaration_Info        : out E_Declaration_Info_List) is
   begin
      if File.LI.Body_Info.Declarations = null then
         File.LI.Body_Info.Declarations := new E_Declaration_Info_Node;
         Declaration_Info := File.LI.Body_Info.Declarations;

      else
         Declaration_Info := File.LI.Body_Info.Declarations;
         loop
            if Declaration_Info.Value.Declaration.Location.Line = Location.Line
              and then Declaration_Info.Value.Declaration.Location.Column =
              Location.Column
            then
               Declaration_Info.Value.Declaration := No_Declaration;
               exit;
            end if;

            if Declaration_Info.Next = null then
               Declaration_Info.Next := new E_Declaration_Info_Node;
               Declaration_Info := Declaration_Info.Next;
               exit;
            end if;
            Declaration_Info := Declaration_Info.Next;
         end loop;
      end if;

      Insert_Declaration
        (Declaration_Info,
         File,
         List,
         Symbol_Name,
         Location,
         Parent_Filename,
         Parent_Location,
         Kind,
         Scope,
         End_Of_Scope_Location,
         Rename_Location);
   end Insert_Declaration;

   ------------------------
   -- Insert_Declaration --
   ------------------------

   procedure Insert_Declaration
     (D_Ptr                   : in out E_Declaration_Info_List;
      File                    : LI_File_Ptr;
      List                    : LI_File_List;
      Symbol_Name             : String;
      Location                : Point;
      Parent_Filename         : VFS.Virtual_File := VFS.No_File;
      Parent_Location         : Point := Invalid_Point;
      Kind                    : E_Kind;
      Scope                   : E_Scope;
      End_Of_Scope_Location   : Point := Invalid_Point;
      Rename_Location         : Point := Invalid_Point)
   is
      Tmp_LI_File_Ptr : LI_File_Ptr;
   begin
      if D_Ptr = null then
         return;
      end if;

      D_Ptr.Value.Declaration.Name := new String'(Symbol_Name);
      D_Ptr.Value.Declaration.Location :=
        (File   => (LI              => File,
                    Part            => Unit_Body,
                    Source_Filename => null),
         Line   => Location.Line,
         Column => Location.Column);
      D_Ptr.Value.Declaration.Kind := Kind;

      if Parent_Location = Invalid_Point then
         D_Ptr.Value.Declaration.Parent_Location := new File_Location_Node'
           (Value => Null_File_Location,
            Kind  => Parent_Type,
            Predefined_Entity_Name => No_Name,
            Next  => null);

      elsif Parent_Location = Predefined_Point then
         D_Ptr.Value.Declaration.Parent_Location := new File_Location_Node'
           (Value => Predefined_Entity_Location,
            Kind  => Parent_Type,
            Predefined_Entity_Name => No_Name,
            Next  => null);

      else
         --  Processing parent information

         if File.LI.Body_Info /= null
           and then Base_Name (File.LI.Body_Info.Source_Filename.all) =
           Base_Name (Parent_Filename)
         then
            Tmp_LI_File_Ptr := File;

         else
            --  Find the parent LI, or create a stub for it.

            Create_Stub_For_File
              (LI            => Tmp_LI_File_Ptr,
               Handler       => CPP_LI_Handler (File.LI.Handler),
               List          => List,
               Full_Filename => Parent_Filename);
         end if;
         D_Ptr.Value.Declaration.Parent_Location := new File_Location_Node'
           (Value => (File   => (LI              => Tmp_LI_File_Ptr,
                                 Part            => Unit_Body,
                                 Source_Filename => null),
                      Line   => Parent_Location.Line,
                      Column => Parent_Location.Column),
            Kind  => Parent_Type,
            Predefined_Entity_Name => No_Name,
            Next  => null);

         --  ??? what does the procedure look like to support multiple
         --  inheritance?
      end if;

      D_Ptr.Value.Declaration.Scope := Scope;

      if End_Of_Scope_Location = Invalid_Point then
         D_Ptr.Value.Declaration.End_Of_Scope := No_Reference;
      else
         D_Ptr.Value.Declaration.End_Of_Scope.Location :=
           (File   => (LI              => File,
                       Part            => Unit_Body,
                       Source_Filename => null),
            Line   => End_Of_Scope_Location.Line,
            Column => End_Of_Scope_Location.Column);
         D_Ptr.Value.Declaration.End_Of_Scope.Kind := End_Of_Body;
      end if;

      if Rename_Location = Invalid_Point then
         D_Ptr.Value.Declaration.Rename := Null_File_Location;
      else
         D_Ptr.Value.Declaration.Rename :=
           (File   => (LI              => No_LI_File,
                       Part            => Unit_Body,
                       Source_Filename => null),
            Line   => Rename_Location.Line,
            Column => Rename_Location.Column);

         --  ??? we need to search for appropriate LI File in which
         --  renamed entity is really declared
      end if;
   end Insert_Declaration;

   ----------------
   -- Add_Parent --
   ----------------

   procedure Add_Parent
     (Declaration_Info : in out E_Declaration_Info_List;
      Handler          : CPP_LI_Handler;
      List             : LI_File_List;
      Parent_Filename  : VFS.Virtual_File;
      Parent_Location  : Point)
   is
      FL_Ptr          : File_Location_List;
      Tmp_LI_File_Ptr : LI_File_Ptr;
   begin
      Assert (Fail_Stream, Declaration_Info /= null, "Invalid declaration");

      if Declaration_Info.Value.Declaration.Parent_Location = null then
         Declaration_Info.Value.Declaration.Parent_Location :=
           new File_Location_Node;
         FL_Ptr := Declaration_Info.Value.Declaration.Parent_Location;

      else
         FL_Ptr := Declaration_Info.Value.Declaration.Parent_Location;

         loop
            if FL_Ptr.Value.Line = Parent_Location.Line
              and then FL_Ptr.Value.Column = Parent_Location.Column
              and then Get_LI_Filename (FL_Ptr.Value.File.LI) = Parent_Filename
            then
               return;
            end if;

            if FL_Ptr.Next = null then
               FL_Ptr.Next := new File_Location_Node;
               FL_Ptr := FL_Ptr.Next;
               exit;
            end if;

            FL_Ptr := FL_Ptr.Next;
         end loop;
      end if;

      --  Create a stub for the parent, or get the existing LI
      Create_Stub_For_File
        (LI            => Tmp_LI_File_Ptr,
         Handler       => Handler,
         List          => List,
         Full_Filename => Parent_Filename);
      FL_Ptr.all :=
        (Value => (File   => (LI              => Tmp_LI_File_Ptr,
                              Part            => Unit_Body,
                              Source_Filename => null),
                   Line   => Parent_Location.Line,
                   Column => Parent_Location.Column),
         Kind => Parent_Type,
         Predefined_Entity_Name => No_Name,
         Next  => null);
   end Add_Parent;

   --------------------------
   -- Create_Stub_For_File --
   --------------------------

   procedure Create_Stub_For_File
     (LI            : out LI_File_Ptr;
      Handler       : access Src_Info.CPP.CPP_LI_Handler_Record'Class;
      List          : LI_File_List;
      Full_Filename : VFS.Virtual_File)
   is
      Project   : constant Project_Type := Get_Project_From_File
        (Registry => Project_Registry (Get_Registry (Handler.Root_Project)),
         Source_Filename   => Full_Filename,
         Root_If_Not_Found => True);
      Xref_Name : constant Virtual_File := Xref_Filename_For
        (Full_Filename,
         Get_DB_Dir (Project),
         Get_Prj_HTable (Handler));
   begin
      LI := Locate (List, Xref_Name);

      if LI = null then
         Create_LI_File
           (File        => LI,
            Project     => Project,
            List        => List,
            LI_Filename => Xref_Name,
            Handler     => LI_Handler (Handler));
      end if;

      if LI.LI.Body_Info = null then
         Create_File_Info
           (FI_Ptr           => LI.LI.Body_Info,
            Full_Filename    => Full_Filename);
      end if;
   end Create_Stub_For_File;

   --------------------
   -- Symbol_Handler --
   --------------------

   type Symbol_Handler is access procedure
     (Sym              : FIL_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List);

   procedure Sym_Default_Handler
     (Sym              : FIL_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List);
   procedure Sym_GV_Handler
     (Sym              : FIL_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List);
   procedure Sym_GV_Handler
     (Sym              : FIL_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List;
      Decl_Info        : out E_Declaration_Info_List);
   procedure Sym_CON_Handler
     (Sym              : FIL_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List);
   procedure Sym_FD_Handler
     (Sym              : FIL_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List);  --  Unreferenced
   procedure Sym_FU_Handler
     (Sym              : FIL_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List);
   procedure Sym_E_Handler
     (Sym              : FIL_Table;
      Handler          : access CPP_LI_Handler_Record'Class;  --  Unreferenced
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List);   --  Unreferenced
   procedure Sym_EC_Handler
     (Sym              : FIL_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List);  --  Unreferenced
   procedure Sym_T_Handler
     (Sym              : FIL_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List);
   procedure Sym_CL_Handler
     (Sym              : FIL_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List);
   procedure Sym_CL_Handler
     (Sym              : FIL_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List;
      Decl_Info        : out E_Declaration_Info_List);
   procedure Sym_UN_Handler
     (Sym              : FIL_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List);  --  Unreferenced
   procedure Sym_IV_Handler
     (Sym              : FIL_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List);
   procedure Sym_IV_Handler
     (Sym              : FIL_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List;
      Decl_Info        : out E_Declaration_Info_List);
   procedure Sym_IU_Handler
     (Sym              : FIL_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List);
   procedure Sym_MA_Handler
     (Sym              : FIL_Table;
      Handler          : access CPP_LI_Handler_Record'Class;  --  Unreferenced
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List);    --  Unreferenced
   procedure Sym_MD_Handler
     (Sym              : FIL_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
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

   ----------------
   -- To_Handler --
   ----------------

   type To_Handler is access procedure
     (Ref              : TO_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List);

   procedure Fu_To_Gv_Handler
     (Ref              : TO_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List);
   procedure Fu_To_Fu_Handler
     (Ref              : TO_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List);     --  Unreferenced
   procedure Fu_To_Con_Handler
     (Ref              : TO_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List);
   procedure Fu_To_E_Handler
     (Ref              : TO_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List);    --  Unreferenced
   procedure Fu_To_Ec_Handler
     (Ref              : TO_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List);    --  Unreferenced
   procedure Fu_To_Iv_Handler
     (Ref              : TO_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List);
   procedure Fu_To_Ma_Handler
     (Ref              : TO_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List);     --  Unreferenced
   procedure Fu_To_Mi_Handler
     (Ref              : TO_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List);
   procedure Fu_To_Cl_Handler
     (Ref              : TO_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List);
   procedure Fu_To_T_Handler
     (Ref              : TO_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List);
   procedure Fu_To_Un_Handler
     (Ref              : TO_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List);     --  Unreferenced

   -----------------
   -- To_Handlers --
   -----------------

   Fu_To_Handlers : constant array (Symbol_Type) of To_Handler :=
     (GV     => Fu_To_Gv_Handler'Access,
      FU     => Fu_To_Fu_Handler'Access,
      FD     => Fu_To_Fu_Handler'Access,
      CON    => Fu_To_Con_Handler'Access,
      E      => Fu_To_E_Handler'Access,
      EC     => Fu_To_Ec_Handler'Access,
      IV     => Fu_To_Iv_Handler'Access,
      MA     => Fu_To_Ma_Handler'Access,
      MI     => Fu_To_Mi_Handler'Access,
      MD     => Fu_To_Mi_Handler'Access,
      CL     => Fu_To_Cl_Handler'Access,
      T      => Fu_To_T_Handler'Access,
      UN     => Fu_To_Un_Handler'Access,
      others => null);

   -----------------------
   -- Table_Type_To_Ext --
   -----------------------

   function Table_Extension (Table : Table_Type) return String;
   --  Given a table type, return the associated file extension, or "" if
   --  there is none.

   function Table_Extension (Table : Table_Type) return String is
   begin
      case Table is
         when FIL    => return ".fil";
         when F      => return ".f";
         when FD     => return ".fd";
         when FU     => return ".fu";
         when T      => return ".t";
         when CL     => return ".cl";
         when GV     => return ".gv";
         when E      => return ".e";
         when EC     => return ".ec";
         when TO     => return ".to";
         when IV     => return ".iv";
         when MI     => return ".mi";
         when MD     => return ".md";
         when SN_IN  => return ".in";
         when UN     => return ".un";
         when MA     => return ".ma";
         when CON    => return ".con";
         when LV     => return ".lv";
         when TA     => return ".ta";
         when others => return "";
      end case;
   end Table_Extension;

   procedure Open_DB_Files
     (DB_Dirs     : in GNAT.OS_Lib.String_List_Access;
      SN_Table    : out SN_Table_Array);
   --  Opens all existing DB files, located in specified directories list.
   --  Returns array of DB_Files (indexed by Symbol_Type).

   procedure Close_DB_Files (SN_Table : in out SN_Table_Array);
   --  Close all DB files.

   procedure Browse_Project
     (Project    : Project_Type;
      Iterator   : in out CPP_LI_Handler_Iterator);
   --  Runs cbrowser for the single (!) given project
   --  Fills Iterator with temporary file name (that holds the list
   --  of processed files) and spawned process descriptor.

   procedure Find_Or_Create_Class
     (Handler         : access CPP_LI_Handler_Record'Class;
      CL_Tab          : CL_Table;
      Source_Filename : VFS.Virtual_File;
      Decl_Info       : out E_Declaration_Info_List;
      File            : in out LI_File_Ptr;
      List            : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List);
   --  Attempts to find class declaration among this Handler.File declarations
   --  and dependency declarations. If not found, creates it.
   --  Return pointer to the created/found declaration or null
   --  on error

   procedure Process_File
     (Full_Filename : VFS.Virtual_File;
      Handler       : access CPP_LI_Handler_Record'Class;
      File          : in out LI_File_Ptr;
      List_Of_Files : LI_File_List);
   --  Process the SN databases to create the LI structure for
   --  Source_Filename. Source_Filename is the name of the file as it appears
   --  in the sources (for instance, if we have #include "dir/file.h", then
   --  Source_Filename is "dir/file.h". Full_Filename is the full path to the
   --  physical file on the disk.
   --  If the SN database doesn't exist, File is set to No_LI_File.

   procedure Process_Local_Variables
     (FU_Tab           : FU_Table;
      Symbol           : Symbol_Type;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List);
   --  Process local variables and arguments for specified function/method
   --  body in FU_Tab. Symbol is either FU or MI.

   procedure Process_Local_Variable
     (Var_Name           : String;
      Var_File_Name      : VFS.Virtual_File;
      Var_Start_Position : Point;
      FU_Tab             : FU_Table;
      Symbol             : Symbol_Type;
      Referred_Symbol    : Symbol_Type;
      Handler            : access CPP_LI_Handler_Record'Class;
      File               : in out LI_File_Ptr;
      Decl_Info          : in out E_Declaration_Info_List);
   --  Finds all places where given local variable (or argument) is used
   --  in specified function/method body and create references from
   --  declaration corresponding to that local variable or argument.

   procedure Process_Template_Arguments
     (Symbol           : Symbol_Type := CL;
      FU_Tab           : FU_Table := Invalid_FU_Table;
      CL_Tab           : CL_Table := Invalid_CL_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List);
   --  Finds arguments for the template specified by (Scope, Template_Args,
   --  File_Name). Here Scope is the name of either class/struct/union or
   --  function/procedure. Template_Args - image of all template arguments
   --  as is in CL/FU_Table.
   --  For each template argument Insert_Declaration is called with appropriate
   --  parameters

   procedure Process_Class_To_TA_Refs
     (CL_Tab    : CL_Table;
      Arg       : TA_Table;
      Handler   : access CPP_LI_Handler_Record'Class;
      File      : in out LI_File_Ptr;
      Decl_Info : in out E_Declaration_Info_List);
   --  Find references CL-to-TA for the specified argument and create
   --  corresponding declarations.

   procedure Create_DB_Directory (DB_Dir : String);
   --  Create the database directory if it doesn't exist yet.

   --  Debugging utils
   procedure Info (Msg : String);
   --  Print info message

   procedure Warn (Msg : String);
   --  Print warning message

   procedure Fail (Msg : String);
   --  Print error message

   pragma Inline (Info, Warn, Fail);

   function Get_SN_Dirs (Project : Project_Type)
      return GNAT.OS_Lib.String_List_Access;
   pragma Inline (Get_SN_Dirs);
   --  Return the names of the directories that contain the source navigator
   --  databases for the current project and all nested ones, or null if no
   --  directories are available

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

   procedure Create_Overload_List
     (Name             : String;
      Class_Name       : String;
      Filename         : VFS.Virtual_File;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List);
   --  Generates list of dependency declarations for the method
   --  with given name

   procedure Create_Overload_List
     (Name             : String;
      Filename         : VFS.Virtual_File;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : out LI_File_Ptr;
      List             : LI_File_List);
   --  Generates list of dependency declarations for the function
   --  with given name

   function Get_Function_Kind
     (Return_Type             : String;
      Attributes              : SN_Attributes) return E_Kind;
   --  Returns function/procedure E_Kind after investigation of its
   --  return type and template flag in the attributes

   function Get_Method_Kind
     (Class_Def               : CL_Table;
      Return_Type             : String;
      Attributes              : SN_Attributes) return E_Kind;
   --  Returns method E_Kind after investigation of its return
   --  type and template parameters of the class

   function Get_Method_Kind
     (Handler                 : access CPP_LI_Handler_Record'Class;
      Class_Name, Return_Type : String;
      Attributes              : SN_Attributes) return E_Kind;
   --  Returns method E_Kind after investigation of its return
   --  type and template parameters of the class


   procedure Find_First_Forward_Declaration
     (Buffer       : in String_Access;
      Class_Name   : in Segment;
      Name         : in Segment;
      Filename     : in VFS.Virtual_File;
      Return_Type  : in Segment;
      Arg_Types    : in Segment;
      Handler      : access CPP_LI_Handler_Record'Class;
      File         : in out LI_File_Ptr;
      List         : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List;
      Decl_Info    : out E_Declaration_Info_List;
      Strict       : Boolean := False);
   --  Attempts to find/create the first forward declaration
   --  for the method. Returns null if not found
   --  Strict controls how arguments for functions and methods
   --  are compared (see Cmp_Arg_Types).

   procedure Find_First_Forward_Declaration
     (Buffer       : in String_Access;
      Name         : in Segment;
      Filename     : in VFS.Virtual_File;
      Return_Type  : in Segment;
      Arg_Types    : in Segment;
      Handler      : access CPP_LI_Handler_Record'Class;
      File         : in out LI_File_Ptr;
      List         : LI_File_List;
      Decl_Info    : out E_Declaration_Info_List;
      Strict       : Boolean := False);
   --  Attempts to find/create the first forward declaration
   --  for the function. Returns null if not found
   --  Strict controls how arguments for functions and methods
   --  are compared (see Cmp_Arg_Types).

   function DB_Dirs_Changed
     (Handler : access CPP_LI_Handler_Record'Class;
      DB_Dirs : GNAT.OS_Lib.String_List_Access) return Boolean;
   --  Compares list of DB directories (saved in Handler.Prj_HTable) and
   --  specified list of directories. Returns True if lists are different.

   function Get_DB_Dir
     (DB_Dirs : String_List_Access;
      DBI     : Integer) return String;
   pragma Inline (Get_DB_Dir);
   --  Return the directory that contains the source navigator files
   --  for specified index

   -------------------------
   -- Create_DB_Directory --
   -------------------------

   procedure Create_DB_Directory (DB_Dir : String) is
   begin
      if DB_Dir /= "" and then not Is_Directory (DB_Dir) then
         Make_Dir (DB_Dir);
      end if;
   end Create_DB_Directory;

   ----------------------------
   -- Generate_LI_For_Source --
   ----------------------------

   function Generate_LI_For_Source
     (Handler       : access CPP_LI_Handler_Record;
      Root_Project  : Project_Type;
      File_Project  : Project_Type;
      Full_Filename : VFS.Virtual_File) return LI_Handler_Iterator'Class
   is
      pragma Unreferenced (Root_Project);

      HI             : CPP_LI_Handler_Iterator;
      Tmp_File       : File_Type;
      Success        : Boolean;
      Process_Alive  : Boolean := False;
      Xref_File_Name : Virtual_File;
      DB_Dir         : constant String := Get_DB_Dir (File_Project);
      Pool           : Xref_Pool;

   begin
      if DB_Dir = "" then
         HI.State := Done;
         return HI;
      end if;

      HI.Handler := CPP_LI_Handler (Handler);

      --  Name of the temporary file to use

      HI.List_Filename := new String'(DB_Dir & "gps_list");

      --  Make sure the database directory exists.

      Create_DB_Directory (DB_Dir);
      Pool := Get_Xref_Pool (Handler.Prj_HTable, DB_Dir);
      Xref_File_Name := Xref_Filename_For (Full_Filename, DB_Dir, Pool);

      --  Recompute Xref is the Xref file is invalid or if the source file
      --  is newer than its Xref

      if not Is_Xref_Valid (Full_Filename, Pool)
        or else To_Timestamp (File_Time_Stamp (Full_Name (Full_Filename))) >
                To_Timestamp (File_Time_Stamp (Full_Name (Xref_File_Name)))
      then
         Set_Valid (Full_Filename, True, Pool);

         --  Remove the current xref file if it exists, since
         --  cbrowser opens it in append mode.

         if Is_Regular_File (Xref_File_Name) then
            Delete (Xref_File_Name);
         end if;

         --  Create the list of files that need to be analyzed.

         Create (Tmp_File, Out_File, Name => HI.List_Filename.all);
         Put_Line (Tmp_File, "@" & Full_Name (Xref_File_Name));
         Put_Line (Tmp_File, Full_Name (Full_Filename));
         Close (Tmp_File);

         Close_DB_Files (Handler.SN_Table);
         SN.Browse.Browse
           (File_Name     => HI.List_Filename.all,
            DB_Directory  => DB_Dir,
            DBIMP_Path    => Handler.DBIMP_Path.all,
            Cbrowser_Path => Handler.CBrowser_Path.all,
            PD            => HI.PD);

         --  Wait for the underlying process to finish

         loop
            Browse.Is_Alive (HI.PD, Process_Alive);

            exit when not Process_Alive;

            delay 0.05;
         end loop;

         Delete_File (HI.List_Filename.all, Success);
         Free (HI.List_Filename);

         Save (Pool,
               Create (Full_Filename => DB_Dir & Browse.Xref_Pool_Filename));
      end if;

      return HI;
   end Generate_LI_For_Source;

   --------------------
   -- Browse_Project --
   --------------------

   procedure Browse_Project
     (Project    : Project_Type;
      Iterator   : in out CPP_LI_Handler_Iterator)
   is
      DB_Dir           : constant String := Get_DB_Dir (Project);
      Num_Source_Files : Natural := 0;
      Tmp_File         : File_Type;
      Success          : Boolean;
      Xref_File_Name   : Virtual_File;
      TO_File_Name     : constant String :=
        DB_Dir & SN.Browse.DB_File_Name & ".to";
      Recompute_TO     : Boolean := False;

   begin
      --  Skip project if we couldn't find out the object directory

      if DB_Dir = "" then
         Iterator.State := Skip_Project;
         return;
      end if;

      --  Prepare the list of files

      Trace (Info_Stream, "Computing the C and C++ sources list for "
             & Project_Name (Project));
      Compute_Sources
        (Iterator,
         Project,
         False, -- no recursion
         Languages => (1 => Name_C, 2 => Name_C_Plus_Plus));

      --  If there is at least one source file, make sure the database
      --  directory exists.

      if Current_Source_File (Iterator) /= VFS.No_File then
         Create_DB_Directory (DB_Dir);

         --  Create the list of files that need to be analyzed.

         Iterator.List_Filename := new String'(DB_Dir & "gps_list");
         Create (Tmp_File, Out_File, Name => Iterator.List_Filename.all);

      else
         Iterator.List_Filename := null;
      end if;

      loop
         declare
            File : constant Virtual_File := Current_Source_File (Iterator);
            Pool : constant Xref_Pool := Get_Xref_Pool
              (Iterator.Handler.Prj_HTable, DB_Dir);

         begin
            exit when File = VFS.No_File;

            --  Start processing next file
            --  File needs to be processed if:
            --  1. Its xref file is invalid (just created)
            --  2. Source is newer than xref file

            Xref_File_Name := Xref_Filename_For (File, DB_Dir, Pool);

            if not Is_Xref_Valid (File, Pool)
              or else To_Timestamp (File_Time_Stamp (Full_Name (File))) >
                To_Timestamp (File_Time_Stamp (Full_Name (Xref_File_Name)))
            then
               Num_Source_Files := Num_Source_Files + 1;

               Set_Valid (File, True, Pool);

               --  Remove the current xref file if it exists, since
               --  cbrowser opens it in append mode.

               if Is_Regular_File (Xref_File_Name) then
                  Delete (Xref_File_Name);
               end if;

               Put_Line (Tmp_File, "@" & Full_Name (Xref_File_Name));
               Put_Line (Tmp_File, Full_Name (File));

            elsif To_Timestamp (File_Time_Stamp (Full_Name (Xref_File_Name))) >
              To_Timestamp (File_Time_Stamp (TO_File_Name))
            then
               Recompute_TO := True;
            end if;
         end;

         Next_Source_File (Iterator);
      end loop;

      if Iterator.List_Filename /= null then
         Close (Tmp_File);
      end if;

      if Num_Source_Files > 0 then
         Iterator.State := Analyze_Files;
         Close_DB_Files (Iterator.Handler.SN_Table);
         SN.Browse.Browse
           (File_Name     => Iterator.List_Filename.all,
            DB_Directory  => DB_Dir,
            DBIMP_Path    => Iterator.Handler.DBIMP_Path.all,
            Cbrowser_Path => Iterator.Handler.CBrowser_Path.all,
            PD            => Iterator.PD);
         Iterator.Process_Running := True;

      elsif Recompute_TO then
         Iterator.State := Analyze_Files;
      else
         if Iterator.List_Filename /= null then
            Delete_File (Iterator.List_Filename.all, Success);
            Free (Iterator.List_Filename);
         end if;

         Iterator.State := Skip_Project;
      end if;
   end Browse_Project;

   -----------------------------
   -- Generate_LI_For_Project --
   -----------------------------

   function Generate_LI_For_Project
     (Handler       : access CPP_LI_Handler_Record;
      Root_Project  : Project_Type;
      Project       : Project_Type;
      Recursive     : Boolean := False)
      return LI_Handler_Iterator'Class
   is
      HI : CPP_LI_Handler_Iterator;
   begin
      HI.Handler       := CPP_LI_Handler (Handler);
      HI.Prj_Iterator  := Start (Project, Recursive);
      HI.Project       := Project;
      HI.Root_Project  := Root_Project;
      HI.List_Filename := null;

      if Current (HI.Prj_Iterator) /= No_Project then
         Browse_Project (Current (HI.Prj_Iterator), HI);
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
      DB_Dirs        : GNAT.OS_Lib.String_List_Access;

      procedure Next_Project;
      --  Promotes iterator to the next project and runs Browse_Project
      --  for it. When no more projects is available, iterator's state
      --  is changed to Done and DB files are opened

      procedure Next_Project is
         DB_Dirs : GNAT.OS_Lib.String_List_Access;
      begin
         --  Proceed with cparser on next project directories
         Next (Iterator.Prj_Iterator);

         if Current (Iterator.Prj_Iterator) = No_Project then
            --  iterations finished

            Iterator.State := Done;
            DB_Dirs := Get_SN_Dirs (Iterator.Project);

            if DB_Dirs = null then
               return;
            end if;

            Open_DB_Files (DB_Dirs, Iterator.Handler.SN_Table);
            Free (DB_Dirs);

         else
            --  go on with other projects

            Browse_Project (Current (Iterator.Prj_Iterator), Iterator);
         end if;
      end Next_Project;

   begin
      Finished := False;

      case Iterator.State is
         when Done =>
            null;
            --  see below.

         when Skip_Project =>
            Trace (Info_Stream, "Passing onto the next project");
            Next_Project;

         when Analyze_Files =>
            --  If we haven't finished the first phase, keep waiting.

            if Iterator.Process_Running then
               Browse.Is_Alive (Iterator.PD, Process_Alive);
               if Process_Alive then
                  return;
               end if;
            end if;

            if Iterator.List_Filename /= null then
               Delete_File (Iterator.List_Filename.all, Success);
               Free (Iterator.List_Filename);
            end if;

            Trace (Info_Stream, "Starting the dbimp process");
            --  All files processed, start generating of xrefs

            Iterator.State := Process_Xrefs;

            DB_Dirs := Get_SN_Dirs (Current (Iterator.Prj_Iterator));

            if DB_Dirs = null then
               Iterator.State := Done;
            else
               Browse.Generate_Xrefs
                 (DB_Directories => DB_Dirs,
                  DBIMP_Path     => Iterator.Handler.DBIMP_Path.all,
                  Temp_Name      => Iterator.Tmp_Filename,
                  PD             => Iterator.PD);
               Free (DB_Dirs);
            end if;

         when Process_Xrefs =>
            --  If we haven't finished the second phase, keep waiting.

            if Iterator.Process_Running then
               Browse.Is_Alive (Iterator.PD, Process_Alive);

               if Process_Alive then
                  return;
               end if;
            end if;

            Iterator.Process_Running := False;

            Trace (Info_Stream, "dbimp finished, passing to next project");

            --  Proceed with cparser on next project directories
            Delete_File (Iterator.Tmp_Filename, Success);
            Next_Project;
      end case;

      if Iterator.State = Done then
         Trace (Info_Stream, "Processing is finished");
         Finished := True;
      end if;
   end Continue;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Iterator : in out CPP_LI_Handler_Iterator) is
   begin
      --  Save xref pools
      if Iterator.Handler.Prj_HTable /= Empty_SN_Prj_HTable then
         declare
            Iter : Imported_Project_Iterator := Start
              (Iterator.Root_Project, Recursive => True);
         begin
            while Current (Iter) /= No_Project loop
               declare
                  DB_Dir : constant String := Get_DB_Dir (Current (Iter));
               begin
                  if DB_Dir /= "" then
                     declare
                        Pool : constant Xref_Pool := Get_Xref_Pool
                          (Iterator.Handler.Prj_HTable, DB_Dir);
                     begin
                        if Pool /= Empty_Xref_Pool then
                           Save (Pool,
                                 Create
                                   (Full_Filename =>
                                      DB_Dir & Browse.Xref_Pool_Filename));
                        end if;
                     end;
                  end if;
               end;

               Next (Iter);
            end loop;
         end;
      end if;

      Destroy (LI_Handler_Iterator (Iterator));
   end Destroy;

   ---------------------
   -- DB_Dirs_Changed --
   ---------------------

   function DB_Dirs_Changed
     (Handler : access CPP_LI_Handler_Record'Class;
      DB_Dirs : GNAT.OS_Lib.String_List_Access) return Boolean
   is
      use SN_Prj_HTables;
      Iter : Iterator;
      Prj_Data : SN_Prj_Data;
      CDirs    : Integer := 0;
   begin
      --  Check if every new DB dir (from DB_Dirs) is in Prj_HTable
      for J in DB_Dirs'Range loop
         Prj_Data := SN_Prj_HTables.Get
           (Handler.Prj_HTable.all, DB_Dirs (J).all);

         if Prj_Data = No_SN_Prj_Data then
            return True;
         end if;
      end loop;

      SN_Prj_HTables.Get_First (Handler.Prj_HTable.all, Iter);

      while Get_Element (Iter) /= No_SN_Prj_Data loop
         CDirs := CDirs + 1;
         SN_Prj_HTables.Get_Next (Handler.Prj_HTable.all, Iter);
      end loop;

      if CDirs = DB_Dirs'Length then
         return False;
      else
         return True;
      end if;
   end DB_Dirs_Changed;

   -----------------
   -- Get_SN_Dirs --
   -----------------

   function Get_SN_Dirs
     (Project : Project_Type) return GNAT.OS_Lib.String_List_Access
   is
      N        : Integer := 0;
      Dirs     : GNAT.OS_Lib.String_List_Access;
      Path     : constant String := Object_Path (Project, True);
      Main_Dir : constant String := Get_DB_Dir (Project);
      J        : Integer := Path'First;
      K        : Integer;
      Tmp      : GNAT.OS_Lib.String_Access;

   begin
      if Path = "" then
         return null;
      end if;

      loop
         J := Ada.Strings.Fixed.Index
           (Path (J .. Path'Last),
            "" & GNAT.OS_Lib.Path_Separator);

         exit when J = 0;

         N := N + 1;
         J := J + 1;
      end loop;

      Dirs := new GNAT.OS_Lib.String_List (1 .. N + 1);

      J := Path'First;
      N := 1;

      loop
         K := J;
         J := Ada.Strings.Fixed.Index
           (Path (J .. Path'Last),
            (1 => GNAT.OS_Lib.Path_Separator));

         if J = 0 then
            Dirs (N) := new String'
              (Name_As_Directory (Path (K .. Path'Last))
                 & Name_As_Directory (Browse.DB_Dir_Name));
            exit;
         end if;

         Dirs (N) := new String'
           (Name_As_Directory (Path (K .. J - 1))
              & Name_As_Directory (Browse.DB_Dir_Name));

         if Dirs (N).all = Main_Dir then -- should swap
            Tmp      := Dirs (1);
            Dirs (1) := Dirs (N);
            Dirs (N) := Tmp;
         end if;

         N := N + 1;
         J := J + 1;
      end loop;

      return Dirs;
   end Get_SN_Dirs;

   -------------------
   -- Open_DB_Files --
   -------------------

   procedure Open_DB_Files
     (DB_Dirs   : in GNAT.OS_Lib.String_List_Access;
      SN_Table  : out SN_Table_Array)
   is
      Success : Boolean;
   begin
      for Table in Table_Type loop
         declare
            Ext   : constant String := Table_Extension (Table);
            Files : String_List_Access :=
              new String_List (1 .. DB_Dirs'Length);

         begin
            for J in DB_Dirs'Range loop
               Files (J) := new String'
                 (DB_Dirs (J).all & Browse.DB_File_Name & Ext);
            end loop;

            if Ext /= "" then
               DB_API.Open (SN_Table (Table), Files, Success);
            end if;

            Free (Files);
         end;
      end loop;
   end Open_DB_Files;

   --------------------
   -- Close_DB_Files --
   --------------------

   procedure Close_DB_Files (SN_Table : in out SN_Table_Array) is
      Success : Boolean;
   begin
      for Table in Table_Type loop
         Close (SN_Table (Table), Success);
      end loop;
   end Close_DB_Files;

   ------------------
   -- Process_File --
   ------------------

   procedure Process_File
     (Full_Filename : VFS.Virtual_File;
      Handler       : access CPP_LI_Handler_Record'Class;
      File          : in out LI_File_Ptr;
      List_Of_Files : LI_File_List)
   is
      P               : Pair_Ptr;
      Module_Typedefs : Src_Info.Type_Utils.Module_Typedefs_List;
      Sym             : FIL_Table;

   begin
      if not Is_Open (Handler.SN_Table (FIL)) then
         --  .fil table does not exist, no data available
         Fail (".fil table does not exist: is SN DB generated already?");
         File := No_LI_File;
         return;
      end if;

      Init (Module_Typedefs);
      Set_Cursor (Handler.SN_Table (FIL),
                  Position    => By_Key,
                  Key         => Full_Name (Full_Filename) & Field_Sep,
                  Exact_Match => False);

      loop -- iterate thru all symbols for specified file
         P := Get_Pair (Handler.SN_Table (FIL), Next_By_Key);
         exit when P = null;

         begin
            Parse_Pair (P.all, Sym);
            --  apply corresponding symbol handler

            Symbol_Handlers (Sym.Symbol)
              (Sym, Handler, File, List_Of_Files, Module_Typedefs);
            Free (Sym);

         exception
            when others =>
               Free (Sym);
               raise;
         end;

         Free (P);
      end loop;

      Release_Cursor (Handler.SN_Table (FIL));
      Free (Module_Typedefs);
   exception
      when others   => -- unexpected exception
         Free (P);
         Free (Module_Typedefs);
         Release_Cursor (Handler.SN_Table (FIL));
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

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Handler : in out CPP_LI_Handler_Record) is
   begin
      --  Free xref pools and project hash table
      Free (Handler.DB_Dirs, Handler.Prj_HTable);

      Free (Handler.DBIMP_Path);
      Free (Handler.CBrowser_Path);
   end Destroy;

   -----------
   -- Reset --
   -----------

   procedure Reset
     (Handler : access CPP_LI_Handler_Record'Class;
      Project : Project_Type)
   is
      use type Project_Type;
   begin
      Handler.Root_Project := Project;
      Handler.DB_Dirs := Get_SN_Dirs (Project);

      if Handler.DB_Dirs = null then
         Free (Handler.DB_Dirs, Handler.Prj_HTable);
         return;
      end if;

      --  Reset the previous contents (if necessary)

      if Handler.Prj_HTable = Empty_SN_Prj_HTable
        or else DB_Dirs_Changed (Handler, Handler.DB_Dirs)
      then
         if Handler.Prj_HTable /= Empty_SN_Prj_HTable then
            Free (Handler.DB_Dirs, Handler.Prj_HTable);
            --  Handler.DB_Dirs released by previous function,
            --  so we need to get it again
            Handler.DB_Dirs := Get_SN_Dirs (Project);
         end if;

         Init (Handler.Prj_HTable);
         --  Load xref pools and fill Prj_HTable
         for J in Handler.DB_Dirs'Range loop
            declare
               Pool   : Xref_Pool;
               DB_Dir : constant String_Access := Handler.DB_Dirs (J);
            begin
               Load (Pool, Create
                       (Full_Filename =>
                          DB_Dir.all & Browse.Xref_Pool_Filename));
               Set_Xref_Pool (Handler.Prj_HTable, DB_Dir, Pool);
            end;
         end loop;
      end if;

      Close_DB_Files (Handler.SN_Table);
      Open_DB_Files (Handler.DB_Dirs, Handler.SN_Table);
   end Reset;

   ---------------------------
   -- Create_Or_Complete_LI --
   ---------------------------

   procedure Create_Or_Complete_LI
     (Handler                : access CPP_LI_Handler_Record;
      File                   : in out LI_File_Ptr;
      Source_Filename        : VFS.Virtual_File;
      List                   : LI_File_List;
      Project                : Project_Type)
   is
      DB_Dir : constant String := Get_DB_Dir (Project);
   begin
      --  Do nothing if we couldn't create the database directory
      if DB_Dir = "" then
         return;
      end if;

      if Dir_Name (Source_Filename) = "./" then
         Warn ("File not found: " & Base_Name (Source_Filename));
         return;
      end if;

      --  Make sure the directory exists
      Create_DB_Directory (DB_Dir);

      --  Find the existing LI, or create a stub (However, we create the file
      --  as unparsed, for the following test).
      Create_Stub_For_File
        (LI            => File,
         Handler       => Handler,
         List          => List,
         Full_Filename => Source_Filename);

      --  check timestamps for the parsed file
      if File /= No_LI_File and then File.LI.Parsed then
         if not Is_Incomplete (File)
           and then To_Timestamp
             (File_Time_Stamp (Full_Name (File.LI.LI_Filename))) <=
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

      Trace (Info_Stream, "Create_Or_Complete_LI "
             & Full_Name (Source_Filename));

      Convert_To_Parsed
        (File, File.LI.LI_Filename, Update_Timestamp => True);

      Process_File (Source_Filename, Handler, File, List);

      Save
        (Get_Xref_Pool (Handler.Prj_HTable, DB_Dir),
         Create (Full_Filename => DB_Dir & Browse.Xref_Pool_Filename));
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
      List                   : LI_File_List;
      In_Directory           : String;
      Project                : Project_Type)
   is
      pragma Unreferenced (Handler);
      pragma Unreferenced (List);
      pragma Unreferenced (In_Directory);
      pragma Unreferenced (Project);
   begin
      null;
   end Parse_All_LI_Information;

   ---------------------------
   -- Parse_File_Constructs --
   ---------------------------

   procedure Parse_File_Constructs
     (Handler      : access CPP_LI_Handler_Record;
      Root_Project : Projects.Project_Type;
      Languages    : access Language_Handlers.Language_Handler_Record'Class;
      File_Name    : VFS.Virtual_File;
      Result       : out Language.Construct_List)
   is
      pragma Unreferenced (Languages);

      Constructs      : Language.Construct_List;
      Iterator        : CPP_LI_Handler_Iterator;
      P               : Pair_Ptr;
      Module_Typedefs : Src_Info.Type_Utils.Module_Typedefs_List;
      Project         : constant Project_Type :=
        Get_Project_From_File
          (Project_Registry'Class
             (Get_Registry (Root_Project)), File_Name);
      DB_Dir          : constant String := Get_DB_Dir (Project);
      Files           : String_List_Access;
      Success         : Boolean;

   begin
      --  Create/update sn databases
      --  The call is blocking

      Iterator := CPP_LI_Handler_Iterator
        (Generate_LI_For_Source
          (Handler,
           Root_Project,
           Project,
           File_Name));

      --  Open the databases if not yet open

      if not Is_Open (Handler.SN_Table (FIL)) then
         Files := new String_List (1 .. 1);
         Files (1) := new String'
           (DB_Dir & Browse.DB_File_Name & Table_Extension (FIL));
         DB_API.Open (Handler.SN_Table (FIL), Files, Success);
         Free (Files (1));

         if not Success then
            return;
         end if;
      end if;

      --  Iterate through all symbols of File_Name
      --  using the FIL table

      Init (Module_Typedefs);
      Set_Cursor
        (Handler.SN_Table (FIL),
         Position    => By_Key,
         Key         => Full_Name (File_Name) & Field_Sep,
         Exact_Match => False);

      loop
         P := Get_Pair (Handler.SN_Table (FIL), Next_By_Key);
         exit when P = null;

         declare
            Sym  : FIL_Table;
            Info : Construct_Access;
            C    : Construct_Access;

         begin
            Parse_Pair (P.all, Sym);

            --  Build the next construct

            case Sym.Symbol is
               when CL | CON | E | IU | T | TA | UN | GV | IV | LV
                    | FD | FU | MA | MD | MI =>
                  --  Build the constructs
                  --  Use subtype instead ???

                  Info := Constructs.Current;
                  Constructs.Current := new Construct_Information;
                  C := Constructs.Current;
                  C.Is_Declaration := False;

                  --  Link

                  if Constructs.First = null then
                     Constructs.First := Constructs.Current;
                  else
                     Constructs.Current.Prev := Info;
                     Constructs.Current.Next := Info.Next;
                     Info.Next               := Constructs.Current;
                  end if;

                  --  Set name and location, common to all categories

                  C.Name := new String'
                    (To_String (Sym.Buffer, Sym.Identifier));

                  --  ??? For now, do not set the third field (absolute source
                  --  location), since the explorer does not use it and
                  --  computing it is not simple.

                  C.Sloc_Start := (Sym.Start_Position.Line,
                                   Sym.Start_Position.Column,
                                   0);
                  C.Sloc_End := (Sym.End_Position.Line,
                                 Sym.End_Position.Column,
                                 0);
                  C.Sloc_Entity := (Sym.Start_Position.Line,
                                    Sym.Start_Position.Column,
                                    0);
               when others =>
                  null;
            end case;

            --  Set the category

            case Sym.Symbol is
               when CL | TA =>
                  --  ??? make the distinction between struct and classes
                  --  which category for templates ???

                  C.Category := Cat_Structure;
               when CON | GV =>
                  C.Category := Cat_Variable;
               when IV | LV =>
                  C.Category := Cat_Local_Variable;
               when E | T =>
                  C.Category := Cat_Type;
               when IU =>
                  C.Category := Cat_Include;
               when UN =>
                  C.Category := Cat_Union;
               when FD =>
                  C.Category := Cat_Function;
                  C.Is_Declaration := True;
               when FU =>
                  C.Category := Cat_Function;
               when MA =>
                  --  Macros can either be "constants" (#define a 0)
                  --  or "functions" (#define f(a) ((a) == 0))
                  --  For now, only consider macros with arguments as
                  --  pseudo functions.

                  if Length (Sym.Types_Of_Arguments) > 0 then
                     C.Category := Cat_Function;
                  else
                     C.Category := Cat_Unknown;
                  end if;

               when MD =>
                  C.Category := Cat_Method;
                  C.Is_Declaration := True;
               when MI =>
                  C.Category := Cat_Method;
               when others =>
                  null;
            end case;

            --  For functions and methods, get the profile

            case Sym.Symbol is
               when FD | FU | MA | MD | MI =>
                  --  Generate the profile

                  if Length (Sym.Types_Of_Arguments) > 0 then
                     C.Profile := new String'
                       ('(' &
                          Sym.Buffer (Sym.Types_Of_Arguments.First ..
                                      Sym.Types_Of_Arguments.Last) &
                          ')');
                  end if;

               when others =>
                  null;
            end case;

            Free (Sym);

         exception
            when others =>
               --  Free Constructs.Current ???
               Free (Sym);
               raise;
         end;

         Free (P);
      end loop;

      Result := Constructs;
      Release_Cursor (Handler.SN_Table (FIL));
      Free (Module_Typedefs);

   exception
      when E : others   =>
         Trace (Info_Stream,
                "Unexpected exception: " & Exception_Information (E));
         Free (P);
         Free (Module_Typedefs);
   end Parse_File_Constructs;

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
      Source_Filename        : VFS.Virtual_File;
      Project                : Project_Type) return VFS.Virtual_File
   is
      DB_Dir : constant String := Get_DB_Dir (Project);
      Xref_Pool_Filename : constant Virtual_File :=
        Create (Full_Filename => DB_Dir & Browse.Xref_Pool_Filename);

   begin
      if Dir_Name (Source_Filename) = "" then
         return VFS.No_File;
      end if;

      declare
         Pool          : Xref_Pool;
         Xref_Filename : VFS.Virtual_File;
      begin
         Xref_Filename_For
           (Source_Filename,
            DB_Dir,
            Handler.Prj_HTable,
            Xref_Filename => Xref_Filename,
            Pool          => Pool);
         Save (Pool, Xref_Pool_Filename);
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
      --  here we may confuse Overloaded_Entity and Class declaration
      --  for when a method is overloaded it is created as Overloaded_Entity
      --  with position pointing to the class beginning
      Type_Decl_Info := Find_Declaration
        (File         => File,
         Symbol_Name  => Type_Name,
         Location     => Type_Decl,
         Kind         => Overloaded_Entity_Kind,
         Negate_Kind  => True);

      --  Before we add the reference we have to make sure there is no
      --  such already. This lookup is required to avoid duplicate refs
      --  that appear during processing of local variables and template
      --  arguments.
      --  The problem is that the references to a type are generated
      --  twice: once during declaration of the local variable and then
      --  during general loop on TO table that has an entry corresponding
      --  to the type usage in declaration. So far we can't determine that
      --  an entry in TO table referes to the declaration and should be skipped
      --  but it is a TODO: to add an attribute to the TO table ???

      if Type_Decl_Info /= null
        and then
          Find_Reference (Type_Decl_Info, File, Reference_Point, Kind) = null
      then
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
      Source_Filename : VFS.Virtual_File;
      Decl_Info       : out E_Declaration_Info_List;
      File            : in out LI_File_Ptr;
      List            : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List)
   is
      Sym        : FIL_Table;
      Class_Kind : E_Kind := Non_Generic_Class;
   begin
      if Full_Name (Source_Filename) =
        CL_Tab.Buffer (CL_Tab.File_Name.First .. CL_Tab.File_Name.Last)
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
              (Sym, Handler, File, List, Module_Type_Defs, Decl_Info);

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

         if Is_Template (CL_Tab) then
            Class_Kind.Is_Generic := True;
         end if;

         if Decl_Info = null then
            Insert_Dependency_Declaration
              (Handler            => Handler,
               File               => File,
               List               => List,
               Symbol_Name        => CL_Tab.Buffer
                 (CL_Tab.Name.First .. CL_Tab.Name.Last),
                     --  ??? Do we really have a full name here ?
                  Referred_Filename  => Create
                    (Full_Filename => CL_Tab.Buffer
                       (CL_Tab.File_Name.First .. CL_Tab.File_Name.Last)),
               Location           => CL_Tab.Start_Position,
               Kind               => Class_Kind,
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

   -----------------------
   -- Get_Function_Kind --
   -----------------------

   function Get_Function_Kind
     (Return_Type : String;
      Attributes  : SN_Attributes) return E_Kind
   is
      Is_Template : constant Boolean := (Attributes and SN_TEMPLATE) /= 0;
      Kind : E_Kinds;
   begin
      if Return_Type = "void" then
         Kind := Procedure_Kind;
      else
         Kind := Function_Or_Operator;
      end if;

      return (Kind,
              Is_Type     => False,
              Is_Generic  => Is_Template,
              Is_Abstract => False);
   end Get_Function_Kind;

   ---------------------
   -- Get_Method_Kind --
   ---------------------

   function Get_Method_Kind
     (Class_Def   : CL_Table;
      Return_Type : String;
      Attributes  : SN_Attributes) return E_Kind
   is
      Is_Template : Boolean := (Attributes and SN_TEMPLATE) /= 0;
      Kind : E_Kinds;
   begin
      Is_Template := Is_Template or else Type_Utils.Is_Template (Class_Def);
      if Return_Type = "void" then
         Kind := Procedure_Kind;
      else
         Kind := Function_Or_Operator;
      end if;

      return (Kind,
              Is_Type     => False,
              Is_Generic  => Is_Template,
              Is_Abstract => False);
   end Get_Method_Kind;

   ---------------------
   -- Get_Method_Kind --
   ---------------------

   function Get_Method_Kind
     (Handler                 : access CPP_LI_Handler_Record'Class;
      Class_Name, Return_Type : String;
      Attributes              : SN_Attributes) return E_Kind
   is
      Class_Def   : CL_Table;
      Kind        : E_Kind;
   begin
      Find (Handler.SN_Table (CL), Class_Name, Tab => Class_Def);
      Kind := Get_Method_Kind (Class_Def, Return_Type, Attributes);
      Free (Class_Def);
      return Kind;
   end Get_Method_Kind;

   ------------------------------------
   -- Find_First_Forward_Declaration --
   ------------------------------------

   procedure Find_First_Forward_Declaration
     (Buffer           : in String_Access;
      Class_Name       : in Segment;
      Name             : in Segment;
      Filename         : in VFS.Virtual_File;
      Return_Type      : in Segment;
      Arg_Types        : in Segment;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List;
      Decl_Info        : out E_Declaration_Info_List;
      Strict           : Boolean := False)
   is
      P            : Pair_Ptr;
      MD_Tab       : MD_Table;
      MD_Tab_Tmp   : MD_Table;
      First_MD_Pos : Point := Invalid_Point;
      CL_Tab       : CL_Table;
      MD_File      : DB_File;
      Success      : Boolean;
   begin
      Decl_Info := null;
      if not Is_Open (Handler.SN_Table (MD)) then
         return; -- .md table does not exist
      end if;

      MD_File := Dup (Handler.SN_Table (MD));

      --  First we have to find the first forward declaration
      --  that corresponds to our method, that is prototypes
      --  should be the same
      Set_Cursor
        (MD_File,
         By_Key,
         Buffer (Class_Name.First .. Class_Name.Last) & Field_Sep
            & Buffer (Name.First .. Name.Last) & Field_Sep,
         False);

      loop
         P := Get_Pair (MD_File, Next_By_Key);

         if P = null then -- no fwd decls at all
            Close (MD_File, Success);
            return;
         end if;

         Parse_Pair (P.all, MD_Tab);
         Free (P);

         --  Update position of the first forward declaration

         exit when Cmp_Prototypes
           (MD_Tab.Buffer,
            Buffer,
            MD_Tab.Arg_Types,
            Arg_Types,
            MD_Tab.Return_Type,
            Return_Type,
            Strict => Strict);

         Free (MD_Tab);
      end loop;

      Release_Cursor (MD_File);

      --  now find the first declaration in the file
      Set_Cursor
        (MD_File,
         By_Key,
         Buffer (Class_Name.First .. Class_Name.Last) & Field_Sep
            & Buffer (Name.First .. Name.Last) & Field_Sep,
         False);

      loop
         P := Get_Pair (MD_File, Next_By_Key);
         exit when P = null;
         Parse_Pair (P.all, MD_Tab_Tmp);
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
               MD_Tab_Tmp.Return_Type,
               Strict => Strict)
            and then ((First_MD_Pos = Invalid_Point)
                      or else MD_Tab_Tmp.Start_Position < First_MD_Pos)
         then
            First_MD_Pos := MD_Tab_Tmp.Start_Position;
         end if;
         Free (MD_Tab_Tmp);
      end loop;

      Release_Cursor (MD_File);
      Close (MD_File, Success);

      Assert (Fail_Stream, First_MD_Pos /= Invalid_Point, "DB inconsistency");

      --  ??? Do we want to compare the full or base name
      if Full_Name (Filename) =
        MD_Tab.Buffer  (MD_Tab.File_Name.First .. MD_Tab.File_Name.Last)
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

            --  ??? Do we really have a full name here ?
            Filename    => Create
              (MD_Tab.Buffer
                 (MD_Tab.File_Name.First .. MD_Tab.File_Name.Last)),
            Location    => First_MD_Pos);

         if Decl_Info = null then
            begin -- create class declaration if needed
               Find
                 (Handler.SN_Table (CL),
                  Buffer (Class_Name.First .. Class_Name.Last),
                  Tab => CL_Tab);
               Find_Or_Create_Class
                 (Handler,
                  CL_Tab,
                  Filename,
                  Decl_Info,
                  File,
                  List,
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

               --  ??? Do we really have a full or base name
               Referred_Filename  => Create
                 (MD_Tab.Buffer
                    (MD_Tab.File_Name.First .. MD_Tab.File_Name.Last)),
               Location           => First_MD_Pos,
               Kind               => Get_Method_Kind
                 (Handler,
                  Buffer (Class_Name.First .. Class_Name.Last),
                  Buffer (Return_Type.First .. Return_Type.Last),
                  MD_Tab.Attributes),
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
      Filename     : in Virtual_File;
      Return_Type  : in Segment;
      Arg_Types    : in Segment;
      Handler      : access CPP_LI_Handler_Record'Class;
      File         : in out LI_File_Ptr;
      List         : LI_File_List;
      Decl_Info    : out E_Declaration_Info_List;
      Strict       : Boolean := False)
   is
      P            : Pair_Ptr;
      FD_Tab       : FD_Table;
      FD_Tab_Tmp   : FD_Table;
      First_FD_Pos : Point;
      Match        : Boolean;
      Target_Kind  : E_Kind;
      FD_File      : DB_File;
      Success      : Boolean;
   begin
      Decl_Info := null;

      if not Is_Open (Handler.SN_Table (FD)) then
         return; -- .fd table does not exist
      end if;

      FD_File := Dup (Handler.SN_Table (FD));

      --  First we have to find the first forward declaration
      --  that corresponds to our function, that is prototypes
      --  should be the same.
      Set_Cursor
        (FD_File,
         By_Key,
         Buffer (Name.First .. Name.Last) & Field_Sep,
         False);

      loop
         Match := False;
         P := Get_Pair (FD_File, Next_By_Key);

         exit when P = null;

         Parse_Pair (P.all, FD_Tab);
         Free (P);
         Match := True;

         exit when Cmp_Prototypes
           (FD_Tab.Buffer,
            Buffer,
            FD_Tab.Arg_Types,
            Arg_Types,
            FD_Tab.Return_Type,
            Return_Type,
            Strict => Strict);
         Free (FD_Tab);
      end loop;

      Release_Cursor (FD_File);

      if not Match then
         --  we did not found what we wanted, that's strange
         Close (FD_File, Success);
         return;
      end if;

      First_FD_Pos := FD_Tab.Start_Position;

      --  now find the first declaration in the file
      Set_Cursor
        (FD_File,
         By_Key,
         Buffer (Name.First .. Name.Last) & Field_Sep,
         False);

      loop
         P := Get_Pair (FD_File, Next_By_Key);
         exit when P = null;
         Parse_Pair (P.all, FD_Tab_Tmp);
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
            FD_Tab_Tmp.Return_Type,
            Strict => Strict);

         if Match and then FD_Tab_Tmp.Start_Position < First_FD_Pos then
            First_FD_Pos := FD_Tab_Tmp.Start_Position;
         end if;

         Free (FD_Tab_Tmp);
      end loop;

      Release_Cursor (FD_File);
      Close (FD_File, Success);

      Assert (Fail_Stream, First_FD_Pos /= Invalid_Point, "DB inconsistency");

      --  ??? Do we need to compare full or base names ?
      if Full_Name (Filename) =
        FD_Tab.Buffer  (FD_Tab.File_Name.First .. FD_Tab.File_Name.Last)
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

            --  ??? Do we really have a full name here ?
            Filename    => Create
              (FD_Tab.Buffer
                 (FD_Tab.File_Name.First .. FD_Tab.File_Name.Last)),
            Location    => First_FD_Pos);

         if Decl_Info = null then
            Target_Kind := Get_Function_Kind
              (Buffer (Return_Type.First .. Return_Type.Last),
               FD_Tab.Attributes);

            Insert_Dependency_Declaration
              (Handler            => Handler,
               File               => File,
               List               => List,
               Symbol_Name        => Buffer (Name.First .. Name.Last),

               --  ??? Do we really have a full name here
               Referred_Filename  => Create
                 (FD_Tab.Buffer
                    (FD_Tab.File_Name.First .. FD_Tab.File_Name.Last)),
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

   procedure Add (HT : LI_File_List; LIFP : LI_File_Ptr) is
   begin
      Add (HT.Table.all, LIFP);
   end Add;

   ----------------------
   -- Fu_To_Cl_Handler --
   ----------------------

   procedure Fu_To_Cl_Handler
     (Ref              : TO_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List)
   is
      pragma Unreferenced (Module_Type_Defs);
      Class_Desc : CType_Description;
      Class_Def  : CL_Table;
      Success    : Boolean;
      Decl_Info  : E_Declaration_Info_List;
      Class_Kind : E_Kind := Non_Generic_Class;
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

      if Is_Template (Class_Def) then
         Class_Kind.Is_Generic := True;
      end if;

      if Ref.Buffer (Ref.File_Name.First .. Ref.File_Name.Last) /=
          Class_Def.Buffer (Class_Def.File_Name.First ..
                            Class_Def.File_Name.Last)
      then
         Decl_Info := Find_Dependency_Declaration
           (File        => File,
            Symbol_Name => Class_Def.Buffer
              (Class_Def.Name.First .. Class_Def.Name.Last),
            Kind        => Class_Kind,
            Location    => Class_Def.Start_Position,

            --  ??? Do we really have a full name here ?
            Filename    => Create
              (Class_Def.Buffer
                 (Class_Def.File_Name.First .. Class_Def.File_Name.Last)));

         if Decl_Info = null then
            Insert_Dependency_Declaration
              (Handler            => Handler,
               File               => File,
               List               => List,
               Symbol_Name        => Class_Def.Buffer
                 (Class_Def.Name.First .. Class_Def.Name.Last),

               --  ??? Do we really have a full name here
               Referred_Filename  => Create
                 (Class_Def.Buffer
                    (Class_Def.File_Name.First .. Class_Def.File_Name.Last)),
               Location           => Class_Def.Start_Position,
               Kind               => Class_Kind,
               Scope              => Global_Scope,
               Declaration_Info   => Decl_Info);
         end if;

      else
         Decl_Info := Find_Declaration
           (File        => File,
            Symbol_Name => Class_Def.Buffer
              (Class_Def.Name.First .. Class_Def.Name.Last),
            Kind        => Class_Kind,
            Location    => Class_Def.Start_Position);

         if Decl_Info = null then
            Insert_Declaration
              (File               => File,
               List               => List,
               Symbol_Name        => Class_Def.Buffer
                 (Class_Def.Name.First .. Class_Def.Name.Last),
               Location           => Class_Def.Start_Position,
               Kind               => Class_Kind,
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

   -----------------------
   -- Fu_To_Con_Handler --
   -----------------------

   procedure Fu_To_Con_Handler
     (Ref              : TO_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List)
   is
      Ref_Kind     : Reference_Kind;
      Decl_Info    : E_Declaration_Info_List;
      Var          : CON_Table;
      Desc         : CType_Description;
      Success      : Boolean;
      Scope        : E_Scope := Global_Scope;
      Sym          : FIL_Table;

   begin
      --  Info ("Fu_To_Con_Handler: "
      --        & Ref.Buffer (Ref.Referred_Symbol_Name.First ..
      --              Ref.Referred_Symbol_Name.Last));

      --  we need declaration's location

      Find (Handler.SN_Table (CON),
            Ref.Buffer (Ref.Referred_Symbol_Name.First ..
                        Ref.Referred_Symbol_Name.Last),
            Tab => Var);

      --  Find declaration

      if Xref_Filename_For
         (Create (Var.Buffer (Var.File_Name.First .. Var.File_Name.Last)),
          Get_DB_Dir (Handler.DB_Dirs, Var.DBI),
          Handler.Prj_HTable) = Get_LI_Filename (File)
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
              (Sym, Handler, File, List, Module_Type_Defs);

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
              Create (Var.Buffer (Var.File_Name.First .. Var.File_Name.Last)),
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

            if (Var.Attributes and SN_STATIC) = SN_STATIC then
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
                  Referred_Filename => Create
                    (Var.Buffer (Var.File_Name.First .. Var.File_Name.Last)),
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
                  Referred_Filename => Create
                    (Var.Buffer (Var.File_Name.First .. Var.File_Name.Last)),
                  Parent_Location   => Desc.Parent_Point,
                  Parent_Filename   => Create (Desc.Parent_Filename.all),
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
        (Declaration_Info => Decl_Info,
         File             => File,
         Location         => Ref.Position,
         Kind             => Ref_Kind);

   exception
      when Not_Found  | DB_Error => -- ignore
         Fail ("unable to find constant " &
               Ref.Buffer (Ref.Referred_Symbol_Name.First ..
                           Ref.Referred_Symbol_Name.Last));
   end Fu_To_Con_Handler;

   ---------------------
   -- Fu_To_E_Handler --
   ---------------------

   procedure Fu_To_E_Handler
     (Ref              : TO_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List)
   is
      pragma Unreferenced (Module_Type_Defs);
      Ref_Id     : constant String := Ref.Buffer
        (Ref.Referred_Symbol_Name.First .. Ref.Referred_Symbol_Name.Last);
      Enum_Desc  : CType_Description;
      Enum_Def   : E_Table;
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
            Kind        => Enumeration_Kind_Entity,
            Location    => Enum_Def.Start_Position,
            Filename    => Create
              (Enum_Def.Buffer
                 (Enum_Def.File_Name.First .. Enum_Def.File_Name.Last)));

         if Decl_Info = null then
            Insert_Dependency_Declaration
              (Handler            => Handler,
               File               => File,
               List               => List,
               Symbol_Name        => Enum_Def.Buffer
                 (Enum_Def.Name.First .. Enum_Def.Name.Last),
               Referred_Filename  => Create
                 (Enum_Def.Buffer
                    (Enum_Def.File_Name.First .. Enum_Def.File_Name.Last)),
               Location           => Enum_Def.Start_Position,
               Kind               => Enumeration_Kind_Entity,
               Scope              => Global_Scope,
               Declaration_Info   => Decl_Info);
         end if;

      else
         Decl_Info := Find_Declaration
           (File        => File,
            Symbol_Name => Enum_Def.Buffer
              (Enum_Def.Name.First .. Enum_Def.Name.Last),
            Kind        => Enumeration_Kind_Entity,
           Location    => Enum_Def.Start_Position);

         if Decl_Info = null then
            Insert_Declaration
              (File               => File,
               List               => List,
               Symbol_Name        => Enum_Def.Buffer
                 (Enum_Def.Name.First .. Enum_Def.Name.Last),
               Location           => Enum_Def.Start_Position,
               Kind               => Enumeration_Kind_Entity,
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

   ----------------------
   -- Fu_To_Ec_Handler --
   ----------------------

   procedure Fu_To_Ec_Handler
     (Ref              : TO_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List)
   is
      pragma Unreferenced (Module_Type_Defs);
      Decl_Info  : E_Declaration_Info_List;
      Enum_Const : EC_Table;
      Ref_Id     : constant String := Ref.Buffer
        (Ref.Referred_Symbol_Name.First .. Ref.Referred_Symbol_Name.Last);

   begin
      --  Info ("Fu_To_EC_Handler: " & Ref_Id);

      Find (Handler.SN_Table (EC), Ref_Id, Tab => Enum_Const);

      --  Find declaration
      if Xref_Filename_For
        (Create
           (Enum_Const.Buffer
              (Enum_Const.File_Name.First .. Enum_Const.File_Name.Last)),
          Get_DB_Dir (Handler.DB_Dirs, Enum_Const.DBI),
          Handler.Prj_HTable) = Get_LI_Filename (File)
      then
         Decl_Info := Find_Declaration
           (File                    => File,
            Symbol_Name             => Ref_Id,
            Location                => Enum_Const.Start_Position);

         if Decl_Info = null then
            Insert_Declaration
              (File              => File,
               List              => List,
               Symbol_Name       => Ref_Id,
               Location          => Enum_Const.Start_Position,
               Kind              => (Enumeration_Literal, False, False, False),
               Scope             => Global_Scope,
               Declaration_Info  => Decl_Info);
         end if;

      else -- another file
         Decl_Info := Find_Dependency_Declaration
           (File                    => File,
            Symbol_Name             => Ref_Id,
            Filename                => Create
              (Enum_Const.Buffer
                 (Enum_Const.File_Name.First .. Enum_Const.File_Name.Last)),
            Location                => Enum_Const.Start_Position);

         if Decl_Info = null then
            Insert_Dependency_Declaration
              (Handler           => Handler,
               File              => File,
               List              => List,
               Symbol_Name       => Ref_Id,
               Location          => Enum_Const.Start_Position,
               Kind              => (Enumeration_Literal, False, False, False),
               Scope             => Global_Scope,
               Referred_Filename => Create
                 (Enum_Const.Buffer
                    (Enum_Const.File_Name.First .. Enum_Const.File_Name.Last)),
               Declaration_Info  => Decl_Info);
         end if;
      end if;
      Free (Enum_Const);

      Insert_Reference
        (Declaration_Info => Decl_Info,
         File             => File,
         Location         => Ref.Position,
         Kind             => Reference);

   exception
      when DB_Error | Not_Found =>
         Fail ("unable to find enumeration constant " & Ref_Id);
   end Fu_To_Ec_Handler;

   --------------------------
   -- Create_Overload_List --
   --------------------------

   procedure Create_Overload_List
     (Name             : String;
      Class_Name       : String;
      Filename         : Virtual_File;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List)
   is
      P              : Pair_Ptr;
      MDecl          : MD_Table;
      MBody          : FU_Table;
      Decl_Info      : E_Declaration_Info_List;
      MI_File        : LI_File_Ptr;
   begin
      if Is_Open (Handler.SN_Table (MD)) then
         Set_Cursor
           (Handler.SN_Table (MD),
            By_Key,
            Class_Name & Field_Sep & Name & Field_Sep,
            False);

         loop
            P := Get_Pair (Handler.SN_Table (MD), Next_By_Key);
            exit when P = null;
            Parse_Pair (P.all, MDecl);
            Free (P);

            --  ??? Should we compare base or full name here ?
            if MDecl.Buffer (MDecl.File_Name.First .. MDecl.File_Name.Last) /=
              Full_Name (Filename)
            then
               --  this will find/create dependency declaration
               Find_First_Forward_Declaration
                 (MDecl.Buffer,
                  MDecl.Class,
                  MDecl.Name,
                  Filename,
                  MDecl.Return_Type,
                  MDecl.Arg_Types,
                  Handler,
                  File,
                  List,
                  Module_Type_Defs,
                  Decl_Info);
            end if;

            Free (MDecl);
         end loop;

         Release_Cursor (Handler.SN_Table (MD));
      end if;

      if Is_Open (Handler.SN_Table (MI)) then
         Set_Cursor
           (Handler.SN_Table (MI),
            By_Key,
            Class_Name & Field_Sep & Name & Field_Sep,
            False);

         loop
            P := Get_Pair (Handler.SN_Table (MI), Next_By_Key);

            exit when P = null;

            Parse_Pair (P.all, MBody);
            Free (P);

            --  ??? Should we compare full or base name
            if MBody.Buffer (MBody.File_Name.First .. MBody.File_Name.Last) /=
              Full_Name (Filename)
            then
               --  this will find/create dependency declaration
               Find_First_Forward_Declaration
                 (MBody.Buffer,
                  MBody.Class,
                  MBody.Name,
                  Filename,
                  MBody.Return_Type,
                  MBody.Arg_Types,
                  Handler,
                  File,
                  List,
                  Module_Type_Defs,
                  Decl_Info);

               --  MI symbols do not appear without MD
               --  add end of scope and body entity references
               if Decl_Info /= null
                 and then Decl_Info.Value.Declaration.End_Of_Scope
                   = No_Reference
               then
                  MI_File := Locate_From_Source
                    (List,
                     Create (MBody.Buffer
                       (MBody.File_Name.First .. MBody.File_Name.Last)));

                  if MI_File = No_LI_File then
                     Create_Stub_For_File
                       (LI            => MI_File,
                        Handler       => Handler,
                        List          => List,
                        Full_Filename => Create
                          (MBody.Buffer
                             (MBody.File_Name.First .. MBody.File_Name.Last)));
                  end if;

                  Insert_Reference
                    (Decl_Info,
                     MI_File,
                     MBody.Start_Position,
                     Body_Entity);
                  Set_End_Of_Scope (Decl_Info, MI_File, MBody.End_Position);
               end if;
            end if;

            Free (MBody);
         end loop;

         Release_Cursor (Handler.SN_Table (MI));
      end if;
   end Create_Overload_List;

   --------------------------
   -- Create_Overload_List --
   --------------------------

   procedure Create_Overload_List
     (Name     : String;
      Filename : Virtual_File;
      Handler  : access CPP_LI_Handler_Record'Class;
      File     : out LI_File_Ptr;
      List     : LI_File_List)
   is
      P           : Pair_Ptr;
      FDecl       : FD_Table;
      Fn          : FU_Table;
      Decl_Info   : E_Declaration_Info_List;
      Fn_File     : LI_File_Ptr;
      Target_Kind : E_Kind;

   begin
      if Is_Open (Handler.SN_Table (FD)) then
         Set_Cursor (Handler.SN_Table (FD), By_Key, Name & Field_Sep, False);

         loop
            P := Get_Pair (Handler.SN_Table (FD), Next_By_Key);

            exit when P = null;

            Parse_Pair (P.all, FDecl);
            Free (P);

            --  ??? Should we compare full or base name
            if FDecl.Buffer (FDecl.File_Name.First .. FDecl.File_Name.Last) /=
               Full_Name (Filename)
            then
               --  this will find/create dependency declaration
               Find_First_Forward_Declaration
                 (FDecl.Buffer,
                  FDecl.Name,
                  Filename,
                  FDecl.Return_Type,
                  FDecl.Arg_Types,
                  Handler,
                  File,
                  List,
                  Decl_Info);
            end if;

            Free (FDecl);
         end loop;

         Release_Cursor (Handler.SN_Table (FD));
      end if;

      if Is_Open (Handler.SN_Table (FU)) then
         Set_Cursor (Handler.SN_Table (FU), By_Key, Name & Field_Sep, False);

         loop
            P := Get_Pair (Handler.SN_Table (FU), Next_By_Key);

            exit when P = null;

            Parse_Pair (P.all, Fn);
            Free (P);

            --  ??? Should we compare full or base name
            if Fn.Buffer (Fn.File_Name.First .. Fn.File_Name.Last) /=
               Full_Name (Filename)
            then
               --  this will find/create dependency declaration
               Find_First_Forward_Declaration
                 (Fn.Buffer,
                  Fn.Name,
                  Filename,
                  Fn.Return_Type,
                  Fn.Arg_Types,
                  Handler,
                  File,
                  List,
                  Decl_Info);

               if Decl_Info = null then -- only implementation
                  Target_Kind := Get_Function_Kind
                    (Fn.Buffer (Fn.Return_Type.First .. Fn.Return_Type.Last),
                     Fn.Attributes);

                  Insert_Dependency_Declaration
                    (Handler            => Handler,
                     File               => File,
                     List               => List,
                     Symbol_Name        => Name,
                     Referred_Filename  => Create
                       (Fn.Buffer
                          (Fn.File_Name.First .. Fn.File_Name.Last)),
                     Location           => Fn.Start_Position,
                     Kind               => Target_Kind,
                     Scope              => Global_Scope,
                     End_Of_Scope_Location => Fn.End_Position,
                     Declaration_Info   => Decl_Info);

               else -- add end of scope and body entity references
                  Fn_File := Locate_From_Source
                    (List,
                     Create
                       (Fn.Buffer (Fn.File_Name.First .. Fn.File_Name.Last)));

                  if Fn_File = No_LI_File then
                     Create_Stub_For_File
                       (LI            => Fn_File,
                        Handler       => Handler,
                        List          => List,
                        Full_Filename => Create
                          (Fn.Buffer
                             (Fn.File_Name.First .. Fn.File_Name.Last)));
                  end if;

                  Insert_Reference
                    (Decl_Info,
                     Fn_File,
                     Fn.Start_Position,
                     Body_Entity);
                  Set_End_Of_Scope (Decl_Info, Fn_File, Fn.End_Position);
               end if;
            end if;

            Free (Fn);
         end loop;

         Release_Cursor (Handler.SN_Table (FU));
      end if;
   end Create_Overload_List;

   ----------------------
   -- Fu_To_Fu_Handler --
   ----------------------

   procedure Fu_To_Fu_Handler
     (Ref              : TO_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List)
   is
      pragma Unreferenced (Module_Type_Defs);
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
      Buffer         : String_Access;
      Return_Type    : Segment;

   begin
      --  Info ("Fu_To_Fu_Handler: " & Ref_Id);

      if Is_Open (Handler.SN_Table (FD)) then
         Set_Cursor (Handler.SN_Table (FD), By_Key, Ref_Id & Field_Sep, False);

         loop
            P := Get_Pair (Handler.SN_Table (FD), Next_By_Key);

            exit when P = null;

            Parse_Pair (P.all, FDecl_Tmp);
            Free (P);

            if not Forward_Declared then
               FDecl := FDecl_Tmp;
               Forward_Declared := True;
            else
               Overloaded := not Cmp_Arg_Types -- skip multiple fwd decls
                  (FDecl.Buffer,
                   FDecl_Tmp.Buffer,
                   FDecl.Arg_Types,
                   FDecl_Tmp.Arg_Types,
                   Strict => True);
               Free (FDecl_Tmp);

               exit when Overloaded;
            end if;
         end loop;

         Release_Cursor (Handler.SN_Table (FD));
      end if;

      if not Overloaded then
         --  Forward declarations may be overloaded by inline implementations
         --  this is what we check here. If no forward declaration was found
         --  above we search for a suitable function body

         Set_Cursor (Handler.SN_Table (FU), By_Key, Ref_Id & Field_Sep, False);

         loop
            P := Get_Pair (Handler.SN_Table (FU), Next_By_Key);

            exit when P = null;

            Parse_Pair (P.all, Fn_Tmp);
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
                   FDecl.Arg_Types,
                   Strict => True);

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

         Release_Cursor (Handler.SN_Table (FU));
      end if;

      if not Forward_Declared and No_Body then
         Fail ("Can't find either forward declaration or body for " & Ref_Id);
         return;
      end if;

      if not Overloaded then
         if Forward_Declared then
            Buffer         := FDecl.Buffer;
            Return_Type    := FDecl.Return_Type;
         else
            Buffer         := Fn.Buffer;
            Return_Type    := Fn.Return_Type;
         end if;

         Kind := Get_Function_Kind
           (Buffer (Return_Type.First .. Return_Type.Last),
            Fn.Attributes);

         --  this is a function defined in the current file
         --  it may be either forward declared or implemented
         --  right away

         if Forward_Declared then
            Find_First_Forward_Declaration
              (FDecl.Buffer,
               FDecl.Name,
               Create
                 (Ref.Buffer (Ref.File_Name.First .. Ref.File_Name.Last)),
               FDecl.Return_Type,
               FDecl.Arg_Types,
               Handler,
               File,
               List,
               Decl_Info,
               Strict => True);

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
              (File               => File,
               List               => List,
               Symbol_Name        => Ref_Id,
               Location           => Ref.Position,
               Kind               => Kind,
               Scope              => Global_Scope,
               Declaration_Info   => Decl_Info);
         end if;

      else  --  overloaded entity
            --  have we already declared it?
         Assert (Warn_Stream, File /= null,
                 "Fu_To_Fu_Handler, File not created yet");
         --  Here we have to generate all dependency declarations
         --  of the overloaded functions

         Create_Overload_List
           (Ref_Id,
            Create (Ref.Buffer (Ref.File_Name.First .. Ref.File_Name.Last)),
            Handler,
            File,
            List);

         Decl_Info := Find_Declaration
           (File        => File,
            Symbol_Name => Ref_Id,
            Kind        => Overloaded_Entity_Kind);

         if Decl_Info = null then
            Decl_Info := new E_Declaration_Info_Node'
              (Value => (Declaration => No_Declaration,
                         References  => null),
               Next => File.LI.Body_Info.Declarations);
            Decl_Info.Value.Declaration.Name := new String'(Ref_Id);
            Decl_Info.Value.Declaration.Kind := Overloaded_Entity_Kind;
            Decl_Info.Value.Declaration.Location :=
              (File =>  (LI              => File,
                         Part            => Unit_Body,
                         Source_Filename => null),
               Line => 1,
               Column => 1);

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
   end Fu_To_Fu_Handler;

   ----------------------
   -- Fu_To_Gv_Handler --
   ----------------------

   procedure Fu_To_Gv_Handler
     (Ref              : TO_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List)
   is
      Ref_Kind     : Reference_Kind;
      Decl_Info    : E_Declaration_Info_List;
      Var          : GV_Table;
      Desc         : CType_Description;
      Success      : Boolean;
      Scope        : E_Scope := Global_Scope;
      Ref_Id       : constant String := Ref.Buffer
        (Ref.Referred_Symbol_Name.First .. Ref.Referred_Symbol_Name.Last);
      Sym          : FIL_Table;
   begin
      --  Info ("Fu_To_GV_Handler: " & Ref_Id);

      --  we need declaration's location
      Find (Handler.SN_Table (GV), Ref_Id, Tab => Var);

      --  Find declaration
      if Xref_Filename_For
         (Create (Var.Buffer (Var.File_Name.First .. Var.File_Name.Last)),
          Get_DB_Dir (Handler.DB_Dirs, Var.DBI),
          Handler.Prj_HTable) = Get_LI_Filename (File)
      then
         Decl_Info := Find_Declaration
           (File                    => File,
            Symbol_Name             => Ref_Id,
            Location                => Var.Start_Position);

         if Decl_Info = null then
--          Info ("Forward reference to the variable: " & Ref_Id);
            Sym.Buffer         := Var.Buffer;
            Sym.Identifier     := Var.Name;
            Sym.Start_Position := Var.Start_Position;
            Sym.File_Name      := Var.File_Name;
            Sym_GV_Handler
              (Sym, Handler, File, List, Module_Type_Defs, Decl_Info);

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
            Filename                => Create
              (Var.Buffer (Var.File_Name.First .. Var.File_Name.Last)),
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

            if (Var.Attributes and SN_STATIC) = SN_STATIC then
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
                  Referred_Filename => Create
                    (Var.Buffer (Var.File_Name.First .. Var.File_Name.Last)),
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
                  Referred_Filename => Create
                    (Var.Buffer (Var.File_Name.First .. Var.File_Name.Last)),
                  Parent_Location   => Desc.Parent_Point,
                  Parent_Filename   => Create (Desc.Parent_Filename.all),
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
        (Declaration_Info => Decl_Info,
         File             => File,
         Location         => Ref.Position,
         Kind             => Ref_Kind);
   exception
      when Not_Found  | DB_Error => -- ignore
         Fail ("unable to find global variable " & Ref_Id);
   end Fu_To_Gv_Handler;

   ----------------------
   -- Fu_To_Iv_Handler --
   ----------------------

   procedure Fu_To_Iv_Handler
     (Ref              : TO_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List)
   is
      Ref_Kind     : Reference_Kind;
      Decl_Info    : E_Declaration_Info_List;
      Var          : IV_Table;
      Desc         : CType_Description;
      Success      : Boolean;
      Ref_Class    : constant String := Ref.Buffer
        (Ref.Referred_Class.First .. Ref.Referred_Class.Last);
      Ref_Id       : constant String := Ref.Buffer
        (Ref.Referred_Symbol_Name.First .. Ref.Referred_Symbol_Name.Last);
      Sym          : FIL_Table;
      Class_Def    : CL_Table;
   begin
      --  Info ("Fu_To_Iv_Handler: " & Ref_Id);

      if not Is_Open (Handler.SN_Table (IV)) then
         --  IV table does not exist, nothing to do ...
         return;
      end if;

      --  we need declaration's location
      Find (Handler.SN_Table (IV), Ref_Class, Ref_Id, Tab => Var);

      --  Find declaration
      if Xref_Filename_For
         (Create (Var.Buffer (Var.File_Name.First .. Var.File_Name.Last)),
          Get_DB_Dir (Handler.DB_Dirs, Var.DBI),
          Handler.Prj_HTable) = Get_LI_Filename (File)
      then
         Decl_Info := Find_Declaration
           (File                    => File,
            Class_Name              => Ref_Class,
            Symbol_Name             => Ref_Id,
            Location                => Var.Start_Position);

         if Decl_Info = null then
--          Info ("Forward reference to the instance variable: " & Ref_Id);
            Sym.Buffer         := Var.Buffer;
            Sym.Class          := Var.Class;
            Sym.Identifier     := Var.Name;
            Sym.Start_Position := Var.Start_Position;
            Sym.File_Name      := Var.File_Name;
            Sym_IV_Handler
              (Sym, Handler, File, List, Module_Type_Defs, Decl_Info);

            if Decl_Info = null then
               Fail ("unable to create declaration for instance variable "
                       & Ref_Id);
               Free (Var);
               return;
            end if;
         end if;

      else -- another file
         Decl_Info := Find_Dependency_Declaration
           (File                    => File,
            Class_Name              => Ref_Class,
            Symbol_Name             => Ref_Id,
            Filename                => Create
              (Var.Buffer (Var.File_Name.First .. Var.File_Name.Last)),
            Location                => Var.Start_Position);

         if Decl_Info = null then
            --  Collect information about the variable:
            --  type, scope, location of type declaration...
            Find_Class
              (Ref_Class,
               Handler.SN_Table,
               Desc,
               Class_Def,
               Success);

            if not Success then -- try unions
               Find_Union
                 (Ref_Class,
                  Handler.SN_Table,
                  Desc,
                  Class_Def,
                  Success);
            end if;

            if not Success then
               Fail ("Failed to locate class/union: "
                  & Ref_Class
                  & " for instance variable "
                  & Ref_Id);
               Free (Var);
               return;
            end if;

            Free (Desc);

            --  make sure class/union declaration exists
            Find_Or_Create_Class
              (Handler,
               Class_Def,
               Get_LI_Filename (File),
               Decl_Info,
               File,
               List,
               Module_Type_Defs);

            Free (Class_Def);

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

            if Desc.Parent_Point = Invalid_Point then
               Insert_Dependency_Declaration
                 (Handler           => Handler,
                  File              => File,
                  List              => List,
                  Symbol_Name       => Ref_Id,
                  Location          => Var.Start_Position,
                  Kind              => Type_To_Object (Desc.Kind),
                  Scope             => Local_Scope,
                  Referred_Filename => Create
                    (Var.Buffer (Var.File_Name.First .. Var.File_Name.Last)),
                  Declaration_Info  => Decl_Info);
            else
               Insert_Dependency_Declaration
                 (Handler           => Handler,
                  File              => File,
                  List              => List,
                  Symbol_Name       => Ref_Id,
                  Location          => Var.Start_Position,
                  Kind              => Type_To_Object (Desc.Kind),
                  Scope             => Local_Scope,
                  Referred_Filename => Create
                    (Var.Buffer (Var.File_Name.First .. Var.File_Name.Last)),
                  Parent_Location   => Desc.Parent_Point,
                  Parent_Filename   => Create (Desc.Parent_Filename.all),
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
         Fail ("unable to find instance variable " & Ref_Class & "." & Ref_Id);
   end Fu_To_Iv_Handler;

   ----------------------
   -- Fu_To_Ma_Handler --
   ----------------------

   procedure Fu_To_Ma_Handler
     (Ref              : TO_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List)
   is
      pragma Unreferenced (Module_Type_Defs);
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

      Find (Handler.SN_Table (MA), Ref_Id, Tab => Macro);

      if Xref_Filename_For
        (Create
           (Macro.Buffer (Macro.File_Name.First .. Macro.File_Name.Last)),
          Get_DB_Dir (Handler.DB_Dirs, Macro.DBI),
          Handler.Prj_HTable) = Get_LI_Filename (File)
      then
         --  look for declaration in current file
         Decl_Info := Find_Declaration
           (File        => File,
            Symbol_Name => Ref_Id,
            Location    => Macro.Start_Position);

         if Decl_Info = null then
            Insert_Declaration
              (File               => File,
               List               => List,
               Symbol_Name        => Ref_Id,
               Location           => Macro.Start_Position,
               Kind               => Unresolved_Entity_Kind,
               Scope              => Global_Scope,
               Declaration_Info   => Decl_Info);
         end if;

      else
         --  look for dependency declaration
         Decl_Info := Find_Dependency_Declaration
           (File                 => File,
            Symbol_Name          => Ref_Id,
            Filename             => Create
              (Macro.Buffer
                 (Macro.File_Name.First .. Macro.File_Name.Last)),
            Location             => Macro.Start_Position);

         if Decl_Info = null then
            Insert_Dependency_Declaration
              (Handler           => Handler,
               File              => File,
               List              => List,
               Symbol_Name       => Ref_Id,
               Location          => Macro.Start_Position,
               Kind              => Unresolved_Entity_Kind,
               Scope             => Global_Scope,
               Referred_Filename => Create
                 (Macro.Buffer
                    (Macro.File_Name.First .. Macro.File_Name.Last)),
               Declaration_Info  => Decl_Info);
         end if;
      end if;

      Insert_Reference
        (Declaration_Info => Decl_Info,
         File             => File,
         Location         => Ref.Position,
         Kind             => Reference);

      Free (Macro);

   exception
      when DB_Error | Not_Found =>
         Fail ("unable to find macro " & Ref_Id);
   end Fu_To_Ma_Handler;

   ----------------------
   -- Fu_To_Mi_Handler --
   ----------------------

   procedure Fu_To_Mi_Handler
     (Ref              : TO_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List)
   is
      P             : Pair_Ptr;
      Fn            : FU_Table;
      MDecl         : MD_Table;
      MDecl_Tmp     : MD_Table;
      Decl_Info     : E_Declaration_Info_List;
      Overloaded    : Boolean := False;
      Init          : Boolean := True;
      Kind          : E_Kind;
      Ref_Id        : constant String := Ref.Buffer
        (Ref.Referred_Symbol_Name.First .. Ref.Referred_Symbol_Name.Last);
      Ref_Class     : constant String := Ref.Buffer
        (Ref.Referred_Class.First .. Ref.Referred_Class.Last);

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
         Parse_Pair (P.all, MDecl_Tmp);
         Free (P);
         if Init then
            Init  := False;
            MDecl := MDecl_Tmp;
         else
            Overloaded := not Cmp_Arg_Types -- skip multiple fws decls
              (MDecl_Tmp.Buffer,
               MDecl.Buffer,
               MDecl_Tmp.Arg_Types,
               MDecl.Arg_Types,
               Strict => True);
            Free (MDecl_Tmp);
            exit when Overloaded;
         end if;
      end loop;
      Release_Cursor (Handler.SN_Table (MD));

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

            Parse_Pair (P.all, Fn);

            Free (P);
            Init := False;

            exit when Cmp_Arg_Types
              (MDecl.Buffer,
               Fn.Buffer,
               MDecl.Arg_Types,
               Fn.Arg_Types,
               Strict => True);

            Init := True;
            Free (Fn);
         end loop;

         Release_Cursor (Handler.SN_Table (MI));

         if Init then -- implementation for the referred method not found
            --  this must be a pure virtual method
            if (MDecl.Attributes and SN_PUREVIRTUAL) /= SN_PUREVIRTUAL then
               Fail ("failed to locate method implementation, but it is not"
                  & " an abstract one: " & Ref_Class & "::" & Ref_Id);
               Free (MDecl);

               return;
            end if;

         else
            Free (Fn);
         end if;

         Kind := Get_Method_Kind
           (Handler,
            Ref_Class,
            MDecl.Buffer (MDecl.Return_Type.First .. MDecl.Return_Type.Last),
            MDecl.Attributes);

         Find_First_Forward_Declaration
           (MDecl.Buffer,
            MDecl.Class,
            MDecl.Name,
            Create (Ref.Buffer (Ref.File_Name.First .. Ref.File_Name.Last)),
            MDecl.Return_Type,
            MDecl.Arg_Types,
            Handler,
            File,
            List,
            Module_Type_Defs,
            Decl_Info,
            Strict => True);

         if Decl_Info = null then
            --  method is used before
            --  declaration. Create forward declaration
            Insert_Declaration
              (File               => File,
               List               => List,
               Symbol_Name        => Ref_Id,
               Location           => Ref.Position,
               Kind               => Kind,
               Scope              => Global_Scope,
               Declaration_Info   => Decl_Info);
         end if;

      else -- overloaded entity
         --  have we already declared it?
         Create_Overload_List
           (Ref_Id,
            Ref_Class,
            Create (Ref.Buffer (Ref.File_Name.First .. Ref.File_Name.Last)),
            Handler,
            File,
            List,
            Module_Type_Defs);

         declare
            Class_Def : CL_Table;
         begin
            Find (Handler.SN_Table (CL), Ref_Class, Tab => Class_Def);
            --  ??? what to do when several classes with one name are available
            --  what about unions?

            Decl_Info := Find_Declaration
              (File        => File,
               Symbol_Name => Ref_Id,
               Class_Name  => Ref_Class,
               Kind        => Overloaded_Entity_Kind,
               Location    => Class_Def.Start_Position);

            if Decl_Info = null then
               Decl_Info := new E_Declaration_Info_Node'
                 (Value =>
                    (Declaration => No_Declaration,
                     References => null),
                  Next => File.LI.Body_Info.Declarations);
               Decl_Info.Value.Declaration.Name := new String'(Ref_Id);
               Decl_Info.Value.Declaration.Kind := Overloaded_Entity_Kind;
               Decl_Info.Value.Declaration.Location :=
                 (File   => (LI              => File,
                             Part            => Unit_Body,
                             Source_Filename => null),
                  Line   => Class_Def.Start_Position.Line,
                  Column => Class_Def.Start_Position.Column);
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

   ---------------------
   -- Fu_To_T_Handler --
   ---------------------

   procedure Fu_To_T_Handler
     (Ref              : TO_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List)
   is
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

      Find (Handler.SN_Table (T), Ref_Id, Tab => Typedef);

      if Xref_Filename_For
        (Create
          (Typedef.Buffer (Typedef.File_Name.First .. Typedef.File_Name.Last)),
          Get_DB_Dir (Handler.DB_Dirs, Typedef.DBI),
          Handler.Prj_HTable) = Get_LI_Filename (File)
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
                 (File              => File,
                  List              => List,
                  Symbol_Name       => Ref_Id,
                  Location          => Typedef.Start_Position,
                  Kind              => Desc.Kind,
                  Scope             => Global_Scope,
                  Declaration_Info  => Decl_Info);
            elsif Desc.Ancestor_Point = Predefined_Point then
               --  typedef for builtin type
               Insert_Declaration
                 (File              => File,
                  List              => List,
                  Symbol_Name       => Ref_Id,
                  Location          => Typedef.Start_Position,
                  Parent_Location   => Predefined_Point,
                  Kind              => Desc.Kind,
                  Scope             => Global_Scope,
                  Declaration_Info  => Decl_Info);
            else
               --  parent type found
               Insert_Declaration
                 (File              => File,
                  List              => List,
                  Symbol_Name       => Ref_Id,
                  Location          => Typedef.Start_Position,
                  Parent_Location   => Desc.Ancestor_Point,
                  Parent_Filename   => Create (Desc.Ancestor_Filename.all),
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
            Filename             => Create
              (Typedef.Buffer
                 (Typedef.File_Name.First .. Typedef.File_Name.Last)),
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
                  Referred_Filename => Create
                    (Typedef.Buffer
                       (Typedef.File_Name.First .. Typedef.File_Name.Last)),
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
                  Referred_Filename => Create
                    (Typedef.Buffer
                       (Typedef.File_Name.First .. Typedef.File_Name.Last)),
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
                  Parent_Filename   => Create (Desc.Ancestor_Filename.all),
                  Kind              => Desc.Kind,
                  Scope             => Global_Scope,
                  Referred_Filename => Create
                    (Typedef.Buffer
                       (Typedef.File_Name.First .. Typedef.File_Name.Last)),
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
      Free (Desc);

   exception
      when DB_Error | Not_Found  =>
         Fail ("unable to find typedef " & Ref_Id);
   end Fu_To_T_Handler;

   ----------------------
   -- Fu_To_Un_Handler --
   ----------------------

   procedure Fu_To_Un_Handler
     (Ref              : TO_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List)
   is
      pragma Unreferenced (Module_Type_Defs);
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
            Kind        => Non_Generic_Class,
            Location    => Union_Def.Start_Position,
            Filename    => Create
              (Union_Def.Buffer
                 (Union_Def.File_Name.First .. Union_Def.File_Name.Last)));

         if Decl_Info = null then
            Insert_Dependency_Declaration
              (Handler            => Handler,
               File               => File,
               List               => List,
               Symbol_Name        => Union_Def.Buffer
                 (Union_Def.Name.First .. Union_Def.Name.Last),
               Referred_Filename  => Create
                 (Union_Def.Buffer
                    (Union_Def.File_Name.First .. Union_Def.File_Name.Last)),
               Location           => Union_Def.Start_Position,
               Kind               => Non_Generic_Class,
               Scope              => Global_Scope,
               Declaration_Info   => Decl_Info);
         end if;

      else
         Decl_Info := Find_Declaration
           (File        => File,
            Symbol_Name => Union_Def.Buffer
              (Union_Def.Name.First .. Union_Def.Name.Last),
            Kind        => Non_Generic_Class,
            Location    => Union_Def.Start_Position);

         if Decl_Info = null then
            Insert_Declaration
              (File               => File,
               List               => List,
               Symbol_Name        => Union_Def.Buffer
                 (Union_Def.Name.First .. Union_Def.Name.Last),
               Location           => Union_Def.Start_Position,
               Kind               => Non_Generic_Class,
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
     (Sym              : FIL_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List)
   is
      Decl_Info      : E_Declaration_Info_List;
   begin
      Sym_CL_Handler
        (Sym, Handler, File, List, Module_Type_Defs, Decl_Info);
   end Sym_CL_Handler;

   --------------------
   -- Sym_CL_Handler --
   --------------------

   procedure Sym_CL_Handler
     (Sym              : FIL_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List;
      Decl_Info        : out E_Declaration_Info_List)
   is
      Desc           : CType_Description;
      Class_Def      : CL_Table;
      Success        : Boolean;
      P              : Pair_Ptr;
      Super          : IN_Table;
      Super_Def      : CL_Table;
      Super_Desc     : CType_Description;
   begin
      Decl_Info := null;

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
        (File                  => File,
         List                  => List,
         Symbol_Name           =>
           Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last),
         Location              => Sym.Start_Position,
         Kind                  => Desc.Kind,
         Scope                 => Global_Scope,
         End_Of_Scope_Location => Class_Def.End_Position,
         Declaration_Info      => Decl_Info);

      --  Adjust EOS reference kind
      Decl_Info.Value.Declaration.End_Of_Scope.Kind := End_Of_Spec;

      if Desc.Is_Template then
         Process_Template_Arguments
           (CL,
            Invalid_FU_Table,
            Class_Def,
            Handler,
            File,
            List,
            Module_Type_Defs);
      end if;

      --  Find all the base classes for this one
      if Is_Open (Handler.SN_Table (SN_IN)) then
         begin
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

               Parse_Pair (P.all, Super);

               --  Lookup base class definition to find its precise location

               Find_Class
                 (Super.Buffer
                    (Super.Base_Class.First .. Super.Base_Class.Last),
                  Handler.SN_Table,
                  Super_Desc,
                  Super_Def,
                  Success);
               if Success then -- if found, add it to parent list
                  Add_Parent
                    (Decl_Info,
                     Handler => CPP_LI_Handler (Handler),
                     List => List,
                     Parent_Filename => Create
                       (Super_Def.Buffer
                          (Super_Def.File_Name.First ..
                             Super_Def.File_Name.Last)),
                     Parent_Location => Super_Def.Start_Position);
                  Free (Super_Desc);
                  Free (Super_Def);
               end if;
               Free (Super);
               Free (P);
            end loop;
            Release_Cursor (Handler.SN_Table (SN_IN));
         exception
            when DB_Error => -- something went wrong, ignore it
               null;
         end;
      end if;

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
     (Sym              : FIL_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List)
   is
      Desc              : CType_Description;
      Var               : GV_Table;
      Success           : Boolean;
      Decl_Info         : E_Declaration_Info_List;
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
      Find
        (Handler.SN_Table (CON),
         Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last),
         Tab => Var);

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
         Desc.Kind := Unresolved_Entity_Kind;
      end if;

      if (Var.Attributes and SN_STATIC) = SN_STATIC then
         Scope := Static_Local;
      end if;

      if Desc.Parent_Point = Invalid_Point then
         Insert_Declaration
           (File              => File,
            List              => List,
            Symbol_Name       =>
              Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last),
            Location          => Sym.Start_Position,
            Kind              => Type_To_Object (Desc.Kind),
            Scope             => Scope,
            Declaration_Info  => Decl_Info);
      else
         Insert_Declaration
           (File              => File,
            List              => List,
            Symbol_Name       =>
              Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last),
            Location          => Sym.Start_Position,
            Kind              => Type_To_Object (Desc.Kind),
            Scope             => Scope,
            Parent_Location   => Desc.Parent_Point,
            Parent_Filename   => Create (Desc.Parent_Filename.all),
            Declaration_Info  => Decl_Info);

            --  add reference to the type of this variable
         if Desc.Is_Template then
            --  template specialization
            Refer_Type
              (Plain_Class_Name
                 (Var.Buffer (Var.Value_Type.First .. Var.Value_Type.Last)),
               Desc.Parent_Point,
               File,
               Var.Type_Start_Position,
               Instantiation_Reference);
         else
            --  default reference kind
            Refer_Type
              (Var.Buffer (Var.Value_Type.First .. Var.Value_Type.Last),
               Desc.Parent_Point,
               File,
               Var.Type_Start_Position);
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
     (Sym              : FIL_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List)
   is
      pragma Unreferenced
        (Sym, Handler, File, List, Module_Type_Defs);
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
     (Sym              : FIL_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List)
   is
      pragma Unreferenced (Handler, Module_Type_Defs);
      Decl_Info : E_Declaration_Info_List;
      E_Id      : constant String := Sym.Buffer
        (Sym.Identifier.First .. Sym.Identifier.Last);

   begin
      --  Info ("Sym_E_Hanlder: """ & E_Id & """");

      Insert_Declaration
        (File              => File,
         List              => List,
         Symbol_Name       => E_Id,
         Location          => Sym.Start_Position,
         Kind              => Enumeration_Kind_Entity,
         Scope             => Global_Scope,
         Declaration_Info  => Decl_Info);
   end Sym_E_Handler;

   --------------------
   -- Sym_EC_Handler --
   --------------------

   procedure Sym_EC_Handler
     (Sym              : FIL_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List)
   is
      pragma Unreferenced (Module_Type_Defs);

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
            EC_Def : EC_Table;
            E_Def  : E_Table;
         begin
            Find (Handler.SN_Table (EC),
                  Ec_Id, Sym.Start_Position, Tab => EC_Def);
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
           (File              => File,
            List              => List,
            Symbol_Name       => Ec_Id,
            Location          => Sym.Start_Position,
            Kind              => (Enumeration_Literal, False, False, False),
            Parent_Location   => Desc.Parent_Point,
            Parent_Filename   => Create (Desc.Parent_Filename.all),
            Scope             => Global_Scope,
            Declaration_Info  => Decl_Info);
      else
         Fail ("could not find enum for '" & Ec_Id & "'");
         Insert_Declaration
           (File              => File,
            List              => List,
            Symbol_Name       => Ec_Id,
            Location          => Sym.Start_Position,
            Kind              => (Enumeration_Literal, False, False, False),
            Scope             => Global_Scope,
            Declaration_Info  => Decl_Info);
      end if;
   end Sym_EC_Handler;

   --------------------
   -- Sym_FD_Handler --
   --------------------

   procedure Sym_FD_Handler
     (Sym              : FIL_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List)
   is
      pragma Unreferenced (Module_Type_Defs);

      Target_Kind  : E_Kind;
      Decl_Info    : E_Declaration_Info_List;
      P            : Pair_Ptr;
      First_FD_Pos : Point := Invalid_Point;
      FD_Tab       : FD_Table;
      FD_Tab_Tmp   : FD_Table;
      FU_Tab       : FU_Table;
      Is_Static    : Boolean;
      Match        : Boolean;
      FU_File      : LI_File_Ptr;

   begin
      --  Info ("Sym_FD_Hanlder: """
      --        & Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last)
      --        & """");

      --  Find this symbol
      Find
        (Handler.SN_Table (FD),
         Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last),
         Sym.Start_Position,
         Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
         Tab => FD_Tab);

      Is_Static  := (FD_Tab.Attributes and SN_STATIC) = SN_STATIC;

      Set_Cursor
        (Handler.SN_Table (FD),
         By_Key,
         Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last) & Field_Sep,
         False);

      loop
         P := Get_Pair (Handler.SN_Table (FD), Next_By_Key);

         exit when P = null;

         Parse_Pair (P.all, FD_Tab_Tmp);
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
            FD_Tab_Tmp.Return_Type,
            Strict => True);

         if (Match and then First_FD_Pos = Invalid_Point)
           or else FD_Tab_Tmp.Start_Position < First_FD_Pos
         then
            First_FD_Pos := FD_Tab_Tmp.Start_Position;
         end if;

         Free (FD_Tab_Tmp);
      end loop;

      Release_Cursor (Handler.SN_Table (FD));

      Assert (Fail_Stream, First_FD_Pos /= Invalid_Point, "DB inconsistency");

      Target_Kind := Get_Function_Kind
        (FD_Tab.Buffer (FD_Tab.Return_Type.First .. FD_Tab.Return_Type.Last),
         FD_Tab.Attributes);

      Decl_Info := Find_Declaration
        (File        => File,
         Symbol_Name => Sym.Buffer
            (Sym.Identifier.First .. Sym.Identifier.Last),
         Location    => First_FD_Pos);

      if Decl_Info = null then
         Insert_Declaration
           (File              => File,
            List              => List,
            Symbol_Name       =>
              Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last),
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

            Parse_Pair (P.all, FU_Tab);
            Free (P);
            Match := Cmp_Prototypes
               (FD_Tab.Buffer,
                FU_Tab.Buffer,
                FD_Tab.Arg_Types,
                FU_Tab.Arg_Types,
                FD_Tab.Return_Type,
                FU_Tab.Return_Type,
                Strict => True);

            exit when Match;

            Free (FU_Tab);
         end loop;

         Release_Cursor (Handler.SN_Table (FU));

         if Match -- we found the body
            and then FU_Tab.Buffer (FU_Tab.File_Name.First ..
                                    FU_Tab.File_Name.Last)
               /= FD_Tab.Buffer (FD_Tab.File_Name.First ..
                                 FD_Tab.File_Name.Last)
               --  and it is in another file
         then
            FU_File := Locate_From_Source
               (List,
               Create
                 (FU_Tab.Buffer
                    (FU_Tab.File_Name.First .. FU_Tab.File_Name.Last)));

            if FU_File = No_LI_File then
               Create_Stub_For_File
                 (LI            => FU_File,
                  Handler       => Handler,
                  List          => List,
                  Full_Filename => Create
                    (FU_Tab.Buffer
                       (FU_Tab.File_Name.First .. FU_Tab.File_Name.Last)));
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
     (Sym              : FIL_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List)
   is
      Decl_Info       : E_Declaration_Info_List := null;
      Target_Kind     : E_Kind;
      P               : Pair_Ptr;
      FU_Tab          : aliased FU_Table;
      Start_Position  : constant Point := Sym.Start_Position;
      Body_Position   : Point := Invalid_Point;
      End_Position    : Point;
      Ref             : TO_Table;
      Our_Ref         : Boolean;
      Class_Def       : CL_Table := Invalid_CL_Table;
      Class_Decl_Info : E_Declaration_Info_List;
      Fu_Id           : constant String := Sym.Buffer
        (Sym.Identifier.First .. Sym.Identifier.Last);

   begin
      --  Info ("Sym_FU_Hanlder: """
      --        & Sym.Buffer (Sym.Class.First .. Sym.Class.Last) & "."
      --        & Fu_Id
      --        & """");

      if Sym.Symbol = MI then
         begin
            Find
              (Handler.SN_Table (MI),
               Sym.Buffer (Sym.Class.First .. Sym.Class.Last),
               Fu_Id,
               Sym.Start_Position,
               Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
               Tab => FU_Tab);

            begin -- check if this class is template
               Find
                 (Handler.SN_Table (CL),
                  Sym.Buffer (Sym.Class.First .. Sym.Class.Last),
                  Tab => Class_Def);
               Target_Kind := Get_Method_Kind
                 (Class_Def,
                  FU_Tab.Buffer (FU_Tab.Return_Type.First ..
                                 FU_Tab.Return_Type.Last),
                  FU_Tab.Attributes);
               Find_Or_Create_Class
                  (Handler,
                   Class_Def,
                  Create
                    (Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last)),
                   Class_Decl_Info,
                   File,
                   List,
                   Module_Type_Defs);

               if not (Class_Def.Start_Position < Sym.Start_Position
                       and Sym.End_Position < Class_Def.End_Position)
               then
                  Insert_Reference
                    (Class_Decl_Info,
                     File,
                     (Sym.Start_Position.Line, 0),
                     --  we don't know the precise position of the class
                     --  name, so set the column to "anywhere"
                     Reference);
               end if;

            exception
               when DB_Error | Not_Found =>
                  null;
            end;

            End_Position := FU_Tab.End_Position;
         exception
            when DB_Error | Not_Found =>
               Fail ("unable to find method "
                     & Sym.Buffer (Sym.Class.First .. Sym.Class.Last) & "."
                     & Fu_Id);
               return;
         end;
      else
         begin
            Find
              (Handler.SN_Table (FU),
               Fu_Id,
               Sym.Start_Position,
               Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
               Tab => FU_Tab);
            Target_Kind := Get_Function_Kind
               (FU_Tab.Buffer (FU_Tab.Return_Type.First ..
                               FU_Tab.Return_Type.Last),
                FU_Tab.Attributes);
            End_Position := FU_Tab.End_Position;
         exception
            when DB_Error | Not_Found =>
               Fail ("unable to find function " & Fu_Id);
               return;
         end;
      end if;

      --  Detect forward declaration. If there are many declarations
      --  we should not try to interpret them, 'cause it may be
      --  overloading.
      --  If exist only one, Start_Position
      --  should point to it and we have to add Body_Entity reference
      --  Otherwise Start_Position should point directly to the body.
      --  We should also try to find GPS declaration created during
      --  FD processing and not create new declaration.

      if Sym.Symbol = MI then
         Find_First_Forward_Declaration
           (FU_Tab.Buffer,
            FU_Tab.Class,
            FU_Tab.Name,
            Create (Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last)),
            FU_Tab.Return_Type,
            FU_Tab.Arg_Types,
            Handler,
            File,
            List,
            Module_Type_Defs,
            Decl_Info,
            Strict => True);

         if Decl_Info /= null then -- Body_Entity is inserted only w/ fwd decl
            Body_Position := Sym.Start_Position;
         end if;
      else
         --  Try to find forward declaration
         Find_First_Forward_Declaration
           (FU_Tab.Buffer,
            FU_Tab.Name,
            Create (Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last)),
            FU_Tab.Return_Type,
            FU_Tab.Arg_Types,
            Handler,
            File,
            List,
            Decl_Info,
            Strict => True);
         if Decl_Info /= null then -- Body_Entity is inserted only w/ fwd decl
            Body_Position := Sym.Start_Position;
         end if;
      end if;

      if Decl_Info = null then
         Insert_Declaration
           (File                  => File,
            List                  => List,
            Symbol_Name           => Fu_Id,
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

      if Target_Kind.Is_Generic then
         Process_Template_Arguments
           (Sym.Symbol,
            FU_Tab,
            Class_Def,
            Handler,
            File,
            List,
            Module_Type_Defs);
      end if;

      if (Sym.Symbol = MI) and (Class_Def /= Invalid_CL_Table) then
         Free (Class_Def);
      end if;

      --  Declaration inserted. Now we need to check the body for usage
      --  of objects of all kinds

      Set_Cursor
        (Handler.SN_Table (TO),
         Position => By_Key,
         Key => Sym.Buffer (Sym.Class.First .. Sym.Class.Last) & Field_Sep &
                Fu_Id &
                Field_Sep & To_String (Sym.Symbol) & Field_Sep,
         Exact_Match => False);

      loop
         P := Get_Pair (Handler.SN_Table (TO), Next_By_Key);
         exit when P = null;

         begin
            Parse_Pair (P.all, Ref);

            if Fu_To_Handlers (Ref.Referred_Symbol) /= null then
               Our_Ref := Cmp_Arg_Types
                 (Ref.Buffer,
                  FU_Tab.Buffer,
                  Ref.Caller_Argument_Types,
                  FU_Tab.Arg_Types);

               if Our_Ref and then
                  Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last) =
                  Ref.Buffer (Ref.File_Name.First .. Ref.File_Name.Last)
               then
                  Fu_To_Handlers (Ref.Referred_Symbol)
                    (Ref, Handler, File, List, Module_Type_Defs);
               end if;
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
      Release_Cursor (Handler.SN_Table (TO));

      Process_Local_Variables
        (FU_Tab, Sym.Symbol, Handler, File, List, Module_Type_Defs);

      Free (FU_Tab);

   exception
      when DB_Error => null; -- non-existent table .to, ignore it
   end Sym_FU_Handler;

   --------------------
   -- Sym_GV_Handler --
   --------------------
   --  NOTE: this handler is called from fu-to-gv handler as well

   procedure Sym_GV_Handler
     (Sym              : FIL_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List)
   is
      Decl_Info : E_Declaration_Info_List;
   begin
      Sym_GV_Handler (Sym, Handler, File, List, Module_Type_Defs, Decl_Info);
   end Sym_GV_Handler;

   --------------------
   -- Sym_GV_Handler --
   --------------------

   procedure Sym_GV_Handler
     (Sym              : FIL_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List;
      Decl_Info        : out E_Declaration_Info_List)
   is
      Desc      : CType_Description;
      Var       : GV_Table;
      Success   : Boolean;
      Scope     : E_Scope := Global_Scope;

   begin
      Decl_Info := null;

      --  Info ("Sym_GV_Handler: """
      --        & Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last)
      --        & """");

      if not Is_Open (Handler.SN_Table (GV)) then
         --  GV table does not exist, nothing to do ...
         return;
      end if;

      --  Lookup variable type
      Find
       (Handler.SN_Table (GV),
        Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last),
        Tab => Var);

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
         Desc.Kind := Unresolved_Entity_Kind;
      end if;

      if (Var.Attributes and SN_STATIC) = SN_STATIC then
         Scope := Static_Local;
      end if;

      if Desc.Parent_Point = Invalid_Point then
         Insert_Declaration
           (File              => File,
            List              => List,
            Symbol_Name       =>
              Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last),
            Location          => Sym.Start_Position,
            Kind              => Type_To_Object (Desc.Kind),
            Scope             => Scope,
            Declaration_Info  => Decl_Info);
      else
         Insert_Declaration
           (File              => File,
            List              => List,
            Symbol_Name       =>
              Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last),
            Location          => Sym.Start_Position,
            Kind              => Type_To_Object (Desc.Kind),
            Scope             => Scope,
            Parent_Location   => Desc.Parent_Point,
            Parent_Filename   => Create (Desc.Parent_Filename.all),
            Declaration_Info  => Decl_Info);

         --  add reference to the type of this variable
         if Desc.Is_Template then
            --  template specialization
            Refer_Type
              (Plain_Class_Name
                 (Var.Buffer (Var.Value_Type.First .. Var.Value_Type.Last)),
               Desc.Parent_Point,
               File,
               Var.Type_Start_Position,
               Instantiation_Reference);
         else
            --  default reference kind
            Refer_Type
              (Var.Buffer (Var.Value_Type.First .. Var.Value_Type.Last),
               Desc.Parent_Point,
               File,
               Var.Type_Start_Position);
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
     (Sym              : FIL_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List)
   is
      pragma Unreferenced (Module_Type_Defs);

      Filename : constant String := Sym.Buffer
        (Sym.Identifier.First .. Sym.Identifier.Last);

      --  ??? We shouldn't use Base_Name below, but should allow find file to
      --  recognize directories in the name.
      Full_Included : constant String := Get_Full_Path_From_File
        (Registry        => Project_Registry
         (Get_Registry (Handler.Root_Project)),
         Filename        => Base_Name (Filename),
         Use_Source_Path => True,
         Use_Object_Path => False);

   begin
      --  Info ("Sym_IU_Handler: """
      --        & Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last)
      --        & " depends on """ & Full_Included
      --        & """/""" & Filename & """");

      if Full_Included = "" then
         Info ("File not found on path: " & Filename);

         --  Put xref files for missing includes to the root project DB dir
         Insert_Dependency
           (Handler           => Handler,
            File              => File,
            List              => List,
            Referred_Filename => Create (Filename));
      else
         Insert_Dependency
           (Handler           => Handler,
            File              => File,
            List              => List,
            Referred_Filename => Create (Full_Included));
      end if;
   end Sym_IU_Handler;

   --------------------
   -- Sym_IV_Handler --
   --------------------

   procedure Sym_IV_Handler
     (Sym              : FIL_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List)
   is
      Decl_Info : E_Declaration_Info_List;
   begin
      Sym_IV_Handler
        (Sym, Handler, File, List, Module_Type_Defs, Decl_Info);
   end Sym_IV_Handler;

   --------------------
   -- Sym_IV_Handler --
   --------------------

   procedure Sym_IV_Handler
     (Sym              : FIL_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List;
      Decl_Info        : out E_Declaration_Info_List)
   is
      Inst_Var  : IV_Table;
      Success   : Boolean;
      Desc      : CType_Description;
      Class_Def : CL_Table;

   begin
      Decl_Info := null;
      --  Info ("Sym_IV_Handler: """
      --        & Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last)
      --        & """");

      if not Is_Open (Handler.SN_Table (IV)) then
         --  IV table does not exist, nothing to do ...
         return;
      end if;

      --  Lookup instance variable
      Find
        (Handler.SN_Table (IV),
         Sym.Buffer (Sym.Class.First .. Sym.Class.Last),
         Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last),
         Tab => Inst_Var);

      Find_Class
        (Sym.Buffer (Sym.Class.First .. Sym.Class.Last),
         Handler.SN_Table,
         Desc,
         Class_Def,
         Success);

      if not Success then -- try unions
         Find_Union
           (Sym.Buffer (Sym.Class.First .. Sym.Class.Last),
            Handler.SN_Table,
            Desc,
            Class_Def,
            Success);
      end if;

      if not Success then
         Fail ("Failed to locate class/union: "
            & Sym.Buffer (Sym.Class.First .. Sym.Class.Last)
            & " for instance variable "
            & Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last));
         Free (Inst_Var);
         return;
      end if;

      --  Determine iv type
      if Desc.Is_Template then
         Free (Desc);
         Type_Name_To_Kind
           (Inst_Var.Buffer
              (Inst_Var.Value_Type.First .. Inst_Var.Value_Type.Last),
            Handler.SN_Table,
            Module_Type_Defs,
            Desc,
            Success,
            CL,
            Invalid_FU_Table,
            Class_Def);
      else
         Free (Desc);
         Type_Name_To_Kind
           (Inst_Var.Buffer
              (Inst_Var.Value_Type.First .. Inst_Var.Value_Type.Last),
            Handler.SN_Table,
            Module_Type_Defs,
            Desc,
            Success);
      end if;

      Free (Class_Def);

      if not Success then
         --  Cannot determine type of this instance variable
         Free (Inst_Var);
         return;
      end if;

      if Desc.Parent_Point = Invalid_Point then
         Insert_Declaration
           (File              => File,
            List              => List,
            Symbol_Name       =>
              Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last),
            Location          => Sym.Start_Position,
            Kind              => Type_To_Object (Desc.Kind),
            Scope             => Local_Scope,
            Declaration_Info  => Decl_Info);
      else
         Insert_Declaration
           (File              => File,
            List              => List,
            Symbol_Name       =>
              Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last),
            Location          => Sym.Start_Position,
            Kind              => Type_To_Object (Desc.Kind),
            Scope             => Local_Scope,
            Parent_Location   => Desc.Parent_Point,
            Parent_Filename   => Create (Desc.Parent_Filename.all),
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
         Fail ("Sym_IV_Handler: unexpected exception");
         Decl_Info := null;
   end Sym_IV_Handler;

   --------------------
   -- Sym_MA_Handler --
   --------------------

   procedure Sym_MA_Handler
     (Sym              : FIL_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List)
   is
      pragma Unreferenced (Handler, Module_Type_Defs);
      tmp_ptr    : E_Declaration_Info_List;
   begin
      --  Info ("Sym_MA_Handler: """
      --        & Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last)
      --        & """");

      Insert_Declaration
        (File              => File,
         List              => List,
         Symbol_Name       =>
           Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last),
         Location          => Sym.Start_Position,
         Kind              => Unresolved_Entity_Kind,
         Scope             => Global_Scope,
         Declaration_Info  => tmp_ptr);
   end Sym_MA_Handler;

   --------------------
   -- Sym_MD_Handler --
   --------------------

   procedure Sym_MD_Handler
     (Sym              : FIL_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List)
   is
      Target_Kind  : E_Kind;
      Decl_Info    : E_Declaration_Info_List;
      P            : Pair_Ptr;
      First_MD_Pos : Point := Invalid_Point;
      MD_Tab       : MD_Table;
      MI_Tab       : FU_Table;
      MD_Tab_Tmp   : MD_Table;
      Found        : Boolean;
      MI_File      : LI_File_Ptr;

   begin
      --  Info ("Sym_MD_Hanlder: """
      --      & Sym.Buffer (Sym.Class.First .. Sym.Class.Last) & "::"
      --      & Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last) & " "
      --      & Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last)
      --      & """");

      --  Find this symbol
      Find
        (Handler.SN_Table (MD),
         Sym.Buffer (Sym.Class.First .. Sym.Class.Last),
         Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last),
         Sym.Start_Position,
         Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
         MD_Tab);

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
         Parse_Pair (P.all, MD_Tab_Tmp);
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
              MD_Tab_Tmp.Return_Type,
              Strict => True)
           and then ((First_MD_Pos = Invalid_Point)
                     or else MD_Tab_Tmp.Start_Position < First_MD_Pos)
         then
            First_MD_Pos := MD_Tab_Tmp.Start_Position;
         end if;

         Free (MD_Tab_Tmp);
      end loop;

      Release_Cursor (Handler.SN_Table (MD));
      Assert (Fail_Stream, First_MD_Pos /= Invalid_Point, "DB inconsistency");

      declare
         Class_Def          : CL_Table;
         Class_Decl_Info    : E_Declaration_Info_List;
      begin -- check if this class is template
         Find
           (Handler.SN_Table (CL),
            Sym.Buffer (Sym.Class.First .. Sym.Class.Last),
            Tab => Class_Def);
         Target_Kind := Get_Method_Kind
           (Class_Def,
            MD_Tab.Buffer (MD_Tab.Return_Type.First ..
                           MD_Tab.Return_Type.Last),
            MD_Tab.Attributes);
         --  Add reference to the class we belong to
         Find_Or_Create_Class
           (Handler,
            Class_Def,
            Create (Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last)),
            Class_Decl_Info, File, List, Module_Type_Defs);

         if Class_Decl_Info /= null then
            Insert_Reference
              (Class_Decl_Info,
               File,
               (Sym.Start_Position.Line, Sym.Start_Position.Column),
               Primitive_Operation);
         end if;

         Free (Class_Def);

      exception
         when DB_Error | Not_Found =>
            null;
      end;

      Decl_Info := Find_Declaration
        (File        => File,
         Symbol_Name => Sym.Buffer
            (Sym.Identifier.First .. Sym.Identifier.Last),
         Location    => First_MD_Pos);

      if Decl_Info = null then
         Insert_Declaration
           (File              => File,
            List              => List,
            Symbol_Name       =>
               Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last),
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

            Parse_Pair (P.all, MI_Tab);
            Free (P);
            Found := Cmp_Prototypes
               (MD_Tab.Buffer,
                MI_Tab.Buffer,
                MD_Tab.Arg_Types,
                MI_Tab.Arg_Types,
                MD_Tab.Return_Type,
                MI_Tab.Return_Type,
                Strict => True);

            exit when Found;

            Free (MI_Tab);
         end loop;

         Release_Cursor (Handler.SN_Table (MI));

         if Found -- we found the body
           and then MI_Tab.Buffer (MI_Tab.File_Name.First ..
                                   MI_Tab.File_Name.Last)
             /= MD_Tab.Buffer (MD_Tab.File_Name.First ..
                               MD_Tab.File_Name.Last)
         --  and it is in another file
         then
            MI_File := Locate_From_Source
              (List,
               Create
                 (MI_Tab.Buffer
                    (MI_Tab.File_Name.First .. MI_Tab.File_Name.Last)));

            if MI_File = No_LI_File then
               Create_Stub_For_File
                 (LI            => MI_File,
                  Handler       => Handler,
                  List          => List,
                  Full_Filename => Create
                    (MI_Tab.Buffer
                       (MI_Tab.File_Name.First .. MI_Tab.File_Name.Last)));
            end if;

            if MI_File /= File
              or else MD_Tab.Start_Position /= MI_Tab.Start_Position
            then
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
     (Sym              : FIL_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List)
   is
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
              (File              => File,
               List              => List,
               Symbol_Name       => Identifier,
               Location          => Sym.Start_Position,
               Kind              => Desc.Kind,
               Scope             => Global_Scope,
               Declaration_Info  => Decl_Info);

         elsif Desc.Ancestor_Point = Predefined_Point then
            --  parent type is builtin: set parent location to predefined one
            --  ??? Builtin_Name is not used anywhere. We should
            --  use it (e.g. for a field like Predefined_Type_Name)
            Insert_Declaration
              (File              => File,
               List              => List,
               Symbol_Name       => Identifier,
               Location          => Sym.Start_Position,
               Parent_Location   => Predefined_Point,
               Kind              => Desc.Kind,
               Scope             => Global_Scope,
               Declaration_Info  => Decl_Info);

         else
            --  Set parent location to ancestor location
            Insert_Declaration
              (File              => File,
               List              => List,
               Symbol_Name       => Identifier,
               Location          => Sym.Start_Position,
               Parent_Filename   => Create (Desc.Ancestor_Filename.all),
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
     (Sym              : FIL_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List)
   is
      pragma Unreferenced (Module_Type_Defs);
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
           (File                  => File,
            List                  => List,
            Symbol_Name           =>
              Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last),
            Location              => Sym.Start_Position,
            Kind                  => Desc.Kind,
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

   function Get_DB_Dir (Project : Project_Type) return String is
      Obj_Dir : constant String := Object_Path (Project, False);
   begin
      if Obj_Dir = "" then
         return "";
      else
         return Name_As_Directory (Obj_Dir)
           & Name_As_Directory (SN.Browse.DB_Dir_Name);
      end if;
   end Get_DB_Dir;

   -----------------------------
   -- Process_Local_Variables --
   -----------------------------

   procedure Process_Local_Variables
     (FU_Tab           : FU_Table;
      Symbol           : Symbol_Type;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List)
   is
      P           : Pair_Ptr;
      Var         : LV_Table;
      Decl_Info   : E_Declaration_Info_List;

      Fu_Id       : constant String := FU_Tab.Buffer
        (FU_Tab.Name.First .. FU_Tab.Name.Last);
   begin
      if not Is_Open (Handler.SN_Table (LV)) then
         --  .lv table does not exist
         return;
      end if;

      --  Find all local variables for given function/method.
      --  Only function/method name is in the key.
      Set_Cursor
        (DB          => Handler.SN_Table (LV),
         Position    => By_Key,
         Key         => Fu_Id & Field_Sep,
         Exact_Match => False);

      loop
         P := Get_Pair (Handler.SN_Table (LV), Next_By_Key);
         exit when P = null;
         Parse_Pair (P.all, Var);
         Free (P);
         --  Check if we found the right local variable:
         --  compare class names (for methods only)
         --  compare file names
         --  compare arg types
         if (Symbol = FU or else
            Var.Buffer (Var.Class.First ..  Var.Class.Last) =
            FU_Tab.Buffer (FU_Tab.Class.First .. FU_Tab.Class.Last))
         and then
            Var.Buffer (Var.File_Name.First .. Var.File_Name.Last) =
            FU_Tab.Buffer (FU_Tab.File_Name.First .. FU_Tab.File_Name.Last)
         and then
            Cmp_Arg_Types
              (FU_Tab.Buffer,
               Var.Buffer,
               FU_Tab.Arg_Types,
               Var.Arg_Types,
               Strict => True)
         then
--          Info
--            (FU_Tab.Buffer (FU_Tab.Class.First .. FU_Tab.Class.Last) &
--             "::" & Fu_Id & ", lv: " &
--             Var.Buffer (Var.Name.First .. Var.Name.Last));
--          Info ("Type: " &
--            Var.Buffer (Var.Value_Type.First .. Var.Value_Type.Last));

            --  Collect information about the local variable:
            --  type, scope, location of type declaration...
            declare
               Desc        : CType_Description;
               Success     : Boolean := False;
               CL_Success  : Boolean := False;
               Scope       : E_Scope := Local_Scope;
               Class_Def   : CL_Table;
            begin
               if (Symbol = MI) or (Symbol = MD) then
                  Find_Class
                    (FU_Tab.Buffer (FU_Tab.Class.First .. FU_Tab.Class.Last),
                     Handler.SN_Table,
                     Desc,
                     Class_Def,
                     CL_Success);

                  if not CL_Success then -- try unions
                     Find_Union
                       (FU_Tab.Buffer (FU_Tab.Class.First ..
                           FU_Tab.Class.Last),
                        Handler.SN_Table,
                        Desc,
                        Class_Def,
                        CL_Success);
                  end if;

                  if not CL_Success then
                     Fail ("Failed to locate class/union: "
                        & FU_Tab.Buffer (FU_Tab.Class.First ..
                           FU_Tab.Class.Last)
                        & " for member function "
                        & FU_Tab.Buffer (FU_Tab.Name.First ..
                           FU_Tab.Name.Last));
                     exit;
                  end if;

                  Free (Desc);
               end if;

               Type_Name_To_Kind
                 (Var.Buffer (Var.Value_Type.First .. Var.Value_Type.Last),
                  Handler.SN_Table,
                  Module_Type_Defs,
                  Desc,
                  Success,
                  Symbol,
                  FU_Tab,
                  Class_Def);

               if CL_Success then
                  Free (Class_Def);
               end if;

               if not Success then -- type not found
                  --  Set kind to Unresolved_Entity for local variables
                  --  which have unknown type.
                  Desc.Kind := Unresolved_Entity_Kind;
               end if;

               if (Var.Attributes and SN_STATIC) = SN_STATIC then
                  Scope := Static_Local;
               end if;

               if Desc.Parent_Point = Invalid_Point then
                  Insert_Declaration
                    (File              => File,
                     List              => List,
                     Symbol_Name       => Var.Buffer
                       (Var.Name.First .. Var.Name.Last),
                     Location          => Var.Start_Position,
                     Kind              => Type_To_Object (Desc.Kind),
                     Scope             => Scope,
                     Declaration_Info  => Decl_Info);
               else
                  Insert_Declaration
                    (File              => File,
                     List              => List,
                     Symbol_Name       => Var.Buffer
                       (Var.Name.First .. Var.Name.Last),
                     Location          => Var.Start_Position,
                     Kind              => Type_To_Object (Desc.Kind),
                     Scope             => Scope,
                     Parent_Location   => Desc.Parent_Point,
                     Parent_Filename   => Create (Desc.Parent_Filename.all),
                     Declaration_Info  => Decl_Info);

                  --  add reference to the type of this variable
                  if Desc.Is_Template then
                     Refer_Type
                       (Plain_Class_Name
                          (Var.Buffer (Var.Value_Type.First ..
                                       Var.Value_Type.Last)),
                        Desc.Parent_Point,
                        File,
                        Var.Type_Start_Position,
                        Instantiation_Reference);
                  else
                     Refer_Type
                       (Var.Buffer
                         (Var.Value_Type.First .. Var.Value_Type.Last),
                        Desc.Parent_Point,
                        File,
                        Var.Type_Start_Position);
                  end if;

               end if;

               if Success then
                  Free (Desc);
               end if;
            end;

            if Decl_Info = null then
               Fail ("unable to create declaration for local variable " &
                 Var.Buffer (Var.Name.First .. Var.Name.Last));
               Free (Var);
               exit;
            end if;

            Process_Local_Variable
              (Var_Name           =>
                  Var.Buffer (Var.Name.First .. Var.Name.Last),
               Var_File_Name      => Create
                 (Var.Buffer (Var.File_Name.First .. Var.File_Name.Last)),
               Var_Start_Position => Var.Start_Position,
               FU_Tab             => FU_Tab,
               Symbol             => Symbol,
               Referred_Symbol    => LV,
               Handler            => Handler,
               File               => File,
               Decl_Info          => Decl_Info);

         end if;
         Free (Var);
      end loop;
      Release_Cursor (Handler.SN_Table (LV));
   end Process_Local_Variables;

   ----------------------------
   -- Process_Local_Variable --
   ----------------------------

   procedure Process_Local_Variable
     (Var_Name           : String;
      Var_File_Name      : Virtual_File;
      Var_Start_Position : Point;
      FU_Tab             : FU_Table;
      Symbol             : Symbol_Type;
      Referred_Symbol    : Symbol_Type;
      Handler            : access CPP_LI_Handler_Record'Class;
      File               : in out LI_File_Ptr;
      Decl_Info          : in out E_Declaration_Info_List)
   is
      P        : Pair_Ptr;
      Ref      : TO_Table;
      Ref_Kind : Reference_Kind;
   begin
--    Info (FU_Tab.Buffer (FU_Tab.Class.First .. FU_Tab.Class.Last) &
--       "::" &
--       FU_Tab.Buffer (FU_Tab.Name.First .. FU_Tab.Name.Last) &
--       ", lv: " &
--       Var.Buffer (Var.Name.First .. Var.Name.Last));

      --  Look thru' .to table for specified local variable (or argument)
      --  usage within current function/method body.
      Set_Cursor
        (DB          => Handler.SN_Table (TO),
         Position    => By_Key,
         Key         =>
           FU_Tab.Buffer (FU_Tab.Class.First .. FU_Tab.Class.Last) &
           Field_Sep &
           FU_Tab.Buffer (FU_Tab.Name.First .. FU_Tab.Name.Last) &
           Field_Sep &
           To_String (Symbol) &
           Field_Sep &
           FU_Tab.Buffer (FU_Tab.Name.First .. FU_Tab.Name.Last) &
           Field_Sep &
           Var_Name &
           Field_Sep & To_String (Referred_Symbol) & Field_Sep,
         Exact_Match => False);

      loop
         P := Get_Pair (Handler.SN_Table (TO), Next_By_Key);

         exit when P = null;

         Parse_Pair (P.all, Ref);
         Free (P);

         --  Check if we found the right lv usage: comapre file name
         --  and argument types

         --  ??? Should we compare full or base names
         if Ref.Buffer (Ref.File_Name.First .. Ref.File_Name.Last) =
           Full_Name (Var_File_Name)
           and then Cmp_Arg_Types
             (FU_Tab.Buffer,
              Ref.Buffer,
              FU_Tab.Arg_Types,
              Ref.Caller_Argument_Types,
              Strict => True)
         then
            --  For each declaration of local variable (in .lv table) SN
            --  creates a xref record with the same name and location
            --  (in .to table). Thus, we don't create a reference to local
            --  variable usage with the same location.

            if Ref.Position /= Var_Start_Position then
               if Ref.Buffer
                 (Ref.Access_Type.First .. Ref.Access_Type.Last) = "w"
               then
                  Ref_Kind := Modification;
               else
                  Ref_Kind := Reference;
               end if;

               Insert_Reference (Decl_Info, File, Ref.Position, Ref_Kind);
            end if;
         end if;

         Free (Ref);
      end loop;

      Release_Cursor (Handler.SN_Table (TO));
   end Process_Local_Variable;

   ------------------------------
   -- Process_Class_To_TA_Refs --
   ------------------------------

   procedure Process_Class_To_TA_Refs
     (CL_Tab    : CL_Table;
      Arg       : TA_Table;
      Handler   : access CPP_LI_Handler_Record'Class;
      File      : in out LI_File_Ptr;
      Decl_Info : in out E_Declaration_Info_List)
   is
      P        : Pair_Ptr;
      Ref      : TO_Table;
      Ref_Kind : Reference_Kind;
   begin
      Set_Cursor
        (DB          => Handler.SN_Table (TO),
         Position    => By_Key,
         Key         =>
           "#" & Field_Sep &
           CL_Tab.Buffer (CL_Tab.Name.First .. CL_Tab.Name.Last) &
           Field_Sep &
           To_String (CL) &
           Field_Sep &
           CL_Tab.Buffer (CL_Tab.Name.First .. CL_Tab.Name.Last) &
           Field_Sep &
           Arg.Buffer (Arg.Name.First .. Arg.Name.Last) &
           Field_Sep & To_String (TA) & Field_Sep,
         Exact_Match => False);

      loop
         P := Get_Pair (Handler.SN_Table (TO), Next_By_Key);

         exit when P = null;

         Parse_Pair (P.all, Ref);
         Free (P);

         if Ref.Buffer (Ref.Access_Type.First .. Ref.Access_Type.Last)
           = "w"
         then
            Ref_Kind := Modification;
         else
            Ref_Kind := Reference;
         end if;

         Insert_Reference (Decl_Info, File, Ref.Position, Ref_Kind);
         Free (Ref);
      end loop;

      Release_Cursor (Handler.SN_Table (TO));
   end Process_Class_To_TA_Refs;

   --------------------------------
   -- Process_Template_Arguments --
   --------------------------------

   procedure Process_Template_Arguments
     (Symbol           : Symbol_Type := CL;
      FU_Tab           : FU_Table := Invalid_FU_Table;
      CL_Tab           : CL_Table := Invalid_CL_Table;
      Handler          : access CPP_LI_Handler_Record'Class;
      File             : in out LI_File_Ptr;
      List             : LI_File_List;
      Module_Type_Defs : Module_Typedefs_List)
   is
      Arg              : TA_Table;
      P                : Pair_Ptr;
      Decl_Info        : E_Declaration_Info_List;
      Desc             : CType_Description;
      Success          : Boolean;
      Buffer           : GNAT.OS_Lib.String_Access;
      Scope            : Segment;
      File_Name        : Segment;
      Template_Args    : Segment;
      Class_Name       : Segment := (1, 0);
      TA_File          : DB_File;
   begin
      if not Is_Open (Handler.SN_Table (TA)) then
         Fail (".ta table does not exist but template argument processing"
               & " required");
         return;
      end if;

      if Symbol = CL then
         Buffer        := CL_Tab.Buffer;
         Scope         := CL_Tab.Name;
         File_Name     := CL_Tab.File_Name;
         Template_Args := CL_Tab.Template_Parameters;
      else
         Buffer        := FU_Tab.Buffer;
         Scope         := FU_Tab.Name;
         File_Name     := FU_Tab.File_Name;
         Template_Args := FU_Tab.Template_Parameters;

         if (Symbol = MD) or (Symbol = MI) then
            Class_Name := FU_Tab.Class;
         end if;
      end if;

      TA_File := Dup (Handler.SN_Table (TA));
      Set_Cursor
        (TA_File,
         Position    => By_Key,
         Key         => Buffer (Scope.First .. Scope.Last) & Field_Sep,
         Exact_Match => False);

      loop
         P := Get_Pair (TA_File, Next_By_Key);

         exit when P = null;

         Parse_Pair (P.all, Arg);
         Free (P);

         if Buffer (File_Name.First .. File_Name.Last)
           = Arg.Buffer (Arg.File_Name.First .. Arg.File_Name.Last)
           and then Buffer (Template_Args.First .. Template_Args.Last)
             = Arg.Buffer (Arg.Template_Parameters.First ..
                           Arg.Template_Parameters.Last)
           and then Buffer (Class_Name.First .. Class_Name.Last)
             = Arg.Buffer (Arg.Class_Name.First .. Arg.Class_Name.Last)
         then
            if Arg.Attributes = SN_TA_TYPE
              or else Arg.Attributes = SN_TA_TEMPLATE
            then
               Insert_Declaration
                 (File             => File,
                  List             => List,
                  Symbol_Name      =>
                     Arg.Buffer (Arg.Name.First .. Arg.Name.Last),
                  Location         => Arg.Start_Position,
                  Kind             => (Private_Type, False, False, False),
                  Scope            => Local_Scope,
                  Declaration_Info => Decl_Info);

            elsif Arg.Attributes = SN_TA_VALUE then
               Type_Name_To_Kind
                 (Arg.Buffer
                    (Arg.Value_Type.First .. Arg.Value_Type.Last),
                  Handler.SN_Table,
                  Module_Type_Defs,
                  Desc,
                  Success,
                  Symbol,
                  FU_Tab,
                  CL_Tab);

               if not Success then -- type not found
                  Desc.Kind := Unresolved_Entity_Kind;
               end if;

               if Desc.Parent_Point = Invalid_Point then
                  Insert_Declaration
                    (File             => File,
                     List             => List,
                     Symbol_Name      =>
                        Arg.Buffer (Arg.Name.First .. Arg.Name.Last),
                     Location         => Arg.Start_Position,
                     Kind             => Type_To_Object (Desc.Kind),
                     Scope            => Local_Scope,
                     Declaration_Info => Decl_Info);
               else
                  Insert_Declaration
                    (File             => File,
                     List             => List,
                     Symbol_Name      =>
                        Arg.Buffer (Arg.Name.First .. Arg.Name.Last),
                     Location         => Arg.Start_Position,
                     Kind             => Type_To_Object (Desc.Kind),
                     Scope            => Local_Scope,
                     Parent_Location  => Desc.Parent_Point,
                     Parent_Filename  => Create (Desc.Parent_Filename.all),
                     Declaration_Info => Decl_Info);
               end if;

               if Arg.Attributes = SN_TA_VALUE then
                  Refer_Type
                    (Arg.Buffer (Arg.Value_Type.First .. Arg.Value_Type.Last),
                     Desc.Parent_Point,
                     File,
                     Arg.Type_Position);
               end if;

               Free (Desc);
            end if;

            if Symbol = FU or else Symbol = FD
              or else Symbol = MD or else Symbol = MI
            then
               Process_Local_Variable
                 (Arg.Buffer (Arg.Name.First .. Arg.Name.Last),
                  Create (FU_Tab.Buffer
                            (FU_Tab.File_Name.First .. FU_Tab.File_Name.Last)),
                  Arg.Start_Position,
                  FU_Tab,
                  Symbol,
                  TA,
                  Handler,
                  File,
                  Decl_Info);
            else
               Process_Class_To_TA_Refs
                 (CL_Tab,
                  Arg,
                  Handler,
                  File,
                  Decl_Info);

               declare
                  P        : Pair_Ptr;
                  MI_Tab   : FU_Table;
                  MI_File  : DB_File;
               begin
                  if Is_Open (Handler.SN_Table (MI)) then
                     MI_File := Dup (Handler.SN_Table (MI));
                     Set_Cursor
                       (MI_File,
                        By_Key,
                        CL_Tab.Buffer (CL_Tab.Name.First .. CL_Tab.Name.Last)
                           & Field_Sep,
                        False);

                     loop -- iterate thru all methods of the class
                        P := Get_Pair (MI_File, Next_By_Key);

                        exit when P = null;

                        Parse_Pair (P.all, MI_Tab);

                        Process_Local_Variable
                          (Arg.Buffer (Arg.Name.First .. Arg.Name.Last),
                           Create
                             (MI_Tab.Buffer
                                (MI_Tab.File_Name.First ..
                                   MI_Tab.File_Name.Last)),
                           Arg.Start_Position,
                           MI_Tab,
                           MI,
                           TA,
                           Handler,
                           File,
                           Decl_Info);
                        Free (MI_Tab);
                     end loop;
                  end if;

                  Release_Cursor (MI_File);
                  Close (MI_File, Success);
               end;
            end if;
         end if;

         Free (Arg);
      end loop;

      Release_Cursor (TA_File);
      Close (TA_File, Success);
   end Process_Template_Arguments;

   ----------
   -- Init --
   ----------

   procedure Init (Prj_HTable : out SN_Prj_HTable) is
   begin
      Prj_HTable := new SN_Prj_HTable_Record;
   end Init;

   ----------
   -- Free --
   ----------

   procedure Free
     (Keys       : in out String_List_Access;
      Prj_HTable : in out SN_Prj_HTable)
   is
      procedure Internal_Free is new Ada.Unchecked_Deallocation
        (SN_Prj_HTable_Record, SN_Prj_HTable);
   begin
      if Prj_HTable = null or Keys = null then
         return;
      end if;

      for J in Keys'Range loop
         SN_Prj_HTables.Remove (Prj_HTable.all, Keys (J).all);
      end loop;

      Internal_Free (Prj_HTable);
      Free (Keys);
   end Free;

   -------------------
   -- Get_Xref_Pool --
   -------------------

   function Get_Xref_Pool
     (Prj_HTable : SN_Prj_HTable;
      DB_Dir     : String) return Xref_Pool
   is
      Prj_Data : SN_Prj_Data;
   begin
      if Prj_HTable = null then
         return Empty_Xref_Pool;
      end if;

      Prj_Data := SN_Prj_HTables.Get (Prj_HTable.all, DB_Dir);

      if Prj_Data = No_SN_Prj_Data then
         Fail ("Get_Xref_Pool: empty pool for " & DB_Dir);
         return Empty_Xref_Pool;
      end if;

      return Prj_Data.Pool;
   end Get_Xref_Pool;

   -------------------
   -- Set_Xref_Pool --
   -------------------

   procedure Set_Xref_Pool
     (Prj_HTable : SN_Prj_HTable;
      DB_Dir     : String_Access;
      Pool       : Xref_Pool)
   is
      Prj_Data : SN_Prj_Data :=
         SN_Prj_HTables.Get (Prj_HTable.all, DB_Dir.all);
   begin
      if Prj_Data /= No_SN_Prj_Data then
         Free (Prj_Data.Pool);
      end if;

      Prj_Data.Pool := Pool;
      SN_Prj_HTables.Set (Prj_HTable.all, DB_Dir.all, Prj_Data);
   end Set_Xref_Pool;

   -----------------------
   -- Xref_Filename_For --
   -----------------------

   function Xref_Filename_For
     (Filename   : VFS.Virtual_File;
      DB_Dir     : String;
      Prj_HTable : SN_Prj_HTable) return VFS.Virtual_File
   is
      Pool : Xref_Pool;
   begin
      Pool := SN_Prj_HTables.Get (Prj_HTable.all, DB_Dir).Pool;

      if Pool = Empty_Xref_Pool then
         Fail ("Xref_Filename_For: empty pool for " & DB_Dir);
         return VFS.No_File;
      end if;

      return Xref_Filename_For
        (Source_Filename => Filename,
         Directory       => DB_Dir,
         Pool            => Pool);
   end Xref_Filename_For;

   -----------------------
   -- Xref_Filename_For --
   -----------------------

   procedure Xref_Filename_For
     (Filename       : VFS.Virtual_File;
      DB_Dir         : String;
      Prj_HTable     : SN_Prj_HTable;
      Xref_Filename  : out VFS.Virtual_File;
      Pool           : out Xref_Pool) is
   begin
      Pool := SN_Prj_HTables.Get (Prj_HTable.all, DB_Dir).Pool;

      if Pool = Empty_Xref_Pool then
         Fail ("Xref_Filename_For: empty pool for " & DB_Dir);
         Xref_Filename := VFS.No_File;
      else
         Xref_Filename := Xref_Filename_For
           (Source_Filename => Filename,
            Directory       => DB_Dir,
            Pool            => Pool);
      end if;
   end Xref_Filename_For;

   --------------------
   -- Get_Prj_HTable --
   --------------------

   function Get_Prj_HTable
     (Handler : access Src_Info.CPP.CPP_LI_Handler_Record'Class)
      return SN_Prj_HTable is
   begin
      return Handler.Prj_HTable;
   end Get_Prj_HTable;

   ----------------------
   -- Get_Root_Project --
   ----------------------

   function Get_Root_Project
     (Handler : access Src_Info.CPP.CPP_LI_Handler_Record'Class)
      return Project_Type is
   begin
      return Handler.Root_Project;
   end Get_Root_Project;

   ----------------
   -- Get_DB_Dir --
   ----------------

   function Get_DB_Dir
     (DB_Dirs : String_List_Access;
      DBI     : Integer) return String is
   begin
      return DB_Dirs (DB_Dirs'First + DBI).all;
   end Get_DB_Dir;

   ----------------
   -- False_Free --
   ----------------

   procedure False_Free_Element (X : in out SN_Prj_Data) is
      pragma Unreferenced (X);
   begin
      null;
   end False_Free_Element;

end Src_Info.CPP;
