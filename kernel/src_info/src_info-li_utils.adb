-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
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

with Text_IO; use Text_IO;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Traces; use Traces;
with SN; use SN;
with Src_Info.CPP;  use Src_Info.CPP;

package body Src_Info.LI_Utils is

   Me : constant Debug_Handle := Create ("Src_Info.LI_Utils");

   procedure Insert_Declaration_Internal
     (D_Ptr                   : in out E_Declaration_Info_List;
      File                    : LI_File_Ptr;
      List                    : in out LI_File_List;
      Project                 : Prj.Project_Id;
      Symbol_Name             : String;
      Location                : Point;
      Parent_Filename         : String := "";
      Parent_Location         : Point := Invalid_Point;
      Kind                    : E_Kind;
      Scope                   : E_Scope;
      End_Of_Scope_Location   : Point := Invalid_Point;
      Rename_Location         : Point := Invalid_Point);
   --  Inserts declaration into specified E_Declaration_Info_List


   function Find_Declaration_Internal
     (Declaration_Info_Ptr    : E_Declaration_Info_List;
      Symbol_Name             : String := "";
      Class_Name              : String := "";
      Kind                    : E_Kind := No_Kind;
      Location                : Point := Invalid_Point;
      Negate_Kind             : Boolean := False)
      return E_Declaration_Info_List;
   --  Finds declaration with given attributes in
   --  specified E_Declaration_Info_List.
   --  Return null if not found.

   function Belongs_To_Class
     (Declaration_Info_Ptr : in E_Declaration_Info_List;
      Class_Name           : String;
      Position             : Point) return Boolean;
   --  Checks if given position belongs to class body (found in the given
   --  list of declarations)

   --------------------------
   --  Insert_declaration  --
   --------------------------

   procedure Insert_Declaration
     (File                    : in out LI_File_Ptr;
      List                    : in out LI_File_List;
      Project                 : Prj.Project_Id;
      Symbol_Name             : String;
      Location                : Point;
      Parent_Filename         : String := "";
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
      Insert_Declaration_Internal
        (Declaration_Info,
         File,
         List,
         Project,
         Symbol_Name,
         Location,
         Parent_Filename,
         Parent_Location,
         Kind,
         Scope,
         End_Of_Scope_Location,
         Rename_Location);
   end Insert_Declaration;

   -----------------------
   -- Convert_To_Parsed --
   -----------------------

   procedure Convert_To_Parsed
     (File               : in out LI_File_Ptr;
      Update_Timestamp   : Boolean := True;
      Compilation_Errors : Boolean := False) is
   begin
      if not File.LI.Parsed then
         File.LI := (Parsed                   => True,
                     Handler                  => File.LI.Handler,
                     LI_Filename              => File.LI.LI_Filename,
                     Spec_Info                => File.LI.Spec_Info,
                     Body_Info                => File.LI.Body_Info,
                     Separate_Info            => File.LI.Separate_Info,
                     LI_Timestamp             => File.LI.LI_Timestamp,
                     Compilation_Errors_Found => Compilation_Errors,
                     Dependencies_Info        => null);
      end if;

      if Update_Timestamp
        and then Is_Regular_File (File.LI.LI_Filename.all)
      then
         File.LI.LI_Timestamp := To_Timestamp
           (File_Time_Stamp (File.LI.LI_Filename.all));
      end if;
   end Convert_To_Parsed;

   -------------------------
   --  Insert_Dependency  --
   -------------------------

   procedure Insert_Dependency
     (Handler              : access Src_Info.CPP.CPP_LI_Handler_Record'Class;
      Project              : Prj.Project_Id;
      File                 : in out LI_File_Ptr;
      List                 : in out LI_File_List;
      Referred_Filename    : String)
   is
      Dep_Ptr : Dependency_File_Info_List;
      Tmp_LI_File_Ptr : LI_File_Ptr;
   begin
      --  Now we are searching through common list of LI_Files and
      --  trying to locate file with given name. If not found we are
      --  inserting new dependency
      Create_Stub_For_File
        (LI            => Tmp_LI_File_Ptr,
         Handler       => CPP_LI_Handler (Handler),
         Project       => Project,
         List          => List,
         Full_Filename => Referred_Filename);

      --  Is this a first dependencies info in this file?
      if File.LI.Dependencies_Info = null then
         File.LI.Dependencies_Info := new Dependency_File_Info_Node;
         Dep_Ptr := File.LI.Dependencies_Info;

      else
         --  Try to locate Dependency_File_Info with given Source_Filename
         Dep_Ptr := File.LI.Dependencies_Info;
         loop
            if Get_Source_Filename (Dep_Ptr.Value.File) =
              Base_Name (Referred_Filename)
            then
               return;
            end if;

            if Dep_Ptr.Next = null then
               --  Unable to find suitable Dependency_File_Info.
               --  Creating a new one.
               Dep_Ptr.Next := new Dependency_File_Info_Node;
               Dep_Ptr := Dep_Ptr.Next;
               exit;
            end if;
            Dep_Ptr := Dep_Ptr.Next;
         end loop;
      end if;

      --  creating new Dependency_File_Info_Node object
      Dep_Ptr.all :=
        (Value => (File         => (LI              => Tmp_LI_File_Ptr,
                                    Part            => Unit_Body,
                                    Source_Filename => null),
                   Dep_Info     => (Depends_From_Spec => False,
                                    Depends_From_Body => True),
                   Declarations => null),
         Next  => null);
   end Insert_Dependency;

   -------------------------------------
   --  Insert_Dependency_Declaration  --
   -------------------------------------

   procedure Insert_Dependency_Declaration
     (Handler               : access Src_Info.CPP.CPP_LI_Handler_Record'Class;
      Project               : Prj.Project_Id;
      File                  : in out LI_File_Ptr;
      List                  : in out LI_File_List;
      Symbol_Name           : String;
      Referred_Filename     : String;
      Location              : Point;
      Parent_Filename       : String := "";
      Parent_Location       : Point := Invalid_Point;
      Kind                  : E_Kind;
      Scope                 : E_Scope;
      End_Of_Scope_Location : Point := Invalid_Point;
      Rename_Location       : Point := Invalid_Point;
      Declaration_Info      : out E_Declaration_Info_List)
   is
      D_Ptr, Tmp_Ptr : E_Declaration_Info_List;
      Dep_Ptr : Dependency_File_Info_List;
      Tmp_LI_File_Ptr : LI_File_Ptr;
   begin
      --  Now we are searching through common list of LI_Files and
      --  trying to locate file with given name. If not found or if there
      --  are no such symbol declared in the found file then
      --  we are inserting new declaration

      Create_Stub_For_File
        (LI            => Tmp_LI_File_Ptr,
         Handler       => Handler,
         Project       => Project,
         List          => List,
         Full_Filename => Referred_Filename);

      D_Ptr := Find_Declaration
        (File              => Tmp_LI_File_Ptr,
         Symbol_Name       => Symbol_Name,
         Location          => Location);

      if D_Ptr = null then
         Insert_Declaration
           (File               => Tmp_LI_File_Ptr,
            List               => List,
            Project            => Project,
            Symbol_Name        => Symbol_Name,
            Location           => Location,
            Parent_Filename    => Parent_Filename,
            Parent_Location    => Parent_Location,
            Kind               => Kind,
            Scope              => Scope,
            End_Of_Scope_Location => End_Of_Scope_Location,
            Declaration_Info   => Tmp_Ptr);
      end if;

      --  Is this is a first dependencies info in this file?
      if File.LI.Dependencies_Info = null then
         --  creating new Dependency_File_Info_Node object
         File.LI.Dependencies_Info := new Dependency_File_Info_Node'
           (Value => (File         => (LI              => Tmp_LI_File_Ptr,
                                       Part            => Unit_Body,
                                       Source_Filename => null),
                      Dep_Info     => (Depends_From_Spec => False,
                                       Depends_From_Body => True),
                      Declarations => null),
            Next  => null);
         Dep_Ptr := File.LI.Dependencies_Info;
      else
         Dep_Ptr := File.LI.Dependencies_Info;
         --  trying to locate Dependency_File_Info with given Source_Filename

         --  ??? MANU: using Base_Name here is a hack, we need to correctly
         --  handle Directory_Name found in #include "dir/file.h"

         while Get_Source_Filename (Dep_Ptr.Value.File) /=
           Base_Name (Referred_Filename)
         loop
            if Dep_Ptr.Next = null then
               --  Unable to find suitable Dependency_File_Info.
               --  Creating a new one.
               Dep_Ptr.Next := new Dependency_File_Info_Node'
                 (Value => (File => (LI              => Tmp_LI_File_Ptr,
                                     Part            => Unit_Body,
                                     Source_Filename => null),
                            Dep_Info     => (Depends_From_Spec => False,
                                             Depends_From_Body => True),
                            Declarations => null),
                  Next  => null);
               Dep_Ptr := Dep_Ptr.Next;
               exit;
            end if;
            Dep_Ptr := Dep_Ptr.Next;
         end loop;
      end if;

      --  Now Dep_Ptr points to valid Dependency_File_Info_Node object
      --  Inserting new declaration
      if Dep_Ptr.Value.Declarations = null then
         --  this is a first declaration for this Dependency_File_Info
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
      Insert_Declaration_Internal
        (D_Ptr,
         Tmp_LI_File_Ptr,
         List,
         Project,
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

   ------------------
   --  Add_Parent  --
   ------------------

   procedure Add_Parent
     (Declaration_Info : in out E_Declaration_Info_List;
      Handler          : CPP_LI_Handler;
      Project          : Prj.Project_Id;
      List             : in out LI_File_List;
      Parent_Filename  : String;
      Parent_Location  : Point)
   is
      FL_Ptr : File_Location_List;
      Tmp_LI_File_Ptr : LI_File_Ptr;
   begin
      Assert (Me, Declaration_Info /= null, "Invalid declaration");
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
         Project       => Project,
         List          => List,
         Full_Filename => Parent_Filename);
      FL_Ptr.all :=
        (Value => (File        => (LI              => Tmp_LI_File_Ptr,
                                   Part            => Unit_Body,
                                   Source_Filename => null),
                   Line        => Parent_Location.Line,
                   Column      => Parent_Location.Column),
         Next  => null);
   end Add_Parent;

   ------------------------
   --  Set_End_Of_Scope  --
   ------------------------

   procedure Set_End_Of_Scope
     (Declaration_Info        : in out E_Declaration_Info_List;
      File                    : LI_File_Ptr;
      Location                : Point;
      Kind                    : Reference_Kind := End_Of_Body) is
   begin
      Declaration_Info.Value.Declaration.End_Of_Scope.Location :=
        (Line   => Location.Line,
         Column => Location.Column,
         File   => (LI              => File,
                    Part            => Unit_Body,
                    Source_Filename => null));
      Declaration_Info.Value.Declaration.End_Of_Scope.Kind := Kind;
   end Set_End_Of_Scope;

   ------------------------
   --  Insert_Reference  --
   ------------------------

   procedure Insert_Reference
     (Declaration_Info        : in out E_Declaration_Info_List;
      File                    : LI_File_Ptr;
      Location                : Point;
      Kind                    : Reference_Kind)
   is
      R_Ptr : E_Reference_List;
   begin
      if Declaration_Info = null then
         --  ??? what should we do if Declaration_Info is null?
         return;
      elsif Declaration_Info.Value.References = null then
         Declaration_Info.Value.References := new E_Reference_Node;
         Declaration_Info.Value.References.Next := null;
         R_Ptr := Declaration_Info.Value.References;
      else
         R_Ptr := Declaration_Info.Value.References;
         loop
            exit when R_Ptr.Next = null;
            R_Ptr := R_Ptr.Next;
         end loop;
         R_Ptr.Next := new E_Reference_Node;
         R_Ptr.Next.Next := null;
         R_Ptr := R_Ptr.Next;
         --  ??? Shouldn't we try to locate existing reference with
         --  given attributes
      end if;
      R_Ptr.Value :=
         (Location => (File   => (LI              => File,
                                  Part            => Unit_Body,
                                  Source_Filename => null),
                       Line   => Location.Line,
                       Column => Location.Column),
          Kind => Kind);
      --  ??? We have R_Ptr.Value.Location.File.LI set to File because
      --  we always insert references only from the current file.
   end Insert_Reference;

   ------------------------
   --  Find_Declaration  --
   ------------------------

   function Find_Declaration
     (File                    : LI_File_Ptr;
      Symbol_Name             : String := "";
      Class_Name              : String := "";
      Kind                    : E_Kind := No_Kind;
      Location                : Point := Invalid_Point;
      Negate_Kind             : Boolean := False)
      return E_Declaration_Info_List is
   begin
      if File = No_LI_File
        or else File.LI.Body_Info = null
        or else File.LI.Body_Info.Declarations = null
      then
         return null;
      else
         return Find_Declaration_Internal
           (Declaration_Info_Ptr => File.LI.Body_Info.Declarations,
            Symbol_Name          => Symbol_Name,
            Class_Name           => Class_Name,
            Kind                 => Kind,
            Location             => Location,
            Negate_Kind          => Negate_Kind);
      end if;
   end Find_Declaration;

   -----------------------------------
   --  Find_Dependency_Declaration  --
   -----------------------------------

   function Find_Dependency_Declaration
     (File                    : LI_File_Ptr;
      Symbol_Name             : String := "";
      Class_Name              : String := "";
      Filename                : String := "";
      Kind                    : E_Kind := No_Kind;
      Location                : Point := Invalid_Point)
   return E_Declaration_Info_List is
      Dep_Ptr : Dependency_File_Info_List;
   begin
      if File = No_LI_File
        or else File.LI.Dependencies_Info = null
      then
         return null;
      end if;

      Dep_Ptr := File.LI.Dependencies_Info;
      loop
         if Dep_Ptr = null then
            return null;
         end if;

         if Dep_Ptr.Value.File = No_Source_File then
            Put_Line ("No_Source_File encountered in the Dep_Info list for "
                      & Get_LI_Filename (File));
         end if;

         exit when Get_Source_Filename (Dep_Ptr.Value.File)
            = Base_Name (Filename);
         Dep_Ptr := Dep_Ptr.Next;
      end loop;

      if Dep_Ptr.Value.Declarations = null then
         return null;
      else
         return Find_Declaration_Internal
           (Declaration_Info_Ptr => Dep_Ptr.Value.Declarations,
            Symbol_Name          => Symbol_Name,
            Class_Name           => Class_Name,
            --  ??? Should the dependency declaration of class
            --  necessary be located together with method dependency
            --  declaration?
            Kind                 => Kind,
            Location             => Location);
      end if;
   end Find_Dependency_Declaration;
   --  ??? Class_Name parameter should be used properly

   --------------------------
   -- Create_Stub_For_File --
   --------------------------

   procedure Create_Stub_For_File
     (LI            : out LI_File_Ptr;
      Handler       : access Src_Info.CPP.CPP_LI_Handler_Record'Class;
      Project       : Prj.Project_Id;
      List          : in out LI_File_List;
      Full_Filename : String)
   is
      Xref_Name : constant String_Access := Xref_Filename_For
        (Full_Filename, Get_DB_Dir (Project), Get_Prj_HTable (Handler));
   begin
      LI := Locate (List, Xref_Name.all);

      if LI = null then
         Create_LI_File
           (File        => LI,
            List        => List,
            LI_Filename => Xref_Name.all,
            Handler     => LI_Handler (Handler));
      end if;

      if LI.LI.Body_Info = null then
         Create_File_Info
           (FI_Ptr           => LI.LI.Body_Info,
            Full_Filename    => Full_Filename);
      end if;
   end Create_Stub_For_File;

   -----------------------------------
   --  Insert_Declaration_Internal  --
   -----------------------------------

   procedure Insert_Declaration_Internal
     (D_Ptr                   : in out E_Declaration_Info_List;
      File                    : LI_File_Ptr;
      List                    : in out LI_File_List;
      Project                 : Prj.Project_Id;
      Symbol_Name             : String;
      Location                : Point;
      Parent_Filename         : String := "";
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
            Next  => null);

      elsif Parent_Location = Predefined_Point then
         D_Ptr.Value.Declaration.Parent_Location := new File_Location_Node'
           (Value => Predefined_Entity_Location,
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
               Project       => Project,
               List          => List,
               Full_Filename => Parent_Filename);
         end if;
         D_Ptr.Value.Declaration.Parent_Location := new File_Location_Node'
           (Value => (File   => (LI              => Tmp_LI_File_Ptr,
                                 Part            => Unit_Body,
                                 Source_Filename => null),
                      Line   => Parent_Location.Line,
                      Column => Parent_Location.Column),
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
   end Insert_Declaration_Internal;

   -----------------------------------
   --  Find_Declaration_Internal  --
   -----------------------------------

   function Find_Declaration_Internal
     (Declaration_Info_Ptr    : E_Declaration_Info_List;
      Symbol_Name             : String := "";
      Class_Name              : String := "";
      Kind                    : E_Kind := No_Kind;
      Location                : Point := Invalid_Point;
      Negate_Kind             : Boolean := False)
      return E_Declaration_Info_List
   is
      D_Ptr : E_Declaration_Info_List := Declaration_Info_Ptr;
   begin
      loop
         exit when D_Ptr = null;
         if ((Symbol_Name'Length > 0
                and then (D_Ptr.Value.Declaration.Name.all = Symbol_Name))
            or else (Symbol_Name'Length = 0))
           and then (
             (Location /= Invalid_Point
                and then D_Ptr.Value.Declaration.Location.Line
                                                   = Location.Line
                and then D_Ptr.Value.Declaration.Location.Column
                                                   = Location.Column)
             or else (Location = Invalid_Point)
           ) and then (
             (Class_Name /= ""
                and then Belongs_To_Class
                          (Declaration_Info_Ptr => Declaration_Info_Ptr,
                           Class_Name           => Class_Name,
                           Position             => Location))
             or else (Class_Name = "")
           ) and then (
             (Kind /= No_Kind
                and then (
                   (Negate_Kind and then D_Ptr.Value.Declaration.Kind /= Kind)
                    or else
                       (not Negate_Kind
                          and then D_Ptr.Value.Declaration.Kind = Kind)))
             or else Kind = No_Kind
           )
         then
            return D_Ptr;
         end if;
         D_Ptr := D_Ptr.Next;
      end loop;

      return null;
   end Find_Declaration_Internal;

   ----------------------
   --  Create_LI_File  --
   ----------------------

   procedure Create_LI_File
     (File        : out LI_File_Ptr;
      List        : in out LI_File_List;
      LI_Filename : String;
      Handler     : LI_Handler)
   is
   begin
      File := new LI_File_Constrained'
        (LI =>  (Parsed        => False,
                 Handler       => Handler,
                 LI_Filename   => new String'(LI_Filename),
                 Body_Info     => null,
                 Spec_Info     => null,
                 Separate_Info => null,
                 LI_Timestamp  => 0));

      Add (List.Table, File);
   end Create_LI_File;

   ------------------------
   --  Create_File_Info  --
   ------------------------

   procedure Create_File_Info
     (FI_Ptr         : out File_Info_Ptr;
      Full_Filename  : String;
      Set_Time_Stamp : Boolean := True;
      Unit_Name      : String := "")
   is
      Directory_Name : constant String :=
        Normalize_Pathname (Dir_Name (Full_Filename));
      Dir : GNAT.OS_Lib.String_Access;
      Unit : GNAT.OS_Lib.String_Access;
      Time : Timestamp := 0;
   begin
      if Directory_Name /= ""
        and then Directory_Name /= "./"
      then
         Dir := new String'(Directory_Name);
      end if;

      if Unit_Name /= "" then
         Unit := new String'(Unit_Name);
      end if;

      if Set_Time_Stamp then
         Time := To_Timestamp (File_Time_Stamp (Full_Filename));
      end if;

      FI_Ptr := new File_Info'
        (Unit_Name         => Unit,
         Source_Filename   => new String'(Base_Name (Full_Filename)),
         Directory_Name    => Dir,
         File_Timestamp    => Time,
         Original_Filename => null,
         Original_Line     => 1,
         Declarations      => null);
   end Create_File_Info;

   ------------------------
   --  Belongs_To_Class  --
   ------------------------

   function Belongs_To_Class
     (Declaration_Info_Ptr : E_Declaration_Info_List;
      Class_Name           : String;
      Position             : Point) return Boolean
   is
      D_Ptr : E_Declaration_Info_List := Declaration_Info_Ptr;
   begin
      loop
         exit when D_Ptr = null;
         if (D_Ptr.Value.Declaration.Kind = Record_Type or
             D_Ptr.Value.Declaration.Kind = Generic_Class)
           and then D_Ptr.Value.Declaration.Name.all = Class_Name
           and then (
             D_Ptr.Value.Declaration.Location.Line < Position.Line
               or else (D_Ptr.Value.Declaration.Location.Line =
                                                   Position.Line and
                 D_Ptr.Value.Declaration.Location.Column <= Position.Column))
           and then (
             D_Ptr.Value.Declaration.End_Of_Scope.Location.Line > Position.Line
               or else (D_Ptr.Value.Declaration.End_Of_Scope.Location.Line =
                                                   Position.Line and
                 D_Ptr.Value.Declaration.End_Of_Scope.Location.Column >
                                                   Position.Column))
         then
            return True;
         end if;
         D_Ptr := D_Ptr.Next;
      end loop;
      return False;
   end Belongs_To_Class;

   function Find_Reference
     (Declaration_Info        : E_Declaration_Info_List;
      File                    : LI_File_Ptr;
      Location                : SN.Point;
      Kind                    : Reference_Kind)
      return E_Reference_List
   is
      Ref_Ptr : E_Reference_List := Declaration_Info.Value.References;
   begin
      while Ref_Ptr /= null loop
         if Get_LI_Filename (Ref_Ptr.Value.Location.File.LI)
            = Get_LI_Filename (File)
            and Ref_Ptr.Value.Location.Line = Location.Line
            and Ref_Ptr.Value.Location.Column = Location.Column
            and Ref_Ptr.Value.Kind = Kind then
            return Ref_Ptr;
         end if;
         Ref_Ptr := Ref_Ptr.Next;
      end loop;

      return null;
   end Find_Reference;

end Src_Info.LI_Utils;
