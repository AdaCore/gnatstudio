-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
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
with GNAT.OS_Lib; use GNAT.OS_Lib;
with SN; use SN;
with Projects; use Projects;
with VFS; use VFS;

package body Src_Info.LI_Utils is

   function Find_Declaration_Internal
     (Declaration_Info_Ptr    : E_Declaration_Info_List;
      Symbol_Name             : String := "";
      Class_Name              : String := "";
      Kind                    : E_Kind := Unresolved_Entity_Kind;
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

   -----------------------
   -- Convert_To_Parsed --
   -----------------------

   procedure Convert_To_Parsed
     (File               : in out LI_File_Ptr;
      Full_LI_Name       : VFS.Virtual_File;
      Update_Timestamp   : Boolean := True;
      Compilation_Errors : Boolean := False) is
   begin
      if not File.LI.Parsed then
         File.LI :=
           (Parsed                   => True,
            Handler                  => File.LI.Handler,
            LI_Filename              => File.LI.LI_Filename,
            LI_Filename_Key          => File.LI.LI_Filename_Key,
            Project                  => File.LI.Project,
            Spec_Info                => File.LI.Spec_Info,
            Body_Info                => File.LI.Body_Info,
            Separate_Info            => File.LI.Separate_Info,
            LI_Timestamp             => File.LI.LI_Timestamp,
            Compilation_Errors_Found => Compilation_Errors,
            Dependencies_Info        => null);
      end if;

      if Update_Timestamp
        and then Is_Regular_File (Full_LI_Name)
      then
         File.LI.LI_Timestamp := To_Timestamp
           (File_Time_Stamp (Full_LI_Name));
      end if;
   end Convert_To_Parsed;

   ----------------------
   -- Set_End_Of_Scope --
   ----------------------

   procedure Set_End_Of_Scope
     (Declaration_Info : in out E_Declaration_Info_List;
      File             : LI_File_Ptr;
      Location         : Point;
      Kind             : Reference_Kind := End_Of_Body) is
   begin
      Declaration_Info.Value.Declaration.End_Of_Scope.Location :=
        (Line   => Location.Line,
         Column => Location.Column,
         File   => (LI              => File,
                    Part            => Unit_Body,
                    Source_Filename => null));
      Declaration_Info.Value.Declaration.End_Of_Scope.Kind := Kind;
   end Set_End_Of_Scope;

   ----------------------
   -- Insert_Reference --
   ----------------------

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

   ----------------------
   -- Find_Declaration --
   ----------------------

   function Find_Declaration
     (File                    : LI_File_Ptr;
      Symbol_Name             : String := "";
      Class_Name              : String := "";
      Kind                    : E_Kind := Unresolved_Entity_Kind;
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
      Filename                : VFS.Virtual_File := VFS.No_File;
      Kind                    : E_Kind := Unresolved_Entity_Kind;
      Location                : Point := Invalid_Point)
      return E_Declaration_Info_List
   is
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
                      & Full_Name (Get_LI_Filename (File)));
         end if;

         exit when Get_Source_Filename (Dep_Ptr.Value.File) = Filename;
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

   -------------------------------
   -- Find_Declaration_Internal --
   -------------------------------

   function Find_Declaration_Internal
     (Declaration_Info_Ptr : E_Declaration_Info_List;
      Symbol_Name          : String := "";
      Class_Name           : String := "";
      Kind                 : E_Kind := Unresolved_Entity_Kind;
      Location             : Point := Invalid_Point;
      Negate_Kind          : Boolean := False)
      return E_Declaration_Info_List
   is
      D_Ptr : E_Declaration_Info_List := Declaration_Info_Ptr;
   begin
      loop
         exit when D_Ptr = null;

         --  Does the name match ?
         if Symbol_Name'Length = 0
           or else D_Ptr.Value.Declaration.Name.all = Symbol_Name
         then

            --  Does the declaration location match ?
            if Location = Invalid_Point
              or else (D_Ptr.Value.Declaration.Location.Line = Location.Line
                       and then D_Ptr.Value.Declaration.Location.Column =
                       Location.Column)
            then

               --  Does the class name match ?
               if Class_Name = ""
                 or else Belongs_To_Class
                   (Declaration_Info_Ptr => Declaration_Info_Ptr,
                    Class_Name           => Class_Name,
                    Position             => Location)
               then

                  --  Does the kind match
                  if Kind = Unresolved_Entity_Kind
                    or else (Negate_Kind
                             and then D_Ptr.Value.Declaration.Kind /= Kind)
                    or else (not Negate_Kind
                             and then D_Ptr.Value.Declaration.Kind = Kind)
                  then
                     return D_Ptr;
                  end if;
               end if;
            end if;
         end if;

         D_Ptr := D_Ptr.Next;
      end loop;
      return null;
   end Find_Declaration_Internal;

   --------------------
   -- Create_LI_File --
   --------------------

   procedure Create_LI_File
     (File        : out LI_File_Ptr;
      Project     : Projects.Project_Type;
      List        : LI_File_List;
      LI_Filename : VFS.Virtual_File;
      Handler     : LI_Handler) is
   begin
      File := new LI_File_Constrained'
        (LI =>  (Parsed        => False,
                 Handler       => Handler,
                 LI_Filename_Key => new String'(Base_Name (LI_Filename)),
                 LI_Filename   => LI_Filename,
                 Project       => Project,
                 Body_Info     => null,
                 Spec_Info     => null,
                 Separate_Info => null,
                 LI_Timestamp  => 0));
      Add (List.Table.all, File);
   end Create_LI_File;

   ----------------------
   -- Create_File_Info --
   ----------------------

   procedure Create_File_Info
     (FI_Ptr         : out File_Info_Ptr;
      Full_Filename  : VFS.Virtual_File;
      Set_Time_Stamp : Boolean := True;
      Unit_Name      : String := "")
   is
      Directory_Name : constant String := Dir_Name (Full_Filename);
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

   ----------------------
   -- Belongs_To_Class --
   ----------------------

   function Belongs_To_Class
     (Declaration_Info_Ptr : E_Declaration_Info_List;
      Class_Name           : String;
      Position             : Point) return Boolean
   is
      D_Ptr : E_Declaration_Info_List := Declaration_Info_Ptr;
   begin
      loop
         exit when D_Ptr = null;

         if D_Ptr.Value.Declaration.Kind.Kind = Class
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
         if Ref_Ptr.Value.Location.File.LI = File
           and Ref_Ptr.Value.Location.Line = Location.Line
           and Ref_Ptr.Value.Location.Column = Location.Column
           and Ref_Ptr.Value.Kind = Kind
         then
            return Ref_Ptr;
         end if;

         Ref_Ptr := Ref_Ptr.Next;
      end loop;

      return null;
   end Find_Reference;

end Src_Info.LI_Utils;
