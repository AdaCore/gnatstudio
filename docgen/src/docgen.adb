-----------------------------------------------------------------------
--                               G P S                               --
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

with Ada.Text_IO;               use Ada.Text_IO;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Generic_List;
with VFS;                       use VFS;
with Glide_Kernel.Project;      use Glide_Kernel, Glide_Kernel.Project;
with Projects.Registry;         use Projects, Projects.Registry;
with Traces;                    use Traces;
with Ada.Exceptions;            use Ada.Exceptions;
with Language_Handlers;         use Language_Handlers;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Basic_Types;
with Language;                  use Language;

package body Docgen is

   Me : constant Debug_Handle := Create ("Docgen");

   -------------------------
   -- Docgen_Backend body --
   -------------------------

   package body Docgen_Backend is

      --------------------
      -- Get_Last_Index --
      --------------------

      function Get_Last_Index (B : Backend'Class) return Natural is
      begin
         return B.Last_Index;
      end Get_Last_Index;

      -------------------
      -- Get_Last_Line --
      -------------------

      function Get_Last_Line (B : Backend'Class) return Natural is
      begin
         return B.Last_Line;
      end Get_Last_Line;

      --------------------
      -- Set_Last_Index --
      --------------------

      procedure Set_Last_Index (B : in out Backend'Class; Value : Natural) is
      begin
         B.Last_Index := Value;
      end Set_Last_Index;

      -------------------
      -- Set_Last_Line --
      -------------------

      procedure Set_Last_Line (B : in out Backend'Class; Value : Natural) is
      begin
         B.Last_Line := Value;
      end Set_Last_Line;

      -----------------------
      -- Launch_Doc_Create --
      -----------------------

      procedure Launch_Doc_Create
        (B                : Backend_Handle;
         Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
         File             : in Ada.Text_IO.File_Type;
         Entity_List      : in out Type_Entity_List.List;
         List_Ref_In_File : in out List_Reference_In_File.List;
         Info             : in out Docgen.Doc_Info;
         Doc_Directory    : String;
         Doc_Suffix       : String) is
      begin
         Doc_Create (B, Kernel, File,
                     Entity_List, List_Ref_In_File,
                     Info, Doc_Directory, Doc_Suffix);
      end Launch_Doc_Create;

      -----------------
      -- Format_File --
      -----------------

      procedure Format_File
        (B                : access Backend'Class;
         Kernel           : access Kernel_Handle_Record'Class;
         File             : Ada.Text_IO.File_Type;
         Entity_List      : in out Type_Entity_List.List;
         List_Ref_In_File : in out List_Reference_In_File.List;
         LI_Unit          : LI_File_Ptr;
         Text             : String;
         File_Name        : VFS.Virtual_File;
         Entity_Line      : Natural;
         Line_In_Body     : Natural;
         Source_File_List : Type_Source_File_List.List;
         Link_All         : Boolean;
         Is_Body          : Boolean;
         Process_Body     : Boolean;
         Info             : Doc_Info)
      is
         function Callback
           (Entity         : Language_Entity;
            Sloc_Start     : Source_Location;
            Sloc_End       : Source_Location;
            Partial_Entity : Boolean) return Boolean;
         --  ??? What does this function do

         --------------
         -- Callback --
         --------------

         function Callback
           (Entity         : Language_Entity;
            Sloc_Start     : Source_Location;
            Sloc_End       : Source_Location;
            Partial_Entity : Boolean) return Boolean
         is
            pragma Unreferenced (Partial_Entity);
         begin
            case Entity is
               when Comment_Text =>
                  Format_Comment
                    (B,
                     File,
                     Text,
                     Sloc_Start.Index,
                     Sloc_Start.Line,
                     Sloc_End.Index - 1,
                     Sloc_End.Line,
                     Entity_Line);

               when Keyword_Text =>
                  Format_Keyword
                    (B,
                     File,
                     Text,
                     Sloc_Start.Index,
                     Sloc_Start.Line,
                     Sloc_End.Index,
                     Sloc_End.Line,
                     Entity_Line);

               when String_Text =>
                  Format_String
                    (B,
                     File,
                     Text,
                     Sloc_Start.Index,
                     Sloc_Start.Line,
                     Sloc_End.Index,
                     Sloc_End.Line,
                     Entity_Line);

               when Character_Text =>
                  Format_Character
                    (B,
                     File,
                     Text,
                     Sloc_Start.Index,
                     Sloc_Start.Line,
                     Sloc_End.Index,
                     Sloc_End.Line,
                     Entity_Line);

               when Identifier_Text =>
                  Format_Identifier
                    (B,
                     Entity_List,
                     List_Ref_In_File,
                     Sloc_Start.Index,
                     Sloc_Start.Line,
                     Sloc_Start.Column,
                     Sloc_End.Index,
                     Sloc_End.Line,
                     Kernel,
                     File,
                     LI_Unit,
                     Text,
                     File_Name,
                     Entity_Line,
                     Line_In_Body,
                     Source_File_List,
                     Link_All,
                     Is_Body,
                     Process_Body,
                     Info);

               when others =>
                  null;
            end case;

            return False;
         end Callback;

      begin
         Initialize (B, Text);
         Parse_Entities (Get_Language_From_File
                           (Get_Language_Handler (Kernel), File_Name),
                            Text,
                            Callback'Unrestricted_Access);
         Finish (B, File, Text, Entity_Line);

      exception
         when E : others =>
            Trace (Me, "Unexpected exception: " & Exception_Information (E));
      end Format_File;

   end Docgen_Backend;

   ---------------------
   -- Format_All_Link --
   ---------------------

   procedure Format_All_Link
     (B                : access Backend'Class;
      Entity_List      : in out Type_Entity_List.List;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Start_Index      : Natural;
      Start_Line       : Natural;
      Start_Column     : Natural;
      End_Index        : Natural;
      Kernel           : access Kernel_Handle_Record'Class;
      File             : Ada.Text_IO.File_Type;
      LI_Unit          : LI_File_Ptr;
      Text             : String;
      File_Name        : VFS.Virtual_File;
      Entity_Line      : Natural;
      Line_In_Body     : Natural;
      Source_File_List : Type_Source_File_List.List;
      Link_All         : Boolean;
      Is_Body          : Boolean;
      Process_Body     : Boolean;
      Info             : Doc_Info)
   is

      use type Basic_Types.String_Access;
      use List_Reference_In_File;
      use Type_Entity_List;
      Loc_End          : Natural;
      Loc_Start        : Natural;
      Point_In_Column  : Natural := 0;
      Entity_Info      : Entity_Information;
      Ref_List_Info    : List_Reference_In_File.List_Node;
      Ref_List_Info_Prec   : List_Reference_In_File.List_Node;
      Result           : Boolean;

      Entity_Node      : Type_Entity_List.List_Node;
      Entity_Node_Succ : Type_Entity_List.List_Node;
      Exist            : Boolean;
      --  Used to delete from Entity_List identifiers contained in inner
      --  packages
      procedure Get_Declaration
        (Text   : String;
         E_I    : in out Src_Info.Queries.Entity_Information;
         Line   : Natural;
         Column : Natural;
         E_L_I  : in List_Reference_In_File.List_Node;
         Result : in out Boolean);
      --  Looks if the reference E_L_I is the same as (Text+Line+Column)
      --  If yes, the declaration of E_L_I is returned and Result is True

      procedure Get_Declaration
        (Text   : String;
         E_I    : in out Src_Info.Queries.Entity_Information;
         Line   : Natural;
         Column : Natural;
         E_L_I  : in List_Reference_In_File.List_Node;
         Result : in out Boolean) is
         pragma Unreferenced (Column);
      begin
         if List_Reference_In_File.Data (E_L_I).Line = Line
           and then
             To_Lower (Text) = List_Reference_In_File.Data (E_L_I).Name.all
         --  and then
         --  List_Reference_In_File.Data (E_L_I).Column = Column
         --  ??? perhaps problems with Parse_Entites about columns
         then
            Result := True;
            E_I := List_Reference_In_File.Data (E_L_I).Entity.all;
         end if;
      end Get_Declaration;

   begin
      Loc_Start := Start_Index;

      --  Take apart parsed entites with any "."'s in the middle
      for J in 1 ..
        1 + Count_Points (Text (Start_Index .. End_Index))
      loop
         Point_In_Column :=
           Index (Text (Loc_Start .. End_Index), ".");

         if Point_In_Column > 0 then
            Loc_End := Point_In_Column - 1;
         else
            Loc_End := End_Index;
         end if;

         if LI_Unit /= No_LI_File then
            --  We search the declaration of the entity
            --  (which is an identifier)

            if Is_Spec_File (Kernel, File_Name) and then
              (Info.Info_Type = Package_Info) then
               --  Only if we are parsing a package
               Entity_Node := Type_Entity_List.First (Entity_List);
               Entity_Node_Succ := Type_Entity_List.Null_Node;
               Exist := False;
               while  Entity_Node /= Type_Entity_List.Null_Node loop
                  if (Get_Name (Data (Entity_Node).Entity) =
                        To_Lower (Text (Loc_Start .. Loc_End)))
                    and then
                      (Get_Declaration_Line_Of (Data (Entity_Node).Entity) =
                         Start_Line + Entity_Line - 1)
                  then
                     --  This identifier is a declaration.
                     Exist := True;
                  end if;
                  if Exist then
                     --  Identifier removed. It won't be duplicated.
                     Type_Entity_List.Remove_Nodes
                       (Entity_List,
                        Entity_Node_Succ,
                        Entity_Node);
                  end if;
                  exit when Exist = True;
                  Entity_Node_Succ := Entity_Node;
                  Entity_Node := Type_Entity_List.Next (Entity_Node_Succ);
               end loop;
            end if;

            Result := False;
            Ref_List_Info
              := List_Reference_In_File.First (List_Ref_In_File);
            Ref_List_Info_Prec := List_Reference_In_File.Null_Node;

            --  Text(Loc_Start .. Loc_End) is a reference.
            --  We search it in the list we have made before in order to
            --  find its declaration.
            while Ref_List_Info /= List_Reference_In_File.Null_Node loop

               Get_Declaration
                 (Text (Loc_Start .. Loc_End),
                  Entity_Info,
                  Start_Line + Entity_Line - 1,
                  Start_Column + Loc_Start - Start_Index,
                  Ref_List_Info,
                  Result);
               if Result = True then
                  List_Reference_In_File.Remove_Nodes
                    (List_Ref_In_File, Ref_List_Info_Prec,
                     Ref_List_Info);
               end if;

               exit when Result = True;
               Ref_List_Info_Prec := Ref_List_Info;
               Ref_List_Info
                 := List_Reference_In_File.Next (Ref_List_Info);
            end loop;

            --  We create a link on the declaration for this entity
            if Result = True then
               Format_Link
                 (B,
                  Start_Index, Start_Line, Start_Column, End_Index,
                  Kernel, File, LI_Unit, Text, File_Name, Entity_Line,
                  Line_In_Body, Source_File_List, Link_All, Is_Body,
                  Process_Body, Loc_End, Loc_Start, Entity_Info);
            end if;

         end if;

         if Point_In_Column > 0 then
            Loc_Start := Point_In_Column + 1;
         end if;
      end loop;
   end Format_All_Link;

   ----------------------
   -- Compare_Elements --
   ----------------------

   function Compare_Elements (X, Y : Source_File_Information) return Boolean is
   begin
      if X.File_Name = Y.File_Name then
         --  ??? Strange test: if one is the spec, the other is also the spec
         --  return Is_Spec_File (Kernel, X.File_Name);
         return True;
      else
         return Full_Name (X.File_Name).all < Full_Name (Y.File_Name).all;
      end if;
   end Compare_Elements;

   ---------------------------
   -- Compare_Elements_Name --
   ---------------------------

   function Compare_Elements_Name
     (X, Y : Entity_List_Information) return Boolean is
   begin
      if X.Is_Private and not Y.Is_Private then
         return False;
      elsif not X.Is_Private and Y.Is_Private then
         return True;
      else
         return Get_Name (X.Entity) < Get_Name (Y.Entity);
      end if;
   end Compare_Elements_Name;

   ---------------------------
   -- Compare_Elements_Line --
   ---------------------------

   function Compare_Elements_Line
     (X, Y : Entity_List_Information) return Boolean is
   begin
      return Get_Declaration_Line_Of (X.Entity) <
        Get_Declaration_Line_Of (Y.Entity);
   end Compare_Elements_Line;

   -----------------------------
   -- Compare_Elements_Column --
   -----------------------------

   function Compare_Elements_Column
     (X, Y : Entity_List_Information) return Boolean is
   begin
      return Get_Declaration_Column_Of (X.Entity) <
        Get_Declaration_Column_Of (Y.Entity);
   end Compare_Elements_Column;

   -----------------------------
   -- Compare_Elements_Column --
   -----------------------------

   function Compare_Elements_Column
     (X, Y : Reference_List_Information) return Boolean is
   begin
      return Get_Declaration_Column_Of (X.Entity) <
        Get_Declaration_Column_Of (Y.Entity);
   end Compare_Elements_Column;

   ---------------------------
   -- Compare_Elements_Name --
   ---------------------------

   function Compare_Elements_Name
     (X, Y : Reference_List_Information) return Boolean is
   begin
      return Get_Name (X.Entity) < Get_Name (Y.Entity);
   end Compare_Elements_Name;

   -----------------------------------------
   -- Compare_Elements_By_Line_And_Column --
   -----------------------------------------

   function Compare_Elements_By_Line_And_Column
     (X, Y : Reference_In_File) return Boolean is
   begin
      return X.Line < Y.Line or else
      (X.Line = Y.Line and then X.Column < Y.Column);
   end Compare_Elements_By_Line_And_Column;


   ----------
   -- Free --
   ----------

   procedure Free (X : in out Source_File_Information) is
   begin
      Free (X.Package_Name);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Entity_List_Information) is
   begin
      Free (X.Name);
      Destroy (X.Entity);

      if not Type_Reference_List.Is_Empty (X.Calls_List) then
         Type_Reference_List.Free (X.Calls_List);
      end if;

      if not Type_Reference_List.Is_Empty (X.Called_List) then
         Type_Reference_List.Free (X.Called_List);
      end if;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Reference_List_Information) is
   begin
      Destroy (X.Entity);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Reference_In_File) is
   begin
      Free (X.Name);
      --  Memory accessed by the field Entity is free separately: those
      --  records are saved in a list. When we destroy this list, it calls
      --  subprogram Free (Entity_Information).
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Src_Info.Queries.Entity_Information) is
   begin
      Destroy (X);
   end Free;

   -----------------
   -- Count_Lines --
   -----------------

   function Count_Lines (Line : String) return Natural is
      Line_Nr : Natural := 1;
   begin
      for J in Line'Range loop
         if Line (J) = ASCII.LF then
            Line_Nr := Line_Nr + 1;
         end if;
      end loop;

      return Line_Nr;
   end Count_Lines;

   -----------------------
   -- Get_Doc_File_Name --
   -----------------------

   function Get_Doc_File_Name
     (Source_Filename : VFS.Virtual_File;
      Source_Path     : String;
      Doc_Suffix      : String) return String
   is
      Ext      : constant String := File_Extension (Source_Filename);
   begin
      return Source_Path &
        Base_Name (Source_Filename, Ext)
        & "_"
        & Ext (Ext'First + 1 .. Ext'Last)
        & Doc_Suffix;
   end Get_Doc_File_Name;

   -----------
   -- Clone --
   -----------

   function Clone
     (Entity : Entity_List_Information) return Entity_List_Information is
   begin
      return
        (Kind         => Entity.Kind,
         Name         => new String'(Entity.Name.all),
         Entity       => Copy (Entity.Entity),
         Is_Private   => Entity.Is_Private,
         Line_In_Body => Entity.Line_In_Body,
         Calls_list   => Type_Reference_List.Null_List,   --  ???
         Called_List  => Type_Reference_List.Null_List);  --  ???
   end Clone;

   -------------------------
   -- Source_File_In_List --
   -------------------------

   function Source_File_In_List
     (Source_File_List : Type_Source_File_List.List;
      Name             : Virtual_File) return Boolean
   is
      package TSFL renames Type_Source_File_List;
      use type TSFL.List_Node;
      Source_File_Node : TSFL.List_Node := TSFL.First (Source_File_List);
   begin
      while Source_File_Node /= TSFL.Null_Node loop
         if TSFL.Data (Source_File_Node).File_Name = Name then
            return True;
         end if;

         Source_File_Node := TSFL.Next (Source_File_Node);
      end loop;

      return False;
   end Source_File_In_List;

   ------------------
   -- Count_Points --
   ------------------

   function Count_Points (Text : String) return Natural is
      Counter : Natural := 0;
   begin
      for J in Text'First .. Text'Last loop
         if Text (J) = '.' then
            Counter := Counter + 1;
         end if;
      end loop;

      return Counter;
   end Count_Points;

   -----------------
   -- Spec_Suffix --
   -----------------

   function Spec_Suffix
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File   : VFS.Virtual_File) return String is
   begin
      if Is_Spec_File (Kernel, File) then
         return File_Extension (File);
      else
         return File_Extension (Other_File_Name (Kernel, File));
      end if;
   end Spec_Suffix;

   -----------------
   -- Body_Suffix --
   -----------------

   function Body_Suffix
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File   : VFS.Virtual_File) return String is
   begin
      if Is_Spec_File (Kernel, File) then
         return File_Extension (Other_File_Name (Kernel, File));
      else
         return File_Extension (File);
      end if;
   end Body_Suffix;

   ------------------
   -- Is_Spec_File --
   ------------------

   function Is_Spec_File
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File   : VFS.Virtual_File) return Boolean is
   begin
      return Get_Unit_Part_From_Filename
        (Get_Project_From_File (Get_Registry (Kernel), File), File) =
        Unit_Spec;
   end Is_Spec_File;

end Docgen;
