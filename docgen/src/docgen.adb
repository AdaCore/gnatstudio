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
with Basic_Types;               use Basic_Types;
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

      ----------------
      -- Get_Indent --
      ----------------

      function Get_Indent (B : Backend'Class) return Natural is
      begin
         return B.Indent;
      end Get_Indent;

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
         List_Ref_In_File : in out List_Reference_In_File.List;
         Info             : in out Docgen.Doc_Info;
         Doc_Directory    : String;
         Doc_Suffix       : String;
         Level            : Natural) is
      begin
         Doc_Create (B, Kernel, File, List_Ref_In_File,
                     Info, Doc_Directory, Doc_Suffix, Level);
      end Launch_Doc_Create;

      -----------------
      -- Format_File --
      -----------------

      procedure Format_File
        (B                : access Backend'Class;
         Kernel           : access Kernel_Handle_Record'Class;
         File             : Ada.Text_IO.File_Type;
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
         Info             : Doc_Info;
         Level            : Natural;
         Indent           : Natural)
      is

         function Is_Operator (Op : String) return Boolean;
         --  Indicates if op is a subprogram which overloads an operator

         function Callback
           (Entity         : Language_Entity;
            Sloc_Start     : Source_Location;
            Sloc_End       : Source_Location;
            Partial_Entity : Boolean) return Boolean;
         --  Looks the value of type Language_Entity returned by
         --  Parse_Entities and calls the good subprograms to format
         --  the current entity

         -------------------
         --  Is_Operator  --
         -------------------

         function Is_Operator (Op : String) return Boolean is
            use List_Reference_In_File;
            Ref_List_Info : List_Reference_In_File.List_Node;
         begin
            --  Can this function be simplified ???

            Ref_List_Info := List_Reference_In_File.First (List_Ref_In_File);

            while Ref_List_Info /= List_Reference_In_File.Null_Node loop
               if List_Reference_In_File.Data (Ref_List_Info).Name /= null then
                  if Op = List_Reference_In_File.Data (Ref_List_Info).Name.all
                     --  op is a reference
                    and then
                      (Get_Kind (List_Reference_In_File.Data
                                   (Ref_List_Info).Entity.all).Kind
                         = Function_Or_Operator
                       or else
                         Get_Kind (List_Reference_In_File.Data
                                     (Ref_List_Info).Entity.all).Kind
                         = Procedure_Kind)
                     --  The entity of this reference overloads an operator
                  then
                     return True;
                  end if;

                  Ref_List_Info := List_Reference_In_File.Next (Ref_List_Info);
               end if;
            end loop;

            return False;
         end Is_Operator;

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
                  --  In this context, we must detect overriden operators
                  if Text (Sloc_Start.Index) = '"'
                    and then
                      Text (Sloc_End.Index) = '"'
                    and then
                      Sloc_Start.Index + 1 <= Sloc_End.Index - 1
                  then
                     if (not Is_Spec_File (Kernel, File_Name)
                         and then
                        --  For a body file, we must search the word in the
                        --  list of reference that has been made before
                        --  because the body is formated in one piece and we
                        --  don't have any information about this word
                           Is_Operator
                             (Text (Sloc_Start.Index + 1 ..
                                      Sloc_End.Index - 1)))
                       or else
                         (Is_Spec_File (Kernel, File_Name)
                          and then
                           --  For a spec file, we know immediatly the nature
                           --  of the word
                            Info.Info_Type = Subprogram_Info
                          and then
                            Text (Sloc_Start.Index + 1 .. Sloc_End.Index - 1)
                            = Get_Name (Info.Subprogram_Entity.Entity))
                     then
                        --  Function which overrides an operator
                           Format_Identifier
                             (B,
                              List_Ref_In_File,
                              Sloc_Start.Index + 1,
                              Sloc_Start.Line,
                              Sloc_Start.Column,
                              Sloc_End.Index - 1,
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
                              Level,
                              Indent);
                     else
                        --  Simple string
                        Format_String
                          (B,
                           File,
                           Text,
                           Sloc_Start.Index,
                           Sloc_Start.Line,
                           Sloc_End.Index,
                           Sloc_End.Line,
                           Entity_Line);
                     end if;
                  else
                     --  Simple string
                     Format_String
                       (B,
                        File,
                        Text,
                        Sloc_Start.Index,
                        Sloc_Start.Line,
                        Sloc_End.Index,
                        Sloc_End.Line,
                        Entity_Line);
                  end if;
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
--                    Trace (Me, "Id :: "
--                      & Text (Sloc_Start.Index .. Sloc_End.Index));
                  if Text (Sloc_Start.Index .. Sloc_End.Index) /= ";" then
                     --  ???  This test is necessary because Parse_Entity
                     --  consider the last ";" of the text as an identifier.
                     --  What is surprising is it occurs only for the text of
                     --  spec file and not for the text of body file. Perhaps,
                     --  the reason is that in the body the ";" ended the
                     --  file.
                     Format_Identifier
                       (B,
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
                        Level,
                        Indent);
                  end if;
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
     (B                   : access Backend'Class;
      List_Ref_In_File    : in out List_Reference_In_File.List;
      Start_Index         : Natural;
      Start_Line          : Natural;
      Start_Column        : Natural;
      End_Index           : Natural;
      Kernel              : access Kernel_Handle_Record'Class;
      File                : Ada.Text_IO.File_Type;
      LI_Unit             : LI_File_Ptr;
      Text                : String;
      File_Name           : VFS.Virtual_File;
      Entity_Line         : Natural;
      Line_In_Body        : in out Natural;
      Source_File_List    : Type_Source_File_List.List;
      Link_All            : Boolean;
      Is_Body             : Boolean;
      Process_Body        : Boolean;
      Level               : Natural;
      Indent              : Natural)
   is
      use type Basic_Types.String_Access;
      use List_Reference_In_File;
      use Type_Entity_List;
      Loc_End            : Natural;
      Loc_Start          : Natural;
      Point_In_Column    : Natural := 0;
      Entity_Info        : Entity_Information;
      Ref_List_Info      : List_Reference_In_File.List_Node;
      Ref_List_Info_Prec : List_Reference_In_File.List_Node;
      Result             : Boolean;
      Entity_Abstract    : Boolean;
      Indentation        : Natural;
      --  This last parameter is used to add levels of indentation
      --  for spec files

      procedure Get_Declaration
        (Text             : String;
         E_I              : in out Src_Info.Queries.Entity_Information;
         Line             : Natural;
         Column           : Natural;
         E_L_I            : in List_Reference_In_File.List_Node;
         Result           : in out Boolean;
         Entity_Abstract  : in out Boolean);
      --  Looks if the reference E_L_I is the same as (Text+Line+Column)
      --  If yes, the declaration of E_L_I is returned and Result is True

      procedure Get_Declaration
        (Text             : String;
         E_I              : in out Src_Info.Queries.Entity_Information;
         Line             : Natural;
         Column           : Natural;
         E_L_I            : in List_Reference_In_File.List_Node;
         Result           : in out Boolean;
         Entity_Abstract  : in out Boolean) is
      begin
         if List_Reference_In_File.Data (E_L_I).Line = Line
           and then
             To_Lower (Text) = List_Reference_In_File.Data (E_L_I).Name.all
           and then
             List_Reference_In_File.Data (E_L_I).Column = Column
         then
            Result := True;
            E_I := List_Reference_In_File.Data (E_L_I).Entity.all;
            if Get_Kind (E_I).Is_Abstract then
               Entity_Abstract := True;
            end if;
         end if;
      end Get_Declaration;

   begin
      if Is_Body then
         Indentation := 0;
      else
         Indentation := Level * Indent;
         --  For spec files, we must add levels of indentation otherwise
         --  a text and its associated reference in list won't match.
         --  In fact, the string which is given to Format_File is obtained by
         --  Get_Whole_Header + Remove_Indent. Get_Whole_Header remove the
         --  indentation of the first line and Remove_Indent remove the
         --  indentation of the other lines
      end if;
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

            Result := False;
            Entity_Abstract := False;
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
                  Start_Column + Loc_Start - Start_Index + Indentation,
                  Ref_List_Info,
                  Result,
                  Entity_Abstract);
               if Result = True then
                  List_Reference_In_File.Remove_Nodes
                    (List_Ref_In_File, Ref_List_Info_Prec,
                     Ref_List_Info);
               end if;

               exit when Is_Body;
               --  for body files, no loop because references in the list
               --  are sorted. So the first element met in list is the good
               --  one (for this elements are removed after being met).
               exit when Result;
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
                  Process_Body, Loc_End, Loc_Start, Entity_Info,
                  Entity_Abstract);
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

   ------------------
   -- Compare_Name --
   ------------------

   function Compare_Name
     (X, Y : Entity_Handle) return Boolean is
   begin
      if X /= null and Y /= null then
         return Get_Name (X.all) < Get_Name (Y.all);
      else
         --  This case normally never happens

         return X /= null;
      end if;
   end Compare_Name;

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

   -----------------
   --  Duplicate  --
   -----------------

   procedure Duplicate
     (List_Out : in out Type_Reference_List.List;
      List_In  : in Type_Reference_List.List)
   is
      use Type_Reference_List;
      Node : Type_Reference_List.List_Node;
   begin
      if not Type_Reference_List.Is_Empty (List_In) then
         Node := Type_Reference_List.First (List_In);
         while Node /= Type_Reference_List.Null_Node loop
            Type_Reference_List.Append
              (List_Out,
               (Copy (Type_Reference_List.Data (Node).Entity),
                Type_Reference_List.Data (Node).Set_Link));
            Node := Type_Reference_List.Next (Node);
         end loop;
      end if;
   end Duplicate;

   -------------------------
   -- Compare_Tagged_Name --
   -------------------------

   function Compare_Tagged_Name
     (X, Y : Tagged_Element) return Boolean is
   begin
      return Get_Name (X.Me.all) < Get_Name (Y.Me.all);
   end Compare_Tagged_Name;

   -----------------------------------------
   -- Compare_Elements_By_Line_And_Column --
   -----------------------------------------

   function Compare_Elements_By_Line_And_Column
     (X, Y : Reference_In_File) return Boolean is
   begin
      return X.Line < Y.Line or else
      (X.Line = Y.Line and then X.Column < Y.Column);
   end Compare_Elements_By_Line_And_Column;

   ------------------
   -- Find_In_List --
   ------------------

   function Find_In_List
     (List : List_Entity_Handle.List;
      Info : Entity_Information) return Entity_Handle
   is
      use List_Entity_Handle;
      Node  : List_Entity_Handle.List_Node;
   begin
      Node  := List_Entity_Handle.First (List);

      while Node /= List_Entity_Handle.Null_Node loop
         if List_Entity_Handle.Data (Node) /= null then
            if Is_Equal (List_Entity_Handle.Data (Node).all, Info) then
               return List_Entity_Handle.Data (Node);
            end if;
         end if;
         Node := List_Entity_Handle.Next (Node);
      end loop;
      return null;
   end Find_In_List;

   ---------------
   -- Add_Child --
   ---------------

   procedure Add_Child
     (List   : in out Type_List_Tagged_Element.List;
      Target : in Entity_Handle;
      Patch  : in Entity_Handle)
   is
      use Type_List_Tagged_Element;
      Node        : Type_List_Tagged_Element.List_Node;
      Follow_Node : Type_List_Tagged_Element.List_Node;
      Found       : Boolean;
      Tag_Elem    : Tagged_Element_Handle;

   begin
      Found       := False;
      Node        := Type_List_Tagged_Element.First (List);
      Follow_Node := Type_List_Tagged_Element.Null_Node;

      if Node /= Type_List_Tagged_Element.Null_Node then
         --  Work on the first node first
         if Is_Equal (Data (Node).Me.all, Target.all) then
            --  Target found: update if needed and exit
            if Find_In_List (Data (Node).My_Children,
                             Patch.all) = null then
               Tag_Elem := new Tagged_Element'(Data (Node));
               List_Entity_Handle.Append (Tag_Elem.My_Children, Patch);
               Tag_Elem.Number_Of_Children := Tag_Elem.Number_Of_Children + 1;
               Type_List_Tagged_Element.Remove_Nodes (List, Follow_Node, Node);
               Type_List_Tagged_Element.Append (List, Tag_Elem.all);
            end if;
            Found := True;
         end if;
         --  Work on the other element of the list
         if not Found then
            Follow_Node := Node;
            Node := Type_List_Tagged_Element.Next (Node);

            while Node /= Type_List_Tagged_Element.Null_Node loop
               if Is_Equal (Data (Node).Me.all, Target.all) then
                  --  Target found: update if needed and exit
                  if Find_In_List (Data (Node).My_Children,
                                   Patch.all) = null then
                     Tag_Elem := new Tagged_Element'(Data (Node));
                     List_Entity_Handle.Append (Tag_Elem.My_Children, Patch);
                     Tag_Elem.Number_Of_Children
                       := Tag_Elem.Number_Of_Children + 1;
                     Type_List_Tagged_Element.Remove_Nodes
                       (List, Follow_Node, Node);
                     Type_List_Tagged_Element.Append (List, Tag_Elem.all);
                  end if;
                  Found := True;
               else
                  Follow_Node := Node;
                  Node := Type_List_Tagged_Element.Next (Node);
               end if;
               exit when Found;
            end loop;
         end if;
      end if;
   end Add_Child;

   ----------------
   -- Add_Parent --
   ----------------

   procedure Add_Parent
     (List   : in out Type_List_Tagged_Element.List;
      Target : in Entity_Handle;
      Patch  : in Entity_Handle)
   is
      use Type_List_Tagged_Element;
      Node        : Type_List_Tagged_Element.List_Node;
      Follow_Node : Type_List_Tagged_Element.List_Node;
      Found       : Boolean;
      Tag_Elem    : Tagged_Element_Handle;

   begin
      Found       := False;
      Node        := Type_List_Tagged_Element.First (List);
      Follow_Node := Type_List_Tagged_Element.Null_Node;

      if Node /= Type_List_Tagged_Element.Null_Node then
         --  Work on the first node first
         if Is_Equal (Data (Node).Me.all, Target.all) then
            --  Target found: update if needed and exit
            if Find_In_List (Data (Node).My_Parents,
                             Patch.all) = null then
               Tag_Elem := new Tagged_Element'(Data (Node));
               List_Entity_Handle.Append (Tag_Elem.My_Parents, Patch);
               Tag_Elem.Number_Of_Parents := Tag_Elem.Number_Of_Parents + 1;
               Type_List_Tagged_Element.Remove_Nodes (List, Follow_Node, Node);
               Type_List_Tagged_Element.Append (List, Tag_Elem.all);
            end if;
            Found := True;
         end if;
         --  Work on the other element of the list
         if not Found then
            Follow_Node := Node;
            Node := Type_List_Tagged_Element.Next (Node);

            while Node /= Type_List_Tagged_Element.Null_Node loop
               if Is_Equal (Data (Node).Me.all, Target.all) then
                  if Find_In_List (Data (Node).My_Parents,
                                   Patch.all) = null then
                     --  Target found: update if needed and exit
                     Tag_Elem := new Tagged_Element'(Data (Node));
                     List_Entity_Handle.Append (Tag_Elem.My_Parents, Patch);
                     Tag_Elem.Number_Of_Parents
                       := Tag_Elem.Number_Of_Parents + 1;
                     Type_List_Tagged_Element.Remove_Nodes
                       (List, Follow_Node, Node);
                     Type_List_Tagged_Element.Append (List, Tag_Elem.all);
                  end if;
                  Found := True;
               else
                  Follow_Node := Node;
                  Node := Type_List_Tagged_Element.Next (Node);
               end if;
               exit when Found;
            end loop;
         end if;
      end if;
   end Add_Parent;

   ----------------------------
   -- Must_Print_Tagged_Type --
   ----------------------------

   procedure Must_Print_Tagged_Type
     (List : in out Type_List_Tagged_Element.List;
      Target : in Entity_Handle)
   is
      use Type_List_Tagged_Element;
      Node        : Type_List_Tagged_Element.List_Node;
      Follow_Node : Type_List_Tagged_Element.List_Node;
      Found       : Boolean;
      Tag_Elem    : Tagged_Element_Handle;
   begin
      Found := False;
      Node        := Type_List_Tagged_Element.First (List);
      Follow_Node := Type_List_Tagged_Element.Null_Node;

      if Node /= Type_List_Tagged_Element.Null_Node then
         --  Work on the first node first
         if Is_Equal (Data (Node).Me.all, Target.all) then
            --  Target found: update if needed and exit
            if Data (Node).Print_Me =  False then
               Tag_Elem := new Tagged_Element'(Data (Node));
               Tag_Elem.Print_Me := True;
               Type_List_Tagged_Element.Remove_Nodes
                 (List, Follow_Node, Node);
               Type_List_Tagged_Element.Append (List, Tag_Elem.all);
            end if;
            Found := True;
         end if;
         --  Work on the other element of the list
         if not Found then
            Follow_Node := Node;
            Node := Type_List_Tagged_Element.Next (Node);

            while Node /= Type_List_Tagged_Element.Null_Node loop
               if Is_Equal (Data (Node).Me.all, Target.all) then
                  --  Target found: update if needed and exit
                  if Data (Node).Print_Me =  False then
                     Tag_Elem := new Tagged_Element'(Data (Node));
                     Tag_Elem.Print_Me := True;
                     Type_List_Tagged_Element.Remove_Nodes
                       (List, Follow_Node, Node);
                     Type_List_Tagged_Element.Append (List, Tag_Elem.all);
                  end if;
                  Found := True;
               else
                  Follow_Node := Node;
                  Node := Type_List_Tagged_Element.Next (Node);
               end if;
               exit when Found;
            end loop;
         end if;
      end if;
   end Must_Print_Tagged_Type;

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
      if X.Public_Declaration /= No_Entity_Information then
         Destroy (X.Public_Declaration);
      end if;

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

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Tagged_Element) is
   pragma Unreferenced (X);
   begin
      null;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Entity_Handle) is
      pragma Unreferenced (X);
   begin
      null;
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
      Ext : constant String := File_Extension (Source_Filename);
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
     (Entity     : Entity_List_Information;
      Copy_Lists : Boolean) return Entity_List_Information
   is
      Calls  : Type_Reference_List.List := Type_Reference_List.Null_List;
      Called : Type_Reference_List.List := Type_Reference_List.Null_List;
   begin
      if Copy_Lists then
         --  Deep copy required
         Duplicate (Calls, Entity.Calls_List);
         Duplicate (Called, Entity.Called_List);
      end if;
      if Entity.Public_Declaration /= No_Entity_Information then
         return
           (Kind         => Entity.Kind,
            Name         => new String'(Entity.Name.all),
            Entity       => Copy (Entity.Entity),
            Is_Private   => Entity.Is_Private,
            Line_In_Body => Entity.Line_In_Body,
            Calls_List   => Calls,
            Called_List  => Called,
            Public_Declaration => Copy (Entity.Public_Declaration));
      else
         return
           (Kind         => Entity.Kind,
            Name         => new String'(Entity.Name.all),
            Entity       => Copy (Entity.Entity),
            Is_Private   => Entity.Is_Private,
            Line_In_Body => Entity.Line_In_Body,
            Calls_List   => Calls,
            Called_List  => Called,
            Public_Declaration => No_Entity_Information);
      end if;
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

         if Full_Name (TSFL.Data (Source_File_Node).File_Name).all
           > Full_Name (Name).all
         then
            --  The list is sorted, we can exit
            return false;
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
