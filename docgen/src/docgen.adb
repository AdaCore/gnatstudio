-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2004                       --
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

   function String_Hash is new HTables.Hash (HTable_Header);

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

      -----------------
      -- Format_Code --
      -----------------

      procedure Format_Code
        (B                : access Backend'Class;
         Kernel           : access Kernel_Handle_Record'Class;
         File             : File_Descriptor;
         List_Ref_In_File : in out List_Reference_In_File.List;
         Text             : String;
         File_Name        : VFS.Virtual_File;
         Entity_Line      : Natural;
         Line_In_Body     : Natural;
         Is_Body          : Boolean;
         Options          : All_Options;
         Source_File_List : Type_Source_File_Table.HTable;
         Level            : Natural;
         Indent           : Natural)
      is
--         Is_Spec : constant Boolean := Is_Spec_File (Kernel, File_Name);

--         function Is_Operator (Op : String) return Boolean;
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

--           function Is_Operator (Op : String) return Boolean is
--              use List_Reference_In_File;
--              Ref_List_Info : List_Reference_In_File.List_Node;
--              Ref           : List_Reference_In_File.Data_Access;
--
--           begin
--              --  Can this function be simplified ???
--
--            Ref_List_Info := List_Reference_In_File.First (List_Ref_In_File);
--
--              while Ref_List_Info /= List_Reference_In_File.Null_Node loop
--                 Ref := List_Reference_In_File.Data_Ref (Ref_List_Info);
--
--                 if Get_Name (Ref.Entity) = Op then
--                    case Get_Kind (Ref.Entity).Kind is
--                       when Function_Or_Operator | Procedure_Kind =>
--                      --  The entity of this reference overloads an operator
--                          return True;
--                       when others  =>
--                          return False;
--                    end case;
--                 end if;
--
--                Ref_List_Info := List_Reference_In_File.Next (Ref_List_Info);
--              end loop;
--
--              return False;
--           end Is_Operator;

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
--                    --  In this context, we must detect overriden operators
--                    if Text (Sloc_Start.Index) = '"'
--                      and then
--                        Text (Sloc_End.Index) = '"'
--                      and then
--                        Sloc_Start.Index + 1 <= Sloc_End.Index - 1
--                    then
--                       if (not Is_Spec
--                           and then
--                          --  For a body file, we must search the word in the
--                          --  list of reference that has been made before
--                         --  because the body is formated in one piece and we
--                          --  don't have any information about this word
--                             Is_Operator
--                               (Text (Sloc_Start.Index + 1 ..
--                                        Sloc_End.Index - 1)))
--                         or else
--                           (Is_Spec
--                          --  For a spec file, we know immediatly the nature
--                             --  of the word
--                            and then Info in Doc_Info_Subprogram'Class
--                            and then
--                         Text (Sloc_Start.Index + 1 .. Sloc_End.Index - 1) =
--                              Get_Name (Doc_Info_Subprogram
--                                          (Info).Subprogram_Entity.Entity))
--                       then
--                          --  Function which overrides an operator
--                          Format_Identifier
--                            (B,
--                             List_Ref_In_File,
--                             Sloc_Start.Index + 1,
--                             Sloc_Start.Line,
--                             Sloc_Start.Column,
--                             Sloc_End.Index - 1,
--                             Sloc_End.Line,
--                             Kernel,
--                             File,
--                             Text,
--                             File_Name,
--                             Entity_Line,
--                             Line_In_Body,
--                             Source_File_List,
--                             Options.Link_All,
--                             Is_Body,
--                             Options.Process_Body_Files,
--                             Level,
--                             Indent);
--                       else
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
--                       end if;
--                    else
--                       --  Simple string
--                       Format_String
--                         (B,
--                          File,
--                          Text,
--                          Sloc_Start.Index,
--                          Sloc_Start.Line,
--                          Sloc_End.Index,
--                          Sloc_End.Line,
--                          Entity_Line);
--                    end if;

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
                        Text,
                        File_Name,
                        Entity_Line,
                        Line_In_Body,
                        Source_File_List,
                        Options.Link_All,
                        Is_Body,
                        Options.Process_Body_Files,
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
      end Format_Code;
   end Docgen_Backend;

   ---------------------
   -- Format_All_Link --
   ---------------------

   procedure Format_All_Link
     (B                   : access Docgen_Backend.Backend'Class;
      List_Ref_In_File    : in out List_Reference_In_File.List;
      Start_Index         : Natural;
      Start_Line          : Natural;
      Start_Column        : Natural;
      End_Index           : Natural;
      Kernel              : access Kernel_Handle_Record'Class;
      File                : File_Descriptor;
      Text                : String;
      File_Name           : VFS.Virtual_File;
      Entity_Line         : Natural;
      Line_In_Body        : in out Natural;
      Source_File_List    : Type_Source_File_Table.HTable;
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
         E_I              : in out Entity_Information;
         Line             : Natural;
         Column           : Natural;
         E_L_I            : in List_Reference_In_File.List_Node;
         Result           : in out Boolean;
         Entity_Abstract  : in out Boolean);
      --  Looks if the reference E_L_I is the same as (Text+Line+Column)
      --  If yes, the declaration of E_L_I is returned and Result is True

      procedure Get_Declaration
        (Text             : String;
         E_I              : in out Entity_Information;
         Line             : Natural;
         Column           : Natural;
         E_L_I            : in List_Reference_In_File.List_Node;
         Result           : in out Boolean;
         Entity_Abstract  : in out Boolean)
      is
         Ref : List_Reference_In_File.Data_Access;
      begin
         Ref := List_Reference_In_File.Data_Ref (E_L_I);

         if Ref.Line = Line
           and then To_Lower (Text) = Get_Name (Ref.Entity)
           and then Ref.Column = Column
         then
            Result := True;
            E_I := Ref.Entity;

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

         --  We search the declaration of the entity
         --  (which is an identifier)

         Result := False;
         Entity_Abstract := False;
         Ref_List_Info := List_Reference_In_File.First (List_Ref_In_File);
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

            if Result then
               List_Reference_In_File.Remove_Nodes
                 (List_Ref_In_File, Ref_List_Info_Prec,
                  Ref_List_Info);
            end if;

            exit when Is_Body;

            --  For body files, no loop because references in the list
            --  are sorted. So the first element met in list is the right
            --  one (for this elements are removed after being met).

            exit when Result;

            Ref_List_Info_Prec := Ref_List_Info;
            Ref_List_Info := List_Reference_In_File.Next (Ref_List_Info);
         end loop;

         --  We create a link on the declaration for this entity
         if Result then
            Docgen_Backend.Format_Link
              (B,
               Start_Index, Start_Line, Start_Column, End_Index,
               Kernel, File, Text, File_Name, Entity_Line,
               Line_In_Body, Source_File_List, Link_All, Is_Body,
               Process_Body, Loc_End, Loc_Start, Entity_Info,
               Entity_Abstract);
         end if;

         if Point_In_Column > 0 then
            Loc_Start := Point_In_Column + 1;
         end if;
      end loop;
   end Format_All_Link;

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
      return Get_Line (Get_Declaration_Of (X.Entity)) <
        Get_Line (Get_Declaration_Of (Y.Entity));
   end Compare_Elements_Line;

   -----------------------------
   -- Compare_Elements_Column --
   -----------------------------

   function Compare_Elements_Column
     (X, Y : Entity_List_Information) return Boolean is
   begin
      return Get_Column (Get_Declaration_Of (X.Entity)) <
        Get_Column (Get_Declaration_Of (Y.Entity));
   end Compare_Elements_Column;

   -----------------------------
   -- Compare_Elements_Column --
   -----------------------------

   function Compare_Elements_Column
     (X, Y : Reference_List_Information) return Boolean is
   begin
      return Get_Column (Get_Declaration_Of (X.Entity)) <
        Get_Column (Get_Declaration_Of (Y.Entity));
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
      Data : Type_Reference_List.Data_Access;

   begin
      if not Type_Reference_List.Is_Empty (List_In) then
         Node := Type_Reference_List.First (List_In);

         while Node /= Type_Reference_List.Null_Node loop
            Data := Type_Reference_List.Data_Ref (Node);
            Ref (Data.Entity);
            Type_Reference_List.Append
              (List_Out, (Data.Entity, Data.Set_Link));
            Node := Type_Reference_List.Next (Node);
         end loop;
      end if;
   end Duplicate;

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
      Free (X.Doc_File_Name);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Entity_List_Information) is
   begin
      Unref (X.Entity);
      if X.Public_Declaration /= null then
         Unref (X.Public_Declaration);
      end if;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Reference_List_Information) is
   begin
      Unref (X.Entity);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Reference_In_File) is
      pragma Unreferenced (X);
   begin
      null;
      --  Memory accessed by the field Entity is free separately: those
      --  records are saved in a list. When we destroy this list, it calls
      --  subprogram Free (Entity_Information).
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Entity_Information) is
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
      Doc_Suffix      : String) return String
   is
      Ext : constant String := File_Extension (Source_Filename);
   begin
      return Base_Name (Source_Filename, Ext)
        & "_"
        & Ext (Ext'First + 1 .. Ext'Last)
        & Doc_Suffix;
   end Get_Doc_File_Name;

   -----------
   -- Clone --
   -----------

   function Clone
     (Entity     : Entity_List_Information) return Entity_List_Information is
   begin
      if Entity.Public_Declaration /= null then
         Ref (Entity.Public_Declaration);
      end if;

      Ref (Entity.Entity);
      return
        (Kind               => Entity.Kind,
         Entity             => Entity.Entity,
         Is_Private         => Entity.Is_Private,
         Line_In_Body       => Entity.Line_In_Body,
         Public_Declaration => Entity.Public_Declaration);
   end Clone;

   ----------
   -- Hash --
   ----------

   function Hash (Key : Entities.Source_File) return HTable_Header is
   begin
      return String_Hash (Full_Name (Get_Filename (Key)).all);
   end Hash;

   -------------------------
   -- Source_File_In_List --
   -------------------------

   function Source_File_In_List
     (Source_File_List : Type_Source_File_Table.HTable;
      File             : Source_File) return Boolean is
   begin
      return Type_Source_File_Table.Get (Source_File_List, File) /=
        No_Source_File_Information;
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

   function Is_Spec_File
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File   : VFS.Virtual_File_Access) return Boolean is
   begin
      return Is_Spec_File (Kernel, File.all);
   end Is_Spec_File;

end Docgen;
