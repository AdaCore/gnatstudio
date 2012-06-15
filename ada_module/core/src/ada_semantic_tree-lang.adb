------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2006-2012, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Characters.Handling; use Ada.Characters.Handling;

with GNAT.Strings;

with Glib.Convert;            use Glib.Convert;

with Diffing;
with Language.Ada;                    use Language.Ada;
with Ada_Semantic_Tree.Parts;         use Ada_Semantic_Tree.Parts;
with Ada_Semantic_Tree.Declarations;  use Ada_Semantic_Tree.Declarations;
with Ada_Semantic_Tree.Generics;      use Ada_Semantic_Tree.Generics;
with Ada_Semantic_Tree.List_Resolver; use Ada_Semantic_Tree.List_Resolver;

with String_Utils;            use String_Utils;

package body Ada_Semantic_Tree.Lang is

   Ada_Assistant_Id : constant String := "ADA_ASSISTANT";

   type Ada_Assistant is new Database_Assistant with record
      Ada_Ref_Key : Construct_Annotations_Pckg.Annotation_Key;
   end record;

   use type GNAT.Strings.String_Access;

   ------------------
   -- Is_Enum_Type --
   ------------------

   function Is_Enum_Type
     (Tree : Construct_Tree;
      It   : Construct_Tree_Iterator) return Boolean
   is
   begin
      return Get_Construct (It).Category = Cat_Type
        and then Get_Construct
          (Next (Tree, It, Jump_Into)).Category = Cat_Literal;
   end Is_Enum_Type;

   ------------------
   -- Get_Language --
   ------------------

   overriding function Get_Language
     (Tree : access Ada_Tree_Language) return Language_Access
   is
      pragma Unreferenced (Tree);
   begin
      return Ada_Lang;
   end Get_Language;

   --------------------
   -- Get_Name_Index --
   --------------------

   overriding function Get_Name_Index
     (Lang      : access Ada_Tree_Language;
      Construct : Simple_Construct_Information) return GNATCOLL.Symbols.Symbol
   is
   begin
      if Construct.Name = No_Symbol then
         return No_Symbol;
      end if;

      if Construct.Category = Cat_Package
        or else Construct.Category = Cat_Procedure
        or else Construct.Category = Cat_Function
      then
         --  If the construct may be a unit name, then we want to store only it
         --  last item (e.g. in Pckg.Child, we store only Child in the db).

         declare
            Id : constant Composite_Identifier :=
                   To_Composite_Identifier (Get (Construct.Name).all);
         begin
            return Get_Language (Lang).Symbols.Find
              (To_Lower (Get_Item (Id, Length (Id))));
         end;
      else
         --  ??? Inefficient, we should compute and keep the lower-cased
         --  version somewhere if we really needed it

         return Get_Language (Lang).Symbols.Find
           (To_Lower (Get (Construct.Name).all));
      end if;
   end Get_Name_Index;

   -----------------
   -- Get_Profile --
   -----------------

   overriding function Get_Profile
     (Lang       : access Ada_Tree_Language;
      Entity     : Entity_Access;
      Raw_Format : Boolean := False) return String
   is
      Tree                 : constant Construct_Tree :=
                               Get_Tree (Get_File (Entity));
      Buffer               : constant GNAT.Strings.String_Access :=
                               Get_Buffer (Get_File (Entity));
      Node                 : constant Construct_Tree_Iterator :=
                               To_Construct_Tree_Iterator (Entity);

      Result               : Unbounded_String;
      Type_Start, Type_End : Source_Location;
      Success              : Boolean;
      Language             : constant Language_Access :=
                               Get_Language
                                 (Tree_Language'Class (Lang.all)'Access);

      Longest_Decoration : Integer := 0;

      procedure Append_Attribute_Decoration
        (Construct  : Simple_Construct_Information;
         Default_In : Boolean);
      --  Append to result the extra qualifiers for the parameter (its mode,
      --  whether it is constant,...). Blanks will be added to use at least
      --  Longest_Decoration characters.

      procedure Append_Default_Value
        (Construct  : Simple_Construct_Information;
         Max_Length : Integer := 30);
      --  Append the default initialization expression to Result.
      --  At most Max_Length characters of the default expression are appended.

      function Remove_Blanks (Str : String) return String;
      --  Return a string will all blanks characters removed (including tabs &
      --  end of line marks)

      ---------------------------------
      -- Append_Attribute_Decoration --
      ---------------------------------

      procedure Append_Attribute_Decoration
        (Construct  : Simple_Construct_Information;
         Default_In : Boolean)
      is
         Len : Natural := 0;
      begin
         if Construct.Attributes (Ada_In_Attribute)
           or else
             (Default_In and then not
                  (Construct.Attributes (Ada_Out_Attribute)
                   or else Construct.Attributes (Ada_Access_Attribute)))
         then
            Append (Result, "in ");
            Len := Len + 3;
         end if;

         if Construct.Attributes (Ada_Out_Attribute) then
            Append (Result, "out ");
            Len := Len + 4;
         end if;

         if Construct.Attributes (Ada_Not_Attribute) then
            Append (Result, "not ");
            Len := Len + 4;
         end if;

         if Construct.Attributes (Ada_Null_Attribute) then
            Append (Result, "null ");
            Len := Len + 5;
         end if;

         if Construct.Attributes (Ada_Access_Attribute) then
            Append (Result, "access ");
            Len := Len + 7;
         end if;

         if Construct.Attributes (Ada_Constant_Attribute) then
            Append (Result, "constant ");
            Len := Len + 9;
         end if;

         if Construct.Attributes (Ada_Aliased_Attribute) then
            Append (Result, "aliased ");
            Len := Len + 8;
         end if;

         Append (Result, (1 .. Longest_Decoration - Len => ' '));
      end Append_Attribute_Decoration;

      --------------------------
      -- Append_Default_Value --
      --------------------------

      procedure Append_Default_Value
        (Construct  : Simple_Construct_Information;
         Max_Length : Integer := 30)
      is
         Length        : Natural := 0;
         Extract_Value : Boolean := False;
         Parent_Depth  : Integer := 0;

         procedure Append_Text (Str : String);
         --  Add text in the result, and return True if there is no more space
         --  left in the buffer.

         function Token_Callback
           (Entity         : Language_Entity;
            Sloc_Start     : Source_Location;
            Sloc_End       : Source_Location;
            Partial_Entity : Boolean) return Boolean;

         -----------------
         -- Append_Text --
         -----------------

         procedure Append_Text (Str : String) is
         begin
            if Length > Max_Length then
               null;

            elsif Length + Str'Length > Max_Length then
               Append
                 (Result,
                  Str (Str'First .. Str'First - 4 +
                      Integer'Min (Str'Length, Max_Length - Length)));
               Append (Result, "...");

            else
               Append
                 (Result,
                  Str (Str'First .. Str'First - 1 +
                      Integer'Min (Str'Length, Max_Length - Length)));
            end if;

            Length := Length + Str'Length;
         end Append_Text;

         --------------------
         -- Token_Callback --
         --------------------

         function Token_Callback
           (Entity         : Language_Entity;
            Sloc_Start     : Source_Location;
            Sloc_End       : Source_Location;
            Partial_Entity : Boolean) return Boolean
         is
            pragma Unreferenced (Partial_Entity);

            Text : constant String :=
                     Buffer (Sloc_Start.Index .. Sloc_End.Index);
         begin
            if Entity = Operator_Text and then Text = ";" then
               return True;
            end if;

            if not Extract_Value  then
               if Entity = Operator_Text and then Text = ":=" then
                  Extract_Value := True;
               end if;
               return False;

            else
               if Entity = Operator_Text then
                  if Text = "(" then
                     Parent_Depth := Parent_Depth + 1;
                     Append_Text (" ");

                  elsif Text = ")" or else Text = "," then
                     if Text = ")" then
                        if Parent_Depth = 0 then
                           return True;
                        end if;
                        Parent_Depth := Parent_Depth - 1;
                     end if;
                  else
                     Append_Text (" ");
                  end if;
               else
                  Append_Text (" ");
               end if;

               if Raw_Format then
                  Append_Text (Text);
               else
                  Append_Text (Glib.Convert.Escape_Text (Text));
               end if;

               return Length > Max_Length;
            end if;
         end Token_Callback;

      begin
         Parse_Entities
           (Ada_Lang,
            Buffer (Construct.Sloc_Entity.Index .. Buffer'Last),
            Token_Callback'Unrestricted_Access);
      end Append_Default_Value;

      -------------------------
      -- Remove_Extra_Blanks --
      -------------------------

      function Remove_Blanks (Str : String) return String is
         Result : String := Str;
         Index  : Integer := Str'First - 1;
      begin
         for J in Result'Range loop
            if not Is_Blank (Result (J)) then
               Index := Index + 1;
               Result (Index) := Result (J);
            end if;
         end loop;

         return Result (Str'First .. Index);
      end Remove_Blanks;

      Has_Parameter : Boolean := False;

   begin
      if Get_Construct (Node).Category in Subprogram_Category then
         declare
            Sub_Iter : Construct_Tree_Iterator := Next (Tree, Node, Jump_Into);
            Longest_Param                : Integer := 0;
            Biggest_Affected_Type_Length : Integer := 0;
            Current_Affected_Type_Length : Integer := 0;
         begin
            if not Raw_Format then
               while Is_Parent_Scope (Node, Sub_Iter) loop
                  if Get_Construct (Sub_Iter).Category = Cat_Parameter then
                     Longest_Param :=
                       Integer'Max
                         (Longest_Param,
                          Get (Get_Construct (Sub_Iter).Name)'Length);

                     Result := Null_Unbounded_String;
                     Append_Attribute_Decoration
                       (Get_Construct (Sub_Iter).all, True);
                     Longest_Decoration :=
                       Integer'Max (Longest_Decoration, Length (Result));

                     if Get_Construct (Sub_Iter).Attributes
                       (Ada_Assign_Attribute)
                     then
                        Get_Referenced_Entity
                          (Language,
                           Buffer.all,
                           Get_Construct (Sub_Iter).all,
                           Type_Start,
                           Type_End,
                           Success);

                        Current_Affected_Type_Length :=
                          Type_End.Index - Type_Start.Index + 1;

                        if Get_Construct (Sub_Iter).Attributes
                          (Ada_Class_Attribute)
                        then
                           Current_Affected_Type_Length :=
                             Current_Affected_Type_Length + 6;
                           --  Addition of the 'Class attribute to the label
                        end if;

                        if Success
                          and then Current_Affected_Type_Length
                            > Biggest_Affected_Type_Length
                        then
                           Biggest_Affected_Type_Length :=
                             Current_Affected_Type_Length;
                        end if;
                     end if;
                  end if;

                  Sub_Iter := Next (Tree, Sub_Iter, Jump_Over);
               end loop;

               Sub_Iter := Next (Tree, Node, Jump_Into);
            end if;

            Result := Null_Unbounded_String;

            while Is_Parent_Scope (Node, Sub_Iter) loop
               if Get_Construct (Sub_Iter).Category = Cat_Parameter then
                  if not Has_Parameter then
                     Has_Parameter := True;

                     if Raw_Format then
                        Append (Result, "(");
                     else
                        Append (Result, "<b>Parameters:</b>" & ASCII.LF & " ");
                     end if;

                  else
                     if Raw_Format then
                        Append (Result, "; ");
                     else
                        Append (Result, ASCII.LF & " ");
                     end if;
                  end if;

                  Get_Referenced_Entity
                    (Language,
                     Buffer.all,
                     Get_Construct (Sub_Iter).all,
                     Type_Start,
                     Type_End,
                     Success);

                  if Get_Construct (Sub_Iter).Attributes
                    (Ada_Assign_Attribute)
                  then
                     if not Raw_Format then
                        Append (Result, "<span foreground=""#555555"">[");
                     end if;
                  end if;

                  Current_Affected_Type_Length :=
                    Type_End.Index - Type_Start.Index + 1;

                  declare
                     Name : constant String :=
                       Escape_Text (Get (Get_Construct (Sub_Iter).Name).all);
                  begin
                     Append (Result, Name);

                     if not Raw_Format then
                        Append (Result,
                                (1 .. Longest_Param - Name'Length => ' '));
                     end if;
                  end;

                  if Success then
                     if Raw_Format then
                        Append (Result, " : ");
                        Append_Attribute_Decoration
                          (Get_Construct (Sub_Iter).all, True);
                     else
                        Append (Result, " : <b>");
                        Append_Attribute_Decoration
                          (Get_Construct (Sub_Iter).all, True);
                        Append (Result, "</b>");
                     end if;

                     Append
                       (Result,
                        Remove_Blanks
                          (Escape_Text
                             (Buffer (Type_Start.Index .. Type_End.Index))));

                     if Get_Construct (Sub_Iter).Attributes
                       (Ada_Class_Attribute)
                     then
                        Append (Result, "'Class");
                        Current_Affected_Type_Length :=
                          Current_Affected_Type_Length + 6;
                     end if;

                  else
                     Append (Result, " : ???");
                  end if;

                  if Get_Construct (Sub_Iter).Attributes
                    (Ada_Assign_Attribute)
                  then
                     Append
                       (Result,
                        (1 .. Biggest_Affected_Type_Length -
                           Current_Affected_Type_Length => ' '));

                     Append (Result, " :=");
                     Append_Default_Value (Get_Construct (Sub_Iter).all);

                     if not Raw_Format then
                        Append (Result, "]</span>");
                     end if;
                  end if;
               end if;

               Sub_Iter := Next (Tree, Sub_Iter, Jump_Over);
            end loop;
         end;

         if Has_Parameter then
            if Raw_Format then
               Append (Result, ")");
            else
               Append (Result, ASCII.LF);
            end if;
         end if;

         Get_Referenced_Entity
           (Language,
            Buffer.all,
            Get_Construct (Node).all,
            Type_Start,
            Type_End,
            Success);

         if Success then
            if Raw_Format then
               Append (Result, " return ");
            else
               Append (Result, "<b>Return:</b>" & ASCII.LF & " <b>");
            end if;

            Longest_Decoration := 0;
            Append_Attribute_Decoration (Get_Construct (Node).all, False);

            if not Raw_Format then
               Append (Result, "</b>");
            end if;

            Append
              (Result,
               Escape_Text (Buffer (Type_Start.Index .. Type_End.Index)));

            if Get_Construct (Node).Attributes (Ada_Class_Attribute) then
               Append (Result, "'Class");
            end if;
         end if;

      elsif Get_Construct (Node).Category in Data_Category then
         declare
            Var_Start, Var_End : Source_Location;
         begin
            Get_Referenced_Entity
              (Language,
               Buffer.all,
               Get_Construct (Node).all,
               Var_Start,
               Var_End,
               Success);

            if Success then
               if Raw_Format then
                  Append (Result, " ");
               else
                  Append (Result, "<b>Type: ");
               end if;

               Append_Attribute_Decoration (Get_Construct (Node).all, False);

               if not Raw_Format then
                  Append (Result, "</b>");
               end if;

               Append
                 (Result,
                  Remove_Blanks
                    (Escape_Text (Buffer (Var_Start.Index .. Var_End.Index))));

               if Get_Construct (Node).Attributes (Ada_Class_Attribute) then
                  Append (Result, "'Class");
               end if;
            end if;
         end;
      end if;

      return To_String (Result);
   end Get_Profile;

   ----------
   -- Diff --
   ----------

   overriding procedure Diff
     (Lang               : access Ada_Tree_Language;
      Old_Tree, New_Tree : Construct_Tree;
      Callback           : Diff_Callback)
   is
      pragma Unreferenced (Lang);

      type Container is record
         Tree        : Construct_Tree;
         First, Last : Construct_Tree_Iterator;
      end record;

      type Iterator is record
         It          : Construct_Tree_Iterator;
         First, Last : Integer;
         Tree        : Construct_Tree;
      end record;

      function Smart_Equal
        (I_1, I_2 : Construct_Tree_Iterator) return Boolean;
      function Length (C : Container) return Integer;
      function First (C : Container) return Iterator;
      function Last (C : Container) return Iterator;
      function Next (I : Iterator) return Iterator;
      function Prev (I : Iterator) return Iterator;
      function At_End (I : Iterator) return Boolean;
      function Get (I : Iterator) return Construct_Tree_Iterator;

      -----------------
      -- Smart_Equal --
      -----------------

      function Smart_Equal
        (I_1, I_2 : Construct_Tree_Iterator) return Boolean
      is
         Construct_1 : constant access Simple_Construct_Information :=
                         Get_Construct (I_1);
         Construct_2 : constant access Simple_Construct_Information :=
                         Get_Construct (I_2);
      begin
         if Construct_1.Category /= Construct_2.Category
           or else Construct_1.Is_Declaration /= Construct_2.Is_Declaration
           or else Get_Identifier (I_1) /= Get_Identifier (I_2)
         then
            return False;
         end if;

         case Construct_1.Category is
            when Cat_Function | Cat_Procedure | Cat_Entry =>
               return Same_Profile
                 (Left_Tree    => Old_Tree,
                  Left_Sb      => I_1,
                  Right_Tree   => New_Tree,
                  Right_Sb     => I_2);

            when others =>
               return True;
         end case;
      end Smart_Equal;

      ------------
      -- Length --
      ------------

      function Length (C : Container) return Integer is
         It : Construct_Tree_Iterator := C.First;
         L  : Integer := 0;
      begin
         while It /= Null_Construct_Tree_Iterator and then It <= C.Last loop
            L := L + 1;

            It := Next (C.Tree, It, Jump_Over);
         end loop;

         return L;
      end Length;

      -----------
      -- First --
      -----------

      function First (C : Container) return Iterator is
         Result : Iterator;
      begin
         Result.It := C.First;
         Result.First := Get_Index (C.First);
         Result.Last := Get_Index (C.Last);
         Result.Tree := C.Tree;

         return Result;
      end First;

      ----------
      -- Last --
      ----------

      function Last (C : Container) return Iterator is
         Result : Iterator;
      begin
         Result.It := C.Last;
         Result.First := Get_Index (C.First);
         Result.Last := Get_Index (C.Last);
         Result.Tree := C.Tree;

         return Result;
      end Last;

      ----------
      -- Next --
      ----------

      function Next (I : Iterator) return Iterator is
         Result : Iterator := I;
      begin
         Result.It := Next (I.Tree, I.It, Jump_Over);

         return Result;
      end Next;

      ----------
      -- Prev --
      ----------

      function Prev (I : Iterator) return Iterator is
         Result : Iterator := I;
      begin
         Result.It := Prev (I.Tree, I.It, Jump_Over);

         return Result;
      end Prev;

      ------------
      -- At_End --
      ------------

      function At_End (I : Iterator) return Boolean is
      begin
         return I.It = Null_Construct_Tree_Iterator
           or else Get_Index (I.It) < I.First
           or else Get_Index (I.It) > I.Last;
      end At_End;

      ---------
      -- Get --
      ---------

      function Get (I : Iterator) return Construct_Tree_Iterator is
      begin
         return I.It;
      end Get;

      package Tree_Diff is new Diffing
        (Object      => Construct_Tree_Iterator,
         Container   => Container,
         Iterator    => Iterator,
         Null_Object => Null_Construct_Tree_Iterator,
         "="         => Smart_Equal,
         Length      => Length,
         First       => First,
         Last        => Last,
         Next        => Next,
         Prev        => Prev,
         At_End      => At_End,
         Get         => Get);

      use Tree_Diff;

      procedure Tree_Callback
        (Old_Obj, New_Obj : Construct_Tree_Iterator;
         State            : Diff_State);

      function To_Container
        (Tree : Construct_Tree; Scope : Construct_Tree_Iterator)
         return Container;

      ------------------
      -- To_Container --
      ------------------

      function To_Container
        (Tree : Construct_Tree; Scope : Construct_Tree_Iterator)
         return Container
      is
         Cont    : Container;
         Next_It : Construct_Tree_Iterator;
      begin
         Cont.Tree := Tree;

         if Scope = Null_Construct_Tree_Iterator then
            Cont.First := First (Tree);
         else
            Cont.First := Next (Tree, Scope, Jump_Into);
         end if;

         if not Is_Parent_Scope (Scope, Cont.First) then
            Cont.First := Null_Construct_Tree_Iterator;
            Cont.Last := Null_Construct_Tree_Iterator;

            return Cont;
         end if;

         Cont.Last := Cont.First;

         Next_It := Next (Tree, Cont.Last, Jump_Over);

         while Next_It /= Null_Construct_Tree_Iterator
           and then Is_Parent_Scope (Scope, Next_It)
         loop
            Cont.Last := Next_It;
            Next_It := Next (Tree, Cont.Last, Jump_Over);
         end loop;

         return Cont;
      end To_Container;

      -------------------
      -- Tree_Callback --
      -------------------

      procedure Tree_Callback
        (Old_Obj, New_Obj : Construct_Tree_Iterator;
         State            : Diff_State)
      is
         It     : Construct_Tree_Iterator;
         It_End : Construct_Tree_Iterator;
      begin
         case State is
            when Equal =>
               Callback (Old_Obj, New_Obj, Preserved);

               Tree_Diff.Diff
                 (To_Container (Old_Tree, Old_Obj),
                  To_Container (New_Tree, New_Obj),
                  Tree_Callback'Access);

            when Added =>
               Callback (Old_Obj, New_Obj, Added);

               It := Next (New_Tree, New_Obj, Jump_Into);
               It_End := Next (New_Tree, New_Obj, Jump_Over);

               while It /= It_End loop
                  Callback (Null_Construct_Tree_Iterator, It, Added);

                  It := Next (New_Tree, It, Jump_Into);
               end loop;

            when Removed =>
               Callback (Old_Obj, New_Obj, Removed);

               It := Next (Old_Tree, Old_Obj, Jump_Into);
               It_End := Next (Old_Tree, Old_Obj, Jump_Over);

               while It /= It_End loop
                  Callback (It, Null_Construct_Tree_Iterator, Removed);

                  It := Next (Old_Tree, It, Jump_Into);
               end loop;

         end case;
      end Tree_Callback;

   begin
      Tree_Diff.Diff
        (To_Container (Old_Tree, Null_Construct_Tree_Iterator),
         To_Container (New_Tree, Null_Construct_Tree_Iterator),
         Tree_Callback'Access);
   end Diff;

   ---------------------
   -- Get_Declaration --
   ---------------------

   overriding function Get_Declaration
     (Lang   : access Ada_Tree_Language;
      Entity : Entity_Access) return Entity_Access
   is
      pragma Unreferenced (Lang);
   begin
      return Get_First_Occurence (Entity);
   end Get_Declaration;

   ----------------------------
   -- Find_Reference_Details --
   ----------------------------

   overriding function Find_Reference_Details
     (Lang     : access Ada_Tree_Language;
      File     : Structured_File_Access;
      Index    : String_Index_Type) return Entity_Reference_Details
   is
      pragma Unreferenced (Lang);
      Str    : constant GNAT.Strings.String_Access := Get_Buffer (File);
      Result : Entity_Reference_Details := Invalid_Reference;

      function Callback
        (Entity         : Language_Entity;
         Sloc_Start     : Source_Location;
         Sloc_End       : Source_Location;
         Partial_Entity : Boolean) return Boolean;

      function Callback
        (Entity         : Language_Entity;
         Sloc_Start     : Source_Location;
         Sloc_End       : Source_Location;
         Partial_Entity : Boolean) return Boolean
      is
         pragma Unreferenced (Partial_Entity);
      begin
         if Result.Index_Start = 0 then
            Result.Index_Start := String_Index_Type (Sloc_Start.Index);
            Result.Index_End := String_Index_Type (Sloc_End.Index);

            --  In case of a composite name, we're actually interrested by the
            --  first element fed to the analysis.

            for J in Sloc_Start.Index .. Sloc_End.Index loop
               if Str (J) = '.' then
                  Result.Index_End := String_Index_Type (J - 1);

                  exit;
               end if;
            end loop;

            return False;
         else
            case Entity is
               when  Annotated_Keyword_Text
                  | Comment_Text
                  | Annotated_Comment_Text =>

                  return False;

               when others =>
                  if Str (Sloc_Start.Index .. Sloc_End.Index) = "=>" then
                     Result.Is_Named_Parameter := True;
                  elsif Str (Sloc_Start.Index .. Sloc_End.Index) = "(" then
                     Result.Parenthesis_Loc :=
                       String_Index_Type (Sloc_Start.Index);
                  end if;

                  return True;

            end case;
         end if;
      end Callback;
   begin
      Parse_Entities
        (Ada_Lang,
         Str (Integer (Index) .. Str'Last),
         Callback'Unrestricted_Access);

      return Result;
   end Find_Reference_Details;

   ----------------------
   -- Find_Declaration --
   ----------------------

   overriding function Find_Declaration
     (Lang     : access Ada_Tree_Language;
      File     : Structured_File_Access;
      Line     : Integer;
      Column   : String_Index_Type) return Entity_Access
   is
      Ref : Entity_Reference_Details;

      List : Entity_List;

      It                 : Entity_Iterator;
      View               : Entity_View;
      Prev_Matching_View : Entity_View;
      Result             : Entity_Access := Null_Entity_Access;

      Decl_Construct : Construct_Tree_Iterator;

      function Actual_Structure_Matches (E : Entity_Access) return Boolean;
      --  Return true if the structure of the actual parameter (number, and
      --  names if any) match the formal parameters of the entity

      function Actual_Structure_Matches (E : Entity_Access) return Boolean is
         Formal_Profile : constant List_Profile := Get_List_Profile
           (E, Null_Visibility_Context);
         Actual_Call    : Actual_Parameter_Resolver :=
           Get_Actual_Parameter_Resolver (Formal_Profile);
         Success : Boolean := True;
      begin
         if Ref.Parenthesis_Loc /= 0 then
            Append_Actuals
              (Actual_Call,
               Get_Buffer (File),
               Ref.Parenthesis_Loc,
               Success);
         end if;

         if Success and then Is_Complete (Actual_Call) then
            return True;
         else
            return False;
         end if;
      end Actual_Structure_Matches;

   begin
      --  First, check if we're already on a declaration

      Decl_Construct := Get_Iterator_At
        (Tree      => Get_Tree (File),
         Location  => To_Location (Line, Column),
         From_Type => Start_Name);

      if Decl_Construct /= Null_Construct_Tree_Iterator
        and then
          Get_Construct (Decl_Construct).Category
            in Cat_Package .. Cat_Literal
      then
         return Lang.Get_Declaration (To_Entity_Access (File, Decl_Construct));
      end if;

      --  Otherwise, we're on a reference. Launch a use-sensitive search

      Ref := Find_Reference_Details
        (Lang, File, Get_Offset_Of_Line (File, Line) + Column - 1);

      if Ref.Is_Named_Parameter then
         --  If there is an arrow, we assume that we're working on a call and
         --  will resolve the parameter.

         declare
            use Token_List;

            Analyzed_Expression : Parsed_Expression :=
              Parse_Expression_Backward
                (Buffer            => Get_Buffer (File),
                 Start_Offset      => Ref.Index_End);

            Enclosing_Call : Parsed_Expression;

            Call_Line   : Integer;
            Call_Column : Visible_Column_Type;
            Call_Index  : String_Index_Type := 0;
            Call_Entity : Entity_Access;

            Call_Node   : Token_List.List_Node;
         begin
            if First (Analyzed_Expression.Tokens) /= Token_List.Null_Node then
               Call_Node := Token_List.First (Analyzed_Expression.Tokens);

               --  First, look if we're in the pattern A'(Something. In this
               --  case, the Call node is the node before the '.

               while Call_Node /= Token_List.Null_Node loop
                  if Next (Call_Node) /= Token_List.Null_Node
                    and then Token_List.Data
                      (Token_List.Next (Call_Node)).Tok_Type = Tok_Tick
                  then
                     exit;
                  end if;

                  Call_Node := Token_List.Next (Call_Node);
               end loop;

               --  If we didn't find a tick, then look for the previous token.

               if Call_Node = Token_List.Null_Node then
                  Enclosing_Call :=
                    Parse_Expression_Backward
                      (Buffer            => Get_Buffer (File),
                       Start_Offset      => Token_List.Data
                         (Token_List.First
                            (Analyzed_Expression.Tokens)).Token_First - 1);

                  Call_Node := Token_List.First (Enclosing_Call.Tokens);
               end if;
            else
               Call_Node := Token_List.Null_Node;
            end if;

            while Call_Node /= Token_List.Null_Node loop
               if Data (Call_Node).Tok_Type = Tok_Open_Parenthesis
                 or else Data (Call_Node).Tok_Type = Tok_Tick
               then
                  exit;
               end if;

               Call_Index := Data (Call_Node).Token_First;
               Call_Node := Next (Call_Node);
            end loop;

            if Call_Index /= 0 then
               To_Line_Column
                 (File                 => File,
                  Absolute_Byte_Offset => Call_Index,
                  Line                 => Call_Line,
                  Column               => Call_Column);

               Call_Index := To_Line_String_Index
                 (File, Call_Line, Call_Column);

               Call_Entity := Lang.Find_Declaration
                 (File, Call_Line, Call_Index);

               if Call_Entity /= Null_Entity_Access then
                  declare
                     Profile : constant List_Profile :=
                       Get_List_Profile (Call_Entity, Null_Visibility_Context);
                     Formals : constant Entity_Array := Get_Formals (Profile);
                     Looked_Name : constant String :=
                       To_Lower
                         (Get_Name
                              (Analyzed_Expression,
                               Data (Last (Analyzed_Expression.Tokens))));
                     Id : Normalized_Symbol;
                  begin
                     for J in Formals'Range loop
                        Id := Get_Identifier (Formals (J));

                        if Looked_Name = Get (Id).all then
                           return Formals (J);
                        end if;
                     end loop;
                  end;
               end if;
            end if;

            Free (Analyzed_Expression);
            Free (Enclosing_Call);
         end;
      end if;

      List := Find_Declarations
        (Context         =>
           (From_File, Null_Instance_Info, File, Ref.Index_End),
         From_Visibility => (File, Ref.Index_End, Everything, Use_Visible));

      It := First (List);

      while not At_End (It) loop
         View := Get_View (It);

         if Prev_Matching_View /= Null_Entity_View then
            if Get_First_Occurence (Get_Entity (Prev_Matching_View))
              = Get_First_Occurence (Get_Entity (View))
            then
               --  First case, the two results are actually part of the same
               --  entity. This can happen with e.g. partial types. We're fine
               --  in this case.

               Result := Get_First_Occurence (Get_Entity (View));
            else
               --  In this case, there is already a match. If they are both
               --  subprograms, check if we can resolve the ambiguity. In all
               --  other cases, we just exit without a result.

               if Get_Construct (Get_Entity (View)).Category not in
                 Subprogram_Category
                 or else Get_Construct
                   (Get_Entity (Prev_Matching_View)).Category not in
                 Subprogram_Category
               then
                  Free (View);
                  Free (Prev_Matching_View);

                  exit;
               end if;

               if Actual_Structure_Matches (Get_Entity (View)) then
                  if Actual_Structure_Matches
                    (Get_Entity (Prev_Matching_View))
                  then
                     --  The two view match the actual structure given, we
                     --  can't decide which one is OK, so don't offer a result.

                     Free (View);
                     Free (Prev_Matching_View);
                     exit;
                  else
                     Free (Prev_Matching_View);
                     Prev_Matching_View := View;
                  end if;
               else
                  --  If the first view doesn't match, we assume that the
                  --  currently matching one works

                  Free (View);
               end if;
            end if;
         else
            Prev_Matching_View := View;
         end if;

         Next (It);

         if At_End (It) then
            --  In this case, there is no more match to check. Return the last
            --  matching one.

            Result := Get_First_Occurence (Get_Entity (Prev_Matching_View));

            Free (Prev_Matching_View);

            exit;
         end if;
      end loop;

      Free (It);
      Free (List);

      return Result;
   end Find_Declaration;

   ---------------------
   -- Find_First_Part --
   ---------------------

   overriding function Find_First_Part
     (Lang   : access Ada_Tree_Language;
      Entity : Entity_Access) return Entity_Access
   is
      pragma Unreferenced (Lang);
   begin
      return Get_First_Occurence (Entity);
   end Find_First_Part;

   --------------------
   -- Find_Next_Part --
   --------------------

   overriding function Find_Next_Part
     (Lang   : access Ada_Tree_Language;
      Entity : Entity_Access) return Entity_Access
   is
      pragma Unreferenced (Lang);

      First, Second, Third : Entity_Access;
   begin
      First  := Get_First_Occurence (Entity);
      Second := Get_Second_Occurence (Entity);
      Third  := Get_Third_Occurence (Entity);

      if Entity = First then
         if Second /= Null_Entity_Access then
            return Second;
         else
            return First;
         end if;
      elsif Entity = Second then
         if Third /= Null_Entity_Access then
            return Third;
         else
            return First;
         end if;
      else
         return First;
      end if;
   end Find_Next_Part;

   ------------------
   -- Same_Profile --
   ------------------

   function Same_Profile
     (Left_Tree    : Construct_Tree;
      Left_Sb      : Construct_Tree_Iterator;
      Right_Tree   : Construct_Tree;
      Right_Sb     : Construct_Tree_Iterator)
      return Boolean
   is
      Left_Param_It, Right_Param_It : Construct_Tree_Iterator;

      function Test_Relevant_Attributes
        (Left, Right : Construct_Attribute_Map) return Boolean;
      --  Test only attributes that are relevant for parameter profiles

      ------------------------------
      -- Test_Relevant_Attributes --
      ------------------------------

      function Test_Relevant_Attributes
        (Left, Right : Construct_Attribute_Map) return Boolean
      is
      begin
         return Left (Ada_Access_Attribute) = Right (Ada_Access_Attribute)
           and then Left (Ada_In_Attribute) = Right (Ada_In_Attribute)
           and then Left (Ada_Out_Attribute) = Right (Ada_Out_Attribute)
           and then Left (Ada_Class_Attribute) = Right (Ada_Class_Attribute);
      end Test_Relevant_Attributes;

   begin
      Left_Param_It := Next (Left_Tree, Left_Sb, Jump_Into);
      Right_Param_It := Next (Right_Tree, Right_Sb, Jump_Into);

      while Is_Parent_Scope (Left_Sb, Left_Param_It)
        and then Is_Parent_Scope (Right_Sb, Right_Param_It)
        and then Get_Construct (Left_Param_It).Category
        = Cat_Parameter
        and then Get_Construct (Right_Param_It).Category
        = Cat_Parameter
        and then Test_Relevant_Attributes
          (Get_Construct (Left_Param_It).Attributes,
           Get_Construct (Right_Param_It).Attributes)
      loop
         --  Check the type of the two parameters

         if Get_Referenced_Identifiers (Left_Param_It)
           /= Get_Referenced_Identifiers (Right_Param_It)
         then
            return False;
         end if;

         Left_Param_It := Next
           (Left_Tree, Left_Param_It, Jump_Over);
         Right_Param_It := Next
           (Right_Tree, Right_Param_It, Jump_Over);
      end loop;

      if Get_Construct (Left_Sb).Category = Cat_Function then
         --  We assume that the two constructs are the same category, so
         --  we can make the test here.

         if not Test_Relevant_Attributes
           (Get_Construct (Left_Sb).Attributes,
            Get_Construct (Right_Sb).Attributes)
         then
            return False;
         end if;

         if Get_Referenced_Identifiers (Left_Sb)
           /= Get_Referenced_Identifiers (Right_Sb)
         then
            return False;
         end if;
      end if;

      --  If there's still one parameter to be analyzed on one
      --  side or the other, return false, otherwise return true.

      return not
        ((Is_Parent_Scope (Left_Sb, Left_Param_It)
          and then Get_Construct (Left_Param_It).Category
          = Cat_Parameter)
         or else
           (Is_Parent_Scope (Right_Sb, Right_Param_It)
            and then Get_Construct (Right_Param_It).Category
            = Cat_Parameter));
   end Same_Profile;

   -------------------------
   -- Is_Compilation_Unit --
   -------------------------

   function Is_Compilation_Unit
     (It : Construct_Tree_Iterator) return Boolean
   is
      Construct : constant access Simple_Construct_Information :=
                    Get_Construct (It);
   begin
      return Get_Parent_Index (It) = 0
        and then
          (Construct.Category = Cat_Package
           or else Construct.Category = Cat_Procedure
           or else Construct.Category = Cat_Function);
   end Is_Compilation_Unit;

   ----------------------------
   -- Register_Ada_Assistant --
   ----------------------------

   procedure Register_Assistant (Db : Construct_Database_Access) is
      use Construct_Annotations_Pckg;

      Key : Construct_Annotations_Pckg.Annotation_Key;
   begin
      Get_Annotation_Key
        (Get_Construct_Annotation_Key_Registry (Db).all, Key);

      Register_Assistant
        (Db,
         Ada_Assistant_Id,
         new Ada_Assistant'(Database_Assistant with Ada_Ref_Key => Key));
   end Register_Assistant;

   -----------------
   -- Get_Ref_Key --
   -----------------

   function Get_Ref_Key
     (Db : Construct_Database_Access)
      return Construct_Annotations_Pckg.Annotation_Key
   is
      use Construct_Annotations_Pckg;

      Assistant : constant Database_Assistant_Access :=
                    Get_Assistant (Db, Ada_Assistant_Id);
   begin
      return Ada_Assistant (Assistant.all).Ada_Ref_Key;
   end Get_Ref_Key;

   --------------------------
   -- Get_Language_Handler --
   --------------------------

   overriding function Get_Language_From_File
     (Handler           : access Ada_Language_Handler;
      Source_Filename   : GNATCOLL.VFS.Virtual_File;
      From_Project_Only : Boolean := False) return Language_Access
   is
      pragma Unreferenced (Handler, Source_Filename, From_Project_Only);
   begin
      return Ada_Lang;
   end Get_Language_From_File;

   ---------------------------------
   -- Get_Tree_Language_From_File --
   ---------------------------------

   overriding function Get_Tree_Language_From_File
     (Handler           : access Ada_Language_Handler;
      Source_Filename   : GNATCOLL.VFS.Virtual_File;
      From_Project_Only : Boolean := False)
      return Tree_Language_Access
   is
      pragma Unreferenced (Handler, Source_Filename, From_Project_Only);
   begin
      return Ada_Tree_Lang;
   end Get_Tree_Language_From_File;

end Ada_Semantic_Tree.Lang;
