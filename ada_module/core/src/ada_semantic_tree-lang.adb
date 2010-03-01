-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2006-2010, AdaCore                  --
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

with Ada.Strings.Unbounded;   use Ada.Strings;
with Ada.Characters.Handling; use Ada.Characters.Handling;

with GNAT.Strings;

with Glib.Convert;            use Glib.Convert;

with Diffing;
with Language.Ada;                   use Language.Ada;
with Language.Documentation;         use Language.Documentation;
with Ada_Semantic_Tree.Parts;        use Ada_Semantic_Tree.Parts;
with Ada_Semantic_Tree.Declarations; use Ada_Semantic_Tree.Declarations;

with String_Utils;            use String_Utils;

package body Ada_Semantic_Tree.Lang is

   Ada_Assistant_Id : constant String := "ADA_ASSISTANT";

   type Ada_Assistant is new Database_Assistant with record
      Ada_Ref_Key : Construct_Annotations_Pckg.Annotation_Key;
   end record;

   use type GNAT.Strings.String_Access;

   type Doc_Kind is (All_Doc, Profile);

   function Format_Documentation
     (Lang     : access Ada_Tree_Language;
      Entity   : Entity_Access;
      Max_Size : Integer;
      Kind     : Doc_Kind) return String;
   --  Factorization of the documentation information. If Max_Size is -1, then
   --  all the documentation is returned, otherwise only the Max_Size first.

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
      Construct : Simple_Construct_Information) return String
   is
      pragma Unreferenced (Lang);
   begin
      if Construct.Name = null then
         return "";
      end if;

      if Construct.Category = Cat_Package
        or else Construct.Category = Cat_Procedure
        or else Construct.Category = Cat_Function
      then
         --  If the construct may be a unit name, then we want to store only it
         --  last item (e.g. in Pckg.Child, we store only Child in the db).

         declare
            Id : constant Composite_Identifier :=
                   To_Composite_Identifier (Construct.Name.all);
         begin
            return To_Lower (Get_Item (Id, Length (Id)));
         end;
      else
         return To_Lower (Construct.Name.all);
      end if;
   end Get_Name_Index;

   -----------------------
   -- Get_Documentation --
   -----------------------

   overriding function Get_Documentation
     (Lang   : access Ada_Tree_Language;
      Entity : Entity_Access) return String
   is
   begin
      return Format_Documentation (Lang, Entity, -1, All_Doc);
   end Get_Documentation;

   -----------------
   -- Get_Profile --
   -----------------

   overriding function Get_Profile
     (Lang     : access Ada_Tree_Language;
      Entity   : Entity_Access;
      Max_Size : Natural) return String is
   begin
      return Format_Documentation (Lang, Entity, Max_Size, Profile);
   end Get_Profile;

   --------------------------
   -- Format_Documentation --
   --------------------------

   function Format_Documentation
     (Lang     : access Ada_Tree_Language;
      Entity   : Entity_Access;
      Max_Size : Integer;
      Kind     : Doc_Kind) return String
   is
      Tree                 : constant Construct_Tree :=
                               Get_Tree (Get_File (Entity));
      Buffer               : constant GNAT.Strings.String_Access :=
                               Get_Buffer (Get_File (Entity));
      Node                 : constant Construct_Tree_Iterator :=
                               To_Construct_Tree_Iterator (Entity);

      Beginning, Current   : Natural;
      Result               : Unbounded.Unbounded_String;

      Type_Start, Type_End : Source_Location;
      Success              : Boolean;
      Language             : constant Language_Access :=
                               Get_Language
                                 (Tree_Language'Class (Lang.all)'Access);

      Overflow : Boolean := False;

      procedure Append (Str : String);
      --  Add the string to the result. The global Overflow is modified to true
      --  if the addition can't be done because Max_Size would be exceeded.

      function Attribute_Decoration
        (Construct  : Simple_Construct_Information;
         Default_In : Boolean) return String;
      --  Return the decoration of the construct given in parameter, i.e.
      --  parameter mode, constant properties, not null constraints...

      function Get_Default_Value
        (Construct  : Simple_Construct_Information;
         Max_Length : Integer := 30) return String;
      --  Return the default initialization expression for this construct, if
      --  any. Result will be stripped when bigger than Max_Length characters

      function Remove_Blanks (Str : String) return String;
      --  Return a string will all blanks characters removed (including tabs &
      --  end of line marks)

      ------------
      -- Append --
      ------------

      procedure Append (Str : String) is
      begin
         if Max_Size = -1 then
            Unbounded.Append (Result, Str);
         else
            if not Overflow then
               if Unbounded.Length (Result) + Str'Length <= Max_Size - 3 then
                  Unbounded.Append (Result, Str);
               else
                  Overflow := True;
               end if;
            end if;
         end if;
      end Append;

      --------------------------
      -- Attribute_Decoration --
      --------------------------

      function Attribute_Decoration
        (Construct  : Simple_Construct_Information;
         Default_In : Boolean) return String
      is
         Buffer : String (1 .. 30);
         Ind    : Integer := 1;
      begin
         if Construct.Attributes (Ada_In_Attribute)
           or else
             (Default_In and then not
                  (Construct.Attributes (Ada_Out_Attribute)
                   or else Construct.Attributes (Ada_Access_Attribute)))
         then
            Buffer (Ind .. Ind + 2) := "in ";
            Ind := Ind + 3;
         end if;

         if Construct.Attributes (Ada_Out_Attribute) then
            Buffer (Ind .. Ind + 3) := "out ";
            Ind := Ind + 4;
         end if;

         if Construct.Attributes (Ada_Not_Attribute) then
            Buffer (Ind .. Ind + 3) := "not ";
            Ind := Ind + 4;
         end if;

         if Construct.Attributes (Ada_Null_Attribute) then
            Buffer (Ind .. Ind + 4) := "null ";
            Ind := Ind + 5;
         end if;

         if Construct.Attributes (Ada_Access_Attribute) then
            Buffer (Ind .. Ind + 6) := "access ";
            Ind := Ind + 7;
         end if;

         if Construct.Attributes (Ada_Constant_Attribute) then
            Buffer (Ind .. Ind + 8) := "constant ";
            Ind := Ind + 9;
         end if;

         if Construct.Attributes (Ada_Aliased_Attribute) then
            Buffer (Ind .. Ind + 7) := "aliased ";
            Ind := Ind + 8;
         end if;

         return Buffer (1 .. Ind - 1);
      end Attribute_Decoration;

      ------------------------
      --  Get_Default_Value --
      ------------------------

      function Get_Default_Value
        (Construct  : Simple_Construct_Information;
         Max_Length : Integer := 30) return String
      is
         Result        : String (1 .. Max_Length);
         Current_Ind   : Integer := 0;
         Extract_Value : Boolean := False;
         Parent_Depth  : Integer := 0;

         function Append_Text (Str : String) return Boolean;
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

         function Append_Text (Str : String) return Boolean is
            Size_Taken : Integer := Str'Length;
            Stop       : Boolean := False;
         begin
            if Size_Taken + Current_Ind > Result'Length - 3 then
               Size_Taken := Result'Length - 3 - Current_Ind;
               Stop := True;
            end if;

            Result (Current_Ind + 1 .. Current_Ind + 1 + Size_Taken - 1) :=
              Str (Str'First .. Str'First + Size_Taken - 1);

            Current_Ind := Current_Ind + Size_Taken;

            if Stop then
               Result (Result'Last - 2 .. Result'Last) := "...";
               Current_Ind := Result'Last;
               return True;
            else
               return False;
            end if;
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

                     return Append_Text (" (");

                  elsif Text = ")" or else Text = "," then
                     if Text = ")" then
                        if Parent_Depth = 0 then
                           return True;
                        end if;

                        Parent_Depth := Parent_Depth - 1;
                     end if;

                     return Append_Text (Text);

                  else
                     return Append_Text (" " & Text);
                  end if;
               end if;

               return Append_Text (" " & Text);
            end if;
         end Token_Callback;

      begin
         Parse_Entities
           (Ada_Lang, Buffer (Construct.Sloc_Entity.Index .. Buffer'Last),
            Token_Callback'Unrestricted_Access);

         return Result (1 .. Current_Ind);
      end Get_Default_Value;

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

      Add_New_Line  : Boolean := False;
      Has_Parameter : Boolean := False;

   begin
      if Kind = All_Doc then
         Get_Documentation_Before
           (Context       => Get_Language_Context (Language).all,
            Buffer        => Buffer.all,
            Decl_Index    => Get_Construct (Node).Sloc_Start.Index,
            Comment_Start => Beginning,
            Comment_End   => Current);

         if Beginning = 0 then
            Get_Documentation_After
              (Context       => Get_Language_Context (Language).all,
               Buffer        => Buffer.all,
               Decl_Index    => Get_Construct (Node).Sloc_End.Index,
               Comment_Start => Beginning,
               Comment_End   => Current);
         end if;

         if Beginning /= 0 then
            Append
              (Escape_Text
                 (Comment_Block
                    (Language,
                     Buffer (Beginning .. Current),
                     Comment => False,
                     Clean   => True)));

            Add_New_Line := True;
         end if;
      end if;

      if Get_Construct (Node).Category in Subprogram_Category then
         declare
            Sub_Iter                     : Construct_Tree_Iterator :=
                                             Next (Tree, Node, Jump_Into);
            Biggest_Parameter_Name       : Integer := 0;
            Biggest_Decoration_Length    : Integer := 0;
            Biggest_Affected_Type_Length : Integer := 0;
            Current_Affected_Type_Length : Integer := 0;
         begin
            while Is_Parent_Scope (Node, Sub_Iter) loop
               if Get_Construct (Sub_Iter).Category = Cat_Parameter then
                  if Get_Construct (Sub_Iter).Name'Length >
                    Biggest_Parameter_Name
                  then
                     Biggest_Parameter_Name :=
                       Get_Construct (Sub_Iter).Name'Length;
                  end if;

                  if Attribute_Decoration
                    (Get_Construct (Sub_Iter).all, True)'Length
                    > Biggest_Decoration_Length
                  then
                     Biggest_Decoration_Length :=
                       Attribute_Decoration
                         (Get_Construct (Sub_Iter).all, True)'Length;
                  end if;

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

            while Is_Parent_Scope (Node, Sub_Iter) loop
               if Get_Construct (Sub_Iter).Category = Cat_Parameter then
                  if not Has_Parameter then
                     if Add_New_Line then
                        Append (ASCII.LF & ASCII.LF);
                     end if;

                     if Kind = All_Doc then
                        Append ("<b>Parameters:</b>");
                        Add_New_Line := True;
                     else
                        Append ("(");
                     end if;

                     Has_Parameter := True;
                  elsif Kind = Profile then
                     Append ("; ");
                  end if;

                  if Kind = All_Doc then
                     Append ((1 => ASCII.LF));
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
                     if Kind = All_Doc then
                        Append
                          ("<span foreground=""#555555"">[");
                     end if;
                  else
                     if Kind = All_Doc then
                        Append (" ");
                     end if;
                  end if;

                  Current_Affected_Type_Length :=
                    Type_End.Index - Type_Start.Index + 1;

                  Append
                    (Escape_Text (Get_Construct (Sub_Iter).Name.all));

                  --  ??? These loops are highly inefficient. Consider
                  --  improving these

                  if Kind = All_Doc then
                     for J in Get_Construct (Sub_Iter).Name'Length + 1
                       .. Biggest_Parameter_Name
                     loop
                        Append (" ");
                     end loop;
                  end if;

                  if Success then
                     Append
                       (" : <b>"
                        & Attribute_Decoration
                          (Get_Construct (Sub_Iter).all, True)
                        & "</b>");

                     if Kind = All_Doc then
                        for J in
                          Attribute_Decoration
                            (Get_Construct (Sub_Iter).all, True)'Length + 1
                          .. Biggest_Decoration_Length
                        loop
                           Append (" ");
                        end loop;
                     end if;

                     Append
                       (Remove_Blanks
                          (Escape_Text
                             (Buffer (Type_Start.Index .. Type_End.Index))));

                     if Get_Construct (Sub_Iter).Attributes
                       (Ada_Class_Attribute)
                     then
                        Append ("'Class");
                        Current_Affected_Type_Length :=
                          Current_Affected_Type_Length + 6;
                     end if;

                  else
                     Append (" : ???");
                  end if;

                  if Get_Construct (Sub_Iter).Attributes
                    (Ada_Assign_Attribute)
                  then
                     for J in Current_Affected_Type_Length + 1
                       .. Biggest_Affected_Type_Length
                     loop
                        Append (" ");
                     end loop;

                     Append
                       (" :="
                        & Reduce
                          (Escape_Text
                             (" " & Get_Default_Value
                                (Get_Construct (Sub_Iter).all))) & "]");

                     if Kind = All_Doc then
                        Append
                          ("</span>");
                     end if;
                  end if;
               end if;

               Sub_Iter := Next (Tree, Sub_Iter, Jump_Over);
            end loop;
         end;

         if Has_Parameter and then Kind = Profile then
            Append (")");
         end if;

         Get_Referenced_Entity
           (Language,
            Buffer.all,
            Get_Construct (Node).all,
            Type_Start,
            Type_End,
            Success);

         if Success then
            if Add_New_Line then
               Append (ASCII.LF & ASCII.LF);
            end if;

            if Kind = All_Doc then
               Append
                 ("<b>Return:</b>"
                  & ASCII.LF & " <b>"
                  & Attribute_Decoration (Get_Construct (Node).all, False)
                  & "</b>"
                  & Escape_Text
                    (Buffer (Type_Start.Index .. Type_End.Index)));
            else
               Append
                 (" return"
                  & Attribute_Decoration (Get_Construct (Node).all, False)
                  & " "
                  & Escape_Text
                    (Buffer (Type_Start.Index .. Type_End.Index)));
            end if;

            if Get_Construct (Node).Attributes (Ada_Class_Attribute) then
               Append ("'Class");
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
               if Add_New_Line then
                  Append (ASCII.LF & ASCII.LF);
               end if;

               Append
                 ("<b>Type: "
                  & Attribute_Decoration (Get_Construct (Node).all, False)
                  & "</b>"
                  & Remove_Blanks
                    (Escape_Text (Buffer (Var_Start.Index .. Var_End.Index))));

               if Get_Construct (Node).Attributes (Ada_Class_Attribute) then
                  Append ("'Class");
               end if;
            end if;
         end;
      end if;

      if not Overflow then
         return Unbounded.To_String (Result);
      else
         return Unbounded.To_String (Result) & "...";
      end if;
   end Format_Documentation;

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

   ----------------------
   -- Find_Declaration --
   ----------------------

   function Forward_Expression
     (Str : String; Index : String_Index_Type) return String_Index_Type;

   function Forward_Expression
     (Str : String; Index : String_Index_Type) return String_Index_Type
   is
      Result : String_Index_Type;

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
         pragma Unreferenced (Entity, Partial_Entity);
      begin
         Result := String_Index_Type (Sloc_End.Index);

         --  In case of a composite name, we're actually interrested by the
         --  first element fed to the analysis.

         for J in Sloc_Start.Index .. Sloc_End.Index loop
            if Str (J) = '.' then
               Result := String_Index_Type (J - 1);

               exit;
            end if;
         end loop;

         return True;
      end Callback;
   begin
      Parse_Entities
        (Ada_Lang,
         Str (Integer (Index) .. Str'Last),
         Callback'Unrestricted_Access);

      return Result;
   end Forward_Expression;

   overriding function Find_Declaration
     (Lang     : access Ada_Tree_Language;
      File     : Structured_File_Access;
      Line     : Integer;
      Column   : String_Index_Type) return Entity_Access
   is
      pragma Unreferenced (Lang);

      List : Entity_List := Find_Declarations
        (Context =>
           (From_File,
            File,
            Forward_Expression
              (Get_Buffer (File).all,
               Get_Offset_Of_Line (File, Line) + Column - 1)));

      It     : Entity_Iterator := First (List);
      View   : Entity_View;
      Result : Entity_Access := Null_Entity_Access;
   begin
      if not At_End (It) then
         View := Get_View (It);

         Next (It);

         if At_End (It) then
            --  In this case, there is a unique match. Return it.

            Result := Get_First_Occurence (Get_Entity (View));
         end if;

         Free (View);
      end if;

      Free (It);
      Free (List);

      return Result;
   end Find_Declaration;

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
