------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                     Copyright (C) 1999-2018, AdaCore                     --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

with Ada.Text_IO;

separate (Templates_Parser)

package body Data is

   -----------
   -- Build --
   -----------

   function Build (Str : String) return Tag_Var is

      function Get_Var_Name (Tag : String) return String;
      --  Given a Tag name, it returns the variable name only. It removes
      --  the tag separator and the filters.

      function Get_Filter_Set (Tag : String) return Filter.Set_Access;
      --  Given a tag name, it retruns a set of filter to apply to this
      --  variable when translated.

      function Get_Attribute (Tag : String) return Attribute_Data;
      --  Returns attribute for the given tag

      function Is_Internal (Name : String) return Internal_Tag;
      --  Returns True if Name is an internal tag

      function Is_Macro return Boolean;
      pragma Inline (Is_Macro);
      --  Returns True if we are parsing a macro

      F_Sep : constant Natural :=
                  Strings.Fixed.Index (Str, ":", Strings.Backward);
      --  Last filter separator

      A_Sep : Natural :=
                  Strings.Fixed.Index (Str, "'", Strings.Backward);
      --  Attribute separator

      MP_Start, MP_End : Natural := 0;
      --  Start/End of the macro parameters, 0 if not a macro

      -------------------
      -- Get_Attribute --
      -------------------

      function Get_Attribute (Tag : String) return Attribute_Data is
         Start, Stop : Natural;
      begin
         if A_Sep = 0 then
            return No_Attribute;
         else
            Start := A_Sep + 1;
            Stop  := Tag'Last - Length (End_Tag);
         end if;

         declare
            A_Name : constant String :=
                       Characters.Handling.To_Lower (Tag (Start .. Stop));
         begin
            if A_Name = "length" then
               return (Length, 0);

            elsif A_Name = "line" then
               return (Line, 0);

            elsif A_Name = "min_column" then
               return (Min_Column, 0);

            elsif A_Name = "max_column" then
               return (Max_Column, 0);

            elsif A_Name'Length >= 8
              and then A_Name (A_Name'First .. A_Name'First + 7) = "up_level"
            then
               if A_Name'Length > 8 then
                  --  We have a parameter
                  declare
                     V : constant String :=
                           Strings.Fixed.Trim
                             (A_Name (A_Name'First + 8 .. A_Name'Last),
                              Strings.Both);
                     N : Integer;
                  begin
                     if V (V'First) = '('
                       and then V (V'Last) = ')'
                       and then Is_Number (V (V'First + 1 .. V'Last - 1))
                     then
                        N := Integer'Value (V (V'First + 1 .. V'Last - 1));
                     else
                        raise Template_Error
                          with "Wrong value for attribute Up_Level";
                     end if;
                     return (Up_Level, N);
                  end;
               else
                  return (Up_Level, 1);
               end if;

            else
               raise Template_Error
                 with "Unknown attribute name """ & A_Name & '"';
            end if;
         end;
      end Get_Attribute;

      --------------------
      -- Get_Filter_Set --
      --------------------

      function Get_Filter_Set (Tag : String) return Filter.Set_Access is

         use type Filter.Callback;

         Start : Natural;
         Stop  : Natural := Tag'Last;
         FS    : Filter.Set (1 .. Strings.Fixed.Count (Tag, ":"));
         --  Note that FS can be larger than needed as ':' can be used inside
         --  filter parameters for example.
         K     : Positive := FS'First;

         function Name_Parameter
           (Filter : String) return Templates_Parser.Filter.Routine;
         --  Given a Filter description, returns the filter handle and
         --  parameter.

         procedure Get_Slice (Slice : String; First, Last : out Integer);
         --  Returns the First and Last slice index as parsed into the Slice
         --  string. Returns First and Last set to 0 if there is not valid
         --  slice definition in Slice.

         function Find_Slash (Str : String) return Natural;
         --  Returns the first slash index in Str, skip espaced slashes

         function Find
           (Str   : String;
            Start : Positive;
            C     : Character) return Natural;
         --  Look backward for character C in Str starting at position Start.
         --  This procedure skips quoted strings and parenthesis. Returns 0 if
         --  the character if not found otherwize it returns the positon of C
         --  in Str.

         ----------
         -- Find --
         ----------

         function Find
           (Str   : String;
            Start : Positive;
            C     : Character) return Natural
         is
            Pos   : Natural := Start;
            Count : Integer := 0;
         begin
            while Pos > Str'First
              and then (Str (Pos) /= C or else Count /= 0)
            loop
               if Pos > Str'First and then Str (Pos - 1) /= '\' then
                  --  This is not a quoted character
                  if Str (Pos) = ')' then
                     Count := Count - 1;
                  elsif Str (Pos) = '(' then
                     Count := Count + 1;
                  end if;
               end if;
               Pos := Pos - 1;
            end loop;

            if Pos = Str'First then
               return 0;
            else
               return Pos;
            end if;
         end Find;

         ----------------
         -- Find_Slash --
         ----------------

         function Find_Slash (Str : String) return Natural is
            Escaped : Boolean := False;
         begin
            for K in Str'Range loop
               if Str (K) = '\' then
                  Escaped := not Escaped;

               elsif Str (K) = '/' and then not Escaped then
                  return K;

               else
                  Escaped := False;
               end if;
            end loop;

            return 0;
         end Find_Slash;

         ---------------
         -- Get_Slice --
         ---------------

         procedure Get_Slice (Slice : String; First, Last : out Integer) is
            P1 : constant Natural := Fixed.Index (Slice, "..");
         begin
            First := 0;
            Last  := 0;

            if P1 = 0 then
               raise Template_Error with "slice expected """ & Slice & '"';

            else
               First := Integer'Value (Slice (Slice'First .. P1 - 1));
               Last  := Integer'Value (Slice (P1 + 2 .. Slice'Last));
            end if;
         end Get_Slice;

         --------------------
         -- Name_Parameter --
         --------------------

         function Name_Parameter
           (Filter : String) return Templates_Parser.Filter.Routine
         is
            package F renames Templates_Parser.Filter;

            use type F.Mode;

            function Unescape (Str : String) return String;
            --  Unespace characters Str, to be used with regpat replacement
            --  pattern.

            --------------
            -- Unescape --
            --------------

            function Unescape (Str : String) return String is
               S : String (Str'Range);
               I : Natural  := S'First - 1;
               K : Positive := Str'First;
            begin
               loop
                  exit when K > Str'Last;

                  I := I + 1;

                  if Str (K) = '\'
                    and then K < Str'Last
                    and then not (Str (K + 1) in '0' .. '9')
                  then
                     --  An escaped character, skip the backslash
                     K := K + 1;

                     --  Handle some special escaped characters \n \r \t

                     case Str (K) is
                        when 'n'    => S (I) := ASCII.LF;
                        when 'r'    => S (I) := ASCII.CR;
                        when 't'    => S (I) := ASCII.HT;
                        when others => S (I) := Str (K);
                     end case;

                  else
                     S (I) := Str (K);
                  end if;

                  K := K + 1;
               end loop;

               return S (S'First .. I);
            end Unescape;

            P1 : constant Natural := Fixed.Index (Filter, "(");
            P2 : constant Natural := Fixed.Index (Filter, ")", Backward);

         begin
            if (P1 = 0 and then P2 /= 0) or else (P1 /= 0 and then P2 = 0) then
               raise Template_Error
                 with "unbalanced parenthesis """ & Filter & '"';

            elsif P2 /= 0
              and then P2 < Filter'Last
              and then Filter (P2 + 1) /= ':'
            then
               raise Template_Error with
                 "unexpected character after parenthesis """ & Filter & '"';
            end if;

            if P1 = 0 then
               --  No parenthesis, so there is no parameter to parse

               if F.Mode_Value (Filter) = F.User_Defined then
                  return
                    (F.Handle (Filter),
                     F.Parameter_Data'(Mode    => F.User_Callback,
                                       Handler => F.User_Handle (Filter),
                                       P       => Null_Unbounded_String));
               else
                  return (F.Handle (Filter),
                          Templates_Parser.Filter.No_Parameter);
               end if;

            else
               declare
                  use GNAT.Regpat;
                  Name : constant String := Filter (Filter'First .. P1 - 1);
                  Mode : constant F.Mode := F.Mode_Value (Name);

                  Parameter : constant String :=
                                No_Quote (Filter (P1 + 1 .. P2 - 1));
               begin
                  case F.Parameter (Mode) is
                     when F.Regexp =>
                        return (F.Handle (Mode),
                                F.Parameter_Data'
                                  (F.Regexp,
                                   R_Str  => To_Unbounded_String (Parameter),
                                   Regexp => new Pattern_Matcher'
                                                   (Compile (Parameter))));

                     when F.Regpat =>
                        declare
                           K : constant Natural := Find_Slash (Parameter);
                        begin
                           if K = 0 then
                              --  No replacement, this is equivalent to
                              --  REPLACE(<regexp>/\1)
                              return (F.Handle (Mode),
                                      F.Parameter_Data'
                                        (F.Regpat,
                                         P_Str  => To_Unbounded_String
                                                     (Parameter),
                                         Regpat => new Pattern_Matcher'
                                                         (Compile (Parameter)),
                                         Param => To_Unbounded_String ("\1")));
                           else
                              return (F.Handle (Mode),
                                      F.Parameter_Data'
                                        (F.Regpat,
                                         P_Str => To_Unbounded_String
                                                    (Parameter
                                                       (Parameter'First
                                                        .. K - 1)),
                                         Regpat => new Pattern_Matcher'
                                                     (Compile
                                                        (Parameter
                                                           (Parameter'First
                                                            .. K - 1))),
                                         Param =>
                                           To_Unbounded_String
                                             (Unescape
                                                (Parameter
                                                   (K + 1
                                                    .. Parameter'Last)))));
                           end if;
                        end;

                     when F.Slice =>
                        declare
                           First, Last : Integer;
                        begin
                           Get_Slice (Parameter, First, Last);

                           return (F.Handle (Mode),
                                   F.Parameter_Data'(F.Slice, First, Last));
                        end;

                     when F.Str =>
                        return (F.Handle (Mode),
                                F.Parameter_Data'
                                  (F.Str,
                                   S => To_Unbounded_String (Parameter)));

                     when F.User_Callback =>
                        return (F.Handle (Mode),
                                F.Parameter_Data'
                                  (F.User_Callback,
                                   F.User_Handle (Name),
                                   P => To_Unbounded_String (Parameter)));
                  end case;
               end;
            end if;
         end Name_Parameter;

      begin
         if FS'Length = 0 then
            return null;
         end if;

         loop
            Start := Tag'First;

            Stop := Find (Str, Stop, ':');

            exit when Stop = 0;

            Start := Find (Str, Stop - 1, ':');

            if Start = 0 then
               --  Last filter found
               FS (K) := Name_Parameter
                 (Tag (Tag'First + Length (Begin_Tag) .. Stop - 1));
            else
               FS (K) := Name_Parameter (Tag (Start + 1 .. Stop - 1));
            end if;

            --  Specific check for the NO_DYNAMIC filter which must appear
            --  first.

            if FS (K).Handle = Filter.No_Dynamic'Access
              and then K /= FS'First
            then
               raise Template_Error with "NO_DYNAMIC must be the first filter";
            end if;

            K := K + 1;

            Stop := Stop - 1;
         end loop;

         return new Filter.Set'(FS (FS'First .. K - 1));
      end Get_Filter_Set;

      ------------------
      -- Get_Var_Name --
      ------------------

      function Get_Var_Name (Tag : String) return String is
         Start, Stop : Natural;
      begin
         if A_Sep = 0 then
            --  No attribute
            Stop := Tag'Last - Length (End_Tag);

            --  Check for macro parameters

            if Tag (Stop) = ')' then
               MP_End := Stop;
               --  Go back to matching open parenthesis
               loop
                  Stop := Stop - 1;
                  --  ??? check for string literal
                  exit when Tag (Stop + 1) = '(' or else Stop = Tag'First;
               end loop;
               MP_Start := Stop + 1;
            end if;

         else
            Stop := A_Sep - 1;
         end if;

         if F_Sep = 0 then
            --  No filter
            Start := Tag'First + Length (Begin_Tag);
         else
            Start := F_Sep + 1;
         end if;

         return Tag (Start .. Stop);
      end Get_Var_Name;

      -----------------
      -- Is_Internal --
      -----------------

      function Is_Internal (Name : String) return Internal_Tag is
      begin
         case Name (Name'First) is
            when 'D' =>
               if Name = "DAY" then
                  return Day;
               elsif Name = "DAY_NAME" then
                  return Day_Name;
               else
                  return No;
               end if;

            when 'H' =>
               if Name = "HOUR" then
                  return Hour;
               else
                  return No;
               end if;

            when 'M' =>
               if Name = "MONTH" then
                  return Month;
               elsif Name = "MONTH_NAME" then
                  return Month_Name;
               elsif Name = "MINUTE" then
                  return Minute;
               else
                  return No;
               end if;

            when 'N' =>
               if Name = "NOW" then
                  return Now;
               elsif Name = "NUMBER_LINE" then
                  return Number_Line;
               else
                  return No;
               end if;

            when 'S' =>
               if Name = "SECOND" then
                  return Second;
               else
                  return No;
               end if;

            when 'T' =>
               if Name = "TABLE_LINE" then
                  return Table_Line;
               elsif Name = "TABLE_LEVEL" then
                  return Table_Level;
               else
                  return No;
               end if;

            when 'U' =>
               if Name = "UP_TABLE_LINE" then
                  return Up_Table_Line;
               else
                  return No;
               end if;

            when 'Y' =>
               if Name = "YEAR" then
                  return Year;
               else
                  return No;
               end if;

            when others =>
               return No;
         end case;
      end Is_Internal;

      --------------
      -- Is_Macro --
      --------------

      function Is_Macro return Boolean is
      begin
         return MP_Start /= 0 and MP_End /= 0;
      end Is_Macro;

      Result : Tag_Var;

   begin
      if A_Sep <= F_Sep then
         --  This is not an attribute in fact, but something like:
         --  Filter(that's it):VAR
         A_Sep := 0;
      end if;

      Result.Filters   := Get_Filter_Set (Str);
      Result.Attribute := Get_Attribute (Str);

      declare
         Name : constant String := Get_Var_Name (Str);
      begin
         Result.Name     := To_Unbounded_String (Name);
         Result.Internal := Is_Internal (Name);

         --  If there is no attribute, check for a macro

         if Result.Attribute = No_Attribute and then Is_Macro then
            Result.Is_Macro := True;

            declare
               P : constant Templates_Parser.Parameter_Set :=
                     Get_Parameters (Str (MP_Start .. MP_End));
            begin
               Result.Parameters := To_Data_Parameters (P);
            end;

            --  Check if this is a known macro

            Result.Def := Clone (Macro.Get (Name));

            if Result.Def /= null then
               Macro.Rewrite (Result.Def, Result.Parameters);
            end if;
         end if;

         if Name (Name'First) = '$'
           and then Strings.Fixed.Count
             (Name, Strings.Maps.Constants.Decimal_Digit_Set) = Name'Length - 1
         then
            Result.N := Natural'Value (Name (Name'First + 1 .. Name'Last));
         else
            Result.N := -1;
         end if;
      end;

      return Result;
   end Build;

   -----------
   -- Clone --
   -----------

   function Clone (V : Tag_Var) return Tag_Var is
      use type Filter.Set_Access;
      R : Tag_Var := V;
   begin
      if R.Filters /= null then
         R.Filters := new Filter.Set'(R.Filters.all);
      end if;

      if R.Is_Macro then
         R.Parameters := new Data.Parameter_Set'(R.Parameters.all);

         for K in R.Parameters'Range loop
            if R.Parameters (K) /= null then
               R.Parameters (K) := Data.Clone (R.Parameters (K));
            end if;
         end loop;

         R.Def := Clone (R.Def);
      end if;

      return R;
   end Clone;

   function Clone (D : Tree) return Tree is
      Root, N : Tree;
   begin
      if D /= null then
         Root := new Node'(D.all);
         N := Root;

         loop
            if N.Kind = Data.Var then
               N.Var := Data.Clone (N.Var);
            end if;

            exit when N.Next = null;

            N.Next := new Node'(N.Next.all);
            N := N.Next;
         end loop;
      end if;
      return Root;
   end Clone;

   -----------
   -- Image --
   -----------

   function Image (T : Tag_Var) return String is
      use type Filter.Set_Access;
      R     : Unbounded_String;
      Named : Boolean := False;
   begin
      R := Begin_Tag;

      --  Filters

      if T.Filters /= null then
         for K in reverse T.Filters'Range loop
            Append (R, Filter.Name (T.Filters (K).Handle));
            Append (R, Filter.Image (T.Filters (K).Parameters));
            Append (R, ":");
         end loop;
      end if;

      --  Tag name

      Append (R, T.Name);

      --  Macro parameters if any

      if T.Is_Macro then
         Append (R, "(");

         for K in T.Parameters'Range loop
            if T.Parameters (K) = null then
               Named := True;

            else
               if Named then
                  Append (R, Natural'Image (K) & " => ");
               end if;

               case T.Parameters (K).Kind is
                  when Text => Append (R, T.Parameters (K).Value);
                  when Var  => Append (R, Image (T.Parameters (K).Var));
               end case;

               if K /= T.Parameters'Last then
                  Append (R, ",");
               end if;
            end if;
         end loop;

         Append (R, ")");
      end if;

      --  Attributes

      case T.Attribute.Attr is
         when Nil        => null;
         when Length     => Append (R, "'Length");
         when Line       => Append (R, "'Line");
         when Min_Column => Append (R, "'Min_Column");
         when Max_Column => Append (R, "'Max_Column");
         when Up_Level   =>
            Append (R, "'Up_Level");
            if T.Attribute.Value /= 1 then
               Append (R, '(' & Utils.Image (T.Attribute.Value) & ')');
            end if;
      end case;

      Append (R, End_Tag);

      return To_String (R);
   end Image;

   -------------------------
   -- Is_Include_Variable --
   -------------------------

   function Is_Include_Variable (T : Tag_Var) return Boolean is
   begin
      return T.N /= -1;
   end Is_Include_Variable;

   -----------
   -- Parse --
   -----------

   function Parse (Line : String) return Tree is

      Begin_Tag : constant String := To_String (Templates_Parser.Begin_Tag);
      End_Tag   : constant String := To_String (Templates_Parser.End_Tag);

      function Build (Line : String) return Tree;
      --  Recursive function to build the tree

      -----------
      -- Build --
      -----------

      function Build (Line : String) return Tree is
         Start, Stop, S : Natural;
      begin
         if Line = "" then
            return null;

         else
            Start := Strings.Fixed.Index (Line, Begin_Tag);

            if Start = 0 then
               --  No more tag
               return new Node'(Text,
                                Next  => null,
                                Value => To_Unbounded_String (Line));

            else
               --  Get matching ending separator, a macro can have variables
               --  as parameter:
               --  @_MACRO(2,@_VAR_@)_@

               S := Start + Begin_Tag'Length;

               Search_Matching_Tag : loop
                  Stop := Strings.Fixed.Index (Line, End_Tag, From => S);
                  S := Strings.Fixed.Index (Line, Begin_Tag, From => S);

                  exit Search_Matching_Tag when S = 0 or else S > Stop;

                  S := Stop + End_Tag'Length;
               end loop Search_Matching_Tag;

               if Stop = 0 then
                  raise Internal_Error with
                    "Tag variable not terminated (missing " & End_Tag & ")";

               else
                  Stop := Stop + End_Tag'Length - 1;

                  if Start = Line'First then
                     --  The first token in Line is a variable
                     return new Node'
                       (Var,
                        Next => Build (Line (Stop + 1 .. Line'Last)),
                        Var  => Build (Line (Start .. Stop)));

                  else
                     --  We have some text before the tag
                     return new Node'
                       (Text,
                        Next  => Build (Line (Start .. Line'Last)),
                        Value => To_Unbounded_String
                                   (Line (Line'First .. Start - 1)));
                  end if;
               end if;
            end if;
         end if;
      end Build;

   begin
      return Build (Line);
   end Parse;

   ----------------
   -- Print_Tree --
   ----------------

   procedure Print_Tree (D : Tree) is
      N  : Tree := D;
      NL : Boolean := False;
   begin
      while N /= null loop
         case N.Kind is
            when Text =>
               declare
                  Value : constant String := To_String (N.Value);
                  VL    : constant Natural := Value'Length;
                  BL    : constant Natural := Utils.BOM_Utf8'Length;
               begin
                  if VL >= BL
                    and then
                      Value
                        (Value'First .. Value'First + BL - 1) = Utils.BOM_Utf8
                  then
                     Text_IO.Put ("U+<FEFF>");
                  else
                     Text_IO.Put (Value);
                  end if;
                  if Value'Length > 0 then
                     NL := Value (Value'Last) = ASCII.LF;
                  else
                     NL := False;
                  end if;
               end;

            when Var =>
               if N.Var.Is_Macro and then Expand_Macro then
                  Print_Tree (N.Var.Def);
               else
                  Text_IO.Put (Image (N.Var));
                  NL := False;
               end if;
         end case;
         N := N.Next;
      end loop;

      if not NL then
         Text_IO.New_Line;
      end if;
   end Print_Tree;

   -------------
   -- Release --
   -------------

   procedure Release (T : in out Tag_Var) is
      use type Filter.Set_Access;
      procedure Unchecked_Free is
         new Ada.Unchecked_Deallocation (Filter.Set, Filter.Set_Access);
   begin
      if T.Filters /= null then
         Filter.Release (T.Filters.all);
         Unchecked_Free (T.Filters);
      end if;

      if T.Parameters /= null then
         for K in T.Parameters'Range loop
            Data.Release (T.Parameters (K));
         end loop;
         Data.Unchecked_Free (T.Parameters);
      end if;

      Release (T.Def, Include => False);
   end Release;

   procedure Release (D : in out Tree; Single : Boolean := False) is

      procedure Unchecked_Free is new Ada.Unchecked_Deallocation (Node, Tree);

      P : Tree;
      T : Tree := D;

   begin
      while T /= null loop
         P := T;
         T := T.Next;

         case P.Kind is
            when Var  => Release (P.Var);
            when Text => null;
         end case;

         Unchecked_Free (P);
         exit when Single;
      end loop;

      D := null;
   end Release;

   ------------------------
   -- To_Data_Parameters --
   ------------------------

   function To_Data_Parameters
     (Parameters : Templates_Parser.Parameter_Set) return Data.Parameters
   is
      P : constant Data.Parameters := new Parameter_Set (Parameters'Range);
   begin
      for K in P'Range loop
         P (K) := Data.Parse (To_String (Parameters (K)));
      end loop;
      return P;
   end To_Data_Parameters;

   ---------------
   -- Translate --
   ---------------

   function Translate
     (T       : Tag_Var;
      Value   : String;
      Context : not null access Filter.Filter_Context) return String
   is
      use type Filter.Set_Access;
   begin
      if T.Filters /= null then
         declare
            R : Unbounded_String := To_Unbounded_String (Value);
         begin
            for K in T.Filters'Range loop
               R := To_Unbounded_String
                 (T.Filters (K).Handle
                  (To_String (R), Context, T.Filters (K).Parameters));
            end loop;

            return To_String (R);
         end;
      end if;

      return Value;
   end Translate;

end Data;
