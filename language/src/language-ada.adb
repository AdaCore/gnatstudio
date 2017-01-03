------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2017, AdaCore                     --
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

with Ada.Wide_Wide_Characters.Handling; use Ada.Wide_Wide_Characters.Handling;
with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.Regpat;               use GNAT.Regpat;
with GNATCOLL.Utils;            use GNATCOLL.Utils;

with Ada_Analyzer;              use Ada_Analyzer;
with String_Utils;              use String_Utils;
with UTF8_Utils;                use UTF8_Utils;

package body Language.Ada is

   use GNAT.Strings;

   Keywords_Regexp : aliased String :=
     "^(a(b(ort|s(tract)?)|cce(pt|ss)|l(iased|l)|nd|rray|t)|b"
     & "(egin|ody)|c(ase|onstant)|d(e(clare|l(ay|ta))|igits|o)|"
     & "e(ls(e|if)|n(d|try)|x(ception|it))|f(or|unction)|g(eneric|"
     & "oto)|i([fs]|n(terface)?)|l(imited|oop)|mod|n(ew|ot|ull)|"
     & "o(thers|ut|[fr]|verriding)|p(ackage|r(agma|ivate|o(cedure|"
     & "tected)))|r(a(ise|nge)|e(cord|m|names|queue|turn|verse))|s"
     & "(e(lect|parate)|ome|ubtype|ynchronized)|t(a(gged|sk)|"
     & "erminate|hen|ype)|u(ntil|se)|w(h(en|ile)|ith)|xor)\b";

   Keywords_List : aliased Pattern_Matcher :=
                     Compile (Keywords_Regexp, Case_Insensitive);

   The_Keywords : constant GNAT.Strings.String_List :=
                    (1  => new String'("abort"),
                     2  => new String'("abs"),
                     3  => new String'("abstract"),
                     4  => new String'("accept"),
                     5  => new String'("access"),
                     6  => new String'("aliased"),
                     7  => new String'("all"),
                     8  => new String'("and"),
                     9  => new String'("array"),
                     10 => new String'("at"),
                     11 => new String'("begin"),
                     12 => new String'("body"),
                     13 => new String'("case"),
                     14 => new String'("constant"),
                     15 => new String'("declare"),
                     16 => new String'("delay"),
                     17 => new String'("delta"),
                     18 => new String'("digits"),
                     19 => new String'("do"),
                     20 => new String'("else"),
                     21 => new String'("elsif"),
                     22 => new String'("end"),
                     23 => new String'("entry"),
                     24 => new String'("exception"),
                     25 => new String'("exit"),
                     26 => new String'("for"),
                     27 => new String'("function"),
                     28 => new String'("generic"),
                     29 => new String'("goto"),
                     30 => new String'("if"),
                     31 => new String'("in"),
                     32 => new String'("interface"),
                     33 => new String'("is"),
                     34 => new String'("limited"),
                     35 => new String'("loop"),
                     36 => new String'("mod"),
                     37 => new String'("new"),
                     38 => new String'("not"),
                     39 => new String'("null"),
                     40 => new String'("others"),
                     41 => new String'("out"),
                     42 => new String'("of"),
                     43 => new String'("or"),
                     44 => new String'("overriding"),
                     45 => new String'("package"),
                     46 => new String'("pragma"),
                     47 => new String'("private"),
                     48 => new String'("procedure"),
                     49 => new String'("protected"),
                     50 => new String'("raise"),
                     51 => new String'("range"),
                     52 => new String'("record"),
                     53 => new String'("rem"),
                     54 => new String'("renames"),
                     55 => new String'("requeue"),
                     56 => new String'("return"),
                     57 => new String'("reverse"),
                     58 => new String'("select"),
                     59 => new String'("separate"),
                     60 => new String'("some"),
                     61 => new String'("subtype"),
                     62 => new String'("synchronized"),
                     63 => new String'("tagged"),
                     64 => new String'("task"),
                     65 => new String'("terminate"),
                     66 => new String'("then"),
                     67 => new String'("type"),
                     68 => new String'("until"),
                     69 => new String'("use"),
                     70 => new String'("when"),
                     71 => new String'("while"),
                     72 => new String'("with"),
                     73 => new String'("xor"));
   --  List of the keywords. Indexes in this array have to correspond to values
   --  declared for token types in the specification.

   --  Make_Entry functions for the explorer

   function Make_Entry_Subprogram
     (Str     : String;
      Matched : Match_Array) return String;
   --  Function used to create an entry in the explorer, for subprograms.
   --  See the description of Explorer_Categories for more information.

   function Make_Entry_Task
     (Str     : String;
      Matched : Match_Array) return String;
   --  Function used to create an entry in the explorer, for tasks.
   --  See the description of Explorer_Categories for more information.

   function Make_Entry_Protected
     (Str     : String;
      Matched : Match_Array) return String;
   --  Function used to create an entry in the explorer, for protected objects
   --  and types.
   --  See the description of Explorer_Categories for more information.

   function Remove_Ada_Comments (Str : String) return String;
   --  Remove all Ada comments from the string (ie from -- to the next end of
   --  line).

   Comment_RE : constant String := "([ \t]*--[^\n]*)?";

   Subprogram_RE : aliased Pattern_Matcher :=
                     Compile
                       ("^[ \t]*(procedure|function)\s+"
                        & "(\w+)(" & Comment_RE & "\s*|\s*\([^\)]+\)"
                        & Comment_RE & ")\s*"
                        & "(return\s+(\w|\.)+\s*)?is\s",
                        Multiple_Lines or Case_Insensitive);

   Package_RE    : aliased Pattern_Matcher :=
                     Compile
                       ("^[ \t]*package[ \t]+((body[ \t]+)?((\w|\.)+))",
                        Multiple_Lines or Case_Insensitive);

   Type_Def_RE   : aliased Pattern_Matcher :=
                     Compile
                       ("^[ \t]*(sub)?type[ \t]+(\w+)",
                        Multiple_Lines or Case_Insensitive);

   Task_RE       : aliased Pattern_Matcher :=
                     Compile
                       ("^[ \t]*task[ \t]+((body|type)[ \t]+)?(\w+)",
                        Multiple_Lines or Case_Insensitive);

   Protected_RE  : aliased Pattern_Matcher :=
                     Compile
                       ("^[ \t]*protected[ \t]+((type|body)[ \t]+)?(\w+)",
                        Multiple_Lines or Case_Insensitive);

   --  The Specs are not parsed specifically. Instead, all the work is done
   --  while parsing for subprograms, and the function Make_Entry_Subprogram
   --  distinguishes between the two cases.

   Ada_Explorer_Categories : constant Explorer_Categories :=
                               ((Category       => Cat_Procedure,
                                 Category_Name  => No_Symbol,
                                 Regexp         => Subprogram_RE'Access,
                                 Position_Index => 2,
                                 End_Index      => 0,
                                 Make_Entry     =>
                                   Make_Entry_Subprogram'Access),

                                (Category       => Cat_Package,
                                 Category_Name  => No_Symbol,
                                 Regexp         => Package_RE'Access,
                                 Position_Index => 3,
                                 End_Index      => 0,
                                 Make_Entry     => null),

                                (Category       => Cat_Type,
                                 Category_Name  => No_Symbol,
                                 Regexp         => Type_Def_RE'Access,
                                 Position_Index => 2,
                                 End_Index      => 0,
                                 Make_Entry     => null),

                                (Category       => Cat_Task,
                                 Category_Name  => No_Symbol,
                                 Regexp         => Task_RE'Access,
                                 Position_Index => 3,
                                 End_Index      => 0,
                                 Make_Entry     => Make_Entry_Task'Access),

                                (Category       => Cat_Protected,
                                 Category_Name  => No_Symbol,
                                 Regexp         => Protected_RE'Access,
                                 Position_Index => 3,
                                 End_Index      => 0,
                                 Make_Entry     =>
                                   Make_Entry_Protected'Access));

   --------------------
   -- Is_Simple_Type --
   --------------------

   overriding function Is_Simple_Type
     (Lang : access Ada_Language; Str : String) return Boolean
   is
      pragma Unreferenced (Lang);
   begin
      return Str = "boolean"
        or else Str = "integer"
        or else Str = "natural"
        or else Str = "positive"
        or else Str = "system.address"
        or else Str = "character";
   end Is_Simple_Type;

   ----------------------
   -- Dereference_Name --
   ----------------------

   overriding function Dereference_Name
     (Lang : access Ada_Language; Name : String) return String
   is
      pragma Unreferenced (Lang);
   begin
      return Name & ".all";
   end Dereference_Name;

   ---------------------
   -- Array_Item_Name --
   ---------------------

   overriding function Array_Item_Name
     (Lang  : access Ada_Language;
      Name  : String;
      Index : String) return String
   is
      pragma Unreferenced (Lang);
   begin
      --  Simplify the expression by getting rid of unnecessary ".all"

      if Name'Length > 4
        and then Name (Name'Last - 3 .. Name'Last) = ".all"
      then
         return Name (Name'First .. Name'Last - 4) & '(' & Index & ')';
      else
         return Name & '(' & Index & ')';
      end if;
   end Array_Item_Name;

   -----------------------
   -- Record_Field_Name --
   -----------------------

   overriding function Record_Field_Name
     (Lang  : access Ada_Language;
      Name  : String;
      Field : String) return String
   is
      pragma Unreferenced (Lang);
   begin
      --  Simplify the expression by getting rid of unnecessary ".all"

      if Name'Length > 4
        and then Name (Name'Last - 3 .. Name'Last) = ".all"
      then
         return Name (Name'First .. Name'Last - 4) & '.' & Field;
      else
         return Name & '.' & Field;
      end if;
   end Record_Field_Name;

   ----------------------
   -- Explorer_Regexps --
   ----------------------

   overriding function Explorer_Regexps
     (Lang : access Ada_Language) return Explorer_Categories
   is
      pragma Unreferenced (Lang);
   begin
      return Ada_Explorer_Categories;
   end Explorer_Regexps;

   -------------------------
   -- Remove_Ada_Comments --
   -------------------------

   function Remove_Ada_Comments (Str : String) return String is
      Result       : String (Str'Range);
      Result_Index : Natural := Result'First;
      J            : Natural := Str'First;
   begin
      while J <= Str'Last loop
         if Str (J) = '-' and then Str (J + 1) = '-' then
            Skip_To_Char (Str, J, ASCII.LF);
         else
            Result (Result_Index) := Str (J);
            Result_Index := Result_Index + 1;
         end if;

         J := J + 1;
      end loop;

      return Result (Result'First .. Result_Index - 1);
   end Remove_Ada_Comments;

   ---------------------------
   -- Make_Entry_Subprogram --
   ---------------------------

   function Make_Entry_Subprogram
     (Str     : String;
      Matched : Match_Array) return String is
   begin
      if Matched (3) = No_Match then
         if Matched (4) = No_Match then
            return Str (Matched (2).First .. Matched (2).Last);
         else
            return Str (Matched (2).First .. Matched (2).Last)
              & " "
              & Reduce (Remove_Ada_Comments
                        (Str (Matched (4).First .. Matched (4).Last)));
         end if;

      elsif Matched (4) = No_Match then
         return
           Str (Matched (2).First .. Matched (2).Last) & ' ' & Reduce
           (Remove_Ada_Comments (Str (Matched (3).First .. Matched (3).Last)));

      else
         return
           Str (Matched (2).First .. Matched (2).Last) & ' ' & Reduce
           (Remove_Ada_Comments
            (Str (Matched (3).First .. Matched (3).Last)
             & " "
             & Str (Matched (4).First .. Matched (4).Last)));
      end if;
   end Make_Entry_Subprogram;

   --------------------------
   -- Make_Entry_Protected --
   --------------------------

   function Make_Entry_Protected
     (Str     : String;
      Matched : Match_Array) return String
   is
      First, Last : Natural;
   begin
      First := Matched (2).First;
      Last := Matched (2).Last;

      if First < Str'First then
         First := Str'First;
      end if;

      return Str (Matched (3).First .. Matched (3).Last) & " (" &
        Reduce (Str (First .. Last)) & ")";
   end Make_Entry_Protected;

   ---------------------
   -- Make_Entry_Task --
   ---------------------

   function Make_Entry_Task
     (Str     : String;
      Matched : Match_Array) return String
   is
      First, Last : Natural;
   begin
      First := Matched (2).First;
      Last := Matched (2).Last;

      if First < Str'First then
         First := Str'First;
      end if;

      return Str (Matched (3).First .. Matched (3).Last) & " (" &
        Reduce (Str (First .. Last)) & ")";
   end Make_Entry_Task;

   --------------------
   -- Is_System_File --
   --------------------

   overriding function Is_System_File
     (Lang      : access Ada_Language;
      File_Name : String) return Boolean
   is
      pragma Unreferenced (Lang);
      Name : constant String := Base_Name (File_Name);
   begin
      return
        (Name'Length > 2
         and then Name (Name'First + 1) = '-'
         and then (Name (Name'First) = 'a'
                   or else Name (Name'First) = 'g'
                   or else Name (Name'First) = 's'
                   or else Name (Name'First) = 'i'))
        or else Name = "gnat.ads"
        or else Name = "ada.ads"
        or else Name = "interfac.ads"
        or else Name = "system.ads";
   end Is_System_File;

   --------------
   -- Keywords --
   --------------

   overriding function Keywords
     (Lang : access Ada_Language) return Strings.String_Access
   is
      pragma Unreferenced (Lang);
   begin
      return Keywords_Regexp'Access;
   end Keywords;

   overriding function Keywords
     (Lang : access Ada_Language) return GNAT.Strings.String_List
   is
      pragma Unreferenced (Lang);
   begin
      return The_Keywords;
   end Keywords;

   overriding function Keywords
     (Lang : access Ada_Language) return GNAT.Expect.Pattern_Matcher_Access
   is
      pragma Unreferenced (Lang);
   begin
      return Keywords_List'Access;
   end Keywords;

   --------------------------
   -- Get_Language_Context --
   --------------------------

   Ada_Context : aliased Language_Context :=
                   (Syntax => (Comment_Start          => null,
                               Comment_End            => null,
                               New_Line_Comment_Start => new String'("--"),
                               New_Line_Comment_Start_Regexp => null),
                    String_Delimiter              => '"',
                    Quote_Character               => ASCII.NUL,
                    Constant_Character            => ''',
                    Can_Indent                    => True,
                    Syntax_Highlighting           => True,
                    Case_Sensitive                => False,
                    Accurate_Xref                 => True,
                    Use_Semicolon                 => True);

   overriding function Get_Language_Context
     (Lang : access Ada_Language) return Language_Context_Access
   is
      pragma Unreferenced (Lang);
   begin
      return Ada_Context'Access;
   end Get_Language_Context;

   ------------------
   -- Comment_Line --
   ------------------

   overriding function Comment_Line
     (Lang    : access Ada_Language;
      Line    : String;
      Comment : Boolean := True;
      Clean   : Boolean := False) return String
   is
      pragma Unreferenced (Lang);
      Local_Index : Natural;
   begin
      if Comment then
         return "--  " & Line;
      end if;

      --  Skip all the spaces and HT up to the first occurrence of "--",
      --  then do the following transformations
      --  1  |  --  blabla
      --  => |  blabla
      --
      --  2  |  -- blabla
      --  => |  blabla
      --
      --  3  |  --blabla
      --  => |  blabla
      --
      --  We do the first transformation in priority, so that
      --  uncommenting un-does the effects of commenting, and to keep
      --  close to the GNAT coding style.

      for Index in Line'First .. Line'Last - 1 loop
         if Line (Index .. Index + 1) = "--" then
            if Index + 3 <= Line'Last and then
              Line (Index .. Index + 3) = "--  "
            then
               if Clean then
                  Local_Index := Index + 4;
                  Skip_Blanks (Line, Local_Index);
                  return Line (Local_Index .. Line'Last);
               end if;
               return Line (Line'First .. Index - 1) &
                 Line (Index + 4 .. Line'Last);

            elsif Index + 2 <= Line'Last and then
              Line (Index .. Index + 2) = "-- "
            then
               if Clean then
                  Local_Index := Index + 3;
                  Skip_Blanks (Line, Local_Index);
                  return Line (Local_Index .. Line'Last);
               end if;
               return Line (Line'First .. Index - 1) &
                 Line (Index + 3 .. Line'Last);

            else
               return Line (Index + 2 .. Line'Last);
            end if;
         end if;

         exit when not (Line (Index) = ' ' or else Line (Index) = ASCII.HT);
      end loop;

      return Line;
   end Comment_Line;

   ----------------------
   -- Parse_Constructs --
   ----------------------

   overriding procedure Parse_Constructs
     (Lang   : access Ada_Language;
      File   : GNATCOLL.VFS.Virtual_File;
      Buffer : UTF8_String;
      Result : out Construct_List)
   is
      pragma Unreferenced (File);
      Constructs : aliased Construct_List;
   begin
      Analyze_Ada_Source
        (Buffer,
         Lang.Symbols,
         Default_Indent_Parameters,
         Format     => False,
         Constructs => Constructs'Unchecked_Access);
      Result := Constructs;
   end Parse_Constructs;

   --------------------
   -- Parse_Entities --
   --------------------

   overriding procedure Parse_Entities
     (Lang     : access Ada_Language;
      Buffer   : String;
      Callback : Entity_Callback) is
   begin
      Analyze_Ada_Source
        (Buffer,
         Lang.Symbols,
         Default_Indent_Parameters,
         Format   => False,
         Callback => Callback);
   end Parse_Entities;

   -------------------
   -- Format_Buffer --
   -------------------

   overriding procedure Format_Buffer
     (Lang                : access Ada_Language;
      Buffer              : String;
      Replace             : Replace_Text_Callback;
      From, To            : Natural := 0;
      Indent_Params       : Indent_Parameters := Default_Indent_Parameters;
      Indent_Offset       : Natural := 0;
      Case_Exceptions     : Case_Handling.Casing_Exceptions :=
        Case_Handling.No_Casing_Exception;
      Is_Optional_Keyword : access function (S : String)
                                             return Boolean := null)
   is
   begin
      Analyze_Ada_Source
        (Buffer, Lang.Symbols, Indent_Params, True, From, To,
         Replace,
         Indent_Offset       => Indent_Offset,
         Case_Exceptions     => Case_Exceptions,
         Is_Optional_Keyword => Is_Optional_Keyword);
   end Format_Buffer;

   --------------
   -- Get_Name --
   --------------

   overriding function Get_Name (Lang : access Ada_Language) return String is
      pragma Unreferenced (Lang);
   begin
      return "Ada";
   end Get_Name;

   ---------------------------
   -- Get_Referenced_Entity --
   ---------------------------

   overriding procedure Get_Referenced_Entity
     (Lang       : access Ada_Language;
      Buffer     : String;
      Construct  : Simple_Construct_Information;
      Sloc_Start : out Source_Location;
      Sloc_End   : out Source_Location;
      Success    : out Boolean;
      From_Index : Natural := 0)
   is
      Paren_Depth          : Integer := 0;
      Has_Reference        : Boolean := False;
      Skip_Next_Identifier : Boolean := False;
      Previous_Is          : Boolean := False;

      In_Anonymous_Access_Type : Boolean := False;
      Anon_Start_Sloc          : Source_Location;
      Anon_End_Sloc            : Source_Location;

      function Token_Callback
        (Entity         : Language_Entity;
         Sloc_Start_Got : Source_Location;
         Sloc_End_Got   : Source_Location;
         Partial_Entity : Boolean) return Boolean;
      --  Used to parse the tokens of the construct

      --------------------
      -- Token_Callback --
      --------------------

      function Token_Callback
        (Entity         : Language_Entity;
         Sloc_Start_Got : Source_Location;
         Sloc_End_Got   : Source_Location;
         Partial_Entity : Boolean) return Boolean
      is
         pragma Unreferenced (Partial_Entity);
         Word : constant String :=
                  Buffer (Sloc_Start_Got.Index .. Sloc_End_Got.Index);
      begin
         if Paren_Depth = 0 then
            if (Construct.Category in Subprogram_Category
                or else Construct.Category = Cat_Package)
              and then Previous_Is
              and then Word /= "new"
            then
               --  We hit the "is" keyword on a subprogram body, or a
               --  package declaration part. There's no more referenced
               --  entities to find. Stop the anlaysis.

               Success := False;

               return True;
            end if;

            if Entity in Identifier_Entity or else Word = "exception" then
               if not Skip_Next_Identifier then
                  if Has_Reference then
                     Sloc_Start := Sloc_Start_Got;
                     Sloc_End := Sloc_End_Got;
                     Success := True;
                  end if;

                  return True;
               end if;

            elsif Entity = Keyword_Text then
               if Word = "is" then
                  Previous_Is := True;
               else
                  Previous_Is := False;
               end if;

               if Word = "access" or else Word = "new"
                 or else Word = "return" or else Word = "renames"
                 or else Word = "of"
                 or else Word = "and"
                 or else (Word = "is"
                          and then Construct.Category = Cat_Subtype)
               then
                  Has_Reference := True;
               end if;

               if Construct.Category = Cat_Parameter then
                  if Word = "access" then
                     In_Anonymous_Access_Type := True;
                  end if;

                  --  For anonymous access types to subprograms the referenced
                  --  entity corresponds with the full text of the subprogram
                  --  profile

                  if In_Anonymous_Access_Type
                    and then Anon_Start_Sloc.Line = 0
                    and then (Word = "protected"
                               or else Word = "function"
                               or else Word = "procedure")
                  then
                     Anon_Start_Sloc := Sloc_Start_Got;
                  end if;
               end if;

            elsif Entity = Operator_Text then
               if Word = ":" then
                  Has_Reference := True;
                  Skip_Next_Identifier := False;

               elsif Word = "," then
                  Skip_Next_Identifier := True;

               elsif Word = ";" and then In_Anonymous_Access_Type then
                  Sloc_Start := Anon_Start_Sloc;
                  Sloc_End := Anon_End_Sloc;
                  Success := True;
                  return True;
               end if;
            end if;
         end if;

         if Entity = Operator_Text then
            if Word = "(" then
               Paren_Depth := Paren_Depth + 1;
            elsif Word = ")" then
               Paren_Depth := Paren_Depth - 1;

               if In_Anonymous_Access_Type
                 and then Paren_Depth <= 0
               then
                  Sloc_Start := Anon_Start_Sloc;

                  if Paren_Depth = 0 then
                     Sloc_End := Sloc_End_Got;

                  --  In an anonymous access to subprogram Paren_Count is -1
                  --  when the target subprogram has no formals and we are
                  --  located in the parenthesis of the enclosing profile.
                  --  For example: procedure P (Q : access procedure)

                  else
                     Sloc_End := Anon_End_Sloc;
                  end if;

                  Success := True;
                  return True;
               end if;
            end if;
         end if;

         if In_Anonymous_Access_Type then
            Anon_End_Sloc := Sloc_End_Got;
         end if;

         return False;
      end Token_Callback;

      Index_Begin : Natural;

   begin
      Success := False;

      if Construct.Category in Type_Category
        or else Construct.Category in Subprogram_Category
        or else Construct.Category in Cat_Variable .. Cat_Field
        or else Construct.Category in Namespace_Category
      then
         if From_Index = 0 then
            if Construct.Name = No_Symbol then
               Index_Begin := Construct.Sloc_Start.Index;
            else
               Index_Begin :=
                 Construct.Sloc_Entity.Index + Get (Construct.Name)'Length;
            end if;

         else
            Index_Begin := From_Index;
         end if;

         if Index_Begin in Buffer'Range
           and then Construct.Sloc_End.Index in Buffer'Range
         then
            Parse_Entities
              (Lang,
               Buffer (Index_Begin .. Construct.Sloc_End.Index),
               Callback => Token_Callback'Unrestricted_Access);
         end if;
      end if;
   end Get_Referenced_Entity;

   -----------
   -- Image --
   -----------

   function Image (Token : Ada_Token) return String is
   begin
      case Token is
         when No_Token              => return "NO_TOKEN";
         when Ada_Reserved_Token    =>
            return "TOK_" & To_Upper (The_Keywords (Integer (Token)).all);
         when Tok_Dot               => return "TOK_DOT";
         when Tok_Open_Parenthesis  => return "TOK_OPEN_PARENTHESIS";
         when Tok_Close_Parenthesis => return "TOK_CLOSE_PARENTHESIS";
         when Tok_Colon             => return "TOK_COLON";
         when Tok_Arrow             => return "TOK_ARROW";
         when Tok_Operator          => return "TOK_OPERATOR";
         when Tok_Comma             => return "TOK_COMMA";
         when Tok_Semicolon         => return "TOK_SEMICOLON";
         when Tok_Blank             => return "TOK_BLANK";
         when Tok_Tick              => return "TOK_TICK";
         when Tok_Dot_Dot           => return "TOK_DOT_DOT";
         when Tok_Identifier        => return "TOK_IDENTIFIER";
         when Tok_String            => return "TOK_STRING";
         when Tok_Expression        => return "TOK_EXPRESSION";
      end case;
   end Image;

   -------------------------------
   -- Parse_Expression_Backward --
   -------------------------------

   overriding procedure Parse_Tokens_Backwards
     (Lang         : access Ada_Language;
      Buffer       : UTF8_String;
      Start_Offset : String_Index_Type;
      End_Offset   : String_Index_Type := 0;
      Callback     : access procedure (Token : Token_Record;
                                       Stop  : in out Boolean))
   is
      pragma Unreferenced (Lang);
      Offset               : Natural := Natural (Start_Offset);
      Offset_Limit         : Natural;
      Token                : Token_Record;
      Prev_Non_Blank_Token : Token_Record;

      procedure Skip_String
        (Offset            : in out Natural;
         Incomplete_String : out Boolean);
      --  Assuming offset is on a closing double quote, skips to before the
      --  corresponding opening double quote. If such opening double quote
      --  can't be found, Incomplete_String is set to True, false otherwise.

      procedure Skip_Comment_Line
        (Offset : in out Natural; Incomplete_String : out Boolean);

      procedure Handle_Token
        (Token : in out Token_Record; Offset : Natural; Stop : out Boolean);
      --  Adjust the position of the token and calls the callback

      -----------------
      -- Skip_String --
      -----------------

      procedure Skip_String
        (Offset            : in out Natural;
         Incomplete_String : out Boolean)
      is
         Close_Found : Boolean := False;
      begin
         Incomplete_String := True;

         while Offset > Offset_Limit loop
            case Buffer (Offset) is
               when '"' =>
                  if Close_Found then
                     if Offset - 1 = Offset_Limit
                       or else Buffer (Offset - 1) /= '"'
                     then
                        Incomplete_String := False;

                        exit;
                     else
                        Offset := UTF8_Prev_Char (Buffer, Offset);
                     end if;
                  else
                     Close_Found := True;
                  end if;

               when ASCII.LF =>
                  --  Strings can't be on spread across multiple lines - this
                  --  probably means that the string is broken.
                  exit;

               when others =>
                  null;
            end case;

            Offset := UTF8_Prev_Char (Buffer, Offset);
         end loop;
      end Skip_String;

      -----------------------
      -- Skip_Comment_Line --
      -----------------------

      procedure Skip_Comment_Line
        (Offset : in out Natural; Incomplete_String : out Boolean)
      is
         Local_Offset      : Natural := Offset;
         Prev_Offset       : Natural;
      begin
         Incomplete_String := False;

         Local_Offset := UTF8_Prev_Char (Buffer, Local_Offset);

         while Local_Offset > Offset_Limit loop
            case Buffer (Local_Offset) is
               when '"' =>
                  Skip_String (Local_Offset, Incomplete_String);

               when ''' =>
                  Local_Offset := UTF8_Prev_Char
                    (Buffer, Local_Offset);
                  Local_Offset := UTF8_Prev_Char
                    (Buffer, Local_Offset);

               when '-' =>
                  Prev_Offset := UTF8_Prev_Char
                    (Buffer, Local_Offset);

                  if Prev_Offset > Offset_Limit
                    and then Buffer (Prev_Offset) = '-'
                  then
                     Offset := Prev_Offset;

                     --  When hitting a comment, we don't care about incomplete
                     --  strings.

                     Incomplete_String := False;
                  end if;

               when ASCII.LF =>
                  exit;

               when others =>
                  null;
            end case;

            Local_Offset := UTF8_Prev_Char
              (Buffer, Local_Offset);
         end loop;
      end Skip_Comment_Line;

      ------------------
      -- Handle_Token --
      ------------------

      procedure Handle_Token
        (Token : in out Token_Record; Offset : Natural; Stop : out Boolean)
      is
      begin
         Stop := False;

         if Token /= Null_Token then
            if Token.Token_First = 0 then
               Token.Token_First := String_Index_Type (Offset);
            end if;

            if Token.Token_Last = 0 then
               Token.Token_Last := String_Index_Type (Offset);
            end if;

            if Token.Tok_Type = Tok_Identifier
              and then Token.Token_Last /= Start_Offset
            then
               --  Check if we got a reserved word to be analyzed as such. We
               --  consider the last word in the expression to be an
               --  indentifier, as it may be used in the context of an
               --  uncomplete expression, e.g. A.with completed into
               --  A.withdrawal. This is not true anymore if there's anything
               --  after the identifier, e.g. a space.

               declare
                  Word : constant String :=
                    To_Lower
                      (Buffer
                           (Integer (Token.Token_First)
                            .. Integer (Token.Token_Last)));
               begin
                  for J in The_Keywords'Range loop
                     if The_Keywords (J).all = Word then
                        Token.Tok_Type := Token_Type (J);

                        exit;
                     end if;
                  end loop;
               end;
            end if;

            Callback (Token, Stop);

            if Token.Tok_Type /= Tok_Blank then
               Prev_Non_Blank_Token := Token;
            end if;

            Token := Null_Token;
         end if;
      end Handle_Token;

      Next_Ind   : Natural;

      Stop : Boolean := False;

      Incomplete_String : Boolean := False;
   begin

      if Buffer'Length = 0 or Offset > Buffer'Last then
         return;
      end if;

      if Natural (End_Offset) < Buffer'First then
         Offset_Limit := Buffer'First - 1;
      else
         Offset_Limit := Natural (End_Offset) - 1;
      end if;

      Skip_Comment_Line (Offset, Incomplete_String);

      if Offset /= Natural (Start_Offset) then
         --  In this case, we are on a comment line. So the expression is
         --  empty.

         return;
      end if;

      if Incomplete_String then
         --  This line contains a string that is not finished, we're actually
         --  in a string, so don't return any token

         return;
      end if;

      --  Adjust the first offset - if the cursor doesn't start at the start
      --  of an UTF-8 character

      declare
         Prev_Offset : constant Integer :=
                         UTF8_Prev_Char (Buffer, Offset);
         Next_Offset : Integer;
      begin
         if Prev_Offset > Buffer'First then
            --  Once we have the previous offset, check for the beginning of
            --  the next one. There are two cases here, either the previous
            --  offset is the first to analyze, and the next one will be passed
            --  the start offset value, or it's the one before and next offset
            --  will be the one to analyze.

            Next_Offset := UTF8_Next_Char (Buffer, Prev_Offset);

            if Next_Offset <= Offset then
               --  If the next offset found is equal or before the initial
               --  one, it means that the prev offset is actually a character
               --  below. Use next offset as the first offset to analyze.

               Offset := Next_Offset;
            else
               --  If the next offset is after the offset where the analysis
               --  starts, then the prev offset is indeed the offset of
               --  the first character to analyze.

               Offset := Prev_Offset;
            end if;
         end if;
      end;

      while Offset > Offset_Limit loop
         case Buffer (Offset) is
            when ';' =>
               Handle_Token (Token, Offset, Stop);
               exit when Stop;

               Token.Tok_Type := Tok_Semicolon;
               Handle_Token (Token, Offset, Stop);
               exit when Stop;

            when ',' =>
               Handle_Token (Token, Offset, Stop);
               exit when Stop;

               Token.Tok_Type := Tok_Comma;
               Handle_Token (Token, Offset, Stop);
               exit when Stop;

            when ')' =>
               Handle_Token (Token, Offset, Stop);
               exit when Stop;

               Token.Tok_Type := Tok_Close_Parenthesis;
               Handle_Token (Token, Offset, Stop);
               exit when Stop;

            when '(' =>
               Handle_Token (Token, Offset, Stop);
               exit when Stop;

               Token.Tok_Type := Tok_Open_Parenthesis;
               Handle_Token (Token, Offset, Stop);
               exit when Stop;

            when '.' =>
               if Offset < Buffer'Last and then Buffer (Offset + 1) = '.' then
                  --  .. case

                  Token.Tok_Type := Tok_Dot_Dot;
                  Token.Token_First := String_Index_Type (Offset);
                  Token.Token_Last := String_Index_Type (Offset + 1);
               else
                  Handle_Token (Token, Offset, Stop);
                  exit when Stop;

                  Token.Tok_Type := Tok_Dot;
                  Token.Token_First := String_Index_Type (Offset);
                  Token.Token_Last := String_Index_Type (Offset);
               end if;

            when ''' =>
               Handle_Token (Token, Offset, Stop);
               exit when Stop;

               Token.Tok_Type := Tok_Tick;

               declare
                  Local_Offset : Integer := Offset;
               begin
                  Local_Offset :=
                    UTF8_Prev_Char (Buffer, Local_Offset);

                  if not Is_Alphanumeric
                    (UTF8_Get_Char (Buffer (Local_Offset .. Offset)))
                  then
                     Token.Tok_Type := No_Token;
                  end if;

                  if Local_Offset > Offset_Limit then
                     Local_Offset :=
                       UTF8_Prev_Char (Buffer, Local_Offset);
                  end if;

                  if Local_Offset > Offset_Limit then
                     if Buffer (Local_Offset) = ''' then
                        Token.Tok_Type := Tok_String;
                        --  ??? This should rather be Tok_Character

                        Token.Token_Last := String_Index_Type (Offset);
                        Token.Token_First := String_Index_Type (Local_Offset);

                        Offset := Local_Offset;
                     end if;
                  end if;
               end;

               exit when Token.Tok_Type = No_Token;

               Handle_Token (Token, Offset, Stop);
               exit when Stop;

            when ' ' | ASCII.HT | ASCII.CR =>
               Handle_Token (Token, Offset, Stop);
               exit when Stop;

               Token.Tok_Type := Tok_Blank;
               Handle_Token (Token, Offset, Stop);
               exit when Stop;

            when '"' =>
               Handle_Token (Token, Offset, Stop);
               exit when Stop;

               if Prev_Non_Blank_Token.Tok_Type = Tok_Open_Parenthesis then
                  --  We are in an operator symbol case

                  Token.Tok_Type := Tok_Identifier;
                  Token.Token_Last := String_Index_Type (Offset);

                  Skip_String (Offset, Incomplete_String);

                  if Incomplete_String then
                     exit;
                  end if;

                  Token.Token_First := String_Index_Type (Offset);

                  Handle_Token (Token, Offset, Stop);
                  exit when Stop;
               else
                  Token.Tok_Type := Tok_String;
                  Token.Token_Last := String_Index_Type (Offset);

                  Skip_String (Offset, Incomplete_String);

                  Handle_Token (Token, Offset, Stop);
                  exit when Stop;
               end if;

            when '<' | '>' =>
               if Offset < Buffer'Last and then Buffer (Offset + 1) = '=' then
                  --  >= or <= case
                  Token.Tok_Type := Tok_Operator;
                  Token.Token_First := String_Index_Type (Offset);
                  Token.Token_Last := String_Index_Type (Offset + 1);
               else
                  Handle_Token (Token, Offset, Stop);
                  exit when Stop;

                  Token.Tok_Type := Tok_Operator;
                  Token.Token_First := String_Index_Type (Offset);
                  Token.Token_Last := String_Index_Type (Offset);
               end if;

            when '=' =>
               if Offset < Buffer'Last and then Buffer (Offset + 1) = '>' then
                  --  => case
                  Token.Tok_Type := Tok_Arrow;
                  Token.Token_First := String_Index_Type (Offset);
                  Token.Token_Last := String_Index_Type (Offset + 1);
               else
                  Handle_Token (Token, Offset, Stop);
                  exit when Stop;

                  Token.Tok_Type := Tok_Operator;
                  Token.Token_First := String_Index_Type (Offset);
                  Token.Token_Last := String_Index_Type (Offset);
               end if;

            when '/' =>
               if Offset < Buffer'Last and then Buffer (Offset + 1) = '=' then
                  --  /= case
                  Token.Tok_Type := Tok_Operator;
                  Token.Token_First := String_Index_Type (Offset);
                  Token.Token_Last := String_Index_Type (Offset + 1);
               else
                  Handle_Token (Token, Offset, Stop);
                  exit when Stop;

                  Token.Tok_Type := Tok_Operator;
                  Token.Token_First := String_Index_Type (Offset);
                  Token.Token_Last := String_Index_Type (Offset);
               end if;

            when '+' | '-' | '&' =>
               Handle_Token (Token, Offset, Stop);
               exit when Stop;

               Token.Tok_Type := Tok_Operator;
               Token.Token_First := String_Index_Type (Offset);
               Token.Token_Last := String_Index_Type (Offset);

            when '*' =>
               if Offset < Buffer'Last and then Buffer (Offset + 1) = '*' then
                  --  ** case
                  Token.Tok_Type := Tok_Operator;
                  Token.Token_First := String_Index_Type (Offset);
                  Token.Token_Last := String_Index_Type (Offset + 1);
               else
                  Handle_Token (Token, Offset, Stop);
                  exit when Stop;

                  Token.Tok_Type := Tok_Operator;
                  Token.Token_First := String_Index_Type (Offset);
                  Token.Token_Last := String_Index_Type (Offset);
               end if;

            when ':' =>
               if Offset < Buffer'Last and then Buffer (Offset + 1) = '=' then
                  --  := case
                  Token.Tok_Type := Tok_Operator;
                  Token.Token_First := String_Index_Type (Offset);
                  Token.Token_Last := String_Index_Type (Offset + 1);
               else
                  Handle_Token (Token, Offset, Stop);
                  exit when Stop;

                  Token.Tok_Type := Tok_Colon;
                  Handle_Token (Token, Offset, Stop);
                  exit when Stop;
               end if;

            when ASCII.LF =>
               Handle_Token (Token, Offset, Stop);
               exit when Stop;

               Token.Tok_Type := Tok_Blank;
               Handle_Token (Token, Offset, Stop);
               exit when Stop;

               Skip_Comment_Line (Offset, Incomplete_String);

               if Incomplete_String then
                  --  This line contains an unfinished string, don't parse
                  --  anything anymore and return.

                  exit;
               end if;

            when others =>
               if Token.Tok_Type /= Tok_Identifier then
                  Handle_Token (Token, Offset, Stop);
                  exit when Stop;
               end if;

               Next_Ind := UTF8_Next_Char (Buffer, Offset) - 1;

               if (Next_Ind in Buffer'Range
                   and then
                     (Is_Alphanumeric
                        (UTF8_Get_Char (Buffer (Offset .. Next_Ind)))))
                 or else
                   (Next_Ind not in Buffer'Range
                    and then Is_Alphanumeric (Buffer (Offset)))
                 or else Buffer (Offset) = '_'
               then
                  Token.Tok_Type := Tok_Identifier;
                  Token.Token_First := String_Index_Type (Offset);

                  if Token.Token_Last = 0 then
                     Token.Token_Last := String_Index_Type (Next_Ind);
                  end if;
               else
                  Handle_Token (Token, Offset, Stop);
                  exit when Stop;
               end if;
         end case;

         Offset := UTF8_Prev_Char (Buffer, Offset);
      end loop;

      if Stop then
         --  If we stopped before doing the prev offset
         Offset := UTF8_Prev_Char (Buffer, Offset);
      end if;

      Handle_Token (Token, Offset, Stop);
   end Parse_Tokens_Backwards;

   -------------------------------
   -- Parse_Reference_Backwards --
   -------------------------------

   overriding function Parse_Reference_Backwards
     (Lang         : access Ada_Language;
      Buffer       : UTF8_String;
      Start_Offset : String_Index_Type;
      End_Offset   : String_Index_Type := 0) return String
   is
      Tmp : Unbounded_String;

      Expression_Depth : Integer := 0;

      Prev_Non_Blank   : Token_Record;
      Ends_With_Blanks : Boolean := False;
      Is_First         : Boolean := True;

      procedure Callback
        (Token : Token_Record;
         Stop  : in out Boolean);

      --------------
      -- Callback --
      --------------

      procedure Callback
        (Token : Token_Record;
         Stop  : in out Boolean)
      is
      begin
         case Token.Tok_Type is
            when Tok_Blank =>
               if Is_First then
                  Ends_With_Blanks := True;
               end if;

               return;

            when Tok_Dot =>
               if Is_First then
                  Stop := True;

                  return;
               end if;

            when Tok_Identifier =>
               if Ends_With_Blanks
                 or else Prev_Non_Blank.Tok_Type = Tok_Identifier
               then
                  Stop := True;

                  return;
               end if;

            when Tok_All | Tok_Tick =>
               if Ends_With_Blanks then
                  Stop := True;

                  return;
               end if;

            when Tok_Close_Parenthesis =>
               Expression_Depth := Expression_Depth + 1;

            when Tok_Open_Parenthesis =>
               Expression_Depth := Expression_Depth - 1;

               if Expression_Depth < 0 then
                  Stop := True;
                  return;
               end if;

            when others =>
               if Expression_Depth <= 0 then
                  Stop := True;
                  return;
               end if;

         end case;

         if Token.Tok_Type /= Tok_Blank then
            Is_First := False;
         end if;

         Insert
           (Tmp,
            1,
            Buffer
              (Natural (Token.Token_First) .. Natural (Token.Token_Last)));

         Prev_Non_Blank := Token;
      end Callback;

   begin
      Lang.Parse_Tokens_Backwards
        (Buffer       => Buffer,
         Start_Offset => Start_Offset,
         End_Offset   => End_Offset,
         Callback     => Callback'Access);

      return To_String (Tmp);
   end Parse_Reference_Backwards;

end Language.Ada;
