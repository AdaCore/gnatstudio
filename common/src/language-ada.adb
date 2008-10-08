-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2000-2008, AdaCore                  --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada_Analyzer;              use Ada_Analyzer;
with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Glib.Unicode;              use Glib.Unicode;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.Regpat;               use GNAT.Regpat;
with GNATCOLL.Utils;            use GNATCOLL.Utils;
with String_Utils;              use String_Utils;

package body Language.Ada is

   use GNAT.Strings;

   Keywords_Regexp : aliased String :=
                       "a(b(ort|s(tract)?)|cce(pt|ss)|l(iased|l)|nd|rray|t)|b"
                 & "(egin|ody)|c(ase|onstant)|d(e(clare|l(ay|ta))|igits|o)|"
                 & "e(ls(e|if)|n(d|try)|x(ception|it))|f(or|unction)|g(eneric|"
                 & "oto)|i([fs]|n(terface)?)|l(imited|oop)|mod|n(ew|ot|ull)|"
                 & "o(thers|ut|[fr]|verriding)|p(ackage|r(agma|ivate|o(cedure|"
                 & "tected)))|r(a(ise|nge)|e(cord|m|names|queue|turn|verse))|s"
                 & "(e(lect|parate)|ubtype|ynchronized)|t(a(gged|sk)|erminate|"
                 & "hen|ype)|u(ntil|se)|w(h(en|ile)|ith)|xor";

   Keywords_List : aliased Pattern_Matcher :=
                     Compile
                       ("^(" & Keywords_Regexp & ")\b", Case_Insensitive);

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
                     60 => new String'("subtype"),
                     61 => new String'("synchronized"),
                     62 => new String'("tagged"),
                     63 => new String'("task"),
                     64 => new String'("terminate"),
                     65 => new String'("then"),
                     66 => new String'("type"),
                     67 => new String'("until"),
                     68 => new String'("use"),
                     69 => new String'("when"),
                     70 => new String'("while"),
                     71 => new String'("with"),
                     72 => new String'("xor"));

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
   --  line)

   Comment_RE : constant String := "([ \t]*--[^\n]*)?";

   Subprogram_RE : aliased Pattern_Matcher :=
     Compile
       ("^[ \t]*(procedure|function)\s+" &
        "(\w+)(" & Comment_RE & "\s*|\s*\([^\)]+\)" & Comment_RE & ")\s*" &
        "(return\s+(\w|\.)+\s*)?is\s", Multiple_Lines or Case_Insensitive);

   Package_RE    : aliased Pattern_Matcher :=
     Compile
       ("^[ \t]*package[ \t]+((body[ \t]+)?((\w|\.)+))",
        Multiple_Lines or Case_Insensitive);

   Type_Def_RE   : aliased Pattern_Matcher :=
     Compile
       ("^[ \t]*(sub)?type[ \t]+(\w+)", Multiple_Lines or Case_Insensitive);

   Task_RE       : aliased Pattern_Matcher :=
     Compile
       ("^[ \t]*task[ \t]+((body|type)[ \t]+)?(\w+)",
        Multiple_Lines or Case_Insensitive);

   Protected_RE : aliased Pattern_Matcher :=
     Compile ("^[ \t]*protected[ \t]+((type|body)[ \t]+)?(\w+)",
              Multiple_Lines or Case_Insensitive);

   --  The Specs are not parsed specifically. Instead, all the work is done
   --  while parsing for subprograms, and the function Make_Entry_Subprogram
   --  distinguishes between the two cases.

   Ada_Explorer_Categories : constant Explorer_Categories :=
                               ((Category       => Cat_Procedure,
                                 Category_Name  => null,
                                 Regexp         => Subprogram_RE'Access,
                                 Position_Index => 2,
                                 End_Index      => 0,
                                 Make_Entry     =>
                                   Make_Entry_Subprogram'Access),

                                (Category       => Cat_Package,
                                 Category_Name  => null,
                                 Regexp         => Package_RE'Access,
                                 Position_Index => 3,
                                 End_Index      => 0,
                                 Make_Entry     => null),

                                (Category       => Cat_Type,
                                 Category_Name  => null,
                                 Regexp         => Type_Def_RE'Access,
                                 Position_Index => 2,
                                 End_Index      => 0,
                                 Make_Entry     => null),

                                (Category       => Cat_Task,
                                 Category_Name  => null,
                                 Regexp         => Task_RE'Access,
                                 Position_Index => 3,
                                 End_Index      => 0,
                                 Make_Entry     => Make_Entry_Task'Access),

                                (Category       => Cat_Protected,
                                 Category_Name  => null,
                                 Regexp         => Protected_RE'Access,
                                 Position_Index => 3,
                                 End_Index      => 0,
                                 Make_Entry     =>
                                   Make_Entry_Protected'Access));

   --  ??? Would be nice to specify the list of available cross compilers
   --  using a configuration file

   Ada_Project_Fields : constant Project_Field_Array :=
     (1 => (Attribute_Name  => new String'("compiler_command"),
            Attribute_Index => new String'("ada"),
            Description     => new String'("Ada compiler"),
            Values          => new GNAT.Strings.String_List'
              (1  => new String'("gnatmake"),
               2  => new String'("powerpc-wrs-vxworks-gnatmake"),
               3  => new String'("powerpc-wrs-vxworksae-gnatmake"),
               4  => new String'("powerpc-elf-gnatmake"),
               5  => new String'("i386-wrs-vxworks-gnatmake"),
               6  => new String'("m68k-wrs-vxworks-gnatmake"),
               7  => new String'("mips-wrs-vxworks-gnatmake"),
               8  => new String'("sparc-wrs-vxworks-gnatmake"),
               9  => new String'("sparc64-wrs-vxworks-gnatmake"),
               10 => new String'("xscale-wrs-vxworks-gnatmake"),
               11 => new String'("powerpc-elf-lynxos-gnatmake"),
               12 => new String'("powerpc-xcoff-lynxos-gnatmake"),
               13 => new String'("gnaampmake")),
            Editable       => True),
      2 => (Attribute_Name  => new String'("gnatlist"),
            attribute_Index => null,
            Description     => new String'("Gnatls"),
            Values          => new GNAT.Strings.String_List'
              (1  => new String'("gnatls"),
               2  => new String'("powerpc-wrs-vxworks-gnatls"),
               3  => new String'("powerpc-wrs-vxworksae-gnatls"),
               4  => new String'("powerpc-elf-gnatls"),
               5  => new String'("i386-wrs-vxworks-gnatls"),
               6  => new String'("m68k-wrs-vxworks-gnatls"),
               7  => new String'("mips-wrs-vxworks-gnatls"),
               8  => new String'("sparc-wrs-vxworks-gnatls"),
               9  => new String'("sparc64-wrs-vxworks-gnatls"),
               10 => new String'("xscale-wrs-vxworks-gnatls"),
               11 => new String'("powerpc-elf-lynxos-gnatls"),
               12 => new String'("powerpc-xcoff-lynxos-gnatls"),
               13 => new String'("gnaampls")),
            Editable        => True));

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
      Result : String (Str'Range);
      Result_Index : Natural := Result'First;
      J : Natural := Str'First;
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
                   (Comment_Start_Length          => 0,
                    Comment_End_Length            => 0,
                    Comment_Start                 => "",
                    Comment_End                   => "",
                    New_Line_Comment_Start        => new String'("--"),
                    New_Line_Comment_Start_Regexp => null,
                    String_Delimiter              => '"',
                    Quote_Character               => ASCII.NUL,
                    Constant_Character            => ''',
                    Can_Indent                    => True,
                    Syntax_Highlighting           => True,
                    Case_Sensitive                => False);

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
      Buffer : Glib.UTF8_String;
      Result : out Construct_List)
   is
      pragma Unreferenced (Lang);
      Constructs : aliased Construct_List;
   begin
      Analyze_Ada_Source
        (Buffer,
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
      Callback : Entity_Callback)
   is
      pragma Unreferenced (Lang);
      pragma Suppress (All_Checks);
      --  See comment in Language.C.Parse_Entities
      --  We've never got an exception for an Ada file, but on the other hand,
      --  no checks are required in this procedure anyway.

   begin
      Analyze_Ada_Source
        (Buffer,
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
      pragma Unreferenced (Lang);
   begin
      Analyze_Ada_Source
        (Buffer, Indent_Params, True, From, To,
         Replace,
         Indent_Offset       => Indent_Offset,
         Case_Exceptions     => Case_Exceptions,
         Is_Optional_Keyword => Is_Optional_Keyword);
   end Format_Buffer;

   ------------------------
   -- Get_Project_Fields --
   ------------------------

   overriding function Get_Project_Fields
     (Lang : access Ada_Language) return Project_Field_Array
   is
      pragma Unreferenced (Lang);
   begin
      return Ada_Project_Fields;
   end Get_Project_Fields;

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
            if Entity = Keyword_Text then
               if Word = "access" or else Word = "new"
                 or else Word = "return" or else Word = "renames"
                 or else Word = "of"
                 or else (Word = "is"
                          and then Construct.Category = Cat_Subtype)
               then
                  Has_Reference := True;
               end if;
            elsif Entity = Identifier_Text then
               if not Skip_Next_Identifier then
                  if Has_Reference then
                     Sloc_Start := Sloc_Start_Got;
                     Sloc_End := Sloc_End_Got;
                     Success := True;
                  end if;

                  return True;
               end if;
            elsif Entity = Operator_Text and then Word = ":" then
               Has_Reference := True;
               Skip_Next_Identifier := False;
            elsif Entity = Operator_Text and then Word = "," then
               Skip_Next_Identifier := True;
            end if;
         end if;

         if Entity = Operator_Text then
            if Word = "(" then
               Paren_Depth := Paren_Depth + 1;
            elsif Word = ")" then
               Paren_Depth := Paren_Depth - 1;
            end if;
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
            if Construct.Name = null then
               Index_Begin := Construct.Sloc_Start.Index;
            else
               Index_Begin :=
                 Construct.Sloc_Entity.Index + Construct.Name'Length;
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

   -------------------------------
   -- Parse_Expression_Backward --
   -------------------------------

   overriding function Parse_Expression_Backward
     (Lang         : access Ada_Language;
      Buffer       : access Glib.UTF8_String;
      Start_Offset : Natural;
      End_Offset   : Natural := 0)
      return Parsed_Expression
   is
      pragma Unreferenced (Lang);
      use Token_List;

      Offset             : Natural := Start_Offset;
      Offset_Limit       : Natural;
      Token              : Token_Record;
      Result             : Parsed_Expression;
      Last_Token_On_Line : Token_List.List_Node;

      procedure Handle_Expression (Offset : in out Natural; Skip : Boolean);
      procedure Skip_String (Offset : in out Natural);
      procedure Skip_Comment_Line (Offset : in out Natural);
      procedure Push_Pckg (Offset : in out Natural);
      function Check_Prev_Word
        (Offset : Positive; Word : String)
         return Boolean;

      procedure Push (Token : in out Token_Record);
      procedure Pop;

      ---------------------
      -- Skip_Expression --
      ---------------------

      procedure Handle_Expression (Offset : in out Natural; Skip : Boolean) is
         Local_Token : Token_Record := Token;
      begin
         Local_Token.Token_Last := Offset;

         while Offset > Offset_Limit loop
            case Buffer (Offset) is
               when ')' =>
                  Offset := UTF8_Find_Prev_Char (Buffer.all, Offset);
                  Handle_Expression (Offset, True);

               when ',' =>
                  if not Skip then
                     Local_Token.Tok_Type := Tok_Expression;
                     Local_Token.Token_First := Offset + 1;
                     Push (Local_Token);
                     Local_Token.Token_Last := Offset - 1;
                  end if;

               when '"' =>
                  Offset := UTF8_Find_Prev_Char (Buffer.all, Offset);
                  Skip_String (Offset);

               when ''' =>
                  Offset := UTF8_Find_Prev_Char (Buffer.all, Offset);
                  Offset := UTF8_Find_Prev_Char (Buffer.all, Offset);

               when '(' =>
                  if not Skip then
                     Local_Token.Tok_Type := Tok_Expression;
                     Local_Token.Token_First := Offset + 1;
                     Push (Local_Token);

                     Local_Token.Tok_Type := Tok_Open_Parenthesis;
                     Push (Local_Token);
                  end if;

                  exit;

               when ASCII.LF =>
                  Skip_Comment_Line (Offset);

               when others =>
                  null;
            end case;

            Offset := UTF8_Find_Prev_Char (Buffer.all, Offset);

         end loop;
      end Handle_Expression;

      -----------------
      -- Skip_String --
      -----------------

      procedure Skip_String (Offset : in out Natural) is
      begin
         while Offset > Offset_Limit + 1 loop
            case Buffer (Offset) is
               when '"' =>
                  if Buffer (Offset - 1) /= '"' then
                     exit;
                  else
                     Offset := UTF8_Find_Prev_Char (Buffer.all, Offset);
                  end if;

               when ASCII.LF =>
                  --  Strings can't be on spread across multiple lines - this
                  --  probably means that the string is broken.
                  exit;

               when others =>
                  null;
            end case;

            Offset := UTF8_Find_Prev_Char (Buffer.all, Offset);

         end loop;
      end Skip_String;

      -----------------------
      -- Skip_Comment_Line --
      -----------------------

      procedure Skip_Comment_Line (Offset : in out Natural) is
         Local_Offset : Natural := Offset;
         Prev_Offset  : Natural;
      begin
         Local_Offset := UTF8_Find_Prev_Char (Buffer.all, Local_Offset);

         while Local_Offset > Offset_Limit loop
            case Buffer (Local_Offset) is
               when '"' =>
                  Local_Offset := UTF8_Find_Prev_Char
                    (Buffer.all, Local_Offset);
                  Skip_String (Local_Offset);

               when ''' =>
                  Local_Offset := UTF8_Find_Prev_Char
                    (Buffer.all, Local_Offset);
                  Local_Offset := UTF8_Find_Prev_Char
                    (Buffer.all, Local_Offset);

               when '-' =>
                  Prev_Offset := UTF8_Find_Prev_Char
                    (Buffer.all, Local_Offset);

                  if Prev_Offset > Offset_Limit
                    and then Buffer (Prev_Offset) = '-'
                  then
                     Local_Offset := UTF8_Find_Prev_Char
                       (Buffer.all, Prev_Offset);

                     --  ??? Not very efficient to use a recursive call here
                     Skip_Comment_Line (Local_Offset);
                     Offset := Local_Offset + 1;

                     exit;
                  end if;

               when ASCII.LF =>
                  exit;

               when others =>
                  null;
            end case;

            Local_Offset := UTF8_Find_Prev_Char
              (Buffer.all, Local_Offset);
         end loop;
      end Skip_Comment_Line;

      ---------------
      -- Push_Pckg --
      ---------------

      procedure Push_Pckg (Offset : in out Natural) is
      begin
         if Check_Prev_Word (Offset, "with") then
            Token.Tok_Type := Tok_With;
            Token.Token_Last := Offset;
            Offset := Offset - 3;
            Token.Token_First := Offset;
            Push (Token);
         elsif Check_Prev_Word (Offset, "use") then
            Token.Tok_Type := Tok_Use;
            Token.Token_Last := Offset;
            Offset := Offset - 2;
            Token.Token_First := Offset;
            Push (Token);
         end if;
      end Push_Pckg;

      ----------
      -- Push --
      ----------

      procedure Push (Token : in out Token_Record) is
         Name : constant String := Get_Name (Result, Token);
      begin
         --  Check if we're on a special keyword

         if Equal (Name, "all", False) then
            Token.Tok_Type := Tok_All;
         end if;

         if Token /= Null_Token then
            if Token.Token_First = 0 then
               Token.Token_First := Offset;
            end if;

            if Token.Token_Last = 0 then
               Token.Token_Last := Offset;
            end if;

            Prepend (Result.Tokens, Token);
            Token := Null_Token;
         end if;

         if Last_Token_On_Line = Token_List.Null_Node then
            Last_Token_On_Line := First (Result.Tokens);
         end if;
      end Push;

      ---------
      -- Pop --
      ---------

      procedure Pop is
      begin
         Remove_Nodes
           (Result.Tokens, Token_List.Null_Node, First (Result.Tokens));
      end Pop;

      ---------------------
      -- Check_Prev_Word --
      ---------------------

      function Check_Prev_Word (Offset : Positive; Word : String)
                                return Boolean is
      begin
         return Offset - (Word'Length - 1) > Offset_Limit
           and then To_Lower (Buffer (Offset - (Word'Length - 1) .. Offset))
           = To_Lower (Word)
           and then
             (Offset - Word'Length <= Offset_Limit
              or else Buffer (Offset - Word'Length) = ' '
              or else Buffer (Offset - Word'Length) = ASCII.LF
              or else Buffer (Offset - Word'Length) = ASCII.HT);
      end Check_Prev_Word;

      Blank_Here, Blank_Before : Boolean := False;
      Next_Ind                 : Natural;
      Possible_Arrow           : Boolean := False;

   begin
      Result.Original_Buffer := Buffer;

      if Buffer'Length = 0 or Offset > Buffer'Last then
         return Result;
      end if;

      if End_Offset < Buffer'First then
         Offset_Limit := Buffer'First - 1;
      else
         Offset_Limit := End_Offset - 1;
      end if;

      Skip_Comment_Line (Offset);

      if Offset /= Start_Offset then
         --  In this case, we are on a comment line. So the expression is
         --  empty.

         return Result;
      end if;

      while Offset > Offset_Limit loop

         Blank_Here := False;

         if Possible_Arrow and then Buffer (Offset) /= '=' then
            exit;
         end if;

         case Buffer (Offset) is
            when ',' =>
               Push (Token);

               if Length (Result.Tokens) = 0 then
                  Offset := UTF8_Find_Prev_Char (Buffer.all, Offset);
                  Handle_Expression (Offset, False);
               else
                  exit;
               end if;

            when ')' =>
               Push (Token);

               Token.Tok_Type := Tok_Close_Parenthesis;
               Push (Token);

               Offset := UTF8_Find_Prev_Char (Buffer.all, Offset);
               Handle_Expression (Offset, False);

            when '(' =>
               Push (Token);

               if Length (Result.Tokens) = 0 then
                  Token.Tok_Type := Tok_Open_Parenthesis;
                  Token.Token_First := Offset;
                  Token.Token_Last := Offset;
                  Push (Token);
               end if;

            when '.' =>
               Push (Token);

               if Length (Result.Tokens) > 0
                 and then Head (Result.Tokens).Tok_Type = Tok_Dot
               then
                  --  In this case, we have a range construct, like A .. B.
                  --  It's the end of the expression.

                  Pop;

                  exit;
               end if;

               Token.Tok_Type := Tok_Dot;
               Push (Token);

            when ' ' | ASCII.HT | ASCII.CR =>
               Push (Token);
               Blank_Here := True;

            when '"' =>
               if First (Result.Tokens) /= Token_List.Null_Node
                 and then
                   (Data (First (Result.Tokens)).Tok_Type
                    = Tok_Open_Parenthesis
                    or else Data (First (Result.Tokens)).Tok_Type
                    = Tok_Expression)
               then
                  --  We are in an operator symbol case

                  Token.Tok_Type := Tok_Identifier;
                  Token.Token_Last := Offset;

                  Offset := UTF8_Find_Prev_Char (Buffer.all, Offset);

                  Skip_String (Offset);

                  Token.Token_First := Offset;

                  Push (Token);
               else
                  exit;
               end if;

            when '>' =>
               Possible_Arrow := True;

            when '=' =>
               if Possible_Arrow then
                  Token.Tok_Type := Tok_Arrow;

                  Push (Token);

                  Possible_Arrow := False;
               else
                  exit;
               end if;

            when ASCII.LF =>
               Push (Token);
               Blank_Here := True;
               Skip_Comment_Line (Offset);

            when others =>
               Next_Ind := UTF8_Next_Char (Buffer.all, Offset);

               if (Next_Ind in Buffer'Range
                   and then
                     (Is_Alnum
                        (UTF8_Get_Char (Buffer (Offset .. Next_Ind)))))
                 or else
                   (Next_Ind not in Buffer'Range
                    and then Is_Alphanumeric (Buffer (Offset)))
                 or else Buffer (Offset) = '_'
               then
                  Token.Tok_Type := Tok_Identifier;
                  Token.Token_First := Offset;

                  if Token.Token_Last = 0 then
                     Token.Token_Last := Offset;

                     if Length (Result.Tokens) = 0 and then Blank_Before then
                        Token := Null_Token;
                        Push_Pckg (Offset);
                        exit;
                     end if;

                     if Length (Result.Tokens) > 0
                       and then Head (Result.Tokens).Tok_Type = Tok_Identifier
                     then
                        Push_Pckg (Offset);
                        Token := Null_Token;
                        exit;
                     end if;
                  end if;
               else
                  Push (Token);
                  exit;
               end if;

         end case;

         Offset := UTF8_Find_Prev_Char (Buffer.all, Offset);
         Blank_Before := Blank_Here;
      end loop;

      Push (Token);

      return Result;
   end Parse_Expression_Backward;

end Language.Ada;
