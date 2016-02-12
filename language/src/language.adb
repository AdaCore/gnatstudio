------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2016, AdaCore                     --
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

with Ada.Unchecked_Deallocation;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded;            use Ada.Strings.Unbounded;
with Ada.Wide_Wide_Characters.Unicode; use Ada.Wide_Wide_Characters.Unicode;
with Ada.Characters.Wide_Wide_Latin_1;
with GNAT.Expect;                 use GNAT.Expect;
with GNAT.Regpat;                 use GNAT.Regpat;
with GNATCOLL.Symbols;            use GNATCOLL.Symbols;
with GNATCOLL.Utils;              use GNATCOLL.Utils;
with String_Utils;                use String_Utils;
with UTF8_Utils;                  use UTF8_Utils;

package body Language is

   Default_Word_Character_Set : constant Character_Set :=
     Constants.Letter_Set or Constants.Decimal_Digit_Set or To_Set ("_");
   --  Default character set for keywords and indentifiers

   procedure Looking_At
     (Lang      : access Language_Root;
      Buffer    : String;
      First     : Natural;
      Entity    : out Language_Entity;
      Next_Char : out Positive;
      Line      : out Natural;
      Column    : out Natural);
   --  Internal version of Looking_At, which also returns the Line and Column,
   --  considering that Buffer (First) is at line 1 column 1.
   --  Column is a byte index, not a character index.

   ---------------------------
   -- Can_Tooltip_On_Entity --
   ---------------------------

   function Can_Tooltip_On_Entity
     (Lang   : access Language_Root;
      Entity : String) return Boolean
   is
      pragma Unreferenced (Lang, Entity);
   begin
      return True;
   end Can_Tooltip_On_Entity;

   ---------------------
   -- Scope_Separator --
   ---------------------

   function Scope_Separator
     (Lang : access Language_Root) return String
   is
      pragma Unreferenced (Lang);
   begin
      return ".";
   end Scope_Separator;

   ----------------------
   -- Explorer_Regexps --
   ----------------------

   function Explorer_Regexps
     (Lang : access Language_Root) return Explorer_Categories
   is
      pragma Unreferenced (Lang);
      E : Explorer_Categories (1 .. 0);
   begin
      return E;
   end Explorer_Regexps;

   ----------
   -- Free --
   ----------

   procedure Free (Context : in out Language_Context_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Language_Context, Language_Context_Access);

      Var : GNAT.Expect.Pattern_Matcher_Access :=
        GNAT.Expect.Pattern_Matcher_Access
          (Context.Syntax.New_Line_Comment_Start_Regexp);
   begin
      if Context /= null then
         GNAT.Strings.Free (Context.Syntax.Comment_Start);
         GNAT.Strings.Free (Context.Syntax.Comment_End);
         Basic_Types.Unchecked_Free (Var);
         GNAT.Strings.Free (Context.Syntax.New_Line_Comment_Start);
         Unchecked_Free (Context);
      end if;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Lang : in out Language_Access) is
      procedure Internal is new Ada.Unchecked_Deallocation
        (Language_Root'Class, Language_Access);
   begin
      if Lang /= null then
         Free (Lang.all);
         Internal (Lang);
      end if;
   end Free;

   procedure Free (List : in out Construct_List) is
      Info, Tmp : Construct_Access;

      procedure Free is new
        Ada.Unchecked_Deallocation (Construct_Information, Construct_Access);

   begin
      Info := List.First;

      loop
         exit when Info = null;

         GNAT.Strings.Free (Info.Profile);
         Tmp := Info;
         Info := Info.Next;
         Free (Tmp);
      end loop;

      List.First   := null;
      List.Current := null;
      List.Last    := null;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Category   : in out Explorer_Category) is
   begin
      Basic_Types.Unchecked_Free (Category.Regexp);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Categories : in out Explorer_Categories) is
   begin
      for C in Categories'Range loop
         Free (Categories (C));
      end loop;
   end Free;

   --------------------
   -- Is_System_File --
   --------------------

   function Is_System_File
     (Lang      : access Language_Root;
      File_Name : String) return Boolean
   is
      pragma Unreferenced (Lang, File_Name);
   begin
      return False;
   end Is_System_File;

   ----------------
   -- Looking_At --
   ----------------

   procedure Looking_At
     (Lang      : access Language_Root;
      Buffer    : String;
      First     : Natural;
      Entity    : out Language_Entity;
      Next_Char : out Positive)
   is
      Line, Column : Natural;
   begin
      Looking_At (Lang, Buffer, First, Entity, Next_Char, Line, Column);
   end Looking_At;

   procedure Looking_At
     (Lang      : access Language_Root;
      Buffer    : String;
      First     : Natural;
      Entity    : out Language_Entity;
      Next_Char : out Positive;
      Line      : out Natural;
      Column    : out Natural)
   is
      Context       : constant Language_Context_Access :=
                        Get_Language_Context (Language_Access (Lang));
      Keys          : constant GNAT.Expect.Pattern_Matcher_Access :=
                        Keywords (Language_Access (Lang));
      Buffer_Length : constant Natural := Buffer'Last - First + 1;
      Matched       : Match_Array (0 .. 1);
      C             : Wide_Wide_Character;
      Tmp           : Natural;
      Found         : Boolean;

      use GNAT.Strings;

   begin
      Line   := 1;
      Column := 1;

      if Buffer (First) = ASCII.LF then
         Next_Char := First + 1;
         Line := Line + 1;
         Column := 1;
         Entity := Normal_Text;
         return;
      end if;

      --  Do we have a comment ?

      if Context.Syntax.Comment_Start /= null
        and then Starts_With
          (Buffer (First .. Buffer'Last), Context.Syntax.Comment_Start.all)
      then
         Entity := Comment_Text;
         Next_Char := First + Context.Syntax.Comment_Start'Length;
         Column := Column + Context.Syntax.Comment_Start'Length;

         while Starts_With
           (Buffer (Next_Char .. Buffer'Last), Context.Syntax.Comment_End.all)
         loop
            Tmp := UTF8_Next_Char (Buffer, Next_Char);
            Column := Column + (Tmp - Next_Char);
            Next_Char := Tmp;

            if Next_Char <= Buffer'Last
              and then Buffer (Next_Char) = ASCII.LF
            then
               Column := 1;
               Line := Line + 1;
            end if;
         end loop;

         Next_Char := Next_Char + Context.Syntax.Comment_End'Length;
         Column := Column + Context.Syntax.Comment_End'Length;
         return;
      end if;

      --  Do we have a comment that ends on newline ?

      if Context.Syntax.New_Line_Comment_Start /= null then
         Found := Starts_With
           (Buffer (First .. Buffer'Last),
            Context.Syntax.New_Line_Comment_Start.all);
      elsif Context.Syntax.New_Line_Comment_Start_Regexp /= null then
         Found := Match (Context.Syntax.New_Line_Comment_Start_Regexp.all,
                         Buffer (First .. Buffer'Last));
      else
         Found := False;
      end if;

      if Found  then
         Entity := Comment_Text;
         Next_Char := UTF8_Next_Char (Buffer, First);

         while Next_Char <= Buffer'Last
           and then Buffer (Next_Char) /= ASCII.LF
         loop
            Tmp := UTF8_Next_Char (Buffer, Next_Char);
            Column := Column + (Tmp - Next_Char);
            Next_Char := Tmp;
         end loop;

         return;
      end if;

      --  Do we have a string ?
      --  Note that we consider that strings never span over multiple lines...

      if Buffer (First) = Context.String_Delimiter then
         Entity := String_Text;
         Next_Char := First;

         if Next_Char < Buffer'Last
           and then Buffer (Next_Char + 1) /= ASCII.LF
         then
            loop
               Next_Char := UTF8_Next_Char (Buffer, Next_Char);
               Column := Column + 1;

               exit when Next_Char >= Buffer'Last
                 or else Buffer (Next_Char + 1) = ASCII.LF
                 or else
                   (Buffer (Next_Char) = Context.String_Delimiter
                      and then
                        (Context.Quote_Character = ASCII.NUL
                         or else
                           Buffer (Next_Char - 1) /= Context.Quote_Character));
            end loop;
         end if;

         if Next_Char <= Buffer'Last then
            Tmp := UTF8_Next_Char (Buffer, Next_Char);
            Column := Column + (Tmp - Next_Char);
            Next_Char := Tmp;
         end if;

         return;
      end if;

      --  A protected constant character
      --  ??? The following test still does not handle cases such as
      --  '\012' for instance, or multi-byte character constants.

      if Buffer_Length > 4
        and then Buffer (First) = Context.Constant_Character
        and then Buffer (First + 1) = Context.Quote_Character
        and then Buffer (First + 3) = Context.Constant_Character
      then
         Entity := Character_Text;
         Next_Char := First + 4;
         Column := Column + 4;
         return;
      end if;

      --  A constant character

      if Buffer_Length > 3
        and then Buffer (First) = Context.Constant_Character
        and then Buffer (First + 2) = Context.Constant_Character
      then
         Entity := Character_Text;
         Next_Char := First + 3;
         Column := Column + 3;
         return;
      end if;

      --  Do we have a keyword ?
      --  ??? It is assumed the regexp should check at the current char
      --  only, not beyond for efficiency...

      if Keys /= null then
         Match (Keys.all, Buffer (First .. Buffer'Last), Matched);
         if Matched (0) /= No_Match then
            Next_Char := UTF8_Next_Char (Buffer, Matched (0).Last);
            Column := Column + Matched (0).Last - Matched (0).First + 1;
            Entity := Keyword_Text;
            return;
         end if;
      end if;

      --  Another special character, not part of a word: just skip it, before
      --  doing some regexp matching
      --  It is better to return a pointer to the newline, so that the icons
      --  on the side might be displayed properly.

      if not Is_Word_Char
        (Language_Access (Lang),
           UTF8_Get_Char (Buffer (First .. Buffer'Last)))
      then
         Entity := Normal_Text;
         Next_Char := UTF8_Next_Char (Buffer, First);
         Column := Column + (Next_Char - First);

         while Next_Char <= Buffer'Last loop
            C := UTF8_Get_Char (Buffer (Next_Char .. Buffer'Last));

            exit when C = Ada.Characters.Wide_Wide_Latin_1.LF
              or else not Is_Space (C);

            Tmp := UTF8_Next_Char (Buffer, Next_Char);
            Column := Column + (Tmp - Next_Char);
            Next_Char := Tmp;
         end loop;

         return;
      end if;

      --  Skip to the next meaningful character. we know we are
      --  starting with a letter

      Next_Char := UTF8_Next_Char (Buffer, First);
      Column := Column + (Next_Char - First);
      Entity := Normal_Text;

      if Buffer (Next_Char) = ASCII.LF then
         return;
      end if;

      --  Skip the current word. We only take into account Is_Entity_Letter,
      --  not the full set of chars supported by the language for its keywords
      --  because of cases like "<foo>bar</foo>" in XML, which would otherwise
      --  consider this as a single word when they are in fact several.

      while Next_Char <= Buffer'Last
        and then Is_Entity_Letter
          (UTF8_Get_Char (Buffer (Next_Char .. Buffer'Last)))
      loop
         Tmp := UTF8_Next_Char (Buffer, Next_Char);
         Column := Column + (Tmp - Next_Char);
         Next_Char := Tmp;
      end loop;
   end Looking_At;

   -------------------------------------
   -- To_Simple_Construct_Information --
   -------------------------------------

   procedure To_Simple_Construct_Information
     (Construct : Construct_Information;
      Simple    : out Simple_Construct_Information;
      Full_Copy : Boolean)
   is
      pragma Unreferenced (Full_Copy);
   begin
      Simple :=
        (Category        => Construct.Category,
         Is_Declaration  => Construct.Is_Declaration,
         Is_Generic_Spec => Construct.Is_Generic_Spec,
         Visibility      => Construct.Visibility,
         Name            => Construct.Name,
         Sloc_Start      => Construct.Sloc_Start,
         Sloc_Entity     => Construct.Sloc_Entity,
         Sloc_End        => Construct.Sloc_End,
         Attributes      => Construct.Attributes,
         Profile_Cache    => null);
   end To_Simple_Construct_Information;

   ------------------
   -- Comment_Line --
   ------------------

   function Comment_Line
     (Lang    : access Language_Root;
      Line    : String;
      Comment : Boolean := True;
      Clean   : Boolean := False) return String
   is
      pragma Unreferenced (Lang, Comment, Clean);
   begin
      return Line;
   end Comment_Line;

   ------------------
   -- Comment_Block --
   ------------------

   function Comment_Block
     (Lang    : access Language_Root;
      Block   : String;
      Comment : Boolean := True;
      Clean   : Boolean := False) return String
   is
      Start_Of_Line : Natural := Block'First;
      End_Of_Line   : Natural;
      New_Block     : Unbounded_String := Null_Unbounded_String;
   begin
      loop
         End_Of_Line := Next_Line (Block, Start_Of_Line);
         if End_Of_Line /= Block'Last and then End_Of_Line /= Block'First then
            End_Of_Line := End_Of_Line - 1;
         end if;

         Append
           (New_Block,
            Comment_Line
              (Language_Access (Lang),
               Block (Start_Of_Line .. End_Of_Line),
               Comment,
               Clean));

         Start_Of_Line := Next_Line (Block, Start_Of_Line);
         exit when Start_Of_Line = Block'Last;
      end loop;

      return To_String (New_Block);
   end Comment_Block;

   ----------------------
   -- Parse_Constructs --
   ----------------------

   procedure Parse_Constructs
     (Lang   : access Language_Root;
      File   : GNATCOLL.VFS.Virtual_File;
      Buffer : UTF8_String;
      Result : out Construct_List)
   is
      pragma Unreferenced (File);
      Matches     : Match_Array (0 .. 10);
      Categories  : constant Explorer_Categories :=
                      Explorer_Regexps (Language_Access (Lang));
      First       : Natural;
      Line        : Natural;
      Line_Pos    : Natural;
      Sloc_Entity : Source_Location;
      Sloc_Start  : Source_Location;
      Sloc_End    : Source_Location;
      Info        : Construct_Access;
      Match_Index : Natural;
      End_Index   : Natural;

      procedure Forward
        (Index : Natural;
         Sloc  : in out Source_Location);
      --  Compute Line and Column fields in Sloc and update Line and Line_Pos

      -------------
      -- Forward --
      -------------

      procedure Forward
        (Index : Natural;
         Sloc  : in out Source_Location) is
      begin
         for J in Index .. Sloc.Index loop
            if Buffer (J) = ASCII.LF then
               Line     := Line + 1;
               Line_Pos := J;
            end if;
         end loop;

         Sloc.Line   := Line;
         Sloc.Column := Sloc.Index - Line_Pos;
      end Forward;

   begin
      Result := (null, null, null, 0);

      --  For each category, parse the buffer

      for C in Categories'Range loop
         First    := Buffer'First;
         Line     := 1;
         Line_Pos := 0;

         loop
            Match (Categories (C).Regexp.all,
                   Buffer (First .. Buffer'Last),
                   Matches);

            exit when Matches (0) = No_Match;

            Match_Index := Categories (C).Position_Index;
            End_Index   := Categories (C).End_Index;

            if Matches (Match_Index) /= No_Match then
               Sloc_Start.Index  := Matches (0).First;
               Sloc_Entity.Index := Matches (Match_Index).First;
               Sloc_End.Index    := Matches (End_Index).Last;

               Forward (First, Sloc_Start);
               Forward (Sloc_Start.Index + 1, Sloc_Entity);
               Forward (Sloc_Entity.Index + 1, Sloc_End);

               Info           := Result.Current;
               Result.Current := new Construct_Information;

               if Result.First = null then
                  Result.First := Result.Current;
               else
                  Result.Current.Prev := Info;
                  Result.Current.Next := Info.Next;
                  Info.Next           := Result.Current;
               end if;

               Result.Last := Result.Current;
               Result.Current.Category := Categories (C).Category;
               Result.Current.Category_Name := Categories (C).Category_Name;
               Result.Size := Result.Size + 1;

               if Categories (C).Make_Entry /= null then
                  Result.Current.Name := Lang.Symbols.Find
                    (Categories (C).Make_Entry (Buffer, Matches));
               else
                  Result.Current.Name := Lang.Symbols.Find
                    (Buffer (Matches (Match_Index).First ..
                             Matches (Match_Index).Last));
               end if;

               --  Result.Current.Profile := ???

               Result.Current.Sloc_Entity    := Sloc_Entity;
               Result.Current.Sloc_Start     := Sloc_Start;
               Result.Current.Sloc_End       := Sloc_End;
               Result.Current.Is_Declaration := False;
            end if;

            First := Matches (End_Index).Last + 1;
         end loop;
      end loop;
   end Parse_Constructs;

   --------------------
   -- Parse_Entities --
   --------------------

   procedure Parse_Entities
     (Lang     : access Language_Root;
      Buffer   : String;
      Callback : Entity_Callback)
   is
      use type GNAT.Strings.String_Access;
      Index      : Natural := Buffer'First;
      Next_Char  : Natural;
      End_Char   : Natural;
      Entity     : Language_Entity;
      Line       : Natural;
      Line_Inc   : Natural;
      Col        : Natural;
      Column     : Natural;
      Column_Inc : Natural;

   begin
      Line := 1;
      Column := 1;

      while Index < Buffer'Last loop
         Looking_At
           (Lang, Buffer, Index, Entity, Next_Char, Line_Inc, Column_Inc);

         if Next_Char = Buffer'Last then
            End_Char := Buffer'Last;
         else
            End_Char := Next_Char - 1;
         end if;

         --  If we are still on the same line, Column_Inc is an increment
         --  compared to what we have initially, otherwise it is an absolute
         --  column.

         if Line_Inc = 1 then
            Column_Inc := Column + Column_Inc - 1;
         end if;

         --  Looking_At goes always one character beyond characters and
         --  strings, otherwise next call to Looking_At would start on
         --  a string or character delimiter. Keywords are also set one
         --  character beyond.

         if Column_Inc > 1
           and then (Entity = String_Text
                     or else Entity = Character_Text
                     or else Entity = Keyword_Text)
         then
            Col := Column_Inc - 1;
         else
            Col := Column_Inc;
         end if;

         exit when Callback
           (Entity,
            (Line, Column, Index),
            (Line + Line_Inc - 1, Col, End_Char),
            Get_Language_Context
              (Language_Access (Lang)).Syntax.Comment_Start /= null
              and then Entity = Comment_Text and then Next_Char > Buffer'Last);

         Line := Line + Line_Inc - 1;
         Column := Column_Inc;

         Index := Next_Char;
      end loop;
   end Parse_Entities;

   ---------------------------
   -- Get_Referenced_Entity --
   ---------------------------

   procedure Get_Referenced_Entity
     (Lang       : access Language_Root;
      Buffer     : String;
      Construct  : Simple_Construct_Information;
      Sloc_Start : out Source_Location;
      Sloc_End   : out Source_Location;
      Success    : out Boolean;
      From_Index : Natural := 0)
   is
      pragma Unreferenced
        (Lang, Buffer, Construct, Sloc_Start, Sloc_End, From_Index);
   begin
      Success := False;
   end Get_Referenced_Entity;

   -------------------
   -- Format_Buffer --
   -------------------

   procedure Format_Buffer
     (Lang                : access Language_Root;
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
      pragma Unreferenced
        (Lang, Indent_Offset, Case_Exceptions, Is_Optional_Keyword);

      Use_Tabs        : Boolean renames Indent_Params.Use_Tabs;
      Index           : Natural;
      Indent          : Natural := 0;
      Start_Of_Line   : Natural;
      Start_Prev_Line : Natural;

      function Find_Line_Start
        (Buffer : String; Index : Natural) return Natural;
      --  Find the starting ASCII.LF character of the line positioned at
      --  Buffer (Index).

      ---------------------
      -- Find_Line_Start --
      ---------------------

      function Find_Line_Start
        (Buffer : String; Index : Natural) return Natural
      is
         Result : Natural := Index;
      begin
         while Result > Buffer'First
           and then Buffer (Result) /= ASCII.LF
         loop
            Result := Result - 1;
         end loop;

         return Result;
      end Find_Line_Start;

   begin
      if Buffer'Length <= 1 or else To > From + 1 then
         return;
      end if;

      Start_Of_Line   := Find_Line_Start (Buffer, Buffer'Last - 1);
      Start_Prev_Line := Find_Line_Start (Buffer, Start_Of_Line - 1);

      --  Compute the indentation level

      for J in Start_Prev_Line + 1 .. Start_Of_Line - 1 loop
         if Buffer (J) = ' ' then
            Indent := Indent + 1;
         elsif Buffer (J) = ASCII.HT then
            Indent := Indent + Tab_Width - (Indent mod Tab_Width);
         else
            exit;
         end if;
      end loop;

      --  Find the blank slice to replace

      Index := Start_Of_Line + 1;

      while Index < Buffer'Last
        and then (Buffer (Index) = ' ' or else Buffer (Index) = ASCII.HT)
      loop
         Index := Index + 1;
      end loop;

      Replace
        (To, 1, Index - Start_Of_Line,
         Blank_Slice (Indent, Use_Tabs, Tab_Width));
   end Format_Buffer;

   -------------------
   -- Category_Name --
   -------------------

   function Category_Name
     (Category : Language.Language_Category;
      Name     : GNATCOLL.Symbols.Symbol := GNATCOLL.Symbols.No_Symbol)
      return String
   is
      use type Strings.String_Access;
   begin
      if Name /= No_Symbol then
         return Get (Name).all;
      end if;

      case Category is
         when Cat_Unknown               => return "";
         when Cat_Custom                => return "custom";
         when Cat_Package               => return "package";
         when Cat_Namespace             => return "namespace";
         when Cat_Task                  => return "task";
         when Cat_Procedure             => return "subprogram";
         when Cat_Function              => return "subprogram";
         when Cat_Method                => return "method";
         when Cat_Constructor           => return "constructor";
         when Cat_Destructor            => return "destructor";
         when Cat_Protected             => return "protected";
         when Cat_Entry                 => return "entry";
         when Cat_Class                 => return "class";
         when Cat_Structure             => return "structure";
         when Cat_Case_Inside_Record    => return "structure variant part";
         when Cat_Union                 => return "union";
         when Cat_Type                  => return "type";
         when Cat_Subtype               => return "subtype";
         when Cat_Variable              => return "variable";
         when Cat_Local_Variable        => return "variable";
         when Cat_Parameter             => return "parameter";
         when Cat_Discriminant          => return "discriminant";
         when Cat_Field                 => return "field";
         when Cat_Literal               => return "literal";
         when Cat_Representation_Clause => return "representation clause";
         when Cat_With                  => return "with";
         when Cat_Use                   => return "use";
         when Cat_Include               => return "include";
         when Construct_Category        => return "";
         when Cat_Exception_Handler     => return "";
         when Cat_Pragma                => return "pragma";
         when Cat_Aspect                => return "aspect";
      end case;
   end Category_Name;

   --------------------------------
   -- Get_Indentation_Parameters --
   --------------------------------

   procedure Get_Indentation_Parameters
     (Lang         : access Language_Root;
      Params       : out Indent_Parameters;
      Indent_Style : out Indentation_Kind) is
   begin
      Params       := Lang.Indent_Params;
      Indent_Style := Lang.Indent_Style;
   end Get_Indentation_Parameters;

   --------------------------------
   -- Set_Indentation_Parameters --
   --------------------------------

   procedure Set_Indentation_Parameters
     (Lang         : access Language_Root;
      Params       : Indent_Parameters;
      Indent_Style : Indentation_Kind) is
   begin
      Lang.Indent_Params := Params;
      Lang.Indent_Style  := Indent_Style;
   end Set_Indentation_Parameters;

   ----------
   -- Free --
   ----------

   procedure Free (Fields : in out Project_Field_Array) is
   begin
      for F in Fields'Range loop
         Strings.Free (Fields (F).Attribute_Name);
         Strings.Free (Fields (F).Attribute_Index);
         Strings.Free (Fields (F).Description);
      end loop;
   end Free;

   ------------------------
   -- Word_Character_Set --
   ------------------------

   function Word_Character_Set
     (Lang : access Language_Root)
      return Character_Set
   is
      pragma Unreferenced (Lang);
   begin
      return Default_Word_Character_Set;
   end Word_Character_Set;

   ------------------
   -- Is_Word_Char --
   ------------------

   function Is_Word_Char
     (Lang : access Language_Root; Char : Wide_Wide_Character) return Boolean
   is
      pragma Unreferenced (Lang);
   begin
      return Is_Entity_Letter (Char);
   end Is_Word_Char;

   ---------
   -- "=" --
   ---------

   overriding function "=" (S1, S2 : Source_Location) return Boolean is
   begin
      if S1.Index > 0 and then S2.Index > 0 then
         return S1.Index = S2.Index;
      else
         return S1.Line = S2.Line
           and then S1.Column = S2.Column;
      end if;
   end "=";

   ---------
   -- "<" --
   ---------

   function "<" (S1, S2 : Source_Location) return Boolean is
   begin
      if S1.Index > 0 and then S2.Index > 0 then
         return S1.Index < S2.Index;
      elsif S1.Line = S2.Line then
         return S1.Column < S2.Column;
      else
         return S1.Line < S2.Line;
      end if;
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<=" (S1, S2 : Source_Location) return Boolean is
   begin
      return S1 = S2 or else S1 < S2;
   end "<=";

   ---------
   -- ">" --
   ---------

   function ">" (S1, S2 : Source_Location) return Boolean is
   begin
      return S2 < S1;
   end ">";

   ----------
   -- ">=" --
   ----------

   function ">=" (S1, S2 : Source_Location) return Boolean is
   begin
      return not (S1 < S2);
   end ">=";

   ---------------------------
   -- Parse_Tokens_Backward --
   ---------------------------

   procedure Parse_Tokens_Backwards
     (Lang              : access Language_Root;
      Buffer            : UTF8_String;
      Start_Offset      : String_Index_Type;
      End_Offset        : String_Index_Type := 0;
      --   ??? This analysis should be done when looking for comments !!!
      Callback          :
      access procedure (Token : Token_Record;
                        Stop : in out Boolean))
   is
      pragma Unreferenced (Lang);
      Lowest : constant String_Index_Type :=
        String_Index_Type'Max (End_Offset, String_Index_Type (Buffer'First));
      Index  : String_Index_Type := Start_Offset;
      Stop   : Boolean := False;
   begin
      if Index not in Lowest .. String_Index_Type (Buffer'Last) then
         return;
      else
         Skip_Word
           (Buffer (Natural (Lowest) .. Natural (Index)),
            Natural (Index),
            Step => -1);

         Callback
           ((Tok_Type    => No_Token,
             Token_First => Index + 1,
             Token_Last  => Start_Offset),
            Stop);
      end if;
   end Parse_Tokens_Backwards;

   ------------------------------
   -- Parse_Reference_Backward --
   ------------------------------

   function Parse_Reference_Backwards
     (Lang              : access Language_Root;
      Buffer            : UTF8_String;
      Start_Offset      : String_Index_Type;
      End_Offset        : String_Index_Type := 0) return String
   is
      Buf_Start : Integer := 1;
      Buf_End   : Integer := 0;

      procedure Callback
        (Token : Token_Record;
         Stop  : in out Boolean);

      procedure Callback
        (Token : Token_Record;
         Stop  : in out Boolean)
      is
      begin
         Buf_End := Integer (Token.Token_Last);
         Buf_Start := Integer (Token.Token_First);
         Stop := True;
      end Callback;

   begin

      Lang.Parse_Tokens_Backwards
        (Buffer            => Buffer,
         Start_Offset      => Start_Offset,
         End_Offset        => End_Offset,
         Callback          => Callback'Access);

      return Buffer (Buf_Start .. Buf_End);
   end Parse_Reference_Backwards;

   -----------------
   -- Set_Symbols --
   -----------------

   procedure Set_Symbols
     (Self   : access Language_Root'Class;
      Symbols : not null access GNATCOLL.Symbols.Symbol_Table_Record'Class) is
   begin
      Self.Symbols := Symbol_Table_Access (Symbols);
   end Set_Symbols;

   -------------
   -- Symbols --
   -------------

   function Symbols
     (Self : access Language_Root'Class)
      return GNATCOLL.Symbols.Symbol_Table_Access is
   begin
      return Self.Symbols;
   end Symbols;

   ----------------------
   -- Entities_Indexed --
   ----------------------

   function Entities_Indexed (Self : Language_Root) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Entities_Indexed;

end Language;
