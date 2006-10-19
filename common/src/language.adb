-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2000-2006                      --
--                              AdaCore                              --
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

with Ada.Unchecked_Deallocation;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;

with Basic_Types;                 use Basic_Types;
with GNAT.Regpat;                 use GNAT.Regpat;
with String_Utils;                use String_Utils;
with Glib.Unicode;                use Glib, Glib.Unicode;

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

   type Comment_Type is
     (No_Comment, Comment_Single_Line, Comment_Multi_Line);
   function Looking_At_Start_Of_Comment
     (Context : Language_Context;
      Buffer  : String;
      Index   : Natural) return Comment_Type;
   --  Whether we have the start of a comment at Index in Buffer

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

   procedure Free (Lang : in out Language_Access) is
      procedure Internal is new Ada.Unchecked_Deallocation
        (Language_Root'Class, Language_Access);
   begin
      Internal (Lang);
   end Free;

   procedure Free (List : in out Construct_List) is
      Info, Tmp : Construct_Access;

      procedure Free is new
        Ada.Unchecked_Deallocation (Construct_Information, Construct_Access);

   begin
      Info := List.First;

      loop
         exit when Info = null;

         Free (Info.Name);
         Free (Info.Profile);
         Tmp := Info;
         Info := Info.Next;
         Free (Tmp);
      end loop;

      List.First   := null;
      List.Current := null;
      List.Last    := null;
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
      Matched       : Match_Array (0 .. 1);
      Context       : constant Language_Context_Access :=
        Get_Language_Context (Language_Access (Lang));
      Keys          : constant Pattern_Matcher_Access :=
        Keywords (Language_Access (Lang));
      C             : Gunichar;
      Buffer_Length : constant Natural := Buffer'Last - First + 1;
      Tmp           : Natural;
      Found         : Boolean;

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

      if Context.Comment_Start_Length /= 0
        and then Buffer_Length > Context.Comment_Start_Length
        and then Buffer
          (First .. First + Context.Comment_Start_Length - 1)
           = Context.Comment_Start
      then
         Entity := Comment_Text;
         Next_Char := First + Context.Comment_Start_Length;
         Column := Column + Context.Comment_Start_Length;

         while Next_Char + Context.Comment_End_Length - 1 <= Buffer'Last
           and then Buffer
             (Next_Char .. Next_Char + Context.Comment_End_Length - 1)
           /= Context.Comment_End
         loop
            Tmp := UTF8_Next_Char (Buffer, Next_Char);
            Column := Column + (Tmp - Next_Char);
            Next_Char := Tmp;

            if Buffer (Next_Char) = ASCII.LF then
               Column := 1;
               Line := Line + 1;
            end if;
         end loop;

         Next_Char := Next_Char + Context.Comment_End_Length;
         Column := Column + Context.Comment_End_Length;
         return;
      end if;

      --  Do we have a comment that ends on newline ?

      if Context.New_Line_Comment_Start /= null then
         Found := Context.New_Line_Comment_Start.all =
           Buffer (First .. First + Context.New_Line_Comment_Start'Length - 1);
      else
         Found := Match (Context.New_Line_Comment_Start_Regexp.all,
                         Buffer (First .. Buffer'Last));
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

      Match (Keys.all, Buffer (First .. Buffer'Last), Matched);

      if Matched (0) /= No_Match then
         Next_Char := UTF8_Next_Char (Buffer, Matched (0).Last);
         Column := Column + Matched (0).Last - Matched (0).First + 1;
         Entity := Keyword_Text;
         return;
      end if;

      --  Another special character, not part of a word: just skip it, before
      --  doing some regexp matching
      --  It is better to return a pointer to the newline, so that the icons
      --  on the side might be displayed properly.

      if not Is_Entity_Letter
        (UTF8_Get_Char (Buffer (First .. Buffer'Last)))
      then
         Entity := Normal_Text;
         Next_Char := UTF8_Next_Char (Buffer, First);
         Column := Column + (Next_Char - First);

         while Next_Char <= Buffer'Last loop
            C := UTF8_Get_Char (Buffer (Next_Char .. Buffer'Last));

            exit when C = Character'Pos (ASCII.LF)
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

      --  Skip the current word

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

   function To_Simple_Construct_Information
     (Construct : Construct_Information; Full_Copy : Boolean)
      return Simple_Construct_Information
   is
      Simple : Simple_Construct_Information :=
        (Category       => Construct.Category,
         Is_Declaration => Construct.Is_Declaration,
         Visibility     => Construct.Visibility,
         Name           => Construct.Name,
         Sloc_Start     => Construct.Sloc_Start,
         Sloc_Entity    => Construct.Sloc_Entity,
         Sloc_End       => Construct.Sloc_End);
   begin
      if Full_Copy then
         if Construct.Name /= null then
            Simple.Name := new String'(Simple.Name.all);
         end if;
      end if;

      return Simple;
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
     (Lang          : access Language_Root;
      Buffer        : String;
      Result        : out Construct_List)
   is
      Matches        : Match_Array (0 .. 10);
      Categories     : constant Explorer_Categories :=
        Explorer_Regexps (Language_Access (Lang));
      First          : Natural;
      Line           : Natural;
      Line_Pos       : Natural;
      Sloc_Entity    : Source_Location;
      Sloc_Start     : Source_Location;
      Sloc_End       : Source_Location;
      Info           : Construct_Access;
      Match_Index    : Natural;

      procedure Forward
        (Index : Natural;
         Sloc  : in out Source_Location);
      --  Compute Line and Column fields in Sloc and update Line and Line_Pos

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
      Result := (null, null, null);

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

            if Matches (Match_Index) /= No_Match then
               Sloc_Start.Index  := Matches (0).First;
               Sloc_Entity.Index := Matches (Match_Index).First;
               Sloc_End.Index    := Matches (0).Last;

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

               if Categories (C).Make_Entry /= null then
                  Result.Current.Name := new String'
                    (Categories (C).Make_Entry (Buffer, Matches));
               else
                  Result.Current.Name := new String'
                    (Buffer (Matches (Match_Index).First ..
                             Matches (Match_Index).Last));
               end if;

               --  Result.Current.Profile := ???

               Result.Current.Sloc_Entity    := Sloc_Entity;
               Result.Current.Sloc_Start     := Sloc_Start;
               Result.Current.Sloc_End       := Sloc_End;
               Result.Current.Is_Declaration := False;
            end if;

            First := Matches (0).Last + 1;
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
         --  a string or character delimiter. Keywors are also set one
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
            Entity = Comment_Text and then Next_Char > Buffer'Last);

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
     (Lang            : access Language_Root;
      Buffer          : String;
      Replace         : Replace_Text_Callback;
      From, To        : Natural := 0;
      Indent_Params   : Indent_Parameters := Default_Indent_Parameters;
      Indent_Offset   : Natural := 0;
      Case_Exceptions : Case_Handling.Casing_Exceptions :=
        Case_Handling.No_Casing_Exception)
   is
      pragma Unreferenced (Lang, Indent_Offset, Case_Exceptions);

      Tab_Width       : Natural renames Indent_Params.Tab_Width;
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

   function Category_Name (Category : Language_Category) return String is
   begin
      case Category is
         when Cat_Unknown               => return "";
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
         when Cat_Union                 => return "union";
         when Cat_Type                  => return "type";
         when Cat_Subtype               => return "subtype";
         when Cat_Variable              => return "variable";
         when Cat_Local_Variable        => return "variable";
         when Cat_Parameter             => return "parameter";
         when Cat_Field                 => return "field";
         when Cat_Literal               => return "literal";
         when Cat_Representation_Clause => return "representation clause";
         when Cat_With                  => return "with";
         when Cat_Use                   => return "use";
         when Cat_Include               => return "include";
         when Cat_Loop_Statement        => return "";
         when Cat_If_Statement          => return "";
         when Cat_Case_Statement        => return "";
         when Cat_Select_Statement      => return "";
         when Cat_Accept_Statement      => return "";
         when Cat_Declare_Block         => return "";
         when Cat_Simple_Block          => return "";
         when Cat_Exception_Handler     => return "";
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
         Free (Fields (F).Attribute_Name);
         Free (Fields (F).Attribute_Index);
         Free (Fields (F).Description);
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

   ---------------------------------
   -- Looking_At_Start_Of_Comment --
   ---------------------------------

   function Looking_At_Start_Of_Comment
     (Context : Language_Context;
      Buffer  : String;
      Index   : Natural) return Comment_Type
   is
   begin
      if Context.New_Line_Comment_Start /= null
        and then Index + Context.New_Line_Comment_Start'Length <= Buffer'Last
        and then Buffer
          (Index .. Index + Context.New_Line_Comment_Start'Length - 1) =
          Context.New_Line_Comment_Start.all
      then
         return Comment_Single_Line;
      end if;

      if Context.New_Line_Comment_Start_Regexp /= null
        and then Match (Context.New_Line_Comment_Start_Regexp.all,
                        Buffer, Data_First => Index)
      then
         return Comment_Single_Line;
      end if;

      if Context.Comment_Start_Length /= 0
        and then Index + Context.Comment_Start_Length <= Buffer'Last
        and then Buffer (Index .. Index + Context.Comment_Start_Length - 1)
        = Context.Comment_Start
      then
         return Comment_Multi_Line;
      end if;

      return No_Comment;
   end Looking_At_Start_Of_Comment;

   -----------------------------------------
   -- Skip_To_Current_Comment_Block_Start --
   -----------------------------------------

   procedure Skip_To_Current_Comment_Block_Start
     (Context : Language_Context;
      Buffer  : String;
      Index   : in out Natural)
   is
      Initial_Index : constant Natural := Index;
      Lines_Skipped : Natural;
      Tmp           : Integer;

      function Only_Blanks_Before
        (Buffer : String;
         Index  : Natural)
         return Boolean;
      --  Return True if there are only blanks characters before the one
      --  pointed by Index in Buffer.
      --  Return False otherwise.

      ------------------------
      -- Only_Blanks_Before --
      ------------------------

      function Only_Blanks_Before
        (Buffer : String;
         Index  : Natural)
         return Boolean
      is
         Tmp : Natural := Index - 1;
      begin
         Skip_Blanks (Buffer, Tmp, -1);
         return Buffer'First = Tmp + 1;
      end Only_Blanks_Before;

   begin
      --  Are we in a multi-line comment ?

      if Context.Comment_End_Length /= 0 then
         Tmp := Line_End (Buffer, Index);

         if Tmp - Context.Comment_End_Length + 1 >= Index
           and then Buffer
             (Tmp - Context.Comment_End_Length + 1 .. Tmp) =
             Context.Comment_End
         then -- The end of a multi-line comment has been found
            while Index >= Buffer'First
              and then Buffer
                (Index .. Index + Context.Comment_Start_Length - 1) /=
                Context.Comment_Start
            loop
               Index := Index - 1;
            end loop;

            if Looking_At_Start_Of_Comment (Context, Buffer, Index) =
              Comment_Multi_Line
            then -- The beginning of a multi-line comment has been found
               return;
            end if;
         end if;
      end if;

      --  Check for single line comments

      Tmp := Initial_Index;

      loop
         while Tmp <= Buffer'Last
           and then (Buffer (Tmp) = ' ' or else Buffer (Tmp) = ASCII.HT)
         loop
            Tmp := Tmp + 1;
         end loop;

         exit when Looking_At_Start_Of_Comment (Context, Buffer, Tmp) =
           No_Comment;

         Index := Tmp;

         exit when Only_Blanks_Before (Buffer, Tmp);

         Skip_Lines (Buffer, -1, Tmp, Lines_Skipped);

         exit when Lines_Skipped /= 1;
      end loop;

      if Looking_At_Start_Of_Comment (Context, Buffer, Index) = No_Comment then
         Index := 0;
      end if;
   end Skip_To_Current_Comment_Block_Start;

   ---------------------------------------
   -- Skip_To_Current_Comment_Block_End --
   ---------------------------------------

   procedure Skip_To_Current_Comment_Block_End
     (Context            : Language_Context;
      Buffer             : String;
      Index              : in out Natural;
      Ignore_Blank_Lines : Boolean := False)
   is
      Last_Comment_Index : Integer := Index;
      Typ                : Comment_Type;
      Lines_Skipped      : Natural;
   begin
      Block_Iteration : loop
         Typ := Looking_At_Start_Of_Comment (Context, Buffer, Index);

         case Typ is
         when No_Comment =>
            Index := Last_Comment_Index;
            exit Block_Iteration;

         when Comment_Single_Line =>
            Index := Line_End (Buffer, Index);

            declare
               Tmp : Integer := Index;
            begin
               loop
                  Skip_Lines (Buffer, 1, Tmp, Lines_Skipped);

                  exit when Lines_Skipped /= 1;

                  while Tmp <= Buffer'Last
                    and then (Buffer (Tmp) = ' ' or Buffer (Tmp) = ASCII.HT)
                  loop
                     Tmp := Tmp + 1;
                  end loop;

                  exit when
                    Looking_At_Start_Of_Comment (Context, Buffer, Tmp) =
                    No_Comment;

                  Index := Tmp;
               end loop;
            end;

         when Comment_Multi_Line =>
            Skip_To_String (Buffer, Index, Context.Comment_End);

         end case;

         if Ignore_Blank_Lines then
            Last_Comment_Index := Index;
            Skip_Lines (Buffer, 1, Index, Lines_Skipped);

            exit Block_Iteration when Lines_Skipped /= 1;

            Skip_Blanks (Buffer, Index);
         else
            exit Block_Iteration;
         end if;

      end loop Block_Iteration;

   end Skip_To_Current_Comment_Block_End;

   --------------------------------
   -- Skip_To_Next_Comment_Start --
   --------------------------------

   procedure Skip_To_Next_Comment_Start
     (Context : Language_Context;
      Buffer  : String;
      Index   : in out Natural)
   is
      Lines_Skipped : Natural;
   begin
      while Index < Buffer'Last loop
         Skip_Lines (Buffer, 1, Index, Lines_Skipped);

         exit when Lines_Skipped /= 1 or else Is_Blank_Line (Buffer, Index);

         Skip_Blanks (Buffer, Index);

         if Looking_At_Start_Of_Comment (Context, Buffer, Index) /=
           No_Comment
         then
            return;
         end if;
      end loop;

      Index := 0;
   end Skip_To_Next_Comment_Start;

   ------------------------------------
   -- Skip_To_Previous_Comment_Start --
   ------------------------------------

   procedure Skip_To_Previous_Comment_Start
     (Context : Language_Context;
      Buffer  : String;
      Index   : in out Natural)
   is
      Lines_Skipped : Natural;
   begin
      loop
         Skip_Lines (Buffer, -1, Index, Lines_Skipped);

         exit when Lines_Skipped /= 1 or else Is_Blank_Line (Buffer, Index);

         Skip_Blanks (Buffer, Index);

         if Looking_At_Start_Of_Comment (Context, Buffer, Index) /=
           No_Comment
         then
            Skip_To_Current_Comment_Block_Start (Context, Buffer, Index);
            return;
         end if;
      end loop;

      Index := 0;
   end Skip_To_Previous_Comment_Start;

end Language;
