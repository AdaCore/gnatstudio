-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2002                         --
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

with GNAT.Regpat; use GNAT.Regpat;
with Language;    use Language;

package body Codefix.Errors_Parser is

   -------------------
   -- Get_Solutions --
   -------------------

   procedure Get_Solutions
     (Current_Text : Text_Navigator_Abstr'Class;
      Errors_List  : in out Errors_Interface'Class;
      Message      : Error_Message;
      Category     : out Dynamic_String;
      Solutions    : out Solution_List)
   is
      Current_Node : Parser_List.List_Node;
      Success      : Boolean := False;
   begin
      Current_Node := First (General_Parse_List);

      while Current_Node /= Parser_List.Null_Node loop
         if Get_Error_State
           (General_Preferences_List,
            Data (Current_Node).Category.all) /= Disabled
         then
            Fix
              (Data (Current_Node).all,
               Errors_List,
               Current_Text,
               Message,
               Solutions,
               Success);
         end if;

         if Success then
            Assign (Category, Data (Current_Node).Category);
            exit;
         end if;

         Current_Node := Next (Current_Node);
      end loop;

   end Get_Solutions;

   ----------------
   -- Add_Parser --
   ----------------

   procedure Add_Parser (New_Parser : Ptr_Parser) is
   begin
      Append (General_Parse_List, New_Parser);
   end Add_Parser;

   ------------------
   -- Free_Parsers --
   ------------------

   procedure Free_Parsers is
   begin
      Free (General_Parse_List);
   end Free_Parsers;

   ----------
   -- Free --
   ----------

   procedure Free (Data : in out Ptr_Parser) is
      procedure Delete is new
         Ada.Unchecked_Deallocation (Error_Parser'Class, Ptr_Parser);
   begin
      Free (Data.all);
      Delete (Data);
   end Free;

   ------------------------
   -- Initialize_Parsers --
   ------------------------

   procedure Initialize_Parsers is
      Current_Node : Parser_List.List_Node;
   begin
      Current_Node := First (General_Parse_List);

      while Current_Node /= Parser_List.Null_Node loop
         Initialize (Data (Current_Node).all);
         Current_Node := Next (Current_Node);
      end loop;
   end Initialize_Parsers;

   ----------------------------------------------------------------------------
   --  Error_Parser
   ----------------------------------------------------------------------------

   ---------
   -- Fix --
   ---------

   procedure Fix
     (This         : Error_Parser'Class;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Success      : out Boolean) is
   begin
      for J in This.Matcher'Range loop
         declare
            Matches : Match_Array (0 .. Paren_Count (This.Matcher (J).all));
         begin
            Match (This.Matcher (J).all, Get_Message (Message), Matches);
            if Matches (0) /= No_Match then
               This.Current_It.all := J;
               Fix
                 (This,
                  Errors_List,
                  Current_Text,
                  Message,
                  Solutions,
                  Matches);
               Success := True;
               return;
            end if;
         exception
            when Uncorrectable_Message =>
               null;
         end;
      end loop;

      Success := False;
   end Fix;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Error_Parser) is
   begin
      Free (This.Current_It);

      for J in This.Matcher'Range loop
         Free (This.Matcher (J));
      end loop;
   end Free;

   --  The following functions are not commented after their name, but after
   --  the first parameter.

   --------------------------
   -- Agregate_Misspelling --
   --------------------------

   procedure Initialize (This : in out Agregate_Misspelling) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
        (Compile ("misspelling of ""=>""")));
   end Initialize;

   procedure Fix
     (This         : Agregate_Misspelling;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Matches, Errors_List);
   begin
      Solutions := Should_Be (Current_Text, Message, "=>", "=");
   end Fix;

   -----------------------
   -- Ligth_Misspelling --
   -----------------------

   procedure Initialize (This : in out Ligth_Misspelling) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
        (Compile ("misspelling of ""([^""]+)""$")));
   end Initialize;

   procedure Fix
     (This         : Ligth_Misspelling;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Errors_List);
   begin
      Solutions := Should_Be
        (Current_Text,
         Message,
         Get_Message (Message) (Matches (1).First .. Matches (1).Last));
   end Fix;

   ------------------------
   -- Double_Misspelling --
   ------------------------

   procedure Initialize (This : in out Double_Misspelling) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
        (Compile ("misspelling of ""([^""]+)"" or ""([^""]+)""")));
   end Initialize;

   procedure Fix
     (This         : Double_Misspelling;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Errors_List);
   begin
      Solutions := Should_Be
        (Current_Text,
         Message,
         Get_Message (Message) (Matches (1).First .. Matches (1).Last));

      Concat
        (Solutions,
         Should_Be
           (Current_Text,
            Message,
            Get_Message (Message) (Matches (2).First .. Matches (2).Last)));
   end Fix;

   ---------------------
   -- Goto_Mispelling --
   ---------------------

   procedure Initialize (This : in out Goto_Misspelling) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
        (Compile ("goto is one word")));
   end Initialize;

   procedure Fix
     (This         : Goto_Misspelling;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Errors_List);
      pragma Unreferenced (Matches);
   begin
      Solutions := Should_Be
        (Current_Text, Message, "goto", "(go[\s]+to)", Regular_Expression);
   end Fix;

   -------------------------
   -- Library_Misspelling --
   -------------------------

   procedure Initialize (This : in out Library_Misspelling) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
        (Compile ("""([^""]+)"" is not a predefined library unit")));
   end Initialize;

   procedure Free (This : in out Library_Misspelling) is
   begin
      Free (Error_Parser (This));
      Free (This.Misspelling_Matcher);
   end Free;

   procedure Fix
     (This         : Library_Misspelling;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      Preview     : Error_Message;
      Fix_Matches : Match_Array (0 .. 1);
   begin
      Get_Preview (Errors_List, Current_Text, Preview);
      Match (This.Misspelling_Matcher.all, Get_Message (Preview), Fix_Matches);

      if Fix_Matches (0) = No_Match then
         raise Uncorrectable_Message;
      end if;

      Solutions := Should_Be
        (Current_Text,
         Message,
         Get_Message (Preview)
           (Fix_Matches (1).First .. Fix_Matches (1).Last),
         Get_Message (Message) (Matches (1).First .. Matches (1).Last),
         Text_Ascii);

      Get_Message (Errors_List, Current_Text, Preview);

   end Fix;

   -----------------------
   -- Sth_Should_Be_Sth --
   -----------------------

   procedure Initialize (This : in out Sth_Should_Be_Sth) is
   begin
      This.Matcher :=
        (new Pattern_Matcher'
           (Compile ("""([^""]+)"" should be ""([^""]+)""")),
         new Pattern_Matcher'
           (Compile ("([^\w\s][^\s]*) should be ([^\w\s][^\s]*)")),
         new Pattern_Matcher'
           (Compile ("""([^""])+"" illegal here, replaced by ""([^""])+""")));
   end Initialize;

   procedure Fix
     (This         : Sth_Should_Be_Sth;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Errors_List);
   begin
      Solutions := Should_Be
        (Current_Text,
         Message,
         Get_Message (Message) (Matches (2).First .. Matches (2).Last),
         Get_Message (Message) (Matches (1).First .. Matches (1).Last),
         Text_Ascii);
   end Fix;

   -------------------------
   -- Should_Be_Semicolon --
   -------------------------

   procedure Initialize (This : in out Should_Be_Semicolon) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
        (Compile ("period should probably be semicolon")));
   end Initialize;

   procedure Fix
     (This         : Should_Be_Semicolon;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Errors_List, Matches);
   begin
      Solutions := Should_Be (Current_Text, Message, ";", ".", Text_Ascii);
   end Fix;

   ---------------
   -- And_Meant --
   ---------------

   procedure Initialize (This : in out And_Meant) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
        (Compile ("maybe ""and"" was meant")));
   end Initialize;

   procedure Fix
     (This         : And_Meant;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Errors_List, Matches);
   begin
      Solutions := Should_Be (Current_Text, Message, "and", "&");
   end Fix;

   --------------
   -- Or_Meant --
   --------------

   procedure Initialize (This : in out Or_Meant) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
        (Compile ("did you mean ""or""")));
   end Initialize;

   procedure Fix
     (This         : Or_Meant;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Errors_List, Matches);
   begin
      Solutions := Should_Be (Current_Text, Message, "or", "\|");
   end Fix;


   -------------------
   -- Bad_End_Block --
   -------------------

   procedure Initialize (This : in out Bad_End_Block) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
        (Compile ("""(end [\w]+;)"" expected in column [\d]+")));
   end Initialize;

   procedure Fix
     (This         : Bad_End_Block;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Errors_List);
   begin
      Solutions := Should_Be
        (Current_Text,
         Message,
         Get_Message (Message) (Matches (1).First .. Matches (1).Last),
         "([^;]+;)",
         Regular_Expression);
   end Fix;

   ----------------------------
   -- Unqualified_Expression --
   ----------------------------

   procedure Initialize (This : in out Unqualified_Expression) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
        (Compile ("if qualified expression was meant")));
   end Initialize;

   procedure Fix
     (This         : Unqualified_Expression;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Errors_List, Matches);
   begin
      Solutions := Should_Be
        (Current_Text,
         Message,
         "'(",
         "(",
         Text_Ascii,
         "Replace conversion by qualification");
   end Fix;

   -----------------
   -- Goes_Before --
   -----------------

   procedure Initialize (This : in out Goes_Before) is
   begin
      This.Matcher :=
        (new Pattern_Matcher'
           (Compile ("""([\w]+)"" goes before ""([\w]+)""")),
         new Pattern_Matcher'
           (Compile ("""([\w]+)"" must come before ""([\w]+)""")),
         new Pattern_Matcher'
           (Compile ("""([\w]+)"" should be before ""([\w]+)""")),
         new Pattern_Matcher'
           (Compile ("""([\w]+)"" must preceed ""([\w]+)""")));
   end Initialize;

   procedure Fix
     (This         : Goes_Before;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Errors_List);
   begin
      Solutions := Wrong_Order
        (Current_Text,
         Message,
         Get_Message (Message) (Matches (1).First .. Matches (1).Last),
         Get_Message (Message) (Matches (2).First .. Matches (2).Last));
   end Fix;

   --------------------
   -- Sth_Expected_3 --
   --------------------

   procedure Initialize (This : in out Sth_Expected_3) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
        (Compile ("""function"", ""procedure"" or ""package"" expected")));
   end Initialize;

   procedure Fix
     (This         : Sth_Expected_3;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Errors_List);
      pragma Unreferenced (Matches);
   begin
      Solutions := Expected (Current_Text, Message, "function");
      Concat (Solutions, Expected (Current_Text, Message, "procedure"));
      Concat (Solutions, Expected (Current_Text, Message, "package"));
   end Fix;

   --------------------
   -- Sth_Expected_2 --
   --------------------

   procedure Initialize (This : in out Sth_Expected_2) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
        (Compile ("""function"" or ""procedure"" expected")));
   end Initialize;

   procedure Fix
     (This         : Sth_Expected_2;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Errors_List);
      pragma Unreferenced (Matches);
   begin
      Solutions := Expected (Current_Text, Message, "function");
      Concat (Solutions, Expected (Current_Text, Message, "procedure"));
   end Fix;

   ------------------
   -- Sth_Expected --
   ------------------

   procedure Initialize (This : in out Sth_Expected) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
        (Compile ("""([\w]+)"" expected")));
   end Initialize;

   procedure Fix
     (This         : Sth_Expected;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Errors_List);
   begin
      Solutions := Expected
        (Current_Text,
         Message,
         Get_Message (Message) (Matches (1).First .. Matches (1).Last));
   end Fix;

   ----------------
   -- Missing_Kw --
   ----------------

   procedure Initialize (This : in out Missing_Kw) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
        (Compile ("missing ""(\w+)""")));
   end Initialize;

   procedure Fix
     (This         : Missing_Kw;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Errors_List);

      Str_Red         : Dynamic_String;
   begin

      Assign
        (Str_Red,
         Get_Message (Message) (Matches (1).First .. Matches (1).Last));

      if Str_Red.all = "return" or else Str_Red.all = "RETURN"
        or else Str_Red.all = "begin" or else Str_Red.all = "BEGIN"
      then
         raise Uncorrectable_Message;
      end if;

      Solutions := Expected
        (Current_Text,
         Message,
         Get_Message (Message) (Matches (1).First .. Matches (1).Last));
   end Fix;

   -----------------
   -- Missing_Sep --
   -----------------

   procedure Free (This : in out Missing_Sep) is
   begin
      Free (This.Wrong_Form);
      Free (Error_Parser (This));
   end Free;

   procedure Initialize (This : in out Missing_Sep) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
        (Compile ("missing ""([^""\w]+)""")));
   end Initialize;

   procedure Fix
     (This         : Missing_Sep;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (Errors_List);
      Wrong_Matches : Match_Array (0 .. 1);
   begin
      Match
        (This.Wrong_Form.all,
         Get_Message (Message) (Matches (1).First .. Matches (1).Last),
         Wrong_Matches);

      if Wrong_Matches (0) /= No_Match then
         raise Uncorrectable_Message;
      end if;

      Solutions := Expected
        (Current_Text,
         Message,
         Get_Message (Message) (Matches (1).First .. Matches (1).Last),
         False);
   end Fix;

   -----------------
   -- Missing_All --
   -----------------

   procedure Free (This : in out Missing_All) is
   begin
      Free (This.Col_Matcher);
      Free (Error_Parser (This));
   end Free;


   procedure Initialize (This : in out Missing_All) is
   begin
      This.Matcher := (new Pattern_Matcher'
        (Compile ("add ""all"" to type ""[\w]+"" defined at (line) ([0-9]+)")),
       new Pattern_Matcher'(Compile
        ("add ""all"" to type ""[\w]+"" defined at ([^:]+):([0-9]+)")));
   end Initialize;

   procedure Fix
     (This         : Missing_All;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (Errors_List);

      Col_Matches        : Match_Array (0 .. 1);
      Declaration_Line   : Positive;
      Line_Red           : Dynamic_String;
      Declaration_Cursor : File_Cursor;

   begin

      if
        Get_Message (Message) (Matches (1).First .. Matches (1).Last) /= "line"
      then
         Assign
           (Declaration_Cursor.File_Name,
            Get_Message (Message) (Matches (1).First .. Matches (1).Last));
      else
         Assign
           (Declaration_Cursor.File_Name, Message.File_Name);
      end if;

      Declaration_Line :=
        Positive'Value
          (Get_Message (Message) (Matches (2).First .. Matches (2).Last));

      Declaration_Cursor.Line := Declaration_Line;
      Declaration_Cursor.Col := 1;
      Assign (Line_Red, Get_Line (Current_Text, Declaration_Cursor));

      Match (This.Col_Matcher.all, Line_Red.all, Col_Matches);

      if Col_Matches (0) = No_Match then
         raise Uncorrectable_Message;
      end if;

      Declaration_Cursor.Col := Col_Matches (1).Last + 1;

      Solutions := Expected (Current_Text, Declaration_Cursor, "all");

      Free (Declaration_Cursor);
      Free (Line_Red);
   end Fix;

   -----------------------
   -- Statement_Missing --
   -----------------------

   procedure Initialize (This : in out Statement_Missing) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
        (Compile ("statement expected")));
   end Initialize;

   procedure Fix
     (This         : Statement_Missing;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Errors_List, Matches);
   begin
      Solutions := Expected
        (Current_Text, Message, "null;", Position => Before);
   end Fix;

   -------------------
   -- Space_Missing --
   -------------------

   procedure Initialize (This : in out Space_Missing) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'(Compile ("space required")));
   end Initialize;

   procedure Fix
     (This         : Space_Missing;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Errors_List, Matches);
   begin
      Solutions := Expected (Current_Text, Message, " ", False);
   end Fix;


   ------------------
   -- Name_Missing --
   ------------------

   procedure Free (This : in out Name_Missing) is
   begin
      for J in This.Matcher_Aux'Range loop
         Free (This.Matcher_Aux (J));
      end loop;

      Free (Error_Parser (This));
   end Free;

   procedure Initialize (This : in out Name_Missing) is
   begin
      This.Matcher :=
        (new Pattern_Matcher'(Compile ("\(style\) ""end ([\w]+)"" required")),
         new Pattern_Matcher'
           (Compile ("\(style\) ""exit ([\w]+)"" required")),
         new Pattern_Matcher'
           (Compile ("\(style\) ""end record ([\w]+)"" required")));
   end Initialize;

   procedure Fix
     (This         : Name_Missing;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (Errors_List);

      Col_Matches : Match_Array (0 .. 1);
      New_Message : Error_Message := Message;
      Line_Cursor : File_Cursor := File_Cursor (Message);

   begin
      Line_Cursor.Col := 1;
      Match (This.Matcher_Aux (This.Current_It.all).all,
             Get_Line (Current_Text, Line_Cursor),
             Col_Matches);
      New_Message.Col := Col_Matches (1).Last + 1;

      Solutions := Expected
        (Current_Text,
         New_Message,
         Get_Message (Message) (Matches (1).First .. Matches (1).Last));
   end Fix;

   --------------------
   -- Double_Keyword --
   --------------------

   procedure Initialize (This : in out Double_Keyword) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
        (Compile ("extra ""([^""])"" ignored")));
   end Initialize;

   procedure Fix
     (This         : Double_Keyword;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Errors_List);
   begin
      Solutions := Unexpected
        (Current_Text,
         Message,
         Get_Message (Message) (Matches (1).First .. Matches (1).Last));
   end Fix;

   -----------------
   -- Extra_Paren --
   -----------------

   procedure Initialize (This : in out Extra_Paren) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
        (Compile ("extra right paren")));
   end Initialize;

   procedure Fix
     (This         : Extra_Paren;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Errors_List, Matches);
   begin
      Solutions := Unexpected (Current_Text, Message, ")");
   end Fix;

   -----------------------
   -- Redundant_Keyword --
   -----------------------

   procedure Initialize (This : in out Redundant_Keyword) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
        (Compile ("redundant (["" \w]+)")));
   end Initialize;

   procedure Fix
     (This         : Redundant_Keyword;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Errors_List);

      Str_Red : Dynamic_String;
   begin
      Str_Red := new String'(Get_Message (Message)
                   (Matches (1).First .. Matches (1).Last));

      if Str_Red.all = "colon" then
         Solutions := Unexpected (Current_Text, Message, ":");
      elsif Str_Red.all = """then""" then
         Solutions := Unexpected (Current_Text, Message, "then");
      else
         Free (Str_Red);
         raise Uncorrectable_Message;
      end if;

      Free (Str_Red);

   end Fix;

   --------------------
   -- Unexpected_Sep --
   --------------------

   procedure Initialize (This : in out Unexpected_Sep) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
        (Compile ("unexpected ""([^""]+)"" ignored")));
   end Initialize;

   procedure Fix
     (This         : Unexpected_Sep;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Errors_List);
   begin
      Solutions := Unexpected
        (Current_Text,
         Message,
         Get_Message (Message) (Matches (1).First .. Matches (1).Last));
   end Fix;

   ---------------------
   -- Unexpected_Word --
   ---------------------

   procedure Initialize (This : in out Unexpected_Word) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
        (Compile ("unexpected ([\w]+) ([\w]+)")));
   end Initialize;

   procedure Fix
     (This         : Unexpected_Word;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Errors_List);

      Str_Red_1, Str_Red_2 : Dynamic_String;
   begin
      Str_Red_1 := new String'(Get_Message (Message)
                                 (Matches (1).First .. Matches (1).Last));

      Str_Red_2 := new String'(Get_Message (Message)
                                 (Matches (2).First .. Matches (2).Last));

      if Str_Red_1.all = "semicolon" and then Str_Red_2.all = "ignored" then
         Solutions := Unexpected (Current_Text, Message, ";");
      elsif Str_Red_1.all = "right" and then Str_Red_2.all = "parenthesis" then
         Solutions := Unexpected (Current_Text, Message, ")");
      else
         Free (Str_Red_1);
         Free (Str_Red_2);
         raise Uncorrectable_Message;
      end if;

      Free (Str_Red_1);
      Free (Str_Red_2);

   end Fix;

   --------------------
   -- Kw_Not_Allowed --
   --------------------

   procedure Initialize (This : in out Kw_Not_Allowed) is
   begin
      This.Matcher :=
        (new Pattern_Matcher'(Compile ("""([\w]+)"" not allowed")),
         new Pattern_Matcher'(Compile ("""([\w]+)"" keyword not allowed")),
         new Pattern_Matcher'
           (Compile ("""([\w]+)"" ignored \(only allowed in")));
   end Initialize;

   procedure Fix
     (This         : Kw_Not_Allowed;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Errors_List);
   begin
      Solutions := Unexpected
        (Current_Text,
         Message,
         Get_Message (Message) (Matches (1).First .. Matches (1).Last));
   end Fix;

   ---------------------
   -- Sep_Not_Allowed --
   ---------------------

   procedure Initialize (This : in out Sep_Not_Allowed) is
   begin
      This.Matcher :=
        (new Pattern_Matcher'
           (Compile ("(form feed) not allowed")),
         new Pattern_Matcher'
           (Compile ("(vertical tab) not allowed")),
         new Pattern_Matcher'
           (Compile ("(trailing spaces) not permitted")),
         new Pattern_Matcher'
           (Compile ("(space) not allowed")));
   end Initialize;


   procedure Fix
     (This         : Sep_Not_Allowed;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Errors_List);

      Word_Read            : Dynamic_String;
      Unallowed_Characters : Dynamic_String;
      Format_Str           : String_Mode;
   begin
      Assign
        (Word_Read,
         Get_Message (Message) (Matches (1).First .. Matches (1).Last));

      if Word_Read.all = "form feed" then
         Assign (Unallowed_Characters, (1 => ASCII.FF));
         Format_Str := Text_Ascii;
      elsif Word_Read.all = "vertical tab" then
         Assign (Unallowed_Characters, (1 => ASCII.HT));
         Format_Str := Text_Ascii;
      elsif Word_Read.all = "trailing spaces" then
         Assign (Unallowed_Characters, "([/s]+)");
         Format_Str := Regular_Expression;
      elsif Word_Read.all = "space" then
         Assign (Unallowed_Characters, " ");
         Format_Str := Text_Ascii;
      end if;

      Solutions := Unexpected
        (Current_Text,
         Message,
         Unallowed_Characters.all,
         Format_Str);

      Free (Unallowed_Characters);
      Free (Word_Read);
   end Fix;

   ------------------
   -- Should_Be_In --
   ------------------

   procedure Initialize (This : in out Should_Be_In) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
         (Compile ("should be in column ([0-9]+)")));
   end Initialize;

   procedure Fix
     (This         : Should_Be_In;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Errors_List);
   begin
      Solutions := Wrong_Column
        (Current_Text,
         Message,
         Integer'Value (Get_Message (Message)
                          (Matches (1).First .. Matches (1).Last)));
   end Fix;

   ----------------
   -- Bad_Column --
   ----------------

   procedure Initialize (This : in out Bad_Column) is
   begin
      This.Matcher :=
        (new Pattern_Matcher'(Compile ("bad column")),
         new Pattern_Matcher'(Compile ("incorrect layout")),
         new Pattern_Matcher'(Compile ("bad indentation")));
   end Initialize;

   procedure Fix
     (This         : Bad_Column;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Errors_List, Matches);
   begin
      Solutions := Wrong_Column (Current_Text, Message);
   end Fix;

   -----------------------
   -- Main_With_Missing --
   -----------------------

   procedure Initialize (This : in out Main_With_Missing) is
   begin
      This.Matcher :=
        (new Pattern_Matcher'
           (Compile ("missing with for ""([^""]+)""")),
         new Pattern_Matcher'
           (Compile ("possible missing with of ([\w]+)")),
         new Pattern_Matcher'
           (Compile ("missing with_clause on ""([^""])""")));
   end Initialize;

   procedure Fix
     (This         : Main_With_Missing;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Errors_List);
   begin
      Solutions := With_Clause_Missing
        (Current_Text,
         Message,
         Get_Message (Message) (Matches (1).First .. Matches (1).Last));
   end Fix;

   -------------------------
   -- Bad_Casing_Standard --
   -------------------------

   procedure Initialize (This : in out Bad_Casing_Standard) is
   begin
      This.Matcher :=
        (new Pattern_Matcher'
           (Compile ("bad casing for entity in Standard")),
         new Pattern_Matcher'
           (Compile ("bad capitalization, mixed case required")));
   end Initialize;

   procedure Fix
     (This         : Bad_Casing_Standard;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Errors_List, Matches);
   begin
      Solutions := Bad_Casing (Current_Text, Message);
   end Fix;

   -------------------------
   -- Bad_Casing_Declared --
   -------------------------

   procedure Initialize (This : in out Bad_Casing_Declared) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
        (Compile ("bad casing of ""([^""]+)"" declared")));
   end Initialize;

   procedure Fix
     (This         : Bad_Casing_Declared;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Errors_List);
   begin
      Solutions := Bad_Casing
        (Current_Text,
         Message,
         Get_Message (Message) (Matches (1).First .. Matches (1).Last));
   end Fix;

   ------------------------
   -- Bad_Casing_Keyword --
   ------------------------

   procedure Initialize (This : in out Bad_Casing_Keyword) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
        (Compile ("reserved words must be all lower case")));
   end Initialize;

   procedure Fix
     (This         : Bad_Casing_Keyword;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Errors_List, Matches);
   begin
      Solutions := Bad_Casing (Current_Text, Message, "", Lower);
   end Fix;

   ---------------------------
   -- Object_Not_Referenced --
   ---------------------------

   procedure Initialize (This : in out Object_Not_Referenced) is
   begin
      This.Matcher :=
        (new Pattern_Matcher'
           (Compile ("(procedure) ""([\w]+)"" is not referenced")),
         new Pattern_Matcher'
           (Compile ("(function) ""(""?[^""]+""?)"" is not referenced")),
         new Pattern_Matcher'
           (Compile ("(variable) ""([\w]+)"" is not referenced")),
         new Pattern_Matcher'
           (Compile ("(constant) ""([\w]+)"" is not referenced")),
         new Pattern_Matcher'
           (Compile ("(parameter) ""([\w]+)"" is not referenced")),
         new Pattern_Matcher'
           (Compile ("(type) ""([\w]+)"" is not referenced")));
   end Initialize;

   procedure Fix
     (This         : Object_Not_Referenced;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Errors_List);

      First_Word : constant String := Get_Message (Message)
        (Matches (1).First .. Matches (1).Last);
      Category   : Language_Category;

   begin
      if First_Word = "procedure" then
         Category := Cat_Procedure;
      elsif First_Word = "function" then
         Category := Cat_Function;
      elsif First_Word = "variable" then
         Category := Cat_Variable;
      elsif First_Word = "constant" then
         Category := Cat_Variable;
      elsif First_Word = "parameter" then
         Category := Cat_Local_Variable; --  Used instead of Cat_Parameter
      elsif First_Word = "type" then
         Category := Cat_Type;
      end if;

      Solutions := Not_Referenced
         (Current_Text,
          Message,
          Category,
          Get_Message (Message) (Matches (2).First .. Matches (2).Last));
   end Fix;

   ------------------------
   -- Pkg_Not_Referenced --
   ------------------------

   procedure Initialize (This : in out Pkg_Not_Referenced) is
   begin
      This.Matcher :=
        (new Pattern_Matcher'
           (Compile ("unit ""([^""]+)"" is not referenced")),
         new Pattern_Matcher'
           (Compile ("no entities of ""([^""]+)"" are referenced")),
         new Pattern_Matcher'
           (Compile ("unit ""([^""]+)"" is never instantiated")));
   end Initialize;

   procedure Fix
     (This         : Pkg_Not_Referenced;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Errors_List);
   begin
      Solutions := Not_Referenced
        (Current_Text,
         Message,
         Cat_With,
         Get_Message (Message) (Matches (1).First .. Matches (1).Last));
   end Fix;

   -----------------------
   -- Pragma_Missplaced --
   -----------------------

   procedure Initialize (This : in out Pragma_Missplaced) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
         (Compile ("pragma must be first line of file")));
   end Initialize;

   procedure Fix
     (This         : Pragma_Missplaced;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Errors_List, Matches);
   begin
      Solutions := First_Line_Pragma (Current_Text, Message);
   end Fix;

   -----------------------
   -- Constant_Expected --
   -----------------------

   procedure Initialize (This : in out Constant_Expected) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
         (Compile ("""([\w]+)"" is not modified, could be declared const")));
   end Initialize;

   procedure Fix
     (This         : Constant_Expected;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Errors_List);
   begin
      Solutions := Not_Modified
        (Current_Text,
         Message,
         Get_Message (Message)
           (Matches (1).First .. Matches (1).Last));
   end Fix;

   -----------------------------
   -- Possible_Interpretation --
   -----------------------------

   procedure Initialize (This : in out Possible_Interpretation) is
   begin
      This.Matcher :=
        (1 => new Pattern_Matcher'
           (Compile
              ("ambiguous expression \(cannot resolve """"?([^""]+)""""?")));
   end Initialize;

   procedure Free (This : in out Possible_Interpretation) is
   begin
      Free (Error_Parser (This));
      Free (This.Source_Matcher);
      Free (This.Local_Matcher);
   end Free;

   procedure Fix
     (This         : Possible_Interpretation;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      Matches_Prev    : Match_Array (0 .. 2);
      Preview         : Error_Message;
      Cursor_List     : Cursor_Lists.List;
   begin
      loop
         declare
            Solution_Cursor : File_Cursor;
         begin
            Solution_Cursor.Col := 1;

            Get_Preview (Errors_List, Current_Text, Preview);
            exit when Preview = Invalid_Error_Message;

            Match
              (This.Source_Matcher.all,
               Get_Message (Preview),
               Matches_Prev);

            if Matches_Prev (0) = No_Match then
               Match
                 (This.Local_Matcher.all, Get_Message (Preview), Matches_Prev);

               exit when Matches_Prev (0) = No_Match;

               Assign
                 (Solution_Cursor.File_Name, Message.File_Name);

               Solution_Cursor.Line := Integer'Value
                 (Get_Message (Preview)
                    (Matches_Prev (1).First .. Matches_Prev (1).Last));
            else
               Assign
                 (Solution_Cursor.File_Name, Get_Message (Preview)
                    (Matches_Prev (1).First .. Matches_Prev (1).Last));

               Solution_Cursor.Line := Integer'Value
                 (Get_Message (Preview)
                    (Matches_Prev (2).First .. Matches_Prev (2).Last));
            end if;

            Append (Cursor_List, Solution_Cursor);
            Skip_Message (Errors_List);
         end;

      end loop;


      Solutions := Resolve_Ambiguity
        (Current_Text,
         Message,
         Cursor_List,
         Get_Message (Message)
           (Matches (1).First .. Matches (1).Last));

      if Length (Solutions) = 0 then
         raise Uncorrectable_Message;
      end if;

   end Fix;

   ------------------------
   -- Hidden_Declaration --
   ------------------------

   procedure Initialize (This : in out Hidden_Declaration) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
         (Compile ("""""?([^""]+)""""? is not visible")));
   end Initialize;

   procedure Free (This : in out Hidden_Declaration) is
   begin
      Free (Error_Parser (This));
      Free (This.Check_Possible);
      Free (This.Get_From_Current_File);
      Free (This.Get_From_Other_File);
   end Free;

   procedure Fix
     (This         : Hidden_Declaration;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      Matches_Check   : Match_Array (0 .. 0);
      Matches_Loc     : Match_Array (0 .. 2);
      Preview         : Error_Message;
      Cursor_List     : Cursor_Lists.List;
   begin
      Get_Preview (Errors_List, Current_Text, Preview);

      if Preview = Invalid_Error_Message then
         raise Uncorrectable_Message;
      end if;

      Match (This.Check_Possible.all, Get_Message (Preview), Matches_Check);

      if Matches_Check (0) = No_Match then
         raise Uncorrectable_Message;
      end if;

      Skip_Message (Errors_List);
      Free (Preview);

      loop
         declare
            Solution_Cursor : File_Cursor;
         begin
            Solution_Cursor.Col := 1;
            Get_Preview (Errors_List, Current_Text, Preview);

            exit when Preview = Invalid_Error_Message;

            Match
              (This.Get_From_Other_File.all,
               Get_Message (Preview),
               Matches_Loc);

            if Matches_Loc (0) = No_Match then
               Match
                 (This.Get_From_Current_File.all,
                  Get_Message (Preview),
                  Matches_Loc);

               exit when Matches_Loc (0) = No_Match;

               Assign
                 (Solution_Cursor.File_Name, Message.File_Name);

               Solution_Cursor.Line := Integer'Value
                 (Get_Message (Preview)
                    (Matches_Loc (1).First .. Matches_Loc (1).Last));
            else
               Assign
                 (Solution_Cursor.File_Name, Get_Message (Preview)
                    (Matches_Loc (1).First .. Matches_Loc (1).Last));

               Solution_Cursor.Line := Integer'Value
                 (Get_Message (Preview)
                    (Matches_Loc (2).First .. Matches_Loc (2).Last));
            end if;

            Append (Cursor_List, Solution_Cursor);

            Skip_Message (Errors_List);
         end;
      end loop;

      Solutions := Resolve_Ambiguity
        (Current_Text,
         Message,
         Cursor_List,
         Get_Message (Message)
           (Matches (1).First .. Matches (1).Last));

      if Length (Solutions) = 0 then
         raise Uncorrectable_Message;
      end if;

   end Fix;

   --------------------------
   -- Redundant_Conversion --
   --------------------------

   procedure Initialize (This : in out Redundant_Conversion) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
        (Compile ("useless conversion, ""([^""])"" has this type")));
   end Initialize;

   procedure Fix
     (This         : Redundant_Conversion;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Errors_List);
   begin
      Solutions := Remove_Conversion
        (Current_Text,
         Message,
         Get_Message (Message)
           (Matches (1).First .. Matches (1).Last));
   end Fix;

   ---------------------
   -- Missplaced_With --
   ---------------------

   procedure Initialize (This : in out Missplaced_With) is
   begin
      This.Matcher :=
        (new Pattern_Matcher'
           (Compile ("with clause can be moved to body")),
         new Pattern_Matcher'
           (Compile ("with clause might be moved to body")));
   end Initialize;

   procedure Fix
     (This         : Missplaced_With;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Errors_List, Matches);
   begin
      Solutions := Move_With_To_Body (Current_Text, Message);
   end Fix;

   --------------------------
   -- Not_Fully_Conformant --
   --------------------------

   procedure Initialize (This : in out Not_Fully_Conformant) is
   begin
      This.Matcher :=
        (new Pattern_Matcher'
           (Compile ("not fully conformant with declaration at " &
                     "([^\:]+):([\d]+)")),
         new Pattern_Matcher'
           (Compile ("not fully conformant with declaration at " &
                     "(line) ([\d]+)")),
         new Pattern_Matcher'
           (Compile ("not type conformant with declaration at " &
                     "([^\:]+):([\d]+)")),
         new Pattern_Matcher'
           (Compile ("not type conformant with declaration at " &
                     "(line) ([\d]+)")));
   end Initialize;

   procedure Fix
     (This         : Not_Fully_Conformant;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Errors_List);

      Spec_Cursor : File_Cursor;
   begin
      Spec_Cursor.Col := 1;
      Spec_Cursor.Line := Natural'Value
        (Get_Message (Message)
           (Matches (2).First .. Matches (2).Last));


      if Get_Message (Message)
        (Matches (1).First .. Matches (1).Last) = "line"
      then
         Assign (Spec_Cursor.File_Name, Message.File_Name);
      else
         Assign
           (Spec_Cursor.File_Name,
            Get_Message (Message)
              (Matches (1).First .. Matches (1).Last));
      end if;

      Solutions := Make_Conformant (Current_Text, Message, Spec_Cursor);

      Free (Spec_Cursor);
   end Fix;

   ---------------------------
   -- Generic_Use_Unallowed --
   ---------------------------

   procedure Initialize (This : in out Generic_Use_Unallowed) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
        (Compile ("a generic package is not allowed in a use clause")));
   end Initialize;

   procedure Fix
     (This         : Generic_Use_Unallowed;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Errors_List, Matches);
   begin
      Solutions := Remove_Use_Clause (Current_Text, Message);
   end Fix;

   -----------------------------
   -- Non_Visible_Declaration --
   -----------------------------

   procedure Initialize (This : in out Non_Visible_Declaration) is
   begin
      This.Matcher :=
        (new Pattern_Matcher'
           (Compile ("non-visible declaration at ([^\:]+):([\d]+)")),
         new Pattern_Matcher'
           (Compile ("non-visible declaration at (line) ([\d]+)")));
   end Initialize;

   procedure Fix
     (This         : Non_Visible_Declaration;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Errors_List);

      Source_Cursor : File_Cursor;
      Seek_With     : Boolean;
   begin
      Source_Cursor.Col := 1;
      Source_Cursor.Line := Natural'Value
        (Get_Message (Message)
           (Matches (2).First .. Matches (2).Last));


      if Get_Message (Message)
        (Matches (1).First .. Matches (1).Last) = "line"
      then
         Assign (Source_Cursor.File_Name, Message.File_Name);
         Seek_With := False;
      else
         Assign
           (Source_Cursor.File_Name,
            Get_Message (Message)
              (Matches (1).First .. Matches (1).Last));
         Seek_With := True;
      end if;

      Solutions := Resolve_Unvisible_Declaration
        (Current_Text, Message, Source_Cursor, Seek_With);

      Free (Source_Cursor);
   end Fix;

begin
   Add_Parser (new Agregate_Misspelling);
   Add_Parser (new Double_Misspelling);
   Add_Parser (new Ligth_Misspelling);
   Add_Parser (new Goto_Misspelling);
   Add_Parser (new Library_Misspelling);
   Add_Parser (new Sth_Should_Be_Sth);
   Add_Parser (new Should_Be_Semicolon);
   Add_Parser (new And_Meant);
   Add_Parser (new Or_Meant);
   Add_Parser (new Bad_End_Block);
   Add_Parser (new Unqualified_Expression);
   Add_Parser (new Goes_Before);
   Add_Parser (new Sth_Expected_3);
   Add_Parser (new Sth_Expected_2);
   Add_Parser (new Sth_Expected);
   Add_Parser (new Missing_Kw);
   Add_Parser (new Missing_Sep);
   Add_Parser (new Missing_All);
   Add_Parser (new Statement_Missing);
   Add_Parser (new Space_Missing);
   Add_Parser (new Name_Missing);
   Add_Parser (new Double_Keyword);
   Add_Parser (new Extra_Paren);
   Add_Parser (new Redundant_Keyword);
   Add_Parser (new Unexpected_Sep);
   Add_Parser (new Unexpected_Word);
   Add_Parser (new Kw_Not_Allowed);
   Add_Parser (new Sep_Not_Allowed);
   Add_Parser (new Should_Be_In);
   Add_Parser (new Bad_Column);
   Add_Parser (new Main_With_Missing);
   Add_Parser (new Bad_Casing_Standard);
   Add_Parser (new Bad_Casing_Declared);
   Add_Parser (new Bad_Casing_Keyword);
   Add_Parser (new Object_Not_Referenced);
   Add_Parser (new Pkg_Not_Referenced);
   Add_Parser (new Pragma_Missplaced);
   Add_Parser (new Constant_Expected);
   Add_Parser (new Possible_Interpretation);
   Add_Parser (new Hidden_Declaration);
   Add_Parser (new Redundant_Conversion);
   Add_Parser (new Missplaced_With);
   Add_Parser (new Not_Fully_Conformant);
   Add_Parser (new Generic_Use_Unallowed);
   Add_Parser (new Non_Visible_Declaration);

   Initialize_Parsers;
end Codefix.Errors_Parser;
