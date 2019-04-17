------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2019, AdaCore                     --
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

with Ada.Exceptions;    use Ada.Exceptions;

with GNAT.Case_Util;    use GNAT.Case_Util;
with GNAT.Regpat;       use GNAT.Regpat;
with GNATCOLL.Symbols;  use GNATCOLL.Symbols;
with GNATCOLL.Utils;    use GNATCOLL.Utils;

with Case_Handling;     use Case_Handling;
with Language.Ada;      use Language.Ada;
with Projects;          use Projects;

with Ada_Semantic_Tree.Parts; use Ada_Semantic_Tree.Parts;

package body Codefix.Text_Manager is

   use type Basic_Types.Visible_Column_Type;

   function Search_Tokens
     (Line     : String;
      Cursor   : File_Cursor'Class;
      Searched : Token_List;
      Step     : Step_Way := Normal_Step) return Word_Cursor'Class;
   --  Return the first token matching one of the token list given in
   --  parameter.

   ------------------
   -- Compare_Last --
   ------------------

   function Compare_Last (Str_1, Str_2 : String) return Boolean is
      Str_1_Lower : String := Str_1;
      Str_2_Lower : String := Str_2;
   begin
      To_Lower (Str_1_Lower);
      To_Lower (Str_2_Lower);

      if Str_1'Length < Str_2'Length then
         return Str_1_Lower = Str_2_Lower
           (Str_2'Last - Str_1'Length + 1 .. Str_2'Last);
      else
         return Str_2_Lower = Str_1_Lower
           (Str_1'Last - Str_2'Length + 1 .. Str_1'Last);
      end if;
   end Compare_Last;

   --------------
   -- Is_Blank --
   --------------

   function Is_Blank (Str : String) return Boolean is
   begin
      for J in Str'Range loop
         if not Is_Blank (Str (J)) then
            return False;
         end if;
      end loop;

      return True;
   end Is_Blank;

   --------------
   -- Is_Blank --
   --------------

   function Is_Blank (Char : Character) return Boolean is
   begin
      return Char = ' ' or else Char = ASCII.HT;
   end Is_Blank;

   -------------------------
   -- Without_Last_Blanks --
   -------------------------

   function Without_Last_Blanks (Str : String) return String is
   begin
      for J in reverse Str'Range loop
         if not Is_Blank (Str (J)) then
            return Str (Str'First .. J);
         end if;
      end loop;

      return "";
   end Without_Last_Blanks;

   ------------------
   -- Is_Separator --
   ------------------

   function Is_Separator (Char : Character) return Boolean is
   begin
      if Is_Blank (Char) then
         return True;
      end if;

      case Char is
         when '.' | ',' | ';' | ''' => return True;
         when others => return False;
      end case;
   end Is_Separator;

   ----------------------------------------------------------------------------
   --  type Text_Cursor
   ----------------------------------------------------------------------------

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Text_Cursor) return Boolean is
   begin
      return Left.Line < Right.Line
        or else (Left.Line = Right.Line and then Left.Col < Right.Col);
   end "<";

   ---------
   -- ">" --
   ---------

   function ">" (Left, Right : Text_Cursor'Class) return Boolean is
   begin
      return not (Left < Right) and then Left /= Right;
   end ">";

   ----------
   -- "<=" --
   ----------

   function "<=" (Left, Right : Text_Cursor'Class) return Boolean is
   begin
      return not (Left > Right);
   end "<=";

   ----------
   -- ">=" --
   ----------

   function ">=" (Left, Right : Text_Cursor'Class) return Boolean is
   begin
      return not (Left < Right);
   end ">=";

   ---------
   -- "=" --
   ---------

   overriding function "=" (Left, Right : File_Cursor) return Boolean is
   begin
      return Left.Line = Right.Line
        and then Left.Col = Right.Col
        and then Left.File = Right.File;
   end "=";

   ---------
   -- "<" --
   ---------

   overriding function "<" (Left, Right : File_Cursor) return Boolean is
   begin
      return Left.File < Right.File
        or else (Left.File = Right.File
                 and then Text_Cursor (Left) < Text_Cursor (Right));
   end "<";

   ------------
   -- Assign --
   ------------

   procedure Assign
     (This : in out File_Cursor'Class; Source : File_Cursor'Class) is
   begin
      This.Col  := Source.Col;
      This.Line := Source.Line;
      This.File := Source.File;
   end Assign;

   ----------------------------------------------------------------------------
   --  type Text_Navigator
   ----------------------------------------------------------------------------

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Mark_Abstr) is
      pragma Unreferenced (This);
   begin
      null;
   end Free;

   ---------------
   -- Free_Data --
   ---------------

   procedure Free_Data (This : in out Mark_Abstr'Class) is
   begin
      Free (This);
   end Free_Data;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Ptr_Mark) is
      procedure Free_Pool is new
        Ada.Unchecked_Deallocation (Mark_Abstr'Class, Ptr_Mark);
   begin
      if This /= null then
         Free (This.all);
      end if;

      Free_Pool (This);
   end Free;

   --------------
   -- Get_File --
   --------------

   function Get_File (This : Mark_Abstr'Class) return Virtual_File is
   begin
      return This.File_Name;
   end Get_File;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Ptr_Text_Navigator) is
      procedure Free_Pool is new Ada.Unchecked_Deallocation
        (Text_Navigator_Abstr'Class, Ptr_Text_Navigator);
   begin
      if This /= null then
         Free (This.all);
         Free_Pool (This);
      end if;
   end Free;

   -----------
   -- Clean --
   -----------

   procedure Clean (This : in out Text_Navigator_Abstr) is
   begin
      This.Files.Clear;
   end Clean;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Text_Navigator_Abstr) is
   begin
      This.Files.Clear;
      Free (This.Files);
   end Free;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This : Text_Navigator_Abstr;
      File : in out Text_Interface'Class)
   is
   begin
      File.Construct_Db := This.Construct_Db;
   end Initialize;

   ------------------
   -- Get_New_Mark --
   ------------------

   function Get_New_Mark
     (Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class) return Mark_Abstr'Class
   is
      Real_Cursor : File_Cursor := File_Cursor (Cursor);
   begin
      if Cursor.Line = 0 then
         Real_Cursor.Line := 1;
      end if;

      declare
         Res : Mark_Abstr'Class := Get_New_Mark
          (Get_File (Current_Text, Get_File (Real_Cursor)).all,
           Real_Cursor);
      begin
         if Cursor.Line = 0 then
            Res.Is_First_Line := True;
         else
            Res.Is_First_Line := False;
         end if;

         Res.File_Name := Get_File (Cursor);
         return Res;
      end;
   end Get_New_Mark;

   ------------------------
   -- Get_Current_Cursor --
   ------------------------

   function Get_Current_Cursor
     (Current_Text : Text_Navigator_Abstr'Class;
      Mark         : Mark_Abstr'Class) return File_Cursor'Class
   is
      Cursor : File_Cursor'Class := Get_Current_Cursor
        (Get_File (Current_Text, Mark.File_Name).all, Mark);
   begin
      if Mark.Is_First_Line then
         Cursor.Line := 0;
      end if;

      return Cursor;
   end Get_Current_Cursor;

   ---------------------
   -- Get_Iterator_At --
   ---------------------

   function Get_Iterator_At
     (Current_Text      : Text_Navigator_Abstr;
      Cursor            : File_Cursor'Class;
      From_Type         : Position_Type := Start_Name;
      Position          : Relative_Position := Specified;
      Categories_Seeked : Category_Array := Null_Category_Array)
      return Construct_Tree_Iterator is
   begin
      return Get_Iterator_At
        (Get_File (Current_Text, Get_File (Cursor)),
         Text_Cursor (Cursor),
         From_Type,
         Position,
         Categories_Seeked);
   end Get_Iterator_At;

   ---------
   -- Get --
   ---------

   function Get
     (This   : Text_Navigator_Abstr;
      Cursor : File_Cursor'Class;
      Len    : Natural) return String is
   begin
      return Get
        (Get_File (This, Get_File (Cursor)).all, Text_Cursor (Cursor), Len);
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (This   : Text_Navigator_Abstr;
      Cursor : File_Cursor'Class) return Character
   is
   begin
      return Get
        (Get_File (This, Get_File (Cursor)).all, Text_Cursor (Cursor));
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (This        : Text_Navigator_Abstr;
      Start, Stop : File_Cursor'Class) return String is
   begin
      return Get
        (Get_File (This, Get_File (Start)).all,
         Text_Cursor (Start),
         Text_Cursor (Stop));
   end Get;

   --------------
   -- Get_Line --
   --------------

   function Get_Line
     (This      : Text_Navigator_Abstr;
      Cursor    : File_Cursor'Class;
      Start_Col : Visible_Column_Type := 0) return String is
   begin
      return Get_Line
        (Get_File (This, Get_File (Cursor)).all,
         Text_Cursor (Cursor),
         Start_Col);
   end Get_Line;

   ---------------
   -- Read_File --
   ---------------

   function Read_File
     (This      : Text_Navigator_Abstr;
      File_Name : Virtual_File) return Unbounded_String is
   begin
      return Read_File (Get_File (This, File_Name).all);
   end Read_File;

   --------------
   -- Get_File --
   --------------

   function Get_File
     (This : Text_Navigator_Abstr'Class;
      Name : GNATCOLL.VFS.Virtual_File) return Ptr_Text
   is
      New_Text : Ptr_Text;
   begin
      for Item of This.Files.all loop
         if Get_File_Name (Item.all) = Name then
            return Item;
         end if;
      end loop;

      New_Text := New_Text_Interface (This);

      Initialize (This, New_Text.all);

      Append (This.Files.all, New_Text);

      New_Text.File_Name := Name;
      Initialize (New_Text.all, Name);

      return New_Text;
   end Get_File;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (This      : in out Text_Navigator_Abstr;
      Cursor    : File_Cursor'Class;
      Len       : Natural;
      New_Value : String) is
   begin
      Replace
        (Get_File (This, Get_File (Cursor)).all,
         Text_Cursor (Cursor),
         Len,
         New_Value);
   end Replace;

   --------------
   -- Add_Line --
   --------------

   procedure Add_Line
     (This     : in out Text_Navigator_Abstr;
      Cursor   : File_Cursor'Class;
      New_Line : String;
      Indent   : Boolean := False)
   is
   begin
      Add_Line
        (Get_File (This, Get_File (Cursor)).all,
         Text_Cursor (Cursor),
         New_Line,
         Indent);
   end Add_Line;

   -----------------
   -- Delete_Line --
   -----------------

   procedure Delete_Line
     (This : in out Text_Navigator_Abstr;
      Cursor : File_Cursor'Class) is
   begin
      Delete_Line
        (Get_File (This, Get_File (Cursor)).all, Text_Cursor (Cursor));
   end Delete_Line;

   -----------------
   -- Line_Length --
   -----------------

   function Line_Length
     (This   : Text_Navigator_Abstr;
      Cursor : File_Cursor'Class) return Natural is
   begin
      return Line_Length
        (Get_File (This, Get_File (Cursor)).all,
         File_Cursor (Cursor));
   end Line_Length;

   ------------------
   -- Search_Token --
   ------------------

   function Search_Token
     (This     : Text_Navigator_Abstr'Class;
      Cursor   : File_Cursor'Class;
      Searched : Token_Record;
      Step     : Step_Way := Normal_Step)
      return Word_Cursor'Class is
   begin
      return Search_Token
        (Get_File (This, Get_File (Cursor)).all,
         File_Cursor (Cursor),
         Searched,
         Step);
   end Search_Token;

   -------------------
   -- Search_Tokens --
   -------------------

   function Search_Tokens
     (This     : Text_Navigator_Abstr'Class;
      Cursor   : File_Cursor'Class;
      Searched : Token_List;
      Step     : Step_Way := Normal_Step)
      return Word_Cursor'Class is
   begin
      return Search_Tokens
        (Get_File (This, Get_File (Cursor)).all,
         File_Cursor (Cursor),
         Searched,
         Step);
   end Search_Tokens;

   -----------------
   -- Search_Unit --
   -----------------

   function Search_Unit
     (This      : Text_Navigator_Abstr'Class;
      File_Name : GNATCOLL.VFS.Virtual_File;
      Category  : Language_Category;
      Name      : String := "") return Simple_Construct_Information is
   begin
      return Search_Unit (Get_File (This, File_Name), Category, Name);
   end Search_Unit;

   --------------
   -- Line_Max --
   --------------

   function Line_Max
     (This      : Text_Navigator_Abstr'Class;
      File_Name : GNATCOLL.VFS.Virtual_File) return Natural is
   begin
      return Line_Max (Get_File (This, File_Name).all);
   end Line_Max;

   ---------------------
   -- Get_Full_Prefix --
   ---------------------

   function Get_Full_Prefix
     (This     : Text_Navigator_Abstr'Class;
      Cursor   : File_Cursor'Class;
      Category : Language_Category := Cat_Unknown)
      return String is
   begin
      return Get_Full_Prefix
        (Get_File (This, Get_File (Cursor)), Cursor, Category);
   end Get_Full_Prefix;

   ----------------
   -- Get_Entity --
   ----------------

   procedure Get_Entity
     (Current_Text         : Text_Navigator_Abstr'Class;
      Cursor               : File_Cursor'Class;
      Spec_Begin, Spec_End : out File_Cursor'Class;
      Body_Begin, Body_End : out File_Cursor'Class)
   is
      Unit_Info, Body_Info : Construct_Tree_Iterator;
   begin
      Unit_Info := Get_Iterator_At
        (Current_Text, Cursor, From_Type => Start_Name);

      if Unit_Info = Null_Construct_Tree_Iterator then
         --  If the location was not the location of an entity name, see if we
         --  can find the beginning of a construct

         Unit_Info := Get_Iterator_At
        (Current_Text, Cursor, From_Type => Start_Construct);
      end if;

      if Unit_Info = Null_Construct_Tree_Iterator then
         File_Cursor (Spec_Begin) := Null_File_Cursor;
         File_Cursor (Body_Begin) := Null_File_Cursor;
         File_Cursor (Spec_End) := Null_File_Cursor;
         File_Cursor (Body_End) := Null_File_Cursor;

         return;
      end if;

      --  ??? We may want to check if the entity retreived has still the
      --  expected name to catch certain changes.

      if Get_Construct (Unit_Info).Is_Declaration then
         Body_Info := To_Construct_Tree_Iterator (Get_Second_Occurence
           (To_Entity_Access
              (Get_Structured_File
                 (Current_Text, Get_File (Cursor)), Unit_Info)));
      else
         Body_Info := Null_Construct_Tree_Iterator;
      end if;

      if Body_Info /= Null_Construct_Tree_Iterator then
         Set_File (Body_Begin, Get_File (Cursor));
         Set_File (Body_End,   Get_File (Cursor));
         Set_File (Spec_Begin, Get_File (Cursor));
         Set_File (Spec_End,   Get_File (Cursor));

         Body_Begin.Col := 1;
         Body_End.Col := 1;
         Body_Begin.Line := Get_Construct (Body_Info).Sloc_Start.Line;
         Body_End.Line := Get_Construct (Body_Info).Sloc_End.Line;

         Body_Begin.Col := To_Column_Index
           (String_Index_Type (Get_Construct (Body_Info).Sloc_Start.Column),
            Get_Line (Current_Text, Body_Begin));
         Body_End.Col := To_Column_Index
           (String_Index_Type (Get_Construct (Body_Info).Sloc_End.Column),
            Get_Line (Current_Text, Body_End));

         Spec_Begin.Col := 1;
         Spec_End.Col := 1;
         Spec_Begin.Line := Get_Construct (Unit_Info).Sloc_Start.Line;
         Spec_End.Line := Get_Construct (Unit_Info).Sloc_End.Line;

         Spec_Begin.Col := To_Column_Index
           (String_Index_Type (Get_Construct (Unit_Info).Sloc_Start.Column),
            Get_Line (Current_Text, Spec_Begin));
         Spec_End.Col := To_Column_Index
           (String_Index_Type (Get_Construct (Unit_Info).Sloc_End.Column),
            Get_Line (Current_Text, Spec_End));
      else
         Set_File (Body_Begin, Get_File (Cursor));
         Set_File (Body_End,   Get_File (Cursor));

         Body_Begin.Col := 1;
         Body_End.Col := 1;
         Body_Begin.Line := Get_Construct (Unit_Info).Sloc_Start.Line;
         Body_End.Line := Get_Construct (Unit_Info).Sloc_End.Line;

         Body_Begin.Col := To_Column_Index
           (String_Index_Type (Get_Construct (Unit_Info).Sloc_Start.Column),
            Get_Line (Current_Text, Body_Begin));
         Body_End.Col := To_Column_Index
           (String_Index_Type (Get_Construct (Unit_Info).Sloc_End.Column),
            Get_Line (Current_Text, Body_End));

         Assign (Spec_Begin, Null_File_Cursor);
         Assign (Spec_End, Null_File_Cursor);
      end if;

   end Get_Entity;

   ---------------
   -- Next_Word --
   ---------------

   procedure Next_Word
     (This   : Text_Navigator_Abstr'Class;
      Cursor : in out File_Cursor'Class;
      Word   : out Word_Cursor) is
   begin
      Next_Word
        (Get_File (This, Get_File (Cursor)).all,
         Cursor,
         Word);
   end Next_Word;

   ----------------
   -- Update_All --
   ----------------

   procedure Update_All (This : Text_Navigator_Abstr'Class) is
   begin
      for Item of This.Files.all loop
         Constrain_Update (Item.all);
      end loop;
   end Update_All;

   -------------------
   -- Previous_Char --
   -------------------

   function Previous_Char
     (This : Text_Navigator_Abstr'Class; Cursor : File_Cursor'Class)
     return File_Cursor'Class
   is
      Result : File_Cursor;
   begin
      Result := (Text_Cursor
                   (Previous_Char
                      (Get_File (This, Get_File (Cursor)).all, Cursor))
                 with Get_File (Cursor));
      return Result;
   end Previous_Char;

   ----------
   -- Undo --
   ----------

   procedure Undo
     (This : Text_Navigator_Abstr'Class; File_Name : Virtual_File) is
   begin
      Undo (Get_File (This, File_Name).all);
   end Undo;

   -------------------------
   -- Get_Structured_File --
   -------------------------

   function Get_Structured_File
     (This : Text_Navigator_Abstr'Class; File : Virtual_File)
      return Structured_File_Access is
   begin
      return Get_Structured_File (Get_File (This, File));
   end Get_Structured_File;

   --------------------
   -- Parse_Entities --
   --------------------

   procedure Parse_Entities
     (Lang     : access Language_Root'Class;
      This     : Text_Navigator_Abstr'Class;
      Callback : Codefix_Entity_Callback;
      Start    : File_Cursor'Class) is
   begin
      Parse_Entities
        (Lang, Get_File (This, Get_File (Start)).all, Callback, Start);
   end Parse_Entities;

   ------------------------------
   -- Parse_Entities_Backwards --
   ------------------------------

   procedure Parse_Entities_Backwards
     (Lang     : access Language_Root'Class;
      This     : Text_Navigator_Abstr'Class;
      Callback : access procedure (Buffer : Unbounded_String;
                                   Token  : Language.Token_Record;
                                   Stop   : in out Boolean);
      Start    : File_Cursor'Class) is
   begin
      Parse_Entities_Backwards
        (Lang, Get_File (This, Get_File (Start)).all, Callback, Start);
   end Parse_Entities_Backwards;

   ----------------------------------------------------------------------------
   --  type Text_Interface
   ----------------------------------------------------------------------------

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Text_Interface) is
   begin
      Free (This.Structure_Up_To_Date);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Ptr_Text) is
      procedure Delete is new
        Ada.Unchecked_Deallocation (Text_Interface'Class, Ptr_Text);
   begin
      Free (This.all);
      Delete (This);
   end Free;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This      : in out Text_Interface;
      File_Name : GNATCOLL.VFS.Virtual_File)
   is
   begin
      This.File_Name := File_Name;
   end Initialize;

   ---------------------
   -- Get_Iterator_At --
   ---------------------

   function Get_Iterator_At
     (Current_Text      : access Text_Interface;
      Cursor            : Text_Cursor'Class;
      From_Type         : Position_Type := Start_Name;
      Position          : Relative_Position := Specified;
      Categories_Seeked : Category_Array := Null_Category_Array)
      return Construct_Tree_Iterator
   is
      Line_Cursor : constant String := Get_Line
        (Text_Interface'Class (Current_Text.all), Cursor, 1);
      It          : Construct_Tree_Iterator;
   begin
      It := Get_Iterator_At
        (Get_Tree (Get_Structured_File (Current_Text)),
         (Absolute_Offset => False,
          Line            => Get_Line (Cursor),
          Line_Offset     =>
            To_Char_Index (Get_Column (Cursor), Line_Cursor)),
         From_Type,
         Position,
         Categories_Seeked);

      return It;
   end Get_Iterator_At;

   -------------------
   -- Get_File_Name --
   -------------------

   function Get_File_Name (This : Text_Interface) return Virtual_File is
   begin
      return This.File_Name;
   end Get_File_Name;

   ---------
   -- Get --
   ---------

   function Get
     (This        : Text_Interface;
      Start, Stop : Text_Cursor'Class) return String
   is

      function Recursive_Get (C : Text_Cursor; Before : String) return String;

      function Recursive_Get (C : Text_Cursor; Before : String) return String
      is
         Line        : constant String :=
           Get_Line (Text_Interface'Class (This), C, 1);
         Next_Cursor : Text_Cursor;
         Char_Start, Char_End : String_Index_Type;
      begin
         if C.Line = Start.Line then
            Char_Start := To_Char_Index (Start.Col, Line);
         else
            Char_Start := String_Index_Type (Line'First);
         end if;

         if C.Line = Stop.Line then
            Char_End := To_Char_Index (Stop.Col, Line);
         else
            Char_End := String_Index_Type (Line'Last);
         end if;

         if C.Line = Stop.Line then
            return Before & Line (Integer (Char_Start) .. Integer (Char_End));
         else
            Next_Cursor := C;
            Next_Cursor.Line := Next_Cursor.Line + 1;
            Next_Cursor.Col := 1;

            return Before
              & Line (Integer (Char_Start) .. Integer (Char_End))
              & Recursive_Get (Next_Cursor, (1 => ASCII.LF));
         end if;
      end Recursive_Get;

   begin
      return Recursive_Get (Text_Cursor (Start), "");
   end Get;

   ------------------------
   -- Remove_Empty_Lines --
   ------------------------

   procedure Remove_Empty_Lines
     (This         : in out Text_Interface'Class;
      Start_Cursor : Text_Cursor'Class;
      End_Cursor   : Text_Cursor'Class)
   is
      Line_Cursor : Text_Cursor := Text_Cursor (Start_Cursor);
   begin
      Line_Cursor.Col := 1;

      for J in reverse Start_Cursor.Line .. End_Cursor.Line loop
         Line_Cursor.Line := J;

         if Is_Blank (This.Get_Line (Line_Cursor)) then
            This.Delete_Line (Line_Cursor);
         end if;
      end loop;
   end Remove_Empty_Lines;

   -----------------
   -- Line_Length --
   -----------------

   function Line_Length
     (This   : Text_Interface'Class;
      Cursor : Text_Cursor'Class) return Natural is
   begin
      return Get_Line (This, Cursor)'Length;
   end Line_Length;

   ------------------
   -- Search_Token --
   ------------------

   function Search_Token
     (This     : Text_Interface'Class;
      Cursor   : Text_Cursor'Class;
      Searched : Token_Record;
      Step     : Step_Way := Normal_Step) return Word_Cursor'Class
   is
      Tokens : constant Token_List := (1 => Searched);
      Result : constant Word_Cursor'Class := Search_Tokens
        (This, Cursor, Tokens, Step);
   begin
      return Result;
   end Search_Token;

   -------------------
   -- Search_Tokens --
   -------------------

   function Search_Tokens
     (Line     : String;
      Cursor   : File_Cursor'Class;
      Searched : Token_List;
      Step     : Step_Way := Normal_Step) return Word_Cursor'Class
   is
      --  ??? This is a bit Ada-specific (casing, use of Ada_Lang).

      Result        : Word_Cursor :=
        (File_Cursor (Cursor) with Null_Unbounded_String, Text_Ascii);
      Start_Index   : String_Index_Type;
      Found         : Boolean := False;

      function Callback
        (Entity         : Language_Entity;
         Sloc_Start     : Source_Location;
         Sloc_End       : Source_Location;
         Partial_Entity : Boolean) return Boolean;

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
         for J in Searched'Range loop
            if Entity = Searched (J).Kind
              and then Equal
                (Line (Sloc_Start.Index .. Sloc_End.Index),
                 To_String (Searched (J).Name),
                 False)
            then
               Found := True;

               Result.Col := To_Column_Index
                 (String_Index_Type (Sloc_Start.Index), Line);
               Result.String_Match :=
                 To_Unbounded_String
                   (Line (Sloc_Start.Index .. Sloc_End.Index));

               if Step = Normal_Step then
                  --  If we are on the normal step, we stop as soon as we find
                  --  something.

                  return True;
               else
                  --  Otherwise we'll go until the last matching
                  --  token.

                  return False;
               end if;
            end if;
         end loop;

         return False;
      end Callback;
   begin
      if Line = "" then
         return Null_Word_Cursor;
      end if;

      if Result.Col = 0 then
         Start_Index := String_Index_Type (Line'Last);
      else
         Start_Index := To_Char_Index (Result.Col, Line);
      end if;

      case Step is
         when Normal_Step =>
            Parse_Entities
              (Lang     => Ada_Lang,
               Buffer   => Line
                 (Integer (Start_Index) .. Line'Last),
               Callback => Callback'Unrestricted_Access);

         when Reverse_Step =>
            Parse_Entities
              (Lang     => Ada_Lang,
               Buffer   => Line
                 (Line'First .. Integer (Start_Index)),
               Callback => Callback'Unrestricted_Access);

      end case;

      if Found then
         return Result;
      else
         return Null_Word_Cursor;
      end if;
   end Search_Tokens;

   function Search_Tokens
     (This     : Text_Interface'Class;
      Cursor   : Text_Cursor'Class;
      Searched : Token_List;
      Step     : Step_Way := Normal_Step) return Word_Cursor'Class
   is
      Last, Increment    : Integer;
      Result             : Word_Cursor;
      New_Cursor         : File_Cursor;
   begin
      New_Cursor := File_Cursor (Cursor);

      case Step is
         when Normal_Step =>
            Last := Line_Max (This);
            Increment := 1;
         when Reverse_Step =>
            Last := 1;
            Increment := -1;
      end case;

      loop
         Result :=
           Word_Cursor
             (Search_Tokens
                  (This.Get_Line (New_Cursor, 1),
                   New_Cursor,
                   Searched,
                   Step));

         if Result /= Null_Word_Cursor then
            return Result;
         end if;

         exit when New_Cursor.Line = Last;

         New_Cursor.Col := 1;
         New_Cursor.Line := New_Cursor.Line + Increment;

         if Step = Reverse_Step then
            New_Cursor.Col := 0;
         end if;

      end loop;

      return Null_Word_Cursor;
   end Search_Tokens;

   -----------------
   -- Search_Unit --
   -----------------

   function Search_Unit
     (This     : access Text_Interface'Class;
      Category : Language_Category;
      Name     : String := "") return Simple_Construct_Information
   is
      Current_Info : Construct_Tree_Iterator;
      Tree         : constant Construct_Tree :=
        Get_Tree (This.Get_Structured_File);
   begin
      Current_Info := First (Tree);

      while Current_Info /= Null_Construct_Tree_Iterator loop
         if Get_Construct (Current_Info).Category = Category
           and then
             (Name = ""
              or else Compare_Last
                (Get (Get_Construct (Current_Info).Name).all, Name))
         then
            return Get_Construct (Current_Info).all;
         end if;

         Current_Info := Next (Tree, Current_Info);
      end loop;

      return Null_Simple_Construct_Info;
   end Search_Unit;

   ---------------------
   -- Get_Full_Prefix --
   ---------------------

   function Get_Full_Prefix
     (This     : access Text_Interface'Class;
      Cursor   : Text_Cursor'Class;
      Category : Language_Category := Cat_Unknown)
      return String
   is
      It : Construct_Tree_Iterator;
   begin
      if Category = Cat_Unknown then
         It := Get_Iterator_At
           (Get_Tree (This.Get_Structured_File),
            (Absolute_Offset => False,
             Line            => Get_Line (Cursor),
             Line_Offset     => 0),
            Position => After);
      else
         It := Get_Iterator_At
           (Get_Tree (This.Get_Structured_File),
            (Absolute_Offset => False,
             Line            => Get_Line (Cursor),
             Line_Offset     => 0),
            Position => After,
            Categories_Seeked => (1 => Category));
      end if;

      declare
         It_Id : constant Composite_Identifier :=
           To_Composite_Identifier
             (Get_Full_Name (Get_Tree (Get_Structured_File (This)), It));
      begin
         if Length (It_Id) <= 1 then
            return "";
         else
            return To_String (Get_Slice (It_Id, 1, Length (It_Id) - 1));
         end if;
      end;
   end Get_Full_Prefix;

   ---------------
   -- Next_Word --
   ---------------

   procedure Next_Word
     (This   : Text_Interface'Class;
      Cursor : in out Text_Cursor'Class;
      Word   : out Word_Cursor)
   is
      Current_Line      : Unbounded_String;
      Line_Cursor       : Text_Cursor := Text_Cursor (Cursor);
      Begin_Word        : String_Index_Type;
      Cursor_Char_Index : String_Index_Type;
   begin
      Word.Mode := Text_Ascii;
      Word.File := This.File_Name;

      Line_Cursor.Col := 1;
      Current_Line := To_Unbounded_String (Get_Line (This, Line_Cursor));
      Cursor_Char_Index := To_Char_Index
        (Get_Column (Cursor), To_String (Current_Line));

      while Is_Blank
        (Slice
           (Current_Line, Natural (Cursor_Char_Index), Length (Current_Line)))
      loop
         Cursor_Char_Index := 1;
         Cursor.Line := Cursor.Line + 1;
         Current_Line := To_Unbounded_String (Get_Line (This, Cursor));
      end loop;

      while Is_Blank (Element (Current_Line, Natural (Cursor_Char_Index))) loop
         Cursor_Char_Index := Cursor_Char_Index + 1;
      end loop;

      Begin_Word := Cursor_Char_Index;

      while Natural (Cursor_Char_Index) < Length (Current_Line)
        and then not Is_Blank
          (Element (Current_Line, Natural (Cursor_Char_Index)))
        and then not Is_Separator
          (Element (Current_Line, Natural (Cursor_Char_Index)))
      loop
         Cursor_Char_Index := Cursor_Char_Index + 1;
      end loop;

      if Is_Blank (Element (Current_Line, Natural (Cursor_Char_Index)))
        or else
          (Is_Separator (Element (Current_Line, Natural (Cursor_Char_Index)))
           and then (Begin_Word /= Cursor_Char_Index))
      then
         --  If the last character is a blank, or a separator and the current
         --  word is not a separator, then forget about the last cursor.

         Word.String_Match :=
           Unbounded_Slice
             (Current_Line,
              Natural (Begin_Word),
              Natural (Cursor_Char_Index) - 1);
      else
         --  Otherwise, we're at the end of the line, get the whole parsed
         --  data.

         Word.String_Match :=
           Unbounded_Slice
             (Current_Line,
              Natural (Begin_Word),
              Natural (Cursor_Char_Index));
      end if;

      Word.Line := Line_Cursor.Line;
      Word.Col := To_Column_Index (Begin_Word, To_String (Current_Line));

      if Cursor_Char_Index = String_Index_Type (Length (Current_Line)) then
         Cursor_Char_Index := 1;
         Cursor.Line := Cursor.Line + 1;
      else
         Cursor_Char_Index := Cursor_Char_Index + 1;
      end if;

      Cursor.Col :=
        To_Column_Index (Cursor_Char_Index, To_String (Current_Line));
   end Next_Word;

   -------------------------
   -- Get_Structured_File --
   -------------------------

   function Get_Structured_File
     (This : access Text_Interface'Class) return Structured_File_Access is
   begin
      Update_Structure_If_Needed (This);

      return This.Construct_File;
   end Get_Structured_File;

   --------------------------------
   -- Update_Structure_If_Needed --
   --------------------------------

   procedure Update_Structure_If_Needed (This : access Text_Interface'Class) is
   begin
      if not This.Structure_Up_To_Date.all then
         Update_Contents (This.Construct_Db, This.File_Name);

         This.Construct_File := Get_Or_Create
           (This.Construct_Db, This.File_Name);

         This.Structure_Up_To_Date.all := True;
      end if;
   end Update_Structure_If_Needed;

   ----------------------
   -- Text_Has_Changed --
   ----------------------

   procedure Text_Has_Changed (This : in out Text_Interface'Class) is
   begin
      This.Structure_Up_To_Date.all := False;
   end Text_Has_Changed;

   -------------------
   -- Previous_Char --
   -------------------

   function Previous_Char
     (This : Text_Interface'Class; Cursor : Text_Cursor'Class)
     return Text_Cursor'Class
   is
      Result, Line_Cursor : Text_Cursor := Text_Cursor (Cursor);
      Current_Line        : Unbounded_String;
   begin
      Line_Cursor.Col := 1;

      Current_Line := To_Unbounded_String (Get_Line (This, Line_Cursor));

      loop
         Result.Col := Result.Col - 1;

         if Result.Col = 0 then
            Result.Line := Result.Line - 1;
            Line_Cursor.Line := Line_Cursor.Line - 1;

            if Line_Cursor.Line = 0 then
               raise Codefix_Panic;
            end if;

            Current_Line := To_Unbounded_String (Get_Line (This, Line_Cursor));
            Result.Col := To_Column_Index
              (String_Index_Type (Length (Current_Line)), Current_Line);
         end if;

         if not Is_Blank
           (Element
              (Current_Line,
               Natural (To_Char_Index (Result.Col, Current_Line))))
         then
            return Result;
         end if;
      end loop;
   end Previous_Char;

   --------------------
   -- Parse_Entities --
   --------------------

   procedure Parse_Entities
     (Lang     : access Language_Root'Class;
      This     : Text_Interface'Class;
      Callback : Codefix_Entity_Callback;
      Start    : Text_Cursor'Class)
   is
      Stop : Boolean := False;
      Line_Offset, Col_Offset : Integer;

      First_Line : constant String := Get_Line (This, Start, 1);
      Cursor     : Text_Cursor := Text_Cursor (Start);
      Last_Line  : constant Integer := Line_Max (This);

   begin
      Line_Offset := Get_Line (Start) - 1;
      Col_Offset := Integer
        (To_Column_Index
           (String_Index_Type (Get_Column (Start)), First_Line)) - 1;

      while not Stop and then Line_Offset + 1 <= Last_Line loop
         declare
            --  ??? The & " " at the end of the line is a temporary workaround
            --  because the ada parser does not seems to be able to retreive
            --  single characters constructs at the end of the buffer. to
            --  be investigated.
            Line : constant String := Get_Line (This, Cursor, 1) & " ";

            function Internal_Callback
              (Entity         : Language_Entity;
               Sloc_Start     : Source_Location;
               Sloc_End       : Source_Location;
               Partial_Entity : Boolean) return Boolean;

            -----------------------
            -- Internal_Callback --
            -----------------------

            function Internal_Callback
              (Entity         : Language_Entity;
               Sloc_Start     : Source_Location;
               Sloc_End       : Source_Location;
               Partial_Entity : Boolean) return Boolean
            is
               Adjusted_Sloc_Start, Adjusted_Sloc_End : Source_Location;
            begin
               Adjusted_Sloc_Start := Sloc_Start;
               Adjusted_Sloc_End := Sloc_End;

               Adjusted_Sloc_Start.Column :=
                 Adjusted_Sloc_Start.Column + Col_Offset;
               Adjusted_Sloc_End.Column :=
                 Adjusted_Sloc_End.Column + Col_Offset;

               Adjusted_Sloc_Start.Line :=
                 Adjusted_Sloc_Start.Line + Line_Offset;
               Adjusted_Sloc_End.Line :=
                 Adjusted_Sloc_End.Line + Line_Offset;

               Stop :=
                 Callback
                   (Entity,
                    Adjusted_Sloc_Start,
                    Adjusted_Sloc_End,
                    Partial_Entity,
                    Line);

               return Stop;
            end Internal_Callback;
         begin
            Parse_Entities
              (Lang,
               Line (1 + Col_Offset .. Line'Last),
               Internal_Callback'Unrestricted_Access);
            Col_Offset := 0;
            Line_Offset := Line_Offset + 1;
         end;

         Set_Location (Cursor, Get_Line (Cursor) + 1, 1);
      end loop;
   end Parse_Entities;

   ------------------------------
   -- Parse_Entities_Backwards --
   ------------------------------

   procedure Parse_Entities_Backwards
     (Lang     : access Language_Root'Class;
      This     : in out Text_Interface'Class;
      Callback : access procedure (Buffer : Unbounded_String;
                                   Token  : Language.Token_Record;
                                   Stop   : in out Boolean);
      Start    : File_Cursor'Class)
   is
      Contents : constant Unbounded_String := This.Read_File;
      Offset   : String_Index_Type;

      procedure Internal_Callback
        (Token  : Language.Token_Record;
         Stop   : in out Boolean);

      procedure Internal_Callback
        (Token  : Language.Token_Record;
         Stop   : in out Boolean) is
      begin
         Callback.all (Contents, Token, Stop);
      end Internal_Callback;

   begin
      Offset := To_String_Index
        (This.Get_Structured_File,
         Start.Line,
         Start.Col);

      Lang.Parse_Tokens_Backwards
        (Buffer       => To_String (Contents),
         Start_Offset => Offset,
         Callback     => Internal_Callback'Access);
   end Parse_Entities_Backwards;

   ----------------------------------------------------------------------------
   --  type Text_Cursor
   ----------------------------------------------------------------------------

   ----------
   -- Free --
   ----------

   procedure Free (This : in out File_Cursor) is
      pragma Unreferenced (This);
   begin
      null;
   end Free;

   -----------
   -- Clone --
   -----------

   function Clone (This : File_Cursor) return File_Cursor is
      New_Cursor : File_Cursor := This;
   begin
      New_Cursor.File := This.File;
      return New_Cursor;
   end Clone;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (This           : in out Text_Navigator_Abstr'Class;
      Position       : File_Cursor'Class;
      Len            : Integer;
      New_Text       : String;
      Blanks_Before  : Replace_Blanks_Policy;
      Blanks_After   : Replace_Blanks_Policy)
   is
      Dest_Start : File_Cursor'Class := Clone (Position);
      Dest_Stop  : File_Cursor'Class := Clone (Position);
      Line       : constant String := This.Get_Line (Position, 1);
   begin
      Dest_Stop.Col := To_Column_Index
        (To_Char_Index
           (Dest_Start.Col, Line) + String_Index_Type (Len) - 1, Line);

      Replace
        (This          => This,
         Dest_Start    => Dest_Start,
         Dest_Stop     => Dest_Stop,
         New_Text      => New_Text,
         Blanks_Before => Blanks_Before,
         Blanks_After  => Blanks_After);

      Free (Dest_Start);
      Free (Dest_Stop);
   end Replace;

   procedure Replace
     (This                  : in out Text_Navigator_Abstr'Class;
      Dest_Start, Dest_Stop : in out File_Cursor'Class;
      New_Text              : String;
      Blanks_Before         : Replace_Blanks_Policy := Keep;
      Blanks_After          : Replace_Blanks_Policy := Keep)
   is
      Dest_Text : constant Ptr_Text := This.Get_File (Dest_Start.File);

      Mark_Start, Mark_Stop : Ptr_Mark;

      type Bound_Type is (First, Last);

      procedure Replace_Blank_Slice
        (Around : Text_Cursor;
         Policy : Replace_Blanks_Policy;
         Pos    : Bound_Type);

      procedure Replace_Blank_Slice
        (Around : Text_Cursor;
         Policy : Replace_Blanks_Policy;
         Pos    : Bound_Type)
      is
         Line : constant String := Dest_Text.Get_Line (Around, 1);
         Begin_Text : String_Index_Type;
         End_Text   : String_Index_Type;
         Tmp_Cursor   : Text_Cursor := Around;
         Blank_Length : String_Index_Type;
      begin
         case Pos is
            when First =>
               End_Text := To_Char_Index (Tmp_Cursor.Col, Line);
               Begin_Text := End_Text - 1;

            when Last =>
               Begin_Text := To_Char_Index (Tmp_Cursor.Col, Line);
               End_Text := Begin_Text + 1;

         end case;

         Skip_Blanks (Line, Integer (End_Text));

         if Integer (Begin_Text) <= Line'Last then
            Skip_Blanks_Backward (Line, Integer (Begin_Text));
         end if;

         if Integer (Begin_Text) < Line'First then
            Tmp_Cursor.Col := 1;
         else
            Tmp_Cursor.Col := To_Column_Index (Begin_Text, Line) + 1;
         end if;

         Blank_Length := End_Text - Begin_Text - 1;

         case Policy is
            when Keep =>
               null;
            when One =>
               Dest_Text.Replace (Tmp_Cursor, Integer (Blank_Length), " ");
            when None =>
               Dest_Text.Replace (Tmp_Cursor, Integer (Blank_Length), "");
         end case;
      end Replace_Blank_Slice;

      Last_Begin_Line : Integer;
      --  Index of the last start of line in New_Text.

   begin
      if Dest_Stop in Word_Cursor'Class then
         --  If Dest_Stop is a word cursor, then create a simple file cursor
         --  out of it an re-call replace with the simpler parameter.

         declare
            New_Stop : File_Cursor;
         begin
            New_Stop.Line := Dest_Stop.Line;
            New_Stop.File := Dest_Stop.File;
            New_Stop.Col := Dest_Stop.Col +
              Word_Cursor (Dest_Stop).Get_Matching_Word (This)'Length - 1;

            This.Replace
              (Dest_Start, New_Stop, New_Text, Blanks_Before, Blanks_After);

            Dest_Stop.Col := New_Stop.Col;
            Dest_Stop.Line := New_Stop.Line;

            Free (New_Stop);

            return;
         end;
      else
         --  Otherwise, just preform the actual replacement, and then
         --  adjustments

         Dest_Text.Replace (Dest_Start, Dest_Stop, New_Text);
      end if;

      Dest_Stop.Line := Dest_Start.Line;

      Last_Begin_Line := New_Text'First;

      for J in New_Text'First .. New_Text'Last - EOL_Str'Length + 1 loop
         if New_Text (J .. J + EOL_Str'Length - 1) = EOL_Str then
            Dest_Stop.Line := Dest_Stop.Line + 1;

            Last_Begin_Line := J + EOL_Str'Length;
         end if;
      end loop;

      if Last_Begin_Line = New_Text'First then
         --  If there is only one line, then add the length of the added text
         --  to the position of the first column to have the new last column

         declare
            Line  : constant String := This.Get_Line (Dest_Start, 1);
            Index : constant String_Index_Type :=
              To_Char_Index (Dest_Start.Col, Line)
              + New_Text'Length - 1;
         begin
            Dest_Stop.Col := To_Column_Index (Index, Line);
         end;
      else
         --  If there are several lines, then get the position of the last one.

         declare
            subtype Line_Indexes is String
              (1 .. New_Text'Last - Last_Begin_Line + 1);
         begin
            Dest_Stop.Col :=
              To_Column_Index
                (String_Index_Type (New_Text'Last - Last_Begin_Line + 1),
                 Line_Indexes (New_Text (Last_Begin_Line .. New_Text'Last)));
         end;
      end if;

      Mark_Start := new Mark_Abstr'Class'(This.Get_New_Mark (Dest_Start));
      Mark_Stop := new Mark_Abstr'Class'(This.Get_New_Mark (Dest_Stop));

      File_Cursor (Dest_Start) :=
        File_Cursor (This.Get_Current_Cursor (Mark_Start.all));
      Replace_Blank_Slice (Text_Cursor (Dest_Start), Blanks_Before, First);

      File_Cursor (Dest_Stop) :=
        File_Cursor (This.Get_Current_Cursor (Mark_Stop.all));
      Replace_Blank_Slice (Text_Cursor (Dest_Stop), Blanks_After, Last);

      Dest_Text.Indent_Line (Dest_Start);
      Dest_Text.Indent_Line (Dest_Stop);

      File_Cursor (Dest_Start) :=
        File_Cursor (This.Get_Current_Cursor (Mark_Start.all));
      File_Cursor (Dest_Stop) :=
        File_Cursor (This.Get_Current_Cursor (Mark_Stop.all));

      Free (Mark_Start);
      Free (Mark_Stop);
   end Replace;

   -----------
   -- Erase --
   -----------

   procedure Erase
     (This            : in out Text_Interface'Class;
      Start, Stop     : File_Cursor'Class)
   is
   begin
      This.Replace (Start, Stop, "");

      if Is_Blank (This.Get_Line (Start, 1)) then
         This.Delete_Line (Start);
      end if;
   end Erase;

   -------------
   -- Comment --
   -------------

   procedure Comment
     (This        : in out Text_Interface'Class;
      Start, Stop : File_Cursor'Class)
   is
      Line_Cursor      : File_Cursor := File_Cursor (Start);
      Start_Char_Index : String_Index_Type;
      Stop_Char_Index : String_Index_Type;
      Tmp_Cursor       : File_Cursor;
   begin
      Line_Cursor.Col := 1;

      if Start.Line = Stop.Line then
         --  Add a new line if there is some text after the entity that has
         --  not to be commented

         declare
            Current_String : constant String := This.Get_Line (Start, 1);
         begin
            Start_Char_Index := To_Char_Index (Start.Col, Current_String);
            Stop_Char_Index := To_Char_Index (Stop.Col, Current_String);

            declare
               Back : constant String := Current_String
                 (Integer (Stop_Char_Index) + 1 .. Current_String'Last);
            begin
               if not Is_Blank (Back) then
                  This.Add_Line (Stop, Back, False);
                  Tmp_Cursor := File_Cursor (Stop);
                  Tmp_Cursor.Col := Tmp_Cursor.Col + 1;
                  This.Replace
                    (Tmp_Cursor, Back'Length, "");

                  --  The indentation must be done after the comment, otherwise
                  --  the file may not be semantically correct while indenting.

                  Tmp_Cursor.Line := Tmp_Cursor.Line + 1;
                  Tmp_Cursor.Col := 1;
                  This.Indent_Line (Tmp_Cursor);
               end if;
            end;

            --  But proper comment at the begining of the entity

            if Is_Blank (Current_String
                         (1 .. Natural (Start_Char_Index) - 1))
            then
               Line_Cursor.Col := 1;
               This.Replace (Line_Cursor, 0, "--  ");
            else
               Line_Cursor.Col := Start.Col;
               This.Replace (Line_Cursor, 0, "--  ");
            end if;
         end;
      else
         --  But proper comment at the begining of the entity

         declare
            Current_String : constant String := This.Get_Line (Start, 1);
         begin
            Start_Char_Index := To_Char_Index (Start.Col, Current_String);

            if Is_Blank
              (Current_String (1 .. Natural (Start_Char_Index) - 1))
            then
               This.Replace (Line_Cursor, 0, "--  ");
            else
               This.Replace (Start, 0, "--  ");
            end if;

            for J in Start.Line + 1 .. Stop.Line - 1 loop
               Line_Cursor.Line := J;
               This.Replace (Line_Cursor, 0, "--  ");
            end loop;
         end;

         Line_Cursor := File_Cursor (Stop);
         Line_Cursor.Col := 1;

         --  Add a new line if there is some text after the entity that has
         --  not to be commented

         declare
            Current_String : constant String := This.Get_Line (Stop);
            Back           : constant String := Current_String
              (Current_String'First + 1 .. Current_String'Last);
         begin
            if not Is_Blank (Back) then
               This.Add_Line (Stop, Back, False);

               Tmp_Cursor := File_Cursor (Stop);
               Tmp_Cursor.Col := Tmp_Cursor.Col + 1;
               This.Replace
                 (Tmp_Cursor, Back'Length, "");

               This.Replace (Line_Cursor, 0, "--  ");

               --  The indentation must be done after the comment, otherwise
               --  the file may not be semantically correct while indenting.

               Tmp_Cursor.Line := Tmp_Cursor.Line + 1;
               Tmp_Cursor.Col := 1;
               This.Indent_Line (Tmp_Cursor);
            else
               This.Replace (Line_Cursor, 0, "--  ");
            end if;
         end;
      end if;
   end Comment;

   ----------
   -- Free --
   ----------

   overriding procedure Free (This : in out Word_Cursor) is
   begin
      Free (File_Cursor (This));
   end Free;

   -----------
   -- Clone --
   -----------

   overriding function Clone (This : Word_Cursor) return Word_Cursor is
   begin
      return (Clone (File_Cursor (This)) with This.String_Match, This.Mode);
   end Clone;

   --------------------
   -- Make_Word_Mark --
   --------------------

   procedure Make_Word_Mark
     (Word         : Word_Cursor;
      Current_Text : Text_Navigator_Abstr'Class;
      Mark         : out Word_Mark) is
   begin
      Mark.Mode := Word.Mode;
      Mark.String_Match := Word.String_Match;
      Mark.Mark_Id := new Mark_Abstr'Class'(Get_New_Mark (Current_Text, Word));
   end Make_Word_Mark;

   ----------------------
   -- Make_Word_Cursor --
   ----------------------

   procedure Make_Word_Cursor
     (Word         : Word_Mark;
      Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : out Word_Cursor)
   is
      Curs : constant File_Cursor'Class :=
        Get_Current_Cursor (Current_Text, Word.Mark_Id.all);
   begin
      Set_File     (Cursor, Get_File (Curs));
      Set_Location (Cursor, Get_Line (Curs), Get_Column (Curs));

      if Word.String_Match = Null_Unbounded_String then
         Set_Word (Cursor, Null_Unbounded_String, Word.Mode);
      else
         Set_Word (Cursor, Word.String_Match, Word.Mode);
      end if;
   end Make_Word_Cursor;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Word_Mark) is
   begin
      Free (This.Mark_Id);
   end Free;

   ------------------
   -- Set_Registry --
   ------------------

   procedure Set_Registry
     (Text   : in out Text_Navigator_Abstr;
      Registry : Project_Registry_Access) is
   begin
      Text.Registry := Registry;
   end Set_Registry;

   ------------------
   -- Get_Registry --
   ------------------

   function Get_Registry
     (Text : Text_Navigator_Abstr)
      return Project_Registry_Access is
   begin
      return Text.Registry;
   end Get_Registry;

   ----------------------------
   -- Set_Construct_Database --
   ----------------------------

   procedure Set_Construct_Database
     (Text : in out Text_Navigator_Abstr;
      Db   : Construct_Database_Access)
   is
   begin
      Text.Construct_Db := Db;
   end Set_Construct_Database;

   ----------------------------
   -- Get_Construct_Database --
   ----------------------------

   function Get_Construct_Database
     (Text : Text_Navigator_Abstr) return Construct_Database_Access
   is
   begin
      return Text.Construct_Db;
   end Get_Construct_Database;

   -----------------
   -- Set_Context --
   -----------------

   procedure Set_Context
     (Text    : in out Text_Navigator_Abstr;
      Context : Factory_Context)
   is
   begin
      Text.Context := Context;
   end Set_Context;

   -----------------
   -- Get_Context --
   -----------------

   function Get_Context
     (Text : Text_Navigator_Abstr)
      return Factory_Context is
   begin
      return Text.Context;
   end Get_Context;

   ----------------------
   -- Get_Body_Or_Spec --
   ----------------------

   function Get_Body_Or_Spec
     (Text : Text_Navigator_Abstr; File_Name : Virtual_File)
      return Virtual_File is
   begin
      return Get_Registry (Text).Tree.Other_File (File_Name);
   end Get_Body_Or_Spec;

   --------------
   -- Get_Line --
   --------------

   function Get_Line (This : Text_Cursor) return Integer is
   begin
      return This.Line;
   end Get_Line;

   ----------------
   -- Get_Column --
   ----------------

   function Get_Column (This : Text_Cursor) return Visible_Column_Type is
   begin
      return This.Col;
   end Get_Column;

   --------------
   -- Get_File --
   --------------

   function Get_File (This : File_Cursor) return GNATCOLL.VFS.Virtual_File is
   begin
      return This.File;
   end Get_File;

   --------------
   -- Set_Line --
   --------------

   procedure Set_Line (This : in out Text_Cursor; Line : Natural) is
   begin
      This.Line := Line;
   end Set_Line;

   ----------------
   -- Set_Column --
   ----------------

   procedure Set_Column
     (This : in out Text_Cursor; Column : Visible_Column_Type)
   is
   begin
      This.Col := Column;
   end Set_Column;

   ------------------
   -- Set_Location --
   ------------------

   procedure Set_Location
     (This : in out Text_Cursor;
      Line : Natural; Column : Visible_Column_Type)
   is
   begin
      This.Line := Line;
      This.Col := Column;
   end Set_Location;

   --------------
   -- Set_File --
   --------------

   procedure Set_File (This : in out File_Cursor; File : Virtual_File) is
   begin
      This.File := File;
   end Set_File;

   --------------
   -- Set_Word --
   --------------

   procedure Set_Word
     (Word         : in out Word_Cursor;
      String_Match : Unbounded_String;
      Mode         : String_Mode := Text_Ascii) is
   begin
      Word.String_Match := String_Match;
      Word.Mode         := Mode;
   end Set_Word;

   --------------
   -- Get_Word --
   --------------

   function Get_Word (Word : Word_Cursor) return String is
   begin
      return To_String (Word.String_Match);
   end Get_Word;

   -----------------------
   -- Get_Matching_Word --
   -----------------------

   function Get_Matching_Word
     (Word  : Word_Cursor;
      Text  : Text_Navigator_Abstr'Class;
      Check : Boolean := False) return String
   is
   begin
      case Word.Mode is
         when Text_Ascii =>
            if not Check then
               return To_String (Word.String_Match);
            end if;

            declare
               Str_Parsed : constant String := Text.Get_Line (Word);
            begin
               if Str_Parsed'Length < Length (Word.String_Match) or else
                 Str_Parsed
                   (Str_Parsed'First ..
                        Str_Parsed'First + Length (Word.String_Match) - 1)
                     /= Word.String_Match
               then
                  raise Codefix_Panic with "string '"
                    & To_String (Word.String_Match)
                    & "' in '"
                    & Str_Parsed & "' can't be found";
               else
                  return To_String (Word.String_Match);
               end if;
            end;
         when Regular_Expression =>
            declare
               Matches    : Match_Array (0 .. 1);
               Matcher    : constant Pattern_Matcher := Compile
                 (To_String (Word.String_Match));
               Str_Parsed : constant String := Text.Get_Line (Word, 1);
               Index      : constant String_Index_Type :=
                 To_Char_Index (Word.Col, Str_Parsed);
            begin
               Match
                 (Matcher,
                  Str_Parsed (Integer (Index) .. Str_Parsed'Last), Matches);

               if Matches (0) = No_Match then
                  raise Codefix_Panic with "pattern '"
                    & To_String (Word.String_Match)
                    & "' in '"
                    & Str_Parsed & "' can't be found";
               else
                  return Str_Parsed (Matches (1).First .. Matches (1).Last);
               end if;
            end;
      end case;
   end Get_Matching_Word;

   ----------------------------------------------------------------------------
   --  type Text_Command
   ----------------------------------------------------------------------------

   ---------------
   -- Free_Data --
   ---------------

   procedure Free_Data (This : in out Text_Command'Class) is
   begin
      Free (This);
   end Free_Data;

   procedure Free (This : in out Ptr_Command) is
      procedure Unchecked_Free is new
        Ada.Unchecked_Deallocation (Text_Command'Class, Ptr_Command);
   begin
      if This /= null then
         Free (This.all);
         Unchecked_Free (This);
      end if;
   end Free;

   -----------------
   -- Set_Caption --
   -----------------

   procedure Set_Caption
     (This    : in out Text_Command'Class;
      Caption : Unbounded_String) is
   begin
      This.Caption := Caption;
   end Set_Caption;

   -----------------
   -- Get_Caption --
   -----------------

   function Get_Caption (This : Text_Command'Class) return String is
   begin
      if This.Caption = Null_Unbounded_String then
         return "fix not documented";
      else
         return To_String (This.Caption);
      end if;
   end Get_Caption;

   function Get_Parser
     (This : Text_Command'Class) return Error_Parser_Access is
   begin
      return This.Parser;
   end Get_Parser;

   procedure Set_Parser
     (This : in out Text_Command'Class;
      Parser : Error_Parser_Access) is
   begin
      This.Parser := Parser;
   end Set_Parser;

   procedure Free (Corruption : in out Execute_Corrupted) is
      procedure Internal_Free is new Ada.Unchecked_Deallocation
        (Execute_Corrupted_Record'Class, Execute_Corrupted);
   begin
      Internal_Free (Corruption);
   end Free;

   procedure Secured_Execute
     (This         : Text_Command'Class;
      Current_Text : in out Text_Navigator_Abstr'Class;
      Error_Cb     : Execute_Corrupted := null)
   is
      Saved_Params : Indent_Parameters;
      New_Params   : Indent_Parameters;
      Saved_Style  : Indentation_Kind;

   begin
      Get_Indentation_Parameters (Ada_Lang, Saved_Params, Saved_Style);

      --  Disable Casing_Policy while we execute the fix.

      New_Params := Saved_Params;
      New_Params.Casing_Policy := Disabled;
      Set_Indentation_Parameters (Ada_Lang, New_Params, Saved_Style);

      Execute (This, Current_Text);
      Set_Indentation_Parameters (Ada_Lang, Saved_Params, Saved_Style);
   exception
      when E : Obsolescent_Fix =>
         if Error_Cb /= null then
            Error_Cb.Obsolescent (Exception_Information (E));
         end if;

      when E : Codefix_Panic =>
         if Error_Cb /= null then
            Error_Cb.Panic (Exception_Information (E));
         end if;

   end Secured_Execute;

end Codefix.Text_Manager;
