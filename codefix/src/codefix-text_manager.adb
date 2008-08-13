-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2003-2008, AdaCore                 --
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

with Ada.Exceptions;    use Ada.Exceptions;
with GNAT.Case_Util;    use GNAT.Case_Util;
with GNAT.Regpat;       use GNAT.Regpat;
with GNATCOLL.Utils;    use GNATCOLL.Utils;

with Ada_Analyzer;      use Ada_Analyzer;
with Language.Ada;      use Language.Ada;
with Projects;          use Projects;
with Projects.Registry; use Projects.Registry;
with String_Utils;      use String_Utils;
with GNATCOLL.VFS;               use GNATCOLL.VFS;

with Ada_Semantic_Tree.Parts; use Ada_Semantic_Tree.Parts;
with Language.Tree.Ada;       use Language.Tree.Ada;

package body Codefix.Text_Manager is

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

   ---------------
   -- Normalize --
   ---------------

   function Normalize (Str : GNAT.Strings.String_Access) return String is
   begin
      if Str /= null then
         return Reduce (Str.all);
      else
         return "";
      end if;
   end Normalize;

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
         when '.' | ',' | ';' => return True;
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

   function "=" (Left, Right : File_Cursor) return Boolean is
   begin
      return Left.Line = Right.Line
        and then Left.Col = Right.Col
        and then Left.File = Right.File;
   end "=";

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : File_Cursor) return Boolean is
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

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Text_Navigator_Abstr) is
   begin
      Free (This.Files.all);
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
      Position          : Relative_Position := Specified;
      Categories_Seeked : Category_Array := Null_Category_Array)
      return Construct_Tree_Iterator is
   begin
      return Get_Iterator_At
        (Get_File (Current_Text, Get_File (Cursor)),
         Text_Cursor (Cursor),
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
      Start_Col : Column_Index := 0) return String is
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
     (This : Text_Navigator_Abstr;
      File_Name : Virtual_File) return GNAT.Strings.String_Access is
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
      Iterator    : Text_List.List_Node := First (This.Files.all);
      New_Text    : Ptr_Text;
   begin
      while Iterator /= Text_List.Null_Node loop
         if Get_File_Name (Data (Iterator).all) = Name then
            return Data (Iterator);
         end if;

         Iterator := Next (Iterator);
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

   -------------------
   -- Search_String --
   -------------------

   function Search_Token
     (This     : Text_Navigator_Abstr'Class;
      Cursor   : File_Cursor'Class;
      Searched : Token_Record;
      Step     : Step_Way := Normal_Step)
      return File_Cursor'Class is
   begin
      return Search_Token
        (Get_File (This, Get_File (Cursor)).all,
         File_Cursor (Cursor),
         Searched,
         Step);
   end Search_Token;

   --------------------
   -- Search_Strings --
   --------------------

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

   -------------------
   -- Get_Full_Name --
   -------------------

   function Get_Full_Prefix
     (This     : Text_Navigator_Abstr'Class;
      Cursor   : File_Cursor'Class;
      Category : Language_Category := Cat_Unknown)
      return String is
   begin
      return Get_Full_Prefix
        (Get_File (This, Get_File (Cursor)), Cursor, Category);
   end Get_Full_Prefix;

   ---------------------
   -- Get_Right_Paren --
   ---------------------

   function Get_Right_Paren
     (Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class)
     return File_Cursor'Class is

      Result      : File_Cursor;
      Line_Cursor : File_Cursor := File_Cursor (Cursor);

   begin
      Line_Cursor.Col := 1;

      Result :=
        (Text_Cursor (Get_Right_Paren
                        (Get_File (Current_Text, Get_File (Cursor)).all,
                         Cursor,
                         Get_Line (Current_Text, Line_Cursor)))
         with File => Get_File (Cursor));

      return Result;
   end Get_Right_Paren;

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
      Unit_Info := Get_Iterator_At (Current_Text, Cursor);

      if Get_Construct (Unit_Info).Is_Declaration then
         Body_Info := To_Construct_Tree_Iterator (Get_Second_Occurence
           (To_Entity_Access
              (Get_Structured_File (Current_Text, Cursor), Unit_Info)));
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
           (Char_Index (Get_Construct (Body_Info).Sloc_Start.Column),
            Get_Line (Current_Text, Body_Begin));
         Body_End.Col := To_Column_Index
           (Char_Index (Get_Construct (Body_Info).Sloc_End.Column),
            Get_Line (Current_Text, Body_End));

         Spec_Begin.Col := 1;
         Spec_End.Col := 1;
         Spec_Begin.Line := Get_Construct (Unit_Info).Sloc_Start.Line;
         Spec_End.Line := Get_Construct (Unit_Info).Sloc_End.Line;

         Spec_Begin.Col := To_Column_Index
           (Char_Index (Get_Construct (Unit_Info).Sloc_Start.Column),
            Get_Line (Current_Text, Spec_Begin));
         Spec_End.Col := To_Column_Index
           (Char_Index (Get_Construct (Unit_Info).Sloc_End.Column),
            Get_Line (Current_Text, Spec_End));
      else
         Set_File (Body_Begin, Get_File (Cursor));
         Set_File (Body_End,   Get_File (Cursor));

         Body_Begin.Col := 1;
         Body_End.Col := 1;
         Body_Begin.Line := Get_Construct (Unit_Info).Sloc_Start.Line;
         Body_End.Line := Get_Construct (Unit_Info).Sloc_End.Line;

         Body_Begin.Col := To_Column_Index
           (Char_Index (Get_Construct (Unit_Info).Sloc_Start.Column),
            Get_Line (Current_Text, Body_Begin));
         Body_End.Col := To_Column_Index
           (Char_Index (Get_Construct (Unit_Info).Sloc_End.Column),
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
      Word   : out GNAT.Strings.String_Access) is
   begin
      Next_Word
        (Get_File (This, Get_File (Cursor)).all,
         Cursor,
         Word);
   end Next_Word;

   -------------------
   -- Get_Structure --
   -------------------

   function Get_Structure
     (This      : Text_Navigator_Abstr'Class;
      File_Name : GNATCOLL.VFS.Virtual_File) return Construct_List_Access is
   begin
      return Get_Structure (Get_File (This, File_Name));
   end Get_Structure;

   ----------------
   -- Update_All --
   ----------------

   procedure Update_All
     (This : Text_Navigator_Abstr'Class)
   is
      Node : Text_List.List_Node := First (This.Files.all);
   begin
      while Node /= Text_List.Null_Node loop
         Constrain_Update (Data (Node).all);
         Node := Next (Node);
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

   --------------
   -- Get_Tree --
   --------------

   function Get_Structured_File
     (This : Text_Navigator_Abstr'Class; Cursor : File_Cursor'Class)
      return Structured_File_Access is
   begin
      return Get_Structured_File (Get_File (This, Get_File (Cursor)));
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

   ----------------------------------------------------------------------------
   --  type Text_Interface
   ----------------------------------------------------------------------------

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Text_Interface) is
   begin
      Free (This.Structure.all);
      Free (This.Structure);
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
            Integer (To_Char_Index (Get_Column (Cursor), Line_Cursor))),
         Start_Name,
         Position,
         Categories_Seeked);

      if It = Null_Construct_Tree_Iterator then
         Raise_Exception
           (Codefix_Panic'Identity,
            "Cannot retreive an entity with the given cursor");
      else
         return It;
      end if;
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
           Get_Line (Text_Interface'Class (This), C);
         Next_Cursor : Text_Cursor;
      begin
         if C.Line = Stop.Line then
            return Before & Line
              (Line'First
               .. Integer
                 (To_Char_Index
                    (Stop.Col,
                     Get_Line (Text_Interface'Class (This), C, 1))));
         else
            Next_Cursor := C;
            Next_Cursor.Line := Next_Cursor.Line + 1;
            Next_Cursor.Col := 1;

            return Before
              & Line & Recursive_Get (Next_Cursor, (1 => ASCII.LF));
         end if;
      end Recursive_Get;

   begin
      return Recursive_Get (Text_Cursor (Start), "");
   end Get;

   ------------------------
   -- Remove_Emtpy_Lines --
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

   ----------------
   -- Get_Buffer --
   ----------------

   function Get_Buffer
     (This : access Text_Interface'Class) return GNAT.Strings.String_Access is
   begin
      if This.Buffer = null then
         This.Buffer := Read_File (This.all);
      end if;

      return This.Buffer;
   end Get_Buffer;

   -------------------
   -- Search_String --
   -------------------

   function Search_Token
     (This     : Text_Interface'Class;
      Cursor   : Text_Cursor'Class;
      Searched : Token_Record;
      Step     : Step_Way := Normal_Step) return File_Cursor'Class
   is
      Tokens : constant Token_List := (1 => Searched);
      Result : Word_Cursor'Class := Search_Tokens
        (This, Cursor, Tokens, Step);
      Real_Result : constant File_Cursor := Clone (File_Cursor (Result));
   begin
      Free (Result);

      return Real_Result;
   end Search_Token;

   --------------------
   -- Search_Strings --
   --------------------

   function Search_Tokens
     (Line     : String;
      Cursor   : File_Cursor'Class;
      Searched : Token_List;
      Step     : Step_Way := Normal_Step) return Word_Cursor'Class
   is
      --  ??? This is a bit Ada-specific (casing, use of Ada_Lang).

      Result        : Word_Cursor :=
        (File_Cursor (Cursor) with null, Text_Ascii);
      Start_Index   : Char_Index;
      Found         : Boolean := False;

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
         for J in Searched'Range loop
            if Entity = Searched (J).Kind
              and then Equal
                (Line (Sloc_Start.Index .. Sloc_End.Index),
                 Searched (J).Name.all,
                 False)
            then
               Found := True;

               if Result.String_Match /= null then
                  Free (Result.String_Match);
               end if;

               Result.Col := To_Column_Index
                 (Char_Index (Sloc_Start.Index), Line);
               Result.String_Match :=
                 new String'
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
         Start_Index := Char_Index (Line'Last);
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
      Old_Col            : Column_Index;
   begin
      New_Cursor := File_Cursor (Cursor);

      case Step is
         when Normal_Step =>
            Last := Line_Max (This);
            Increment := 1;
         when Reverse_Step =>
            Last := 1;
            Increment := -1;
            Old_Col := New_Cursor.Col;
            New_Cursor.Col := 1;

            New_Cursor.Col := Old_Col;
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
            Result := Clone (Result);
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
      Current_Info : Construct_Access;
      Result       : Simple_Construct_Information;
   begin
      Current_Info := Get_Structure (This).First;

      while Current_Info /= null loop
         if Current_Info.Category = Category
           and then
             (Name = "" or else Compare_Last (Current_Info.Name.all, Name))
         then
            To_Simple_Construct_Information (Current_Info.all, Result, False);

            return Result;
         end if;

         Current_Info := Current_Info.Next;
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
         It := Get_Iterator_At (This, Cursor, After);
      else
         It := Get_Iterator_At (This, Cursor, After, (1 => Category));
      end if;

      declare
         It_Id : constant Composite_Identifier :=
           To_Composite_Identifier
             (Get_Full_Name (Get_Tree (Get_Structured_File (This)), It));
      begin
         if Length (It_Id) = 1 then
            return "";
         else
            return To_String (Get_Slice (It_Id, 1, Length (It_Id) - 1));
         end if;
      end;
   end Get_Full_Prefix;

   ---------------------
   -- Get_Right_Paren --
   ---------------------

   function Get_Right_Paren
     (This         : Text_Interface'Class;
      Cursor       : Text_Cursor'Class;
      Current_Line : String) return Text_Cursor'Class
   is

      function Internal_Get_Right_Paren
        (Cursor       : Text_Cursor'Class;
         Index        : Char_Index;
         Current_Line : String) return Text_Cursor;
      --  This internal function works with a char index. This way, we'll only
      --  do the conversion to a column when returning the result.

      function Internal_Get_Right_Paren
        (Cursor : Text_Cursor'Class; Index : Char_Index; Current_Line : String)
         return Text_Cursor
      is
         Local_Cursor : Text_Cursor := Text_Cursor (Cursor);
         Local_Line   : GNAT.Strings.String_Access :=
                          new String'(Current_Line);
         Local_Cursor_Char_Index : Char_Index := Index;

      begin
         loop
            if Natural (Local_Cursor_Char_Index) > Local_Line'Last then
               Local_Cursor_Char_Index := 1;
               Local_Cursor.Col := 1;
               Local_Cursor.Line := Local_Cursor.Line + 1;
               Assign (Local_Line, Get_Line (This, Local_Cursor));
            end if;

            case Local_Line (Natural (Local_Cursor_Char_Index)) is
               when '(' =>
                  Local_Cursor_Char_Index := Local_Cursor_Char_Index + 1;

                  Local_Cursor := Internal_Get_Right_Paren
                    (Local_Cursor,
                     Local_Cursor_Char_Index,
                     Local_Line.all);

                  declare
                     Cursor_Line : Text_Cursor := Local_Cursor;
                  begin
                     Cursor_Line.Col := 1;
                     Assign (Local_Line, Get_Line (This, Cursor_Line));
                  end;

                  Local_Cursor_Char_Index :=
                    To_Char_Index (Local_Cursor.Col, Local_Line.all) + 1;

               when ')' =>
                  Local_Cursor.Col := To_Column_Index
                    (Local_Cursor_Char_Index, Local_Line.all);
                  Free (Local_Line);

                  return Local_Cursor;

               when others =>
                  Local_Cursor_Char_Index := Local_Cursor_Char_Index + 1;
            end case;
         end loop;
      end Internal_Get_Right_Paren;

   begin
      return Internal_Get_Right_Paren
        (Cursor,
         To_Char_Index (Get_Column (Cursor), Current_Line) + 1, Current_Line);
   end Get_Right_Paren;

   ---------------
   -- Next_Word --
   ---------------

   procedure Next_Word
     (This   : Text_Interface'Class;
      Cursor : in out Text_Cursor'Class;
      Word   : out GNAT.Strings.String_Access)
   is
      Current_Line      : GNAT.Strings.String_Access;
      Line_Cursor       : Text_Cursor := Text_Cursor (Cursor);
      Begin_Word        : Char_Index;
      Cursor_Char_Index : Char_Index;

   begin
      Line_Cursor.Col := 1;
      Assign (Current_Line, Get_Line (This, Line_Cursor));
      Cursor_Char_Index := To_Char_Index
        (Get_Column (Cursor), Current_Line.all);

      while Is_Blank
        (Current_Line (Natural (Cursor_Char_Index) .. Current_Line'Last)) loop
         Cursor_Char_Index := 1;
         Cursor.Line := Cursor.Line + 1;
         Assign (Current_Line, Get_Line (This, Cursor));
      end loop;

      while Is_Blank (Current_Line (Natural (Cursor_Char_Index))) loop
         Cursor_Char_Index := Cursor_Char_Index + 1;
      end loop;

      Begin_Word := Cursor_Char_Index;

      while Natural (Cursor_Char_Index) < Current_Line'Last
        and then not Is_Blank (Current_Line (Natural (Cursor_Char_Index)))
      loop
         Cursor_Char_Index := Cursor_Char_Index + 1;
      end loop;

      if Is_Blank (Current_Line (Natural (Cursor_Char_Index))) then
         Word := new String'
           (Current_Line
              (Natural (Begin_Word) .. Natural (Cursor_Char_Index) - 1));
      else
         Word := new String'
           (Current_Line
              (Natural (Begin_Word) .. Natural (Cursor_Char_Index)));
      end if;

      if Cursor_Char_Index = Char_Index (Current_Line'Last) then
         Cursor_Char_Index := 1;
         Cursor.Line := Cursor.Line + 1;
      else
         Cursor_Char_Index := Cursor_Char_Index + 1;
      end if;

      Cursor.Col := To_Column_Index (Cursor_Char_Index, Current_Line.all);
      Free (Current_Line);
   end Next_Word;

   -------------------
   -- Get_Structure --
   -------------------

   function Get_Structure
     (This : access Text_Interface'Class) return Construct_List_Access
   is
      procedure Display_Constructs;
      pragma Unreferenced (Display_Constructs);
      --  Debug procedure used to display the contents of New_Text

      -----------------------
      -- Display_Contructs --
      -----------------------

      procedure Display_Constructs is
         Current : Construct_Access := This.Structure.First;
      begin
         while Current /= null loop
            if Current.Name /= null then
               Put (Current.Name.all & ": ");
            end if;

            Put (Current.Category'Img);
            Put (", [" & Current.Sloc_Start.Line'Img & ", ");
            Put (Current.Sloc_Start.Column'Img);
            Put ("]-[");
            Put (Current.Sloc_End.Line'Img & ", ");
            Put (Current.Sloc_End.Column'Img);
            Put ("] (");

            if Current.Is_Declaration then
               Put ("spec");
            else
               Put ("body");
            end if;

            Put (")");
            New_Line;
            Current := Current.Next;
         end loop;
      end Display_Constructs;

   begin
      Update_Structure_If_Needed (This);

      return This.Structure;
   end Get_Structure;

   --------------
   -- Get_Tree --
   --------------

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
         Free (This.Structure);
         Free (This.Buffer);

         This.Structure := new Construct_List;
         This.Buffer := Read_File (This.all);

         --  ??? Should be language independent
         Analyze_Ada_Source
           (Buffer           => This.Buffer.all,
            Indent_Params    => Default_Indent_Parameters,
            Format           => False,
            Constructs       => This.Structure);

         --  ??? Should be language independent
         This.Construct_File := Get_Or_Create
           (This.Construct_Db,
            This.File_Name,
            Ada_Tree_Lang);

         This.Structure_Up_To_Date.all := True;
      end if;
   end Update_Structure_If_Needed;

   -----------------------
   --  Text_Has_Changed --
   -----------------------

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
      Current_Line        : GNAT.Strings.String_Access;
   begin
      Line_Cursor.Col := 1;

      Current_Line := new String'(Get_Line (This, Line_Cursor));

      loop
         Result.Col := Result.Col - 1;

         if Result.Col = 0 then
            Result.Line := Result.Line - 1;
            Line_Cursor.Line := Line_Cursor.Line - 1;

            if Line_Cursor.Line = 0 then
               raise Codefix_Panic;
            end if;

            Assign (Current_Line, Get_Line (This, Line_Cursor));
            Result.Col := To_Column_Index
              (Char_Index (Current_Line'Last), Current_Line.all);
         end if;

         if not Is_Blank
           (Current_Line
              (Natural (To_Char_Index (Result.Col, Current_Line.all))))
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
        (To_Column_Index (Char_Index (Get_Column (Start)), First_Line)) - 1;

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
     (This                      : in out Text_Navigator_Abstr'Class;
      Dest_Start, Dest_Stop     : in out File_Cursor;
      Source_Start, Source_Stop : File_Cursor'Class;
      Blanks_Before             : Replace_Blanks_Policy := Keep;
      Blanks_After              : Replace_Blanks_Policy := Keep)
   is
      New_Text : constant String := This.Get (Source_Start, Source_Stop);

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
         Line  : constant String := Dest_Text.Get_Line (Around, 1);
         Begin_Blanks : Char_Index;
         End_Blanks   : Char_Index;
         Tmp_Cursor   : Text_Cursor := Around;
         Blank_Length : Char_Index;
      begin
         case Pos is
            when First =>
               Begin_Blanks := To_Char_Index (Around.Col - 1, Line);
            when Last =>
               Begin_Blanks := To_Char_Index (Around.Col + 1, Line);
         end case;

         Skip_Blanks (Line, Integer (Begin_Blanks), -1);
         End_Blanks := Begin_Blanks + 1;
         Skip_Blanks (Line, Integer (End_Blanks), 1);
         Blank_Length := End_Blanks - Begin_Blanks - 1;

         if Blank_Length /= 0 then
            if Begin_Blanks = 0 then
               Tmp_Cursor.Col := 1;
            else
               Tmp_Cursor.Col := To_Column_Index (Begin_Blanks, Line) + 1;
            end if;
         end if;

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

   begin
      Dest_Text.Replace (Dest_Start, Dest_Stop, New_Text);

      Dest_Stop.Line := Dest_Start.Line;

      Last_Begin_Line := New_Text'First;

      for J in New_Text'First .. New_Text'Last - EOL_Str'Length + 1 loop
         if New_Text (J .. J + EOL_Str'Length - 1) = EOL_Str then
            Dest_Stop.Line := Dest_Stop.Line + 1;

            Last_Begin_Line := J + EOL_Str'Length;
         end if;
      end loop;

      Dest_Stop.Col :=
        To_Column_Index
          (Char_Index (New_Text'Last),
           New_Text (Last_Begin_Line .. New_Text'Last));

      if Last_Begin_Line = New_Text'First then
         Dest_Stop.Col := Dest_Start.Col + Dest_Stop.Col;
      end if;

      Mark_Start := new Mark_Abstr'Class'(This.Get_New_Mark (Dest_Start));
      Mark_Stop := new Mark_Abstr'Class'(This.Get_New_Mark (Dest_Stop));

      Dest_Start := File_Cursor (This.Get_Current_Cursor (Mark_Start.all));
      Replace_Blank_Slice (Text_Cursor (Dest_Start), Blanks_Before, First);

      Dest_Stop := File_Cursor (This.Get_Current_Cursor (Mark_Stop.all));
      Replace_Blank_Slice (Text_Cursor (Dest_Stop), Blanks_After, Last);

      Dest_Text.Indent_Line (Dest_Start);
      Dest_Text.Indent_Line (Dest_Stop);

      Dest_Start := File_Cursor (This.Get_Current_Cursor (Mark_Start.all));
      Dest_Stop := File_Cursor (This.Get_Current_Cursor (Mark_Stop.all));

      Free (Mark_Start);
      Free (Mark_Stop);
   end Replace;

   -----------
   -- Erase --
   -----------

   procedure Erase
     (This            : in out Text_Interface'Class;
      Start, Stop     : File_Cursor'Class;
      Remove_If_Blank : Boolean := True)
   is
   begin
      This.Replace (Start, Stop, "");

      if Remove_If_Blank and then Is_Blank (This.Get_Line (Start, 1)) then
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
      Start_Char_Index : Char_Index;
      Stop_Char_Index : Char_Index;
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

   ----------------------------------------------------------------------------
   --  type Text_Command
   ----------------------------------------------------------------------------

   ------------------
   -- Text_Command --
   ------------------

   procedure Free (This : in out Text_Command) is
   begin
      Free (This.Caption);
   end Free;

   procedure Free_Data (This : in out Text_Command'Class) is
   begin
      Free (This);
   end Free_Data;

   procedure Set_Caption
     (This : in out Text_Command'Class;
      Caption : String) is
   begin
      Assign (This.Caption, Caption);
   end Set_Caption;

   function Get_Caption (This : Text_Command'Class) return String is
   begin
      return This.Caption.all;
   end Get_Caption;

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
   begin
      Execute (This, Current_Text);
   exception
      when Obscolescent_Fix =>
         null;

      when E : Codefix_Panic =>
         if Error_Cb /= null then
            Error_Cb.Error (Exception_Information (E));
         end if;

   end Secured_Execute;

   -----------------
   -- Word_Cursor --
   -----------------

   procedure Free (This : in out Word_Cursor) is
   begin
      Free (This.String_Match);
      Free (File_Cursor (This));
   end Free;

   -----------
   -- Clone --
   -----------

   function Clone (This : Word_Cursor) return Word_Cursor is
   begin
      return (Clone (File_Cursor (This)) with
              Clone (This.String_Match), This.Mode);
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
      Assign (Mark.String_Match, Word.String_Match);
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

      if Word.String_Match = null then
         Set_Word (Cursor, "", Word.Mode);
      else
         Set_Word (Cursor, Word.String_Match.all, Word.Mode);
      end if;
   end Make_Word_Cursor;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Word_Mark) is
   begin
      Free (This.String_Match);
      Free (This.Mark_Id);
   end Free;

   ---------------------
   -- Remove_Word_Cmd --
   ---------------------

   procedure Initialize
     (This         : in out Remove_Word_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Word         : Word_Cursor'Class) is
   begin
      Make_Word_Mark (Word, Current_Text, This.Word);
   end Initialize;

   procedure Free (This : in out Remove_Word_Cmd) is
   begin
      Free (This.Word);
      Free (Text_Command (This));
   end Free;

   procedure Execute
     (This         : Remove_Word_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class)
   is
      Word : Word_Cursor;
   begin
      Make_Word_Cursor (This.Word, Current_Text, Word);

      Current_Text.Replace
        (Word, Word.Get_Matching_Word (Current_Text)'Length, "");

      if Current_Text.Get_Line (Word, 1) = "" then
         Current_Text.Delete_Line (Word);
      end if;

      Free (Word);
   end Execute;

   ---------------------
   -- Insert_Word_Cmd --
   ---------------------

   procedure Initialize
     (This            : in out Insert_Word_Cmd;
      Current_Text    : Text_Navigator_Abstr'Class;
      Word            : Word_Cursor'Class;
      New_Position    : File_Cursor'Class;
      Add_Spaces      : Boolean := True;
      Position        : Relative_Position := Specified;
      Insert_New_Line : Boolean := False)
   is
      New_Word : Word_Cursor;
   begin
      This.Add_Spaces := Add_Spaces;
      This.Position := Position;
      Make_Word_Mark (Word, Current_Text, This.Word);

      Set_File (New_Word, Get_File (New_Position));
      Set_Location
        (New_Word, Get_Line (New_Position), Get_Column (New_Position));
      Set_Word (New_Word, "", Text_Ascii);
      Make_Word_Mark (New_Word, Current_Text, This.New_Position);
      This.Insert_New_Line := Insert_New_Line;
   end Initialize;

   procedure Free (This : in out Insert_Word_Cmd) is
   begin
      Free (This.Word);
      Free (This.New_Position);
      Free (Text_Command (This));
   end Free;

   overriding
   procedure Execute
     (This         : Insert_Word_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class)
   is
      New_Str         : GNAT.Strings.String_Access;
      Line_Cursor     : File_Cursor;
      Space_Cursor    : File_Cursor;
      Word            : Word_Cursor;
      New_Pos         : Word_Cursor;
      Word_Char_Index : Char_Index;
      Modified_Text   : Ptr_Text;
   begin
      Make_Word_Cursor (This.Word, Current_Text, Word);
      Make_Word_Cursor (This.New_Position, Current_Text, New_Pos);

      Modified_Text := Current_Text.Get_File (New_Pos.File);

      Line_Cursor := Clone (File_Cursor (New_Pos));
      Line_Cursor.Col := 1;

      Word_Char_Index :=
        To_Char_Index (Word.Col, Get_Line (Current_Text, Line_Cursor));

      Assign (New_Str, Word.Get_Matching_Word (Current_Text));

      if This.Position = Specified then
         if This.Add_Spaces then
            Space_Cursor := Clone (File_Cursor (New_Pos));
            Space_Cursor.Col := Space_Cursor.Col - 1;

            if Word_Char_Index > 1
              and then not Is_Separator (Get (Current_Text, Space_Cursor))
            then
               Assign (New_Str, " " & New_Str.all);
            end if;

            Space_Cursor.Col := Space_Cursor.Col + 1;

            if Natural (Word_Char_Index) <
              Line_Length (Current_Text, Line_Cursor)
              and then not Is_Separator (Get (Current_Text, Space_Cursor))
            then
               Assign (New_Str, New_Str.all & " ");
            end if;
         end if;

         if This.Insert_New_Line then
            declare
               Line : constant String := Modified_Text.Get_Line (New_Pos, 1);
               Pos_Char_Index : Char_Index;
            begin
               Pos_Char_Index := To_Char_Index (Get_Column (New_Pos), Line);

               Modified_Text.Replace
                 (New_Pos,
                  Line (Natural (Pos_Char_Index) .. Line'Last)'Length, "");

               Modified_Text.Add_Line
                 (New_Pos, Line (Natural (Pos_Char_Index) .. Line'Last), True);
            end;
         end if;

         Modified_Text.Replace
           (New_Pos,
            0,
            New_Str.all);
      elsif This.Position = After then
         Modified_Text.Add_Line
           (New_Pos, New_Str.all, True);
      elsif This.Position = Before then
         New_Pos.Line := New_Pos.Line - 1;
         Modified_Text.Add_Line
           (New_Pos, New_Str.all, True);
      end if;

      Free (New_Str);
      Free (Word);
   end Execute;

   -------------------
   -- Move_Word_Cmd --
   -------------------

   procedure Initialize
     (This            : in out Move_Word_Cmd;
      Current_Text    : Text_Navigator_Abstr'Class;
      Word            : Word_Cursor'Class;
      New_Position    : File_Cursor'Class;
      Insert_New_Line : Boolean := False) is
   begin
      Initialize
        (This            => This.Step_Insert,
         Current_Text    => Current_Text,
         Word            => Word,
         New_Position    => New_Position,
         Insert_New_Line => Insert_New_Line);
      Initialize (This.Step_Remove, Current_Text, Word);
   end Initialize;

   procedure Free (This : in out Move_Word_Cmd) is
   begin
      Free (This.Step_Remove);
      Free (This.Step_Insert);
      Free (Text_Command (This));
   end Free;

   procedure Execute
     (This         : Move_Word_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class)
   is
   begin
      This.Step_Insert.Execute (Current_Text);
      This.Step_Remove.Execute (Current_Text);
   end Execute;

   ----------------------
   -- Replace_Word_Cmd --
   ----------------------

   procedure Initialize
     (This           : in out Replace_Word_Cmd;
      Current_Text   : Text_Navigator_Abstr'Class;
      Word           : Word_Cursor'Class;
      New_Word       : String;
      Do_Indentation : Boolean := False) is
   begin
      Make_Word_Mark (Word, Current_Text, This.Mark);
      Assign (This.Str_Expected, New_Word);
      This.Do_Indentation := Do_Indentation;
   end Initialize;

   procedure Free (This : in out Replace_Word_Cmd) is
   begin
      Free (This.Mark);
      Free (This.Str_Expected);
      Free (Text_Command (This));
   end Free;

   procedure Execute
     (This         : Replace_Word_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class)
   is
      Current_Word : Word_Cursor;
      Text         : Ptr_Text;

   begin
      Make_Word_Cursor (This.Mark, Current_Text, Current_Word);

      declare
         Match        : constant String :=
           Current_Word.Get_Matching_Word (Current_Text);
      begin
         Text := Current_Text.Get_File (Current_Word.File);

         Text.Replace
           (Cursor    => Current_Word,
            Len       => Match'Length,
            New_Value => This.Str_Expected.all);

         if This.Do_Indentation then
            Text.Indent_Line (Current_Word);
         end if;

         Free (Current_Word);
      end;
   end Execute;

   ---------------------
   -- Invert_Word_Cmd --
   ---------------------

   procedure Initialize
     (This         : in out Invert_Words_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Word1, Word2 : Word_Cursor'Class) is
   begin
      Initialize
        (This.Step_Word1, Current_Text, Word1, Word2.String_Match.all);
      Initialize
        (This.Step_Word2, Current_Text, Word2, Word1.String_Match.all);
   end Initialize;

   procedure Free (This : in out Invert_Words_Cmd) is
   begin
      Free (This.Step_Word1);
      Free (This.Step_Word2);
      Free (Text_Command (This));
   end Free;

   procedure Execute
     (This         : Invert_Words_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class)
   is
   begin
      Execute (This.Step_Word1, Current_Text);
      Execute (This.Step_Word2, Current_Text);
   end Execute;

   -------------------
   --  Add_Line_Cmd --
   -------------------

   procedure Initialize
     (This         : in out Add_Line_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Position     : File_Cursor'Class;
      Line         : String) is
   begin
      Assign (This.Line, Line);
      This.Position := new Mark_Abstr'Class'
        (Get_New_Mark (Current_Text, Position));
   end Initialize;

   procedure Free (This : in out Add_Line_Cmd) is
   begin
      Free (This.Line);
      Free (This.Position);
   end Free;

   procedure Execute
     (This         : Add_Line_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class)
   is
      Cursor : constant File_Cursor'Class :=
        Current_Text.Get_Current_Cursor (This.Position.all);
   begin
      Add_Line
        (Get_File (Current_Text, This.Position.File_Name).all,
         Cursor, This.Line.all);
   end Execute;

   ----------------------
   -- Replace_Slice_Cmd --
   ----------------------

   procedure Initialize
     (This                     : in out Replace_Slice_Cmd;
      Current_Text             : Text_Navigator_Abstr'Class;
      Start_Cursor, End_Cursor : File_Cursor'Class;
      New_Text                 : String) is
   begin
      This.Start_Mark := new Mark_Abstr'Class'
        (Get_New_Mark (Current_Text, Start_Cursor));
      This.End_Mark := new Mark_Abstr'Class'
        (Get_New_Mark (Current_Text, End_Cursor));
      This.New_Text := new String'(New_Text);
   end Initialize;

   procedure Free (This : in out Replace_Slice_Cmd) is
   begin
      Free (This.Start_Mark);
      Free (This.End_Mark);
      Free (This.New_Text);
   end Free;

   procedure Execute
     (This         : Replace_Slice_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class)
   is
      Start_Cursor, End_Cursor : File_Cursor;
      Modified_Text            : Ptr_Text;
   begin
      Start_Cursor := File_Cursor
        (Get_Current_Cursor (Current_Text, This.Start_Mark.all));
      End_Cursor := File_Cursor
        (Get_Current_Cursor (Current_Text, This.End_Mark.all));
      Modified_Text := Current_Text.Get_File (Start_Cursor.File);

      Modified_Text.Replace (Start_Cursor, End_Cursor, This.New_Text.all);
   end Execute;

   ----------------------------
   -- Remove_Blank_Lines_Cmd --
   ----------------------------

   procedure Initialize
     (This         : in out Remove_Blank_Lines_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Start_Cursor : File_Cursor'Class)
   is
   begin
      This.Start_Mark := new Mark_Abstr'Class'
        (Current_Text.Get_New_Mark (Start_Cursor));
   end Initialize;

   procedure Free (This : in out Remove_Blank_Lines_Cmd) is
   begin
      Free (This.Start_Mark);
   end Free;

   procedure Execute
     (This         : Remove_Blank_Lines_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class)
   is
      Cursor   : File_Cursor := File_Cursor
        (Current_Text.Get_Current_Cursor (This.Start_Mark.all));
      Text     : constant Ptr_Text := Current_Text.Get_File (Cursor.File);
      Max_Line : constant Integer := Text.Line_Max;
   begin
      Cursor.Col := 1;

      while Cursor.Line <= Max_Line
        and then Is_Blank (Text.Get_Line (Cursor, 1))
      loop
         Text.Delete_Line (Cursor);
      end loop;
   end Execute;

   ----------------
   -- Set_Kernel --
   ----------------

   procedure Set_Registry
     (Text   : in out Text_Navigator_Abstr;
      Registry : Projects.Registry.Project_Registry_Access) is
   begin
      Text.Registry := Registry;
   end Set_Registry;

   ----------------
   -- Get_Kernel --
   ----------------

   function Get_Registry
     (Text : Text_Navigator_Abstr)
      return Projects.Registry.Project_Registry_Access is
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

   ----------------------
   -- Get_Body_Or_Spec --
   ----------------------

   function Get_Body_Or_Spec
     (Text : Text_Navigator_Abstr; File_Name : Virtual_File)
      return Virtual_File
   is
   begin
      return Create
        (Name            => Other_File_Base_Name
           (Get_Project_From_File
              (Project_Registry (Get_Registry (Text).all), File_Name),
            File_Name),
         Registry        => Get_Registry (Text).all,
         Use_Object_Path => False);
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

   function Get_Column (This : Text_Cursor) return Column_Index is
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

   procedure Set_Column (This : in out Text_Cursor; Column : Column_Index) is
   begin
      This.Col := Column;
   end Set_Column;

   ------------------
   -- Set_Location --
   ------------------

   procedure Set_Location
     (This : in out Text_Cursor; Line : Natural; Column : Column_Index) is
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
     (Word : in out Word_Cursor;
      String_Match : String;
      Mode         : String_Mode := Text_Ascii) is
   begin
      if String_Match = "" then
         Free (Word.String_Match);
      else
         Assign (Word.String_Match, new String'(String_Match));
      end if;
      Word.Mode := Mode;
   end Set_Word;

   --------------
   -- Get_Word --
   --------------

   function Get_Word (Word : Word_Cursor) return String is
   begin
      if Word.String_Match = null then
         return "";
      else
         return Word.String_Match.all;
      end if;
   end Get_Word;

   -----------------------
   -- Get_Matching_Word --
   -----------------------

   function Get_Matching_Word
     (Word : Word_Cursor; Text : Text_Navigator_Abstr'Class) return String
   is
   begin
      case Word.Mode is
         when Text_Ascii =>
            return Word.String_Match.all;
         when Regular_Expression =>
            declare
               Matches    : Match_Array (0 .. 1);
               Matcher    : constant Pattern_Matcher := Compile
                 (Word.String_Match.all);
               Str_Parsed : constant String := Text.Get_Line (Word, 1);
               Index      : constant Char_Index :=
                 To_Char_Index (Word.Col, Str_Parsed);
            begin
               Match
                 (Matcher,
                  Str_Parsed (Integer (Index) .. Str_Parsed'Last), Matches);

               if Matches (0) = No_Match then
                  raise Codefix_Panic with "pattern '"
                    & Word.String_Match.all
                    & " in '"
                    & Word.String_Match.all & "' can't be found";
               else
                  return Str_Parsed (Matches (1).First .. Matches (1).Last);
               end if;
            end;
      end case;
   end Get_Matching_Word;

end Codefix.Text_Manager;
