-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2003-2007                      --
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
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Exceptions; use Ada.Exceptions;
with GNAT.Case_Util; use GNAT.Case_Util;
with GNAT.Regpat;    use GNAT.Regpat;

with Ada_Analyzer;   use Ada_Analyzer;
with Basic_Types;    use Basic_Types;
with String_Utils;   use String_Utils;
with VFS;            use VFS;

package body Codefix.Text_Manager is

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
      pragma Unreferenced (This, File);
   begin
      null;
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
      File_Name : VFS.Virtual_File) return GNAT.Strings.String_Access is
   begin
      return Read_File (Get_File (This, File_Name).all);
   end Read_File;

   --------------
   -- Get_File --
   --------------

   function Get_File
     (This : Text_Navigator_Abstr'Class;
      Name : VFS.Virtual_File) return Ptr_Text
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
     (This        : in out Text_Navigator_Abstr;
      Cursor      : File_Cursor'Class;
      New_Line    : String) is
   begin
      Add_Line
        (Get_File (This, Get_File (Cursor)).all,
         Text_Cursor (Cursor),
         New_Line);
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

   ----------------
   -- Get_Entity --
   ----------------

   procedure Get_Entity
     (This         : in out Extract;
      Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class)
   is
      Unit_Info, Body_Info : Construct_Tree_Iterator;
      Line_Cursor          : File_Cursor := File_Cursor (Cursor);
   begin
      Line_Cursor.Col := 1;

      Unit_Info := Get_Iterator_At (Current_Text, Cursor);

      if Get_Construct (Unit_Info).Is_Declaration then

         Body_Info := To_Construct_Tree_Iterator (Get_First_Body
           (Get_Tree (Current_Text, Cursor),
            Get_Ada_Tree (Current_Text, Cursor),
            Unit_Info));

         for J in Get_Construct (Body_Info).Sloc_Start.Line
           .. Get_Construct (Body_Info).Sloc_End.Line
         loop
            Line_Cursor.Line := J;
            Get_Line (Current_Text, Line_Cursor, This);
         end loop;
      end if;

      for J in Get_Construct (Unit_Info).Sloc_Start.Line
        .. Get_Construct (Unit_Info).Sloc_End.Line
      loop
         Line_Cursor.Line := J;
         Get_Line (Current_Text, Line_Cursor, This);
      end loop;
   end Get_Entity;

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

   ------------
   -- Commit --
   ------------

   procedure Commit (This : Text_Navigator_Abstr) is
      Iterator : Text_List.List_Node := First (This.Files.all);
   begin
      while Iterator /= Text_List.Null_Node loop
         Commit (Data (Iterator).all);
         Iterator := Next (Iterator);
      end loop;
   end Commit;

   -------------------
   -- Search_String --
   -------------------

   function Search_String
     (This           : Text_Navigator_Abstr'Class;
      Cursor         : File_Cursor'Class;
      Searched       : String;
      Escape_Manager : Escape_Str_Manager'Class;
      Step           : Step_Way := Normal_Step)
      return File_Cursor'Class is
   begin
      return Search_String
        (Get_File (This, Get_File (Cursor)).all,
         File_Cursor (Cursor),
         Searched,
         Escape_Manager,
         Step);
   end Search_String;

   --------------------
   -- Search_Strings --
   --------------------

   function Search_Strings
     (This           : Text_Navigator_Abstr'Class;
      Cursor         : File_Cursor'Class;
      Searched       : GNAT.Strings.String_List;
      Escape_Manager : Escape_Str_Manager'Class;
      Step           : Step_Way := Normal_Step)
      return Word_Cursor'Class is
   begin
      return Search_Strings
        (Get_File (This, Get_File (Cursor)).all,
         File_Cursor (Cursor),
         Searched,
         Escape_Manager,
         Step);
   end Search_Strings;

   -----------------
   -- Search_Unit --
   -----------------

   function Search_Unit
     (This      : Text_Navigator_Abstr'Class;
      File_Name : VFS.Virtual_File;
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
      File_Name : VFS.Virtual_File) return Natural is
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
         Body_Info := To_Construct_Tree_Iterator (Get_First_Body
           (Get_Tree (Current_Text, Cursor),
            Get_Ada_Tree (Current_Text, Cursor),
            Unit_Info));
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
      File_Name : VFS.Virtual_File) return Construct_List_Access is
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
     (This : Text_Navigator_Abstr'Class; File_Name : VFS.Virtual_File) is
   begin
      Undo (Get_File (This, File_Name).all);
   end Undo;

   --------------
   -- Get_Tree --
   --------------

   function Get_Tree
     (This : Text_Navigator_Abstr'Class; Cursor : File_Cursor'Class)
      return Construct_Tree is
   begin
      return Get_Tree (Get_File (This, Get_File (Cursor)));
   end Get_Tree;

   ------------------
   -- Get_Ada_Tree --
   ------------------

   function Get_Ada_Tree
     (This : Text_Navigator_Abstr'Class; Cursor : File_Cursor'Class)
      return Ada_Construct_Tree is
   begin
      return Get_Ada_Tree (Get_File (This, Get_File (Cursor)));
   end Get_Ada_Tree;

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
      Free (This.Tree, This.Ada_Tree);
      Free (This.Tree);
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
        (Get_Tree (Current_Text),
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

   function Get_File_Name (This : Text_Interface) return VFS.Virtual_File is
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

   function Search_String
     (This           : Text_Interface'Class;
      Cursor         : Text_Cursor'Class;
      Searched       : String;
      Escape_Manager : Escape_Str_Manager'Class;
      Step           : Step_Way := Normal_Step) return File_Cursor'Class
   is
      Str_Array : GNAT.Strings.String_List := (1 => new String'(Searched));
      Result    : Word_Cursor'Class := Search_Strings
        (This,
         Cursor,
         Str_Array,
         Escape_Manager,
         Step);
      Real_Result : constant File_Cursor := Clone (File_Cursor (Result));
   begin
      Free (Str_Array);
      Free (Result);

      return Real_Result;
   end Search_String;

   --------------------
   -- Search_Strings --
   --------------------

   function Search_Strings
     (This           : Text_Interface'Class;
      Cursor         : Text_Cursor'Class;
      Searched       : GNAT.Strings.String_List;
      Escape_Manager : Escape_Str_Manager'Class;
      Step           : Step_Way := Normal_Step) return Word_Cursor'Class
   is
      Last, Increment    : Integer;
      Ext_Red            : Extract_Line;
      Result             : Word_Cursor;
      New_Cursor         : File_Cursor;
      Line_Cursor        : File_Cursor;
      Old_Col            : Column_Index;
   begin
      New_Cursor := File_Cursor (Cursor);
      Line_Cursor := New_Cursor;
      Line_Cursor.Col := 1;

      case Step is
         when Normal_Step =>
            Last := Line_Max (This);
            Increment := 1;
            Get_Line (This, Line_Cursor, Ext_Red);
         when Reverse_Step =>
            Last := 1;
            Increment := -1;
            Old_Col := New_Cursor.Col;
            New_Cursor.Col := 1;
            --  ??? possible error there, since we used to get only the part of
            --  the string where we wanted to do the analysis (as in normal
            --  step), but this operation is not straighforward since we don't
            --  know at this stage the size of the extract.
            Get_Line (This, Line_Cursor, Ext_Red);
            New_Cursor.Col := Old_Col;
      end case;

      loop
         Result :=
           Word_Cursor
             (Search_Strings
                  (Ext_Red,
                   New_Cursor,
                   Searched,
                   Escape_Manager,
                   Step));

         if Result /= Null_Word_Cursor then
            Result := Clone (Result);
            Free (Ext_Red);
            return Result;
         end if;

         exit when New_Cursor.Line = Last;

         New_Cursor.Col := 1;
         New_Cursor.Line := New_Cursor.Line + Increment;
         Free (Ext_Red);
         Get_Line (This, New_Cursor, Ext_Red);

         if Step = Reverse_Step then
            New_Cursor.Col := 0;
         end if;

      end loop;

      Free (Ext_Red);
      return Null_Word_Cursor;
   end Search_Strings;

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
           To_Composite_Identifier (Get_Full_Name (Get_Tree (This), It));
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

         raise Codefix_Panic;
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

   function Get_Tree
     (This : access Text_Interface'Class) return Construct_Tree is
   begin
      Update_Structure_If_Needed (This);

      return This.Tree;
   end Get_Tree;

   ------------------
   -- Get_Ada_Tree --
   ------------------

   function Get_Ada_Tree
     (This : access Text_Interface'Class) return Ada_Construct_Tree is
   begin
      Update_Structure_If_Needed (This);

      return This.Ada_Tree;
   end Get_Ada_Tree;

   --------------------------------
   -- Update_Structure_If_Needed --
   --------------------------------

   procedure Update_Structure_If_Needed (This : access Text_Interface'Class) is
   begin
      if not This.Structure_Up_To_Date.all then
         Free (This.Tree, This.Ada_Tree);
         Free (This.Tree);
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

         This.Tree := To_Construct_Tree (This.Structure, False);

         This.Ada_Tree := Generate_Ada_Construct_Tree
           (This.Tree, This.Buffer);

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

   ----------------------------------------------------------------------------
   --  type Extract_Line
   ----------------------------------------------------------------------------

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Extract_Line) return Boolean is
   begin
      return Left.Cursor = Right.Cursor
        and then Left.Context = Right.Context
        and then (Left.Context = Unit_Deleted
                  or else To_String (Left.Content) =
                    To_String (Right.Content));
   end "=";

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Ptr_Extract_Line) return Boolean is
   begin
      return Left.Cursor < Right.Cursor;
   end "<";

   ------------
   -- Assign --
   ------------

   procedure Assign (This : in out Extract_Line; Value : Extract_Line) is
   begin
      This.Context := Value.Context;
      Free (This.Cursor);
      This.Cursor := Clone (Value.Cursor);
      Assign (This.Content, Value.Content);
   end Assign;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Extract_Line) is
   begin
      Free (This.Content);
      Free (This.Cursor);
      if This.Next /= null then
         Free (This.Next.all);
         Free (This.Next);
      end if;
   end Free;

   ---------------
   -- Free_Data --
   ---------------

   procedure Free_Data (This : in out Extract_Line) is
   begin
      Free (This.Content);
      Free (This.Cursor);
   end Free_Data;

   -----------------
   -- Get_Context --
   -----------------

   function Get_Context (This : Extract_Line) return Merge_Info is
   begin
      return This.Context;
   end Get_Context;

   -----------------
   -- Set_Context --
   -----------------

   procedure Set_Context (This : in out Extract_Line; Value : Merge_Info) is
   begin
      This.Context := Value;
   end Set_Context;

   ----------
   -- Next --
   ----------

   function Next (This : Ptr_Extract_Line) return Ptr_Extract_Line is
   begin
      return This.Next;
   end Next;

   function Next (This : Extract_Line) return Ptr_Extract_Line is
   begin
      return This.Next;
   end Next;

   ----------------
   -- Get_String --
   ----------------

   function Get_String (This : Extract_Line) return String is
   begin
      return To_String (This.Content);
   end Get_String;

   ----------------
   -- Get_Cursor --
   ----------------

   function Get_Cursor (This : Extract_Line) return File_Cursor'Class is
   begin
      return This.Cursor;
   end Get_Cursor;

   ------------
   -- Length --
   ------------

   function Length (This : Extract_Line) return Natural is
   begin
      if This.Next /= null then
         return Length (This.Next.all) + 1;
      else
         return 1;
      end if;
   end Length;

   ------------
   -- Commit --
   ------------

   procedure Commit
     (This         : in out Extract_Line;
      Current_Text : in out Text_Navigator_Abstr'Class;
      Offset_Line  : in out Integer)
   is
      Cursor : File_Cursor := This.Cursor;

      procedure Commit_Modified_Line;
      --  Commit separately each part of a modified line, in order to keep
      --  marks.

      --------------------------
      -- Commit_Modified_Line --
      --------------------------

      procedure Commit_Modified_Line is
         It      : Mask_Iterator;
         Len     : Natural;
         Info    : Merge_Info;
         Content : constant String (1 .. Get_String (This)'Length)
           := Get_String (This);
         Cursor_Char_Index : Char_Index := To_Char_Index (Cursor.Col, Content);
      begin
         Reset (It, This.Content);

         loop
            Get_Next_Area (This.Content, It, Cursor_Char_Index, Len, Info);

            exit when Len = 0;

            case Info is
               when Unit_Modified =>
                  Cursor.Col := To_Column_Index (Cursor_Char_Index, Content);

                  Replace
                    (Current_Text,
                     Cursor,
                     Len,
                     Content
                       (Natural (Cursor_Char_Index)
                        .. Natural (Cursor_Char_Index) + Len - 1));
               when Unit_Created =>
                  Cursor.Col := To_Column_Index (Cursor_Char_Index, Content);

                  Replace
                    (Current_Text,
                     Cursor,
                     0,
                     Content
                       (Natural (Cursor_Char_Index)
                        .. Natural (Cursor_Char_Index) + Len - 1));
               when Unit_Deleted =>
                  Cursor.Col := To_Column_Index (Cursor_Char_Index, Content);

                  Replace
                    (Current_Text,
                     Cursor,
                     Len,
                     "");
               when Original_Unit =>
                  null;
            end case;
         end loop;
      end Commit_Modified_Line;

   begin
      Cursor.Line := This.Cursor.Line + Offset_Line;

      case This.Context is
         when Unit_Created =>
            Add_Line
              (Current_Text,
               Cursor,
               To_String (This.Content));
            Offset_Line := Offset_Line + 1;
         when Unit_Deleted =>
            Delete_Line
              (Current_Text,
               Cursor);
            Offset_Line := Offset_Line - 1;
         when Unit_Modified =>
            Commit_Modified_Line;
         when Original_Unit =>
            null;
      end case;
   end Commit;

   ------------------------
   -- Get_Number_Actions --
   ------------------------

   function Get_Number_Actions (This : Extract_Line) return Natural is
      Cursor : File_Cursor := This.Cursor;

      function Count_Modified_Line_Actions return Natural;
      --  Commit separately each part of a modified line, in order to keep
      --  marks.

      -------------------------
      -- Count_Modified_Line --
      -------------------------

      function Count_Modified_Line_Actions return Natural is
         It                : Mask_Iterator;
         Len               : Natural;
         Info              : Merge_Info;
         Number_Actions    : Natural := 0;
      begin
         Reset (It, This.Content);

         loop
            declare
               This_Content : constant String := To_String (This.Content);
               Cursor_Char_Index : Char_Index;
            begin
               Cursor_Char_Index :=
                 To_Char_Index (Cursor.Col, This_Content);
               Get_Next_Area
                 (This.Content,
                  It,
                  Cursor_Char_Index,
                  Len,
                  Info);
               Cursor.Col := To_Column_Index (Cursor_Char_Index, This_Content);
            end;

            exit when Len = 0;

            case Info is
               when Unit_Modified | Unit_Created | Unit_Deleted =>
                  Number_Actions := Number_Actions + 1;
               when Original_Unit =>
                  null;
            end case;
         end loop;

         return Number_Actions;
      end Count_Modified_Line_Actions;

   begin
      case This.Context is
         when Unit_Created | Unit_Deleted =>
            return 1;
         when Unit_Modified =>
            return Count_Modified_Line_Actions;
         when Original_Unit =>
            return 0;
      end case;
   end Get_Number_Actions;

   -----------
   -- Clone --
   -----------

   function Clone
     (This      : Extract_Line;
      Recursive : Boolean) return Extract_Line
   is
      New_Line : Extract_Line := This;
   begin

      New_Line.Cursor := Clone (New_Line.Cursor);
      New_Line.Content := Clone (New_Line.Content);

      if Recursive and then New_Line.Next /= null then
         New_Line.Next := new Extract_Line'(Clone (New_Line.Next.all, True));
      else
         New_Line.Next := null;
      end if;

      return New_Line;
   end Clone;

   -----------
   -- Clone --
   -----------

   function Clone (This : Extract_Line) return Extract_Line is
   begin
      return Clone (This, False);
   end Clone;

   ---------------------
   -- Get_Word_Length --
   ---------------------

   function Get_Word_Length
     (This   : Extract_Line;
      Col    : Column_Index;
      Format : String) return Natural
   is
      Matches    : Match_Array (0 .. 1);
      Matcher    : constant Pattern_Matcher := Compile (Format);
      Str_Parsed : constant String := To_String (This.Content);
      Index      : constant Char_Index :=
        To_Char_Index (Col, Str_Parsed);

   begin
      Match
        (Matcher,
         Str_Parsed (Natural (Index) .. Str_Parsed'Last), Matches);

      if Matches (0) = No_Match then
         Raise_Exception (Codefix_Panic'Identity, "pattern '" & Format &
                          "' from col" & Char_Index'Image (Index) & " in '" &
                            Str_Parsed & "' can't be found");

      else
         return Matches (1).Last - Natural (Index) + 1;
      end if;
   end Get_Word_Length;

   -------------------
   -- Search_String --
   -------------------

   function Search_String
     (This           : Extract_Line;
      Cursor         : File_Cursor'Class;
      Searched       : String;
      Escape_Manager : Escape_Str_Manager'Class;
      Step           : Step_Way := Normal_Step) return File_Cursor'Class
   is
      Str_Array : GNAT.Strings.String_List := (1 => new String'(Searched));
      Result    : File_Cursor'Class := Search_Strings
        (This,
         Cursor,
         Str_Array,
         Escape_Manager,
         Step);
      Real_Result : constant File_Cursor := Clone (File_Cursor (Result));
   begin
      Free (Str_Array);
      Free (Result);

      return Real_Result;
   end Search_String;

   --------------------
   -- Search_Strings --
   --------------------

   function Search_Strings
     (This           : Extract_Line;
      Cursor         : File_Cursor'Class;
      Searched       : GNAT.Strings.String_List;
      Escape_Manager : Escape_Str_Manager'Class;
      Step           : Step_Way := Normal_Step) return Word_Cursor'Class
   is
      Result        : Word_Cursor;
      Result_String : constant String := To_String (This.Content);
      Start_Index   : Char_Index;

      function Test_Result
        (Str : GNAT.Strings.String_Access; Index : Char_Index)
        return Boolean;
      --  Return true if the string given in parameter is on the Index position
      --  of the content, and set the Result accordingly if needed.

      function Test_Result
        (Str   : GNAT.Strings.String_Access;
         Index : Char_Index)
        return Boolean
      is
      begin
         if Natural (Index) + Str.all'Last - 1 <= Result_String'Last
           and then Result_String
             (Natural (Index) .. Natural (Index) + Str.all'Last - 1) = Str.all
           and then not
             Is_In_Escape_Part (Escape_Manager, Result_String, Index)
         then
            Result.Col := To_Column_Index (Index, Result_String);
            Result.String_Match := new String'(Str.all);

            return True;
         end if;

         return False;
      end Test_Result;

   begin
      Result := (File_Cursor (Cursor) with null, Text_Ascii);
      --  ??? If this assignment is done directly as an initialization in the
      --  elaboration of the procedure, GPS will crash (see FA09-007). So this
      --  assignment has to stay here as a workaround until the underlying
      --  issue is fixed.

      if Result.Col = 0 then
         Start_Index := Char_Index (Result_String'Last);
      else
         Start_Index := To_Char_Index (Result.Col, Result_String);
      end if;

      case Step is
         when Normal_Step =>
            for J in Start_Index .. Char_Index (Result_String'Last) loop
               for K in Searched'Range loop
                  if Test_Result (Searched (K), J) then
                     return Result;
                  end if;
               end loop;
            end loop;

         when Reverse_Step =>
            for J in reverse
              Char_Index (Result_String'First) .. Start_Index
            loop
               for K in Searched'Range loop
                  if Test_Result (Searched (K), J) then
                     return Result;
                  end if;
               end loop;
            end loop;
      end case;

      return Null_Word_Cursor;
   end Search_Strings;

   ---------
   -- Get --
   ---------

   procedure Get
     (This        : Text_Interface'Class;
      Cursor      : File_Cursor'Class;
      Len         : Natural;
      Destination : in out Extract_Line) is
   begin
      Destination :=
        (Context         => Original_Unit,
         Cursor          => Clone (File_Cursor (Cursor)),
         Original_Length => Len,
         Content         => To_Mergable_String (Get (This, Cursor, Len)),
         Next            => null,
         Coloration      => True);
   end Get;

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line
     (This        : Text_Navigator_Abstr'Class;
      Cursor      : File_Cursor'Class;
      Destination : in out Extract_Line)
   is
      Str : GNAT.Strings.String_Access;
   begin
      Str := new String'(Get_Line (This, Cursor));
      Destination :=
        (Context         => Original_Unit,
         Cursor          => File_Cursor (Cursor),
         Original_Length => Str.all'Last,
         Content         => To_Mergable_String (Str.all),
         Next            => null,
         Coloration      => True);
      Free (Str);
   end Get_Line;

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line
     (This        : Text_Interface'Class;
      Cursor      : File_Cursor'Class;
      Destination : in out Extract_Line)
   is
      Str : GNAT.Strings.String_Access;
   begin
      Str := new String'(Get_Line (This, Cursor));
      Free_Data (Destination);
      Destination :=
        (Context         => Original_Unit,
         Cursor          => Clone (File_Cursor (Cursor)),
         Original_Length => Str'Last,
         Content         => To_Mergable_String (Str.all),
         Next            => null,
         Coloration      => True);
      Free (Str);
   end Get_Line;

   ------------------
   -- Get_New_Text --
   ------------------

   function Get_New_Text
     (This : Extract_Line; Detail : Boolean := True) return String
   is
      pragma Unreferenced (Detail);
   begin
      if This.Context /= Unit_Deleted then
         return To_String (This.Content);
      else
         return "";
      end if;
   end Get_New_Text;

   ------------------
   -- Get_Old_Text --
   ------------------

   function Get_Old_Text
     (This         : Extract_Line;
      Current_Text : Text_Navigator_Abstr'Class) return String
   is
      Old_Extract : Extract;
   begin
      case This.Context is
         when Original_Unit | Unit_Modified =>
            Get (Current_Text, This.Cursor, This.Original_Length, Old_Extract);

            declare
               Res : constant String := Get_New_Text
                 (Get_Record (Old_Extract, 1).all,
                  False);
            begin
               Free (Old_Extract);
               return Res;
            end;

         when Unit_Created =>
            null;
         when Unit_Deleted =>
            Get (Current_Text, This.Cursor, This.Original_Length, Old_Extract);

            declare
               Res : constant String := Get_New_Text
                 (Get_Record (Old_Extract, 1).all,
                  False);
            begin
               Free (Old_Extract);
               return Res;
            end;
      end case;

      return "";
   end Get_Old_Text;

   -------------------------
   -- Get_New_Text_Length --
   -------------------------

   function Get_New_Text_Length
     (This      : Extract_Line;
      Recursive : Boolean := False) return Natural
   is
      Total  : Natural := 0;
      Buffer : constant String := Get_New_Text (This);

   begin
      if Recursive and then This.Next /= null then
         Total := Get_New_Text_Length (This.Next.all, True);
      end if;

      return Total + Buffer'Length;
   end Get_New_Text_Length;

   -------------------------
   -- Get_Old_Text_Length --
   -------------------------

   function Get_Old_Text_Length
     (This         : Extract_Line;
      Current_Text : Text_Navigator_Abstr'Class;
      Recursive    : Boolean := False) return Natural
   is
      Total  : Natural := 0;
      Buffer : constant String := Get_Old_Text (This, Current_Text);

   begin
      if Recursive and then This.Next /= null then
         Total := Get_Old_Text_Length (This.Next.all, Current_Text, True);
      end if;

      return Total + Buffer'Length;
   end Get_Old_Text_Length;

   -------------------
   -- Extend_Before --
   -------------------

   procedure Extend_Before
     (This          : in out Ptr_Extract_Line;
      Prev          : in out Ptr_Extract_Line;
      Current_Text  : Text_Navigator_Abstr'Class;
      Size          : Natural)
   is
      Line_Cursor            : File_Cursor;
      New_Line, Current_Line : Ptr_Extract_Line;

   begin
      if This = null then
         return;
      end if;

      Line_Cursor := This.Cursor;
      Line_Cursor.Col := 1;
      New_Line := new Extract_Line;
      Current_Line := This;

      if Prev = null then
         for J in 1 .. Size loop
            if Current_Line.Context /= Unit_Created then
               Line_Cursor.Line := Line_Cursor.Line - 1;
            end if;

            exit when Line_Cursor.Line = 0;

            Get_Line (Current_Text, Clone (Line_Cursor), New_Line.all);
            New_Line.Next := Current_Line;
            Current_Line := New_Line;
            New_Line := new Extract_Line;
         end loop;

      else
         for J in 1 .. Size loop
            if Current_Line.Context /= Unit_Created then
               Line_Cursor.Line := Line_Cursor.Line - 1;
            end if;

            exit when Get_File (Prev.Cursor) = Get_File (Current_Line.Cursor)
              and then (Prev.Cursor.Line + 1 = Current_Line.Cursor.Line
                        or else Prev.Cursor.Line = Current_Line.Cursor.Line);
            exit when Line_Cursor.Line = 0;

            Get_Line (Current_Text, Clone (Line_Cursor), New_Line.all);
            New_Line.Next := Current_Line;
            Prev.Next := New_Line;
            Current_Line := New_Line;
            New_Line := new Extract_Line;
         end loop;

      end if;

      Free (New_Line);
      Extend_Before (This.Next, This, Current_Text, Size);
      This := Current_Line;
   end Extend_Before;

   ------------------
   -- Extend_After --
   ------------------

   procedure Extend_After
     (This         : in out Ptr_Extract_Line;
      Current_Text : Text_Navigator_Abstr'Class;
      Size         : Natural)
   is
      Line_Cursor            : File_Cursor;
      New_Line, Current_Line : Ptr_Extract_Line;
      End_Of_File            : Natural;

   begin
      if This = null then
         return;
      end if;

      Line_Cursor := This.Cursor;
      Line_Cursor.Col := 1;
      New_Line := new Extract_Line;
      Current_Line := This;
      End_Of_File := Line_Max (Current_Text, Get_File (Line_Cursor));

      if This.Next = null then
         for J in 1 .. Size loop
            Line_Cursor.Line := Line_Cursor.Line + 1;

            exit when Line_Cursor.Line > End_Of_File;

            Get_Line (Current_Text, Clone (Line_Cursor), New_Line.all);
            New_Line.Next := Current_Line.Next;
            Current_Line.Next := New_Line;
            Current_Line := New_Line;
            New_Line := new Extract_Line;
         end loop;

      else
         for J in 1 .. Size loop
            exit when Get_File (Current_Line.Cursor) =
              Get_File (Current_Line.Next.Cursor)
              and then (Current_Line.Cursor.Line + 1 =
                          Current_Line.Next.Cursor.Line
                        or else Current_Line.Next.Cursor.Line =
                          Current_Line.Cursor.Line);

            Line_Cursor.Line := Line_Cursor.Line + 1;

            exit when Line_Cursor.Line > End_Of_File;

            Get_Line (Current_Text, Clone (Line_Cursor), New_Line.all);
            New_Line.Next := Current_Line.Next;
            Current_Line.Next := New_Line;
            Current_Line := New_Line;
            New_Line := new Extract_Line;
         end loop;
      end if;

      Free (New_Line);
      Extend_After (Current_Line.Next, Current_Text, Size);
   end Extend_After;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (This       : in out Extract_Line;
      Start      : Column_Index;
      Len        : Natural;
      New_String : String)
   is
      This_String      : constant String := To_String (This.Content);
      Start_Char_Index : constant Char_Index :=
        To_Char_Index (Start, This_String);
   begin
      if Len /= New_String'Length
        or else This_String
          (Natural (Start_Char_Index) ..
             Natural (Start_Char_Index) + Len - 1) /=
          New_String
      then
         This.Context := Unit_Modified;

         Replace (This.Content, Start_Char_Index, Len, New_String);
      end if;
   end Replace;

   --------------------
   -- Replace_To_End --
   --------------------

   procedure Replace_To_End
     (This  : in out Extract_Line;
      Start : Column_Index;
      Value : String) is
   begin
      Replace (This, Start, To_String (This.Content)'Last, Value);
   end Replace_To_End;

   --------------------
   -- Set_Coloration --
   --------------------

   procedure Set_Coloration (This : in out Extract_Line; Value : Boolean) is
   begin
      This.Coloration := Value;
   end Set_Coloration;

   --------------------
   -- Get_Coloration --
   --------------------

   function Get_Coloration (This : Extract_Line) return Boolean is
   begin
      return This.Coloration;
   end Get_Coloration;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (This, Prev : Ptr_Extract_Line; Container : in out Extract) is
   begin
      if Prev = null then
         Container.First := This.Next;
      else
         Prev.Next := This.Next;
      end if;

      This.Next := null;
   end Remove;

   --------------
   -- Get_Line --
   --------------

   function Get_Line
     (This : Ptr_Extract_Line; Cursor : File_Cursor'Class)
      return Ptr_Extract_Line is
   begin
      if This = null then
         return null;
      elsif Get_File (This.Cursor) = Get_File (Cursor)
        and then This.Cursor.Line = Cursor.Line
      then
         return This;
      elsif This.Next = null then
         return null;
      else
         return Get_Line (This.Next, Cursor);
      end if;
   end Get_Line;

   -----------------
   -- Merge_Lines --
   -----------------

   procedure Merge_Lines
     (Result              : out Extract_Line;
      Object_1, Object_2  : Extract_Line;
      Success             : out Boolean;
      Chronologic_Changes : Boolean) is
   begin
      Merge_String
        (Result.Content,
         Object_1.Content,
         Object_2.Content,
         Success,
         Chronologic_Changes);

      Result.Cursor := Clone (Object_1.Cursor);
      Result.Original_Length := Object_1.Original_Length;
      Result.Coloration := Object_1.Coloration;
   end Merge_Lines;

   function Data (This : Ptr_Extract_Line) return Extract_Line is
   begin
      return This.all;
   end Data;

   function Is_Null (This : Ptr_Extract_Line) return Boolean is
   begin
      return This = null;
   end Is_Null;

   ----------------------------------------------------------------------------
   --  type Extract
   ----------------------------------------------------------------------------

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Ptr_Extract) is
      procedure Free_Pool is new Ada.Unchecked_Deallocation
        (Extract'Class, Ptr_Extract);
   begin
      Free (This.all);
      Free_Pool (This);
   end Free;

   -----------
   -- Clone --
   -----------

   function Clone (This : Extract) return Extract is
      New_Extract : Extract := This;
   begin
      if New_Extract.First /= null then
         New_Extract.First :=
           new Extract_Line'(Clone (New_Extract.First.all, True));
      end if;

      return New_Extract;
   end Clone;

   ------------
   -- Assign --
   ------------

   procedure Assign (This : in out Extract'Class; Source : Extract'Class) is
   begin
      This.First := new Extract_Line'(Clone (Source.First.all, True));
   end Assign;

   ---------
   -- Get --
   ---------

   procedure Get
     (This        : Text_Navigator_Abstr'Class;
      Cursor      : File_Cursor'Class;
      Len         : Natural;
      Destination : in out Extract) is
   begin
      Add_Element
        (Destination,
         new Extract_Line'
           (Context         => Original_Unit,
            Cursor          => File_Cursor (Clone (Cursor)),
            Original_Length => Len,
            Content         => To_Mergable_String (Get (This, Cursor, Len)),
            Next            => null,
            Coloration      => True));
   end Get;

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line
     (This        : Text_Navigator_Abstr'Class;
      Cursor      : File_Cursor'Class;
      Destination : in out Extract)
   is
      Str : constant String := Get_Line (This, Cursor);
   begin
      Add_Element
        (Destination,
         new Extract_Line'
           (Context         => Original_Unit,
            Cursor          => Clone (File_Cursor (Cursor)),
            Original_Length => Str'Length,
            Content         => To_Mergable_String (Str),
            Next            => null,
            Coloration      => True));
   end Get_Line;

   ----------------
   -- Get_String --
   ----------------

   function Get_String
     (This     : Extract;
      Position : Natural := 1) return String
   is
      Current_Extract : Ptr_Extract_Line := This.First;
   begin
      for J in 1 .. Position - 1 loop
         Current_Extract := Current_Extract.Next;
      end loop;

      return To_String (Current_Extract.Content);
   end Get_String;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (This          : in out Extract;
      Start         : Column_Index;
      Length        : Natural;
      Value         : String;
      Line_Number   : Natural := 1)
   is
      Current_Extract : Ptr_Extract_Line := This.First;
   begin
      for J in 1 .. Line_Number - 1 loop
         Current_Extract := Current_Extract.Next;
      end loop;

      Replace (Current_Extract.all, Start, Length, Value);
   end Replace;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (This   : in out Extract;
      Start  : File_Cursor'Class;
      Length : Natural;
      Value  : String)
   is
      Current_Extract : constant Ptr_Extract_Line := Get_Line (This, Start);
   begin
      Replace (Current_Extract.all, Start.Col, Length, Value);
   end Replace;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (This                      : in out Extract;
      Dest_Start, Dest_Stop     : File_Cursor'Class;
      Source_Start, Source_Stop : File_Cursor'Class;
      Current_Text              : Text_Navigator_Abstr'Class)
   is
      function Get_Next_Source_Line return GNAT.Strings.String_Access;
      --  Retutn the next source line that have to be added to the extract,
      --  addinf Head or Bottom if needed.

      Destination_Line : Ptr_Extract_Line := Get_Line (This, Dest_Start);

      Cursor_Dest      : File_Cursor := File_Cursor (Dest_Start);
      Cursor_Source    : File_Cursor := File_Cursor (Source_Start);
      Source_Line      : GNAT.Strings.String_Access;
      Head, Bottom     : GNAT.Strings.String_Access;
      Last_Line        : GNAT.Strings.String_Access;

      --------------------------
      -- Get_Next_Source_Line --
      --------------------------

      function Get_Next_Source_Line return GNAT.Strings.String_Access is
         Line : GNAT.Strings.String_Access :=
           new String'(Get_Line (Current_Text, Cursor_Source));
      begin
         if Cursor_Source.Line = Source_Stop.Line then
            Assign
              (Line,
               Line (1 .. Natural (To_Char_Index (Source_Stop.Col, Line.all)))
               & Bottom.all);
         end if;

         if Cursor_Source.Line = Source_Start.Line then
            Assign
              (Line,
               Head.all
               & Line
                 (Natural (To_Char_Index (Source_Start.Col, Line.all))
                  .. Line'Last));
         end if;

         Cursor_Source.Line := Cursor_Source.Line + 1;

         return Line;
      end Get_Next_Source_Line;

      Dest_String           : constant String :=
        Get_String (Get_Line (This, Dest_Start).all);
      Dest_Start_Char_Index : constant Char_Index :=
        To_Char_Index (Dest_Start.Col, Dest_String);

      Bottom_String        : constant String :=
        Get_String (Get_Line (This, Dest_Stop).all);
      Dest_Stop_Char_Index : constant Char_Index :=
        To_Char_Index (Dest_Stop.Col, Bottom_String);

   begin
      Cursor_Source.Col := 1;

      Assign (Head, Dest_String (1 .. Natural (Dest_Start_Char_Index) - 1));

      Assign (Bottom, Bottom_String
              (Natural (Dest_Stop_Char_Index) + 1 .. Bottom_String'Last));

      while Cursor_Source.Line <= Source_Stop.Line loop
         Free (Source_Line);
         Source_Line := Get_Next_Source_Line;

         if Destination_Line /= null then
            Replace_To_End
              (Destination_Line.all,
               1,
               Source_Line.all);

            Assign (Last_Line, Get_String (Destination_Line.all));

            if Cursor_Dest.Line < Dest_Stop.Line then
               Cursor_Dest.Line := Cursor_Dest.Line + 1;
               Destination_Line := Get_Line (Destination_Line, Cursor_Dest);
               null;
            else
               Destination_Line := null;
            end if;
         else
            if Last_Line /= null then
               Add_Indented_Line
                 (This,
                  Cursor_Dest,
                  Source_Line.all,
                  Last_Line.all);
            else
               Add_Indented_Line
                 (This,
                  Cursor_Dest,
                  Source_Line.all,
                  Current_Text);
            end if;
         end if;

      end loop;

      if Destination_Line /= null then
         loop
            Destination_Line.Context := Unit_Deleted;
            Cursor_Dest.Line := Cursor_Dest.Line + 1;

            exit when Cursor_Dest.Line > Dest_Stop.Line;

            Destination_Line := Get_Line (Destination_Line, Cursor_Dest);
         end loop;
      end if;

      Free (Source_Line);
      Free (Head);
      Free (Bottom);
      Free (Last_Line);
   end Replace;

   ------------
   -- Commit --
   ------------

   procedure Commit
     (This         : Extract;
      Current_Text : in out Text_Navigator_Abstr'Class)
   is
      Current_Extract : Ptr_Extract_Line := This.First;
      Last_File_Name  : VFS.Virtual_File := VFS.No_File;
      Offset_Line     : Integer := 0;
   begin
      while Current_Extract /= null loop
         if Get_File (Current_Extract.Cursor) /= Last_File_Name then
            Last_File_Name := Get_File (Current_Extract.Cursor);
            Offset_Line := 0;
         end if;

         Commit (Current_Extract.all, Current_Text, Offset_Line);
         Current_Extract := Current_Extract.Next;
      end loop;
   end Commit;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Extract) is
   begin
      if This.First /= null then
         Free (This.First.all);
         Free (This.First);
      end if;
   end Free;

   ---------------
   -- Free_Data --
   ---------------

   procedure Free_Data (This : in out Extract'Class) is
   begin
      Free (This);
   end Free_Data;

   ------------------
   -- Get_New_Text --
   ------------------

   function Get_New_Text
     (This         : Extract;
      Current_Text : Text_Navigator_Abstr'Class;
      Lines_Before : Natural := 0;
      Lines_After  : Natural := 0) return String
   is
      Extended_Extract : Extract;
   begin
      Extended_Extract := Clone (This);
      Extend_Before (Extended_Extract, Current_Text, Lines_Before);
      Extend_After (Extended_Extract, Current_Text, Lines_After);

      declare
         Buffer : constant String := Get_New_Text (Extended_Extract);
      begin
         Free (Extended_Extract);
         return Buffer;
      end;
   end Get_New_Text;

   ------------------
   -- Get_New_Text --
   ------------------

   function Get_New_Text (This : Extract) return String is
      Current_Extract  : Ptr_Extract_Line := This.First;
      Current_Col      : Natural := 1;
      Current_Length   : Natural;
      Buffer : String
        (1 .. Get_New_Text_Length (This) +
           EOL_Str'Length * Get_Number_Lines (This));
   begin
      Current_Extract := This.First;

      while Current_Extract /= null loop
         Current_Length := Get_New_Text_Length (Current_Extract.all)
           + EOL_Str'Length;
         Buffer (Current_Col ..
                   Current_Col + Current_Length - 1) :=
             Get_New_Text (Current_Extract.all) & EOL_Str;
         Current_Col := Current_Col + Current_Length;
         Current_Extract := Current_Extract.Next;
      end loop;

      return Buffer;
   end Get_New_Text;

   ------------------
   -- Get_Old_Text --
   ------------------

   function Get_Old_Text
     (This         : Extract;
      Current_Text : Text_Navigator_Abstr'Class;
      Lines_Before : Natural := 0;
      Lines_After  : Natural := 0) return String
   is
      Current_Extract  : Ptr_Extract_Line;
      Extended_Extract : Extract;
      Current_Col      : Natural := 1;
      Current_Length   : Natural;
   begin

      Extended_Extract := Clone (This);
      Extend_Before (Extended_Extract, Current_Text, Lines_Before);
      Extend_After (Extended_Extract, Current_Text, Lines_After);
      Current_Extract := Extended_Extract.First;

      declare
         Buffer : String (1 .. Get_Old_Text_Length
                            (Extended_Extract, Current_Text) +
                            EOL_Str'Length * Get_Number_Lines
                              (Extended_Extract));
      begin
         while Current_Extract /= null loop
            Current_Length := Get_Old_Text_Length
              (Current_Extract.all,
               Current_Text) + EOL_Str'Length;
            Buffer (Current_Col ..
                      Current_Col + Current_Length - 1) :=
                Get_Old_Text (Current_Extract.all, Current_Text) & EOL_Str;
            Current_Col := Current_Col + Current_Length;
            Current_Extract := Current_Extract.Next;
         end loop;

         Free (Extended_Extract);

         return Buffer;
      end;
   end Get_Old_Text;

   -------------------------
   -- Get_New_Text_Length --
   -------------------------

   function Get_New_Text_Length (This : Extract) return Natural is
   begin
      if This.First /= null then
         return Get_New_Text_Length (This.First.all, True);
      else
         return 0;
      end if;
   end Get_New_Text_Length;

   -------------------------
   -- Get_Old_Text_Length --
   -------------------------

   function Get_Old_Text_Length
     (This         : Extract;
      Current_Text : Text_Navigator_Abstr'Class) return Natural is
   begin
      if This.First /= null then
         return Get_Old_Text_Length (This.First.all, Current_Text, True);
      else
         return 0;
      end if;
   end Get_Old_Text_Length;

   --------------
   -- Get_Line --
   --------------

   function Get_Line
     (This : Extract; Position : File_Cursor'Class) return Ptr_Extract_Line
   is
      Current_Extract : Ptr_Extract_Line := This.First;
   begin
      while Current_Extract /= null loop
         if Current_Extract.Cursor.Line = Position.Line
           and Get_File (Current_Extract.Cursor) = Get_File (Position)
         then
            return Current_Extract;
         end if;

         Current_Extract := Current_Extract.Next;
      end loop;

      Raise_Exception
        (Codefix_Panic'Identity,
         "line" & Natural'Image (Position.Line) & " of " &
           Full_Name (Get_File (Position)).all & " not found in an extract");
   end Get_Line;

   ----------------
   -- Get_Record --
   ----------------

   function Get_Record
     (This : Extract; Number : Natural) return Ptr_Extract_Line
   is
      Current_Extract : Ptr_Extract_Line := This.First;
      Iterator        : Integer;

   begin
      Iterator := 1;

      loop
         if Current_Extract = null then
            Raise_Exception
              (Codefix_Panic'Identity,
               "record" & Natural'Image (Number) & " not found in an extract");
         end if;

         exit when Iterator = Number;

         Iterator := Iterator + 1;
         Current_Extract := Current_Extract.Next;
      end loop;

      return Current_Extract;
   end Get_Record;

   ------------------
   -- Replace_Word --
   ------------------

   procedure Replace_Word
     (This         : in out Extract;
      Cursor       : File_Cursor'Class;
      New_String   : String;
      Old_String   : String;
      Format_Old   : String_Mode := Text_Ascii)
   is
      Word_Length  : Natural;
      Old_Line     : GNAT.Strings.String_Access;
      Current_Line : Ptr_Extract_Line;

   begin
      Current_Line := Get_Line (This, Cursor);
      Assign (Old_Line, To_String (Current_Line.Content));

      case Format_Old is
         when Regular_Expression =>
            Word_Length := Get_Word_Length
              (Current_Line.all, Cursor.Col, Old_String);

         when Text_Ascii =>
            Word_Length := Old_String'Length;
      end case;

      Replace (Current_Line.all, Cursor.Col, Word_Length, New_String);

      Free (Old_Line);
   end Replace_Word;

   ------------------
   -- Replace_Word --
   ------------------

   procedure Replace_Word
     (This         : in out Extract;
      Cursor       : File_Cursor'Class;
      New_String   : String;
      Old_Length   : Natural)
   is
      Current_Line : Ptr_Extract_Line;
   begin
      Current_Line := Get_Line (This, Cursor);
      Replace (Current_Line.all, Cursor.Col, Old_Length, New_String);
   end Replace_Word;

   --------------
   -- Add_Word --
   --------------

   procedure Add_Word
     (This   : in out Extract;
      Cursor : File_Cursor'Class;
      Word   : String)
   is
      Current_Line : constant Ptr_Extract_Line := Get_Line (This, Cursor);
   begin
      Insert (Current_Line.Content, Cursor.Col, Word);

      Current_Line.Context := Unit_Modified;
   end Add_Word;

   ----------------------
   -- Get_Number_Lines --
   ----------------------

   function Get_Number_Lines (This : Extract) return Natural is
   begin
      if This.First = null then
         return 0;
      else
         return Length (This.First.all);
      end if;
   end Get_Number_Lines;

   --------------
   -- Add_Line --
   --------------

   procedure Add_Line
     (This   : in out Extract;
      Cursor : File_Cursor'Class;
      Text   : String)
   is
      Line_Cursor : File_Cursor := File_Cursor (Clone (Cursor));
   begin
      Line_Cursor.Col := 1;
      Add_Element
        (This, new Extract_Line'
         (Context         => Unit_Created,
          Cursor          => Line_Cursor,
          Original_Length => 0,
          Content         => To_Mergable_String (Text),
          Next            => null,
          Coloration      => True));
   end Add_Line;

   -----------------------
   -- Add_Indented_Line --
   -----------------------

   procedure Add_Indented_Line
     (This         : in out Extract;
      Cursor       : File_Cursor'Class;
      Text         : String;
      Current_Text : Text_Navigator_Abstr'Class)
   is
      Line_Cursor : File_Cursor := File_Cursor (Cursor);
   begin
      Line_Cursor.Col := 1;

      Add_Indented_Line
        (This,
         Cursor,
         Text,
         Get_Line (Current_Text, Line_Cursor));
   end Add_Indented_Line;

   -----------------------
   -- Add_Indented_Line --
   -----------------------

   procedure Add_Indented_Line
     (This          : in out Extract;
      Cursor        : File_Cursor'Class;
      Text          : String;
      Previous_Line : String)
   is
      Line_Cursor : File_Cursor := File_Cursor (Clone (Cursor));
      Indent      : Natural;
      First_Char  : Natural := Text'First;

   begin
      while First_Char <= Text'Last and then Is_Blank (Text (First_Char)) loop
         First_Char := First_Char + 1;
      end loop;

      Line_Cursor.Col := 1;
      Indent          := 0;

      --  ??? Consider using the "Format selection" command instead.

      for J in Previous_Line'Range loop
         case Previous_Line (J) is
            when ' ' =>
               Indent := Indent + 1;
            when ASCII.HT =>
               Indent := Indent + Tab_Width - (Indent mod Tab_Width);
            when others =>
               exit;
         end case;
      end loop;

      Add_Element
        (This, new Extract_Line'
          (Context         => Unit_Created,
           Cursor          => Line_Cursor,
           Original_Length => 0,
           Content         =>
              To_Mergable_String ((1 .. Indent => ' ') &
                                   Text (First_Char .. Text'Last)),
           Next            => null,
           Coloration      => True));
   end Add_Indented_Line;

   -----------------
   -- Delete_Line --
   -----------------

   procedure Delete_Line
     (This   : in out Extract;
      Cursor : File_Cursor'Class)
   is
      Line : Ptr_Extract_Line;
   begin
      Line := Get_Line (This, Cursor);
      Line.Context := Unit_Deleted;
   end Delete_Line;

   ----------------------
   -- Delete_All_Lines --
   ----------------------

   procedure Delete_All_Lines (This : in out Extract) is
      Line : Ptr_Extract_Line := This.First;
   begin
      while Line /= null loop
         Line.Context := Unit_Deleted;
         Line := Line.Next;
      end loop;
   end Delete_All_Lines;

   -----------------
   -- Add_Element --
   -----------------

   procedure Add_Element
     (This, Previous, Element : Ptr_Extract_Line;
      Container : in out Extract) is
   begin
      pragma Assert (This /= null or else Previous /= null);

      if This = null then
         Previous.Next := Element;
      else
         if This.Cursor > Element.Cursor then
            Element.Next := This;

            if Previous /= null then
               Previous.Next := Element;
            else
               Container.First := Element;
            end if;
         elsif This.Cursor = Element.Cursor
           and then (This.Context = Unit_Modified
                     or else This.Context = Original_Unit)
           and then (Element.Context = Unit_Modified
                     or else Element.Context = Original_Unit)
         then
            return;
         else
            Add_Element (This.Next, This, Element, Container);
         end if;
      end if;
   end Add_Element;

   -----------------
   -- Add_Element --
   -----------------

   procedure Add_Element (This : in out Extract; Element : Ptr_Extract_Line) is
   begin
      if This.First = null then
         This.First := Element;
      else
         Add_Element (This.First, null, Element, This);
      end if;
   end Add_Element;

   procedure Add_Element (This : in out Extract; Element : Extract_Line) is
   begin
      Add_Element (This, new Extract_Line'(Element));
   end Add_Element;

   ---------------------
   -- Get_Word_Length --
   ---------------------

   function Get_Word_Length
     (This   : Extract;
      Cursor : File_Cursor'Class;
      Format : String) return Natural is
   begin
      return Get_Word_Length
        (Get_Line (This, Cursor).all,
         Cursor.Col,
         Format);
   end Get_Word_Length;

   -------------------
   -- Search_String --
   -------------------

   function Search_String
     (This           : Extract;
      Searched       : String;
      Escape_Manager : Escape_Str_Manager'Class;
      Cursor         : File_Cursor'Class := Null_File_Cursor;
      Step           : Step_Way := Normal_Step) return File_Cursor'Class
   is
      Current, Last_Line : Ptr_Extract_Line;
      Result             : File_Cursor := Null_File_Cursor;
      Current_Cursor     : File_Cursor := Null_File_Cursor;
      Real_Result        : File_Cursor;

   begin
      if File_Cursor (Cursor) /= Null_File_Cursor then
         Current_Cursor := File_Cursor (Cursor);
      else
         if Step = Normal_Step then
            Current_Cursor :=
              File_Cursor (Get_Cursor (Get_First_Line (This).all));
         else
            Last_Line := Get_Last_Line (This);
            Current_Cursor := File_Cursor (Get_Cursor (Last_Line.all));
            Current_Cursor.Col :=
              To_Column_Index
                (Char_Index (Get_String (Last_Line.all)'Last),
                 Get_String (Last_Line.all));
         end if;
      end if;

      Current := Get_Line (This, Current_Cursor);

      loop
         if Current = null then
            return Null_File_Cursor;
         end if;

         Result := File_Cursor (Search_String
           (Current.all,
            Current_Cursor,
            Searched,
            Escape_Manager,
            Step));

         if Result /= Null_File_Cursor then
            --  ??? Do I really need this copy ???
            Real_Result := Clone (Result);
            Free (Result);
            Real_Result.Line := Get_Cursor (Current.all).Line;

            return Real_Result;
         end if;

         exit when Current = null;

         case Step is
            when Normal_Step =>
               Current := Current.Next;
               Current_Cursor.Col := 1;
            when Reverse_Step =>
               Current := Previous (This, Current);
               Current_Cursor.Col := 0;
         end case;

      end loop;

      return Null_File_Cursor;
   end Search_String;

   --------------
   -- Previous --
   --------------

   function Previous
     (Container : Extract; Node : Ptr_Extract_Line) return Ptr_Extract_Line
   is
      Current : Ptr_Extract_Line := Container.First;
   begin
      if Current = Node then
         return null;
      end if;

      while Current.Next /= Node loop
         Current := Current.Next;

         if Current = null then
            Raise_Exception
              (Codefix_Panic'Identity,
               "Line unknowm in the specified extract");
         end if;
      end loop;

      return Current;
   end Previous;

   --------------------
   -- Get_First_Line --
   --------------------

   function Get_First_Line (This : Extract) return Ptr_Extract_Line is
   begin
      return This.First;
   end Get_First_Line;

   -------------------
   -- Get_Last_Line --
   -------------------

   function Get_Last_Line (This : Extract) return Ptr_Extract_Line is
      Result : Ptr_Extract_Line := This.First;
   begin
      while Result.Next /= null loop
         Result := Result.Next;
      end loop;

      return Result;
   end Get_Last_Line;

   -------------------
   -- Extend_Before --
   -------------------

   procedure Extend_Before
     (This         : in out Extract;
      Current_Text : Text_Navigator_Abstr'Class;
      Size         : Natural)
   is
      Null_Prev : Ptr_Extract_Line := null;
   begin
      Extend_Before (This.First, Null_Prev, Current_Text, Size);
   end Extend_Before;

   ------------------
   -- Extend_After --
   ------------------

   procedure Extend_After
     (This         : in out Extract;
      Current_Text : Text_Navigator_Abstr'Class;
      Size         : Natural) is
   begin
      Extend_After (This.First, Current_Text, Size);
   end Extend_After;

   ------------
   -- Reduce --
   ------------

   procedure Reduce
     (This                    : in out Extract;
      Size_Before, Size_After : Natural)
   is
      type Lines_Array is array (Natural range <>) of Ptr_Extract_Line;

      procedure Delete_First;
      --  ???

      procedure Add_Last;
      --  ???

      Current_Line : Ptr_Extract_Line;
      Lines        : Lines_Array (1 .. Size_Before);
      Left_After   : Natural := 0;
      Next_Line    : Ptr_Extract_Line;
      Prev_Line    : Ptr_Extract_Line;

      procedure Delete_First is
      begin
         Remove (Lines (1), Previous (This, Lines (1)), This);
         Free (Lines (1).all);
         Free (Lines (1));
         Lines (1 .. Lines'Last - 1) := Lines (2 .. Lines'Last);
         Lines (Lines'Last) := null;
      end Delete_First;

      procedure Add_Last is
      begin
         for J in Lines'Range loop
            if Lines (J) = null then
               Lines (J) := Current_Line;
               return;
            end if;
         end loop;

         Delete_First;
         Lines (Lines'Last) := Current_Line;
      end Add_Last;

   begin
      Current_Line := Get_First_Line (This);

      while Current_Line /= null loop
         Next_Line := Next (Current_Line.all);

         if Current_Line.Coloration = False
           or else Current_Line.Context = Original_Unit
         then
            if Left_After = 0 then
               if Size_Before = 0 then
                  Remove (Current_Line, Prev_Line, This);
                  Free (Current_Line.all);
                  Free (Current_Line);
               else
                  Prev_Line := Current_Line;
                  Add_Last;
               end if;
            else
               Prev_Line := Current_Line;
               Left_After := Left_After - 1;
            end if;
         else
            Prev_Line := Current_Line;
            Left_After := Size_After;
            Lines := (others => null);
         end if;

         Current_Line := Next_Line;
      end loop;

      if Left_After = 0 then
         for J in Lines'Range loop
            exit when Lines (J) = null;
            Remove (Lines (J), Previous (This, Lines (J)), This);
            Free (Lines (J).all);
            Free (Lines (J));
         end loop;
      end if;
   end Reduce;

   -----------
   -- Erase --
   -----------

   procedure Erase
     (This        : in out Extract;
      Start, Stop : File_Cursor'Class)
   is
      Current_Line : Ptr_Extract_Line;
      Line_Cursor  : File_Cursor := File_Cursor (Start);
      Start_Char_Index : Char_Index;
      Stop_Char_Index  : Char_Index;
   begin
      Line_Cursor.Col := 1;

      if Start.Line = Stop.Line then
         Current_Line := Get_Line (This, Line_Cursor);

         declare
            Current_String : constant String :=
              To_String (Current_Line.Content);
         begin
            Start_Char_Index := To_Char_Index (Start.Col, Current_String);
            Stop_Char_Index := To_Char_Index (Stop.Col, Current_String);
         end;

         Delete
           (Current_Line.Content,
            Start_Char_Index,
            Natural (Stop_Char_Index - Start_Char_Index + 1));

         if Is_Blank (To_String (Current_Line.Content)) then
            Current_Line.Context := Unit_Deleted;
         else
            Current_Line.Context := Unit_Modified;
         end if;

      else
         Current_Line := Get_Line (This, Line_Cursor);

         Delete (Current_Line.Content, Start.Col);

         if Is_Blank (To_String (Current_Line.Content)) then
            Current_Line.Context := Unit_Deleted;
         else
            Current_Line.Context := Unit_Modified;
         end if;

         for J in Start.Line + 1 .. Stop.Line - 1 loop
            Current_Line := Next (Current_Line.all);
            Current_Line.Context := Unit_Deleted;
         end loop;

         Current_Line := Next (Current_Line.all);

         declare
            Current_String : constant String :=
              To_String (Current_Line.Content);
         begin
            Delete
              (Current_Line.Content,
               Char_Index (1),
               Natural (To_Char_Index (Stop.Col, Current_String)));
         end;

         if Is_Blank (To_String (Current_Line.Content)) then
            Current_Line.Context := Unit_Deleted;
         else
            Current_Line.Context := Unit_Modified;
         end if;
      end if;
   end Erase;

   -------------
   -- Comment --
   -------------

   procedure Comment
     (This        : in out Extract;
      Start, Stop : File_Cursor'Class)
   is
      Current_Line     : Ptr_Extract_Line;
      Line_Cursor      : File_Cursor := File_Cursor (Start);
      Stop_Char_Index  : Char_Index;
      Start_Char_Index : Char_Index;

      function Back_Of_Line return String;
      --  Returns the back of the line, assuming that Current_Line is set on
      --  the last line of the extract. The back of lines begins after the
      --  last column of the entity. Stop_Char_Index has also to be set
      --  correctly.

      function Back_Of_Line return String is
      begin
         return To_String (Current_Line.Content)
           (Natural (Stop_Char_Index) + 1 ..
              To_String (Current_Line.Content)'Last);
      end Back_Of_Line;

   begin
      Line_Cursor.Col := 1;
      Current_Line := Get_Line (This, Line_Cursor);

      if Start.Line = Stop.Line then
         --  Add a new line if there is some text after the entity that has
         --  not to be commented

         declare
            Current_String : constant String :=
              To_String (Current_Line.Content);
         begin
            Start_Char_Index := To_Char_Index (Start.Col, Current_String);
            Stop_Char_Index := To_Char_Index (Stop.Col, Current_String);
         end;

         if not Is_Blank (Back_Of_Line) then
            Add_Indented_Line
              (This,
               Stop,
               Back_Of_Line,
               To_String (Current_Line.Content)
               (1 .. Natural (Stop_Char_Index)));
            Replace
              (Current_Line.all,
               Stop.Col + 1,
               Back_Of_Line'Length, "");
         end if;

         --  But proper comment at the begining of the entity

         if Is_Blank (To_String (Current_Line.Content)
                      (1 .. Natural (Start_Char_Index) - 1))
         then
            Insert (Current_Line.Content, Char_Index (1), "--  ");
         else
            Insert (Current_Line.Content, Start_Char_Index, "--  ");
         end if;

         Current_Line.Context := Unit_Modified;
      else
         --  But proper comment at the begining of the entity

         declare
            Current_String : constant String :=
              To_String (Current_Line.Content);
         begin
            Start_Char_Index := To_Char_Index (Start.Col, Current_String);
         end;

         if Is_Blank (To_String (Current_Line.Content)
                      (1 .. Natural (Start_Char_Index) - 1))
         then
            Insert (Current_Line.Content, Char_Index (1), "--  ");
         else
            Insert (Current_Line.Content, Start.Col, "--  ");
         end if;

         Current_Line.Context := Unit_Modified;

         for J in Start.Line + 1 .. Stop.Line - 1 loop
            Current_Line := Next (Current_Line.all);
            Insert (Current_Line.Content, Char_Index (1), "--  ");
            Current_Line.Context := Unit_Modified;
         end loop;

         Current_Line := Next (Current_Line.all);

         --  Add a new line if there is some text after the entity that has
         --  not to be commented

         declare
            Current_String : constant String :=
              To_String (Current_Line.Content);
         begin
            Stop_Char_Index := To_Char_Index (Stop.Col, Current_String);
         end;

         if not Is_Blank (Back_Of_Line) then
            Add_Indented_Line
              (This,
               Stop,
               Back_Of_Line,
               To_String
                 (Current_Line.Content) (1 .. Natural (Stop_Char_Index)));
            Replace
              (Current_Line.all, Stop.Col + 1, Back_Of_Line'Length, "");
         end if;

         Insert (Current_Line.Content, Char_Index (1), "--  ");
         Current_Line.Context := Unit_Modified;
      end if;
   end Comment;

   ---------------------
   -- Get_Files_Names --
   ---------------------

   function Get_Files_Names
     (This : Extract; Size_Max : Natural := 0) return String
   is
      Previous     : VFS.Virtual_File;
      Current_Line : Ptr_Extract_Line := Get_First_Line (This);
      Result       : GNAT.Strings.String_Access;

   begin
      if Current_Line = null then
         return "";
      end if;

      Previous := Get_File (Current_Line.Cursor);
      Assign (Result, Full_Name (Previous).all);

      while Current_Line /= null loop
         if Previous /= Get_File (Current_Line.Cursor) then
            Assign
              (Result, Result.all & " / " &
               Full_Name (Get_File (Current_Line.Cursor)).all);
            Previous := Get_File (Current_Line.Cursor);
         end if;
         Current_Line := Current_Line.Next;
      end loop;

      declare
         Result_Stack : constant String := Result.all;
      begin
         Free (Result);
         if Size_Max > 0 and then Result_Stack'Length > Size_Max then
            return Result_Stack
              (Result_Stack'First .. Result_Stack'First + Size_Max)
               & "...";
         else
            return Result_Stack;
         end if;
      end;
   end Get_Files_Names;

   ------------------
   -- Get_Nb_Files --
   ------------------

   function Get_Nb_Files (This : Extract) return Natural is
      Previous     : VFS.Virtual_File;
      Current_Line : Ptr_Extract_Line := Get_First_Line (This);
      Total        : Natural := 1;

   begin
      if Current_Line = null then
         return 0;
      end if;

      Previous := Get_File (Current_Line.Cursor);

      while Current_Line /= null loop
         if Previous /= Get_File (Current_Line.Cursor) then
            Total := Total + 1;
            Previous := Get_File (Current_Line.Cursor);
         end if;
         Current_Line := Current_Line.Next;
      end loop;

      return Total;
   end Get_Nb_Files;

   ------------------------
   -- Delete_Empty_Lines --
   ------------------------

   procedure Delete_Empty_Lines (This : in out Extract) is
      Node : Ptr_Extract_Line := Get_First_Line (This);
   begin
      while Node /= null loop
         if Is_Blank (Get_String (Node.all)) then
            Node.Context := Unit_Deleted;
         end if;

         Node := Next (Node);
      end loop;
   end Delete_Empty_Lines;

   ------------------------
   -- Get_Number_Actions --
   ------------------------

   function Get_Number_Actions (This : Extract) return Natural is
      Line  : Ptr_Extract_Line := Get_First_Line (This);
      Total : Natural := 0;
   begin
      while Line /= null loop
         Total := Total + Get_Number_Actions (Line.all);
         Line := Next (Line);
      end loop;

      return Total;
   end Get_Number_Actions;

   ----------
   -- Undo --
   ----------

   procedure Undo
     (This : Extract; Current_Text : Text_Navigator_Abstr'Class)
   is
      Line : Ptr_Extract_Line := Get_First_Line (This);
   begin
      while Line /= null loop
         for J in 1 .. Get_Number_Actions (Line.all) loop
            Undo (Current_Text, Get_File (Line.Cursor));
         end loop;

         Line := Line.Next;
      end loop;
   end Undo;

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
      Current_Text : Text_Navigator_Abstr'Class;
      New_Extract  : out Extract'Class;
      Error_Cb     : Execute_Corrupted := null) is
   begin
      Update_All (Current_Text);
      Execute (This, Current_Text, New_Extract);

   exception
      when E : Codefix_Panic =>
         if Error_Cb /= null then
            Error (Error_Cb, Exception_Information (E));
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
      Current_Text : Text_Navigator_Abstr'Class;
      New_Extract  : out Extract'Class)
   is
      Line_Cursor : File_Cursor;
      Word        : Word_Cursor;
   begin
      Make_Word_Cursor (This.Word, Current_Text, Word);
      Line_Cursor := File_Cursor (Word);

      Line_Cursor.Col := 1;
      Get_Line (Current_Text, Line_Cursor, New_Extract);

      case Word.Mode is
         when Text_Ascii =>
            Delete
              (Get_First_Line (New_Extract).Content,
               Get_Column (Word),
               Word.String_Match'Length);
         when Regular_Expression =>
            Delete
              (Get_First_Line (New_Extract).Content,
               Get_Column (Word),
               Get_Word_Length (New_Extract, Word, Word.String_Match.all));
      end case;

      Get_First_Line (New_Extract).Context := Unit_Modified;
      Delete_Empty_Lines (New_Extract);

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

   procedure Execute
     (This         : Insert_Word_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      New_Extract  : out Extract'Class)
   is
      New_Str         : GNAT.Strings.String_Access;
      Line_Cursor     : File_Cursor;
      Space_Cursor    : File_Cursor;
      Word            : Word_Cursor;
      Length          : Integer;
      New_Pos         : Word_Cursor;
      Word_Char_Index : Char_Index;

      Tmp_Extract     : Extract;
   begin
      Make_Word_Cursor (This.Word, Current_Text, Word);
      Make_Word_Cursor (This.New_Position, Current_Text, New_Pos);

      Line_Cursor := Clone (File_Cursor (New_Pos));
      Line_Cursor.Col := 1;

      Word_Char_Index :=
        To_Char_Index (Word.Col, Get_Line (Current_Text, Line_Cursor));

      case Word.Mode is
         when Regular_Expression =>
            Get_Line (Current_Text, File_Cursor (Word), Tmp_Extract);
            declare
               Str : constant String := Get_String (Tmp_Extract);
            begin
               Length := Get_Word_Length
                 (Tmp_Extract, Word, Word.String_Match.all);
               Assign (New_Str, Str (Str'First .. Str'First + Length - 1));
            end;

         when Text_Ascii =>
            Assign (New_Str, Word.String_Match);
      end case;

      if This.Position = Specified then
         Get_Line (Current_Text, Line_Cursor, New_Extract);

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
               Extract_Line   : Ptr_Extract_Line;
            begin
               Extract_Line := Get_Line (New_Extract, New_Pos);

               declare
                  Line : constant String := Get_String (Extract_Line.all);
                  Pos_Char_Index : Char_Index;
               begin
                  Pos_Char_Index := To_Char_Index (Get_Column (New_Pos), Line);
                  Replace
                    (New_Extract,
                     New_Pos,
                     Line (Natural (Pos_Char_Index) .. Line'Last)'Length,
                     "");
                  Add_Indented_Line
                    (New_Extract,
                     New_Pos,
                     Line (Natural (Pos_Char_Index) .. Line'Last),
                     Current_Text);
               end;
            end;
         end if;

         Add_Word (New_Extract, New_Pos, New_Str.all);

      elsif This.Position = After then
         Add_Indented_Line
           (New_Extract, New_Pos, New_Str.all, Current_Text);
      elsif This.Position = Before then
         New_Pos.Line := New_Pos.Line - 1;
         Add_Indented_Line
           (New_Extract, New_Pos, New_Str.all, Current_Text);
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
      Current_Text : Text_Navigator_Abstr'Class;
      New_Extract  : out Extract'Class)
   is
      Extract_Remove, Extract_Insert : Extract;
      Success                        : Boolean;
   begin
      Execute (This.Step_Insert, Current_Text, Extract_Insert);
      Execute (This.Step_Remove, Current_Text, Extract_Remove);
      Merge_Extracts
        (New_Extract,
         Extract_Remove,
         Extract_Insert,
         Success,
         False);

      if not Success then
         raise Codefix_Panic;
      end if;

      Free (Extract_Remove);
      Free (Extract_Insert);
   end Execute;

   ----------------------
   -- Replace_Word_Cmd --
   ----------------------

   procedure Initialize
     (This         : in out Replace_Word_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Word         : Word_Cursor'Class;
      New_Word     : String) is
   begin
      Make_Word_Mark (Word, Current_Text, This.Mark);
      Assign (This.Str_Expected, New_Word);
   end Initialize;

   procedure Free (This : in out Replace_Word_Cmd) is
   begin
      Free (This.Mark);
      Free (This.Str_Expected);
      Free (Text_Command (This));
   end Free;

   procedure Execute
     (This         : Replace_Word_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      New_Extract  : out Extract'Class)
   is
      Current_Word : Word_Cursor;
      Line_Cursor  : File_Cursor;
   begin
      Make_Word_Cursor (This.Mark, Current_Text, Current_Word);

      Line_Cursor := File_Cursor (Current_Word);
      Line_Cursor.Col := 1;
      Get_Line (Current_Text, Line_Cursor, New_Extract);

      Replace_Word
        (New_Extract,
         Current_Word,
         This.Str_Expected.all,
         Current_Word.String_Match.all,
         Current_Word.Mode);

      Free (Current_Word);
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
      Current_Text : Text_Navigator_Abstr'Class;
      New_Extract  : out Extract'Class)
   is
      Extract1, Extract2 : Extract;
      Success            : Boolean;
   begin
      Execute (This.Step_Word1, Current_Text, Extract1);
      Execute (This.Step_Word2, Current_Text, Extract2);
      Merge_Extracts
        (New_Extract,
         Extract1,
         Extract2,
         Success,
         False);

      if not Success then
         raise Codefix_Panic;
      end if;

      Free (Extract1);
      Free (Extract2);
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
      Current_Text : Text_Navigator_Abstr'Class;
      New_Extract  : out Extract'Class)
   is
      Cursor : File_Cursor;
   begin
      Cursor := File_Cursor
        (Get_Current_Cursor (Current_Text, This.Position.all));
      Add_Line (New_Extract, Cursor, This.Line.all);
      Free (Cursor);
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
      Current_Text : Text_Navigator_Abstr'Class;
      New_Extract  : out Extract'Class)
   is
      Start_Cursor, End_Cursor : File_Cursor;
   begin
      Start_Cursor := File_Cursor
        (Get_Current_Cursor (Current_Text, This.Start_Mark.all));
      End_Cursor := File_Cursor
        (Get_Current_Cursor (Current_Text, This.End_Mark.all));

      for J in Start_Cursor.Line .. End_Cursor.Line loop
         declare
            Cursor : File_Cursor := Start_Cursor;
         begin
            Set_Location (Cursor, J, 1);
            Get_Line (Current_Text, Cursor, New_Extract);
         end;
      end loop;

      Erase (New_Extract, Start_Cursor, End_Cursor);
      Replace (New_Extract, Start_Cursor, 0, This.New_Text.all);
   end Execute;

   --------------------
   -- Merge_Extracts --
   --------------------

   procedure Merge_Extracts
     (Result              : out Extract'Class;
      Object_1, Object_2  : Extract'Class;
      Success             : out Boolean;
      Chronologic_Changes : Boolean)
   is
      procedure Merge_Internal is new Generic_Merge
        (Merge_Type     => Extract,
         Merged_Unit    => Extract_Line,
         Merge_Iterator => Ptr_Extract_Line,
         First          => Get_First_Line,
         Get_Merge_Info => Get_Context,
         Set_Merge_Info => Set_Context,
         Append         => Add_Element,
         Merge_Units    => Merge_Lines);

   begin
      Merge_Internal
        (Extract (Result),
         Extract (Object_1), Extract (Object_2),
         Success,
         Chronologic_Changes);
   end Merge_Extracts;

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

   function Get_File (This : File_Cursor) return VFS.Virtual_File is
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

   ----------
   -- Free --
   ----------

   procedure Free (Line : in out Ptr_Extract_Line) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Extract_Line, Ptr_Extract_Line);
   begin
      Unchecked_Free (Line);
   end Free;

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

   procedure Set_File (This : in out File_Cursor; File : VFS.Virtual_File) is
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

end Codefix.Text_Manager;
