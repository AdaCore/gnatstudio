-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2002-2007, AdaCore                 --
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

with Ada.Unchecked_Deallocation;
with GNAT.Strings;

with Language;               use Language;
with Language.Tree;          use Language.Tree;
with Language.Tree.Database; use Language.Tree.Database;
with VFS;
with Projects.Registry;

with Generic_List;

with Codefix.Merge_Utils; use Codefix.Merge_Utils;

package Codefix.Text_Manager is

   type Step_Way is (Normal_Step, Reverse_Step);
   type Case_Type is (Lower, Upper, Mixed);

   function Is_Blank (Str : String) return Boolean;
   --  Return true if Str is only composed by white characters.

   function Is_Blank (Char : Character) return Boolean;
   --  Return true if Str is a white characters.

   function Without_Last_Blanks (Str : String) return String;
   --  Return the string Str without the last blanks characters.

   function Is_Separator (Char : Character) return Boolean;
   --  Return True if the character can be considerate as a separator.

   ----------------------------------------------------------------------------
   --  type Text_Cursor
   ----------------------------------------------------------------------------

   type Text_Cursor is tagged private;

   function "<" (Left, Right : Text_Cursor) return Boolean;
   --  Return True when Left is before Right.

   function ">" (Left, Right : Text_Cursor'Class) return Boolean;
   --  Return True when Left is after Right.

   function "<=" (Left, Right : Text_Cursor'Class) return Boolean;
   --  Return True when Left is before or in the same position than Right.

   function ">=" (Left, Right : Text_Cursor'Class) return Boolean;
   --  Return True when Left is after or in the same position than Right.

   procedure Set_Location
     (This : in out Text_Cursor; Line : Natural; Column : Column_Index);
   --  Set the location information

   function Get_Line (This : Text_Cursor) return Integer;
   function Get_Column (This : Text_Cursor) return Column_Index;
   --  Return the location

   procedure Set_Line (This : in out Text_Cursor; Line : Natural);
   procedure Set_Column (This : in out Text_Cursor; Column : Column_Index);
   --  Set the line or column

   type File_Cursor is new Text_Cursor with private;
   Null_File_Cursor : constant File_Cursor;

   procedure Set_File (This : in out File_Cursor; File : VFS.Virtual_File);
   --  Set the file information

   function Get_File (This : File_Cursor) return VFS.Virtual_File;
   --  Return the file associated with the cursor

   function "=" (Left, Right : File_Cursor) return Boolean;
   --  Return true when Left is in the same position than rigth.

   function "<" (Left, Right : File_Cursor) return Boolean;
   --  Return True when Left is before Right.

   procedure Free (This : in out File_Cursor);
   --  Frees the memory used by fields of File_Cursor.
   --  Does nothing, just needed for generic instantiation

   function Clone (This : File_Cursor) return File_Cursor;
   --  Duplicate all informations of a File_Cursor, specially informations
   --  memorized in dynamic memory.

   procedure Assign
     (This : in out File_Cursor'Class; Source : File_Cursor'Class);

   type Mark_Abstr is abstract tagged private;
   type Ptr_Mark is access all Mark_Abstr'Class;

   procedure Free (This : in out Mark_Abstr);
   --  Free the memory associated to a Mark_Abstr.

   procedure Free_Data (This : in out Mark_Abstr'Class);
   --  Free the memory associated to a Mark_Abstr.

   procedure Free (This : in out Ptr_Mark);
   --  Free the memory associated to a Ptr_Mark.

   type Word_Cursor is new File_Cursor with private;
   --  Word_cursor is an object that describes a specific word in the text. In
   --  case where it is used to match a word in the text, the mode can be
   --  'Regular_Expression'. Otherwise, this field is ignored.

   Null_Word_Cursor : constant Word_Cursor;

   type String_Mode is (Text_Ascii, Regular_Expression);

   type Word_Mark is record
      Mark_Id      : Ptr_Mark;
      String_Match : GNAT.Strings.String_Access;
      Mode         : String_Mode := Text_Ascii;
   end record;

   procedure Set_Word
     (Word         : in out Word_Cursor;
      String_Match : String;
      Mode         : String_Mode := Text_Ascii);
   --  Change the attributes of Word.
   --  No String_Match is registered if an empty string is passed

   function Get_Word (Word : Word_Cursor) return String;
   --  Return the word currently stored by the cursor, "" if none.

   procedure Free (This : in out Word_Mark);
   --  Free the memory associated to a Word_Mark.

   procedure Free (This : in out Word_Cursor);
   --  Free the memory associated to a Word_Cursor.

   ----------------------------------------------------------------------------
   --  type Text_Interface
   ----------------------------------------------------------------------------

   type Text_Interface is abstract tagged private;
   type Ptr_Text is access all Text_Interface'Class;

   procedure Undo (This : in out Text_Interface) is abstract;
   --  Undo the last action for the Text_Interface

   function Get_New_Mark
     (Current_Text : Text_Interface;
      Cursor       : File_Cursor'Class) return Mark_Abstr'Class is abstract;
   --  Create a new mark at the position specified by the cursor.

   function Get_Current_Cursor
     (Current_Text : Text_Interface;
      Mark         : Mark_Abstr'Class) return File_Cursor'Class is abstract;
   --  Return the current position of the mark.

   procedure Free (This : in out Ptr_Text);
   --  Free the memory associated to Ptr_Text and the object referenced.

   procedure Initialize
     (This      : in out Text_Interface;
      File_Name : VFS.Virtual_File) is abstract;
   --  Initialize the structure of the Text_Interface.

   procedure Free (This : in out Text_Interface);
   --  Free the memory associated to the Text_Interface.

   function Get
     (This   : Text_Interface;
      Cursor : Text_Cursor'Class;
      Len    : Natural) return String is abstract;
   --  Get Len characters from the the position specified by the cursor. The
   --  String resulting must have parameter 'First equal to Cursor.Col.

   function Get
     (This        : Text_Interface;
      Start, Stop : Text_Cursor'Class) return String;
   --  Return the text extracted between the two cursors.

   function Get
     (This   : Text_Interface;
      Cursor : Text_Cursor'Class) return Character is abstract;
   --  Get the characters from the the position specified by the cursor.

   function Get_Line
     (This      : Text_Interface;
      Cursor    : Text_Cursor'Class;
      Start_Col : Column_Index := 0) return String is abstract;
   --  Get all character from the column specified by the cursor to the end of
   --  the line, or beginning by Start_Col if not 0. The String resulting must
   --  have parameter 'First equal to Cursor.Col.

   procedure Replace
     (This      : in out Text_Interface;
      Cursor    : Text_Cursor'Class;
      Len       : Natural;
      New_Value : String) is abstract;
   --  Replace the Len characters, from the position designed by the cursor, by
   --  New_Value

   procedure Add_Line
     (This        : in out Text_Interface;
      Cursor      : Text_Cursor'Class;
      New_Line    : String) is abstract;
   --  Add a line AFTER the line specified by the cursor. To add a line at the
   --  begining of the text, set cursor line = 0.

   procedure Delete_Line
     (This : in out Text_Interface;
      Cursor : Text_Cursor'Class) is abstract;
   --  Delete the line where the cursor is.

   procedure Indent_Line
     (This   : in out Text_Interface;
      Cursor : Text_Cursor'Class) is abstract;
   --  Indent the line pointed by the cursor.

   function Line_Length
     (This   : Text_Interface'Class;
      Cursor : Text_Cursor'Class) return Natural;
   --  Returns le length of a line from the position of the cursor.

   function Read_File
     (This : Text_Interface) return GNAT.Strings.String_Access is abstract;
   --  Get the entire file in a String_Access.

   function Get_Buffer
     (This : access Text_Interface'Class) return GNAT.Strings.String_Access;
   --  Return the buffer stored in this text

   function Get_File_Name (This : Text_Interface) return VFS.Virtual_File;
   --  Return the name of the file.

   type Token_Record is record
      Name : String_Access;
      Kind : Language_Entity;
   end record;

   type Token_List is array (Integer range <>) of Token_Record;

   Open_Paren_Tok : constant Token_Record :=
     (Kind => Operator_Text, Name => new String'("("));
   Close_Paren_Tok : constant Token_Record :=
     (Kind => Operator_Text, Name => new String'(")"));
   Semicolon_Tok : constant Token_Record :=
     (Kind => Operator_Text, Name => new String'(":"));
   Is_Tok : constant Token_Record :=
     (Kind => Keyword_Text, Name => new String'("is"));

   function Search_Token
     (This     : Text_Interface'Class;
      Cursor   : Text_Cursor'Class;
      Searched : Token_Record;
      Step     : Step_Way := Normal_Step) return File_Cursor'Class;
   --  Search a token in the text and returns a cursor at the beginning. If
   --  noting is found, then the cursor is Null_Cursor.

   function Search_Tokens
     (This     : Text_Interface'Class;
      Cursor   : Text_Cursor'Class;
      Searched : Token_List;
      Step     : Step_Way := Normal_Step) return Word_Cursor'Class;
   --  Search a token in the text, among serveal possibilities, and returns a
   --  cursor at the beginning. Cursor is returned on the first matching
   --  token If noting is found, then the cursor is Null_Cursor.

   function Line_Max (This : Text_Interface) return Natural is abstract;
   --  Return the number of the last line in the text loaded.

   procedure Text_Has_Changed (This : in out Text_Interface'Class);
   --  This function informs a Text_Interface that the text has changed. If
   --  an analyse of the structure is asked, then the text will be re-parsed.

   function Search_Unit
     (This     : access Text_Interface'Class;
      Category : Language_Category;
      Name     : String := "") return Simple_Construct_Information;
   --  Return the first Contruct_Information that matche Category and name.
   --  If not found, return a Contruct_Information with Category = Cat_Unknown.
   --  If Name is "", then the first unit with the rigth Category will be
   --  returned.

   function Get_Full_Prefix
     (This     : access Text_Interface'Class;
      Cursor   : Text_Cursor'Class;
      Category : Language_Category := Cat_Unknown)
      return String;
   --  Return the entire prefix of the first unit of category after the cursor.

   function Get_Right_Paren
     (This         : Text_Interface'Class;
      Cursor       : Text_Cursor'Class;
      Current_Line : String)
      return Text_Cursor'Class;
   --  Return the right paren corresponding to the one in the cursor.

   procedure Next_Word
     (This   : Text_Interface'Class;
      Cursor : in out Text_Cursor'Class;
      Word   : out GNAT.Strings.String_Access);
   --  Put Cursor after the next word, and set 'Word' to this value, knowing
   --  that a word is a succession of non-blanks characters.

   function Get_Structure
     (This : access Text_Interface'Class) return Construct_List_Access;
   --  Return the parsed strucutre of the text_interface.

   function Get_Structured_File
     (This : access Text_Interface'Class) return Structured_File_Access;
   --  Return the tree associated to this text.

   procedure Constrain_Update (This : in out Text_Interface) is abstract;
   --  This function should constrain the update of the information contained
   --  in This.

   function Previous_Char
     (This : Text_Interface'Class; Cursor : Text_Cursor'Class)
     return Text_Cursor'Class;
   --  Return a cursor positioned on the first non-blank character before the
   --  position specified by the cursor

   type Codefix_Entity_Callback is access function
     (Entity         : Language_Entity;
      Sloc_Start     : Source_Location;
      Sloc_End       : Source_Location;
      Partial_Entity : Boolean;
      Line           : String) return Boolean;

   procedure Parse_Entities
     (Lang     : access Language_Root'Class;
      This     : Text_Interface'Class;
      Callback : Codefix_Entity_Callback;
      Start    : Text_Cursor'Class);
   --  Parse entities (as defined by Language_Entity) contained in buffer.
   --  For each match, call Callback. Stops at the end of Buffer or when
   --  callback returns True.

   ----------------------------------------------------------------------------
   --  type Text_Navigator
   ----------------------------------------------------------------------------

   type Text_Navigator_Abstr is abstract tagged private;

   type Ptr_Text_Navigator is access all Text_Navigator_Abstr'Class;

   procedure Free (This : in out Ptr_Text_Navigator);

   procedure Set_Registry
     (Text     : in out Text_Navigator_Abstr;
      Registry : Projects.Registry.Project_Registry_Access);

   function Get_Registry
     (Text : Text_Navigator_Abstr)
      return Projects.Registry.Project_Registry_Access;

   procedure Set_Construct_Database
     (Text : in out Text_Navigator_Abstr;
      Db   : Construct_Database_Access);

   function Get_Construct_Database
     (Text : Text_Navigator_Abstr) return Construct_Database_Access;

   function Get_Body_Or_Spec
     (Text : Text_Navigator_Abstr; File_Name : VFS.Virtual_File)
      return VFS.Virtual_File;
   --  When File_Name is a spec file, this function returns the body
   --  corresponding, otherwise it returns the spec.

   function New_Text_Interface (This : Text_Navigator_Abstr)
     return Ptr_Text is abstract;
   --  Create and initialise a new Text_Interface used by the text navigator.

   procedure Initialize
     (This : Text_Navigator_Abstr;
      File : in out Text_Interface'Class);
   --  This fonction is call after the initialization of a new Text_Interface.
   --  This one doesn't do anything, but it might be usefull for later
   --  extentions.

   function Get_New_Mark
     (Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class) return Mark_Abstr'Class;
   --  Create a new mark at the position specified by the cursor. If the line
   --  defined by the cursor is 0, then the call of Get_Current_Cursor will
   --  restitute this 0.

   function Get_Current_Cursor
     (Current_Text : Text_Navigator_Abstr'Class;
      Mark         : Mark_Abstr'Class) return File_Cursor'Class;
   --  Return the current position of the mark.

   procedure Free (This : in out Text_Navigator_Abstr);
   --  Free the memory associated to a Text_Navigator.

   function Get_Iterator_At
     (Current_Text      : Text_Navigator_Abstr;
      Cursor            : File_Cursor'Class;
      Position          : Relative_Position := Specified;
      Categories_Seeked : Category_Array := Null_Category_Array)
      return Construct_Tree_Iterator;
   --  Get an iterator pointing to a construct found at the specified position,
   --  or the nearest before or after the position (depends on the value of
   --  Position. Raises an exception if no construct if found. The scope of
   --  search can be reduce to a certain set of categories.

   function Get
     (This   : Text_Navigator_Abstr;
      Cursor : File_Cursor'Class;
      Len    : Natural) return String;
   --  Get Len characters from the file and the position specified by the
   --  cursor.

   function Get
     (This   : Text_Navigator_Abstr;
      Cursor : File_Cursor'Class) return Character;
   --  Get a caracter at the position specified by the cursor.

   function Get
     (This        : Text_Navigator_Abstr;
      Start, Stop : File_Cursor'Class) return String;
   --  Return the text extracted between the two cursors. We assume here that
   --  the two cursors are on the same file.

   function Get_Line
     (This      : Text_Navigator_Abstr;
      Cursor    : File_Cursor'Class;
      Start_Col : Column_Index := 0) return String;
   --  Get all character from the file and the column specified by the cursor
   --  to the end of the line, or beginning by Start_Col if not 0.
   --  The String resulting must have parameter 'First equal to Cursor.Col.

   function Read_File
     (This      : Text_Navigator_Abstr;
      File_Name : VFS.Virtual_File) return GNAT.Strings.String_Access;
   --  Get the entire file File_Name.

   procedure Replace
     (This      : in out Text_Navigator_Abstr;
      Cursor    : File_Cursor'Class;
      Len       : Natural;
      New_Value : String);
   --  Replace the Len characters, from the position designed by the cursor, by
   --  New_Value.

   procedure Add_Line
     (This     : in out Text_Navigator_Abstr;
      Cursor   : File_Cursor'Class;
      New_Line : String;
      Indent   : Boolean := False);
   --  Add a line AFTER the line specified by the cursor. To add a line at the
   --  begining of the text, set cursor line = 0.

   procedure Delete_Line
     (This : in out Text_Navigator_Abstr;
      Cursor : File_Cursor'Class);
   --  Delete the line where the cursor is.

   function Line_Length
     (This   : Text_Navigator_Abstr;
      Cursor : File_Cursor'Class) return Natural;
   --  Return le length of a line from the position of the cursor.

   function Search_Token
     (This     : Text_Navigator_Abstr'Class;
      Cursor   : File_Cursor'Class;
      Searched : Token_Record;
      Step     : Step_Way := Normal_Step) return File_Cursor'Class;
   --  Search a string in the text and returns a cursor at the beginning. If
   --  noting is found, then the cursor is Null_Cursor.

   function Search_Tokens
     (This     : Text_Navigator_Abstr'Class;
      Cursor   : File_Cursor'Class;
      Searched : Token_List;
      Step     : Step_Way := Normal_Step) return Word_Cursor'Class;
   --  Search a string in the text and returns a cursor at the beginning. The
   --  position of the first matching string is returned. If noting is found,
   --  then the cursor is Null_Cursor.

   function Search_Unit
     (This      : Text_Navigator_Abstr'Class;
      File_Name : VFS.Virtual_File;
      Category  : Language_Category;
      Name      : String := "") return Simple_Construct_Information;
   --  Return the first Contruct_Information that matche Category and name.
   --  If not found, return a Contruct_Information with Category = Cat_Unknown.
   --  If Name is "", then the first unit with the rigth Category will be
   --  returned.

   function Line_Max
     (This      : Text_Navigator_Abstr'Class;
      File_Name : VFS.Virtual_File) return Natural;
   --  Return the number of the last line in the text loaded.

   function Get_Full_Prefix
     (This     : Text_Navigator_Abstr'Class;
      Cursor   : File_Cursor'Class;
      Category : Language_Category := Cat_Unknown)
      return String;
   --  Return the entire prefix of the first unit of category after the cursor.

   function Get_Right_Paren
     (Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class)
     return File_Cursor'Class;
   --  Return the right paren corresponding to the one in the cursor.

   procedure Get_Entity
     (Current_Text         : Text_Navigator_Abstr'Class;
      Cursor               : File_Cursor'Class;
      Spec_Begin, Spec_End : out File_Cursor'Class;
      Body_Begin, Body_End : out File_Cursor'Class);
   --  Find the unit speficied at the position of the cursor. If it is a
   --  spec, find also the body. If it isn't a spec, then only the body
   --  informations are initialized.

   procedure Next_Word
     (This   : Text_Navigator_Abstr'Class;
      Cursor : in out File_Cursor'Class;
      Word   : out GNAT.Strings.String_Access);
   --  Put Cursor after the next word, and set 'Word' to this value, knowing
   --  that a word is a succession of non-blanks characters.

   function Get_Structure
     (This      : Text_Navigator_Abstr'Class;
      File_Name : VFS.Virtual_File) return Construct_List_Access;
   --  Return the parsed strucutre of the text_interface.

   procedure Update_All
     (This : Text_Navigator_Abstr'Class);
   --  This function update all the text contained in This.

   function Previous_Char
     (This : Text_Navigator_Abstr'Class; Cursor : File_Cursor'Class)
     return File_Cursor'Class;
   --  Return a cursor positioned on the first non-blank character before the
   --  position specified by the cursor

   procedure Undo
     (This : Text_Navigator_Abstr'Class; File_Name : VFS.Virtual_File);
   --  Undo the last action from the File_Name.

   function Get_Structured_File
     (This : Text_Navigator_Abstr'Class; Cursor : File_Cursor'Class)
      return Structured_File_Access;
   --  Return the construct tree corresponding to the file pointed by the given
   --  cursor.

   procedure Parse_Entities
     (Lang     : access Language_Root'Class;
      This     : Text_Navigator_Abstr'Class;
      Callback : Codefix_Entity_Callback;
      Start    : File_Cursor'Class);
   --  Parse entities (as defined by Language_Entity) contained in buffer.
   --  For each match, call Callback. Stops at the end of Buffer or when
   --  callback returns True.

   ----------------------------------------------------------------------------
   --  type Extract_Line
   ----------------------------------------------------------------------------

   type Extract_Line is private;
   type Ptr_Extract_Line is access all Extract_Line;

   function "=" (Left, Right : Extract_Line) return Boolean;
   --  Return True if both Extract_Lines are containing similar lines.

   function "<" (Left, Right : Ptr_Extract_Line) return Boolean;
   --  Return True if Left is before Right.

   procedure Assign (This : in out Extract_Line; Value : Extract_Line);
   --  Initialize information of This with clones of Value.

   function Get_Context (This : Extract_Line) return Merge_Info;
   --  Return the context associated to an Extract_Line.

   procedure Set_Context (This : in out Extract_Line; Value : Merge_Info);
   --  Set the context associated to an Extract_Line.

   function Next (This : Ptr_Extract_Line) return Ptr_Extract_Line;
   --  If the Extract_Line is a component of a list (typically, a list
   --  contained in an extract), then Next returns the next entry of the list.

   function Next (This : Extract_Line) return Ptr_Extract_Line;
   --  If the Extract_Line is a component of a list (typically, a list
   --  contained in an extract), then Next returns the next entry of the list.

   function Get_String (This : Extract_Line) return String;
   --  Returns the string memorized in an Extract_Line.

   function Get_Cursor (This : Extract_Line) return File_Cursor'Class;
   --  Return the cursor memorized in an Extract_Line.

   procedure Set_Indentation (This : in out Extract_Line; Value : Boolean);
   --  Set wether we want to force identation on that line or not.

   procedure Commit
     (This         : in out Extract_Line;
      Current_Text : in out Text_Navigator_Abstr'Class;
      Offset_Line  : in out Integer);
   --  Upate changes of the Extract_Line in the representation of the text. The
   --  parameted Offset_Line counts the offset of inserted - delete lines of
   --  the current commit.

   procedure Free (This : in out Extract_Line);
   --  Free the memory associated to an Extract_Line, and if this line is in
   --  a list of line, free recursivly the whole list.

   procedure Free_Data (This : in out Extract_Line);
   --  Free the memory associated to an Extract line but do not free the lines
   --  that can be connected to.

   function Clone
     (This      : Extract_Line;
      Recursive : Boolean) return Extract_Line;
   --  Clone an Extract_Line. Recursive True means that all the lines of the
   --  extract that record this line are cloned.

   function Clone (This : Extract_Line) return Extract_Line;
   --  Same one as the previous but Recursive is consider as True.

   function Search_Token
     (This     : Extract_Line;
      Cursor   : File_Cursor'Class;
      Searched : Token_Record;
      Step     : Step_Way := Normal_Step) return File_Cursor'Class;
   --  Search a token in the text and returns a cursor at the beginning. If
   --  noting is found, then the cursor is Null_Cursor. If Cursor.Col = 0, then
   --  the scan in initialized from the end of the content.

   function Search_Tokens
     (This     : Extract_Line;
      Cursor   : File_Cursor'Class;
      Searched : Token_List;
      Step     : Step_Way := Normal_Step) return Word_Cursor'Class;
   --  Search a token in the text and returns a cursor at the beginning. First
   --  match is returned. If noting is found, then the cursor is Null_Cursor.
   --  If Cursor.Col = 0, then the scan in initialized from the end of the
   --  content.

   procedure Get_Line
     (This        : Text_Navigator_Abstr'Class;
      Cursor      : File_Cursor'Class;
      Destination : in out Extract_Line);
   --  Get a line from the position specified by the cursor.

   function Get_Old_Text
     (This         : Extract_Line;
      Current_Text : Text_Navigator_Abstr'Class) return String;
   --  Return the original content of the line.

   function Get_New_Text
     (This : Extract_Line;
      Detail : Boolean := True) return String;
   --  Return the current content of the line.

   function Get_New_Text_Length
     (This      : Extract_Line;
      Recursive : Boolean := False) return Natural;
   --  Return the length of the current text in the line.

   function Get_Old_Text_Length
     (This      : Extract_Line;
      Current_Text : Text_Navigator_Abstr'Class;
      Recursive : Boolean := False) return Natural;
   --  Return the length of the current text before modifications.

   procedure Extend_Before
     (This          : in out Ptr_Extract_Line;
      Prev          : in out Ptr_Extract_Line;
      Current_Text  : Text_Navigator_Abstr'Class;
      Size          : Natural);
   --  Add to the extract size lines before each beginning of paragraph
   --  recorded.

   procedure Extend_After
     (This          : in out Ptr_Extract_Line;
      Current_Text  : Text_Navigator_Abstr'Class;
      Size          : Natural);
   --  Add to the extract size lines after each beginning of paragraph
   --  recorded.

   procedure Replace
     (This       : in out Extract_Line;
      Start      : Column_Index;
      Len        : Natural;
      New_String : String);
   --  Replace 'len' characters from 'start' column with 'New_String'.

   procedure Replace_To_End
     (This  : in out Extract_Line;
      Start : Column_Index;
      Value : String);
   --  Replace by Value the characters from Start to the end of the line.

   procedure Set_Coloration (This : in out Extract_Line; Value : Boolean);
   --  Set the boolean used to know if the line has to be colored in the window
   --  or not.

   function Get_Coloration (This : Extract_Line) return Boolean;
   --  Return the boolean used to know if the line has to be colored in the
   --  window or not.

   function Get_Line
     (This : Ptr_Extract_Line; Cursor : File_Cursor'Class)
      return Ptr_Extract_Line;
   --  If the line is referenced into an extract, this function search the
   --  first line from this one which is at the position specified by the
   --  cursor.

   procedure Merge_Lines
     (Result              : out Extract_Line;
      Object_1, Object_2  : Extract_Line;
      Success             : out Boolean;
      Chronologic_Changes : Boolean);
   --  Merge the two lines in result. See declartion of Generic_Merge in
   --  Codefix.Merge_Utils for more details.

   function Get_Number_Actions (This : Extract_Line) return Natural;
   --  Return the number of actions that will be needed to commit the lines.

   ----------------------------------------------------------------------------
   --  type Extract
   ----------------------------------------------------------------------------

   type Extract is tagged private;
   --  An extract is a temporary object that contains a part of the source
   --  code, modified or not. The modifications made in an extract do not have
   --  any influence in the source code before the call of Update function.

   type Ptr_Extract is access all Extract'Class;

   procedure Free (This : in out Ptr_Extract);
   --  Free the pointer and the memory associated to it.

   procedure Remove
     (This, Prev : Ptr_Extract_Line; Container : in out Extract);
   --  Remove the line This from the Container

   procedure Add_Element
     (This, Previous, Element : Ptr_Extract_Line;
      Container               : in out Extract);
   --  Add a new line in the line list. The line is always disposed in order
   --  to preserve the internal order of the lines, not just at the end of the
   --  list. If a line that already exisis is tried to be added, it is just
   --  ignored.

   procedure Add_Element (This : in out Extract; Element : Ptr_Extract_Line);
   --  Add a new line in the line list. The line is always disposed in order
   --  to preserve the internal order of the lines, not just at the end of the
   --  list.

   procedure Add_Element (This : in out Extract; Element : Extract_Line);
   --  Add a new line in the line list. The line is always disposed in order
   --  to preserve the internal order of the lines, not just at the end of the
   --  list.

   procedure Set_Indentation (This : in out Extract; Value : Boolean);
   --  Set wether we want to force identation on the modified lines of the
   --  extract

   function Clone (This : Extract) return Extract;
   --  Duplicate all informations associated to an extract, specially
   --  information referenced in pools.

   procedure Assign (This : in out Extract'Class; Source : Extract'Class);
   --  Initiqlize all fields of This by clones from source.

   procedure Get
     (This        : Text_Navigator_Abstr'Class;
      Cursor      : File_Cursor'Class;
      Len         : Natural;
      Destination : in out Extract);
   --  Put un Destination Len characterts got from the position and the file
   --  specified by the cursor.

   procedure Get_Line
     (This        : Text_Navigator_Abstr'Class;
      Cursor      : File_Cursor'Class;
      Destination : in out Extract);
   --  Put in Destination a line from the position specified by the cursor to
   --  the end of the line.

   function Get_String (This : Extract; Position : Natural := 1) return String;
   --  Get the string of the line of an extract. Strings are ordonned in the
   --  order where they are recorded.

   procedure Replace
     (This          : in out Extract;
      Start         : Column_Index;
      Length        : Natural;
      Value         : String;
      Line_Number   : Natural := 1);
   --  Replace 'len' characters from 'start' column and 'line_number' line
   --  with 'Value'.

   procedure Replace
     (This   : in out Extract;
      Start  : File_Cursor'Class;
      Length : Natural;
      Value  : String);
   --  Replace 'len' characters from 'start' column with 'Value'.

   type Replace_Blanks_Policy is (Keep, One, None);

   procedure Replace
     (This                      : in out Extract;
      Dest_Start, Dest_Stop     : File_Cursor'Class;
      Source_Start, Source_Stop : File_Cursor'Class;
      Current_Text              : Text_Navigator_Abstr'Class;
      Blanks_Before             : Replace_Blanks_Policy := Keep;
      Blanks_After              : Replace_Blanks_Policy := Keep);
   --  Replace in this the text from Start to Stop by the one from Source_Start
   --  to Source_End. Note that This must have been previously initialised with
   --  lines from 'Dest_Start' to 'Dest_Stop'. If Blank_pos is None, blanks
   --  will be removed, if it's One, only one blank will be put.

   procedure Commit
     (This         : Extract;
      Current_Text : in out Text_Navigator_Abstr'Class);
   --  Upate changes of the Extract_Line in the representation of the text.

   procedure Replace_Word
     (This         : in out Extract;
      Cursor       : File_Cursor'Class;
      New_String   : String;
      Old_String   : String;
      Format_Old   : String_Mode := Text_Ascii);
   --  Replace a word by another in the extract.

   procedure Replace_Word
     (This         : in out Extract;
      Cursor       : File_Cursor'Class;
      New_String   : String;
      Old_Length   : Natural);
   --  Replace a word by another in the extract.

   procedure Add_Word
     (This   : in out Extract;
      Cursor : File_Cursor'Class;
      Word   : String);
   --  Add a word at the position specified by the cursor. Check if it needs
   --  a space before or after, and add it.

   function Get_Word_Length
     (This   : Extract;
      Cursor : File_Cursor'Class;
      Format : String) return Natural;
   --  Get the length of a word what begins at the position specified by the
   --  cursor.

   procedure Free (This : in out Extract);
   --  Free the memory associated to an Extract.

   procedure Free_Data (This : in out Extract'Class);
   --  Free the memory associated to an Extract.

   function Get_Line
     (This : Extract; Position : File_Cursor'Class) return Ptr_Extract_Line;
   --  Return the line with the number specified in the original text.

   function Get_Record
     (This : Extract; Number : Natural) return Ptr_Extract_Line;
   --  Return the line recorded at the position Number in the extract.

   function Get_Number_Lines (This : Extract) return Natural;
   --  Return the number of the lines in the extract.

   procedure Add_Line
     (This   : in out Extract;
      Cursor : File_Cursor'Class;
      Text   : String;
      Indent : Boolean := False);
   --  Add a line AFTER the line specified by the cursor.
   --  Make a cursor with 0 for the line number to add a line at the beginning
   --  of the file.

   procedure Delete_Line
     (This   : in out Extract;
      Cursor : File_Cursor'Class);
   --  Delete the line of the extract at the line number and in the file
   --  specified by the cursor.

   procedure Delete_All_Lines (This : in out Extract);
   --  Delete all the lines from the extract.

   procedure Get_Entity
     (This         : in out Extract;
      Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class);
   --  Add in the Extract lines of the Entity witch begins at the position
   --  specified by the cursor (if it is a spec, the body is also got).

   function Search_Token
     (This     : Extract;
      Searched : Token_Record;
      Cursor   : File_Cursor'Class := Null_File_Cursor;
      Step     : Step_Way := Normal_Step) return File_Cursor'Class;
   --  Search a string in the text and returns a cursor at the beginning. If
   --  noting is found, then the cursor is Null_Cursor. If Cursor is
   --  Null_File_Cursor,then the research will begin at the begenning of the
   --  extract if Step is Normal_Step or at the end of the extract if Step is
   --  Reverse_Step.

   function Get_Old_Text
     (This         : Extract;
      Current_Text : Text_Navigator_Abstr'Class;
      Lines_Before : Natural := 0;
      Lines_After  : Natural := 0) return String;
   --  Return the original text contained in the extract. EOL_Str is used to
   --  make a new line.

   function Get_New_Text
     (This         : Extract;
      Current_Text : Text_Navigator_Abstr'Class;
      Lines_Before : Natural := 0;
      Lines_After  : Natural := 0) return String;
   --  Return the current form of the text contained in the extract. EOL_Str
   --  is used to make a new line.

   function Get_New_Text (This : Extract) return String;
   --  Return the current form of the text contained in the extract. EOL_Str
   --  is used to make a new line.

   function Get_New_Text_Length (This : Extract) return Natural;
   --  Return the length of the current text in the extract.

   function Get_Old_Text_Length
     (This : Extract;
      Current_Text : Text_Navigator_Abstr'Class) return Natural;
   --  Return the length of the current text before modifications.

   function Get_First_Line (This : Extract) return Ptr_Extract_Line;
   --  Return the first line recored in an extract.

   function Get_Last_Line (This : Extract) return Ptr_Extract_Line;
   --  Return the last line recorded in an extract.

   procedure Extend_Before
     (This         : in out Extract;
      Current_Text : Text_Navigator_Abstr'Class;
      Size         : Natural);
   --  Add to the extract size lines before each beginning of paragraph
   --  recorded.

   procedure Extend_After
     (This         : in out Extract;
      Current_Text : Text_Navigator_Abstr'Class;
      Size         : Natural);
   --  Add to the extract size lines after each beginning of paragraph
   --  recorded.

   procedure Reduce
     (This                    : in out Extract;
      Size_Before, Size_After : Natural);
   --  Reduce the number of non-modified lines before and after each paragraph.

   procedure Erase
     (This        : in out Extract;
      Start, Stop : File_Cursor'Class);
   --  Erase the text from Start to Stop. If a line, after the deletion, is
   --  empty, then this line will be deleted.

   procedure Comment
     (This        : in out Extract;
      Start, Stop : File_Cursor'Class);
   --  Comment from Start to Stop on the given extract

   function Get_Files_Names
     (This     : Extract;
      Size_Max : Natural := 0) return String;
   --  Return a string containing all the files names of the extract separate
   --  by '/'. If Size_Max /= 0, then the String returned cannot be bigger than
   --  Size_Max + 3.

   function Get_Nb_Files (This : Extract) return Natural;
   --  Return the number of different files names contained in the extract.

   function Data (This : Ptr_Extract_Line) return Extract_Line;
   --  Return the object pointed by This.

   function Is_Null (This : Ptr_Extract_Line) return Boolean;
   --  Return True if This doesn't references any object.

   procedure Merge_Extracts
     (Result              : out Extract'Class;
      Object_1, Object_2  : Extract'Class;
      Success             : out Boolean;
      Chronologic_Changes : Boolean);
   --  Merge the two extracts in result. See declartion of Generic_Merge in
   --  Codefix.Merge_Utils for more details.

   procedure Delete_Empty_Lines (This : in out Extract);
   --  Delete all lines that are composed only by blanks characters.

   function Get_Number_Actions (This : Extract) return Natural;
   --  Returns the number of actions that have been made in the extract
   --  during his commit.

   procedure Undo (This : Extract; Current_Text : Text_Navigator_Abstr'Class);
   --  Undo each changes that have been made into the extract.

   ----------------------------------------------------------------------------
   --  type Text_Command
   ----------------------------------------------------------------------------

   procedure Make_Word_Mark
     (Word         : Word_Cursor;
      Current_Text : Text_Navigator_Abstr'Class;
      Mark         : out Word_Mark);
   --  Create a Word_Mark from information given by the cursor.

   procedure Make_Word_Cursor
     (Word         : Word_Mark;
      Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : out Word_Cursor);
   --  Create a Word_Cursor from information given by the mark.

   function Clone (This : Word_Cursor) return Word_Cursor;
   --  Duplicate all informations of a Word_Cursor, specially informations
   --  memorized in dynamic memory.

   ------------------
   -- Text_Command --
   ------------------

   type Text_Command is abstract tagged private;
   --  A Text_Command is a modification in the text that can be defined one
   --  time, and made later, with taking into account others possible changes.

   type Ptr_Command is access all Text_Command'Class;

   procedure Execute
     (This         : Text_Command;
      Current_Text : Text_Navigator_Abstr'Class;
      New_Extract  : out Extract'Class) is abstract;
   --  Execute a command, and create an extract to preview the changes. This
   --  procedure raises a Codefix_Panic is the correction is no longer avaible.

   type Execute_Corrupted_Record is abstract tagged null record;

   type Execute_Corrupted is access all Execute_Corrupted_Record'Class;

   procedure Error
     (Corruption : access Execute_Corrupted_Record; Error_Message : String)
   is abstract;

   procedure Free (Corruption : in out Execute_Corrupted);

   procedure Secured_Execute
     (This         : Text_Command'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      New_Extract  : out Extract'Class;
      Error_Cb     : Execute_Corrupted := null);
   --  Same as the previous one, but when problems happend no exception is
   --  raised but Error_Cb is called. This function also
   --  updates the current text, in order to be conformant with user's changes,

   procedure Free (This : in out Text_Command);
   --  Free the memory associated to a Text_Command.

   procedure Free_Data (This : in out Text_Command'Class);
   --  Free the memory associated to a Text_Command.

   procedure Set_Caption
     (This : in out Text_Command'Class;
      Caption : String);
   --  Define the caption that describes the action of a Text_Command.

   function Get_Caption (This : Text_Command'Class) return String;
   --  Return the caption associated to a Text_Command.

   ---------------------
   -- Remove_Word_Cmd --
   ---------------------

   type Remove_Word_Cmd is new Text_Command with private;

   procedure Initialize
     (This         : in out Remove_Word_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Word         : Word_Cursor'Class);
   --  Set all the marks that will be necessary later to remove the word.

   procedure Free (This : in out Remove_Word_Cmd);
   --  Free the memory associated to a Remove_Word_Cmd.

   procedure Execute
     (This         : Remove_Word_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      New_Extract  : out Extract'Class);
   --  Set an extract with the word removed.

   ---------------------
   -- Insert_Word_Cmd --
   ---------------------

   type Insert_Word_Cmd is new Text_Command with private;

   procedure Initialize
     (This            : in out Insert_Word_Cmd;
      Current_Text    : Text_Navigator_Abstr'Class;
      Word            : Word_Cursor'Class;
      New_Position    : File_Cursor'Class;
      Add_Spaces      : Boolean := True;
      Position        : Relative_Position := Specified;
      Insert_New_Line : Boolean := False);
   --  Set all the marks that will be necessary later to insert the word.

   procedure Free (This : in out Insert_Word_Cmd);
   --  Fre the memory associated to an Insert_Word_Cmd.

   procedure Execute
     (This         : Insert_Word_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      New_Extract  : out Extract'Class);
   --  Set an extract with the word inserted.

   --------------------
   -- Move_Word_Cmd  --
   --------------------

   type Move_Word_Cmd is new Text_Command with private;

   procedure Initialize
     (This            : in out Move_Word_Cmd;
      Current_Text    : Text_Navigator_Abstr'Class;
      Word            : Word_Cursor'Class;
      New_Position    : File_Cursor'Class;
      Insert_New_Line : Boolean := False);
   --  Set all the marks that will be needed to move the word later.

   procedure Free (This : in out Move_Word_Cmd);
   --  Free the memory associated to a Move_Word_Cmd.

   procedure Execute
     (This         : Move_Word_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      New_Extract  : out Extract'Class);
   --  Set an extract with the word moved.

   ----------------------
   -- Replace_Word_Cmd --
   ----------------------

   type Replace_Word_Cmd is new Text_Command with private;

   procedure Initialize
     (This           : in out Replace_Word_Cmd;
      Current_Text   : Text_Navigator_Abstr'Class;
      Word           : Word_Cursor'Class;
      New_Word       : String;
      Do_Indentation : Boolean := False);
   --  Set all the marks that will be needed to replace the word later.

   procedure Free (This : in out Replace_Word_Cmd);
   --  Free the memory associated to a Replace_Word_Cmd.

   procedure Execute
     (This         : Replace_Word_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      New_Extract  : out Extract'Class);
   --  Set an extract with the word replaced.

   ----------------------
   -- Invert_Words_Cmd --
   ----------------------

   type Invert_Words_Cmd is new Text_Command with private;

   procedure Initialize
     (This         : in out Invert_Words_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Word1, Word2 : Word_Cursor'Class);
   --  Set all the marks that will be needed to invert the two words later,

   procedure Free (This : in out Invert_Words_Cmd);
   --  Free the memory associated to an Invert_Word_Cmd.

   procedure Execute
     (This         : Invert_Words_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      New_Extract  : out Extract'Class);
   --  Set an extract with the invertion of the two word.

   ------------------
   -- Add_Line_Cmd --
   ------------------

   type Add_Line_Cmd is new Text_Command with private;

   procedure Initialize
     (This         : in out Add_Line_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Position     : File_Cursor'Class;
      Line         : String);
   --  Set all the marks that will be needed to add the line later.

   procedure Free (This : in out Add_Line_Cmd);
   --  Free the memory associated to an Add_Line_Cmd.

   procedure Execute
     (This         : Add_Line_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      New_Extract  : out Extract'Class);
   --  Set an extract with the invertion add of the line.

   -----------------------
   -- Replace_Slice_Cmd --
   -----------------------

   type Replace_Slice_Cmd is new Text_Command with private;

   procedure Initialize
     (This                     : in out Replace_Slice_Cmd;
      Current_Text             : Text_Navigator_Abstr'Class;
      Start_Cursor, End_Cursor : File_Cursor'Class;
      New_Text                 : String);
   --  Set all the marks that will be necessary later to remove the slice.

   procedure Free (This : in out Replace_Slice_Cmd);
   --  Free the memory associated to a Remove_Sloce_Cmd.

   procedure Execute
     (This         : Replace_Slice_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      New_Extract  : out Extract'Class);
   --  Set an extract with the slice removed.

private

   function Compare_Last (Str_1, Str_2 : String) return Boolean;
   --  Return true when one of the two strings is equal to the last characters
   --  of the other one.

   ----------------------------------------------------------------------------
   --  type Escape_Str_Manager
   ----------------------------------------------------------------------------

   type Escape_Str_Manager is abstract tagged record
      null;
   end record;

   ----------------------------------------------------------------------------
   --  type Text_Navigator
   ----------------------------------------------------------------------------

   package Text_List is new Generic_List (Ptr_Text);
   use Text_List;

   type Ptr_List_Text is access Text_List.List;

   procedure Free
     is new Ada.Unchecked_Deallocation (Text_List.List, Ptr_List_Text);

   type Text_Navigator_Abstr is abstract tagged record
      Files        : Ptr_List_Text := new Text_List.List;
      Registry     : Projects.Registry.Project_Registry_Access;
      Construct_Db : Construct_Database_Access;
   end record;

   function Get_File
     (This    : Text_Navigator_Abstr'Class;
      Name    : VFS.Virtual_File) return Ptr_Text;
   --  Returns the existent file interface, or create a new one if it doesn't
   --  exists.

   type Mark_Abstr is abstract tagged record
      Is_First_Line : Boolean := False;
      File_Name     : VFS.Virtual_File;
   end record;

   ----------------------------------------------------------------------------
   --  type Text_Interface
   ----------------------------------------------------------------------------

   procedure Free is new Ada.Unchecked_Deallocation
     (Construct_List, Construct_List_Access);

   type Ptr_Boolean is access all Boolean;
   procedure Free is new Ada.Unchecked_Deallocation (Boolean, Ptr_Boolean);

   type Text_Interface is abstract tagged record
      Structure            : Construct_List_Access := new Construct_List;
      Construct_File       : Structured_File_Access := null;
      Buffer               : GNAT.Strings.String_Access := null;
      File_Name            : VFS.Virtual_File;
      Structure_Up_To_Date : Ptr_Boolean := new Boolean'(False);
      Construct_Db         : Construct_Database_Access;
   end record;

   function Get_Iterator_At
     (Current_Text      : access Text_Interface;
      Cursor            : Text_Cursor'Class;
      Position          : Relative_Position := Specified;
      Categories_Seeked : Category_Array := Null_Category_Array)
      return Construct_Tree_Iterator;
   --  Get an iterator pointing to a construct found at the specified position,
   --  or the nearest before or after the position (depends on the value of
   --  Position. Raises an exception if no construct if found. The scope of
   --  search can be reduce to a certain set of categories.

   procedure Update_Structure_If_Needed (This : access Text_Interface'Class);
   --  Update the constructs and the tree store if there have been changes
   --  since the last computation

   ----------------------------------------------------------------------------
   --  type Extract_Line
   ----------------------------------------------------------------------------

   type Extract_Line is record
      Context         : Merge_Info := Original_Unit;

      Cursor          : File_Cursor;
      --  This cursor specify the position where the line begins.

      Original_Length : Natural := 0;
      Content         : Mergable_String;
      Next            : Ptr_Extract_Line;
      Coloration      : Boolean := False;

      Do_Indentation  : Boolean := False;
   end record;

   procedure Get
     (This        : Text_Interface'Class;
      Cursor      : File_Cursor'Class;
      Len         : Natural;
      Destination : in out Extract_Line);

   procedure Get_Line
     (This        : Text_Interface'Class;
      Cursor      : File_Cursor'Class;
      Destination : in out Extract_Line);

   procedure Free (Line : in out Ptr_Extract_Line);
   --  Free the memory associated with the line

   ----------------------------------------------------------------------------
   --  type Extract
   ----------------------------------------------------------------------------

   type Extract is tagged record
      First : Ptr_Extract_Line;
   end record;

   function Get_Word_Length
     (This   : Extract_Line;
      Col    : Column_Index;
      Format : String)
     return Natural;

   function Length (This : Extract_Line) return Natural;

   function Previous (Container : Extract; Node : Ptr_Extract_Line)
     return Ptr_Extract_Line;

   ----------------------------------------------------------------------------
   --  type Text_Command
   ----------------------------------------------------------------------------

   type Text_Command is abstract tagged record
      Caption      : GNAT.Strings.String_Access;
   end record;

   type Remove_Word_Cmd is new Text_Command with record
      Word : Word_Mark;
   end record;

   type Insert_Word_Cmd is new Text_Command with record
      Word            : Word_Mark;
      Add_Spaces      : Boolean := True;
      Position        : Relative_Position := Specified;
      New_Position    : Word_Mark;
      Insert_New_Line : Boolean := False;
   end record;

   type Move_Word_Cmd is new Text_Command with record
      Step_Remove : Remove_Word_Cmd;
      Step_Insert : Insert_Word_Cmd;
   end record;

   type Replace_Word_Cmd is new Text_Command with record
      Mark           : Word_Mark;
      Str_Expected   : GNAT.Strings.String_Access;
      Do_Indentation : Boolean := False;
   end record;

   type Invert_Words_Cmd is new Text_Command with record
      Step_Word1 : Replace_Word_Cmd;
      Step_Word2 : Replace_Word_Cmd;
   end record;

   type Add_Line_Cmd is new Text_Command with record
      Line     : GNAT.Strings.String_Access;
      Position : Ptr_Mark;
   end record;

   type Replace_Slice_Cmd is new Text_Command with record
      Start_Mark : Ptr_Mark;
      End_Mark   : Ptr_Mark;
      New_Text   : GNAT.Strings.String_Access;
   end record;

   ----------------------------------------------------------------------------
   --  Others
   ----------------------------------------------------------------------------

   type Text_Cursor is tagged record
      Line : Natural := 0;
      --  If Line = 0, indicates a special case to handle the first line
      --  differently. ??? Not quite clear why it is needed to handle it
      --  specially.

      Col  : Column_Index := 0;
      --  The reason why we store columns rather than char index is that
      --  converting a column index into a char index may be more expensive.
      --  When you convert from char to column, you usually already have the
      --  string to be processed, which is less frequently the case when you
      --  have a column (because it might come from a GNAT message for
      --  instance).
   end record;

   type File_Cursor is new Text_Cursor with record
      File : VFS.Virtual_File;
   end record;

   Null_File_Cursor : constant File_Cursor := (0, 0, VFS.No_File);

   type Word_Cursor is new File_Cursor with record
      String_Match : GNAT.Strings.String_Access;
      Mode         : String_Mode := Text_Ascii;
   end record;

   Null_Word_Cursor : constant Word_Cursor :=
     (Null_File_Cursor with null, Text_Ascii);

   function Normalize (Str : GNAT.Strings.String_Access) return String;
   --  Change the string in order to make comparaisons between lists of
   --  parameters.

end Codefix.Text_Manager;
