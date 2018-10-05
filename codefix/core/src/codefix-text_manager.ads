------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2002-2018, AdaCore                     --
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

with Language;               use Language;
with Language.Tree;          use Language.Tree;
with Language.Tree.Database; use Language.Tree.Database;
with GNATCOLL.VFS;           use GNATCOLL.VFS;
with Projects;
with Refactoring;            use Refactoring;

private with GPS_Vectors;

package Codefix.Text_Manager is

   type Step_Way is (Normal_Step, Reverse_Step);
   type Case_Type is (Lower, Upper, Mixed);

   function Is_Blank (Str : String) return Boolean;
   --  Return true if Str is only composed by white characters

   function Is_Blank (Char : Character) return Boolean;
   --  Return true if Str is a white characters

   function Without_Last_Blanks (Str : String) return String;
   --  Return the string Str without the last blanks characters

   function Is_Separator (Char : Character) return Boolean;
   --  Return True if the character can be considerate as a separator

   ----------------------------------------------------------------------------
   --  Early type declarations
   ----------------------------------------------------------------------------

   type Text_Navigator_Abstr is abstract tagged private;

   ----------------------------------------------------------------------------
   --  type Text_Cursor
   ----------------------------------------------------------------------------

   type Text_Cursor is tagged private;

   function "<" (Left, Right : Text_Cursor) return Boolean;
   --  Return True when Left is before Right

   function ">" (Left, Right : Text_Cursor'Class) return Boolean;
   --  Return True when Left is after Right

   function "<=" (Left, Right : Text_Cursor'Class) return Boolean;
   --  Return True when Left is before or in the same position than Right

   function ">=" (Left, Right : Text_Cursor'Class) return Boolean;
   --  Return True when Left is after or in the same position than Right

   procedure Set_Location
     (This : in out Text_Cursor; Line : Natural; Column : Visible_Column_Type);
   --  Set the location information

   function Get_Line (This : Text_Cursor) return Integer;
   function Get_Column (This : Text_Cursor) return Visible_Column_Type;
   --  Return the location

   procedure Set_Line (This : in out Text_Cursor; Line : Natural);
   procedure Set_Column
     (This : in out Text_Cursor; Column : Visible_Column_Type);
   --  Set the line or column

   type File_Cursor is new Text_Cursor with private;
   Null_File_Cursor : constant File_Cursor;

   procedure Set_File
     (This : in out File_Cursor; File : GNATCOLL.VFS.Virtual_File);
   --  Set the file information

   function Get_File (This : File_Cursor) return GNATCOLL.VFS.Virtual_File;
   --  Return the file associated with the cursor

   overriding function "=" (Left, Right : File_Cursor) return Boolean;
   --  Return true when Left is in the same position than rigth

   overriding function "<" (Left, Right : File_Cursor) return Boolean;
   --  Return True when Left is before Right

   procedure Free (This : in out File_Cursor);
   --  Frees the memory used by fields of File_Cursor.
   --  Does nothing, just needed for generic instantiation.

   function Clone (This : File_Cursor) return File_Cursor;
   --  Duplicate all informations of a File_Cursor, specially informations
   --  memorized in dynamic memory.

   procedure Assign
     (This : in out File_Cursor'Class; Source : File_Cursor'Class);

   type Mark_Abstr is abstract tagged private;
   type Ptr_Mark is access all Mark_Abstr'Class;

   procedure Free (This : in out Mark_Abstr);
   --  Free the memory associated to a Mark_Abstr

   procedure Free_Data (This : in out Mark_Abstr'Class);
   --  Free the memory associated to a Mark_Abstr

   procedure Free (This : in out Ptr_Mark);
   --  Free the memory associated to a Ptr_Mark

   function Get_File (This : Mark_Abstr'Class) return Virtual_File;
   --  return the file where this mark is placed.

   type Word_Cursor is new File_Cursor with private;
   --  Word_cursor is an object that describes a specific word in the text. In
   --  case where it is used to match a word in the text, the mode can be
   --  'Regular_Expression'. Otherwise, this field is ignored.

   Null_Word_Cursor : constant Word_Cursor;

   type String_Mode is (Text_Ascii, Regular_Expression);

   type Word_Mark is record
      Mark_Id      : Ptr_Mark;
      String_Match : Unbounded_String;
      Mode         : String_Mode := Text_Ascii;
   end record;

   procedure Set_Word
     (Word         : in out Word_Cursor;
      String_Match : Unbounded_String;
      Mode         : String_Mode := Text_Ascii);
   --  Change the attributes of Word.
   --  No String_Match is registered if an empty string is passed.

   function Get_Word (Word : Word_Cursor) return String;
   --  Return the word currently stored by the cursor, "" if none

   function Get_Matching_Word
     (Word  : Word_Cursor;
      Text  : Text_Navigator_Abstr'Class;
      Check : Boolean := False) return String;
   --  Return the word matching the cursor given in parameter, if the mode
   --  of the word is regular expression. Otherwise, just return the word.
   --  If Check is True, then check the presence of Word in Text even for plain
   --  text.

   procedure Free (This : in out Word_Mark);
   --  Free the memory associated to a Word_Mark

   overriding procedure Free (This : in out Word_Cursor);
   --  Free the memory associated to a Word_Cursor

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
   --  Create a new mark at the position specified by the cursor

   function Get_Current_Cursor
     (Current_Text : Text_Interface;
      Mark         : Mark_Abstr'Class) return File_Cursor'Class is abstract;
   --  Return the current position of the mark

   procedure Free (This : in out Ptr_Text);
   --  Free the memory associated to Ptr_Text and the object referenced

   procedure Initialize
     (This      : in out Text_Interface;
      File_Name : GNATCOLL.VFS.Virtual_File);
   --  Initialize the structure of the Text_Interface

   procedure Free (This : in out Text_Interface);
   --  Free the memory associated to the Text_Interface

   function Get
     (This   : Text_Interface;
      Cursor : Text_Cursor'Class;
      Len    : Natural) return String is abstract;
   --  Get Len characters from the position specified by the cursor. The
   --  String resulting must have parameter 'First equal to Cursor.Col.

   function Get
     (This        : Text_Interface;
      Start, Stop : Text_Cursor'Class) return String;
   --  Return the text extracted between the two cursors

   function Get
     (This   : Text_Interface;
      Cursor : Text_Cursor'Class) return Character is abstract;
   --  Get the characters from the position specified by the cursor

   function Get_Line
     (This      : Text_Interface;
      Cursor    : Text_Cursor'Class;
      Start_Col : Visible_Column_Type := 0) return String is abstract;
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

   procedure Replace
     (This         : in out Text_Interface;
      Start_Cursor : Text_Cursor'Class;
      End_Cursor   : Text_Cursor'Class;
      New_Value    : String) is abstract;
   --  Replace the characters between Start_Cursor and End_Cursor by New_Value.

   procedure Add_Line
     (This     : in out Text_Interface;
      Cursor   : Text_Cursor'Class;
      New_Line : String;
      Indent   : Boolean := False) is abstract;
   --  Add a line AFTER the line specified by the cursor. To add a line at the
   --  begining of the text, set cursor line = 0. If Indent is true then the
   --  new line will get automatically indented.

   procedure Delete_Line
     (This : in out Text_Interface;
      Cursor : Text_Cursor'Class) is abstract;
   --  Delete the line where the cursor is

   procedure Indent_Line
     (This   : in out Text_Interface;
      Cursor : Text_Cursor'Class) is abstract;
   --  Indent the line pointed by the cursor

   procedure Remove_Empty_Lines
     (This : in out Text_Interface'Class;
      Start_Cursor : Text_Cursor'Class;
      End_Cursor   : Text_Cursor'Class);
   --  Removes all the empty lines that are located between the two cursors

   function Line_Length
     (This   : Text_Interface'Class;
      Cursor : Text_Cursor'Class) return Natural;
   --  Returns le length of a line from the position of the cursor

   function Read_File
     (This : Text_Interface) return Unbounded_String is abstract;
   --  Get the entire file in a String_Access

   function Get_File_Name
     (This : Text_Interface) return GNATCOLL.VFS.Virtual_File;
   --  Return the name of the file

   type Token_Record is record
      Name : Unbounded_String;
      Kind : Language_Entity;
   end record;

   type Token_List is array (Integer range <>) of Token_Record;

   Open_Paren_Tok : constant Token_Record :=
     (Kind => Operator_Text, Name => To_Unbounded_String ("("));
   Close_Paren_Tok : constant Token_Record :=
     (Kind => Operator_Text, Name => To_Unbounded_String (")"));
   Colon_Tok : constant Token_Record :=
     (Kind => Operator_Text, Name => To_Unbounded_String (":"));
   Semicolon_Tok : constant Token_Record :=
     (Kind => Operator_Text, Name => To_Unbounded_String (";"));
   Tick_Tok : constant Token_Record :=
     (Kind => Operator_Text, Name => To_Unbounded_String ("'"));
   Equals_Tok : constant Token_Record :=
     (Kind => Operator_Text, Name => To_Unbounded_String ("="));
   Not_Equals_Tok : constant Token_Record :=
     (Kind => Operator_Text, Name => To_Unbounded_String ("/="));
   Aliased_Tok : constant Token_Record :=
     (Kind => Keyword_Text, Name => To_Unbounded_String ("aliased"));
   Renames_Tok : constant Token_Record :=
     (Kind => Keyword_Text, Name => To_Unbounded_String ("renames"));
   Is_Tok : constant Token_Record :=
     (Kind => Keyword_Text, Name => To_Unbounded_String ("is"));
   Overriding_Tok : constant Token_Record :=
     (Kind => Keyword_Text, Name => To_Unbounded_String ("overriding"));
   True_Tok : constant Token_Record :=
     (Kind => Identifier_Text, Name => To_Unbounded_String ("true"));

   function Search_Token
     (This     : Text_Interface'Class;
      Cursor   : Text_Cursor'Class;
      Searched : Token_Record;
      Step     : Step_Way := Normal_Step) return Word_Cursor'Class;
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
   --
   --  ??? This subprogram is not correctly implemented and does a had hoc
   --  analysis while we now have all the tools to do better. Remove its usage.

   function Get_Full_Prefix
     (This     : access Text_Interface'Class;
      Cursor   : Text_Cursor'Class;
      Category : Language_Category := Cat_Unknown)
      return String;
   --  Return the entire prefix of the first unit of category after the cursor

   procedure Next_Word
     (This   : Text_Interface'Class;
      Cursor : in out Text_Cursor'Class;
      Word   : out Word_Cursor);
   --  Put Cursor after the next word, and set 'Word' to this value, knowing
   --  that a word is a succession of non-blanks characters. potentially
   --  stopped by a separator (see Is_Separator_Char). Separators are words
   --  on their own.

   function Get_Structured_File
     (This : access Text_Interface'Class) return Structured_File_Access;
   --  Return the tree associated to this text

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

   procedure Parse_Entities_Backwards
     (Lang     : access Language_Root'Class;
      This     : in out Text_Interface'Class;
      Callback : access procedure (Buffer : Unbounded_String;
                                   Token  : Language.Token_Record;
                                   Stop   : in out Boolean);
      Start    : File_Cursor'Class);
   --  Parse entities in reverse order, as defined by the package Language.

   procedure Erase
     (This            : in out Text_Interface'Class;
      Start, Stop     : File_Cursor'Class);
   --  Erase the text from Start to Stop. If a line, after the deletion, is
   --  empty, then this line will be deleted. If Remove_If_Blank is true, the
   --  remaining line will get removed if it contains only blank characters.

   procedure Comment
     (This        : in out Text_Interface'Class;
      Start, Stop : File_Cursor'Class);
   --  Comment from Start to Stop on the given extract

   ----------------------------------------------------------------------------
   --  type Text_Navigator
   ----------------------------------------------------------------------------

   type Ptr_Text_Navigator is access all Text_Navigator_Abstr'Class;

   procedure Free (This : in out Ptr_Text_Navigator);

   procedure Set_Registry
     (Text     : in out Text_Navigator_Abstr;
      Registry : Projects.Project_Registry_Access);

   function Get_Registry
     (Text : Text_Navigator_Abstr)
      return Projects.Project_Registry_Access;

   procedure Set_Construct_Database
     (Text : in out Text_Navigator_Abstr;
      Db   : Construct_Database_Access);

   function Get_Construct_Database
     (Text : Text_Navigator_Abstr) return Construct_Database_Access;

   procedure Set_Context
     (Text    : in out Text_Navigator_Abstr;
      Context : Factory_Context);

   function Get_Context
     (Text : Text_Navigator_Abstr)
      return Factory_Context;

   function Get_Body_Or_Spec
     (Text : Text_Navigator_Abstr; File_Name : GNATCOLL.VFS.Virtual_File)
      return GNATCOLL.VFS.Virtual_File;
   --  When File_Name is a spec file, this function returns the body
   --  corresponding, otherwise it returns the spec.

   function New_Text_Interface (This : Text_Navigator_Abstr)
     return Ptr_Text is abstract;
   --  Create and initialise a new Text_Interface used by the text navigator

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
   --  Return the current position of the mark

   procedure Clean (This : in out Text_Navigator_Abstr);
   --  Clean the temporary memory used by this Text_Navigator, in particular
   --  all the text interfaces. The Text_Navigator can still be used after
   --  that.

   procedure Free (This : in out Text_Navigator_Abstr);
   --  Free the memory associated to a Text_Navigator

   function Get_Iterator_At
     (Current_Text      : Text_Navigator_Abstr;
      Cursor            : File_Cursor'Class;
      From_Type         : Position_Type := Start_Name;
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
   --  Get a caracter at the position specified by the cursor

   function Get
     (This        : Text_Navigator_Abstr;
      Start, Stop : File_Cursor'Class) return String;
   --  Return the text extracted between the two cursors. We assume here that
   --  the two cursors are on the same file.

   function Get_Line
     (This      : Text_Navigator_Abstr;
      Cursor    : File_Cursor'Class;
      Start_Col : Visible_Column_Type := 0) return String;
   --  Get all character from the file and the column specified by the cursor
   --  to the end of the line, or beginning by Start_Col if not 0.
   --  The String resulting must have parameter 'First equal to Cursor.Col.

   function Read_File
     (This      : Text_Navigator_Abstr;
      File_Name : GNATCOLL.VFS.Virtual_File) return Unbounded_String;
   --  Get the entire file File_Name. Result must be freed by the caller.

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
   --  Delete the line where the cursor is

   function Line_Length
     (This   : Text_Navigator_Abstr;
      Cursor : File_Cursor'Class) return Natural;
   --  Return le length of a line from the position of the cursor

   function Search_Token
     (This     : Text_Navigator_Abstr'Class;
      Cursor   : File_Cursor'Class;
      Searched : Token_Record;
      Step     : Step_Way := Normal_Step)
      return Word_Cursor'Class;
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
      File_Name : GNATCOLL.VFS.Virtual_File;
      Category  : Language_Category;
      Name      : String := "") return Simple_Construct_Information;
   --  Return the first Contruct_Information that matche Category and name.
   --  If not found, return a Contruct_Information with Category = Cat_Unknown.
   --  If Name is "", then the first unit with the rigth Category will be
   --  returned.

   function Line_Max
     (This      : Text_Navigator_Abstr'Class;
      File_Name : GNATCOLL.VFS.Virtual_File) return Natural;
   --  Return the number of the last line in the text loaded

   function Get_Full_Prefix
     (This     : Text_Navigator_Abstr'Class;
      Cursor   : File_Cursor'Class;
      Category : Language_Category := Cat_Unknown)
      return String;
   --  Return the entire prefix of the first unit of category after the cursor

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
      Word   : out Word_Cursor);
   --  Put Cursor after the next word, and set 'Word' to this value, knowing
   --  that a word is a succession of non-blanks characters.

   procedure Update_All
     (This : Text_Navigator_Abstr'Class);
   --  This function update all the text contained in This

   function Previous_Char
     (This : Text_Navigator_Abstr'Class; Cursor : File_Cursor'Class)
     return File_Cursor'Class;
   --  Return a cursor positioned on the first non-blank character before the
   --  position specified by the cursor.

   procedure Undo
     (This      : Text_Navigator_Abstr'Class;
      File_Name : GNATCOLL.VFS.Virtual_File);
   --  Undo the last action from the File_Name

   function Get_Structured_File
     (This : Text_Navigator_Abstr'Class; File : Virtual_File)
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

   procedure Parse_Entities_Backwards
     (Lang     : access Language_Root'Class;
      This     : Text_Navigator_Abstr'Class;
      Callback : access procedure (Buffer : Unbounded_String;
                                   Token  : Language.Token_Record;
                                   Stop   : in out Boolean);
      Start    : File_Cursor'Class);
   --  Parse entities in reverse order, as defined by the package Language.

   type Replace_Blanks_Policy is (Keep, One, None);

   procedure Replace
     (This          : in out Text_Navigator_Abstr'Class;
      Position      : File_Cursor'Class;
      Len           : Integer;
      New_Text      : String;
      Blanks_Before : Replace_Blanks_Policy;
      Blanks_After  : Replace_Blanks_Policy);
   procedure Replace
     (This                  : in out Text_Navigator_Abstr'Class;
      Dest_Start, Dest_Stop : in out File_Cursor'Class;
      New_Text              : String;
      Blanks_Before         : Replace_Blanks_Policy := Keep;
      Blanks_After          : Replace_Blanks_Policy := Keep);
   --  Replace in this the text from Start to Stop by the one from Source_Start
   --  to Source_End. If Blank_pos is None, blanks will be removed, if it's
   --  One, only one blank will be put. At the end of the process, Dest_Start
   --  and Dest_Stop are updated with the new positions. If Dest_Stop is a
   --  Word_Cursor, then the whole word will be modified.

   ----------------------------------------------------------------------------
   --  type Text_Command
   ----------------------------------------------------------------------------

   procedure Make_Word_Mark
     (Word         : Word_Cursor;
      Current_Text : Text_Navigator_Abstr'Class;
      Mark         : out Word_Mark);
   --  Create a Word_Mark from information given by the cursor

   procedure Make_Word_Cursor
     (Word         : Word_Mark;
      Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : out Word_Cursor);
   --  Create a Word_Cursor from information given by the mark

   overriding function Clone (This : Word_Cursor) return Word_Cursor;
   --  Duplicate all informations of a Word_Cursor, specially informations
   --  memorized in dynamic memory.

   ----------------------------------------------------------------------------
   --  type Text_Command
   ----------------------------------------------------------------------------

   ------------------
   -- Text_Command --
   ------------------

   type Fix_Complexity is (Simple, Complex);

   type Text_Command
     (Complexity : Fix_Complexity) is abstract tagged private;
   --  A Text_Command is a modification in the text that can be defined one
   --  time, and made later, with taking into account others possible changes.

   type Ptr_Command is access all Text_Command'Class;

   procedure Execute
     (This         : Text_Command;
      Current_Text : in out Text_Navigator_Abstr'Class) is null;
   --  New version of Execute. Reset success to True if the command is in the
   --  new kind, false if the old execute has still to be called.

   function Is_Writable (This : Text_Command) return Boolean is abstract;
   --  This primitive returns true if the text command passed in parameter
   --  will can write on all the needed files, false otherwise.

   type Execute_Corrupted_Record is abstract tagged null record;

   type Execute_Corrupted is access all Execute_Corrupted_Record'Class;

   procedure Panic
     (Corruption : access Execute_Corrupted_Record; Error_Message : String)
   is abstract;
   --  This primitive is called when a Codefix_Panic is caught while applying a
   --  fix.

   procedure Obsolescent
     (Corruption : access Execute_Corrupted_Record; Error_Message : String)
   is abstract;
   --  This primitive is called when a Obsolescent_Fix is caught while applying
   --  a fix.

   procedure Free (Corruption : in out Execute_Corrupted);

   procedure Secured_Execute
     (This         : Text_Command'Class;
      Current_Text : in out Text_Navigator_Abstr'Class;
      Error_Cb     : Execute_Corrupted := null);
   --  Same as execute, but:
   --    * The command is executed with Casing_Policy disabled. Required since
   --      the Casing_Policy preference must be considered only when the buffer
   --      is being manually modified by the user; otherwise it may cause
   --      unexpected changes to the fix.
   --    * Catches exception. The routine Error_Cb.Panic is called in case of
   --      Codefix_Panic, and Error_Cb.Obsolescent is called in case of an
   --      Obsolescent fix caught.
   --        ??? Cases where Obsolescent_Fix should be raised instead of
   --        Codefix_Panic should be investigated further.

   procedure Free (This : in out Text_Command) is null;
   --  Free the memory associated to a Text_Command

   procedure Free_Data (This : in out Text_Command'Class);
   --  Free the memory associated to a Text_Command

   procedure Free (This : in out Ptr_Command);
   --  Free the data associated to a Ptr_Command

   procedure Set_Caption
     (This    : in out Text_Command'Class;
      Caption : Unbounded_String);
   --  Define the caption that describes the action of a Text_Command

   function Get_Caption (This : Text_Command'Class) return String;
   --  Return the caption associated to a Text_Command

   function Get_Parser (This : Text_Command'Class) return Error_Parser_Access;

   procedure Set_Parser
     (This : in out Text_Command'Class;
      Parser : Error_Parser_Access);

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

   package Text_List is new GPS_Vectors (Ptr_Text);
   use Text_List;

   type Ptr_List_Text is access Text_List.Vector;

   procedure Free
     is new Ada.Unchecked_Deallocation (Text_List.Vector, Ptr_List_Text);

   type Text_Navigator_Abstr is abstract tagged record
      Files        : Ptr_List_Text := new Text_List.Vector;
      Registry     : Projects.Project_Registry_Access;
      Construct_Db : Construct_Database_Access;
      Context      : Factory_Context;
   end record;

   function Get_File
     (This    : Text_Navigator_Abstr'Class;
      Name    : GNATCOLL.VFS.Virtual_File) return Ptr_Text;
   --  Returns the existent file interface, or create a new one if it doesn't
   --  exists.

   type Mark_Abstr is abstract tagged record
      Is_First_Line : Boolean := False;
      File_Name     : GNATCOLL.VFS.Virtual_File;
   end record;

   ----------------------------------------------------------------------------
   --  type Text_Interface
   ----------------------------------------------------------------------------

   procedure Free is new Ada.Unchecked_Deallocation
     (Construct_List, Construct_List_Access);

   type Ptr_Boolean is access all Boolean;
   procedure Free is new Ada.Unchecked_Deallocation (Boolean, Ptr_Boolean);

   type Text_Interface is abstract tagged record
      Construct_File       : Structured_File_Access := null;
      File_Name            : GNATCOLL.VFS.Virtual_File;
      Structure_Up_To_Date : Ptr_Boolean := new Boolean'(False);
      Construct_Db         : Construct_Database_Access;
   end record;

   function Get_Iterator_At
     (Current_Text      : access Text_Interface;
      Cursor            : Text_Cursor'Class;
      From_Type         : Position_Type := Start_Name;
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
   --  Others
   ----------------------------------------------------------------------------

   type Text_Cursor is tagged record
      Line : Natural := 0;
      --  If Line = 0, indicates a special case to handle the first line
      --  differently. ??? Not quite clear why it is needed to handle it
      --  specially.

      Col  : Visible_Column_Type := 0;
      --  The reason why we store columns rather than char index is that
      --  converting a column index into a char index may be more expensive.
      --  When you convert from char to column, you usually already have the
      --  string to be processed, which is less frequently the case when you
      --  have a column (because it might come from a GNAT message for
      --  instance).
   end record;

   type File_Cursor is new Text_Cursor with record
      File : GNATCOLL.VFS.Virtual_File;
   end record;

   Null_File_Cursor : constant File_Cursor := (0, 0, GNATCOLL.VFS.No_File);

   type Word_Cursor is new File_Cursor with record
      String_Match : Unbounded_String;
      Mode         : String_Mode := Text_Ascii;
   end record;

   Null_Word_Cursor : constant Word_Cursor :=
     (Null_File_Cursor with Null_Unbounded_String, Text_Ascii);

   type Text_Command (Complexity : Fix_Complexity) is abstract tagged record
      Caption : Unbounded_String;
      Parser  : Error_Parser_Access;
      --  ??? To be set right after the validated fix!
   end record;

end Codefix.Text_Manager;
