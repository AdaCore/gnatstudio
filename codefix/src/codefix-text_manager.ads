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

with Ada.Unchecked_Deallocation;

with Ada_Analyzer; use Ada_Analyzer;
with Language;     use Language;

with Generic_List;

package Codefix.Text_Manager is

   Text_Manager_Error : exception;

   type Step_Way is (Normal_Step, Reverse_Step);
   type Relative_Position is (Before, After, Specified);

   function Is_Blank (Str : String) return Boolean;
   --  Return true if Str is only composed by white characters.

   ----------------------------------------------------------------------------
   --  type Text_Cursor
   ----------------------------------------------------------------------------

   type Text_Cursor is tagged record
      Line, Col : Natural;
   end record;

   function "<" (Left, Right : Text_Cursor) return Boolean;
   --  Return True when Left is before Right.

   function ">" (Left, Right : Text_Cursor) return Boolean;
   --  Return True when Left is after Right.

   type File_Cursor is new Text_Cursor with record
      File_Name : Dynamic_String;
   end record;

   Null_File_Cursor : constant File_Cursor;

   function "=" (Left, Right : File_Cursor) return Boolean;
   --  Return true when Left is in the same position than rigth.

   procedure Free (This : in out File_Cursor);
   --  Frees the memory used by fields of File_Cursor.

   function Clone (This : File_Cursor) return File_Cursor;
   --  Duplicate all informations of a File_Cursor, specially informations
   --  memorized in dynamic memory.

   ----------------------------------------------------------------------------
   --  type Text_Interface
   ----------------------------------------------------------------------------

   type Text_Interface is abstract tagged private;
   type Ptr_Text is access all Text_Interface'Class;

   procedure Free (This : in out Ptr_Text);
   --  Free the memory associated to Ptr_Text and the object referenced.

   procedure Initialize
     (This      : in out Text_Interface;
      File_Name : String) is abstract;
   --  Initialize the structure of the Text_Interface.

   procedure Free (This : in out Text_Interface);
   --  Free the memory associated to the Text_Interface.

   function Get
     (This   : Text_Interface;
      Cursor : Text_Cursor'Class;
      Len    : Natural) return String is abstract;
   --  Get Len characters from the the position specified by the cursor.

   function Get_Line
     (This   : Text_Interface;
      Cursor : Text_Cursor'Class) return String is abstract;
   --  Get all character from the column specified by the cursor to the end of
   --  the line.

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
   --  Add a line at the cursor specified. To add a line at the
   --  begining of the text, set cursor line = 0.

   procedure Delete_Line
     (This : in out Text_Interface;
      Cursor : Text_Cursor'Class) is abstract;
   --  Delete the line where the cursor is.

   function Line_Length
     (This   : Text_Interface'Class;
      Cursor : Text_Cursor'Class) return Natural;
   --  Returns le length of a line from the position of the cursor.

   function Read_File
     (This : Text_Interface) return Dynamic_String is abstract;
   --  Get the entire file in a Dynamic_String.

   function Get_File_Name (This : Text_Interface) return String;
   --  Return the name of the file.

   procedure Commit (This : Text_Interface) is abstract;
   --  Update the changes previously made in the real text.

   function Search_String
     (This         : Text_Interface'Class;
      Cursor       : Text_Cursor'Class;
      Searched     : String;
      Step         : Step_Way := Normal_Step;
      Jump_String  : Boolean := True) return File_Cursor'Class;
   --  Search a string in the text and returns a cursor at the beginning. If
   --  noting is found, then the cursor is Null_Cursor.

   function Line_Max (This : Text_Interface) return Natural is abstract;
   --  Return the number of the last line in the text loaded.

   function Search_Unit
     (This     : Text_Interface'Class;
      Category : Language_Category;
      Name     : String) return Construct_Information;
   --  Return the first Contruct_Information that matche Category and name.
   --  If not found, return a Contruct_Information with Category = Cat_Unknown

   ----------------------------------------------------------------------------
   --  type Text_Navigator
   ----------------------------------------------------------------------------

   type Text_Navigator_Abstr is abstract tagged private;

   procedure Free (This : in out Text_Navigator_Abstr);

   function Get_Unit
     (Current_Text           : Text_Navigator_Abstr;
      Cursor                 : File_Cursor'Class;
      Position               : Relative_Position := Specified;
      Category_1, Category_2 : Language_Category := Cat_Unknown)
      return Construct_Information;
   --  Get the Construct_Information found at the specified position, or the
   --  nearest before or after the position (depends on the value of
   --  Position_Expected.

   function Search_Body
     (Current_Text : Text_Navigator_Abstr;
      File_Name    : String;
      Spec         : Construct_Information) return Construct_Information;
   --  Returns the body of a subprogramm, only if this body is in the same
   --  file.

   function Get
     (This   : Text_Navigator_Abstr;
      Cursor : File_Cursor'Class;
      Len    : Natural) return String;
   --  Get Len characters from the file and the position specified by the
   --  cursor.

   function Get_Line
     (This   : Text_Navigator_Abstr;
      Cursor : File_Cursor'Class) return String;
   --  Get all character from the file and the column specified by the cursor
   --  to the end of the line.

   function Read_File
     (This      : Text_Navigator_Abstr;
      File_Name : String) return Dynamic_String;
   --  Get the entire file File_Name in a Dynamic_String.

   procedure Replace
     (This      : in out Text_Navigator_Abstr;
      Cursor    : File_Cursor'Class;
      Len       : Natural;
      New_Value : String);
   --  Replace the Len characters, from the position designed by the cursor, by
   --  New_Value.

   procedure Add_Line
     (This        : in out Text_Navigator_Abstr;
      Cursor      : File_Cursor'Class;
      New_Line    : String);
   --  Add a line at the cursor specified. To add a line at the
   --  begining of the text, set cursor line = 0.

   procedure Delete_Line
     (This : in out Text_Navigator_Abstr;
      Cursor : File_Cursor'Class);
   --  Delete the line where the cursor is.

   function Line_Length
     (This   : Text_Navigator_Abstr;
      Cursor : File_Cursor'Class) return Natural;
   --  Return le length of a line from the position of the cursor.

   procedure Commit (This : Text_Navigator_Abstr);
   --  Update the changes previously made in the real text.

   function New_Text_Interface (This : Text_Navigator_Abstr)
      return Ptr_Text is abstract;
   --  Create and initialise a new Text_Interface used by the text navigator.

   function Search_String
     (This         : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class;
      Searched     : String;
      Step         : Step_Way := Normal_Step;
      Jump_String  : Boolean := True) return File_Cursor'Class;
   --  Search a string in the text and returns a cursor at the beginning. If
   --  noting is found, then the cursor is Null_Cursor.

   function Search_Unit
     (This      : Text_Navigator_Abstr'Class;
      File_Name : String;
      Category  : Language_Category;
      Name      : String) return Construct_Information;
   --  Return the first Contruct_Information that matche Category and name.
   --  If not found, return a Contruct_Information with Category = Cat_Unknown

   function Line_Max
     (This      : Text_Navigator_Abstr'Class;
      File_Name : String) return Natural;
   --  Return the number of the last line in the text loaded.

   ----------------------------------------------------------------------------
   --  type Extract_Line
   ----------------------------------------------------------------------------

   type Extract_Line is private;
   type Ptr_Extract_Line is access Extract_Line;

   type Line_Context is
     (Original_Line,
      Line_Modified,
      Line_Created,
      Line_Deleted);

   procedure Assign (This : in out Extract_Line; Value : Extract_Line);

   function Get_Context (This : Extract_Line) return Line_Context;
   --  Return the context associated to an Extract_Line.

   function Next (This : Extract_Line) return Ptr_Extract_Line;
   --  If the Extract_Line is a component of a list (typically, a list
   --  contained in an extract), then Next returns the next entry of the list.

   function Get_String (This : Extract_Line) return String;
   --  Returns the string memorized in an Extract_Line.

   function Get_Cursor (This : Extract_Line) return File_Cursor'Class;
   --  Return the cursor memorized in an Extract_Line.

   procedure Commit
     (This         : Extract_Line;
      Current_Text : in out Text_Navigator_Abstr'Class;
      Offset_Line  : in out Integer);
   --  Upate changes of the Extract_Line in the representation of the text,

   procedure Free (This : in out Extract_Line);
   --  Free the memory associated to an Extract_Line, and if this line is in
   --  a list of line, free recursivly the whole list.

   function Clone
     (This      : Extract_Line;
      Recursive : Boolean) return Extract_Line;
   --  Clone an Extract_Line. Recursive True means that all the lines of the
   --  extract that record this line are cloned.

   function Search_String
     (This         : Extract_Line;
      Cursor       : File_Cursor'Class;
      Searched     : String;
      Step         : Step_Way := Normal_Step;
      Jump_String  : Boolean := True) return File_Cursor'Class;
   --  Search a string in the text and returns a cursor at the beginning. If
   --  noting is found, then the cursor is Null_Cursor. If Cursor.Col = 0, then
   --  the scan in initialized from the end of the content.

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

   procedure Set_String
     (This        : in out Extract_Line;
      New_String  : String;
      First, Last : Natural := 0);

   procedure Set_Coloration (This : in out Extract_Line; Value : Boolean);

   function Get_Coloration (This : Extract_Line) return Boolean;

   ----------------------------------------------------------------------------
   --  type Extract
   ----------------------------------------------------------------------------

   type Extract is tagged private;
   --  An extract is a temporary object that contains a part of the source
   --  code, modified or not. The modifications made in an extract do not have
   --  any influence in the source code before the call of Update function.

   type String_Mode is (Text_Ascii, Regular_Expression);

   function Clone (This : Extract) return Extract;
   --  Duplicate all informations associated to an extract, specially
   --  information referenced in pools.

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

   procedure Set_String
     (This     : Extract;
      Value    : String;
      Position : Natural := 1);
   --  Set a string recorded in the Extract. Position in the position of the
   --  record, and not the number of the line.

   procedure Commit
     (This         : Extract;
      Current_Text : in out Text_Navigator_Abstr'Class;
      Offset_Line  : in out Natural);
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

   function Get_Line
     (This : Extract; Position : File_Cursor'Class) return Ptr_Extract_Line;
   --  Returns the line with the number specified in the original text.

   function Get_Record
     (This : Extract; Number : Natural) return Ptr_Extract_Line;
   --  Returns the line recorded at the position Number in the extract.

   function Get_Number_Lines (This : Extract) return Natural;
   --  Returns the number of the lines in the extract.

   procedure Add_Line
     (This   : in out Extract;
      Cursor : File_Cursor'Class;
      Text   : String);
   --  Add a line AFTER the line specified by the cursor. Make a cursor with
   --  0 for the line number to add a line at the begenning of the file.

   procedure Delete_Line
     (This   : in out Extract;
      Cursor : File_Cursor'Class);
   --  Delete the line of the extract at the line number and in the file
   --  specified by the cursor.

   procedure Delete_All_Lines (This : in out Extract);
   --  Delete all the lines from the extract

   procedure Get_Entity
     (This : in out Extract;
      Current_Text : Text_Navigator_Abstr'Class;
      Cursor : File_Cursor'Class);
   --  Add in the Extract lines of the Entity witch begins at the position
   --  specified by the cursor (if it is a spec, the body is also got).

   function Search_String
     (This         : Extract;
      Searched     : String;
      Cursor       : File_Cursor'Class := Null_File_Cursor;
      Step         : Step_Way := Normal_Step;
      Jump_String  : Boolean := True) return File_Cursor'Class;
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

   function Get_New_Text_Length (This : Extract) return Natural;
   --  Return the length of the current text in the extract.

   function Get_Old_Text_Length
     (This : Extract;
      Current_Text : Text_Navigator_Abstr'Class) return Natural;
   --  Return the length of the current text before modifications.

   procedure Set_Caption (This : in out Extract; Caption : String);
   --  Set the caption associated to an extract.

   function Get_Caption (This : Extract) return String;
   --  Return the caption associated to an extract.

   function Get_First_Line (This : Extract) return Ptr_Extract_Line;
   --  Return the first line recored in an extract.

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

   procedure Merge
     (This                 : out Extract;
      Extract_1, Extract_2 : Extract'Class;
      Current_Text         : Text_Navigator_Abstr'Class;
      Success              : out Boolean;
      Partial_Merge        : Boolean := False);
   --  Merge Extract_1 and Extract_2 into This. If no solution can be found,
   --  then success is false. If Partial_Merge is True, then only lines
   --  contained in Extract_1 and Extract_2 will be merged.

   procedure Merge
     (New_Str : out Dynamic_String;
      Original_Str, Str_1, Str_2 : String;
      Success : out Boolean);
   --  Merge Str_1 and Str_2 into This. If no solution can be found,
   --  then success is false.

private

   function Compare_Pkg (Pkg_1, Pkg_2 : String) return Boolean;
   --  Compare two package names and determine if their are coherent. For
   --  example, Direct_IO and Ada.Direct_IO are coherent packages names.

   ----------------------------------------------------------------------------
   --  type Text_Navigator
   ----------------------------------------------------------------------------

   package Text_List is new Generic_List (Ptr_Text);
   use Text_List;

   type Ptr_List_Text is access Text_List.List;

   procedure Free
     is new Ada.Unchecked_Deallocation (Text_List.List, Ptr_List_Text);

   type Text_Navigator_Abstr is abstract tagged record
      Files : Ptr_List_Text := new Text_List.List;
   end record;

   function Get_File
     (This : Text_Navigator_Abstr'Class;
      Name : String) return Ptr_Text;
   --  Returns the existent file interface, or create a new one if it
   --  doesn't exists.

   ----------------------------------------------------------------------------
   --  type Text_Interface
   ----------------------------------------------------------------------------

   procedure Free is new Ada.Unchecked_Deallocation
     (Construct_List, Construct_List_Access);

   type Text_Interface is abstract tagged record
      Tokens_List : Construct_List_Access := new Construct_List;
      File_Name   : Dynamic_String;
   end record;

   function Get_Unit
     (Current_Text           : Text_Interface;
      Cursor                 : Text_Cursor'Class;
      Position               : Relative_Position := Specified;
      Category_1, Category_2 : Language_Category := Cat_Unknown)
   return Construct_Information;

   function Search_Body
     (Current_Text : Text_Interface;
      Spec         : Construct_Information)
   return Construct_Information;

   ----------------------------------------------------------------------------
   --  type Extract_Line
   ----------------------------------------------------------------------------

   type Extract_Line is record
      Context         : Line_Context := Original_Line;
      Cursor          : File_Cursor;
      Original_Length : Natural := 0;
      Content         : Dynamic_String;
      Next            : Ptr_Extract_Line;
      Coloration      : Boolean := False;
   end record;

   procedure Add_Element
     (This, Previous, Element : Ptr_Extract_Line;
      Container               : in out Extract);
   --  Add a new line in the line list. The line is always disposed in order
   --  to preserve the internal order of the lines, not just at the end of the
   --  list.

   procedure Free is
      new Ada.Unchecked_Deallocation (Extract_Line, Ptr_Extract_Line);

   procedure Get
     (This        : Text_Interface'Class;
      Cursor      : File_Cursor'Class;
      Len         : Natural;
      Destination : in out Extract_Line);

   procedure Get_Line
     (This        : Text_Interface'Class;
      Cursor      : File_Cursor'Class;
      Destination : in out Extract_Line);

   ----------------------------------------------------------------------------
   --  type Extract
   ----------------------------------------------------------------------------

   type Extract is tagged record
      First   : Ptr_Extract_Line;
      Caption : Dynamic_String;
   end record;

   procedure Add_Element (This : in out Extract; Element : Ptr_Extract_Line);

   function Get_Word_Length
     (This   : Extract_Line;
      Col    : Natural;
      Format : String)
     return Natural;

   function Length (This : Extract_Line) return Natural;

   function Previous (Container : Extract; Node : Ptr_Extract_Line)
     return Ptr_Extract_Line;

   ----------------------------------------------------------------------------
   --  Others
   ----------------------------------------------------------------------------

   Null_File_Cursor : constant File_Cursor := (0, 0, null);

end Codefix.Text_Manager;
