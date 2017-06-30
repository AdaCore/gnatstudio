------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2002-2017, AdaCore                     --
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

--  An error message treatment in Codefix is divided into two parts: the
--  structure of the message and its semantic meaning. Those two parts
--  are separated into two different packages: Codefix.Errors_Parsers and
--  Codefix.Formal_Errors.

with GNAT.Regpat;

with Codefix.Text_Manager;  use Codefix.Text_Manager;

with Language;              use Language;
with Language.Tree;         use Language.Tree;
with Projects;
with GNATCOLL.VFS;
with GPS_Vectors;

private with Ada.Unchecked_Deallocation;

package Codefix.Formal_Errors is

   package Cursor_Lists is new GPS_Vectors (File_Cursor);
   use Cursor_Lists;

   ----------------------------------------------------------------------------
   --  type Error_Message
   ----------------------------------------------------------------------------

   type Error_Message is new File_Cursor with private;

   Invalid_Error_Message : constant Error_Message;

   procedure Initialize
     (This          : in out Error_Message;
      Registry      : Projects.Project_Registry_Access;
      Error_Line    : Unbounded_String;
      Regexp        : GNAT.Regpat.Pattern_Matcher;
      File_Index    : Integer;
      Line_Index    : Integer;
      Col_Index     : Integer;
      Msg_Index     : Integer;
      Style_Index   : Integer;
      Warning_Index : Integer;
      Order         : Long_Long_Integer);
   --  Parses an error message from the tool based on the regular expression

   procedure Initialize
     (This    : in out Error_Message;
      File    : GNATCOLL.VFS.Virtual_File;
      Line    : Positive;
      Col     : Visible_Column_Type;
      Message : Unbounded_String;
      Order   : Long_Long_Integer);
   --  Store the contents of an error message, after it has been parsed

   function Get_Message (This : Error_Message) return String;
   --  Returns the message text (no line or column number)

   function Get_Order (This : Error_Message) return Long_Long_Integer;
   --  Return the order number of this message.

   procedure Cancel (This : in out Error_Message);
   --  Cancels the error message - it will not be taken into account by the
   --  fixes.

   function Is_Cancelled (This : Error_Message) return Boolean;
   --  Does the message needs to be taken into account, or has it been
   --  cancelled?

   overriding procedure Free (This : in out Error_Message);
   --  Frees the memory used by the object.

   type Solution_List is private;

   Null_Solution_List : constant Solution_List;

   procedure Concat
     (Dest : in out Solution_List; Source : Solution_List);
   --  Adds the contents of Sources at the end of Dest. No deep copy is done by
   --  this function.

   procedure Unique_Concat
     (Dest : in out Solution_List; Source : Solution_List);
   --  The same as Concat but avoid dublicated solutions.
   --  Source list isn't modified.

   function Length (List : Solution_List) return Integer;
   --  Return the number of elements in the list.

   type Solution_List_Iterator is private;

   function First (List : Solution_List) return Solution_List_Iterator;
   --  Return an iterator on the first element of the list.

   function Next (It : Solution_List_Iterator) return Solution_List_Iterator;
   --  Moves the iterator to the next element of the list.

   function At_End (It : Solution_List_Iterator) return Boolean;
   --  Return true if the iterator reached the end of the list.

   function Get_Command
     (It : Solution_List_Iterator) return Ptr_Command;
   --  Return the command currently pointed by this iterator.

   function Is_Style_Or_Warning (Error : Error_Message) return Boolean;
   --  Return true if the error message is either a style error or a warning.
   --  Such errors have a lesser priority, and will not be auto-fixed if there
   --  are standard errors on the same line.
   --  This is false for Invalid_Error_Message.

   function Get_Command
     (This     : Solution_List;
      Position : Positive) return Ptr_Command;
   --  Get the extract recorded in a solution list at the given position.

   procedure Set_Parser
     (It : Solution_List_Iterator; Parser : Error_Parser_Access);
   --  Modifies the parser associated to the command stored in parameter.

   procedure Free_List (This : in out Solution_List);
   --  Free the memory associated to a Solution_List.

   ----------------------------------------------------------------------------
   --  generic formal errors
   ----------------------------------------------------------------------------

   function Should_Be
     (Current_Text : Text_Navigator_Abstr'Class;
      Message      : File_Cursor'Class;
      Str_Expected : Unbounded_String;
      Str_Read     : Unbounded_String := Null_Unbounded_String;
      Format_Read  : String_Mode      := Text_Ascii;
      Caption      : Unbounded_String := Null_Unbounded_String)
      return Solution_List;
   --  This function replace Str_Read by Str_Expected in the current text by
   --  the position specified in the Message. If there is no Str_Read, it
   --  looks for the first word in the position.

   function Wrong_Order
     (Current_Text  : Text_Navigator_Abstr'Class;
      Message       : Error_Message;
      First_String  : Unbounded_String;
      Second_String : Unbounded_String) return Solution_List;
   --  Seach the position of the second string from the position specified
   --  in the message to the beginning, and invert the two strings.

   function Expected
     (Current_Text    : Text_Navigator_Abstr'Class;
      Message         : File_Cursor'Class;
      String_Expected : Unbounded_String;
      After_Pattern   : String := "";
      Add_Spaces      : Boolean := True;
      Position        : Relative_Position := Specified) return Solution_List;
   --  Add the missing keyword into the text. If after pattern is not empty,
   --  then the cursor will be moved from the location specified after the
   --  pattern given in parameter.

   function Unexpected
     (Current_Text      : Text_Navigator_Abstr'Class;
      Message           : File_Cursor'Class;
      String_Unexpected : Unbounded_String;
      Mode              : String_Mode := Text_Ascii;
      Search_Forward    : Boolean     := False;
      All_Occurrences   : Boolean     := False) return Solution_List;
   --  Delete one occurrence of String_Unexpected from location Message. The
   --  Mode parameter discriminates if the unexpected string is specified in
   --  plain Ascii (default) or as a regular expression.
   --
   --  Two optional parameters extend the functionality of this routine:
   --  * Search_Forward is set to true if we cannot rely on Message as the
   --    exact location of an occurrence of Unexpected_String. It enables
   --    searching for the first occurrence of the unexpected string from
   --    the column referenced by Message to the end of the line.
   --
   --  * All_Occurrences is set to True to remove all the consecutive
   --    occurrences of the unexpected string found before and after the
   --    cursor.

   function Expand_Tabs
     (Current_Text      : Text_Navigator_Abstr'Class;
      Message           : File_Cursor'Class) return Solution_List;
   --  Expand all the horizontal tabs into spaces.

   function Wrong_Column
     (Current_Text    : Text_Navigator_Abstr'Class;
      Message         : File_Cursor'Class;
      Column_Expected : Visible_Column_Type := 0) return Solution_List;
   --  Try re-indent the line

   function Clause_Missing
     (Current_Text   : Text_Navigator_Abstr'Class;
      Cursor         : File_Cursor'Class;
      Missing_Clause : Unbounded_String;
      Add_With       : Boolean;
      Add_Use        : Boolean) return Solution_List;
   --  Add the missing clause in the text

   function Bad_Casing
     (Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class;
      Correct_Word : Unbounded_String := Null_Unbounded_String;
      Word_Case    : Case_Type := Mixed) return Solution_List;
   --  Re-case the word

   function Not_Referenced
     (Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class;
      Category     : Language_Category;
      Name         : Unbounded_String;
      Operations   : Useless_Entity_Operations) return Solution_List;
   --  Propose to delete the unit unreferrenced or, in some cases, to add
   --  a pragma 'not referrenced'. Those operations can be disabled with
   --  the appropriate mask.

   function First_Line_Pragma
     (Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class) return Solution_List;
   --  Move the pragma to the beginning of the file

   function Not_Modified
     (Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class;
      Name         : Unbounded_String) return Solution_List;
   --  Add 'constant' to the declaration of the variable Name. Create a new
   --  declaration if needed.

   function Resolve_Ambiguity
     (Current_Text     : Text_Navigator_Abstr'Class;
      Error_Cursor     : File_Cursor'Class;
      Solution_Cursors : Cursor_Lists.Vector;
      Name             : Unbounded_String) return Solution_List;
   --  Add to the object Name the prefix of the package declared at the
   --  position Solution_Cursor. If the ambiguity can't be solved by this
   --  function, then Extract_List.List is empty.

   function Remove_Conversion
     (Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class;
      Message      : Unbounded_String) return Solution_List;
   --  Remove the conversion at made at Cursor position.

   function Move_With_To_Body
     (Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class) return Solution_List;
   --  Move all use and with clauses to the body, if needed. Otherwise, their
   --  are just deleted.

   function Make_Conformant
     (Current_Text : Text_Navigator_Abstr'Class;
      Body_Cursor  : File_Cursor'Class;
      Spec_Cursor  : File_Cursor'Class) return Solution_List;
   --  Propose to make the body profile conformant with the spec one, or
   --  the spec profile conformant with the body one.

   function Remove_Dependency_Clause
     (Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class;
      Category     : Dependency_Category;
      Position     : Relative_Position;
      Look_For_Use : Boolean := False) return Solution_List;
   --  Remove a with/use clause at the position defined by the cursor. The
   --  position parameter specify where the actual begin of the unit is
   --  relatively to the cursor position. If Look_For_Use is true, then use
   --  clauses will be removed along with with ones.

   function Resolve_Unvisible_Declaration
     (Current_Text  : Text_Navigator_Abstr'Class;
      Object_Cursor : File_Cursor'Class;
      Pkg_Cursor    : File_Cursor'Class;
      Seek_With     : Boolean) return Solution_List;
   --  Propose to add a use or to prefix the object.

   function Remove_Element_From_Unreferenced_Pragma
     (Current_Text  : Text_Navigator_Abstr'Class;
      Object_Cursor : File_Cursor'Class;
      Object_Name   : String) return Solution_List;
   --  Remove a name from an "Unreferenced" pragma

   function Add_Line
     (Current_Text  : Text_Navigator_Abstr'Class;
      Object_Cursor : File_Cursor'Class;
      Line          : Unbounded_String;
      Indent        : Boolean) return Solution_List;
   --  Add a line at the given location

   ----------------------------------------------------------------------------
   --  Ada specific formal errors
   ----------------------------------------------------------------------------

   function Add_Record_Rep_Clause
     (Current_Text  : Text_Navigator_Abstr'Class;
      Cursor        : File_Cursor'Class;
      Caption       : Unbounded_String;
      First_Clause  : String;
      Second_Clause : String := "";
      With_Clause   : String := "")
      return Solution_List;
   --  Proposes to add a record representation clause to a record type
   --  declaration

   function Remove_Extra_Underlines
     (Current_Text : Text_Navigator_Abstr'Class; Cursor : File_Cursor'Class)
      return Solution_List;
   --  Propose to remove the extra underlines of the identifier pointed by the
   --  cursor.

   function Change_To_Tick_Valid
     (Current_Text : Text_Navigator_Abstr'Class; Cursor : File_Cursor'Class)
      return Solution_List;
   --  Proposes to change the code given at the cursor by the corresponding
   --  'Valid

   function Remove_Blank_Lines
     (Current_Text : Text_Navigator_Abstr'Class; Cursor : File_Cursor'Class)
      return Solution_List;
   --  Propose a fix removing all the blank lines starting at the cursor
   --  location.

   function Remove_Parenthesis_Couple
     (Current_Text : Text_Navigator_Abstr'Class;
      Open_Paren : File_Cursor'Class)
      return Solution_List;
   --  Removes a couple of parenthesis.

   function Fix_Index_Number
     (Current_Text : Text_Navigator_Abstr'Class;
      Location     : File_Cursor'Class;
      Do_Remove    : Boolean)
      return Solution_List;
   --  Adds or remove the index used in array attributes.

   function Reorder_Subprogram
     (Current_Text : Text_Navigator_Abstr'Class;
      Location     : File_Cursor'Class)
      return Solution_List;
   --  Reorders a subprogram in the list.

   function Remove_Statement
     (Current_Text : Text_Navigator_Abstr'Class;
      Location     : File_Cursor'Class)
      return Solution_List;
   --  Remove the statement located at the location given in parameter

   function Remove_Attribute
     (Current_Text : Text_Navigator_Abstr'Class;
      Location     : File_Cursor'Class)
      return Solution_List;
   --  Removes the attribute at the given location, e.g. useless 'Base.

   function Replace_Attribute
     (Current_Text : Text_Navigator_Abstr'Class;
      Location     : File_Cursor'Class;
      Replace_By   : String)
      return Solution_List;
   --  Replaces the attribute at the given location with given text.

   function Renames_To_Constant
     (Current_Text : Text_Navigator_Abstr'Class;
      Location     : File_Cursor'Class)
      return Solution_List;
   --  Changes a renaming declaration to a constant variable

   function Remove_Comparison
     (Current_Text : Text_Navigator_Abstr'Class;
      Location     : File_Cursor'Class)
      return Solution_List;
   --  Removes a useless comparison, e.g. = True or /= True

   function Add_Elaborate_All
     (Current_Text   : Text_Navigator_Abstr'Class;
      Cursor         : File_Cursor'Class;
      Package_Name   : String) return Solution_List;
   --  Add pragma Elaborate_All for given package

   ----------------------------------------------------------------------------
   --  SPARK specific formal errors
   ----------------------------------------------------------------------------

   function Move_Tilde_Or_Percent
     (Current_Text : Text_Navigator_Abstr'Class; Cursor : File_Cursor'Class)
      return Solution_List;
   --  Propose a fix moving the misplaced tilde or percent at the cursor
   --  location to its correct place

   overriding function Clone (This : Error_Message) return Error_Message;
   --  Duplicate all the information used in Error_Message, specially the
   --  object referenced in.

private

   type Error_Message is new File_Cursor with record
      Message      : Unbounded_String;
      --  Message should be encoded in UTF-8.
      Is_Style     : Boolean := False;
      Is_Warning   : Boolean := False;

      Order        : Long_Long_Integer;
      --  This has to be a long long integer, as it may be initialized with a
      --  timestamp on e.g. GNATbench.

      Is_Cancelled : Boolean := False;
      --  Messages can be canceled when already taken into account by other
      --  fixes.
   end record;

   type List_Node_Record;
   type Solution_List_Iterator is access List_Node_Record;
   type List_Node_Access is access Solution_List_Iterator;

   Null_Node : constant Solution_List_Iterator := null;

   type Solution_List is record
      First : List_Node_Access;
      Last  : List_Node_Access;
   end record;
   --  This is a list of solutions proposed to solve an error.

   List_Empty : exception;

   Null_Solution_List : constant Solution_List := Solution_List'(null, null);

   type Data_Access is access Ptr_Command;

   type List_Node_Record is record
      Element : Data_Access;
      Next    : Solution_List_Iterator;
   end record;

   procedure Free_Element is new
     Ada.Unchecked_Deallocation (Ptr_Command, Data_Access);

   procedure Free_Node is new
     Ada.Unchecked_Deallocation (List_Node_Record, Solution_List_Iterator);

   procedure Free_Node_Access is new
     Ada.Unchecked_Deallocation (Solution_List_Iterator, List_Node_Access);

   procedure Append (This : in out Solution_List; Command : Ptr_Command);

   Invalid_Error_Message : constant Error_Message :=
     (Null_File_Cursor with Null_Unbounded_String, False, False, 0, True);

end Codefix.Formal_Errors;
