-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2002-2003                    --
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

with Generic_List;
with GNAT.OS_Lib;
with Language; use Language;
with VFS;

with Codefix.Text_Manager; use Codefix.Text_Manager;
with GNAT.Regpat;
with GPS.Kernel;

package Codefix.Formal_Errors is

   package Cursor_Lists is new Generic_List (File_Cursor);
   use Cursor_Lists;

   ----------------------------------------------------------------------------
   --  type Error_Message
   ----------------------------------------------------------------------------

   type Error_Message is new File_Cursor with private;

   Invalid_Error_Message : constant Error_Message;

   procedure Initialize
     (This       : in out Error_Message;
      Kernel     : access GPS.Kernel.Kernel_Handle_Record'Class;
      Error_Line : String;
      Regexp     : GNAT.Regpat.Pattern_Matcher;
      File_Index, Line_Index, Col_Index, Msg_Index : Integer;
      Style_Index, Warning_Index : Integer);
   --  Parses an error message from the tool based on the regular expression

   procedure Initialize
     (This      : in out Error_Message;
      File      : VFS.Virtual_File;
      Line, Col : Positive;
      Message   : String);
   --  Store the contents of an error message, after it has been parsed

   function Get_Message (This : Error_Message) return String;
   --  Returns the message text (no line or column number)

   procedure Free (This : in out Error_Message);
   --  Frees the memory used by the object.

   package Command_List is new Generic_List (Text_Command'Class, Free_Data);
   use Command_List;

   subtype Solution_List is Command_List.List;
   --  This is a list of solutions proposed to solve an error.

   function Is_Style_Or_Warning (Error : Error_Message) return Boolean;
   --  Return true if the error message is either a style error or a warning.
   --  Such errors have a lesser priority, and will not be auto-fixed if there
   --  are standard errors on the same line.
   --  This is false for Invalid_Error_Message.

   function Get_Command
     (This     : Solution_List;
      Position : Positive) return Text_Command'Class;
   --  Get the extract recorded in a solution list at the given position.

   procedure Free (This : in out Solution_List);
   --  Free the memory associated to a Solution_List.

   ----------------------------------------------------------------------------
   --  functions of formal errors
   ----------------------------------------------------------------------------

   function Should_Be
     (Current_Text : Text_Navigator_Abstr'Class;
      Message      : File_Cursor'Class;
      Str_Expected : String;
      Str_Red      : String := "";
      Format_Red   : String_Mode := Text_Ascii;
      Caption      : String := "") return Solution_List;
   --  This fonction replace Str_Red by Str_Expected in the current text by
   --  the position specified in the Message. If there is no Str_Red, it
   --  looks for the first word in the position.

   function Wrong_Order
     (Current_Text                : Text_Navigator_Abstr'Class;
      Message                     : Error_Message;
      First_String, Second_String : String) return Solution_List;
   --  Seach the position of the second string from the position specified
   --  in the message to the beginning, and invert the two strings.

   function Expected
     (Current_Text    : Text_Navigator_Abstr'Class;
      Message         : File_Cursor'Class;
      String_Expected : String;
      Add_Spaces      : Boolean := True;
      Position        : Relative_Position := Specified) return Solution_List;
   --  Add the missing keyword into the text.

   function Unexpected
     (Current_Text      : Text_Navigator_Abstr'Class;
      Message           : File_Cursor'Class;
      String_Unexpected : String;
      Mode              : String_Mode := Text_Ascii) return Solution_List;
   --  Delete the unexpected string

   function Wrong_Column
     (Current_Text    : Text_Navigator_Abstr'Class;
      Message         : File_Cursor'Class;
      Column_Expected : Natural := 0) return Solution_List;
   --  Try re-indent the line

   function With_Clause_Missing
     (Current_Text   : Text_Navigator_Abstr'Class;
      Cursor         : File_Cursor'Class;
      Missing_Clause : String) return Solution_List;
   --  Add the missing clause in the text

   function Bad_Casing
     (Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class;
      Correct_Word : String := "";
      Word_Case    : Case_Type := Mixed) return Solution_List;
   --  Re-case the word

   function Not_Referenced
     (Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class;
      Category     : Language_Category;
      Name         : String) return Solution_List;
   --  Propose to delete the unit unreferrenced or, in some cases, to add
   --  a pragma 'not referreced'

   function First_Line_Pragma
     (Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class) return Solution_List;
   --  Move the pragma to the beginning of the file

   function Not_Modified
     (Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class;
      Name         : String) return Solution_List;
   --  Add 'constant' to the declaration of the variable Name. Create a new
   --  declaration if needed.

   function Resolve_Ambiguity
     (Current_Text     : Text_Navigator_Abstr'Class;
      Error_Cursor     : File_Cursor'Class;
      Solution_Cursors : Cursor_Lists.List;
      Name             : String) return Solution_List;
   --  Add to the object Name the prefix of the package declared at the
   --  position Solution_Cursor. If the ambiguity can't be solved by this
   --  function, then Extract_List.List is empty.

   function Remove_Conversion
     (Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class;
      Object_Name  : String) return Solution_List;
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

   function Remove_Use_Clause
     (Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class) return Solution_List;
   --  Remove a use clause at the position defined by the cursor.

   function Resolve_Unvisible_Declaration
     (Current_Text  : Text_Navigator_Abstr'Class;
      Object_Cursor : File_Cursor'Class;
      Pkg_Cursor    : File_Cursor'Class;
      Seek_With     : Boolean) return Solution_List;
   --  Propose to add a use or to prefix the object.


private

   type Error_Message is new File_Cursor with record
      Message : GNAT.OS_Lib.String_Access;
      Is_Style, Is_Warning : Boolean := False;
   end record;

   function Clone (This : Error_Message) return Error_Message;
   --  Duplicate all the information used in Error_Message, specially the
   --  object referenced in.

   Invalid_Error_Message : constant Error_Message :=
     (Null_File_Cursor with null, False, False);

end Codefix.Formal_Errors;
