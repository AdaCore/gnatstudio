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

with Generic_List;
with Language; use Language;

with Codefix.Text_Manager; use Codefix.Text_Manager;
with Codefix.Text_Manager.Ada_Extracts; use Codefix.Text_Manager.Ada_Extracts;

package Codefix.Formal_Errors is

   ----------------------------------------------------------------------------
   --  type Error_Message
   ----------------------------------------------------------------------------

   type Error_Message is new File_Cursor with private;

   Invalid_Error_Message : constant Error_Message;

   procedure Initialize (This : in out Error_Message; Message : String);
   --  Parse the message headed in order to get the col number and the
   --  line number.

   procedure Initialize (This : in out Error_Message; Line, Col : Positive);
   --  Sets the value of Line and Col field of an Error_Message.

   function Get_Message (This : Error_Message) return String;
   --  Returns the message with the header.

   procedure Free (This : in out Error_Message);
   --  Frees the memory used by the object.

   package Extract_List is new Generic_List (Extract'Class, Free_Data);
   use Extract_List;

   subtype Solution_List is Extract_List.List;
   --  This is a list of solutions proposed to solve an error.

   function Get_Extract
     (This     : Solution_List;
      Position : Positive) return Extract'Class;
   --  Get the extract recorded in a solution list at the given position.

   procedure Free (This : in out Solution_List);
   --  Free the memory associated to a Solution_List.

   ----------------------------------------------------------------------------
   --  functions of formal errors
   ----------------------------------------------------------------------------

   function Should_Be
     (Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Str_Expected : String;
      Str_Red      : String := "";
      Format_Red   : String_Mode := Text_Ascii) return Extract;
   --  This fonction replace Str_Red by Str_Expected in the current text by
   --  the position specified in the Message. If there is no Str_Red, it
   --  looks for the first word in the position.

   function Wrong_Order
     (Current_Text                : Text_Navigator_Abstr'Class;
      Message                     : Error_Message;
      First_String, Second_String : String) return Extract;
   --  Seach the position of the second string from the position specified
   --  in the message to the beginning, and invert the two strings.

   function Expected
     (Current_Text    : Text_Navigator_Abstr'Class;
      Message         : Error_Message;
      String_Expected : String;
      Add_Spaces      : Boolean := True) return Extract;
   --  Add the missing keyword into the text.

   function Unexpected
     (Current_Text      : Text_Navigator_Abstr'Class;
      Message           : Error_Message;
      String_Unexpected : String;
      Mode              : String_Mode := Text_Ascii) return Extract;
   --  Delete the unexpected string

   function Wrong_Column
     (Current_Text    : Text_Navigator_Abstr'Class;
      Message         : Error_Message;
      Column_Expected : Natural := 0) return Extract;
   --  Try re-indent the line

   function With_Clause_Missing
     (Current_Text   : Text_Navigator_Abstr'Class;
      Cursor         : File_Cursor'Class;
      Missing_Clause : String) return Extract;
   --  Add the missing clause in the text

   type Case_Type is (Lower, Upper, Mixed);

   function Bad_Casing
     (Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class;
      Correct_Word : String := "";
      Word_Case    : Case_Type := Mixed) return Extract;
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
      Cursor       : File_Cursor'Class) return Extract;
   --  Move the pragma to the beginning of the file

   function Not_Modified
     (Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class;
      Name         : String) return Ada_List;

   function Resolve_Ambiguity
     (Current_Text    : Text_Navigator_Abstr'Class;
      Error_Cursor    : File_Cursor'Class;
      Solution_Cursor : File_Cursor'Class;
      Name            : String) return Extract;

   function Remove_Conversion
     (Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class;
      Object_Name   : String) return Ada_Instruction;

private

   type Error_Message is new File_Cursor with record
      Message : Dynamic_String;
   end record;

   procedure Parse_Head (Message : String; This : out Error_Message);
   --  Initialize Col, Line and File_Name fields of This by parsing the head
   --  of the message.

   function Clone (This : Error_Message) return Error_Message;
   --  Duplicate all the information used in Error_Message, specially the
   --  object referenced in.

   Invalid_Error_Message : constant Error_Message := (0, 0, null, null);

end Codefix.Formal_Errors;
