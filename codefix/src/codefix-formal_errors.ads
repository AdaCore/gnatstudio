with Ada.Text_IO; use Ada.Text_IO;

with Generic_List;
with Language; use Language;

with Codefix.Text_Manager; use Codefix.Text_Manager;

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

   package Extract_List is new Generic_List (Extract);
   use Extract_List;

   subtype Solution_List is Extract_List.List;
   --  This is a list of solutions proposed to solve an error.

   function Get_Extract
     (This     : Solution_List;
      Position : Positive)
     return Extract;
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
      Str_Red      : String := "")
      return Extract;
   --  This fonction replace Str_Red by Str_Expected in the current text by
   --  the position specified in the Message. If there is no Str_Red, it
   --  looks for the first word in the position.

   function Wrong_Order
     (Current_Text                : Text_Navigator_Abstr'Class;
      Message                     : Error_Message;
      First_String, Second_String : String)
      return Extract;
   --  Seach the position of the second string from the position specified
   --  in the message to the beginning, and invert the two strings.

   function Expected
     (Current_Text    : Text_Navigator_Abstr'Class;
      Message         : Error_Message;
      String_Expected : String;
      Add_Spaces      : Boolean := True)
      return Extract;
   --  Add the missing keyword into the text.

   function Unexpected
     (Current_Text      : Text_Navigator_Abstr'Class;
      Message           : Error_Message;
      String_Unexpected : String;
      Mode              : String_Mode := Text_Ascii)
      return Extract;
   --  Delete the unexpected string

   function Wrong_Column
     (Current_Text    : Text_Navigator_Abstr'Class;
      Message         : Error_Message;
      Column_Expected : Natural := 0)
      return Extract;
   --  Try re-indent the line

   function With_Clause_Missing
     (Current_Text   : Text_Navigator_Abstr'Class;
      Cursor         : File_Cursor'Class;
      Missing_Clause : String)
      return Extract;
   --  Add the missing clause in the text

   type Case_Type is (Lower, Upper, Mixed);

   function Bad_Casing
     (Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class;
      Correct_Word : String := "";
      Word_Case    : Case_Type := Mixed)
   return Extract;
   --  Re-case the word

   function Not_Referenced
     (Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class;
      Category     : Language_Category;
      Name         : String)
   return Solution_List;
   --  Propose to delete the unit unreferrenced or, in some cases, to add
   --  a pragma 'not referreced'

   function First_Line_Pragma
     (Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class)
   return Extract;
   --  Move the pragma to the beginning of the file

private

   type Error_Message is new File_Cursor with record
      Message : Dynamic_String;
   end record;

   procedure Parse_Head (Message : String; This : out Error_Message);
   function Clone (This : Error_Message) return Error_Message;

   Invalid_Error_Message : constant Error_Message := (0, 0, null, null);

end Codefix.Formal_Errors;
