with Text_Manager; use Text_Manager;
with Messages; use Messages;
with Generic_List;
with Ada.Text_IO; use Ada.Text_IO;

package Formal_Errors is

   Ambiguous_Correction : exception;

   package Extract_List is new Generic_List (Extract);
   use Extract_List;

   subtype Solution_List is Extract_List.List;
   --  This is a list of solutions proposed to solve an error.

   function Get_Extract
     (This     : Solution_List;
      Position : Positive)
      return Extract;
   type Correction_Manager (Current_Text : Ptr_Text) is record
      Corrections : Solution_List;
   end record;
   --  type Correction_Manager (Current_Text : Ptr_Text) is private;

   procedure Add_Correction
     (This           : in out Correction_Manager;
      New_Correction : Extract);

   procedure Update (This : in out Correction_Manager);
   --  Check a certain quantity of things...
   --  (doubles modifs for example)

   procedure Free (This : in out Correction_Manager);

private

   package Line_List is new Generic_List (Extract_Line, Free);
   use Line_List;

--   type Correction_Manager (Current_Text : Ptr_Text) is record
--      Corrections : Solution_List;
--   end record;

   function Sort (List : Solution_List) return Line_List.List;

   function Should_Be
     (Current_Text : Text_Interface'Class;
      Message      : Error_Message;
      Str_Expected : String;
      Str_Red      : String := "")
      return Extract;
   --  This fonction replace Str_Red by Str_Expected in the current text by
   --  the position specified in the Message. If there is no Str_Red, it
   --  looks for the first word in the position.

   function Wrong_Order
     (Current_Text                : Text_Interface'Class;
      Message                     : Error_Message;
      First_String, Second_String : String)
      return Extract;
   --  Seach the position of the second string from the position specified
   --  in the message to the beginning, and invert the two strings.

   function Expected
     (Current_Text    : Text_Interface'Class;
      Message         : Error_Message;
      String_Expected : String;
      Add_Spaces      : Boolean := True)
      return Extract;
   --  Add the missing keyword into the text.

   function Unexpected
     (Current_Text      : Text_Interface'Class;
      Message           : Error_Message;
      String_Unexpected : String;
      Mode              : String_Mode := Text_Ascii)
      return Extract;
   --  Delete the unexpected string

   function Wrong_Column
     (Current_Text    : Text_Interface'Class;
      Message         : Error_Message;
      Column_Expected : Natural := 0)
      return Extract;
   --  Try re-indent the line

   function With_Clause_Missing
     (Current_Text   : Text_Interface'Class;
      Cursor         : File_Cursor'Class;
      Missing_Clause : String)
      return Extract;
   --  Add the missing clause in the text

   type Case_Type is (Lower, Upper, Mixed);

   function Bad_Casing
     (Current_Text : Text_Interface'Class;
      Cursor       : File_Cursor'Class;
      Correct_Word : String := "";
      Word_Case    : Case_Type := Mixed)
   return Extract;
   --  Re-case the word

end Formal_Errors;
