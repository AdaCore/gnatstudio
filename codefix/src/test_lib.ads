with Ada.Text_IO; use Ada.Text_IO;

with Codefix; use Codefix;
with Codefix.Text_Manager; use Codefix.Text_Manager;
with Codefix.Errors_Manager; use Codefix.Errors_Manager;
with Codefix.Errors_Parser; use Codefix.Errors_Parser;
with Codefix.Formal_Errors; use Codefix.Formal_Errors;
with Codefix.File_Io; use Codefix.File_Io;
with Codefix.Text_Navigators;
use Codefix.Formal_Errors.Extract_List;

package Test_Lib is


   package Navigator is new Text_Navigators (File_Interface);
   use Navigator;

   package Int_IO is new Integer_IO (Integer);
   use Int_IO;

   procedure Corrections_Proposed
     (Message      : Error_Message;
      Id           : Error_Id;
      Solutions    : Solution_List;
      Current_Text : Text_Navigator_Abstr'Class;
      Corrector    : in out Correction_Manager);

   function Get_Number (Min, Max : Integer) return Integer;

   procedure Ambiguity
     (Alternative_1, Alternative_2 : Extract;
      Delete_Choice                : out Alternative_Choice);

end Test_Lib;
