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

with Ada.Text_IO; use Ada.Text_IO;

with Codefix; use Codefix;
with Codefix.Text_Manager; use Codefix.Text_Manager;
with Codefix.Errors_Manager; use Codefix.Errors_Manager;
with Codefix.Formal_Errors; use Codefix.Formal_Errors;
with Codefix.File_Io; use Codefix.File_Io;
with Codefix.Text_Navigators;
use Codefix.Formal_Errors.Extract_List;

package Test_Lib is

   package Navigator is new Text_Navigators (File_Interface);
   use Navigator;

   package Int_IO is new Integer_IO (Integer);
   use Int_IO;

   Visible      : Boolean := True;
   Capture_File : File_Type;

   procedure Corrections_Proposed
     (Message      : Error_Message;
      Id           : Error_Id;
      Solutions    : Solution_List;
      Current_Text : Text_Navigator_Abstr'Class;
      Corrector    : in out Correction_Manager);
   --  Put on screen all the solutions proposed to solve the error, and get
   --  the choice of the user.

   function Get_Number (Min, Max : Integer) return Integer;
   --  Get a number from the user between Min and Max. If the number is
   --  incorrect, the user is asked again.

   procedure Ambiguity
     (Alternative_1, Alternative_2 : Extract;
      Current_Text                 : Text_Navigator_Abstr'Class;
      Delete_Choice                : out Alternative_Choice);
   --  Put on screen the ambiguity problem.

end Test_Lib;
