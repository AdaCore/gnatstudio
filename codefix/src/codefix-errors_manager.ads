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

with Codefix.Text_Manager; use Codefix.Text_Manager;
with Codefix.Formal_Errors; use Codefix.Formal_Errors;
use Codefix.Formal_Errors.Extract_List;

package Codefix.Errors_Manager is

   ----------------------------------------------------------------------------
   --  type Errors_Interface
   ----------------------------------------------------------------------------

   type Errors_Interface is abstract tagged private;
   --  Type used to manage error messages send by the compilator.

   procedure Get_Direct_Message
     (This    : in out Errors_Interface;
      Current : out Error_Message) is abstract;
   --  Get a message without any modification of cols or lines numbers.

   procedure Get_Message
     (This         : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Current      : out Error_Message);
   --  Returns the next message to be analyzed, with the correct modifications.
   --  (change the cols to be conformant with tabs).

   function No_More_Messages
     (This : Errors_Interface) return Boolean is abstract;
   --  Is true where all the messages are got fron Get_Message.

   ----------------------------------------------------------------------------
   --  type Error_Id
   ----------------------------------------------------------------------------

   type Error_Id is private;

   Null_Error_Id : constant Error_Id;

   function Next (This : Error_Id) return Error_Id;
   --  Return the next error from the error list wich contains Error_Id.
   --  If Error_Id is the last error of the list, return Null_Error_Id.

   function Get_Solutions (This : Error_Id) return Solution_List;
   --  Return the solutions found for the error.

   function Get_Error_Message (This : Error_Id) return Error_Message;
   --  Return the error message associated to the id.

   ----------------------------------------------------------------------------
   --  type Correction_Manager
   ----------------------------------------------------------------------------

   type Correction_Manager is private;

   type Error_Callback is access procedure
     (Message      : Error_Message; --  ??? Remove this parameter ?
      Id           : Error_Id;
      Solutions    : Solution_List; --  ??? Remove this parameter ?
      Current_Text : Text_Navigator_Abstr'Class;
      Corrector    : in out Correction_Manager);
   --  Type of procedure that can be called when a correctible error message
   --  is found.

   procedure Analyze
     (This        : in out Correction_Manager;
      Source_Text : Text_Navigator_Abstr'Class;
      Errors_List : in out Errors_Interface'Class;
      Callback    : Error_Callback := null);
   --  Cover the whole list of errors, and add then into This. If Callback
   --  is not null, then each time a correctible error is found the function
   --  is call.

   procedure Validate
     (This   : in out Correction_Manager;
      Error  : Error_Id;
      Choice : Natural);
   --  Specify a choice between the differents correction'possibilities
   --  of a message.

   procedure Validate
     (This   : in out Correction_Manager;
      Error  : Error_Id;
      Choice : Extract);
   --  Specify a choice between the differents correction'possibilities
   --  of a message.

   subtype Alternative_Choice is Natural range 0 .. 2;

   type Ambiguous_Callback is access procedure
     (Alternative_1, Alternative_2 : Extract;
      Current_Text                 : Text_Navigator_Abstr'Class;
      Delete_Choice                : out Alternative_Choice);
   --  Is called when ambiguities appears. If Delete_Choice is 0, no solution
   --  are chosen and the ambiguity stay. Otherwise, the choice is deleted.

   procedure Commit
     (This         : in out Correction_Manager;
      Success      : out Boolean;
      Current_Text : in out Text_Navigator_Abstr'Class;
      Callback     : Ambiguous_Callback := null);
   --  Check amiguities and call the callback to solve them. If all
   --  ambiguities are solved, then success is True and the modifications
   --  made in the correction manager are updated in the real text.

   procedure Check_Ambiguities
     (Solutions        : in out Solution_List;
      Callback         : Ambiguous_Callback;
      Current_Text     : Text_Navigator_Abstr'Class;
      No_More_Problems : out Boolean);
   --  Call the Callback when 2 solutions concerns the same line. If, at the
   --  end, there are no more ambiguities then No_More_Problems is Ture.
   --  Otherwise, it is false.

   procedure Free (This : in out Correction_Manager);
   --  Free the memory associated to a Correction_Manager.

   function Get_First_Error (This : Correction_Manager) return Error_Id;
   --  Return the first error found in the correction manager.

   function Search_Error (This : Correction_Manager; Message : String)
     return Error_Id;
   --  Return the Error_Id contained in the correction manager correspondant to
   --  the message. If this error does not exist, Null_Error_Id is returned.

   procedure Update_Changes
     (This         : Correction_Manager;
      Current_Text : Text_Navigator_Abstr'Class;
      Object       : in out Extract'Class;
      Success      : out Boolean);

private

   type Errors_Interface is abstract tagged null record;

   type Error_Id_Record is record
      Message   : Error_Message := Invalid_Error_Message;
      Solutions : Solution_List := Extract_List.Null_List;
   end record;

   procedure Free (This : in out Error_Id_Record);

   package Memorized_Corrections is new Generic_List (Error_Id_Record);
   use Memorized_Corrections;

   type Error_Id is new Memorized_Corrections.List_Node;

   Null_Error_Id : constant Error_Id :=
     Error_Id (Memorized_Corrections.Null_Node);

   type Correction_Manager is record
      Potential_Corrections : Memorized_Corrections.List;
      Valid_Corrections     : Solution_List;
      Offset_Line           : Integer := 0;
   end record;

   procedure Add_Error
     (This      : in out Correction_Manager;
      Message   : Error_Message;
      Solutions : Solution_List;
      New_Error : out Error_Id);

   package Line_List is new Generic_List (Extract_Line, Free);
   use Line_List;

   function Sort (List : Solution_List) return Line_List.List;
   --  ??? Can't we use List_Utils.Sort instead

end Codefix.Errors_Manager;
