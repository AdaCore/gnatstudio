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
with Codefix.Errors_Parser; use Codefix.Errors_Parser;
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
   --  type Correction_Manager
   ----------------------------------------------------------------------------

   type Correction_Manager is private;

   type Error_Id is private;

   type Error_Callback is access procedure
     (Message      : Error_Message;
      Id           : Error_Id;
      Solutions    : Solution_List;
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
     (This         : in out Correction_Manager;
      Error        : Error_Id;
      Choice       : Natural;
      Later_Update : Boolean := True);
   --  Specify a choice between the differents correction'possibilities
   --  of a message;

   subtype Alternative_Choice is Natural range 0 .. 2;

   type Ambiguous_Callback is access procedure
     (Alternative_1, Alternative_2 : Extract;
      Delete_Choice                : out Alternative_Choice);
   --  Is called when ambiguities appears. If Delete_Choice is 0, no solution
   --  are chosen and the ambiguity stay. Otherwise, the choice is deleted.

   procedure Update
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
      No_More_Problems : out Boolean);
   --  Call the Callback when 2 solutions concerns the same line. If, at the
   --  end, there are no more ambiguities then No_More_Problems is Ture.
   --  Otherwise, it is false.

   procedure Free (This : in out Correction_Manager);
   --  Free the memory associated to a Correction_Manager.

private

   type Errors_Interface is abstract tagged null record;

   package Memorized_Corrections is new Generic_List (Solution_List);
   use Memorized_Corrections;

   type Error_Id is record
      Ptr_Solutions : Memorized_Corrections.List_Node :=
         Memorized_Corrections.Null_Node;
   end record;

   type Correction_Manager is record
      Potential_Corrections : Memorized_Corrections.List;
      Valid_Corrections     : Solution_List;
   end record;

   procedure Add_Error
     (This      : in out Correction_Manager;
      Solutions : Solution_List;
      New_Error : out Error_Id);

   package Line_List is new Generic_List (Extract_Line, Free);
   use Line_List;

   function Sort (List : Solution_List) return Line_List.List;
   --  ??? Can't we use List_Utils.Sort instead

end Codefix.Errors_Manager;
