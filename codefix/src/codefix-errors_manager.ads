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

with Codefix.Text_Manager; use Codefix.Text_Manager;
with Codefix.Formal_Errors; use Codefix.Formal_Errors;
use Codefix.Formal_Errors.Command_List;
with GNAT.OS_Lib;
with VFS;

with Ada.Unchecked_Deallocation;

package Codefix.Errors_Manager is

   function Cut_Message (Str : String) return String;
   --  Return the message label of a message without line/column/file
   --  indication

   ----------------------------------------------------------------------------
   --  type Errors_Interface
   ----------------------------------------------------------------------------

   type Errors_Interface is abstract tagged private;
   --  Type used to manage error messages send by the compilator.

   type Ptr_Errors_Interface is access all Errors_Interface'Class;

   procedure Free (This : in out Ptr_Errors_Interface);

   procedure Free (This : in out Errors_Interface) is abstract;

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

   procedure Get_Preview
     (This         : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Preview      : out Error_Message);
   --  Return the next message, but without remove it from the message list.
   --  The same message can still be got by Get_Message.

   function No_More_Messages
     (This : Errors_Interface) return Boolean is abstract;
   --  Is true where all the messages are got fron Get_Message.

   procedure Skip_Message (This : in out Errors_Interface'Class);
   --  Skip the next message.

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

   function Get_Category (This : Error_Id) return String;
   --  Return the category of the error.

   function Is_Fixed (This : Error_Id) return Boolean;
   --  Return True if the Error_Id has already been fixed, that means that a
   --  validate function has already be used with it.

   function Get_Number_Of_Fixes (This : Error_Id) return Natural;
   --  Return the number of possible fixes contained in Error_Id.

   procedure Undo (This : Error_Id; Current_Text : Text_Navigator_Abstr'Class);
   --  Undo the changes that have been made with This.

   ----------------------------------------------------------------------------
   --  type Correction_Manager
   ----------------------------------------------------------------------------

   type Correction_Manager is private;
   type Ptr_Correction_Manager is access all Correction_Manager;

   procedure Free (This : in out Ptr_Correction_Manager);

   type Error_Callback is access procedure
     (Id           : Error_Id;
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

   procedure Validate_And_Commit
     (This         : in out Correction_Manager;
      Current_Text : in out Text_Navigator_Abstr'Class;
      Error        : Error_Id;
      Choice       : Natural);
   --  Execute Choice and commit it to the Current_Text.

   procedure Validate_And_Commit
     (This         : in out Correction_Manager;
      Current_Text : in out Text_Navigator_Abstr'Class;
      Error        : Error_Id;
      Choice       : Text_Command'Class);
   --  Execute Choice and commit it to the Current_Text.

   procedure Free (This : in out Correction_Manager);
   --  Free the memory associated to a Correction_Manager.

   function Get_First_Error (This : Correction_Manager) return Error_Id;
   --  Return the first error found in the correction manager.

   function Search_Error
     (This         : Correction_Manager;
      File         : VFS.Virtual_File;
      Line, Column : Integer;
      Message      : String)
     return Error_Id;
   --  Return the Error_Id contained in the correction manager correspondant to
   --  the message. If this error does not exist, Null_Error_Id is returned.

   procedure Set_Error_Cb
     (This     : in out Correction_Manager;
      Error_Cb : Execute_Corrupted);
   --  Set the function that will be called when the execution of a command \
   --  doesn't work.

   function Get_Previous_Error
     (This : Correction_Manager; Error : Error_Id) return Error_Id;
   --  Return the error that have been recorded before Error in This.

   ----------------------------------------------------------------------------
   --  type Error_State
   ----------------------------------------------------------------------------

   type Error_State is (Enabled, Disabled, Unknown);
   --  The two states possible for an error.

   type State_List is private;

   procedure Set_Error_State
     (List : in out State_List; Error : String; State : Error_State);
   --  Modify the current error state.

   function Get_Error_State
     (List : State_List; Error : String) return Error_State;
   --  Return the current error state.

private

   type Errors_Interface is abstract tagged record
      Preview : Error_Message := Invalid_Error_Message;
   end record;

   type Ptr_Boolean is access all Boolean;
   procedure Free is new Ada.Unchecked_Deallocation (Boolean, Ptr_Boolean);

   type Error_Id_Record is record
      Message         : Error_Message := Invalid_Error_Message;
      Solutions       : Solution_List := Command_List.Null_List;
      Category        : GNAT.OS_Lib.String_Access;
      Fixed           : Ptr_Boolean := new Boolean'(False);
      Solution_Chosen : Ptr_Extract := new Extract;
   end record;

   procedure Free (This : in out Error_Id_Record);

   package Memorized_Corrections is new Generic_List (Error_Id_Record);
   use Memorized_Corrections;

   type Error_Id is new Memorized_Corrections.List_Node;

   Null_Error_Id : constant Error_Id :=
     Error_Id (Memorized_Corrections.Null_Node);

   type Correction_Manager is record
      Potential_Corrections : Memorized_Corrections.List;
      Offset_Line           : Integer := 0;
      Error_Cb              : Execute_Corrupted;
   end record;

   procedure Add_Error
     (This      : in out Correction_Manager;
      Message   : Error_Message;
      Solutions : Solution_List;
      Category  : String;
      New_Error : out Error_Id);

   type State_Node is record
      Error : GNAT.OS_Lib.String_Access;
      State : Error_State := Unknown;
   end record;

   procedure Free (This : in out State_Node);

   package State_Lists is new Generic_List (State_Node);
   use State_Lists;

   type State_List is new State_Lists.List;

end Codefix.Errors_Manager;
