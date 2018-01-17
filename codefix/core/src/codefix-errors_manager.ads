------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2002-2018, AdaCore                     --
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

with Codefix.Text_Manager;  use Codefix.Text_Manager;
with Codefix.Formal_Errors; use Codefix.Formal_Errors;
with Codefix.Error_Lists;   use Codefix.Error_Lists;
with Codefix.Errors_Parser; use Codefix.Errors_Parser;
with GNATCOLL.VFS;

with Ada.Unchecked_Deallocation;

private with GPS_Vectors;

package Codefix.Errors_Manager is

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
      Processor   : Fix_Processor;
      Source_Text : Text_Navigator_Abstr'Class;
      Errors_List : in out Error_Message_List;
      Options     : Fix_Options;
      Callback    : Error_Callback := null);
   --  Cover the whole list of errors, and add them into This. If Callback
   --  is not null, it is called each time a correctible error is found.

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

   function Get_Number_Of_Errors (This : Correction_Manager) return Natural;
   --  Return the number of errors found in This.

   function Search_Error
     (This         : Correction_Manager;
      File         : GNATCOLL.VFS.Virtual_File;
      Line         : Integer;
      Column       : Visible_Column_Type;
      Message      : String := "")
     return Error_Id;
   --  Return the Error_Id contained in the correction manager correspondant to
   --  the message. If this error does not exist, Null_Error_Id is returned.
   --  If Message is the empty string, the first error at that location is
   --  returned

   procedure Set_Error_Cb
     (This     : in out Correction_Manager;
      Error_Cb : Execute_Corrupted);
   --  Set the function that will be called when the execution of a command
   --  doesn't work.

   function Get_Previous_Error (Error : Error_Id) return Error_Id;
   --  Return the error that have been recorded before Error in This.

private

   type Ptr_Boolean is access all Boolean;
   procedure Free is new Ada.Unchecked_Deallocation (Boolean, Ptr_Boolean);

   type Error_Id_Record is record
      Message         : Error_Message := Invalid_Error_Message;
      Solutions       : Solution_List := Null_Solution_List;
      Fixed           : Ptr_Boolean := new Boolean'(False);
   end record;

   procedure Free (This : in out Error_Id_Record);

   package Memorized_Corrections is new GPS_Vectors (Error_Id_Record);
   use Memorized_Corrections;

   type Error_Id is new Std_Vectors.Cursor;

   Null_Error_Id : constant Error_Id := Error_Id (Std_Vectors.No_Element);

   type Correction_Manager is record
      Potential_Corrections : Memorized_Corrections.Vector;
      Offset_Line           : Integer := 0;
      Error_Cb              : Execute_Corrupted;
   end record;

   procedure Add_Error
     (This      : in out Correction_Manager;
      Message   : Error_Message;
      Solutions : Solution_List;
      New_Error : out Error_Id);

end Codefix.Errors_Manager;
