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

with Glide_Kernel;           use Glide_Kernel;

with Codefix.Text_Manager;   use Codefix.Text_Manager;
with Codefix.Errors_Manager; use Codefix.Errors_Manager;
with Codefix.Formal_Errors;  use Codefix.Formal_Errors;

package Codefix.GPS_Io is

   type Console_Interface is new Text_Interface with private;

   type GPS_Mark is new Mark_Abstr with private;

   function Get_Id (This : GPS_Mark) return String;

   function Get_New_Mark
     (Current_Text : Console_Interface;
      Cursor       : Text_Cursor'Class) return Mark_Abstr'Class;

   function Get_Current_Cursor
     (Current_Text : Console_Interface;
      Mark         : Mark_Abstr'Class) return File_Cursor'Class;

   procedure Free (This : in out GPS_Mark);

   procedure Free (This : in out Console_Interface);
   --  Free the memory associated with the Console_Interface

   function Get
     (This   : Console_Interface;
      Cursor : Text_Cursor'Class;
      Len    : Natural) return String;
   --  Get Len characters from the file and the position specified by the
   --  cursor.

   function Get_Line
     (This   : Console_Interface;
      Cursor : Text_Cursor'Class) return String;
   --  Get all character from the column and the line specified by the cursor
   --  to the end of the line.

   procedure Replace
     (This      : in out Console_Interface;
      Cursor    : Text_Cursor'Class;
      Len       : Natural;
      New_Value : String);
   --  Replace the Len characters, from the position designed by the cursor, by
   --  New_Value.

   procedure Add_Line
     (This        : in out Console_Interface;
      Cursor      : Text_Cursor'Class;
      New_Line    : String);
   --  Add a line at the cursor specified. To add a line at the
   --  begining of the text, set cursor line = 0.

   procedure Delete_Line
     (This : in out Console_Interface;
      Cursor : Text_Cursor'Class);
   --  Delete the line where the cursor is.

   procedure Initialize
     (This : in out Console_Interface;
      Path : String);
   --  Initialize the structure of the Console_Interface. Actually do noting.

   function Read_File (This : Console_Interface) return Dynamic_String;
   --  Get the entire file in a Dynamic_String.

   procedure Commit (This : Console_Interface);
   --  Save the file.

   function Line_Max (This : Console_Interface) return Natural;
   --  Return the last position of line in File_Interface.

   procedure Set_Kernel
     (This : in out Console_Interface; Kernel : Kernel_Handle);

   type Compilation_Output is new Errors_Interface with private;
   --  This type is an interface to the list of compilation errors that the
   --  compilator has found.

   procedure Free (This : in out Compilation_Output);
   --  Free the memory associated to an Error_File.

   procedure Get_Direct_Message
     (This    : in out Compilation_Output;
      Current : out Error_Message);
   --  Get a message without any modification of cols or lines numbers.

   function No_More_Messages (This : Compilation_Output) return Boolean;
   --  Is true where all the messages are got fron Get_Message.

   procedure Get_Last_Output
     (This : in out Compilation_Output; Kernel : Kernel_Handle);

private

   type Console_Interface is new Text_Interface with record
      Kernel : Kernel_Handle;
   end record;

   type GPS_Mark is new Mark_Abstr with record
      Id : Dynamic_String;
   end record;

   type Compilation_Output is new Errors_Interface with record
      Errors_Buffer : Dynamic_String := null;
      Current_Index : Natural := 1;
   end record;

end Codefix.GPS_Io;
