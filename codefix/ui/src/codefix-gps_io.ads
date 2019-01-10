------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2019, AdaCore                     --
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

with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;

with GPS.Editors;            use GPS.Editors;
with GPS.Kernel;             use GPS.Kernel;
with GNATCOLL.VFS;

with Codefix.Text_Manager;   use Codefix.Text_Manager;

package Codefix.GPS_Io is

   type Console_Interface is new Text_Interface with private;

   type GPS_Mark is new Mark_Abstr with private;

   overriding
   function Get_New_Mark
     (Current_Text : Console_Interface;
      Cursor       : File_Cursor'Class) return Mark_Abstr'Class;
   --  Create a new mark at the position specified by the cursor.

   overriding
   function Get_Current_Cursor
     (Current_Text : Console_Interface;
      Mark         : Mark_Abstr'Class) return File_Cursor'Class;
   --  Return the current position of the mark.

   overriding
   procedure Undo (This : in out Console_Interface);
   --  Undo the last action made in This.

   overriding
   function Get
     (This   : Console_Interface;
      Cursor : Text_Cursor'Class;
      Len    : Natural) return String;
   --  Get Len characters from the file and the position specified by the
   --  cursor.

   overriding
   function Get
     (This   : Console_Interface;
      Cursor : Text_Cursor'Class) return Character;
   --  Get the character from the file and the position specified by the
   --  cursor.

   overriding
   function Get_Line
     (This      : Console_Interface;
      Cursor    : Text_Cursor'Class;
      Start_Col : Visible_Column_Type := 0) return String;
   --  Get all character from the column and the line specified by the cursor
   --  to the end of the line.

   overriding
   procedure Replace
     (This      : in out Console_Interface;
      Cursor    : Text_Cursor'Class;
      Len       : Natural;
      New_Value : String);
   --  Replace the Len characters, from the position designed by the cursor, by
   --  New_Value.

   overriding
   procedure Replace
     (This         : in out Console_Interface;
      Start_Cursor : Text_Cursor'Class;
      End_Cursor   : Text_Cursor'Class;
      New_Value    : String);
   --  Replace the characters between Start_Cursor and End_Cursor by New_Value.

   overriding
   procedure Add_Line
     (This     : in out Console_Interface;
      Cursor   : Text_Cursor'Class;
      New_Line : String;
      Indent   : Boolean := False);
   --  Add a line at the cursor specified. To add a line at the
   --  begining of the text, set cursor line = 0.

   overriding
   procedure Delete_Line
     (This : in out Console_Interface;
      Cursor : Text_Cursor'Class);
   --  Delete the line where the cursor is.

   overriding
   procedure Indent_Line
     (This   : in out Console_Interface;
      Cursor : Text_Cursor'Class);
   --  Indent the line pointed by the cursor.

   overriding
   procedure Initialize
     (This : in out Console_Interface;
      Path : GNATCOLL.VFS.Virtual_File);
   --  Initialize the structure of the Console_Interface. Actually do noting.

   overriding function Read_File
     (This : Console_Interface) return Unbounded_String;
   --  Get the entire file

   overriding
   function Line_Max (This : Console_Interface) return Natural;
   --  Return the last position of line in File_Interface.

   procedure Set_Kernel
     (This : in out Console_Interface; Kernel : Kernel_Handle);
   --  Set the value of the kernel linked to the Console_Interface.

   overriding
   procedure Constrain_Update (This : in out Console_Interface);
   --  Set the text to modified state.

private

   type Console_Interface is new Text_Interface with record
      Kernel : Kernel_Handle;
   end record;

   type GPS_Mark is new Mark_Abstr with record
      Mark : Editor_Mark_Holders.Holder;
   end record;

end Codefix.GPS_Io;
