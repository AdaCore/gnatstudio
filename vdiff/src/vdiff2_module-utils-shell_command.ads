------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2012, AdaCore                     --
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

--  This package provides wraper for shell command.

package Vdiff2_Module.Utils.Shell_Command is

   function Add_Line
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Pos    : Natural;
      Style  : String := "";
      Number : Natural := 1) return Natural;
   --  Add a blank line at line Pos of a given file editor,
   --  using Style for color.
   --  Return corresponding Mark.
   pragma Inline (Add_Line);

   procedure Edit
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      File     : Virtual_File);
   --  Open editor for File
   pragma Inline (Edit);

   procedure Synchronize_Scrolling
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File1  : Virtual_File;
      File2  : Virtual_File;
      File3  : Virtual_File := No_File);
   --  Synchronize the scrolling between files

   function Get_Chars
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Line   : Natural;
      Column : Natural;
      Before : Integer := -1;
      After  : Integer := -1) return String;
   --  Get the characters around a certain position.
   --  Returns string between <before> characters before the mark
   --  and <after> characters after the position.
   --  If <before> or <after> is omitted,
   --  the bounds will be at the beginning and/or the end of the line.
   pragma Inline (Get_Chars);

   function Get_File_Last_Line
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : Virtual_File) return Natural;
   --  Return the number of line in file File
   pragma Inline (Get_File_Last_Line);

   function Get_Line_Number
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Mark   : String) return Natural;
   --  Returns the current line of Mark
   pragma Inline (Get_Line_Number);

   procedure Delete_Mark
     (Kernel : Kernel_Handle;
      Link   : String);
   pragma Inline (Delete_Mark);

   procedure Highlight_Line
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Pos    : Natural;
      Style  : String := "";
      Number : Natural := 1);
   --  Color a line at line Pos in a given file editor, using Style for color
   pragma Inline (Highlight_Line);

   procedure Highlight_Range
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File    : Virtual_File;
      Style   : String;
      Line    : Natural := 0;
      Start_C : Integer := -1;
      End_C   : Integer := -1);
   --  Highlights a portion of a line in a file with the given category
   pragma Inline (Highlight_Range);

   procedure Register_Highlighting
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register color preferences

   procedure Remove_Blank_Lines
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Mark   : Natural);
   --  Remove blank lines located at mark
   pragma Inline (Remove_Blank_Lines);

   procedure Replace_Text
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Line   : Natural;
      Column : Natural;
      Text   : String;
      Before : Integer := -1;
      After  : Integer := -1);
   --  Replace the characters around a certain position. <before> characters
   --  before (line, column), and up to <after> characters after are removed,
   --  and the new text is inserted instead. If <before> or <after> is omitted,
   --  the bounds will be at the beginning and/or the end of the line.
   pragma Inline (Replace_Text);

   procedure Unhighlight
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Pos    : Natural;
      Style  : String := "");
   pragma Inline (Unhighlight);
   --  Remove highlighting of line number Pos in file File

   procedure Unhighlight_Line
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Pos    : Natural;
      Style  : String := "");
   --  Remove highlighting of line number Pos in file File
   pragma Inline (Unhighlight_Line);

   procedure Unhighlight_Range
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      File    : Virtual_File;
      Style   : String;
      Line    : Natural := 0;
      Start_C : Integer := -1;
      End_C   : Integer := -1);
   --  Remove fine highlighting
   pragma Inline (Unhighlight_Range);

end Vdiff2_Module.Utils.Shell_Command;
