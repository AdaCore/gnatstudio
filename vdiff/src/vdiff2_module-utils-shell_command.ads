-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2003                         --
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

--  This package provides wraper for shell command.

package Vdiff2_Module.Utils.Shell_Command is

   function Add_Line
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Pos    : Natural;
      Style  : String := "";
      Number : Natural := 1) return String;
   --  Add a blank line at line Pos of a given file editor,
   --  using Style for color.
   --  Return corresponding Mark.
   pragma Inline (Add_Line);

   procedure Edit
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File   : Virtual_File);
   --  Open editor for File
   pragma Inline (Edit);

   function Get_Chars
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
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
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File : Virtual_File) return Natural;
   --  Return the number of line in file File
   pragma Inline (Get_File_Last_Line);

   function Get_Line_Number
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Mark   : String) return Natural;
   --  Returns the current line of Mark
   pragma Inline (Get_Line_Number);

   procedure Goto_Difference
     (Kernel : Kernel_Handle;
      Link   : Diff_Chunk_Access);
   pragma Inline (Goto_Difference);

   procedure Delete_Mark
     (Kernel : Kernel_Handle;
      Link   : String);
   pragma Inline (Delete_Mark);

   procedure Highlight_Line
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Pos    : Natural;
      Style  : String := "";
      Number : Natural := 1);
   --  Color a line at line Pos in a given file editor, using Style for color
   pragma Inline (Highlight_Line);

   procedure Highlight_Range
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File    : Virtual_File;
      Style   : String;
      Line    : Natural := 0;
      Start_C : Integer := -1;
      End_C   : Integer := -1);
   --  Highlights a portion of a line in a file with the given category
   pragma Inline (Highlight_Range);

   function Mark_Diff_Block
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File  : Virtual_File;
      Pos   : Natural) return String;
   --  Return the mark corresponding the begining of line number Pos
   pragma Inline (Mark_Diff_Block);

   procedure Register_Highlighting
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Register color preferences

   procedure Remove_Blank_Lines
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Mark   : in out String_Access);
   --  Remove blank lines located at mark
   pragma Inline (Remove_Blank_Lines);

   procedure Replace_Text
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
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
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File  : Virtual_File;
      Pos   : Natural;
      Style : String := "");
   pragma Inline (Unhighlight);
   --  Remove highlighting of line number Pos in file File

   procedure Unhighlight_Line
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File  : Virtual_File;
      Pos   : Natural;
      Style : String := "");
   --  Remove highlighting of line number Pos in file File
   pragma Inline (Unhighlight_Line);

   procedure Unhighlight_Range
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File    : Virtual_File;
      Style   : String;
      Line    : Natural := 0;
      Start_C : Integer := -1;
      End_C   : Integer := -1);
   --  Remove fine highlighting
   pragma Inline (Unhighlight_Range);


end Vdiff2_Module.Utils.Shell_Command;
