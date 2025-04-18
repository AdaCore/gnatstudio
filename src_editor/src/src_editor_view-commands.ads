------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2003-2023, AdaCore                     --
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

--  This package handles commands related to the editor view

with Commands.Interactive; use Commands.Interactive;

package Src_Editor_View.Commands is

   type Movement_Type is (Word, Paragraph, Line, Char, Page);
   type Move_Command is new Interactive_Command with record
      Kind             : Movement_Type;
      Step             : Integer;
      Extend_Selection : Boolean := False;
   end record;
   overriding function Execute
     (Command : access Move_Command;
      Context : Interactive_Command_Context)
      return Standard.Commands.Command_Return_Type;
   --  This command moves the cursor to a new position

   type Scroll_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Scroll_Command;
      Context : Interactive_Command_Context)
      return Standard.Commands.Command_Return_Type;
   --  This command scrolls the current editor.
   --  Currently, it can only be used to center the cursor.

   type Delete_Command is new Interactive_Command with record
      Kind   : Movement_Type := Word;
      Count  : Integer := 1;  --  Delete backward if negative
   end record;
   overriding function Execute
     (Command : access Delete_Command;
      Context : Interactive_Command_Context)
      return Standard.Commands.Command_Return_Type;
   --  This command deletes some text

   type Formatting_Command is new Interactive_Command with record
      Indent : Boolean := False;
   end record;

   overriding function Execute
     (Command : access Formatting_Command;
      Context : Interactive_Command_Context)
      return Standard.Commands.Command_Return_Type;
   --  This command formats or indent the current line/selection

   type Control_Type is (As_Is, Sticky_As_Is);
   --  As_Is: The next key will be interpreted as-is (no casing/indentation)

   type Control_Command is new Interactive_Command with record
      Mode   : Control_Type;
   end record;
   overriding function Execute
     (Command : access Control_Command;
      Context : Interactive_Command_Context)
      return Standard.Commands.Command_Return_Type;
   --  This is used for control commands. A control command is an action whose
   --  purpose is to modify the status of the current view.

   type Tab_As_Space_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Tab_As_Space_Command;
      Context : Interactive_Command_Context)
      return Standard.Commands.Command_Return_Type;
   --  A command that inserts spaces in the current editor

   type Delete_Tab_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Delete_Tab_Command;
      Context : Interactive_Command_Context)
      return Standard.Commands.Command_Return_Type;
   --  A command that deletes spaces/tabs in the current editor

   type Lock_Or_Unlock_Commmand is new Interactive_Command with record
      Split : Boolean;
      --  When set to True, the editor being locked will be put in a separate
      --  notebook, splitting the MDI in two if needed.
   end record;
   overriding function Execute
     (Command : access Lock_Or_Unlock_Commmand;
      Context : Interactive_Command_Context)
      return Standard.Commands.Command_Return_Type;
   --  Lock or unlock the current editor.

   type Add_String_Comment_Command is
     new Interactive_Command with null record;
   overriding function Execute
     (Command : access Add_String_Comment_Command;
      Context : Interactive_Command_Context)
      return Standard.Commands.Command_Return_Type;
   --  Split string literal on two lines or add new line to continue
   --  string literal or comment

   type Split_String_Command is
     new Interactive_Command with null record;
   overriding function Execute
     (Command : access Split_String_Command;
      Context : Interactive_Command_Context)
      return Standard.Commands.Command_Return_Type;
   --  Split string literal on two parts

   type Paste_Into_String_Command is
     new Interactive_Command with null record;
   overriding function Execute
     (Command : access Paste_Into_String_Command;
      Context : Interactive_Command_Context)
      return Standard.Commands.Command_Return_Type;
   --  Split string literal and paste clipboard content as a variable
   --  between these two parts

end Src_Editor_View.Commands;
