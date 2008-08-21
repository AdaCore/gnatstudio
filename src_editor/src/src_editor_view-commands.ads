-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                   Copyright (C) 2003-2008, AdaCore                --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
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

--  This package handles commands related to the editor view

with Commands.Interactive; use Commands.Interactive;
with GPS.Kernel;

package Src_Editor_View.Commands is

   type Movement_Type is (Word, Paragraph, Line, Char, Page);
   type Move_Command is new Interactive_Command with record
      Kernel : GPS.Kernel.Kernel_Handle;
      Kind   : Movement_Type;
      Step   : Integer;
   end record;
   overriding function Execute
     (Command : access Move_Command;
      Context : Interactive_Command_Context)
      return Standard.Commands.Command_Return_Type;
   --  This command moves the cursor to a new position

   type Scroll_Command is new Interactive_Command with record
      Kernel : GPS.Kernel.Kernel_Handle;
   end record;
   overriding function Execute
     (Command : access Scroll_Command;
      Context : Interactive_Command_Context)
      return Standard.Commands.Command_Return_Type;
   --  This command scrolls the current editor.
   --  Currently, it can only be used to center the cursor.

   type Delete_Command is new Interactive_Command with record
      Kernel : GPS.Kernel.Kernel_Handle;
      Kind   : Movement_Type := Word;
      Count  : Integer := 1;  --  Delete backward if negative
   end record;
   overriding function Execute
     (Command : access Delete_Command;
      Context : Interactive_Command_Context)
      return Standard.Commands.Command_Return_Type;
   --  This command deletes some text

   type Indentation_Command is new Interactive_Command with record
      Kernel : GPS.Kernel.Kernel_Handle;
   end record;
   overriding function Execute
     (Command : access Indentation_Command;
      Context : Interactive_Command_Context)
      return Standard.Commands.Command_Return_Type;
   --  This command reindents the current line

   type Control_Type is (As_Is);
   --  As_Is: The next key will be interpreted as-is (no casing/indentation)

   type Control_Command is new Interactive_Command with record
      Kernel : GPS.Kernel.Kernel_Handle;
      Mode   : Control_Type;
   end record;
   overriding function Execute
     (Command : access Control_Command;
      Context : Interactive_Command_Context)
      return Standard.Commands.Command_Return_Type;
   --  This is used for control commands. A control command is an action whose
   --  purpose is to modify the status of the current view.

   type Tab_As_Space_Command is new Interactive_Command with record
      Kernel : GPS.Kernel.Kernel_Handle;
   end record;
   overriding function Execute
     (Command : access Tab_As_Space_Command;
      Context : Interactive_Command_Context)
      return Standard.Commands.Command_Return_Type;
   --  A command that inserts spaces in the current editor

end Src_Editor_View.Commands;
