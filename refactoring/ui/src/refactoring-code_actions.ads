------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2021-2026, AdaCore                  --
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

--  This package provides the user interface for editor side actions which
--  apply refactorings to the editor.
--
--  This handles
--      - the creation and lifecycle of Messages meant to apply actions
--      - the Action that associates a keyboard shortcut to this
--      - the menu that pops up when clicking on an action

with Basic_Types;  use Basic_Types;
with Commands;     use Commands;
with GNATCOLL.VFS; use GNATCOLL.VFS;
with GPS.Kernel;   use GPS.Kernel;

package Refactoring.Code_Actions is

   procedure Add_Code_Action
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      File     : Virtual_File;
      Line     : Editable_Line_Type;
      Column   : Visible_Column_Type;
      Markup   : String;
      Category : String;
      Command  : Command_Access);
   --  Add a code action at the given location. Markup is an UTF8 pango markup
   --  string, used to display the description of the command being launched.
   --  Command is the command to run if the user activates the code action.
   --  If there are multiple possible actions on the given line, call this
   --  multiple times.
   --  Category is a label representing the LSP.Messages.CodeActionKind if
   --  specified. Set it to an empty string if no kind has been specified.
   --  The code actions are automatically cleared when the location changes.
   --  The ownership of Command is transferred to this module: do not free it.

   procedure Invalidate_Code_Actions
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Invalidate all the previous code actions

   procedure Register_Actions
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the actions and hooks for this engine

end Refactoring.Code_Actions;
