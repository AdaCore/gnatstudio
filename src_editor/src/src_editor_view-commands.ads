-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                       Copyright (C) 2003                          --
--                            ACT-Europe                             --
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
with Glide_Kernel;
with Gdk.Event;

package Src_Editor_View.Commands is

   type Movement_Type is (Word, Paragraph);
   type Move_Command is new Interactive_Command with record
      Kernel : Glide_Kernel.Kernel_Handle;
      Kind   : Movement_Type;
      Step   : Integer;
   end record;
   function Execute
     (Command : access Move_Command; Event : Gdk.Event.Gdk_Event)
      return Standard.Commands.Command_Return_Type;
   --  This command moves the cursor to a new position

end Src_Editor_View.Commands;
