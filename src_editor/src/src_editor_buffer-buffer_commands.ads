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

--  This package handles commands related to the editor buffer, such
--  as completion commands or delimiter jump commands.

with Gdk.Event; use Gdk.Event;
with Commands;
with Commands.Interactive; use Commands.Interactive;

package Src_Editor_Buffer.Buffer_Commands is

   type Jump_To_Delimiter_Command is new Interactive_Command
      with record
         Kernel : Glide_Kernel.Kernel_Handle;
      end record;
   function Execute
     (Command : access Jump_To_Delimiter_Command; Event : Gdk.Event.Gdk_Event)
      return Command_Return_Type;
   --  This commands jmps to the next delimiter for the one currently
   --  under the cursor.

   type Completion_Command is new Interactive_Command with record
      Kernel : Glide_Kernel.Kernel_Handle;
   end record;
   function Execute
     (Command : access Completion_Command; Event : Gdk.Event.Gdk_Event)
      return Command_Return_Type;
   --  This command completes the word under the cursor based on the
   --  contents of the buffer.

end Src_Editor_Buffer.Buffer_Commands;
