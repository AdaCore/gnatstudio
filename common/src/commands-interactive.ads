-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003                            --
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

--  This package defines a new kind of command which can be called
--  interactively by the user (bound to keys, menus or toolbars for
--  instance)

with Gdk.Event;

package Commands.Interactive is

   type Interactive_Command is abstract new Root_Command with null record;
   type Interactive_Command_Access is access all Interactive_Command'Class;

   function Execute
     (Command : access Interactive_Command;
      Event   : Gdk.Event.Gdk_Event) return Command_Return_Type is abstract;
   --  Execute the command.
   --  If Event is null, it should be executed non-interactively. Otherwise,
   --  Event is set to the event that started the execution (a Gdk_Key_Event
   --  if started from a key, a Gtk_Button_Event if started from a menu,...)

   function Execute (Command : access Interactive_Command)
      return Command_Return_Type;
   --  Execute the command non-interactively

   procedure Launch_Synchronous_Interactive
     (Command : access Interactive_Command'Class;
      Event   : Gdk.Event.Gdk_Event;
      Wait    : Duration := 0.0);
   --  Execute the command synchronously.
   --  This is similar to Commands.Lauch_Synchronous, except it also propagates
   --  the Event parameter.

end Commands.Interactive;
