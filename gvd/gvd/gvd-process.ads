-----------------------------------------------------------------------
--                 Odd - The Other Display Debugger                  --
--                                                                   --
--                         Copyright (C) 2000                        --
--                 Emmanuel Briot and Arnaud Charlet                 --
--                                                                   --
-- Odd is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with System;

with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.Expect; use GNAT.Expect;

with Gtk.Window; use Gtk.Window;

with Debugger; use Debugger;
with Language; use Language;

package Odd.Process is

   type Debugger_Descriptor is record
      Debugger : Debugger_Access;
      Window   : Gtk_Window;
   end record;
   --  This type holds all the informations related to a given debugger.

   procedure Create_Debugger
     (Window : access Gtk_Window_Record'Class;
      Params : Argument_List);
   --  Create a debugger with a given list of arguments.

   procedure Send_Command (Debugger : Debugger_Descriptor; Command : String);
   --  Send a given command to the debugger.
   --  If Command is internal, execute it without actually sending it.

   --   - Asynchronous: do not wait for an answer

   --  procedure Send_Command
   --    (Debugger : Debugger_Descriptor;
   --     Command  : String;
   --     Result   : out String;
   --     Len      : out Natural;
   --     Pool     : Boolean := True);
   --  Send a given command to the debugger.
   --  If Command is internal, execute it without actually sending it.
   --  Return the answer from the debugger in Result.
   --  If Pool, poll for UI events while waiting.

end Odd.Process;
