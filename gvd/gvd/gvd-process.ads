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
with Glib;

with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.Expect; use GNAT.Expect;

with Gtk.Window; use Gtk.Window;
with Gtk.Object; use Gtk.Object;

with Debugger; use Debugger;
with Language; use Language;
with Process_Tab_Pkg;

package Odd.Process is

   ---------------------------
   -- Debugger_Processs_Tab --
   ---------------------------
   --  This type represents one of the tabs in the process notebook, and
   --  its associated debugger session.
   --  This is the graphical part of the Debugger.* packages, and all graphic
   --  subprogram calls should be done on that type instead of on a
   --  Debugger'Class.
   --  Note also that the real contents of the notebook page is not the
   --  Debugger_Process_Tab_Record itself, but rather its Process_Paned
   --  field.

   type Debugger_Process_Tab_Record is new Process_Tab_Pkg.Process_Tab_Record
     with record
        Debugger : Debugger_Access;
        --  The underlying debugger process.

        Edit_Pos : Glib.Guint;
        --  The last position in the text window of the debugger where text
        --  was inserted. This is used to find what was typed by the user.

     end record;
   type Debugger_Process_Tab is access all Debugger_Process_Tab_Record'Class;


   package Process_User_Data is new User_Data (Debugger_Process_Tab);
   --  This is used to convert from the notebook page associated with the
   --  debugger and the Debugger_Process_Tab structure.
   --  ??? This would not be required if Process_Tab_Record was directly
   --  a Gtk_VPaned, instead of a toplevel window.


   function Create_Debugger
     (Params       : Argument_List;
      Process_Name : String := "")
     return Debugger_Process_Tab;
   --  Create a debugger with a given list of arguments.
   --  A new page is added to the main notebook. Process_Name is used as the
   --  name of the tab, or if it is the empty string an automatic name is
   --  chosen.
   --  This function returns a Process_Tab_Access.

   Debugger_Not_Found : exception;

   function Convert (Pid : GNAT.Expect.Pipes_Id)
                    return Debugger_Process_Tab;
   --  Return the debugger_descriptor associated with a pipes_id.
   --  If no such page is found, an exception Debugger_Not_Found is raised.

   procedure Send_Command (Debugger : Debugger_Process_Tab; Command : String);
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
