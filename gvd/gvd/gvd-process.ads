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

with Glib;

with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.Expect; use GNAT.Expect;

with Gtk.Object; use Gtk.Object;
with Gdk.Color;
with Gdk.Font;

with Debugger; use Debugger;
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

   type Debugger_Process_Tab_Record is new
     Process_Tab_Pkg.Process_Tab_Record with
   record
      Debugger : Debugger_Access;
       --  The underlying debugger process.

      Edit_Pos : Glib.Guint;
       --  The last position in the text window of the debugger where text
       --  was inserted. This is used to find what was typed by the user.

      Debugger_Text_Highlight_Color : Gdk.Color.Gdk_Color;
       --  Color used for highlighting in the debugger window.

      Debugger_Text_Font : Gdk.Font.Gdk_Font := Gdk.Font.Null_Font;
       --  Font used in the debugger window.

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

   function Convert
     (Descriptor : GNAT.Expect.Process_Descriptor) return Debugger_Process_Tab;
   --  Return the debugger_descriptor associated with a Process_Descriptor.
   --  If no such page is found, an exception Debugger_Not_Found is raised.

   procedure Process_User_Command
     (Debugger : Debugger_Process_Tab;
      Command : String);
   --  Process a command entered by the user.
   --  In most cases, the command is simply transfered asynchronously to the
   --  debugger process. However, commands internal to odd are filtered and
   --  are not transmitted to the debugger.

   procedure Text_Output_Handler
     (Process : Debugger_Process_Tab;
      Str     : String);
   --  Insert Str in the debugger window.
   --  Note that this function does not change the Edit_Pos for the record,
   --  so this should be used only for temporary output.
   --  This also does some highlighting if the debugger has been defined to
   --  support highlighting.

end Odd.Process;
