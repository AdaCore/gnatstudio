------------------------------------------------------------------------------
--                      GVD - The GNU Visual Debugger                       --
--                                                                          --
--                     Copyright (C) 2000-2018, AdaCore                     --
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

--  Shows a list of processes that can be attached to

with GVD.Process;         use GVD.Process;
with Gtk.Dialog;          use Gtk.Dialog;
with Gtk.Tree_View;       use Gtk.Tree_View;
with Gtk.Tree_Store;      use Gtk.Tree_Store;
with Gtk.GEntry;          use Gtk.GEntry;

package GVD.Process_Lists is

   type Process_List_Record is new Gtk_Dialog_Record with private;
   type Process_List is access all Process_List_Record'Class;

   procedure Gtk_New
     (Self    : out Process_List;
      Process : not null access Visual_Debugger_Record'Class);
   --  Create a new dialog

   function Get_Selection
     (Self  : not null access Process_List_Record)
      return String;
   --  Display the dialog and let the user select a new process to attach to.
   --  Returns "" if the user clicked on the Cancel button, otherwise returns
   --  the PID of the process.
   --  The dialog is not destroyed automatically.

private

   type Process_List_Record is new Gtk_Dialog_Record with record
      Tree_Model     : Gtk_Tree_Store;
      Tree_View      : Gtk_Tree_View;
      Ent            : Gtk_Entry;
   end record;

end GVD.Process_Lists;
