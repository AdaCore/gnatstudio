-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2003-2007                      --
--                              AdaCore                              --
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

--  This package handles the GUI part of the task manager.

with Glib; use Glib;

with Gdk.GC;     use Gdk.GC;
with Gdk.Pixmap; use Gdk.Pixmap;

with Gtk.Box;                  use Gtk.Box;
with Gtkada.Tree_View;         use Gtkada.Tree_View;
with Gtk.Tree_Model;           use Gtk.Tree_Model;

with Pango.Layout;             use Pango.Layout;

with Ada.Unchecked_Deallocation;

package Task_Manager.GUI is

   type Task_Manager_Interface_Record is new Gtk_Hbox_Record with private;
   type Task_Manager_Interface is access all
     Task_Manager_Interface_Record'Class;

   procedure Gtk_New
     (View    : out Task_Manager_Interface;
      Manager : Task_Manager_Access;
      Dialog  : Gtk_Widget := null);
   --  Create a new Task_Manager_Interface. If Dialog is non-null, then it
   --  will be destroyed when there are no more running tasks.

   procedure Initialize
     (View    : access Task_Manager_Interface_Record'Class;
      Manager : Task_Manager_Access;
      Dialog  : Gtk_Widget := null);
   --  Internal initialization procedure.

   procedure Refresh (Manager : Task_Manager_Access);
   --  Refresh the information in View from the Task_Manager.

   procedure Interrupt_Command
     (Manager : Task_Manager_Access;
      Index   : Integer);
   --  Interrupt command referenced by Index.

private

   type Iter_Array is array (Natural range <>) of Gtk_Tree_Iter;
   type Iter_Array_Access is access Iter_Array;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Iter_Array, Iter_Array_Access);

   type Task_Manager_Interface_Record is new Gtk_Hbox_Record with record
      Tree    : Tree_View;
      Manager : Task_Manager_Access;

      Lines   : Iter_Array_Access;

      Dialog  : Gtk_Widget := null;

      Progress_Background_GC : Gdk_GC;
      Progress_Foreground_GC : Gdk_GC;
      Progress_Text_GC       : Gdk_GC;
      Progress_Template      : Gdk_Pixmap;

      Progress_Width         : Gint;
      Progress_Height        : Gint;

      Progress_Layout        : Pango_Layout;
   end record;

   procedure Push_State (Manager : Task_Manager_Access);
   procedure Pop_State (Manager : Task_Manager_Access);
   --  Push and pop the "busy" state of the task manager.

end Task_Manager.GUI;
