-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2003-2009, AdaCore                 --
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
with Gtk.Image;                use Gtk.Image;
with Gdk.Pixbuf;               use Gdk.Pixbuf;

with Pango.Layout;             use Pango.Layout;

with Gtk.Label;                use Gtk.Label;
with Gtk.Button;               use Gtk.Button;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Progress_Bar;         use Gtk.Progress_Bar;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;

with Ada.Unchecked_Deallocation;
with Gtk.Tree_View; use Gtk.Tree_View;

with GPS.Kernel; use GPS.Kernel;

with Generic_Stack;
with Glib.Main;

package Task_Manager.GUI is

   function Create
     (Kernel : Kernel_Handle;
      Widget : Gtk_Widget) return Task_Manager_Access;
   --  ??? Missing documentation

   type Task_Manager_Widget_Record is new Gtk_Hbox_Record with private;
   type Task_Manager_Widget_Access is access all
     Task_Manager_Widget_Record'Class;

   type Task_Manager_Interface_Record is new Gtk_Hbox_Record with private;
   type Task_Manager_Interface is access all
     Task_Manager_Interface_Record'Class;

   function Task_Manager_Dialog
     (Manager : Task_Manager_Access;
      Dialog  : Gtk_Widget := null) return Task_Manager_Widget_Access;
   --  Create a new Task_Manager tree view. If Dialog is non-null, then the
   --  view will be destroyed when there are no more running tasks.

   procedure Gtk_New
     (View    : out Task_Manager_Interface;
      Kernel  : Kernel_Handle;
      Manager : Task_Manager_Access;
      Widget  : Gtk_Widget);
   --  Create a new Task_Manager_Interface.

   procedure Initialize
     (View    : access Task_Manager_Interface_Record'Class;
      Kernel  : Kernel_Handle;
      Manager : Task_Manager_Access;
      Widget  : Gtk_Widget);
   --  Internal initialization procedure.

   procedure Interrupt_Command
     (Manager : Task_Manager_Access;
      Index   : Integer);
   --  Interrupt command referenced by Index.

   procedure Pause_Command
     (Manager : Task_Manager_Access;
      Index   : Integer);
   --  Pause command referenced by Index.

   procedure Queue_Added
     (GUI   : Task_Manager_Interface;
      Index : Integer);
   --  Inform the GUI that a queue has been added

   procedure Queue_Removed
     (GUI   : Task_Manager_Interface;
      Index : Integer);
   --  Inform the GUI that a queue has been removed

   procedure Queue_Changed
     (GUI               : Task_Manager_Interface;
      Index             : Integer;
      Immediate_Refresh : Boolean);
   --  Inform the GUI that the progress or running information of a queue has
   --  been changed. If Immediate_Refresh is True, reflect the changes in the
   --  GUI immediately, otherwise do it in a timeout callback.

private

   type Iter_Array is array (Natural range <>) of Gtk_Tree_Iter;
   type Iter_Array_Access is access Iter_Array;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Iter_Array, Iter_Array_Access);

   type Task_Manager_Widget_Record is new Gtk_Hbox_Record with record
      Model      : Task_Manager_Interface;
      Tree       : Gtk_Tree_View;
      Dialog     : Gtk_Widget := null;
      Quit_Button_Col  : Gtk_Tree_View_Column;
      Pause_Button_Col : Gtk_Tree_View_Column;
   end record;

   package Integer_Stack is new Generic_Stack (Integer);

   package Task_Manager_Source is new Glib.Main.Generic_Sources
     (Task_Manager_Interface);

   type Task_Manager_Interface_Record is new Gtk_Hbox_Record with record
      Kernel                 : Kernel_Handle;
      Model                  : Gtk_Tree_Model;
      Manager                : Task_Manager_Access;

      Progress_Bar_Button    : Gtk_Button;

      Button_Image           : Gtk_Image;
      Label                  : Gtk_Label;

      Progress_Template      : Gdk_Pixmap;
      Progress_Background_GC : Gdk_GC;
      Progress_Foreground_GC : Gdk_GC;
      Progress_Text_GC       : Gdk_GC;
      Progress_Width         : Gint;
      Progress_Height        : Gint;

      Progress_Image         : Gtk_Image;
      Progress_Layout        : Pango_Layout;

      Reference_Widget       : Gtk_Widget;
      --  A reference widget to create the graphical contexts.

      Close_Button_Pixbuf     : Gdk_Pixbuf;
      Pause_Button_Pixbuf     : Gdk_Pixbuf;
      Play_Button_Pixbuf      : Gdk_Pixbuf;

      To_Refresh               : Integer_Stack.Simple_Stack;

      Timeout_Cb               : Glib.Main.G_Source_Id :=
        Glib.Main.No_Source_Id;
      --  The registered refresh timeout callback.

      Main_Progress_Bar        : Gtk_Progress_Bar;
   end record;

   procedure Push_State (Manager : Task_Manager_Access);
   procedure Pop_State (Manager : Task_Manager_Access);
   --  Push and pop the "busy" state of the task manager.

end Task_Manager.GUI;
