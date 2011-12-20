------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2012, AdaCore                     --
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

--  This package handles the GUI part of the task manager

with Glib; use Glib;

with Gtk.Image;                use Gtk.Image;
with Gdk.Pixbuf;               use Gdk.Pixbuf;

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

   function Get_GUI (Manager : Task_Manager_Access) return Gtk_Widget;
   procedure Set_GUI
     (Manager : Task_Manager_Access;
      GUI     : Gtk_Widget);
   --  Get and set the active graphical interface for Manager

   procedure Set_Progress_Area
     (Manager : Task_Manager_Access;
      Area    : Gtk.Box.Gtk_Hbox);
   --  Indicate an area in which progress bars can be displayed

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
   --  Create a new Task_Manager_Interface

   procedure Initialize
     (View    : access Task_Manager_Interface_Record'Class;
      Kernel  : Kernel_Handle;
      Manager : Task_Manager_Access;
      Widget  : Gtk_Widget);
   --  Internal initialization procedure

   procedure Interrupt_Command
     (Manager : access Task_Manager_Record'Class;
      Index   : Integer);
   --  Interrupt command referenced by Index

   procedure Pause_Command
     (Manager : access Task_Manager_Record'Class;
      Index   : Integer);
   --  Pause command referenced by Index

   procedure Resume_Command
     (Manager : access Task_Manager_Record'Class;
      Index   : Integer);
   --  Resume paused command referenced by Index

   function Get_Focus_Widget
     (GUI : Task_Manager_Widget_Access) return Gtk_Widget;
   --  Return the widget that should get the focus by default

private

   type Iter_Array is array (Natural range <>) of Gtk_Tree_Iter;
   type Iter_Array_Access is access Iter_Array;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Iter_Array, Iter_Array_Access);

   type Task_Manager_Widget_Record is new Gtk_Hbox_Record with record
      Model            : Task_Manager_Interface;
      Tree             : Gtk_Tree_View;
      Dialog           : Gtk_Widget := null;
      Quit_Button_Col  : Gtk_Tree_View_Column;
      Pause_Button_Col : Gtk_Tree_View_Column;
   end record;

   package Integer_Stack is new Generic_Stack (Integer);

   package Task_Manager_Source is new Glib.Main.Generic_Sources
     (Task_Manager_Interface);

   type Task_Manager_UI_Record is new Task_Manager_Record with record
      GUI           : Task_Manager_Interface := null;
      Progress_Area : Gtk.Box.Gtk_Hbox := null;
   end record;
   type Task_Manager_UI_Access is access all Task_Manager_UI_Record'Class;

   overriding procedure Queue_Added
     (Manager : access Task_Manager_UI_Record;
      Index   : Integer);
   --  Inform the GUI that a queue has been added

   overriding procedure Queue_Removed
     (Manager : access Task_Manager_UI_Record;
      Index   : Integer);
   --  Inform the GUI that a queue has been removed

   overriding procedure Queue_Changed
     (Manager           : access Task_Manager_UI_Record;
      Index             : Integer;
      Immediate_Refresh : Boolean);
   --  Inform the GUI that the progress or running information of a queue has
   --  been changed. If Immediate_Refresh is True, reflect the changes in the
   --  GUI immediately, otherwise do it in a timeout callback.

   type Task_Manager_Interface_Record is new Gtk_Hbox_Record with record
      Kernel                 : Kernel_Handle;
      Model                  : Gtk_Tree_Model;
      Manager                : Task_Manager_UI_Access;

      Progress_Bar_Button    : Gtk_Button;

      Button_Image           : Gtk_Image;
      Label                  : Gtk_Label;

      Reference_Widget       : Gtk_Widget;
      --  A reference widget to create the graphical contexts

      Close_Button_Pixbuf    : Gdk_Pixbuf;
      Pause_Button_Pixbuf    : Gdk_Pixbuf;
      Play_Button_Pixbuf     : Gdk_Pixbuf;

      To_Refresh             : Integer_Stack.Simple_Stack;

      Timeout_Cb             : Glib.Main.G_Source_Id := Glib.Main.No_Source_Id;
      --  The registered refresh timeout callback

      Main_Progress_Bar      : Gtk_Progress_Bar;
   end record;

   procedure Push_State (Manager : access Task_Manager_Record'Class);
   procedure Pop_State (Manager : access Task_Manager_Record'Class);
   --  Push and pop the "busy" state of the task manager

end Task_Manager.GUI;
