-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
--                              ACT-Europe                           --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
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

with Glib; use Glib;
with Gtk.Item_Factory; use Gtk.Item_Factory;
with Factory_Data;

package GVD.Menu is
   use Factory_Data;

   ---------------
   -- Callbacks --
   ---------------

   procedure On_Open_Program
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);

   procedure On_Open_Debugger
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);

   procedure On_Open_Core_Dump
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);

   procedure On_Add_Symbols
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);

   procedure On_Edit_Source
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);

   procedure On_Open_Source
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);

   procedure On_Reload_Sources
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);

   procedure On_Open_Session
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);

   procedure On_Save_Session_As
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);

   procedure On_Attach_To_Process
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);

   procedure On_Detach_Process
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);

   procedure On_Change_Directory
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);

   procedure On_Close
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);

   procedure On_Exit
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);

   procedure On_Undo
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);

   procedure On_Redo
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);

   procedure On_Cut
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);

   procedure On_Copy
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);

   procedure On_Paste
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);

   procedure On_Select_All
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);

   procedure On_Preferences
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);

   procedure On_Run
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);

   procedure On_Step
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);

   procedure On_Step_Instruction
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);

   procedure On_Next
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);

   procedure On_Next_Instruction
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);

   procedure On_Finish
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);

   procedure On_Continue
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);

   procedure On_Kill
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);

   procedure On_Interrupt
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);

   procedure On_Command_History
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);

   procedure On_Clear_Window
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);

   procedure On_Call_Stack
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);

   procedure On_Threads
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);

   procedure On_Tasks
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);

   procedure On_Edit_Breakpoints
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);

   procedure On_Examine_Memory
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);

   procedure On_Display_Local_Variables
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);

   procedure On_Display_Arguments
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);

   procedure On_Display_Registers
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);

   procedure On_Display_Expression (Object : Data_Type_Access);

   procedure On_Display_Expression
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);

   procedure On_Refresh
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);

   procedure On_Show
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);

   procedure On_Manual
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);

   procedure On_About_GVD
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);

   --------------------
   -- GVD_Menu_Items --
   --------------------

   type Gtk_Item_Factory_Entry_Access is access Gtk_Item_Factory_Entry_Array;

   function GVD_Menu_Items return Gtk_Item_Factory_Entry_Access;
   --  Return a pointer to the Factory_Entry_Array needed to create the
   --  GVD menu items.

end GVD.Menu;
