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

with Gtk.Arguments;
with Gtk.Widget; use Gtk.Widget;

package Memory_View_Pkg.Callbacks is
   function On_Memory_View_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;

   procedure On_Memory_View_Size_Allocate
     (Object : access Gtk_Window_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure On_Address_Entry_Activate
     (Object : access Gtk_Entry_Record'Class);

   procedure On_Address_View_Clicked
     (Object : access Gtk_Button_Record'Class);

   procedure On_Size_Entry_Changed
     (Object : access Gtk_Entry_Record'Class);

   procedure On_Data_Entry_Changed
     (Object : access Gtk_Entry_Record'Class);

   procedure On_Show_Ascii_Toggled
     (Object : access Gtk_Check_Button_Record'Class);

   procedure On_Pgup_Clicked
     (Object : access Gtk_Button_Record'Class);

   procedure On_Pgdn_Clicked
     (Object : access Gtk_Button_Record'Class);

   function On_View_Key_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;

   procedure On_View_Move_Cursor
     (Object : access Gtk_Text_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   function On_View_Button_Release_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;

   function On_View_Button_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;

   procedure On_Reset_Clicked
     (Object : access Gtk_Button_Record'Class);

   procedure On_Submit_Clicked
     (Object : access Gtk_Button_Record'Class);

   procedure On_Cancel_Clicked
     (Object : access Gtk_Button_Record'Class);

   procedure On_Help_Clicked
     (Object : access Gtk_Button_Record'Class);

end Memory_View_Pkg.Callbacks;
