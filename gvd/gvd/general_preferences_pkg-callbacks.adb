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

with Gtk.Widget;             use Gtk.Widget;
with GVD.Main_Window;        use GVD.Main_Window;
with GVD.Preferences;        use GVD.Preferences;
with GVD.Preferences_Dialog; use GVD.Preferences_Dialog;
with GNAT.OS_Lib;            use GNAT.OS_Lib;

package body General_Preferences_Pkg.Callbacks is

   use Gtk.Arguments;

   -------------------------------------
   -- On_Gvd_Preferences_Delete_Event --
   -------------------------------------

   function On_Gvd_Preferences_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      --  Arg1 : Gdk_Event := To_Event (Params, 1);
   begin
      Hide (Object);
      return True;
   end On_Gvd_Preferences_Delete_Event;

   --------------------------
   -- On_Ok_Button_Clicked --
   --------------------------

   procedure On_Ok_Button_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Pref : GVD_Preferences_Access := GVD_Preferences_Access (Object);
   begin
      Set_Preferences (Pref);
      Hide (Get_Toplevel (Object));
      Preferences_Changed (GVD_Main_Window (Pref.Main_Window));
      Save_Preferences
        (GVD_Main_Window (Pref.Main_Window).Home_Dir.all
         & Directory_Separator & "preferences");
   end On_Ok_Button_Clicked;

   ----------------------------
   -- On_Test_Button_Clicked --
   ----------------------------

   procedure On_Test_Button_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Pref : GVD_Preferences_Access := GVD_Preferences_Access (Object);
   begin
      Apply_Preferences (Pref);
      Preferences_Changed (GVD_Main_Window (Pref.Main_Window));
   end On_Test_Button_Clicked;

   ------------------------------
   -- On_Cancel_Button_Clicked --
   ------------------------------

   procedure On_Cancel_Button_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Pref : GVD_Preferences_Access := GVD_Preferences_Access (Object);
   begin
      Cancel_Preferences (Pref);
      Hide (Get_Toplevel (Object));
      Preferences_Changed (GVD_Main_Window (Pref.Main_Window));
   end On_Cancel_Button_Clicked;

end General_Preferences_Pkg.Callbacks;
