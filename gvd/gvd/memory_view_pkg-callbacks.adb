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

with Glib;

with Gdk.Event;   use Gdk.Event;

with Gdk.Types;         use Gdk.Types;
with Gdk.Types.Keysyms; use Gdk.Types.Keysyms;
with Gtk.Arguments;     use Gtk.Arguments;
with Gtk.Widget;        use Gtk.Widget;
with Gtk.GEntry;        use Gtk.GEntry;
with Gtk.Handlers;      use Gtk.Handlers;

with GVD.Memory_View;   use GVD.Memory_View;
with Basic_Types;       use Basic_Types;

package body Memory_View_Pkg.Callbacks is

   use Gtk.Arguments;

   ---------------------------------
   -- On_Memory_View_Delete_Event --
   ---------------------------------

   function On_Memory_View_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
   begin
      Hide (Get_Toplevel (Object));
      return True;
   end On_Memory_View_Delete_Event;

   ----------------------------------
   -- On_Memory_View_Size_Allocate --
   ----------------------------------

   procedure On_Memory_View_Size_Allocate
     (Object : access Gtk_Window_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      View : GVD_Memory_View := GVD_Memory_View (Get_Toplevel (Object));
   begin
      Update_Display (View);
   end On_Memory_View_Size_Allocate;

   -------------------------------
   -- On_Address_Entry_Activate --
   -------------------------------

   procedure On_Address_Entry_Activate
     (Object : access Gtk_Entry_Record'Class)
   is
      View : GVD_Memory_View := GVD_Memory_View (Get_Toplevel (Object));
   begin
      Display_Memory (View, Get_Text (View.Address_Entry));
   end On_Address_Entry_Activate;

   -----------------------------
   -- On_Address_View_Clicked --
   -----------------------------

   procedure On_Address_View_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
      View : constant GVD_Memory_View :=
        GVD_Memory_View (Get_Toplevel (Object));
   begin
      Display_Memory (View, Get_Text (View.Address_Entry));
   end On_Address_View_Clicked;

   ---------------------------
   -- On_Size_Entry_Changed --
   ---------------------------

   procedure On_Size_Entry_Changed
     (Object : access Gtk_Entry_Record'Class)
   is
      View : GVD_Memory_View := GVD_Memory_View (Get_Toplevel (Object));
   begin
      Update_Display (View);
   end On_Size_Entry_Changed;

   ---------------------------
   -- On_Data_Entry_Changed --
   ---------------------------

   procedure On_Data_Entry_Changed
     (Object : access Gtk_Entry_Record'Class)
   is
      View : GVD_Memory_View := GVD_Memory_View (Get_Toplevel (Object));
   begin
      Update_Display (View);
   end On_Data_Entry_Changed;

   -----------------------------
   -- On_View_Key_Press_Event --
   -----------------------------

   function On_View_Key_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      View : GVD_Memory_View := GVD_Memory_View (Get_Toplevel (Object));
      Arg1 : Gdk_Event := To_Event (Params, 1);
   begin
      case Get_Key_Val (Arg1) is
         when GDK_Right =>
            Move_Cursor (View, Right);
         when GDK_Left =>
            Move_Cursor (View, Left);
         when GDK_Up =>
            Move_Cursor (View, Up);
         when GDK_Down =>
            Move_Cursor (View, Down);
         when GDK_BackSpace | GDK_Clear | GDK_Delete =>
            Emit_Stop_By_Name (View.View, "key_press_event");
         when GDK_Page_Up | GDK_KP_Page_Up =>
            Page_Up (View);
         when GDK_Page_Down | GDK_KP_Page_Down =>
            Page_Down (View);
         when others =>
            Emit_Stop_By_Name (View.View, "key_press_event");
            if Get_String (Arg1)'Length /= 0 then
               Insert (View, Get_String (Arg1));
            end if;
      end case;

      return False;
   end On_View_Key_Press_Event;

   ---------------------
   -- On_Pgup_Clicked --
   ---------------------

   procedure On_Pgup_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
      View : constant GVD_Memory_View :=
        GVD_Memory_View (Get_Toplevel (Object));
   begin
      Page_Up (View);
   end On_Pgup_Clicked;

   ---------------------
   -- On_Pgdn_Clicked --
   ---------------------

   procedure On_Pgdn_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
      View : constant GVD_Memory_View :=
        GVD_Memory_View (Get_Toplevel (Object));
   begin
      Page_Down (View);
   end On_Pgdn_Clicked;

   -------------------------
   -- On_View_Move_Cursor --
   -------------------------

   procedure On_View_Move_Cursor
     (Object : access Gtk_Text_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
   begin
      null;
   end On_View_Move_Cursor;

   ----------------------------------
   -- On_View_Button_Release_Event --
   ----------------------------------

   function On_View_Button_Release_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      View : GVD_Memory_View := GVD_Memory_View (Get_Toplevel (Object));
      use type Glib.Gint;
      use type Glib.Guint;
   begin
      if View.Values = null then
         return False;
      end if;

      if Get_Selection_End_Pos (View.View)
        = Get_Selection_Start_Pos (View.View)
      then
         Watch_Cursor_Location (View);
      end if;
      return True;
   end On_View_Button_Release_Event;

   --------------------------------
   -- On_View_Button_Press_Event --
   --------------------------------

   function On_View_Button_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
   begin
      return False;
   end On_View_Button_Press_Event;

   ----------------------
   -- On_Reset_Clicked --
   ----------------------

   procedure On_Reset_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
      View : GVD_Memory_View := GVD_Memory_View (Get_Toplevel (Object));
   begin
      Free (View.Flags);
      View.Flags := new String' (View.Values.all);
      Update_Display (View);
   end On_Reset_Clicked;

   -----------------------
   -- On_Submit_Clicked --
   -----------------------

   procedure On_Submit_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
      View : GVD_Memory_View := GVD_Memory_View (Get_Toplevel (Object));
   begin
      Apply_Changes (View);
   end On_Submit_Clicked;

   -----------------------
   -- On_Cancel_Clicked --
   -----------------------

   procedure On_Cancel_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      Hide (Get_Toplevel (Object));
   end On_Cancel_Clicked;

   ---------------------------
   -- On_Show_Ascii_Toggled --
   ---------------------------

   procedure On_Show_Ascii_Toggled
     (Object : access Gtk_Check_Button_Record'Class)
   is
      View : GVD_Memory_View := GVD_Memory_View (Get_Toplevel (Object));
   begin
      Update_Display (View);
   end On_Show_Ascii_Toggled;

end Memory_View_Pkg.Callbacks;
