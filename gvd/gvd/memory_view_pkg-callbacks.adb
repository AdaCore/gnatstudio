-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2001-2007                      --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Exceptions;    use Ada.Exceptions;
with GNAT.Strings;

with Gdk.Event;         use Gdk.Event;
with Gdk.Types.Keysyms; use Gdk.Types.Keysyms;
with Gdk.Types;         use Gdk.Types;

with Glib;              use Glib;

with Gtk.Arguments;     use Gtk.Arguments;
with Gtk.Handlers;      use Gtk.Handlers;
with Gtk.Text_Buffer;   use Gtk.Text_Buffer;
with Gtk.Text_Iter;     use Gtk.Text_Iter;

with GVD.Memory_View;   use GVD.Memory_View;
with Traces;            use Traces;

package body Memory_View_Pkg.Callbacks is

   use type GNAT.Strings.String_Access;

   -------------------------------
   -- On_Address_Entry_Activate --
   -------------------------------

   procedure On_Address_Entry_Activate
     (Object : access Gtk_Entry_Record'Class)
   is
      View : constant GVD_Memory_View :=
               GVD_Memory_View (Get_Toplevel (Object));
   begin
      Display_Memory (View, Get_Text (View.Address_Entry));

   exception
      when E : others => Trace (Exception_Handle, E);
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

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Address_View_Clicked;

   ---------------------------
   -- On_Size_Entry_Changed --
   ---------------------------

   procedure On_Size_Entry_Changed
     (Object : access Gtk_Entry_Record'Class)
   is
      View : constant GVD_Memory_View :=
               GVD_Memory_View (Get_Toplevel (Object));
   begin
      Update_Display (View);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Size_Entry_Changed;

   ---------------------------
   -- On_Data_Entry_Changed --
   ---------------------------

   procedure On_Data_Entry_Changed
     (Object : access Gtk_Entry_Record'Class)
   is
      View : constant GVD_Memory_View :=
               GVD_Memory_View (Get_Toplevel (Object));
   begin
      Update_Display (View);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Data_Entry_Changed;

   ---------------------------
   -- On_Show_Ascii_Toggled --
   ---------------------------

   procedure On_Show_Ascii_Toggled
     (Object : access Gtk_Check_Button_Record'Class)
   is
      View : constant GVD_Memory_View :=
        GVD_Memory_View (Get_Toplevel (Object));
   begin
      Update_Display (View);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Show_Ascii_Toggled;

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

   exception
      when E : others => Trace (Exception_Handle, E);
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

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Pgdn_Clicked;

   -----------------------------
   -- On_View_Key_Press_Event --
   -----------------------------

   function On_View_Key_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : GValues) return Boolean
   is
      View  : constant GVD_Memory_View :=
                GVD_Memory_View (Get_Toplevel (Object));
      Arg1  : Gdk_Event;
      Proxy : constant C_Proxy := Get_Proxy (Nth (Params, 1));

   begin
      if Proxy = null then
         return False;
      else
         Arg1 := Gdk_Event (Proxy);
      end if;

      if Arg1 = null
        or else Get_Event_Type (Arg1) /= Key_Press
      then
         return False;
      end if;

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

   exception
      --  On windows, it seems that pressing the control key generates
      --  an event for which Get_String is invalid

      when Invalid_Field =>
         return False;

      when E : others => Trace (Exception_Handle, E);
         return False;
   end On_View_Key_Press_Event;

   -------------------------
   -- On_View_Move_Cursor --
   -------------------------

   procedure On_View_Move_Cursor
     (Object : access Gtk_Text_View_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      pragma Unreferenced (Object, Params);
   begin
      null;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_View_Move_Cursor;

   ----------------------------------
   -- On_View_Button_Release_Event --
   ----------------------------------

   function On_View_Button_Release_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      pragma Unreferenced (Params);

      View : constant GVD_Memory_View :=
               GVD_Memory_View (Get_Toplevel (Object));
      Start_Iter : Gtk_Text_Iter;
      End_Iter   : Gtk_Text_Iter;
      Result     : Boolean;
   begin
      if View.Values = null then
         return False;
      end if;

      Get_Selection_Bounds
        (Get_Buffer (View.View), Start_Iter, End_Iter, Result);

      if Result = False then
         Watch_Cursor_Location (View);
      end if;

      return False;

   exception
      when E : others => Trace (Exception_Handle, E);
      return False;
   end On_View_Button_Release_Event;

   ----------------------
   -- On_Reset_Clicked --
   ----------------------

   procedure On_Reset_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
      View : constant GVD_Memory_View :=
               GVD_Memory_View (Get_Toplevel (Object));
   begin
      GNAT.Strings.Free (View.Flags);
      View.Flags := new String'(View.Values.all);
      Update_Display (View);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Reset_Clicked;

   -----------------------
   -- On_Submit_Clicked --
   -----------------------

   procedure On_Submit_Clicked (Object : access Gtk_Button_Record'Class) is
      View : constant GVD_Memory_View :=
        GVD_Memory_View (Get_Toplevel (Object));
   begin
      Apply_Changes (View);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Submit_Clicked;

   ----------------------
   -- On_Close_Clicked --
   ----------------------

   procedure On_Close_Clicked (Object : access Gtk_Button_Record'Class) is
   begin
      Destroy (Get_Toplevel (Object));

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Close_Clicked;

   -----------------------
   -- On_Button_Release --
   -----------------------

   function On_Button_Release
     (Object : access Gtk_Entry_Record'Class;
      Params : GValues) return Boolean
   is
      pragma Unreferenced (Params);
      View : constant GVD_Memory_View :=
               GVD_Memory_View (Get_Toplevel (Object));
   begin
      Update_Display (View);

      return False;
   end On_Button_Release;

end Memory_View_Pkg.Callbacks;
