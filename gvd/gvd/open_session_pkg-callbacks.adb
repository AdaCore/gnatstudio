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

with Gtk.Main; use Gtk.Main;
with Gtk.Label; use Gtk.Label;
with Gtk; use Gtk;
with Gtk.List; use Gtk.List;
with Open_Session_Pkg; use Open_Session_Pkg;
with Gtk.Container; use Gtk.Container;
with Gtk.Widget; use Gtk.Widget;

package body Open_Session_Pkg.Callbacks is

   use Gtk.Arguments;

   --------------------------
   -- On_List_Select_Child --
   --------------------------

   procedure On_List_Select_Child
     (Object : access Gtk_List_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Open : Open_Session_Access :=
        Open_Session_Access (Get_Toplevel (Object));
      Arg1 : Gtk_Widget := Gtk_Widget (To_Object (Params, 1));
      use Widget_List;

      Text : String :=
        Get (Gtk_Label (Get_Data (Children (Gtk_Container (Arg1)))));

   begin
      Set_Text (Open.Entry1, Text);

      if not Open.Lock_Buttons then
         Remove_All_Buttons (Open);
         Create_Buttons (Open, Text);
      end if;
   end On_List_Select_Child;

   ------------------------------
   -- On_Cancel_Button_Clicked --
   ------------------------------

   procedure On_Cancel_Button_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      Set_Text (Open_Session_Access (Get_Toplevel (Object)).Entry1, "");
      Main_Quit;
      Hide_All (Get_Toplevel (Object));
   end On_Cancel_Button_Clicked;

   ---------------------------
   -- On_Select_All_Clicked --
   ---------------------------

   procedure On_Select_All_Clicked
     (Object : access Gtk_Button_Record'Class)
     is
      Open : Open_Session_Access
        := Open_Session_Access (Get_Toplevel (Object));
      Button : Button_Link := Open.First_Button;
   begin
      while Button /= null loop
         Set_Active (Button.Button, True);
         Button := Button.Next;
      end loop;
   end On_Select_All_Clicked;

   -----------------------------
   -- On_Unselect_All_Clicked --
   -----------------------------

   procedure On_Unselect_All_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
      Open : Open_Session_Access
        := Open_Session_Access (Get_Toplevel (Object));
      Button : Button_Link := Open.First_Button;
   begin
      while Button /= null loop
         Set_Active (Button.Button, False);
         Button := Button.Next;
      end loop;
   end On_Unselect_All_Clicked;

   ------------------------------
   -- On_Ok_Button_Clicked --
   ------------------------------

   procedure On_Ok_Button_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      Main_Quit;
      Hide_All (Get_Toplevel (Object));
   end On_Ok_Button_Clicked;

   ----------------------------
   -- On_Help_Button_Clicked --
   ----------------------------

   procedure On_Help_Button_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      null;
   end On_Help_Button_Clicked;

end Open_Session_Pkg.Callbacks;
