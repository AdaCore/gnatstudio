-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2000-2005                       --
--                             AdaCore                               --
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

with Glib;                  use Glib;
with Gtk;                   use Gtk;
with Gtk.Accel_Group;       use Gtk.Accel_Group;
with Gtk.Box;               use Gtk.Box;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Menu_Bar;          use Gtk.Menu_Bar;
with Gtk.Menu_Item;         use Gtk.Menu_Item;
with Gtk.Object;            use Gtk.Object;
with Gtk.Widget;            use Gtk.Widget;
with Gtkada.Handlers;       use Gtkada.Handlers;
with Gtkada.Types;

with Factory_Data;
with GVD.Types;             use GVD.Types;
with Glide_Intl;            use Glide_Intl;

with Interfaces.C.Strings;  use Interfaces.C.Strings;

package body GVD.Main_Window is

   Signals : constant Gtkada.Types.Chars_Ptr_Array :=
     (1 => New_String ("preferences_changed"));
   Class_Record : GObject_Class := Uninitialized_Class;

   procedure On_Destroy (Window : access Gtk_Widget_Record'Class);
   --  Callback for the "destroy" dialog

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Main_Window : out GVD_Main_Window;
      Key         : String;
      Menu_Items  : Gtk_Item_Factory_Entry_Array) is
   begin
      Main_Window := new GVD_Main_Window_Record;
      GVD.Main_Window.Initialize (Main_Window, Key, Menu_Items);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Main_Window : access GVD_Main_Window_Record'Class;
      Key         : String;
      Menu_Items  : Gtk_Item_Factory_Entry_Array)
   is
      Menu : Gtk_Widget;
   begin
      Gtk.Window.Initialize (Main_Window, Window_Toplevel);
      Initialize_Class_Record
        (Main_Window, Signals, Class_Record, Type_Name => "GpsMainWindow");

      Set_Policy (Main_Window, False, True, False);
      Set_Position (Main_Window, Win_Pos_None);
      Set_Modal (Main_Window, False);
      Set_Default_Size (Main_Window, 800, 700);

      Gtk_New_Vbox (Main_Window.Vbox, False, 0);
      Add (Main_Window, Main_Window.Vbox);

      Gtk_New_Vbox (Main_Window.Toolbar_Box, False, 0);
      Pack_Start (Main_Window.Vbox, Main_Window.Toolbar_Box, False, False, 0);

      Gtk_New (Main_Window.Statusbar);
      Pack_End (Main_Window.Vbox, Main_Window.Statusbar, False, False, 0);

      Gtk_New (Main_Window.Main_Accel_Group);
      Add_Accel_Group (Main_Window, Main_Window.Main_Accel_Group);
      Gtk_New (Main_Window.Process_Mdi, Main_Window.Main_Accel_Group);
      Add (Main_Window.Vbox, Main_Window.Process_Mdi);
      Gtk_New
        (Main_Window.Factory, Gtk.Menu_Bar.Get_Type,
         Key, Main_Window.Main_Accel_Group);
      Factory_Data.Create_Items
        (Main_Window.Factory, Menu_Items, Main_Window.all'Access);
      Menu := Get_Widget (Main_Window.Factory, Key);
      Main_Window.Menu_Bar := Gtk_Menu_Bar (Menu);
      Gtk_New_Hbox (Main_Window.Menu_Box, False, 0);
      Pack_Start (Main_Window.Vbox, Main_Window.Menu_Box, False, False);
      Pack_Start (Main_Window.Menu_Box, Menu);
      Reorder_Child (Main_Window.Vbox, Main_Window.Menu_Box, 0);
      Set_Submenu
        (Gtk_Menu_Item (Get_Widget (Main_Window.Factory, '/' & (-"Window"))),
         Create_Menu (Main_Window.Process_Mdi));

      Widget_Callback.Connect
        (Main_Window, "destroy",
         Widget_Callback.To_Marshaller (On_Destroy'Access));
   end Initialize;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Window : access Gtk_Widget_Record'Class) is
      Win : constant GVD_Main_Window := GVD_Main_Window (Window);
   begin
      Unref (Win.Factory);

      if Win.Task_Dialog /= null then
         Destroy (Win.Task_Dialog);
      end if;

      if Win.Thread_Dialog /= null then
         Destroy (Win.Thread_Dialog);
      end if;

      if Win.PD_Dialog /= null then
         Destroy (Win.PD_Dialog);
      end if;

      if Win.History_Dialog /= null then
         Destroy (Win.History_Dialog);
      end if;

      if Win.Memory_View /= null then
         Destroy (Win.Memory_View);
      end if;
   end On_Destroy;

end GVD.Main_Window;
