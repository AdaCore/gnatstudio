-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
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

with Glib.Values;
with Glib.Error;                use Glib.Error;
with Glib.Object;               use Glib.Object;
with Glide_Kernel;              use Glide_Kernel;
with Glide_Kernel.Preferences;  use Glide_Kernel.Preferences;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Image;                 use Gtk.Image;
with Gtk.Main;                  use Gtk.Main;
with Gtk.Window;                use Gtk.Window;
with Gtk.Widget;                use Gtk.Widget;
with Gtkada.Dialogs;            use Gtkada.Dialogs;
with Gtkada.Handlers;           use Gtkada.Handlers;
with Glide_Intl;                use Glide_Intl;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

package body Glide_Main_Window is

   function Delete_Callback
     (Widget : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues) return Boolean;
   --  Callback for the delete event.

   procedure Preferences_Changed
     (Main_Window : access GObject_Record'Class;
      Kernel      : Kernel_Handle);
   --  Called when the preferences have changed.

   -------------
   -- Anim_Cb --
   -------------

   function Anim_Cb (Kernel : Kernel_Handle) return Boolean is
      Window : constant Glide_Window :=
        Glide_Window (Get_Main_Window (Kernel));
   begin
      if Advance (Window.Animation_Iter) then
         Set (Window.Animation_Image, Get_Pixbuf (Window.Animation_Iter));
      end if;

      return True;
   end Anim_Cb;

   ---------------------------
   -- Display_Default_Image --
   ---------------------------

   procedure Display_Default_Image (Kernel : Glide_Kernel.Kernel_Handle) is
      Window : constant Glide_Window :=
        Glide_Window (Get_Main_Window (Kernel));
   begin
      if Window /= null and then Window.Animation /= null then
         Set (Window.Animation_Image, Get_Static_Image (Window.Animation));
      end if;
   end Display_Default_Image;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Main_Window      : out Glide_Window;
      Key              : String;
      Menu_Items       : Gtk_Item_Factory_Entry_Array;
      Home_Dir         : String;
      Prefix_Directory : String) is
   begin
      Main_Window := new Glide_Window_Record;
      Glide_Main_Window.Initialize
        (Main_Window, Key, Menu_Items, Home_Dir, Prefix_Directory);
   end Gtk_New;

   ---------------------
   -- Delete_Callback --
   ---------------------

   function Delete_Callback
     (Widget : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues) return Boolean
   is
      pragma Unreferenced (Params);

      Win    : constant Glide_Window := Glide_Window (Widget);
      Button : constant Message_Dialog_Buttons :=
        Message_Dialog
          (Msg            => -"Are you sure you want to quit ?",
           Dialog_Type    => Confirmation,
           Buttons        => Button_Yes or Button_No,
           Default_Button => Button_No,
           Parent         => Gtk_Window (Win));
   begin
      if Button = Button_Yes then
         Quit (Win);
      end if;

      return True;
   end Delete_Callback;

   ----------
   -- Quit --
   ----------

   procedure Quit (Main_Window : access Glide_Window_Record'Class) is
   begin
      if Save_All_MDI_Children (Main_Window.Kernel) then
         Main_Quit;
      end if;
   end Quit;

   -------------------------
   -- Preferences_Changed --
   -------------------------

   procedure Preferences_Changed
     (Main_Window : access GObject_Record'Class;
      Kernel      : Kernel_Handle)
   is
      Main : constant Glide_Window := Glide_Window (Main_Window);
   begin
      if Get_Pref (Kernel, Toolbar_Show_Text) then
         Set_Style (Main.Toolbar, Toolbar_Both);
      else
         Set_Style (Main.Toolbar, Toolbar_Icons);
      end if;
   end Preferences_Changed;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Main_Window      : access Glide_Window_Record'Class;
      Key              : String;
      Menu_Items       : Gtk_Item_Factory_Entry_Array;
      Home_Dir         : String;
      Prefix_Directory : String)
   is
      Box1   : Gtk_Hbox;
      Error  : GError;
      Pixbuf : Gdk_Pixbuf;

   begin
      Gtk_New (Main_Window.Kernel, Gtk_Window (Main_Window), Home_Dir);
      GVD.Main_Window.Initialize (Main_Window, Key, Menu_Items);
      Main_Window.Home_Dir := new String'(Home_Dir);
      Main_Window.Prefix_Directory := new String'(Prefix_Directory);
      Main_Window.Standalone := False;

      Gtk_New_Hbox (Box1);
      Pack_Start (Main_Window.Toolbar_Box, Box1);
      Gtk_New (Main_Window.Toolbar, Orientation_Horizontal, Toolbar_Icons);
      Set_Tooltips (Main_Window.Toolbar, True);
      Pack_Start (Box1, Main_Window.Toolbar, True, True);

      declare
         File : constant String := Format_Pathname
           (Prefix_Directory & "/share/gps/" &
            Get_Pref (Main_Window.Kernel, Animated_Image));
      begin
         if Is_Regular_File (File) then
            Gtk_New (Main_Window.Animation_Frame);
            Set_Shadow_Type (Main_Window.Animation_Frame, Shadow_In);
            Pack_End (Box1, Main_Window.Animation_Frame, False, False);

            Gdk_New_From_File (Main_Window.Animation, File, Error);
            Gtk_New (Main_Window.Animation_Image, Main_Window.Animation);
            Main_Window.Animation_Iter := Get_Iter (Main_Window.Animation);
            Pixbuf := Get_Pixbuf (Main_Window.Animation_Iter);
            Set (Main_Window.Animation_Image, Pixbuf);
            Add (Main_Window.Animation_Frame, Main_Window.Animation_Image);
         end if;
      end;

      Kernel_Callback.Connect
        (Main_Window, Preferences_Changed_Signal,
         Kernel_Callback.To_Marshaller (Preferences_Changed'Access),
         Kernel_Handle (Main_Window.Kernel));

      Preferences_Changed (Main_Window, Main_Window.Kernel);

      Return_Callback.Object_Connect
        (Main_Window, "delete_event",
         Delete_Callback'Access,
         Gtk_Widget (Main_Window),
         After => False);
   end Initialize;

end Glide_Main_Window;
