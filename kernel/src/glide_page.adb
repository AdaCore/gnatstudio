-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
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

with GVD.Process;
with GVD.Text_Box.Source_Editor.Glide;
with Glib.Object;              use Glib.Object;
with Glide_Consoles;           use Glide_Consoles;
with Glide_Kernel.Preferences; use Glide_Kernel.Preferences;
with Glide_Kernel;             use Glide_Kernel;
with Glide_Main_Window;        use Glide_Main_Window;
with Gtkada.MDI;               use Gtkada.MDI;

package body Glide_Page is
   use GVD.Text_Box.Source_Editor;

   procedure Preferences_Changed
     (Page   : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  Called when preferences have changed

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Page   : out Glide_Page;
      Window : access Glide_Window_Record'Class) is
   begin
      Page := new Glide_Page_Record;
      Initialize (Page, Window);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Page   : access Glide_Page_Record'Class;
      Window : access Glide_Window_Record'Class)
   is
      Edit  : Glide.GEdit;

   begin
      Glide.Gtk_New (Edit, Window);
      GVD.Process.Initialize (Page, Window, Source_Editor (Edit));
      Set_Priorities
        (Page.Process_Mdi, (Left => 1, Right => 4, Top => 2, Bottom => 3));

      Preferences_Changed (Page, Window.Kernel);

      Kernel_Callback.Object_Connect
        (Window.Kernel, Preferences_Changed_Signal,
         Kernel_Callback.To_Marshaller (Preferences_Changed'Access),
         Slot_Object => Page,
         User_Data   => Kernel_Handle (Window.Kernel));
   end Initialize;

   -------------------------
   -- Preferences_Changed --
   -------------------------

   procedure Preferences_Changed
     (Page   : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Page);
   begin
      Configure
        (Get_MDI (Kernel),
         Opaque_Resize     => Get_Pref (Kernel, MDI_Opaque),
         Opaque_Move       => Get_Pref (Kernel, MDI_Opaque),
         Opaque_Docks      => Get_Pref (Kernel, MDI_Opaque),
         Close_Floating_Is_Unfloat =>
           not Get_Pref (Kernel, MDI_Destroy_Floats),
         Title_Font        => Get_Pref (Kernel, MDI_Title_Font),
         Background_Color  => Get_Pref (Kernel, MDI_Background_Color),
         Title_Bar_Color   => Get_Pref (Kernel, MDI_Title_Bar_Color),
         Focus_Title_Color => Get_Pref (Kernel, MDI_Focus_Title_Color));
   end Preferences_Changed;

   ------------------
   -- Load_Desktop --
   ------------------

   procedure Load_Desktop (Window : access Glide_Window_Record'Class) is
      Was_Loaded : Boolean;
   begin
      Was_Loaded := Load_Desktop (Window.Kernel);
   end Load_Desktop;

   --------------
   -- Set_Busy --
   --------------

   procedure Set_Busy
     (Page          : access Glide_Page_Record;
      Busy          : Boolean := True;
      Force_Refresh : Boolean := False)
   is
      pragma Unreferenced (Force_Refresh);
   begin
      if Busy then
         Push_State (Glide_Window (Page.Window).Kernel, Processing);
      else
         Pop_State (Glide_Window (Page.Window).Kernel);
      end if;
   end Set_Busy;

end Glide_Page;
