-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2002                      --
--                             ACT-Europe                            --
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

with Glib;                use Glib;
with Gtk.Widget;          use Gtk.Widget;
with Gtk.Main;            use Gtk.Main;
with Gtk.Menu_Item;       use Gtk.Menu_Item;
with Gtk.Handlers;        use Gtk.Handlers;
with Gtk.Notebook;        use Gtk.Notebook;
with Gtk.Window;          use Gtk.Window;
with Gtkada.Types;        use Gtkada.Types;
with Gtkada.MDI;          use Gtkada.MDI;

with GNAT.OS_Lib;         use GNAT.OS_Lib;

with GVD;                 use GVD;
with GVD.Process;         use GVD.Process;
with GVD.Window_Settings; use GVD.Window_Settings;
with GVD.Main_Window;     use GVD.Main_Window;
with Odd_Intl;            use Odd_Intl;
with Breakpoints_Editor;  use Breakpoints_Editor;

package body Main_Debug_Window_Pkg.Callbacks is

   use Gtk.Arguments;

   ---------------------------------------
   -- On_Main_Debug_Window_Delete_Event --
   ---------------------------------------

   function On_Main_Debug_Window_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      pragma Unreferenced (Params);
      --  Arg1 : Gdk_Event := To_Event (Params, 1);
   begin
      --  Ref the object since we will destroy it in the main procedure.

      Ref (Object);
      Save_Window_Settings
        (GVD_Main_Window (Object).Home_Dir.all &
         Directory_Separator & "window_settings", Gtk_Widget (Object));
      Cleanup_Debuggers (GVD_Main_Window (Object));
      Main_Quit;

      return False;
   end On_Main_Debug_Window_Delete_Event;

   -------------------------------------
   -- On_Process_Notebook_Switch_Page --
   -------------------------------------

   procedure On_Process_Notebook_Switch_Page
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      --  Arg1 : Address := To_Address (Params, 1);
      Arg2 : Guint := To_Guint (Params, 2);
      --  Number of the page that will be displayed

      Page      : constant Gtk_Widget := Get_Nth_Page
        (GVD_Main_Window (Object).Process_Notebook, Gint (Arg2));
      Main      : constant GVD_Main_Window := GVD_Main_Window (Object);
      Process   : Debugger_Process_Tab;
      Menu_Item : Gtk_Menu_Item;

   begin
      if Main.Locked then
         Emit_Stop_By_Name (Object, "switch_page");
         return;
      end if;

      Process :=
        Debugger_Process_Tab (Process_User_Data.Get (Page));
      Update_External_Dialogs (Main, Gtk_Widget (Process));

      Menu_Item := Gtk_Menu_Item (Get_Widget (Main.Factory, -"/Window"));
      Set_Submenu (Menu_Item, Create_Menu (Process.Process_Mdi));

      if Main.Breakpoints_Editor /= null then
         Set_Process
           (Breakpoint_Editor_Access (Main.Breakpoints_Editor), Process);
      end if;

   exception
      --  The page wasn't associated with a debugger yet
      when Gtkada.Types.Data_Error =>
         null;
   end On_Process_Notebook_Switch_Page;

end Main_Debug_Window_Pkg.Callbacks;
