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
with Glib.Object;         use Glib.Object;
with Gtk.Widget;          use Gtk.Widget;
with Gtk.Main;            use Gtk.Main;
with Gtk.Window;          use Gtk.Window;

with GNAT.OS_Lib;         use GNAT.OS_Lib;

with GVD;                 use GVD;
with GVD.Process;         use GVD.Process;
with GVD.Window_Settings; use GVD.Window_Settings;
with GVD.Main_Window;     use GVD.Main_Window;
with Odd_Intl;            use Odd_Intl;
with Breakpoints_Editor;  use Breakpoints_Editor;
with Debugger;            use Debugger;

package body Main_Debug_Window_Pkg.Callbacks is

   use Gtk.Arguments;

   function Idle_Exit (Window : GVD_Main_Window) return Boolean;
   --  Idle function called to finish handling of exiting.

   procedure On_Process_Notebook_Switch_Page
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);
   pragma Unreferenced (On_Process_Notebook_Switch_Page);
   --  ??? Need to update to new multi-process handling

   ---------------
   -- Idle_Exit --
   ---------------

   function Idle_Exit (Window : GVD_Main_Window) return Boolean is
   begin
      Cleanup_Debuggers (Window);
      Main_Quit;
      return False;
   end Idle_Exit;

   ---------------------------------------
   -- On_Main_Debug_Window_Delete_Event --
   ---------------------------------------

   function On_Main_Debug_Window_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      pragma Unreferenced (Params);

      --  Arg1 : Gdk_Event := To_Event (Params, 1);
      Id         : Idle_Handler_Id;
      pragma Unreferenced (Id);

      GVD_Window : constant GVD_Main_Window := GVD_Main_Window (Object);

   begin
      --  Ref the object since we will destroy it in the main procedure.

      if GVD_Window.Standalone then
         Ref (Object);
         Save_Window_Settings
           (GVD_Window.Home_Dir.all &
            Directory_Separator & "window_settings", Gtk_Widget (Object));
         Prepare_Cleanup_Debuggers (GVD_Window);
         Id := Main_Window_Idle.Add (Idle_Exit'Access, GVD_Window);

         return True;
      end if;

      return False;
   end On_Main_Debug_Window_Delete_Event;

   -------------------------------------
   -- On_Process_Notebook_Switch_Page --
   -------------------------------------

   procedure On_Process_Notebook_Switch_Page
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Params);

      --  Arg2 : constant Guint := To_Guint (Params, 2);
      --  Number of the page that will be displayed

      Main        : constant GVD_Main_Window := GVD_Main_Window (Object);
      Process     : Visual_Debugger;
      Widget      : Gtk_Widget;
      WTX_Version : Natural;

   begin
      return;
      --  ??? Need to update when using new process setting

      Update_External_Dialogs (Main, GObject (Process));

      if Main.Breakpoints_Editor /= null then
         Set_Process
           (Breakpoint_Editor_Access (Main.Breakpoints_Editor), Process);
      end if;

      --  Update the sensitivity of the Data/Protection Domains menu
      --  item

      Widget := Get_Widget (Main.Factory, -"/Data/Protection Domains");

      if Widget = null then
         --  This means that GVD is part of GPS

         Widget := Get_Widget
           (Main.Factory, -"/Debug/Data/Protection Domains");
      end if;

      if Widget /= null then
         Info_WTX (Process.Debugger, WTX_Version);

         if WTX_Version /= 3 then
            Set_Sensitive (Widget, False);
         else
            Set_Sensitive (Widget, True);
         end if;
      end if;
   end On_Process_Notebook_Switch_Page;

end Main_Debug_Window_Pkg.Callbacks;
