-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2003                      --
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

with Gtk.Widget;          use Gtk.Widget;
with Gtk.Main;            use Gtk.Main;

with GNAT.OS_Lib;         use GNAT.OS_Lib;

with GVD;                 use GVD;
with GVD.Window_Settings; use GVD.Window_Settings;
with GVD.Main_Window;     use GVD.Main_Window;

package body Main_Debug_Window_Pkg.Callbacks is

   use Gtk.Arguments;

   function Idle_Exit (Window : GVD_Main_Window) return Boolean;
   --  Idle function called to finish handling of exiting.

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

end Main_Debug_Window_Pkg.Callbacks;
