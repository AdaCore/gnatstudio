-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2001-2008, AdaCore                  --
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

with Ada.Text_IO;               use Ada.Text_IO;
pragma Warnings (Off);
with GNAT.Expect.TTY.Remote;    use GNAT.Expect.TTY.Remote;
pragma Warnings (On);

with Gtk_Utils;                 use Gtk_Utils;

with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with Traces;                    use Traces;

package body GPS.Callbacks is

   Me        : constant Debug_Handle := Create ("GPS");
   Gtk_Trace : constant Debug_Handle := Create ("Gtk+");

   GPS_Started_Hook : constant Hook_Name := "gps_started";

   -------------
   -- Gtk_Log --
   -------------

   procedure Gtk_Log
     (Log_Domain : String;
      Log_Level  : Log_Level_Flags;
      Message    : String) is
   begin
      if Log_Domain = "" then
         --  Ignore this message, to avoid generating too much noise
         return;
      end if;

      if (Log_Level and Log_Level_Critical) /= 0 then
         Trace (Gtk_Trace, Log_Domain & "-CRITICAL: " & Message);
      elsif (Log_Level and Log_Level_Warning) /= 0 then
         Trace (Gtk_Trace, Log_Domain & "-WARNING: " & Message);
      else
         Trace (Gtk_Trace, Log_Domain & "-MISC: " & Message);
      end if;
   exception
      when E : others =>
         Trace (Exception_Handle, E);
   end Gtk_Log;

   --------------------
   -- Ctrl_C_Handler --
   --------------------

   procedure Ctrl_C_Handler is
   begin
      --  Ignore Ctrl-C events

      null;
   end Ctrl_C_Handler;

   --------------------
   -- On_GPS_Started --
   --------------------

   function On_GPS_Started return Boolean is
   begin
      --  Cannot call Have_Render earlier, since we need a valid Gdk Window
      --  associated with GPS_Main, which happens only very late in the
      --  processing

      if not Have_Render (Get_Window (GPS_Main)) then
         Trace (Me, "RENDER extension NOT detected");
         Put_Line
           ("Warning: X RENDER extension is not detected, " &
            "display will be slow");
      end if;

      Run_Hook (GPS_Main.Kernel, GPS_Started_Hook);
      return False;
   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return False;
   end On_GPS_Started;

   -------------------
   -- Title_Changed --
   -------------------

   procedure Title_Changed
     (MDI    : access GObject_Record'Class;
      Child  : Gtk_Args;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (MDI);
      C : MDI_Child;
   begin
      if not Exiting then
         C := MDI_Child (To_Object (Child, 1));
         Set_Main_Title (Kernel, C);
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end Title_Changed;

   --------------------
   -- Set_Main_Title --
   --------------------

   procedure Set_Main_Title
     (Kernel : access Kernel_Handle_Record'Class;
      Child  : MDI_Child) is
   begin
      if Started then
         if Child = null then
            Reset_Title (GPS_Window (Get_Main_Window (Kernel)));
         else
            if Get_Pref (Pref_Draw_Title_Bars) then
               Reset_Title
                 (GPS_Window (Get_Main_Window (Kernel)),
                  Get_Short_Title (Child));
            else
               Reset_Title
                 (GPS_Window (Get_Main_Window (Kernel)),
                  Get_Title (Child));
            end if;
         end if;
      end if;
   end Set_Main_Title;

   --------------------
   -- Child_Selected --
   --------------------

   procedure Child_Selected
     (Mdi    : access GObject_Record'Class;
      Params : Glib.Values.GValues;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Mdi);
      Child : MDI_Child;
   begin
      if Exiting then
         return;
      end if;

      Child := MDI_Child (To_Object (Params, 1));
      Set_Main_Title (Kernel, Child);

      if Started then
         Context_Changed (Kernel);
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end Child_Selected;

end GPS.Callbacks;
