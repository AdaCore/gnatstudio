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

with Gtkada.Canvas;   use Gtkada.Canvas;
with Gtk.Handlers;    use Gtk.Handlers;
with GVD.Preferences; use GVD.Preferences;

package body GVD.Canvas is

   package Canvas_Handler is new Gtk.Handlers.Callback
     (GVD_Canvas_Record);

   ------------------------
   -- Get_Detect_Aliases --
   ------------------------

   function Get_Detect_Aliases
     (Canvas : access GVD_Canvas_Record'Class) return Boolean is
   begin
      return Canvas.Detect_Aliases;
   end Get_Detect_Aliases;

   ------------------------
   -- Set_Detect_Aliases --
   ------------------------

   procedure Set_Detect_Aliases
     (Canvas   : access GVD_Canvas_Record'Class;
      Activate : Boolean) is
   begin
      --  ??? We should modify the items displayed so as to remove currently
      --  detected aliases. This is part of the whole aliases detection
      --  implementation.
      Canvas.Detect_Aliases := Activate;
   end Set_Detect_Aliases;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Canvas : out GVD_Canvas) is
   begin
      Canvas := new GVD_Canvas_Record;
      Canvas.Detect_Aliases := Get_Pref (Default_Detect_Aliases);
      Initialize (Canvas);
   end Gtk_New;

   -----------------------
   -- Get_Next_Item_Num --
   -----------------------

   function Get_Next_Item_Num
     (Canvas : access GVD_Canvas_Record'Class) return Integer is
   begin
      Canvas.Item_Num := Canvas.Item_Num + 1;
      return Canvas.Item_Num;
   end Get_Next_Item_Num;

   -----------------
   -- Set_Process --
   -----------------

   procedure Set_Process
     (Canvas  : access GVD_Canvas_Record;
      Process : access Gtk.Window.Gtk_Window_Record'Class) is
   begin
      Canvas.Process := Gtk.Window.Gtk_Window (Process);
   end Set_Process;

   -----------------
   -- Get_Process --
   -----------------

   function Get_Process (Canvas : access GVD_Canvas_Record)
      return Gtk.Window.Gtk_Window is
   begin
      return Canvas.Process;
   end Get_Process;

end GVD.Canvas;
