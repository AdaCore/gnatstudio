-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                         Copyright (C) 2000                        --
--                 Emmanuel Briot and Arnaud Charlet                 --
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

with Gtkada.Canvas;
with Gtk.Window;

package GVD.Canvas is

   ----------------
   -- GVD_Canvas --
   ----------------

   type GVD_Canvas_Record is new Gtkada.Canvas.Interactive_Canvas_Record
     with private;
   type GVD_Canvas is access all GVD_Canvas_Record'Class;

   procedure Gtk_New (Canvas : out GVD_Canvas);
   --  Create a new canvas.

   function Get_Detect_Aliases
     (Canvas : access GVD_Canvas_Record'Class) return Boolean;
   --  Return True if aliases detection has been activated.

   procedure Set_Detect_Aliases
     (Canvas   : access GVD_Canvas_Record'Class;
      Activate : Boolean);
   --  Change the status of aliases detection in the canvas

   function Get_Next_Item_Num
     (Canvas : access GVD_Canvas_Record'Class) return Integer;
   --  Return the number that should be used for the next item inserted into
   --  the canvas.
   --  Two successive calls to that function will not return the same value.

   procedure Set_Process
     (Canvas  : access GVD_Canvas_Record;
      Process : access Gtk.Window.Gtk_Window_Record'Class);
   --  Set the process associated with the canvas.

   function Get_Process (Canvas : access GVD_Canvas_Record)
     return Gtk.Window.Gtk_Window;
   --  Return the process tab that contains the canvas.

private

   type GVD_Canvas_Record is new Gtkada.Canvas.Interactive_Canvas_Record with
   record
      Detect_Aliases : Boolean;
      Item_Num       : Integer := 0;
      Process        : Gtk.Window.Gtk_Window;
      --  The process tab that contains the canvas
   end record;
end GVD.Canvas;
