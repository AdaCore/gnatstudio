-----------------------------------------------------------------------
--                 Odd - The Other Display Debugger                  --
--                                                                   --
--                         Copyright (C) 2000                        --
--                 Emmanuel Briot and Arnaud Charlet                 --
--                                                                   --
-- Odd is free  software;  you can redistribute it and/or modify  it --
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

package Odd.Canvas is

   ---------------
   -- Constants --
   ---------------
   --  ??? These will eventually be moved to a preferences package.

   Default_Detect_Aliases : constant Boolean := True;
   --  If True, do not create new items when a matching item is already
   --  present in the canvas.

   ----------------
   -- Odd_Canvas --
   ----------------

   type Odd_Canvas_Record is new Gtkada.Canvas.Interactive_Canvas_Record
     with private;
   type Odd_Canvas is access all Odd_Canvas_Record'Class;

   procedure Gtk_New (Canvas : out Odd_Canvas);
   --  Create a new canvas.

   function Get_Detect_Aliases
     (Canvas : access Odd_Canvas_Record'Class) return Boolean;
   --  Return True if aliases detection has been activated.

   procedure Set_Detect_Aliases
     (Canvas   : access Odd_Canvas_Record'Class;
      Activate : Boolean);
   --  Change the status of aliases detection in the canvas

   function Get_Next_Item_Num
     (Canvas : access Odd_Canvas_Record'Class) return Integer;
   --  Return the number that should be used for the next item inserted into
   --  the canvas.
   --  Two successive calls to that function will not return the same value.

private

   type Odd_Canvas_Record is new Gtkada.Canvas.Interactive_Canvas_Record with
   record
      Detect_Aliases : Boolean := Default_Detect_Aliases;
      Item_Num       : Integer := 0;
   end record;
end Odd.Canvas;
