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

with Gtkada.Canvas;  use Gtkada.Canvas;
with Gtk.Handlers;   use Gtk.Handlers;
with Gtk.Arguments;  use Gtk.Arguments;
with Ada.Text_IO;    use Ada.Text_IO;
with Unchecked_Conversion;
with System;
with Display_Items;  use Display_Items;

package body Odd.Canvas is

   package Canvas_Handler is new Gtk.Handlers.Callback
     (Odd_Canvas_Record);

   procedure Item_Selected_Cb
     (Canvas : access Odd_Canvas_Record'Class;
      Args   : Gtk_Args);
   --  Called whenever the user has selected an item before starting a drag
   --  operation;

   ----------------------
   -- Item_Selected_Cb --
   ----------------------

   procedure Item_Selected_Cb
     (Canvas : access Odd_Canvas_Record'Class;
      Args   : Gtk_Args)
   is
      function To_Display_Item is new Unchecked_Conversion
        (System.Address, Display_Item);

      Selection : Display_Item := To_Display_Item (To_Address (Args, 1));

      function Add_Item
        (Canvas : access Interactive_Canvas_Record'Class;
         Item   : access Canvas_Item_Record'Class) return Boolean;
      --  Check if Item should be added to the current selection in the canvas.
      --  This is the case of Item is an alias of Selection, so that both are
      --  moved at the same time.

      function Add_Item
        (Canvas : access Interactive_Canvas_Record'Class;
         Item   : access Canvas_Item_Record'Class) return Boolean is
      begin
         if Is_Alias_Of (Display_Item (Item)) = Selection then
            Add_To_Selection (Canvas, Item);
         end if;

         return True;
      end Add_Item;

   begin
      For_Each_Item (Canvas, Add_Item'Unrestricted_Access);
   end Item_Selected_Cb;

   ------------------------
   -- Get_Detect_Aliases --
   ------------------------

   function Get_Detect_Aliases
     (Canvas : access Odd_Canvas_Record'Class) return Boolean is
   begin
      return Canvas.Detect_Aliases;
   end Get_Detect_Aliases;

   ------------------------
   -- Set_Detect_Aliases --
   ------------------------

   procedure Set_Detect_Aliases
     (Canvas   : access Odd_Canvas_Record'Class;
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

   procedure Gtk_New (Canvas : out Odd_Canvas) is
   begin
      Canvas := new Odd_Canvas_Record;
      Initialize (Canvas);

      Canvas_Handler.Connect
        (Canvas, "item_selected", Item_Selected_Cb'Access);
   end Gtk_New;

end Odd.Canvas;
