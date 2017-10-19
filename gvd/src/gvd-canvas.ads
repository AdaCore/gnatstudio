------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2017, AdaCore                          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;

with Gtkada.Canvas_View;     use Gtkada.Canvas_View;
with Gtkada.Style;           use Gtkada.Style;

with Browsers;               use Browsers;
with Browsers.Canvas;        use Browsers.Canvas;
with Language;               use Language;
with Items;

package GVD.Canvas is

   --------------------
   --  The Data view --
   --------------------

   type Debugger_Data_View_Record is new Browsers.Canvas.General_Browser_Record
   with record
      Modified : Drawing_Style;  --  when value has changed
      Freeze   : Drawing_Style;  --  item's value is not refreshed

      --  Settings to draw items
      --  Modified_Color: used to highlight the items whose value has changed
      --    since the last update.
   end record;
   type Debugger_Data_View is access all Debugger_Data_View_Record'Class;

   function Item_Hidden
     (View : not null access Debugger_Data_View_Record'Class)
      return Container_Item;
   --  Return the item that shows when an item is hidden

   ---------------------
   -- Drawing Context --
   ---------------------

   type Display_Mode is (Value, Type_Only, Type_Value);
   --  What information should be displayed in the item.

   function Show_Value (Mode : Display_Mode) return Boolean;
   --  Whether we should display the value of the item

   function Show_Type (Mode : Display_Mode) return Boolean;
   --  Whether we should display the type of the item

   -----------------------------
   -- Printing and Displaying --
   -----------------------------

   type Component_Item_Record is new Rect_Item_Record with record
      Name      : Ada.Strings.Unbounded.Unbounded_String;
      Component : Items.Generic_Type_Access;
   end record;
   type Component_Item is access all Component_Item_Record'Class;
   --  The GUI representation of a Generic_Type

   function New_Component_Item
     (Styles    : not null access Browser_Styles;
      Component : not null access Items.Generic_Type'Class;
      Name      : String) return Component_Item;

   procedure Initialize_Component_Item
     (Self      : not null access Component_Item_Record'Class;
      Styles    : not null access Browser_Styles;
      Component : not null access Items.Generic_Type'Class;
      Name      : String);
   --  Build a new component item

   type Collapsible_Item_Record is new Rect_Item_Record
     and Clickable_Item with record
         For_Component : access Items.Generic_Type'Class;
     end record;
   type Collapsible_Item is access all Collapsible_Item_Record'Class;

   overriding procedure On_Click
     (Self    : not null access Collapsible_Item_Record;
      View    : not null access GPS_Canvas_View_Record'Class;
      Details : Gtkada.Canvas_View.Event_Details_Access);
   --  An box that can be double-clicked to be hidden, and thus save screen
   --  estate

   type Xref_Item_Record is
     new Component_Item_Record and Clickable_Item with null record;
   overriding procedure On_Click
     (Self    : not null access Xref_Item_Record;
      View    : not null access GPS_Canvas_View_Record'Class;
      Details : Gtkada.Canvas_View.Event_Details_Access);

end GVD.Canvas;
