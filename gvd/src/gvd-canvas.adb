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

with GVD.Canvas.View;

package body GVD.Canvas is

   -------------------------------
   -- Initialize_Component_Item --
   -------------------------------

   procedure Initialize_Component_Item
     (Self      : not null access Component_Item_Record'Class;
      Styles    : not null access Browser_Styles;
      Component : not null access Items.Generic_Type'Class;
      Name      : String) is
   begin
      Self.Initialize_Rect (Styles.Invisible);
      Self.Component := Items.Generic_Type_Access (Component);
      Self.Name      := To_Unbounded_String (Name);
   end Initialize_Component_Item;

   -----------------
   -- Item_Hidden --
   -----------------

   function Item_Hidden
     (View : not null access Debugger_Data_View_Record'Class)
      return Container_Item
   is
   begin
      return Container_Item
        (Gtk_New_Image
           (View.Get_View.Get_Styles.Invisible,
            "gps-hidden-item-symbolic",
            Width => 16.0, Height => 16.0,
            Allow_Rescale => False));
   end Item_Hidden;

   ------------------------
   -- New_Component_Item --
   ------------------------

   function New_Component_Item
     (Styles    : not null access Browser_Styles;
      Component : not null access Items.Generic_Type'Class;
      Name      : String) return Component_Item
   is
      R : constant Component_Item := new Component_Item_Record;
   begin
      R.Initialize_Component_Item (Styles, Component, Name);
      return R;
   end New_Component_Item;

   --------------
   -- On_Click --
   --------------

   overriding procedure On_Click
     (Self    : not null access Collapsible_Item_Record;
      View    : not null access GPS_Canvas_View_Record'Class;
      Details : Gtkada.Canvas_View.Event_Details_Access)
   is
      pragma Unreferenced (View);
   begin
      if Details.Event_Type = Double_Click then
         GVD.Canvas.View.Change_Visibility (Self, Self.For_Component);
      end if;
   end On_Click;

   --------------
   -- On_Click --
   --------------

   overriding procedure On_Click
     (Self    : not null access Xref_Item_Record;
      View    : not null access GPS_Canvas_View_Record'Class;
      Details : Gtkada.Canvas_View.Event_Details_Access)
   is
      pragma Unreferenced (View, Details);
   begin
      GVD.Canvas.View.Dereference_Item (Self);
   end On_Click;

   ---------------
   -- Show_Type --
   ---------------

   function Show_Type (Mode : Display_Mode) return Boolean is
   begin
      return Mode = Type_Only or else Mode = Type_Value;
   end Show_Type;

   ----------------
   -- Show_Value --
   ----------------

   function Show_Value (Mode : Display_Mode) return Boolean is
   begin
      return Mode = Value or else Mode = Type_Value;
   end Show_Value;

end GVD.Canvas;
