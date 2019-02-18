------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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

with Glib;
with Gtkada.Canvas_View;
with Gtkada.Style; use Gtkada.Style;
with XML_Utils;

package Browsers is

   Default_Space_Between_Items  : constant Glib.Gdouble := 10.0;
   Default_Space_Between_Layers : constant Glib.Gdouble := 30.0;
   --  Controlling the layout algorithm

   Left_Arrow     : constant  Glib.UTF8_String :=
     (Character'Val (16#E2#)     --  \u25C0 black left pointing triangle
      & Character'Val (16#97#)
      & Character'Val (16#80#));

   Right_Arrow    : constant  Glib.UTF8_String :=
     (Character'Val (16#E2#)     --  \u25B6 black right pointing triangle
      & Character'Val (16#96#)
      & Character'Val (16#B6#));

   Collapse_Arrow : constant  Glib.UTF8_String :=
     (Character'Val (16#E2#)     --  \u25BC black down pointing triangle
      & Character'Val (16#96#)
      & Character'Val (16#BC#));

   type Background_Type is
     (Background_None,
      Background_Color,
      Background_Grid_Lines,
      Background_Grid_Dots);
   --  The types of background we can display in a browser

   type GPS_Canvas_View_Record is new Gtkada.Canvas_View.Canvas_View_Record
   with private;
   type GPS_Canvas_View is access all GPS_Canvas_View_Record'Class;

   Margin : constant Gtkada.Canvas_View.Margins := (4.0, 4.0, 4.0, 4.0);
   --  Margins on all sides of a nested item

   type Browser_Styles is record
      Item        : Drawing_Style; --  Style to draw the item itself
      Title       : Drawing_Style; --  Style to use the background of the title
      Title_Font  : Drawing_Style; --  Style to draw the title itself
      Text_Font   : Drawing_Style; --  Style to draw the contents of the box
      Link_Label  : Drawing_Style; --  Style to draw labels on links
      Link        : Drawing_Style; --  Style to draw the links themselves
      Link2       : Drawing_Style; --  Style to draw other links
      Highlight   : Drawing_Style; --  Parents or children of selected items
      Hyper_Link  : Drawing_Style; --  Style to draw hyper links

      Circle      : Drawing_Style; --  For items with a circle
      Label       : Drawing_Style; --  semi-transparent white
      Search      : Drawing_Style; --  matches for local search and filters

      Nested      : Drawing_Style; --  A nested item (with border)

      Invisible      : Drawing_Style; --  Invisible item
      Selected_Link  : Drawing_Style; --  link to selected items
   end record;
   --  The styles to use when drawing items

   procedure Gtk_New
     (Self  : out GPS_Canvas_View;
      Model : not null access Gtkada.Canvas_View.Canvas_Model_Record'Class);
   --  Create a new canvas

   procedure Create_Styles (Self : not null access GPS_Canvas_View_Record);
   --  Recompute the styles for the canvas, based on user preferences.

   procedure Set_Read_Only
      (Self : not null access GPS_Canvas_View_Record;
       Read_Only : Boolean := True);
   function Is_Read_Only
      (Self : not null access GPS_Canvas_View_Record) return Boolean;
   --  Items in a read-only canvas cannot be moved, nor can the layout be
   --  changed.

   function Get_Styles
     (Self : not null access GPS_Canvas_View_Record'Class)
      return access Browser_Styles;
   --  Retrieve the styles for drawing items

   overriding procedure Draw_Internal
     (Self    : not null access GPS_Canvas_View_Record;
      Context : Gtkada.Canvas_View.Draw_Context;
      Area    : Gtkada.Canvas_View.Model_Rectangle);

   -----------
   -- Items --
   -----------

   type Clickable_Item is interface;
   procedure On_Click
     (Self    : not null access Clickable_Item;
      View    : not null access GPS_Canvas_View_Record'Class;
      Details : Gtkada.Canvas_View.Event_Details_Access) is abstract;
   --  All clickable items should implement this interface, so that the
   --  view automatically call their On_Click primitive op.

   type Close_Button_Record
      is new Gtkada.Canvas_View.Image_Item_Record and Clickable_Item
      with null record;
   type Close_Button is access all Close_Button_Record'Class;
   procedure Gtk_New (Self : out Close_Button);
   overriding procedure On_Click
     (Self    : not null access Close_Button_Record;
      View    : not null access GPS_Canvas_View_Record'Class;
      Details : Gtkada.Canvas_View.Event_Details_Access);
   --  A button that closes the corresponding toplevel item

   type Left_Arrow_Record
      is abstract new Gtkada.Canvas_View.Text_Item_Record and Clickable_Item
      with null record;
   procedure Initialize
     (Self : not null access Left_Arrow_Record'Class;
      Font : Font_Style);
   --  Left-pointing arrow in title bars.
   --  You must override On_Click to use this.

   type Left_Arrow_Access is access all Left_Arrow_Record'Class;

   type Right_Arrow_Record
      is abstract new Gtkada.Canvas_View.Text_Item_Record and Clickable_Item
      with null record;
   procedure Initialize
     (Self : not null access Right_Arrow_Record'Class;
      Font : Font_Style);
   --  Right-pointing arrow in title bars.
   --  You must override On_Click to use this.

   type Right_Arrow_Access is access all Right_Arrow_Record'Class;

   -----------
   -- Links --
   -----------

   type GPS_Link_Record is new Gtkada.Canvas_View.Canvas_Link_Record with
      record
         Default_Style : Gtkada.Style.Drawing_Style;
         --  The default style to use for the item. This is overridden either
         --  when the link is made invisible by the user, or is a link to a
         --  selected item.

         Invisible     : Boolean := False;
         --  Whether the link is current invisible
      end record;
   type GPS_Link is access all GPS_Link_Record'Class;

   procedure Save_To_XML
     (Self : not null access GPS_Link_Record;
      Node : not null XML_Utils.Node_Ptr) is null;
   --  Override this function to save special properties when saving a link
   --  to the desktop.
   --  You will also need to override the browser's Load_From_ML

private

   type GPS_Canvas_View_Record is new Gtkada.Canvas_View.Canvas_View_Record
   with record
      Background : Background_Type := Background_None;
      Grid_Style : Drawing_Style;
      Styles     : aliased Browser_Styles;
      Read_Only  : Boolean := False;
   end record;

end Browsers;
