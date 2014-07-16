------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2014, AdaCore                          --
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

with Cairo;                    use Cairo;
with Gdk.RGBA;                 use Gdk.RGBA;
with Gdk.Types;                use Gdk.Types;
with Glib.Object;              use Glib, Glib.Object;
with Gtkada.Canvas_View;       use Gtkada.Canvas_View;
with Gtkada.Canvas_View.Views; use Gtkada.Canvas_View.Views;
with Pango.Font;               use Pango.Font;

package body Browsers is

   function On_Item_Event_Zoom is new On_Item_Event_Zoom_Generic
     (Modifier => Mod1_Mask);
   function On_Item_Event_Key_Navigate
   is new On_Item_Event_Key_Navigate_Generic (Modifier => 0);
   function On_Item_Event_Key_Scrolls is new On_Item_Event_Key_Scrolls_Generic
     (Modifier => Mod1_Mask);

   function On_Click
     (Self    : not null access GObject_Record'Class;
      Details : Event_Details_Access) return Boolean;
   --  Called when an item is clicked, to possibly execute its action

   --------------
   -- On_Click --
   --------------

   function On_Click
     (Self    : not null access GObject_Record'Class;
      Details : Event_Details_Access) return Boolean
   is
   begin
      if Details.Event_Type = Button_Release
        and then Details.Button = 1
        and then Details.Item /= null
        and then Details.Item.all in Clickable_Item'Class
      then
         Clickable_Item'Class (Details.Item.all).On_Click
           (GPS_Canvas_View (Self));
         return True;
      end if;
      return False;
   end On_Click;

   -------------------
   -- Draw_Internal --
   -------------------

   overriding procedure Draw_Internal
     (Self    : not null access GPS_Canvas_View_Record;
      Context : Draw_Context;
      Area    : Model_Rectangle)
   is
   begin
      case Self.Background is
         when Background_None =>
            null;

         when Background_Color =>
            if Self.Grid_Style.Get_Fill /= Null_Pattern then
               Set_Source (Context.Cr, Self.Grid_Style.Get_Fill);
               Paint (Context.Cr);
            end if;

         when Background_Grid_Lines =>
            Draw_Grid_Lines
              (Self    => Self,
               Style   => Self.Grid_Style,
               Context => Context,
               Area    => Area);

         when Background_Grid_Dots =>
            Draw_Grid_Lines
              (Self    => Self,
               Style   => Self.Grid_Style,
               Context => Context,
               Area    => Area);
      end case;

      Canvas_View_Record (Self.all).Draw_Internal (Context, Area);
   end Draw_Internal;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Self  : out GPS_Canvas_View;
      Model : not null access Gtkada.Canvas_View.Canvas_Model_Record'Class)
   is
   begin
      Self := new GPS_Canvas_View_Record;
      Gtkada.Canvas_View.Initialize (Self, Model);

      --  Put this first
      Self.On_Item_Event (On_Click'Access);

      Self.On_Item_Event (On_Item_Event_Select'Access);
      Self.On_Item_Event (On_Item_Event_Move_Item'Access);
      Self.On_Item_Event (On_Item_Event_Scroll_Background'Access);
      Self.On_Item_Event (On_Item_Event_Key_Navigate'Access);
      Self.On_Item_Event (On_Item_Event_Key_Scrolls'Access);
      Self.On_Item_Event (On_Item_Event_Zoom'Access);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : not null access Left_Arrow_Record'Class) is
   begin
      Self.Initialize_Text
        (Gtk_New (Stroke => Null_RGBA,
                  Font => (Name => From_String ("sans 12"), others => <>)),
         Character'Val (16#E2#)     --  \u21E8 leftwards white arrow
         & Character'Val (16#87#)
         & Character'Val (16#A6#));
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : not null access Right_Arrow_Record'Class) is
   begin
      Self.Initialize_Text
        (Gtk_New (Stroke => Null_RGBA,
                  Font => (Name => From_String ("sans 12"), others => <>)),
         Character'Val (16#E2#)     --  \u21E6 rightwards white arrow
         & Character'Val (16#87#)
         & Character'Val (16#A8#));
   end Initialize;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Close_Button) is
   begin
      Self := new Close_Button_Record;
      Self.Initialize_Text
        (Gtk_New (Stroke => Null_RGBA,
                  Font => (Name => From_String ("sans 12"), others => <>)),
         "x");
   end Gtk_New;

   --------------
   -- On_Click --
   --------------

   overriding procedure On_Click
     (Self : not null access Close_Button_Record;
      View : not null access GPS_Canvas_View_Record'Class) is
   begin
      View.Model.Remove (Self.Get_Toplevel_Item);
      View.Model.Refresh_Layout;
      View.Queue_Draw;
   end On_Click;

end Browsers;
