------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2014-2017, AdaCore                     --
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
with GPS.Kernel.Preferences;   use GPS.Kernel.Preferences;
with Gdk.RGBA;                 use Gdk.RGBA;
with Gdk.Types;                use Gdk.Types;
with Glib.Object;              use Glib, Glib.Object;
with Gtkada.Canvas_View.Views; use Gtkada.Canvas_View.Views;
with Gtkada.Canvas_View;       use Gtkada.Canvas_View;
with Pango.Enums;              use Pango.Enums;
with Pango.Font;               use Pango.Font;

package body Browsers is

   function On_Item_Event_Zoom is new On_Item_Event_Zoom_Generic
     (Modifier => Mod1_Mask);
   function On_Item_Event_Key_Navigate
   is new On_Item_Event_Key_Navigate_Generic (Modifier => 0);
   function On_Item_Event_Key_Scrolls is new On_Item_Event_Key_Scrolls_Generic
     (Modifier => Mod1_Mask);

   function On_Item_Event
     (Self    : not null access GObject_Record'Class;
      Details : Event_Details_Access) return Boolean;
   --  Called when an item is clicked, to possibly execute its action

   -------------------
   -- On_Item_Event --
   -------------------

   function On_Item_Event
     (Self    : not null access GObject_Record'Class;
      Details : Event_Details_Access) return Boolean
   is
      Canvas : constant GPS_Canvas_View := GPS_Canvas_View (Self);
      It : Abstract_Item;
   begin
      if not Canvas.Read_Only
         and then
            (On_Item_Event_Select (Self, Details)
             or else On_Item_Event_Move_Item (Self, Details)
             or else On_Item_Event_Edit (Self, Details))
      then
         return True;
      end if;

      if (Details.Event_Type = Button_Release
          or else Details.Event_Type = Double_Click)
        and then Details.Button = 1
      then
         --  propagate event up, if needed

         It := Details.Item;
         while It /= null loop
            if It.all in Clickable_Item'Class then
               Clickable_Item'Class (It.all).On_Click
                 (GPS_Canvas_View (Self), Details);
               return True;
            end if;
            It := It.Parent;
         end loop;
      end if;
      return False;
   end On_Item_Event;

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

         when Background_Grid_Lines | Background_Grid_Dots =>
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
      Self.On_Item_Event (On_Item_Event'Access);

      Self.On_Item_Event (On_Item_Event_Scroll_Background'Access);
      Self.On_Item_Event (On_Item_Event_Zoom'Access);
      Self.On_Item_Event (On_Item_Event_Key_Navigate'Access);
      Self.On_Item_Event (On_Item_Event_Key_Scrolls'Access);
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
         & Character'Val (16#A6#),
         Height => 12.0);
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
         & Character'Val (16#A8#),
         Height => 12.0);
   end Initialize;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Close_Button) is
   begin
      Self := new Close_Button_Record;
      Self.Initialize_Image
        (Gtk_New (Stroke => Null_RGBA),
         Icon_Name => "gps-close-symbolic",
         Allow_Rescale => False,
         Width => 12.0,
         Height => 12.0);
   end Gtk_New;

   --------------
   -- On_Click --
   --------------

   overriding procedure On_Click
     (Self : not null access Close_Button_Record;
      View : not null access GPS_Canvas_View_Record'Class;
      Details : Gtkada.Canvas_View.Event_Details_Access)
   is
      pragma Unreferenced (Details);
   begin
      View.Model.Remove (Self.Get_Toplevel_Item);
      View.Model.Refresh_Layout;
      View.Queue_Draw;
   end On_Click;

   ----------------
   -- Get_Styles --
   ----------------

   function Get_Styles
     (Self : not null access GPS_Canvas_View_Record'Class)
      return access Browser_Styles is
   begin
      return Self.Styles'Access;
   end Get_Styles;

   -------------------
   -- Create_Styles --
   -------------------

   procedure Create_Styles (Self : not null access GPS_Canvas_View_Record) is

      function Create_Title_Style (Base : Gdk_RGBA) return Drawing_Style;
      function Create_Title_Style (Base : Gdk_RGBA) return Drawing_Style is
         B : Gdk_RGBA;
         P : Cairo_Pattern;
      begin
         P := Pattern_Create_Linear (0.0, 0.0, 0.0, 1.0);
         B := Lighten (Base, 0.1);
         Pattern_Add_Color_Stop_Rgb (P, 0.0, B.Red, B.Green, B.Blue);
         B := Shade (Base, 0.1);
         Pattern_Add_Color_Stop_Rgb (P, 1.0, B.Red, B.Green, B.Blue);
         return Gtk_New
           (Fill   => P,
            Stroke => Null_RGBA,
            Font   => (Name   => From_String ("sans 8"),
                       Halign => Pango.Enums.Pango_Align_Center,
                       others => <>));
      end Create_Title_Style;

      Selected : constant Gdk_RGBA := Selected_Item_Color.Get_Pref;
      F        : constant Pango_Font_Description :=
        GPS.Kernel.Preferences.Default_Font.Get_Pref;
      F2, F3, F4 : Pango_Font_Description;

   begin
      --  ??? Should update the style properties directly, to refresh
      --  existing items, but for now we have no preference for those
      --  colors.

      F2 := Copy (F);
      Set_Size (F2, Get_Size (F) - Gint (0.5 * Pango_Scale));

      F3 := Copy (F);
      Set_Size (F3, Get_Size (F) - Gint (0.5 * Pango_Scale));

      F4 := Copy (F);
      Set_Size (F4, Get_Size (F) - Gint (0.5 * Pango_Scale));

      Self.Styles :=
        (Item => Gtk_New
           (Fill => Create_Rgba_Pattern (White_RGBA),
            Shadow => (Color => (0.0, 0.0, 0.0, 0.1), others => <>)),
         Nested => Gtk_New
           (Stroke => (0.8, 0.8, 0.8, 0.8)),
         Title      =>
           Create_Title_Style (White_RGBA),
         Title_Font => Gtk_New
           (Font   => (Name   => Copy (F),
                       Halign => Pango.Enums.Pango_Align_Center,
                       others => <>),
            Stroke => Null_RGBA),
         Text_Font  => Gtk_New
           (Font   => (Name => F2, others => <>),
            Stroke => Null_RGBA),
         Hyper_Link  => Gtk_New
           (Font      => (Name => F4,
                          Color     => Browsers_Hyper_Link_Color.Get_Pref,
                          Underline => Pango_Underline_Single,
                          others => <>),
            Stroke    => Null_RGBA),
         Link_Label => Gtk_New (Font => (Name => F3, others => <>)),
         Link       => Gtk_New
           (Stroke   => Unselected_Link_Color.Get_Pref,
            Arrow_To => (Head   => Solid,
                         Stroke => Null_RGBA,
                         Length => 8.0,
                         Fill   => Unselected_Link_Color.Get_Pref,
                         others => <>)),
         Link2      =>  Gtk_New
           (Stroke   => Unselected_Link_Color.Get_Pref,
            Arrow_To => (Head   => Solid,
                         Stroke => Null_RGBA,
                         Length => 8.0,
                         Fill   => Unselected_Link_Color.Get_Pref,
                         others => <>),
            Dashes   => (5.0, 5.0)),
         Highlight  =>  Gtk_New
           (Stroke     => Parent_Linked_Item_Color.Get_Pref,
            Line_Width => 2.0),
         Search     => Gtk_New
           (Stroke     => (0.8, 0.0, 0.0, 0.7),
            Line_Width => 2.0),
         Circle     => Gtk_New
           (Stroke     => (0.27, 0.5, 0.7, 1.0),
            Line_Width => 2.0),
         Label      => Gtk_New
           (Stroke => Null_RGBA,
            Fill => Create_Rgba_Pattern ((1.0, 1.0, 1.0, 0.6)),
            Font   => (Name => From_String ("sans 8"),
                       others => <>)),
         Invisible  => Gtk_New (Stroke => Null_RGBA),
         Selected_Link => Gtk_New
           (Stroke   => Selected_Link_Color.Get_Pref,
            Arrow_To => (Head   => Solid,
                         Stroke => Null_RGBA,
                         Length => 8.0,
                         Fill   => Selected_Link_Color.Get_Pref,
                         others => <>)));

      Self.Set_Selection_Style
        (Gtk_New
           (Stroke     => Selected,
            Line_Width => 3.0));
   end Create_Styles;

   -------------------
   -- Set_Read_Only --
   -------------------

   procedure Set_Read_Only
      (Self : not null access GPS_Canvas_View_Record;
       Read_Only : Boolean := True) is
   begin
      Self.Read_Only := Read_Only;
   end Set_Read_Only;

   ------------------
   -- Is_Read_Only --
   ------------------

   function Is_Read_Only
      (Self : not null access GPS_Canvas_View_Record) return Boolean is
   begin
      return Self.Read_Only;
   end Is_Read_Only;

end Browsers;
