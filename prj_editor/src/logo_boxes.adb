-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
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

with Gdk.Bitmap;    use Gdk.Bitmap;
with Gdk.Color;     use Gdk.Color;
with Gdk.Pixmap;    use Gdk.Pixmap;
with Gtk.Box;       use Gtk.Box;
with Gtk.Dialog;    use Gtk.Dialog;
with Gtk.Enums;     use Gtk.Enums;
with Gtk.Event_Box; use Gtk.Event_Box;
with Gtk.Frame;     use Gtk.Frame;
with Gtk.Label;     use Gtk.Label;
with Gtk.Pixmap;    use Gtk.Pixmap;
with Gtk.Style;     use Gtk.Style;
with Gtk.Widget;    use Gtk.Widget;

with Pixmaps_Prj;   use Pixmaps_Prj;

package body Logo_Boxes is

   Bg_Color : constant String := "#0e79bd";

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Win : out Logo_Box;
      Title  : String;
      Parent : Gtk.Window.Gtk_Window := null;
      Title_Font : Pango.Font.Pango_Font_Description := null) is
   begin
      Win := new Logo_Box_Record;
      Logo_Boxes.Initialize (Win, Title, Parent, Title_Font);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Win    : access Logo_Box_Record'Class;
      Title  : String;
      Parent : Gtk.Window.Gtk_Window;
      Title_Font : Pango.Font.Pango_Font_Description)
   is
      Color : Gdk_Color;
      Style : Gtk_Style;
      Box, Vbox   : Gtk_Box;
      Event : Gtk_Event_Box;
      Pix : Gdk_Pixmap;
      Mask : Gdk_Bitmap;
      Gpix : Gtk_Pixmap;
      Frame : Gtk_Frame;
   begin
      Gtk.Dialog.Initialize
        (Dialog  => Win,
         Title   => Title,
         Parent  => Parent,
         Flags   => Modal or Destroy_With_Parent);

      Color := Parse (Bg_Color);
      Alloc (Get_Default_Colormap, Color);
      Style := Copy (Get_Style (Win));
      Set_Background (Style, State_Normal, Color);
      Set_Font_Description (Style, Title_Font);

      Gtk_New_Hbox (Box, False, 0);
      Pack_Start (Get_Vbox (Win), Box, True, True, 0);

      Gtk_New (Event);
      Set_Style (Event, Style);
      Pack_Start (Box, Event, False);

      Gtk_New_Vbox (Win.Side_Box, False, 6);
      Set_Border_Width (Win.Side_Box, 7);
      Add (Event, Win.Side_Box);

      Create_From_Xpm_D
        (Pix, null, Get_Default_Colormap, Mask, Null_Color, logo_xpm);
      Gtk_New (Gpix, Pix, Mask);
      Pack_Start (Win.Side_Box, Gpix, Expand => False, Padding => 10);

      Gtk_New_Vbox (Vbox, False, 0);
      Pack_Start (Box, Vbox, True, True, 0);

      Gtk_New (Event);
      Set_Style (Event, Style);
      Pack_Start (Vbox, Event, False);

      Gtk_New (Win.Title, Title);
      Set_Alignment (Win.Title, 0.5, 0.5);
      Set_Padding (Win.Title, 0, 10);
      Set_Justify (Win.Title, Justify_Center);
      Set_Line_Wrap (Win.Title, False);
      Set_Style (Win.Title, Style);
      Add (Event, Win.Title);

      Gtk_New (Frame);
      Set_Shadow_Type (Frame, Shadow_In);
      Pack_Start (Vbox, Frame, True, True, 0);

      Gtk_New_Hbox (Win.Content, Homogeneous => True);
      Add (Frame, Win.Content);
   end Initialize;

   ------------------
   -- Get_Side_Box --
   ------------------

   function Get_Side_Box (Win : access Logo_Box_Record)
      return Gtk.Box.Gtk_Box is
   begin
      return Win.Side_Box;
   end Get_Side_Box;

   ---------------------
   -- Get_Title_Label --
   ---------------------

   function Get_Title_Label (Win : access Logo_Box_Record)
      return Gtk.Label.Gtk_Label is
   begin
      return Win.Title;
   end Get_Title_Label;

   ------------------
   -- Get_Contents --
   ------------------

   function Get_Contents (Win : access Logo_Box_Record)
      return Gtk.Box.Gtk_Box is
   begin
      return Win.Content;
   end Get_Contents;

end Logo_Boxes;
