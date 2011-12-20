------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
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

with Gdk.Color;     use Gdk.Color;
with Gtk.Box;       use Gtk.Box;
with Gtk.Dialog;    use Gtk.Dialog;
with Gtk.Image;     use Gtk.Image;
with Gtk.Enums;     use Gtk.Enums;
with Gtk.Event_Box; use Gtk.Event_Box;
with Gtk.Label;     use Gtk.Label;
with Gtk.Style;     use Gtk.Style;
with Gtk.Widget;    use Gtk.Widget;

package body Logo_Boxes is

   Bg_Color : constant String := "#ffffff";

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Win        : out Logo_Box;
      Title      : String;
      Parent     : Gtk.Window.Gtk_Window := null;
      Show_Toc   : Boolean := True;
      Title_Font : Pango.Font.Pango_Font_Description := null) is
   begin
      Win := new Logo_Box_Record;
      Logo_Boxes.Initialize (Win, Title, Parent, Show_Toc, Title_Font);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Win        : access Logo_Box_Record'Class;
      Title      : String;
      Parent     : Gtk.Window.Gtk_Window;
      Show_Toc   : Boolean := True;
      Title_Font : Pango.Font.Pango_Font_Description)
   is
      Color       : Gdk_Color;
      Style       : Gtk_Style;
      Box, Vbox   : Gtk_Box;
      Event       : Gtk_Event_Box;
      Image       : Gtk_Image;
   begin
      Gtk.Dialog.Initialize
        (Dialog  => Win,
         Title   => Title,
         Parent  => Parent,
         Flags   => Modal or Destroy_With_Parent);

      Color := Parse ("#FF0000");
      Alloc (Get_Default_Colormap, Color);
      Win.Error_Style := Copy (Get_Style (Win));
      Set_Foreground (Win.Error_Style, State_Normal, Color);

      Color := Parse (Bg_Color);
      Alloc (Get_Default_Colormap, Color);
      Style := Copy (Get_Style (Win));
      Set_Background (Style, State_Normal, Color);
      Set_Font_Description (Style, Title_Font);

      Gtk_New_Hbox (Box, False, 0);
      Pack_Start (Get_Vbox (Win), Box, True, True, 0);

      --  Side box

      Gtk_New (Event);
      Set_Style (Event, Style);
      Pack_Start (Box, Event, False);

      Gtk_New_Vbox (Win.Side_Box, False, 6);
      Set_Border_Width (Win.Side_Box, 7);
      Add (Event, Win.Side_Box);

      Gtk_New
        (Image,
         Pixbuf => Render_Icon (Win, "adacore-logo", Icon_Size_Large_Toolbar));
      Pack_Start (Win.Side_Box, Image, Expand => False, Padding => 10);

      if not Show_Toc then
         Set_Size_Request (Event, 0, 0);
         Hide_All (Event);
         Set_Child_Visible (Event, False);
      end if;

      --  Title box

      Gtk_New_Vbox (Vbox, False, 0);
      Pack_Start (Box, Vbox, True, True, 0);

      Gtk_New (Event);
      Set_Style (Event, Style);
      Pack_Start (Vbox, Event, False);
      Gtk_New (Win.Title, Title);
      Set_Alignment (Win.Title, 0.5, 0.5);
      Set_Padding (Win.Title, 0, 10);
      Set_Line_Wrap (Win.Title, False);
      Set_Style (Win.Title, Style);
      Add (Event, Win.Title);

      --  Error box

      Style := Copy (Get_Style (Win));
      Set_Background (Style, State_Normal, White (Get_Default_Colormap));

      Gtk_New (Event);
      Set_Style (Event, Style);
      Pack_Start (Vbox, Event, False);

      Gtk_New (Win.Message, Title);
      Add (Event, Win.Message);
      Set_Alignment (Win.Message, 0.0, 0.5);
      Set_Justify (Win.Message, Justify_Left);
      Set_Line_Wrap (Win.Message, False);
      Set_Style (Win.Message, Win.Error_Style);
      Hide_All (Event);
      Set_Child_Visible (Event, False);

      --  Main content

      Gtk_New_Hbox (Win.Content, Homogeneous => True);
      Pack_Start (Vbox, Win.Content, True, True, 0);
      Set_Name (Win.Content, "wizard contents");  --  testsuite
   end Initialize;

   ---------------------
   -- Display_Message --
   ---------------------

   procedure Display_Message
     (Win      : access Logo_Box_Record;
      Msg      : String;
      As_Error : Boolean := False) is
   begin
      if Msg = "" then
         Hide_All (Get_Parent (Win.Message));
         Set_Child_Visible (Get_Parent (Win.Message), False);
      else
         Set_Text (Win.Message, Msg);
         Set_Child_Visible (Get_Parent (Win.Message), True);
         Show_All (Get_Parent (Win.Message));

         if As_Error then
            Set_Style (Win.Message, Win.Error_Style);
         else
            Set_Style (Win.Message, Get_Style (Win));
         end if;
      end if;
   end Display_Message;

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
