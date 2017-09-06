------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2006-2017, AdaCore                     --
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

with Gtkada.Style;              use Gtkada.Style;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Image;                 use Gtk.Image;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Separator;             use Gtk.Separator;
with Gtk.Widget;                use Gtk.Widget;

with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with Language;                  use Language;
with Language.Icons;            use Language.Icons;
with Tooltips;
with Xref;                      use Xref;

with Entities_Tooltips_Utility; use Entities_Tooltips_Utility;

package body Entities_Tooltips is
   function Get_Pixbuf
     (Kernel : access Kernel_Handle_Record'Class;
      Entity : Root_Entity'Class) return String;
   --  Return the image (icon-name) associated to an entity

   function Draw_Tooltip
     (Header      : String;
      Doc         : String;
      Icon_Name   : String;
      Draw_Border : Boolean;
      Guess       : Boolean := False) return Gtk_Widget;
   --  Helper function, factorizing the tooltip widget creation

   ----------------
   -- Get_Pixbuf --
   ----------------

   function Get_Pixbuf
     (Kernel : access Kernel_Handle_Record'Class;
      Entity : Root_Entity'Class) return String
   is
      Info : constant Tooltip_Information :=
        Get_Tooltip_Information (Kernel, Entity);
   begin
      return Stock_From_Category
        (Info.Is_Spec, Info.Visibility, Info.Category);
   end Get_Pixbuf;

   ------------------
   -- Draw_Tooltip --
   ------------------

   function Draw_Tooltip
     (Kernel        : access Kernel_Handle_Record'Class;
      Entity        : Root_Entity'Class;
      Ref           : Root_Entity_Reference'Class;
      Draw_Border   : Boolean) return Gtk_Widget is
   begin
      return Draw_Tooltip
        (Guess       => Is_Guess (Entity),
         Header      => Get_Tooltip_Header (Kernel, Entity),
         Icon_Name   => Get_Pixbuf (Kernel, Entity),
         Draw_Border => Draw_Border,
         Doc         => Get_Tooltip_Documentation (Kernel, Entity, Ref));
   end Draw_Tooltip;

   ------------------
   -- Draw_Tooltip --
   ------------------

   function Draw_Tooltip
     (Kernel      : access Kernel_Handle_Record'Class;
      Entity      : Semantic_Node'Class;
      Draw_Border : Boolean;
      Guess       : Boolean := False) return Gtk_Widget
   is
      pragma Unreferenced (Guess, Kernel);
   begin
      return Draw_Tooltip
        (Guess       => False,
         Header      => Entity.Documentation_Header,
         Draw_Border => Draw_Border,
         Doc         => Entity.Documentation_Body,
         Icon_Name => Stock_From_Category
           (Entity.Is_Declaration,
            Entity.Visibility,
            Entity.Category));
   end Draw_Tooltip;

   ------------------
   -- Draw_Tooltip --
   ------------------

   function Draw_Tooltip
     (Header      : String;
      Doc         : String;
      Icon_Name   : String;
      Draw_Border : Boolean;
      Guess       : Boolean := False)
     return Gtk_Widget
   is
      pragma Unreferenced (Draw_Border);

      Header_Label, Doc_Label : Gtk_Label;
      Box, Hbox : Gtk_Box;
      Image : Gtk_Image;
      Sep : Gtk_Separator;

   begin
      Gtk_New_Vbox (Box, Homogeneous => False);

      Gtk_New_Hbox (Hbox, Homogeneous => False);
      Box.Pack_Start (Hbox, Expand => False, Fill => False);

      if Icon_Name /= "" then
         Gtk_New_From_Icon_Name
           (Image, Icon_Name => Icon_Name,
            Size => Icon_Size_Small_Toolbar);
         Image.Set_Alignment (0.0, 0.0);
         Hbox.Pack_Start (Image, Expand => False, Fill => False);
      end if;

      Gtk_New (Header_Label);
      Header_Label.Set_Alignment (0.0, 0.5);
      Hbox.Pack_Start (Header_Label, Expand => True, Fill => True);

      if Guess then
         Header_Label.Set_Markup
           ("<span foreground ="""
            & To_Hex (Shade_Or_Lighten (Tooltips.Tooltips_Foreground_Color))
            & """>"
            & Tooltip_Guess_Message & "</span>" & ASCII.LF & Header);
      else
         Header_Label.Set_Markup (Header);
      end if;

      if Doc /= "" then
         Gtk_New_Hseparator (Sep);
         Box.Pack_Start (Sep, Expand => False, Fill => False, Padding => 5);

         Gtk_New (Doc_Label);
         Doc_Label.Set_Alignment (0.0, 0.5);
         Box.Pack_Start (Doc_Label, Expand => True, Fill => True);

         Doc_Label.Override_Font (View_Fixed_Font.Get_Pref);

         Doc_Label.Set_Markup (Doc);
      end if;

      return Gtk_Widget (Box);
   end Draw_Tooltip;

end Entities_Tooltips;
