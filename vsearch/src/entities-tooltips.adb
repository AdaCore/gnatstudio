------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2006-2012, AdaCore                     --
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

with Ada.Strings.Unbounded;

with GNATCOLL.Symbols;          use GNATCOLL.Symbols;
with GNATCOLL.VFS;              use GNATCOLL.VFS;

with Cairo.Image_Surface;       use Cairo.Image_Surface;

with Pango.Font;                use Pango.Font;
with Pango.Layout;              use Pango.Layout;
with Gdk.Color;                 use Gdk.Color;
with Gdk.Display;               use Gdk.Display;
with Gdk.Pixbuf;                use Gdk.Pixbuf;
with Gdk.Screen;                use Gdk.Screen;
with Gdk.Window;                use Gdk.Window;
with Glib.Convert;              use Glib.Convert;
with Glib;                      use Glib;
with Gtk.Widget;                use Gtk.Widget;
with Gtkada.Style;              use Gtkada.Style;

with Entities.Tooltips_Assistant; use Entities.Tooltips_Assistant;
with Doc_Utils;                 use Doc_Utils;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with Language;                  use Language;
with Language.Icons;            use Language.Icons;
with Language.Tree;             use Language.Tree;
with Language_Handlers;         use Language_Handlers;
with String_Utils;              use String_Utils;

package body Entities.Tooltips is

   function Get_Instance (Entity_Ref : Entity_Reference) return String;
   --  Return the text describing from what instance the entity is

   function Get_Pixbuf (Entity : Entity_Information) return Gdk_Pixbuf;
   --  Return the image associated to an entity

   ----------------
   -- Get_Pixbuf --
   ----------------

   function Get_Pixbuf (Entity : Entity_Information) return Gdk_Pixbuf is
      Info : constant Tooltip_Information := Get_Tooltip_Information (Entity);
   begin
      return Entity_Icons (Info.Is_Spec, Info.Visibility) (Info.Category);
   end Get_Pixbuf;

   -----------------------
   -- Get_Documentation --
   -----------------------

   function Get_Documentation
     (Kernel : access Kernel_Handle_Record'Class;
      Entity : Entity_Information) return String
   is
      Handler    : constant Language_Handler := Get_Language_Handler (Kernel);
      Database   : constant Construct_Database_Access :=
                     Get_Construct_Database (Kernel);
      Comment_Found : aliased Boolean := False;
      Documentation : constant String := Get_Tooltip_Documentation
         (Handler       => Handler,
          Database      => Database,
          Entity        => Entity,
          Comment_Found => Comment_Found'Access);
   begin
      if Documentation = "" then
         --  Try to get the documentation from somewhere else than the
         --  construct database.
         return Escape_Text (Get_Documentation (Handler, Entity));
      elsif Comment_Found then
         return Documentation;
      else
         --  No comment found, try to get them from entities
         declare
            Comments : constant String :=
              Escape_Text (Get_Documentation (Handler, Entity));
         begin
            if Comments = "" then
               return Documentation;
            else
               return Comments & ASCII.LF & ASCII.LF & Documentation;
            end if;
         end;
      end if;
   end Get_Documentation;

   ------------------
   -- Get_Instance --
   ------------------

   function Get_Instance (Entity_Ref : Entity_Reference) return String is
      use Ada.Strings.Unbounded;
      Result  : Unbounded_String;
      Inst    : Entity_Instantiation;
      Inst_E  : Entity_Information;
      Inst_Of : Entity_Information;

   begin
      if Entity_Ref /= No_Entity_Reference then
         Inst := From_Instantiation_At (Entity_Ref);

         while Inst /= No_Instantiation loop
            Inst_E := Get_Entity (Inst);
            Inst_Of := Is_Instantiation_Of (Inst_E);

            if Inst_Of = null then
               Result := Result
                 & (-"from instance at ");
            else
               Result := Result
                 & (-"from instance of ")
                 & Get (Get_Name (Inst_Of)).all & ':'
                 & Display_Base_Name (Get_Filename
                     (Get_File (Get_Declaration_Of (Inst_Of)))) & ':'
                 & Image (Get_Line (Get_Declaration_Of (Inst_Of)))
                 & ASCII.LF & "  at ";
            end if;

            Result := Result
              & Get (Get_Name (Inst_E)).all
              & ':'
              &  Display_Base_Name (Get_Filename
                              (Get_File (Get_Declaration_Of (Inst_E)))) & ':'
              & Image (Get_Line (Get_Declaration_Of (Inst_E)))
              & ASCII.LF & ASCII.LF;
            Inst := Generic_Parent (Inst);
         end loop;
      end if;

      return To_String (Result);
   end Get_Instance;

   ------------------
   -- Draw_Tooltip --
   ------------------

   function Draw_Tooltip
     (Kernel      : access Kernel_Handle_Record'Class;
      Header      : String;
      Doc         : String;
      Pixbuf      : Gdk_Pixbuf;
      Draw_Border : Boolean;
      Guess       : Boolean := False) return Cairo.Cairo_Surface;
   --  Helper function, factorizing the tooltip widget creation

   function Draw_Tooltip
     (Kernel        : access Kernel_Handle_Record'Class;
      Entity        : Entity_Information;
      Ref           : Entity_Reference;
      Status        : Find_Decl_Or_Body_Query_Status;
      Accurate_Xref : Boolean;
      Draw_Border   : Boolean) return Cairo.Cairo_Surface
   is
      Doc : constant String :=
              Get_Instance (Ref) & Get_Documentation (Kernel, Entity);
   begin
      return Draw_Tooltip
        (Kernel      => Kernel,
         Guess       => Is_Tooltip_Guess (Status, Accurate_Xref),
         Header      => Get_Tooltip_Header (Entity),
         Pixbuf      => Get_Pixbuf (Entity),
         Draw_Border => Draw_Border,
         Doc         => Doc);
   end Draw_Tooltip;

   function Draw_Tooltip
     (Kernel      : access Kernel_Handle_Record'Class;
      Entity      : Entity_Access;
      Draw_Border : Boolean;
      Guess       : Boolean := False) return Cairo.Cairo_Surface
   is
      pragma Unreferenced (Guess);

      Construct : constant access Simple_Construct_Information :=
        Get_Construct (Entity);
   begin
      return Draw_Tooltip
        (Kernel => Kernel,
         Guess  => False,
         Header => "<b>" & Get (Get_Construct (Entity).Name).all & "</b>",
         Draw_Border => Draw_Border,
         Doc    => Get_Documentation
           (Get_Tree_Language (Get_File (Entity)), Entity, null),
         Pixbuf => Entity_Icons
           (Construct.Is_Declaration, Construct.Visibility)
           (Construct.Category));
   end Draw_Tooltip;

   function Draw_Tooltip
     (Kernel      : access Kernel_Handle_Record'Class;
      Header      : String;
      Doc         : String;
      Pixbuf      : Gdk_Pixbuf;
      Draw_Border : Boolean;
      Guess       : Boolean := False) return Cairo.Cairo_Surface
   is
      Widget : constant Gtk_Widget := Gtk_Widget (Get_Main_Window (Kernel));
      Pixmap : Cairo.Cairo_Surface;
      Cr     : Cairo.Cairo_Context;

      Font   : constant Pango_Font_Description := Default_Font.Get_Pref_Font;
      Fixed  : constant Pango_Font_Description := View_Fixed_Font.Get_Pref;

      Header_Layout, Doc_Layout : Pango_Layout;

      Width, Height, W1, H1, W2, H2 : Gint := 0;
      Color  : Gtkada.Style.Cairo_Color;
      Max_Height, Max_Width : Gint;

      H_Pad : constant := 4;
      V_Pad : constant := 3;
   begin
      Header_Layout := Create_Pango_Layout (Widget, "");

      if Guess then
         Set_Markup
           (Header_Layout, "<span foreground =""#555555"">" &
              Get_Tooltip_Guess_Message & "</span>" & ASCII.LF & Header);
      else
         Set_Markup (Header_Layout, Header);
      end if;

      Set_Font_Description (Header_Layout, Font);
      Get_Pixel_Size (Header_Layout, W1, H1);
      Height := Height + V_Pad * 2 + H1;

      if Doc /= "" then
         Doc_Layout := Create_Pango_Layout (Widget, "");
         Set_Markup (Doc_Layout, Doc);
         Set_Font_Description (Doc_Layout, Fixed);
         Get_Pixel_Size (Doc_Layout, W2, H2);
         Height := Height + V_Pad * 2 + 1 + H2;
      end if;

      Width  := Gint'Max (W1 + Get_Width (Pixbuf) + H_Pad * 2, W2 + H_Pad * 2);

      Color := To_Cairo (Tooltip_Color.Get_Pref);

      Max_Height := Get_Height (Get_Default_Screen (Gdk.Display.Get_Default));
      Height := Gint'Min (Height, Max_Height);

      Max_Width := Get_Width (Get_Default_Screen (Gdk.Display.Get_Default));
      Width := Gint'Min (Width, Max_Width);

      Pixmap := Create (Cairo_Format_ARGB32, Width, Height);
      Cr := Create (Pixmap);
      Set_Line_Width (Cr, 0.5);
      Draw_Rectangle (Cr, Color, True, 0, 0, Width, Height);

      Color := To_Cairo (Gdk_Color'(Black (Get_Default_Colormap)));
      if Draw_Border then
         Draw_Rectangle (Cr, Color, False, 0, 0, Width, Height);
      end if;

      Draw_Pixbuf (Cr, Pixbuf, V_Pad, H_Pad);

      Draw_Layout
        (Cr, Color, V_Pad + Get_Width (Pixbuf), H_Pad, Header_Layout);
      Unref (Header_Layout);

      if Doc_Layout /= null then
         Draw_Line (Cr, Color, 0, H1 + V_Pad * 2, Width - 1, H1 + V_Pad * 2);
         Draw_Layout (Cr, Color, H_Pad, H1 + 1 + V_Pad * 3, Doc_Layout);
         Unref (Doc_Layout);
      end if;

      Destroy (Cr);

      return Pixmap;
   end Draw_Tooltip;

end Entities.Tooltips;
