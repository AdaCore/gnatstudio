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

with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;

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
with Glib;                      use Glib;
with Glib.Convert;              use Glib.Convert;
with Gtk.Widget;                use Gtk.Widget;
with Gtkada.Style;              use Gtkada.Style;

with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with Language;                  use Language;
with Language.Icons;            use Language.Icons;
with Language.Tree;             use Language.Tree;
with String_Utils;              use String_Utils;
with Xref;                      use Xref;

package body Entities_Tooltips is

   type Tooltip_Information is record
      Is_Spec    : Boolean;
      Visibility : Construct_Visibility;
      Category   : Language_Category;
   end record;

   function Get_Tooltip_Information
     (Kernel : access Kernel_Handle_Record'Class;
      Entity : General_Entity) return Tooltip_Information;
   --  Return information to be able to display the right icon
   --  depending on category and visibility.

   function Get_Instance
     (Db         : access General_Xref_Database_Record'Class;
      Entity_Ref : General_Entity_Reference) return String;
   --  Return the text describing from what instance the entity is

   function Get_Pixbuf
     (Kernel : access Kernel_Handle_Record'Class;
      Entity : General_Entity) return Gdk_Pixbuf;
   --  Return the image associated to an entity

   function Draw_Tooltip
     (Kernel      : access Kernel_Handle_Record'Class;
      Header      : String;
      Doc         : String;
      Pixbuf      : Gdk_Pixbuf;
      Draw_Border : Boolean;
      Guess       : Boolean := False) return Cairo.Cairo_Surface;
   --  Helper function, factorizing the tooltip widget creation

   function Get_Tooltip_Header
     (Kernel : access Kernel_Handle_Record'Class;
      Entity : General_Entity) return String;
   --  Return the header of the tooltip

   ------------------------
   -- Get_Tooltip_Header --
   ------------------------

   function Get_Tooltip_Header
     (Kernel : access Kernel_Handle_Record'Class;
      Entity : General_Entity) return String
   is
      Decl : General_Entity_Declaration;
      Attrs : Unbounded_String;
   begin
      Decl := Kernel.Databases.Get_Declaration (Entity);

--        if Get_Kind (Entity).Kind = Include_File then
--           return  "<b>"
--             & Escape_Text (Kernel.Databases.Get_Name (Entity))
--             & "</b>" & ASCII.LF
--             & Kernel.Databases.Get_Display_Kind (Entity) & ' '
--             & Decl.Loc.File.Display_Full_Name;
--        else
         if Kernel.Databases.Is_Global (Entity) then
            Append (Attrs, "global ");
         elsif Kernel.Databases.Is_Static_Local (Entity) then
            Append (Attrs, "static ");
         end if;

         return  "<b>"
           & Escape_Text (Kernel.Databases.Qualified_Name (Entity))
           & "</b>" & ASCII.LF
           & To_String (Attrs)
           & Kernel.Databases.Get_Display_Kind (Entity)
           & (-" declared at ")
           & Decl.Loc.File.Display_Base_Name & ':'
           & Image (Decl.Loc.Line);
--        end if;
   end Get_Tooltip_Header;

   ----------------
   -- Get_Pixbuf --
   ----------------

   function Get_Pixbuf
     (Kernel : access Kernel_Handle_Record'Class;
      Entity : General_Entity) return Gdk_Pixbuf
   is
      Info : constant Tooltip_Information :=
        Get_Tooltip_Information (Kernel, Entity);
   begin
      return Entity_Icons (Info.Is_Spec, Info.Visibility) (Info.Category);
   end Get_Pixbuf;

   ------------------
   -- Get_Instance --
   ------------------

   function Get_Instance
     (Db         : access General_Xref_Database_Record'Class;
      Entity_Ref : General_Entity_Reference) return String
   is
      use Ada.Strings.Unbounded;
      Result  : Unbounded_String;
      Inst_E  : General_Entity;
      Inst_Of : General_Entity;
      Loc     : General_Location;

   begin
      if Entity_Ref /= No_General_Entity_Reference then
         declare
            Insts : constant Xref.Entity_Array :=
              Db.From_Instances (Entity_Ref);
         begin
            for Inst in Insts'Range loop
               Inst_E := Insts (Inst);
               Inst_Of := Db.Instance_Of (Inst_E);

               if Inst_Of = No_General_Entity then
                  Append (Result,  -"from instance at ");
               else
                  Loc := Db.Get_Declaration (Inst_Of).Loc;
                  Append
                    (Result,
                     (-"from instance of ")
                     & Db.Get_Name (Inst_Of) & ':'
                     & Loc.File.Display_Base_Name & ':'
                     & Image (Loc.Line) & ASCII.LF & "  at ");
               end if;

               Loc := Db.Get_Declaration (Inst_E).Loc;
               Append
                 (Result,
                  Db.Get_Name (Inst_E) & ':'
                  & Loc.File.Display_Base_Name & ':'
                  & Image (Loc.Line) & ASCII.LF & ASCII.LF);
            end loop;
         end;
      end if;

      return To_String (Result);
   end Get_Instance;

   ------------------
   -- Draw_Tooltip --
   ------------------

   function Draw_Tooltip
     (Kernel        : access Kernel_Handle_Record'Class;
      Entity        : General_Entity;
      Ref           : General_Entity_Reference;
      Draw_Border   : Boolean) return Cairo.Cairo_Surface
   is
      Doc : constant String :=
        Get_Instance (Kernel.Databases, Ref)
        & Kernel.Databases.Documentation
           (Handler => Kernel.Get_Language_Handler,
            Entity  => Entity);
   begin
      return Draw_Tooltip
        (Kernel      => Kernel,
         Guess       => Is_Fuzzy (Entity),
         Header      => Get_Tooltip_Header (Kernel, Entity),
         Pixbuf      => Get_Pixbuf (Kernel, Entity),
         Draw_Border => Draw_Border,
         Doc         => Doc);
   end Draw_Tooltip;

   ------------------
   -- Draw_Tooltip --
   ------------------

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
         Doc => Kernel.Databases.Documentation
           (Handler => Kernel.Get_Language_Handler,
            Entity  => From_Constructs (Entity)),
         Pixbuf => Entity_Icons
           (Construct.Is_Declaration, Construct.Visibility)
           (Construct.Category));
   end Draw_Tooltip;

   ------------------
   -- Draw_Tooltip --
   ------------------

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
              Tooltip_Guess_Message & "</span>" & ASCII.LF & Header);
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

   -----------------------------
   -- Get_Tooltip_Information --
   -----------------------------

   function Get_Tooltip_Information
     (Kernel : access Kernel_Handle_Record'Class;
      Entity : General_Entity) return Tooltip_Information
   is
      Tooltip_Info : Tooltip_Information;

   begin
      Tooltip_Info.Visibility := Visibility_Public;
      Tooltip_Info.Category := Cat_Variable;
      Tooltip_Info.Is_Spec := False;

      if Kernel.Databases.Is_Subprogram (Entity) then
         Tooltip_Info.Category := Cat_Function;
      elsif Kernel.Databases.Is_Type (Entity) then
         Tooltip_Info.Category := Cat_Type;
      elsif Kernel.Databases.Is_Container (Entity) then
         Tooltip_Info.Category := Cat_Package;
      end if;

      --  When we were using SourceNavigator for the C++ xref, we used to know
      --  about the private/protected/public visibility of entities, but this
      --  is no longer the case with g++-based xref.
      return Tooltip_Info;
   end Get_Tooltip_Information;

end Entities_Tooltips;
