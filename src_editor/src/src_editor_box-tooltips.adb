------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2012, AdaCore                     --
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

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with GNAT.Strings;

with Cairo;                     use Cairo;
with Cairo.Image_Surface;       use Cairo.Image_Surface;
with Cairo.Surface;             use Cairo.Surface;

with Glib;                      use Glib;
with Gdk.Color;                 use Gdk, Gdk.Color;
with Gdk.Cairo;                 use Gdk.Cairo;
with Gdk.Rectangle;             use Gdk.Rectangle;
with Gdk.Types;
with Gdk.Pixbuf;                use Gdk.Pixbuf;
with Gdk.Window;                use Gdk.Window;
with Gtk.Text_Iter;             use Gtk.Text_Iter;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Widget;                use Gtk.Widget;
with Pango.Cairo;               use Pango.Cairo;
with Pango.Font;                use Pango.Font;
with Pango.Layout;              use Pango.Layout;

with Entities;                  use Entities;
with Entities.Queries;          use Entities.Queries;
with Entities.Tooltips;         use Entities.Tooltips;
with GPS.Kernel.Contexts;       use GPS.Kernel, GPS.Kernel.Contexts;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Modules.UI;     use GPS.Kernel.Modules.UI;
with GUI_Utils;                 use GUI_Utils;
with Src_Editor_View;           use Src_Editor_View;
with Src_Editor_Buffer.Line_Information;
use Src_Editor_Buffer.Line_Information;
with Tooltips;                  use Tooltips;
with Traces;                    use Traces;
with GNATCOLL.VFS;              use GNATCOLL.VFS;
with GPS.Editors;               use GPS.Editors;

package body Src_Editor_Box.Tooltips is

   V_Padding : constant := 4;
   H_Padding : constant := 5;
   --  Padding in tooltips, in cairo units

   Me : constant Debug_Handle := Create ("Editor.Tooltips");

   package Pixmap_List is new Ada.Containers.Doubly_Linked_Lists
     (Cairo.Cairo_Surface);

   type Editor_Tooltips is new Standard.Tooltips.Pixmap_Tooltips with record
      Box : Source_Editor_Box;
   end record;

   function To_Single_Pixmap (List : Pixmap_List.List) return Cairo_Surface;
   --  Concatenate all pixmaps in List to create one single pixmap suitable for
   --  displaying in a tooltip.
   --  This frees the pixmaps allocated in List.

   procedure Draw_Content
     (Content     : Unbounded_String;
      Icon        : Gdk_Pixbuf;
      Widget      : Source_View;
      Draw_Border : Boolean;
      Pixmap      : out Cairo_Surface);
   --  Render a string to a pixmap

   overriding procedure Draw
     (Tooltip : access Editor_Tooltips;
      Pixmap  : out Cairo.Cairo_Surface;
      Area    : out Gdk.Rectangle.Gdk_Rectangle);
   --  See inherited documentation

   procedure Get_Declaration_Info
     (Editor  : access Source_Editor_Box_Record;
      Context : Selection_Context;
      Entity  : out Entity_Information;
      Ref     : out Entity_Reference;
      Status  : out Find_Decl_Or_Body_Query_Status);
   --  Perform a cross-reference to the declaration of the entity located at
   --  (Line, Column) in Editor. Fail silently when no declaration or no
   --  entity can be located, and set File_Decl to null.
   --  Entity is set to the entity that was found, or No_Entity_Information if
   --  not found. It must be destroyed by the caller.
   --  Ref is the closest reference to the entity from Context. It might not be
   --  set if we haven't found this information

   ---------------------
   -- Create_Tooltips --
   ---------------------

   function Create_Tooltips
     (Box : access Source_Editor_Box_Record'Class)
      return Standard.Tooltips.Tooltips_Access is
   begin
      return new Editor_Tooltips'
        (Pixmap_Tooltips with Box => Source_Editor_Box (Box));
   end Create_Tooltips;

   --------------------------
   -- Get_Declaration_Info --
   --------------------------

   procedure Get_Declaration_Info
     (Editor  : access Source_Editor_Box_Record;
      Context : Selection_Context;
      Entity  : out Entity_Information;
      Ref     : out Entity_Reference;
      Status  : out Find_Decl_Or_Body_Query_Status)
   is
      Filename : constant Virtual_File := Get_Filename (Editor);
   begin
      Ref := No_Entity_Reference;

      if Filename = GNATCOLL.VFS.No_File then
         Entity := null;
         Status := Entity_Not_Found;
         return;
      end if;

      Push_State (Editor.Kernel, Busy);

      --  Don't use Find_Declaration_Or_Overloaded, since we don't want to
      --  ask the user interactively for the tooltips.
      Find_Declaration
        (Db          => Get_Database (Editor.Kernel),
         File_Name   => Get_Filename (Editor),
         Entity_Name => Entity_Name_Information (Context),
         Line        => Contexts.Line_Information (Context),
         Column      => Entity_Column_Information (Context),
         Entity      => Entity,
         Closest_Ref => Ref,
         Status      => Status);

      Pop_State (Editor.Kernel);

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         Pop_State (Editor.Kernel);
         Entity := null;
   end Get_Declaration_Info;

   ----------------------
   -- To_Single_Pixmap --
   ----------------------

   function To_Single_Pixmap (List : Pixmap_List.List) return Cairo_Surface
   is
      Separator_Width             : constant := 1;
      Pixmap, Result              : Cairo_Surface;
      Width, Height               : Gint := 0;
      Pixmap_Width, Pixmap_Height : Gint;
      Current_Y                   : Gdouble := 0.0;

      use Pixmap_List;
      Cursor : Pixmap_List.Cursor;

      Cr     : Cairo_Context;
   begin
      --  Compute sizes
      Cursor := List.First;

      while Has_Element (Cursor) loop
         Pixmap := Element (Cursor);
         Pixmap_Width := Get_Width (Pixmap);
         Pixmap_Height := Get_Height (Pixmap);
         Width := Gint'Max (Pixmap_Width, Width);
         Height := Height + Pixmap_Height + Separator_Width;
         Next (Cursor);
      end loop;

      --  Create the final pixmap

      Result := Cairo.Image_Surface.Create
        (Cairo_Format_ARGB32, Width, Height);
      Cr := Create (Result);

      --  Background
      Set_Source_Color (Cr, Tooltip_Color.Get_Pref);

      Cairo.Rectangle (Cr, 0.0, 0.0, Gdouble (Width), Gdouble (Height));
      Cairo.Fill (Cr);

      --  Render and free the pixmaps list
      Cursor := List.First;

      while Has_Element (Cursor) loop
         Pixmap := Element (Cursor);
         Pixmap_Height := Cairo.Image_Surface.Get_Height (Pixmap);

         Save (Cr);
         Translate (Cr, 0.0, Current_Y);
         Cairo.Set_Source_Surface (Cr, Pixmap, 0.0, 0.0);
         Paint (Cr);
         Restore (Cr);

         Current_Y := Current_Y + Gdouble (Pixmap_Height);

         Next (Cursor);
         if Has_Element (Cursor) then
            Current_Y := Current_Y + Gdouble (Separator_Width);
            Move_To (Cr, 0.0, Current_Y);
            Set_Source_Rgba (Cr, 0.0, 0.0, 0.0, 0.5);
            Rel_Line_To (Cr, Gdouble (Width), 0.0);
            Stroke (Cr);
         end if;

         Destroy (Pixmap);
      end loop;

      --  Border
      Cairo.Rectangle (Cr, 0.0, 0.0, Gdouble (Width), Gdouble (Height));
      Set_Source_Color (Cr, Black (Get_Default_Colormap));
      Stroke (Cr);

      Destroy (Cr);

      return Result;
   end To_Single_Pixmap;

   ------------------
   -- Draw_Content --
   ------------------

   procedure Draw_Content
     (Content     : Unbounded_String;
      Icon        : Gdk_Pixbuf;
      Widget      : Source_View;
      Draw_Border : Boolean;
      Pixmap      : out Cairo_Surface)
   is
      Layout : Pango_Layout;
      Cr     : Cairo_Context;
      Font   : constant Pango_Font_Description := Default_Font.Get_Pref_Font;

      Width,
      Height,
      Icon_Width,
      Icon_Height,
      Layout_Height : Gint := 0;

   begin
      Layout := Create_Pango_Layout (Widget, "");
      Set_Font_Description (Layout, Font);
      Set_Markup (Layout, To_String (Content));

      Get_Pixel_Size (Layout, Width, Layout_Height);

      if Icon = null then
         Width := Width + H_Padding * 2;
      else
         Icon_Width  := Get_Width (Icon);
         Icon_Height := Get_Height (Icon);
         Width := Width + Icon_Width + H_Padding * 3;
      end if;

      Height := Gint'Max (Layout_Height, Icon_Height) + V_Padding * 2;

      Pixmap := Create (Cairo_Format_ARGB32, Width, Height);

      Cr := Create (Pixmap);
      Set_Source_Color (Cr, Tooltip_Color.Get_Pref);

      Cairo.Rectangle
        (Cr, 0.0, 0.0, Gdouble (Width), Gdouble (Height));
      Fill_Preserve (Cr);

      if Draw_Border then
         Set_Source_Rgb (Cr, 0.0, 0.0, 0.0);
         Stroke (Cr);
      end if;

      if Icon = null then
         Move_To (Cr, Gdouble (H_Padding), Gdouble (V_Padding));
      else
         Move_To (Cr, Gdouble (H_Padding * 2 + Icon_Width),
                  Gdouble (V_Padding));
      end if;

      Set_Source_Rgb (Cr, 0.0, 0.0, 0.0);
      Show_Layout (Cr, Layout);
      Unref (Layout);

      if Icon /= null then
         Translate (Cr,
                    Gdouble (H_Padding),
                    Gdouble (V_Padding + (Layout_Height - Icon_Height) / 2));
         Set_Source_Pixbuf (Cr, Icon, 0.0, 0.0);
         Paint (Cr);
      end if;

      Destroy (Cr);
   end Draw_Content;

   ----------
   -- Draw --
   ----------

   overriding procedure Draw
     (Tooltip : access Editor_Tooltips;
      Pixmap  : out Cairo.Cairo_Surface;
      Area    : out Gdk.Rectangle.Gdk_Rectangle)
   is
      use type GNAT.Strings.String_Access;
      Box              : constant Source_Editor_Box := Tooltip.Box;
      Widget           : constant Source_View := Get_View (Tooltip.Box);
      Line, Col        : Gint;
      Mouse_X, Mouse_Y : Gint;
      Win_X, Win_Y     : Gint;
      Start_Iter       : Gtk_Text_Iter;
      End_Iter         : Gtk_Text_Iter;
      Mask             : Gdk.Types.Gdk_Modifier_Type;
      Win              : Gdk.Gdk_Window;
      Location         : Gdk_Rectangle;
      Out_Of_Bounds    : Boolean;
      Window           : Gdk.Gdk_Window;
      Window_Width     : Gint;
      Window_Height    : Gint;
      Window_Depth     : Gint;
      Line_Info        : Line_Info_Width_Array_Access;

   begin
      Pixmap := Null_Surface;
      Area   := (0, 0, 0, 0);

      if not Display_Tooltip.Get_Pref then
         return;
      end if;

      Window := Get_Window (Widget, Text_Window_Text);

      Get_Geometry
        (Window, Win_X, Win_Y, Window_Width, Window_Height, Window_Depth);
      Get_Pointer
        (Window, Mouse_X, Mouse_Y, Mask, Win);

      if Mouse_X < Win_X
        and then Mouse_Y > Win_Y
        and then Win_X + Window_Width > Mouse_X
        and then Win_Y + Window_Height > Mouse_Y
      then
         --  In the side column, see if a tooltip information is to be
         --  displayed.
         Window_To_Buffer_Coords
           (Widget, Win_X, Mouse_Y, Line, Col, Out_Of_Bounds);

         declare
            Content       : Unbounded_String;
            Has_Info      : Boolean := False;
            Action        : GPS.Kernel.Messages.Action_Item;
            Icon          : Gdk_Pixbuf;

            C : Message_List.Cursor;
         begin
            Line_Info := Get_Side_Information
              (Box.Source_Buffer,
               Buffer_Line_Type (Line + 1));

            --  Concatenate the tooltip information for all columns

            if Line_Info /= null then
               for K in Line_Info'Range loop
                  C := Line_Info (K).Messages.Last;

                  while Message_List.Has_Element (C) loop
                     Action := Message_List.Element (C).Get_Action;

                     Icon := null;
                     if Action /= null then
                        Icon := Action.Image;

                        if Action.Tooltip_Text /= null then
                           if Content /= Null_Unbounded_String then
                              Append (Content, ASCII.LF);
                           end if;

                           Append (Content, Action.Tooltip_Text.all);
                           Has_Info := True;
                        end if;
                     end if;

                     Message_List.Previous (C);
                  end loop;
               end loop;
            end if;

            if Has_Info then
               Draw_Content (Content, Icon, Widget, True, Pixmap);
            end if;
         end;

         return;
      end if;

      Window_To_Buffer_Coords
        (Widget, Mouse_X, Mouse_Y, Line, Col, Out_Of_Bounds);

      if Out_Of_Bounds then
         --  Do not display a tooltip in an invalid location,
         --  for example after the end of a line.

         return;
      end if;

      Line_Info := Get_Side_Information
        (Box.Source_Buffer,
         Buffer_Line_Type (Line + 1));

      Get_Iter_At_Line_Offset (Box.Source_Buffer, Start_Iter, Line, Col);
      Search_Entity_Bounds (Start_Iter, End_Iter);
      Get_Screen_Position (Box.Source_Buffer, Start_Iter, Line, Col);

      --  Compute the area surrounding the entity, relative to the pointer
      --  coordinates.

      Get_Iter_Location (Widget, Start_Iter, Location);
      Buffer_To_Window_Coords
        (Widget, Text_Window_Text, Location.X, Location.Y, Area.X, Area.Y);
      Get_Iter_Location (Widget, End_Iter, Location);
      Buffer_To_Window_Coords
        (Widget, Text_Window_Text, Location.X, Location.Y, Win_X, Win_Y);

      Area.Width  := Win_X - Area.X + Location.Width;
      Area.Height := Win_Y - Area.Y + Location.Height;

      declare
         Tmp_X, Tmp_Y, Tmp_Width, Tmp_Height, Tmp_Depth : Gint;
      begin
         Get_Geometry (Get_Window (Widget, Text_Window_Left),
                       Tmp_X, Tmp_Y, Tmp_Width, Tmp_Height, Tmp_Depth);
         Area.X := Area.X + Tmp_Width;
      end;

      declare
         Entity     : Entity_Information;
         Entity_Ref : Entity_Reference;
         Status     : Find_Decl_Or_Body_Query_Status;
         Context    : Selection_Context := New_Context;

         Pix        : Cairo.Cairo_Surface;
         Pixmaps    : Pixmap_List.List;
      begin
         Get_Contextual_Menu
           (Context  => Context,
            Kernel   => Box.Kernel,
            Object   => Box,
            Location => Location_Mouse);

         Trace (Me, "Tooltip on " & Entity_Name_Information (Context));
         Compute_Tooltip (Box.Kernel, Context, Pixmap);

         if Pixmap /= Null_Surface then
            return;
         end if;

         --  If there is a message on this line, display it

         if Line_Info /= null then
            for J in Line_Info'Range loop
               declare
                  C : Message_List.Cursor;
                  Message : Message_Access;
                  Text    : Unbounded_String;
                  Icon    : Gdk_Pixbuf;
               begin
                  C := Line_Info (J).Messages.Last;

                  while Message_List.Has_Element (C) loop
                     Message := Message_List.Element (C);

                     declare
                        M : constant GPS.Editors.Editor_Mark'Class
                          := Message.Get_Editor_Mark;
                     begin
                        if Col + 1 >= Gint (M.Column)
                          and then Col + 1 <= Gint
                            (M.Column
                             + Visible_Column_Type
                               (Message.Get_Highlighting_Length))
                        then
                           if Text /= Null_Unbounded_String then
                              Text := Text & ASCII.LF;
                           end if;

                           Text := Text & Message.Get_Text;
                        end if;
                     end;
                     Message_List.Previous (C);
                  end loop;

                  if Text /= Null_Unbounded_String then
                     Icon := null;

                     if Message.Get_Action /= null then
                        Icon := Message.Get_Action.Image;
                     end if;

                     Draw_Content (Text, Icon, Widget, False, Pix);
                     Pixmaps.Append (Pix);
                  end if;
               end;
            end loop;
         end if;

         --  If the mouse is not on top of text, do not display a tooltip

         if Entity_Name_Information (Context) = "" then
            return;
         end if;

         --  No module wants to handle this tooltip. Default to built-in
         --  tooltip, based on cross references.

         Get_Declaration_Info
           (Box, Context, Entity, Entity_Ref, Status);

         if Entity = null then
            return;
         end if;

         --  Ref the entity, so that if Draw_Tooltip regenerates the xref info,
         --  we are sure to always have a valid entity reference.
         Ref (Entity);
         Pix := Draw_Tooltip
           (Box.Kernel, Entity, Entity_Ref, Status,
            Box.Source_Buffer.Get_Language.Get_Language_Context.Accurate_Xref,
            Draw_Border => False);

         Pixmaps.Prepend (Pix);
         Pixmap := To_Single_Pixmap (Pixmaps);

         Unref (Entity);
      end;

   exception
      when E : others => Trace (Exception_Handle, E);
   end Draw;

end Src_Editor_Box.Tooltips;
