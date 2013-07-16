------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2013, AdaCore                     --
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
with GNAT.Strings;

with Glib;                      use Glib;
with Gdk.Rectangle;             use Gdk.Rectangle;
with Gdk.Pixbuf;                use Gdk.Pixbuf;
with Gdk.Window;                use Gdk.Window;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Image;                 use Gtk.Image;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Text_Iter;             use Gtk.Text_Iter;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Widget;                use Gtk.Widget;

with Entities_Tooltips;
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
with GNATCOLL.Xref;
with GPS.Editors;               use GPS.Editors;
with Xref;                      use Xref;

package body Src_Editor_Box.Tooltips is
   use type GNATCOLL.Xref.Visible_Column;

   Me : constant Debug_Handle := Create ("Editor.Tooltips");

   type Editor_Tooltips is new Standard.Tooltips.Tooltips with record
      Box : Source_Editor_Box;
   end record;

   overriding function Create_Contents
     (Tooltip  : not null access Editor_Tooltips;
      Widget   : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      X, Y     : Glib.Gint) return Gtk.Widget.Gtk_Widget;
   --  See inherited documentation

   procedure Get_Declaration_Info
     (Editor  : access Source_Editor_Box_Record;
      Context : Selection_Context;
      Entity  : out General_Entity;
      Ref     : out General_Entity_Reference);
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
        (Standard.Tooltips.Tooltips with Box => Source_Editor_Box (Box));
   end Create_Tooltips;

   --------------------------
   -- Get_Declaration_Info --
   --------------------------

   procedure Get_Declaration_Info
     (Editor  : access Source_Editor_Box_Record;
      Context : Selection_Context;
      Entity  : out General_Entity;
      Ref     : out General_Entity_Reference)
   is
      Filename : constant Virtual_File := Get_Filename (Editor);
   begin
      Ref := No_General_Entity_Reference;
      Entity := No_General_Entity;

      if Filename = GNATCOLL.VFS.No_File then
         return;
      end if;

      Push_State (Editor.Kernel, Busy);

      Editor.Kernel.Databases.Find_Declaration_Or_Overloaded
        (Loc => (File   => Get_Filename (Editor),
                 Line   => Contexts.Line_Information (Context),
                 Column => Entity_Column_Information (Context)),
         Entity_Name => Entity_Name_Information (Context),
         Entity      => Entity,
         Closest_Ref => Ref);

      Pop_State (Editor.Kernel);

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         Pop_State (Editor.Kernel);
   end Get_Declaration_Info;

   ---------------------
   -- Create_Contents --
   ---------------------

   overriding function Create_Contents
     (Tooltip  : not null access Editor_Tooltips;
      Widget   : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      X, Y     : Glib.Gint) return Gtk.Widget.Gtk_Widget
   is
      pragma Unreferenced (Widget);
      use type GNAT.Strings.String_Access;
      Box              : constant Source_Editor_Box := Tooltip.Box;
      View             : constant Source_View := Get_View (Tooltip.Box);
      Line, Col        : Gint;
      Win_X, Win_Y     : Gint;
      Start_Iter       : Gtk_Text_Iter;
      End_Iter         : Gtk_Text_Iter;
      Location         : Gdk_Rectangle;
      Out_Of_Bounds    : Boolean;
      Window           : Gdk.Gdk_Window;
      Window_Width     : Gint;
      Window_Height    : Gint;
      Line_Info        : Line_Info_Width_Array_Access;
      Vbox             : Gtk_Box;
      Label            : Gtk_Label;
      Area             : Gdk_Rectangle;
      Image            : Gtk_Image;
      HBox             : Gtk_Box;

   begin
      if not Display_Tooltip.Get_Pref then
         return null;
      end if;

      Window := Get_Window (View, Text_Window_Text);

      Get_Geometry (Window, Win_X, Win_Y, Window_Width, Window_Height);

      if X < Win_X
        and then Y > Win_Y
        and then Win_X + Window_Width > X
        and then Win_Y + Window_Height > Y
      then
         --  In the side column, see if a tooltip information is to be
         --  displayed.
         Window_To_Buffer_Coords
           (View, Win_X, Y, Line, Col, Out_Of_Bounds);

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
               if Icon /= null then
                  Gtk_New (Label, To_String (Content));
                  return Gtk_Widget (Label);
               else
                  Gtk_New_Hbox (HBox, Homogeneous => False);
                  Gtk_New (Image, Icon);
                  HBox.Pack_Start (Image, Expand => False, Fill => False);
                  Gtk_New (Label, To_String (Content));
                  HBox.Pack_Start (Label, Expand => True, Fill => True);
                  return Gtk_Widget (HBox);
               end if;
            end if;
         end;

         return null;
      end if;

      Window_To_Buffer_Coords
        (View,
         X - Get_Border_Window_Size (View, Text_Window_Left),
         Y - Get_Border_Window_Size (View, Text_Window_Top),
         Line, Col, Out_Of_Bounds);

      if Out_Of_Bounds then
         --  Do not display a tooltip in an invalid location,
         --  for example after the end of a line.

         return null;
      end if;

      Line_Info := Get_Side_Information
        (Box.Source_Buffer,
         Buffer_Line_Type (Line + 1));

      Get_Iter_At_Line_Offset (Box.Source_Buffer, Start_Iter, Line, Col);
      Search_Entity_Bounds (Start_Iter, End_Iter);
      Get_Screen_Position (Box.Source_Buffer, Start_Iter, Line, Col);

      --  Compute the area surrounding the entity, relative to the pointer
      --  coordinates.

      Get_Iter_Location (View, Start_Iter, Location);
      Buffer_To_Window_Coords
        (View, Text_Window_Text, Location.X, Location.Y, Area.X, Area.Y);
      Get_Iter_Location (View, End_Iter, Location);
      Buffer_To_Window_Coords
        (View, Text_Window_Text, Location.X, Location.Y, Win_X, Win_Y);

      Area.Width  := Win_X - Area.X + Location.Width;
      Area.Height := Win_Y - Area.Y + Location.Height;

      Tooltip.Set_Tip_Area (Area);

      declare
         Entity     : General_Entity;
         Entity_Ref : General_Entity_Reference;
         Context    : Selection_Context := New_Context;
         W          : Gtk_Widget;
      begin
         Get_Contextual_Menu
           (Context  => Context,
            Kernel   => Box.Kernel,
            Object   => Box,
            Location => Location_Mouse);

         Trace (Me, "Tooltip on " & Entity_Name_Information (Context));
         W := Compute_Tooltip (Box.Kernel, Context);

         if W /= null then
            return W;
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

                     if Vbox = null then
                        Gtk_New_Vbox (Vbox, Homogeneous => False);
                     end if;

                     if Icon /= null then
                        Gtk_New (Label, To_String (Text));
                        Vbox.Pack_Start (Label, Expand => False, Fill => True);
                     else
                        Gtk_New_Hbox (HBox, Homogeneous => False);
                        Vbox.Pack_Start (HBox, Expand => False, Fill => True);
                        Gtk_New (Image, Icon);
                        HBox.Pack_Start
                          (Image, Expand => False, Fill => False);
                        Gtk_New (Label, To_String (Text));
                        HBox.Pack_Start (Label, Expand => True, Fill => True);
                     end if;
                  end if;
               end;
            end loop;
         end if;

         --  If the mouse is not on top of text, do not display a tooltip

         if Entity_Name_Information (Context) = "" then
            return Gtk_Widget (Vbox);
         end if;

         --  No module wants to handle this tooltip. Default to built-in
         --  tooltip, based on cross references.

         Get_Declaration_Info (Box, Context, Entity, Entity_Ref);

         if Entity = No_General_Entity then
            return Gtk_Widget (Vbox);
         end if;

         --  Ref the entity, so that if Draw_Tooltip regenerates the xref info,
         --  we are sure to always have a valid entity reference.
         Ref (Entity);

         W := Entities_Tooltips.Draw_Tooltip
           (Box.Kernel, Entity, Entity_Ref, Draw_Border => False);
         if W /= null then
            if Vbox = null then
               Gtk_New_Vbox (Vbox, Homogeneous => False);
            end if;
            Vbox.Pack_Start (W, Expand => False, Fill => True);
         end if;

         Unref (Entity);

         return Gtk_Widget (Vbox);
      end;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return null;
   end Create_Contents;

end Src_Editor_Box.Tooltips;
