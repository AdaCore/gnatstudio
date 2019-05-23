------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2019, AdaCore                     --
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

with Ada.Strings.Unbounded;           use Ada.Strings.Unbounded;

with GNATCOLL.Traces;                 use GNATCOLL.Traces;
with GNATCOLL.VFS;                    use GNATCOLL.VFS;

with Glib;                            use Glib;
with Glib.Convert;                    use Glib.Convert;
with Gdk;                             use Gdk;
with Gdk.Rectangle;                   use Gdk.Rectangle;
with Gdk.Window;                      use Gdk.Window;
with Gtk.Box;                         use Gtk.Box;
with Gtk.Image;                       use Gtk.Image;
with Gtk.Label;                       use Gtk.Label;
with Gtk.Text_Iter;                   use Gtk.Text_Iter;
with Gtk.Enums;                       use Gtk.Enums;
with Gtk.Separator;                   use Gtk.Separator;
with Gtk.Widget;                      use Gtk.Widget;

with Entities_Tooltips;
with GPS.Editors;                     use GPS.Editors;
with GPS.Editors.Line_Information;    use GPS.Editors.Line_Information;
with GPS.Kernel.Contexts;             use GPS.Kernel, GPS.Kernel.Contexts;
with GPS.Kernel.Preferences;          use GPS.Kernel.Preferences;
with GPS.Kernel.Modules.UI;           use GPS.Kernel.Modules.UI;
with GUI_Utils;                       use GUI_Utils;
with Src_Editor_View;                 use Src_Editor_View;
with Src_Editor_Buffer.Line_Information;
use Src_Editor_Buffer.Line_Information;
with Tooltips;                        use Tooltips;
with Xref;                            use Xref;

with Language.Abstract_Language_Tree; use Language.Abstract_Language_Tree;

package body Src_Editor_Box.Tooltips is

   use type Basic_Types.Visible_Column_Type;

   Me : constant Trace_Handle := Create ("GPS.SOURCE_Editor.Tooltips");

   function Get_Declaration_Info
     (Context : Selection_Context;
      Ref     : out Root_Entity_Reference_Ref) return Root_Entity'Class;
   --  Perform a cross-reference to the declaration of the entity located at
   --  (Line, Column) in Editor. Fail silently when no declaration or no
   --  entity can be located, and set File_Decl to null.
   --  Entity is set to the entity that was found, or No_Entity_Information if
   --  not found. It must be destroyed by the caller.
   --  Ref is the closest reference to the entity from Context. It might not be
   --  set if we haven't found this information

   --------------------------------------------
   -- Default_Editor_Tooltip_Handler_Factory --
   --------------------------------------------

   function Default_Editor_Tooltip_Handler_Factory
     (Box : not null access Source_Editor_Box_Record'Class)
      return Editor_Tooltip_Handler_Access is
   begin
      return new Editor_Tooltip_Handler'
        (Standard.Tooltips.Tooltip_Handler
         with Box => Source_Editor_Box (Box));
   end Default_Editor_Tooltip_Handler_Factory;

   ---------------------------
   -- Get_Source_Editor_Box --
   ---------------------------

   function Get_Source_Editor_Box
     (Tooltip : not null access Editor_Tooltip_Handler)
      return Source_Editor_Box is
   begin
      return Tooltip.Box;
   end Get_Source_Editor_Box;

   ---------------------------
   -- Set_Source_Editor_Box --
   ---------------------------

   procedure Set_Source_Editor_Box
     (Tooltip : not null access Editor_Tooltip_Handler;
      Box     : not null access Source_Editor_Box_Record'Class) is
   begin
      Tooltip.Box := Source_Editor_Box (Box);
   end Set_Source_Editor_Box;

   --------------------------
   -- Get_Declaration_Info --
   --------------------------

   function Get_Declaration_Info
     (Context : Selection_Context;
      Ref     : out Root_Entity_Reference_Ref) return Root_Entity'Class is
   begin
      if not Contexts.Has_File_Information (Context) then
         return No_Root_Entity;
      end if;

      declare
         Result : constant Root_Entity'Class := Contexts.Get_Entity (Context);
      begin
         Ref := Root_Entity_Reference_Refs.To_Holder
           (Contexts.Get_Closest_Ref (Context));
         return Result;
      end;

   exception
      when E : others =>
         Trace (Me, E);
         return No_Root_Entity;
   end Get_Declaration_Info;

   -----------------------------------
   -- Get_Tooltip_Widget_For_Entity --
   -----------------------------------

   function Get_Tooltip_Widget_For_Entity
     (Tooltip : not null access Editor_Tooltip_Handler;
      Context : Selection_Context) return Gtk.Widget.Gtk_Widget
   is
      Tree       : constant Semantic_Tree'Class :=
                     Tooltip.Box.Kernel.Get_Abstract_Tree_For_File
                       ("XREF", Tooltip.Box.Get_Filename);
      Entity_Ref : Root_Entity_Reference_Ref;
   begin
      --  We do not want to compute an xref-based tooltip if the source
      --  is not indexed.
      if not Tree.Is_Ready then
         return null;
      end if;

      declare
         Entity : constant Root_Entity'Class := Get_Declaration_Info
           (Context, Entity_Ref);
      begin
         if Entity = No_Root_Entity then
            return null;
         end if;

         return Entities_Tooltips.Draw_Tooltip
           (Tooltip.Box.Kernel, Entity, Entity_Ref.Element,
            Draw_Border => False);
      end;
   end Get_Tooltip_Widget_For_Entity;

   ---------------------
   -- Create_Contents --
   ---------------------

   overriding function Create_Contents
     (Tooltip  : not null access Editor_Tooltip_Handler;
      Widget   : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      X, Y     : Glib.Gint) return Gtk.Widget.Gtk_Widget
   is
      pragma Unreferenced (Widget);

      Box              : constant Source_Editor_Box := Tooltip.Box;
      View             : constant Source_View       := Get_View (Tooltip.Box);
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
      HBox             : Gtk_Box;
      LX, LY           : Gint;
      --  The coordinates relative to the view, not the box

      In_Side_Area     : Boolean;

   begin
      if not Display_Tooltip.Get_Pref then
         return null;
      end if;

      Window := Get_Window (View, Text_Window_Text);

      Get_Geometry (Window, Win_X, Win_Y, Window_Width, Window_Height);

      --  Convert box coordinates to view coordinates
      declare
         Box_Win  : constant Gdk_Window := Box.Get_Window;
         View_Win : constant Gdk_Window := View.Get_Window;
         Box_X, Box_Y : Gint;
         View_X, View_Y : Gint;
      begin
         Get_Origin (Box_Win, Box_X, Box_Y);
         Get_Origin (View_Win, View_X, View_Y);
         LX := X + Box_X - View_X;
         LY := Y + Box_Y - View_Y;
         In_Side_Area := X < (View_X - Box_X);
      end;

      if In_Side_Area then
         --  In the side column, see if a tooltip information is to be
         --  displayed.
         Window_To_Buffer_Coords
           (View, LX, LY, Line, Col, Out_Of_Bounds);

         declare
            Content  : Unbounded_String;
            Has_Info : Boolean := False;
            Action   : GPS.Kernel.Messages.Action_Item;
            Image    : Gtk_Image;

            C : Message_Reference_List.Cursor;
            M : Message_Access;
         begin
            Line_Info := Get_Side_Information
              (Box.Source_Buffer,
               Buffer_Line_Type (Line + 1));

            --  Concatenate the tooltip information for all columns

            if Line_Info /= null then
               for K in Line_Info'Range loop
                  C := Line_Info (K).Messages.Last;

                  while Message_Reference_List.Has_Element (C) loop
                     M := Message_Reference_List.Element (C).Message;
                     if M /= null then
                        Action := M.Get_Action;
                     end if;

                     if Action /= null then
                        if Image = null
                          and then Action.Image /= Null_Unbounded_String
                        then
                           Gtk_New_From_Icon_Name
                              (Image, To_String (Action.Image), 20);
                        end if;

                        if Action.Tooltip_Text /= Null_Unbounded_String then
                           if Content /= Null_Unbounded_String then
                              Append (Content, ASCII.LF);
                           end if;

                           Append (Content, Action.Tooltip_Text);
                           Has_Info := True;
                        end if;
                     end if;

                     Message_Reference_List.Previous (C);
                  end loop;
               end loop;
            end if;

            --  visualization framework of internal data
            if Visualize_Internal_Buffers.Is_Active then
               Has_Info := True;
               if Content /= Null_Unbounded_String then
                  Append (Content, ASCII.LF);
               end if;

               Append
                 (Content, Get_Internal_Tooltip
                    (Box.Source_Buffer,
                     Buffer_Line_Type (Line + 1)));
            end if;

            if Has_Info then
               Create_Tooltip_Label (Label, To_String (Content));
               Label.Override_Font (View_Fixed_Font.Get_Pref);

               if Image = null then
                  return Gtk_Widget (Label);
               else
                  Gtk_New_Hbox (HBox, Homogeneous => False);
                  HBox.Pack_Start (Image, Expand => False, Fill => False);
                  HBox.Pack_Start (Label, Expand => True, Fill => True);
                  return Gtk_Widget (HBox);
               end if;
            end if;
         end;

         return null;
      end if;

      Window_To_Buffer_Coords
        (View,
         LX - Get_Border_Window_Size (View, Text_Window_Left),
         LY - Get_Border_Window_Size (View, Text_Window_Top),
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
      Area.X := Area.X + X - LX;
      Area.Y := Area.Y + Y - LY;

      Tooltip.Set_Tip_Area (Area);

      declare
         Context    : Selection_Context;
         W          : Gtk_Widget;
         Sep        : Gtk_Hseparator;
      begin
         Context := Build_Editor_Context
           (View     => Box.Source_View,
            Location => Location_Mouse);

         Trace (Me, "Tooltip on " & Entity_Name_Information (Context));
         W := Compute_Tooltip (Box.Kernel, Context);

         if W /= null then
            if Vbox = null then
               Gtk_New_Vbox (Vbox, Homogeneous => False);
            end if;
            Vbox.Pack_Start (W, Expand => False);

            Gtk_New_Hseparator (Sep);
            Vbox.Pack_Start (Sep, Expand => False);
            Sep := null;
         end if;

         --  If there is a message on this line, display it

         if Line_Info /= null then
            for J in Line_Info'Range loop
               declare
                  C       : Message_Reference_List.Cursor;
                  Message : Message_Access;
                  Text    : Unbounded_String;
                  Image   : Gtk_Image;
               begin
                  C := Line_Info (J).Messages.Last;

                  while Message_Reference_List.Has_Element (C) loop
                     Message := Message_Reference_List.Element (C).Message;

                     if Message /= null then
                        declare
                           Highlighting_Length : constant Highlight_Length :=
                             Message.Get_Highlighting_Length;

                           Mark_Column : constant Gint :=
                             Gint (Message.Get_Editor_Mark.Column);
                        begin
                           if Highlighting_Length = Highlight_Whole_Line
                             or else Col + 1 in Mark_Column .. Mark_Column +
                               Gint (Highlighting_Length)
                           then
                              if Text /= Null_Unbounded_String then
                                 Text := Text & ASCII.LF;
                              end if;

                              Text := Text & Message.Get_Markup;
                           end if;
                        end;
                     end if;
                     Message_Reference_List.Previous (C);
                  end loop;

                  if Text /= Null_Unbounded_String then
                     if Message /= null
                       and then Message.Get_Action /= null
                       and then Message.Get_Action.Image /=
                         Null_Unbounded_String
                     then
                        Gtk_New_From_Icon_Name
                          (Image,
                           To_String (Message.Get_Action.Image),
                           Icon_Size_Menu);
                     end if;

                     if Vbox = null then
                        Gtk_New_Vbox (Vbox, Homogeneous => False);
                     end if;

                     if Image = null then
                        Create_Tooltip_Label
                          (Label, Escape_Text (To_String (Text)));
                        Label.Override_Font (View_Fixed_Font.Get_Pref);
                        Vbox.Pack_Start (Label, Expand => False, Fill => True);
                     else
                        Gtk_New_Hbox (HBox, Homogeneous => False);
                        Vbox.Pack_Start (HBox, Expand => False, Fill => True);
                        HBox.Pack_Start
                          (Image, Expand => False, Fill => False);
                        Create_Tooltip_Label
                          (Label, Escape_Text (To_String (Text)));
                        Label.Override_Font (View_Fixed_Font.Get_Pref);
                        HBox.Pack_Start (Label, Expand => True, Fill => True);
                     end if;

                     if Sep = null then
                        Gtk_New_Hseparator (Sep);
                     end if;
                  end if;
               end;
            end loop;
         end if;

         if Sep /= null then
            Vbox.Pack_Start (Sep, Expand => False);
         end if;

         --  If the mouse is not on top of text, do not display a tooltip

         if not Has_Entity_Name_Information (Context) then
            return Gtk_Widget (Vbox);
         end if;

         --  No module wants to handle this tooltip. Default to built-in
         --  tooltip, based on cross references.

         W := Editor_Tooltip_Handler'Class
           (Tooltip.all).Get_Tooltip_Widget_For_Entity (Context);

         if W /= null then
            if Vbox = null then
               Gtk_New_Vbox (Vbox, Homogeneous => False);
            end if;
            Vbox.Pack_Start (W, Expand => False, Fill => True);
         end if;

         return Gtk_Widget (Vbox);
      end;

   exception
      when E : others =>
         Trace (Me, E);
         return null;
   end Create_Contents;

end Src_Editor_Box.Tooltips;
