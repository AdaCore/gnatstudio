-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2005-2006                      --
--                              AdaCore                              --
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

with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Strings.Unbounded;

with GNAT.Strings;

with Glib;                      use Glib;
with Gdk.Color;                 use Gdk, Gdk.Color;
with Gdk.GC;                    use Gdk.GC;
with Gdk.Rectangle;             use Gdk.Rectangle;
with Gdk.Drawable;              use Gdk.Drawable;
with Gdk.Types;
with Gdk.Pixmap;                use Gdk.Pixmap;
with Gdk.Window;                use Gdk.Window;
with Gtk.Text_Iter;             use Gtk.Text_Iter;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Widget;                use Gtk.Widget;
with Pango.Font;                use Pango.Font;
with Pango.Layout;              use Pango.Layout;

with Tooltips;                  use Tooltips;
with GPS.Kernel.Contexts;       use GPS.Kernel, GPS.Kernel.Contexts;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with Entities;                  use Entities;
with Entities.Queries;          use Entities.Queries;
with Entities.Tooltips;         use Entities.Tooltips;
with VFS;                       use VFS;
with Traces;                    use Traces;
with Src_Editor_Module;         use Src_Editor_Module;
with Src_Editor_View;           use Src_Editor_View;
with GUI_Utils;                 use GUI_Utils;
with Src_Editor_Buffer.Line_Information;
use Src_Editor_Buffer.Line_Information;

package body Src_Editor_Box.Tooltips is
   Me : constant Debug_Handle := Create ("Editor.Tooltips");

   type Editor_Tooltips is new Standard.Tooltips.Pixmap_Tooltips with record
      Box : Source_Editor_Box;
   end record;
   procedure Draw
     (Tooltip : access Editor_Tooltips;
      Pixmap  : out Gdk.Pixmap.Gdk_Pixmap;
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

      if Filename = VFS.No_File then
         Entity := null;
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
         Pop_State (Editor.Kernel);
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         Entity := null;
   end Get_Declaration_Info;

   ----------
   -- Draw --
   ----------

   procedure Draw
     (Tooltip : access Editor_Tooltips;
      Pixmap  : out Gdk.Pixmap.Gdk_Pixmap;
      Area    : out Gdk.Rectangle.Gdk_Rectangle)
   is
      use Ada.Strings.Unbounded;
      use type GNAT.Strings.String_Access;
      Box                   : constant Source_Editor_Box := Tooltip.Box;
      Widget                : constant Source_View := Get_View (Tooltip.Box);
      Line, Col             : Gint;
      --        Cursor_Col            : Gint;
      Mouse_X, Mouse_Y      : Gint;
      Win_X, Win_Y          : Gint;
      Start_Iter            : Gtk_Text_Iter;
      End_Iter              : Gtk_Text_Iter;
      Mask                  : Gdk.Types.Gdk_Modifier_Type;
      Win                   : Gdk.Gdk_Window;
      Location              : Gdk_Rectangle;
      Filename              : constant Virtual_File := Get_Filename (Box);
      Out_Of_Bounds         : Boolean;
      Window                : Gdk.Gdk_Window;
      Window_Width          : Gint;
      Window_Height         : Gint;
      Window_Depth          : Gint;
      Context               : Selection_Context;

   begin
      Pixmap := null;
      Area   := (0, 0, 0, 0);

      if not Get_Pref (Display_Tooltip) then
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
            Line_Info     : constant Line_Info_Width_Array_Access :=
                              Get_Side_Information
                                (Box.Source_Buffer,
                                 Editable_Line_Type (Line + 1));
            Content       : Unbounded_String;
            Font          : constant Pango_Font_Description :=
                              Get_Pref (Default_Font);
            Layout        : Pango_Layout;
            Width, Height : Gint := 0;
            GC            : Gdk.Gdk_GC;
            Has_Info      : Boolean := False;

         begin
            --  Concatenate the tooltip information for all columns

            if Line_Info /= null then
               for K in Line_Info'Range loop
                  if Line_Info (K).Info /= null
                    and then Line_Info (K).Info.Tooltip_Text /= null
                  then
                     if Content /= Null_Unbounded_String then
                        Append (Content, ASCII.LF);
                     end if;

                     Append (Content, Line_Info (K).Info.Tooltip_Text.all);
                     Has_Info := True;
                  end if;
               end loop;
            end if;

            if Has_Info then
               Layout := Create_Pango_Layout (Widget, "");
               Set_Font_Description (Layout, Font);
               Set_Markup (Layout, To_String (Content));

               Get_Pixel_Size (Layout, Width, Height);

               Width := Width + 6;
               Height := Height + 4;

               Gdk_New (GC, Get_Window (Widget));
               Set_Foreground (GC, Get_Pref (Tooltip_Color));

               Gdk.Pixmap.Gdk_New (Pixmap, Get_Window (Widget), Width, Height);
               Draw_Rectangle (Pixmap, GC, True, 0, 0, Width - 1, Height - 1);

               Set_Foreground (GC, Black (Get_Default_Colormap));
               Draw_Rectangle (Pixmap, GC, False, 0, 0, Width - 1, Height - 1);

               Draw_Layout (Pixmap, GC, 2, 0, Layout);
               Unref (Layout);

               Unref (GC);
            end if;
         end;

         return;
      end if;

      if Mouse_X < Win_X
        or else Mouse_Y < Win_Y
        or else Win_X + Window_Width < Mouse_X
        or else Win_Y + Window_Height < Mouse_Y
      then
         --  Invalid position: the cursor is outside the text, do not
         --  display a tooltip.

         return;
      end if;

      Window_To_Buffer_Coords
        (Widget, Mouse_X, Mouse_Y, Line, Col, Out_Of_Bounds);

      if Out_Of_Bounds then
         --  Do not display a tooltip in an invalid location,
         --  for example after the end of a line.

         return;
      end if;

      --  Cursor_Col := Col;
      Get_Iter_At_Line_Offset (Box.Source_Buffer, Start_Iter, Line, Col);
      Search_Entity_Bounds (Start_Iter, End_Iter);
      Get_Screen_Position (Box.Source_Buffer, Start_Iter, Line, Col);

      --  Compute the area surrounding the entity, relative to the pointer
      --  coordinates.

      Get_Iter_Location (Widget, Start_Iter, Location);
      Buffer_To_Window_Coords
        (Widget, Text_Window_Text, Location.X, Location.Y, Win_X, Win_Y);
      Area.X := Win_X - Mouse_X;
      Area.Y := Win_Y - Mouse_Y;

      Get_Iter_Location (Widget, End_Iter, Location);
      Buffer_To_Window_Coords
        (Widget, Text_Window_Text, Location.X, Location.Y, Win_X, Win_Y);
      Area.Width  := Win_X - Mouse_X - Area.X + Location.Width;
      Area.Height := Win_Y - Mouse_Y - Area.Y + Location.Height;

      declare
         Entity_Name : constant String := Get_Text (Start_Iter, End_Iter);
         Entity      : Entity_Information;
         Entity_Ref  : Entity_Reference;
         Status      : Find_Decl_Or_Body_Query_Status;
         Editable_Line : constant Editable_Line_Type :=
           Get_Editable_Line (Box.Source_Buffer, Buffer_Line_Type (Line + 1));

         Column      : constant Visible_Column_Type :=
           Expand_Tabs (Box.Source_Buffer, Editable_Line,
                        Character_Offset_Type (Col + 1));
      begin
         if Entity_Name = "" then
            return;
         end if;

         Trace (Me, "Tooltip on " & Entity_Name);

         Context := New_Context;
         Set_Context_Information
           (Context, Box.Kernel,
            Abstract_Module_ID (Src_Editor_Module_Id));
         Set_File_Information
           (Context => Context,
            File    => Filename,
            Line    => Integer (Editable_Line),
            Column  => Column);
         --  ??? Should we use the cursor column here ?

         Set_Entity_Information
           (Context       => Context,
            Entity_Name   => Entity_Name,
            Entity_Column => Column);
         GPS.Kernel.Modules.Compute_Tooltip (Box.Kernel, Context, Pixmap);

         if Pixmap /= null then
            return;
         end if;

         --  No module wants to handle this tooltip. Default to built-in
         --  tooltip, based on cross references.

         Get_Declaration_Info
           (Box, Context, Entity, Entity_Ref, Status);

         if Entity = null then
            return;
         end if;

         Ref (Entity);
         --  ??? What is the purpose of the Ref/Unref call?

         Pixmap := Draw_Tooltip (Box.Kernel, Entity, Entity_Ref, Status);

         Unref (Entity);
      end;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Draw;

end Src_Editor_Box.Tooltips;
