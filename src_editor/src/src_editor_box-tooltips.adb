-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2005                            --
--                            AdaCore                                --
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

with Ada.Strings.Unbounded;
with Ada.Exceptions;            use Ada.Exceptions;

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
with Pango.Tabs;                use Pango.Tabs;

with Tooltips;                  use Tooltips;
with Doc_Utils;                 use Doc_Utils;
with GPS.Kernel.Contexts;       use GPS.Kernel, GPS.Kernel.Contexts;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Intl;                  use GPS.Intl;
with Entities;                  use Entities;
with Entities.Queries;          use Entities.Queries;
with VFS;                       use VFS;
with Traces;                    use Traces;
with Src_Editor_Module;         use Src_Editor_Module;
with Src_Editor_View;           use Src_Editor_View;
with GUI_Utils;                 use GUI_Utils;
with String_Utils;              use String_Utils;

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
      Context : access Entity_Selection_Context'Class;
      Entity  : out Entity_Information;
      Ref     : out Entity_Reference);
   --  Perform a cross-reference to the declaration of the entity located at
   --  (Line, Column) in Editor. Fail silently when no declaration or no
   --  entity can be located, and set File_Decl to null.
   --  Entity is set to the entity that was found, or No_Entity_Information if
   --  not found. It must be destroyed by the caller.
   --  Ref is the closest reference to the entity from Context. It might not be
   --  set if we haven't found this information

   function Get_Documentation
     (Kernel : access Kernel_Handle_Record'Class;
      Entity : Entity_Information) return String;
   --  Return the documentation for the entity (prefixed by a LF char if not
   --  null)

   function Get_Parameters
     (Entity : Entity_Information;
      Widget : access Gtk_Widget_Record'Class;
      Font   : Pango_Font_Description) return Pango_Layout;
   --  Return the list of parameters for the entity

   function Get_Instance (Entity_Ref : Entity_Reference) return String;
   --  Return the text describing from what instance the entity is

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

   --------------------
   -- Get_Parameters --
   --------------------

   function Get_Parameters
     (Entity : Entity_Information;
      Widget : access Gtk_Widget_Record'Class;
      Font   : Pango_Font_Description) return Pango_Layout
   is
      use Ada.Strings.Unbounded;
      --  ??? Display should depend on the language
      Iter       : Subprogram_Iterator;
      Param      : Entity_Information;
      Param_Type : Entity_Information;
      Result     : Unbounded_String;
      Longuest_Param, Longuest_Type : Gint := 0;
      Layout     : Pango_Layout;
      Tabs       : Pango_Tab_Array;
      Char_Width, Char_Height : Gint;
   begin
      if Is_Subprogram (Entity) then
         Layout := Create_Pango_Layout (Widget, "");
         Set_Font_Description (Layout, Font);

         Iter := Get_Subprogram_Parameters (Subprogram => Entity);
         Result := To_Unbounded_String ("<b>Parameters:</b>");

         loop
            Get (Iter, Param);
            exit when Param = null;

            Set_Markup (Layout, "   <b>" & Get_Name (Param).all & "</b>");
            Get_Pixel_Size (Layout, Char_Width, Char_Height);
            Longuest_Param := Gint'Max (Longuest_Param, Char_Width);

            Set_Text (Layout, ": " & Image (Get_Type (Iter)));
            Get_Pixel_Size (Layout, Char_Width, Char_Height);
            Longuest_Type := Gint'Max (Longuest_Type, Char_Width);

            Next (Iter);
         end loop;

         Pango_New (Tabs, Initial_Size => 2, Positions_In_Pixels => True);
         Set_Tab (Tabs, 0, Location => Longuest_Param + 2);
         Set_Tab (Tabs, 1, Location => Longuest_Param + 2 + Longuest_Type);
         Set_Tabs (Layout, Tabs);

         Iter := Get_Subprogram_Parameters (Subprogram => Entity);
         loop
            Get (Iter, Param);
            exit when Param = null;

            Result := Result & ASCII.LF
              & "   <b>" & Get_Name (Param).all & "</b>" & ASCII.HT
              & ": " & Image (Get_Type (Iter)) & ASCII.HT & " ";

            if Get_Type (Iter) = Access_Parameter then
               Param_Type := Pointed_Type (Param);
            else
               Param_Type := Get_Type_Of (Param);
            end if;

            if Param_Type /= null then
               Result := Result & Get_Name (Param_Type).all;
            end if;

            Next (Iter);
         end loop;

         Param := Returned_Type (Entity);
         if Param /= null then
            Result := Result & ASCII.LF & "   return <b>"
              & Get_Name (Param).all & "</b>";
         end if;

         Set_Markup (Layout, To_String (Result));

         Free (Tabs);
         return Layout;
      else
         return null;
      end if;
   end Get_Parameters;

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
                 & ASCII.LF & (-"from instance at ");
            else
               Result := Result & ASCII.LF
                 & (-"from instance of ")
                 & Get_Name (Inst_Of).all & ':'
                 & Base_Name (Get_Filename
                     (Get_File (Get_Declaration_Of (Inst_Of)))) & ':'
                 & Image (Get_Line (Get_Declaration_Of (Inst_Of)))
                 & ASCII.LF & "  at ";
            end if;
            Result := Result
              & Get_Name (Inst_E).all
              & ':'
              &  Base_Name (Get_Filename
                              (Get_File (Get_Declaration_Of (Inst_E)))) & ':'
              & Image (Get_Line (Get_Declaration_Of (Inst_E)));
            Inst := Generic_Parent (Inst);
         end loop;
      end if;

      return To_String (Result);
   end Get_Instance;

   --------------------------
   -- Get_Declaration_Info --
   --------------------------

   procedure Get_Declaration_Info
     (Editor  : access Source_Editor_Box_Record;
      Context : access Entity_Selection_Context'Class;
      Entity  : out Entity_Information;
      Ref     : out Entity_Reference)
   is
      Status   : Find_Decl_Or_Body_Query_Status;
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

   -----------------------
   -- Get_Documentation --
   -----------------------

   function Get_Documentation
     (Kernel : access Kernel_Handle_Record'Class;
      Entity : Entity_Information) return String
   is
      Doc : constant String :=
        Get_Documentation (Get_Language_Handler (Kernel), Entity);
   begin
      if Doc /= "" then
         return Doc;
      else
         return "";
      end if;
   end Get_Documentation;

   ----------
   -- Draw --
   ----------

   procedure Draw
     (Tooltip : access Editor_Tooltips;
      Pixmap  : out Gdk.Pixmap.Gdk_Pixmap;
      Area    : out Gdk.Rectangle.Gdk_Rectangle)
   is
      Box                   : constant Source_Editor_Box := Tooltip.Box;
      Widget                : constant Source_View := Get_View (Tooltip.Box);
      Line, Col, Cursor_Col : Gint;
      Mouse_X, Mouse_Y      : Gint;
      Win_X, Win_Y          : Gint;
      Start_Iter            : Gtk_Text_Iter;
      End_Iter              : Gtk_Text_Iter;
      Mask                  : Gdk.Types.Gdk_Modifier_Type;
      Win                   : Gdk.Gdk_Window;
      Context               : aliased Entity_Selection_Context;
      Location              : Gdk_Rectangle;
      Filename              : constant Virtual_File := Get_Filename (Box);
      Out_Of_Bounds         : Boolean;
      Window                : Gdk.Gdk_Window;
      Window_Width          : Gint;
      Window_Height         : Gint;
      Window_Depth          : Gint;

   begin
      Pixmap := null;
      Area   := (0, 0, 0, 0);

      if not Get_Pref (Box.Kernel, Display_Tooltip) then
         return;
      end if;

      Window := Get_Window (Widget, Text_Window_Text);

      Get_Geometry
        (Window, Win_X, Win_Y, Window_Width, Window_Height, Window_Depth);
      Get_Pointer
        (Window, Mouse_X, Mouse_Y, Mask, Win);

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

      Cursor_Col := Col;
      Get_Iter_At_Line_Offset (Box.Source_Buffer, Start_Iter, Line, Col);
      Search_Entity_Bounds (Start_Iter, End_Iter);
      Get_Screen_Position (Box.Source_Buffer, Start_Iter, Line, Col);

      --  Compute the area surrounding the entity, relative to the pointer
      --  coordinates

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

      begin
         if Entity_Name = "" then
            return;
         end if;

         Trace (Me, "Tooltip on " & Entity_Name);
         Set_Context_Information
           (Context'Unchecked_Access, Box.Kernel, Src_Editor_Module_Id);
         Set_File_Information
           (Context      => Context'Unchecked_Access,
            File         => Filename,
            Line         => To_Box_Line (Box.Source_Buffer, Line),
            Column       => To_Box_Column (Cursor_Col));
         Set_Entity_Information
           (Context       => Context'Unchecked_Access,
            Entity_Name   => Entity_Name,
            Entity_Column => To_Box_Column (Col));
         GPS.Kernel.Modules.Compute_Tooltip
           (Box.Kernel, Context'Unchecked_Access, Pixmap);

         if Pixmap /= null then
            Destroy (Context);
            return;
         end if;

         --  No module wants to handle this tooltip. Default to built-in
         --  tooltip, based on cross references.

         Get_Declaration_Info
           (Box, Context'Unchecked_Access, Entity, Entity_Ref);

         Destroy (Context);

         if Entity = null then
            return;
         end if;

         Ref (Entity);

         declare
            Str : constant String :=
              "<b>" & Attributes_To_String (Get_Attributes (Entity)) & ' ' &
              (-Kind_To_String (Get_Kind (Entity))) & ' ' &
               Get_Full_Name (Entity, ".") & "</b>"
              & ASCII.LF
              & (-"declared at ")
              & Base_Name (Get_Filename
                  (Get_File (Get_Declaration_Of (Entity)))) & ':'
              & Image (Get_Line (Get_Declaration_Of (Entity)));
            Str2 : constant String :=
              Get_Instance (Entity_Ref)
              & Get_Documentation (Box.Kernel, Entity);
            Font : constant Pango_Font_Description :=
              Get_Pref (Box.Kernel, Default_Font);
            Color : Gdk_Color;
            Layout1, Layout2, Layout3 : Pango_Layout;
            Width, Height, W1, H1, W2, H2, W3, H3 : Gint := 0;
            GC     : Gdk.Gdk_GC;
         begin
            Color := Parse ("#EEEEEE");
            Alloc (Get_Default_Colormap, Color);

            Layout1 := Create_Pango_Layout (Widget, "");
            Set_Markup (Layout1, Str);
            Set_Font_Description (Layout1, Font);
            Get_Pixel_Size (Layout1, W1, H1);

            if Str2 /= "" then
               Layout2 := Create_Pango_Layout (Widget, Str2);
               Set_Font_Description (Layout2, Font);
               Get_Pixel_Size (Layout2, W2, H2);
            end if;

            Layout3 := Get_Parameters (Entity, Widget, Font);
            if Layout3 /= null then
               Get_Pixel_Size (Layout3, W3, H3);
               H3 := H3 + 5;
            end if;

            Width  := 4 + Gint'Max (W1, W2);
            Height := 6 + H1 + H2;
            Width  := Gint'Max (Width, W3);
            Height := Height + H3;

            Gdk_New (GC, Get_Window (Widget));
            Set_Foreground (GC, Color);

            Gdk.Pixmap.Gdk_New (Pixmap, Get_Window (Widget), Width, Height);
            Draw_Rectangle (Pixmap, GC, True, 0, 0, Width - 1, Height - 1);

            Set_Foreground (GC, Black (Get_Default_Colormap));
            Draw_Rectangle (Pixmap, GC, False, 0, 0, Width - 1, Height - 1);

            Draw_Layout (Pixmap, GC, 2, 0, Layout1);
            Unref (Layout1);

            if Layout2 /= null or else Layout3 /= null then
               Draw_Line   (Pixmap, GC, 0, H1 + 1, Width - 1, H1 + 1);
            end if;

            if Layout2 /= null then
               Draw_Layout (Pixmap, GC, 2, H1 + 3, Layout2);
               Unref (Layout2);
            end if;

            if Layout3 /= null then
               Draw_Layout (Pixmap, GC, 2, H1 + H2 + 8, Layout3);
               Unref (Layout3);
            end if;

            Unref (GC);
         end;

         Unref (Entity);
      end;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Draw;

end Src_Editor_Box.Tooltips;
