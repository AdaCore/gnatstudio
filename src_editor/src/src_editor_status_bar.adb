------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2013, AdaCore                          --
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

with Gtk.Event_Box; use Gtk.Event_Box;
with Gtk.Label; use Gtk.Label;
with Gtk.Image; use Gtk.Image;
with Glib; use Glib;
with Basic_Types; use Basic_Types;
with String_Utils; use String_Utils;
with GNATCOLL.VFS; use GNATCOLL.VFS;
with Glib.Object;
use Glib.Object;
with Glib.Values;
with GNATCOLL.Traces; use GNATCOLL.Traces;
with Gtk.Enums; use Gtk.Enums;
with Traces;
with Pango.Layout; use Pango.Layout;
with GPS.Kernel.Preferences; use GPS.Kernel.Preferences;
with Gtk.Separator; use Gtk.Separator;
with Gtk.Handlers; use Gtk.Handlers;
with GPS.Kernel.MDI; use GPS.Kernel.MDI;
with Gtk.Widget; use Gtk.Widget;
with Gtkada.Handlers;
with Gtk.Arguments; use Gtk.Arguments;
with Src_Editor_Module.Markers; use Src_Editor_Module.Markers;
with Src_Editor_Box; use Src_Editor_Box;
with GPS.Editors; use GPS.Editors;
with Src_Editor_Module.Commands; use Src_Editor_Module.Commands;
with Gtkada.MDI; use Gtkada.MDI;
with Src_Editor_Module; use Src_Editor_Module;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GPS.Intl;                   use GPS.Intl;
with Gdk.Pixbuf; use Gdk.Pixbuf;
with GPS.Stock_Icons; use GPS.Stock_Icons;
with Gtk.Style_Context; use Gtk.Style_Context;
with Language.Tree; use Language.Tree;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Language; use Language;
with GNATCOLL.Symbols; use GNATCOLL.Symbols;
with GNAT.Strings; use GNAT.Strings;

package body Src_Editor_Status_Bar is

--     Me : constant Trace_Handle := Create ("Src_Editor_Status_Bar");

   Show_Modified_Unmodified_In_Status_Bar : constant Boolean := False;
   --  Whether to show the modified/unmodified/saved status in the status bar.

   procedure Setup (Data : Source_Editor_Status_Bar; Id : Handler_Id);
   package Bar_Callback is new Gtk.Handlers.User_Callback_With_Setup
     (Widget_Type => Glib.Object.GObject_Record,
      User_Type   => Source_Editor_Status_Bar,
      Setup       => Setup);

   procedure Cursor_Position_Changed_Handler
     (Buffer : access Glib.Object.GObject_Record'Class;
      Params : Glib.Values.GValues;
      Bar    : Source_Editor_Status_Bar);
   --  This handler is merely a proxy to Show_Cursor_Position. It just
   --  extracts the necessary values from Params, and pass them on to
   --  Show_Cursor_Position.

   procedure Show_Cursor_Position
     (Bar    : access Source_Editor_Status_Bar_Record'Class;
      Line   : Editable_Line_Type;
      Column : Character_Offset_Type);
   --  Redraw the cursor position in the Line/Column areas of the status bar

   function On_Read_Only_Pressed
     (Ob : access GObject_Record'Class) return Boolean;
   --  Toggle read-only/writable state of a given box

   function On_Subprogram_Link
     (Ob  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Args : Gtk_Args) return Boolean;
   --  Called when the user clicks on one of the links in the subprogram box in
   --  the status bar.

   function On_Goto_Line_Func
     (Ob : access GObject_Record'Class) return Boolean;
   --  Callback when clicking on the line number in the status bar

   procedure On_Bar_Destroy
     (Object : access Glib.Object.GObject_Record'Class;
      Params : Glib.Values.GValues;
      Bar    : Source_Editor_Status_Bar);
   --  Callback for the "destroy" signal

   procedure Buffer_Information_Handler
     (Ob     : access Glib.Object.GObject_Record'Class;
      Params : Glib.Values.GValues;
      Bar    : Source_Editor_Status_Bar);
   --  Reflect the change in buffer information

   procedure Destroy_Info_Frames
     (Bar : access Source_Editor_Status_Bar_Record'Class);
   --  Destroy Box.Buffer_Info_Frames

   -------------------------
   -- Destroy_Info_Frames --
   -------------------------

   procedure Destroy_Info_Frames
     (Bar : access Source_Editor_Status_Bar_Record'Class) is
   begin
      if Bar.Buffer_Info_Frames /= null then
         for J in Bar.Buffer_Info_Frames'Range loop
            Remove (Bar, Bar.Buffer_Info_Frames (J).Label);
            Remove (Bar, Bar.Buffer_Info_Frames (J).Separator);
         end loop;

         Unchecked_Free (Bar.Buffer_Info_Frames);
      end if;
   end Destroy_Info_Frames;

   --------------------------------
   -- Buffer_Information_Handler --
   --------------------------------

   procedure Buffer_Information_Handler
     (Ob     : access Glib.Object.GObject_Record'Class;
      Params : Glib.Values.GValues;
      Bar    : Source_Editor_Status_Bar)
   is
      pragma Unreferenced (Ob, Params);

      Info  : constant Extra_Information_Array_Access :=
        Get_Extra_Information (Bar.Buffer);
      Label : Gtk_Label;
      Image : Gtk_Image;

   begin
      Destroy_Info_Frames (Bar);

      if Info = null then
         return;
      end if;

      Bar.Buffer_Info_Frames := new Frames_Array (Info'Range);

      for J in Bar.Buffer_Info_Frames'Range loop
         if Info (J).Icon.all /= "" then
            Gtk_New (Image, Stock_Id => Info (J).Icon.all,
                     Size => Icon_Size_Menu);
            Bar.Buffer_Info_Frames (J).Label := Gtk_Widget (Image);
         else
            if Info (J).Info.Text /= null then
               Gtk_New (Label, Info (J).Info.Text.all);
            else
               Gtk_New (Label);
            end if;
            Bar.Buffer_Info_Frames (J).Label := Gtk_Widget (Label);
         end if;

         if Info (J).Tooltip /= null
           and then Info (J).Tooltip.all /= ""
         then
            Bar.Buffer_Info_Frames (J).Label.Set_Tooltip_Markup
              (Info (J).Tooltip.all);
         else
            Bar.Buffer_Info_Frames (J).Label.Set_Tooltip_Markup ("");
         end if;

         Gtk_New_Vseparator (Bar.Buffer_Info_Frames (J).Separator);
         Pack_End
           (Bar,
            Bar.Buffer_Info_Frames (J).Separator,
            Expand => False,
            Fill => False);

         Pack_End
           (Bar,
            Bar.Buffer_Info_Frames (J).Label,
            Expand  => False,
            Fill    => True,
            Padding => 0);
      end loop;

      Show_All (Bar);
   end Buffer_Information_Handler;

   --------------------
   -- On_Bar_Destroy --
   --------------------

   procedure On_Bar_Destroy
     (Object : access Glib.Object.GObject_Record'Class;
      Params : Glib.Values.GValues;
      Bar    : Source_Editor_Status_Bar)
   is
      pragma Unreferenced (Object, Params);
   begin
      Disconnect (Bar.Buffer, Bar.Cursor_Handler);
      Disconnect (Bar.Buffer, Bar.Buffer_Info_Handler);
   exception
      when E : others =>
         Trace (Traces.Exception_Handle, E);
   end On_Bar_Destroy;

   ----------------------------
   -- Update_Subprogram_Name --
   ----------------------------

   procedure Update_Subprogram_Name
     (Bar : not null access Source_Editor_Status_Bar_Record'Class)
   is
      Block : Block_Record;
      Iter  : Construct_Tree_Iterator;
      Val   : Unbounded_String;
   begin
      if Display_Subprogram_Names.Get_Pref then
         Block := Get_Subprogram_Block (Bar.Buffer, Bar.Current_Line);
         if Block.Block_Type /= Cat_Unknown
           and then Block.Name /= No_Symbol
         then
            --  We cannot control the underline from the theme unfortunately.
            Val := To_Unbounded_String
              ("<span underline='none'><a href='" & Block.First_Line'Img & "'>"
               & Get (Block.Name).all & "</a></span>");

            Iter := Get_Parent_Scope (Block.Tree, Block.Iter);
            while Iter /= Null_Construct_Tree_Iterator loop
               Val :=
                 "<span underline='none'><a href='"
                 & Get_Construct (Iter).Sloc_Start.Line'Img
                 & "'>" & Get (Get_Construct (Iter).Name).all & "</a></span>."
                 & Val;
               Iter := Get_Parent_Scope (Block.Tree, Iter);
            end loop;

            Bar.Function_Label.Set_Markup (To_String (Val));
            return;
         end if;
      end if;

      Bar.Function_Label.Set_Text ("");
   end Update_Subprogram_Name;

   -------------------
   -- Update_Status --
   -------------------

   procedure Update_Status
     (Bar : not null access Source_Editor_Status_Bar_Record'Class)
   is
      Child : constant MDI_Child := Find_Child
        (Get_Kernel (Bar.Buffer), Source_Editor_Box (Bar.Box));
   begin
      case Get_Status (Bar.Buffer) is
         when Unmodified =>
            if Show_Modified_Unmodified_In_Status_Bar then
               Bar.Modified_Label.Set_Tooltip_Text (-"Unmodified");
               Bar.Modified_Label.Set (File_Pixbuf);
            end if;

            if Child /= null and then File_Pixbuf /= null then
               Set_Icon (Child, File_Pixbuf);
            end if;

         when Readonly =>
            if Show_Modified_Unmodified_In_Status_Bar then
               Bar.Modified_Label.Set_Tooltip_Text (-"Read only");
               Bar.Modified_Label.Set (File_Pixbuf);
            end if;

            if Child /= null and then File_Pixbuf /= null then
               Set_Icon (Child, File_Pixbuf);
            end if;

         when Unsaved =>
            if Show_Modified_Unmodified_In_Status_Bar then
               Bar.Modified_Label.Set_Tooltip_Text (-"Unsaved");
               Bar.Modified_Label.Set (File_Unsaved_Pixbuf);
            end if;

            if Child /= null and then File_Unsaved_Pixbuf /= null then
               Set_Icon (Child, File_Unsaved_Pixbuf);
            end if;

         when Saved =>
            if Show_Modified_Unmodified_In_Status_Bar then
               Bar.Modified_Label.Set_Tooltip_Text (-"Saved");
               Bar.Modified_Label.Set (File_Pixbuf);
            end if;

            if Child /= null and then File_Pixbuf /= null then
               Set_Icon (Child, File_Pixbuf);
            end if;

         when Modified =>
            if Show_Modified_Unmodified_In_Status_Bar then
               Bar.Modified_Label.Set_Tooltip_Text (-"Modified");
               Bar.Modified_Label.Set (File_Modified_Pixbuf);
            end if;

            if Child /= null and then File_Modified_Pixbuf /= null then
               Set_Icon (Child, File_Modified_Pixbuf);
            end if;
      end case;

      if Get_Writable (Bar.Buffer) then
         Set (Bar.Read_Only_Label, GPS_Writable, Icon_Size_Local_Toolbar);
         Bar.Read_Only_Label.Set_Tooltip_Text (-"Writable");
         Get_Style_Context (Bar.View).Remove_Class ("read-only");
      else
         Set (Bar.Read_Only_Label, GPS_Read_Only, Icon_Size_Local_Toolbar);
         Bar.Read_Only_Label.Set_Tooltip_Text (-"Read Only");
         Get_Style_Context (Bar.View).Add_Class ("read-only");
      end if;
   end Update_Status;

   -----------------------
   -- On_Goto_Line_Func --
   -----------------------

   function On_Goto_Line_Func
     (Ob : access GObject_Record'Class) return Boolean
   is
      Bar : constant Source_Editor_Status_Bar := Source_Editor_Status_Bar (Ob);
   begin
      --  ??? Not nice to get a context here
      On_Goto_Line
        (Bar.Box,
         Get_Kernel (Bar.Buffer));
      return True;
   end On_Goto_Line_Func;

   ------------------------
   -- On_Subprogram_Link --
   ------------------------

   function On_Subprogram_Link
     (Ob  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Args : Gtk_Args) return Boolean
   is
      Bar : constant Source_Editor_Status_Bar := Source_Editor_Status_Bar (Ob);
      URI : constant String := To_String (Args, 1);
      Line : constant Editable_Line_Type := Editable_Line_Type'Value (URI);
      Box  : constant Source_Editor_Box := Source_Editor_Box (Bar.Box);
   begin
      Push_Current_Editor_Location_In_History (Get_Kernel (Bar.Buffer));
      Set_Cursor_Location (Box, Line, 1, Centering => With_Margin);
      Add_Navigation_Location (Box);
      return True;
   end On_Subprogram_Link;

   --------------------------
   -- On_Read_Only_Pressed --
   --------------------------

   function On_Read_Only_Pressed
     (Ob : access GObject_Record'Class) return Boolean
   is
      Bar : constant Source_Editor_Status_Bar := Source_Editor_Status_Bar (Ob);
   begin
      Set_Writable
        (Source_Editor_Box (Bar.Box),
         not Get_Writable (Bar.Buffer), Explicit => True);

      return False;

   exception
      when E : others =>
         Trace (Traces.Exception_Handle, E);
         return False;
   end On_Read_Only_Pressed;

   -----------
   -- Setup --
   -----------

   procedure Setup (Data : Source_Editor_Status_Bar; Id : Handler_Id) is
   begin
      Add_Watch (Id, Data);
   end Setup;

   --------------------------
   -- Show_Cursor_Position --
   --------------------------

   procedure Show_Cursor_Position
     (Bar    : access Source_Editor_Status_Bar_Record'Class;
      Line   : Editable_Line_Type;
      Column : Character_Offset_Type) is
   begin
      Bar.Cursor_Loc_Label.Set_Text
        (String_Utils.Image (Integer (Line))
         & ':' & String_Utils.Image (Integer (Column)));
   end Show_Cursor_Position;

   -------------------------------------
   -- Cursor_Position_Changed_Handler --
   -------------------------------------

   procedure Cursor_Position_Changed_Handler
     (Buffer : access Glib.Object.GObject_Record'Class;
      Params : Glib.Values.GValues;
      Bar    : Source_Editor_Status_Bar)
   is
      pragma Unreferenced (Buffer);
      Child : MDI_Child;
      File  : GNATCOLL.VFS.Virtual_File := Get_Filename (Bar.Buffer);
   begin
      if File = GNATCOLL.VFS.No_File then
         File := Get_File_Identifier (Bar.Buffer);
      end if;

      Bar.Current_Line :=
        Editable_Line_Type (Values.Get_Int (Values.Nth (Params, 1)));

      --  In case there are multiple views, we only want to change the one that
      --  last had the focus. Otherwise, they would all end up with the same
      --  line number, which is inaccurate.
      --  The box might not have the focus currently: if we are for instance
      --  changing the current line from the "Go to line" dialog, the latter
      --  still has the focus at this point.

      Child := Find_Editor (Get_Kernel (Bar.Buffer), File);

      if Child /= null and then Get_Widget (Child) = Gtk_Widget (Bar.Box) then
         Show_Cursor_Position
           (Bar,
            Line   => Bar.Current_Line,
            Column => Character_Offset_Type
              (Values.Get_Int (Values.Nth (Params, 2))));
      end if;

   exception
      when E : others =>
         Trace (Traces.Exception_Handle, E);
   end Cursor_Position_Changed_Handler;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Bar    : out Source_Editor_Status_Bar;
      Box    : Gtk_Hbox;
      View   : Source_View;
      Buffer : Source_Buffer)
   is
   begin
      Bar := new Source_Editor_Status_Bar_Record;
      Initialize (Bar, Box, View, Buffer);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Bar    : not null access Source_Editor_Status_Bar_Record'Class;
      Box    : Gtk_Hbox;
      View   : Source_View;
      Buffer : Source_Buffer)
   is
      Event_Box      : Gtk_Event_Box;
      Separator      : Gtk_Vseparator;
   begin
      Gtk.Box.Initialize_Hbox (Bar);
      Bar.View := View;
      Bar.Buffer := Buffer;
      Bar.Box := Box;

      --  Avoid resizing the main window whenever a label is changed
      Set_Resize_Mode (Bar, Resize_Queue);

      --  Line:Column number area...
      Gtk_New (Event_Box);

      Pack_End (Bar, Event_Box, Expand => False, Fill => False);
      Gtk_New (Bar.Cursor_Loc_Label, "1:1");
      Add (Event_Box, Bar.Cursor_Loc_Label);

      Object_Return_Callback.Object_Connect
        (Event_Box, Signal_Button_Press_Event, On_Goto_Line_Func'Access, Bar);

      --  Setup a minimal size for the line:column area, to avoid too much
      --  resizing.

      declare
         Layout : constant Pango_Layout :=
                    Create_Pango_Layout (Bar.Cursor_Loc_Label, "99999:999");
         Width, Height : Gint;
      begin
         Set_Font_Description (Layout, Default_Font.Get_Pref_Font);
         Get_Pixel_Size (Layout, Width, Height);
         Set_Size_Request (Event_Box, Width, Height);
         Unref (Layout);
      end;

      --  Modified file area...
      if Show_Modified_Unmodified_In_Status_Bar then
         Gtk_New_Vseparator (Separator);
         Pack_End (Bar, Separator, Expand => False, Fill => False);

         Gtk_New (Bar.Modified_Label);
         Pack_End
           (Bar, Bar.Modified_Label, Expand => False, Fill => True);
      end if;

      --  Read only file area...
      Gtk_New_Vseparator (Separator);
      Pack_End (Bar, Separator, Expand => False, Fill => False);
      Gtk_New (Event_Box);
      Pack_End
        (Bar, Event_Box, Expand => False, Fill => True);
      Gtk_New (Bar.Read_Only_Label);
      Add (Event_Box, Bar.Read_Only_Label);
      Object_Return_Callback.Object_Connect
        (Event_Box, Signal_Button_Press_Event,
         On_Read_Only_Pressed'Access, Bar);

      --  Function location area
      Gtk_New (Bar.Function_Label);
      Set_Ellipsize (Bar.Function_Label, Ellipsize_Start);
      Set_Alignment (Bar.Function_Label, 0.0, 0.5);
      Bar.Pack_Start
        (Bar.Function_Label, Expand => True, Fill => True);
      Gtkada.Handlers.Return_Callback.Object_Connect
        (Bar.Function_Label,
         Gtk.Label.Signal_Activate_Link, On_Subprogram_Link'Access, Bar);

      Bar.Cursor_Handler := Bar_Callback.Connect
        (Bar.Buffer,
         Signal_Cursor_Position_Changed,
         Cursor_Position_Changed_Handler'Access,
         User_Data => Source_Editor_Status_Bar (Bar),
         After     => True);

      Bar.Buffer_Info_Handler := Bar_Callback.Connect
        (Bar.Buffer,
         Signal_Buffer_Information_Changed,
         Buffer_Information_Handler'Access,
         User_Data => Source_Editor_Status_Bar (Bar),
         After     => True);

      Show_Cursor_Position (Bar, Line => 1, Column => 1);

      Bar_Callback.Connect
        (Bar.View,
         Signal_Destroy,
         On_Bar_Destroy'Access,
         User_Data => Source_Editor_Status_Bar (Bar));
   end Initialize;

end Src_Editor_Status_Bar;
