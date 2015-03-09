------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2013-2015, AdaCore                     --
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
with Basic_Types;               use Basic_Types;
with GNAT.Strings;              use GNAT.Strings;
with GNATCOLL.Symbols;          use GNATCOLL.Symbols;
with GNATCOLL.Traces;           use GNATCOLL.Traces;
with GNATCOLL.Utils;            use GNATCOLL.Utils;
with GNATCOLL.VFS;              use GNATCOLL.VFS;
with GPS.Editors;               use GPS.Editors;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GPS.Stock_Icons;           use GPS.Stock_Icons;
with Glib.Convert;
with Glib.Object;               use Glib.Object;
with Glib.Values;
with Glib; use Glib;
with Gtk.Arguments; use Gtk.Arguments;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Handlers; use Gtk.Handlers;
with Gtk.Image; use Gtk.Image;
with Gtk.Label; use Gtk.Label;
with Gtk.Separator; use Gtk.Separator;
with Gtk.Style_Context; use Gtk.Style_Context;
with Gtk.Text_Iter;     use Gtk.Text_Iter;
with Gtk.Widget; use Gtk.Widget;
with Gtkada.Handlers;
with Gtkada.MDI; use Gtkada.MDI;
with Language; use Language;
with Pango.Layout; use Pango.Layout;
with Src_Editor_Box; use Src_Editor_Box;
with Src_Editor_Module.Commands; use Src_Editor_Module.Commands;
with Src_Editor_Module.Markers; use Src_Editor_Module.Markers;
with Src_Editor_Module; use Src_Editor_Module;
with String_Utils; use String_Utils;
with Language.Abstract_Language_Tree; use Language.Abstract_Language_Tree;

package body Src_Editor_Status_Bar is

   Me : constant Trace_Handle := Create ("Src_Editor_Status_Bar");

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
            Remove (Bar.Info_Box, Bar.Buffer_Info_Frames (J).Label);
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
            Gtk_New_From_Icon_Name
               (Image, Icon_Name => Info (J).Icon.all,
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

         Pack_End
           (Bar.Info_Box,
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
         Trace (Me, E);
   end On_Bar_Destroy;

   ----------------------------
   -- Update_Subprogram_Name --
   ----------------------------

   procedure Update_Subprogram_Name
     (Bar : not null access Source_Editor_Status_Bar_Record'Class;
      Update_Tree : Boolean := False)
   is
      Block : Block_Record;
      Node  : Sem_Node_Holders.Holder;
      Val   : Unbounded_String;
   begin
      if Display_Subprogram_Names.Get_Pref then
         Block := Get_Subprogram_Block (Bar.Buffer, Bar.Current_Line,
                                        Update_Tree);
         if Block.Block_Type /= Cat_Unknown
           and then Block.Name /= No_Symbol
         then
            --  We cannot control the underline from the theme unfortunately.
            Val := To_Unbounded_String
              ("<span underline='none'><a href='" & Block.First_Line'Img & "'>"
               & Glib.Convert.Escape_Text (Get (Block.Name).all)
               & "</a></span>");

            Node := Sem_Node_Holders.To_Holder
              (Block.Tree_Node.Element.Parent);
            while Node.Element /= No_Semantic_Node loop
               Val :=
                 "<span underline='none'><a href='"
                 & Node.Element.Sloc_Start.Line'Img
                 & "'>" & Get (Node.Element.Name).all & "</a></span>."
                 & Val;
               Node.Replace_Element (Node.Element.Parent);
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
      Icon_Size : constant := 16;
   begin
      case Get_Status (Bar.Buffer) is
         when Unmodified =>
            if Show_Modified_Unmodified_In_Status_Bar then
               Bar.Modified_Label.Set_Tooltip_Text (-"Unmodified");
               Bar.Modified_Label.Set_From_Icon_Name
                  (File_Pixbuf, Icon_Size);
            end if;

            if Child /= null then
               Child.Set_Icon_Name (File_Pixbuf);
            end if;

         when Readonly =>
            if Show_Modified_Unmodified_In_Status_Bar then
               Bar.Modified_Label.Set_Tooltip_Text (-"Read only");
               Bar.Modified_Label.Set_From_Icon_Name
                  (File_Pixbuf, Icon_Size);
            end if;

            if Child /= null then
               Child.Set_Icon_Name (File_Pixbuf);
            end if;

         when Unsaved =>
            if Show_Modified_Unmodified_In_Status_Bar then
               Bar.Modified_Label.Set_Tooltip_Text (-"Unsaved");
               Bar.Modified_Label.Set_From_Icon_Name
                  (File_Unsaved_Pixbuf, Icon_Size);
            end if;

            if Child /= null then
               Child.Set_Icon_Name (File_Unsaved_Pixbuf);
            end if;

         when Saved =>
            if Show_Modified_Unmodified_In_Status_Bar then
               Bar.Modified_Label.Set_Tooltip_Text (-"Saved");
               Bar.Modified_Label.Set_From_Icon_Name
                  (File_Pixbuf, Icon_Size);
            end if;

            if Child /= null then
               Child.Set_Icon_Name (File_Pixbuf);
            end if;

         when Modified =>
            if Show_Modified_Unmodified_In_Status_Bar then
               Bar.Modified_Label.Set_Tooltip_Text (-"Modified");
               Bar.Modified_Label.Set_From_Icon_Name
                  (File_Modified_Pixbuf, Icon_Size);
            end if;

            if Child /= null then
               Child.Set_Icon_Name (File_Modified_Pixbuf);
            end if;
      end case;

      if Get_Writable (Bar.Buffer) then
         Set_From_Icon_Name
            (Bar.Read_Only_Label, "gps-unlock-symbolic",
             Icon_Size_Local_Toolbar);
         Bar.Read_Only_Label.Set_Tooltip_Text (-"Writable");
         Get_Style_Context (Bar.View).Remove_Class ("read-only");
      else
         Set_From_Icon_Name
            (Bar.Read_Only_Label, "gps-lock-symbolic",
             Icon_Size_Local_Toolbar);
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
         Trace (Me, E);
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
      Column : Character_Offset_Type)
   is
      Pos : constant String :=
         String_Utils.Image (Integer (Line))
         & ':' & String_Utils.Image (Integer (Column));
      Start, The_End : Gtk_Text_Iter;
      Result : Boolean;
      Lines, Offset : Gint;
   begin
      Bar.Buffer.Get_Selection_Bounds (Start, The_End, Result);
      if Result then
         Lines := Get_Line (The_End) - Get_Line (Start) + 1;
         Offset := Get_Offset (The_End) - Get_Offset (Start);
         Bar.Cursor_Loc_Label.Set_Text
            ("("
             & Image (Integer (Lines), Min_Width => 1) & " lines,"
             & Offset'Img & " chars) " & Pos);
      else
         Bar.Cursor_Loc_Label.Set_Text (Pos);
      end if;
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

      Child := Find_Editor
        (Get_Kernel (Bar.Buffer),
         File,
         Get_Project (Bar.View));

      if Child /= null and then Get_Widget (Child) = Gtk_Widget (Bar.Box) then
         Show_Cursor_Position
           (Bar,
            Line   => Bar.Current_Line,
            Column => Character_Offset_Type
              (Values.Get_Int (Values.Nth (Params, 2))));
      end if;

   exception
      when E : others =>
         Trace (Me, E);
   end Cursor_Position_Changed_Handler;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Bar    : out Source_Editor_Status_Bar;
      Box    : Gtk_Event_Box;
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
      Box    : Gtk_Event_Box;
      View   : Source_View;
      Buffer : Source_Buffer)
   is
      Event_Box      : Gtk_Event_Box;
      Separator      : Gtk_Vseparator;
   begin
      Gtk.Event_Box.Initialize (Bar);
      Gtk_New_Hbox (Bar.HBox);
      Bar.Add (Bar.HBox);
      Bar.View := View;
      Bar.Buffer := Buffer;
      Bar.Box := Box;

      --  Avoid resizing the main window whenever a label is changed
      Bar.Set_Resize_Mode (Resize_Queue);

      --  Function location area
      Gtk_New (Bar.Function_Label);
      Bar.Function_Label.Set_Ellipsize (Ellipsize_Start);
      Bar.Function_Label.Set_Alignment (0.0, 0.5);
      Bar.HBox.Pack_Start (Bar.Function_Label, Expand => True, Fill => True);
      Gtkada.Handlers.Return_Callback.Object_Connect
        (Bar.Function_Label,
         Gtk.Label.Signal_Activate_Link, On_Subprogram_Link'Access, Bar);

      --  Line:Column number area...
      Gtk_New (Event_Box);
      Bar.HBox.Pack_Start
        (Event_Box, Expand => False, Fill => True, Padding => 5);
      Gtk_New (Bar.Cursor_Loc_Label, "1:1");
      Event_Box.Add (Bar.Cursor_Loc_Label);
      Object_Return_Callback.Object_Connect
        (Event_Box, Signal_Button_Press_Event, On_Goto_Line_Func'Access, Bar);

      Gtk_New_Vseparator (Separator);
      Pack_Start (Bar.HBox, Separator, Expand => False, Fill => False);

      --  Modified file area...
      if Show_Modified_Unmodified_In_Status_Bar then
         Gtk_New (Bar.Modified_Label);
         Bar.HBox.Pack_Start
           (Bar.Modified_Label, Expand => False, Fill => False);
      end if;

      Gtk_New_Hbox (Bar.Info_Box, Homogeneous => False);
      Bar.HBox.Pack_Start (Bar.Info_Box, Expand => False, Fill => False);

      --  Read only file area...
      --  Leave this to the right of the info_box area, since the latter
      --  is displayed after a delay, and people trying to click on the lock
      --  icon too early might end up clicking on the info_box area
      --  instead.
      Gtk_New (Bar.Read_Only_Label);
      Gtk_New (Event_Box);
      Event_Box.Add (Bar.Read_Only_Label);
      Bar.HBox.Pack_Start (Event_Box, Expand => False, Fill => False);
      Object_Return_Callback.Object_Connect
        (Event_Box, Signal_Button_Press_Event,
         On_Read_Only_Pressed'Access, Bar);

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

      Get_Style_Context (Bar).Add_Class ("gps-editor-status-bar");
   end Initialize;

end Src_Editor_Status_Bar;
