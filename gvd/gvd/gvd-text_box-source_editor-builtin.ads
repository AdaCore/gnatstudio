-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
--                              ACT-Europe                           --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Glib;
with Gdk.Bitmap;
with Gdk.Color;
with Gdk.Pixmap;
with Gdk.Rectangle;
with Gtk.Main;
with Gtk.Menu;
with Gtk.Widget;
with Gtkada.Types;
with Language;
with Basic_Types;
with GVD.Types;
with GVD.Tooltips;
pragma Elaborate_All (GVD.Tooltips);
with Items;
with Gtk.Text;

--  This package provides an implementation of Source_Editor built-in
--  GVD, which means that GVD specific features are used, providing a very
--  tight integration with the debugger.
--  See GVD.Text_Box.Source_Editor.Socket for another implementation of
--  a Source_Editor.

package GVD.Text_Box.Source_Editor.Builtin is

   type Builtin_Record is new Source_Editor_Record with private;
   type Builtin is access all Builtin_Record'Class;

   procedure Gtk_New
     (Editor            : out Builtin;
      Process           : access Gtk.Widget.Gtk_Widget_Record'Class;
      TTY_Mode          : Boolean;
      Ps_Font_Name      : String;
      Font_Size         : Glib.Gint;
      Default_Icon      : Gtkada.Types.Chars_Ptr_Array;
      Current_Line_Icon : Gtkada.Types.Chars_Ptr_Array;
      Stop_Icon         : Gtkada.Types.Chars_Ptr_Array;
      Comments_Color    : Gdk.Color.Gdk_Color;
      Strings_Color     : Gdk.Color.Gdk_Color;
      Keywords_Color    : Gdk.Color.Gdk_Color);
   --  Create a new source editor.
   --  See GVD.Code_Editors.Configure for more information on the font, color
   --  and icon parameters.

   procedure Initialize
     (Editor            : access Builtin_Record'Class;
      Process           : access Gtk.Widget.Gtk_Widget_Record'Class;
      TTY_Mode          : Boolean;
      Ps_Font_Name      : String;
      Font_Size         : Glib.Gint;
      Default_Icon      : Gtkada.Types.Chars_Ptr_Array;
      Current_Line_Icon : Gtkada.Types.Chars_Ptr_Array;
      Stop_Icon         : Gtkada.Types.Chars_Ptr_Array;
      Comments_Color    : Gdk.Color.Gdk_Color;
      Strings_Color     : Gdk.Color.Gdk_Color;
      Keywords_Color    : Gdk.Color.Gdk_Color);
   --  Internal procedure.

   procedure Attach
     (Editor : access Builtin_Record;
      Parent : access Gtk.Container.Gtk_Container_Record'Class);
   --  See GVD.Text_Box.Source_Editor for more information.

   procedure Detach (Editor : access Builtin_Record);
   --  See GVD.Text_Box.Source_Editor for more information.

   procedure Highlight_Word
     (Editor   : access Builtin_Record;
      Line     : Natural;
      Column   : Natural;
      Position : Basic_Types.Position_Type);
   --  See GVD.Text_Box.Source_Editor for more information.

   procedure Update_Breakpoints
     (Editor : access Builtin_Record;
      Br     : GVD.Types.Breakpoint_Array);
   --  See GVD.Text_Box.Source_Editor for more information.

   procedure Set_Show_Line_Nums
     (Editor : access Builtin_Record;
      Show   : Boolean := False);
   --  See GVD.Text_Box.Source_Editor for more information.

   function Get_Show_Line_Nums
     (Editor : access Builtin_Record) return Boolean;
   --  Return the state of line numbers in the editor

   procedure Set_Show_Lines_With_Code
     (Editor : access Builtin_Record;
      Show   : Boolean);

   function Get_Show_Lines_With_Code
     (Editor : access Builtin_Record) return Boolean;
   --  Indicate whether lines where a user can set a breakpoint have a small
   --  dot displayed on the side.

   procedure Load_File
     (Editor      : access Builtin_Record;
      File_Name   : String;
      Set_Current : Boolean := True;
      Force       : Boolean := False);
   --  See GVD.Text_Boxes.Source_Editor for more information.

   procedure File_Not_Found
     (Editor    : access Builtin_Record;
      File_Name : String);
   --  Report a file not found.
   --  This delete the currently displayed file, and display a warning
   --  message.

   procedure Highlight_Current_Line (Editor : access Builtin_Record);
   --  See GVD.Text_Boxes.Source_Editor for more information.

   procedure Preferences_Changed (Editor : access Builtin_Record);
   --  See GVD.Text_Boxes.Source_Editor for more information.

   procedure Set_Line
     (Editor      : access Builtin_Record;
      Line        : Natural;
      Set_Current : Boolean := True);
   --  See GVD.Text_Boxes.Source_Editor for more information.

   function Get_Line (Editor : access Builtin_Record) return Natural;
   --  See GVD.Text_Boxes.Source_Editor for more information.

private
   type Editor_Tooltip_Data is record
      Box  : Builtin;
      Mode : Items.Display_Mode := Items.Value;
   end record;

   procedure Draw_Tooltip
     (Widget : access Gtk.Text.Gtk_Text_Record'Class;
      Data   : in out Editor_Tooltip_Data;
      Pixmap : out Gdk.Pixmap.Gdk_Pixmap;
      Width,
      Height : out Glib.Gint;
      Area   : out Gdk.Rectangle.Gdk_Rectangle);

   package Editor_Tooltips is new GVD.Tooltips
     (Editor_Tooltip_Data, Gtk.Text.Gtk_Text_Record, Draw_Tooltip);

   type Color_Array is array (Language.Language_Entity'Range) of
     Gdk.Color.Gdk_Color;

   type Builtin_Record is new Source_Editor_Record with record
      Process              : Gtk.Widget.Gtk_Widget;

      Show_Line_Nums       : Boolean;
      Show_Lines_With_Code : Boolean;
      --  Whether the lines where one can set a breakpoint have a small dot
      --  on the side.

      Default_Pixmap : Gdk.Pixmap.Gdk_Pixmap := Gdk.Pixmap.Null_Pixmap;
      Default_Mask   : Gdk.Bitmap.Gdk_Bitmap := Gdk.Bitmap.Null_Bitmap;
      Stop_Pixmap    : Gdk.Pixmap.Gdk_Pixmap := Gdk.Pixmap.Null_Pixmap;
      Stop_Mask      : Gdk.Bitmap.Gdk_Bitmap := Gdk.Bitmap.Null_Bitmap;
      Colors         : Color_Array := (others => Gdk.Color.Null_Color);

      Breakpoint_Buttons : Gtk.Widget.Widget_List.Glist;
      --  The pixmaps for each of the breakpoints

      Idle_Id : Gtk.Main.Idle_Handler_Id := 0;
      --  Id for the Idle handle that is used to recompute the lines that
      --  contain some code.

      Current_File_Cache : Basic_Types.File_Cache_List;
      --  Cached data for the file currently displayed

      Tooltip : Editor_Tooltips.Tooltips;
      --  Those tooltips display the value of variables pointed to by the
      --  mouse.

      Highlight_Color     : Gdk.Color.Gdk_Color;
      Contextual_Menu     : Gtk.Menu.Gtk_Menu;
      Never_Attached      : Boolean := True;
   end record;

end GVD.Text_Box.Source_Editor.Builtin;
