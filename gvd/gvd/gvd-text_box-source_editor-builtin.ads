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
with GVD.Types;
with GVD.Explorer;
with GVD.Text_Boxes;
with GVD.Tooltips;
with Items;
with Gtk.Text;

package GVD.Source_Editors is

   type Source_Editor_Record is new GVD.Text_Boxes.GVD_Text_Box_Record with
     private;
   type Source_Editor is access all Source_Editor_Record'Class;

   procedure Gtk_New
     (Editor  : out Source_Editor;
      Process : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Create a new asm editor.

   procedure Initialize
     (Editor  : access Source_Editor_Record'Class;
      Process : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Internal procedure.

   procedure Configure
     (Editor            : access Source_Editor_Record;
      Ps_Font_Name      : String;
      Font_Size         : Glib.Gint;
      Default_Icon      : Gtkada.Types.Chars_Ptr_Array;
      Current_Line_Icon : Gtkada.Types.Chars_Ptr_Array;
      Stop_Icon         : Gtkada.Types.Chars_Ptr_Array;
      Comments_Color    : Gdk.Color.Gdk_Color;
      Strings_Color     : Gdk.Color.Gdk_Color;
      Keywords_Color    : Gdk.Color.Gdk_Color);
   --  See GVD.Code_Editors for more information

   function On_Pixmap_Clicked
     (Editor : access Source_Editor_Record;
      Button : Natural;
      Line   : Natural) return Boolean;
   --  See GVD.Text_Boxes for documentation

   function Invisible_Column_Width
     (Editor : access Source_Editor_Record) return Glib.Gint;
   --  See GVD.Text_Boxes for documentation

   function Child_Contextual_Menu
     (Source : access Source_Editor_Record;
      Line   : Natural;
      Entity : String) return Gtk.Menu.Gtk_Menu;
   --  See GVD.Text_Boxes for documentation

   procedure Insert_Buffer
     (Editor : access Source_Editor_Record;
      Buffer : String);
   --  Insert the contents of the buffer in the editor. Color highlighting is
   --  provided, and line numbers may or may not be added.

   procedure Set_Current_Language
     (Editor : access Source_Editor_Record;
      Lang   : Language.Language_Access);
   --  Change the current language for the editor.
   --  The text already present in the editor is not re-highlighted for the
   --  new language, this only influences future addition to the editor.
   --
   --  If Lang is null, then no color highlighting will be performed.

   procedure Highlight_Word
     (Editor   : access Source_Editor_Record;
      Position : GVD.Explorer.Position_Type);
   --  Highlight the word that starts at the given position in the file
   --  associated with the editor (ie ignoring the line numbers that could
   --  be displayed).

   procedure Update_Breakpoints
     (Editor : access Source_Editor_Record;
      Br     : GVD.Types.Breakpoint_Array);
   --  Change the list of breakpoints to highlight in the editor.
   --  All the breakpoints that previously existed are removed from the screen,
   --  and replaced by the new ones.
   --  The breakpoints that do not apply to the current file are ignored.

   procedure Set_Show_Line_Nums
     (Editor : access Source_Editor_Record;
      Show   : Boolean := False);
   --  Indicate whether line numbers should be displayed or not.

   function Get_Show_Line_Nums
     (Editor : access Source_Editor_Record) return Boolean;
   --  Return the state of line numbers in the editor

   procedure Set_Show_Lines_With_Code
     (Editor : access Source_Editor_Record;
      Show   : Boolean);
   function Get_Show_Lines_With_Code
     (Editor : access Source_Editor_Record) return Boolean;
   --  Indicate whether lines where a user can set a breakpoint have a small
   --  dot displayed on the side.

   function Get_Current_File
     (Editor : access Source_Editor_Record) return String;
   --  See GVD.Code_Editors for more information

   procedure Load_File
     (Editor      : access Source_Editor_Record;
      File_Name   : String;
      Set_Current : Boolean := True);
   --  Load and append a file in the editor.
   --  The contents is highlighted based on the current language.
   --  Debugger is used to calculate which lines should get icons on the side,
   --  through calls to Line_Contains_Code.
   --  If Set_Current is True, then File_Name becomes the current file for the
   --  debugger (ie the one that contains the current execution line).

   procedure File_Not_Found
     (Editor    : access Source_Editor_Record;
      File_Name : String);
   --  Report a file not found.
   --  This delete the currently displayed file, and display a warning
   --  message.

   procedure Highlight_Current_Line (Editor : access Source_Editor_Record);
   --  Highlight the current line in the editor, if required by the user.

private
   type Editor_Tooltip_Data is record
      Box  : Source_Editor;
      Mode : Items.Display_Mode := Items.Value;
      Lang : Language.Language_Access;
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

   type Source_Editor_Record is new GVD.Text_Boxes.GVD_Text_Box_Record with
   record
      Process : Gtk.Widget.Gtk_Widget;

      Show_Line_Nums       : Boolean;
      Show_Lines_With_Code : Boolean;
      --  Whether the lines where one can set a breakpoint have a small dot
      --  on the side.

      Current_File : GVD.Types.String_Access;

      Debugger_Current_File : GVD.Types.String_Access;
      --  The file/line on which the debugger is stopped (ie these were set
      --  when the Set_Current parameter is True for Set_line and Load_File)

      Lang           : Language.Language_Access;
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

      Current_File_Cache : GVD.Types.File_Cache_List;
      --  Cached data for the file currently displayed

      Tooltip : Editor_Tooltips.Tooltips;
      --  Those tooltips display the value of variables pointed to by the
      --  mouse.

      Highlight_Color     : Gdk.Color.Gdk_Color;
   end record;

end GVD.Source_Editors;
