-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                   Copyright (C) 2001-2002                         --
--                            ACT-Europe                             --
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

--  <description>
--  This package provides a high-level text view extended to support
--  many functionalities related to source code editing.
--  </description>

with Glib; use Glib;
with Pango.Font;
with Gdk.GC;
with Gdk.Event;
with Gdk.Font;

with Gtk.Text_Mark;
with Gtk.Text_View;

with Glide_Kernel.Modules; use Glide_Kernel.Modules;
with Src_Editor_Buffer;

with Generic_List;
with Basic_Types; use Basic_Types;

package Src_Editor_View is

   type Source_View_Record is new Gtk.Text_View.Gtk_Text_View_Record
     with private;
   type Source_View is access all Source_View_Record'Class;

   procedure Gtk_New
     (View              : out Source_View;
      Buffer            : Src_Editor_Buffer.Source_Buffer := null;
      Font              : Pango.Font.Pango_Font_Description;
      Show_Line_Numbers : Boolean := False);
   --  Create a new Source_View from the given parameters.
   --  If no Buffer is given, then a new one will be created. For tasks such
   --  as source code edition, it is recommended to specify a fixed-width font,
   --  as the default font used when not specified is proportional (which means
   --  that 'i's will be smaller than 'm's for instance).
   --
   --  If requested, the line numbers are displayed in a small area on
   --  the left of the text view.

   procedure Initialize
     (View              : access Source_View_Record;
      Buffer            : Src_Editor_Buffer.Source_Buffer;
      Font              : Pango.Font.Pango_Font_Description;
      Show_Line_Numbers : Boolean);
   --  Internal initialization procedure.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Set_Show_Line_Numbers
     (View         : access Source_View_Record;
      Show_Numbers : Boolean := True);
   --  Set whether the line numbers should be displayed or not.

   function Get_Show_Line_Numbers
     (View : access Source_View_Record) return Boolean;
   --  Returns True if the line numbers are displayed.

   procedure Scroll_To_Cursor_Location (View : access Source_View_Record);
   --  Scroll the Source View if the position of the insert cursor is not
   --  within the part of the text currently visible.

   procedure Window_To_Buffer_Coords
     (View          : access Source_View_Record;
      X, Y          : Gint;
      Line          : out Gint;
      Column        : out Gint;
      Out_Of_Bounds : out Boolean);
   --  Translate the window coordinates (X, Y) into a Line/Column
   --  position in the buffer of the given Source_View_Record.
   --  If X, Y is outside the text area (for instance too far to the right of
   --  a line), then Line and Column are set to the closest matching position,
   --  and Out_Of_Bounds is set to True.

   procedure Event_To_Buffer_Coords
     (View          : access Source_View_Record;
      Event         : Gdk.Event.Gdk_Event;
      Line          : out Gint;
      Column        : out Gint;
      Out_Of_Bounds : out Boolean);
   --  Translate the window coordinates of the Event into a Line/Column
   --  position in the buffer of the given Source_View_Record.
   --  If X, Y is outside the text area (for instance too far to the right of
   --  a line), then Line and Column are set to the closest matching position,
   --  and Out_Of_Bounds is set to True.

   procedure Add_File_Information
     (View          : access Source_View_Record;
      Identifier    : String;
      Info          : Glide_Kernel.Modules.Line_Information_Data;
      Stick_To_Data : Boolean := True);
   --  Add the line information to the view.
   --
   --  ??? Who frees Info (the caller or Add_File_Information) ?

private

   type Line_Information_Access is access Line_Information_Record;

   type Line_Info_Width is record
      Info  : Line_Information_Access;
      Width : Integer;
   end record;

   procedure Free (X : in out Line_Info_Width);

   package Line_Info_List is new Generic_List (Line_Info_Width);

   type Position is (Left, Right);

   type Line_Info_Display_Record is record
      Identifier    : String_Access;
      Starting_X    : Integer;
      Width         : Integer;
      Column_Info   : Line_Info_List.List;
      Stick_To_Data : Boolean;
   end record;

   type Line_Info_Display_Array is array (Natural range <>)
     of Line_Info_Display_Record;

   type Line_Info_Display_Access is access Line_Info_Display_Array;

   type List_Access is access Line_Info_List.List;

   type Source_View_Record is new Gtk.Text_View.Gtk_Text_View_Record with
   record
      Saved_Insert_Mark   : Gtk.Text_Mark.Gtk_Text_Mark;
      Pango_Font          : Pango.Font.Pango_Font_Description;
      Font                : Gdk.Font.Gdk_Font;
      Line_Numbers_GC     : Gdk.GC.Gdk_GC;
      Show_Line_Numbers   : Boolean;
      LNA_Width_In_Digits : Natural;

      Top_Line            : Natural := 0;
      Bottom_Line         : Natural := 0;

      Min_Top_Line        : Natural := 0;
      Max_Bottom_Line     : Natural := 0;

      Line_Info           : Line_Info_Display_Access;
      Total_Column_Width  : Natural := 0;
   end record;

end Src_Editor_View;
