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
with Gdk.GC;
with Gdk.Event;
with Gdk.Font;

with Pango.Font;

with Gtk.Text_View;

with Glide_Kernel.Modules; use Glide_Kernel.Modules;
with Src_Editor_Buffer;

with Basic_Types; use Basic_Types;

with Ada.Unchecked_Deallocation;

package Src_Editor_View is

   type Source_View_Record is new Gtk.Text_View.Gtk_Text_View_Record
     with private;
   type Source_View is access all Source_View_Record'Class;

   procedure Gtk_New
     (View   : out Source_View;
      Buffer : Src_Editor_Buffer.Source_Buffer := null;
      Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Create a new Source_View from the given parameters.
   --  If no Buffer is given, then a new one will be created. For tasks such
   --  as source code edition, it is recommended to specify a fixed-width font,
   --  as the default font used when not specified is proportional (which means
   --  that 'i's will be smaller than 'm's for instance).
   --
   --  If requested, the line numbers are displayed in a small area on
   --  the left of the text view.

   procedure Initialize
     (View   : access Source_View_Record;
      Buffer : Src_Editor_Buffer.Source_Buffer;
      Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Internal initialization procedure.
   --  See the section "Creating your own widgets" in the documentation.

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

   procedure Create_Line_Information_Column
     (View          : access Source_View_Record;
      Identifier    : String;
      Stick_To_Data : Boolean;
      Every_Line    : Boolean);

   procedure Remove_Line_Information_Column
     (View       : access Source_View_Record;
      Identifier : String);

   procedure Add_File_Information
     (View       : access Source_View_Record;
      Identifier : String;
      Info       : Glide_Kernel.Modules.Line_Information_Data);
   --  Add the line information to the view.
   --  User must not free Info.

   procedure Delete (View : access Source_View_Record);
   --  Free memory associated to View.

private

   type Line_Information_Access is access Line_Information_Record;

   type Line_Info_Width is record
      Info  : Line_Information_Access;
      Width : Integer := 0;
   end record;

   type Line_Info_Width_Array is array (Natural range <>) of Line_Info_Width;
   type Line_Info_Width_Array_Access is access Line_Info_Width_Array;

   type Position is (Left, Right);

   type Line_Info_Display_Record is record
      Identifier    : String_Access;
      --  This identifies the column.

      Starting_X    : Integer;
      --  The pixel distance between the left border of the column and
      --  the left border of the left window.

      Width         : Integer;
      --  The pixel width of the column.

      Column_Info   : Line_Info_Width_Array_Access;
      --  The information that should be displayed in the column.

      Stick_To_Data : Boolean;
      --  If Stick_To_Data is True, then the column contains information
      --  that are relative to the file as it was opened in the first
      --  place: when you insert or remove lines, the information should
      --  stick to the lines they are associated with.
      --  If Stick_To_Data is False, then the information "sticks" to
      --  the line numbers.

      Every_Line : Boolean;
      --  If Every_Line is True, then there must be data at every line in
      --  this column.
   end record;
   type Line_Info_Display_Access is access Line_Info_Display_Record;

   type Line_Info_Display_Array is array (Natural range <>)
     of Line_Info_Display_Access;

   type Line_Info_Display_Array_Access is access Line_Info_Display_Array;
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Line_Info_Display_Array, Line_Info_Display_Array_Access);

   type Natural_Array is array (Natural range <>) of Natural;
   type Natural_Array_Access is access Natural_Array;
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Natural_Array, Natural_Array_Access);

   type Source_View_Record is new Gtk.Text_View.Gtk_Text_View_Record with
   record
      Saved_Cursor_Line   : Gint;
      Saved_Cursor_Column : Gint;

      Pango_Font          : Pango.Font.Pango_Font_Description;
      Font                : Gdk.Font.Gdk_Font;
      Side_Column_GC      : Gdk.GC.Gdk_GC;
      Side_Background_GC  : Gdk.GC.Gdk_GC;

      Top_Line            : Natural := 1;
      Bottom_Line         : Natural := 0;

      Line_Info           : Line_Info_Display_Array_Access;
      --  The information that should be displayed in the left window.

      Total_Column_Width  : Natural := 3;
      --  Width of the Left Window, in pixels.

      Original_Lines_Number : Natural := 1;
      --  The number of lines in the file when it was first opened.

      Real_Lines            : Natural_Array_Access;
      --  This array associates original line numbers (ie lines that were
      --  in the view the first time it was opened) with lines in the current
      --  view.
      --  ??? The line number is already stored in Line_Information_Access

      Original_Text_Inserted : Boolean := False;
   end record;

end Src_Editor_View;
