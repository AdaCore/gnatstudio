-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                     Copyright (C) 2001                            --
--                         ACT-Europe                                --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Glib;                       use Glib;
with Glib.Values;
with Gtk;                        use Gtk;
with Gtk.Box;                    use Gtk.Box;
with Gtk.Container;              use Gtk.Container;
with Gtk.Enums;                  use Gtk.Enums;
with Gtk.Frame;                  use Gtk.Frame;
with Gtk.Handlers;               use Gtk.Handlers;
with Gtk.Label;                  use Gtk.Label;
with Gtk.Scrolled_Window;        use Gtk.Scrolled_Window;
with Gtk.Text_Layout;            use Gtk.Text_Layout;

with Language;                   use Language;
with Src_Editor;                 use Src_Editor;
with Src_Editor_Buffer;          use Src_Editor_Buffer;
with Src_Editor_View;            use Src_Editor_View;

package body Src_Editor_Box is

   Min_Line_Number_Width : constant := 3;
   --  The minimum number of digits that the Line Number Area should be
   --  able to display.

   Min_Column_Number_Width : constant := 3;
   --  The minimum number of digits that the Column Number Area should be
   --  able to display.

   package View_Callback is new Gtk.Handlers.User_Callback
     (Widget_Type => Source_View_Record,
      User_Type => Source_Editor_Box);

   package Buffer_Callback is new Gtk.Handlers.User_Callback
     (Widget_Type => Source_Buffer_Record,
      User_Type => Source_Editor_Box);

   --------------------------
   -- Forward declarations --
   --------------------------

   function To_Box_Line (Line : Gint) return Positive;
   pragma Inline (To_Box_Line);
   --  Convert a line number in the Source Buffer to a line number in the
   --  Source Box. This conversion is necessary because line numbers start
   --  from 1 in the Source Box (this is the natural numbering for humans),
   --  whereas it starts from 0 in the Source Box.

   function To_Box_Column (Col : Gint) return Positive;
   pragma Inline (To_Box_Column);
   --  Convert a column number in the Source Buffer to a column number
   --  in the Source Box. Same rationale as in To_Box_Line.

   function To_Buffer_Line (Line : Positive) return Gint;
   pragma Inline (To_Buffer_Line);
   --  Convert a line number in the Source Box to a line number in the
   --  Source Buffer. Same rationale as in To_Box_Line.

   function To_Buffer_Column (Col : Positive) return Gint;
   pragma Inline (To_Buffer_Column);
   --  Convert to a column number in the Source Box to a column number
   --  in the Source Buffer. Same rationale as in To_Box_Line.

   procedure Show_Cursor_Position
     (Box    : Source_Editor_Box;
      Line   : Gint;
      Column : Gint);
   --  Redraw the cursor position in the Line/Column areas of the status bar.

   procedure Cursor_Position_Changed_Handler
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues;
      Box    : Source_Editor_Box);
   --  This handler is merly a proxy to Show_Cursor_Position. It just
   --  extracts the necessary values from Params, and pass them on to
   --  Show_Cursor_Position.

   procedure Initialize_Box
     (Box    : access Source_Editor_Box_Record;
      Source : Source_Buffer := null;
      Lang   : Language.Language_Access);
   --  Perform the initialization of the given editor box. If Source_Buffer
   --  is null, then a new buffer will automatically be created. Otherwise,
   --  the editor creates a new editor for the same Source_Buffer.

   -----------------
   -- To_Box_Line --
   -----------------

   function To_Box_Line (Line : Gint) return Positive is
   begin
      return Positive (Line + 1);
   end To_Box_Line;

   -------------------
   -- To_Box_Column --
   -------------------

   function To_Box_Column (Col : Gint) return Positive is
   begin
      return Positive (Col + 1);
   end To_Box_Column;

   --------------------
   -- To_Buffer_Line --
   --------------------

   function To_Buffer_Line (Line : Positive) return Gint is
   begin
      return Gint (Line - 1);
   end To_Buffer_Line;

   ----------------------
   -- To_Buffer_Column --
   ----------------------

   function To_Buffer_Column (Col : Positive) return Gint is
   begin
      return Gint (Col - 1);
   end To_Buffer_Column;

   --------------------------
   -- Show_Cursor_Position --
   --------------------------

   procedure Show_Cursor_Position
     (Box    : Source_Editor_Box;
      Line   : Gint;
      Column : Gint)
   is
      Nb_Lines : constant Gint := Get_Line_Count (Box.Source_Buffer);
      Nb_Digits_For_Line_Number : constant Positive :=
        Positive'Max (Image (Nb_Lines)'Length, Min_Line_Number_Width);
   begin
      --  In the source buffer, the Line and Column indexes start from
      --  0. It is more natural to start from one, so the Line and Column
      --  number displayed are incremented by 1 to start from 1.
      Set_Text
        (Box.Cursor_Line_Label,
         Image (To_Box_Line (Line), Nb_Digits_For_Line_Number));
      Set_Text
        (Box.Cursor_Column_Label,
         Image (To_Box_Column (Column), Min_Column_Number_Width));
   end Show_Cursor_Position;

   -------------------------------------
   -- Cursor_Position_Changed_Handler --
   -------------------------------------

   procedure Cursor_Position_Changed_Handler
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues;
      Box    : Source_Editor_Box)
   is
      Line : constant Gint := Values.Get_Int (Values.Nth (Params, 1));
      Col  : constant Gint := Values.Get_Int (Values.Nth (Params, 2));
   begin
      Show_Cursor_Position (Box, Line => Line, Column => Col);
   end Cursor_Position_Changed_Handler;

   --------------------
   -- Initialize_Box --
   --------------------

   procedure Initialize_Box
     (Box    : access Source_Editor_Box_Record;
      Source : Source_Buffer := null;
      Lang   : Language.Language_Access)
   is
      Frame          : Gtk_Frame;
      Hbox           : Gtk_Box;
      Frame_Hbox     : Gtk_Box;
      Scrolling_Area : Gtk_Scrolled_Window;
      Label          : Gtk_Label;
   begin
      Gtk_New_Vbox (Box.Root_Container, Homogeneous => False);

      Gtk_New (Frame);
      Set_Shadow_Type (Frame, Shadow_Out);
      Pack_Start (Box.Root_Container, Frame, Expand => True, Fill => True);

      Gtk_New (Scrolling_Area);
      Set_Policy
        (Scrolling_Area,
         H_Scrollbar_Policy => Policy_Automatic,
         V_Scrollbar_Policy => Policy_Automatic);
      Add (Frame, Scrolling_Area);

      if Source = null then
         Gtk_New (Box.Source_Buffer, Lang => Lang);
      else
         Box.Source_Buffer := Source;
         Set_Language (Box.Source_Buffer, Lang => Lang);
      end if;

      Gtk_New (Box.Source_View, Box.Source_Buffer, Show_Line_Numbers => True);
      Add (Scrolling_Area, Box.Source_View);

      --  The status bar, at the bottom of the window...

      Gtk_New (Frame);
      Set_Shadow_Type (Frame, Shadow_Out);
      Pack_Start (Box.Root_Container, Frame, Expand => False, Fill => False);

      Gtk_New_Hbox (Hbox, Homogeneous => False, Spacing => 2);
      Add (Frame, Hbox);

      --  Filename area...
      Gtk_New (Frame);
      Set_Shadow_Type (Frame, Shadow_In);
      Pack_Start (Hbox, Frame, Expand => True, Fill => True);
      Gtk_New (Box.Filename_Label);

      --  Line number area...
      Gtk_New (Frame);
      Set_Shadow_Type (Frame, Shadow_In);
      Pack_Start (Hbox, Frame, Expand => False, Fill => True);
      Gtk_New_Hbox (Frame_Hbox, Homogeneous => False);
      Add (Frame, Frame_Hbox);
      Gtk_New (Label, -"Line:");
      Pack_Start (Frame_Hbox, Label, Expand => False, Fill => True);
      Gtk_New (Box.Cursor_Line_Label, "1");
      Pack_End
        (Frame_Hbox, Box.Cursor_Line_Label, Expand => False, Fill => True);

      --  Column number area...
      Gtk_New (Frame);
      Set_Shadow_Type (Frame, Shadow_In);
      Pack_Start (Hbox, Frame, Expand => False, Fill => True);
      Gtk_New_Hbox (Frame_Hbox, Homogeneous => False);
      Add (Frame, Frame_Hbox);
      Gtk_New (Label, -"Col:");
      Pack_Start (Frame_Hbox, Label, Expand => False, Fill => True);
      Gtk_New (Box.Cursor_Column_Label, "1");
      Pack_End
        (Frame_Hbox, Box.Cursor_Column_Label, Expand => False, Fill => True);

      Buffer_Callback.Connect
        (Widget    => Box.Source_Buffer,
         Name      => "cursor_position_changed",
         Cb        => Cursor_Position_Changed_Handler'Access,
         User_Data => Source_Editor_Box (Box),
         After     => True);

      Show_Cursor_Position (Source_Editor_Box (Box), Line => 0, Column => 0);

   end Initialize_Box;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Box  : out Source_Editor_Box;
      Lang : Language.Language_Access := null) is
   begin
      Box := new Source_Editor_Box_Record;
      Initialize (Box, Lang);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Box  : access Source_Editor_Box_Record;
      Lang : Language.Language_Access) is
   begin
      Initialize_Box (Box, Lang => Lang);
   end Initialize;

   ---------------------
   -- Create_New_View --
   ---------------------

   procedure Create_New_View
     (Box    : out Source_Editor_Box;
      Source : access Source_Editor_Box_Record) is
   begin
      Box := new Source_Editor_Box_Record;
      Initialize_Box (Box, Source.Source_Buffer, Get_Language (Source));
   end Create_New_View;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Box : in out Source_Editor_Box) is
   begin
      Unref (Box.Root_Container);
      Free (Box.Filename);
      Box := null;
   end Destroy;

   ------------
   -- Attach --
   ------------

   procedure Attach
     (Box    : access Source_Editor_Box_Record;
      Parent : access Gtk.Container.Gtk_Container_Record'Class) is
   begin
      Add (Parent, Box.Root_Container);

      --  When detaching the Root_Container, the Root_Container is Ref'ed to
      --  avoid its automatic destruction (see procedure Detach below). This
      --  implies that we need to Unref it each time we attach it, except for
      --  the first time (or we might end up destroying the Root_Container, as
      --  it has never been Ref'ed).

      if Box.Never_Attached then
         Box.Never_Attached := False;
      else
         Unref (Box.Root_Container);
      end if;
   end Attach;

   ------------
   -- Detach --
   ------------

   procedure Detach (Box    : access Source_Editor_Box_Record) is
      Parent : constant Gtk_Container :=
        Gtk_Container (Get_Parent (Box.Root_Container));
   begin
      --  Increment the reference counter before detaching the Root_Container
      --  from the parent widget, to make sure it is not automatically
      --  destroyed by gtk.
      Ref (Box.Root_Container);
      Remove (Parent, Box.Root_Container);
   end Detach;

   ---------------
   -- Load_File --
   ---------------

   procedure Load_File
     (Editor          : access Source_Editor_Box_Record;
      Filename        : String;
      Lang_Autodetect : Boolean := True;
      Success         : out Boolean) is
   begin
      Load_File (Editor.Source_Buffer, Filename, Lang_Autodetect, Success);
   end Load_File;

   ------------------
   -- Set_Language --
   ------------------

   procedure Set_Language
     (Editor : access Source_Editor_Box_Record;
      Lang   : Language.Language_Access := null) is
   begin
      Set_Language (Editor.Source_Buffer, Lang);
   end Set_Language;

   ------------------
   -- Get_Language --
   ------------------

   function Get_Language
     (Editor : access Source_Editor_Box_Record)
      return Language.Language_Access is
   begin
      return Get_Language (Editor.Source_Buffer);
   end Get_Language;

   -------------------------
   -- Set_Cursor_Location --
   -------------------------

   procedure Set_Cursor_Location
     (Editor  : access Source_Editor_Box_Record;
      Line    : Positive;
      Column  : Positive := 1;
      Success : out Boolean)
   is
      Buffer_Line  : constant Gint := To_Buffer_Line (Line);
      Buffer_Col   : constant Gint := To_Buffer_Column (Column);
   begin
      Set_Cursor_Position
        (Editor.Source_Buffer, Buffer_Line, Buffer_Col, Success);
      Scroll_To_Cursor_Location (Editor.Source_View);
   end Set_Cursor_Location;

   ------------------------
   -- Get_CursorLocation --
   ------------------------

   procedure Get_Cursor_Location
     (Editor  : access Source_Editor_Box_Record;
      Line    : out Positive;
      Column  : out Positive)
   is
      Buffer_Line : Gint;
      Buffer_Col  : Gint;
   begin
      Get_Cursor_Position (Editor.Source_Buffer, Buffer_Line, Buffer_Col);
      Line   := To_Box_Line (Buffer_Line);
      Column := To_Box_Column (Buffer_Col);
   end Get_Cursor_Location;

   --------------------
   -- Highlight_Line --
   --------------------

   procedure Highlight_Line
     (Editor  : access Source_Editor_Box_Record;
      Line    : Positive;
      Success : out Boolean) is
   begin
      Highlight_Line (Editor.Source_Buffer, To_Buffer_Line (Line), Success);
      --  Tell the view that something has changed in the layout, to avoid
      --  waiting for the next cursor blink to see the highlighting appearing.
      --  ??? Would that be a bug in Gtk+?
      Default_Style_Changed (Get_Layout (Editor.Source_View));
   end Highlight_Line;

   ----------------------
   -- Unhighlight_Line --
   ----------------------

   procedure Unhighlight_Line
     (Editor  : access Source_Editor_Box_Record;
      Line    : Positive;
      Success : out Boolean) is
   begin
      Unhighlight_Line (Editor.Source_Buffer, To_Buffer_Line (Line), Success);
      --  Tell the view that something has changed in the layout, to avoid
      --  waiting for the next cursor blink to see the highlighting appearing.
      --  ??? Would that be a bug in Gtk+?
      Default_Style_Changed (Get_Layout (Editor.Source_View));
   end Unhighlight_Line;

   ---------------------------
   -- Cancel_Highlight_Line --
   ---------------------------

   procedure Cancel_Highlight_Line
     (Editor : access Source_Editor_Box_Record) is
   begin
      Cancel_Highlight_Line (Editor.Source_Buffer);
      --  Tell the view that something has changed in the layout, to avoid
      --  waiting for the next cursor blink to see the highlighting appearing.
      --  ??? Would that be a bug in Gtk+?
      Default_Style_Changed (Get_Layout (Editor.Source_View));
   end Cancel_Highlight_Line;

end Src_Editor_Box;
