-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
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

with Glib;                       use Glib;
with Glib.Object;
with Glib.Values;
with Glide_Kernel;               use Glide_Kernel;
with Glide_Kernel.Console;       use Glide_Kernel.Console;
with Glide_Kernel.Modules;       use Glide_Kernel.Modules;
with Glide_Kernel.Project;       use Glide_Kernel.Project;
with Gdk;                        use Gdk;
with Gdk.Event;                  use Gdk.Event;
with Gdk.Rectangle;              use Gdk.Rectangle;
with Gdk.Types;
with Gdk.Window;                 use Gdk.Window;

with Gtk;                        use Gtk;
with Gtk.Box;                    use Gtk.Box;
with Gtk.Container;              use Gtk.Container;
with Gtk.Enums;                  use Gtk.Enums;
with Gtk.Frame;                  use Gtk.Frame;
with Gtk.Handlers;               use Gtk.Handlers;
with Gtk.Label;                  use Gtk.Label;
with Gtk.Menu;                   use Gtk.Menu;
with Gtk.Menu_Item;              use Gtk.Menu_Item;
with Gtk.Scrolled_Window;        use Gtk.Scrolled_Window;
with Gtk.Text_Iter;              use Gtk.Text_Iter;
with GUI_Utils;                  use GUI_Utils;
with Glide_Intl;                 use Glide_Intl;
with GNAT.Directory_Operations;  use GNAT.Directory_Operations;

with Language;                   use Language;
with String_Utils;               use String_Utils;
with Src_Editor_Buffer;          use Src_Editor_Buffer;
with Src_Editor_View;            use Src_Editor_View;
with Src_Editor_Module;          use Src_Editor_Module;
with Src_Info;                   use Src_Info;
with Src_Info.Queries;           use Src_Info.Queries;
with Traces;                     use Traces;

package body Src_Editor_Box is

   Me : Debug_Handle := Create ("Source_Editor");

   Min_Line_Number_Width : constant := 3;
   --  The minimum number of digits that the Line Number Area should be
   --  able to display.

   Min_Column_Number_Width : constant := 3;
   --  The minimum number of digits that the Column Number Area should be
   --  able to display.

   package Widget_Callback is new Gtk.Handlers.User_Callback
     (Widget_Type => Glib.Object.GObject_Record,
      User_Type   => Source_Editor_Box);

   package Return_Callback is new Gtk.Handlers.User_Return_Callback
     (Widget_Type => Source_View_Record,
      Return_Type => Boolean,
      User_Type   => Source_Editor_Box);

   --------------------------
   -- Forward declarations --
   --------------------------

   function To_Box_Line (Line : Gint) return Natural;
   pragma Inline (To_Box_Line);
   --  Convert a line number in the Source Buffer to a line number in the
   --  Source Box. This conversion is necessary because line numbers start
   --  from 1 in the Source Box (this is the natural numbering for humans),
   --  whereas it starts from 0 in the Source Box.

   function To_Box_Column (Col : Gint) return Natural;
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
     (Buffer : access Glib.Object.GObject_Record'Class;
      Params : Glib.Values.GValues;
      Box    : Source_Editor_Box);
   --  This handler is merely a proxy to Show_Cursor_Position. It just
   --  extracts the necessary values from Params, and pass them on to
   --  Show_Cursor_Position.

   procedure Initialize_Box
     (Box    : access Source_Editor_Box_Record;
      Kernel : Glide_Kernel.Kernel_Handle;
      Source : Source_Buffer := null;
      Lang   : Language.Language_Access);
   --  Perform the initialization of the given editor box. If Source_Buffer
   --  is null, then a new buffer will automatically be created. Otherwise,
   --  the editor creates a new editor for the same Source_Buffer.

   function Is_Entity_Letter (Char : Character) return Boolean;
   --  Return True if the given letter is a valid letter for an entity name
   --  (ie if the letter is either alphanumeric or an '_').

   procedure Search_Entity_Bounds
     (Buffer       : access Source_Buffer_Record'Class;
      Start_Iter   : in out Gtk_Text_Iter;
      End_Iter     : out Gtk_Text_Iter);
   --  Find the position of the begining and the end of the entity pointed to
   --  by Start_Iter.

   ----------------------------------
   -- The contextual menu handling --
   ----------------------------------

   package CMenu is new GUI_Utils.User_Contextual_Menus
     (User_Data => Source_Editor_Box);
   --  Used to register contextual menus with a user data.

   function Get_Contextual_Menu
     (Editor : Source_Editor_Box;
      Event  : Gdk_Event) return Gtk_Menu;
   --  Return the contextual menu to use for the source box.

   procedure On_Goto_Declaration_Or_Body
     (Widget : access Glib.Object.GObject_Record'Class;
      Params : Glib.Values.GValues;
      Editor : Source_Editor_Box);
   --  Callback for the "Goto Declaration<->Body" contextual menu

   procedure Entity_At_Cursor
     (Editor                   : access Source_Editor_Box_Record;
      Line, Column             : Natural := 0;
      Entity_Start, Entity_End : out Gtk_Text_Iter);
   --  Return the beginning position of the entity at (Line, Column), or under
   --  the cursor if Line or Column is 0.
   --  The entity itself can be read through a call to Get_Text.

   ----------------------
   -- Entity_At_Cursor --
   ----------------------

   procedure Entity_At_Cursor
     (Editor                   : access Source_Editor_Box_Record;
      Line, Column             : Natural := 0;
      Entity_Start, Entity_End : out Gtk_Text_Iter)
   is
      Tmp_Line   : Natural := Line;
      Tmp_Col    : Natural := Column;

   begin
      --  Get the cursor position if either Line or Column is equal to 0
      if Tmp_Line = 0 or else Tmp_Col = 0 then
         Get_Cursor_Location (Editor, Tmp_Line, Tmp_Col);
      end if;

      --  Transform this position into an iterator, and then locate the
      --  bounds of the entity...
      Get_Iter_At_Line_Offset
        (Editor.Source_Buffer, Entity_Start,
         To_Buffer_Line (Tmp_Line), To_Buffer_Column (Tmp_Col));
      Search_Entity_Bounds (Editor.Source_Buffer, Entity_Start, Entity_End);
   end Entity_At_Cursor;

   ------------------------------
   -- Goto_Declaration_Or_Body --
   ------------------------------

   procedure Goto_Declaration_Or_Body
     (Kernel : access Kernel_Handle_Record'Class;
      Line   : Natural := 0;
      Column : Natural := 0)
   is
      Source        : Source_Editor_Box := Find_Current_Editor (Kernel);
      Source_Info   : LI_File_Ptr;
      Filename      : String_Access;
      Entity_Start, Entity_End : Gtk_Text_Iter;
      Start_Line    : Positive;
      Start_Column  : Positive;
      End_Line      : Positive;
      End_Column    : Positive;
      Status        : Src_Info.Queries.Find_Decl_Or_Body_Query_Status;

   begin
      if Source = null or else Get_Filename (Source) = "" then
         Console.Insert
           (Kernel, "Cross-references not possible on unamed files!",
            Highlight_Sloc => False);
         return;
      end if;

      Entity_At_Cursor (Source, Line, Column, Entity_Start, Entity_End);

      Source_Info := Locate_From_Source_And_Complete
        (Kernel, Get_Filename (Source));

      --  Abort if we could not locate the associated Source_Info.
      --  Probably means that we either could not locate the ALI file,
      --  or it could also be that we failed to parse it. Either way,
      --  a message should have already been printed. So, just abort.
      if Source_Info = No_LI_File then
         Console.Insert
           (Kernel,
            "Failed to find or parse ALI file " & Get_Filename (Source),
            Highlight_Sloc => False);
         return;
      end if;

      Src_Info.Queries.Find_Declaration_Or_Body
        (Lib_Info        => Source_Info,
         File_Name       => Base_Name (Source.Filename.all),
         Entity_Name     => Get_Text (Entity_Start, Entity_End),
         Line            => To_Box_Line (Get_Line (Entity_Start)),
         Column          => To_Box_Column (Get_Line_Offset (Entity_Start)),
         File_Name_Found => Filename,
         Start_Line      => Start_Line,
         Start_Column    => Start_Column,
         End_Line        => End_Line,
         End_Column      => End_Column,
         Status          => Status);

      case Status is
         when Entity_Not_Found =>
            Console.Insert
              (Kernel, "Cross-reference failed.", Highlight_Sloc => False);
            Free (Filename);
            return;
         when Internal_Error =>
            Console.Insert
              (Kernel, "Cross-reference internal error detected.",
               Highlight_Sloc => False);
            Free (Filename);
            return;
         when No_Body_Entity_Found =>
            Console.Insert
              (Kernel,
               "This entity does not have an associated declaration or body.",
               Highlight_Sloc => False);
            Free (Filename);
            return;
         when Success =>
            null; --  No error message to print
      end case;

      --  Extra safety check, verify that Filename is not null before starting
      --  dereferencing it. That would be a programing error
      if Filename = null then
         Console.Insert
           (Kernel, "Internal error detected.", Highlight_Sloc => False);
         --  Note that the error message is different from the internal error
         --  message above. This will help us pin-pointing the location of
         --  the error message without any doubt, thus helping us debug the
         --  situation, should this happen. (but it won't, of course :-)
         Free (Filename);
         return;
      end if;

      --  Open the file, and reset Source to the new editor in order to
      --  highlight the region returned by the Xref query.
      Open_File_Editor
        (Kernel,
         Find_Source_File (Kernel, Filename.all,
                           Use_Predefined_Source_Path => True),
         Start_Line, Start_Column, False);
      Source := Find_Current_Editor (Kernel);

      --  Abort if we failed to go to the xref location. An error message
      --  has already been printed, so just bail-out.
      if Source /= null then
         --  Get the source box that was just opened/raised, and highlight
         --  the target entity.
         Unhighlight_All (Source);
         Highlight_Region
           (Source, Start_Line, Start_Column, End_Line, End_Column);
      end if;

      Free (Filename);
   end Goto_Declaration_Or_Body;

   ------------------
   -- Draw_Tooltip --
   ------------------

   procedure Draw_Tooltip
     (Widget        : access Source_View_Record'Class;
      Data          : in out Editor_Tooltip_Data;
      Pixmap        : out Gdk.Gdk_Pixmap;
      Width, Height : out Glib.Gint;
      Area          : out Gdk_Rectangle)
   is
      Line, Col    : Natural;
      X, Y         : Gint;
      Win_X, Win_Y : Gint;
      Start_Iter   : Gtk_Text_Iter;
      End_Iter     : Gtk_Text_Iter;
      Mask         : Gdk.Types.Gdk_Modifier_Type;
      Win          : Gdk.Gdk_Window;

   begin
      Width  := 0;
      Height := 0;

      Get_Pointer
        (Get_Window (Widget, Text_Window_Text), Win_X, Win_Y, Mask, Win);
      Window_To_Buffer_Coords (Widget, Text_Window_Text, Win_X, Win_Y, X, Y);
      Get_Iter_At_Location (Widget, Start_Iter, X, Y);
      Search_Entity_Bounds (Data.Box.Source_Buffer, Start_Iter, End_Iter);

      Line := To_Box_Line (Get_Line (Start_Iter));
      Col  := To_Box_Column (Get_Line_Offset (Start_Iter));

      declare
         Entity_Name : constant String := Get_Text (Start_Iter, End_Iter);
      begin
         Trace (Me, "Tooltip on " & Entity_Name);
      end;

      Pixmap := null;
      Area := (0, 0, 0, 0);
      return;
   end Draw_Tooltip;

   -----------------
   -- To_Box_Line --
   -----------------

   function To_Box_Line (Line : Gint) return Natural is
   begin
      return Natural (Line + 1);
   end To_Box_Line;

   -------------------
   -- To_Box_Column --
   -------------------

   function To_Box_Column (Col : Gint) return Natural is
   begin
      return Natural (Col + 1);
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
      Nb_Lines                  : constant Natural :=
        Natural (Get_Line_Count (Box.Source_Buffer));
      Nb_Digits_For_Line_Number : constant Positive :=
        Positive'Max (Number_Of_Digits (Nb_Lines), Min_Line_Number_Width);

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
     (Buffer : access Glib.Object.GObject_Record'Class;
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
      Kernel : Glide_Kernel.Kernel_Handle;
      Source : Source_Buffer := null;
      Lang   : Language.Language_Access)
   is
      Frame          : Gtk_Frame;
      Hbox           : Gtk_Box;
      Frame_Hbox     : Gtk_Box;
      Scrolling_Area : Gtk_Scrolled_Window;
      Label          : Gtk_Label;
      Data           : Editor_Tooltip_Data;

   begin
      Box.Kernel := Kernel;
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

      Data.Box := Source_Editor_Box (Box);
      Editor_Tooltips.New_Tooltip (Box.Source_View, Data, Box.Tooltip);

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
      --  Gtk_New (Box.Filename_Label);
      --  ??? Commented out as not used for the moment.

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

      Widget_Callback.Connect
        (Widget    => Box.Source_Buffer,
         Name      => "cursor_position_changed",
         Cb        => Cursor_Position_Changed_Handler'Access,
         User_Data => Source_Editor_Box (Box),
         After     => True);

      Show_Cursor_Position (Source_Editor_Box (Box), Line => 0, Column => 0);

      --  The Contextual Menu handling
      CMenu.Register_Contextual_Menu
        (Box.Source_View, Source_Editor_Box (Box), Get_Contextual_Menu'Access);
   end Initialize_Box;

   ----------------------
   -- Is_Entity_Letter --
   ----------------------

   function Is_Entity_Letter (Char : Character) return Boolean is
   begin
      --  ??? Does not handle accentuated letters

      case Char is
         when 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Entity_Letter;

   --------------------------
   -- Search_Entity_Bounds --
   --------------------------

   procedure Search_Entity_Bounds
     (Buffer       : access Source_Buffer_Record'Class;
      Start_Iter   : in out Gtk_Text_Iter;
      End_Iter     : out Gtk_Text_Iter)
   is
      Ignored     : Boolean;
   begin
      --  Search forward the end of the entity...
      Copy (Source => Start_Iter, Dest => End_Iter);

      while not Is_End (End_Iter) loop
         exit when not Is_Entity_Letter (Get_Char (End_Iter));
         Forward_Char (End_Iter, Ignored);
      end loop;

      --  And search backward the begining of the entity...
      while not Is_Start (Start_Iter) loop
         Backward_Char (Start_Iter, Ignored);

         if not Is_Entity_Letter (Get_Char (Start_Iter)) then
            Forward_Char (Start_Iter, Ignored);
            exit;
         end if;
      end loop;
   end Search_Entity_Bounds;

   -------------------------
   -- Get_Contextual_Menu --
   -------------------------

   function Get_Contextual_Menu
     (Editor : Source_Editor_Box;
      Event  : Gdk_Event) return Gtk_Menu
   is
      V          : Source_View := Editor.Source_View;
      Line       : Gint;
      Column     : Gint;
      Item       : Gtk_Menu_Item;
      Start_Iter : Gtk_Text_Iter;
      End_Iter   : Gtk_Text_Iter;
   begin
      if Get_Window (Event) = Get_Window (V, Text_Window_Left) then
         if Editor.Left_Contextual_Menu /= null then
            Destroy (Editor.Left_Contextual_Menu);
         end if;

         Gtk_New (Editor.Left_Contextual_Menu);
         Gtk_New (Item, -"Go to line...");
         Add (Editor.Left_Contextual_Menu, Item);
         Gtk_New (Item, -"Go to previous reference");
         Add (Editor.Left_Contextual_Menu, Item);
         Gtk_New (Item, -"Go to file spec/body");
         Add (Editor.Left_Contextual_Menu, Item);
         Gtk_New (Item, -"Go to parent unit");
         Add (Editor.Left_Contextual_Menu, Item);

         return Editor.Left_Contextual_Menu;

      else
         if Editor.Contextual_Menu /= null then
            Destroy (Editor.Contextual_Menu);
         end if;

         Gtk_New (Editor.Contextual_Menu);

         Event_To_Buffer_Coords (Editor.Source_View, Event, Line, Column);
         Editor.Menu_Line_Pos := To_Box_Line (Line);
         Editor.Menu_Col_Pos  := To_Box_Column (Column);

         if Editor.Menu_Line_Pos = 0 or else Editor.Menu_Col_Pos = 0 then
            --  Means that the user did not click on some text. Set Start_Iter
            --  and End_Iter to the same location, so that the slice between
            --  these 2 iterators is the empty string.
            Get_Start_Iter (Editor.Source_Buffer, Start_Iter);
            Get_Start_Iter (Editor.Source_Buffer, End_Iter);
         else
            Get_Iter_At_Line_Offset
              (Editor.Source_Buffer, Start_Iter,
               To_Buffer_Line (Editor.Menu_Line_Pos),
               To_Buffer_Column (Editor.Menu_Col_Pos));
            Search_Entity_Bounds (Editor.Source_Buffer, Start_Iter, End_Iter);
         end if;

         Gtk_New (Item, -"Go to previous reference");
         Add (Editor.Contextual_Menu, Item);

         declare
            Entity_Name : constant String := Get_Text (Start_Iter, End_Iter);
         begin
            Gtk_New (Item, -"Go to declaration/body of " & Entity_Name);
            Add (Editor.Contextual_Menu, Item);
            if Entity_Name'Length = 0 then
               Set_State (Item, State_Insensitive);
            end if;
         end;
         Widget_Callback.Connect
           (Widget    => Item,
            Name      => "activate",
            Cb        => On_Goto_Declaration_Or_Body'Access,
            User_Data => Editor,
            After     => True);

         Gtk_New (Item, -"Go to body");
         Add (Editor.Contextual_Menu, Item);
         Gtk_New (Item, -"List references");
         Add (Editor.Contextual_Menu, Item);
         Gtk_New (Item);
         Add (Editor.Contextual_Menu, Item);
         Gtk_New (Item, -"Go to file spec/body");
         Add (Editor.Contextual_Menu, Item);
         Gtk_New (Item, -"Go to parent unit");
         Add (Editor.Contextual_Menu, Item);
         return Editor.Contextual_Menu;
      end if;
   end Get_Contextual_Menu;

   ---------------------------------
   -- On_Goto_Declaration_Or_Body --
   ---------------------------------

   procedure On_Goto_Declaration_Or_Body
     (Widget : access Glib.Object.GObject_Record'Class;
      Params : Glib.Values.GValues;
      Editor : Source_Editor_Box) is
   begin
      Goto_Declaration_Or_Body
        (Editor.Kernel, Editor.Menu_Line_Pos, Editor.Menu_Col_Pos);
   end On_Goto_Declaration_Or_Body;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Box    : out Source_Editor_Box;
      Kernel : Glide_Kernel.Kernel_Handle;
      Lang   : Language.Language_Access := null) is
   begin
      Box := new Source_Editor_Box_Record;
      Initialize (Box, Kernel, Lang);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Box    : access Source_Editor_Box_Record;
      Kernel : Glide_Kernel.Kernel_Handle;
      Lang   : Language.Language_Access) is
   begin
      Initialize_Box (Box, Kernel, Lang => Lang);
   end Initialize;

   ---------------------
   -- Create_New_View --
   ---------------------

   procedure Create_New_View
     (Box    : out Source_Editor_Box;
      Source : access Source_Editor_Box_Record) is
   begin
      Box := new Source_Editor_Box_Record;
      Initialize_Box
        (Box, Source.Kernel, Source.Source_Buffer, Get_Language (Source));
   end Create_New_View;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Box : in out Source_Editor_Box) is
   begin
      Editor_Tooltips.Destroy_Tooltip (Box.Tooltip);
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

   ----------------
   -- Get_Kernel --
   ----------------

   function Get_Kernel
     (Editor : access Source_Editor_Box_Record)
      return Glide_Kernel.Kernel_Handle is
   begin
      return Editor.Kernel;
   end Get_Kernel;

   ------------------
   -- Set_Filename --
   ------------------

   procedure Set_Filename
     (Editor   : access Source_Editor_Box_Record;
      Filename : String) is
   begin
      Free (Editor.Filename);
      Editor.Filename := new String'(Filename);
   end Set_Filename;

   ------------------
   -- Get_Filename --
   ------------------

   function Get_Filename
     (Editor : access Source_Editor_Box_Record) return String is
   begin
      if Editor.Filename = null then
         return "";
      end if;
      return Editor.Filename.all;
   end Get_Filename;

   ---------------
   -- Load_File --
   ---------------

   procedure Load_File
     (Editor          : access Source_Editor_Box_Record;
      Filename        : String;
      Lang_Autodetect : Boolean := True;
      Success         : out Boolean) is
   begin
      Free (Editor.Filename);
      Load_File (Editor.Source_Buffer, Filename, Lang_Autodetect, Success);
      Set_Cursor_Location (Editor, 1, 1);

      if Success then
         Editor.Filename := new String' (Filename);
         --  ??? Not sure whether we should store the basename or not. For the
         --  moment we store the full path, but this might be an error,
         --  expecially if we change the cwd inside GLIDE. Should not
         --  happen though.
      end if;
   end Load_File;

   ------------------
   -- Save_To_File --
   ------------------

   procedure Save_To_File
     (Editor   : access Source_Editor_Box_Record;
      Filename : String := "";
      Success  : out Boolean) is
   begin
      if Filename = "" then
         if Editor.Filename = null then
            Success := False;
         else
            Save_To_File (Editor.Source_Buffer, Editor.Filename.all, Success);
         end if;
      else
         Save_To_File (Editor.Source_Buffer, Filename, Success);
      end if;
   end Save_To_File;

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

   -----------------------
   -- Is_Valid_Location --
   -----------------------

   function Is_Valid_Location
     (Editor : access Source_Editor_Box_Record;
      Line   : Positive;
      Column : Positive := 1) return Boolean
   is
      Buffer_Line : constant Gint := To_Buffer_Line (Line);
      Buffer_Col  : constant Gint := To_Buffer_Column (Column);
   begin
      return Is_Valid_Position (Editor.Source_Buffer, Buffer_Line, Buffer_Col);
   end Is_Valid_Location;

   -------------------------
   -- Set_Cursor_Location --
   -------------------------

   procedure Set_Cursor_Location
     (Editor  : access Source_Editor_Box_Record;
      Line    : Positive;
      Column  : Positive := 1)
   is
      Buffer_Line  : constant Gint := To_Buffer_Line (Line);
      Buffer_Col   : constant Gint := To_Buffer_Column (Column);
   begin
      Set_Cursor_Position (Editor.Source_Buffer, Buffer_Line, Buffer_Col);
      Scroll_To_Cursor_Location (Editor.Source_View);
   end Set_Cursor_Location;

   -------------------------
   -- Get_Cursor_Location --
   -------------------------

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

   --------------------------
   -- Get_Selection_Bounds --
   --------------------------

   procedure Get_Selection_Bounds
     (Editor       : access Source_Editor_Box_Record;
      Start_Line   : out Positive;
      Start_Column : out Positive;
      End_Line     : out Positive;
      End_Column   : out Positive;
      Found        : out Boolean)
   is
      Start_L : Gint;
      Start_C : Gint;
      End_L   : Gint;
      End_C   : Gint;
   begin
      Get_Selection_Bounds
        (Editor.Source_Buffer, Start_L, Start_C, End_L, End_C, Found);
      Start_Line   := To_Box_Line (Start_L);
      Start_Column := To_Box_Column (Start_C);
      End_Line     := To_Box_Line (End_L);
      End_Column   := To_Box_Column (End_C);
   end Get_Selection_Bounds;

   -------------------
   -- Get_Selection --
   -------------------

   function Get_Selection
     (Editor : access Source_Editor_Box_Record) return String is
   begin
      return Get_Selection (Editor.Source_Buffer);
   end Get_Selection;

   ------------
   -- Search --
   ------------

   procedure Search
     (Editor             : access Source_Editor_Box_Record;
      Pattern            : String;
      Case_Sensitive     : Boolean := True;
      Whole_Word         : Boolean := False;
      Search_Forward     : Boolean := True;
      From_Line          : Positive := 1;
      From_Column        : Positive := 1;
      Found              : out Boolean;
      Match_Start_Line   : out Positive;
      Match_Start_Column : out Positive;
      Match_End_Line     : out Positive;
      Match_End_Column   : out Positive)
   is
      Match_Start_L : Gint;
      Match_Start_C : Gint;
      Match_End_L   : Gint;
      Match_End_C   : Gint;
   begin
      Search (Editor.Source_Buffer,
              Pattern,
              Case_Sensitive, Whole_Word, Search_Forward,
              To_Buffer_Line (From_Line), To_Buffer_Column (From_Column),
              Found,
              Match_Start_L, Match_Start_C, Match_End_L, Match_End_C);
      Match_Start_Line   := To_Box_Line (Match_Start_L);
      Match_Start_Column := To_Box_Column (Match_Start_C);
      Match_End_Line     := To_Box_Line (Match_End_L);
      Match_End_Column   := To_Box_Column (Match_End_C);
   end Search;

   ---------------
   -- Get_Slice --
   ---------------

   function Get_Slice
     (Editor       : access Source_Editor_Box_Record;
      Start_Line   : Positive;
      Start_Column : Positive;
      End_Line     : Positive;
      End_Column   : Positive) return String is
   begin
      return Get_Slice
        (Editor.Source_Buffer,
         To_Buffer_Line (Start_Line), To_Buffer_Column (Start_Column),
         To_Buffer_Line (End_Line), To_Buffer_Column (End_Column));
   end Get_Slice;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Editor  : access Source_Editor_Box_Record;
      Line    : Positive;
      Column  : Positive;
      Text    : String) is
   begin
      Insert
        (Editor.Source_Buffer,
         To_Buffer_Line (Line), To_Buffer_Column (Column),
         Text);
   end Insert;

   -------------------
   -- Replace_Slice --
   -------------------

   procedure Replace_Slice
     (Editor       : access Source_Editor_Box_Record;
      Start_Line   : Positive;
      Start_Column : Positive;
      End_Line     : Positive;
      End_Column   : Positive;
      Text         : String) is
   begin
      Replace_Slice
        (Editor.Source_Buffer,
         To_Buffer_Line (Start_Line), To_Buffer_Column (Start_Column),
         To_Buffer_Line (End_Line), To_Buffer_Column (End_Column),
         Text);
   end Replace_Slice;

   ------------------
   -- Delete_Slice --
   ------------------

   procedure Delete_Slice
     (Editor       : access Source_Editor_Box_Record;
      Start_Line   : Positive;
      Start_Column : Positive;
      End_Line     : Positive;
      End_Column   : Positive) is
   begin
      Delete_Slice
        (Editor.Source_Buffer,
         To_Buffer_Line (Start_Line), To_Buffer_Column (Start_Column),
         To_Buffer_Line (End_Line), To_Buffer_Column (End_Column));
   end Delete_Slice;

   ----------------
   -- Select_All --
   ----------------

   procedure Select_All (Editor : access Source_Editor_Box_Record) is
   begin
      Select_All (Editor.Source_Buffer);
   end Select_All;

   -------------------
   -- Cut_Clipboard --
   -------------------

   procedure Cut_Clipboard (Editor : access Source_Editor_Box_Record) is
   begin
      Cut_Clipboard (Editor.Source_Buffer, Default_Editable => True);
   end Cut_Clipboard;

   --------------------
   -- Copy_Clipboard --
   --------------------

   procedure Copy_Clipboard (Editor : access Source_Editor_Box_Record) is
   begin
      Copy_Clipboard (Editor.Source_Buffer);
   end Copy_Clipboard;

   ---------------------
   -- Paste_Clipboard --
   ---------------------

   procedure Paste_Clipboard (Editor : access Source_Editor_Box_Record) is
   begin
      Paste_Clipboard (Editor.Source_Buffer, Default_Editable => True);
   end Paste_Clipboard;

   --------------------
   -- Highlight_Line --
   --------------------

   procedure Highlight_Line
     (Editor  : access Source_Editor_Box_Record;
      Line    : Positive) is
   begin
      Highlight_Line (Editor.Source_Buffer, To_Buffer_Line (Line));
   end Highlight_Line;

   ----------------------
   -- Unhighlight_Line --
   ----------------------

   procedure Unhighlight_Line
     (Editor  : access Source_Editor_Box_Record;
      Line    : Positive) is
   begin
      Unhighlight_Line (Editor.Source_Buffer, To_Buffer_Line (Line));
   end Unhighlight_Line;

   ---------------------------
   -- Cancel_Highlight_Line --
   ---------------------------

   procedure Cancel_Highlight_Line
     (Editor : access Source_Editor_Box_Record) is
   begin
      Cancel_Highlight_Line (Editor.Source_Buffer);
   end Cancel_Highlight_Line;

   ----------------------
   -- Highlight_Region --
   ----------------------

   procedure Highlight_Region
     (Editor : access Source_Editor_Box_Record;
      Start_Line   : Positive;
      Start_Column : Positive;
      End_Line     : Positive;
      End_Column   : Positive) is
   begin
      Highlight_Region
        (Editor.Source_Buffer,
         To_Buffer_Line (Start_Line), To_Buffer_Column (Start_Column),
         To_Buffer_Line (End_Line), To_Buffer_Column (End_Column));
   end Highlight_Region;

   ------------------------
   -- Unhighlight_Region --
   ------------------------

   procedure Unhighlight_Region
     (Editor : access Source_Editor_Box_Record;
      Start_Line   : Positive;
      Start_Column : Positive;
      End_Line     : Positive;
      End_Column   : Positive) is
   begin
      Unhighlight_Region
        (Editor.Source_Buffer,
         To_Buffer_Line (Start_Line), To_Buffer_Column (Start_Column),
         To_Buffer_Line (End_Line), To_Buffer_Column (End_Column));
   end Unhighlight_Region;

   ---------------------
   -- Unhighlight_All --
   ---------------------

   procedure Unhighlight_All (Editor : access Source_Editor_Box_Record) is
   begin
      Unhighlight_All (Editor.Source_Buffer);
   end Unhighlight_All;

   ---------------------------
   -- Set_Show_Line_Numbers --
   ---------------------------

   procedure Set_Show_Line_Numbers
     (Editor       : access Source_Editor_Box_Record;
      Show_Numbers : Boolean := True) is
   begin
      Set_Show_Line_Numbers (Editor.Source_View, Show_Numbers);
   end Set_Show_Line_Numbers;

   ---------------------------
   -- Get_Show_Line_Numbers --
   ---------------------------

   function Get_Show_Line_Numbers
     (Editor : access Source_Editor_Box_Record) return Boolean is
   begin
      return Get_Show_Line_Numbers (Editor.Source_View);
   end Get_Show_Line_Numbers;

end Src_Editor_Box;
