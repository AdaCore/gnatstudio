-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free software; you can redistribute it and/or modify  it   --
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

with Ada.Exceptions;             use Ada.Exceptions;
with Glib;                       use Glib;
with Glib.Object;                use Glib.Object;
with Glib.Values;
with Glide_Kernel;               use Glide_Kernel;
with Glide_Kernel.Console;       use Glide_Kernel.Console;
with Glide_Kernel.Modules;       use Glide_Kernel.Modules;
with Glide_Kernel.Project;       use Glide_Kernel.Project;
with Glide_Kernel.Preferences;   use Glide_Kernel.Preferences;
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
with Gtk.Widget;                 use Gtk.Widget;
with Gtk.Window;                 use Gtk.Window;
with Gtkada.Dialogs;             use Gtkada.Dialogs;
with GUI_Utils;                  use GUI_Utils;
with Glide_Intl;                 use Glide_Intl;
with GNAT.Directory_Operations;  use GNAT.Directory_Operations;

with Commands.Controls;          use Commands.Controls;
with Language;                   use Language;
with Language_Handlers;          use Language_Handlers;
with Language_Handlers.Glide;    use Language_Handlers.Glide;
with String_Utils;               use String_Utils;
with Src_Editor_Buffer;          use Src_Editor_Buffer;
with Src_Editor_View;            use Src_Editor_View;
with Src_Editor_Module;          use Src_Editor_Module;
with Src_Info;                   use Src_Info;
with Src_Info.Queries;           use Src_Info.Queries;
with Traces;                     use Traces;

package body Src_Editor_Box is

   Me : Debug_Handle := Create ("Source_Editor");

   package Box_Callback is new Gtk.Handlers.User_Callback
     (Widget_Type => Glib.Object.GObject_Record,
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

   procedure On_Box_Destroy
     (Object : access Glib.Object.GObject_Record'Class;
      Params : Glib.Values.GValues;
      Box    : Source_Editor_Box);
   --  Callback for the "destroy" signal.

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

   function Focus_In (Box : access GObject_Record'Class) return Boolean;
   --  Callback for the focus_in event. This checks whether the physical file
   --  on the disk is more recent than the one that was read for the editor.

   function Focus_Out (Box : access GObject_Record'Class) return Boolean;
   --  Callback for the focus_out event.

   ----------------------------------
   -- The contextual menu handling --
   ----------------------------------

   package CMenu is new GUI_Utils.User_Contextual_Menus
     (User_Data => Source_Editor_Box);
   --  Used to register contextual menus with a user data.

   procedure On_Goto_Declaration
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Callback for the "Goto Declaration" contextual menu

   procedure On_Goto_Next_Body
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Callback for the "Goto Body" contextual menu

   procedure On_Goto_Other_File
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Callback for the "Goto spec <-> body" contextual menu

   procedure Entity_At_Cursor
     (Editor                   : access Source_Editor_Box_Record;
      Line, Column             : Natural := 0;
      Entity_Start, Entity_End : out Gtk_Text_Iter;
      Success                  : out Boolean);
   --  Return the beginning position of the entity at (Line, Column), or under
   --  the cursor if Line or Column is 0.
   --  The entity itself can be read through a call to Get_Text.
   --  Set Success to False if Line/Column is an invalid position.

   ----------------------
   -- Entity_At_Cursor --
   ----------------------

   procedure Entity_At_Cursor
     (Editor                   : access Source_Editor_Box_Record;
      Line, Column             : Natural := 0;
      Entity_Start, Entity_End : out Gtk_Text_Iter;
      Success                  : out Boolean)
   is
      Tmp_Line : Natural := Line;
      Tmp_Col  : Natural := Column;

   begin
      --  Get the cursor position if either Line or Column is equal to 0

      if Tmp_Line = 0 or else Tmp_Col = 0 then
         Get_Cursor_Location (Editor, Tmp_Line, Tmp_Col);

      elsif not Is_Valid_Location (Editor, Line, Column) then
         --  This should never happen at this point. It probably means that
         --  Editor does not match the (Line, Column) location; exit before
         --  confusing Gtk+

         Trace (Me, "Invalid location: " & Get_Filename (Editor) & ':' &
                Image (Line) & ':' & Image (Column));
         Success := False;
         return;
      end if;

      --  Transform this position into an iterator, and then locate the
      --  bounds of the entity...

      Get_Iter_At_Line_Offset
        (Editor.Source_Buffer, Entity_Start,
         To_Buffer_Line (Tmp_Line), To_Buffer_Column (Tmp_Col));
      Search_Entity_Bounds (Editor.Source_Buffer, Entity_Start, Entity_End);
      Success := True;
   end Entity_At_Cursor;

   ------------------------------
   -- Goto_Declaration_Or_Body --
   ------------------------------

   procedure Goto_Declaration_Or_Body
     (Kernel : access Kernel_Handle_Record'Class;
      To_Body : Boolean;
      Editor : Source_Editor_Box := null;
      Line   : Natural := 0;
      Column : Natural := 0)
   is
      Source         : Source_Editor_Box := Editor;
      Source_Info    : LI_File_Ptr;
      Entity_Start   : Gtk_Text_Iter;
      Entity_End     : Gtk_Text_Iter;
      Status         : Src_Info.Queries.Find_Decl_Or_Body_Query_Status;
      Entity         : Entity_Information;
      Location       : File_Location;
      Entity_Success : Boolean;
      L, C           : Natural;

   begin
      Push_State (Kernel_Handle (Kernel), Busy);

      if Source = null then
         Source := Find_Current_Editor (Kernel);
      end if;

      if Source = null or else Get_Filename (Source) = "" then
         Console.Insert
           (Kernel, -"Cross-references not possible on unamed files!",
            Highlight_Sloc => False);
         Pop_State (Kernel_Handle (Kernel));
         return;
      end if;

      Entity_At_Cursor
        (Source, Line, Column, Entity_Start, Entity_End, Entity_Success);

      if not Entity_Success then
         Console.Insert
           (Kernel,
            -"Failed to find an entity for file " & Get_Filename (Source),
            Highlight_Sloc => False);
         Pop_State (Kernel_Handle (Kernel));
         return;
      end if;

      Source_Info := Locate_From_Source_And_Complete
        (Kernel, Get_Filename (Source));

      --  Abort if we could not locate the associated Source_Info.
      --  Probably means that we either could not locate the ALI file,
      --  or it could also be that we failed to parse it. Either way,
      --  a message should have already been printed. So, just abort.

      if Source_Info = No_LI_File then
         Console.Insert
           (Kernel,
            -"Failed to find or parse ALI file " & Get_Filename (Source),
            Highlight_Sloc => False);
         Pop_State (Kernel_Handle (Kernel));
         return;
      end if;

      declare
         Entity_Name : constant String := Get_Text (Entity_Start, Entity_End);
      begin
         if To_Body then
            Find_Next_Body
              (Kernel             => Kernel,
               Lib_Info           => Source_Info,
               File_Name          => Base_Name (Get_Filename (Source)),
               Entity_Name        => Entity_Name,
               Line               => To_Box_Line (Get_Line (Entity_Start)),
               Column             =>
                 To_Box_Column (Get_Line_Offset (Entity_Start)),
               Location           => Location,
               Status             => Status);
         else
            Src_Info.Queries.Find_Declaration
              (Lib_Info           => Source_Info,
               File_Name          => Base_Name (Get_Filename (Source)),
               Entity_Name        => Entity_Name,
               Line               => To_Box_Line (Get_Line (Entity_Start)),
               Column             =>
                 To_Box_Column (Get_Line_Offset (Entity_Start)),
               Entity             => Entity,
               Status             => Status);
         end if;

         case Status is
            when Entity_Not_Found =>
               Console.Insert
                 (Kernel, -"Cross-reference failed.", Highlight_Sloc => False);
               Pop_State (Kernel_Handle (Kernel));
               return;
            when Internal_Error =>
               Console.Insert
                 (Kernel, -"Cross-reference internal error detected.",
                  Highlight_Sloc => False);
               Pop_State (Kernel_Handle (Kernel));
               return;
            when No_Body_Entity_Found =>
               Console.Insert
                 (Kernel,
                  (-"This entity does not have an associated ")
                  & (-"declaration or body."),
                  Highlight_Sloc => False);
               Pop_State (Kernel_Handle (Kernel));
               return;
            when Success =>
               null; --  No error message to print
         end case;

         if To_Body then
            --  Open the file, and reset Source to the new editor in order to
            --  highlight the region returned by the Xref query.

            L := Get_Line (Location);
            C := Get_Column (Location);

            Open_File_Editor
              (Kernel,
               Find_Source_File
                 (Kernel, Get_File (Location),
                  Use_Predefined_Source_Path => True),
               L, C, False);


         else
            --  Open the file, and reset Source to the new editor in order to
            --  highlight the region returned by the Xref query.

            L := Get_Declaration_Line_Of (Entity);
            C := Get_Declaration_Column_Of (Entity);

            Open_File_Editor
              (Kernel,
               Find_Source_File
                 (Kernel, Get_Declaration_File_Of (Entity),
                  Use_Predefined_Source_Path => True),
               L, C, False);

            Destroy (Entity);
         end if;

         Source := Find_Current_Editor (Kernel);

         --  Abort if we failed to go to the xref location. An error message
         --  has already been printed, so just bail-out.

         if Source /= null then
            --  Get the source box that was just opened/raised, and highlight
            --  the target entity.

            Unhighlight_All (Source);

            if not Is_Valid_Location (Source, L, C + Entity_Name'Length) then
               --  This should never happen. If it does, Source probably does
               --  not correspond to the right editor.

               Console.Insert
                 (Kernel,
                  -"Internal error, invalid location: " &
                  Get_Filename (Source) & ':' &
                  Image (L) & ':' & Image (C + Entity_Name'Length),
                  Highlight_Sloc => False);
               return;
            end if;

            Highlight_Region
              (Source, L, C, L, C + Entity_Name'Length);
         end if;
      end;

      Pop_State (Kernel_Handle (Kernel));

   exception
      when E : Unsupported_Language =>
         Insert (Kernel, Exception_Message (E),
                 Mode => Glide_Kernel.Console.Error);

      when E : others =>
         Pop_State (Kernel_Handle (Kernel));
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
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
      Line, Col    : Gint;
      Win_X, Win_Y : Gint;
      Start_Iter   : Gtk_Text_Iter;
      End_Iter     : Gtk_Text_Iter;
      Mask         : Gdk.Types.Gdk_Modifier_Type;
      Win          : Gdk.Gdk_Window;

   begin
      Width  := 0;
      Height := 0;
      Pixmap := null;
      Area   := (0, 0, 0, 0);

      Get_Pointer
        (Get_Window (Widget, Text_Window_Text), Win_X, Win_Y, Mask, Win);
      Window_To_Buffer_Coords (Widget, Win_X, Win_Y, Line, Col);

      if Line = -1 or else Col = -1 then
         --  Invalid position: the cursor is outside the text, do not
         --  display a tooltip.

         return;

      else
         Get_Iter_At_Line_Offset
           (Data.Box.Source_Buffer, Start_Iter, Line, Col);
         Search_Entity_Bounds
           (Data.Box.Source_Buffer, Start_Iter, End_Iter);
      end if;

      declare
         Entity_Name : constant String := Get_Text (Start_Iter, End_Iter);
      begin
         Trace (Me, "Tooltip on " & Entity_Name);
      end;

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
      Modified_Buffer           : constant Boolean :=
        Get_Modified (Box.Source_Buffer);

   begin
      if Modified_Buffer /= Box.Modified then
         if Modified_Buffer then
            Set_Text (Box.Modified_Label, -"Modified");
         else
            Set_Text (Box.Modified_Label, -"Unmodified");
         end if;

         Box.Modified := Modified_Buffer;
      end if;

      --  In the source buffer, the Line and Column indexes start from
      --  0. It is more natural to start from one, so the Line and Column
      --  number displayed are incremented by 1 to start from 1.

      Set_Text
        (Box.Cursor_Loc_Label,
         Image (To_Box_Line (Line)) & ':' & Image (To_Box_Column (Column)));
   end Show_Cursor_Position;

   -------------------------------------
   -- Cursor_Position_Changed_Handler --
   -------------------------------------

   procedure Cursor_Position_Changed_Handler
     (Buffer : access Glib.Object.GObject_Record'Class;
      Params : Glib.Values.GValues;
      Box    : Source_Editor_Box)
   is
      pragma Unreferenced (Buffer);

      Line : constant Gint := Values.Get_Int (Values.Nth (Params, 1));
      Col  : constant Gint := Values.Get_Int (Values.Nth (Params, 2));
   begin
      Show_Cursor_Position (Box, Line => Line, Column => Col);
   end Cursor_Position_Changed_Handler;

   --------------------
   -- On_Box_Destroy --
   --------------------

   procedure On_Box_Destroy
     (Object : access Glib.Object.GObject_Record'Class;
      Params : Glib.Values.GValues;
      Box    : Source_Editor_Box)
   is
      pragma Unreferenced (Object, Params);

      Success : Boolean;
   begin
      Editor_Tooltips.Destroy_Tooltip (Box.Tooltip);

      if Box.Modified then
         if Message_Dialog
             (Msg            => (-"File ") & Get_Filename (Box) &
                                (-" modified.") & ASCII.LF &
                                (-"Do you want to save it ?"),
              Dialog_Type    => Confirmation,
              Buttons        => Button_Yes or Button_No,
              Default_Button => Button_Yes,
              Parent         => Get_Main_Window (Box.Kernel)) =
           Button_Yes
         then
            Save_To_File (Box, Success => Success);
         end if;
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Box_Destroy;

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
      Scrolling_Area : Gtk_Scrolled_Window;
      Data           : Editor_Tooltip_Data;

   begin
      Glib.Object.Initialize (Box);
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

      Gtk_New (Box.Source_View, Box.Source_Buffer,
               Get_Pref (Kernel, Default_Source_Editor_Font),
               Show_Line_Numbers => True);
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

      --  Read only file area...
      Gtk_New (Frame);
      Set_Shadow_Type (Frame, Shadow_In);
      Pack_Start (Hbox, Frame, Expand => False, Fill => True);
      Gtk_New (Box.Read_Only_Label);
      Add (Frame, Box.Read_Only_Label);

      --  Modified file area...
      Gtk_New (Frame);
      Set_Shadow_Type (Frame, Shadow_In);
      Pack_Start (Hbox, Frame, Expand => False, Fill => True);
      Gtk_New (Box.Modified_Label);
      Add (Frame, Box.Modified_Label);

      --  Line:Column number area...
      Gtk_New (Frame);
      Set_Shadow_Type (Frame, Shadow_In);
      Pack_Start (Hbox, Frame, Expand => False, Fill => True);
      Gtk_New (Box.Cursor_Loc_Label, "1:1");
      Add (Frame, Box.Cursor_Loc_Label);

      Box_Callback.Connect
        (Box.Source_Buffer,
         "cursor_position_changed",
         Cursor_Position_Changed_Handler'Access,
         User_Data => Source_Editor_Box (Box),
         After     => True);

      Box_Callback.Connect
        (Box.Source_View,
         "destroy",
         On_Box_Destroy'Access,
         User_Data => Source_Editor_Box (Box));

      Show_Cursor_Position (Source_Editor_Box (Box), Line => 0, Column => 0);

      Add_Events (Box.Source_View, Focus_Change_Mask);
      Object_Return_Callback.Object_Connect
        (Box.Source_View, "focus_in_event",
         Object_Return_Callback.To_Marshaller (Focus_In'Access), Box, False);
      Object_Return_Callback.Object_Connect
        (Box.Source_View, "focus_out_event",
         Object_Return_Callback.To_Marshaller (Focus_Out'Access), Box, False);

      --  The Contextual Menu handling
      Register_Contextual_Menu
        (Kernel          => Kernel,
         Event_On_Widget => Box.Source_View,
         Object          => Box,
         ID              => Src_Editor_Module_Id,
         Context_Func    => Get_Contextual_Menu'Access);
   end Initialize_Box;

   --------------
   -- Focus_In --
   --------------

   function Focus_In (Box : access GObject_Record'Class) return Boolean is
      B             : Source_Editor_Box := Source_Editor_Box (Box);
      New_Timestamp : Timestamp;
      Undo_Redo     : Undo_Redo_Information;
   begin
      if B.Filename /= null then
         New_Timestamp := To_Timestamp (File_Time_Stamp (B.Filename.all));

         if New_Timestamp > B.Timestamp then
            if Message_Dialog
              (Msg         => Base_Name (B.Filename.all)
               & (-" changed on disk. Really edit ?"),
               Dialog_Type => Confirmation,
               Buttons     => Button_Yes or Button_No,
               Title       => -"File changed on disk",
               Parent      => Get_Main_Window (B.Kernel)) /= Button_Yes
            then
               --  ??? Should move focus in some other widget, to prevent
               --  edition.
               return False;
            end if;
            B.Timestamp := New_Timestamp;
         end if;
      end if;

      Undo_Redo := Undo_Redo_Data.Get (B.Kernel, Undo_Redo_Id);

      Set_Controls (Get_Queue (B.Source_Buffer),
                    Undo_Redo.Undo_Button,
                    Undo_Redo.Redo_Button,
                    Undo_Redo.Undo_Menu_Item,
                    Undo_Redo.Redo_Menu_Item);
      return False;
   end Focus_In;

   ---------------
   -- Focus_Out --
   ---------------

   function Focus_Out (Box : access GObject_Record'Class) return Boolean is
      B         : Source_Editor_Box := Source_Editor_Box (Box);
   begin
      Unset_Controls (Get_Queue (B.Source_Buffer));

      return False;
   end Focus_Out;

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
      pragma Unreferenced (Buffer);

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
     (Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu) return Selection_Context_Access
   is
      pragma Unreferenced (Event_Widget);

      Editor     : Source_Editor_Box := Source_Editor_Box (Object);
      V          : Source_View := Editor.Source_View;
      Line       : Gint;
      Column     : Gint;
      X, Y       : Gint;
      L, C       : Integer;
      Item       : Gtk_Menu_Item;
      Start_Iter : Gtk_Text_Iter;
      End_Iter   : Gtk_Text_Iter;
      Context    : Entity_Selection_Context_Access;

   begin
      Context := new Entity_Selection_Context;
      Set_Context_Information
        (Context => Context,
         Kernel  => Kernel,
         Creator => Src_Editor_Module_Id);
      Set_File_Information
        (Context,
         Directory => Dir_Name (Editor.Filename.all),
         File_Name => Base_Name (Editor.Filename.all));

      if Event /= null
        and then Get_Window (Event) = Get_Window (V, Text_Window_Left)
      then
         Window_To_Buffer_Coords
           (Editor.Source_View, Text_Window_Left,
            Gint (Get_X (Event)), Gint (Get_Y (Event)),
            X, Y);
         Get_Iter_At_Location (Editor.Source_View, Start_Iter, X, Y);
         Line := Get_Line (Start_Iter);
         Set_Entity_Information (Context, Line => To_Box_Line (Line));

         if Menu /= null then
            Gtk_New (Item, -"Go to line...");
            Add (Menu, Item);
            Set_Sensitive (Item, False);

            Gtk_New (Item, -"Go to previous reference");
            Add (Menu, Item);
            Set_Sensitive (Item, False);

            Gtk_New (Item, -"Go to file spec/body");
            Add (Menu, Item);
            Context_Callback.Object_Connect
              (Item, "activate",
               Context_Callback.To_Marshaller (On_Goto_Other_File'Access),
               User_Data   => Selection_Context_Access (Context),
               Slot_Object => Editor,
               After       => True);

            Gtk_New (Item, -"Go to parent unit");
            Add (Menu, Item);
            Set_Sensitive (Item, False);
         end if;

      else
         if Event = null then
            Get_Iter_At_Mark (Editor.Source_Buffer, Start_Iter,
                              Get_Insert (Editor.Source_Buffer));
            Line   := Get_Line (Start_Iter);
            Column := Get_Line_Offset (Start_Iter);

         else
            Event_To_Buffer_Coords (Editor.Source_View, Event, Line, Column);
         end if;

         L := To_Box_Line (Line);
         C := To_Box_Column (Column);

         if L = 0 or else C = 0 then
            --  Invalid position: the cursor is outside the text.

            Get_Start_Iter (Editor.Source_Buffer, Start_Iter);
            Get_Start_Iter (Editor.Source_Buffer, End_Iter);

            if L /= 0 then
               Set_Entity_Information
                 (Context, Line => Integer (Get_Line (Start_Iter)) + 1);
            end if;
         else
            Get_Iter_At_Line_Offset
              (Editor.Source_Buffer, Start_Iter,
               To_Buffer_Line (L), To_Buffer_Column (C));
            Search_Entity_Bounds (Editor.Source_Buffer, Start_Iter, End_Iter);
            Set_Entity_Information
              (Context, Get_Text (Start_Iter, End_Iter),
               Integer (Get_Line (Start_Iter)) + 1,
               Integer (Get_Line_Offset (Start_Iter)) + 1);

            --  Get the focus now. If we let the contextual menu handler in the
            --  kernel do the grab focus, then the source window will be
            --  scrolled up.
            Grab_Focus (Editor.Source_View);
            Place_Cursor (Editor.Source_Buffer, Start_Iter);
         end if;

         if Menu /= null then
            Gtk_New (Item, -"Go to previous reference");
            Add (Menu, Item);

            if Has_Entity_Name_Information (Context) then
               Gtk_New (Item, -"Go to declaration of "
                          & Entity_Name_Information (Context));
               Add (Menu, Item);
               Context_Callback.Object_Connect
                 (Item, "activate",
                  Context_Callback.To_Marshaller
                    (On_Goto_Declaration'Access),
                  User_Data   => Selection_Context_Access (Context),
                  Slot_Object => Editor,
                  After       => True);

               Gtk_New (Item, -"Go to body of "
                          & Entity_Name_Information (Context));
               Add (Menu, Item);
               Context_Callback.Object_Connect
                 (Item, "activate",
                  Context_Callback.To_Marshaller
                    (On_Goto_Next_Body'Access),
                  User_Data   => Selection_Context_Access (Context),
                  Slot_Object => Editor,
                  After       => True);
            end if;

            Gtk_New (Item, -"Go to file spec/body");
            Add (Menu, Item);
            Context_Callback.Object_Connect
              (Item, "activate",
               Context_Callback.To_Marshaller (On_Goto_Other_File'Access),
               User_Data   => Selection_Context_Access (Context),
               Slot_Object => Editor,
               After       => True);

            Gtk_New (Item, -"Go to parent unit");
            Add (Menu, Item);
            Set_Sensitive (Item, False);
         end if;
      end if;

      return Selection_Context_Access (Context);
   end Get_Contextual_Menu;

   ------------------------
   -- On_Goto_Other_File --
   ------------------------

   procedure On_Goto_Other_File
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
      C      : constant Entity_Selection_Context_Access :=
        Entity_Selection_Context_Access (Context);
      Kernel : constant Kernel_Handle := Get_Kernel (Context);

   begin
      Push_State (Kernel, Busy);

      if Has_File_Information (C) then
         declare
            Other_File : constant String := Get_Other_File_Of
              (Kernel, File_Information (C));
         begin
            if Other_File /= "" then
               Open_File_Editor (Kernel, Other_File);
            end if;
         end;
      end if;

      Pop_State (Kernel);

   exception
      when E : others =>
         Pop_State (Kernel);
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Goto_Other_File;

   -------------------------
   -- On_Goto_Declaration --
   -------------------------

   procedure On_Goto_Declaration
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      C      : constant Entity_Selection_Context_Access :=
        Entity_Selection_Context_Access (Context);
      Kernel : constant Kernel_Handle := Get_Kernel (Context);
   begin
      Goto_Declaration_Or_Body
        (Kernel,
         To_Body => False,
         Editor  => Source_Editor_Box (Widget),
         Line    => Line_Information (C),
         Column  => Column_Information (C));
   end On_Goto_Declaration;

   -----------------------
   -- On_Goto_Next_Body --
   -----------------------

   procedure On_Goto_Next_Body
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      C      : constant Entity_Selection_Context_Access :=
        Entity_Selection_Context_Access (Context);
      Kernel : constant Kernel_Handle := Get_Kernel (Context);
   begin
      Goto_Declaration_Or_Body
        (Kernel,
         To_Body => True,
         Editor  => Source_Editor_Box (Widget),
         Line    => Line_Information (C),
         Column  => Column_Information (C));
   end On_Goto_Next_Body;

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
      Kernel : access Kernel_Handle_Record'Class;
      Source : access Source_Editor_Box_Record) is
   begin
      Box := new Source_Editor_Box_Record;
      Initialize_Box
        (Box, Kernel_Handle (Kernel), Source.Source_Buffer,
         Get_Language (Source));
      Box.Writable := Source.Writable;
      Box.Modified := Source.Modified;

      if Source.Filename /= null then
         Box.Filename := new String' (Source.Filename.all);
      end if;

      Set_Text (Box.Modified_Label, Get_Text (Source.Modified_Label));

      if Box.Writable then
         Set_Text (Box.Read_Only_Label, -"Writable");
      else
         Set_Text (Box.Read_Only_Label, -"Read Only");
         Set_Editable (Box.Source_View, False);
      end if;
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
      Lang_Handler    : Language_Handlers.Language_Handler;
      Lang_Autodetect : Boolean := True;
      Success         : out Boolean) is
   begin
      Free (Editor.Filename);
      Load_File (Editor.Source_Buffer, Filename, Lang_Handler,
                 Lang_Autodetect, Success);
      Set_Cursor_Location (Editor, 1, 1);

      if Success then
         Editor.Filename := new String' (Filename);
         Editor.Timestamp := To_Timestamp (File_Time_Stamp (Filename));
         Set_Text (Editor.Modified_Label, -"Unmodified");
         Editor.Modified := False;
         Editor.Writable := Is_Writable_File (Filename);

         if Editor.Writable then
            Set_Text (Editor.Read_Only_Label, -"Writable");
         else
            Set_Text (Editor.Read_Only_Label, -"Read Only");
            Set_Editable (Editor.Source_View, False);
         end if;
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
      if not Editor.Writable then
         Success := False;
         return;
      end if;

      Success := True;

      if Filename = "" then
         if Editor.Filename = null then
            Success := False;
         else
            if To_Timestamp (File_Time_Stamp (Editor.Filename.all)) >
              Editor.Timestamp
              and then Message_Dialog
                (Msg => Base_Name (Editor.Filename.all)
                        & (-" changed on disk. Do you want to overwrite ?"),
                 Dialog_Type => Confirmation,
                 Buttons => Button_OK or Button_Cancel,
                 Title => -"File changed on disk",
                 Parent => Get_Main_Window (Editor.Kernel)) /= Button_OK
            then
               Success := False;
            else
               Save_To_File
                 (Editor.Source_Buffer, Editor.Filename.all, Success);
            end if;
         end if;

      else
         if Is_Regular_File (Filename) then
            if Message_Dialog
              (Msg => Base_Name (Filename)
               & (-" already exists. Do you want to overwrite ?"),
               Dialog_Type => Confirmation,
               Buttons => Button_OK or Button_Cancel,
               Title => "Confirm overwriting",
               Parent => Get_Main_Window (Editor.Kernel)) /= Button_OK
            then
               Success := False;
            end if;
         end if;

         if Success then
            Save_To_File (Editor.Source_Buffer, Filename, Success);
            Free (Editor.Filename);
            Editor.Filename := new String' (Filename);
         end if;
      end if;

      if Success then
         Set_Text (Editor.Modified_Label, -"Saved");
         Editor.Modified := False;
         Editor.Timestamp := To_Timestamp
           (File_Time_Stamp (Editor.Filename.all));
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
