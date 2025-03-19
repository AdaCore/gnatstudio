------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2003-2023, AdaCore                     --
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

with Gdk.Rectangle;

with Gtk.Adjustment;                  use Gtk.Adjustment;
with Gtk.Enums;                       use Gtk.Enums;
with Gtk.Scrolled_Window;             use Gtk.Scrolled_Window;
with Gtk.Text_Iter;                   use Gtk.Text_Iter;
with Gtkada.MDI;                      use Gtkada.MDI;

with Basic_Types;                     use Basic_Types;
with Commands;                        use Commands;
with Completion_Module;               use Completion_Module;
with GPS.Kernel;                      use GPS.Kernel;
with GPS.Kernel.Clipboard;            use GPS.Kernel.Clipboard;
with GPS.Kernel.MDI;                  use GPS.Kernel.MDI;
with GPS.Kernel.Modules;              use GPS.Kernel.Modules;
with GUI_Utils;                       use GUI_Utils;
with Language;                        use Language;
with Src_Editor_Buffer;               use Src_Editor_Buffer;
with Src_Editor_Buffer.Text_Handling; use Src_Editor_Buffer.Text_Handling;
with Src_Editor_Buffer.Cursors;       use Src_Editor_Buffer.Cursors;
with Src_Editor_Box;                  use Src_Editor_Box;
with Src_Editor_Module;               use Src_Editor_Module;
with Tooltips;                        use Tooltips;

package body Src_Editor_View.Commands is

   Locked_MDI_Group : constant Child_Group := 667;
   --  MDI group for all locked editors.
   --  See Gtkada.MDI for other MDI groups.

   procedure Move_Iter
     (Iter         : in out Gtk_Text_Iter;
      Kind         : Movement_Type;
      Step         : Integer;
      Horiz_Offset : Gint := -1);
   --  Move the iterator according to Kind. Kind should be different from page

   procedure Skip_Whitespace
     (Iter    : in out Gtk_Text_Iter;
      Forward : Boolean);
   --  Skip whitespaces (space and tabs). Stops at the first non-whitespace or
   --  newline character. Does nothing if the iterator is already next to the
   --  first non-whitespace.

   ---------------------
   -- Skip_Whitespace --
   ---------------------

   procedure Skip_Whitespace
     (Iter    : in out Gtk_Text_Iter;
      Forward : Boolean)
   is
      Success : Boolean;
      G       : Gunichar;
   begin
      if Forward then
         loop
            G := Get_Char (Iter);
            exit when G /= Character'Pos (' ')
              and then G /= Character'Pos (ASCII.HT);

            Forward_Char (Iter, Success);
            exit when not Success;
         end loop;

      else
         loop
            Backward_Char (Iter, Success);
            exit when not Success;

            G := Get_Char (Iter);
            exit when G /= Character'Pos (' ')
              and then G /= Character'Pos (ASCII.HT);
         end loop;

         if Success then
            Forward_Char (Iter, Success);
         end if;
      end if;
   end Skip_Whitespace;

   ---------------
   -- Move_Iter --
   ---------------

   procedure Move_Iter
     (Iter         : in out Gtk_Text_Iter;
      Kind         : Movement_Type;
      Step         : Integer;
      Horiz_Offset : Gint := -1)
   is
      Ignored, Success : Boolean;
      Offset           : Gint;
   begin
      case Kind is
         when Word =>
            if Step > 0 then
               Forward_Visible_Word_Ends (Iter, Gint (Step), Ignored);
            else
               Backward_Visible_Word_Starts (Iter, -Gint (Step), Ignored);
            end if;

         when Paragraph =>
            if Step > 0 then
               Move_Paragraph_Forward :
               for J in 1 .. Step loop
                  loop
                     --  to start of next line
                     Forward_Visible_Line (Iter, Success);

                     exit Move_Paragraph_Forward when not Success;

                     --  We are at beginning of line, skip spaces
                     --  If at end of line, we found a new paragraph
                     Skip_Whitespace (Iter, Forward => True);
                     exit when Ends_Line (Iter);
                  end loop;
               end loop Move_Paragraph_Forward;

            else
               Move_Paragraph_Backward :
               for J in 1 .. abs Step loop
                  loop
                     Set_Line_Index (Iter, 0);
                     Backward_Char (Iter, Success); --  to end of previous line
                     exit Move_Paragraph_Backward when not Success;

                     --  We are at beginning of line, skip spaces
                     Skip_Whitespace (Iter, Forward => False);
                     exit when Starts_Line (Iter);
                  end loop;
               end loop Move_Paragraph_Backward;
            end if;

         when Char =>
            if Step > 0 then
               Forward_Visible_Cursor_Positions (Iter, Gint (Step), Ignored);
            else
               Backward_Visible_Cursor_Positions (Iter, -Gint (Step), Ignored);
            end if;

         when Line =>
            Offset := Horiz_Offset;
            if Step > 0 then
               Forward_Visible_Lines (Iter, Gint (Step), Ignored);
               Forward_Visible_Cursor_Positions
                 (Iter, Gint'Min
                    (Offset, Get_Chars_In_Line (Iter) - 1), Ignored);
            else
               if Get_Line (Iter) = 0 then
                  Set_Line_Offset (Iter, 0);
                  return;
               end if;
               Backward_Visible_Lines (Iter, -Gint (Step), Ignored);
               Forward_Cursor_Positions
                 (Iter, Gint'Min
                    (Offset, Get_Chars_In_Line (Iter) - 1), Ignored);
            end if;

         when Page =>
            --  The page case is never handled by move iter
            raise Program_Error with "Should not be here";
      end case;
   end Move_Iter;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Move_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel        : constant Kernel_Handle :=
                       Get_Kernel (Src_Editor_Module_Id.all);
      Editor        : constant MDI_Child := Find_Current_Editor (Kernel);
      Source_Box    : constant Source_Editor_Box :=
                       Get_Source_Box_From_MDI (Editor);
      View          : constant Source_View := Source_Box.Get_View;
      Buffer        : constant Source_Buffer := Get_Buffer (Source_Box);
      Iter          : Gtk_Text_Iter;
      Saved_Mark    : constant Gtk_Text_Mark := View.Saved_Cursor_Mark;
      Scrolled      : Gtk_Scrolled_Window;
      Adj           : Gtk_Adjustment;
      Moved         : Boolean;
      C             : constant Src_Editor_Buffer.Cursors.Cursor
        := Get_Main_Cursor (Buffer);
      Column        : constant Gint :=
        Get_Column_Memory (Get_Main_Cursor (Buffer));

      Extend_Selection : constant Boolean :=
        Buffer.Should_Extend_Selection (Command.Extend_Selection);

      Should_Scroll : Boolean := True;
      List_Cursors  : Src_Editor_Buffer.Cursors.Cursors_Lists.List;
      Iter_Line     : Buffer_Line_Type;

      pragma Unreferenced (Context, Moved);
   begin
      Remove_Completion;
      Hide_Tooltip;
      Set_Manual_Sync (C);

      if Command.Kind = Page then
         Scrolled := Gtk_Scrolled_Window (Get_Parent (View));
         Adj      := Get_Vadjustment (Scrolled);
         Adj.Set_Value
           (Adj.Get_Value + Gdouble (Command.Step) * Adj.Get_Page_Increment);

         if Extend_Selection then
            Moved := Move_Mark_Onscreen (View, Buffer.Get_Insert);
         else
            Moved := Place_Cursor_Onscreen (View)
              or Move_Mark_Onscreen (View, Saved_Mark);
         end if;

      else
         List_Cursors := Get_Cursors (Buffer);
         for Cursor of List_Cursors loop
            declare
               Cursor_Mark  : constant Gtk_Text_Mark := Get_Mark (Cursor);
               Sel_Mark     : Gtk_Text_Mark;
               Horiz_Offset : constant Gint := Get_Column_Memory (Cursor);
               Prevent      : Boolean := False;
               Old_Iter     : Gtk_Text_Iter;
            begin
               Buffer.Get_Iter_At_Mark (Iter, Cursor_Mark);
               Old_Iter := Iter;
               Move_Iter (Iter, Command.Kind, Command.Step,
                          Get_Column_Memory (Cursor));
               Iter_Line := Buffer_Line_Type (Get_Line (Iter));

               if Command.Kind /= Line
                 and then not Cursor.Is_Main_Cursor
                 and then Gint (Iter_Line) /= Get_Line (Old_Iter)
               then
                  Prevent := True;
               end if;

               if not Prevent then
                  if Cursor.Is_Main_Cursor then
                     if not Extend_Selection then
                        Buffer.Place_Cursor (Iter);
                     else
                        Buffer.Move_Mark (Cursor_Mark, Iter);
                     end if;

                     --  Don't Scroll if the main cursor is still in the
                     --  visible rect
                     declare
                        Visible_Rect : Gdk.Rectangle.Gdk_Rectangle;
                        Cursor_Rect  : Gdk.Rectangle.Gdk_Rectangle;
                        Inter_Rect   : Gdk.Rectangle.Gdk_Rectangle;
                        Do_Intersect : Boolean;
                     begin
                        Get_Visible_Rect (View, Visible_Rect);
                        Get_Iter_Location (View, Iter, Cursor_Rect);
                        --  Can't intersect when the width is too small
                        if Cursor_Rect.Width <= 1 then
                           Cursor_Rect.Width := 1;
                        end if;

                        Gdk.Rectangle.Intersect
                          (Src1      => Visible_Rect,
                           Src2      => Cursor_Rect,
                           Dest      => Inter_Rect,
                           Intersect => Do_Intersect);

                        Should_Scroll :=
                          not Do_Intersect
                          --  If the cursor is not fully visible => Scroll
                          or else Inter_Rect.Width < Cursor_Rect.Width
                          or else Inter_Rect.Height < Cursor_Rect.Height;
                     end;

                  else
                     if not Extend_Selection then
                        --  We can't use Place_Cursor for the multicursors
                        Sel_Mark := Get_Sel_Mark (Cursor);
                        Buffer.Move_Mark (Sel_Mark, Iter);
                     end if;

                     Buffer.Move_Mark (Cursor_Mark, Iter);
                  end if;

                  if (Command.Kind = Line or Command.Kind = Page)
                    and then Get_Line (Iter) /= 0
                  then
                     Set_Column_Memory (Cursor, Horiz_Offset);
                  end if;

                  --  Move also the saved cursor's mark: this is automatically
                  --  done by Gtk when the cursor is visible (i.e: when the
                  --  editor has the focus) but not when the cursor is not
                  --  visible (e.g: when the omnisearch has the focus).

                  if Cursor = Get_Main_Cursor (Buffer) then
                     Buffer.Move_Mark (Saved_Mark, Iter);
                  end if;
               end if;
            end;
         end loop;

         if Integer (List_Cursors.Length) > 1 then
            Update_MC_Selection (Buffer);
         end if;

         if Should_Scroll then
            View.Scroll_To_Cursor_Location;
         end if;
      end if;

      Buffer.Get_Iter_At_Mark (Iter, Saved_Mark);
      Set_Cursors_Auto_Sync (Buffer);

      if (Command.Kind = Line or Command.Kind = Page)
        and then Get_Line (Iter) /= 0
      then
         Set_Column_Memory (C, Column);
      end if;

      Grab_Toplevel_Focus
        (MDI     => Get_MDI (Kernel),
         Widget  => Editor,
         Present => True);

      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Scroll_Command;
      Context : Interactive_Command_Context)
      return Standard.Commands.Command_Return_Type
   is
      pragma Unreferenced (Command, Context);
      Kernel       : constant Kernel_Handle :=
                       Get_Kernel (Src_Editor_Module_Id.all);
      Editor       : constant MDI_Child := Find_Current_Editor (Kernel);
      Source_Box   : constant Source_Editor_Box :=
                       Get_Source_Box_From_MDI (Editor);
      View         : constant Source_View := Source_Box.Get_View;
      Adj          : constant Gtk_Adjustment := View.Get_Hadjustment;
      Val          : constant Gdouble := Adj.Get_Value;
   begin
      Remove_Completion;
      --  First center the mark onscreen
      Scroll_To_Mark
        (View,
         View.Saved_Cursor_Mark,
         Within_Margin => 0.0,
         Use_Align     => True,
         Xalign        => 0.0,
         Yalign        => 0.5);

      --  Then restore the previous adjustment
      Adj.Set_Value (Val);

      Grab_Toplevel_Focus
        (MDI     => Get_MDI (Kernel),
         Widget  => Editor,
         Present => True);

      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Delete_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Context);

      Kernel      : constant Kernel_Handle := Get_Kernel
        (Src_Editor_Module_Id.all);
      Editor      : constant MDI_Child := Find_Current_Editor (Kernel);
      Source_Box  : constant Source_Editor_Box :=
                       Get_Source_Box_From_MDI (Editor);
      View        : constant Source_View := Source_Box.Get_View;
      Buffer      : constant Source_Buffer :=
        Source_Buffer (Get_Buffer (View));
      Iter, Start, To : Gtk_Text_Iter;

   begin
      Remove_Completion;
      for Cursor of Get_Cursors (Buffer) loop
         declare
            Cursor_Mark : constant Gtk_Text_Mark := Get_Mark (Cursor);
            Delete_Line : Boolean := True;
            Result      : Boolean := True;
         begin
            Set_Manual_Sync (Cursor);
            Get_Iter_At_Mark (Buffer, Iter, Cursor_Mark);
            Copy (Source => Iter, Dest => Start);
            Copy (Source => Iter, Dest => To);

            --  Do nothing if we at the beggining of the line
            if Get_Line_Offset (To) = 0 then
               Move_Iter
                 (Iter         => To,
                  Kind         => Line,
                  Step         => -1,
                  Horiz_Offset => Gint'Last);

            else
               --  Checks that current line is empty
               loop
                  if Character'Pos (Get_Char (To)) > 32 then
                     Delete_Line := False;
                     exit;
                  end if;
                  exit when Get_Line_Offset (To) = 0;
                  Backward_Char (To, Result);
                  exit when not Result;
               end loop;
            end if;

            if Delete_Line then
               Copy (Source => To, Dest => Iter);

            else
               Move_Iter
                 (Iter => Iter,
                  Kind => Command.Kind,
                  Step => Command.Count);
            end if;

            Delete (Buffer, Iter, Start);
         end;
      end loop;

      Set_Cursors_Auto_Sync (Buffer);

      Grab_Toplevel_Focus
        (MDI     => Get_MDI (Kernel),
         Widget  => Editor,
         Present => True);

      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Formatting_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Context);
      Kernel : constant Kernel_Handle := Get_Kernel (Src_Editor_Module_Id.all);
      Editor : constant MDI_Child := Find_Current_Editor (Kernel);
      Box    : constant Source_Editor_Box := Get_Source_Box_From_MDI (Editor);
      View   : constant Source_View   := Get_View (Box);
      Buffer : constant Source_Buffer := Get_Buffer (Box);
      Result : Boolean;

   begin
      if not Get_Editable (View) then
         return Failure;
      end if;

      Result := Do_Indentation
        (Buffer, Force => True, Formatting => not Command.Indent);

      if Result then
         Grab_Toplevel_Focus
           (MDI     => Get_MDI (Kernel),
            Widget  => Editor,
            Present => True);

         return Success;
      else
         return Failure;
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Control_Command;
      Context : Interactive_Command_Context)
      return Standard.Commands.Command_Return_Type
   is
      pragma Unreferenced (Context);
      Kernel : constant Kernel_Handle := Get_Kernel (Src_Editor_Module_Id.all);
      Box    : constant Source_Editor_Box :=
                 Get_Source_Box_From_MDI (Find_Current_Editor (Kernel));
      View   : constant Source_View   := Get_View (Box);
   begin
      case Command.Mode is
         when As_Is =>
            View.As_Is_Mode := Enabled;

         when Sticky_As_Is =>
            if View.As_Is_Mode = Sticky_Enabled then
               View.As_Is_Mode := Disabled;
            else
               View.As_Is_Mode := Sticky_Enabled;
            end if;
      end case;
      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Tab_As_Space_Command;
      Context : Interactive_Command_Context)
      return Standard.Commands.Command_Return_Type
   is
      pragma Unreferenced (Context, Command);
      Kernel     : constant Kernel_Handle := Get_Kernel
        (Src_Editor_Module_Id.all);
      Editor     : constant MDI_Child := Find_Current_Editor (Kernel);
      Source_Box : constant Source_Editor_Box :=
                        Get_Source_Box_From_MDI (Editor);
      View       : constant Source_View := Source_Box.Get_View;
      Buffer     : constant Source_Buffer := Source_Box.Get_Buffer;
      Tab_Width  : Natural;

      Line       : Editable_Line_Type;
      Column     : Character_Offset_Type;
      Num        : Natural;
   begin
      if View = null then
         return Failure;
      end if;

      --  Read the indentation parameters corresponding to the language
      Tab_Width := Get_Language (Buffer).Get_Indentation_Level;

      --  Get the cursor position
      Get_Cursor_Position (Buffer, Line, Column);

      --  Compute the number of spaces to insert
      Num := Tab_Width - (Natural (Column - 1) mod Tab_Width);

      --  Insert the spaces
      declare
         Text : constant String (1 .. Num) := (others => ' ');
      begin
         Replace_Slice (Buffer, Text, Line, Column, Before => 0, After => 0);
      end;

      Grab_Toplevel_Focus
        (MDI     => Get_MDI (Kernel),
         Widget  => Editor,
         Present => True);

      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Add_String_Comment_Command;
      Context : Interactive_Command_Context)
      return Standard.Commands.Command_Return_Type
   is
      pragma Unreferenced (Context, Command);
      Kernel          : constant Kernel_Handle := Get_Kernel
        (Src_Editor_Module_Id.all);
      Editor          : constant MDI_Child := Find_Current_Editor (Kernel);
      Source_Box      : constant Source_Editor_Box :=
                        Get_Source_Box_From_MDI (Editor);
      View            : constant Source_View := Source_Box.Get_View;
      Buffer          : constant Source_Buffer := Source_Box.Get_Buffer;
      Line            : Editable_Line_Type;
      Column          : Character_Offset_Type;
      Iter            : Gtk_Text_Iter;
      Cursor_Position : Character_Offset_Type := 4;
      --  Set cursor after the `& "` pattern on the new line

      Dummy           : Boolean;

   begin
      if View = null then
         return Failure;
      end if;

      --  Get the cursor position
      Buffer.Get_Cursor_Position (Iter);
      Buffer.Get_Iter_Position (Iter, Line, Column);

      declare

         --------------
         -- Get_Text --
         --------------

         function Get_Text return String;
         function Get_Text return String
         is
            Result : Boolean;
         begin
            if Buffer.Is_In_Comment (Iter) then
               Cursor_Position := 5;
               return ASCII.LF & "--  ";
            end if;

            if Buffer.Is_In_String (Iter) then
               return '"' & ASCII.LF & "& """;
            end if;

            Backward_Char (Iter, Result);

            while Result loop
               if Buffer.Is_In_Comment (Iter) then
                  Cursor_Position := 5;
                  return ASCII.LF & "--  ";

               elsif Get_Char (Iter) = '"' then
                  return ASCII.LF & "& " & '"' & '"';

               elsif Get_Char (Iter) = ' ' then
                  null;

               else
                  return "";
               end if;

               Backward_Char (Iter, Result);
            end loop;

            return "";
         end Get_Text;

         Text : constant String := Get_Text;
      begin
         --  Text is not prepared, do nothing
         if Text = "" then
            return Success;
         end if;

         --  Insert the string prepared text
         Replace_Slice (Buffer, Text, Line, Column, Before => 0, After => 0);
      end;

      --  Set cursor after the inserted text
      Buffer.Set_Cursor_Position
        (Line => Line + 1, Column => Cursor_Position, Internal => True);

      --  Indent added text
      Dummy := Do_Indentation
        (Buffer,
         Current_Line_Only => True,
         Force             => True,
         Formatting        => False);

      Grab_Toplevel_Focus
        (MDI     => Get_MDI (Kernel),
         Widget  => Editor,
         Present => True);

      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Split_String_Command;
      Context : Interactive_Command_Context)
      return Standard.Commands.Command_Return_Type
   is
      pragma Unreferenced (Context, Command);
      Kernel          : constant Kernel_Handle := Get_Kernel
        (Src_Editor_Module_Id.all);
      Editor          : constant MDI_Child := Find_Current_Editor (Kernel);
      Source_Box      : constant Source_Editor_Box :=
                        Get_Source_Box_From_MDI (Editor);
      View            : constant Source_View := Source_Box.Get_View;
      Buffer          : constant Source_Buffer := Source_Box.Get_Buffer;
      Line            : Editable_Line_Type;
      Column          : Character_Offset_Type;
      Iter            : Gtk_Text_Iter;
      Dummy           : Boolean;

   begin
      if View = null then
         return Failure;
      end if;

      --  Get the cursor position
      Buffer.Get_Cursor_Position (Iter);
      Buffer.Get_Iter_Position (Iter, Line, Column);

      if Buffer.Is_In_String (Iter) then
         Replace_Slice
           (Buffer, """ &  & """, Line, Column, Before => 0, After => 0);

         --  Set cursor inside the inserted text
         Buffer.Set_Cursor_Position
           (Line => Line, Column => Column + 4, Internal => True);

         Grab_Toplevel_Focus
           (MDI     => Get_MDI (Kernel),
            Widget  => Editor,
            Present => True);
      else
         Buffer.Insert (Line => Line, Column => Column, Text => " ");
      end if;

      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Paste_Into_String_Command;
      Context : Interactive_Command_Context)
      return Standard.Commands.Command_Return_Type
   is
      pragma Unreferenced (Context, Command);
      Kernel          : constant Kernel_Handle := Get_Kernel
        (Src_Editor_Module_Id.all);
      Editor          : constant MDI_Child := Find_Current_Editor (Kernel);
      Source_Box      : constant Source_Editor_Box :=
                        Get_Source_Box_From_MDI (Editor);
      View            : constant Source_View := Source_Box.Get_View;
      Buffer          : constant Source_Buffer := Source_Box.Get_Buffer;
      Line            : Editable_Line_Type;
      Column          : Character_Offset_Type;
      Iter            : Gtk_Text_Iter;
      Dummy           : Boolean;

      List            : constant Selection_List :=
        Get_Clipboard (Kernel).Get_Content;
   begin
      if View = null then
         return Failure;
      end if;

      --  Get the cursor position
      Buffer.Get_Cursor_Position (Iter);
      Buffer.Get_Iter_Position (Iter, Line, Column);

      if List'Length > 0
        and then Buffer.Is_In_String (Iter)
      then
         declare
            Txt : constant String := """ & " & List (List'First).all & " & """;
         begin
            Replace_Slice
              (Buffer, Txt, Line, Column, Before => 0, After => 0);

            --  Set cursor after the inserted text
            Buffer.Set_Cursor_Position
              (Line     => Line,
               Column   => Column + Txt'Length,
               Internal => True);
         end;

         Grab_Toplevel_Focus
           (MDI     => Get_MDI (Kernel),
            Widget  => Editor,
            Present => True);
      end if;

      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Lock_Or_Unlock_Commmand;
      Context : Interactive_Command_Context)
      return Standard.Commands.Command_Return_Type
   is
      Kernel        : constant Kernel_Handle := Get_Kernel
        (Src_Editor_Module_Id.all);
      Editor        : constant MDI_Child := Find_Current_Editor (Kernel);
      Source_Box    : constant Source_Editor_Box :=
        Get_Source_Box_From_MDI (Editor);
   begin
      Source_Box.Set_Is_Locked (not Source_Box.Is_Locked);

      --  Change the MDI group of locked editors to separate them from
      --  non-locked editors. This is useful to keep the locked editor always
      --  visible if it's placed in a separate notebook, which is the sual
      --  case.
      if Source_Box.Is_Locked then
         Editor.Change_Group (Locked_MDI_Group);
      else
         Editor.Change_Group (Group_Default);
      end if;

      if Command.Split and then Source_Box.Is_Locked then
         Split
           (Get_MDI (Kernel),
            Orientation_Horizontal,
            Editor, Mode => Before_Reuse);
      end if;

      return Success;
   end Execute;

end Src_Editor_View.Commands;
