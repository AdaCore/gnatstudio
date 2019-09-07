------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2003-2019, AdaCore                     --
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
with Gtk.Scrolled_Window;             use Gtk.Scrolled_Window;
with Gtk.Text_Iter;                   use Gtk.Text_Iter;
with Gtkada.MDI;                      use Gtkada.MDI;

with Basic_Types;                     use Basic_Types;
with Commands;                        use Commands;
with Completion_Module;               use Completion_Module;
with GPS.Kernel;                      use GPS.Kernel;
with GPS.Kernel.MDI;                  use GPS.Kernel.MDI;
with GPS.Kernel.Modules;              use GPS.Kernel.Modules;
with GUI_Utils;                       use GUI_Utils;
with Language;                        use Language;
with Src_Editor_Buffer;               use Src_Editor_Buffer;
with Src_Editor_Buffer.Text_Handling; use Src_Editor_Buffer.Text_Handling;
with Src_Editor_Buffer.Cursors; use Src_Editor_Buffer.Cursors;
with Src_Editor_Box;                  use Src_Editor_Box;
with Src_Editor_Module;               use Src_Editor_Module;
with Src_Editor_View;                 use Src_Editor_View;

package body Src_Editor_View.Commands is

   procedure Move_Iter
     (Iter : in out Gtk_Text_Iter;
      Kind : Movement_Type;
      Step : Integer;
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
     (Iter : in out Gtk_Text_Iter;
      Kind : Movement_Type;
      Step : Integer;
      Horiz_Offset : Gint := -1)
   is
      Ignored, Success : Boolean;
      Offset           : Gint;
   begin
      case Kind is
         when Word =>
            if Step > 0 then
               Forward_Word_Ends (Iter, Gint (Step), Ignored);
            else
               Backward_Word_Starts (Iter, -Gint (Step), Ignored);
            end if;

         when Paragraph =>
            if Step > 0 then
               Move_Paragraph_Forward :
               for J in 1 .. Step loop
                  loop
                     Forward_Line (Iter, Success);  --  to start of next line
                     exit Move_Paragraph_Forward when not Success;

                     --  We are at beginning of line, skip spaces
                     --  If at end of line, we found a new paragraph
                     Skip_Whitespace (Iter, Forward => True);
                     exit when Ends_Line (Iter);
                  end loop;
               end loop Move_Paragraph_Forward;

            else
               Move_Paragraph_Backward :
               for J in 1 .. abs (Step) loop
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
               Forward_Cursor_Positions (Iter, Gint (Step), Ignored);
            else
               Backward_Cursor_Positions (Iter, -Gint (Step), Ignored);
            end if;

         when Line =>
            Offset := Horiz_Offset;
            if Step > 0 then
               Forward_Lines (Iter, Gint (Step), Ignored);
               Forward_Cursor_Positions
                 (Iter, Gint'Min
                    (Offset, Get_Chars_In_Line (Iter) - 1), Ignored);
            else
               if Get_Line (Iter) = 0 then
                  Set_Line_Offset (Iter, 0);
                  return;
               end if;
               Backward_Lines (Iter, -Gint (Step), Ignored);
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
      Kernel      : constant Kernel_Handle := Get_Kernel
        (Src_Editor_Module_Id.all);
      Editor      : constant MDI_Child := Find_Current_Editor (Kernel);
      Source_Box  : constant Source_Editor_Box :=
                       Get_Source_Box_From_MDI (Editor);
      View        : constant Source_View := Source_Box.Get_View;
      Buffer      : constant Source_Buffer :=
                      Source_Buffer (Get_Buffer (View));
      Iter, Start : Gtk_Text_Iter;
      pragma Unreferenced (Context);

   begin
      Remove_Completion;
      for Cursor of Get_Cursors (Buffer) loop
         declare
            Cursor_Mark : constant Gtk_Text_Mark := Get_Mark (Cursor);
         begin
            Set_Manual_Sync (Cursor);
            Get_Iter_At_Mark (Buffer, Iter, Cursor_Mark);
            Copy (Source => Iter, Dest => Start);
            Move_Iter (Iter, Command.Kind, Command.Count);
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
     (Command : access Indentation_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Context, Command);
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

      Result := Do_Indentation (Buffer, Force => True);

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
      Kernel        : constant Kernel_Handle := Get_Kernel
        (Src_Editor_Module_Id.all);
      Editor        : constant MDI_Child := Find_Current_Editor (Kernel);
      Source_Box    : constant Source_Editor_Box :=
                        Get_Source_Box_From_MDI (Editor);
      View          : constant Source_View := Source_Box.Get_View;
      Buffer        : constant Source_Buffer := Source_Box.Get_Buffer;
      Indent_Params : Indent_Parameters;
      Indent_Style  : Indentation_Kind;
      Tab_Width     : Natural renames Indent_Params.Indent_Level;

      Line          : Editable_Line_Type;
      Column        : Character_Offset_Type;
      Num           : Natural;
   begin
      if View = null then
         return Failure;
      end if;

      --  Read the indentation parameters corresponding to the language
      Get_Indentation_Parameters
        (Get_Language (Buffer), Indent_Params, Indent_Style);

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

end Src_Editor_View.Commands;
