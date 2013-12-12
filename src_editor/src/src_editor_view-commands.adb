------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2013, AdaCore                     --
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

with Gtk.Adjustment;                  use Gtk.Adjustment;
with Gtk.Scrolled_Window;             use Gtk.Scrolled_Window;
with Gtk.Text_Iter;                   use Gtk.Text_Iter;

with Basic_Types;                     use Basic_Types;
with Commands;                        use Commands;
with GPS.Kernel;                      use GPS.Kernel;
with GPS.Kernel.MDI;                  use GPS.Kernel.MDI;
with GPS.Kernel.Modules;              use GPS.Kernel.Modules;
with Language;                        use Language;
with Src_Editor_Buffer;               use Src_Editor_Buffer;
with Src_Editor_Buffer.Text_Handling; use Src_Editor_Buffer.Text_Handling;
with Src_Editor_Buffer.Multi_Cursors; use Src_Editor_Buffer.Multi_Cursors;
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

   ---------------
   -- Move_Iter --
   ---------------

   procedure Move_Iter
     (Iter : in out Gtk_Text_Iter;
      Kind : Movement_Type;
      Step : Integer;
      Horiz_Offset : Gint := -1)
   is
      Ignored : Boolean;
      Offset  : Gint;
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
               Forward_Sentence_Ends (Iter, Gint (Step), Ignored);
            else
               Backward_Sentence_Starts (Iter, -Gint (Step), Ignored);
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
      Kernel : constant Kernel_Handle := Get_Kernel (Src_Editor_Module_Id.all);
      View         : constant Source_View   :=
        Source_View (Get_Current_Focus_Widget (Kernel));
      Buffer       : constant Source_Buffer :=
        Source_Buffer (Get_Buffer (View));
      Iter         : Gtk_Text_Iter;
      Mark         : constant Gtk_Text_Mark := View.Saved_Cursor_Mark;
      Scrolled     : Gtk_Scrolled_Window;
      Adj          : Gtk_Adjustment;
      Moved        : Boolean;
      Column       : constant Gint := Buffer.Get_Column_Memory;
      Extend_Selection : constant Boolean :=
        View.Get_Extend_Selection or Command.Extend_Selection;

      pragma Unreferenced (Context, Moved);

   begin
      Set_Multi_Cursors_Manual_Sync (Buffer);

      if Command.Kind = Page then
         Scrolled := Gtk_Scrolled_Window (Get_Parent (View));
         Adj      := Get_Vadjustment (Scrolled);
         if Command.Step > 0 then
            Set_Value
              (Adj,
               Get_Value (Adj)
               + Gdouble (Command.Step) * Get_Page_Increment (Adj));
         else
            Set_Value
              (Adj,
               Get_Value (Adj)
               + Gdouble (Command.Step) * Get_Page_Increment (Adj));
         end if;
         Moved := Place_Cursor_Onscreen (View);
         Moved := Move_Mark_Onscreen (View, Mark);
      else
         Get_Iter_At_Mark (Buffer, Iter, Mark);
         Move_Iter (Iter, Command.Kind, Command.Step, Column);
         Move_Mark (Buffer, Mark, Iter);

         if Extend_Selection then
            Move_Mark (Buffer, Buffer.Get_Insert, Iter);
         else
            Place_Cursor (Buffer, Iter);
         end if;

         View.Scroll_To_Cursor_Location;

         Update_MC_Selection (Buffer);

         for Cursor of Get_Multi_Cursors (Buffer) loop
            declare
               Mark : Gtk_Text_Mark := Get_Mark (Cursor);
               Horiz_Offset : constant Gint :=
                 Get_Column_Memory (Cursor);
            begin
               Buffer.Get_Iter_At_Mark (Iter, Mark);
               Move_Iter (Iter, Command.Kind, Command.Step,
                          Get_Column_Memory (Cursor));
               Buffer.Move_Mark (Mark, Iter);

               if not Extend_Selection then
                  Mark := Get_Sel_Mark (Cursor);
                  Buffer.Move_Mark (Mark, Iter);
               end if;

               if (Command.Kind = Line or Command.Kind = Page)
                 and then Get_Line (Iter) /= 0
               then
                  Set_Column_Memory (Cursor, Horiz_Offset);
               end if;

            end;
         end loop;

         Update_MC_Selection (Buffer);

      end if;

      Buffer.Get_Iter_At_Mark (Iter, Mark);
      Set_Multi_Cursors_Auto_Sync (Buffer);
      if (Command.Kind = Line or Command.Kind = Page)
        and then Get_Line (Iter) /= 0
      then
         Buffer.Set_Column_Memory (Column);
      end if;

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
      Kernel : constant Kernel_Handle := Get_Kernel (Src_Editor_Module_Id.all);
      View : constant Source_View :=
               Source_View (Get_Current_Focus_Widget (Kernel));

      Adj : constant Gtk_Adjustment := View.Get_Hadjustment;
      Val : constant Gdouble := Adj.Get_Value;
   begin
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
      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Delete_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Src_Editor_Module_Id.all);
      View        : constant Source_View   :=
                      Source_View (Get_Current_Focus_Widget (Kernel));
      Buffer      : constant Source_Buffer :=
                      Source_Buffer (Get_Buffer (View));
      Iter, Start : Gtk_Text_Iter;
      pragma Unreferenced (Context);

   begin
      Set_Multi_Cursors_Manual_Sync (Buffer);

      Get_Iter_At_Mark (Buffer, Iter, View.Saved_Cursor_Mark);
      Copy (Source => Iter, Dest => Start);
      Move_Iter (Iter, Command.Kind, Command.Count);
      Delete (Buffer, Iter, Start);

      for Cursor of Get_Multi_Cursors (Buffer) loop
         declare
            Cursor_Mark : constant Gtk_Text_Mark := Get_Mark (Cursor);
         begin
            Set_Multi_Cursors_Manual_Sync (Buffer, Cursor.Cursor);
            Get_Iter_At_Mark (Buffer, Iter, Cursor_Mark);
            Copy (Source => Iter, Dest => Start);
            Move_Iter (Iter, Command.Kind, Command.Count);
            Delete (Buffer, Iter, Start);
         end;
      end loop;

      Set_Multi_Cursors_Auto_Sync (Buffer);

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

      --  Get the current MDI child. We know it is an editor, since the filter
      --  has matched, and this is faster than looking for the current focus
      --  widget, and from there the source editor. In addition, it is possible
      --  that no widget has the focus, for instance because a dialog has
      --  temporarily been opened.
      Box    : constant Source_Editor_Box :=
                 Get_Source_Box_From_MDI (Find_Current_Editor (Kernel));
      View   : constant Source_View   := Get_View (Box);
      Buffer : constant Source_Buffer := Get_Buffer (Box);
      Result : Boolean;

   begin
      if not Get_Editable (View) then
         return Failure;
      end if;

      Result := Do_Indentation (Buffer, Force => True);

      if Result then
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
      View : constant Source_View :=
               Source_View (Get_Current_Focus_Widget (Kernel));
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
      Kernel : constant Kernel_Handle := Get_Kernel (Src_Editor_Module_Id.all);
      View          : constant Source_View :=
                        Source_View
                          (Get_Current_Focus_Widget (Kernel));
      Buffer        : constant Source_Buffer :=
                        Source_Buffer (Get_Buffer (View));
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

      return Success;
   end Execute;

end Src_Editor_View.Commands;
