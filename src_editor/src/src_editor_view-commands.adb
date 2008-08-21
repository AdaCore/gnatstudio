-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                   Copyright (C) 2003-2008, AdaCore                --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
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

with Basic_Types;         use Basic_Types;
with Gtk.Adjustment;      use Gtk.Adjustment;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Text_Iter;       use Gtk.Text_Iter;
with Commands;            use Commands;
with GPS.Kernel;          use GPS.Kernel;
with Src_Editor_Buffer;   use Src_Editor_Buffer;
with Src_Editor_Buffer.Text_Handling; use Src_Editor_Buffer.Text_Handling;
with Src_Editor_Box;      use Src_Editor_Box;
with Src_Editor_Module;   use Src_Editor_Module;
with Src_Editor_View;     use Src_Editor_View;
with Language;            use Language;

package body Src_Editor_View.Commands is

   procedure Move_Iter
     (Iter : in out Gtk_Text_Iter;
      Kind : Movement_Type;
      Step : Integer);
   --  Move the iterator according to Kind. Kind should be different from
   --  Page

   ---------------
   -- Move_Iter --
   ---------------

   procedure Move_Iter
     (Iter : in out Gtk_Text_Iter;
      Kind : Movement_Type;
      Step : Integer)
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
            if Step > 0 then
               Offset := Get_Line_Offset (Iter);
               Forward_Lines (Iter, Gint (Step), Ignored);
               Forward_Cursor_Positions
                 (Iter, Gint'Min
                    (Offset, Get_Chars_In_Line (Iter) - 1), Ignored);
            else
               Offset := Get_Line_Offset (Iter);
               Backward_Lines (Iter, -Gint (Step), Ignored);
               Forward_Cursor_Positions
                 (Iter, Gint'Min
                    (Offset, Get_Chars_In_Line (Iter) - 1), Ignored);
            end if;

         when Page =>
            null;
--  ??? why is this code commented out
--              if Step > 0 then
--                 for S in 1 .. Step loop
--                    Forward_Display_Line (View, Iter, Ignored);
--                 end loop;
--              else
--                 for S in 1 .. -Step loop
--                    Backward_Display_Line (View, Iter, Ignored);
--                 end loop;
--              end if;
      end case;
   end Move_Iter;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Move_Command;
      Context : Interactive_Command_Context)
      return Command_Return_Type
   is
      pragma Unreferenced (Context);
      View   : constant Source_View   :=
        Source_View (Get_Current_Focus_Widget (Command.Kernel));
      Buffer : constant Source_Buffer := Source_Buffer (Get_Buffer (View));
      Iter   : Gtk_Text_Iter;
      Mark   : constant Gtk_Text_Mark := View.Saved_Cursor_Mark;
      Scrolled : Gtk_Scrolled_Window;
      Adj      : Gtk_Adjustment;
      Moved    : Boolean;
      pragma Unreferenced (Moved);

   begin
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
         return Success;

      else
         Get_Iter_At_Mark (Buffer, Iter, Mark);
         Move_Iter (Iter, Command.Kind, Command.Step);
         Move_Mark (Buffer, Mark, Iter);
         Place_Cursor (Buffer, Iter);
         return Success;
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Scroll_Command;
      Context : Interactive_Command_Context)
      return Standard.Commands.Command_Return_Type
   is
      pragma Unreferenced (Context);
      View : constant Source_View :=
        Source_View (Get_Current_Focus_Widget (Command.Kernel));

   begin
      Scroll_To_Mark
        (View,
         View.Saved_Cursor_Mark,
         Within_Margin => 0.0,
         Use_Align     => True,
         Xalign        => 0.0,
         Yalign        => 0.5);
      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Delete_Command;
      Context : Interactive_Command_Context)
      return Command_Return_Type
   is
      pragma Unreferenced (Context);
      View   : constant Source_View   :=
        Source_View (Get_Current_Focus_Widget (Command.Kernel));
      Buffer : constant Source_Buffer := Source_Buffer (Get_Buffer (View));
      Iter, Start : Gtk_Text_Iter;

   begin
      Get_Iter_At_Mark (Buffer, Iter, View.Saved_Cursor_Mark);
      Copy (Source => Iter, Dest => Start);
      Move_Iter (Iter, Command.Kind, Command.Count);
      Delete (Buffer, Iter, Start);
      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Indentation_Command;
      Context : Interactive_Command_Context)
      return Command_Return_Type
   is
      pragma Unreferenced (Context);

      --  Get the current MDI child. We know it is an editor, since the filter
      --  has matched, and this is faster than looking for the current focus
      --  widget, and from there the source editor. In addition, it is possible
      --  that no widget has the focus, for instance because a dialog has
      --  temporarily been opened
      Box    : constant Source_Editor_Box :=
        Get_Source_Box_From_MDI (Find_Current_Editor (Command.Kernel));
      View   : constant Source_View   := Get_View (Box);
      Buffer : constant Source_Buffer := Get_Buffer (Box);
      Result : Boolean;

   begin
      if not Get_Editable (View) then
         return Failure;
      end if;

      Push_State (Command.Kernel, Busy);
      Result := Do_Indentation (Buffer, Force => True);
      Pop_State (Command.Kernel);

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
      View : constant Source_View :=
        Source_View (Get_Current_Focus_Widget (Command.Kernel));
   begin
      case Command.Mode is
         when As_Is =>
            View.As_Is_Mode := True;
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
      pragma Unreferenced (Context);
      View          : constant Source_View :=
        Source_View (Get_Current_Focus_Widget (Command.Kernel));
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

      --  Read the indentation parameters corresponding to the language.
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
