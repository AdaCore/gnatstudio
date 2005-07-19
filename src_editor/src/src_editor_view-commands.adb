-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2003-2005                       --
--                            AdaCore                                --
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

with Glib;              use Glib;
with Gtk.Text_Iter;     use Gtk.Text_Iter;
with Gtk.Text_Mark;     use Gtk.Text_Mark;
with Gtkada.Text_View;  use Gtkada.Text_View;

with Commands;          use Commands;
with GPS.Kernel;        use GPS.Kernel;
with Src_Editor_Buffer; use Src_Editor_Buffer;
with Src_Editor_View;   use Src_Editor_View;

package body Src_Editor_View.Commands is

   procedure Move_Iter
     (Iter : in out Gtk_Text_Iter; Kind : Movement_Type; Step : Integer);
   --  Move the iterator according to Kind

   ---------------
   -- Move_Iter --
   ---------------

   procedure Move_Iter
     (Iter : in out Gtk_Text_Iter; Kind : Movement_Type; Step : Integer)
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
      end case;
   end Move_Iter;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Move_Command;
      Context : Interactive_Command_Context)
      return Command_Return_Type
   is
      pragma Unreferenced (Context);
      View   : constant Source_View   :=
        Source_View (Get_Current_Focus_Widget (Command.Kernel));
      Buffer : constant Source_Buffer := Source_Buffer (Get_Buffer (View));
      Iter   : Gtk_Text_Iter;
      Mark   : constant Gtk_Text_Mark := Get_Saved_Cursor_Mark (View);

   begin
      Get_Iter_At_Mark (Buffer, Iter, Mark);
      Move_Iter (Iter, Command.Kind, Command.Step);
      Move_Mark (Buffer, Mark, Iter);
      Place_Cursor (Buffer, Iter);
      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute
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
         Get_Saved_Cursor_Mark (View),
         Within_Margin => 0.0,
         Use_Align     => True,
         Xalign        => 0.0,
         Yalign        => 0.5);
      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute
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
      Get_Iter_At_Mark (Buffer, Iter, Get_Saved_Cursor_Mark (View));
      Copy (Source => Iter, Dest => Start);
      Move_Iter (Iter, Command.Kind, Command.Count);
      Delete (Buffer, Iter, Start);
      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Indentation_Command;
      Context : Interactive_Command_Context)
      return Command_Return_Type
   is
      pragma Unreferenced (Context);
      View   : constant Source_View   :=
        Source_View (Get_Current_Focus_Widget (Command.Kernel));
      Buffer : constant Source_Buffer := Source_Buffer (Get_Buffer (View));
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

   function Execute
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

end Src_Editor_View.Commands;
