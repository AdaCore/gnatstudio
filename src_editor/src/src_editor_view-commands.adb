-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                       Copyright (C) 2003                          --
--                            ACT-Europe                             --
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

with Commands;          use Commands;
with Glide_Kernel;      use Glide_Kernel;
with Src_Editor_Buffer; use Src_Editor_Buffer;
with Src_Editor_View;   use Src_Editor_View;
with Gtk.Text_Iter;     use Gtk.Text_Iter;
with Glib;              use Glib;

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
      end case;
   end Move_Iter;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Move_Command; Event : Gdk.Event.Gdk_Event)
      return Command_Return_Type
   is
      pragma Unreferenced (Event);
      View   : constant Source_View   :=
        Source_View (Get_Current_Focus_Widget (Command.Kernel));
      Buffer : constant Source_Buffer := Source_Buffer (Get_Buffer (View));
      Iter        : Gtk_Text_Iter;
   begin
      Get_Iter_At_Mark (Buffer, Iter, View.Saved_Cursor_Mark);
      Move_Iter (Iter, Command.Kind, Command.Step);
      Move_Mark (Buffer, View.Saved_Cursor_Mark, Iter);
      Place_Cursor (Buffer, Iter);
      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Scroll_Command; Event : Gdk.Event.Gdk_Event)
      return Standard.Commands.Command_Return_Type
   is
      pragma Unreferenced (Event);
      View   : constant Source_View   :=
        Source_View (Get_Current_Focus_Widget (Command.Kernel));
   begin
      Scroll_To_Mark
        (View,
         View.Saved_Cursor_Mark,
         Within_Margin => 0.0,
         Use_Align => True,
         Xalign => 0.0,
         Yalign => 0.5);
      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Delete_Command; Event : Gdk.Event.Gdk_Event)
      return Command_Return_Type
   is
      pragma Unreferenced (Event);
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

   function Execute
     (Command : access Indentation_Command; Event : Gdk.Event.Gdk_Event)
      return Command_Return_Type
   is
      pragma Unreferenced (Event);
      View   : constant Source_View   :=
        Source_View (Get_Current_Focus_Widget (Command.Kernel));
      Buffer : constant Source_Buffer := Source_Buffer (Get_Buffer (View));
   begin
      if Do_Indentation (Buffer, Get_Language (Buffer)) then
         return Success;
      else
         return Failure;
      end if;
   end Execute;

end Src_Editor_View.Commands;
