-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                  Copyright (C) 2008-2008, AdaCore                 --
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

--  This widget is a Gtk_Text_Buffer that emulates a terminal, that is it
--  understands ESCAPE sequences to move cursor, change color,...
--  Currently, it knows how to emulate xterm.

with Gtk.Text_Buffer;
with Gtk.Text_Mark;
with Gtk.Text_Iter;
with Gtk.Text_Tag;
with Interfaces.C;

package Gtkada.Terminal is

   type Gtkada_Terminal_Record is new Gtk.Text_Buffer.Gtk_Text_Buffer_Record
      with private;
   type Gtkada_Terminal is access all Gtkada_Terminal_Record'Class;

   procedure Gtk_New
     (Self                             : out Gtkada_Terminal;
      Prevent_Cursor_Motion_With_Mouse : Boolean := False);
   procedure Initialize
     (Self                             : access Gtkada_Terminal_Record'Class;
      Prevent_Cursor_Motion_With_Mouse : Boolean := False);
   --  Creates or initializes a terminal.
   --  Prevent_Cursor_Motion_With_Mouse should be true if the terminal should
   --  manage its own insertion point, possibly different from the gtk+ cursor.
   --  If it is true, the only way to move the cursor is through escape
   --  sequences, not the mouse nor the arrow keys will work. This is only
   --  suitable for those applications that use curses (like a Unix shell, vi
   --  or other full-screen applications), not if you are using the terminal
   --  just to be able to see colors for instance.

   overriding procedure Place_Cursor
     (Self   : access Gtkada_Terminal_Record;
      Where  : Gtk.Text_Iter.Gtk_Text_Iter);
   --  See inherited subprogram

   procedure On_Set_Title
     (Term  : access Gtkada_Terminal_Record;
      Title : String);
   --  Called when the title of the terminal should be changed. Since the
   --  terminal is not a widget in itself, you must override this subprogram to
   --  make something useful with it

private
   type FSM_Transition;
   type FSM_Transition_Access is access FSM_Transition;
   --  A finite state machine used to parse the text written on the terminal,
   --  and efficiently detect special escape sequences.

   type Tag_Array is array (1 .. 8) of Gtk.Text_Tag.Gtk_Text_Tag;
   type Numerical_Arguments is array (1 .. 9) of Integer;

   type FSM_Current_State is record
      Arg         : Numerical_Arguments;
      Current_Arg : Integer := Numerical_Arguments'First;
      --  Numerical arguments to some of the commands

      Current           : FSM_Transition_Access;
      Tmp               : FSM_Transition_Access;
      --  Current state of the finite-state machine. Tmp is used when we need
      --  to follow two possible branches at once (eg when parsing numerical
      --  arguments)

      Start_Of_Sequence : Interfaces.C.size_t := 0;
      --  The last char for which current was Class.FSM, ie the last
      --  self-insert char. This is used to rollback when an escape sequence
      --  could not be interpreted after all.

      Parsing_Number    : Boolean := False;
      --  Whether we are parsing a digit argument
   end record;
   --  Describes the current state of the finite state machine

   type Gtkada_Terminal_Record is new Gtk.Text_Buffer.Gtk_Text_Buffer_Record
   with record
      FSM : FSM_Transition_Access;
      --  The finite state machine to find the special escape sequences. This
      --  is null if the terminal should not try to find such sequences

      Prevent_Cursor_Motion : Boolean := False;
      --  If True, the terminal memorizes where the external application left
      --  the cursor, and always inserts at that position, even if the cursor
      --  was moved with the mouse to another location

      Cursor_Mark       : Gtk.Text_Mark.Gtk_Text_Mark;

      Alternate_Charset : Boolean := False;
      --  Whether we are in the alternate character set. This is a way for
      --  applications to display height bit chars by sending only 7bits

      Bold_Tag       : Gtk.Text_Tag.Gtk_Text_Tag;
      Bold           : Boolean := False;
      --  Whether text should be output in bold

      Standout_Tag   : Gtk.Text_Tag.Gtk_Text_Tag;
      Standout       : Boolean := False;
      --  Whether text should be printed in reverse video

      Foreground_Tags    : Tag_Array;
      Current_Foreground : Integer := -1;

      Background_Tags    : Tag_Array;
      Current_Background : Integer := -1;

      State              : FSM_Current_State;
   end record;

end Gtkada.Terminal;
