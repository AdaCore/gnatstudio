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

package Gtkada.Terminal is

   type GtkAda_Terminal_Record is new Gtk.Text_Buffer.Gtk_Text_Buffer_Record
      with private;
   type GtkAda_Terminal is access all GtkAda_Terminal_Record'Class;

   procedure Gtk_New (Self : out GtkAda_Terminal);
   procedure Initialize
     (Self : access GtkAda_Terminal_Record'Class);
   --  Creates or initializes a terminal.

   procedure On_Set_Title
     (Term  : access GtkAda_Terminal_Record;
      Title : String);
   --  Called when the title of the terminal should be changed. Since the
   --  terminal is not a widget in itself, you must override this subprogram to
   --  make something useful with it

private

   type FSM_Transition;
   type FSM_Transition_Access is access FSM_Transition;
   --  A finite state machine used to parse the text written on the terminal,
   --  and efficiently detect special escape sequences.

   type GtkAda_Terminal_Record is new Gtk.Text_Buffer.Gtk_Text_Buffer_Record
   with record
      Alternate_Charset : Boolean := False;
      --  Whether we are in the alternate character set. This is a way for
      --  applications to display height bit chars by sending only 7bits

      FSM : FSM_Transition_Access;
      --  The finite state machine to find the special escape sequences. This
      --  is null if the terminal should not try to find such sequences
   end record;

end Gtkada.Terminal;
