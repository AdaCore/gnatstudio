-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2009, AdaCore                       --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
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

--  This package provides an Entity View widget

with Gtk.GEntry; use Gtk.GEntry;

package Completion_Window.Entity_Views is

   type Entity_View_Record is new Gtk_Vbox_Record with private;
   type Entity_View_Access is access all Entity_View_Record'Class;

   procedure Gtk_New
     (View     : out Entity_View_Access;
      Kernel   : Kernel_Handle;
      Initial  : UTF8_String);
   --  Create a new Completion_Explorer

   procedure Initialize
     (View     : access Entity_View_Record'Class;
      Kernel   : Kernel_Handle;
      Initial  : UTF8_String);
   --  Internal initialization procedure

private

   type Entity_View_Record is new Gtk_Vbox_Record with record
      Explorer : Completion_Explorer_Access;
      Ent      : Gtk_Entry;
   end record;

end Completion_Window.Entity_Views;
