-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
--                              ACT-Europe                           --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
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

with Glib; use Glib;
with Gtk.Container;
with Gtk.Window;
with GVD.Types;

--  This package provides an implementation of Source_Editor based on
--  the Gtk.Socket mechanism that enables Gtk+ to embed external X windows
--  as Gtk+ widgets.

package GVD.Text_Box.Source_Editor.Socket is

   type Socket_Record is new Source_Editor_Record with private;
   type Socket is access all Socket_Record'Class;

   procedure Gtk_New
     (Editor     : out Socket;
      Socket_XID : Guint32;
      TTY_Mode   : Boolean);
   --  Create a new source editor.

   procedure Initialize
     (Editor     : access Socket_Record'Class;
      Socket_XID : Guint32;
      TTY_Mode   : Boolean);
   --  Internal initialization procedure.

   procedure Attach
     (Editor : access Socket_Record;
      Parent : access Gtk.Container.Gtk_Container_Record'Class);
   --  See GVD.Text_Box.Source_Editor for more information.

   procedure Detach (Editor : access Socket_Record);
   --  See GVD.Text_Box.Source_Editor for more information.

   procedure Highlight_Word
     (Editor   : access Socket_Record;
      Line     : Natural;
      Column   : Natural;
      Position : GVD.Types.Position_Type);
   --  See GVD.Text_Box.Source_Editor for more information.

   procedure Update_Breakpoints
     (Editor : access Socket_Record;
      Br     : GVD.Types.Breakpoint_Array);
   --  See GVD.Text_Box.Source_Editor for more information.

   procedure Load_File
     (Editor      : access Socket_Record;
      File_Name   : String;
      Set_Current : Boolean := True;
      Force       : Boolean := False);
   --  See GVD.Text_Box.Source_Editor for more information.

   procedure Highlight_Current_Line (Editor : access Socket_Record);
   --  See GVD.Text_Box.Source_Editor for more information.

   procedure Preferences_Changed (Editor : access Socket_Record);
   --  See GVD.Text_Box.Source_Editor for more information.

   procedure Set_Line
     (Editor      : access Socket_Record;
      Line        : Natural;
      Set_Current : Boolean := True);
   --  See GVD.Text_Box.Source_Editor for more information.

   function Get_Line (Editor : access Socket_Record) return Natural;
   --  See GVD.Text_Box.Source_Editor for more information.

private
   type Socket_Record is new Source_Editor_Record with record
      TTY_Mode       : Boolean;
      Never_Attached : Boolean := True;
      Socket_XID     : Guint32;
      Win            : Gtk.Window.Gtk_Window;
      Line           : Natural := 0;
   end record;

end GVD.Text_Box.Source_Editor.Socket;
