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

with Gtk.Container;     use Gtk.Container;
with Gtk.Socket;        use Gtk.Socket;
with Gtk.Widget;        use Gtk.Widget;
with Gtk.Window;        use Gtk.Window;
with Basic_Types;       use Basic_Types;
with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with GNAT.IO;           use GNAT.IO;

package body GVD.Text_Box.Source_Editor.Socket is

   ----------------------
   -- Local Procedures --
   ----------------------

   procedure Print_File_Location
     (File     : String;
      Line     : Natural;
      Position : Natural);
   --  Print on standard output the location of a given file, following
   --  the same syntax than gdb.
   --  ??? This is gdb specific but gives a much better integration, so
   --  it is worth it.

   ------------
   -- Attach --
   ------------

   procedure Attach
     (Editor : access Socket_Record;
      Parent : access Gtk.Container.Gtk_Container_Record'Class) is
   begin
      if Editor.Never_Attached then
         Show (Editor.Widget);
         Add (Parent, Editor.Widget);
         Realize (Editor.Widget);
         Steal (Gtk_Socket (Editor.Widget), Editor.Socket_XID);
         Editor.Never_Attached := False;
      else
         Reparent (Editor.Widget, Parent);
         Destroy (Editor.Win);
      end if;
   end Attach;

   ------------
   -- Detach --
   ------------

   procedure Detach (Editor : access Socket_Record) is
   begin
      Gtk_New (Editor.Win);
      Show (Editor.Win);
      Reparent (Editor.Widget, Editor.Win);
   end Detach;

   --------------
   -- Get_Line --
   --------------

   function Get_Line (Editor : access Socket_Record) return Natural is
   begin
      return Editor.Line;
   end Get_Line;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Editor     : out Socket;
      Socket_XID : Guint32;
      TTY_Mode   : Boolean) is
   begin
      Editor := new Socket_Record;
      Initialize (Editor, Socket_XID, TTY_Mode);
   end Gtk_New;

   ----------------------------
   -- Highlight_Current_Line --
   ----------------------------

   procedure Highlight_Current_Line (Editor : access Socket_Record) is
   begin
      if Editor.TTY_Mode then
         Print_File_Location (Editor.Current_File.all, Editor.Line, 0);
      end if;
   end Highlight_Current_Line;

   --------------------
   -- Highlight_Word --
   --------------------

   procedure Highlight_Word
     (Editor   : access Socket_Record;
      Line     : Natural;
      Column   : Natural;
      Position : Position_Type) is
   begin
      if Editor.TTY_Mode then
         Print_File_Location
           (Editor.Current_File.all, Line, Natural (Position));
      end if;
   end Highlight_Word;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Editor     : access Socket_Record'Class;
      Socket_XID : Guint32;
      TTY_Mode   : Boolean)
   is
      Socket : Gtk_Socket;
   begin
      Gtk_New (Socket);
      Editor.Widget := Gtk_Widget (Socket);
      Editor.TTY_Mode := TTY_Mode;
      Editor.Socket_XID := Socket_XID;
   end Initialize;

   ---------------
   -- Load_File --
   ---------------

   procedure Load_File
     (Editor      : access Socket_Record;
      File_Name   : String;
      Set_Current : Boolean := True;
      Force       : Boolean := False) is
   begin
      Free (Editor.Current_File);
      Editor.Current_File := new String' (File_Name);

      if Editor.TTY_Mode then
         Print_File_Location (File_Name, 1, 0);
      end if;
   end Load_File;

   -------------------------
   -- Preferences_Changed --
   -------------------------

   procedure Preferences_Changed (Editor : access Socket_Record) is
   begin
      null;
   end Preferences_Changed;

   procedure Print_File_Location
     (File     : String;
      Line     : Natural;
      Position : Natural) is
   begin
      Put_Line (ASCII.SUB & ASCII.SUB & File & ":" &
        Trim (Natural'Image (Line), Left) & ":" &
        Trim (Natural'Image (Position), Left) & ":beg:0x0");
   end Print_File_Location;

   --------------
   -- Set_Line --
   --------------

   procedure Set_Line
     (Editor      : access Socket_Record;
      Line        : Natural;
      Set_Current : Boolean := True) is
   begin
      Editor.Line := Line;

      if Editor.TTY_Mode then
         Print_File_Location (Editor.Current_File.all, Line, 0);
      end if;
   end Set_Line;

   ------------------------
   -- Update_Breakpoints --
   ------------------------

   procedure Update_Breakpoints
     (Editor : access Socket_Record;
      Br     : GVD.Types.Breakpoint_Array) is
   begin
      null;
   end Update_Breakpoints;

end GVD.Text_Box.Source_Editor.Socket;

