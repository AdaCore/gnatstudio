-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
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

with Glib.Object;         use Glib.Object;
with Gtk.Button;          use Gtk.Button;
with Gtk.Stock;           use Gtk.Stock;
with Gtk.Toolbar;         use Gtk.Toolbar;
with Glide_Intl;          use Glide_Intl;
with Glide_Kernel.Editor; use Glide_Kernel.Editor;

package body Glide_Toolbar is

   procedure On_New_File
     (Object : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  Callback for the 'New' Button

   -----------------
   -- On_New_File --
   -----------------

   procedure On_New_File
     (Object : access GObject_Record'Class;
      Kernel : Kernel_Handle) is
   begin
      New_Editor (Kernel);

   exception
      when others =>
         null;
         --  ??? Log_Exception (E);
   end On_New_File;

   procedure Register_Default_Toolbar
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Toolbar : constant Gtk_Toolbar := Get_Toolbar (Kernel);
      Button  : Gtk_Button;

   begin
      Set_Tooltips (Toolbar, True);
      Button := Insert_Stock (Toolbar, Stock_New, -"Create a New File");
      Kernel_Callback.Connect
        (Button, "clicked",
         Kernel_Callback.To_Marshaller (On_New_File'Access),
         Kernel_Handle (Kernel));

      Button := Insert_Stock (Toolbar, Stock_Open, -"Open a File");
      Button := Insert_Stock (Toolbar, Stock_Save, -"Save Current File");
      Append_Space (Toolbar);
      Button := Insert_Stock (Toolbar, Stock_Undo, -"Undo Previous Action");
      Button := Insert_Stock (Toolbar, Stock_Redo, -"Redo Previous Action");
      Append_Space (Toolbar);
      Button := Insert_Stock (Toolbar, Stock_Cut, -"Cut to Clipboard");
      Button := Insert_Stock (Toolbar, Stock_Copy, -"Copy from Clipboard");
      Button := Insert_Stock (Toolbar, Stock_Paste, -"Paste to Clipboard");
   end Register_Default_Toolbar;

end Glide_Toolbar;
