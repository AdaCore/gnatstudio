-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
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

--  This package contains the files view for the explorer,

with Glide_Kernel;
with Gtk.Main;
with Gtk.Handlers;
with Gtk.Scrolled_Window;
with Gtk.Tree_Model;
with Gtk.Tree_View;
with Gtk.Tree_Store;

with Generic_List;

package Project_Explorers_Files is

   type Project_Explorer_Files_Record is new
     Gtk.Scrolled_Window.Gtk_Scrolled_Window_Record with private;
   type Project_Explorer_Files
      is access all Project_Explorer_Files_Record'Class;

   procedure Gtk_New
     (Explorer : out Project_Explorer_Files;
      Kernel   : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Create a new explorer.

   procedure Initialize
     (Explorer : access Project_Explorer_Files_Record'Class;
      Kernel   : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Internal initialization procedure.

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Register the module into the list

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  You should connect to the "context_changed" signal in the kernel to get
   --  report on selection changes.
   --  </signals>

private

   type Append_Directory_Idle_Data;
   type Append_Directory_Idle_Data_Access is access Append_Directory_Idle_Data;
   --  Custom data for the asynchronous fill function.

   package File_Append_Directory_Timeout is
      new Gtk.Main.Timeout (Append_Directory_Idle_Data_Access);

   procedure Free (D : in out Gtk.Main.Timeout_Handler_Id);

   package Timeout_Id_List is new Generic_List (Gtk.Main.Timeout_Handler_Id);

   type Project_Explorer_Files_Record is new
     Gtk.Scrolled_Window.Gtk_Scrolled_Window_Record with
   record
      Kernel     : Glide_Kernel.Kernel_Handle;
      File_Tree  : Gtk.Tree_View.Gtk_Tree_View;
      File_Model : Gtk.Tree_Store.Gtk_Tree_Store;
      Expanding  : Boolean := False;

      Scroll_To_Directory : Boolean := False;
      Path                : Gtk.Tree_Model.Gtk_Tree_Path;
      Realize_Cb_Id       : Gtk.Handlers.Handler_Id;

      Fill_Timeout_Ids : Timeout_Id_List.List;
      --  ??? This is implemented as a list of handlers instead of just one
      --  handler, in case the fill function should call itself recursively :
      --  to be investigated.
   end record;
end Project_Explorers_Files;
