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

with Gtk.Tree_View;            use Gtk.Tree_View;
with Gtk.Tree_Store;           use Gtk.Tree_Store;
with Gtk.Box;                  use Gtk.Box;

with Glide_Kernel;             use Glide_Kernel;
with String_List_Utils;

package Glide_Result_View is

   type Result_View_Record is new Gtk_Hbox_Record with private;
   type Result_View is access all Result_View_Record'Class;

   procedure Gtk_New
     (View        : out Result_View;
      Kernel      : Kernel_Handle := null);
   --  Create a new Location_View.

   procedure Initialize
     (View   : access Result_View_Record'Class;
      Kernel : Kernel_Handle := null);
   --  Internal initialization procedure.

   procedure Insert
     (View              : access Result_View_Record'Class;
      Identifier        : String;
      Source_File       : String;
      Source_Line       : Natural;
      Source_Column     : Natural;
      Message           : String);
   --  See comment in glide_kernel-result_tree.ads.

private
   type Result_View_Record is new Gtk_Hbox_Record with record
      Kernel : Kernel_Handle;
      Tree   : Gtk_Tree_View;
      Model  : Gtk_Tree_Store;

      Unopened_Files : String_List_Utils.String_List.List;
   end record;

end Glide_Result_View;
