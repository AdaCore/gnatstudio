-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
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

with Gdk.Color;                use Gdk.Color;
with Gdk.Pixbuf;               use Gdk.Pixbuf;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Box;                  use Gtk.Box;

with Glide_Kernel;             use Glide_Kernel;
with Glide_Kernel.Modules;     use Glide_Kernel.Modules;
with VFS;

with Gtkada.Tree_View;         use Gtkada.Tree_View;

package Glide_Result_View is

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Register this module in GPS.


   type Result_View_Record is new Gtk_Hbox_Record with private;
   type Result_View is access all Result_View_Record'Class;

   procedure Gtk_New
     (View        : out Result_View;
      Kernel      : Kernel_Handle;
      Module      : Module_ID);
   --  Create a new Location_View.

   procedure Initialize
     (View   : access Result_View_Record'Class;
      Kernel : Kernel_Handle;
      Module : Module_ID);
   --  Internal initialization procedure.

   procedure Insert
     (View          : access Result_View_Record'Class;
      Identifier    : String;
      Source_File   : VFS.Virtual_File;
      Source_Line   : Positive;
      Source_Column : Positive;
      Message       : String;
      Length        : Natural;
      Highlight     : Boolean := False);
   --  Insert a new location item for the category corresponding to
   --  Identifier. Message is the text that will be displayed next to the
   --  file location. If necessary, the category corresponding to Identifier
   --  is created.
   --  If Highlight is True, the corresponding line will be highlighted in
   --  the editor with the highlighting category identified by Identifier.

   procedure Remove_Category
     (View          : access Result_View_Record'Class;
      Identifier    : String);
   --  Remove category Identifier from the view. All corresponding marks
   --  are deleted.

   procedure Next_Item
     (View      : access Result_View_Record'Class;
      Backwards : Boolean := False);
   --  If an item if selected, jump to the location pointed to by the iter
   --  immediately following it in the same category. If there is none, jump
   --  to the first item in the category.

   procedure Add_Action_Item
     (View          : access Result_View_Record'Class;
      Identifier    : String;
      Category      : String;
      File          : VFS.Virtual_File;
      Line          : Natural;
      Column        : Natural;
      Message       : String;
      Action        : Action_Item);
   --  Add an action item to be associated to a specified location.
   --  If Action is null, the action item will be removed from that location.

private
   type Result_View_Record is new Gtk_Hbox_Record with record
      Kernel : Kernel_Handle;
      Tree   : Tree_View;

      Non_Leaf_Color : Gdk.Color.Gdk_Color;
      Leaf_Color     : Gdk.Color.Gdk_Color;
      --  The color to use in the first column, depending on the status of the
      --  line.

      Category_Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf;
      File_Pixbuf     : Gdk.Pixbuf.Gdk_Pixbuf;

      Action_Column   : Gtk_Tree_View_Column;
   end record;

end Glide_Result_View;
