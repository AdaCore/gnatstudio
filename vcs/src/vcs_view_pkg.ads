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

with Gtk.Window; use Gtk.Window;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Text;   use Gtk.Text;

with Gtk.Tree_View;  use Gtk.Tree_View;
with Gtk.Tree_Store; use Gtk.Tree_Store;

with GNAT.OS_Lib;     use GNAT.OS_Lib;

with Glide_Kernel;    use Glide_Kernel;

with VCS; use VCS;

package VCS_View_Pkg is

   type VCS_View_Record;
   type VCS_View_Access is access all VCS_View_Record'Class;

   type VCS_View_Record is new Gtk_Window_Record with record
      Current_Directory : String_Access;
      --  The directory that is currently being viewed.
      --  It must be an absolute directory name ending
      --  with Directory_Separator.

      Tree  : Gtk_Tree_View;
      Model : Gtk_Tree_Store;

      Model_Sync : Boolean := False;
      --  This boolean indicates whether the model is currently
      --  being synchronized with the view.

      All_Selected : Boolean := False;
      --  Indicates whether all the files in the view are selected.

      VCS_Ref : VCS_Access := null;

      Edit_Log_Button            : Gtk_Widget;
      Edit_Multiple_Log_Button   : Gtk_Widget;
      View_Log_Button            : Gtk_Widget;
      View_Diff_Button           : Gtk_Widget;
      Annotate_Button            : Gtk_Widget;
      Get_Status_Button          : Gtk_Widget;
      Update_Button              : Gtk_Widget;
      Open_Button                : Gtk_Widget;
      Commit_Button              : Gtk_Widget;
      Revert_Button              : Gtk_Widget;
      Add_Button                 : Gtk_Widget;
      Remove_Button              : Gtk_Widget;

      Message_Text               : Gtk_Text;

      Kernel                     : Kernel_Handle;
      --  Reference to the Glide kernel that launched the explorer, if any.
   end record;

   procedure Gtk_New (VCS_View : out VCS_View_Access;
                      Kernel   : Kernel_Handle := null);
   procedure Initialize (VCS_View : access VCS_View_Record'Class);

   procedure Show_Files (Explorer  : VCS_View_Access;
                         Directory : String);
   --

end VCS_View_Pkg;
