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

with Gtk.Widget; use Gtk.Widget;
with Gtk.Text;   use Gtk.Text;
with Gtk.Box;    use Gtk.Box;

with Gtk.Tree_View;  use Gtk.Tree_View;
with Gtk.Tree_Store; use Gtk.Tree_Store;

with GNAT.OS_Lib;     use GNAT.OS_Lib;

with Glide_Kernel;            use Glide_Kernel;
with Glide_Kernel.Console;    use Glide_Kernel.Console;

with String_List;
with VCS; use VCS;

package VCS_View_Pkg is

   type VCS_View_Record;
   type VCS_View_Access is access all VCS_View_Record'Class;

   type VCS_View_Record is new Gtk_Hbox_Record with record
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
      --  When in independant mode, this acts as a console for displaying text.

      Kernel                     : Kernel_Handle;
      --  Reference to the Glide kernel that launched the explorer, if any.
   end record;

   procedure Gtk_New (VCS_View : out VCS_View_Access;
                      Kernel   : Kernel_Handle := null;
                      Ref      : VCS_Access);

   procedure Initialize (VCS_View : access VCS_View_Record'Class);

   procedure Show_Files (Explorer  : VCS_View_Access;
                         Directory : String;
                         Ref       : VCS_Access);

   function Get_Selected_Files
     (Explorer : VCS_View_Access) return String_List.List;
   --  Return the list of files that are selected.

   procedure Get_Status
     (Explorer : VCS_View_Access;
      Files    : String_List.List);
   --  Updates the status for Files.

   procedure Open_Files
     (Explorer : VCS_View_Access;
      Kernel   : Kernel_Handle;
      Files    : String_List.List;
      Ref      : VCS_Access);
   --  Open a list of files.
   --  User must free Files afterwards.

   procedure Commit
     (Explorer : VCS_View_Access;
      Kernel   : Kernel_Handle;
      Files    : String_List.List;
      Log      : String;
      Ref      : VCS_Access);
   --  Commit a list of files with a given log.
   --  User must free Files afterwards.

   procedure Edit_Log
     (Explorer : VCS_View_Access;
      Kernel   : Kernel_Handle;
      Files    : String_List.List;
      Ref      : VCS_Access);
   --  Launch log editors for these files.
   --  User must free Files afterwards.

   procedure Diff_Files
     (Explorer : VCS_View_Access;
      Kernel   : Kernel_Handle;
      Files    : String_List.List;
      Ref      : VCS_Access);
   --  View differences between Files and the head revision.
   --  User must free Files afterwards.

   procedure Update_File_List
     (Explorer : VCS_View_Access;
      Kernel   : Kernel_Handle;
      Files    : String_List.List;
      Ref      : VCS_Access);
   --  Updates a list of files.
   --  User must free Files afterwards.

   procedure Push_Message
     (Explorer : VCS_View_Access;
      Kernel   : Kernel_Handle;
      M_Type   : Message_Type;
      Message  : String);
   --  Display a message.

   procedure Display_String_List
     (Explorer : VCS_View_Access;
      Kernel   : Kernel_Handle;
      List     : String_List.List;
      M_Type   : Message_Type := Verbose);
   --  Convenience procedure to output a String_List.List.
   --  One of Explorer or Kernel can be Null.
end VCS_View_Pkg;
