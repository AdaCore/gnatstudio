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

with Gtk.Box;                  use Gtk.Box;
with Gtk.Tree_View;            use Gtk.Tree_View;
with Gtk.Tree_Store;           use Gtk.Tree_Store;

with Glide_Kernel;             use Glide_Kernel;
with Glide_Kernel.Console;     use Glide_Kernel.Console;

with Prj;
with String_List_Utils;        use String_List_Utils;

with VCS;                      use VCS;

package VCS_View_Pkg is

   type VCS_View_Record;
   type VCS_View_Access is access all VCS_View_Record'Class;

   type VCS_View_Record is new Gtk_Hbox_Record with record
      Tree   : Gtk_Tree_View;
      Model  : Gtk_Tree_Store;

      Kernel : Kernel_Handle;
      --  Reference to the Glide kernel that launched the explorer, if any.

      Hide_Up_To_Date : Boolean := False;
      --  Whether up-to-date and unknown files should be hidden in the view.

      Hide_Not_Registered : Boolean := False;
      --  Whether files that are not registered should be hidden in the view.

      Stored_Status   : File_Status_List.List;
      Cached_Status   : File_Status_List.List;
   end record;

   procedure Gtk_New
     (VCS_View : out VCS_View_Access;
      Kernel   : Kernel_Handle := null);
   --  Create a new VCS explorer.

   procedure Initialize
     (VCS_View : access VCS_View_Record'Class;
      Kernel   : Kernel_Handle);
   --  Internal initialization function.

   procedure Clear (Explorer : VCS_View_Access);
   --  Clear all the files in the model.

   procedure Display_File_Status
     (Kernel         : Kernel_Handle;
      Status         : File_Status_List.List;
      Override_Cache : Boolean;
      Force_Display  : Boolean := False);
   --  Display Status in the explorer.
   --  Status must be freed by the user after calling this function.
   --  If Override_Cache is set to True, then the cache will be updated
   --  with the new status information, if needed. Otherwise, the values from
   --  the cache will be used for displaying the files.
   --  If Force_Display is True, then files that are not already visible
   --  will be added to the list. Otherwise this function only updates
   --  status for files that are currently visible in the view.

   function Get_Selected_Files
     (Explorer : VCS_View_Access) return String_List.List;
   --  Return the list of files that are selected.

   procedure Display_String_List
     (Kernel   : Kernel_Handle;
      List     : String_List.List;
      M_Type   : Message_Type := Verbose);
   --  Convenience procedure to output a String_List.List.
   --  One of Explorer or Kernel can be Null.

   procedure Push_Message
     (Kernel   : Kernel_Handle;
      M_Type   : Message_Type;
      Message  : String);
   --  Display a message.

   function Get_Current_Dir
     (Kernel : access Kernel_Handle_Record'Class)
     return String;
   --  Convenience function to get the current directory.

   function Get_Current_File (Kernel : Kernel_Handle) return String;
   --  Convenience function to get the current file.

   function Get_Current_Ref
     (Kernel : access Kernel_Handle_Record'Class)
     return VCS_Access;
   --  Convenience function to get the current file.

   function Get_Selected_Files
     (Kernel : Kernel_Handle) return String_List.List;
   --  Return the currently selected files, as a list.
   --  Caller must free this list afterwards.

   function Get_Explorer (Kernel : Kernel_Handle) return VCS_View_Access;
   --  Return the vcs explorer, if created, null otherwise.

   function Get_Files_In_Project
     (Project_View : Prj.Project_Id) return String_List.List;

   function Get_Dirs_In_Project
     (Kernel : Kernel_Handle) return String_List.List;

end VCS_View_Pkg;
