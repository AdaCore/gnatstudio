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

with Gtk.Box;    use Gtk.Box;

with Gtk.Tree_View;  use Gtk.Tree_View;
with Gtk.Tree_Store; use Gtk.Tree_Store;

with Glide_Kernel;            use Glide_Kernel;
with Glide_Kernel.Console;    use Glide_Kernel.Console;

with String_List;
with VCS; use VCS;

package VCS_View_Pkg is

   type VCS_View_Record;
   type VCS_View_Access is access all VCS_View_Record'Class;

   type VCS_View_Record is new Gtk_Hbox_Record with record
      Tree   : Gtk_Tree_View;
      Model  : Gtk_Tree_Store;

      Kernel : Kernel_Handle;
      --  Reference to the Glide kernel that launched the explorer, if any.

      Show_All : Boolean := False;

      Stored_Status : File_Status_List.List;
      Cached_Status : File_Status_List.List;
   end record;

   procedure Gtk_New (VCS_View : out VCS_View_Access;
                      Kernel   : Kernel_Handle := null;
                      Ref      : VCS_Access);

   procedure Initialize (VCS_View : access VCS_View_Record'Class);

   procedure Clear (Explorer : VCS_View_Access);
   --  Clear all the files in the model.

   procedure Display_File_Status
     (Kernel         : Kernel_Handle;
      Status         : File_Status_List.List;
      Override_Cache : Boolean);
   --  Display Status in the explorer. Status must NOT be freed by the user.
   --  Memory allocated to this list is freed when calling Clear.
   --
   --  If Override_Cache is set to True, then the cache will be updated
   --  with the new status information, if needed. Otherwise, the values from
   --  the cache will be used for displaying the files.

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

   procedure Edit_Log
     (Explorer : VCS_View_Access;
      Kernel   : Kernel_Handle;
      Files    : String_List.List;
      Ref      : VCS_Access);
   --  Launch log editors for these files.
   --  User must free Files afterwards.

end VCS_View_Pkg;
