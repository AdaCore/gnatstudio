-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2005                       --
--                              AdaCore                              --
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

with Ada.Unchecked_Deallocation;

with String_Hash;

with VCS;          use VCS;

package VCS_View.Explorer is

   type VCS_Explorer_View_Record is new VCS_View_Record with private;
   type VCS_Explorer_View_Access is access all VCS_Explorer_View_Record'Class;

   procedure Gtk_New
     (Explorer : out VCS_Explorer_View_Access;
      Kernel   : Kernel_Handle := null);
   --  Create a new VCS explorer

   procedure Display_File_Status
     (Kernel         : Kernel_Handle;
      Status         : File_Status_List.List;
      VCS_Identifier : VCS_Access;
      Override_Cache : Boolean;
      Force_Display  : Boolean := False;
      Clear_Logs     : Boolean := False;
      Display        : Boolean := True);
   --  Display Status in the explorer.
   --  Status must be freed by the user after calling this function.
   --  If Override_Cache is set to True, then the cache will be updated
   --  with the new status information, if needed. Otherwise, the values from
   --  the cache will be used for displaying the files.
   --  If Force_Display is True, then files that are not already visible
   --  will be added to the list. Otherwise this function only updates
   --  status for files that are currently visible in the view.
   --  If Clear_Logs is True, then the log files corresponding
   --  to the files that have an "Up_To_Date" status will be deleted,
   --  as well as the corresponding file/log mapping for those files.
   --  If Display is False, do not display the status but only refresh the
   --  cache.

   function Get_Selected_Files
     (Kernel : Kernel_Handle) return String_List.List;
   --  Return the currently selected files, as a list.
   --  Caller must free this list afterwards.

   procedure On_Remove_Project
     (Explorer : access VCS_Explorer_View_Record;
      Project  : String);
   --  Remove Project from the explorer

private

   type Status_Record is record
      Status  : File_Status;
      Display : Boolean := True;
   end record;

   type Status_Array is array (Natural range <>) of Status_Record;
   type Status_Array_Access is access Status_Array;

   procedure Free is new Ada.Unchecked_Deallocation
     (Status_Array, Status_Array_Access);

   package Status_Hash is new String_Hash (Status_Array_Access, Free, null);
   use Status_Hash.String_Hash_Table;
   --  Store for each file the current status. This is a cache to avoid sending
   --  requests to the VCS.

   procedure Free (X : in out Natural);

   package File_Hash is new String_Hash (Natural, Free, 1);
   use File_Hash.String_Hash_Table;

   type VCS_Explorer_View_Record is new VCS_View_Record with record
      VCS    : VCS_Access;
      --  Current VCS handled

      Status : Status_Hash.String_Hash_Table.HTable;
      --  Recorded status for each VCS

      Hidden : File_Hash.String_Hash_Table.HTable;
      --  ??? A table to record all files that does not match the current
      --  filter. This table is parsed every time the filter is changed. This
      --  will be replaced when the Gtk_Tree_View will support invisible line.
      --  In this case it will be more space-efficient.
   end record;

   function Columns_Types
     (Explorer : access VCS_Explorer_View_Record) return GType_Array;

   procedure Do_Delete (Explorer : VCS_Explorer_View_Record);
   --  ???

   procedure Do_Refresh (Explorer : access VCS_Explorer_View_Record);
   --  ???

   procedure Do_Fill_Info
     (Explorer  : VCS_Explorer_View_Record;
      Iter      : Gtk_Tree_Iter;
      Line_Info : Line_Record;
      Success   : out Boolean);
   --  ???

   procedure Do_Initialize
     (Explorer : access VCS_Explorer_View_Record;
      Kernel   : Kernel_Handle);
   --  ???

end VCS_View.Explorer;
