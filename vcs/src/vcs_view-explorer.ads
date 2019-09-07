------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;

with String_Hash;
with GPS.Kernel.MDI;  use GPS.Kernel.MDI;
with VCS;             use VCS;
with VCS.Unknown_VCS; use VCS.Unknown_VCS;

package VCS_View.Explorer is

   type VCS_Explorer_View_Record is new VCS_View_Record with private;
   type VCS_Explorer_View_Access is access all VCS_Explorer_View_Record'Class;

   procedure Gtk_New
     (Explorer : out VCS_Explorer_View_Access;
      Kernel   : access Kernel_Handle_Record'Class := null);
   --  Create a new VCS explorer

   procedure Display_File_Status
     (Kernel         : not null access Kernel_Handle_Record'Class;
      Status         : File_Status_List.Vector;
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
     (Kernel : Kernel_Handle) return File_Array_Access;
   --  Return the currently selected files, as a list.
   --  Caller must free this list afterwards.

   procedure On_Remove_Project
     (Explorer : access VCS_Explorer_View_Record;
      Project  : String);
   --  Remove Project from the explorer

   procedure No_VCS_Message (Explorer : VCS_Explorer_View_Access);
   --  Display a message stating that no VCS is defined for the project

   overriding function Build_View_Context
     (Explorer : not null access VCS_Explorer_View_Record;
      Event : Gdk.Event.Gdk_Event)
      return Selection_Context;
   --  Describe the current selection

private

   type Status_Record is record
      Status  : VCS_File_Status;
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

   procedure Free (X : in out Natural) is null;

   package File_Hash is new String_Hash (Natural, Free, 1);
   use File_Hash.String_Hash_Table;

   type VCS_Explorer_View_Record is new VCS_View_Record with record
      VCS    : VCS_Access := Unknown_VCS_Reference;
      --  Current VCS handled

      Status : Status_Hash.String_Hash_Table.Instance;
      --  Recorded status for each VCS

      Hidden : File_Hash.String_Hash_Table.Instance;
      --  ??? A table to record all files that does not match the current
      --  filter. This table is parsed every time the filter is changed. This
      --  will be replaced when the Gtk_Tree_View will support invisible line.
      --  In this case it will be more space-efficient.
   end record;

   overriding function Columns_Types
     (Explorer : access VCS_Explorer_View_Record) return GType_Array;
   overriding procedure Do_Delete (Explorer : VCS_Explorer_View_Record);
   overriding procedure Do_Refresh
     (Explorer : access VCS_Explorer_View_Record);
   overriding procedure Do_Fill_Info
     (Explorer  : VCS_Explorer_View_Record;
      Iter      : Gtk_Tree_Iter;
      Line_Info : Line_Record;
      Success   : out Boolean);
   overriding procedure Do_Initialize
     (Explorer : access VCS_Explorer_View_Record;
      Kernel   : Kernel_Handle);
   overriding procedure On_Destroy (Self : in out VCS_Explorer_View_Record);
   --  See inherited documentation

end VCS_View.Explorer;
