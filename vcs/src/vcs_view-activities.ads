------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2018, AdaCore                     --
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

with GPS.Kernel.MDI; use GPS.Kernel.MDI;
with VCS;            use VCS;
with VCS_Activities; use VCS_Activities;

package VCS_View.Activities is

   type VCS_Activities_View_Record is new VCS_View_Record with private;
   type VCS_Activities_View_Access is
     access all VCS_Activities_View_Record'Class;

   procedure Gtk_New
     (Explorer : out VCS_Activities_View_Access;
      Kernel   : access Kernel_Handle_Record'Class := null);
   --  Create a new VCS explorer

   overriding
   function Columns_Types
     (Explorer : access VCS_Activities_View_Record) return GType_Array;
   --  Redefined for the VCS Activities

   procedure Display_File_Status
     (Kernel         : not null access Kernel_Handle_Record'Class;
      Activity       : Activity_Id;
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

   procedure On_Create_Activity (Kernel : Kernel_Handle);
   --  Create the given activity in the explorer, set the new activity in
   --  editing mode.

   procedure On_Delete_Activity
     (Kernel   : not null access Kernel_Handle_Record'Class;
      Activity : Activity_Id);
   --  Delete the given activity from the explorer

   procedure On_Close_Open_Activity
     (Kernel   : not null access Kernel_Handle_Record'Class;
      Activity : Activity_Id);
   --  Change Activity's status to/from closed/opened

   procedure On_Remove_From_Activity
     (Kernel : Kernel_Handle; Activity : Activity_Id; File : Virtual_File);
   --  Remove file from the given activity

   function Get_Selected_Files
     (Kernel : Kernel_Handle) return File_Array_Access;
   --  Return the currently selected files, as a list.
   --  Caller must free this list afterwards.

   overriding function Build_View_Context
     (Explorer : not null access VCS_Activities_View_Record;
      Event : Gdk.Event.Gdk_Event)
      return Selection_Context;
   --  Describe the current selection

private

   type VCS_Activities_View_Record is new VCS_View_Record with null record;
   overriding procedure Do_Delete (Explorer : VCS_Activities_View_Record);
   overriding procedure Do_Refresh
     (Explorer : access VCS_Activities_View_Record);
   overriding procedure Do_Fill_Info
     (Explorer  : VCS_Activities_View_Record;
      Iter      : Gtk_Tree_Iter;
      Line_Info : Line_Record;
      Success   : out Boolean);
   overriding procedure Do_Initialize
     (Explorer : access VCS_Activities_View_Record;
      Kernel   : Kernel_Handle);
   --  See inherited documentation

end VCS_View.Activities;
