-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2005-2006                      --
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

with GPS.Kernel;         use GPS.Kernel;
with GPS.Kernel.Scripts; use GPS.Kernel.Scripts;
with String_List_Utils;  use String_List_Utils;
with VFS;                use VFS;
with VCS;                use VCS;

package VCS_Activities is

   type Activity_Id is private;

   No_Activity : constant Activity_Id;

   function Image (Activity : Activity_Id) return String;
   --  Return the Activity string representation

   function Value (Str : String) return Activity_Id;
   --  Return the activity given its string representation

   procedure Load_Activities (Kernel : access Kernel_Handle_Record'Class);
   --  Read the custom activities file

   procedure Save_Activities (Kernel : access Kernel_Handle_Record'Class);
   --  Save all activities information

   function New_Activity
     (Kernel : access Kernel_Handle_Record'Class) return Activity_Id;
   --  Create a new activity with a uniq id

   procedure Delete_Activity
     (Kernel : access Kernel_Handle_Record'Class; Activity : Activity_Id);
   --  Create a new activity with a uniq id

   function First return Activity_Id;
   --  Returns the first activity or No_Activity if there is no activity

   function Next return Activity_Id;
   --  Returns the next activity or No_Activity if we have reached the last

   procedure Set_Instance (Activity : Activity_Id; Instance : Class_Instance);
   --  Sets the activity's class instance

   function Get_Instance (Activity : Activity_Id) return Class_Instance;
   --  Returns the activity's class instance

   function Get_Name (Activity : Activity_Id) return String;
   --  Returns the name for this activity or the empty string if No_Activity

   procedure Set_Name
     (Kernel   : access Kernel_Handle_Record'Class;
      Activity : Activity_Id;
      Name     : String);
   --  Set the name of the activity

   procedure Set_Closed
     (Kernel   : access Kernel_Handle_Record'Class;
      Activity : Activity_Id;
      To       : Boolean);
   --  Set the committed status for this activity

   function Is_Closed (Activity : Activity_Id) return Boolean;
   --  Returns the committed status for this activity

   procedure Toggle_Closed_Status
     (Kernel   : access Kernel_Handle_Record'Class;
      Activity : Activity_Id);
   --  Invert the committed status

   function Has_Log
     (Kernel   : Kernel_Handle;
      Activity : Activity_Id) return Boolean;
   --  Returns True if this activity has a log file

   function Get_Log_File
     (Kernel   : Kernel_Handle;
      Activity : Activity_Id) return Virtual_File;
   --  Retruns the log file for the given activity

   function Get_Log
     (Kernel   : Kernel_Handle;
      Activity : Activity_Id) return String;
   --  Returns the Log for the activity

   function Get_Project_Path (Activity : Activity_Id) return VFS.Virtual_File;
   --  Retruns the project full pathname for this activity

   function Get_File_Activity (File : VFS.Virtual_File) return Activity_Id;
   --  Returns the Activity_Id for File or No_Activity if File is not
   --  part of an activity.

   function Get_Files_In_Activity
     (Activity : Activity_Id) return String_List.List;
   --  Returns the list of files in the given activity. The list must not be
   --  freed.

   function Get_VCS_For_Activity
     (Kernel   : access Kernel_Handle_Record'Class;
      Activity : Activity_Id) return VCS_Access;
   --  Returns the VCS to be used to handle this activity. Note that the API
   --  ensures that a single VCS is used for all files in an activity.

   procedure Add_File
     (Kernel   : access Kernel_Handle_Record'Class;
      Activity : Activity_Id;
      File     : Virtual_File);
   --  Add file into the activity, does nothing if the file already exists

   procedure Remove_File
     (Kernel   : access Kernel_Handle_Record'Class;
      Activity : Activity_Id;
      File     : Virtual_File);
   --  Remove file into the activity, does nothing if the file is not present
   --  into this activity.

   function Get_Group_Commit (Activity : Activity_Id) return Boolean;
   --  Returns the group-commit status

   procedure Toggle_Group_Commit
     (Kernel   : access Kernel_Handle_Record'Class;
      Activity : Activity_Id);
   --  Invert the group-commit status

private

   type Activity_Id is new String (1 .. 17);
   --  17 is the length of the time-stamp id used in the implementation

   No_Activity : constant Activity_Id := (others => ' ');

end VCS_Activities;
