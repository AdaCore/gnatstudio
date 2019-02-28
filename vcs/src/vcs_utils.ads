------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2019, AdaCore                     --
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

with GNATCOLL.VFS;      use GNATCOLL.VFS;

with GPS.Kernel;        use GPS.Kernel;
with VCS;               use VCS;
with VCS_Activities;    use VCS_Activities;

package VCS_Utils is

   procedure Display_Editor_Status
     (Kernel : access Kernel_Handle_Record'Class;
      Ref    : VCS_Access;
      Status : File_Status_Record);
   --  Display a label corresponding to the file. If the file is not open,
   --  do nothing.

   function Save_Files
     (Kernel    : not null access Kernel_Handle_Record'Class;
      Files     : File_Array;
      Activity  : Activity_Id := No_Activity;
      Save_Logs : Boolean     := False) return Boolean;
   --  Ask the user whether he wants to save the file editors for Files.
   --  Return False if the user has cancelled the action.

   function Get_Current_Dir
     (Context : Selection_Context) return GNATCOLL.VFS.Virtual_File;
   --  Convenience function to get the current directory

   function Get_Current_File
     (Context : Selection_Context) return GNATCOLL.VFS.Virtual_File;
   --  Convenience function to get the current file

   procedure Update_Files_Status
     (Kernel         : not null access Kernel_Handle_Record'Class;
      Status         : File_Status_List.Vector;
      VCS_Identifier : VCS_Access;
      Clear_Logs     : Boolean;
      Up_To_Date     : VCS_File_Status);
   --  For all files in Status, remove the log file if Clear_Logs is set and
   --  the file has the Up_To_Date status. Also update the editor status.

   function Revision_Lower (Rev1, Rev2 : String) return Boolean;
   --  Return True if Rev1 is lower than Rev2

end VCS_Utils;
