-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2003                         --
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

--  This package provides an generic derivation of VCS_Record.
--
--  See package VCS for a complete spec of this package.

package VCS.Generic_VCS is

   type Generic_VCS_Record is new VCS_Record with private;
   --  A value used to reference a Generic_VCS repository.

   type Generic_VCS_Access is access all Generic_VCS_Record'Class;

   function Name (Ref : access Generic_VCS_Record) return String;

   procedure Free (Ref : access Generic_VCS_Record);

   procedure Get_Status
     (Rep         : access Generic_VCS_Record;
      Filenames   : String_List.List;
      Clear_Logs  : Boolean := False);

   function Local_Get_Status
     (Rep       : access Generic_VCS_Record;
      Filenames : String_List.List)
     return File_Status_List.List;

   procedure Open
     (Rep       : access Generic_VCS_Record;
      Filenames : String_List.List;
      User_Name : String := "");

   procedure Commit
     (Rep       : access Generic_VCS_Record;
      Filenames : String_List.List;
      Logs      : String_List.List);

   procedure Update
     (Rep       : access Generic_VCS_Record;
      Filenames : String_List.List);

   procedure Merge
     (Rep       : access Generic_VCS_Record;
      Filenames : String_List.List);

   procedure Add
     (Rep       : access Generic_VCS_Record;
      Filenames : String_List.List);

   procedure Remove
     (Rep       : access Generic_VCS_Record;
      Filenames : String_List.List);

   procedure Revert
     (Rep       : access Generic_VCS_Record;
      Filenames : String_List.List);

   procedure Diff
     (Rep       : access Generic_VCS_Record;
      File      : VFS.Virtual_File;
      Version_1 : String := "";
      Version_2 : String := "");

   procedure Log
     (Rep  : access Generic_VCS_Record;
      File : VFS.Virtual_File;
      Rev  : String);

   procedure Annotate
     (Rep  : access Generic_VCS_Record;
      File : VFS.Virtual_File);

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Register the VCS.Generic_VCS module

private

   type Generic_VCS_Record is new VCS_Record with record
      null;
   end record;

end VCS.Generic_VCS;
