-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
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

--  This package provides a CVS object implementating the VCS abstract
--  specification.
--
--  See package VCS for a complete spec of this package.

package VCS.CVS is

   type CVS_Record is new VCS_Record with private;
   --  A value used to reference a CVS repository.

   type CVS_Access is access all CVS_Record'Class;

   function Name (Ref : access CVS_Record) return String;

   procedure Free (Ref : access CVS_Record);

   procedure Get_Status
     (Rep         : access CVS_Record;
      Filenames   : String_List.List);

   function Local_Get_Status
     (Rep       : access CVS_Record;
      Filenames : String_List.List)
     return File_Status_List.List;

   procedure Open
     (Rep       : access CVS_Record;
      Filenames : String_List.List;
      User_Name : String := "");

   procedure Commit
     (Rep       : access CVS_Record;
      Filenames : String_List.List;
      Logs      : String_List.List);

   procedure Update
     (Rep       : access CVS_Record;
      Filenames : String_List.List);

   procedure Merge
     (Rep       : access CVS_Record;
      Filenames : String_List.List);

   procedure Add
     (Rep       : access CVS_Record;
      Filenames : String_List.List);

   procedure Remove
     (Rep       : access CVS_Record;
      Filenames : String_List.List);

   procedure Revert
     (Rep       : access CVS_Record;
      Filenames : String_List.List);

   procedure Diff
     (Rep       : access CVS_Record;
      File      : String;
      Version_1 : String := "";
      Version_2 : String := "");

   procedure Log
     (Rep  : access CVS_Record;
      File : String);

   procedure Annotate
     (Rep  : access CVS_Record;
      File : String);

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Register the VCS.CVS module

private
   type CVS_Record is new VCS_Record with null record;

end VCS.CVS;
