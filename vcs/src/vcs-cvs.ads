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

--  This package provides a CVS object implementating the VCS abstract
--  specification.
--
--  See package VCS for a complete spec of this package.

with GNAT.Expect;               use GNAT.Expect;
with Gtk.Main;                  use Gtk.Main;
with Generic_List;
with Unchecked_Deallocation;

package VCS.CVS is

   type CVS_Record is new VCS_Record with private;
   --  A value used to reference a CVS repository.

   type CVS_Access is access all CVS_Record'Class;

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

   procedure Register_Module;

private
   type String_List_Handler is access
     procedure (Kernel : Kernel_Handle;
                Head   : String_List.List;
                List   : String_List.List);

   type String_List_And_Handler is record
      Rep     : CVS_Access;
      Head    : String_List.List;
      List    : String_List.List;
      Handler : String_List_Handler;
   end record;

   type String_List_And_Handler_Access is access String_List_And_Handler;

   procedure Destroy (D : in String_List_And_Handler_Access);

   procedure Free is new Unchecked_Deallocation
     (String_List_And_Handler, String_List_And_Handler_Access);

   package String_List_Idle is
      new Gtk.Main.Timeout (String_List_And_Handler_Access);

   type Command_Record is record
      Command : String_List.List;
      Dir     : String_List.List;
      Args    : String_List.List;
      Head    : String_List.List;
      Handler : String_List_Handler;
   end record;

   procedure Free (D : in out Command_Record);

   package Command_List is new Generic_List (Command_Record);

   type CVS_Record is new VCS_Record with record
      Command_In_Progress : Boolean := False;
      Command_Queue       : Command_List.List;
      Fd                  : Process_Descriptor;
   end record;

end VCS.CVS;
