-----------------------------------------------------------------------
--                           GLIDE II                                --
--                     Copyright (C) 2001                            --
--                          ACT-Europe                               --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

package VCS is

   type VCS_Record is abstract tagged limited private;
   --  A value used to reference a VCS repository. Conceptually this value
   --  includes the identity of the repository, and a sequential position
   --  within it.

   type VCS_Access is access all VCS_Record'Class;

   type VCS_Entry is record
      Is_Dir       : Boolean;
      Filename     : String (1 .. 1024);
      Filename_Len : Natural;
      Version      : String (1 .. 32);
      Version_Len  : Natural;
      Date         : String (1 .. 32);
      Date_Len     : Natural;
   end record;

   procedure Open (Rep : access VCS_Record; Dir_Name : in String) is abstract;
   --  Opens the directory named by Dir_Name and returns a VCS_Record value
   --  that refers to this directory, and is positioned at the first entry.
   --
   --  Raises Name_Error if Dir_Name cannot be accessed.

   procedure Close (Rep : access VCS_Record) is abstract;
   --  Closes the VCS stream referred to by Dir. After calling Close
   --  Is_Open will return False.
   --
   --  Raises Use_Error if Dir has not be opened.

   function Is_Open (Rep : access VCS_Record) return Boolean is abstract;
   --  Returns True if Dir is open, or False otherwise.

   procedure Read
     (Rep : access VCS_Record;
      Ent : out VCS_Entry) is abstract;
   --  Reads the next entry from the VCS repository and sets Ent to the
   --  current entry. Ent.Filename_Len is 0 when there is no more entry in the
   --  repository.
   --
   --  Raises Use_Error if Rep has not be opened.

   procedure Add (Rep : access VCS_Record; Name : String) is abstract;
   --  Add a given file/directory name in the specified VCS repository

   procedure Remove (Rep : access VCS_Record; Name : String) is abstract;
   --  Remove a given file/directory name from the specified VCS repository

   procedure Commit
     (Rep  : access VCS_Record;
      Name : String;
      Log  : String) is abstract;
   --  Check a file Name in the specified repository.
   --  Log is used as the revision history.

   procedure Checkout (Rep : access VCS_Record; Name : String) is abstract;
   --  Check a file out the specified repository.

   --  missing:
   --  annotate
   --  diff
   --  init
   --  log
   --  status
   --  tag

private
   type VCS_Record is abstract tagged limited null record;
end VCS;
