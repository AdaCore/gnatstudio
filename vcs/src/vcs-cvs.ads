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

with Ada.Text_IO;

--  This package provide a CVS object implementating the VCS abstract
--  specification.
--
--  See package VCS for a complete spec of this package.

package VCS.CVS is

   type CVS_Record is new VCS_Record with private;
   --  A value used to reference a CVS repository.

   type CVS_Access is access all CVS_Record'Class;

   procedure Open (Rep : access CVS_Record; Dir_Name : in String);

   procedure Close (Rep : access CVS_Record);

   function Is_Open (Rep : access CVS_Record) return Boolean;

   procedure Read
     (Rep : access CVS_Record;
      Ent : out VCS_Entry);

   procedure Add (Rep : access CVS_Record; Name : String);

   procedure Remove (Rep : access CVS_Record; Name : String);

   procedure Commit
     (Rep  : access CVS_Record;
      Name : String;
      Log  : String);

   procedure Checkout (Rep : access CVS_Record; Name : String);

private
   type CVS_Record is new VCS_Record with record
      Dir : Ada.Text_IO.File_Type;
   end record;
end VCS.CVS;
