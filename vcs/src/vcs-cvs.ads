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

--  This package provides a CVS object implementating the VCS abstract
--  specification.
--
--  See package VCS for a complete spec of this package.

-- generic
--    type Data_Type is private;

package VCS.CVS is

   type CVS_Record is new VCS_Record with private;
   --  A value used to reference a CVS repository.

   type CVS_Access is access all CVS_Record'Class;

   function Get_Status
     (Rep         : access CVS_Record;
      Filenames   : String_List.List;
      Get_Status  : Boolean          := True;
      Get_Version : Boolean          := True;
      Get_Tags    : Boolean          := False;
      Get_Users   : Boolean          := False)
     return File_Status_List.List;

   function Local_Get_Status
     (Rep       : access CVS_Record;
      Filenames :        String_List.List)
     return File_Status_List.List;

   procedure Open
     (Rep       : access CVS_Record;
      Filenames :        String_List.List;
      User_Name :        String           := "");

   procedure Commit
     (Rep       : access CVS_Record;
      Filenames :        String_List.List;
      Logs      :        String_List.List);

   procedure Update
     (Rep       : access CVS_Record;
      Filenames :        String_List.List);

   procedure Merge
     (Rep       : access CVS_Record;
      Filenames :        String_List.List);

   procedure Add
     (Rep       : access CVS_Record;
      Filenames :        String_List.List);

   procedure Remove
     (Rep       : access CVS_Record;
      Filenames :        String_List.List);

   function Diff
     (Rep       : access CVS_Record;
      File      :        String;
      Version_1 :        String     := "";
      Version_2 :        String     := "")
     return String_List.List;

   function Log
     (Rep  : access CVS_Record;
      File :        String)
      return String_List.List;

   function Annotate
     (Rep  : access CVS_Record;
      File :        String)
      return String_List.List;

   function Success (Rep : access CVS_Record) return Boolean;

   function Get_Message (Rep : access CVS_Record) return String;

   procedure Register_Idle_Function
     (Rep     : access CVS_Record;
      Func    : Idle_Function;
      Timeout : Integer := 200);

private
   type Parameterless_Procedure is access procedure;

   type CVS_Record is new VCS_Record with record
      Success              : Boolean := True;
      Message              : String_List.List;
      Local_Idle_Function  : Idle_Function := null;
      Timeout              : Integer := 200;
   end record;
end VCS.CVS;
