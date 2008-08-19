-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2006-2008, AdaCore                 --
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

--  This package provides an Ada-specific completer

package Completion.Ada is

   type Ada_Completion_Manager is new Completion_Manager with private;

   function Get_Initial_Completion_List
     (Manager : access Ada_Completion_Manager; Context : Completion_Context)
      return Completion_List;
   --  See inherited documentation

private

   type Ada_Completion_Manager is new Completion_Manager with record
      null;
   end record;

   type Ada_Completion_Context is new Completion_Context_Record with
      record
         Expression : Parsed_Expression;
      end record;

end Completion.Ada;
