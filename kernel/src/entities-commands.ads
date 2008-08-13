-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                         Copyright (C) 2005                        --
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

--  This package contains various subprograms to do highlevel xref queries
--  in background mode. In particular, it is used for the call-graph related
--  queries.

with GPS.Kernel;
with Gtk.Widget;

package Entities.Commands is

   ---------------
   -- User data --
   ---------------

   type Commands_User_Data_Record is abstract tagged null record;
   type Commands_User_Data is access all Commands_User_Data_Record'Class;

   procedure Destroy
     (Data : in out Commands_User_Data_Record; Cancelled : Boolean);
   --  Called when the user data is no longer needed (ie at the end of the
   --  search or when the search has been cancelled).
   --  Cancelled is set to True if the search didn't complete. It is possible
   --  that the "Watch" widget passed to Examine_Ancestors_Call_Graph was
   --  destroyed and is no longer valid

   function On_Entity_Found
     (Data                : access Commands_User_Data_Record;
      Entity              : Entities.Entity_Information;
      Parent              : Entities.Entity_Information;
      Ref                 : Entities.Entity_Reference;
      Through_Dispatching : Boolean;
      Is_Renaming         : Boolean) return Boolean is abstract;
   --  If Parent is a renaming of the original Entity, Is_Renaming is set to
   --  true, and Ref is set to No_Entity_Reference.
   --  Entity is the entity that was searched initially.
   --  If False is returned, the search is stopped.
   --  If Through_Dispatching is true, then the call occurs through dispatching

   ----------------------------------------
   -- Searching for callers of an entity --
   ----------------------------------------

   procedure Examine_Ancestors_Call_Graph
     (Kernel            : access GPS.Kernel.Kernel_Handle_Record'Class;
      Entity            : Entity_Information;
      User_Data         : access Commands_User_Data_Record'Class;
      Background_Mode   : Boolean := True;
      Dispatching_Calls : Boolean := False;
      Watch             : Gtk.Widget.Gtk_Widget := null);
   --  Search for all entities calling Entity.
   --  For each of this entity, Callback is called. In this case, its Parent
   --  parameter is the caller, and Ref is the occurrence of Entity within its
   --  Parent.
   --  Is_Renaming will be set to True if Parent is a renaming of Entity.
   --
   --  By default, the search is done asynchronously in background mode.
   --  User_Data will be deallocated automatically when the search is finished.
   --
   --  If Watch is destroyed during the search, then the latter is cancelled.
   --
   --  If Dispatching_Calls is true, then any caller that might call the
   --  Entity indirectly through a dispatching call is also listed.

   ------------------------------------------------------
   -- Searching for all entities called by another one --
   ------------------------------------------------------

   procedure Examine_Entity_Call_Graph
     (Kernel            : access GPS.Kernel.Kernel_Handle_Record'Class;
      Entity            : Entity_Information;
      User_Data         : access Commands_User_Data_Record'Class;
      Get_All_Refs      : Boolean;
      Dispatching_Calls : Boolean);
   --  Search for all entities called by Entity.
   --  If Get_All_Refs is true, then all occurrences of the called entities
   --  within Entity will result in a call to On_Entity_Found. Otherwise, a
   --  single call it made for each entity, passing Ref as No_Entity_Reference.
   --  This call is synchronous.
   --  If Dispatching_Calls is true, then a dispatching call within Entity will
   --  be expanded to the full list of subprograms that might possibly be
   --  called at that place.

end Entities.Commands;
