------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2012-2013, AdaCore                     --
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

--  This package provides utilities for creating and maintaining
--  a GNATCOLL Xref database for the kernel.

with Glib.Values;   use Glib.Values;
with GNATCOLL.VFS;  use GNATCOLL.VFS;
with GNATCOLL.Xref; use GNATCOLL.Xref;
with Gtk.Widget;
with Xref;          use Xref;

package GPS.Kernel.Xref is

   -----------------------
   -- GPS_Xref_Database --
   -----------------------

   type GPS_Xref_Database is new Extended_Xref_Database with private;
   type GPS_Xref_Database_Access is access all GPS_Xref_Database'Class;

   overriding procedure On_Error
     (Self  : GPS_Xref_Database;
      Error : String);
   --  Handler for an error

   -------------------------------
   -- GPS_General_Xref_Database --
   -------------------------------

   type GPS_General_Xref_Database_Record is new General_Xref_Database_Record
       with private;

   overriding function Select_Entity_Declaration
     (Self   : access GPS_General_Xref_Database_Record;
      File   : GNATCOLL.VFS.Virtual_File;
      Entity : General_Entity) return General_Entity;
   --  see inherited documentation

   procedure Create_Database
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Create and initialize the xref databases

   procedure Compilation_Finished
     (Kernel : access Kernel_Handle_Record'Class;
      C_Only : Boolean);
   --  Called when some files have been recompiled, this will update the
   --  cross-references as needed.
   --  C_Only is specific to the old LI engine, and indicates whether we should
   --  load all xref in memory, or only C/C++ related ones.

   ------------------------
   -- Background queries --
   ------------------------
   --  This package contains various subprograms to do highlevel xref queries
   --  in background mode. In particular, it is used for the call-graph related
   --  queries.
   --  ??? These are mostly obsolete with the sqlite-based engine, since the
   --  results are immediately accessible. These subprograms are kept
   --  temporarily for compatibility with the old database.

   procedure Load_Xref_In_Memory
     (Kernel       : access Kernel_Handle_Record'Class;
      C_Only       : Boolean);
   --  Load all xref info in memory for faster access.
   --  ??? This is needed only for the "old" database, and should be removed
   --  when switching to GNATCOLL.Xref.

   procedure Parse_All_LI_Information
     (Kernel    : access Kernel_Handle_Record'Class;
      Project   : GNATCOLL.Projects.Project_Type;
      Recursive : Boolean);
   --  Parse all the LI information in Project, for all the supported
   --  languages. This can be used in cases where there is no obvious way to
   --  find the LI file matching a given source file (for instance, with a
   --  separate krunched file in Ada).

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
      Entity              : General_Entity;
      Parent              : General_Entity;
      Ref                 : General_Entity_Reference;
      Through_Dispatching : Boolean;
      Is_Renaming         : Boolean) return Boolean is abstract;
   --  If Parent is a renaming of the original Entity, Is_Renaming is set to
   --  true, and Ref is set to No_Entity_Reference.
   --  Entity is the entity that was searched initially.
   --  If False is returned, the search is stopped.
   --  If Through_Dispatching is true, then the call occurs through dispatching

   procedure Examine_Ancestors_Call_Graph
     (Kernel            : access GPS.Kernel.Kernel_Handle_Record'Class;
      Entity            : General_Entity;
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

   procedure Examine_Entity_Call_Graph
     (Kernel            : access GPS.Kernel.Kernel_Handle_Record'Class;
      Entity            : General_Entity;
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

   ----------------------
   -- Context handling --
   ----------------------

   procedure Ensure_Context_Up_To_Date (Context : Selection_Context);
   --  Ensure that the current context has up to date information
   --  ??? This is needed only for the "old" database, and should be removed
   --  when switching to GNATCOLL.Xref.

   -------------
   -- GValues --
   -------------
   --  The following subprograms can be used to store an entity in a GValue,
   --  for instance to store it in a tree view

   function To_GValue (Entity : General_Entity) return Glib.Values.GValue;
   function From_GValue (Value : Glib.Values.GValue) return General_Entity;
   --  Store an entity in a GValue, or get its value back. This properly
   --  handles reference counting, so that while the GValue is in use, the
   --  entity remains valid. The returned entity has a borrow reference, and
   --  thus needs to be Ref'ed if you want to keep it. Removing the row in the
   --  tree for instance makes the entity invalid.

   function Get_Entity_Information_Type return Glib.GType;
   --  Return the type associated with an entity. This is the type that should
   --  be used when creating the tree model.

private

   type GPS_General_Xref_Database_Record is new General_Xref_Database_Record
   with record
      Kernel : Kernel_Handle;
   end record;

   type GPS_Xref_Database is new Extended_Xref_Database with record
      Kernel : Kernel_Handle;
   end record;

end GPS.Kernel.Xref;
