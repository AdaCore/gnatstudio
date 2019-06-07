------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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

--  This package defines the GPS module for communication with VCS

with Ada.Containers.Indefinite_Hashed_Maps;  use Ada;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Hash_Case_Insensitive;

with GNATCOLL.Projects;     use GNATCOLL.Projects;
with GNATCOLL.VFS;          use GNATCOLL.VFS;
with Gdk.Event;             use Gdk.Event;
with Gtk.Widget;            use Gtk.Widget;

with Default_Preferences;   use Default_Preferences;
with GPS.Kernel;            use GPS.Kernel;
with GPS.Kernel.MDI;        use GPS.Kernel.MDI;
with GPS.Kernel.Modules;    use GPS.Kernel.Modules;
with Projects;              use Projects;
with VCS;                   use VCS;
with VCS_View.Explorer;     use VCS_View.Explorer;
with VCS_View.Activities;   use VCS_View.Activities;
with VCS_Status;            use VCS_Status;

package VCS_Module is

   VCS_Module_Name : constant String := "VCS_Interface";

   -- VCS --
   Implicit_Status           : Boolean_Preference;
   Default_VCS               : String_Preference;

   function Equiv_VCS (Left, Right : String) return Boolean;

   package VCS_Map is new Containers.Indefinite_Hashed_Maps
     (String, VCS_Access, Strings.Hash_Case_Insensitive, Equiv_VCS);

   package VCS_Project_Cache_Map is new Containers.Hashed_Maps
     (Project_Type, VCS_Access, Projects.Project_Name_Hash, "=");

   package Ref_Map is new Containers.Hashed_Maps
     (Virtual_File, Virtual_File, Full_Name_Hash, "=");

   type Explorer_Child_Record is new GPS_MDI_Child_Record with null record;
   type Explorer_Child_Access is access all Explorer_Child_Record'Class;
   overriding function Build_Context
     (Self  : not null access Explorer_Child_Record;
      Event : Gdk.Event.Gdk_Event := null)
      return Selection_Context;

   type Activities_Child_Record is new GPS_MDI_Child_Record with null record;
   type Activities_Child_Access is access all Activities_Child_Record'Class;
   overriding function Build_Context
     (Self  : not null access Activities_Child_Record;
      Event : Gdk.Event.Gdk_Event := null)
      return Selection_Context;

   --  Global variable to store all the registered handlers

   type VCS_Module_ID_Record is new Module_ID_Record with record
      Registered_VCS    : VCS_Map.Map;
      --  The list of all VCS systems recognized by the kernel

      VCS_Project_Cache : VCS_Project_Cache_Map.Map;
      --  Cache the actual VCS for all projects

      Explorer          : VCS_Explorer_View_Access;
      --  The VCS Explorer

      Explorer_Child    : Explorer_Child_Access;
      --  The child containing the VCS Explorer

      Activities        : VCS_Activities_View_Access;
      --  The VCS Activities explorer

      Activities_Child  : Activities_Child_Access;

      Cached_Status     : Status_Cache;

      Reference_Map     : Ref_Map.Map;
   end record;

   type VCS_Module_ID_Access is access all VCS_Module_ID_Record'Class;

   VCS_Module_ID : VCS_Module_ID_Access;

   type VCS_Explorer_Module_ID_Record is new Module_ID_Record with null record;
   type VCS_Explorer_Module_ID_Access is access
     all VCS_Explorer_Module_ID_Record;
   VCS_Explorer_Module_Id : VCS_Explorer_Module_ID_Access;

   overriding procedure Destroy (Module : in out VCS_Module_ID_Record);
   --  See inherited documentation

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the module into the list

   procedure For_Every_VCS
     (Process : not null access procedure (VCS : VCS_Access));
   --  Iterate through all the registered VCS

   procedure Register_VCS (Id : String; Handle : VCS_Access);
   --  Add a VCS identifier to the list of recognized VCS systems.
   --  ??? This is temporary, until the VCS module can directly add a page in
   --  the wizard or the project properties editor.

   function Get_VCS_From_Id (Id : String) return VCS_Access;
   --  Browse through all VCS identifiers that are registered and return
   --  a VCS reference to an appropriate system, if any.
   --  If no satisfying system was found, Null is returned.
   --  VCS identifiers are registered using Register_VCS_Identifier.

   function Get_Explorer
     (Kernel      : not null access Kernel_Handle_Record'Class;
      Raise_Child : Boolean := True;
      Show        : Boolean := False) return VCS_Explorer_View_Access;
   --  Return the VCS Explorer. If Show is True, place it in the MDI and show
   --  it.

   procedure Hide_VCS_Explorer;
   --  Call this subprogram when the VCS Explorer is about to be removed

   function Explorer_Is_Open return Boolean;
   --  Return whether the Explorer is open

   function Get_Activities_Explorer
     (Kernel      : not null access Kernel_Handle_Record'Class;
      Raise_Child : Boolean := True;
      Show        : Boolean := False) return VCS_Activities_View_Access;
   --  Return the VCS Explorer. If Show is True, place it in the MDI and show
   --  it.

   procedure Hide_VCS_Activities_Explorer;
   --  Call this subprogram when the VCS Explorer is about to be removed

   function Activities_Explorer_Is_Open return Boolean;
   --  Return whether the Explorer is open

   function Get_Status_Cache return Status_Cache;
   --  Return the status cache

   procedure Set_Reference (File, Reference : Virtual_File);
   --  Record a reference file into the map. The reference file is the file
   --  which is under version control and is the based of the file passed in
   --  parameters. This is used to find the VCS file for a diff buffer for
   --  example. The goal is to be able to run any VCS actions (annotation,
   --  create revision log...) from those auxiliary buffers.

   function Get_Reference (File : Virtual_File) return Virtual_File;
   --  Return the reference file for the given file or No_File if no reference
   --  files are recorded.

end VCS_Module;
