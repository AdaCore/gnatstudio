-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2001-2009, AdaCore                  --
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

--  This package defines the GPS module for communication with VCS

with Ada.Containers.Indefinite_Hashed_Maps;  use Ada;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Hash_Case_Insensitive;

with Gtk.Widget;
with Gtkada.MDI;          use Gtkada.MDI;

with GPS.Kernel;          use GPS.Kernel;
with GPS.Kernel.Modules;  use GPS.Kernel.Modules;
with Projects;            use Projects;
with VCS;                 use VCS;
with VCS_View.Explorer;   use VCS_View.Explorer;
with VCS_View.Activities; use VCS_View.Activities;
with VCS_Status;          use VCS_Status;

package VCS_Module is

   VCS_Module_Name : constant String := "VCS_Interface";

   Commit_Done_Hook : constant Hook_Name := "commit_done_hook";
   --  Raised when a commit has been done

   Activity_Checked_Hook : constant Hook_Name := "activity_checked_hook";
   --  Raised when an activity status has been checked

   Log_Parsed_Hook : constant Hook_Name := "log_parsed_hook";
   --  Raised when the last log has been parsed

   Status_Parsed_Hook : constant Hook_Name := "status_parsed_hook";
   --  Raised when the last status has been parsed

   Revision_Parsed_Hook : constant Hook_Name := "revision_parsed_hook";
   --  Raised when the last revision has been parsed

   Annotation_Parsed_Hook : constant Hook_Name := "annotation_parsed_hook";
   --  Raised when the last annotation has been parsed

   function Equiv_VCS (Left, Right : String) return Boolean;

   package VCS_Map is new Containers.Indefinite_Hashed_Maps
     (String, VCS_Access, Strings.Hash_Case_Insensitive, Equiv_VCS);

   package VCS_Project_Cache_Map is new Containers.Hashed_Maps
     (Project_Type, VCS_Access, Projects.Project_Name_Hash, "=");

   --  Global variable to store all the registered handlers

   type VCS_Module_ID_Record is new Module_ID_Record with record
      Registered_VCS    : VCS_Map.Map;
      --  The list of all VCS systems recognized by the kernel

      VCS_Project_Cache : VCS_Project_Cache_Map.Map;
      --  Cache the actual VCS for all projects

      Explorer          : VCS_Explorer_View_Access;
      --  The VCS Explorer

      Explorer_Child    : MDI_Child;
      --  The child containing the VCS Explorer

      Activities        : VCS_Activities_View_Access;
      --  The VCS Activities explorer

      Activities_Child  : MDI_Child;

      Cached_Status     : Status_Cache;
   end record;

   type VCS_Module_ID_Access is access all VCS_Module_ID_Record'Class;

   VCS_Module_ID : VCS_Module_ID_Access;

   overriding procedure Destroy (Module : in out VCS_Module_ID_Record);
   overriding procedure Default_Context_Factory
     (Module  : access VCS_Module_ID_Record;
      Context : in out Selection_Context;
      Child   : Gtk.Widget.Gtk_Widget);
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

end VCS_Module;
