------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2016-2018, AdaCore                   --
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
--  This package contains loader code to load data from the database.

with GNATCOLL.VFS;
with GNAThub.Module;

private with GPS.Scripts.Commands;
private with GNATCOLL.Projects;
private with Ada.Containers.Ordered_Maps;

package GNAThub.Loader is

   type Loader
     (Module : not null access GNAThub.Module.GNAThub_Module_Id_Record'Class)
   is tagged limited private;

   procedure Load
     (Self     : in out Loader'Class;
      Database : GNATCOLL.VFS.Virtual_File);
   --  Start loading of data from the database.

   procedure Initialize (Self : in out Loader'Class);
   --  Initialize loader.

private

   type Resource_Record is limited record
      Name : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   type Resource_Access is access all Resource_Record;

   package Resource_Maps is
     new Ada.Containers.Ordered_Maps (Natural, Resource_Access);

   package Severity_Maps is
     new Ada.Containers.Ordered_Maps (Natural, Severity_Access);

   package Rule_Maps is
     new Ada.Containers.Ordered_Maps (Natural, Rule_Access);

   type Loader
     (Module : not null access GNAThub.Module.GNAThub_Module_Id_Record'Class)
   is tagged limited record
      Severities   : Severity_Maps.Map;
      Rules        : Rule_Maps.Map;
      --  Database's id to object mappings.

      Resources    : Resource_Maps.Map;
      Current      : Resource_Maps.Cursor;

      Command      : GPS.Scripts.Commands.Scheduled_Command_Access;
      --  Command that is used to load data from database

      Source_Files : GNATCOLL.Projects.File_And_Project_Array_Access;
   end record;

end GNAThub.Loader;
