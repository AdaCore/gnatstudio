------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2016-2019, AdaCore                   --
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

private with Ada.Containers.Ordered_Maps;
private with GNATCOLL.Projects;

package GNAThub.Loader.Databases is

   type Database_Loader_Type is new Loader_Type with private;
   type Database_Loader_Access is access all Database_Loader_Type'Class;

   overriding procedure Prepare_Loading (Self : in out Database_Loader_Type);

   overriding function Has_Data_To_Load
     (Self : Database_Loader_Type) return Boolean;

   overriding procedure Load_Data
     (Self : in out Database_Loader_Type);

   overriding procedure Cleanup (Self : in out Database_Loader_Type);

   procedure Remove_Database (Self : in out Database_Loader_Type);
   --  Remove the GNAThub database.

private

   type Resource_Kind_Type is (From_Project, From_Directory, From_File);
   --  The kind of resources stored in the GNAThub database. The enumeration
   --  values' position should stay synchronised with the IDs refering to the
   --  resource kind:
   --
   --    From_Project = 0
   --    From_Directory = 1
   --    From_File = 2

   type Resource_Record (Kind : Resource_Kind_Type) is limited record
      Name : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   type Resource_Access is access all Resource_Record;

   package Resource_Maps is
     new Ada.Containers.Ordered_Maps (Natural, Resource_Access);

   package Severity_Maps is
     new Ada.Containers.Ordered_Maps (Natural, Severity_Access);

   package Rule_Maps is
     new Ada.Containers.Ordered_Maps (Natural, Rule_Access);

   type Database_Loader_Type is new Loader_Type with record
      Rules     : Rule_Maps.Map;
      --  Database's id to object mappings.

      Metrics   : Rule_Maps.Map;
      --  Database's id to object mappings for the metrics

      Resources : Resource_Maps.Map;
   end record;

end GNAThub.Loader.Databases;
