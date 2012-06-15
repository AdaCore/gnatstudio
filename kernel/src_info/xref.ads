------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2012, AdaCore                          --
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

--  This package provides high-level cross-reference operations.
--  This should be the preferred entry point for accessing entity
--  information from Ada/C/C++ code.

with Basic_Types;   use Basic_Types;
with GNATCOLL.VFS;  use GNATCOLL.VFS;
with GNATCOLL.Xref; use GNATCOLL.Xref;
with Entities;
with Language_Handlers;
with Language.Tree.Database;

package Xref is

   -----------
   -- Types --
   -----------

   --  The database

   type General_Xref_Database is record
      Entities   : Standard.Entities.Entities_Database;
      --  The "legacy" LI database

      Xref       : GNATCOLL.Xref.Xref_Database_Access;
      --  The "new" LI database

      Constructs : Language.Tree.Database.Construct_Database_Access;
      --  The constructs database
   end record;

   type Extended_Xref_Database is new GNATCOLL.Xref.Xref_Database
      with private;
   --  This database overrides a number of subprograms so that we use
   --  either the constructs database or the LI database.
   --  It is further extended (GPS-specific) in GPS.Kernel.Xref.

   --  File location

   type General_Location is record
      File   : Virtual_File;
      Line   : Integer := 0;
      Column : Visible_Column_Type := 0;
   end record;
   No_Location : constant General_Location := (No_File, 0, 0);

   --  Entities

   type General_Entity is record
      Old_Entity : Entities.Entity_Information := null;
      Entity     : GNATCOLL.Xref.Entity_Information := No_Entity;

      Node       : Language.Tree.Database.Entity_Access :=
        Language.Tree.Database.Null_Entity_Access;
      --  The corresponding node in the constructs database. This can be
      --  computed from the other two fields.
   end record;
   No_General_Entity : constant General_Entity;

   --  Entity references

   type General_Entity_Reference is record
      Old_Ref : Entities.Entity_Reference := Entities.No_Entity_Reference;
      Ref : Entity_Reference := No_Entity_Reference;
   end record;
   No_General_Entity_Reference : constant General_Entity_Reference;

   ---------------------------
   -- High level operations --
   ---------------------------

   --  These functions provide high-level facilities not exposing the
   --  entities backend, and are back-end independent.
   --
   --  Some operations might even query using one back-end, then fall back
   --  on a less precise back-end if the first query is not precise enough.

   function Get_Entity
     (Db   : General_Xref_Database;
      Name : String;
      Loc  : General_Location) return General_Entity;
   --  Retrieve the entity referenced at the given location

   function Get_Entity (Ref : General_Entity_Reference) return General_Entity;
   --  Return the entity the reference is pointing to

   function Get_Name
     (Db     : General_Xref_Database;
      Entity : General_Entity) return String;
   --  Return the name of the entity

   function Get_Location
     (Ref : General_Entity_Reference) return General_Location;
   --  Return the location of this reference

   function Get_Declaration
     (Db     : General_Xref_Database;
      Entity : General_Entity) return General_Location;
   --  Return the location of the entity declaration

   function Get_Body
     (Db     : General_Xref_Database;
      Entity : General_Entity) return General_Location;
   --  Return the location of the first body for this entity

   function Documentation
     (Self             : General_Xref_Database;
      Handler          : Language_Handlers.Language_Handler;
      Entity           : General_Entity;
      Raw_Format       : Boolean := False;
      Check_Constructs : Boolean := True) return String;
   --  Return the documentation (tooltips,...) for the entity.
   --  If Raw_Format is False, the documentation is formated in HTML.
   --
   --  Check_Constructs shoudl be False to disable the use of the constructs
   --  database.

   -------------------------
   -- Life cycle handling --
   -------------------------

   procedure Ref (Entity : General_Entity);
   procedure Unref (Entity : in out General_Entity);
   --  Increase/Decrease the reference counter on entities.
   --  ??? This is needed only as long as the legacy system is in place.

private
   type Extended_Xref_Database is new GNATCOLL.Xref.Xref_Database with
      null record;

   No_General_Entity : constant General_Entity :=
     (Old_Entity => null,
      Entity     => No_Entity,
      Node       => Language.Tree.Database.Null_Entity_Access);

   No_General_Entity_Reference : constant General_Entity_Reference :=
     (Old_Ref => Entities.No_Entity_Reference,
      Ref     => No_Entity_Reference);

end Xref;
