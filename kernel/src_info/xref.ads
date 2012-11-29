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
with Entities.Queries; use Entities.Queries;
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

      Node       : Language.Tree.Database.Entity_Persistent_Access :=
        Language.Tree.Database.Null_Entity_Persistent_Access;
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

   procedure Find_Next_Body
     (Dbase                : General_Xref_Database;
      Entity               : General_Entity;
      Current_Location     : General_Location := No_Location;
      Location             : out General_Location;
      No_Location_If_First : Boolean := False);
   --  Find the location for one of the bodies of the entities. If the
   --  current location is not a body, the first body found is returned.
   --  Otherwise, the first one different from Current_Location is returned.
   --  Calling this subprogram multiple times will eventually return all the
   --  bodies.
   --  This also returns completion for incomplete types.
   --  If No_Location_If_First is True, then this iterator will not loop
   --  to the first body on reaching the last.

   procedure For_Each_Dispatching_Call
     (Dbase     : General_Xref_Database;
      Entity    : General_Entity;
      Ref       : General_Entity_Reference;
      On_Callee : access function
                    (Callee, Primitive_Of : General_Entity) return Boolean;
      Filter    : Entities.Reference_Kind_Filter := Entity_Has_Declaration;
      Policy    : Entities.Queries.Dispatching_Menu_Policy);
   --  If Ref references a dispatching call then call On_Callee with all the
   --  overridding primitives (that is, all the primitives that might possibly
   --  be called instead of Entity). For example, if you have:
   --         procedure Dispatch (Self : Base'Class) is
   --         begin
   --            Proc (Self);
   --         end Dispatch;
   --  and call For_Each_Dispatching_Call on Proc, you will get the primitive
   --  operation of Base and all the overriding primitive ops of its children.
   --
   --  Filter can be used to make sure the entity has some specific type of
   --  reference. The most common use is to ensure that the entity does have
   --  a body (ie is not abstract), in which case the filter is set to
   --  Entity_Has_Body.
   --
   --  Search stops when On_Callee returns False
   --
   --  Nothing is done if Ref does not point to a dispatching call.
   --  This procedure does not propagate any exception.

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

   function Get_Category
     (Db     : General_Xref_Database;
      Entity : General_Entity) return Entities.Entity_Category;
   --  Compute the category of the entity

   function Get_Kind
     (Db     : General_Xref_Database;
      Entity : General_Entity) return Entities.E_Kind;
   --  Return the kind of the entity

   function Get_Type_Of
     (Db     : General_Xref_Database;
      Entity : General_Entity) return General_Entity;
   --  Return the type of the entity

   function Is_Predefined_Entity
     (Db : General_Xref_Database;
      E  : General_Entity) return Boolean;
   --  True if E is a predefined entity

   function Pointed_Type
     (Dbase  : General_Xref_Database;
      Entity : General_Entity) return General_Entity;
   --  Return the type pointed to by entity

   function To_General_Entity
     (E : Entities.Entity_Information) return General_Entity;
   --  Convert Entities.Entity_Information to General_Entity. This routine is
   --  provided to support to legacy code. It will be eventually removed???

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
      Node       => Language.Tree.Database.Null_Entity_Persistent_Access);

   No_General_Entity_Reference : constant General_Entity_Reference :=
     (Old_Ref => Entities.No_Entity_Reference,
      Ref     => No_Entity_Reference);

end Xref;
