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

--  This package provides utilities for creating and maintaining
--  a GNATCOLL Xref database for the kernel.

with GNATCOLL.VFS;  use GNATCOLL.VFS;
with GNATCOLL.Xref; use GNATCOLL.Xref;

with Basic_Types;   use Basic_Types;
with Entities;

with Xref; use Xref;

package GPS.Kernel.Xref is

   -----------------------
   -- GPS_Xref_Database --
   -----------------------

   type GPS_Xref_Database is new Xref_Database with private;
   type GPS_Xref_Database_Access is access all GPS_Xref_Database'Class;

   overriding procedure On_Error
     (Self  : GPS_Xref_Database;
      Error : String);
   --  Handler for an error

   procedure Setup
     (Self   : GPS_Xref_Database_Access;
      Kernel : Kernel_Handle);
   --  Create the GPS Xref database.
   --  This should be called every time the project view changes.

   ----------------------
   -- Context handling --
   ----------------------

   procedure Ensure_Context_Up_To_Date (Context : Selection_Context);
   --  Ensure that the current context has up to date information
   --  ??? This is needed only for the "old" database, and should be removed
   --  when switching to GNATCOLL.Xref.

   function Get_Context_Entity
     (Context           : Selection_Context;
      Ask_If_Overloaded : Boolean := False) return General_Entity;
   --  Get the entity pointed to by the current context

   -----------------------------------
   -- Utilities working on entities --
   -----------------------------------

   --  These functions provide high-level facilities working on entities

   procedure Find_Declaration
     (Db              : Xref_Database'Class;
      File            : Virtual_File;
      Entity_Name     : String := "";
      Line            : Natural;
      Column          : Basic_Types.Visible_Column_Type;
      Entity          : out GNATCOLL.Xref.Entity_Information;
      Closest_Ref     : out GNATCOLL.Xref.Entity_Reference;
      Status          : out Entities.Queries.Find_Decl_Or_Body_Query_Status;
      Check_Decl_Only : Boolean := False;
      Fuzzy_Expected  : Boolean := False);
   --  Find the entity that is referenced at the given location.
   --  If Entity_Name is unspecified, GPS will no take this into account
   --  If Check_Decl_Only is True, then only declarations are checked, not
   --  any other kind of reference.
   --  The Handler is computed automatically if not passed as an argument.
   --  Closest_Ref is the reference to the entity that was the closest to the
   --  given location.
   --  If Fuzzy_Expected is true, then the search won't try to fallback to
   --  the constructs in case of a fuzzy result.

private

   type GPS_Xref_Database is new Xref_Database with record
      Kernel : Kernel_Handle;
   end record;

end GPS.Kernel.Xref;
