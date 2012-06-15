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

with Xref; use Xref;

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

private

   type GPS_Xref_Database is new Extended_Xref_Database with record
      Kernel : Kernel_Handle;
   end record;

end GPS.Kernel.Xref;
