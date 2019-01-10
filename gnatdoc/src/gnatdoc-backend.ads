------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2019, AdaCore                     --
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

--  This package defines abstract interface of the backend

with GNATdoc.Frontend;  use GNATdoc.Frontend;

private package GNATdoc.Backend is

   type GNATdoc_Backend is abstract tagged null record;

   procedure Initialize
     (Backend : in out GNATdoc_Backend;
      Context : access constant Docgen_Context) is abstract;
   --  Initialize the backend and create the destination directory with
   --  support files.

   procedure Process_File
     (Backend : in out GNATdoc_Backend;
      Tree    : access Tree_Type) is abstract;
   --  Generate documentation of a single file

   procedure Finalize
     (Backend : in out GNATdoc_Backend;
      Update_Global_Index : Boolean) is abstract;
   --  If Update_Global_Index is true then update the global indexes.

   function New_Backend (Name : String) return GNATdoc_Backend'Class;
   --  Factory method
   --  Raises Unknown_Backend if the specified backend is not supported.

   Unknown_Backend : exception;

end GNATdoc.Backend;
