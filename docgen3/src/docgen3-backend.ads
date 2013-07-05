------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2013, AdaCore                     --
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

with Docgen3.Frontend;  use Docgen3.Frontend;

private package Docgen3.Backend is

   type Docgen3_Backend is abstract tagged null record;

   procedure Initialize
     (Backend : in out Docgen3_Backend;
      Context : access constant Docgen_Context) is abstract;
   --  Initialize the backend and create the destination directory with
   --  support files.

   procedure Process_File
     (Backend : in out Docgen3_Backend;
      Tree    : access Tree_Type) is abstract;
   --  Generate documentation of a single file

   procedure Finalize
     (Backend : in out Docgen3_Backend;
      Update_Global_Index : Boolean) is abstract;
   --  If Update_Global_Index is true then update the global indexes.

   function New_Backend return Docgen3_Backend'Class;
   --  Factory method

   ---------------------------------------
   -- Complementary backend subprograms --
   ---------------------------------------

   function Get_Doc_Directory
     (Kernel : Kernel_Handle) return Virtual_File;
   --  If the Directory_Dir attribute is defined in the project, then use the
   --  value; otherwise use the default directory (that is, a subdirectory
   --  'doc' in the object directory, or in the project directory if no
   --  object dir is defined).

end Docgen3.Backend;
