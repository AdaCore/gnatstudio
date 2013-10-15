------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2013, AdaCore                        --
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

package GNATdoc.Backend.Base is

   type Base_Backend is
     abstract new GNATdoc.Backend.GNATdoc_Backend with record
      Context : access constant Docgen_Context;
   end record;

   function Name (Self : Base_Backend) return String is abstract;
   --  Returns name of the backend in lowercase. It is used to construct paths.

   function Get_Resource_File
     (Self      : Base_Backend'Class;
      File_Name : GNATCOLL.VFS.Filesystem_String)
      return GNATCOLL.VFS.Virtual_File;
   --  Returns full name of the specified resource file. This subprogram do
   --  lookup in all resource directories and take in sense name of backend.

   overriding procedure Initialize
     (Backend : in out Base_Backend;
      Context : access constant Docgen_Context);
   --  Initialize backend.

end GNATdoc.Backend.Base;
