------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                        Copyright (C) 2013, AdaCore                       --
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
--  The package defines simple buffer provider used by XRef engine.
--

with GNAT.Strings;                    use GNAT.Strings;
with GNATCOLL.VFS;                    use GNATCOLL.VFS;
with Language.Tree.Database;          use Language.Tree.Database;

package GPS.CLI_Buffer_Providers is

   type CLI_Buffer_Provider is new Buffer_Provider with private;

   overriding function Get_Buffer
     (Provider : access CLI_Buffer_Provider;
      File     : Virtual_File) return String_Access;
   --  Return the buffer from the editor if any, from the file otherwise

   overriding function Get_Timestamp
     (Provider : access CLI_Buffer_Provider;
      File     : Virtual_File) return Integer;

private

   type CLI_Buffer_Provider is new Buffer_Provider with null record;

end GPS.CLI_Buffer_Providers;
