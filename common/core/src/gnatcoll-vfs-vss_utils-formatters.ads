------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2025-2026, AdaCore                   --
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

--  Formatter of `Virtual_File`
--
--  By default, full path name of the file is substituted. To substitute base
--  file name `base` format option can be used.

with VSS.Strings.Formatters;

package GNATCOLL.VFS.VSS_Utils.Formatters is

   type Formatter is
     new VSS.Strings.Formatters.Abstract_Formatter with private;

   function Image
     (Item : GNATCOLL.VFS.Virtual_File) return Formatter;

   function Image
     (Name : VSS.Strings.Virtual_String;
      Item : GNATCOLL.VFS.Virtual_File) return Formatter;

private

   type Formatter is
     new VSS.Strings.Formatters.Abstract_Formatter with record
      Name  : VSS.Strings.Virtual_String;
      Value : GNATCOLL.VFS.Virtual_File;
   end record;

   overriding function Name
     (Self : Formatter) return VSS.Strings.Virtual_String;

   overriding function Format
     (Self   : Formatter;
      Format : VSS.Strings.Formatters.Format_Information)
      return VSS.Strings.Virtual_String;

end GNATCOLL.VFS.VSS_Utils.Formatters;
