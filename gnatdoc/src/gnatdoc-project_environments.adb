------------------------------------------------------------------------------
--                               GNAT Studio                                --
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

package body GNATdoc.Project_Environments is

   ----------------------
   -- Get_GNAT_Version --
   ----------------------

   function Get_GNAT_Version
     (Self : GNATdoc_Project_Environment'Class) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Self.GNAT_Version);
   end Get_GNAT_Version;

   ----------------------
   -- Set_GNAT_Version --
   ----------------------

   overriding procedure Set_GNAT_Version
     (Self    : in out GNATdoc_Project_Environment;
      Version : String) is
   begin
      Self.GNAT_Version := Ada.Strings.Unbounded.To_Unbounded_String (Version);
   end Set_GNAT_Version;

end GNATdoc.Project_Environments;
