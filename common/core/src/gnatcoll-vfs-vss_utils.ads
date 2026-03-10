------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2022-2026, AdaCore                   --
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

with VSS.Strings;

package GNATCOLL.VFS.VSS_Utils is

   function Create
     (Full_Filename : VSS.Strings.Virtual_String;
      Host          : String  := Local_Host;
      Normalize     : Boolean := False) return Virtual_File;
   --  Equivalent of `Create_From_UTF8`

   function Full_Name
     (File      : Virtual_File;
      Normalize : Boolean := False) return VSS.Strings.Virtual_String;
   --  Equivalent of `Display_Full_Name`

   function Base_Name
     (File   : Virtual_File;
      Suffix : VSS.Strings.Virtual_String := VSS.Strings.Empty_Virtual_String)
      return VSS.Strings.Virtual_String;
   --  Equivalent of `Display_Base_Name`

   function Relative_Path
     (File : Virtual_File;
      From : Virtual_File) return VSS.Strings.Virtual_String;
   --  Return the path of File relative to From. Return the full_name in case
   --  From and File are not on the same drive.

end GNATCOLL.VFS.VSS_Utils;
