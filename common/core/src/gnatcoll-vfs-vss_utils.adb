------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2022-2025, AdaCore                   --
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

with VSS.Strings.Conversions;

package body GNATCOLL.VFS.VSS_Utils is

   ------------
   -- Create --
   ------------

   function Create
     (Full_Filename : VSS.Strings.Virtual_String;
      Host          : String  := Local_Host;
      Normalize     : Boolean := False) return Virtual_File is
   begin
      return
        Create_From_UTF8
          (VSS.Strings.Conversions.To_UTF_8_String (Full_Filename),
           Host,
           Normalize);
   end Create;

   ---------------
   -- Full_Name --
   ---------------

   function Full_Name
     (File      : Virtual_File;
      Normalize : Boolean := False) return VSS.Strings.Virtual_String is
   begin
      return
        VSS.Strings.Conversions.To_Virtual_String
          (File.Display_Full_Name (Normalize));
   end Full_Name;

end GNATCOLL.VFS.VSS_Utils;
