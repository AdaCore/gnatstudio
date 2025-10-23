------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2025, AdaCore                        --
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

package body GNATCOLL.VFS.VSS_Utils.Formatters is

   ------------
   -- Format --
   ------------

   overriding function Format
     (Self   : Formatter;
      Format : VSS.Strings.Formatters.Format_Information)
      return VSS.Strings.Virtual_String
   is
      use type VSS.Strings.Virtual_String;

   begin
      if Format.Format = "base" then
         return
           VSS.Strings.Conversions.To_Virtual_String
             (Self.Value.Display_Base_Name);

      else
         return
           VSS.Strings.Conversions.To_Virtual_String
             (Self.Value.Display_Full_Name);
      end if;
   end Format;

   -----------
   -- Image --
   -----------

   function Image
     (Item : GNATCOLL.VFS.Virtual_File) return Formatter is
   begin
      return (Name => <>, Value => Item);
   end Image;

   -----------
   -- Image --
   -----------

   function Image
     (Name : VSS.Strings.Virtual_String;
      Item : GNATCOLL.VFS.Virtual_File) return Formatter is
   begin
      return (Name => Name, Value => Item);
   end Image;

   ----------
   -- Name --
   ----------

   overriding function Name
     (Self : Formatter) return VSS.Strings.Virtual_String is
   begin
      return Self.Name;
   end Name;

end GNATCOLL.VFS.VSS_Utils.Formatters;
