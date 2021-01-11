------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2016-2021, AdaCore                     --
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

package body GPS.Markers is

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Location_Marker_Data'Class) is
   begin
      Destroy (Self);
   end Free;

   ----------
   -- Save --
   ----------

   procedure Save
     (Self  : Location_Marker;
      Value : out JSON_Value) is
   begin
      if Self /= No_Marker then
         Self.Unchecked_Get.Save (Value);
      end if;
   end Save;

end GPS.Markers;
