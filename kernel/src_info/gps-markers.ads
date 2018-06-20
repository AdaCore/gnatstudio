------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2016-2018, AdaCore                     --
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

--  Abstract data types for markers.

with GNATCOLL.Refcount; use GNATCOLL.Refcount;
with XML_Utils;         use XML_Utils;

package GPS.Markers is

   type Location_Marker_Data is abstract tagged null record;
   --  A place in the GUI or in the resources.
   --  In the case of files, this mark remains at a constant position even when
   --  the file is edited.
   --  Can't use an interface since AJIS does not support them.

   function Go_To
     (Self  : not null access Location_Marker_Data) return Boolean is abstract;
   --  Move the focus in GPS to the location marked by Self.
   --  If this function returns False, it is assumed the marker is no longer
   --  legal, and should be removed from the history.

   procedure Destroy (Self : in out Location_Marker_Data) is null;
   --  Free the memory used by Self

   function To_String
     (Self : not null access Location_Marker_Data) return String is abstract;
   --  Return a displayable string describing Self.
   --  This string doesn't need to be unique for each marker, it is used in the
   --  user interface to allow the user to select a specific marker.

   function Save
     (Self : not null access Location_Marker_Data)
     return XML_Utils.Node_Ptr is abstract;
   --  Saves the marker to an XML node, so that it can be reloaded later on,
   --  possibly in a different GPS session.

   function Similar
     (Left        : not null access Location_Marker_Data;
      Dummy_Right : not null access Location_Marker_Data'Class)
      return Boolean is (False);
   --  Return True if Left and Right point to the same location in the sense
   --  that GPS should not add a new marker in history for two locations that
   --  are the same.

   function Distance
     (Left        : not null access Location_Marker_Data;
      Dummy_Right : not null access Location_Marker_Data'Class) return Integer
      is (Integer'Last);
   --  Return a value represented distance between two locations.
   --  Return Integer'Last if locations are not comparable, for example marks
   --  are in different files.

   procedure Free (Self : in out Location_Marker_Data'Class) with Inline;
   --  Destroy self and reclame memory.

   package Markers is new Shared_Pointers
     (Element_Type => Location_Marker_Data'Class,
      Release      => Free);
   subtype Location_Marker is Markers.Ref;
   No_Marker : constant Location_Marker := Markers.Null_Ref;
   --  Make locations refcounted, so that they can more conveniently be stored
   --  in data structures.

   function "=" (Left, Right : Location_Marker) return Boolean
     renames Markers."=";

   function Go_To (Self  : Location_Marker) return Boolean
      is (Self /= No_Marker and then Self.Unchecked_Get.Go_To);
   function To_String (Self : Location_Marker) return String
      is (if Self = No_Marker then "" else Self.Unchecked_Get.To_String);
   function Save (Self : Location_Marker) return XML_Utils.Node_Ptr
      is (if Self = No_Marker then null else Self.Unchecked_Get.Save);
   function Similar (Left, Right  : Location_Marker) return Boolean
   is ((Left = No_Marker and then Right = No_Marker)
       or else (Left /= No_Marker
                and then Right /= No_Marker
                and then Left.Unchecked_Get.Similar (Right.Unchecked_Get)));
   function Distance (Left, Right  : Location_Marker) return Integer
   is (if Left = No_Marker or else Right = No_Marker then Integer'Last
       else Left.Unchecked_Get.Distance (Right.Unchecked_Get));
   --  Convenience functions

end GPS.Markers;
