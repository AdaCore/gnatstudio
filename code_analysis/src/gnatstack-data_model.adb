-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2010, AdaCore                   --
--                                                                   --
-- GPS is Free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded.Hash;
with Ada.Unchecked_Deallocation;

package body GNATStack.Data_Model is

   use Ada.Strings.Unbounded;
   use Subprogram_Location_Sets;

   ---------
   -- "=" --
   ---------

   overriding function "="
     (Left  : Subprogram_Identifier;
      Right : Subprogram_Identifier) return Boolean is
   begin
      return
        Left.Prefix_Name = Right.Prefix_Name
          and Left.Locations = Right.Locations;
   end "=";

   -----------
   -- Clear --
   -----------

   procedure Clear (Item : in out Analysis_Information) is

      procedure Deallocate_Subprogram
        (Position : Subprogram_Information_Sets.Cursor);
      --  Deallocates subprogram information object.

      procedure Deallocate_Location
        (Position : Subprogram_Location_Sets.Cursor);
      --  Deallocates subprogram location object.

      -------------------------
      -- Deallocate_Location --
      -------------------------

      procedure Deallocate_Location
        (Position : Subprogram_Location_Sets.Cursor)
      is
         procedure Free is
           new Ada.Unchecked_Deallocation
             (GPS.Editors.Editor_Mark'Class, Editor_Mark_Access);

         Mark : Editor_Mark_Access := Element (Position).Mark;

      begin
         Free (Mark);
      end Deallocate_Location;

      ---------------------------
      -- Deallocate_Subprogram --
      ---------------------------

      procedure Deallocate_Subprogram
        (Position : Subprogram_Information_Sets.Cursor)
      is
         use Subprogram_Information_Sets;

         procedure Free is
           new Ada.Unchecked_Deallocation
                 (Subprogram_Information, Subprogram_Information_Access);

         Subprogram : Subprogram_Information_Access := Element (Position);

      begin
         Subprogram.Identifier.Locations.Iterate (Deallocate_Location'Access);
         Free (Subprogram);
      end Deallocate_Subprogram;

   begin
      Item.Subprogram_Set.Iterate (Deallocate_Subprogram'Access);
      Item.Subprogram_Set.Clear;
      Item.Unbounded_Set.Clear;
      Item.External_Set.Clear;
      Item.Indirect_Set.Clear;
      Item.Cycle_Set.Clear;
      Item.Entry_Set.Clear;
   end Clear;

   ---------------------
   -- Element_Is_Less --
   ---------------------

   function Element_Is_Less
     (Left  : Subprogram_Information_Access;
      Right : Subprogram_Information_Access) return Boolean is
   begin
      return Left.Identifier.Prefix_Name < Right.Identifier.Prefix_Name;
   end Element_Is_Less;

   -------------------------
   -- Equivalent_Elements --
   -------------------------

   function Equivalent_Elements
     (Left  : Subprogram_Information_Access;
      Right : Subprogram_Information_Access) return Boolean is
   begin
      return Left.Identifier = Right.Identifier;
   end Equivalent_Elements;

   ----------
   -- Hash --
   ----------

   function Hash
     (Item : Subprogram_Identifier) return Ada.Containers.Hash_Type
   is
      Position : Subprogram_Location_Sets.Cursor := Item.Locations.First;
      Aux      : Unbounded_String                := Item.Prefix_Name;

   begin
      while Has_Element (Position) loop
         declare
            Location : constant Subprogram_Location := Element (Position);

         begin
            Append (Aux, Location.Name);
            Append (Aux, Location.File);
            Append (Aux, Integer'Image (Location.Line));
            Append (Aux, Integer'Image (Location.Column));

            Next (Position);
         end;
      end loop;

      return Ada.Strings.Unbounded.Hash (Aux);
   end Hash;

   ----------
   -- Hash --
   ----------

   function Hash
     (Item : Subprogram_Location) return Ada.Containers.Hash_Type is
   begin
      return
        Ada.Strings.Unbounded.Hash
          (Item.Name
           & Item.File
           & Integer'Image (Item.Line)
           & Integer'Image (Item.Column));
   end Hash;

   ----------
   -- Hash --
   ----------

   function Hash
     (Item : Subprogram_Information_Access) return Ada.Containers.Hash_Type is
   begin
      return Hash (Item.Identifier);
   end Hash;

   -----------
   -- Image --
   -----------

   function Image (Item : Stack_Usage_Information) return String is

      use Ada.Strings;
      use Ada.Strings.Fixed;

      function Qualifier_Image return String;
      --  Returns textual representation of the qualifier

      ---------------------
      -- Qualifier_Image --
      ---------------------

      function Qualifier_Image return String is
      begin
         if Item.Qualifier = "UNBOUNDED" then
            return " (unbounded)";

         elsif Item.Qualifier = "UNKNOWN" then
            return " (?)";

         elsif Item.Qualifier = "STATIC" then
            return "";

         else
            raise Program_Error;
         end if;
      end Qualifier_Image;

   begin
      if Item.Size >= 0 then
         return
           Trim (Integer'Image (Item.Size), Both) & " bytes" & Qualifier_Image;

      else
         return Trim (Qualifier_Image, Both);
      end if;
   end Image;

end GNATStack.Data_Model;
