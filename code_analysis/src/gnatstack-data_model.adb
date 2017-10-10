------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2017, AdaCore                     --
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

with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;
with GPS.Editors; use GPS.Editors;

package body GNATStack.Data_Model is

   use Ada.Strings.Unbounded;
   use Subprogram_Location_Sets;

   function "<"
     (Left  : Subprogram_Location_Sets.Set;
      Right : Subprogram_Location_Sets.Set) return Boolean;

   ---------
   -- "<" --
   ---------

   function "<"
     (Left  : Subprogram_Identifier;
      Right : Subprogram_Identifier) return Boolean is
   begin
      if Left.Prefix_Name /= Right.Prefix_Name then
         return Left.Prefix_Name < Right.Prefix_Name;
      end if;

      if Left.Locations /= Right.Locations then
         return Left.Locations < Right.Locations;
      end if;

      return False;
   end "<";

   ---------
   -- "<" --
   ---------

   function "<"
     (Left  : Subprogram_Location;
      Right : Subprogram_Location) return Boolean is
   begin
      if Left.Name /= Right.Name then
         return Left.Name < Right.Name;
      end if;

      if Left.File /= Right.File then
         return Left.File < Right.File;
      end if;

      if Left.Line /= Right.Line then
         return Left.Line < Right.Line;
      end if;

      if Left.Column /= Right.Column then
         return Left.Column < Right.Column;
      end if;

      return False;
   end "<";

   ---------
   -- "<" --
   ---------

   function "<"
     (Left  : Subprogram_Location_Sets.Set;
      Right : Subprogram_Location_Sets.Set) return Boolean
   is
      Left_Position  : Subprogram_Location_Sets.Cursor := Left.First;
      Right_Position : Subprogram_Location_Sets.Cursor := Right.First;
      Left_Element   : Subprogram_Location;
      Right_Element  : Subprogram_Location;

   begin
      loop
         if Has_Element (Left_Position) and Has_Element (Right_Position) then
            Left_Element := Element (Left_Position);
            Right_Element := Element (Right_Position);

            if Left_Element /= Right_Element then
               return Left_Element < Right_Element;
            end if;

         elsif Has_Element (Left_Position)
           xor Has_Element (Right_Position)
         then
            return Has_Element (Right_Position);

         else
            return False;
         end if;

         Next (Left_Position);
         Next (Right_Position);
      end loop;
   end "<";

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

   ---------
   -- "=" --
   ---------

   overriding function "="
     (Left  : Subprogram_Location;
      Right : Subprogram_Location) return Boolean is
   begin
      return
        Left.Name = Right.Name
        and Left.File = Right.File
        and Left.Line = Right.Line
        and Left.Column = Right.Column;
   end "=";

   -----------
   -- Clear --
   -----------

   procedure Clear (Item : in out Analysis_Information) is

      procedure Deallocate_Subprogram
        (Position : Subprogram_Information_Sets.Cursor);
      --  Deallocates subprogram information object.

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
      return Left.Identifier < Right.Identifier;
   end Element_Is_Less;

   -------------------------
   -- Equivalent_Elements --
   -------------------------

   function Equivalent_Elements
     (Left  : Subprogram_Information_Access;
      Right : Subprogram_Information_Access) return Boolean is
   begin
      return Left.Id = Right.Id;
   end Equivalent_Elements;

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
      return Hash (Item.Id);
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

         elsif Item.Qualifier = "STATIC"
           or else Item.Qualifier = "BOUNDED"
         then
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
