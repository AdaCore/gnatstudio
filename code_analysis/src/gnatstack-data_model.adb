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
with Ada.Strings.Unbounded.Hash;

package body GNATStack.Data_Model is

   use Ada.Strings.Unbounded;
   use Subprogram_Location_Sets;

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

end GNATStack.Data_Model;
