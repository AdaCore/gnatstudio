------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2016, AdaCore                        --
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
--  Root package of GNAThub integration module.

with Ada.Containers.Hashed_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

package GNAThub is

   type Tool_Record;
   type Tool_Access is access all Tool_Record;

   --------------
   -- Severity --
   --------------

   type Severity_Record is limited record
      Name       : Ada.Strings.Unbounded.Unbounded_String;
      On_Sidebar : Boolean;
   end record;

   type Severity_Access is access all Severity_Record;

   package Severity_Vectors is
     new Ada.Containers.Vectors (Positive, Severity_Access);

   function Hash (Item : Severity_Access) return Ada.Containers.Hash_Type;

   package Severity_Natural_Maps is
     new Ada.Containers.Hashed_Maps (Severity_Access, Natural, Hash, "=");

   ----------
   -- Rule --
   ----------

   type Rule_Record is limited record
      Name       : Ada.Strings.Unbounded.Unbounded_String;
      Identifier : Ada.Strings.Unbounded.Unbounded_String;
      Tool       : Tool_Access;
      Count      : Severity_Natural_Maps.Map;
   end record;

   type Rule_Access is access all Rule_Record;

   function Less (Left : Rule_Access; Right : Rule_Access) return Boolean;

   package Rule_Sets is
     new Ada.Containers.Ordered_Sets (Rule_Access, Less);

   ----------
   -- Tool --
   ----------

   type Tool_Record is limited record
      Name  : Ada.Strings.Unbounded.Unbounded_String;
      Rules : Rule_Sets.Set;
   end record;

   package Tool_Vectors is
     new Ada.Containers.Vectors (Positive, Tool_Access);

end GNAThub;
