------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2009-2019, AdaCore                     --
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

with Ada.Containers.Hashed_Maps;

package CodePeer.Bridge is

   function Hash (Item : Natural) return Ada.Containers.Hash_Type;

   package Annotation_Category_Maps is new Ada.Containers.Hashed_Maps
     (Natural, Annotation_Category_Access, Hash, "=");

   package Positive_Subprogram_Maps is
     new Ada.Containers.Hashed_Maps
       (Positive,
        Code_Analysis.Subprogram_Access,
        Hash,
        "=",
        Code_Analysis."=");

end CodePeer.Bridge;
