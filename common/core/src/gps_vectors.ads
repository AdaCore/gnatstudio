------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2016-2019, AdaCore                     --
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

with Ada.Containers.Vectors;

--  Package based on standard vector with call Free for all removed elements

generic
   type Data_Type is private;

   with procedure Free (Data : in out Data_Type) is <>;
   --  Free any dynamic memory associated with Data

package GPS_Vectors is

   pragma Suppress (Container_Checks);
   package Std_Vectors is new Ada.Containers.Vectors (Positive, Data_Type);
   use Std_Vectors;

   type Vector is new Std_Vectors.Vector with null record;

   overriding procedure Clear (Container : in out Vector);

   overriding procedure Delete
     (Container : in out Vector;
      Index     : Extended_Index;
      Count     : Ada.Containers.Count_Type := 1);

   overriding procedure Delete
     (Container : in out Vector;
      Position  : in out Cursor;
      Count     : Ada.Containers.Count_Type := 1);

   overriding procedure Delete_First
     (Container : in out Vector;
      Count     : Ada.Containers.Count_Type := 1);

   overriding procedure Delete_Last
     (Container : in out Vector;
      Count     : Ada.Containers.Count_Type := 1);

   Empty_Vector : constant Vector;

private

   Empty_Vector : constant Vector := (Std_Vectors.Vector with null record);

end GPS_Vectors;
