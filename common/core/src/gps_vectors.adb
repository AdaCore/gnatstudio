------------------------------------------------------------------------------
--                                  G P S                                   --
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

package body GPS_Vectors is

   use type Ada.Containers.Count_Type;

   -----------
   -- Clear --
   -----------

   overriding procedure Clear (Container : in out Vector) is
   begin
      for Item of Container loop
         Free (Item);
      end loop;
      Std_Vectors.Clear (Std_Vectors.Vector (Container));
   end Clear;

   ------------
   -- Delete --
   ------------

   overriding procedure Delete
     (Container : in out Vector;
      Index     : Extended_Index;
      Count     : Ada.Containers.Count_Type := 1)
   is
      C : Cursor := To_Cursor (Container, Index);
   begin
      Delete (Container, C, Count);
   end Delete;

   ------------
   -- Delete --
   ------------

   overriding procedure Delete
     (Container : in out Vector;
      Position  : in out Cursor;
      Count     : Ada.Containers.Count_Type := 1)
   is
      P     : Cursor := Position;
      Index : Ada.Containers.Count_Type := Count;
      Data  : Data_Type;
   begin
      while Index > 0
        and then Has_Element (P)
      loop
         Data := Element (P);
         Free (Data);
         Index := Index - 1;
         Next (P);
      end loop;
      Std_Vectors.Delete (Std_Vectors.Vector (Container), Position, Count);
   end Delete;

   ------------------
   -- Delete_First --
   ------------------

   overriding procedure Delete_First
     (Container : in out Vector;
      Count     : Ada.Containers.Count_Type := 1)
   is
      Position : Cursor := Container.First;
      Index    : Ada.Containers.Count_Type := Count;
      Data     : Data_Type;
   begin
      while Index > 0
        and then Has_Element (Position)
      loop
         Data := Element (Position);
         Free (Data);
         Index := Index - 1;
         Next (Position);
      end loop;
      Std_Vectors.Delete_First (Std_Vectors.Vector (Container), Count);
   end Delete_First;

   -----------------
   -- Delete_Last --
   -----------------

   overriding procedure Delete_Last
     (Container : in out Vector;
      Count     : Ada.Containers.Count_Type := 1)
   is
      Position : Cursor := Container.Last;
      Index    : Ada.Containers.Count_Type := Count;
      Data     : Data_Type;
   begin
      while Index > 0
        and then Has_Element (Position)
      loop
         Data := Element (Position);
         Free (Data);
         Index := Index - 1;
         Previous (Position);
      end loop;
      Std_Vectors.Delete_Last (Std_Vectors.Vector (Container), Count);
   end Delete_Last;

end GPS_Vectors;
