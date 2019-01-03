------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2019, AdaCore                     --
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

--  This package provides a generic diff algorithm for any kind of data.
--  See a description of the algorithm at:
--     http://en.wikipedia.org/wiki/Longest_common_subsequence_problem

generic
   type Object (<>) is limited private;
   type Container is limited private;
   type Iterator is private;

   Null_Object : Object;

   with function "=" (Left, Right : Object) return Boolean is <>;
   with function Length (C : Container) return Natural is <>;
   with function First (C : Container) return Iterator is <>;
   with function Last (C : Container) return Iterator is <>;
   with function Get (I : Iterator) return Object is <>;
   with function Next (I : Iterator) return Iterator is <>;
   with function Prev (I : Iterator) return Iterator is <>;
   with function At_End (I : Iterator) return Boolean is <>;

package Diffing is

   type Diff_State is (Equal, Added, Removed);
   --  This type is used to identify the state of an element during the diff
   --  process

   type Diff_Callback is access procedure
     (Old_Obj, New_Obj : Object; State : Diff_State);
   --  This function is called by the generic diff algorithm. Depending on the
   --  value of State, arguments means something different:
   --    Equal:   Old_Obj contains the object from the old container, New_Obj
   --             from the new container. Even if equals regarding the diff
   --             procedures, objects can be distinct in memory.
   --    Added:   Old_Obj is irrelevant, New_Obj contains the value of the
   --             added object in the new container.
   --    Removed: New_Obj is irrelevant, Old_Obj contains the value of the
   --             removed object from the old container.

   procedure Diff
     (Old_Container, New_Container : Container;
      Callback                     : Diff_Callback);
   --  Performs a diff operation between the new and the old container. The
   --  overall complexity of the algoritm is rather high, but optimization is
   --  done to avoid computing diff on the firsts and lasts equals characters.

end Diffing;
