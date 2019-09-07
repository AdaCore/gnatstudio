------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2008-2019, AdaCore                     --
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

with Diffing;
with GNAT.Strings; use GNAT.Strings;

package body String_Diff is

   type Iter is record
      S : String_Access;
      I : Natural;
   end record;

   Null_Object : constant Character := ASCII.NUL;

   function Length (C : String_Access) return Natural;
   function First (C : String_Access) return Iter;
   function Last (C : String_Access) return Iter;
   function Get (I : Iter) return Character;
   function Next (I : Iter) return Iter;
   function Prev (I : Iter) return Iter;
   function At_End (I : Iter) return Boolean;
   --  See documentation in Diffing

   ------------
   -- Length --
   ------------

   function Length (C : String_Access) return Natural is
   begin
      return C'Length;
   end Length;

   -----------
   -- First --
   -----------

   function First (C : String_Access) return Iter is
   begin
      return (S => C, I => C'First);
   end First;

   ----------
   -- Last --
   ----------

   function Last (C : String_Access) return Iter is
   begin
      return (S => C, I => C'Last);
   end Last;

   ---------
   -- Get --
   ---------

   function Get (I : Iter) return Character is
   begin
      return I.S (I.I);
   end Get;

   ----------
   -- Next --
   ----------

   function Next (I : Iter) return Iter is
   begin
      return (S => I.S, I => I.I + 1);
   end Next;

   ----------
   -- Prev --
   ----------

   function Prev (I : Iter) return Iter is
   begin
      return (S => I.S, I => I.I - 1);
   end Prev;

   ------------
   -- At_End --
   ------------

   function At_End (I : Iter) return Boolean is
   begin
      return I.I >= I.S'Last;
   end At_End;

   package String_Access_Diff is new Diffing
     (Object      => Character,
      Container   => String_Access,
      Iterator    => Iter,
      Null_Object => Null_Object,
      "="         => "=",
      Length      => Length,
      Last        => Last,
      First       => First,
      Get         => Get,
      Next        => Next,
      Prev        => Prev,
      At_End      => At_End);

   ----------
   -- Diff --
   ----------

   procedure Diff
     (Old_String : String;
      New_String : String;
      Callback   : Diff_Callback)
   is
      Ol : String_Access := new String'(Old_String);
      Ne : String_Access := new String'(New_String);

      procedure Wrapper
        (Old_Obj, New_Obj : Character;
         State : String_Access_Diff.Diff_State);
      --  Callback passed to the diff function

      -------------
      -- Wrapper --
      -------------

      procedure Wrapper
        (Old_Obj, New_Obj : Character;
         State : String_Access_Diff.Diff_State) is
      begin
         Callback (Old_Obj, New_Obj,
                   Diff_State'Val (String_Access_Diff.Diff_State'Pos (State)));
      end Wrapper;

   begin
      String_Access_Diff.Diff (Ol, Ne, Wrapper'Unrestricted_Access);
      GNAT.Strings.Free (Ol);
      GNAT.Strings.Free (Ne);
   end Diff;

end String_Diff;
