------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2013, AdaCore                          --
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

--  A search provider that matches actions.

with GPS.Kernel.Actions;
with GPS.Search;

package GPS.Kernel.Search.Actions is

   type Actions_Search_Provider is new Kernel_Search_Provider
     with private;
   overriding function Documentation
     (Self    : not null access Actions_Search_Provider) return String;
   overriding procedure Set_Pattern
     (Self    : not null access Actions_Search_Provider;
      Pattern : not null access GPS.Search.Search_Pattern'Class;
      Limit   : Natural := Natural'Last);
   overriding procedure Next
     (Self     : not null access Actions_Search_Provider;
      Result   : out GPS.Search.Search_Result_Access;
      Has_Next : out Boolean);
   overriding function Display_Name
     (Self     : not null access Actions_Search_Provider) return String
     is ("Actions");

   type Actions_Search_Result is new Kernel_Search_Result with private;
   overriding procedure Free (Self : in out Actions_Search_Result);

private
   type Actions_Search_Provider is new Kernel_Search_Provider with record
      Pattern : GPS.Search.Search_Pattern_Access;
      --  Current pattern, do not free

      Iter    : GPS.Kernel.Actions.Action_Iterator;
      --  The current iterator
   end record;

   type Actions_Search_Result is new Kernel_Search_Result with record
      Name : GNAT.Strings.String_Access;
   end record;
   overriding procedure Execute
     (Self       : not null access Actions_Search_Result;
      Give_Focus : Boolean);
   overriding function Full
     (Self       : not null access Actions_Search_Result) return String;

end GPS.Kernel.Search.Actions;
