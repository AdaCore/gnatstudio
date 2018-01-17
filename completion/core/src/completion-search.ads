------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2013-2018, AdaCore                     --
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

--  A search provider that matches entities, to be used in particular in the
--  global search box.

with Language.Tree.Database;
with GPS.Kernel.Search;  use GPS.Kernel.Search;
with GPS.Search;         use GPS.Search;

package Completion.Search is

   type Entities_Search_Provider is new Kernel_Search_Provider with private;

   overriding function Documentation
     (Self    : not null access Entities_Search_Provider) return String;
   overriding procedure Free (Self : in out Entities_Search_Provider);
   overriding procedure Set_Pattern
     (Self    : not null access Entities_Search_Provider;
      Pattern : not null access GPS.Search.Search_Pattern'Class;
      Limit   : Natural := Natural'Last);
   overriding procedure Next
     (Self     : not null access Entities_Search_Provider;
      Result   : out GPS.Search.Search_Result_Access;
      Has_Next : out Boolean);
   overriding function Display_Name
     (Self     : not null access Entities_Search_Provider) return String
     is (Provider_Entities);
   overriding function Complete_Suffix
     (Self      : not null access Entities_Search_Provider;
      Pattern   : not null access GPS.Search.Search_Pattern'Class)
      return String;

private

   type Entities_Search_Provider is new Kernel_Search_Provider with record
      Pattern : Search_Pattern_Access;
      --  Do not free

      Iter : Language.Tree.Database.Construct_Db_Iterator;
   end record;

end Completion.Search;
