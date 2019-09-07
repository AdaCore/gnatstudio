------------------------------------------------------------------------------
--                               GNAT Studio                                --
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
with Ada.Containers.Hashed_Maps;

package body GNATdoc.Customization.Tag_Handlers is

   package Inline_Tag_Handler_Maps is
     new Ada.Containers.Hashed_Maps
       (Unbounded_String,
        Inline_Tag_Handler_Access,
        Ada.Strings.Unbounded.Hash,
        "=");

   Inline_Tags : Inline_Tag_Handler_Maps.Map;

   ---------------------
   -- Get_Inline_Tags --
   ---------------------

   function Get_Inline_Tags return Unbounded_String_Sets.Set is
   begin
      return Result : Unbounded_String_Sets.Set do
         for Handler of Inline_Tags loop
            Result.Insert (To_Unbounded_String (Handler.Name));
         end loop;
      end return;
   end Get_Inline_Tags;

   ----------------------------
   -- Get_Inline_Tag_Handler --
   ----------------------------

   function Get_Inline_Tag_Handler
     (Name : String) return Inline_Tag_Handler_Access is
   begin
      return Inline_Tags (To_Unbounded_String (Name));
   end Get_Inline_Tag_Handler;

   --------------
   -- Register --
   --------------

   procedure Register (Handler : Tag_Handler_Access) is
   begin
      if Handler.all in Abstract_Inline_Tag_Handler'Class then
         Inline_Tags.Insert
           (To_Unbounded_String (Handler.Name),
            Inline_Tag_Handler_Access (Handler));
      end if;
   end Register;

end GNATdoc.Customization.Tag_Handlers;
