------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                        Copyright (C) 2012-2019, AdaCore                  --
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

package body GPS.Kernel.Messages.References is

   ------------
   -- Create --
   ------------

   function Create
     (Message : not null Message_Access) return Message_Reference is
   begin
      return Self : Message_Reference do
         Self.Set (Message);
      end return;
   end Create;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Self : Message_Reference) return Boolean is
   begin
      return Self.Message = null;
   end Is_Empty;

   -------------
   -- Message --
   -------------

   function Message (Self : Message_Reference) return Message_Access is
   begin
      return Self.Message;
   end Message;

   ---------
   -- Set --
   ---------

   overriding procedure Set
     (Self : in out Message_Reference; Message : not null Message_Access) is
   begin
      Abstract_Reference (Self).Set (Message);
   end Set;

   -----------
   -- Unset --
   -----------

   overriding procedure Unset (Self : in out Message_Reference) is
   begin
      Abstract_Reference (Self).Unset;
   end Unset;

end GPS.Kernel.Messages.References;
