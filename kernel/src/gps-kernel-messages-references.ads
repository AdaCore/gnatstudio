------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                        Copyright (C) 2012-2018, AdaCore                  --
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
--  This package provides data type to store references to messages. Once set,
--  reference can be used to access to message, but in contrast to objects of
--  access to message type, reference is set to null automatically when message
--  is destroyed.
------------------------------------------------------------------------------
package GPS.Kernel.Messages.References is

   type Message_Reference is tagged private;

   function Create
     (Message : not null Message_Access) return Message_Reference;
   --  Creates new refernce to the specified message.

   function Is_Empty (Self : Message_Reference) return Boolean;
   --  Returns True then referenced message was not set or was destroyed.

   function Message (Self : Message_Reference) return Message_Access;
   --  Returns referenced message.

   procedure Set
     (Self : in out Message_Reference; Message : not null Message_Access);
   --  Sets reference to the specified message.

   procedure Unset (Self : in out Message_Reference);
   --  Unsets reference.

private

   type Message_Reference is new Abstract_Reference with null record;

end GPS.Kernel.Messages.References;
