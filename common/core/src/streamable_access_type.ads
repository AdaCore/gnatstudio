------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2015-2018, AdaCore                   --
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

with Ada.Streams; use Ada.Streams;
with Ada.Unchecked_Deallocation;

--  This package represents a very simple streamable access type. The modus
--  operandi is as follows:
--
--  - When writing, this will write the value pointed to by the instance of the
--  access type
--
--  - When reading, this will allocate a new value, put the read value in it,
--  and return the pointer to it

generic
   type Streamable_Type (<>) is private;
package Streamable_Access_Type is
   type Access_Type is access all Streamable_Type;

   procedure Output_Access_Type
     (Stream : not null access Root_Stream_Type'Class;
      Instance : Access_Type);
   --  Write Instance to stream

   function Input_Access_Type
     (Stream : not null access Root_Stream_Type'Class) return Access_Type;
   --  Read an instance from stream

   procedure Input_Access_Type
     (Stream : not null access Root_Stream_Type'Class;
      Item   : out Access_Type);
   --  Read an instance from stream

   for Access_Type'Input use Input_Access_Type;
   for Access_Type'Output use Output_Access_Type;
   for Access_Type'Read  use Input_Access_Type;
   for Access_Type'Write use Output_Access_Type;

   procedure Free is new Ada.Unchecked_Deallocation
     (Streamable_Type, Access_Type);

end Streamable_Access_Type;
