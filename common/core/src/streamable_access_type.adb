------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2015-2019, AdaCore                   --
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

package body Streamable_Access_Type is

   ------------------------
   -- Output_Access_Type --
   ------------------------

   procedure Output_Access_Type
     (Stream   : not null access Root_Stream_Type'Class;
      Instance : Access_Type) is
   begin
      Streamable_Type'Output (Stream, Instance.all);
   end Output_Access_Type;

   -----------------------
   -- Input_Access_Type --
   -----------------------

   function Input_Access_Type
     (Stream : not null access Root_Stream_Type'Class) return Access_Type is
   begin
      return new Streamable_Type'(Streamable_Type'Input (Stream));
   end Input_Access_Type;

   procedure Input_Access_Type
     (Stream : not null access Root_Stream_Type'Class;
      Item   : out Access_Type)
   is
   begin
      Item := Input_Access_Type (Stream);
   end Input_Access_Type;

end Streamable_Access_Type;
