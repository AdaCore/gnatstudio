------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2012, AdaCore                          --
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

with Ada.Unchecked_Deallocation;

package body Tools_Output_Parsers is

   -----------
   -- Child --
   -----------

   function Child
     (Self : not null access Tools_Output_Parser'Class)
      return access Tools_Output_Parser'Class is
   begin
      return Self.Child;
   end Child;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Self : not null access Tools_Output_Parser) is
   begin
      if Self.Child /= null then
         Self.Child.Destroy;
         Free (Self.Child);
      end if;
   end Destroy;

   -------------------
   -- End_Of_Stream --
   -------------------

   procedure End_Of_Stream (Self : not null access Tools_Output_Parser) is
   begin
      if Self.Child /= null then
         Self.Child.End_Of_Stream;
      end if;
   end End_Of_Stream;

   procedure Free (Self : in out Tools_Output_Parser_Access) is
      procedure Free_Instance is
        new Ada.Unchecked_Deallocation
          (Tools_Output_Parser'Class, Tools_Output_Parser_Access);
   begin
      Free_Instance (Self);
   end Free;

   ---------------------------
   -- Parse_Standard_Output --
   ---------------------------

   procedure Parse_Standard_Output
     (Self : not null access Tools_Output_Parser;
      Item : String) is
   begin
      if Self.Child /= null then
         Self.Child.Parse_Standard_Output (Item);
      end if;
   end Parse_Standard_Output;

   --------------------------
   -- Parse_Standard_Error --
   --------------------------

   procedure Parse_Standard_Error
     (Self : not null access Tools_Output_Parser;
      Item : String) is
   begin
      if Self.Child /= null then
         Self.Child.Parse_Standard_Error (Item);
      end if;
   end Parse_Standard_Error;

end Tools_Output_Parsers;
