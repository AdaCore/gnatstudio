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
with Ada.Containers.Ordered_Maps;

package body GPS.Kernel.Tools_Output is

   type Output_Parser_Fabric_Access is access all Output_Parser_Fabric'Class;

   package Fabric_Maps is new Ada.Containers.Ordered_Maps
     (Natural, Output_Parser_Fabric_Access);

   Last : Natural := 0;
   --  Unique parser index
   Map : Fabric_Maps.Map;
   --  Map of registered parser sorted by priority and unique parser index

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Self : not null access Tools_Output_Parser) is
      Child : Tools_Output_Parser_Access := Self.Child;
   begin
      if Child /= null then
         Free (Child);
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

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Tools_Output_Parser_Access) is
      procedure Free_Instance is
        new Ada.Unchecked_Deallocation
          (Tools_Output_Parser'Class, Tools_Output_Parser_Access);
   begin
      Self.Destroy;
      Free_Instance (Self);
   end Free;

   ----------------------
   -- New_Parser_Chain --
   ----------------------

   function New_Parser_Chain return Tools_Output_Parser_Access is
      use Fabric_Maps;
      Pos    : Cursor := Map.Last;
      Result : Tools_Output_Parser_Access;
   begin
      while Has_Element (Pos) loop
         Result := Element (Pos).Create (Result);
         Previous (Pos);
      end loop;

      return Result;
   end New_Parser_Chain;

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

   ----------------------------
   -- Register_Output_Parser --
   ----------------------------

   procedure Register_Output_Parser
     (Fabric   : access Output_Parser_Fabric'Class;
      Priority : Parser_Priority)
   is
      Index : constant Natural := Last + Natural (Priority) * 1024;
   begin
      Map.Insert (Index, Output_Parser_Fabric_Access (Fabric));
      Last := Last + 1;
   end Register_Output_Parser;

end GPS.Kernel.Tools_Output;
