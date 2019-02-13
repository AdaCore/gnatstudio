------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2012-2019, AdaCore                     --
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
with Ada.Containers.Indefinite_Ordered_Maps;

package body GPS.Tools_Output is
   use String_List_Utils.String_List;

   package Fabric_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (String, Output_Parser_Fabric_Access);

   Map : Fabric_Maps.Map;
   --  Map of registered parser sorted by unique parser name
   --  ??? Should be moved to a module or the kernel

   External_Parsers_Fabric : External_Parser_Fabric_Access;

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

   procedure End_Of_Stream
     (Self    : not null access Tools_Output_Parser;
      Status  : Integer;
      Command : access Root_Command'Class) is
   begin
      if Self.Child /= null then
         Self.Child.End_Of_Stream (Status, Command);
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

   function New_Parser_Chain
     (Name_List : Vector) return Tools_Output_Parser_Access
   is
      Node    : Cursor := Name_List.First;
      Result  : Tools_Output_Parser_Access;
      Found   : Boolean;
   begin
      while Has_Element (Node) loop
         if Map.Contains (Element (Node)) then
            declare
               Fabric : constant Output_Parser_Fabric_Access :=
                 Map (Element (Node));
            begin
               Result := Fabric.Create (Result);
            end;

            Next (Node);
         elsif External_Parsers_Fabric = null then
            Next (Node);
         else
            External_Parsers_Fabric.Create_External_Parsers
              (Node, Result, Found);

            if not Found then
               --  Both Ada and python parsers not found
               --  Report error and skip to next parser
               Next (Node);
            end if;
         end if;
      end loop;

      return Result;
   end New_Parser_Chain;

   ---------------------------
   -- Parse_Standard_Output --
   ---------------------------

   procedure Parse_Standard_Output
     (Self    : not null access Tools_Output_Parser;
      Item    : String;
      Command : access Root_Command'Class) is
   begin
      if Self.Child /= null then
         Self.Child.Parse_Standard_Output (Item, Command);
      end if;
   end Parse_Standard_Output;

   --------------------------
   -- Parse_Standard_Error --
   --------------------------

   procedure Parse_Standard_Error
     (Self    : not null access Tools_Output_Parser;
      Item    : String;
      Command : access Root_Command'Class) is
   begin
      if Self.Child /= null then
         Self.Child.Parse_Standard_Error (Item, Command);
      end if;
   end Parse_Standard_Error;

   ----------------------------
   -- Register_Output_Parser --
   ----------------------------

   procedure Register_Output_Parser
     (Fabric   : access Output_Parser_Fabric'Class;
      Name     : String) is
   begin
      Map.Insert (Name, Output_Parser_Fabric_Access (Fabric));
   end Register_Output_Parser;

   --------------------------------
   -- Set_External_Parser_Fabric --
   --------------------------------

   procedure Set_External_Parser_Fabric
     (Value : External_Parser_Fabric_Access) is
   begin
      External_Parsers_Fabric := Value;
   end Set_External_Parser_Fabric;

end GPS.Tools_Output;
