------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2016-2019, AdaCore                   --
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
--  Root package of GNAThub integration module.

with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;          use Ada.Strings.Unbounded;

with GPS.Kernel.Messages;            use GPS.Kernel.Messages;
with GPS.Kernel.Messages.References; use GPS.Kernel.Messages.References;
with GPS.Kernel.Style_Manager;       use GPS.Kernel.Style_Manager;

package GNAThub is

   type Filterable_Item is tagged limited private;

   type Tool_Record;
   type Tool_Access is access all Tool_Record;

   function Get_Current_Count (Self : Filterable_Item) return Natural;

   function Get_Total_Count (Self : Filterable_Item) return Natural;

   procedure Reset_Counters (Self : in out Filterable_Item);

   procedure Increment_Current_Count (Self : in out Filterable_Item);

   procedure Decrement_Current_Count (Self : in out Filterable_Item);

   procedure Increment_Total_Count (Self : in out Filterable_Item);

   procedure Decrement_Total_Count (Self : in out Filterable_Item);

   function Image (Self : Filterable_Item) return String;

   --------------
   -- Severity --
   --------------

   type Severity_Record is limited new Filterable_Item with private;

   type Severity_Access is access all Severity_Record;

   function Get_Name
     (Item : Severity_Record)
      return Ada.Strings.Unbounded.Unbounded_String;

   function Hash (Item : Severity_Access) return Ada.Containers.Hash_Type;

   function Less (L, R : GNAThub.Severity_Access) return Boolean;

   package Severities_Ordered_Sets is new Ada.Containers.Ordered_Sets
     (GNAThub.Severity_Access, Less, "=");

   Severity_History_Prefix : constant String := "gnathub-severities";

   ----------
   -- Rule --
   ----------

   type Rule_Record is limited new Filterable_Item with private;

   type Rule_Access is access all Rule_Record;

   function Less (Left : Rule_Access; Right : Rule_Access) return Boolean;

   package Rule_Sets is
     new Ada.Containers.Ordered_Sets (Rule_Access, Less);

   ----------
   -- Tool --
   ----------

   type Tool_Record is limited new Filterable_Item with private;

   package Tool_Vectors is
     new Ada.Containers.Vectors (Positive, Tool_Access);

   function Less (L, R : GNAThub.Tool_Access) return Boolean;

   package Tools_Ordered_Sets is new Ada.Containers.Ordered_Sets
     (GNAThub.Tool_Access, Less, "=");

   --------------
   -- Analysis --
   --------------

   type Entity_Data is private;

   No_Entity_Data : constant Entity_Data;

   package Messages_Vectors is
     new Ada.Containers.Vectors (Positive, Message_Reference);

   type Counts_Array is array (Positive range <>) of Integer;
   type Counts_Array_Access is access all Counts_Array;

private

   type Filterable_Item is tagged limited record
      Current : Natural;
      Total   : Natural;
   end record;

   type Severity_Record is limited new Filterable_Item with record
      Ranking : Message_Importance_Type;
      Style   : Style_Access;
   end record;

   type Rule_Record is limited new Filterable_Item with record
      Name       : Ada.Strings.Unbounded.Unbounded_String;
      Identifier : Ada.Strings.Unbounded.Unbounded_String;
      Tool       : Tool_Access;
   end record;

   type Tool_Record is limited new Filterable_Item with record
      Name    : Ada.Strings.Unbounded.Unbounded_String;
      Rules   : Rule_Sets.Set;
   end record;

   type Entity_Data is record
      Name   : Unbounded_String;
      Line   : Natural;
      Column : Natural;
   end record;

   No_Entity_Data : constant Entity_Data := (To_Unbounded_String (""), 0, 0);

end GNAThub;
