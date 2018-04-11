------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2016-2018, AdaCore                   --
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

with Ada.Containers.Hashed_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with Code_Analysis;                  use Code_Analysis;
with GPS.Kernel.Messages.References; use GPS.Kernel.Messages.References;
with Default_Preferences;            use Default_Preferences;

package GNAThub is

   type Tool_Record;
   type Tool_Access is access all Tool_Record;

   --------------
   -- Severity --
   --------------

   type Severity_Enum is (Annotation,
                          Unspecified,
                          Info,
                          Low,
                          Medium,
                          High);

   type Severity_Record is limited record
      Ranking : Severity_Enum;
      Color   : Color_Preference;
   end record;

   type Severity_Access is access all Severity_Record;

   function Get_Name (Item : Severity_Record)
                      return Ada.Strings.Unbounded.Unbounded_String;

   function Hash (Item : Severity_Access) return Ada.Containers.Hash_Type;

   package Severity_Natural_Maps is
     new Ada.Containers.Hashed_Maps (Severity_Access, Natural, Hash, "=");

   function Less (L, R : GNAThub.Severity_Access) return Boolean;

   package Severities_Ordered_Sets is new Ada.Containers.Ordered_Sets
     (GNAThub.Severity_Access, Less, "=");

   Severity_History_Prefix : constant String := "gnathub-severities";

   ----------
   -- Rule --
   ----------

   type Rule_Record is limited record
      Name       : Ada.Strings.Unbounded.Unbounded_String;
      Identifier : Ada.Strings.Unbounded.Unbounded_String;
      Tool       : Tool_Access;
      Count      : Severity_Natural_Maps.Map;
   end record;

   type Rule_Access is access all Rule_Record;

   function Less (Left : Rule_Access; Right : Rule_Access) return Boolean;

   package Rule_Sets is
     new Ada.Containers.Ordered_Sets (Rule_Access, Less);

   ----------
   -- Tool --
   ----------

   type Tool_Record is limited record
      Name  : Ada.Strings.Unbounded.Unbounded_String;
      Rules : Rule_Sets.Set;
   end record;

   package Tool_Vectors is
     new Ada.Containers.Vectors (Positive, Tool_Access);

   function Less (L, R : GNAThub.Tool_Access) return Boolean is
     (Ada.Strings.Unbounded."<" (L.Name, R.Name));

   package Tools_Ordered_Sets is new Ada.Containers.Ordered_Sets
     (GNAThub.Tool_Access, Less, "=");

   --------------
   -- Analisis --
   --------------

   package Messages_Vectors is
     new Ada.Containers.Vectors (Positive, Message_Reference);

   type Counts_Array is array (Positive range <>) of Integer;
   type Counts_Array_Access is access all Counts_Array;

   ----------------------
   --  GNAThub_Project --
   ----------------------

   type GNAThub_Project (Counts_Size : Natural) is
     new Code_Analysis.Project with record
      Counts : Counts_Array (1 .. Counts_Size);
   end record;
   type GNAThub_Project_Access is access all GNAThub_Project;

   -------------------
   --  GNAThub_File --
   -------------------

   type GNAThub_File (Counts_Size : Natural) is
     new Code_Analysis.File with record
      Counts   : Counts_Array (1 .. Counts_Size);
      Messages : Messages_Vectors.Vector;
   end record;
   type GNAThub_File_Access is access all GNAThub_File;

   ------------------------
   -- GNAThub_Subprogram --
   ------------------------

   type GNAThub_Subprogram (Counts_Size : Natural) is
     new Code_Analysis.Subprogram with record
      Counts   : Counts_Array (1 .. Counts_Size);
      Messages : Messages_Vectors.Vector;
   end record;
   type GNAThub_Subprogram_Access is access all GNAThub_Subprogram;

end GNAThub;
