------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2012-2018, AdaCore                     --
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

package body Elaboration_Cycles is

   ------------------------
   -- Dependencies_Count --
   ------------------------

   function Dependencies_Count (Self : Cycle) return Natural is
   begin
      return Self.Dependencies.Last_Index;
   end Dependencies_Count;

   -------------
   -- Element --
   -------------

   function Element (Self : Cycle; Index : Positive) return Dependency is
   begin
      return Self.Dependencies.Element (Index);
   end Element;

   ------------
   -- Append --
   ------------

   procedure Append (Self : in out Cycle; Item : Dependency) is
   begin
      Self.Dependencies.Append (Item);
   end Append;

   ----------------------
   -- Before_Unit_Name --
   ----------------------

   function Before_Unit_Name (Self : Dependency) return String is
   begin
      return To_String (Self.Before_Unit);
   end Before_Unit_Name;

   ---------------------
   -- After_Unit_Name --
   ---------------------

   function After_Unit_Name (Self : Dependency) return String is
   begin
      return To_String (Self.After_Unit);
   end After_Unit_Name;

   -----------
   -- Image --
   -----------

   function Image (Reason : Dependency_Reason) return String is
   begin
      case Reason is
         when Withed                  => return "withed";
         when Pragma_Elaborate        => return "pragma Elaborate";
         when Pragma_Elaborate_All    => return "pragma Elaborate_All";
         when Elaborate_All_Desirable => return "Elaborate_All desirable";
         when Elaborate_Desirable     => return "Elaborate desirable";
         when Specification_First     => return "specification first";
      end case;
   end Image;

   ------------
   -- Reason --
   ------------

   function Reason (Self : Dependency) return Dependency_Reason is
   begin
      return Self.Reason;
   end Reason;

   --------------------
   -- Elaborate_Body --
   --------------------

   function Elaborate_Body (Self : Dependency) return Boolean is
   begin
      return Self.Elaborate_Body;
   end Elaborate_Body;

   ------------------------
   -- Set_Elaborate_Body --
   ------------------------

   procedure Set_Elaborate_Body (Self : in out Dependency) is
   begin
      Self.Elaborate_Body := True;
   end Set_Elaborate_Body;

   -----------------
   -- Links_Count --
   -----------------

   function Links_Count (Self : Dependency) return Natural is
   begin
      return Self.Links.Last_Index;
   end Links_Count;

   -------------
   -- Element --
   -------------

   function Element (Self : Dependency; Index : Positive) return Link is
   begin
      return Self.Links.Element (Index);
   end Element;

   ------------
   -- Append --
   ------------

   procedure Append (Self : in out Dependency; Item : Link) is
   begin
      Self.Links.Append (Item);
   end Append;

   ---------------
   -- Unit_Name --
   ---------------

   function Unit_Name (Self : Link) return String is
   begin
      return To_String (Self.Unit);
   end Unit_Name;

   ----------
   -- Kind --
   ----------

   function Kind (Self : Link) return Link_Kind is
   begin
      return Self.Kind;
   end Kind;

   -----------------
   -- Create_Link --
   -----------------

   function Create_Link
     (Unit : String;
      Kind : Link_Kind)
      return Link
   is
   begin
      return (To_Unbounded_String (Unit), Kind);
   end Create_Link;

   -----------------------
   -- Create_Dependency --
   -----------------------

   function Create_Dependency
     (Before_Unit : String;
      After_Unit  : String;
      Reason      : Dependency_Reason)
      return Dependency is
   begin
      return (Before_Unit    => To_Unbounded_String (Before_Unit),
              After_Unit     => To_Unbounded_String (After_Unit),
              Reason         => Reason,
              Elaborate_Body => False,
              Links          => <>);
   end Create_Dependency;

end Elaboration_Cycles;
