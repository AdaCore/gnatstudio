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

--  This package provides API to store cyclic elaboration dependencies
--  parsed from gnatbind output

with Ada.Strings.Unbounded;            use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

package Elaboration_Cycles is

   type Cycle is private;
   --  Each elaboration dependency cycle contains one or more Dependency

   type Dependency is private;
   --  A dependency describes the relationship between two units, and why
   --  they depend on one another (perhaps through a pragma Elaborate_All for
   --  instance).
   --  When dependency caused by explicit or implicit pragma Elaborate_All
   --  relation between two units may include intermediate units.
   --  These intermediate units collected as Links.

   type Link is private;
   --  Intermediate units for Elaborate_All dependencies.

   function Dependencies_Count (Self : Cycle) return Natural;
   --  Number of dependencies in cycle

   function Element (Self : Cycle; Index : Positive) return Dependency;
   --  Get dependency by Index.

   procedure Append (Self : in out Cycle; Item : Dependency);
   --  Append dependency to cycle

   function Before_Unit_Name (Self : Dependency) return String;
   --  Name of unit that should be compiled first in this dependency.

   function After_Unit_Name (Self : Dependency) return String;
   --  Name of unit that should be compiled last in this dependency.

   type Dependency_Reason is
     (Withed,
      Pragma_Elaborate,
      Pragma_Elaborate_All,
      Elaborate_All_Desirable,
      Elaborate_Desirable,
      Specification_First,
      Pragma_Elaborate_All_Closure,
      Pragma_Elaborate_Closure,
      Elaborate_Body_Subject,
      Elaborate_Body_Closure,
      F_Switch_Forced,
      Invokes_Construct);

   function Image (Reason : Dependency_Reason) return String;
   --  Return a string suitable for display to users corresponding to Reason

   function Reason (Self : Dependency) return Dependency_Reason;
   --  The reason of dependency

   procedure Set_Elaborate_Body (Self : in out Dependency);
   function Elaborate_Body (Self : Dependency) return Boolean;
   --  Mark the dependency as coming from a "pragma Elaborate_Body". This
   --  provides a special handling by the binder, so that both spec and body
   --  are handled as a single entity from the point of view of determining
   --  an elaboration order.
   --  See also "Note on handling of Elaborate_Body" in binde.adb.

   function Links_Count (Self : Dependency) return Natural;
   --  Length of chain of link for Pragma_Elaborate_All and
   --  Elaborate_All_Desirable reason

   function Element (Self : Dependency; Index : Positive) return Link;
   --  Get link by Index

   procedure Append (Self : in out Dependency; Item : Link);
   --  Append Link to chain

   function Unit_Name (Self : Link) return String;
   --  Name of unit for given link

   type Link_Kind is (Withed, Body_With_Specification);
   --  Kind of intermediate units for a dependency one of
   --   * specification withed by other unit
   --   * body for some specification

   function Kind (Self : Link) return Link_Kind;
   --  Kind of link

   function Create_Link
     (Unit : String;
      Kind : Link_Kind)
      return Link;
   --  Link constructor

   function Create_Dependency
     (Before_Unit : String;
      After_Unit  : String;
      Reason      : Dependency_Reason)
      return Dependency;
   --  Dependency constructor

private
   type Link is record
      Unit : Unbounded_String;
      Kind : Link_Kind;
   end record;

   package Link_Vectors is new Ada.Containers.Vectors (Positive, Link);

   type Dependency is record
      Before_Unit    : Unbounded_String;
      After_Unit     : Unbounded_String;
      Reason         : Dependency_Reason;
      Elaborate_Body : Boolean;
      Links          : Link_Vectors.Vector;
   end record;

   package Dependency_Vectors is
     new Ada.Containers.Vectors (Positive, Dependency);

   type Cycle is record
      Dependencies : Dependency_Vectors.Vector;
   end record;

end Elaboration_Cycles;
