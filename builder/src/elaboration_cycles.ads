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
--  This package provides API to store cyclic elaboration dependencies
--  parsed from gnatbind output

with Ada.Strings.Unbounded;            use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

package Elaboration_Cycles is

   type Cycle is tagged private;
   --  Each elaboration dependency cycle contains one or more Dependency
   type Dependency is tagged private;
   --  Dependency with reason ragma_Elaborate_All or Elaborate_All_Desirable
   --  in it's turn contains one or more Link
   type Link is tagged private;

   function Length (Self : Cycle) return Natural;
   --  Number of dependencies in cycle

   function Element (Self : Cycle'Class; Index : Positive) return Dependency;
   --  Get dependency by Index

   procedure Append (Self : in out Cycle; Item : Dependency'Class);
   --  Append dependency to cycle

   function Before_Unit_Name (Self : Dependency) return String;
   --  Name of unit that should be compiled before another
   function After_Unit_Name (Self : Dependency) return String;
   --  Name of unit that should be compiled after another

   type Dependency_Reason is
     (Withed,
      Pragma_Elaborate,
      Pragma_Elaborate_All,
      Elaborate_All_Desirable,
      Elaborate_Desirable,
      Specification_First);

   function Reason (Self : Dependency) return Dependency_Reason;
   --  The reason of dependency

   function Elaborate_Body (Self : Dependency) return Boolean;
   --  A flag to mark dependencies as result of a pragma Elaborate_Body
   --  Read "Note on handling of Elaborate_Body" in binde.adb for more info
   procedure Set_Elaborate_Body (Self : in out Dependency);

   function Length (Self : Dependency) return Natural;
   --  Length of chain of link for Pragma_Elaborate_All and
   --  Elaborate_All_Desirable reason

   function Element (Self : Dependency'Class; Index : Positive) return Link;
   --  Get link by Index

   procedure Append (Self : in out Dependency; Item : Link'Class);
   --  Append Link to chain

   function Unit_Name (Self : Link) return String;
   --  Name of unit for link

   type Link_Kind is (Withed, Body_With_Specification);

   function Kind (Self : Link) return Link_Kind;
   --  Kind of link

   function Create_Link
     (Unit : String;
      Kind : Link_Kind)
      return Link;

   function Create_Dependency
     (Before_Unit : String;
      After_Unit  : String;
      Reason      : Dependency_Reason)
      return Dependency;

private
   type Link is tagged record
      Unit : Unbounded_String;
      Kind : Link_Kind;
   end record;

   package Link_Vectors is new Ada.Containers.Vectors (Positive, Link);

   type Dependency is tagged record
      Before_Unit    : Unbounded_String;
      After_Unit     : Unbounded_String;
      Reason         : Dependency_Reason;
      Elaborate_Body : Boolean;
      Links          : Link_Vectors.Vector;
   end record;

   package Dependency_Vectors is
     new Ada.Containers.Vectors (Positive, Dependency);

   type Cycle is tagged record
      Dependencies : Dependency_Vectors.Vector;
   end record;

end Elaboration_Cycles;
