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

--  Declare parser to get binder errors about elaboration circularities.

with Elaboration_Cycles;
with Ada.Strings.Unbounded;            use Ada.Strings.Unbounded;

with Commands; use Commands;

with GPS.Tools_Output;                 use GPS.Tools_Output;

package Browsers.Elaborations.Cycle_Parser is

   type Circularity_Parser is new Tools_Output_Parser with private;
   --  This parser reads binder errors about elaboration circularities

   --  Each error report has one or more cycle.
   --  Each cycle has one or more dependency.
   --  Each dependency has
   --     * two units (After, Before)
   --     * reason (Withed, Elab, Elab_All, Elab_All_Desirable, Elab_Desirable
   --               and Spec_First)
   --     * links, in case when reason Elab_All or Elab_All_Desirable
   --     * Elab_Body boolean flag

   overriding procedure Parse_Standard_Output
     (Self    : not null access Circularity_Parser;
      Item    : String;
      Command : access Root_Command'Class);

   type Output_Parser_Fabric is
     new GPS.Tools_Output.Output_Parser_Fabric with private;

   overriding function Create
     (Self  : access Output_Parser_Fabric;
      Child : Tools_Output_Parser_Access)
      return Tools_Output_Parser_Access;
   --  Create new parser to get binder errors about elaboration circularities

private

   type Output_Parser_Fabric is
     new GPS.Tools_Output.Output_Parser_Fabric with null record;

   type State_Kinds is
     (New_Cycle, New_Dependency, New_Reason,
      Skip_Recompile, New_Link, Link_Reason,
      Skip_Because, Skip_Therefore);

   type Circularity_Parser is new Tools_Output_Parser with record
      Last_Cycle  : Elaboration_Cycles.Cycle;
      Has_Cycle   : Boolean;
      Last_Dep    : Elaboration_Cycles.Dependency;
      Last_Link   : Unbounded_String;
      Last_Before : Unbounded_String;
      Last_After  : Unbounded_String;
      State       : State_Kinds := New_Cycle;
   end record;

end Browsers.Elaborations.Cycle_Parser;
