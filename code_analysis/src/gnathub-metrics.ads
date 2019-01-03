------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2018-2019, AdaCore                   --
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

with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded.Hash;
with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;
with Projects.Views;

package GNAThub.Metrics is

   ------------
   -- Metric --
   ------------

   type Metric_Record is tagged private;
   type Metric_Access is access all Metric_Record'Class;

   function Less (L, R : Metric_Access) return Boolean;

   package Metrics_Ordered_Sets is new Ada.Containers.Ordered_Sets
     (Metric_Access, Less, "=");

   package Metric_Tool_Maps is
     new Ada.Containers.Hashed_Maps
       (Key_Type        => Unbounded_String,
        Element_Type    => Metrics_Ordered_Sets.Set,
        Hash            => Ada.Strings.Unbounded.Hash,
        Equivalent_Keys => "=",
        "="             => Metrics_Ordered_Sets."=");

   procedure Initialize
     (Self         : not null access Metric_Record'Class;
      Severity     : Severity_Access;
      Rule         : not null Rule_Access;
      Value        : Float;
      Project_View : Projects.Views.Project_View_Reference;
      File         : GNATCOLL.VFS.Virtual_File;
      Entity       : Entity_Data);
   --  Create a new metric, associating it with a severity, rule, a value and a
   --  location.

   function Get_Severity
     (Self : not null access Metric_Record) return Severity_Access;
   --  Return the metric's severity.

   function Get_Value
     (Self : not null access Metric_Record) return Float;
   --  Return the metric's value.

   function Get_Rule
     (Self : not null access Metric_Record) return Rule_Access;
   --  Return the metric's rule.

   function Get_Project
     (Self : not null access Metric_Record)
      return GNATCOLL.Projects.Project_Type;
   --  Return the metric's project.

   function Get_File
     (Self : not null access Metric_Record)
      return GNATCOLL.VFS.Virtual_File;
   --  return the metric's file.

   function Get_Entity
     (Self : not null access Metric_Record) return Entity_Data;
   --  Return the metric's entity data.

   -----------------------
   -- Metrics Listeners --
   -----------------------

   type Metrics_Listener_Interface is limited interface;
   type Metrics_Listener is access all Metrics_Listener_Interface'Class;
   --  Interface used to react to changes made regarding metrics.

   procedure Metric_Added
     (Self   : not null access Metrics_Listener_Interface;
      Metric : not null access Metric_Record'Class) is abstract;
   --  Called each time a new metric is added.

   procedure Register_Listener
     (Listener : not null access Metrics_Listener_Interface'Class);
   --  Register the metrics' listener.

   procedure Unregister_Listener
     (Listener : not null access Metrics_Listener_Interface'Class);
   --  Unregister the metrics' listener.

   procedure Register_Module
     (Self : not null access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the metrics' module.

private

   type Metric_Record is tagged record
      Severity     : Severity_Access;
      Rule         : Rule_Access;
      Value        : Float;
      Project_View : Projects.Views.Project_View_Reference;
      File         : GNATCOLL.VFS.Virtual_File;
      Entity       : Entity_Data;
   end record;

end GNAThub.Metrics;
