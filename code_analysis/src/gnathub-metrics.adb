------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2018, AdaCore                        --
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

with Ada.Containers.Vectors;

with GPS.Kernel.Modules; use GPS.Kernel.Modules;

package body GNAThub.Metrics is

   package Metrics_Listener_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Metrics_Listener,
      "="          => "=");

   type GNAThub_Metrics_Module_ID_Record is new Module_ID_Record with record
      Listeners : Metrics_Listener_Vectors.Vector;
   end record;
   type GNAThub_Metrics_Module_ID is
     access all GNAThub_Metrics_Module_ID_Record'Class;

   Module : GNAThub_Metrics_Module_ID;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self     : not null access Metric_Record'Class;
      Severity : Severity_Access;
      Rule     : not null Rule_Access;
      Value    : Float;
      Project  : GNATCOLL.Projects.Project_Type;
      File     : GNATCOLL.VFS.Virtual_File;
      Entity   : Entity_Data) is
   begin
      Self.Severity := Severity;
      Self.Rule := Rule;
      Self.Value := Value;
      Self.Project := Project;
      Self.File := File;
      Self.Entity := Entity;

      for Listener of Module.Listeners loop
         Listener.Metric_Added (Self);
      end loop;
   end Initialize;

   ------------------
   -- Get_Severity --
   ------------------

   function Get_Severity
     (Self : not null access Metric_Record) return Severity_Access
   is
      (Self.Severity);

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
     (Self : not null access Metric_Record) return Float
   is
     (Self.Value);

   --------------
   -- Get_Rule --
   --------------

   function Get_Rule
     (Self : not null access Metric_Record) return Rule_Access
   is
      (Self.Rule);

   -----------------
   -- Get_Project --
   -----------------

   function Get_Project
     (Self : not null access Metric_Record)
      return GNATCOLL.Projects.Project_Type
   is
      (Self.Project);

   --------------
   -- Get_File --
   --------------

   function Get_File
     (Self : not null access Metric_Record)
      return GNATCOLL.VFS.Virtual_File
   is
      (Self.File);

   ----------------
   -- Get_Entity --
   ----------------

   function Get_Entity
     (Self : not null access Metric_Record) return Entity_Data
   is
     (Self.Entity);

   ----------
   -- Less --
   ----------

   function Less (L, R : Metric_Access) return Boolean
   is
     (Less (L.Rule, R.Rule));

   -----------------------
   -- Register_Listener --
   -----------------------

   procedure Register_Listener
     (Listener : not null access Metrics_Listener_Interface'Class) is
   begin
      Module.Listeners.Append (Listener);
   end Register_Listener;

   -------------------------
   -- Unregister_Listener --
   -------------------------

   procedure Unregister_Listener
     (Listener : not null access Metrics_Listener_Interface'Class)
   is
      Position : Metrics_Listener_Vectors.Cursor := Module.Listeners.Find
        (Listener);
   begin
      if Metrics_Listener_Vectors.Has_Element (Position) then
         Module.Listeners.Delete (Position);
      end if;
   end Unregister_Listener;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Self : not null access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Self);
   begin
      Module := new GNAThub_Metrics_Module_ID_Record;
   end Register_Module;

end GNAThub.Metrics;
