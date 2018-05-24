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

with GNATCOLL.Projects;
with GNATCOLL.VFS;

with GNAThub.Messages;
with GNAThub.Module;
with GPS.Scripts.Commands;

package GNAThub.Loader is

   type Loader_Type is abstract tagged private;

   procedure Initialize
     (Self   : in out Loader_Type;
      Module : not null GNAThub.Module.GNAThub_Module_Id);

   type Loader_Access is access all GNAThub.Loader.Loader_Type'Class;

   function Load (Self : in out Loader_Type'Class) return Boolean;
   --  Called to load the data used to create the GNAThub messages that will
   --  be displayed in the GNAThub Report.
   --  Should return True when data has been found, False otherwise.

   procedure Remove_Messages (Self : in out Loader_Type);

   procedure Cleanup (Self : in out Loader_Type);

   procedure Prepare_Loading (Self : in out Loader_Type) is abstract;

   function Has_Data_To_Load (Self : Loader_Type) return Boolean is abstract;

   procedure Load_Data (Self : in out Loader_Type) is abstract;

private

   type Loader_Type is abstract tagged record
      Module       : GNAThub.Module.GNAThub_Module_Id;
      Messages     : Messages_Vectors.Vector;
      Command      : GPS.Scripts.Commands.Scheduled_Command_Access;
   end record;

   procedure Insert_Message
     (Self    : in out Loader_Type'Class;
      Project : GNATCOLL.Projects.Project_Type;
      Message : GNAThub.Messages.Message_Access);

   procedure Insert_Metric
     (Self    : in out Loader_Type'Class;
      Project : GNATCOLL.Projects.Project_Type;
      File    : GNATCOLL.VFS.Virtual_File;
      Line    : Natural;
      Metric  : Metric_Access);

end GNAThub.Loader;
