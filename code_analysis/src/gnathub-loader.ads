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

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.Projects;

with GNAThub.Messages;      use GNAThub.Messages;
with GNAThub.Module;
with GPS.Scripts.Commands;

package GNAThub.Loader is

   ------------
   -- Loader --
   ------------

   type Loader_Type is abstract tagged private;

   procedure Initialize
     (Self   : in out Loader_Type;
      Module : not null GNAThub.Module.GNAThub_Module_Id);

   type Loader_Access is access all GNAThub.Loader.Loader_Type'Class;

   procedure Load (Self : in out Loader_Type'Class);
   --  Called to load asynchronously the data used to create the GNAThub
   --  messages that will be displayed in the Analysis Report.
   --  If listeners have been registered for this loader, On_Finish_Loading
   --  will be called once the loader finish to load all its data or if
   --  the loader has not any data to load.

   procedure Remove_Messages (Self : in out Loader_Type);
   --  Remove the messages loaded by the given loader.

   procedure Cleanup (Self : in out Loader_Type);
   --  Cleanup the loader, interrupting the loading if needed.

   procedure Prepare_Loading (Self : in out Loader_Type) is abstract;
   --  Should be called just before loading data via the Load subprogram.
   --  Override this procedure if you need to perform specific operations
   --  before loading data.

   function Has_Data_To_Load (Self : Loader_Type) return Boolean is abstract;
   --  Should return True if there is some data to load, False otherwise.

   procedure Load_Data (Self : in out Loader_Type) is abstract;
   --  The procedure that will actually load the data.
   --  This procedure should only take a definite amount of time: the
   --  asynchonous command will re-call this procedure if Has_Data_To_Load
   --  keeps returning True and stop otherwise.

   ----------------------
   -- Loader Listeners --
   ----------------------

   type Loader_Listener_Interface is interface;
   type Loader_Listener is access all Loader_Listener_Interface'Class;

   procedure On_Finish_Loading
     (Self : not null access Loader_Listener_Interface) is abstract;
   --  Called when the loader has finished to load data.

   procedure Register_Listener
     (Self : not null access Loader_Type'Class;
      Listener : not null access Loader_Listener_Interface'Class);
   --  Register a listener for the given loader.

   procedure Unregister_Listener
     (Self : not null access Loader_Type'Class;
      Listener : not null access Loader_Listener_Interface'Class);
   --  Unregister the listener for the given loader.

private

   package Loader_Listener_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Loader_Listener,
      "="          => "=");

   type Loader_Type is abstract tagged record
      Module    : GNAThub.Module.GNAThub_Module_Id;
      Messages  : Messages_Vectors.Vector;
      Command   : GPS.Scripts.Commands.Scheduled_Command_Access;
      Listeners : Loader_Listener_Vectors.Vector;
   end record;

   procedure Insert_Message
     (Self    : in out Loader_Type'Class;
      Message : GNAThub_Message_Access);

end GNAThub.Loader;
