------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2018, AdaCore                          --
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

with GPS.Kernel;       use GPS.Kernel;
with GNAThub.Messages; use GNAThub.Messages;

package GNAThub.Loader.External is

   type External_Loader_Type is new Loader_Type with private;
   type External_Loader_Access is access all External_Loader_Type'Class;

   overriding procedure Remove_Messages
     (Self : in out External_Loader_Type);

   overriding procedure Prepare_Loading
     (Self : in out External_Loader_Type);

   overriding function Has_Data_To_Load
     (Self : External_Loader_Type) return Boolean;

   overriding procedure Load_Data
     (Self : in out External_Loader_Type);

   overriding procedure Cleanup
     (Self : in out External_Loader_Type);

   procedure Add_External_Message
     (Self    : in out External_Loader_Type'Class;
      Message : GNAThub_Message_Access);
   --  Add an external message to the list of messages that will be displayed
   --  in the GNAThub report.

private

   type External_Loader_Type is new Loader_Type with record
      Messages_To_Process : Messages_Vectors.Vector;
   end record;

end GNAThub.Loader.External;
