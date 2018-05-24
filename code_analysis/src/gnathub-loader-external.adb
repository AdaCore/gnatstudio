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

with GNATCOLL.Scripts;   use GNATCOLL.Scripts;

with GPS.Kernel.Project; use GPS.Kernel.Project;

package body GNAThub.Loader.External is

   Load_Messages_Limit : constant := 30;
   --  The maximum number of messages loaded when executing the background
   --  command that loads external messages.

   ---------------------
   -- Prepare_Loading --
   ---------------------

   overriding procedure Prepare_Loading
     (Self : in out External_Loader_Type) is
   begin
      Self.Current_Message := Self.Messages.First_Index;
   end Prepare_Loading;

   ----------------------
   -- Has_Data_To_Load --
   ----------------------

   overriding function Has_Data_To_Load
     (Self : External_Loader_Type) return Boolean is
   begin
      return Self.Current_Message <= Self.Messages_To_Process.Last_Index;
   end Has_Data_To_Load;

   -------------
   -- Cleanup --
   -------------

   overriding procedure Cleanup
     (Self : in out External_Loader_Type) is
   begin
      Self.Current_Message := Self.Messages_To_Process.First_Index;
   end Cleanup;

   ---------------
   -- Load_Data --
   ---------------

   overriding procedure Load_Data
     (Self : in out External_Loader_Type)
   is
      M_Ref    : Message_Reference;
      Message  : GNAThub.Messages.Message_Access;
      Rule     : GNAThub.Rule_Access;
      Severity : GNAThub.Severity_Access;
      Position : GNAThub.Severity_Natural_Maps.Cursor;
      Count    : Natural := 0;
   begin
      while Self.Current_Message <= Self.Messages_To_Process.Last_Index loop
         M_Ref := Self.Messages_To_Process (Self.Current_Message);

         if not M_Ref.Is_Empty then
            Message := GNAThub.Messages.Message_Access (M_Ref.Message);

            --  Insert the message in the module's tree

            Insert_Message
              (Self    => Self,
               Project => Get_Registry
                 (Self.Module.Kernel).Tree.Info (Message.Get_File).Project,
               Message => Message);

            --  Update the severities counter

            Severity := Message.Get_Severity;
            Rule     := Message.Get_Rule;
            Position := Rule.Count.Find (Severity);

            if Severity_Natural_Maps.Has_Element (Position) then
               Rule.Count.Replace_Element
                 (Position, Severity_Natural_Maps.Element (Position) + 1);
            else
               Rule.Count.Insert (Severity, 1);
            end if;
         end if;

         Count := Count + 1;
         Self.Current_Message := Self.Current_Message + 1;

         exit when Count >= Load_Messages_Limit;
      end loop;
   end Load_Data;

   --------------------------
   -- Add_External_Message --
   --------------------------

   procedure Add_External_Message
     (Self    : in out External_Loader_Type'Class;
      Message : GNAThub.Messages.Message_Access) is
   begin
      Self.Messages_To_Process.Append
        (GPS.Kernel.Messages.References.Create
           (GPS.Kernel.Messages.Message_Access (Message)));
   end Add_External_Message;

end GNAThub.Loader.External;
