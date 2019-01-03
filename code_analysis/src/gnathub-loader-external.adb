------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2018-2019, AdaCore                     --
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

with GNATCOLL.Scripts; use GNATCOLL.Scripts;

package body GNAThub.Loader.External is

   Load_Messages_Limit : constant := 30;
   --  The maximum number of messages loaded when executing the background
   --  command that loads external messages.

   ---------------------
   -- Remove_Messages --
   ---------------------

   overriding procedure Remove_Messages
     (Self : in out External_Loader_Type) is
   begin
      --  Don't remove the messages if there is no new data to load: this
      --  allows to display the previously loaded data when displaying the
      --  Analysis Report.
      if Self.Has_Data_To_Load then
         Loader_Type (Self).Remove_Messages;
      end if;
   end Remove_Messages;

   ---------------------
   -- Prepare_Loading --
   ---------------------

   overriding procedure Prepare_Loading
     (Self : in out External_Loader_Type) is
   begin
      --  Hide the messages: the filter will decide if we need
      --  to show them. Thus triggering the listerners.
      if not Self.Messages_To_Process.Is_Empty then
         for Message_Ref of Self.Messages_To_Process loop
            if not Message_Ref.Is_Empty then
               declare
                  Message : constant GNAThub_Message_Access :=
                    GNAThub_Message_Access (Message_Ref.Message);
               begin
                  Message.Set_Flags (GPS.Kernel.Messages.Empty_Message_Flags);
               end;
            end if;
         end loop;

      --  If there is no new messages to process, put the prevously loaded
      --  messages in the queue again so that the Analysis Report gets filled
      --  with the previous data when no new analysis has been performed.
      elsif not Self.Messages.Is_Empty then
         for Message_Ref of Self.Messages loop
            if not Message_Ref.Is_Empty then
               declare
                  Message : constant GNAThub_Message_Access :=
                    GNAThub_Message_Access (Message_Ref.Message);
               begin
                  Message.Set_Flags (GPS.Kernel.Messages.Empty_Message_Flags);
                  Self.Add_External_Message (Message);
               end;
            end if;
         end loop;

         Self.Messages.Clear;
      end if;
   end Prepare_Loading;

   ----------------------
   -- Has_Data_To_Load --
   ----------------------

   overriding function Has_Data_To_Load
     (Self : External_Loader_Type) return Boolean is
   begin
      return not Self.Messages_To_Process.Is_Empty;
   end Has_Data_To_Load;

   -------------
   -- Cleanup --
   -------------

   overriding procedure Cleanup
     (Self : in out External_Loader_Type) is null;

   ---------------
   -- Load_Data --
   ---------------

   overriding procedure Load_Data
     (Self : in out External_Loader_Type)
   is
      M_Ref    : Message_Reference;
      Message  : GNAThub_Message_Access;
      Count    : Natural := 0;
   begin
      while not Self.Messages_To_Process.Is_Empty loop
         M_Ref := Self.Messages_To_Process.First_Element;

         if not M_Ref.Is_Empty then
            Message := GNAThub_Message_Access (M_Ref.Message);

            --  Insert the message in the module's tree

            Insert_Message
              (Self    => Self,
               Message => Message);
         end if;

         Count := Count + 1;

         Self.Messages_To_Process.Delete_First;

         exit when Count >= Load_Messages_Limit;
      end loop;
   end Load_Data;

   --------------------------
   -- Add_External_Message --
   --------------------------

   procedure Add_External_Message
     (Self    : in out External_Loader_Type'Class;
      Message : GNAThub_Message_Access) is
   begin
      Self.Messages_To_Process.Append
        (GPS.Kernel.Messages.References.Create
           (GPS.Kernel.Messages.Message_Access (Message)));
   end Add_External_Message;

end GNAThub.Loader.External;
