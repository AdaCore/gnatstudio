------------------------------------------------------------------------------
--                               GNAT Studio                                --
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

with Commands;

with GPS.Kernel.Task_Manager;
with Language.Abstract_Language_Tree;

package body GNAThub.Loader is

   type Loader_Command
     (Loader : not null access Loader_Type'Class) is
     new Commands.Root_Command with null record;
   overriding function Execute
     (Self : access Loader_Command)
      return Commands.Command_Return_Type;
   --  Used to load GNAThub messages in the background

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self   : in out Loader_Type;
      Module : not null GNAThub.Module.GNAThub_Module_Id) is
   begin
      Self.Module := Module;
   end Initialize;

   ----------
   -- Load --
   ----------

   procedure Load (Self : in out Loader_Type'Class) is
      Aux : Commands.Command_Access;
   begin
      if not Self.Has_Data_To_Load then

         --  If there is not any data to load, warn the listeners that the
         --  loader has finished.

         for Listener of Self.Listeners loop
            Listener.On_Finish_Loading;
         end loop;

         return;
      end if;

      --  Launch the command that will load data in background.

      Aux := new Loader_Command (Self'Unchecked_Access);
      Self.Command :=
        GPS.Kernel.Task_Manager.Launch_Background_Command
          (Kernel   => Self.Module.Get_Kernel,
           Command  => Aux,
           Active   => True,
           Show_Bar => False);
   end Load;

   ---------------------
   -- Remove_Messages --
   ---------------------

   procedure Remove_Messages (Self : in out Loader_Type) is
      M_Ref   : Message_Reference;
      Message : GNAThub_Message_Access;
   begin
      while not Self.Messages.Is_Empty loop
         M_Ref := Self.Messages.First_Element;

         if not M_Ref.Is_Empty then
            Message := GNAThub_Message_Access (M_Ref.Message);

            --  Remove the message and decrement its counters

            Message.Decrement_Current_Counters;
            Message.Decrement_Total_Counters;
            Message.Remove;
         end if;

         Self.Messages.Delete_First;
      end loop;
   end Remove_Messages;

   -------------
   -- Cleanup --
   -------------

   procedure Cleanup (Self : in out Loader_Type) is
      use type GPS.Scripts.Commands.Scheduled_Command_Access;
   begin
      --  Interrupt the loading comand that runs in background if needed
      if Self.Command /= null then
         GPS.Kernel.Task_Manager.Interrupt_Queue
           (Self.Module.Get_Kernel, Self.Command);
         Self.Command := null;
      end if;
   end Cleanup;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self : access Loader_Command)
      return Commands.Command_Return_Type is
   begin
      Self.Loader.Load_Data;

      if Self.Loader.Has_Data_To_Load then
         return Commands.Execute_Again;

      else
         Self.Loader.Command := null;
         Self.Loader.Cleanup;

         for Listener of Self.Loader.Listeners loop
            Listener.On_Finish_Loading;
         end loop;

         return Commands.Success;
      end if;
   end Execute;

   -----------------------
   -- Register_Listener --
   -----------------------

   procedure Register_Listener
     (Self     : not null access Loader_Type'Class;
      Listener : not null access Loader_Listener_Interface'Class) is
   begin
      Self.Listeners.Append (Listener);
   end Register_Listener;

   -------------------------
   -- Unregister_Listener --
   -------------------------

   procedure Unregister_Listener
     (Self     : not null access Loader_Type'Class;
      Listener : not null access Loader_Listener_Interface'Class)
   is
      Position : Loader_Listener_Vectors.Cursor := Self.Listeners.Find
        (Listener);
   begin
      if Loader_Listener_Vectors.Has_Element (Position) then
         Self.Listeners.Delete (Position);
      end if;
   end Unregister_Listener;

   --------------------
   -- Insert_Message --
   --------------------

   procedure Insert_Message
     (Self    : in out Loader_Type'Class;
      Message : GNAThub_Message_Access)
   is
      M_Ref : Message_Reference;
   begin
      --  Add this message to the ones already loaded by the loaded

      M_Ref := GPS.Kernel.Messages.References.Create
        (GPS.Kernel.Messages.Message_Access (Message));
      Self.Messages.Append (M_Ref);

      --  Increment the message's total counters

      Message.Increment_Total_Counters;
   end Insert_Message;

end GNAThub.Loader;
