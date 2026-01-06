------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2023-2026, AdaCore                  --
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

with DAP.Tools;

package body DAP.Clients.Next is

   ------------
   -- Create --
   ------------

   function Create
     (Kernel      : not null Kernel_Handle;
      Thread_Id   : Integer;
      Instruction : Boolean)
      return Next_Request_Access
   is
      Self : constant Next_Request_Access := new Next_Request (Kernel);
   begin
      Self.Parameters.arguments.threadId := Thread_Id;
      if Instruction then
         Self.Parameters.arguments.granularity :=
           (Is_Set => True, Value => DAP.Tools.Enum.instruction);
      end if;

      return Self;
   end Create;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self        : in out Next_Request;
      Client      : not null access DAP.Clients.DAP_Client'Class;
      Result      : DAP.Tools.NextResponse;
      New_Request : in out DAP_Request_Access)
   is
      pragma Unreferenced (New_Request);
   begin
      Client.On_Continue;
   end On_Result_Message;

   ---------------
   -- Send_Next --
   ---------------

   procedure Send_Next (Client : not null access DAP.Clients.DAP_Client'Class)
   is
      Request : DAP.Requests.DAP_Request_Access :=
        DAP.Requests.DAP_Request_Access
          (Create
             (Kernel      => Kernel_Handle (Client.Kernel),
              Thread_Id   => Client.Get_Current_Thread,
              Instruction => False));
   begin
      Client.Enqueue (Request);
   end Send_Next;

   ---------------------------
   -- Send_Next_Instruction --
   ---------------------------

   procedure Send_Next_Instruction
     (Client : not null access DAP.Clients.DAP_Client'Class)
   is
      Request : DAP.Requests.DAP_Request_Access :=
        DAP.Requests.DAP_Request_Access
          (Create
             (Kernel      => Kernel_Handle (Client.Kernel),
              Thread_Id   => Client.Get_Current_Thread,
              Instruction => True));
   begin
      Client.Enqueue (Request);
   end Send_Next_Instruction;

end DAP.Clients.Next;
