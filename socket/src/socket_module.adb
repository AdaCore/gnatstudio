-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Gtk.Main;                use Gtk.Main;

with GNAT.Sockets;            use GNAT.Sockets;

with Ada.Exceptions;          use Ada.Exceptions;
with Ada.IO_Exceptions;       use Ada.IO_Exceptions;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Glide_Kernel;            use Glide_Kernel;
with Glide_Kernel.Modules;    use Glide_Kernel.Modules;

with Traces;                  use Traces;

with Commands.Socket;         use Commands.Socket;
with Commands;                use Commands;

with Ada.Unchecked_Deallocation;

package body Socket_Module is

   Me : constant Debug_Handle := Create ("Socket_Module");

   GPS_Port : constant := 50000;
   --  <preferences>

   Max_Number_Of_Reads : constant := 128;

   use Commands.Command_Queues;

   type Socket_Set_Type_Access is access Socket_Set_Type;
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Socket_Set_Type, Socket_Set_Type_Access);

   type Socket_Module_Record is new Module_ID_Record with record
      Timeout_Handler  : Timeout_Handler_Id;
      Kernel           : Kernel_Handle;
      Commands_Present : Boolean := False;
      Commands_List    : List;

      Selector : Selector_Access;
      R_Set    : Socket_Set_Type_Access;
      W_Set    : Socket_Set_Type_Access;
      Address  : Sock_Addr_Type;
      Server   : Socket_Type;
   end record;
   type Socket_Module is access all Socket_Module_Record'Class;

   type Read_Data_Record is record
      Channel  : Stream_Access;
      Selector : Selector_Access;
      R_Set    : Socket_Set_Type_Access;
      W_Set    : Socket_Set_Type_Access;
      Socket   : Socket_Type;

      Buffer   : String (1 .. 4096);
      Index    : Natural := 1;
   end record;
   type Read_Data_Access is access Read_Data_Record;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Selector_Type, Selector_Access);

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Read_Data_Record, Read_Data_Access);

   package Read_Timeout is new Gtk.Main.Timeout (Read_Data_Access);

   ------------------------
   -- Local declarations --
   ------------------------

   procedure Destroy (Id : in out Socket_Module_Record);
   --  Free memory associated to Id and close the corresponding server socket.

   function Timeout_Process_Commands return Boolean;
   --  Timeout callback to process commands.

   procedure Close (R : in out Read_Data_Access);
   --  Free memory associated to R and shutdown all
   --  sockets associated to R.

   function Idle_Accept return Boolean;
   --  Accept new connections on the Server socket.

   function Idle_Read (Data : Read_Data_Access) return Boolean;
   --  ???

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Id : in out Socket_Module_Record) is
   begin
      Empty (Id.R_Set.all);
      Empty (Id.W_Set.all);
      Close_Selector (Id.Selector.all);

      Unchecked_Free (Id.R_Set);
      Unchecked_Free (Id.W_Set);
      Unchecked_Free (Id.Selector);

      Free (Id.Commands_List);

      Timeout_Remove (Id.Timeout_Handler);
      Close_Socket (Id.Server);
   end Destroy;

   -----------
   -- Close --
   -----------

   procedure Close (R : in out Read_Data_Access) is
   begin
      Empty (R.R_Set.all);
      Empty (R.W_Set.all);
      Close_Selector (R.Selector.all);

      Unchecked_Free (R.R_Set);
      Unchecked_Free (R.W_Set);
      Unchecked_Free (R.Selector);

      Close_Socket (R.Socket);

      Unchecked_Free (R);
   end Close;

   ------------------------------
   -- Timeout_Process_Commands --
   ------------------------------

   function Timeout_Process_Commands return Boolean is
      Data    : Socket_Module := Socket_Module (Socket_Module_ID);
      Success : Boolean;
   begin
      if Data.Commands_Present then
         if Is_Empty (Data.Commands_List) then
            Data.Commands_Present := False;
         else
            Success := Execute (Head (Data.Commands_List));
            Next (Data.Commands_List);
         end if;
      end if;

      return True;
   end Timeout_Process_Commands;

   ---------------
   -- Idle_Read --
   ---------------

   function Idle_Read (Data : Read_Data_Access) return Boolean is
      C               : Character;
      Status          : Selector_Status;
      Number_Of_Reads : Integer := 0;
   begin
      Set (Data.R_Set.all, Data.Socket);

      Read_Loop : loop
         Check_Selector
           (Data.Selector.all,
            Data.R_Set.all,
            Data.W_Set.all,
            Status,
            0.0001);

         case Status is
            when Expired =>
               return True;

            when Aborted =>
               return False;

            when Completed =>
               Number_Of_Reads := Number_Of_Reads + 1;
               exit Read_Loop when Number_Of_Reads > Max_Number_Of_Reads;

               Character'Read (Data.Channel, C);

               if Is_Graphic (C) then
                  Data.Buffer (Data.Index) := C;
                  Data.Index := Data.Index + 1;

                  if Data.Index = 4096 then
                     --  ??? must warn the user that the buffer has overflown.
                     Data.Index := 1;
                  end if;
               else
                  if Data.Index /= 1 then
                     declare
                        Command     : Socket_Command_Access;
                        Module_Data : constant Socket_Module :=
                          Socket_Module (Socket_Module_ID);
                        Success     : Boolean;

                     begin
                        if Data.Buffer (1 .. Data.Index - 1) = "exit" then
                           return False;
                        end if;

                        Create
                          (Command,
                           Module_Data.Kernel,
                           Data.Buffer (1 .. Data.Index - 1),
                           Data.Channel);

                        Success := Execute (Command);
                     end;
                  end if;

                  Data.Index := 1;

               end if;
         end case;
      end loop Read_Loop;

      return True;

   exception
      when Socket_Error | End_Error =>
         Trace (Me, "Communication error, closing socket.");
         return False;

      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         return False;
   end Idle_Read;

   -----------------
   -- Idle_Accept --
   -----------------

   function Idle_Accept return Boolean is
      Module_Data : constant Socket_Module := Socket_Module (Socket_Module_ID);
      Status      : Selector_Status;
   begin
      Set (Module_Data.R_Set.all, Module_Data.Server);

      Check_Selector
        (Module_Data.Selector.all,
         Module_Data.R_Set.all,
         Module_Data.W_Set.all,
         Status,
         0.0001);

      if Status = Completed then
         declare
            Data : Read_Data_Access;
            T    : Timeout_Handler_Id;
         begin
            Data := new Read_Data_Record;

            Accept_Socket
              (Socket_Module (Socket_Module_ID).Server,
               Data.Socket,
               Socket_Module (Socket_Module_ID).Address);

            Data.Channel := Stream (Data.Socket);
            Data.Selector := new Selector_Type;

            Create_Selector (Data.Selector.all);
            Data.R_Set := new Socket_Set_Type;
            Data.W_Set := new Socket_Set_Type;

            Set (Data.R_Set.all, Data.Socket);

            T := Read_Timeout.Add
              (100, Idle_Read'Access, Data, Close'Access);
         end;
      end if;

      return True;

   exception
      when Socket_Error | End_Error =>
         Trace (Me, "Communication error, closing server socket.");
         return False;

      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         return False;
   end Idle_Accept;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      T : Timeout_Handler_Id;
   begin
      Socket_Module_ID := new Socket_Module_Record;
      Socket_Module (Socket_Module_ID).Kernel := Kernel_Handle (Kernel);

      Socket_Module (Socket_Module_ID).Timeout_Handler :=
        Timeout_Add (100, Timeout_Process_Commands'Access);

      Socket_Module (Socket_Module_ID).Address.Addr :=
        Addresses (Get_Host_By_Name (Host_Name), 1);

      --  Get a socket address that is an Internet address and a port

      Socket_Module (Socket_Module_ID).Address.Port := GPS_Port;

      --  Create the server socket.
      Create_Socket (Socket_Module (Socket_Module_ID).Server);

      Set_Socket_Option
        (Socket_Module (Socket_Module_ID).Server,
         Socket_Level,
           (Reuse_Address, True));

      Bind_Socket
        (Socket_Module (Socket_Module_ID).Server,
         Socket_Module (Socket_Module_ID).Address);

      --  A server marks a socket as willing to receive connect events.

      Listen_Socket (Socket_Module (Socket_Module_ID).Server);

      Socket_Module (Socket_Module_ID).Selector := new Selector_Type;

      --  Create and launch the Timeout accept handler.

      Create_Selector (Socket_Module (Socket_Module_ID).Selector.all);
      Socket_Module (Socket_Module_ID).R_Set := new Socket_Set_Type;
      Socket_Module (Socket_Module_ID).W_Set := new Socket_Set_Type;

      Set (Socket_Module (Socket_Module_ID).R_Set.all,
           Socket_Module (Socket_Module_ID).Server);

      T := Timeout_Add (2000, Idle_Accept'Access);

      Register_Module
        (Module                  => Socket_Module_ID,
         Kernel                  => Kernel,
         Module_Name             => Socket_Module_Name,
         Priority                => Default_Priority,
         Contextual_Menu_Handler => null);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Register_Module;

end Socket_Module;
