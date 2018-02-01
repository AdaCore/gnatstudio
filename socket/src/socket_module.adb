------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2018, AdaCore                     --
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

with Glib.Main;               use Glib.Main;
with GNAT.Sockets;            use GNAT.Sockets;
with GNATCOLL.Scripts;        use GNATCOLL.Scripts;
with Ada.IO_Exceptions;       use Ada.IO_Exceptions;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with GPS.Kernel;              use GPS.Kernel;
with GPS.Kernel.Modules;      use GPS.Kernel.Modules;
with GPS.Kernel.Scripts;      use GPS.Kernel.Scripts;
with GNATCOLL.Traces;                  use GNATCOLL.Traces;
with Commands.Socket;         use Commands.Socket;
with Commands;                use Commands;
with GPS.Intl;                use GPS.Intl;

with Ada.Unchecked_Deallocation;
with GNAT.Strings;

package body Socket_Module is

   Me : constant Trace_Handle := Create ("GPS.OTHERS.SOCKET_MODULE");

   Socket_Module_ID   : GPS.Kernel.Modules.Module_ID;
   Socket_Module_Name : constant String := "Socket";

   Max_Number_Of_Reads : constant := 128;

   use Commands.Command_Lists;

   type Socket_Set_Type_Access is access Socket_Set_Type;
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Socket_Set_Type, Socket_Set_Type_Access);

   type Read_Data_Record;
   type Read_Data_Access is access Read_Data_Record;

   type Socket_Module_Record is new Module_ID_Record with record
      Timeout_Handler  : Glib.Main.G_Source_Id;
      Commands_Present : Boolean := False;
      Commands_List    : List;

      Selector  : Selector_Access;
      R_Set     : Socket_Set_Type_Access;
      W_Set     : Socket_Set_Type_Access;
      Address   : Sock_Addr_Type;
      Server    : Socket_Type;
      Data_List : Read_Data_Access;
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

      Name     : GNAT.Strings.String_Access := new String'("");
      Next     : Read_Data_Access;

      Timeout  : Glib.Main.G_Source_Id;
      --  The handler for Read()
   end record;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Selector_Type, Selector_Access);

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Read_Data_Record, Read_Data_Access);

   package Read_Timeout is new Glib.Main.Generic_Sources (Read_Data_Access);

   ------------------------
   -- Local declarations --
   ------------------------

   overriding procedure Destroy (Id : in out Socket_Module_Record);
   --  Free memory associated to Id and close the corresponding server socket.

   function Timeout_Process_Commands return Boolean;
   --  Timeout callback to process commands.

   procedure Close (R : in out Read_Data_Access);
   --  Free memory associated to R and shutdown all
   --  sockets associated to R.

   function Idle_Accept return Boolean;
   --  Accept new connections on the Server socket.

   function Idle_Read (Data : Read_Data_Access) return Boolean;
   --  Read input on Data.Socket and process it.

   procedure Socket_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Interactive command handler for the socket module.

   procedure Socket_Static_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Handles all shell commands for static methods

   function Find_Data (Id : String) return Read_Data_Access;
   --  Find the Read_Data associated with "Id"
   --  Return null if Id can't be found.

   ---------------
   -- Find_Data --
   ---------------

   function Find_Data (Id : String) return Read_Data_Access is
      Module_Data : constant Socket_Module := Socket_Module (Socket_Module_ID);
      Temp        : Read_Data_Access;
   begin
      Temp := Module_Data.Data_List;

      while Temp /= null loop
         if Temp.Name.all = Id then
            return Temp;
         end if;

         Temp := Temp.Next;
      end loop;

      return null;
   end Find_Data;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Id : in out Socket_Module_Record) is
   begin
      Empty (Id.R_Set.all);
      Empty (Id.W_Set.all);
      Close_Selector (Id.Selector.all);

      Unchecked_Free (Id.R_Set);
      Unchecked_Free (Id.W_Set);
      Unchecked_Free (Id.Selector);

      Free (Id.Commands_List);

      Glib.Main.Remove (Id.Timeout_Handler);
      Close_Socket (Id.Server);
   end Destroy;

   -----------
   -- Close --
   -----------

   procedure Close (R : in out Read_Data_Access) is
      Module_Data : constant Socket_Module := Socket_Module (Socket_Module_ID);
      Temp        : Read_Data_Access;
   begin
      Temp := Module_Data.Data_List;

      if Temp = R then
         Module_Data.Data_List := Temp.Next;
      else
         while Temp /= null and then Temp.Next /= R loop
            Temp := Temp.Next;

            pragma Assert (Temp /= null);
            exit when Temp = null;
         end loop;

         if Temp /= null then
            Temp.Next := R.Next;
         end if;
      end if;

      Empty (R.R_Set.all);
      Empty (R.W_Set.all);
      Close_Selector (R.Selector.all);

      Unchecked_Free (R.R_Set);
      Unchecked_Free (R.W_Set);
      Unchecked_Free (R.Selector);

      Close_Socket (R.Socket);

      GNAT.Strings.Free (R.Name);
      Unchecked_Free (R);
   end Close;

   ------------------------------
   -- Timeout_Process_Commands --
   ------------------------------

   function Timeout_Process_Commands return Boolean is
      Data   : constant Socket_Module := Socket_Module (Socket_Module_ID);
      Result : Command_Return_Type;
      pragma Unreferenced (Result);
   begin
      if Data.Commands_Present then
         if Is_Empty (Data.Commands_List) then
            Data.Commands_Present := False;
         else
            Result := Execute (Data.Commands_List.First_Element);
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
                        Result      : Command_Return_Type;
                        pragma Unreferenced (Result);

                     begin
                        if Data.Buffer (1 .. Data.Index - 1) = "logout" then
                           return False;

                        elsif Data.Index > 4
                          and then Data.Buffer (1 .. 3) = "id "
                        then
                           GNAT.Strings.Free (Data.Name);
                           Data.Name :=
                             new String'(Data.Buffer (4 .. Data.Index - 1));
                           String'Write (Data.Channel, "id set to '" &
                             Data.Name.all & "'" & ASCII.LF & "GPS>> ");

                        elsif Data.Index > 8
                          and then Data.Buffer (1 .. 7) = "python "
                        then
                           Create
                             (Command,
                              Get_Kernel (Module_Data.all),
                              Data.Buffer (8 .. Data.Index - 1),
                              "Python",
                              Data.Channel);
                           Result := Execute (Command);
                        else
                           Create
                             (Command,
                              Get_Kernel (Module_Data.all),
                              Data.Buffer (1 .. Data.Index - 1),
                              Stream => Data.Channel);
                           Result := Execute (Command);
                        end if;
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
         Trace (Me, E);
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

            Data.Next := Module_Data.Data_List;
            Module_Data.Data_List := Data;

            Set (Data.R_Set.all, Data.Socket);
            String'Write (Data.Channel, "GPS>> ");

            Data.Timeout := Read_Timeout.Timeout_Add
              (100, Idle_Read'Access, Data, Notify => Close'Access);
         end;
      end if;

      return True;

   exception
      when Socket_Error | End_Error =>
         Trace (Me, "Communication error, closing server socket.");
         return False;

      when E : others =>
         Trace (Me, E);
         return False;
   end Idle_Accept;

   ----------------------------
   -- Socket_Command_Handler --
   ----------------------------

   procedure Socket_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Kernel       : constant Kernel_Handle := Get_Kernel (Data);
      Socket_Class : constant Class_Type := New_Class (Kernel, "Socket");
      Read_Data    : Read_Data_Access;
      Inst        : constant Class_Instance := Nth_Arg (Data, 1, Socket_Class);
   begin
      if Command = Constructor_Method then
         Read_Data := Find_Data (Nth_Arg (Data, 2));
         if Read_Data = null then
            Set_Error_Msg (Data, Command & ": " & (-"invalid id"));
            return;
         end if;
         Set_Data (Inst, Socket_Class, String'(Nth_Arg (Data, 2)));

      elsif Command = "send" then
         Read_Data := Find_Data (String'(Get_Data (Inst, Socket_Class)));
         if Read_Data /= null then
            String'Write (Read_Data.Channel, Nth_Arg (Data, 2));
         end if;

      elsif Command = "close" then
         Read_Data := Find_Data (String'(Get_Data (Inst, Socket_Class)));
         if Read_Data /= null then
            Glib.Main.Remove (Read_Data.Timeout);
         end if;
      end if;

   exception
      when Socket_Error | End_Error =>
         Trace (Me, "Communication error, closing socket.");

      when E : others => Trace (Me, E);
   end Socket_Command_Handler;

   -----------------------------------
   -- Socket_Static_Command_Handler --
   -----------------------------------

   procedure Socket_Static_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Read_Data    : Read_Data_Access;
   begin
      if Command = "list" then
         Set_Return_Value_As_List (Data);
         Read_Data := Socket_Module (Socket_Module_ID).Data_List;
         while Read_Data /= null loop
            Set_Return_Value (Data, Read_Data.Name.all);
            Read_Data := Read_Data.Next;
         end loop;
      end if;
   end Socket_Static_Command_Handler;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Register_Module (Kernel, Default_GPS_Port);
   end Register_Module;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Port   : Natural)
   is
      T : Glib.Main.G_Source_Id;
      pragma Unreferenced (T);
      Socket_Class : constant Class_Type := New_Class (Kernel, "Socket");

   begin
      GNAT.Sockets.Initialize;

      Socket_Module_ID := new Socket_Module_Record;
      Socket_Module (Socket_Module_ID).Timeout_Handler :=
        Glib.Main.Timeout_Add (100, Timeout_Process_Commands'Access);
      Socket_Module (Socket_Module_ID).Address.Addr :=
        Inet_Addr ("127.0.0.1");

      --  Get a socket address that is an Internet address and a port

      Socket_Module (Socket_Module_ID).Address.Port := Port_Type (Port);

      --  Create the server socket.
      Create_Socket (Socket_Module (Socket_Module_ID).Server);

      Set_Socket_Option
        (Socket_Module (Socket_Module_ID).Server,
         Socket_Level, (Reuse_Address, True));

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
        (Module      => Socket_Module_ID,
         Kernel      => Kernel,
         Module_Name => Socket_Module_Name,
         Priority    => Default_Priority);

      Register_Command
        (Kernel, Constructor_Method,
         Class        => Socket_Class,
         Minimum_Args => 1,
         Maximum_Args => 1,
         Handler      => Socket_Command_Handler'Access);
      Register_Command
        (Kernel, "send",
         Class        => Socket_Class,
         Minimum_Args => 1,
         Maximum_Args => 1,
         Handler      => Socket_Command_Handler'Access);
      Register_Command
        (Kernel, "close",
         Class        => Socket_Class,
         Handler      => Socket_Command_Handler'Access);
      Register_Command
        (Kernel, "list",
         Class         => Socket_Class,
         Static_Method => True,
         Handler       => Socket_Static_Command_Handler'Access);

   exception
      when E : others => Trace (Me, E);
   end Register_Module;

end Socket_Module;
