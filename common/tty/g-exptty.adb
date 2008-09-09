------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                      G N A T . E X P E C T . T T Y                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                    Copyright (C) 2000-2008, AdaCore                      --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT is maintained by Ada Core Technologies Inc (http://www.gnat.com).   --
--                                                                          --
------------------------------------------------------------------------------

with GNAT.IO;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with System;      use System;

package body GNAT.Expect.TTY is

   -----------
   -- Close --
   -----------

   overriding procedure Close
     (Descriptor : in out TTY_Process_Descriptor;
      Status     : out Integer)
   is
      procedure Terminate_Process (Process : System.Address);
      pragma Import (C, Terminate_Process, "gvd_terminate_process");

      function Waitpid (Process : System.Address) return Integer;
      pragma Import (C, Waitpid, "gvd_waitpid");
      --  Wait for a specific process id, and return its exit code

      procedure Free_Process (Process : System.Address);
      pragma Import (C, Free_Process, "gvd_free_process");

   begin
      --  If we haven't already closed the process
      if Descriptor.Process /= System.Null_Address then
         Close (Descriptor.Input_Fd);

         if Descriptor.Error_Fd /= Descriptor.Output_Fd then
            Close (Descriptor.Error_Fd);
         end if;

         Close (Descriptor.Output_Fd);

         --  Send a Ctrl-C to the process first. This way, if the
         --  launched process is a "sh" or "cmd", the child processes
         --  will get terminated as well. Otherwise, terminating the
         --  main process brutally will leave the children running.
         Interrupt (Descriptor);
         delay (0.05);

         Terminate_Process (Descriptor.Process);
         Status := Waitpid (Descriptor.Process);
         Free_Process (Descriptor.Process'Address);
         Descriptor.Process := System.Null_Address;

         GNAT.OS_Lib.Free (Descriptor.Buffer);
         Descriptor.Buffer_Size := 0;
      end if;
   end Close;

   overriding procedure Close (Descriptor : in out TTY_Process_Descriptor) is
      Status : Integer;
   begin
      Close (Descriptor, Status);
   end Close;

   -----------------------------
   -- Close_Pseudo_Descriptor --
   -----------------------------

   procedure Close_Pseudo_Descriptor
     (Descriptor : in out TTY_Process_Descriptor) is
   begin
      Descriptor.Buffer_Size := 0;
      GNAT.OS_Lib.Free (Descriptor.Buffer);
   end Close_Pseudo_Descriptor;

   ---------------
   -- Interrupt --
   ---------------

   overriding procedure Interrupt
     (Descriptor : in out TTY_Process_Descriptor)
   is
      procedure Internal (Process : System.Address);
      pragma Import (C, Internal, "gvd_interrupt_process");

   begin
      if Descriptor.Process /= System.Null_Address then
         Internal (Descriptor.Process);
      end if;
   end Interrupt;

   procedure Interrupt (Pid : Integer) is
      procedure Internal (Pid : Integer);
      pragma Import (C, Internal, "gvd_interrupt_pid");

   begin
      Internal (Pid);
   end Interrupt;

   ----------
   -- Send --
   ----------

   overriding procedure Send
     (Descriptor   : in out TTY_Process_Descriptor;
      Str          : String;
      Add_LF       : Boolean := True;
      Empty_Buffer : Boolean := False)
   is
      Header : String (1 .. 5);
      Length : Natural;
      Ret    : Natural;

      procedure Internal
        (Process : System.Address;
         S       : in out String;
         Length  : Natural;
         Ret     : out Natural);
      --  ??? missing spec
      pragma Import (C, Internal, "gvd_send_header");

   begin
      Length := Str'Length;

      if Add_LF then
         Length := Length + 1;
      end if;

      Internal (Descriptor.Process, Header, Length, Ret);

      if Ret = 1 then
         --  Need to use the header

         GNAT.Expect.Send
           (Process_Descriptor (Descriptor),
            Header & Str, Add_LF, Empty_Buffer);

      else
         GNAT.Expect.Send
           (Process_Descriptor (Descriptor),
            Str, Add_LF, Empty_Buffer);
      end if;
   end Send;

   --------------
   -- Set_Size --
   --------------

   procedure Set_Size
     (Descriptor : in out TTY_Process_Descriptor'Class;
      Rows       : Natural;
      Columns    : Natural)
   is
      procedure Internal (Process : System.Address; R, C : Integer);
      pragma Import (C, Internal, "gvd_setup_winsize");

   begin
      if Descriptor.Process /= System.Null_Address then
         Internal (Descriptor.Process, Rows, Columns);
      end if;
   end Set_Size;

   -----------------------
   -- Pseudo_Descriptor --
   -----------------------

   procedure Pseudo_Descriptor
     (Descriptor  : out TTY_Process_Descriptor'Class;
      TTY         : GNAT.TTY.TTY_Handle;
      Buffer_Size : Natural := 4096) is
   begin
      Descriptor.Input_Fd  := GNAT.TTY.TTY_Descriptor (TTY);
      Descriptor.Output_Fd := Descriptor.Input_Fd;

      --  Create the buffer

      Descriptor.Buffer_Size := Buffer_Size;

      if Buffer_Size /= 0 then
         Descriptor.Buffer := new String (1 .. Positive (Buffer_Size));
      end if;
   end Pseudo_Descriptor;

   ---------------------------
   -- Set_Up_Communications --
   ---------------------------

   overriding procedure Set_Up_Communications
     (Pid        : in out TTY_Process_Descriptor;
      Err_To_Out : Boolean;
      Pipe1      : access Pipe_Type;
      Pipe2      : access Pipe_Type;
      Pipe3      : access Pipe_Type)
   is
      pragma Unreferenced (Err_To_Out, Pipe1, Pipe2, Pipe3);

      procedure Internal (Process : System.Address);
      pragma Import (C, Internal, "gvd_setup_communication");

   begin
      Internal (Pid.Process'Address);
   end Set_Up_Communications;

   ----------------------------------
   -- Set_Up_Parent_Communications --
   ----------------------------------

   overriding procedure Set_Up_Parent_Communications
     (Pid   : in out TTY_Process_Descriptor;
      Pipe1 : in out Pipe_Type;
      Pipe2 : in out Pipe_Type;
      Pipe3 : in out Pipe_Type)
   is
      pragma Unreferenced (Pipe1, Pipe2, Pipe3);

      procedure Internal
        (Process  : System.Address;
         Inputfp  : out File_Descriptor;
         Outputfp : out File_Descriptor;
         Errorfp  : out File_Descriptor;
         Pid      : out Process_Id);
      pragma Import (C, Internal, "gvd_setup_parent_communication");

   begin
      Internal
        (Pid.Process, Pid.Input_Fd, Pid.Output_Fd, Pid.Error_Fd, Pid.Pid);
   end Set_Up_Parent_Communications;

   ---------------------------------
   -- Set_Up_Child_Communications --
   ---------------------------------

   overriding procedure Set_Up_Child_Communications
     (Pid   : in out TTY_Process_Descriptor;
      Pipe1 : in out Pipe_Type;
      Pipe2 : in out Pipe_Type;
      Pipe3 : in out Pipe_Type;
      Cmd   : String;
      Args  : System.Address)
   is
      pragma Unreferenced (Pipe1, Pipe2, Pipe3, Cmd);
      function Internal
        (Process : System.Address; Argv : System.Address; Use_Pipes : Integer)
         return Process_Id;
      pragma Import (C, Internal, "gvd_setup_child_communication");

   begin
      Pid.Pid := Internal (Pid.Process, Args, Boolean'Pos (Pid.Use_Pipes));
   end Set_Up_Child_Communications;

   -------------------
   -- Set_Use_Pipes --
   -------------------

   procedure Set_Use_Pipes
     (Descriptor : in out TTY_Process_Descriptor;
      Use_Pipes  : Boolean) is
   begin
      Descriptor.Use_Pipes := Use_Pipes;
   end Set_Use_Pipes;

end GNAT.Expect.TTY;
