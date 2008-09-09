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

pragma Warnings (Off);
with GNAT.TTY;
pragma Warnings (On);
with GNAT.Expect; use GNAT.Expect;

with System;

package GNAT.Expect.TTY is

   ------------------
   --  TTY_Process --
   ------------------

   type TTY_Process_Descriptor is new Process_Descriptor with private;
   --  Similar, but the parent is set up as a full terminal (Unix sense, see
   --  tty(4)).

   procedure Pseudo_Descriptor
     (Descriptor  : out TTY_Process_Descriptor'Class;
      TTY         : GNAT.TTY.TTY_Handle;
      Buffer_Size : Natural := 4096);
   --  Given a terminal descriptor (TTY), create a pseudo process descriptor
   --  to be used with GNAT.Expect.
   --  Note that it is invalid to call Close, Interrupt, Send_Signal on the
   --  resulting descriptor. To deallocate memory associated with Process,
   --  call Close_Pseudo_Descriptor instead.

   procedure Close_Pseudo_Descriptor
     (Descriptor : in out TTY_Process_Descriptor);
   --  Free memory and ressources associated with Descriptor.
   --  Will *not* close the associated TTY, it is the caller's responsibility
   --  to call GNAT.TTY.Close_TTY.

   procedure Interrupt (Pid : Integer);
   --  Interrupt a process given its pid

   overriding procedure Send
     (Descriptor   : in out TTY_Process_Descriptor;
      Str          : String;
      Add_LF       : Boolean := True;
      Empty_Buffer : Boolean := False);
   --  See parent

   procedure Set_Use_Pipes
     (Descriptor : in out TTY_Process_Descriptor;
      Use_Pipes  : Boolean);
   --  Tell Expect.TTY wether to use Pipes or Console (on windows). Need to be
   --  set before spawning the process. Default is to use Pipes.

   procedure Set_Size
     (Descriptor : in out TTY_Process_Descriptor'Class;
      Rows       : Natural;
      Columns    : Natural);
   --  Sets up the size of the terminal as reported to the spawned process.

private

   overriding procedure Close
     (Descriptor : in out TTY_Process_Descriptor;
      Status     : out Integer);

   overriding procedure Close
     (Descriptor : in out TTY_Process_Descriptor);

   overriding procedure Interrupt (Descriptor : in out TTY_Process_Descriptor);
   --  When we use pseudo-terminals, we do not need to use signals to
   --  interrupt the debugger, we can simply send the appropriate character.
   --  This provides a better support for remote debugging for instance.

   procedure Set_Up_Communications
     (Pid        : in out TTY_Process_Descriptor;
      Err_To_Out : Boolean;
      Pipe1      : access Pipe_Type;
      Pipe2      : access Pipe_Type;
      Pipe3      : access Pipe_Type);

   procedure Set_Up_Parent_Communications
     (Pid   : in out TTY_Process_Descriptor;
      Pipe1 : in out Pipe_Type;
      Pipe2 : in out Pipe_Type;
      Pipe3 : in out Pipe_Type);

   procedure Set_Up_Child_Communications
     (Pid   : in out TTY_Process_Descriptor;
      Pipe1 : in out Pipe_Type;
      Pipe2 : in out Pipe_Type;
      Pipe3 : in out Pipe_Type;
      Cmd   : String;
      Args  : System.Address);

   type TTY_Process_Descriptor is new Process_Descriptor with record
      Process   : System.Address;  --  Underlying structure used in C.
      Use_Pipes : Boolean := True;
   end record;

end GNAT.Expect.TTY;
