with GNAT.Expect; use GNAT.Expect;
with System;

package GNAT.Expect.Tty is

   ------------------
   --  TTY_Process --
   ------------------

   type TTY_Process_Descriptor is new Process_Descriptor with private;
   --  Similar, but the parent is set up as a full terminal (Unix sense, see
   --  tty(4)).

private

   procedure Interrupt (Descriptor : in out TTY_Process_Descriptor);
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
      Cmd   : in String;
      Args  : in System.Address);

   type TTY_Process_Descriptor is new Process_Descriptor with record
      Process    : System.Address;  --  Underlying structure used in C.
   end record;
end GNAT.Expect.Tty;
