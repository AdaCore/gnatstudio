with GNAT.OS_Lib; use GNAT.OS_Lib;

package body GNAT.Expect.Tty is

   ---------------
   -- Interrupt --
   ---------------

   procedure Interrupt (Descriptor : in out TTY_Process_Descriptor) is
   begin
      Send (Descriptor, Str => "" & Character'Val (3));
   end Interrupt;

   ---------------------------
   -- Set_Up_Communications --
   ---------------------------

   procedure Set_Up_Communications
     (Pid        : in out TTY_Process_Descriptor;
      Err_To_Out : Boolean;
      Pipe1      : access Pipe_Type;
      Pipe2      : access Pipe_Type;
      Pipe3      : access Pipe_Type)
   is
      procedure Internal (Process    : System.Address);
      pragma Import (C, Internal, "setupCommunication");

   begin
      Internal (Pid.Process'Address);
   end Set_Up_Communications;

   ----------------------------------
   -- Set_Up_Parent_Communications --
   ----------------------------------

   procedure Set_Up_Parent_Communications
     (Pid   : in out TTY_Process_Descriptor;
      Pipe1 : in out Pipe_Type;
      Pipe2 : in out Pipe_Type;
      Pipe3 : in out Pipe_Type)
   is
      procedure Internal
        (Process  : System.Address;
         Inputfp  : out File_Descriptor;
         Outputfp : out File_Descriptor;
         Errorfp  : out File_Descriptor;
         Pid      : out Process_Id);
      pragma Import (C, Internal, "setupParentCommunication");
   begin
      Internal
        (Pid.Process, Pid.Input_Fd, Pid.Output_Fd, Pid.Error_Fd, Pid.Pid);
   end Set_Up_Parent_Communications;

   ---------------------------------
   -- Set_Up_Child_Communications --
   ---------------------------------

   procedure Set_Up_Child_Communications
     (Pid   : in out TTY_Process_Descriptor;
      Pipe1 : in out Pipe_Type;
      Pipe2 : in out Pipe_Type;
      Pipe3 : in out Pipe_Type;
      Cmd   : in String;
      Args  : in System.Address)
   is
      procedure Internal (Process   : System.Address; Argv : System.Address);
      pragma Import (C, Internal, "setupChildCommunication");
   begin
      Internal (Pid.Process, Args);
   end Set_Up_Child_Communications;

end GNAT.Expect.Tty;
