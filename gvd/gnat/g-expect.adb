with GNAT.IO;
with GNAT.OS_Lib;   use GNAT.OS_Lib;
with GNAT.Regpat;   use GNAT.Regpat;
with System;        use System;
with Unchecked_Conversion;
with Unchecked_Deallocation;

package body GNAT.Expect is

   function To_Pid is new
     Unchecked_Conversion (OS_Lib.Process_Id, Process_Id);

   type Array_Of_Pd is array (Positive range <>) of Process_Descriptor_Access;

   procedure Expect_Internal
     (Descriptors : in out Array_Of_Pd;
      Result      : out Expect_Match;
      Timeout     : Integer;
      Full_Buffer : Boolean);
   --  Internal function used to read from the process Descriptor.
   --  Three outputs are possible:
   --     Result=Expect_Timeout, if no output was available before the timeout
   --        expired.
   --     Result=Expect_Full_Buffer, if Full_Buffer is True and some characters
   --        had to be discarded from the internal buffer of Descriptor.
   --     Result=<integer>, indicates how many characters were added to the
   --        internal buffer. These characters are from indexes
   --        Descriptor.Buffer_Index - Result + 1 .. Descriptor.Buffer_Index
   --  Process_Died is raised if the process is no longer valid.

   procedure Reinitialize_Buffer (Descriptor : in out Process_Descriptor);
   --  Reinitialize the internal buffer.
   --  The buffer is deleted up to the end of the last match.

   procedure Free is new Unchecked_Deallocation
     (Pattern_Matcher, Pattern_Matcher_Access);

   ------------------------------
   -- Target dependent section --
   ------------------------------

   function Dup (Fd : File_Descriptor) return File_Descriptor;
   pragma Import (C, Dup);

   procedure Dup2 (Old_Fd, New_Fd : File_Descriptor);
   pragma Import (C, Dup2);

   type Pipe_Type is record
      Input, Output : File_Descriptor;
   end record;

   procedure Kill (Pid : Process_Id; Sig_Num : Integer);
   pragma Import (C, Kill);

   procedure Create_Pipe (Pipe : access Pipe_Type);
   pragma Import (C, Create_Pipe, "__gnat_pipe");

   function Read
     (Fd   : File_Descriptor;
      A    : System.Address;
      N    : Integer) return Integer;
   pragma Import (C, Read, "read");
   --  Read N bytes to address A from file referenced by FD. Returned value
   --  is count of bytes actually read, which can be less than N at EOF.

   procedure Close (Fd : File_Descriptor);
   pragma Import (C, Close);
   --  Close a file given its file descriptor.

   function Write
     (Fd   : File_Descriptor;
      A    : System.Address;
      N    : Integer) return Integer;
   pragma Import (C, Write, "write");
   --  Read N bytes to address A from file referenced by FD. Returned value
   --  is count of bytes actually read, which can be less than N at EOF.

   function Poll
     (Fds     : System.Address;
      Num_Fds : Integer;
      Timeout : Integer;
      Is_Set  : System.Address) return Integer;
   pragma Import (C, Poll, "__gnat_expect_poll");
   --  Check whether there is any data waiting on the file descriptor
   --  Out_fd, and wait if there is none, at most Timeout milliseconds
   --  Returns -1 in case of error, 0 if the timeout expired before
   --  data became available.
   --  Out_Is_Set is set to 1 if data was available, 0 otherwise.

   ---------
   -- "+" --
   ---------

   function "+" (S : String) return GNAT.OS_Lib.String_Access is
   begin
      return new String'(S);
   end "+";

   ---------
   -- "+" --
   ---------

   function "+"
     (P : GNAT.Regpat.Pattern_Matcher) return Pattern_Matcher_Access is
   begin
      return new GNAT.Regpat.Pattern_Matcher'(P);
   end "+";

   ----------------------
   -- Add_Input_Filter --
   ----------------------

   procedure Add_Input_Filter
     (Descriptor : in out Process_Descriptor;
      Filter     : Filter_Function;
      User_Data  : System.Address := System.Null_Address;
      After      : Boolean := False)
   is
      Current : Filter_List := Descriptor.In_Filters;
   begin
      if After then
         while Current /= null and then Current.Next /= null loop
            Current := Current.Next;
         end loop;

         if Current = null then
            Descriptor.In_Filters :=
              new Filter_List_Elem'
                (Filter => Filter, User_Data => User_Data, Next => null);
         else
            Current.Next :=
              new Filter_List_Elem'
                (Filter => Filter, User_Data => User_Data, Next => null);
         end if;
      else
         Descriptor.In_Filters :=
           new Filter_List_Elem'
             (Filter => Filter, User_Data => User_Data,
              Next => Descriptor.In_Filters);
      end if;
   end Add_Input_Filter;

   -------------------------
   -- Remove_Input_Filter --
   -------------------------

   procedure Remove_Input_Filter
     (Descriptor : in out Process_Descriptor;
      Filter     : Filter_Function)
   is
      Previous : Filter_List := null;
      Current  : Filter_List := Descriptor.In_Filters;
   begin
      while Current /= null loop
         if Current.Filter = Filter then
            if Previous = null then
               Descriptor.In_Filters := Current.Next;
            else
               Previous.Next := Current.Next;
            end if;
         end if;
         Previous := Current;
         Current := Current.Next;
      end loop;
   end Remove_Input_Filter;

   -----------------------
   -- Add_Output_Filter --
   -----------------------

   procedure Add_Output_Filter
     (Descriptor : in out Process_Descriptor;
      Filter     : Filter_Function;
      User_Data  : System.Address := System.Null_Address;
      After      : Boolean := False)
   is
      Current : Filter_List := Descriptor.Out_Filters;
   begin
      if After then
         while Current /= null and then Current.Next /= null loop
            Current := Current.Next;
         end loop;

         if Current = null then
            Descriptor.Out_Filters :=
              new Filter_List_Elem'
                (Filter => Filter, User_Data => User_Data, Next  => null);
         else
            Current.Next :=
              new Filter_List_Elem'
                (Filter => Filter, User_Data => User_Data, Next  => null);
         end if;
      else
         Descriptor.Out_Filters :=
           new Filter_List_Elem'
             (Filter => Filter, User_Data => User_Data,
              Next => Descriptor.Out_Filters);
      end if;
   end Add_Output_Filter;

   --------------------------
   -- Remove_Output_Filter --
   --------------------------

   procedure Remove_Output_Filter
     (Descriptor : in out Process_Descriptor;
      Filter     : Filter_Function)
   is
      Previous : Filter_List := null;
      Current  : Filter_List := Descriptor.Out_Filters;
   begin
      while Current /= null loop
         if Current.Filter = Filter then
            if Previous = null then
               Descriptor.Out_Filters := Current.Next;
            else
               Previous.Next := Current.Next;
            end if;
         end if;
         Previous := Current;
         Current := Current.Next;
      end loop;
   end Remove_Output_Filter;

   -----------
   -- Close --
   -----------

   procedure Close (Descriptor : in out Process_Descriptor) is
      Success : Boolean;
      Pid     : OS_Lib.Process_Id;

   begin
      Close (Descriptor.Input_Fd);

      if Descriptor.Error_Fd /= Descriptor.Output_Fd then
         Close (Descriptor.Error_Fd);
      end if;

      Close (Descriptor.Output_Fd);
      Kill (Descriptor.Pid, 9);

      GNAT.OS_Lib.Free (Descriptor.Buffer);
      Descriptor.Buffer_Size := 0;

      Wait_Process (Pid, Success);
      Descriptor.Pid := To_Pid (Pid);
   end Close;

   -----------------
   -- Send_Signal --
   -----------------

   procedure Send_Signal
     (Descriptor : Process_Descriptor;
      Signal     : Integer) is
   begin
      Kill (Descriptor.Pid, Signal);
      --  ??? Need to check process status here.
   end Send_Signal;

   ---------------
   -- Interrupt --
   ---------------

   procedure Interrupt (Descriptor : Process_Descriptor) is
      SIGINT : constant := 2;
   begin
      Send_Signal (Descriptor, SIGINT);
   end Interrupt;

   ---------------------
   -- Expect_Internal --
   ---------------------

   procedure Expect_Internal
     (Descriptors : in out Array_Of_Pd;
      Result      : out Expect_Match;
      Timeout     : Integer;
      Full_Buffer : Boolean)
   is
      Num_Descriptors : Integer;
      Buffer_Size     : Integer;

      N               : Integer;
      Current_Filter  : Filter_List;

      type File_Descriptor_Array is
        array (Descriptors'Range) of File_Descriptor;
      Fds : File_Descriptor_Array;

      type Integer_Array is array (Descriptors'Range) of Integer;
      Is_Set : Integer_Array;

   begin
      for J in Descriptors'Range loop
         Fds (J) := Descriptors (J).Output_Fd;
      end loop;

      --  Until we match or we have a timeout

      loop
         Num_Descriptors :=
           Poll (Fds'Address, Fds'Length, Timeout, Is_Set'Address);

         case Num_Descriptors is

            --  Error ?
            when -1 => raise Process_Died;

            --  Timeout ?
            when 0  =>
               Result := Expect_Timeout;
               return;

            --  some input
            when others =>
               for J in Descriptors'Range loop
                  if Is_Set (J) = 1 then

                     Buffer_Size := Descriptors (J).Buffer_Size;
                     if Buffer_Size = 0 then
                        Buffer_Size := 4096;
                     end if;

                     declare
                        Buffer : aliased String (1 .. Buffer_Size);
                        --  No point in reading more than we can process, so
                        --  we limit we size of the buffer.

                     begin
                        N := Read (Descriptors (J).Output_Fd, Buffer'Address,
                                   Buffer'Length);

                        --  If N is null it means that the external process
                        --  died.

                        if N = 0 then
                           raise Process_Died;
                        end if;

                        --  If there is no limit to the buffer size

                        if Descriptors (J).Buffer_Size = 0 then

                           declare
                              Tmp : String_Access := Descriptors (J).Buffer;
                           begin
                              if Tmp /= null then
                                 Descriptors (J).Buffer :=
                                   new String (1 .. Tmp'Length + N);
                                 Descriptors (J).Buffer (1 .. Tmp'Length) :=
                                   Tmp.all;
                                 Descriptors (J).Buffer
                                   (Tmp'Length + 1 .. Tmp'Length + N) :=
                                     Buffer (1 .. N);
                                 Free (Tmp);
                                 Descriptors (J).Buffer_Index :=
                                   Descriptors (J).Buffer'Last;

                              else
                                 Descriptors (J).Buffer := new String (1 .. N);
                                 Descriptors (J).Buffer.all := Buffer (1 .. N);
                                 Descriptors (J).Buffer_Index := N;
                              end if;
                           end;

                        else
                           --  Add what we read to the buffer

                           if Descriptors (J).Buffer_Index + N - 1 >
                             Descriptors (J).Buffer_Size
                           then

                              --  If the user wants to know when we have read
                              --  more than the buffer can contain.

                              if Full_Buffer then
                                 Result := Expect_Full_Buffer;
                                 return;
                              end if;

                              --  Keep as much as possible from the buffer,
                              --  and forget old characters.

                              Descriptors (J).Buffer
                                (1 .. Descriptors (J).Buffer_Size - N) :=
                                  Descriptors (J).Buffer
                                    (N - Descriptors (J).Buffer_Size +
                                       Descriptors (J).Buffer_Index + 1 ..
                                     Descriptors (J).Buffer_Index);
                              Descriptors (J).Buffer_Index :=
                                Descriptors (J).Buffer_Size - N;
                           end if;

                           --  Keep what we read in the buffer.

                           Descriptors (J).Buffer
                             (Descriptors (J).Buffer_Index + 1 ..
                              Descriptors (J).Buffer_Index + N) :=
                                Buffer (1 .. N);
                           Descriptors (J).Buffer_Index :=
                             Descriptors (J).Buffer_Index + N;
                        end if;

                        --  Call each of the output filter with what we read.

                        Current_Filter := Descriptors (J).Out_Filters;

                        while Current_Filter /= null loop
                           Current_Filter.Filter
                             (Descriptors (J).all, Buffer (1 .. N),
                              Current_Filter.User_Data);
                           Current_Filter := Current_Filter.Next;
                        end loop;

                        Result := Expect_Match (N);
                        return;
                     end;
                  end if;
               end loop;
         end case;
      end loop;
   end Expect_Internal;

   ------------------
   -- Get_Input_Fd --
   ------------------

   function Get_Input_Fd
     (Descriptor : Process_Descriptor) return GNAT.OS_Lib.File_Descriptor is
   begin
      return Descriptor.Input_Fd;
   end Get_Input_Fd;

   -------------------
   -- Get_Output_Fd --
   -------------------

   function Get_Output_Fd
     (Descriptor : Process_Descriptor) return GNAT.OS_Lib.File_Descriptor is
   begin
      return Descriptor.Output_Fd;
   end Get_Output_Fd;

   ------------------
   -- Get_Error_Fd --
   ------------------

   function Get_Error_Fd
     (Descriptor : Process_Descriptor) return GNAT.OS_Lib.File_Descriptor is
   begin
      return Descriptor.Error_Fd;
   end Get_Error_Fd;

   -------------
   -- Get_Pid --
   -------------

   function Get_Pid
     (Descriptor : Process_Descriptor) return Process_Id is
   begin
      return Descriptor.Pid;
   end Get_Pid;

   -------------------------
   -- Reinitialize_Buffer --
   -------------------------

   procedure Reinitialize_Buffer (Descriptor : in out Process_Descriptor) is
   begin
      if Descriptor.Buffer_Size = 0 then
         declare
            Tmp : String_Access := Descriptor.Buffer;
         begin
            Descriptor.Buffer :=
              new String
                (1 .. Descriptor.Buffer_Index - Descriptor.Last_Match_End);

            if Tmp /= null then
               Descriptor.Buffer.all := Tmp
                 (Descriptor.Last_Match_End + 1 .. Descriptor.Buffer_Index);
               Free (Tmp);
            end if;
         end;

         Descriptor.Buffer_Index := Descriptor.Buffer'Last;

      else
         Descriptor.Buffer
           (1 .. Descriptor.Buffer_Index - Descriptor.Last_Match_End) :=
             Descriptor.Buffer
               (Descriptor.Last_Match_End + 1 .. Descriptor.Buffer_Index);

         if Descriptor.Buffer_Index > Descriptor.Last_Match_End then
            Descriptor.Buffer_Index :=
              Descriptor.Buffer_Index - Descriptor.Last_Match_End;
         else
            Descriptor.Buffer_Index := 0;
         end if;
      end if;

      Descriptor.Last_Match_Start := 0;
      Descriptor.Last_Match_End := 0;
   end Reinitialize_Buffer;

   ------------
   -- Expect --
   ------------

   procedure Expect
     (Descriptor  : in out Process_Descriptor;
      Result      : out Expect_Match;
      Regexp      : String;
      Timeout     : Integer := 10000;
      Full_Buffer : Boolean := False) is
   begin
      Expect (Descriptor, Result, Compile (Regexp), Timeout, Full_Buffer);
   end Expect;

   ------------
   -- Expect --
   ------------

   procedure Expect
     (Descriptor  : in out Process_Descriptor;
      Result      : out Expect_Match;
      Regexp      : String;
      Matched     : out GNAT.Regpat.Match_Array;
      Timeout     : Integer := 10000;
      Full_Buffer : Boolean := False)
   is
   begin
      pragma Assert (Matched'First = 0);
      Expect
        (Descriptor, Result, Compile (Regexp), Matched, Timeout, Full_Buffer);
   end Expect;

   ------------
   -- Expect --
   ------------

   procedure Expect
     (Descriptor  : in out Process_Descriptor;
      Result      : out Expect_Match;
      Regexp      : GNAT.Regpat.Pattern_Matcher;
      Timeout     : Integer := 10000;
      Full_Buffer : Boolean := False)
   is
      Matched : GNAT.Regpat.Match_Array (0 .. 0);
   begin
      Expect (Descriptor, Result, Regexp, Matched, Timeout, Full_Buffer);
   end Expect;

   ------------
   -- Expect --
   ------------

   procedure Expect
     (Descriptor  : in out Process_Descriptor;
      Result      : out Expect_Match;
      Regexp      : GNAT.Regpat.Pattern_Matcher;
      Matched     : out GNAT.Regpat.Match_Array;
      Timeout     : Integer := 10000;
      Full_Buffer : Boolean := False)
   is
      N           : Expect_Match;
      Descriptors : Array_Of_Pd := (1 => Descriptor'Unrestricted_Access);

   begin
      pragma Assert (Matched'First = 0);
      Reinitialize_Buffer (Descriptor);

      loop

         --  First, test if what is already in the buffer matches (This is
         --  required if this package is used in multi-task mode, since one of
         --  the tasks might have added something in the buffer, and we don't
         --  want other tasks to wait for new input to be available before
         --  checking the regexps).

         Match
           (Regexp, Descriptor.Buffer (1 .. Descriptor.Buffer_Index), Matched);

         if Matched (0).First /= 0 then
            Result := 1;
            Descriptor.Last_Match_Start := Matched (0).First;
            Descriptor.Last_Match_End := Matched (0).Last;
            return;
         end if;

         --  Else try to read new input

         Expect_Internal (Descriptors, N, Timeout, Full_Buffer);

         if N = Expect_Timeout or else N = Expect_Full_Buffer then
            Result := N;
            return;
         end if;
      end loop;
   end Expect;

   ------------
   -- Expect --
   ------------

   procedure Expect
     (Descriptor  : in out Process_Descriptor;
      Result      : out Expect_Match;
      Regexps     : Regexp_Array;
      Timeout     : Integer := 10000;
      Full_Buffer : Boolean := False)
   is
      Patterns : Compiled_Regexp_Array (Regexps'Range);
      Matched  : GNAT.Regpat.Match_Array (0 .. 0);

   begin
      for J in Regexps'Range loop
         Patterns (J) := new Pattern_Matcher'(Compile (Regexps (J).all));
      end loop;

      Expect (Descriptor, Result, Patterns, Matched, Timeout, Full_Buffer);

      for J in Regexps'Range loop
         Free (Patterns (J));
      end loop;
   end Expect;

   ------------
   -- Expect --
   ------------

   procedure Expect
     (Descriptor  : in out Process_Descriptor;
      Result      : out Expect_Match;
      Regexps     : Compiled_Regexp_Array;
      Timeout     : Integer := 10000;
      Full_Buffer : Boolean := False)
   is
      Matched  : GNAT.Regpat.Match_Array (0 .. 0);
   begin
      Expect (Descriptor, Result, Regexps, Matched, Timeout, Full_Buffer);
   end Expect;

   ------------
   -- Expect --
   ------------

   procedure Expect
     (Result      : out Expect_Match;
      Regexps     : Multiprocess_Regexp_Array;
      Timeout     : Integer := 10000;
      Full_Buffer : Boolean := False)
   is
      Matched  : GNAT.Regpat.Match_Array (0 .. 0);
   begin
      Expect (Result, Regexps, Matched, Timeout, Full_Buffer);
   end Expect;

   ------------
   -- Expect --
   ------------

   procedure Expect
     (Descriptor  : in out Process_Descriptor;
      Result      : out Expect_Match;
      Regexps     : Regexp_Array;
      Matched     : out GNAT.Regpat.Match_Array;
      Timeout     : Integer := 10000;
      Full_Buffer : Boolean := False)
   is
      Patterns : Compiled_Regexp_Array (Regexps'Range);
   begin
      pragma Assert (Matched'First = 0);

      for J in Regexps'Range loop
         Patterns (J) := new Pattern_Matcher'(Compile (Regexps (J).all));
      end loop;

      Expect (Descriptor, Result, Patterns, Matched, Timeout, Full_Buffer);

      for J in Regexps'Range loop
         Free (Patterns (J));
      end loop;
   end Expect;

   ------------
   -- Expect --
   ------------

   procedure Expect
     (Descriptor  : in out Process_Descriptor;
      Result      : out Expect_Match;
      Regexps     : Compiled_Regexp_Array;
      Matched     : out GNAT.Regpat.Match_Array;
      Timeout     : Integer := 10000;
      Full_Buffer : Boolean := False)
   is
      N           : Expect_Match;
      Descriptors : Array_Of_Pd := (1 => Descriptor'Unrestricted_Access);

   begin
      pragma Assert (Matched'First = 0);

      Reinitialize_Buffer (Descriptor);

      loop

         --  First, test if what is already in the buffer matches (This is
         --  required if this package is used in multi-task mode, since one of
         --  the tasks might have added something in the buffer, and we don't
         --  want other tasks to wait for new input to be available before
         --  checking the regexps).

         if Descriptor.Buffer /= null then
            for J in Regexps'Range loop
               Match
                 (Regexps (J).all,
                  Descriptor.Buffer (1 .. Descriptor.Buffer_Index),
                  Matched);

               if Matched (0) /= No_Match then
                  Result := Expect_Match (J);
                  Descriptor.Last_Match_Start := Matched (0).First;
                  Descriptor.Last_Match_End := Matched (0).Last;
                  return;
               end if;
            end loop;
         end if;

         Expect_Internal (Descriptors, N, Timeout, Full_Buffer);

         if N = Expect_Timeout or else N = Expect_Full_Buffer then
            Result := N;
            return;
         end if;
      end loop;
   end Expect;

   ------------
   -- Expect --
   ------------

   procedure Expect
     (Result      : out Expect_Match;
      Regexps     : Multiprocess_Regexp_Array;
      Matched     : out GNAT.Regpat.Match_Array;
      Timeout     : Integer := 10000;
      Full_Buffer : Boolean := False)
   is
      N           : Expect_Match;
      Descriptors : Array_Of_Pd (Regexps'Range);

   begin
      pragma Assert (Matched'First = 0);

      for J in Descriptors'Range loop
         Descriptors (J) := Regexps (J).Descriptor;
         Reinitialize_Buffer (Regexps (J).Descriptor.all);
      end loop;

      loop

         --  First, test if what is already in the buffer matches (This is
         --  required if this package is used in multi-task mode, since one of
         --  the tasks might have added something in the buffer, and we don't
         --  want other tasks to wait for new input to be available before
         --  checking the regexps).

         for J in Regexps'Range loop
            Match (Regexps (J).Regexp.all,
                   Regexps (J).Descriptor.Buffer
                     (1 .. Regexps (J).Descriptor.Buffer_Index),
                   Matched);

            if Matched (0) /= No_Match then
               Result := Expect_Match (J);
               Regexps (J).Descriptor.Last_Match_Start := Matched (0).First;
               Regexps (J).Descriptor.Last_Match_End := Matched (0).Last;
               return;
            end if;
         end loop;

         Expect_Internal (Descriptors, N, Timeout, Full_Buffer);

         if N = Expect_Timeout or else N = Expect_Full_Buffer then
            Result := N;
            return;
         end if;
      end loop;
   end Expect;

   ----------------
   -- Expect_Out --
   ----------------

   function Expect_Out (Descriptor : Process_Descriptor) return String is
   begin
      return Descriptor.Buffer (1 .. Descriptor.Last_Match_End);
   end Expect_Out;

   ----------------------
   -- Expect_Out_Match --
   ----------------------

   function Expect_Out_Match (Descriptor : Process_Descriptor) return String is
   begin
      return Descriptor.Buffer
        (Descriptor.Last_Match_Start .. Descriptor.Last_Match_End);
   end Expect_Out_Match;

   ------------------------
   -- Non_Blocking_Spawn --
   ------------------------

   function Non_Blocking_Spawn
     (Command     : String;
      Args        : GNAT.OS_Lib.Argument_List;
      Buffer_Size : Natural := 4096;
      Err_To_Out  : Boolean := False) return Process_Descriptor
   is
      Descriptor        : Process_Descriptor;
      Pipe1             : aliased Pipe_Type;
      Pipe2             : aliased Pipe_Type;
      Pipe3             : aliased Pipe_Type;
      Input,
      Output,
      Error             : File_Descriptor;
      Command_With_Path : String_Access;

   begin
      --  Create the pipes

      Create_Pipe (Pipe1'Unchecked_Access);
      Create_Pipe (Pipe2'Unchecked_Access);
      Descriptor.Input_Fd  := Pipe1.Output;
      Descriptor.Output_Fd := Pipe2.Input;

      if Err_To_Out then
         Pipe3 := Pipe2;
      else
         Create_Pipe (Pipe3'Unchecked_Access);
      end if;

      Descriptor.Error_Fd := Pipe3.Input;

      --  Since Windows does not have a separate fork/exec, we need to
      --  perform the following actions:
      --    - save stdin, stdout, stderr
      --    - replace them by our pipes
      --    - create the child with process handle inheritance
      --    - revert to the previous stdin, stdout and stderr.

      Input  := Dup (GNAT.OS_Lib.Standin);
      Output := Dup (GNAT.OS_Lib.Standout);
      Error  := Dup (GNAT.OS_Lib.Standerr);
      Dup2 (Pipe1.Input,  GNAT.OS_Lib.Standin);
      Dup2 (Pipe2.Output, GNAT.OS_Lib.Standout);
      Dup2 (Pipe3.Output, GNAT.OS_Lib.Standerr);

      Close (Pipe1.Input);
      Close (Pipe2.Output);

      if not Err_To_Out then
         Close (Pipe3.Output);
      end if;

      Command_With_Path := Locate_Exec_On_Path (Command);
      Descriptor.Pid :=
        To_Pid (GNAT.OS_Lib.Non_Blocking_Spawn (Command_With_Path.all, Args));
      Free (Command_With_Path);

      Dup2 (Input,  GNAT.OS_Lib.Standin);
      Close (Input);
      Dup2 (Output, GNAT.OS_Lib.Standout);
      Close (Output);
      Dup2 (Error, GNAT.OS_Lib.Standerr);
      Close (Error);

      --  Create the buffer

      Descriptor.Buffer_Size := Buffer_Size;

      if Buffer_Size /= 0 then
         Descriptor.Buffer := new String (1 .. Positive (Buffer_Size));
      end if;

      return Descriptor;
   end Non_Blocking_Spawn;

   ----------
   -- Send --
   ----------

   procedure Send
     (Descriptor : in out Process_Descriptor;
      Str        : String;
      Add_LF     : Boolean := True;
      Empty_Buffer : Boolean := False)
   is
      N        : Natural;
      Current  : Filter_List := Descriptor.In_Filters;
      Full_Str : constant String := Str & ASCII.LF;
      Last     : Natural;
      Result   : Expect_Match;
      Descriptors : Array_Of_Pd := (1 => Descriptor'Unrestricted_Access);

   begin
      if Empty_Buffer then
         --  Force a read on the process if there is anything waiting.
         Expect_Internal (Descriptors, Result,
                          Timeout => 0, Full_Buffer => False);
         Descriptor.Last_Match_End := Descriptor.Buffer_Index;

         --  Empty the buffer.
         Reinitialize_Buffer (Descriptor);
      end if;

      if Add_LF then
         Last := Full_Str'Last;
      else
         Last := Full_Str'Last - 1;
      end if;

      while Current /= null loop
         Current.Filter
           (Descriptor, Full_Str (Full_Str'First .. Last),
            Current.User_Data);
         Current := Current.Next;
      end loop;

      N := Write
        (Descriptor.Input_Fd, Full_Str'Address, Last - Full_Str'First + 1);
   end Send;

   ------------------
   -- Trace_Filter --
   ------------------

   procedure Trace_Filter
     (Descriptor : Process_Descriptor;
      Str        : String;
      User_Data  : System.Address := System.Null_Address) is
   begin
      GNAT.IO.Put (Str);
   end Trace_Filter;

end GNAT.Expect;
