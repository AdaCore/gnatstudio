
--  Usage
--  =====
--
--  This package provides a set of subprograms similar to what is available
--  with the standard Tcl Expect tool.
--  It allows you to easily spawn and communicate with an external process.
--  You can send commands or inputs to the process, and compare the output
--  with some expected regular expression.
--
--  Usage example:
--
--      Fd := Non_Blocking_Spawn ("ftp machine@domaine");
--      Timeout := 10000;  --  10 seconds
--      Expect (Fd, Result, Regexp_Array'(+"\(user\)", +"\(passwd\)"),
--              Timeout);
--      case Result is
--         when 1 => Send (Fd, "my_name");   --  matched "user"
--         when 2 => Send (Fd, "my_passwd"); --  matched "passwd"
--         when Expect_Timeout => null;      --  timeout
--         when others => null;
--      end case;
--      Close (Fd);
--
--  You can also combine multiple regular expressions together, and get the
--  specific string matching a parenthesis pair by doing something like. If you
--  expect either "lang=optional ada" or "lang=ada" from the external process,
--  you can group the two together, which is more efficient, and simply get the
--  name of the language by doing:
--
--      declare
--         Matched : Regexp_Array (0 .. 2);
--      begin
--         Expect (Fd, Result, "lang=(optional)? ([a-z]+)", Matched);
--         Put_Line ("Seen: " &
--                   Expect_Out (Fd) (Matched (2).First .. Matched (2).Last));
--      end;
--
--  Alternatively, you might choose to use a lower-level interface to the
--  processes, where you can give your own input and output filters every
--  time characters are read from or written to the process.
--
--      procedure My_Filter (Descriptor : Process_Descriptor; Str : String) is
--      begin
--         Put_Line (Str);
--      end;
--
--      Fd := Non_Blocking_Spawn ("tail -f a_file");
--      Add_Output_Filter (Fd, My_Filter'Access);
--      Expect (Fd, Result, "", 0);  --  wait forever
--
--  The above example should probably be run in a separate task, since it is
--  blocking on the call to Expect.
--
--  Both examples can be combined, for instance to systematically print the
--  output seen by expect, even though you still want to let Expect do the
--  filtering. You can use the Trace_Filter subprogram for such a filter.
--
--  If you want to get the output of a simple command, and ignore any previous
--  existing output, it is recommended to do something like:
--
--      Expect (Fd, Result, ".*", Timeout => 0);
--            -- empty the buffer, by matching everything (after checking
--            -- if there was any input).
--      Send (Fd, "command");
--      Expect (Fd, Result, ".."); -- match only on the output of command
--
--  Task Safety
--  ===========
--
--  This package is not task-safe. However, you can easily make is task safe
--  by encapsulating the type Process_Descriptor in a protected record.
--  There should not be concurrent calls to Expect.

with System;
with GNAT.OS_Lib;
with GNAT.Regpat;

package GNAT.Expect is

   type Process_Id is new Integer;
   Invalid_Pid : constant Process_Id := -1;
   Null_Pid    : constant Process_Id := 0;

   type Process_Descriptor is private;
   --  Contains all the components needed to describe a process handled
   --  in this package, including a process identifier, file descriptors
   --  associated with the standard input, output and error, and the buffer
   --  needed to handle the expect calls.

   type Process_Descriptor_Access is access Process_Descriptor;

   ------------------------
   -- Spawning a process --
   ------------------------

   function Non_Blocking_Spawn
     (Command     : String;
      Args        : GNAT.OS_Lib.Argument_List;
      Buffer_Size : Natural := 4096;
      Err_To_Out  : Boolean := False) return Process_Descriptor;
   --  Spawn a new process.
   --  You can then send any commands to it, or parse its output
   --  automatically.
   --  The expect buffer associated with that process can contain at most
   --  Buffer_Size characters. Older characters are simply discarded when
   --  this buffer is full. Beware that if the buffer is too big, this could
   --  slow down the Expect calls if not output is matched, since Expect has
   --  to match all the regexp against all the characters in the buffer.
   --  If Buffer_Size is 0, there is no limit (ie all the characters are kept
   --  till Expect matches), but this is slower.
   --
   --  If Err_To_Out is True, then the standard error of the spawned process is
   --  connected to the standard output. This is the only way to get the
   --  Except subprograms also match on output on standard error.
   --
   --  Invalid_Process is raised if the process could not be spawned.

   procedure Close (Descriptor : in out Process_Descriptor);
   --  Terminate the process and close the pipes to it.
   --  (It implicitly does the 'wait' command required to clean up the
   --  process table).
   --  This also frees the buffer associated with the proces id.

   function Get_Input_Fd
     (Descriptor : Process_Descriptor) return GNAT.OS_Lib.File_Descriptor;
   --  Return the input file descriptor associated with Descriptor.

   function Get_Output_Fd
     (Descriptor : Process_Descriptor) return GNAT.OS_Lib.File_Descriptor;
   --  Return the output file descriptor associated with Descriptor.

   function Get_Error_Fd
     (Descriptor : Process_Descriptor) return GNAT.OS_Lib.File_Descriptor;
   --  Return the error output file descriptor associated with Descriptor.

   function Get_Pid
     (Descriptor : Process_Descriptor) return Process_Id;
   --  Return the process id assocated with a given process descriptor.

   --------------------
   -- Adding filters --
   --------------------
   --  This is a rather low-level interface to subprocesses, since basically
   --  the filtering is left entirely to the user. See the Expect subprograms
   --  below for higher level functions.

   type Filter_Function is access
     procedure
       (Descriptor : Process_Descriptor;
        Str        : String;
        User_Data  : System.Address := System.Null_Address);
   --  Function called every time new characters are read from or written
   --  to the process.
   --  Str is a string of all these characters.
   --  User_Data, if specified, is a user specific data that will be passed to
   --  the filter. Note that no checks are done on this parameter that should
   --  be used with cautiousness.

   procedure Add_Input_Filter
     (Descriptor : in out Process_Descriptor;
      Filter     : Filter_Function;
      User_Data  : System.Address := System.Null_Address;
      After      : Boolean := False);
   --  Add a new input filter.
   --  Every time new characters are written to the process associated with
   --  Descriptor, the filter is called with these new characters in argument.
   --  This filter will be run before all the existing filters, unless
   --  After is True.
   --  User_Data will be passed as is to the filter procedure.
   --  Note that input is only generated by calls to Send below.

   procedure Add_Output_Filter
     (Descriptor : in out Process_Descriptor;
      Filter     : Filter_Function;
      User_Data  : System.Address := System.Null_Address;
      After      : Boolean := False);
   --  Add a new output filter.
   --  Every time new characters are read from the process associated with
   --  Descriptor, the filter is called with these new characters in argument.
   --  This filter will be run before all the existing filters, unless
   --  After is True.
   --  User_Data will be passed as is to the filter procedure.
   --  Note that output is only generated when the program is blocked in
   --  a call to Expect.

   procedure Remove_Input_Filter
     (Descriptor : in out Process_Descriptor;
      Filter     : Filter_Function);
   --  Remove a filter from the list of input filters.

   procedure Remove_Output_Filter
     (Descriptor : in out Process_Descriptor;
      Filter     : Filter_Function);
   --  Remove a filter from the list of output filters.

   procedure Trace_Filter
     (Descriptor : Process_Descriptor;
      Str        : String;
      User_Data  : System.Address := System.Null_Address);
   --  Function that can be used a filter and that simply outputs Str on
   --  Standard_Output. This is mainly used for debugging purposes.
   --  User_Data is ignored.

   ------------------
   -- Sending data --
   ------------------

   procedure Send
     (Descriptor   : in out Process_Descriptor;
      Str          : String;
      Add_LF       : Boolean := True;
      Empty_Buffer : Boolean := False);
   --  Send a string to the file descriptor.
   --  The string is not formated in anyway, except if Add_LF is True, in which
   --  case an ASCII.LF is added at the end, so that Str is recognized as a
   --  command by the external process.
   --  If Empty_Buffer is True, any input waiting from the process (or in the
   --  buffer) is first discarded before the command is sent. The output
   --  filters are of course called as usual.

   -----------------------------------------------------------
   -- Working on the output (single process, simple regexp) --
   -----------------------------------------------------------

   type Expect_Match is new Integer;
   Expect_Full_Buffer : constant Expect_Match := -1;
   --  If the buffer was fulled and some characters were discarded.

   Expect_Timeout     : constant Expect_Match := -2;
   --  If not output matching the regexps was found before the timeout.

   function "+" (S : String) return GNAT.OS_Lib.String_Access;
   --  Allocate some memory for the string.
   --  This is merely a convenience function to help create the array of
   --  regexp in the call to Expect.

   procedure Expect
     (Descriptor  : in out Process_Descriptor;
      Result      : out Expect_Match;
      Regexp      : String;
      Timeout     : Integer := 10000;
      Full_Buffer : Boolean := False);
   --  Wait till a string matching Fd can be read from Fd, and return 1
   --  if a match was found.
   --  It consumes all the characters read from Fd until a match found, and
   --  then sets the return values for the subprograms Expect_Out and
   --  Expect_Out_Match.
   --  The empty string "" will never match, and can be used if you only want
   --  to match after a specific timeout. Beware that if Timeout is 0 at the
   --  time, the current task will be blocked forever.
   --
   --  This command times out after Timeout milliseconds (or never if Timeout
   --  is -1). In that case, Expect_Timeout is returned. The value returned by
   --  Expect_Out and Expect_Out_Match are meaningless in that case.
   --  The regular expression must obey the syntax described in GNAT.Regpat.
   --
   --  If Full_Buffer is True, then Expect will match if the buffer was too
   --  small and some characters were about to be discarded. In that case,
   --  Expect_Full_Buffer is returned.

   procedure Expect
     (Descriptor  : in out Process_Descriptor;
      Result      : out Expect_Match;
      Regexp      : GNAT.Regpat.Pattern_Matcher;
      Timeout     : Integer := 10000;
      Full_Buffer : Boolean := False);
   --  Same as the previous one, but with a precompiled regular expression.
   --  This is more efficient however, especially if you are using this
   --  expression multiple times, since this package won't need to recompile
   --  the regexp every time.

   procedure Expect
     (Descriptor  : in out Process_Descriptor;
      Result      : out Expect_Match;
      Regexp      : String;
      Matched     : out GNAT.Regpat.Match_Array;
      Timeout     : Integer := 10000;
      Full_Buffer : Boolean := False);
   --  Same as above, but it is now possible to get the indexes of the
   --  substrings for the parentheses in the regexp (see the example at the
   --  top of this package, as well as the documentation in the package
   --  GNAT.Regpat).
   --  Matched'First should be 0, and this index will contain the indexes for
   --  the whole string that was matched. The index 1 will contain the indexes
   --  for the first parentheses-pair, and so on.

   ------------
   -- Expect --
   ------------

   procedure Expect
     (Descriptor  : in out Process_Descriptor;
      Result      : out Expect_Match;
      Regexp      : GNAT.Regpat.Pattern_Matcher;
      Matched     : out GNAT.Regpat.Match_Array;
      Timeout     : Integer := 10000;
      Full_Buffer : Boolean := False);
   --  Same as above, but with a precompiled regular expression.

   -------------------------------------------------------------
   -- Working on the output (single process, multiple regexp) --
   -------------------------------------------------------------

   type Regexp_Array is array (Positive range <>) of GNAT.OS_Lib.String_Access;

   type Pattern_Matcher_Access is access GNAT.Regpat.Pattern_Matcher;
   type Compiled_Regexp_Array is array (Positive range <>)
     of Pattern_Matcher_Access;

   function "+"
     (P : GNAT.Regpat.Pattern_Matcher) return Pattern_Matcher_Access;
   --  Allocate some memory for the pattern matcher.
   --  This is only a convenience function to help create the array of
   --  copmiled regular expressoins.

   procedure Expect
     (Descriptor  : in out Process_Descriptor;
      Result      : out Expect_Match;
      Regexps     : Regexp_Array;
      Timeout     : Integer := 10000;
      Full_Buffer : Boolean := False);
   --  Wait till a string matching one of the regular expressions in Regexps
   --  is found. This function returns the index of the regexp that matched.
   --  This command is blocking, but will timeout after Timeout milliseconds.
   --  In that case, Timeout is returned.

   procedure Expect
     (Descriptor  : in out Process_Descriptor;
      Result      : out Expect_Match;
      Regexps     : Compiled_Regexp_Array;
      Timeout     : Integer := 10000;
      Full_Buffer : Boolean := False);
   --  Same as the previous one, but with precompiled regular expressions.
   --  This can be much faster if you are using them multiple times.

   procedure Expect
     (Descriptor  : in out Process_Descriptor;
      Result      : out Expect_Match;
      Regexps     : Regexp_Array;
      Matched     : out GNAT.Regpat.Match_Array;
      Timeout     : Integer := 10000;
      Full_Buffer : Boolean := False);
   --  Same as above, except that you can also access the parenthesis
   --  groups inside the matching regular expression.
   --  The first index in Matched must be 0, or Constraint_Error will be
   --  raised. The index 0 contains the indexes for the whole string that was
   --  matched, the index 1 contains the indexes for the first parentheses
   --  pair, and so on.

   procedure Expect
     (Descriptor  : in out Process_Descriptor;
      Result      : out Expect_Match;
      Regexps     : Compiled_Regexp_Array;
      Matched     : out GNAT.Regpat.Match_Array;
      Timeout     : Integer := 10000;
      Full_Buffer : Boolean := False);
   --  Same as above, but with precompiled regular expressions.
   --  The first index in Matched must be 0, or Constraint_Error will be
   --  raised.

   -------------------------------------------
   -- Working on the output (multi-process) --
   -------------------------------------------

   type Multiprocess_Regexp is record
      Descriptor : Process_Descriptor_Access;
      Regexp     : Pattern_Matcher_Access;
   end record;
   type Multiprocess_Regexp_Array is array (Positive range <>)
     of Multiprocess_Regexp;

   procedure Expect
     (Result      : out Expect_Match;
      Regexps     : Multiprocess_Regexp_Array;
      Matched     : out GNAT.Regpat.Match_Array;
      Timeout     : Integer := 10000;
      Full_Buffer : Boolean := False);
   --  Same as above, but for multiprocesses.

   procedure Expect
     (Result      : out Expect_Match;
      Regexps     : Multiprocess_Regexp_Array;
      Timeout     : Integer := 10000;
      Full_Buffer : Boolean := False);
   --  Same as the previous one, but for multiple processes.
   --  This procedure finds the first regexp that match the associated process.

   ------------------------
   -- Getting the output --
   ------------------------

   function Expect_Out (Descriptor : Process_Descriptor) return String;
   --  Return the string matched by the last Expect call.
   --  The returned string is in fact the concatenation of all the strings
   --  read from the file descriptor up to, and including, the characters
   --  that matched the regular expression.
   --  For instance, with an input "philosophic", and a regular expression
   --  "hi" in the call to expect, the strings returned the first and second
   --  time would be respectively "phi" and "losophi".

   function Expect_Out_Match (Descriptor : Process_Descriptor) return String;
   --  Return the string matched by the last Expect call.
   --  The returned string includes only the character that matched the
   --  specific regular expression. All the characters that came before are
   --  simply discarded.
   --  For instance, with an input "philosophic", and a regular expression
   --  "hi" in the call to expect, the strings returned the first and second
   --  time would both be "hi".

   ----------------
   -- Exceptions --
   ----------------

   Invalid_Process : exception;
   --  Raised by most subprograms above when the parameter Descriptor is not a
   --  valid process or is a closed process.

   Process_Died : exception;
   --  Raised by all the expect subprograms if Descriptor was originally a
   --  valid process that died while Expect was executing.

private
   type Filter_List_Elem;
   type Filter_List is access Filter_List_Elem;
   type Filter_List_Elem is record
      Filter    : Filter_Function;
      User_Data : System.Address;
      Next      : Filter_List;
   end record;

   type Process_Descriptor is record
      Pid              : Process_Id := Invalid_Pid;
      Input_Fd         : GNAT.OS_Lib.File_Descriptor := GNAT.OS_Lib.Invalid_FD;
      Output_Fd        : GNAT.OS_Lib.File_Descriptor := GNAT.OS_Lib.Invalid_FD;
      Error_Fd         : GNAT.OS_Lib.File_Descriptor := GNAT.OS_Lib.Invalid_FD;
      Out_Filters      : Filter_List := null;
      In_Filters       : Filter_List := null;

      Buffer           : GNAT.OS_Lib.String_Access := null;
      Buffer_Size      : Natural := 0;
      Buffer_Index     : Natural := 0;

      Last_Match_Start : Natural := 0;
      Last_Match_End   : Natural := 0;
   end record;

end GNAT.Expect;
