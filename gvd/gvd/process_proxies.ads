-----------------------------------------------------------------------
--                 Odd - The Other Display Debugger                  --
--                                                                   --
--                         Copyright (C) 2000                        --
--                 Emmanuel Briot and Arnaud Charlet                 --
--                                                                   --
-- Odd is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with System;
with GNAT.Expect;
with GNAT.Regpat;

package Process_Proxies is

   Internal_Status_Stack_Size : constant := 20;
   --  Number of successive Push_Internal_Command_Status calls that can be
   --  emitted.

   type Process_Proxy is tagged private;
   type Process_Proxy_Access is access all Process_Proxy'Class;
   --  This type acts as a proxy to an external process as found in
   --  GNAT.Expect. This implements a set of common subprograms that
   --  can not be implemented in GNAT.Expect itself, but that we don't
   --  want to duplicate in all the debugger implementation.

   procedure Free (Proxy : in out Process_Proxy_Access);
   --  Free the space occupied by the proxy and its pipe_id.

   function Command_In_Process (Proxy : access Process_Proxy) return Boolean;
   --  Return True if a command is currently being processed for Proxy.
   --  Since, in graphic mode, the main loop events are processed, all the
   --  callbacks have to check whether a command is already executing. Since
   --  the external debugger can process a single command at a time, the
   --  callback should not do anything.

   function Is_Internal_Command (Proxy : access Process_Proxy) return Boolean;
   --  Return True if the external process is currently process some commands
   --  internally (ie that was not sent by the user, and whose output should
   --  be hidden).

   procedure Push_Internal_Command_Status
     (Proxy       : access Process_Proxy;
      Is_Internal : Boolean);
   --  Set a new internal status for the following commands, and save the
   --  previous value. If you are trying to call this function more than
   --  Internal_Status_Stack_Size times (without an Pop_Internal_Command_Status
   --  in between), an exception Internal_Command_Status_Stack_Overfull is
   --  emitted.
   --  The default behavior is False, so as to show as much output as
   --  possible from the debugger.
   --  The new value remains active until this function is called again or
   --  Pop_Internal_Command_Status is called.

   procedure Pop_Internal_Command_Status (Proxy : access Process_Proxy);
   --  Go back to status preceeding the last call to
   --  Push_Internal_Command_Status.

   procedure Set_Parse_File_Name (Proxy : access Process_Proxy;
                                  Parse : Boolean);
   --  Indicate whether we should parse file names/line pattern in the output
   --  of the debugger.

   function Get_Parse_File_Name (Proxy : access Process_Proxy)
                                return Boolean;
   --  Indicate whether we should parse file names/line pattern in the output
   --  of the debugger.

   function Get_Descriptor
     (Proxy : access Process_Proxy)
     return GNAT.Expect.Process_Descriptor_Access;
   --  Returns the associates Process_Descriptor, so that all the functions of
   --  GNAT.Expect can be applied to it.
   --  You should not use Expect directly, but rather Wait below.

   procedure Set_Descriptor
     (Proxy      : access Process_Proxy;
      Descriptor : GNAT.Expect.Process_Descriptor_Access);
   --  Set the external process descriptor.

   procedure Empty_Buffer
     (Proxy        : access Process_Proxy;
      At_Least_One : Boolean := False);
   --  Empty the current output buffer for the external process, as well as
   --  any waiting input.
   --  If At_Least_One is True, then wait until at least one character is
   --  available in the buffer.

   procedure Wait
     (Proxy   : access Process_Proxy;
      Result  : out GNAT.Expect.Expect_Match;
      Pattern : GNAT.Regpat.Pattern_Matcher;
      Timeout : Integer := 20);
   --  Wait until some output from the debugger matches Pattern.
   --  This functions waits at least (Timeout * 50ms), and is overriden
   --  in graphic mode so that we give a change to the GtkAda main loop to
   --  handle events. The procedure can actually wait longer, depending on
   --  what is processes between each iteration.
   --  The function returns the same kind of result as an Expect call would.
   --  Default Timeout is one second.

   procedure Wait
     (Proxy   : access Process_Proxy;
      Result  : out GNAT.Expect.Expect_Match;
      Pattern : GNAT.Regpat.Pattern_Matcher;
      Matched : out GNAT.Regpat.Match_Array;
      Timeout : Integer := 20);
   --  Same but Matched is also filled.

   procedure Wait
     (Proxy   : access Process_Proxy;
      Result  : out GNAT.Expect.Expect_Match;
      Pattern : String;
      Timeout : Integer := 20);
   --  Same, but the regular expression is given a string.

   procedure Send (Proxy : access Process_Proxy;
                   Cmd   : String;
                   Empty_Buffer : Boolean := False);
   --  Send a command to the underlying process.
   --  If Empty_Buffer is True, any input waiting from the process (or in the
   --  buffer) is first discarded before the command is sent.

   function Expect_Out (Proxy : access Process_Proxy) return String;
   --  Equivalent to Expect_Out, can be called after a Wait.

   type Gui_Process_Proxy is new Process_Proxy with private;

   procedure Wait
     (Proxy   : access Gui_Process_Proxy;
      Result  : out GNAT.Expect.Expect_Match;
      Pattern : GNAT.Regpat.Pattern_Matcher;
      Timeout : Integer := 20);
   --  In GUI mode, this processes the graphic events between each iteration.

   procedure Wait
     (Proxy   : access Gui_Process_Proxy;
      Result  : out GNAT.Expect.Expect_Match;
      Pattern : GNAT.Regpat.Pattern_Matcher;
      Matched : out GNAT.Regpat.Match_Array;
      Timeout : Integer := 20);
   --  In GUI mode, this processes the graphic events between each iteration.

   -------------
   -- Filters --
   -------------

   procedure TTY_Filter
     (Descriptor : GNAT.Expect.Process_Descriptor;
      Str        : String;
      Proxy      : System.Address);
   --  Can be used as a filter to simulate a command line interface.
   --  This will print to stdout all output received when the process proxy
   --  is not in internal state.
   --  Proxy is the address of a Process_Proxy'Class.

   ----------------
   -- Exceptions --
   ----------------

   Internal_Command_Status_Stack_Overfull : exception;

private

   type Boolean_Access is access Boolean;
   type Boolean_Array is array (1 .. Internal_Status_Stack_Size) of
     Boolean;
   pragma Pack (Boolean_Array);

   type Process_Proxy is tagged record
      Descriptor         : GNAT.Expect.Process_Descriptor_Access;

      Command_In_Process : Boolean_Access := new Boolean'(False);
      --  This is implemented as an access type so that Process_Proxy does
      --  not always have to be passed as an "in out" parameter, but simply
      --  an "in" parameter.

      Internal_Command_Stack : Boolean_Array := (others => False);
      Internal_Command   : Positive := 1;
      --  Stack of internal command status. Internal_Command is an index to
      --  the current status;

      Parse_File_Name    : Boolean := True;
      --  True if file name/lines patterns should be recognized in the output
      --  of the debugger. If set to False, the text displayed in the code
      --  editor will not be changed.
   end record;

   type Gui_Process_Proxy is new Process_Proxy with null record;

end Process_Proxies;
