------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2019, AdaCore                     --
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

with GNAT.Expect;
with GNAT.Regpat;
with GVD.Types;

private with Ada.Finalization;

package Process_Proxies is

   type Process_Proxy is tagged private;
   type Process_Proxy_Access is access all Process_Proxy'Class;
   --  This type acts as a proxy to an external process as found in
   --  GNAT.Expect. This implements a set of common subprograms that
   --  can not be implemented in GNAT.Expect itself, but that we don't
   --  want to duplicate in all the debugger implementations.

   procedure Free (Proxy : in out Process_Proxy_Access);
   --  Free the space occupied by the proxy and its pipe_id.

   function Command_In_Process (Proxy : access Process_Proxy) return Boolean;
   --  Return True if a command is currently being processed for Proxy.
   --  Since, in graphic mode, the main loop events are processed, all the
   --  callbacks have to check whether a command is already executing. Since
   --  the external debugger can process a single command at a time, the
   --  callback should not do anything.

   procedure Set_Command_In_Process
     (Proxy      : access Process_Proxy;
      In_Process : Boolean := True);
   --  Set the corresponding Flag in Proxy.
   --  See Command_In_Process for more details.

   function Get_Command_Mode (Proxy : access Process_Proxy)
      return GVD.Types.Command_Type;
   --  Return the type of the command currently processed.

   procedure Set_Command_Mode
     (Proxy : access Process_Proxy;
      Mode  : GVD.Types.Command_Type);
   --  Save the type of the command currently processed.

   procedure Set_Parse_File_Name
     (Proxy : access Process_Proxy;
      Parse : Boolean);
   --  Indicate whether we should parse file names/line pattern in the output
   --  of the debugger.

   function Get_Parse_File_Name
     (Proxy : access Process_Proxy) return Boolean;
   --  Indicate whether we should parse file names/line pattern in the output
   --  of the debugger.

   function Get_Descriptor
     (Proxy : access Process_Proxy)
      return GNAT.Expect.Process_Descriptor_Access;
   --  Return the associated Process_Descriptor, so that all the functions of
   --  GNAT.Expect can be applied to it.
   --  You should not use Expect directly, but rather Wait below.

   procedure Set_Descriptor
     (Proxy      : access Process_Proxy;
      Descriptor : GNAT.Expect.Process_Descriptor_Access);
   --  Set the external process descriptor.

   function Interrupted (Proxy : access Process_Proxy) return Boolean;
   --  Return the Interrupted flag associated with Proxy.
   --  Interrupted should be set to True when an attempt to interrupt the
   --  underlying process is made.

   procedure Set_Interrupted
     (Proxy       : access Process_Proxy;
      Interrupted : Boolean := True);
   --  Set the Interrupted flag.

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
      Timeout : Integer := 1000);
   --  Wait until some output from the debugger matches Pattern.
   --  This functions waits at least Timeout ms, and is overridden
   --  in simple graphic mode so that Gtk+ events are handled by hand.
   --  The procedure can actually wait longer, depending on
   --  what is processed between each iteration.
   --  The function returns the same kind of result as an Expect call would.
   --  Default Timeout is one second.

   procedure Wait
     (Proxy   : access Process_Proxy;
      Result  : out GNAT.Expect.Expect_Match;
      Pattern : GNAT.Regpat.Pattern_Matcher;
      Matched : out GNAT.Regpat.Match_Array;
      Timeout : Integer := 1000);
   --  Same but Matched is also filled.

   procedure Wait
     (Proxy   : access Process_Proxy;
      Result  : out GNAT.Expect.Expect_Match;
      Pattern : String;
      Timeout : Integer := 1000);
   --  Same, but the regular expression is given a string.

   procedure Send
     (Proxy : access Process_Proxy;
      Cmd   : String;
      Empty_Buffer : Boolean := False);
   --  Send a command to the underlying process.
   --  If Empty_Buffer is True, any input waiting from the process (or in the
   --  buffer) is first discarded before the command is sent.

   function Expect_Out (Proxy : access Process_Proxy) return String;
   --  Equivalent to Expect_Out, can be called after a Wait.

   type Gui_Process_Proxy is new Process_Proxy with private;

   overriding procedure Wait
     (Proxy   : access Gui_Process_Proxy;
      Result  : out GNAT.Expect.Expect_Match;
      Pattern : GNAT.Regpat.Pattern_Matcher;
      Timeout : Integer := 1000);
   --  In GUI mode, this processes the graphic events between each iteration.
   --  Note that it is not recommended to use this procedure, since handling
   --  Gtk+ events by hand can cause lots of confusion, and in particular,
   --  create unwanted recursion in the handling of callbacks.
   --  The recommended way is to use a (non GUI) Process_Proxy and call Wait
   --  with small timeouts inside Gtk+ timeout handlers.

   overriding procedure Wait
     (Proxy   : access Gui_Process_Proxy;
      Result  : out GNAT.Expect.Expect_Match;
      Pattern : GNAT.Regpat.Pattern_Matcher;
      Matched : out GNAT.Regpat.Match_Array;
      Timeout : Integer := 1000);
   --  In GUI mode, this processes the graphic events between each iteration.
   --  See comments above.

   type Parse_File_Switch
     (Proxy : Process_Proxy_Access) is tagged limited private;
   --  This type disables file parsing while its instance exists
   --  Usage:
   --     declare
   --        Block : Parse_File_Switch (Proxy) with Unreferenced;
   --     begin
   --        --  file parsing is disabled here
   --        ...
   --     end;
   --     --  file parsing is enabled here if it was enabled before

private

   type Boolean_Access is access Boolean;

   type Process_Proxy is tagged record
      Descriptor         : GNAT.Expect.Process_Descriptor_Access;

      Command_In_Process : Boolean_Access := new Boolean'(False);
      --  This is implemented as an access type so that Process_Proxy does
      --  not always have to be passed as an "in out" parameter, but simply
      --  an "in" parameter.

      Internal_Mode      : GVD.Types.Command_Type := GVD.Types.Hidden;
      --  Indicates whether the current output from the debugger should be
      --  displayed in the output window

      Parse_File_Name    : Boolean := True;
      --  True if file name/lines patterns should be recognized in the output
      --  of the debugger. If set to False, the text displayed in the code
      --  editor will not be changed.

      Interrupted        : Boolean := False;
      --  Whether the process has been interrupted

      Waiting            : Boolean := False;
      --  Whether we are already polling the output of the process.
   end record;

   type Gui_Process_Proxy is new Process_Proxy with null record;

   type Parse_File_Switch
     (Proxy : Process_Proxy_Access) is
     new Ada.Finalization.Limited_Controlled
   with record
      Work : Boolean := False;
      --  Holds the value of Proxy.Parse_File_Name and if it is True sets it
      --  to False in Initialize and restore in Finalize and does nothing when
      --  it is False.
   end record;

   overriding procedure Initialize (Self : in out Parse_File_Switch);
   overriding procedure Finalize   (Self : in out Parse_File_Switch);

end Process_Proxies;
