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

with GNAT.Expect;
with GNAT.Regpat;

package Process_Proxies is

   type Process_Proxy is tagged private;
   type Process_Proxy_Access is access all Process_Proxy'Class;
   --  This type acts as a proxy to an external process as found in
   --  GNAT.Expect. This implements a set of common subprograms that
   --  can not be implemented in GNAT.Expect itself, but that we don't
   --  want to duplicate in all the debugger implementation.

   procedure Free (Proxy : in out Process_Proxy_Access);
   --  Free the space occupied by the proxy and its pipe_id.

   function Command_In_Process (Proxy : Process_Proxy) return Boolean;
   --  Return True if a command is currently being processed for Proxy.
   --  Since, in graphic mode, the main loop events are processed, all the
   --  callbacks have to check whether a command is already executing. Since
   --  the external debugger can process a single command at a time, the
   --  callback should not do anything.

   function Get_Pipes (Proxy : Process_Proxy)
                      return GNAT.Expect.Pipes_Id_Access;
   --  Returns the associates Pipes_Id, so that all the functions of
   --  GNAT.Expect can be applied to it.
   --  You should not use Expect directly, but rather Wait below.

   procedure Set_Pipes (Proxy : in out Process_Proxy;
                        Pipes : GNAT.Expect.Pipes_Id_Access);
   --  Set the external pipe.

   procedure Wait (Proxy   : Process_Proxy;
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

   procedure Wait (Proxy   : Process_Proxy;
                   Result  : out GNAT.Expect.Expect_Match;
                   Pattern : String;
                   Timeout : Integer := 20);
   --  Same, but the regular expression is given a string.

   procedure Send (Proxy : Process_Proxy;
                   Cmd   : String);
   --  Send a command to the underlying process.

   function Expect_Out (Proxy : Process_Proxy) return String;
   --  Equivalent to Expect_Out, can be called after a Wait.

   type Gui_Process_Proxy is new Process_Proxy with private;

   procedure Wait (Proxy   : Gui_Process_Proxy;
                   Result  : out GNAT.Expect.Expect_Match;
                   Pattern : GNAT.Regpat.Pattern_Matcher;
                   Timeout : Integer := 20);
   --  In GUI mode, this processes the graphic events between each iteration.

private

   type Boolean_Access is access Boolean;

   type Process_Proxy is tagged record
      Pipes              : GNAT.Expect.Pipes_Id_Access;

      Command_In_Process : Boolean_Access := new Boolean'(False);
      --  This is implemented as an access type so that Process_Proxy does
      --  not always have to be passed as an "in out" parameter, but simply
      --  an "in" parameter.
   end record;

   type Gui_Process_Proxy is new Process_Proxy with null record;

end Process_Proxies;
