-----------------------------------------------------------------------
--                                                                   --
--                     Copyright (C) 2001                            --
--                          ACT-Europe                               --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with GNAT.Source_Info;

package Debug is

   type Debug_Handle is private;
   --  A handle for a debug stream.
   --  One such handle should be created for each module/unit/package where it
   --  is relevant. They are associated with a specific name and output stream,
   --  and can be activated through a configuration file.
   --
   --  Recommended use with generics:
   --  Since generics might be used in various, independent modules, the
   --  recommended use is to have one more generic paraemter for the debug
   --  handle. Internally, it is then possible to specialize this stream (see
   --  the subprogram Unit_Name):
   --     generic
   --         Self_Debug : Debug_Handle := Create ("My_Generic");
   --     package My_Generic is ....

   Config_File_Environment : constant String := "ADA_DEBUG_FILE";
   Default_Config_File     : constant String := ".gnatdebug";
   --  Name of the default configuration file. This file is looked for first in
   --  the current directory, then in the user's home directory.  If no file is
   --  found, then no handle will be activated.  The name of this file can be
   --  overriden by the environment variable Config_File_Environment, which
   --  should be an absolute name (or relative to the current directory). If
   --  this variable is set, the standard file is never searched.
   --
   --  The format of the configuration file is the following:
   --    * activating a module:
   --      MODULE_NAME=yes
   --      MODULE_NAME
   --    * deactivating a module (default)
   --      MODULE_NAME=no
   --    * redirecting all modules to a file:
   --      >filename
   --    * redirecting all outputs to standard error
   --      >&2
   --    * redirecting a specific module to a file
   --      MODULE_NAME=yes >filename
   --    * comments
   --      -- comment
   --    * Activate traces for all modules, unless explicitely deactivated
   --      +
   --      Note that this doesn't apply to the predefined entities (see below)

   Debug_Mode : constant Boolean := True;
   --  Set the global activation status for the debug traces. If this is set to
   --  False and the subprograms below are inlined, then no code will be
   --  generated to support debug traces. Otherwise, if Debug_Mode is True,
   --  then the debug traces can be activated selectively for each module.

   function Create (Unit_Name : String) return Debug_Handle;
   --  Create a new handle.
   --  Name is upper-cases, and looked-for in the configuration file to check
   --  whether debug traces should be emitted for that module.
   --
   --  Two calls to this subprogram with the same name (case insensitive) will
   --  always return the same handle.

   function Unit_Name (Handle : Debug_Handle) return String;
   --  Return the unit name (upper-cased) for this handle. This can be used for
   --  instance in generic packages to specialize the handle for a specific
   --  instance.

   procedure Trace (Handle   : Debug_Handle;
                    Message  : String;
                    Location : String := GNAT.Source_Info.Source_Location;
                    Entity   : String := GNAT.Source_Info.Enclosing_Entity);
   pragma Inline (Trace);
   --  Output Message to the stream associated with Handle, along with any
   --  extra information setup by the user (see the default handles below).
   --
   --  If message includes ASCII.LF characters, then several lines are output,
   --  starting with a special prefix
   --
   --  Do not modify the parameters Location and Entity, they will have proper
   --  default values.

   procedure Assert (Handle             : Debug_Handle;
                     Condition          : Boolean;
                     Error_Message      : String;
                     Message_If_Success : String := "";
                     Location : String := GNAT.Source_Info.Source_Location;
                     Entity   : String := GNAT.Source_Info.Enclosing_Entity);
   pragma Inline (Assert);
   --  If Condition is False, then output Error_Message to Handle.
   --  Assertion_Error is raised if Condition is False.
   --
   --  Condition is not tested if Handle is not active.
   --  Message_If_Success is logged if Condition is True and the message
   --  is not the empty string.

   function Active (Handle : Debug_Handle) return Boolean;
   pragma Inline (Active);
   --  Return True if traces for Handle are actived.
   --  This function can be used to avoid the evaluation of complex
   --  expressions in case traces are not actived, as in the following
   --  code:
   --     if Active (Handle) then
   --        begin
   --           Trace (Handle, Message & Expensive_Computation);
   --        end;
   --     end if;
   --  The extra begin...end block can be used to limit the impact on the
   --  heap for the evaluation of Expensive_Compuation.

   ------------------------
   -- Predefined handles --
   ------------------------
   --  The following handles are predefined. They shouldn't be used to
   --  actually output a message. However, they can be activated through
   --  the configuration file as usual, and will output additional
   --  information for each call to Trace by other handles.
   --
   --  "DEBUG.ABSOLUTE_TIME"
   --  If this handle is activated, then the absolute time Trace is called
   --  wil be added to the output string.

   --  "DEBUG.ELAPSED_TIME"
   --  If this handle is activated, then the elapsed time since the last
   --  call to Trace for this handler will be displayed.

   --  "DEBUG.STACK_TRACE"
   --  If this handle is activated, then the stack trace will be displayed.

   --  "DEBUG.STACK_TRACE_SYMBOLIC"
   --  If this handle is activated, then the symbol stack trace will be
   --  output instead.

   --  "DEBUG.LOCATION"
   --  If this is activated, then the location of the call to Trace is
   --  displayed. Note that, contrary to DEBUG.STACK_TRACE, this works on
   --  all targets, and even if the executable wasn't compiled with debug
   --  information.

   --  "DEBUG.COLORS"
   --  If this handle is activated, then the messages will use colors to
   --  separate the actual message from the information output in the
   --  stream.

   --  "DEBUG.ENCLOSING_ENTITY"
   --  If this handle is activated, the name of the enclosing entity at the
   --  location of the call to Trace will be displayed.

private
   type Debug_Handle_Record;
   type Debug_Handle is access Debug_Handle_Record;
end Debug;
