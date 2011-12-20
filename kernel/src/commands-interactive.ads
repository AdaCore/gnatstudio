------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2012, AdaCore                     --
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

--  This package defines a new kind of command which can be called
--  interactively by the user (bound to keys, menus or toolbars for
--  instance)

with Gdk.Event;
with GNAT.Strings;
with GNATCOLL.VFS; use GNATCOLL.VFS;
with GPS.Kernel;
with Gtk.Box;
with XML_Utils;

package Commands.Interactive is

   type Component_Iterator_Record is abstract tagged null record;
   type Component_Iterator is access all Component_Iterator_Record'Class;

   ---------------------------------
   -- Interactive_Command_Context --
   ---------------------------------

   type Interactive_Command_Context is record
      Event   : Gdk.Event.Gdk_Event := null;
      Context : GPS.Kernel.Selection_Context;

      Synchronous : Boolean := False;
      --  Whether the command should be executed synchronously

      Dir     : GNATCOLL.VFS.Virtual_File;
      --  The directory in which the execution should take place

      Args    : GNAT.Strings.String_List_Access;
      --  Args is the list of arguments to pass to this action. These can be
      --  accessed through $1, $2 in XML files.

      Label   : GNAT.Strings.String_Access;

      Repeat_Count     : Positive := 1;
      Remaining_Repeat : Natural := 0;
      --  The number of times that this command has been executed in a row,
      --  and the number of times it will still be executed.
      --  This is only different from 0 when the command "repeat-next"
      --  has been executed just before the current command.
   end record;
   --  Information about the current context.
   --  Event, if specified, must be a Deep_Copy of the actual event, since its
   --  lifetime might be different from that of the original event. If you are
   --  passing the context to Create_Proxy below, the deep copy will be done
   --  automatically for you.

   Null_Context : constant Interactive_Command_Context :=
                    (Event            => null,
                     Context          => GPS.Kernel.No_Context,
                     Synchronous      => False,
                     Dir              => No_File,
                     Args             => null,
                     Label            => null,
                     Repeat_Count     => 1,
                     Remaining_Repeat => 0);

   procedure Free (X : in out Interactive_Command_Context);
   --  Free memory associated to X

   ------------------------
   --  Command_Component --
   ------------------------

   type Command_Component_Record is abstract tagged private;
   type Command_Component is access all Command_Component_Record'Class;
   --  A command is usually a succession of small steps to reach a specific
   --  goal. These steps can be defined in a number of ways: either they are
   --  coded in the GPS source code itself or in a module programmed in Ada,
   --  or they are defined by the user in customization files, and are the
   --  result of executing GPS shell or Python scripts, or running external
   --  applications.

   function Get_Name
     (Component : access Command_Component_Record) return String is abstract;
   --  Return a short name for the component. This will generally be the
   --  command that it executes. This is used when listing all the components
   --  of an action

   -------------------------
   -- Interactive_Command --
   -------------------------

   type Interactive_Command is abstract new Root_Command with private;
   type Interactive_Command_Access is access all Interactive_Command'Class;

   function Execute
     (Command : access Interactive_Command;
      Context : Interactive_Command_Context)
      return Command_Return_Type is abstract;
   --  Execute the command.
   --  Context is the current context when the command is started. Its Event
   --  field is the event that started the execution (a Gdk_Key_Event
   --  if started from a key, a Gtk_Button_Event if started from a menu,...)

   overriding function Execute (Command : access Interactive_Command)
      return Command_Return_Type;
   --  Execute the command non-interactively, with a Null_Context

   function Start
     (Command : access Interactive_Command) return Component_Iterator;
   --  Return the first component that makes up the command. Such commands are
   --  used mostly for graphical description of the command.
   --  By default, the command is considered as internal, and thus contains
   --  only one component, not editable graphically.
   --  Returned value must be freed by the caller.

   type Command_Editor_Record is abstract new Gtk.Box.Gtk_Box_Record
      with null record;
   type Command_Editor is access all Command_Editor_Record'Class;

   function Create_Command_Editor
     (Command : access Interactive_Command;
      Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Command_Editor;
   --  Return a widget to edit a command and its various components

   function To_XML
     (Editor : access Command_Editor_Record)
      return XML_Utils.Node_Ptr is abstract;
   --  Return a newly allocated XML node representing the command edited by
   --  Editor. null will be returned if the command cannot be represented in
   --  XML (internal command for instance).

   -------------------------
   --  Component_Iterator --
   -------------------------

   function Get
     (Iter : access Component_Iterator_Record)
      return Command_Component is abstract;
   --  Return the current component, or null if there are no more components.
   --  The return value mustn't be freed by the caller.
   --  It still exists while the action exists

   procedure Next (Iter : access Component_Iterator_Record) is abstract;
   --  Move to the next component

   procedure Free (Iter : in out Component_Iterator_Record);
   procedure Free (Iter : in out Component_Iterator);
   --  Free the iterator. Does nothing by default

   function On_Failure
     (Iter : access Component_Iterator_Record) return Component_Iterator;
   --  Return a new iterator for the components to execute in case of failure
   --  of the current component.
   --  By default, this returns null to indicate that nothing will happen
   --  in this case, except that the command will stop executing

   -------------------------------
   -- Interactive_Command_Proxy --
   -------------------------------

   type Interactive_Command_Proxy is new Root_Command with record
      Command : Interactive_Command_Access;
      Context : Interactive_Command_Context;
   end record;
   type Interactive_Command_Proxy_Access
      is access Interactive_Command_Proxy'Class;
   --  This acts as a proxy for Interactive_Command, so that they can be called
   --  with an event. This should be used when one need to execute a procedure
   --  that expects a Root_Command.

   function Create_Proxy
     (Command : access Interactive_Command'Class;
      Context : Interactive_Command_Context) return Command_Access;
   --  Create a new proxy. Context.Args will be freed automatically by the
   --  proxy. Context.Event is also automatically copied (see comment for
   --  Interactive_Command_Context)

   overriding procedure Interrupt (Command : in out Interactive_Command_Proxy);
   overriding function Execute (Command : access Interactive_Command_Proxy)
      return Command_Return_Type;
   overriding function Name
     (Command : access Interactive_Command_Proxy) return String;
   overriding procedure Free (X : in out Interactive_Command_Proxy);
   overriding function Progress
     (Command : access Interactive_Command_Proxy) return Progress_Record;
   overriding procedure Set_Progress
     (Command  : access Interactive_Command_Proxy;
      Progress : Progress_Record);
   overriding function Undo
     (Command : access Interactive_Command_Proxy) return Boolean;
   --  See doc from inherited subprogram

private
   type Interactive_Command is abstract new Root_Command with null record;
   type Command_Component_Record is abstract tagged null record;
end Commands.Interactive;
