-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2003 - 2005                      --
--                            AdaCore                                --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package defines a new kind of command which can be called
--  interactively by the user (bound to keys, menus or toolbars for
--  instance)

with Gdk.Event;
with Glib.Xml_Int;
with GNAT.OS_Lib;
with GPS.Kernel;
with Gtk.Widget;

package Commands.Interactive is

   type Component_Iterator_Record is abstract tagged null record;
   type Component_Iterator is access all Component_Iterator_Record'Class;

   ---------------------------------
   -- Interactive_Command_Context --
   ---------------------------------

   type Interactive_Command_Context is record
      Event   : Gdk.Event.Gdk_Event := null;
      Context : GPS.Kernel.Selection_Context_Access;

      Synchronous : Boolean := False;
      --  Whether the command should be executed synchronously

      Dir     : GNAT.OS_Lib.String_Access;
      --  The directory in which the execution should take place

      Args    : GNAT.OS_Lib.String_List_Access;
      Label   : GNAT.OS_Lib.String_Access;
   end record;

   Null_Context : constant Interactive_Command_Context :=
     (Event       => null,
      Context     => null,
      Synchronous => False,
      Dir         => null,
      Args        => null,
      Label       => null);

   procedure Free (X : in out Interactive_Command_Context);
   --  Free memory associated to X.

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

   function Component_Editor
     (Kernel    : access GPS.Kernel.Kernel_Handle_Record'Class;
      Component : access Command_Component_Record)
      return Gtk.Widget.Gtk_Widget is abstract;
   --  Return a graphical widget that can be used to edit Component.
   --  This widget should provide all the fields to edit the contents of the
   --  component.
   --  The result will be destroyed automatically when its parent container
   --  is destroyed.

   procedure Update_From_Editor
     (Component : access Command_Component_Record;
      Editor    : access Gtk.Widget.Gtk_Widget_Record'Class) is abstract;
   --  This function is passed the Editor created by Component_Editor, and
   --  should update the component accordingly.

   function Get_Name
     (Component : access Command_Component_Record) return String is abstract;
   --  Return a short name for the component. This will generally be the
   --  command that it executes. This is used when listing all the components
   --  of an action

   procedure To_XML
     (Component   : access Command_Component_Record;
      Action_Node : Glib.Xml_Int.Node_Ptr) is abstract;
   --  Create an XML node for the Component, and add it to Child, which should
   --  describe the command itself.

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

   function Execute (Command : access Interactive_Command)
      return Command_Return_Type;
   --  Execute the command non-interactively, with a Null_Context

   procedure Launch_Synchronous_Interactive
     (Command : access Interactive_Command'Class;
      Context : Interactive_Command_Context;
      Wait    : Duration := 0.0);
   --  Execute the command synchronously.
   --  This is similar to Commands.Lauch_Synchronous, except it also propagates
   --  the Event parameter.

   function Start
     (Command : access Interactive_Command) return Component_Iterator;
   --  Return the first component that makes up the command. Such commands are
   --  used mostly for graphical description of the command.
   --  By default, the command is considered as internal, and thus contains
   --  only one component, not editable graphically.
   --  Returned value must be freed by the caller.

   function Command_Editor
     (Command : access Interactive_Command) return Gtk.Widget.Gtk_Widget;
   --  Return a widget to edit the general properties of the command.
   --  This editor should include the various command_components, which are
   --  edited separately. For instance, the return widget should be used to
   --  edit the default window in which the output of the command goes,
   --  whether it applies to such or such module,...
   --  null should be returned if there is no specific property to edit for
   --  this command. This is the default.

   procedure Update_From_Editor
     (Command : access Interactive_Command;
      Editor  : Gtk.Widget.Gtk_Widget);
   --  Edit the properties of the command from the editor returned by
   --  Command_Editor

   procedure To_XML
     (Command     : access Interactive_Command;
      Action_Node : Glib.Xml_Int.Node_Ptr);
   --  Create XML nodes for Command. This should add attributes to Action_Node,
   --  and new children if needed.
   --  This should also save all components of the command
   --  By default, this does nothing

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
   --  proxy

   procedure Interrupt (Command : in out Interactive_Command_Proxy);
   function Execute (Command : access Interactive_Command_Proxy)
      return Command_Return_Type;
   function Name (Command : access Interactive_Command_Proxy) return String;
   procedure Free (X : in out Interactive_Command_Proxy);
   function Progress
     (Command : access Interactive_Command_Proxy) return Progress_Record;
   procedure Set_Progress
     (Command  : access Interactive_Command_Proxy;
      Progress : Progress_Record);
   function Undo (Command : access Interactive_Command_Proxy) return Boolean;
   --  See doc from inherited subprogram

private
   type Interactive_Command is abstract new Root_Command with null record;
   type Command_Component_Record is abstract tagged null record;
end Commands.Interactive;
