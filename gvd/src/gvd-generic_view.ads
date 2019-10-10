------------------------------------------------------------------------------
--                      GVD - The GNU Visual Debugger                       --
--                                                                          --
--                     Copyright (C) 2005-2019, AdaCore                     --
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

--  This package provides a number of subprograms to ease the creation of views
--  in the context of GVD.
--  This is a generic package. Because of elaboration circularity issues,
--  several of the formal parameters can really just be passed from
--  gvd-process and gvd_module.ads.
--
--  See default instanciations in GVD.Views

with Generic_Views;
with GPS.Debuggers;       use GPS.Debuggers;
with GPS.Kernel;          use GPS.Kernel;
with GPS.Kernel.MDI;

private with GPS.Kernel.Hooks;
private with Gtkada.Handlers;
private with Gtk.Widget;

package GVD.Generic_View is

   -----------------------
   -- View_With_Process --
   -----------------------

   type View_With_Process is interface;

   procedure Set_Process
     (Self    : not null access View_With_Process;
      Process : access Base_Visual_Debugger'Class) is abstract;
   function Get_Process
     (Self : not null access View_With_Process)
      return access Base_Visual_Debugger'Class is abstract;
   --  Return the debugger associated with that view, or null if the view
   --  is not associated currently.

   procedure On_Attach
     (View    : not null access View_With_Process;
      Process : not null access Base_Visual_Debugger'Class) is null;
   procedure On_Detach
     (View    : not null access View_With_Process;
      Process : not null access Base_Visual_Debugger'Class) is null;
   --  Called when the view is being attached to Process. This procedure should
   --  typically be used to connect to specific events on Process. However, it
   --  doesn't need to force a refresh of the view, which is done
   --  automatically.

   procedure Update (View : not null access View_With_Process) is null;
   --  Refresh the view by getting up-to-date information from the debugger.
   --  Nothing is done when the view is not associated with a debugger.
   --  It does nothing by default.

   procedure Frame_Changed (View : not null access View_With_Process) is null;
   --  Refresh the view by getting up-to-date information from the debugger if
   --  it is needed when the current frame is changed.
   --  Nothing is done when the view is not associated with a debugger.
   --  It does nothing by default.

   procedure On_Process_Terminated
     (View : not null access View_With_Process) is null;
   --  Called when the debugged process has terminated.

   procedure On_State_Changed
     (View      : not null access View_With_Process;
      New_State : GPS.Debuggers.Debugger_State) is null;
   --  Called when the debugger state has changed. One possibly use is to
   --  clear the view while the debugger is executing because its information
   --  might be confusing in such a case.

   ------------------
   -- Simple views --
   ------------------

   type Process_View_Record is abstract
      new Generic_Views.View_Record and View_With_Process with private;
   --  The base type for all debugger-related views.
   --  These are associated with a debugger (when one is running), and display
   --  info that debugger.

   overriding procedure Set_Process
     (Self    : not null access Process_View_Record;
      Process : access Base_Visual_Debugger'Class);
   overriding function Get_Process
     (Self : not null access Process_View_Record)
      return access Base_Visual_Debugger'Class;

   generic
      Works_Without_Debugger : Boolean := False;
      --  If true, the user can open this view even when no debugger is
      --  running.

      type Formal_View_Record is
        new Generic_Views.View_Record and View_With_Process with private;
      --  The type used as a parent for the view, and used to store the view in
      --  a Visual_Debugger.

      type Formal_MDI_Child is new GPS.Kernel.MDI.GPS_MDI_Child_Record
         with private;
      --  ??? Seems to be needed because of a bug in GNAT, since we get errors
      --  when Formal_MDI_Child is not explicitly declared in Views below.

      with package Views is new Generic_Views.Simple_Views
        (Formal_View_Record              => Formal_View_Record,
         Formal_MDI_Child                => Formal_MDI_Child,
         Save_Duplicates_In_Perspectives => False,
         Commands_Category               => "",
         others                          => <>);
      --  The description of the view in the MDI.
      --  The Commands_Category should be the empty string, since creating new
      --  views should attach them to the current debugger.
      --  ??? Can this be done via a primitive operation, rather than have to
      --  rewrite our own command.

      with function Get_View
        (Process : not null access Base_Visual_Debugger'Class)
         return access Formal_View_Record'Class is <>;
      with procedure Set_View
        (Process : not null access Base_Visual_Debugger'Class;
         View    : access Formal_View_Record'Class := null) is <>;
      --  Get or set the view in the visual debugger structure, so that it is
      --  closely associated with that process.
      --  The function Set_View is a good place to save the current state of
      --  the view in the properties if need be. It always occurs before the
      --  widget is actually destroyed, and while the debugger still exists
      --  (you should save the contents of the current view, before setting the
      --  new one). When Set_View is called with a null View, it means the
      --  process is being detached. At that point, it is a good idea to clear
      --  graphically the contents of the old view. At that point the
      --  view is still attached to a process with which you can interact.

      with function Get_Current_Debugger
        (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
         return access Base_Visual_Debugger'Class is <>;
      --  A proxy to the corresponding function in GVD_Module, because of
      --  elaboration circularities

   package Simple_Views is

      procedure Attach_To_View
        (Process             : access Base_Visual_Debugger'Class;
         Kernel              : not null access Kernel_Handle_Record'Class;
         Create_If_Necessary : Boolean);
      --  Attach the process to an instance of the view. It is valid to pass a
      --  null process, just to open a view (this will attach to the current
      --  debugger if possible)
      --
      --  If an unattached view exists in the desktop, it is reused.
      --  If none exists, one is created if Create_If_Necessary is true.
      --  Nothing is done when Process is already attached to a view.
      --
      --  The debugger console should be created already. When it is closed (ie
      --  the debugger exits), the view will be destroyed.

      procedure Register_Module
        (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class);
      --  Register the functions needed to load and save the desktop

      procedure Register_Open_View_Action
        (Kernel      : not null access GPS.Kernel.Kernel_Handle_Record'Class;
         Action_Name : String;
         Description : String;
         Filter      : Action_Filter := null);
      --  Create a new action that will open the view.

   private
      procedure On_Destroy
        (View : access Gtk.Widget.Gtk_Widget_Record'Class);
      Destroy_Access : constant Gtkada.Handlers.Widget_Callback.Simple_Handler
         := On_Destroy'Access;
      --  Callback for the "destroy_event" signal on the Call Stack window.
      --  This needs to be in the spec since it is used as a callback in the
      --  body.

      type On_Debugger_Terminate is
        new GPS.Kernel.Hooks.Debugger_Hooks_Function with null record;
      overriding procedure Execute
        (Self     : On_Debugger_Terminate;
         Kernel   : not null access GPS.Kernel.Kernel_Handle_Record'Class;
         Debugger : access Base_Visual_Debugger'Class);
      --  Callback when the debugger is terminated.
      --  This needs to be in the spec since it is used as a callback in the
      --  body.

      type On_Update is
        new GPS.Kernel.Hooks.Debugger_Hooks_Function with null record;
      overriding procedure Execute
        (Self     : On_Update;
         Kernel   : not null access GPS.Kernel.Kernel_Handle_Record'Class;
         Debugger : access Base_Visual_Debugger'Class);
      --  Hook called when the view needs to be refreshed

      type On_Debugger_Frame_Changed is
        new GPS.Kernel.Hooks.Debugger_Hooks_Function with null record;
      overriding procedure Execute
        (Self     : On_Debugger_Frame_Changed;
         Kernel   : not null access GPS.Kernel.Kernel_Handle_Record'Class;
         Debugger : access Base_Visual_Debugger'Class);
      --  Hook called when the current frame of the debugger changes

      type On_Debugger_State_Changed is
        new GPS.Kernel.Hooks.Debugger_States_Hooks_Function with null record;
      overriding procedure Execute
        (Self      : On_Debugger_State_Changed;
         Kernel    : not null access GPS.Kernel.Kernel_Handle_Record'Class;
         Debugger  : access Base_Visual_Debugger'Class;
         New_State : GPS.Debuggers.Debugger_State);
      --  Hook called when the state of the debugger changes

      type On_Debug_Process_Terminated is
        new GPS.Kernel.Hooks.Debugger_Hooks_Function with null record;
      overriding procedure Execute
        (Self     : On_Debug_Process_Terminated;
         Kernel   : not null access GPS.Kernel.Kernel_Handle_Record'Class;
         Debugger : access Base_Visual_Debugger'Class);
      --  Called when the process has terminated

   end Simple_Views;

private
   type Process_View_Record is abstract
      new Generic_Views.View_Record and View_With_Process with
   record
      Process : access Base_Visual_Debugger'Class;
      --  The process associated with the view
   end record;

   overriding function Get_Process
     (Self : not null access Process_View_Record)
      return access Base_Visual_Debugger'Class
     is (Self.Process);

end GVD.Generic_View;
