------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2022-2023, AdaCore                  --
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

with GNATCOLL.VFS;     use GNATCOLL.VFS;

with Gtk.Widget;
with Gtkada.Handlers;

with Generic_Views;
with GPS.Debuggers;
with GPS.Kernel;       use GPS.Kernel;
with GPS.Kernel.Hooks;
with GPS.Kernel.MDI;

with DAP.Clients;

package DAP.Views is

   --------------------
   -- View_Interface --
   --------------------

   type View_Interface is interface;

   procedure Set_Client
     (Self    : not null access View_Interface;
      Process : access DAP.Clients.DAP_Client'Class) is abstract;
   function Get_Client
     (Self : not null access View_Interface)
      return DAP.Clients.DAP_Client_Access is abstract;
   --  Return the debugger associated with that view, or null if the view
   --  is not associated currently.

   procedure On_Attach
     (Self   : not null access View_Interface;
      Client : not null access DAP.Clients.DAP_Client'Class) is null;
   procedure On_Detach
     (Self   : not null access View_Interface;
      Client : not null access DAP.Clients.DAP_Client'Class) is null;

   procedure On_Process_Terminated
     (Self : not null access View_Interface) is null;
   --  Called when the debugged process has terminated.

   procedure On_Status_Changed
     (Self   : not null access View_Interface;
      Status : GPS.Debuggers.Debugger_State) is null;

   procedure On_Location_Changed
     (Self : not null access View_Interface) is null;

   procedure Update (Self : not null access View_Interface) is null;

   -----------------
   -- View_Record --
   -----------------

   type View_Record is abstract
      new Generic_Views.View_Record and View_Interface with private;
   type View_Access is access all View_Record'Class;

   overriding procedure Set_Client
     (Self   : not null access View_Record;
      Client : access DAP.Clients.DAP_Client'Class);
   overriding function Get_Client
     (Self : not null access View_Record)
      return DAP.Clients.DAP_Client_Access;

   generic
      Works_Without_Debugger : Boolean := False;
      --  If true, the user can open this view even when no debugger is
      --  running.

      type Formal_View_Record is
        new Generic_Views.View_Record and View_Interface with private;
      --  The type used as a parent for the view, and used to store the view in
      --  a Visual_Debugger.

      type Formal_MDI_Child is new GPS.Kernel.MDI.GPS_MDI_Child_Record
         with private;
      --  ??? Seems to be needed because of a bug in GNAT, since we get errors
      --  when Formal_MDI_Child is not explicitly declared in Views below.

      with package Formal_Views is new Generic_Views.Simple_Views
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

   package Simple_Views is

      procedure Attach_To_View
        (Client              : access DAP.Clients.DAP_Client'Class;
         Kernel              : not null access Kernel_Handle_Record'Class;
         Create_If_Necessary : Boolean;
         Update_On_Attach    : Boolean := True;
         Name                : String := "");
      --  Call Update for the view after attaching when
      --  Update_On_Attach is True.

      procedure Register_Module
        (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class);
      --  Register the functions needed to load and save the desktop

      procedure Register_Open_View_Action
        (Kernel      : not null access GPS.Kernel.Kernel_Handle_Record'Class;
         Action_Name : String;
         Description : String;
         Filter      : GPS.Kernel.Action_Filter := null);
      --  Create a new action that will open the view.

   private
      procedure On_Destroy
        (View : access Gtk.Widget.Gtk_Widget_Record'Class);
      Destroy_Access : constant Gtkada.Handlers.Widget_Callback.Simple_Handler
         := On_Destroy'Access;
      --  Callback for the "destroy_event" signal on the Call Stack window.
      --  This needs to be in the spec since it is used as a callback in the
      --  body.

      type On_Debugger_Started is
        new GPS.Kernel.Hooks.Debugger_Hooks_Function with null record;
      overriding procedure Execute
        (Self     : On_Debugger_Started;
         Kernel   : not null access Kernel_Handle_Record'Class;
         Debugger : access GPS.Debuggers.Base_Visual_Debugger'Class);
      --  Called when the debugger is started, to connect non-attached views

      type On_Debugger_State_Changed is
        new GPS.Kernel.Hooks.Debugger_States_Hooks_Function with null record;
      overriding procedure Execute
        (Self      : On_Debugger_State_Changed;
         Kernel    : not null access GPS.Kernel.Kernel_Handle_Record'Class;
         Debugger  : access GPS.Debuggers.Base_Visual_Debugger'Class;
         New_State : GPS.Debuggers.Debugger_State);
      --  Hook called when the state of the debugger changes

      type On_Debug_Location_Changed is
        new GPS.Kernel.Hooks.Debugger_Hooks_Function with null record;
      overriding procedure Execute
        (Self     : On_Debug_Location_Changed;
         Kernel   : not null access GPS.Kernel.Kernel_Handle_Record'Class;
         Debugger : access GPS.Debuggers.Base_Visual_Debugger'Class);

      type On_Debug_Process_Terminated is
        new GPS.Kernel.Hooks.Debugger_Hooks_Function with null record;
      overriding procedure Execute
        (Self     : On_Debug_Process_Terminated;
         Kernel   : not null access GPS.Kernel.Kernel_Handle_Record'Class;
         Debugger : access GPS.Debuggers.Base_Visual_Debugger'Class);
      --  Called when the process has terminated

      type On_Debugger_Terminated is
        new GPS.Kernel.Hooks.Debugger_Hooks_Function with null record;
      overriding procedure Execute
        (Self     : On_Debugger_Terminated;
         Kernel   : not null access GPS.Kernel.Kernel_Handle_Record'Class;
         Debugger : access GPS.Debuggers.Base_Visual_Debugger'Class);

   end Simple_Views;

private
   type View_Record is abstract
      new Generic_Views.View_Record and View_Interface with
   record
      Client : access DAP.Clients.DAP_Client'Class;
      --  The process associated with the view
   end record;

   overriding function Get_Client
     (Self : not null access View_Record)
      return DAP.Clients.DAP_Client_Access
     is (Self.Client);

end DAP.Views;
