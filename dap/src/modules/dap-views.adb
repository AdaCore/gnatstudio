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

with Ada.Tags;
with Gtkada.MDI;           use Gtkada.MDI;
with Gtkada.Handlers;      use Gtkada.Handlers;

with GNATCOLL.Traces;      use GNATCOLL.Traces;

with GPS.Kernel.Actions;

with Commands;             use Commands;
with Commands.Interactive; use Commands.Interactive;

with DAP.Module;

package body DAP.Views is

   Me : constant GNATCOLL.Traces.Trace_Handle := Create ("GPS.DAP.Views");

   ----------------
   -- Set_Client --
   ----------------

   overriding procedure Set_Client
     (Self : not null access View_Record;
      Id   : Integer) is
   begin
      Self.Client_Id := Id;
   end Set_Client;

   ------------------
   -- Simple_Views --
   ------------------

   package body Simple_Views is

      use DAP.Clients;

      type Open_Command is new Interactive_Command with null record;
      overriding function Execute
        (Self    : access Open_Command;
         Context : Interactive_Command_Context) return Command_Return_Type;
      --  Opens the view and attach to current debugger

      --------------------
      -- Attach_To_View --
      --------------------

      procedure Attach_To_View
        (Client              : not null access DAP.Clients.DAP_Client'Class;
         Kernel              : not null access Kernel_Handle_Record'Class;
         Create_If_Necessary : Boolean;
         Update_On_Attach    : Boolean := True;
         Name                : String := "")

      is
         Child : MDI_Child;
         View  : access Formal_View_Record'Class;
      begin
         --  Try to retrieve any existing view
         View := Formal_Views.Retrieve_View (Kernel);

         --  If there is no available view: create a new one if asked,
         --  appending Name to the MDI child's title.
         --  Otherwise, just retrieve the already existing MDI child.
         if View = null and then Create_If_Necessary then
            View  := Formal_Views.Get_Or_Create_View (Kernel);
            Widget_Callback.Connect
              (View, Gtk.Widget.Signal_Destroy, Destroy_Access);
            Child := Formal_Views.Child_From_View (View);
            Set_Title (Child, Formal_Views.View_Name & Name);
         elsif View /= null then
            Child := Formal_Views.Child_From_View (View);
         end if;

         --  We have a view, either an already existing one or a newly created
         --  one: attach it to the given DAP client
         if Child /= null then
            Trace
              (Me,
               "Attaching client to view: "
               & Ada.Tags.Expanded_Name (View'Tag));

            View.Set_Client (Client.Id);
            On_Attach (View, Client);

            --  Update the view if asked
            if Update_On_Attach then
               Update (View);
            end if;

            --  Make sure to make the view visible
            Show (View);
            Raise_Child (Child);
         end if;
      end Attach_To_View;

      -------------
      -- Execute --
      -------------

      overriding function Execute
        (Self    : access Open_Command;
         Context : Interactive_Command_Context) return Command_Return_Type
      is
         pragma Unreferenced (Self);
         use DAP.Clients;

         Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
         Client : constant DAP.Clients.DAP_Client_Access :=
           DAP.Module.Get_Current_Debugger;
         Dummy  : Formal_Views.View_Access;
      begin
         if Client /= null then
            Attach_To_View (Client, Kernel, Create_If_Necessary => True);

         elsif Works_Without_Debugger then
            Dummy := Formal_Views.Get_Or_Create_View (Kernel);
         end if;

         return Commands.Success;
      end Execute;

      -------------
      -- Execute --
      -------------

      overriding procedure Execute
        (Self     : On_Debugger_Started;
         Kernel   : not null access Kernel_Handle_Record'Class;
         Debugger : access GPS.Debuggers.Base_Visual_Debugger'Class)
      is
         pragma Unreferenced (Self);

         Client : DAP.Clients.DAP_Client_Access;

      begin
         if Debugger /= null then
            Client := DAP.Clients.DAP_Visual_Debugger_Access (Debugger).Client;
         end if;

         if Client /= null then
            Attach_To_View (Client, Kernel, Create_If_Necessary => False);
         end if;
      end Execute;

      -------------
      -- Execute --
      -------------

      overriding procedure Execute
        (Self      : On_Debugger_State_Changed;
         Kernel    : not null access GPS.Kernel.Kernel_Handle_Record'Class;
         Debugger  : access GPS.Debuggers.Base_Visual_Debugger'Class;
         New_State : GPS.Debuggers.Debugger_State)
      is
         pragma Unreferenced (Self);

         V      : constant access Formal_View_Record'Class :=
           Formal_Views.Retrieve_View (Kernel);
      begin

         if V /= null then
            V.On_Status_Changed (New_State);
         end if;
      end Execute;

      -------------
      -- Execute --
      -------------

      overriding procedure Execute
         (Self     : On_Debug_Process_Terminated;
          Kernel   : not null access GPS.Kernel.Kernel_Handle_Record'Class;
          Debugger : access GPS.Debuggers.Base_Visual_Debugger'Class)
      is
         pragma Unreferenced (Self);

         V : constant access Formal_View_Record'Class :=
           Formal_Views.Retrieve_View (Kernel);
      begin
         if V /= null then
            V.On_Process_Terminated;
         end if;
      end Execute;

      -------------
      -- Execute --
      -------------

      overriding procedure Execute
         (Self     : On_Debugger_Terminated;
          Kernel   : not null access GPS.Kernel.Kernel_Handle_Record'Class;
          Debugger : access GPS.Debuggers.Base_Visual_Debugger'Class)
      is
         V : constant access Formal_View_Record'Class :=
           Formal_Views.Retrieve_View (Kernel);
      begin

         if V /= null and then V.Get_Client /= null then
            V.On_Detach (V.Get_Client);
            V.Set_Client (-1);
         end if;
      end Execute;

      -------------
      -- Execute --
      -------------

      overriding procedure Execute
        (Self     : On_Debug_Location_Changed;
         Kernel   : not null access GPS.Kernel.Kernel_Handle_Record'Class;
         Debugger : access GPS.Debuggers.Base_Visual_Debugger'Class)
      is
         pragma Unreferenced (Self);

         V      : constant access Formal_View_Record'Class :=
           Formal_Views.Retrieve_View (Kernel);
      begin
         if V /= null then
            V.On_Location_Changed;
         end if;
      end Execute;

      ----------------
      -- On_Destroy --
      ----------------

      procedure On_Destroy
        (View : access Gtk.Widget.Gtk_Widget_Record'Class)
      is
         use DAP.Clients;

         V : constant Formal_Views.View_Access :=
           Formal_Views.View_Access (View);
      begin
         if Get_Client (V) /= null then
            V.On_Detach (Get_Client (V));
            V.Set_Client (-1);
         end if;
      end On_Destroy;

      ---------------------
      -- Register_Module --
      ---------------------

      procedure Register_Module
        (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class) is
      begin
         Formal_Views.Register_Module (Kernel);

         GPS.Kernel.Hooks.Debugger_Started_Hook.Add
           (new On_Debugger_Started, Last => False);

         GPS.Kernel.Hooks.Debugger_Location_Changed_Hook.Add
           (new On_Debug_Location_Changed);
         GPS.Kernel.Hooks.Debugger_State_Changed_Hook.Add
           (new On_Debugger_State_Changed);
         GPS.Kernel.Hooks.Debugger_Process_Terminated_Hook.Add
           (new On_Debug_Process_Terminated);
         GPS.Kernel.Hooks.Debugger_Terminated_Hook.Add
           (new On_Debugger_Terminated);
      end Register_Module;

      -------------------------------
      -- Register_Open_View_Action --
      -------------------------------

      procedure Register_Open_View_Action
        (Kernel      : not null access GPS.Kernel.Kernel_Handle_Record'Class;
         Action_Name : String;
         Description : String;
         Filter      : GPS.Kernel.Action_Filter := null)
      is
         F : GPS.Kernel.Action_Filter := Filter;
      begin

         if not Works_Without_Debugger then
            F := F and Lookup_Filter (Kernel, "Has debuggers");
         end if;

         GPS.Kernel.Actions.Register_Action
           (Kernel, Action_Name, new Open_Command,
            Description => Description,
            Category    => "Views",
            Filter      => F);
      end Register_Open_View_Action;

   end Simple_Views;

   ----------------
   -- Get_Client --
   ----------------

   overriding function Get_Client
     (Self : not null access View_Record)
      return DAP.Clients.DAP_Client_Access
     is (DAP.Module.Get_Debugger (Self.Client_Id));

end DAP.Views;
