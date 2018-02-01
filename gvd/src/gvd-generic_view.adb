------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2018, AdaCore                     --
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

with Commands;             use Commands;
with Commands.Interactive; use Commands.Interactive;
with Glib;                 use Glib;
with GPS.Kernel.Actions;   use GPS.Kernel.Actions;
with GPS.Kernel.Hooks;     use GPS.Kernel.Hooks;
with GPS.Intl;             use GPS.Intl;
with Gtk.Widget;           use Gtk.Widget;
with Gtk.Window;           use Gtk.Window;
with Gtkada.Dialogs;       use Gtkada.Dialogs;
with Gtkada.Handlers;      use Gtkada.Handlers;
with Gtkada.MDI;           use Gtkada.MDI;
with String_Utils;         use String_Utils;
with GNATCOLL.Traces;      use GNATCOLL.Traces;

package body GVD.Generic_View is
   Me : constant GNATCOLL.Traces.Trace_Handle := Create
     ("GPS.DEBUGGING.GENERIC_VIEW");

   -----------------
   -- Set_Process --
   -----------------

   overriding procedure Set_Process
     (Self    : not null access Process_View_Record;
      Process : access Base_Visual_Debugger'Class) is
   begin
      Self.Process := Process;
   end Set_Process;

   package body Simple_Views is

      type Open_Command is new Interactive_Command with null record;
      overriding function Execute
        (Self    : access Open_Command;
         Context : Interactive_Command_Context) return Command_Return_Type;
      --  Opens the view and attach to current debugger

      type On_Debugger_Started is new Debugger_Hooks_Function with null record;
      overriding procedure Execute
        (Self     : On_Debugger_Started;
         Kernel   : not null access Kernel_Handle_Record'Class;
         Debugger : access Base_Visual_Debugger'Class);
      --  Called when the debugger is started, to connect non-attached views

      ----------------
      -- On_Destroy --
      ----------------

      procedure On_Destroy (View : access Gtk_Widget_Record'Class) is
         V : constant Views.View_Access := Views.View_Access (View);
      begin
         if Get_Process (V) /= null then
            V.On_Detach (Get_Process (V));
            Set_View (Get_Process (V), null);
            V.Set_Process (null);
         end if;
      end On_Destroy;

      -------------
      -- Execute --
      -------------

      overriding procedure Execute
         (Self     : On_Debugger_Terminate;
          Kernel   : not null access GPS.Kernel.Kernel_Handle_Record'Class;
          Debugger : access Base_Visual_Debugger'Class)
      is
         Block_Me : constant Block_Trace_Handle :=
           Create (Me, "Closing view " & Views.View_Name);
         V : constant access Formal_View_Record'Class := Get_View (Debugger);
         pragma Unreferenced (Self, Block_Me, Kernel);
      begin
         if V /= null then
            V.On_Detach (Debugger);
            Set_View (Debugger, null);
            V.Set_Process (null);

            --  ??? We used to call Close_Child on the Child_From_View (V).
            --  This unfortunately is fragile (given the comment below, and
            --  also because typing 'q' in the console results in invalid
            --  memory access). Also this means that the current desktop gets
            --  changed, so if the user opens the debugger again (or a second
            --  one again), the views are back to their default position.
            --
            --  For now, this code is thus commented out.

            --  Do not destroy the view when we are in the process of
            --  destroying the main window. What might happen otherwise is the
            --  following: we have the debugger console and debuggee console in
            --  the same notebook. The first is destroyed as a result of
            --  destroying the notebook. When that first is destroyed, it also
            --  calls this On_Debugger_Terminate for the debuggee console. If
            --  we were to destroy the latter, this means that
            --  gtk_notebook_destroy's loop would then point to an invalid
            --  location.
--              if Kernel.Get_Main_Window /= null
--                and then not Kernel.Get_Main_Window.In_Destruction
--              then
--                 Views.Child_From_View (V).Close_Child (Force => True);
--              end if;
         end if;
      end Execute;

      --------------------
      -- Attach_To_View --
      --------------------

      procedure Attach_To_View
        (Process             : access Base_Visual_Debugger'Class;
         Kernel              : not null access Kernel_Handle_Record'Class;
         Create_If_Necessary : Boolean)
      is
         MDI     : constant MDI_Window := GPS.Kernel.MDI.Get_MDI (Kernel);
         Child   : MDI_Child;
         Iter    : Child_Iterator;
         View    : access Formal_View_Record'Class;
         Button  : Message_Dialog_Buttons with Unreferenced;
      begin
         if Process = null then
            --  ??? Should try to attach to the current debugger, but there are
            --  elaboration circularities.
            --  P := Visual_Debugger
            --     (GVD_Module.Get_Current_Debugger (Kernel));
            null;
         else
            View := Get_View (Process);
         end if;

         if View = null then
            --  Do we have an existing unattached view ?
            Iter := First_Child (MDI);

            loop
               Child := Get (Iter);
               exit when Child = null;

               if Child.all in Views.Local_Formal_MDI_Child'Class then
                  View := Views.View_From_Child (Child);
                  exit when Get_Process (View) = null;
               end if;

               Next (Iter);
            end loop;

            --  If no existing view was found, create one

            if Child = null and then Create_If_Necessary then
               View := Views.Get_Or_Create_View (Kernel);
               Child := Views.Child_From_View (View);
            end if;

            if Child /= null then
               --  In case it was hidden because of the preference
               Show (View);

               --  Make it visible again
               Raise_Child (Child);

               if Process /= null then
                  View.Set_Process (Process);
                  Set_View (Process, View);

                  if Get_Num (Process) = 1 then
                     Set_Title (Child, Views.View_Name);
                  else
                     Set_Title
                       (Child,
                        Views.View_Name
                        & " <"
                        & Image (Integer (Get_Num (Process)))
                        & ">");
                  end if;

                  On_Attach (View, Process);

                  if Process.Command_In_Process then
                     Button := Message_Dialog
                       (-"Cannot update " & Views.View_Name
                        & (-" while the debugger is busy." & ASCII.LF &
                          (-"Interrupt the debugger or wait for its"
                             & " availability.")),
                        Dialog_Type => Warning,
                        Buttons     => Button_OK,
                        Parent      => Kernel.Get_Main_Window);
                  else
                     Update (View);
                  end if;

                  Widget_Callback.Connect
                    (View, Signal_Destroy, Destroy_Access);
               end if;
            end if;

         else
            Child := Views.Child_From_View (View);

            if Child /= null then
               Raise_Child (Child);
            else
               --  Something really bad happened: the stack window is not
               --  part of the MDI, reset it.
               Destroy (View);

               if Process /= null then
                  Set_View (Process, null);
               end if;
            end if;
         end if;
      end Attach_To_View;

      -------------
      -- Execute --
      -------------

      overriding procedure Execute
        (Self     : On_Update;
         Kernel   : not null access GPS.Kernel.Kernel_Handle_Record'Class;
         Debugger : access Base_Visual_Debugger'Class)
      is
         pragma Unreferenced (Self, Kernel);
         V : constant access Formal_View_Record'Class := Get_View (Debugger);
      begin
         if V /= null then
            V.Update;
         end if;
      end Execute;

      -------------
      -- Execute --
      -------------

      overriding procedure Execute
        (Self      : On_Debugger_State_Changed;
         Kernel    : not null access GPS.Kernel.Kernel_Handle_Record'Class;
         Debugger  : access Base_Visual_Debugger'Class;
         New_State : Debugger_State)
      is
         pragma Unreferenced (Self, Kernel);
         V : constant access Formal_View_Record'Class := Get_View (Debugger);
      begin
         if V /= null then
            V.On_State_Changed (New_State);
         end if;
      end Execute;

      -------------
      -- Execute --
      -------------

      overriding procedure Execute
         (Self     : On_Debug_Process_Terminated;
          Kernel   : not null access GPS.Kernel.Kernel_Handle_Record'Class;
          Debugger : access Base_Visual_Debugger'Class)
      is
         pragma Unreferenced (Self, Kernel);
         V : constant access Formal_View_Record'Class := Get_View (Debugger);
      begin
         if V /= null then
            V.On_Process_Terminated;
         end if;
      end Execute;

      ---------------------
      -- Register_Module --
      ---------------------

      procedure Register_Module
        (Kernel : not null access Kernel_Handle_Record'Class) is
      begin
         Views.Register_Module (Kernel);

         --  We need to attach the view to the starting debugger before all
         --  the other hook functions: this avoids attaching the view to
         --  a debugger that is already exiting because of an hook function
         --  that was run before this one.

         Debugger_Started_Hook.Add (new On_Debugger_Started, Last => False);

         Debugger_Process_Stopped_Hook.Add (new On_Update);
         Debugger_Context_Changed_Hook.Add (new On_Update);
         Debugger_State_Changed_Hook.Add (new On_Debugger_State_Changed);
         Debugger_Terminated_Hook.Add (new On_Debugger_Terminate);
         Debugger_Process_Terminated_Hook.Add
           (new On_Debug_Process_Terminated);
      end Register_Module;

      -------------
      -- Execute --
      -------------

      overriding procedure Execute
        (Self     : On_Debugger_Started;
         Kernel   : not null access Kernel_Handle_Record'Class;
         Debugger : access Base_Visual_Debugger'Class)
      is
         pragma Unreferenced (Self);
      begin
         Attach_To_View (Debugger, Kernel, Create_If_Necessary => False);
      end Execute;

      -------------
      -- Execute --
      -------------

      overriding function Execute
        (Self    : access Open_Command;
         Context : Interactive_Command_Context) return Command_Return_Type
      is
         pragma Unreferenced (Self);
         Kernel  : constant Kernel_Handle := Get_Kernel (Context.Context);
         Process : constant access Base_Visual_Debugger'Class :=
           Get_Current_Debugger (Kernel);
      begin
         if Works_Without_Debugger or else Process /= null then
            Attach_To_View (Process, Kernel, Create_If_Necessary => True);
         end if;
         return Commands.Success;
      end Execute;

      -------------------------------
      -- Register_Open_View_Action --
      -------------------------------

      procedure Register_Open_View_Action
        (Kernel      : not null access GPS.Kernel.Kernel_Handle_Record'Class;
         Action_Name : String;
         Description : String;
         Filter      : Action_Filter := null)
      is
         F : Action_Filter := Filter;
      begin
         if not Works_Without_Debugger then
            F := F and Lookup_Filter (Kernel, "Debugger active");
         end if;

         Register_Action
           (Kernel, Action_Name, new Open_Command,
            Description => Description,
            Category    => -"Views",
            Filter      => F);
      end Register_Open_View_Action;

   end Simple_Views;

end GVD.Generic_View;
