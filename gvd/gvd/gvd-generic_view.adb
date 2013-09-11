------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2013, AdaCore                     --
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

with Ada.Tags;            use Ada.Tags;
with Glib;                use Glib;
with Glib.Object;         use Glib.Object;
with GPS.Kernel;          use GPS.Kernel;
with GPS.Kernel.Hooks;    use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;      use GPS.Kernel.MDI;
with GPS.Kernel.Modules;  use GPS.Kernel.Modules;
with GPS.Intl;            use GPS.Intl;
with Gtk.Widget;          use Gtk.Widget;
with Gtk.Window;          use Gtk.Window;
with Gtkada.Dialogs;      use Gtkada.Dialogs;
with Gtkada.Handlers;     use Gtkada.Handlers;
with Gtkada.MDI;          use Gtkada.MDI;
with String_Utils;        use String_Utils;
with GNATCOLL.Traces;     use GNATCOLL.Traces;

package body GVD.Generic_View is
   Me : constant GNATCOLL.Traces.Trace_Handle := Create ("GVD");

   procedure Set_Process
     (View    : access Process_View_Record'Class;
      Process : Visual_Debugger);
   --  Set the debugger associated with View

   -----------------
   -- Set_Process --
   -----------------

   procedure Set_Process
     (View    : access Process_View_Record'Class;
      Process : Visual_Debugger) is
   begin
      View.Process := Process;
   end Set_Process;

   -----------------
   -- Get_Process --
   -----------------

   function Get_Process
     (View : access Process_View_Record)
      return Visual_Debugger is
   begin
      return View.Process;
   end Get_Process;

   -------------------
   -- Unset_Process --
   -------------------

   procedure Unset_Process (View : access Process_View_Record) is
   begin
      View.Process := null;
   end Unset_Process;

   ------------------
   -- Simple_Views --
   ------------------

   package body Simple_Views is
      function Local_Initialize
        (View   : access Formal_View_Record'Class)
         return Gtk_Widget;
      --  Initialize the view and returns the focus widget.

      package Views is new Generic_Views.Simple_Views
        (Module_Name        => Module_Name,
         View_Name          => View_Name,
         Formal_View_Record => Formal_View_Record,
         Formal_MDI_Child   => GPS_MDI_Child_Record,
         Reuse_If_Exist     => False,
         Initialize         => Local_Initialize,
         Position           => Position,
         Areas              => Areas,
         Commands_Category  => "",  --  No "open ... " command, since we might
                                    --  reuse existing views
         Group              => Group);
      subtype Formal_View_Access is Views.View_Access;
      use type Formal_View_Access;

      ----------------
      -- On_Destroy --
      ----------------

      procedure On_Destroy (View : access Gtk_Widget_Record'Class) is
         V : constant Formal_View_Access := Formal_View_Access (View);
      begin
         if Get_Process (V) /= null then
            Set_View (Get_Process (V), null);
            Unset_Process (V);
         end if;

      exception
         when E : others => Trace (Me, E);
      end On_Destroy;

      ---------------------------
      -- On_Debugger_Terminate --
      ---------------------------

      procedure On_Debugger_Terminate
        (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
         Data   : access GPS.Kernel.Hooks.Hooks_Data'Class)
      is
         P : constant Visual_Debugger := Get_Process (Data);
         V : constant Formal_View_Access := Formal_View_Access (Get_View (P));
      begin
         Trace (Me, "On_Debugger_Terminate, closing view " & Module_Name);
         if P /= null and then V /= null then
            --  Do not destroy the view when we are in the process of
            --  destroying the main window. What might happen otherwise is the
            --  following: we have the debugger console and debuggee console in
            --  the same notebook. The first is destroyed as a result of
            --  destroying the notebook. When that first is destroyed, it also
            --  calls this On_Debugger_Terminate for the debuggee console. If
            --  we were to destroy the latter, this means that
            --  gtk_notebook_destroy's loop would then point to an invalid
            --  location.
            if Get_Main_Window (Kernel) /= null
              and then not Get_Main_Window (Kernel).In_Destruction
              and then Get_Process (V) /= null
            then
               Set_View (Get_Process (V), null);
               Unset_Process (V);
               Views.Child_From_View (V).Close_Child (Force => True);
            else
               Set_View (Get_Process (V), null);
               Unset_Process (V);
            end if;
         end if;
         Trace (Me, "On_Debugger_Terminate, done closing view " & Module_Name);

      exception
         when E : others => Trace (Me, E);
      end On_Debugger_Terminate;

      --------------------
      -- Attach_To_View --
      --------------------

      procedure Attach_To_View
        (Process             : access Visual_Debugger_Record'Class;
         Create_If_Necessary : Boolean)
      is
         Kernel  : constant Kernel_Handle := Get_Kernel (Get_Module.all);
         MDI     : constant MDI_Window := Get_MDI (Kernel);
         Child   : MDI_Child;
         Iter    : Child_Iterator;
         View    : Formal_View_Access;
         Button  : Message_Dialog_Buttons;
         pragma Unreferenced (Button);

--           List : Debugger_List_Link;
         P    : constant Visual_Debugger := Visual_Debugger (Process);

      begin
         if Process = null then
            null;
            --  ??? Should try to attach to the current debugger, but there are
            --  elaboration circularities.
--              List := Get_Debugger_List (Kernel);
--              if List /= null then
--                 P := Visual_Debugger (List.Debugger);
--              end if;
         end if;

         View := Formal_View_Access (Get_View (P));

         if View = null then
            --  Do we have an existing unattached view ?
            Iter := First_Child (MDI);

            loop
               Child := Get (Iter);
               exit when Child = null;

               if Get_Widget (Child)'Tag = Formal_View_Record'Tag then
                  View := Formal_View_Access (Get_Widget (Child));
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

               if P /= null then
                  Set_Process (View, P);
                  Set_View (P, Base_Type_Access (View));

                  if Get_Num (P) = 1 then
                     Set_Title (Child, View_Name);
                  else
                     Set_Title
                       (Child,
                        View_Name
                        & " <"
                        & Image (Integer (Get_Num (P)))
                        & ">");
                  end if;

                  On_Attach (View, P);

                  if Command_In_Process (P) then
                     Button := Message_Dialog
                       (-"Cannot update " & View_Name
                        & (-" while the debugger is busy." & ASCII.LF &
                          (-"Interrupt the debugger or wait for its"
                             & " availability.")),
                        Dialog_Type => Warning,
                        Buttons     => Button_OK);
                  else
                     Update (View);
                  end if;

                  Widget_Callback.Connect
                    (View, Signal_Destroy, Destroy_Access);
               end if;
            end if;

         else
            Child := Find_MDI_Child (MDI, View);

            if Child /= null then
               Raise_Child (Child);
            else
               --  Something really bad happened: the stack window is not
               --  part of the MDI, reset it.
               Destroy (View);

               if P /= null then
                  Set_View (P, null);
               end if;
            end if;
         end if;
      end Attach_To_View;

      ----------------------
      -- Local_Initialize --
      ----------------------

      function Local_Initialize
        (View   : access Formal_View_Record'Class)
         return Gtk_Widget
      is
         Focus_Widget : Gtk_Widget;
      begin
         Focus_Widget := Initialize (View, View.Kernel);
         Add_Hook
           (View.Kernel,
            Hook  => Debugger_Terminated_Hook,
            Func  => Wrapper (On_Debugger_Terminate'Unrestricted_Access),
            Name  => "terminate_" & Module_Name,
            Watch => GObject (View));
         return Focus_Widget;
      end Local_Initialize;

      ---------------
      -- On_Update --
      ---------------

      procedure On_Update
        (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
         Data   : access GPS.Kernel.Hooks.Hooks_Data'Class)
      is
         pragma Unreferenced (Kernel);
         Process : constant Visual_Debugger := Get_Process (Data);
         View    : constant Formal_View_Access :=
                     Formal_View_Access (Get_View (Process));
      begin
         if View /= null then
            Update (View);
         end if;

      exception
         when E : others => Trace (Me, E);
      end On_Update;

      -------------------
      -- State_Changed --
      -------------------

      procedure State_Changed
        (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
         Data   : access GPS.Kernel.Hooks.Hooks_Data'Class)
      is
         pragma Unreferenced (Kernel);
         Process : constant Visual_Debugger := Get_Process (Data);
         View    : constant Formal_View_Access :=
                     Formal_View_Access (Get_View (Process));
      begin
         if View /= null then
            On_State_Changed (View, Get_State (Data));
         end if;

      exception
         when E : others => Trace (Me, E);
      end State_Changed;

      ------------------------
      -- Process_Terminated --
      ------------------------

      procedure Process_Terminated
        (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
         Data   : access GPS.Kernel.Hooks.Hooks_Data'Class)
      is
         pragma Unreferenced (Kernel);
         Process : constant Visual_Debugger := Get_Process (Data);
         View    : constant Formal_View_Access :=
                     Formal_View_Access (Get_View (Process));
      begin
         if View /= null then
            On_Process_Terminated (View);
         end if;

      exception
         when E : others => Trace (Me, E);
      end Process_Terminated;

      --------------------------------
      -- Register_Desktop_Functions --
      --------------------------------

      procedure Register_Desktop_Functions
        (Kernel : access Kernel_Handle_Record'Class)
      is
      begin
         Views.Register_Module (Kernel, Menu_Name => "");
                  Add_Hook (Kernel, Debugger_Process_Stopped_Hook,
                   Wrapper (On_Update'Unrestricted_Access),
                   Name => Module_Name & ".process_stopped");
         Add_Hook (Kernel, Debugger_Context_Changed_Hook,
                   Wrapper (On_Update'Unrestricted_Access),
                   Name => Module_Name & ".context_changed");
         Add_Hook (Kernel, Debugger_State_Changed_Hook,
                   Wrapper (State_Changed'Unrestricted_Access),
                   Name => Module_Name & ".state_changed");
         Add_Hook (Kernel, Debugger_Process_Terminated_Hook,
                   Wrapper (Process_Terminated'Unrestricted_Access),
                   Name => Module_Name & ".process_terminated");
      end Register_Desktop_Functions;

   end Simple_Views;

end GVD.Generic_View;
