-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2000-2007                      --
--                              AdaCore                              --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Tags;           use Ada.Tags;

with Glib;               use Glib;
with Glib.Xml_Int;       use Glib.Xml_Int;
with GPS.Kernel;         use GPS.Kernel;
with GPS.Kernel.Hooks;   use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;     use GPS.Kernel.MDI;
with GPS.Kernel.Modules; use GPS.Kernel.Modules;
with GPS.Intl;           use GPS.Intl;
with Gtk.Object;         use Gtk.Object;
with Gtk.Widget;         use Gtk.Widget;
with Gtk.Window;         use Gtk.Window;
with Gtkada.Dialogs;     use Gtkada.Dialogs;
with Gtkada.Handlers;    use Gtkada.Handlers;
with Gtkada.MDI;         use Gtkada.MDI;
with String_Utils;       use String_Utils;
with Traces;             use Traces;

package body GVD.Generic_View is

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

   ---------------
   -- On_Attach --
   ---------------

   procedure On_Attach
     (View    : access Process_View_Record;
      Process : access Visual_Debugger_Record'Class)
   is
      pragma Unreferenced (View, Process);
   begin
      null;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Attach;

   -----------------
   -- Save_To_XML --
   -----------------

   function Save_To_XML
     (View : access Process_View_Record) return Glib.Xml_Int.Node_Ptr
   is
      pragma Unreferenced (View);
   begin
      return null;
   end Save_To_XML;

   -------------------
   -- Load_From_XML --
   -------------------

   procedure Load_From_XML
     (View : access Process_View_Record; XML : Glib.Xml_Int.Node_Ptr)
   is
      pragma Unreferenced (View, XML);
   begin
      null;
   end Load_From_XML;

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

   ------------
   -- Update --
   ------------

   procedure Update (View : access Process_View_Record) is
      pragma Unreferenced (View);
   begin
      null;
   end Update;

   ------------------
   -- Simple_Views --
   ------------------

   package body Simple_Views is
      type Formal_View_Access is access all Formal_View_Record'Class;

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
         when E : others => Trace (Exception_Handle, E);
      end On_Destroy;

      ---------------------------
      -- On_Debugger_Terminate --
      ---------------------------

      procedure On_Debugger_Terminate
        (View : access Gtk_Widget_Record'Class)
      is
         V : constant Formal_View_Access := Formal_View_Access (View);
         P : constant Visual_Debugger := Get_Process (V);
      begin
         if P /= null then
            --  Do not destroy the view when we are in the process of
            --  destroying the main window. What might happen otherwise is the
            --  following: we have the debugger console and debuggee console in
            --  the same notebook. The first is destroyed as a result of
            --  destroying the notebook. When that first is destroyed, it also
            --  calls this On_Debugger_Terminate for the debuggee console. If
            --  we were to destroy the latter, this means that
            --  gtk_notebook_destroy's loop would then point to an invalid
            --  location.
            if Get_Main_Window (Get_Kernel (P)) /= null
              and then not Gtk.Object.In_Destruction_Is_Set
                (Get_Main_Window (Get_Kernel (P)))
            then
               Destroy (View);
            else
               Set_View (Get_Process (V), null);
               Unset_Process (V);
            end if;
         end if;

      exception
         when E : others => Trace (Exception_Handle, E);
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
         Child2  : GPS_MDI_Child;
         Iter    : Child_Iterator;
         View    : Formal_View_Access;
         Button  : Message_Dialog_Buttons;
         pragma Unreferenced (Button);

      begin
         View := Formal_View_Access (Get_View (Process));

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
               View := new Formal_View_Record;
               Initialize (View, Kernel);
               Gtk_New (Child2, View,
                        Flags  => MDI_Child_Flags,
                        Group  => Group,
                        Default_Width => 150,
                        Default_Height => 150,
                        Module => Get_Module);
               Child := MDI_Child (Child2);
               Set_Title (Child, View_Name);
               Put (MDI, Child, Initial_Position => Position);
               Raise_Child (Child);
            end if;

            if Child /= null then
               --  First connect to the console. This way, if we are currently
               --  creating it, we won't really attach to it and avoid event
               --  loops

               if Get_Console (Process) /= null then
                  Widget_Callback.Object_Connect
                    (Get_Console (Process), Signal_Destroy,
                     On_Debugger_Terminate'Unrestricted_Access,
                     Slot_Object => View);
               end if;

               Set_Process (View, Visual_Debugger (Process));
               Set_View (Process, Base_Type_Access (View));

               if Get_Num (Visual_Debugger (Process)) = 1 then
                  Set_Title (Child, View_Name);
               else
                  Set_Title
                    (Child,
                     View_Name
                     & " <"
                     & Image (Integer (Get_Num (Visual_Debugger (Process))))
                     & ">");
               end if;

               On_Attach (View, Process);

               if Command_In_Process (Process) then
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
                 (View, Signal_Destroy, On_Destroy'Unrestricted_Access);
            end if;

         else
            Child := Find_MDI_Child (MDI, View);

            if Child /= null then
               Raise_Child (Child);
            else
               --  Something really bad happened: the stack window is not
               --  part of the MDI, reset it.
               Destroy (View);
               Set_View (Process, null);
            end if;
         end if;
      end Attach_To_View;

      ------------------
      -- Load_Desktop --
      ------------------

      function Load_Desktop
        (MDI    : MDI_Window;
         Node   : Glib.Xml_Int.Node_Ptr;
         Kernel : Kernel_Handle) return MDI_Child
      is
         Child : GPS_MDI_Child;
         View  : Formal_View_Access;
      begin
         if Node.Tag.all = Module_Name then
            View := new Formal_View_Record;
            Initialize (View, Kernel);

            if Node.Child /= null then
               Load_From_XML (View, Node.Child);
            end if;

            Gtk_New (Child, View,
                     Flags  => MDI_Child_Flags,
                     Group  => Group_Debugger_Stack,
                     Module => Get_Module);
            Set_Title (Child, View_Name);
            Put (MDI, Child, Initial_Position => Position_Right);
            return MDI_Child (Child);
         end if;

         return null;
      end Load_Desktop;

      ------------------
      -- Save_Desktop --
      ------------------

      function Save_Desktop
        (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
         Kernel : Kernel_Handle) return Glib.Xml_Int.Node_Ptr
      is
         pragma Unreferenced (Kernel);
         N : Glib.Xml_Int.Node_Ptr;
      begin
         if Widget'Tag = Formal_View_Record'Tag then
            N       := new Glib.Xml_Int.Node;
            N.Tag   := new String'(Module_Name);
            N.Child := Save_To_XML (Formal_View_Access (Widget));
            return N;
         end if;

         return null;
      end Save_Desktop;

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
         when E : others => Trace (Exception_Handle, E);
      end On_Update;

      --------------------------------
      -- Register_Desktop_Functions --
      --------------------------------

      procedure Register_Desktop_Functions
        (Kernel : access Kernel_Handle_Record'Class) is
      begin
         GPS.Kernel.Kernel_Desktop.Register_Desktop_Functions
           (Simple_Views.Save_Desktop'Unrestricted_Access,
            Simple_Views.Load_Desktop'Unrestricted_Access);
         Add_Hook (Kernel, Debugger_Process_Stopped_Hook,
                   Wrapper (On_Update'Unrestricted_Access),
                   Name => Module_Name & ".process_stopped");
         Add_Hook (Kernel, Debugger_Context_Changed_Hook,
                   Wrapper (On_Update'Unrestricted_Access),
                   Name => Module_Name & ".context_changed");
      end Register_Desktop_Functions;

   end Simple_Views;

end GVD.Generic_View;
