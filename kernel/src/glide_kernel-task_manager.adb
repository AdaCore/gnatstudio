-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2003                         --
--                            ACT-Europe                             --
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

with GVD.Status_Bar;           use GVD.Status_Bar;

with Glide_Main_Window;        use Glide_Main_Window;

with Glide_Kernel.Modules;     use Glide_Kernel.Modules;
with Glide_Kernel.Scripts;     use Glide_Kernel.Scripts;
with Task_Manager;             use Task_Manager;
with Task_Manager.GUI;         use Task_Manager.GUI;

with Glib.Xml_Int;             use Glib.Xml_Int;
with Glib.Object;              use Glib.Object;
with Gtkada.MDI;               use Gtkada.MDI;
with Traces;                   use Traces;
with Ada.Exceptions;           use Ada.Exceptions;
with Glide_Intl;               use Glide_Intl;

with Commands.Custom;          use Commands.Custom;

package body Glide_Kernel.Task_Manager is

   type Task_Manager_Module_Id_Record is new Module_ID_Record with
     null record;

   type Task_Manager_Module_Id_Access is access all
     Task_Manager_Module_Id_Record'Class;

   Task_Manager_Module_Id   : Task_Manager_Module_Id_Access;
   Task_Manager_Module_Name : constant String := "Glide_Kernel.Task_Manager";

   Me : constant Debug_Handle := Create (Task_Manager_Module_Name);

   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child;
   --  Restore the status of the explorer from a saved XML tree.

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
      return Node_Ptr;
   --  Save the status of the project explorer to an XML tree

   procedure Destroy (Module : in out Task_Manager_Module_Id_Record);
   --  Called when the module is destroyed.

   function Get_Task_Manager
     (Kernel : access Kernel_Handle_Record'Class) return Task_Manager_Access;
   --  Return the GPS task manager.

   function Get_Or_Create_Task_Manager_Interface_MDI
     (Kernel         : access Kernel_Handle_Record'Class;
      Allow_Creation : Boolean := True)
      return MDI_Child;
   --  Internal version of Get_Or_Create_Task_Manager_Interface

   procedure On_Task_Manager
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Callback for Tools->Task Manager.

   type Wrapper_Command is new Root_Command with record
      Command : Command_Access;
   end record;
   type Wrapper_Command_Access is access all Wrapper_Command'Class;
   function Execute
     (Command : access Wrapper_Command) return Command_Return_Type;
   --  A wrapper for commands, so that they are not destroyed on exit.

   function Create_Wrapper
     (Command : access Root_Command'Class) return Command_Access;
   --  Create a new wrapper

   ---------------------
   -- On_Task_Manager --
   ---------------------

   procedure On_Task_Manager
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Child : MDI_Child;
   begin
      Child := Get_Or_Create_Task_Manager_Interface_MDI (Kernel, True);

      if Child /= null then
         Set_Focus_Child (Child);
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Task_Manager;

   ----------------------------------------------
   -- Get_Or_Create_Task_Manager_Interface_MDI --
   ----------------------------------------------

   function Get_Or_Create_Task_Manager_Interface_MDI
     (Kernel         : access Kernel_Handle_Record'Class;
      Allow_Creation : Boolean := True)
      return MDI_Child
   is
      Child   : MDI_Child := Find_MDI_Child_By_Tag
        (Get_MDI (Kernel), Task_Manager_Interface_Record'Tag);
      Interface : Task_Manager_Interface;
   begin
      if Child = null then
         if not Allow_Creation then
            return null;
         end if;

         Gtk_New (Interface, Get_Task_Manager (Kernel));

         Child := Put
           (Kernel, Interface, Module => Task_Manager_Module_Id,
            Desktop_Independent => True);
         Set_Focus_Child (Child);

         Set_Title (Child, -"Task Manager");
         Set_Dock_Side (Child, Bottom);
         Dock_Child (Child);
         return Child;
      else
         return Child;
      end if;
   end Get_Or_Create_Task_Manager_Interface_MDI;

   ------------------
   -- Load_Desktop --
   ------------------

   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child
   is
      pragma Unreferenced (MDI);
   begin
      if Node.Tag.all = "Task_Manager_Record" then
         return Get_Or_Create_Task_Manager_Interface_MDI
           (User, Allow_Creation => True);
      end if;

      return null;
   end Load_Desktop;

   ------------------
   -- Save_Desktop --
   ------------------

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
     return Node_Ptr
   is
      N : Node_Ptr;
   begin
      if Widget.all in Task_Manager_Interface_Record'Class then
         N := new Node;
         N.Tag := new String'("Task_Manager_Record");
         return N;
      end if;

      return null;
   end Save_Desktop;

   ----------------------
   -- Get_Task_Manager --
   ----------------------

   function Get_Task_Manager
     (Kernel : access Kernel_Handle_Record'Class) return Task_Manager_Access is
   begin
      if Kernel.Tasks = null then
         Kernel.Tasks := new Task_Manager_Record;
      end if;

      return Kernel.Tasks;
   end Get_Task_Manager;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Wrapper_Command) return Command_Return_Type is
   begin
      return Execute (Command.Command);
   end Execute;

   --------------------
   -- Create_Wrapper --
   --------------------

   function Create_Wrapper
     (Command : access Root_Command'Class) return Command_Access
   is
      C : Wrapper_Command_Access := new Wrapper_Command;
   begin
      C.Command := Command_Access (Command);
      return Command_Access (C);
   end Create_Wrapper;

   -------------------------------
   -- Launch_Background_Command --
   -------------------------------

   procedure Launch_Background_Command
     (Kernel   : access Kernel_Handle_Record'Class;
      Command  : access Root_Command'Class;
      Active   : Boolean;
      Queue_Id : String := "";
      Destroy_On_Exit : Boolean := True)
   is
      Manager : constant Task_Manager_Access := Get_Task_Manager (Kernel);
   begin
      if Destroy_On_Exit then
         Add_Command (Manager, Command_Access (Command), Active, Queue_Id);
      else
         Add_Command (Manager, Create_Wrapper (Command), Active, Queue_Id);
      end if;
   end Launch_Background_Command;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Tools : constant String := "/" & (-"Tools");
      Shell : constant String := Tools &  "/" & (-"Shell Console");

      Push_Command, Pop_Command : Custom_Command_Access;
      Script                    : Scripting_Language;
   begin
      Task_Manager_Module_Id :=
        new Task_Manager_Module_Id_Record;
      Register_Module
        (Module       => Module_ID (Task_Manager_Module_Id),
         Kernel       => Kernel,
         Module_Name  => Task_Manager_Module_Name,
         Priority     => Default_Priority);
      Glide_Kernel.Kernel_Desktop.Register_Desktop_Functions
        (Save_Desktop'Access, Load_Desktop'Access);

      Set_Progress_Area
        (Get_Task_Manager (Kernel),
         Get_Progress_Area
           (Glide_Window (Get_Main_Window (Kernel)).Statusbar));

      Script := Lookup_Scripting_Language (Kernel, GPS_Shell_Name);

      Create
        (Push_Command, Kernel_Handle (Kernel), "busy_indicator.push_state",
         Script);

      Create
        (Pop_Command, Kernel_Handle (Kernel), "busy_indicator.pop_state",
         Script);

      Set_Busy_Commands
        (Get_Task_Manager (Kernel),
         Command_Access (Push_Command),
         Command_Access (Pop_Command));

      Register_Menu
        (Kernel,
         Tools,
           -"Task Manager",
         Callback => On_Task_Manager'Access,
         Ref_Item => Shell);
   end Register_Module;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Module : in out Task_Manager_Module_Id_Record) is
      pragma Unreferenced (Module);
   begin
      null;
   end Destroy;

end Glide_Kernel.Task_Manager;
