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

with XML_Utils;                 use XML_Utils;
with Glib.Object;               use Glib.Object;
with Gtk.Dialog;                use Gtk.Dialog;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Stock;                 use Gtk.Stock;
with Gtkada.MDI;                use Gtkada.MDI;

with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;     use GPS.Kernel.Modules.UI;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GPS.Main_Window;           use GPS.Main_Window;
with Task_Manager.GUI;          use Task_Manager.GUI;
with Task_Manager.Shell;        use Task_Manager.Shell;
with Traces;                    use Traces;
with Commands.Custom;           use Commands.Custom;

package body GPS.Kernel.Task_Manager is

   type Task_Manager_Module_Id_Record is new Module_ID_Record with null record;
   type Task_Manager_Module_Id_Access is access all
     Task_Manager_Module_Id_Record'Class;

   Task_Manager_Module_Id   : Task_Manager_Module_Id_Access;
   Task_Manager_Module_Name : constant String := "GPS.Kernel.Task_Manager";
   Command_Class_Name       : constant String := "Command";

   type Command_Property is new Instance_Property_Record with record
      Command : Scheduled_Command_Access;
   end record;
   type Command_Property_Access is access all Command_Property'Class;

   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child;
   --  Restore the status of the explorer from a saved XML tree

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      User   : Kernel_Handle) return Node_Ptr;
   --  Save the status of the project explorer to an XML tree

   overriding procedure Destroy
     (Module : in out Task_Manager_Module_Id_Record);
   --  Called when the module is destroyed

   function Get_Or_Create_Task_Manager_Interface_MDI
     (Kernel         : access Kernel_Handle_Record'Class;
      Allow_Creation : Boolean := True) return MDI_Child;
   --  Internal version of Get_Or_Create_Task_Manager_Interface

   procedure On_Task_Manager
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Callback for Tools->Views->Tasks

   function Create_Wrapper
     (Command         : access Root_Command'Class;
      Destroy_On_Exit : Boolean) return Scheduled_Command_Access;
   --  Create a new wrapper

   function On_Exit_Hook
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class) return Boolean;
   --  Called before GPS exits

   ------------------
   -- On_Exit_Hook --
   ------------------

   function On_Exit_Hook
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class) return Boolean
   is
      pragma Unreferenced (Data);

      Manager  : constant Task_Manager_Access := Get_Task_Manager (Kernel);
      Dialog   : Gtk_Dialog;
      Label    : Gtk_Label;
      Iface    : Task_Manager_Widget_Access;
      Button   : Gtk_Widget;
      Response : Gtk_Response_Type;

      Previous_Interface : constant Gtk_Widget := Get_GUI (Manager);
   begin
      if not Has_Running_Commands (Manager, Consider_Silent => False) then
         return True;
      end if;

      Gtk_New
        (Dialog,
         Title  => -"Tasks are running",
         Parent => Get_Current_Window (Kernel),
         Flags  => Modal or Destroy_With_Parent);

      Gtk_New
        (Label, -"The following tasks are running, do you want to quit GPS?" &
         ASCII.LF & (-"Warning: Quitting will kill all running tasks"));

      Set_Alignment (Label, 0.0, 0.0);
      Pack_Start (Get_Vbox (Dialog), Label, Expand => False, Padding => 10);

      Iface := Task_Manager_Dialog (Manager, Dialog => Gtk_Widget (Dialog));
      Pack_Start (Get_Vbox (Dialog), Iface, Padding => 10);

      Button := Add_Button (Dialog, Stock_Quit, Gtk_Response_Yes);
      Grab_Default (Button);
      Button := Add_Button (Dialog, -"Don't Quit", Gtk_Response_Cancel);

      Set_Default_Size (Dialog, 400, 300);
      Show_All (Dialog);
      Response := Run (Dialog);

      case Response is
         when Gtk_Response_Cancel =>
            Destroy (Dialog);
            Set_GUI (Manager, Previous_Interface);
            return False;

         when Gtk_Response_Yes =>
            Destroy (Dialog);
            Set_GUI (Manager, Previous_Interface);
            return True;

         when Gtk_Response_None | Gtk_Response_No =>
            Set_GUI (Manager, Previous_Interface);
            return False;

         when others =>
            Destroy (Dialog);
            Set_GUI (Manager, Previous_Interface);
            return False;
      end case;
   end On_Exit_Hook;

   -----------------------
   -- Show_Task_Manager --
   -----------------------

   procedure Show_Task_Manager (Kernel : Kernel_Handle) is
      Child : MDI_Child;
   begin
      Child := Get_Or_Create_Task_Manager_Interface_MDI (Kernel, True);

      if Child /= null then
         Set_Focus_Child (Child);
      end if;
   end Show_Task_Manager;

   ---------------------
   -- On_Task_Manager --
   ---------------------

   procedure On_Task_Manager
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      Show_Task_Manager (Kernel);
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Task_Manager;

   ----------------------------------------------
   -- Get_Or_Create_Task_Manager_Interface_MDI --
   ----------------------------------------------

   function Get_Or_Create_Task_Manager_Interface_MDI
     (Kernel         : access Kernel_Handle_Record'Class;
      Allow_Creation : Boolean := True) return MDI_Child
   is
      Child : GPS_MDI_Child := GPS_MDI_Child (Find_MDI_Child_By_Tag
        (Get_MDI (Kernel), Task_Manager_Widget_Record'Tag));
      Iface : Task_Manager_Widget_Access;
   begin
      if Child = null then
         if not Allow_Creation then
            return null;
         end if;

         Iface := Task_Manager_Dialog (Get_Task_Manager (Kernel));
         Gtk_New (Child, Iface,
                  Group               => Group_Consoles,
                  Module              => Task_Manager_Module_Id,
                  Focus_Widget        => Get_Focus_Widget (Iface),
                  Desktop_Independent => True);
         Set_Title (Child, -"Task Manager");
         Put (Get_MDI (Kernel), Child, Initial_Position => Position_Bottom);
         Set_Focus_Child (Child);
         return MDI_Child (Child);
      else
         return MDI_Child (Child);
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
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      User   : Kernel_Handle)
     return Node_Ptr
   is
      pragma Unreferenced (User);
      N : Node_Ptr;
   begin
      if Widget.all in Task_Manager_Widget_Record'Class then
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
      if Kernel.Tasks = No_Task_Manager then
         Kernel.Tasks := Create
           (Kernel_Handle (Kernel), Gtk_Widget (Get_Main_Window (Kernel)));
      end if;

      return Kernel.Tasks;
   end Get_Task_Manager;

   -----------------
   -- Get_Command --
   -----------------

   function Get_Command (Command : access Scheduled_Command'Class)
      return Command_Access is
   begin
      return Command.Command;
   end Get_Command;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Scheduled_Command) return Command_Return_Type is
   begin
      return Execute (Command.Command);
   end Execute;

   ----------
   -- Name --
   ----------

   overriding function Name
     (Command : access Scheduled_Command) return String is
   begin
      return Name (Command.Command);
   end Name;

   --------------
   -- Progress --
   --------------

   overriding function Progress
     (Command : access Scheduled_Command) return Progress_Record is
   begin
      return Progress (Command.Command);
   end Progress;

   ------------------
   -- Set_Progress --
   ------------------

   overriding procedure Set_Progress (Command : access Scheduled_Command;
                                      Progress : Progress_Record) is
   begin
      Set_Progress (Command.Command, Progress);
   end Set_Progress;

   ---------------
   -- Interrupt --
   ---------------

   overriding procedure Interrupt (Command : in out Scheduled_Command) is
   begin
      Interrupt (Command.Command.all);
   end Interrupt;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Command : in out Scheduled_Command) is
      Instances : constant Instance_Array := Get_Instances (Command.Instances);
   begin
      if Command.Is_Dead then
         for J in Instances'Range loop
            if Instances (J) /= No_Class_Instance then
               return;
            end if;
         end loop;

         if Command.Destroy_On_Exit then
            Unref (Command.Command);
         end if;

         Free (Command.Instances);
      else
         declare
            Dead_Command : Scheduled_Command_Access :=
                             new Scheduled_Command;
            Found        : Boolean := False;
         begin
            Dead_Command.Command := Command.Command;
            Dead_Command.Destroy_On_Exit := Command.Destroy_On_Exit;
            Dead_Command.Is_Dead := True;

            for J in Instances'Range loop
               if Instances (J) /= No_Class_Instance then
                  Found := True;
                  Set_Data
                    (Instances (J), Command_Class_Name, Command_Property'
                       (Command => Dead_Command));
               end if;
            end loop;

            --  If the command is not referenced by any script, then we just
            --  remove it.

            if not Found then
               Unref (Command_Access (Dead_Command));
            end if;

            Free (Command.Instances);
         end;
      end if;
   end Free;

   ----------
   -- Undo --
   ----------

   overriding function Undo (This : access Scheduled_Command) return Boolean is
   begin
      return Undo (This.Command);
   end Undo;

   --------------------
   -- Create_Wrapper --
   --------------------

   function Create_Wrapper
     (Command : access Root_Command'Class; Destroy_On_Exit : Boolean)
      return Scheduled_Command_Access
   is
      C     : constant Scheduled_Command_Access := new Scheduled_Command;
   begin
      C.Command := Command_Access (Command);
      C.Destroy_On_Exit := Destroy_On_Exit;

      return C;
   end Create_Wrapper;

   -------------------------------
   -- Launch_Background_Command --
   -------------------------------

   procedure Launch_Background_Command
     (Kernel          : access Kernel_Handle_Record'Class;
      Command         : access Root_Command'Class;
      Active          : Boolean;
      Show_Bar        : Boolean;
      Queue_Id        : String := "";
      Destroy_On_Exit : Boolean := True;
      Block_Exit      : Boolean := True)
   is
      Wrapper : constant Scheduled_Command_Access := Launch_Background_Command
        (Kernel          => Kernel,
         Command         => Command,
         Active          => Active,
         Show_Bar        => Show_Bar,
         Queue_Id        => Queue_Id,
         Destroy_On_Exit => Destroy_On_Exit,
         Block_Exit      => Block_Exit);
      pragma Unreferenced (Wrapper);
   begin
      null;
   end Launch_Background_Command;

   -------------------------------
   -- Launch_Background_Command --
   -------------------------------

   function Launch_Background_Command
     (Kernel          : access Kernel_Handle_Record'Class;
      Command         : access Root_Command'Class;
      Active          : Boolean;
      Show_Bar        : Boolean;
      Queue_Id        : String := "";
      Destroy_On_Exit : Boolean := True;
      Block_Exit      : Boolean := True) return Scheduled_Command_Access
   is
      Manager : constant Task_Manager_Access := Get_Task_Manager (Kernel);
      Wrapper : constant Scheduled_Command_Access :=
                  Create_Wrapper (Command, Destroy_On_Exit);
   begin
      Add_Command
        (Manager,
         Command_Access (Wrapper),
         Active, Show_Bar, Queue_Id, Block_Exit);

      return Wrapper;
   end Launch_Background_Command;

   -------------------------------
   -- Launch_Foreground_Command --
   -------------------------------

   procedure Launch_Foreground_Command
     (Kernel          : access Kernel_Handle_Record'Class;
      Command         : access Root_Command'Class;
      Destroy_On_Exit : Boolean := True)
   is
      Wrapper : Scheduled_Command_Access :=
                  Create_Wrapper (Command, Destroy_On_Exit);
      Result  : Command_Return_Type;
   begin
      Push_State (Kernel, Busy);
      loop
         begin
            Result := Execute (Command);
         exception
            when E : others =>
               Trace (Exception_Handle, E);
               Result := Failure;
         end;

         exit when Result = Success or Result = Failure;
      end loop;

      Pop_State (Kernel);
      Unref (Command_Access (Wrapper));
   end Launch_Foreground_Command;

   ---------------------------
   -- Interrupt_Latest_Task --
   ---------------------------

   procedure Interrupt_Latest_Task
     (Kernel : access Kernel_Handle_Record'Class) is
   begin
      Interrupt_Latest_Task (Get_Task_Manager (Kernel));
   end Interrupt_Latest_Task;

   ---------------------
   -- Interrupt_Queue --
   ---------------------

   procedure Interrupt_Queue
     (Kernel  : access Kernel_Handle_Record'Class;
      Command : Command_Access)
   is
      Manager : constant Task_Manager_Access := Get_Task_Manager (Kernel);
   begin
      Interrupt_Queue (Manager, Command);
   end Interrupt_Queue;

   ---------------
   -- Has_Queue --
   ---------------

   function Has_Queue
     (Kernel   : access Kernel_Handle_Record'Class;
      Queue_Id : String) return Boolean
   is
      Manager : constant Task_Manager_Access := Get_Task_Manager (Kernel);
   begin
      return Has_Queue (Manager, Queue_Id);
   end Has_Queue;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Tools                     : constant String :=
                                    "/" & (-"Tools") & '/' & (-"Views");
      Push_Command, Pop_Command : Custom_Command_Access;
      Script                    : Scripting_Language;

   begin
      Task_Manager_Module_Id := new Task_Manager_Module_Id_Record;
      Register_Module
        (Module       => Module_ID (Task_Manager_Module_Id),
         Kernel       => Kernel,
         Module_Name  => Task_Manager_Module_Name,
         Priority     => Default_Priority);
      Register_Desktop_Functions (Save_Desktop'Access, Load_Desktop'Access);

      Set_Progress_Area
        (Get_Task_Manager (Kernel),
         GPS_Window (Get_Main_Window (Kernel)).Statusbar);

      Script := Lookup_Scripting_Language
        (Get_Scripts (Kernel), GPS_Shell_Name);
      Create
        (Push_Command, "set_busy", Kernel_Handle (Kernel), "set_busy", Script);
      Create
        (Pop_Command, "unset_busy",
         Kernel_Handle (Kernel), "unset_busy", Script);

      Set_Busy_Commands
        (Get_Task_Manager (Kernel),
         Command_Access (Push_Command),
         Command_Access (Pop_Command));

      Register_Menu
        (Kernel,
         Tools,
         -"Tas_ks",
         Callback => On_Task_Manager'Access);

      Add_Hook
        (Kernel, Before_Exit_Action_Hook,
         Wrapper (On_Exit_Hook'Access),
         Name => "task_manager.on_exit");

      Standard.Task_Manager.Shell.Register_Commands (Kernel);
   end Register_Module;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy
     (Module : in out Task_Manager_Module_Id_Record) is
   begin
      Destroy (Get_Task_Manager (Get_Kernel (Module)));
   end Destroy;

   ------------------
   -- Get_Instance --
   ------------------

   function Get_Instance
     (Command       : access Scheduled_Command'Class;
      Language      : access Scripting_Language_Record'Class;
      Command_Class : Class_Type := No_Class)
      return Class_Instance
   is
      Inst : Class_Instance := Get (Command.Instances, Language);
   begin
      if Inst = No_Class_Instance then

         if Command_Class = No_Class then
            declare
               Root_Command_Class : constant Class_Type :=
                                      New_Class
                                        (Get_Kernel (Language),
                                         Command_Class_Name);
            begin
               Inst := New_Instance (Language, Root_Command_Class);
            end;
         else
            Inst := New_Instance (Language, Command_Class);
         end if;

         Set_Instance (Command, Language, Inst);
      end if;

      return Inst;
   end Get_Instance;

   ------------------
   -- Set_Instance --
   ------------------

   procedure Set_Instance
     (Command  : access Scheduled_Command'Class;
      Language : access Scripting_Language_Record'Class;
      Instance : Class_Instance) is
   begin
      Set_Data
        (Instance, Command_Class_Name, Command_Property'
           (Command => Scheduled_Command_Access (Command)));
      Ref (Command_Access (Command));  --  unrefed in GPS.Command.__del__
      Set (Command.Instances, Language, Instance);
   end Set_Instance;

   ---------------------
   -- Remove_Instance --
   ---------------------

   procedure Remove_Instance
     (Command  : access Scheduled_Command'Class;
      Language : access Scripting_Language_Record'Class) is
   begin
      Set (Command.Instances, Language, No_Class_Instance);
   end Remove_Instance;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (Instance : Class_Instance) return Scheduled_Command_Access
   is
      Cmd : Command_Property_Access;
   begin
      Cmd := Command_Property_Access
        (Instance_Property'(Get_Data (Instance, Command_Class_Name)));
      if Cmd = null then
         return null;
      else
         return Cmd.Command;
      end if;
   exception
      when Invalid_Data =>
         return null;
   end Get_Data;

end GPS.Kernel.Task_Manager;
