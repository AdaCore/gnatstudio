------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2022, AdaCore                       --
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

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with GNATCOLL.Any_Types;           use GNATCOLL.Any_Types;
with GNATCOLL.Projects;            use GNATCOLL.Projects;
with GNATCOLL.Traces;              use GNATCOLL.Traces;

with Commands;                     use Commands;
with Commands.Interactive;         use Commands.Interactive;
with GPS.Kernel.Actions;
with GPS.Kernel.Hooks;             use GPS.Kernel.Hooks;
with GPS.Kernel.Modules;           use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;
with GPS.Kernel.Project;
with GUI_Utils;
with Remote;

with DAP.Clients;
with DAP.Breakpoints;

package body DAP.Module is

   Me : constant Trace_Handle := Create ("GPS.DEBUGGING.DAP_MODULE", Off);

   package DAP_Client_Vectors is new Ada.Containers.Vectors
     (Positive, DAP.Clients.DAP_Client_Access, "=" => DAP.Clients."=");
   subtype DAP_Client_Vector is DAP_Client_Vectors.Vector;

   procedure Free is new Ada.Unchecked_Deallocation
     (DAP.Clients.DAP_Client'Class, DAP.Clients.DAP_Client_Access);

   type DAP_Module_Record is new Module_ID_Record with record
      Dynamic_Actions : Action_Lists.List;
      --  Actions that have been registered dynamically by this module,
      --  for the dynamic menus

      Clients            : DAP_Client_Vector;
      --  Clients that handles DAP requests

      Current_Debuger_ID : Integer := 0;

      Client_ID          : Positive := 1;
   end record;

   overriding procedure Destroy (Id : in out DAP_Module_Record);

   type DAP_Module is access all DAP_Module_Record'Class;

   -- Hooks callbacks --

   type On_Project_View_Changed is new Simple_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Project_View_Changed;
      Kernel : not null access Kernel_Handle_Record'Class);
   --  Called every time the project view changes, to recompute the dynamic
   --  menus.

   -- Filters --

   type Has_Debuggers_Filter is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Has_Debuggers_Filter;
      Context : Selection_Context) return Boolean;

   -- Commands --

   type Initialize_Debugger_Command is new Interactive_Command with record
      Project : Project_Type;
      Exec    : Virtual_File;
   end record;
   overriding function Execute
     (Command : access Initialize_Debugger_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Debug->Initialize

   type Terminate_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Terminate_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Debug->Terminate Current

   type Terminate_All_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Terminate_All_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Debug->Terminate

   -- Utils --

   procedure Debug_Init
     (Kernel  : GPS.Kernel.Kernel_Handle;
      Project : Project_Type;
      File    : GNATCOLL.VFS.Virtual_File;
      Args    : String);
   --  Initialize the debugger

   function To_File
     (Kernel  : not null access Kernel_Handle_Record'Class;
      Name    : String)
      return GNATCOLL.VFS.Virtual_File;

   DAP_Module_Name : constant String := "DAP";
   Debug_Adapter   : Ada.Strings.Unbounded.Unbounded_String;

   DAP_Module_ID   : DAP_Module;

   ----------------
   -- Debug_Init --
   ----------------

   procedure Debug_Init
     (Kernel  : GPS.Kernel.Kernel_Handle;
      Project : Project_Type;
      File    : GNATCOLL.VFS.Virtual_File;
      Args    : String)
   is
      Client : constant DAP.Clients.DAP_Client_Access :=
        new DAP.Clients.DAP_Client (Kernel, DAP_Module_ID.Client_ID);
   begin
      DAP_Module_ID.Current_Debuger_ID := DAP_Module_ID.Client_ID;

      if DAP_Module_ID.Client_ID < Positive'Last then
         DAP_Module_ID.Client_ID := DAP_Module_ID.Client_ID + 1;
      else
         DAP_Module_ID.Client_ID := 1;
      end if;

      DAP_Module_ID.Clients.Append (Client);
      Client.Start
        (Ada.Strings.Unbounded.To_String (Debug_Adapter),
         Project,
         File,
         Args);

      Kernel.Refresh_Context;
   end Debug_Init;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Id : in out DAP_Module_Record) is
   begin
      for D of Id.Clients loop
         begin
            D.Quit;
         exception
            when E : others =>
               Trace (Me, E);
         end;
      end loop;
   end Destroy;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Project_View_Changed;
      Kernel : not null access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Self);

      procedure Create_Action_And_Menu
        (Prj : Project_Type; Main : Virtual_File);
      --  Create the action and menu to initialize a specific executable

      Mains : Any_Type :=
        Compute_Build_Targets_Hook.Run (Kernel, "executable");

      Show_Project_In_Menu : constant Boolean :=
        GPS.Kernel.Modules.UI.Group_Mains_Into_Projects
          (Kernel, Mains.Length);

      ----------------------------
      -- Create_Action_And_Menu --
      ----------------------------

      procedure Create_Action_And_Menu
        (Prj : Project_Type; Main : Virtual_File)
      is
         Main_Name         : constant String :=
           (if Main = No_File
            then "no main file"
            else Main.Display_Base_Name);
         Escaped_Main_Name : constant String :=
           GUI_Utils.Escape_Underscore
             (GUI_Utils.Escape_Menu_Name (Main_Name));

         Action  : constant String :=
           "debug initialize " & Prj.Name & ":" & Main_Name;
         Menu    : constant String :=
           "/Debug/Initialize/"
           & (if not Show_Project_In_Menu or else Main = No_File
              then ""
              else GUI_Utils.Escape_Underscore (Prj.Name) & '/')
           & Escaped_Main_Name;
         Command : Interactive_Command_Access;
      begin
         Command := new Initialize_Debugger_Command'
           (Interactive_Command with
            Project => Prj,
            Exec    => Main);
         DAP_Module_ID.Dynamic_Actions.Append (Action);

         GPS.Kernel.Actions.Register_Action
           (Kernel, Action, Command,
            (if Main /= No_File
             then ("Initialize the debugger on the file "
               & Main.Display_Full_Name)
             else "Initialize the debugger, no file specified"),
            Category => "Debug");
         GPS.Kernel.Modules.UI.Register_Menu (Kernel, Menu, Action => Action);
      end Create_Action_And_Menu;

   begin
      for A of DAP_Module_ID.Dynamic_Actions loop
         GPS.Kernel.Actions.Unregister_Action
           (Kernel, A, Remove_Menus_And_Toolbars => True);
      end loop;
      DAP_Module_ID.Dynamic_Actions.Clear;

      for J in 1 .. Mains.Length loop
         if Mains.List (J).Length /= 0 then
            declare
               Main : constant Virtual_File :=
                  To_File (Kernel, Mains.List (J).Tuple (2).Str);
               Prj  : constant Virtual_File :=
                  To_File (Kernel, Mains.List (J).Tuple (3).Str);
               P    : constant Project_Type :=
                  Kernel.Registry.Tree.Project_From_Path (Prj);
            begin
               Create_Action_And_Menu (P, Main);
            end;
         end if;
      end loop;

      Free (Mains);

      --  Specific entry to start the debugger without any main program.
      --  We need to pass the root project so that Ide'debugger_command is
      --  found.

      Create_Action_And_Menu
        (GPS.Kernel.Project.Get_Project (Kernel), No_File);

   exception
      when E : others =>
         Trace (Me, E);
         --  Debug_Terminate (Kernel_Handle (Kernel));
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Initialize_Debugger_Command;
      Context : Interactive_Command_Context) return Command_Return_Type is
   begin
      Debug_Init
         (Get_Kernel (Context.Context), Command.Project, Command.Exec, "");

      return Success;
   exception
      when E : others =>
         Trace (Me, E);
         return Failure;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Terminate_Command;
      Context : Interactive_Command_Context) return Command_Return_Type is
   begin
      if DAP_Module_ID.Client_ID /= 0 then
         for D of DAP_Module_ID.Clients loop
            if D.Id = DAP_Module_ID.Client_ID then
               D.Quit;
               exit;
            end if;
         end loop;
      end if;

      return Success;
   exception
      when E : others =>
         Trace (Me, E);
         return Failure;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Terminate_All_Command;
      Context : Interactive_Command_Context) return Command_Return_Type is
   begin
      Terminate_Debuggers;
      return Success;
   exception
      when E : others =>
         Trace (Me, E);
         return Failure;
   end Execute;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Has_Debuggers_Filter;
      Context : Selection_Context) return Boolean is
   begin
      return not DAP_Module_ID.Clients.Is_Empty;
   end Filter_Matches_Primitive;

   -------------------------
   -- Initialize_Debugger --
   -------------------------

   procedure Initialize_Debugger
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Args   : String) is
   begin
      Debug_Init
        (GPS.Kernel.Kernel_Handle (Kernel),
         GPS.Kernel.Project.Get_Project (Kernel),
         No_File,
         Args);
   end Initialize_Debugger;

   --------------
   -- Finished --
   --------------

   procedure Finished (Id : Positive) is
      use DAP_Client_Vectors;
      use type DAP.Clients.DAP_Client_Access;

      C      : Cursor := DAP_Module_ID.Clients.First;
      Client : DAP.Clients.DAP_Client_Access := null;
   begin
      while Has_Element (C) loop
         if Element (C).Id = Id then
            Client := Element (C);
            exit;
         end if;
         Next (C);
      end loop;

      if Client /= null then
         DAP_Module_ID.Clients.Delete (C);
         Free (Client);

         if DAP_Module_ID.Client_ID = Id then
            DAP_Module_ID.Client_ID := DAP_Module_ID.Clients.First_Element.Id;
         end if;
      end if;
   end Finished;

   -------------------------
   -- Terminate_Debuggers --
   -------------------------

   procedure Terminate_Debuggers is
   begin
      DAP_Module_ID.Destroy;
   end Terminate_Debuggers;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel     : access GPS.Kernel.Kernel_Handle_Record'Class;
      Prefix_Dir : Virtual_File)
   is
      Debugger_Active           : Action_Filter;

   begin
      DAP_Module_ID := new DAP_Module_Record;
      if Kernel /= null then
         Register_Module
           (Module          => Module_ID (DAP_Module_ID),
            Kernel          => Kernel,
            Module_Name     => DAP_Module_Name,
            Priority        => Default_Priority + 20);
      end if;

      Debug_Adapter := Ada.Strings.Unbounded.To_Unbounded_String
        (+Prefix_Dir.Create_From_Dir
           ("share/gnatstudio/cdt-gdb-adapter/debugAdapter.js").Full_Name);

      Debugger_Active := new Has_Debuggers_Filter;
      Register_Filter (Kernel, Debugger_Active, "Has debuggers");

      Project_View_Changed_Hook.Add (new On_Project_View_Changed);

      GPS.Kernel.Modules.UI.Register_Contextual_Submenu
        (Kernel, "Debug",
         Group => GPS.Kernel.Modules.UI.Debug_Contextual_Group);

      GPS.Kernel.Actions.Register_Action
        (Kernel, "terminate debugger", new Terminate_Command,
         Icon_Name   => "gps-debugger-terminate-symbolic",
         Description => "Terminate the current debugger",
         Filter      => Debugger_Active);

      GPS.Kernel.Actions.Register_Action
        (Kernel, "terminate all debuggers", new Terminate_All_Command,
         Description => "Terminate all running debugger",
         Filter      => Debugger_Active);

      DAP.Breakpoints.Register_Module (Kernel);
   end Register_Module;

   -------------
   -- To_File --
   -------------

   function To_File
     (Kernel  : not null access Kernel_Handle_Record'Class;
      Name    : String)
     return GNATCOLL.VFS.Virtual_File
   is
      F : Virtual_File;
   begin
      --  Translate filename into local file if needed
      F := To_Local
        (Create (+Name, Remote.Get_Nickname (Remote.Debug_Server)));

      --  Convert from a patch returned by the debugger to the actual
      --  path in the project, in case sources have changed
      if not F.Is_Absolute_Path or else not F.Is_Regular_File then
         F := Kernel.Create_From_Base (F.Full_Name);
      end if;

      return F;
   end To_File;

end DAP.Module;
