-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                 Copyright (C) 2001-2010, AdaCore                  --
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

with Ada.Characters.Handling;    use Ada.Characters.Handling;
with GNAT.Expect;                use GNAT.Expect;
pragma Warnings (Off);
with GNAT.Expect.TTY;            use GNAT.Expect.TTY;
pragma Warnings (On);
with GNATCOLL.Projects;          use GNATCOLL.Projects;
with GNATCOLL.Scripts;           use GNATCOLL.Scripts;
with GNATCOLL.VFS;               use GNATCOLL.VFS;
with Glib;                       use Glib;
with Glib.Object;                use Glib.Object;
with Gdk.Types;                  use Gdk.Types;
with Gdk.Types.Keysyms;          use Gdk.Types.Keysyms;
with Gtk.Menu_Item;              use Gtk.Menu_Item;
with Gtk.Stock;                  use Gtk.Stock;
with Gtk.Widget;                 use Gtk.Widget;
with Gtkada.MDI;                 use Gtkada.MDI;

with GPS.Intl;                   use GPS.Intl;
with GPS.Kernel;                 use GPS.Kernel;
with GPS.Kernel.Commands;        use GPS.Kernel.Commands;
with GPS.Kernel.Hooks;           use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;             use GPS.Kernel.MDI;
with GPS.Kernel.Modules;         use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;      use GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences;     use GPS.Kernel.Preferences;
with GPS.Kernel.Project;         use GPS.Kernel.Project;
with GPS.Kernel.Task_Manager;    use GPS.Kernel.Task_Manager;
with GPS.Kernel.Scripts;         use GPS.Kernel.Scripts;

with Entities;                   use Entities;
with Entities.Queries;           use Entities.Queries;
with Build_Command_Manager;
with Builder_Facility_Module;
with Traces;                     use Traces;
with Commands;                   use Commands;

package body Builder_Module is

   Xrefs_Loading_Queue : constant String := "xrefs_loading";

   type Builder_Module_ID_Record is
     new GPS.Kernel.Modules.Module_ID_Record
   with record
      Build_Count : Natural := 0;
      --  Number of on-going builds
   end record;
   type Builder_Module_ID_Access is access all Builder_Module_ID_Record;
   --  Data stored with the module id

   Builder_Module_ID : Builder_Module_ID_Access;

   procedure Interrupt_Xrefs_Loading
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Interrupts all xrefs loading

   -------------------------
   -- Load_Xref_In_Memory --
   -------------------------

   type All_LI_Information_Command is new Root_Command with record
      Iter : Recursive_LI_Information_Iterator;
      Count, Total : Natural := 0;
      Chunk_Size   : Natural := 10;  --  ??? Should be configurable
   end record;

   overriding function Progress
     (Command : access All_LI_Information_Command) return Progress_Record;
   overriding function Execute
     (Command : access All_LI_Information_Command) return Command_Return_Type;
   overriding function Name
     (Command : access All_LI_Information_Command) return String;

   type C_LI_Information_Command is new All_LI_Information_Command
     with null record;

   overriding function Name
     (Command : access C_LI_Information_Command) return String;

   --------------------
   -- Menu Callbacks --
   --------------------

   procedure On_Compute_Xref
     (Object : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Build->Compute Xref information menu

   procedure On_Load_Xref_In_Memory
     (Object : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Build->Load Xref info

   procedure On_Compilation_Finished
     (Kernel : access Kernel_Handle_Record'Class;
      Data : access Hooks_Data'Class);
   --  Called when the compilation is finished

   function On_Compilation_Starting
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class) return Boolean;
   --  Called when the compilation is starting

   procedure Load_Xref_In_Memory (Kernel : access Kernel_Handle_Record'Class);
   --  Load the Xref info in memory, in a background task

   function C_Filter (Lang : String) return Boolean;
   --  Return true if Lang is C or C++ (case insensitive)

   procedure Load_C_Xref_In_Memory
     (Kernel : access Kernel_Handle_Record'Class);
   --  Load the C/C++ Xref info in memory, in a background task

   procedure On_Tools_Interrupt
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Tools->Interrupt menu

   procedure On_View_Changed (Kernel : access Kernel_Handle_Record'Class);
   --  Called every time the project view has changed, ie potentially the list
   --  of main units.

   procedure On_Project_Changed (Kernel : access Kernel_Handle_Record'Class);
   --  Called every time a new project is loaded

   procedure Compile_Command
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Command handler for the "compile" command

   ---------------------
   -- Compile_Command --
   ---------------------

   procedure Compile_Command
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
   begin
      if Command = "compute_xref" then
         Build_Command_Manager.Launch_Target
           (Kernel,
            Builder_Facility_Module.Registry,
            "Build All", "xref",
            GNATCOLL.VFS.No_File,
            Extra_Args  => null,
            Quiet       => True,
            Synchronous => True,
            Dialog      => Build_Command_Manager.Force_No_Dialog,
            Background  => False,
            Main        => No_File);

      elsif Command = "compute_xref_bg" then
         Build_Command_Manager.Launch_Target
           (Kernel,
            Builder_Facility_Module.Registry,
            "Build All", "xref",
            GNATCOLL.VFS.No_File,
            Extra_Args  => null,
            Quiet       => True,
            Synchronous => False,
            Background  => False,
            Dialog      => Build_Command_Manager.Force_No_Dialog,
            Main        => No_File);
      end if;
   end Compile_Command;

   ---------------------
   -- On_Compute_Xref --
   ---------------------

   procedure On_Compute_Xref
     (Object : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Object);
   begin
      Build_Command_Manager.Launch_Target
        (Kernel,
         Builder_Facility_Module.Registry,
         "Build All", "xref",
         GNATCOLL.VFS.No_File,
         Extra_Args  => null,
         Quiet       => False,
         Synchronous => False,
         Background  => False,
         Dialog      => Build_Command_Manager.Force_No_Dialog,
         Main        => No_File);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Compute_Xref;

   ----------------------------
   -- On_Load_Xref_In_Memory --
   ----------------------------

   procedure On_Load_Xref_In_Memory
     (Object : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Object);
   begin
      Load_Xref_In_Memory (Kernel);
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Load_Xref_In_Memory;

   -----------------------------
   -- On_Compilation_Starting --
   -----------------------------

   function On_Compilation_Starting
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class) return Boolean
   is
      pragma Unreferenced (Data);
   begin
      Interrupt_Xrefs_Loading (Kernel);
      Builder_Module_ID.Build_Count := Builder_Module_ID.Build_Count + 1;

      return True;
   end On_Compilation_Starting;

   -----------------------------
   -- On_Compilation_Finished --
   -----------------------------

   procedure On_Compilation_Finished
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      pragma Unreferenced (Data);
   begin
      if Builder_Module_ID.Build_Count > 0 then
         Builder_Module_ID.Build_Count := Builder_Module_ID.Build_Count - 1;
      end if;

      if Builder_Module_ID.Build_Count = 0 then
         if Automatic_Xrefs_Load.Get_Pref then
            Load_Xref_In_Memory (Kernel);
         else
            --  We need to load all C/C++ xref info so that source navigation
            --  works properly
            Load_C_Xref_In_Memory (Kernel);
         end if;
      end if;
   end On_Compilation_Finished;

   -----------------------------
   -- Interrupt_Xrefs_Loading --
   -----------------------------

   procedure Interrupt_Xrefs_Loading
     (Kernel : access Kernel_Handle_Record'Class) is
   begin
      Kill_File_Iteration (Kernel, Xrefs_Loading_Queue);
   end Interrupt_Xrefs_Loading;

   ----------
   -- Name --
   ----------

   overriding function Name
     (Command : access All_LI_Information_Command) return String is
      pragma Unreferenced (Command);
   begin
      return -"load xref info";
   end Name;

   overriding function Name
     (Command : access C_LI_Information_Command) return String is
      pragma Unreferenced (Command);
   begin
      return -"load C/C++ xref info";
   end Name;

   --------------
   -- Progress --
   --------------

   overriding function Progress
     (Command : access All_LI_Information_Command) return Progress_Record is
   begin
      return Progress_Record'
        (Activity => Running,
         Current  => Command.Count,
         Total    => Command.Total);
   end Progress;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access All_LI_Information_Command) return Command_Return_Type
   is
   begin
      Next (Command.Iter, Steps => Command.Chunk_Size,
            Count => Command.Count, Total => Command.Total);

      if Command.Count >= Command.Total then
         Free (Command.Iter);
         return Success;
      else
         return Execute_Again;
      end if;
   end Execute;

   --------------
   -- C_Filter --
   --------------

   function C_Filter (Lang : String) return Boolean is
      Str : constant String := To_Lower (Lang);
   begin
      return Str = "c" or else Str = "c++";
   end C_Filter;

   ---------------------------
   -- Load_C_Xref_In_Memory --
   ---------------------------

   procedure Load_C_Xref_In_Memory
     (Kernel : access Kernel_Handle_Record'Class)
   is
      C : constant Command_Access := new C_LI_Information_Command;
   begin
      Start (C_LI_Information_Command (C.all).Iter,
             Get_Language_Handler (Kernel),
             Get_Project (Kernel).Start (Recursive => True),
             C_Filter'Access);
      Launch_Background_Command
        (Kernel,
         C,
         Active     => True,
         Show_Bar   => True,
         Queue_Id   => "load C/C++ xrefs info",
         Block_Exit => False);
   end Load_C_Xref_In_Memory;

   -------------------------
   -- Load_Xref_In_Memory --
   -------------------------

   procedure Load_Xref_In_Memory
     (Kernel : access Kernel_Handle_Record'Class)
   is
      C : constant Command_Access := new All_LI_Information_Command;
   begin
      Start (All_LI_Information_Command (C.all).Iter,
             Get_Language_Handler (Kernel),
             Get_Project (Kernel).Start (Recursive => True));
      Launch_Background_Command
        (Kernel,
         C,
         Active     => True,
         Show_Bar   => True,
         Queue_Id   => "load xrefs info",
         Block_Exit => False);
   end Load_Xref_In_Memory;

   ------------------------
   -- On_Tools_Interrupt --
   ------------------------

   procedure On_Tools_Interrupt
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Child : constant MDI_Child := Get_Focus_Child (Get_MDI (Kernel));
   begin
      --  Check whether the current MDI child can handle interrupt on its own

      if Child = null
        or else Child.all not in GPS_MDI_Child_Record'Class
        or else not Interrupt (GPS_MDI_Child (Child))
      then
         --  Else default is to kill the last process we started
         Interrupt_Latest_Task (Kernel);
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Tools_Interrupt;

   ---------------------
   -- On_View_Changed --
   ---------------------

   procedure On_View_Changed (Kernel : access Kernel_Handle_Record'Class) is
   begin
      if Automatic_Xrefs_Load.Get_Pref then
         Load_Xref_In_Memory (Kernel_Handle (Kernel));
      else
         --  We need to load all C/C++ xref info so that source navigation
         --  works properly
         Load_C_Xref_In_Memory (Kernel_Handle (Kernel));
      end if;
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_View_Changed;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Build_Menu : constant String := '/' & (-"_Build") & '/';
      Tools : constant String := '/' & (-"Tools") & '/';
      Mitem : Gtk_Menu_Item;
   begin
      --  This memory is allocated once, and lives as long as the application

      Builder_Module_ID := new Builder_Module_ID_Record;
      Register_Module
        (Module      => Builder_Module_ID,
         Kernel      => Kernel,
         Module_Name => "Builder",
         Priority    => Default_Priority);

      Gtk_New (Mitem);
      Register_Menu (Kernel, Build_Menu, Mitem, Ref_Item => -"Settings");

      Register_Menu
        (Kernel, Build_Menu, -"Recompute _Xref info", "",
         On_Compute_Xref'Access, Ref_Item => -"Settings");
      Register_Menu
        (Kernel, Build_Menu, -"Load Xref info in memory", "",
         On_Load_Xref_In_Memory'Access, Ref_Item => -"Settings");

      Gtk_New (Mitem);
      Register_Menu (Kernel, Build_Menu, Mitem, Ref_Item => -"Settings");

      Gtk_New (Mitem);
      Register_Menu (Kernel, Tools, Mitem);
      Register_Menu
        (Kernel, Tools, -"_Interrupt", Stock_Stop, On_Tools_Interrupt'Access,
         null, GDK_C, Control_Mask + Shift_Mask);

      Add_Hook
        (Kernel => Kernel,
         Hook   => Project_View_Changed_Hook,
         Func   => Wrapper (On_View_Changed'Access),
         Name   => "builder_module.on_view_changed");
      Add_Hook
        (Kernel => Kernel,
         Hook   => Compilation_Finished_Hook,
         Func   => Wrapper (On_Compilation_Finished'Access),
         Name   => "load_xrefs");
      Add_Hook
        (Kernel => Kernel,
         Hook   => Compilation_Starting_Hook,
         Func   => Wrapper (On_Compilation_Starting'Access),
         Name   => "stop_load_xrefs");
      Add_Hook
        (Kernel => Kernel,
         Hook   => Project_Changed_Hook,
         Func   => Wrapper (On_Project_Changed'Access),
         Name   => "interrupt_xrefs_loading");

      Register_Command
        (Kernel, "compute_xref",
         Handler => Compile_Command'Access);
   end Register_Module;

   ------------------------
   -- On_Project_Changed --
   ------------------------

   procedure On_Project_Changed (Kernel : access Kernel_Handle_Record'Class) is
   begin
      Interrupt_Xrefs_Loading (Kernel);
   end On_Project_Changed;

end Builder_Module;
