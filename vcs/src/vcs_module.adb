------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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

with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with GNATCOLL.Scripts;          use GNATCOLL.Scripts;

with XML_Utils;                 use XML_Utils;

with Gtkada.MDI;                use Gtkada.MDI;
with Gtk.Menu;

with Log_Utils;

with Commands.Interactive;      use Commands, Commands.Interactive;
with GPS.Core_Kernels;          use GPS.Core_Kernels;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Actions;        use GPS.Kernel.Actions;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.Modules.UI;     use GPS.Kernel.Modules.UI;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with GPS.VCS;                   use GPS.VCS;

with VCS.Generic_VCS;
with VCS.Unknown_VCS;           use VCS.Unknown_VCS;
with VCS_Activities;            use VCS_Activities;
with VCS_Activities_View_API;   use VCS_Activities_View_API;
with VCS_Utils;                 use VCS_Utils;
with VCS_View;                  use VCS_View;
with VCS_View_API;              use VCS_View_API;
with VCS2.Engines;              use VCS2.Engines;

with VCS_Module.Actions;        use VCS_Module.Actions;

package body VCS_Module is

   type VCS_Contextual_Menu is new Submenu_Factory_Record with null record;
   overriding procedure Append_To_Menu
     (Factory : access VCS_Contextual_Menu;
      Context : Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  Fill Menu with the contextual menu for the VCS module,
   --  if Context is appropriate.

   type Explorer_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Explorer_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Display the VCS explorer

   type Activities_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Activities_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Display the VCS Activities explorer

   type On_File_Edited is new File_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_File_Edited;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File);
   --  Callback for the "file_edited" signal

   type On_File_Closed is new File_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_File_Closed;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File);
   --  Callback for the "file_closed" signal

   type On_File_Status_Changed is new File_Status_Hooks_Function
      with null record;
   overriding procedure Execute
     (Self   : On_File_Status_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Status : GPS.Kernel.File_Status);
   --  Callback for the file status changed hook

   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child;
   --  Restore the status of the explorer from a saved XML tree

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      User   : Kernel_Handle) return Node_Ptr;
   --  Save the status of the project explorer to an XML tree

   procedure Status_Parse_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Handler for the command "VCS.status_parse"

   procedure Update_Parse_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Handler for the command "VCS.update_parse"

   procedure Annotations_Parse_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Handler for the command "VCS.annotations_parse"

   procedure Log_Parse_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Handler for the command "VCS.log_parse"

   procedure Revision_Parse_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Handler for the command "VCS.revision_parse"

   procedure VCS_Command_Handler_No_Param
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Handler for VCS commands that take no parameter

   procedure VCS_Activities_Class_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Handler for VCS Activities commands that take no parameter

   type On_Project_Changing is new File_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Project_Changing;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File);
   --  Called when project is about to change

   type On_Project_Changed is new Simple_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Project_Changed;
      Kernel : not null access Kernel_Handle_Record'Class);
   --  Called when project has been changed and is fully loaded

   ---------------
   -- Equiv_VCS --
   ---------------

   function Equiv_VCS (Left, Right : String) return Boolean is
   begin
      return To_Lower (Left) = To_Lower (Right);
   end Equiv_VCS;

   -------------------
   -- For_Every_VCS --
   -------------------

   procedure For_Every_VCS
     (Process : not null access procedure (VCS : VCS_Access))
   is
      procedure Action (Position : VCS_Map.Cursor);
      --  Called for every registered action in the map

      ------------
      -- Action --
      ------------

      procedure Action (Position : VCS_Map.Cursor) is
      begin
         Process (VCS_Map.Element (Position));
      end Action;

   begin
      VCS_Module_ID.Registered_VCS.Iterate (Action'Access);
   end For_Every_VCS;

   ---------------------
   -- Get_VCS_From_Id --
   ---------------------

   function Get_VCS_From_Id (Id : String) return VCS_Access is
      Pos : constant VCS_Map.Cursor := VCS_Module_ID.Registered_VCS.Find (Id);
   begin
      if VCS_Map.Has_Element (Pos) then
         return VCS_Map.Element (Pos);
      else
         return Unknown_VCS_Reference;
      end if;
   end Get_VCS_From_Id;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Explorer_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
   begin
      Open_Explorer (Kernel, Context.Context);
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Activities_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
   begin
      Open_Activities_Explorer (Kernel, Context.Context);
      return Commands.Success;
   end Execute;

   --------------------
   -- Append_To_Menu --
   --------------------

   overriding procedure Append_To_Menu
     (Factory : access VCS_Contextual_Menu;
      Context : Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      pragma Unreferenced (Factory);
      Creator : constant Abstract_Module := Get_Creator (Context);
   begin
      if (Creator /= Abstract_Module (VCS_Module_ID)
          and then Creator /= Abstract_Module (VCS_Explorer_Module_Id))
        or else Has_Activity_Information (Context)
      then
         VCS_View_API.VCS_Explorer_Contextual_Menu (Context, Menu, False);
      end if;
   end Append_To_Menu;

   ------------------
   -- Register_VCS --
   ------------------

   procedure Register_VCS (Id : String; Handle : VCS_Access) is
   begin
      VCS_Module_ID.Registered_VCS.Include (Id, Handle);
   end Register_VCS;

   ----------------------------------
   -- VCS_Command_Handler_No_Param --
   ----------------------------------

   procedure VCS_Command_Handler_No_Param
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Unknown_VCS_Name : constant String := Name (Unknown_VCS_Reference);

   begin
      if Command = "supported_systems" then
         declare
            Choices : Unbounded_String := To_Unbounded_String ("Auto");

            procedure Add_VCS (Position : VCS_Map.Cursor);
            --  Add a VCS into the returned data

            -------------
            -- Add_VCS --
            -------------

            procedure Add_VCS (Position : VCS_Map.Cursor) is
               Name : constant String := VCS_Map.Key (Position);
            begin
               --  Filter out the Unknown VCS
               if Name /= Unknown_VCS_Name then
                  Choices := Choices & ASCII.LF & Name;
               end if;
            end Add_VCS;

         begin
            VCS_Module_ID.Registered_VCS.Iterate (Add_VCS'Access);
            Data.Set_Return_Value (To_String (Choices));
         end;

      elsif Command = "get_current_vcs" then
         declare
            Kernel : constant Kernel_Handle := Get_Kernel (Data);
            Ref    : constant VCS_Access :=
                       Get_Current_Ref (Kernel, Get_Project (Kernel));
         begin
            if Ref /= null
              and then Ref /= Unknown_VCS_Reference
            then
               Set_Return_Value (Data, Name (Ref));
            else
               Set_Return_Value (Data, Default_VCS.Get_Pref);
            end if;
         end;
      end if;
   end VCS_Command_Handler_No_Param;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Module : in out VCS_Module_ID_Record) is

      procedure Free (VCS : VCS_Map.Cursor);
      --  Free VCS Access

      ----------
      -- Free --
      ----------

      procedure Free (VCS : VCS_Map.Cursor) is
         procedure Unchecked_Free is new Ada.Unchecked_Deallocation
           (VCS_Record'Class, VCS_Access);
         V : VCS_Access := VCS_Map.Element (VCS);
      begin
         Free (V.all);
         Unchecked_Free (V);
         VCS_Module_ID.Registered_VCS.Replace_Element (VCS, null);
      end Free;

   begin
      if Module.Explorer /= null then
         Unref (Module.Explorer);
         Module.Explorer := null;
      end if;

      VCS_Module_ID.Registered_VCS.Iterate (Free'Access);
      VCS_Module_ID.Registered_VCS.Clear;
      Clear_Cache (Module.Cached_Status, Free_Memory => True);
   end Destroy;

   ------------------
   -- Load_Desktop --
   ------------------

   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child
   is
      pragma Unreferenced (MDI);
      M        : constant VCS_Module_ID_Access := VCS_Module_ID;
      Explorer : VCS_Explorer_View_Access;
   begin
      if Node.Tag.all = "VCS_View_Record" then
         --  First we want to clear the current content of the VCS Explorer
         Explorer := Get_Explorer (User, True, True);
         Clear (Explorer);
         Open_Explorer (User, No_Context);
         return MDI_Child (M.Explorer_Child);

      elsif Node.Tag.all = "VCS_Activities_View_Record" then
         Open_Activities_Explorer (User, No_Context);
         return MDI_Child (M.Activities_Child);
      end if;

      return null;
   end Load_Desktop;

   ------------------
   -- Save_Desktop --
   ------------------

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      User   : Kernel_Handle) return Node_Ptr
   is
      pragma Unreferenced (User);
      N : Node_Ptr;
   begin
      if Widget.all in VCS_Explorer_View_Record'Class then
         N := new Node;
         N.Tag := new String'("VCS_View_Record");
         return N;

      elsif Widget.all in VCS_Activities_View_Record'Class then
         N := new Node;
         N.Tag := new String'("VCS_Activities_View_Record");
         return N;
      end if;

      return null;
   end Save_Desktop;

   -------------------
   -- Build_Context --
   -------------------

   overriding function Build_Context
     (Self  : not null access Activities_Child_Record;
      Event : Gdk.Event.Gdk_Event := null)
      return Selection_Context is
   begin
      return Build_View_Context (Self, Event);
   end Build_Context;

   -------------------
   -- Build_Context --
   -------------------

   overriding function Build_Context
     (Self  : not null access Explorer_Child_Record;
      Event : Gdk.Event.Gdk_Event := null)
      return Selection_Context is
   begin
      return Build_View_Context (Self, Event);
   end Build_Context;

   ------------------------------------------
   -- VCS_Activities_Class_Command_Handler --
   ------------------------------------------

   procedure VCS_Activities_Class_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Kernel               : constant Kernel_Handle := Get_Kernel (Data);
      VCS_Activities_Class : constant Class_Type :=
                               New_Class (Kernel, "Activities");
      Inst                 : Class_Instance;
   begin
      if Command = Constructor_Method then
         declare
            A : constant Activity_Id := New_Activity (Kernel);
         begin
            Set_Name (Kernel, A, Nth_Arg (Data, 2, ""));
            Inst := Nth_Arg (Data, 1, VCS_Activities_Class);
            Set_Data (Inst, VCS_Activities_Class, Image (A));
            Set_Instance (A, Inst);
         end;

      elsif Command = "get" then
         declare
            A : constant Activity_Id := Value (Nth_Arg (Data, 1, ""));
         begin
            Inst := No_Class_Instance;

            if A /= No_Activity then
               --  Get class instance

               Inst := Get_Instance (A);

               if Inst = No_Class_Instance then
                  Inst := New_Instance
                    (Get_Script (Data), VCS_Activities_Class);
                  Set_Data (Inst, VCS_Activities_Class, Image (A));
                  Set_Instance (A, Inst);
               end if;
            end if;

            Set_Return_Value (Data, Inst);
         end;

         Refresh (Get_Activities_Explorer (Kernel));

      elsif Command = "list" then
         declare
            A : Activity_Id;
         begin
            Set_Return_Value_As_List (Data);

            A := First;
            while A /= No_Activity loop
               if Get_Registry (Kernel).Tree.Root_Project.Project_Path =
                  Get_Project_Path (A)
               then
                  Set_Return_Value (Data, Image (A));
               end if;
               A := Next;
            end loop;
         end;

      elsif Command = "from_file" then
         declare
            A : constant Activity_Id := Get_File_Activity (Nth_Arg (Data, 1));
         begin
            if A = No_Activity then
               Inst := No_Class_Instance;

            else
               Inst := Get_Instance (A);

               if Inst = No_Class_Instance then
                  Inst := New_Instance
                    (Get_Script (Data), VCS_Activities_Class);
                  Set_Data (Inst, VCS_Activities_Class, Image (A));
                  Set_Instance (A, Inst);
               end if;
            end if;

            Set_Return_Value (Data, Inst);
         end;
      end if;
   end VCS_Activities_Class_Command_Handler;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      VCS_Class            : constant Class_Type := New_Class (Kernel, "VCS");
      VCS_Activities_Class : constant Class_Type :=
                               New_Class (Kernel, "Activities");
      V : constant access VCS_Repository := new VCS_Repository'
         (Abstract_VCS_Repository with Kernel => Kernel);
   begin
      VCS_Module_ID := new VCS_Module_ID_Record;
      VCS_Explorer_Module_Id := new VCS_Explorer_Module_ID_Record;

      Kernel.Set_VCS (V);

      Register_Module
        (Module      => Module_ID (VCS_Module_ID),
         Kernel      => Kernel,
         Module_Name => VCS_Module_Name,
         Priority    => Default_Priority);

      Register_Module
        (Module      => VCS_Explorer_Module_Id,
         Kernel      => Kernel,
         Module_Name => "VCS_Explorer",
         Priority    => Default_Priority);

      -- VCS --

      Implicit_Status := Kernel.Get_Preferences.Create
        (Name    => "VCS-Implicit-Status",
         Default => True,
         Doc     =>
           -("Requires explicit status checks commands. These might be too"
           & " costly to run systematically as part of other commands."),
         Label   => -"Implicit status",
         Path    => -"VCS:General");

      Default_VCS := Kernel.Get_Preferences.Create
        (Name    => "Default-VCS",
         Default => "Auto",
         Path    => -"VCS:General",
         Doc     => -"Default VCS to use when none is defined in the project.",
         Label   => -"Default VCS");

      Load_Activities (Kernel);

      Register_Desktop_Functions (Save_Desktop'Access, Load_Desktop'Access);

      Log_Utils.Initialize (Kernel);

      Standard.VCS.Unknown_VCS.Register_Module (Kernel);
      Standard.VCS.Generic_VCS.Register_Module (Kernel);

      File_Edited_Hook.Add (new On_File_Edited);
      File_Status_Changed_Hook.Add (new On_File_Status_Changed);
      File_Closed_Hook.Add (new On_File_Closed);
      Project_Changing_Hook.Add (new On_Project_Changing);
      Project_View_Changed_Hook.Add (new On_Project_Changed);

      Load_Cache (Kernel, VCS_Module_ID.Cached_Status);

      --  Register VCS commands

      Kernel.Scripts.Register_Command
        ("supported_systems",
         Class         => VCS_Class,
         Static_Method => True,
         Handler       => VCS_Command_Handler_No_Param'Access);
      Kernel.Scripts.Register_Command
        ("get_current_vcs",
         Class         => VCS_Class,
         Static_Method => True,
         Handler       => VCS_Command_Handler_No_Param'Access);

      Kernel.Scripts.Register_Command
        ("get_status",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => VCS_Class,
         Static_Method => True,
         Handler       => VCS_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("update",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => VCS_Class,
         Static_Method => True,
         Handler       => VCS_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("commit",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => VCS_Class,
         Static_Method => True,
         Handler       => VCS_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("diff_head",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => VCS_Class,
         Static_Method => True,
         Handler       => VCS_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("diff_working",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => VCS_Class,
         Static_Method => True,
         Handler       => VCS_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("annotate",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => VCS_Class,
         Static_Method => True,
         Handler       => VCS_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("remove_annotations",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => VCS_Class,
         Static_Method => True,
         Handler       => VCS_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("log",
         Minimum_Args  => 1,
         Maximum_Args  => 2,
         Class         => VCS_Class,
         Static_Method => True,
         Handler       => VCS_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("repository_path",
         Minimum_Args  => 1,
         Maximum_Args  => 2,
         Class         => VCS_Class,
         Static_Method => True,
         Handler       => VCS_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("repository_dir",
         Minimum_Args  => 0,
         Maximum_Args  => 1,
         Class         => VCS_Class,
         Static_Method => True,
         Handler       => VCS_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("set_reference",
         Minimum_Args  => 2,
         Maximum_Args  => 2,
         Class         => VCS_Class,
         Static_Method => True,
         Handler       => VCS_Command_Handler'Access);

      Kernel.Scripts.Register_Command
        ("get_log_file",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => VCS_Class,
         Static_Method => True,
         Handler       => VCS_Command_Handler'Access);

      Kernel.Scripts.Register_Command
        ("status_parse",
         Minimum_Args  => 4,
         Maximum_Args  => 5,
         Class         => VCS_Class,
         Static_Method => True,
         Handler       => Status_Parse_Handler'Access);
      Kernel.Scripts.Register_Command
        ("update_parse",
         Minimum_Args  => 2,
         Maximum_Args  => 3,
         Class         => VCS_Class,
         Static_Method => True,
         Handler       => Update_Parse_Handler'Access);
      Kernel.Scripts.Register_Command
        ("annotations_parse",
         Minimum_Args  => 3,
         Maximum_Args  => 3,
         Class         => VCS_Class,
         Static_Method => True,
         Handler       => Annotations_Parse_Handler'Access);
      Kernel.Scripts.Register_Command
        ("log_parse",
         Minimum_Args  => 3,
         Maximum_Args  => 3,
         Class         => VCS_Class,
         Static_Method => True,
         Handler       => Log_Parse_Handler'Access);
      Kernel.Scripts.Register_Command
        ("revision_parse",
         Minimum_Args  => 3,
         Maximum_Args  => 3,
         Class         => VCS_Class,
         Static_Method => True,
         Handler       => Revision_Parse_Handler'Access);

      --  Register VCS Activities commands

      Kernel.Scripts.Register_Command
        (Constructor_Method,
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => VCS_Activities_Class,
         Handler       => VCS_Activities_Class_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("get",
         Class         => VCS_Activities_Class,
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Static_Method => True,
         Handler       => VCS_Activities_Class_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("from_file",
         Class         => VCS_Activities_Class,
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Static_Method => True,
         Handler       => VCS_Activities_Class_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("list",
         Class         => VCS_Activities_Class,
         Static_Method => True,
         Handler       => VCS_Activities_Class_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("id",
         Class         => VCS_Activities_Class,
         Handler       => VCS_Activities_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("name",
         Class         => VCS_Activities_Class,
         Handler       => VCS_Activities_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("has_log",
         Class         => VCS_Activities_Class,
         Handler       => VCS_Activities_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("log_file",
         Class         => VCS_Activities_Class,
         Handler       => VCS_Activities_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("log",
         Class         => VCS_Activities_Class,
         Handler       => VCS_Activities_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("is_closed",
         Class         => VCS_Activities_Class,
         Handler       => VCS_Activities_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("set_closed",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => VCS_Activities_Class,
         Handler       => VCS_Activities_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("group_commit",
         Class         => VCS_Activities_Class,
         Handler       => VCS_Activities_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("toggle_group_commit",
         Class         => VCS_Activities_Class,
         Handler       => VCS_Activities_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("files",
         Class         => VCS_Activities_Class,
         Handler       => VCS_Activities_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("commit",
         Class         => VCS_Activities_Class,
         Handler       => VCS_Activities_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("vcs",
         Class         => VCS_Activities_Class,
         Handler       => VCS_Activities_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("add_file",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => VCS_Activities_Class,
         Handler       => VCS_Activities_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("remove_file",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => VCS_Activities_Class,
         Handler       => VCS_Activities_Command_Handler'Access);

      --  Register the main VCS menu and the VCS actions

      Register_Action
        (Kernel, "VCS open explorer", new Explorer_Command,
         Description => -"Open the VCS explorer",
         Category => -"Views");

      Register_Action
        (Kernel, "VCS open activities window", new Activities_Command,
         Description => -"Open the VCS activities window",
         Category    => -"Views");

      --  Register contextual menus

      Register_Actions (Kernel);
   end Register_Module;

   --------------------------
   -- Status_Parse_Handler --
   --------------------------

   procedure Status_Parse_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      pragma Unreferenced (Command);

      Kernel : constant Kernel_Handle := Get_Kernel (Data);

      Ref    : VCS_Access;

      VCS_Identifier : constant String := Nth_Arg (Data, 1);
      S              : constant String := Nth_Arg (Data, 2);

      Clear_Logs     : constant Boolean := Nth_Arg (Data, 3);
      Local          : constant Boolean := Nth_Arg (Data, 4);
      Dir            : constant String  := Nth_Arg (Data, 5, "");

   begin
      Ref := Get_VCS_From_Id (VCS_Identifier);

      if Ref = null then
         Insert (Kernel,
                 -"Could not find registered VCS corresponding to identifier: "
                 & VCS_Identifier);
         return;
      end if;

      Parse_Status (Ref, S, Local, Clear_Logs, Dir);
   end Status_Parse_Handler;

   --------------------------
   -- Update_Parse_Handler --
   --------------------------

   procedure Update_Parse_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      pragma Unreferenced (Command);

      Kernel : constant Kernel_Handle := Get_Kernel (Data);

      Ref    : VCS_Access;

      VCS_Identifier : constant String := Nth_Arg (Data, 1);
      S              : constant String := Nth_Arg (Data, 2);
      Dir            : constant String := Nth_Arg (Data, 3, "");

   begin
      Ref := Get_VCS_From_Id (VCS_Identifier);

      if Ref = null then
         Insert (Kernel,
                 -"Could not find registered VCS corresponding to identifier: "
                 & VCS_Identifier);
         return;
      end if;

      Parse_Update (Ref, S, Dir);
   end Update_Parse_Handler;

   -------------------------------
   -- Annotations_Parse_Handler --
   -------------------------------

   procedure Annotations_Parse_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      pragma Unreferenced (Command);
      Kernel         : constant Kernel_Handle := Get_Kernel (Data);
      Ref            : VCS_Access;
      VCS_Identifier : constant String := Nth_Arg (Data, 1);
      S              : constant String := Nth_Arg (Data, 3);
      File           : GNATCOLL.VFS.Virtual_File;
   begin
      Ref := Get_VCS_From_Id (VCS_Identifier);

      if Ref = null then
         Insert (Kernel,
                 -"Could not find registered VCS corresponding to identifier: "
                 & VCS_Identifier);
         return;
      end if;

      File := Ref.Create_From_VCS (Nth_Arg (Data, 2));

      Parse_Annotations (Ref, File, S);
   end Annotations_Parse_Handler;

   -----------------------
   -- Log_Parse_Handler --
   -----------------------

   procedure Log_Parse_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      pragma Unreferenced (Command);
      Kernel         : constant Kernel_Handle := Get_Kernel (Data);
      VCS_Identifier : constant String := Nth_Arg (Data, 1);
      S              : constant String := Nth_Arg (Data, 3);
      Ref            : VCS_Access;
      File           : GNATCOLL.VFS.Virtual_File;
      Escaped        : String (S'First .. S'First + S'Length * 2);
      Last           : Integer := S'First;
   begin
      Ref := Get_VCS_From_Id (VCS_Identifier);

      if Ref = null then
         Insert (Kernel,
                 -"Could not find registered VCS corresponding to identifier: "
                 & VCS_Identifier);
         return;
      end if;

      File := Ref.Create_From_VCS (Nth_Arg (Data, 2));

      for J in S'Range loop
         if S (J) = '%' then
            Escaped (Last .. Last + 1) := "%%";
            Last := Last + 2;
         else
            Escaped (Last) := S (J);
            Last := Last + 1;
         end if;
      end loop;

      Parse_Log (Ref, File, Escaped (Escaped'First .. Last - 1));
   end Log_Parse_Handler;

   ----------------------------
   -- Revision_Parse_Handler --
   ----------------------------

   procedure Revision_Parse_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      pragma Unreferenced (Command);
      Kernel         : constant Kernel_Handle := Get_Kernel (Data);
      VCS_Identifier : constant String := Nth_Arg (Data, 1);
      S              : constant String := Nth_Arg (Data, 3);
      Ref            : VCS_Access;
      File           : GNATCOLL.VFS.Virtual_File;
   begin
      Ref := Get_VCS_From_Id (VCS_Identifier);

      if Ref = null then
         Insert (Kernel,
                 -"Could not find registered VCS corresponding to identifier: "
                 & VCS_Identifier);
         return;
      end if;

      File := Ref.Create_From_VCS (Nth_Arg (Data, 2));

      Parse_Revision (Ref, File, S);
   end Revision_Parse_Handler;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_File_Edited;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File)
   is
      pragma Unreferenced (Self);

      --  Choose the first possible project, since for the same physical file,
      --  the VCS will be the same anyway
      F_Info : constant File_Info'Class := File_Info'Class
          (Get_Registry (Kernel).Tree.Info_Set (File).First_Element);
      Ref    : constant VCS_Access :=
        Get_Current_Ref (Kernel, F_Info.Project (True));
      Status : File_Status_Record;
   begin
      if Ref = null then
         return;
      end if;

      Status := Get_Cache (Get_Status_Cache, File).Status;

      if Status.File = GNATCOLL.VFS.No_File
        or else Status.Status.Icon_Name.all = Unknown_Stock
      then
         --  If file not found in the cache or the status is not yet known
         Get_Status (Ref, (1 => File), False, Local => True);
      else
         Display_Editor_Status (Kernel_Handle (Kernel), Ref, Status);
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_File_Closed;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File)
   is
      pragma Unreferenced (Self, Kernel);
      M : constant VCS_Module_ID_Access := VCS_Module_ID;
   begin
      M.Reference_Map.Exclude (File);
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_File_Status_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Status : GPS.Kernel.File_Status)
   is
      pragma Unreferenced (Self);

      --  At random chose the first possible project. If this is the same
      --  physicial source file, the VCS system will be the same anyway
      F_Info   : constant File_Info'Class :=
        File_Info'Class
          (Get_Registry (Kernel).Tree.Info_Set (File).First_Element);
      Project  : constant Project_Type := F_Info.Project;
      Ref      : VCS_Access;
      Stat     : Line_Record;
   begin
      if Status = Unmodified then
         --  Nothing else to do, this is a status changed hook run when the
         --  file is first loaded. There is no local modification done to this
         --  file.
         return;
      end if;

      if Project /= No_Project then
         Ref := Get_Current_Ref (Kernel, Project);
      end if;

      if Ref = null then
         return;
      end if;

      --  First ensure that the file is already in the explorer

      Stat := Get_Cache (Get_Status_Cache, File);

      if Stat = No_Data then
         --  This is not part of the cache yet
         Stat.Status.File := File;
      end if;

      if Stat.Status.Status.Icon_Name.all /= Added_Stock
        and then Stat.Status.Status.Icon_Name.all /= Removed_Stock
      then
         --  We do not want to change the status of added or removed files

         declare
            S : constant Status_Array := Get_Registered_Status (Ref);
         begin
            --  ??? Status are a bit lousy, a more structured/typed design will
            --  probably benefit the maintenance.
            for K in S'Range loop
               if S (K).Icon_Name.all = Modified_Stock then
                  Stat.Status.Status := S (K);
                  exit;
               end if;
            end loop;
         end;

         Set_Cache (Get_Status_Cache, File, Stat);

         declare
            F_Status : File_Status_List.Vector;
         begin
            File_Status_List.Append (F_Status, Copy_File_Status (Stat.Status));

            Display_File_Status
              (Kernel_Handle (Kernel), F_Status, Ref, False, True);
            --  Just ensure that this file is added into the explorer if
            --  not yet present.

            F_Status.Clear;
         end;

         Refresh_File
           (Get_Explorer (Kernel_Handle (Kernel), Raise_Child => False), File);
         Refresh_File
           (Get_Activities_Explorer
              (Kernel_Handle (Kernel), Raise_Child => False), File);
      end if;
   end Execute;

   ------------------
   -- Get_Explorer --
   ------------------

   function Get_Explorer
     (Kernel      : not null access Kernel_Handle_Record'Class;
      Raise_Child : Boolean := True;
      Show        : Boolean := False) return VCS_Explorer_View_Access
   is
      M     : constant VCS_Module_ID_Access := VCS_Module_ID;
   begin
      if M.Explorer = null then
         Gtk_New (M.Explorer, Kernel);
         Ref (M.Explorer);
      end if;

      if Show and then M.Explorer_Child = null then
         M.Explorer_Child := new Explorer_Child_Record;
         Initialize (M.Explorer_Child, M.Explorer,
                     Kernel         => Kernel,
                     Areas          => Central_Only,
                     Group          => Group_VCS_Explorer,
                     Module         => VCS_Explorer_Module_Id);
         Set_Title (M.Explorer_Child, -"VCS Explorer");
         Put (Get_MDI (Kernel), M.Explorer_Child);
         Set_Focus_Child (M.Explorer_Child);
      end if;

      if M.Explorer_Child /= null and then Raise_Child then
         Gtkada.MDI.Raise_Child (M.Explorer_Child);
      end if;

      return M.Explorer;
   end Get_Explorer;

   -----------------------
   -- Hide_VCS_Explorer --
   -----------------------

   procedure Hide_VCS_Explorer is
      M : constant VCS_Module_ID_Access := VCS_Module_ID;
   begin
      if M.Explorer = null or else M.Explorer_Child = null then
         return;
      else
         Close_Child (M.Explorer_Child, True);
         M.Explorer_Child := null;
      end if;
   end Hide_VCS_Explorer;

   ----------------------
   -- Explorer_Is_Open --
   ----------------------

   function Explorer_Is_Open return Boolean is
      M : constant VCS_Module_ID_Access := VCS_Module_ID;
   begin
      return M.Explorer /= null and then M.Explorer_Child /= null;
   end Explorer_Is_Open;

   -----------------------------
   -- Get_Activities_Explorer --
   -----------------------------

   function Get_Activities_Explorer
     (Kernel      : not null access Kernel_Handle_Record'Class;
      Raise_Child : Boolean := True;
      Show        : Boolean := False) return VCS_Activities_View_Access
   is
      M     : constant VCS_Module_ID_Access := VCS_Module_ID;
   begin
      if M.Activities = null then
         Gtk_New (M.Activities, Kernel);
      end if;

      if Show and then M.Activities_Child = null then
         M.Activities_Child := new Activities_Child_Record;
         Initialize (M.Activities_Child, M.Activities,
                     Kernel         => Kernel,
                     Group          => Group_VCS_Activities,
                     Module         => VCS_Module_ID);
         Set_Title (M.Activities_Child, -"VCS Activities");
         Put (Get_MDI (Kernel), M.Activities_Child);
         Set_Focus_Child (M.Activities_Child);
      end if;

      if M.Activities_Child /= null and then Raise_Child then
         Gtkada.MDI.Raise_Child (M.Activities_Child);
      end if;

      return M.Activities;
   end Get_Activities_Explorer;

   ----------------------------------
   -- Hide_VCS_Activities_Explorer --
   ----------------------------------

   procedure Hide_VCS_Activities_Explorer is
      M : constant VCS_Module_ID_Access := VCS_Module_ID;
   begin
      if M.Activities = null or else M.Activities_Child = null then
         return;
      else
         Ref (M.Activities);
         Close_Child (M.Activities_Child, True);
         M.Activities_Child := null;
      end if;
   end Hide_VCS_Activities_Explorer;

   ---------------------------------
   -- Activities_Explorer_Is_Open --
   ---------------------------------

   function Activities_Explorer_Is_Open return Boolean is
      M : constant VCS_Module_ID_Access := VCS_Module_ID;
   begin
      return M.Activities /= null and then M.Activities_Child /= null;
   end Activities_Explorer_Is_Open;

   ----------------------
   -- Get_Status_Cache --
   ----------------------

   function Get_Status_Cache return Status_Cache is
      M : constant VCS_Module_ID_Access := VCS_Module_ID;
   begin
      return M.Cached_Status;
   end Get_Status_Cache;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Project_Changed;
      Kernel : not null access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Self);
   begin
      VCS_Module_ID.VCS_Project_Cache.Clear;

      if Activities_Explorer_Is_Open then
         --  The VCS Activities window is opened, refresh it
         Open_Activities_Explorer (Kernel, No_Context);
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Project_Changing;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File)
   is
      pragma Unreferenced (Self, Kernel, File);
   begin
      --  ??? This could be removed
      VCS_Module_ID.VCS_Project_Cache.Clear;
   end Execute;

   -------------------
   -- Get_Reference --
   -------------------

   function Get_Reference
     (File : Virtual_File) return Virtual_File
   is
      M : constant VCS_Module_ID_Access := VCS_Module_ID;
   begin
      if M.Reference_Map.Contains (File) then
         return Ref_Map.Element (M.Reference_Map.Find (File));
      else
         return No_File;
      end if;
   end Get_Reference;

   -------------------
   -- Set_Reference --
   -------------------

   procedure Set_Reference (File, Reference : Virtual_File) is
      M : constant VCS_Module_ID_Access := VCS_Module_ID;
   begin
      M.Reference_Map.Include (File, Reference);
   end Set_Reference;

end VCS_Module;
