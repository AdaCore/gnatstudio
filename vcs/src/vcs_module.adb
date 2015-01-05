------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2015, AdaCore                     --
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
with Ada.Unchecked_Deallocation;

with GNATCOLL.Scripts;          use GNATCOLL.Scripts;

with Glib.Object;               use Glib.Object;
with XML_Utils;                 use XML_Utils;
with Glib;                      use Glib;

with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Widget;                use Gtk.Widget;

with Log_Utils;                 use Log_Utils;

with Commands.Interactive;      use Commands, Commands.Interactive;
with GPS.Core_Kernels;          use GPS.Core_Kernels;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Actions;        use GPS.Kernel.Actions;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.Modules.UI;     use GPS.Kernel.Modules.UI;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;

with VCS.Generic_VCS;           use VCS.Generic_VCS;
with VCS.Unknown_VCS;           use VCS.Unknown_VCS;
with VCS_Activities;            use VCS_Activities;
with VCS_Activities_View_API;   use VCS_Activities_View_API;
with VCS_Utils;                 use VCS_Utils;
with VCS_View;                  use VCS_View;
with VCS_View_API;              use VCS_View_API;

with VCS_Module.Actions;        use VCS_Module.Actions;

package body VCS_Module is

   type VCS_Contextual_Menu is new Submenu_Factory_Record with null record;
   overriding procedure Append_To_Menu
     (Factory : access VCS_Contextual_Menu;
      Object  : access Glib.Object.GObject_Record'Class;
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

   procedure File_Edited_Cb
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Callback for the "file_edited" signal

   procedure File_Closed_Cb
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Callback for the "file_closed" signal

   procedure File_Status_Changed_Cb
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
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

   procedure On_Project_Changing
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Called when project is about to change

   procedure On_Project_Changed
     (Kernel : access Kernel_Handle_Record'Class);
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
      Object  : access Glib.Object.GObject_Record'Class;
      Context : Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      pragma Unreferenced (Factory, Object);
      Creator : constant Abstract_Module := Get_Creator (Context);
   begin
      if (Creator /= Abstract_Module (VCS_Module_ID)
          and then Creator /= Abstract_Module (VCS_Explorer_Module_Id))
        or else Has_Activity_Information (Context)
      then
         VCS_View_API.VCS_Contextual_Menu
           (Get_Kernel (Context), Context, Menu, False);
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
            Set_Return_Value (Data, Name);
         end if;
      end Add_VCS;

   begin
      if Command = "supported_systems" then
         Set_Return_Value_As_List (Data);
         Set_Return_Value (Data, String'("Auto"));
         VCS_Module_ID.Registered_VCS.Iterate (Add_VCS'Access);

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
         return M.Explorer_Child;

      elsif Node.Tag.all = "VCS_Activities_View_Record" then
         Open_Activities_Explorer (User, No_Context);
         return M.Activities_Child;
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

   -----------------------------
   -- Default_Context_Factory --
   -----------------------------

   overriding procedure Default_Context_Factory
     (Module  : access VCS_Module_ID_Record;
      Context : in out Selection_Context;
      Child   : Glib.Object.GObject)
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Module.all);
   begin
      if Child = GObject (Get_Explorer (Kernel, False)) then
         Context := VCS_View_API.Context_Factory (Kernel, Gtk_Widget (Child));
      else
         Context := VCS_Activities_View_API.Context_Factory
           (Kernel, Gtk_Widget (Child));
      end if;
   end Default_Context_Factory;

   -----------------------------
   -- Default_Context_Factory --
   -----------------------------

   overriding procedure Default_Context_Factory
     (Module  : access VCS_Explorer_Module_ID_Record;
      Context : in out Selection_Context;
      Child   : Glib.Object.GObject) is
   begin
      Context := VCS_View_API.Context_Factory
        (Module.Get_Kernel, Gtk_Widget (Child));
   end Default_Context_Factory;

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

      VCS_Action_Context   : constant Action_Filter := GPS.Kernel.Create;

   begin
      VCS_Module_ID := new VCS_Module_ID_Record;
      VCS_Explorer_Module_Id := new VCS_Explorer_Module_ID_Record;

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

      Load_Activities (Kernel);

      Register_Desktop_Functions (Save_Desktop'Access, Load_Desktop'Access);

      Log_Utils.Initialize (Kernel);

      Standard.VCS.Unknown_VCS.Register_Module (Kernel);
      Standard.VCS.Generic_VCS.Register_Module (Kernel);

      Add_Hook (Kernel, File_Edited_Hook,
                Wrapper (File_Edited_Cb'Access),
                Name => "vcs.file_edited");

      Add_Hook (Kernel, File_Status_Changed_Action_Hook,
                Wrapper (File_Status_Changed_Cb'Access),
                Name => "vcs.file_status_changed");

      Add_Hook (Kernel, File_Closed_Hook,
                Wrapper (File_Closed_Cb'Access),
                Name => "vcs.file_closed_edited");

      Add_Hook
        (Kernel, Project_Changing_Hook,
         Wrapper (On_Project_Changing'Access), "vcs.project_changing");

      Add_Hook
        (Kernel, Project_View_Changed_Hook,
         Wrapper (On_Project_Changed'Access), "vcs.project_changed");

      Load_Cache (Kernel, VCS_Module_ID.Cached_Status);

      Register_Hook_No_Args (Kernel, Commit_Done_Hook);
      Register_Hook_No_Args (Kernel, Activity_Checked_Hook);
      Register_Hook_No_Args (Kernel, Log_Parsed_Hook);
      Register_Hook_No_Args (Kernel, Status_Parsed_Hook);
      Register_Hook_No_Args (Kernel, Revision_Parsed_Hook);
      Register_Hook_No_Args (Kernel, Annotation_Parsed_Hook);

      --  Register VCS commands

      Register_Command
        (Kernel, "supported_systems",
         Class         => VCS_Class,
         Static_Method => True,
         Handler       => VCS_Command_Handler_No_Param'Access);
      Register_Command
        (Kernel, "get_current_vcs",
         Class         => VCS_Class,
         Static_Method => True,
         Handler       => VCS_Command_Handler_No_Param'Access);

      Register_Command
        (Kernel, "get_status",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => VCS_Class,
         Static_Method => True,
         Handler       => VCS_Command_Handler'Access);
      Register_Command
        (Kernel, "update",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => VCS_Class,
         Static_Method => True,
         Handler       => VCS_Command_Handler'Access);
      Register_Command
        (Kernel, "commit",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => VCS_Class,
         Static_Method => True,
         Handler       => VCS_Command_Handler'Access);
      Register_Command
        (Kernel, "diff_head",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => VCS_Class,
         Static_Method => True,
         Handler       => VCS_Command_Handler'Access);
      Register_Command
        (Kernel, "diff_working",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => VCS_Class,
         Static_Method => True,
         Handler       => VCS_Command_Handler'Access);
      Register_Command
        (Kernel, "annotate",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => VCS_Class,
         Static_Method => True,
         Handler       => VCS_Command_Handler'Access);
      Register_Command
        (Kernel, "remove_annotations",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => VCS_Class,
         Static_Method => True,
         Handler       => VCS_Command_Handler'Access);
      Register_Command
        (Kernel, "log",
         Minimum_Args  => 1,
         Maximum_Args  => 2,
         Class         => VCS_Class,
         Static_Method => True,
         Handler       => VCS_Command_Handler'Access);
      Register_Command
        (Kernel, "repository_path",
         Minimum_Args  => 1,
         Maximum_Args  => 2,
         Class         => VCS_Class,
         Static_Method => True,
         Handler       => VCS_Command_Handler'Access);
      Register_Command
        (Kernel, "repository_dir",
         Minimum_Args  => 0,
         Maximum_Args  => 1,
         Class         => VCS_Class,
         Static_Method => True,
         Handler       => VCS_Command_Handler'Access);
      Register_Command
        (Kernel, "set_reference",
         Minimum_Args  => 2,
         Maximum_Args  => 2,
         Class         => VCS_Class,
         Static_Method => True,
         Handler       => VCS_Command_Handler'Access);

      Register_Command
        (Kernel, "get_log_file",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => VCS_Class,
         Static_Method => True,
         Handler       => VCS_Command_Handler'Access);

      Register_Command
        (Kernel, "status_parse",
         Minimum_Args  => 4,
         Maximum_Args  => 5,
         Class         => VCS_Class,
         Static_Method => True,
         Handler       => Status_Parse_Handler'Access);
      Register_Command
        (Kernel, "update_parse",
         Minimum_Args  => 2,
         Maximum_Args  => 3,
         Class         => VCS_Class,
         Static_Method => True,
         Handler       => Update_Parse_Handler'Access);
      Register_Command
        (Kernel, "annotations_parse",
         Minimum_Args  => 3,
         Maximum_Args  => 3,
         Class         => VCS_Class,
         Static_Method => True,
         Handler       => Annotations_Parse_Handler'Access);
      Register_Command
        (Kernel, "log_parse",
         Minimum_Args  => 3,
         Maximum_Args  => 3,
         Class         => VCS_Class,
         Static_Method => True,
         Handler       => Log_Parse_Handler'Access);
      Register_Command
        (Kernel, "revision_parse",
         Minimum_Args  => 3,
         Maximum_Args  => 3,
         Class         => VCS_Class,
         Static_Method => True,
         Handler       => Revision_Parse_Handler'Access);

      --  Register VCS Activities commands

      Register_Command
        (Kernel, Constructor_Method,
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => VCS_Activities_Class,
         Handler       => VCS_Activities_Class_Command_Handler'Access);
      Register_Command
        (Kernel, "get",
         Class         => VCS_Activities_Class,
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Static_Method => True,
         Handler       => VCS_Activities_Class_Command_Handler'Access);
      Register_Command
        (Kernel, "from_file",
         Class         => VCS_Activities_Class,
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Static_Method => True,
         Handler       => VCS_Activities_Class_Command_Handler'Access);
      Register_Command
        (Kernel, "list",
         Class         => VCS_Activities_Class,
         Static_Method => True,
         Handler       => VCS_Activities_Class_Command_Handler'Access);
      Register_Command
        (Kernel, "id",
         Class         => VCS_Activities_Class,
         Handler       => VCS_Activities_Command_Handler'Access);
      Register_Command
        (Kernel, "name",
         Class         => VCS_Activities_Class,
         Handler       => VCS_Activities_Command_Handler'Access);
      Register_Command
        (Kernel, "has_log",
         Class         => VCS_Activities_Class,
         Handler       => VCS_Activities_Command_Handler'Access);
      Register_Command
        (Kernel, "log_file",
         Class         => VCS_Activities_Class,
         Handler       => VCS_Activities_Command_Handler'Access);
      Register_Command
        (Kernel, "log",
         Class         => VCS_Activities_Class,
         Handler       => VCS_Activities_Command_Handler'Access);
      Register_Command
        (Kernel, "is_closed",
         Class         => VCS_Activities_Class,
         Handler       => VCS_Activities_Command_Handler'Access);
      Register_Command
        (Kernel, "set_closed",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => VCS_Activities_Class,
         Handler       => VCS_Activities_Command_Handler'Access);
      Register_Command
        (Kernel, "group_commit",
         Class         => VCS_Activities_Class,
         Handler       => VCS_Activities_Command_Handler'Access);
      Register_Command
        (Kernel, "toggle_group_commit",
         Class         => VCS_Activities_Class,
         Handler       => VCS_Activities_Command_Handler'Access);
      Register_Command
        (Kernel, "files",
         Class         => VCS_Activities_Class,
         Handler       => VCS_Activities_Command_Handler'Access);
      Register_Command
        (Kernel, "commit",
         Class         => VCS_Activities_Class,
         Handler       => VCS_Activities_Command_Handler'Access);
      Register_Command
        (Kernel, "vcs",
         Class         => VCS_Activities_Class,
         Handler       => VCS_Activities_Command_Handler'Access);
      Register_Command
        (Kernel, "add_file",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => VCS_Activities_Class,
         Handler       => VCS_Activities_Command_Handler'Access);
      Register_Command
        (Kernel, "remove_file",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => VCS_Activities_Class,
         Handler       => VCS_Activities_Command_Handler'Access);

      --  Register the main VCS menu and the VCS actions

      Register_Filter (Kernel, VCS_Action_Context, "VCS");

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

   --------------------
   -- File_Edited_Cb --
   --------------------

   procedure File_Edited_Cb
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      D      : constant File_Hooks_Args := File_Hooks_Args (Data.all);

      --  Choose the first possible project, since for the same physical file,
      --  the VCS will be the same anyway
      F_Info : constant File_Info'Class :=
        File_Info'Class
          (Get_Registry (Kernel).Tree.Info_Set (D.File).First_Element);
      Ref    : constant VCS_Access :=
        Get_Current_Ref (Kernel, F_Info.Project (True));
      Status : File_Status_Record;
   begin
      if Ref = null then
         return;
      end if;

      Status := Get_Cache (Get_Status_Cache, D.File).Status;

      if Status.File = GNATCOLL.VFS.No_File
        or else Status.Status.Stock_Id.all = "gps-vcs-unknown"
      then
         --  If file not found in the cache or the status is not yet known
         Get_Status (Ref, (1 => D.File), False, Local => True);
      else
         Display_Editor_Status (Kernel_Handle (Kernel), Ref, Status);
      end if;
   end File_Edited_Cb;

   --------------------
   -- File_Closed_Cb --
   --------------------

   procedure File_Closed_Cb
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      pragma Unreferenced (Kernel);
      M : constant VCS_Module_ID_Access := VCS_Module_ID;
      D : constant File_Hooks_Args := File_Hooks_Args (Data.all);
   begin
      M.Reference_Map.Exclude (D.File);
   end File_Closed_Cb;

   ----------------------------
   -- File_Status_Changed_Cb --
   ----------------------------

   procedure File_Status_Changed_Cb
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      D        : constant File_Status_Changed_Hooks_Args :=
        File_Status_Changed_Hooks_Args (Data.all);

      --  At random chose the first possible project. If this is the same
      --  physicial source file, the VCS system will be the same anyway
      F_Info   : constant File_Info'Class :=
        File_Info'Class
          (Get_Registry (Kernel).Tree.Info_Set (D.File).First_Element);
      Project  : constant Project_Type := F_Info.Project;
      Ref      : VCS_Access;
      Status   : Line_Record;
      F_Status : File_Status_List.List;
   begin
      if D.Status = Unmodified then
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

      Status := Get_Cache (Get_Status_Cache, D.File);

      if Status = No_Data then
         --  This is not part of the cache yet
         Status.Status.File := D.File;
      end if;

      if Status.Status.Status.Stock_Id.all /= "gps-vcs-added"
        and then Status.Status.Status.Stock_Id.all /= "gps-vcs-removed"
      then
         --  We do not want to change the status of added or removed files

         declare
            S : constant Status_Array := Get_Registered_Status (Ref);
         begin
            --  ??? Status are a bit lousy, a more structured/typed design will
            --  probably benefit the maintenance.
            for K in S'Range loop
               if S (K).Stock_Id.all = "gps-vcs-modified" then
                  Status.Status.Status := S (K);
                  exit;
               end if;
            end loop;
         end;

         Set_Cache (Get_Status_Cache, D.File, Status);

         File_Status_List.Append (F_Status, Copy_File_Status (Status.Status));

         Display_File_Status
           (Kernel_Handle (Kernel), F_Status, Ref, False, True);
         --  Just ensure that this file is added into the explorer if not yet
         --  present.

         Refresh_File
           (Get_Explorer
              (Kernel_Handle (Kernel), Raise_Child => False), D.File);
         Refresh_File
           (Get_Activities_Explorer
              (Kernel_Handle (Kernel), Raise_Child => False), D.File);
      end if;
   end File_Status_Changed_Cb;

   ------------------
   -- Get_Explorer --
   ------------------

   function Get_Explorer
     (Kernel      : not null access Kernel_Handle_Record'Class;
      Raise_Child : Boolean := True;
      Show        : Boolean := False) return VCS_Explorer_View_Access
   is
      M     : constant VCS_Module_ID_Access := VCS_Module_ID;
      Child : GPS_MDI_Child;
   begin
      if M.Explorer = null then
         Gtk_New (M.Explorer, Kernel);
         Ref (M.Explorer);
      end if;

      if Show and then M.Explorer_Child = null then
         Gtk_New (Child, M.Explorer,
                  Areas          => Central_Only,
                  Group          => Group_VCS_Explorer,
                  Module         => VCS_Explorer_Module_Id);
         M.Explorer_Child := MDI_Child (Child);
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
      Child : GPS_MDI_Child;
   begin
      if M.Activities = null then
         Gtk_New (M.Activities, Kernel);
      end if;

      if Show and then M.Activities_Child = null then
         Gtk_New (Child, M.Activities,
                  Group          => Group_VCS_Activities,
                  Module         => VCS_Module_ID);
         M.Activities_Child := MDI_Child (Child);
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

   ---------------
   -- Get_Cache --
   ---------------

   function Get_Status_Cache return Status_Cache is
      M : constant VCS_Module_ID_Access := VCS_Module_ID;
   begin
      return M.Cached_Status;
   end Get_Status_Cache;

   ------------------------
   -- On_Project_Changed --
   ------------------------

   procedure On_Project_Changed (Kernel : access Kernel_Handle_Record'Class) is
   begin
      VCS_Module_ID.VCS_Project_Cache.Clear;

      if Activities_Explorer_Is_Open then
         --  The VCS Activities window is opened, refresh it
         Open_Activities_Explorer (Kernel, No_Context);
      end if;
   end On_Project_Changed;

   -------------------------
   -- On_Project_Changing --
   -------------------------

   procedure On_Project_Changing
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      pragma Unreferenced (Data, Kernel);
   begin
      --  ??? This could be removed
      VCS_Module_ID.VCS_Project_Cache.Clear;
   end On_Project_Changing;

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
