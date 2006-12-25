-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2001-2006                       --
--                             AdaCore                               --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
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

with Ada.Exceptions;            use Ada.Exceptions;
with GNAT.Strings;

with Glib.Object;               use Glib.Object;
with Glib.Xml_Int;              use Glib.Xml_Int;
with Glib;                      use Glib;

with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Widget;                use Gtk.Widget;

with Basic_Types;               use Basic_Types;
with Commands.VCS;              use Commands.VCS;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Actions;        use GPS.Kernel.Actions;
with GPS.Kernel.Console;        use GPS.Kernel.Console;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with Log_Utils;
with Projects.Registry;         use Projects.Registry;
with Projects;                  use Projects;
with String_List_Utils;
with Traces;                    use Traces;
with VCS.Generic_VCS;           use VCS.Generic_VCS;
with VCS.Unknown_VCS;           use VCS.Unknown_VCS;
with VCS;                       use VCS;
with VCS_Activities;            use VCS_Activities;
with VCS_Activities_View_API;   use VCS_Activities_View_API;
with VCS_Utils;                 use VCS_Utils;
with VCS_View;                  use VCS_View;
with VCS_View_API;              use VCS_View_API;
with VFS;                       use VFS;

package body VCS_Module is

   Auto_Detect : constant String := "None";

   type Has_VCS_Filter is new Action_Filter_Record with null record;
   function Filter_Matches_Primitive
     (Filter  : access Has_VCS_Filter;
      Context : Selection_Context) return Boolean;
   --  True when the current context is associated with a known VCS

   type VCS_Contextual_Menu is new Submenu_Factory_Record with null record;
   procedure Append_To_Menu
     (Factory : access VCS_Contextual_Menu;
      Object  : access Glib.Object.GObject_Record'Class;
      Context : Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  Fill Menu with the contextual menu for the VCS module,
   --  if Context is appropriate.

   procedure On_Open_Interface
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  Display the VCS explorer

   procedure On_Open_Activities_Interface
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  Display the VCS Activities explorer

   procedure File_Edited_Cb
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Callback for the "file_edited" signal

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
      User   : Kernel_Handle)
      return Node_Ptr;
   --  Save the status of the project explorer to an XML tree

   procedure Status_Parse_Handler
     (Data    : in out GPS.Kernel.Scripts.Callback_Data'Class;
      Command : String);
   --  Handler for the command "vcs_status_parse"

   procedure Annotations_Parse_Handler
     (Data    : in out GPS.Kernel.Scripts.Callback_Data'Class;
      Command : String);
   --  Handler for the command "VCS.annotations_parse"

   procedure Log_Parse_Handler
     (Data    : in out GPS.Kernel.Scripts.Callback_Data'Class;
      Command : String);
   --  Handler for the command "VCS.log_parse"

   procedure Revision_Parse_Handler
     (Data    : in out GPS.Kernel.Scripts.Callback_Data'Class;
      Command : String);
   --  Handler for the command "VCS.revision_parse"

   procedure VCS_Command_Handler_No_Param
     (Data    : in out GPS.Kernel.Scripts.Callback_Data'Class;
      Command : String);
   --  Handler for VCS commands that take no parameter

   procedure VCS_Activities_Class_Command_Handler
     (Data    : in out GPS.Kernel.Scripts.Callback_Data'Class;
      Command : String);
   --  Handler for VCS Activities commands that take no parameter

   -----------------------
   -- On_Open_Interface --
   -----------------------

   procedure On_Open_Interface
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      Open_Explorer (Kernel, No_Context);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Open_Interface;

   ----------------------------------
   -- On_Open_Activities_Interface --
   ----------------------------------

   procedure On_Open_Activities_Interface
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      Open_Activities_Explorer (Kernel, No_Context);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Open_Activities_Interface;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   function Filter_Matches_Primitive
     (Filter  : access Has_VCS_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      return (Has_File_Information (Context)
              or else Has_Project_Information (Context)
              or else Has_Directory_Information (Context))
        and then Get_Current_Ref (Context) /= Unknown_VCS_Reference
        and then (not Has_Entity_Name_Information (Context)
                  or else Get_Name (Module_ID (Get_Creator (Context))) =
                    "Source_Editor");
   end Filter_Matches_Primitive;

   --------------------
   -- Append_To_Menu --
   --------------------

   procedure Append_To_Menu
     (Factory : access VCS_Contextual_Menu;
      Object  : access Glib.Object.GObject_Record'Class;
      Context : Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      pragma Unreferenced (Factory, Object);
      C : Selection_Context := Context;
   begin
      --  ??? C is a workaround for now. For some reason, VCS_Contextual_Menu
      --  is trying to modify the context, where in fact it should really just
      --  build the menu. It is too late to modify the context at this stage,
      --  and this should be done in the context factory instead

      if Get_Creator (Context) /= Abstract_Module_ID (VCS_Module_ID)
        or else Has_Activity_Information (Context)
      then
         VCS_View_API.VCS_Contextual_Menu
           (Get_Kernel (Context),
            C,
            Menu,
            False);
      end if;
   end Append_To_Menu;

   ------------------
   -- Get_VCS_List --
   ------------------

   function Get_VCS_List
     (Module : Module_ID) return Argument_List is
   begin
      return VCS_Module_ID_Access (Module).VCS_List.all;
   end Get_VCS_List;

   ------------------
   -- Register_VCS --
   ------------------

   procedure Register_VCS (Module : Module_ID; VCS_Identifier : String) is
      M   : constant VCS_Module_ID_Access := VCS_Module_ID_Access (Module);
      Old : Argument_List_Access;
   begin
      if M.VCS_List = null then
         M.VCS_List := new Argument_List'(1 => new String'(VCS_Identifier));
      else
         Old := M.VCS_List;
         M.VCS_List := new Argument_List (1 .. M.VCS_List'Length + 1);
         M.VCS_List (Old'Range) := Old.all;
         M.VCS_List (M.VCS_List'Last) := new String'(VCS_Identifier);
         Basic_Types.Unchecked_Free (Old);
      end if;
   end Register_VCS;

   ----------------------------------
   -- VCS_Command_Handler_No_Param --
   ----------------------------------

   procedure VCS_Command_Handler_No_Param
     (Data    : in out GPS.Kernel.Scripts.Callback_Data'Class;
      Command : String) is
   begin
      if Command = "supported_systems" then
         declare
            Systems : constant Argument_List :=
                        Get_VCS_List (Module_ID (VCS_Module_ID));
         begin
            Set_Return_Value_As_List (Data);
            for S in Systems'Range loop
               if Systems (S).all = "" then
                  Set_Return_Value (Data, -Auto_Detect);
               else
                  Set_Return_Value (Data, Systems (S).all);
               end if;
            end loop;
         end;
      end if;
   end VCS_Command_Handler_No_Param;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Module : in out VCS_Module_ID_Record) is
   begin
      Free (Module.VCS_List);
      Clear_Cache (Module.Cached_Status);
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
      M          : constant VCS_Module_ID_Access := VCS_Module_ID;
      A_Explorer : VCS_Activities_View_Access;
      Explorer   : VCS_Explorer_View_Access;
      pragma Unreferenced (A_Explorer);
   begin
      if Node.Tag.all = "VCS_View_Record" then
         --  First we want to clear the current content of the VCS Explorer
         Explorer := Get_Explorer (User, True, True);
         Clear (Explorer);
         Open_Explorer (User, No_Context);
         return M.Explorer_Child;

      elsif Node.Tag.all = "VCS_Activities_View_Record" then
         A_Explorer := Get_Activities_Explorer (User, True, True);
         Query_Activities_Files (User, Real_Query => False);
         return M.Activities_Child;
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

   function Default_Context_Factory
     (Module : access VCS_Module_ID_Record;
      Child  : Gtk.Widget.Gtk_Widget) return Selection_Context is
   begin
      return VCS_View_API.Context_Factory (Get_Kernel (Module.all), Child);
   end Default_Context_Factory;

   ------------------------------------------
   -- VCS_Activities_Class_Command_Handler --
   ------------------------------------------

   procedure VCS_Activities_Class_Command_Handler
     (Data    : in out GPS.Kernel.Scripts.Callback_Data'Class;
      Command : String)
   is
      Kernel               : constant Kernel_Handle := Get_Kernel (Data);
      VCS_Activities_Class : constant Class_Type :=
                               New_Class (Kernel, "Activities");
      Inst                 : Class_Instance;
   begin
      if Command = Constructor_Method then
         Inst := Nth_Arg (Data, 1, VCS_Activities_Class);
         Set_Data (Inst, VCS_Activities_Class, Nth_Arg (Data, 2, ""));

      elsif Command = "create" then
         declare
            A : Activity_Id;
         begin
            A := New_Activity (Kernel);
            Set_Name (Kernel, A, Nth_Arg (Data, 1, ""));

            Inst := New_Instance (Get_Script (Data), VCS_Activities_Class);
            Set_Data (Inst, VCS_Activities_Class, Image (A));
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
               if Project_Path
                 (Get_Root_Project
                    (Get_Registry (Kernel).all)) = Get_Project_Path (A)
               then
                  Set_Return_Value (Data, Image (A));
               end if;
               A := Next;
            end loop;
         end;

      elsif Command = "from_file" then
         declare
            A : Activity_Id;
         begin
            Inst := New_Instance (Get_Script (Data), VCS_Activities_Class);
            A := Get_File_Activity (Get_Data (Data, 1));
            if A /= No_Activity then
               Set_Data (Inst, VCS_Activities_Class, Image (A));
               Set_Return_Value (Data, Inst);
            end if;
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
      VCS_Menu             : constant String := "/_" & (-"VCS");
      Tools_Menu           : constant String := -"Tools" & '/' & (-"Views");

      VCS_Action_Context   : constant Action_Filter := Action_Filter (Create);

      File_Filter          : constant Action_Filter :=
                               Lookup_Filter (Kernel, "File");
      Dir_Filter           : constant Action_Filter :=
                               Lookup_Filter (Kernel, "Directory");
      Prj_Filter           : constant Action_Filter :=
                               Lookup_Filter (Kernel, "Project");

      Command : Generic_Kernel_Command_Access;
      Filter  : Action_Filter;
      Mitem   : Gtk_Menu_Item;

      procedure Register_Action_Menu
        (Action_Label : String;
         Description  : String;
         Menu_Label   : String;
         Filter       : Action_Filter;
         Callback     : Context_Callback.Marshallers.Void_Marshaller.Handler);
      --  Registers an action and a menu

      --------------------------
      -- Register_Action_Menu --
      --------------------------

      procedure Register_Action_Menu
        (Action_Label : String;
         Description  : String;
         Menu_Label   : String;
         Filter       : Action_Filter;
         Callback     : Context_Callback.Marshallers.Void_Marshaller.Handler)
      is
         Parent_String : GNAT.Strings.String_Access;
      begin
         Create (Command, Kernel_Handle (Kernel), Callback);
         Register_Action
           (Kernel, Action_Label, Command, Description, Filter,
            Category => "VCS");

         if Filter = Dir_Filter then
            Parent_String := new String'("/" & (-"Directory"));
         elsif Filter = Prj_Filter then
            Parent_String := new String'("/" & (-"Project"));
         else
            Parent_String := new String'("");
         end if;

         Register_Menu
           (Kernel      => Kernel,
            Parent_Path => VCS_Menu & Parent_String.all,
            Text        => Menu_Label,
            Callback    => null,
            Action      => Lookup_Action (Kernel, Action_Label));

         Free (Parent_String);
      end Register_Action_Menu;

   begin
      VCS_Module_ID := new VCS_Module_ID_Record;

      Register_Module
        (Module      => Module_ID (VCS_Module_ID),
         Kernel      => Kernel,
         Module_Name => VCS_Module_Name,
         Priority    => Default_Priority);

      Load_Activities (Kernel);

      GPS.Kernel.Kernel_Desktop.Register_Desktop_Functions
        (Save_Desktop'Access, Load_Desktop'Access);

      Filter := new Has_VCS_Filter;
      Register_Filter (Kernel, Filter, "VCS");

      Register_Contextual_Submenu
        (Kernel  => Kernel,
         Name    => -"Version Control",
         Filter  => Filter,
         Submenu => new VCS_Contextual_Menu);

      Log_Utils.Initialize (Kernel);

      Standard.VCS.Unknown_VCS.Register_Module (Kernel);
      Standard.VCS.Generic_VCS.Register_Module (Kernel);

      Add_Hook (Kernel, File_Edited_Hook,
                Wrapper (File_Edited_Cb'Access),
                Name => "vcs.file_edited");

      Add_Hook (Kernel, File_Status_Changed_Action_Hook,
                Wrapper (File_Status_Changed_Cb'Access),
                Name => "vcs.file_status_changed");

      Load_Cache (Kernel, VCS_Module_ID.Cached_Status);

      Register_Hook_No_Args (Kernel, Commit_Done_Hook);
      Register_Hook_No_Args (Kernel, Activity_Checked_Hook);

      --  Register VCS commands

      Register_Command
        (Kernel, "supported_systems",
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
        (Kernel, "status_parse",
         Minimum_Args  => 4,
         Maximum_Args  => 5,
         Class         => VCS_Class,
         Static_Method => True,
         Handler       => Status_Parse_Handler'Access);
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
        (Kernel, "create",
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

      Register_Menu
        (Kernel, VCS_Menu, -"_Explorer", "",
         On_Open_Interface'Access,
         Ref_Item   => -"Navigate",
         Add_Before => False);
      Register_Menu
        (Kernel, Tools_Menu, -"_VCS Explorer", "",
         On_Open_Interface'Access);

      Register_Menu
        (Kernel, VCS_Menu, -"_Activities", "",
         On_Open_Activities_Interface'Access);
      Register_Menu
        (Kernel, Tools_Menu, -"VCS _Activities", "",
         On_Open_Activities_Interface'Access);

      Register_Menu
        (Kernel, VCS_Menu, -"Update all _projects", "",
         Update_All'Access);
      Register_Menu
        (Kernel, VCS_Menu, -"_Query status for all projects", "",
         Query_Status_For_Project'Access);
      Gtk_New (Mitem);
      Register_Menu (Kernel, VCS_Menu, Mitem);

      Register_Action_Menu
        ("Status",
         -"Query the status of the current selection",
         -"Query _status",
         File_Filter,
         On_Menu_Get_Status'Access);

      Register_Action_Menu
        ("Update",
         -"Update to the current repository revision",
         -"_Update",
         File_Filter,
         On_Menu_Update'Access);

      Register_Action_Menu
        ("Commit",
         -"Commit current file, or file corresponding to the current log",
         -"_Commit",
         File_Filter,
         On_Menu_Commit'Access);

      Gtk_New (Mitem);
      Register_Menu (Kernel, VCS_Menu, Mitem);

      Register_Action_Menu
        ("Open",
         -"Open the current file for editing",
         -"_Open",
         File_Filter,
         On_Menu_Open'Access);

      Register_Action_Menu
        ("History",
         -"View the revision history for the current file",
         -"View _entire revision history",
         File_Filter,
         On_Menu_View_Log'Access);

      Register_Action_Menu
        ("History for revision...",
         -"View the revision history for one revision of the current file",
         -"View specific revision _history",
         File_Filter,
         On_Menu_View_Log_Rev'Access);

      Gtk_New (Mitem);
      Register_Menu (Kernel, VCS_Menu, Mitem);

      Register_Action_Menu
        ("Diff against head",
         -"Compare current file with the most recent revision",
         -"Compare against head revision",
         File_Filter,
         On_Menu_Diff'Access);

      Register_Action_Menu
        ("Diff against revision...",
         -"Compare current file against a specified revision",
         -"Compare against specific revision",
         File_Filter,
         On_Menu_Diff_Specific'Access);

      Register_Action_Menu
        ("Diff between two revisions",
         -"Compare two specified revisions of current file",
         -"Compare two revisions",
         File_Filter,
         On_Menu_Diff2'Access);

      Register_Action_Menu
        ("Diff base against head",
         -"Compare base and head revisions of current file",
         -"Compare base against head",
         File_Filter,
         On_Menu_Diff_Base_Head'Access);

      Gtk_New (Mitem);
      Register_Menu (Kernel, VCS_Menu, Mitem);

      Register_Action_Menu
        ("Annotate",
         -"Annotate the current file",
         -"Add annotations",
         File_Filter,
         On_Menu_Annotate'Access);

      Register_Action_Menu
        ("Remove Annotate",
         -"Remove the annotations from current file",
         -"Remove annotations",
         File_Filter,
         On_Menu_Remove_Annotate'Access);

      Register_Action_Menu
        ("Edit revision log",
         -"Edit the revision log for the current file",
         -"Edit revision log",
         File_Filter,
         On_Menu_Edit_Log'Access);

      Register_Action_Menu
        ("Edit global ChangeLog",
         -"Edit the global ChangeLog for the current selection",
         -"Edit global ChangeLog",
         File_Filter,
         On_Menu_Edit_ChangeLog'Access);

      Register_Action_Menu
        ("Remove revision log",
         -"Remove the revision log corresponding to the current file",
         -"Remove revision log",
         File_Filter,
         On_Menu_Remove_Log'Access);

      Gtk_New (Mitem);
      Register_Menu (Kernel, VCS_Menu, Mitem);

      Register_Action_Menu
        ("Add",
         -"Add the current file to repository",
         -"_Add",
         File_Filter,
         On_Menu_Add'Access);

      Register_Action_Menu
        ("Add no commit",
         -"Add the current file to repository, do not commit",
         -"Add/_No commit",
         File_Filter,
         On_Menu_Add_No_Commit'Access);

      Register_Action_Menu
        ("Remove",
         -"Remove the current file from repository",
         -"_Remove",
         File_Filter,
         On_Menu_Remove'Access);

      Register_Action_Menu
        ("Remove no commit",
         -"Remove the current file from repository, do not commit",
         -"Remove/N_o commit",
         File_Filter,
         On_Menu_Remove_No_Commit'Access);

      Register_Action_Menu
        ("Revert",
         -"Revert the current file to repository revision",
         -"Re_vert",
         File_Filter,
         On_Menu_Revert'Access);

      Register_Action_Menu
        ("Resolved",
         -"Mark file conflicts resolved",
         -"Reso_lved",
         File_Filter,
         On_Menu_Resolved'Access);

      Gtk_New (Mitem);
      Register_Menu (Kernel, VCS_Menu, Mitem);

      Register_Action_Menu
        ("Create tag",
         -"Create a tag or branch tag",
         -"Create _tag...",
         File_Filter,
         On_Menu_Create_Tag'Access);

      Register_Action_Menu
        ("Switch tag",
         -"Switch to a specific tag or branch",
         -"S_witch tag...",
         File_Filter,
         On_Menu_Switch_Tag'Access);

      Gtk_New (Mitem);
      Register_Menu (Kernel, VCS_Menu, Mitem);

      Register_Action_Menu
        ("Status dir",
         -"Query the status of the current directory",
         -"_Query status for directory",
         Dir_Filter,
         On_Menu_Get_Status_Dir'Access);

      Register_Action_Menu
        ("Update dir",
         -"Update the current directory",
         -"_Update directory",
         Dir_Filter,
         On_Menu_Update_Dir'Access);

      Register_Action_Menu
        ("Status dir (recursively)",
         -"Query the status of the current directory recursively",
         -"Query _status for directory (recursively)",
         Dir_Filter,
         On_Menu_Get_Status_Dir_Recursive'Access);

      Register_Action_Menu
        ("Update dir (recursively)",
         -"Update the current directory (recursively)",
         -"Update _directory (recursively)",
         Dir_Filter,
         On_Menu_Update_Dir_Recursive'Access);

      Register_Action_Menu
        ("List project",
         -"List all the files in project",
         -"_List all files in project",
         Prj_Filter,
         On_Menu_List_Project_Files'Access);

      Register_Action_Menu
        ("Status project",
         -"Query the status of the current project",
         -"_Query status",
         Prj_Filter,
         On_Menu_Get_Status_Project'Access);

      Register_Action_Menu
        ("Update project",
         -"Update the current project",
         -"_Update project",
         Prj_Filter,
         On_Menu_Update_Project'Access);

      Register_Action_Menu
        ("List project (recursively)",
         -"List all the files in project and subprojects",
         -"List _all files in project (recursively)",
         Prj_Filter,
         On_Menu_List_Project_Files_Recursive'Access);

      Register_Action_Menu
        ("Status project (recursively)",
         -"Query the status of the current project recursively",
         -"Query _status (recursively)",
         Prj_Filter,
         On_Menu_Get_Status_Project_Recursive'Access);

      Register_Action_Menu
        ("Update project (recursively)",
         -"Update the current project (recursively)",
         -"Update _project (recursively)",
         Prj_Filter,
         On_Menu_Update_Project_Recursive'Access);
   end Register_Module;

   --------------------------
   -- Status_Parse_Handler --
   --------------------------

   procedure Status_Parse_Handler
     (Data    : in out GPS.Kernel.Scripts.Callback_Data'Class;
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
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Status_Parse_Handler;

   -------------------------------
   -- Annotations_Parse_Handler --
   -------------------------------

   procedure Annotations_Parse_Handler
     (Data    : in out GPS.Kernel.Scripts.Callback_Data'Class;
      Command : String)
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
      Ref    : VCS_Access;
      VCS_Identifier : constant String := Nth_Arg (Data, 1);
      File           : constant VFS.Virtual_File :=
                         Create (Nth_Arg (Data, 2), Kernel);
      S              : constant String := Nth_Arg (Data, 3);
   begin
      Ref := Get_VCS_From_Id (VCS_Identifier);

      if Ref = null then
         Insert (Kernel,
                 -"Could not find registered VCS corresponding to identifier: "
                 & VCS_Identifier);
         return;
      end if;

      Parse_Annotations (Ref, File, S);
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Annotations_Parse_Handler;

   -----------------------
   -- Log_Parse_Handler --
   -----------------------

   procedure Log_Parse_Handler
     (Data    : in out GPS.Kernel.Scripts.Callback_Data'Class;
      Command : String)
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
      Ref    : VCS_Access;
      VCS_Identifier : constant String := Nth_Arg (Data, 1);
      File           : constant VFS.Virtual_File :=
                         Create (Nth_Arg (Data, 2), Kernel);
      S              : constant String := Nth_Arg (Data, 3);
   begin
      Ref := Get_VCS_From_Id (VCS_Identifier);

      if Ref = null then
         Insert (Kernel,
                 -"Could not find registered VCS corresponding to identifier: "
                 & VCS_Identifier);
         return;
      end if;

      Parse_Log (Ref, File, S);
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Log_Parse_Handler;

   ----------------------------
   -- Revision_Parse_Handler --
   ----------------------------

   procedure Revision_Parse_Handler
     (Data    : in out GPS.Kernel.Scripts.Callback_Data'Class;
      Command : String)
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
      Ref    : VCS_Access;
      VCS_Identifier : constant String := Nth_Arg (Data, 1);
      File           : constant VFS.Virtual_File :=
                         Create (Nth_Arg (Data, 2), Kernel);
      S              : constant String := Nth_Arg (Data, 3);
   begin
      Ref := Get_VCS_From_Id (VCS_Identifier);

      if Ref = null then
         Insert (Kernel,
                 -"Could not find registered VCS corresponding to identifier: "
                 & VCS_Identifier);
         return;
      end if;

      Parse_Revision (Ref, File, S);
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Revision_Parse_Handler;

   --------------------
   -- File_Edited_Cb --
   --------------------

   procedure File_Edited_Cb
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      use String_List_Utils.String_List;
      D      : constant File_Hooks_Args := File_Hooks_Args (Data.all);
      Ref    : constant VCS_Access :=
                 Get_Current_Ref
                   (Get_Project_From_File
                      (Get_Registry (Kernel).all, D.File, True));
      Files  : List;
      Status : File_Status_Record;
   begin
      if Ref = null then
         return;
      end if;

      Status := Get_Cache (Get_Status_Cache, D.File).Status;

      if Status.File = VFS.No_File then
         Append (Files, Full_Name (D.File).all);
         Get_Status (Ref, Files, False, Local => True);
         Free (Files);
      else
         Display_Editor_Status (Kernel_Handle (Kernel), Ref, Status);
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end File_Edited_Cb;

   ----------------------------
   -- File_Status_Changed_Cb --
   ----------------------------

   procedure File_Status_Changed_Cb
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      D        : constant File_Status_Changed_Hooks_Args :=
                   File_Status_Changed_Hooks_Args (Data.all);
      Project  : constant Project_Type :=
                   Get_Project_From_File
                     (Get_Registry (Kernel).all, D.File, False);
      Ref      : VCS_Access;
      Status   : Line_Record;
      F_Status : File_Status_List.List;
   begin
      if Project /= No_Project then
         Ref := Get_Current_Ref (Project);
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

         --  Now refresh both explorers to take into account the new status

         Refresh
           (Get_Explorer (Kernel_Handle (Kernel), Raise_Child => False));

         Refresh
           (Get_Activities_Explorer
              (Kernel_Handle (Kernel), Raise_Child => False));
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end File_Status_Changed_Cb;

   ------------------
   -- Get_Explorer --
   ------------------

   function Get_Explorer
     (Kernel      : Kernel_Handle;
      Raise_Child : Boolean := True;
      Show        : Boolean := False) return VCS_Explorer_View_Access
   is
      M     : constant VCS_Module_ID_Access := VCS_Module_ID;
      Child : GPS_MDI_Child;
   begin
      if M.Explorer = null then
         Gtk_New (M.Explorer, Kernel);
      end if;

      if Show and then M.Explorer_Child = null then
         Gtk_New (Child, M.Explorer,
                  Default_Width  => Get_Pref (Default_Widget_Width),
                  Default_Height => Get_Pref (Default_Widget_Height),
                  Group          => Group_VCS_Explorer,
                  Module         => VCS_Module_ID);
         M.Explorer_Child := MDI_Child (Child);
         Set_Title (M.Explorer_Child, -"VCS Explorer");
         Put (Get_MDI (Kernel), M.Explorer_Child);
         Set_Focus_Child (M.Explorer_Child);
      end if;

      if M.Explorer_Child /= null
        and then Raise_Child
      then
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
      if M.Explorer = null
        or else M.Explorer_Child = null
      then
         return;
      else
         Ref (M.Explorer);
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
     (Kernel      : Kernel_Handle;
      Raise_Child : Boolean := True;
      Show        : Boolean := False) return VCS_Activities_View_Access
   is
      M : constant VCS_Module_ID_Access := VCS_Module_ID;
      Child : GPS_MDI_Child;
   begin
      if M.Activities = null then
         Gtk_New (M.Activities, Kernel);
      end if;

      if Show and then M.Activities_Child = null then
         Gtk_New (Child, M.Activities,
                  Default_Width  => Get_Pref (Default_Widget_Width),
                  Default_Height => Get_Pref (Default_Widget_Height),
                  Group          => Group_VCS_Activities,
                  Module         => VCS_Module_ID);
         M.Activities_Child := MDI_Child (Child);
         Set_Title (M.Activities_Child, -"VCS Activities");
         Put (Get_MDI (Kernel), M.Activities_Child);
         Set_Focus_Child (M.Activities_Child);
      end if;

      if M.Activities_Child /= null
        and then Raise_Child
      then
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
      if M.Activities = null
        or else M.Activities_Child = null
      then
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
      return M.Activities /= null
        and then M.Activities_Child /= null;
   end Activities_Explorer_Is_Open;

   ---------------
   -- Get_Cache --
   ---------------

   function Get_Status_Cache return Status_Cache is
      M : constant VCS_Module_ID_Access := VCS_Module_ID;
   begin
      return M.Cached_Status;
   end Get_Status_Cache;

end VCS_Module;
