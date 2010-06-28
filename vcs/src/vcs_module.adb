-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2001-2010, AdaCore                  --
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

with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Unchecked_Deallocation;

with GNATCOLL.Scripts;          use GNATCOLL.Scripts;
with GNAT.Strings;
with GNAT.OS_Lib;               use GNAT.OS_Lib;

with Glib.Object;               use Glib.Object;
with XML_Utils;                 use XML_Utils;
with Glib;                      use Glib;

with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Widget;                use Gtk.Widget;

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
with Traces;                    use Traces;
with VCS.Generic_VCS;           use VCS.Generic_VCS;
with VCS.Unknown_VCS;           use VCS.Unknown_VCS;
with VCS_Activities;            use VCS_Activities;
with VCS_Activities_View_API;   use VCS_Activities_View_API;
with VCS_Utils;                 use VCS_Utils;
with VCS_View;                  use VCS_View;
with VCS_View_API;              use VCS_View_API;

package body VCS_Module is

   type Has_VCS_Filter is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Has_VCS_Filter;
      Context : Selection_Context) return Boolean;
   --  True when the current context is associated with a known VCS

   type VCS_Contextual_Menu is new Submenu_Factory_Record with null record;
   overriding procedure Append_To_Menu
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

   procedure On_GPS_Started
     (Kernel : access Kernel_Handle_Record'Class);
   --  Called when GPS is starting

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

   --------------------
   -- On_GPS_Started --
   --------------------

   procedure On_GPS_Started (Kernel : access Kernel_Handle_Record'Class) is

      No_Action : constant VCS_Actions := (2 .. 1 => None);

      procedure Register_Action_Menu
        (Action_Label : String;
         Description  : String;
         Menu_Label   : String;
         Filter       : Action_Filter;
         Callback     : Context_Callback.Marshallers.Void_Marshaller.Handler;
         Actions      : VCS_Actions := No_Action);
      --  Registers an action and a menu only if Action is at least defined in
      --  a loaded VCS.

      procedure Create_Separator;
      --  Create a separator if needed (items inserted since last separator)

      function One_Action_Defined (Actions : VCS_Actions) return Boolean;
      --  Returns true if at least one action in Actions is defined

      function Log_Required_In_VCS return Boolean;
      --  Returns true if at least one VCS requires log content

      VCS_Menu    : constant String := "/_" & (-"VCS");
      Dir_Filter  : constant Action_Filter :=
                      Lookup_Filter (Kernel, "Directory");
      Prj_Filter  : constant Action_Filter :=
                      Lookup_Filter (Kernel, "Project");
      File_Filter : constant Action_Filter :=
                      Lookup_Filter (Kernel, "File");

      Mitem       : Gtk_Menu_Item;
      Items       : Boolean := True;

      ------------------------
      -- One_Action_Defined --
      ------------------------

      function One_Action_Defined (Actions : VCS_Actions) return Boolean is

         function Action_Defined (Action : VCS_Action) return Boolean;
         --  Return True if Action is at least defined in one VCS

         --------------------
         -- Action_Defined --
         --------------------

         function Action_Defined (Action : VCS_Action) return Boolean is

            procedure Check_Action (VCS : VCS_Access);
            --  Check Action presence in VCS

            Result : Boolean := False;

            ------------------
            -- Check_Action --
            ------------------

            procedure Check_Action (VCS : VCS_Access) is
            begin
               Result := Result or Is_Action_Defined (VCS, Action);
            end Check_Action;

         begin
            For_Every_VCS (Check_Action'Access);
            return Result;
         end Action_Defined;

      begin
         for K in Actions'Range loop
            if Action_Defined (Actions (K)) then
               return True;
            end if;
         end loop;
         return False;
      end One_Action_Defined;

      -------------------------
      -- Log_Required_In_VCS --
      -------------------------

      function Log_Required_In_VCS return Boolean is

         procedure Check_Log (VCS : VCS_Access);
         --  Check that log requires VCS

         Result : Boolean := False;

         ---------------
         -- Check_Log --
         ---------------

         procedure Check_Log (VCS : VCS_Access) is
         begin
            Result := Result or VCS.Require_Log;
         end Check_Log;

      begin
         For_Every_VCS (Check_Log'Access);
         return Result;
      end Log_Required_In_VCS;

      ----------------------
      -- Create_Separator --
      ----------------------

      procedure Create_Separator is
      begin
         if Items then
            Gtk_New (Mitem);
            Register_Menu (Kernel, VCS_Menu, Mitem);
            Items := False;
         end if;
      end Create_Separator;

      --------------------------
      -- Register_Action_Menu --
      --------------------------

      procedure Register_Action_Menu
        (Action_Label : String;
         Description  : String;
         Menu_Label   : String;
         Filter       : Action_Filter;
         Callback     : Context_Callback.Marshallers.Void_Marshaller.Handler;
         Actions      : VCS_Actions := No_Action)
      is
         Parent_String : GNAT.Strings.String_Access;
         Command       : Generic_Kernel_Command_Access;
      begin
         if Actions = No_Action or else One_Action_Defined (Actions) then
            Items := True;
            Create (Command, Kernel, Callback);
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
         end if;
      end Register_Action_Menu;

   begin
      if One_Action_Defined ((1 => Update)) then
         Register_Menu
           (Kernel, VCS_Menu, -"Update all _projects", "",
            Update_All'Access);
      end if;

      if One_Action_Defined ((Status_Files, Status_Dir)) then
         Register_Menu
           (Kernel, VCS_Menu, -"_Query status for all projects", "",
            Query_Status_For_Project'Access);
      end if;

      Create_Separator;

      Register_Action_Menu
        ("Status",
         -"Query the status of the current selection",
         -"Query _status",
         File_Filter,
         On_Menu_Get_Status'Access, (1 => Status_Files));

      Register_Action_Menu
        ("Update",
         -"Update to the current repository revision",
         -"_Update",
         File_Filter,
         On_Menu_Update'Access, (1 => Update));

      Register_Action_Menu
        ("Commit",
         -"Commit current file, or file corresponding to the current log",
         -"_Commit",
         File_Filter,
         On_Menu_Commit'Access, (1 => Commit));

      Create_Separator;

      Register_Action_Menu
        ("Open",
         -"Open the current file for editing",
         -"_Open",
         File_Filter,
         On_Menu_Open'Access, (1 => Open));

      Register_Action_Menu
        ("History",
         -"View the revision history for the current file",
         -"View _entire revision history",
         File_Filter,
         On_Menu_View_Log'Access, (1 => History));

      Register_Action_Menu
        ("History for revision...",
         -"View the revision history for one revision of the current file",
         -"View specific revision _history",
         File_Filter,
         On_Menu_View_Log_Rev'Access, (1 => History_Revision));

      Create_Separator;

      Register_Action_Menu
        ("Diff against head",
         -"Compare current file with the most recent revision",
         -"Compare against head revision",
         File_Filter,
         On_Menu_Diff'Access, (1 => Diff_Head));

      Register_Action_Menu
        ("Diff against revision...",
         -"Compare current file against a specified revision",
         -"Compare against specific revision",
         File_Filter,
         On_Menu_Diff_Specific'Access, (1 => Diff));

      Register_Action_Menu
        ("Diff between two revisions",
         -"Compare two specified revisions of current file",
         -"Compare two revisions",
         File_Filter,
         On_Menu_Diff2'Access, (1 => Diff2));

      Register_Action_Menu
        ("Diff base against head",
         -"Compare base and head revisions of current file",
         -"Compare base against head",
         File_Filter,
         On_Menu_Diff_Base_Head'Access, (1 => Diff_Base_Head));

      Create_Separator;

      Register_Action_Menu
        ("Annotate",
         -"Annotate the current file",
         -"Add annotations",
         File_Filter,
         On_Menu_Annotate'Access, (1 => Annotate));

      Register_Action_Menu
        ("Remove Annotate",
         -"Remove the annotations from current file",
         -"Remove annotations",
         File_Filter,
         On_Menu_Remove_Annotate'Access, (1 => Annotate));

      --  Add the log handling actions only if at least one VCS supports log

      if Log_Required_In_VCS then
         Register_Action_Menu
           ("Edit revision log",
            -"Edit the revision log for the current file",
            -"Edit revision log",
            File_Filter,
            On_Menu_Edit_Log'Access, (Add, Commit));

         Register_Action_Menu
           ("Edit global ChangeLog",
            -"Edit the global ChangeLog for the current selection",
            -"Edit global ChangeLog",
            File_Filter,
            On_Menu_Edit_ChangeLog'Access, (Add, Remove, Commit));

         Register_Action_Menu
           ("Remove revision log",
            -"Remove the revision log corresponding to the current file",
            -"Remove revision log",
            File_Filter,
            On_Menu_Remove_Log'Access, (Add, Remove, Commit));
      end if;

      Create_Separator;

      Register_Action_Menu
        ("Add",
         -"Add the current file to repository",
         -"_Add",
         File_Filter,
         On_Menu_Add'Access, (1 => Add));

      Register_Action_Menu
        ("Add no commit",
         -"Add the current file to repository, do not commit",
         -"Add/_No commit",
         File_Filter,
         On_Menu_Add_No_Commit'Access, (1 => Add_No_Commit));

      Register_Action_Menu
        ("Remove",
         -"Remove the current file from repository",
         -"_Remove",
         File_Filter,
         On_Menu_Remove'Access, (1 => Remove));

      Register_Action_Menu
        ("Remove no commit",
         -"Remove the current file from repository, do not commit",
         -"Remove/N_o commit",
         File_Filter,
         On_Menu_Remove_No_Commit'Access, (1 => Remove_No_Commit));

      Register_Action_Menu
        ("Revert",
         -"Revert the current file to repository revision",
         -"Re_vert",
         File_Filter,
         On_Menu_Revert'Access, (1 => Revert));

      Register_Action_Menu
        ("Resolved",
         -"Mark file conflicts resolved",
         -"Reso_lved",
         File_Filter,
         On_Menu_Resolved'Access, (1 => Resolved));

      Create_Separator;

      Register_Action_Menu
        ("Create tag",
         -"Create a tag or branch tag",
         -"Create _tag...",
         File_Filter,
         On_Menu_Create_Tag'Access, (1 => Create_Tag));

      Register_Action_Menu
        ("Switch tag",
         -"Switch to a specific tag or branch",
         -"S_witch tag...",
         File_Filter,
         On_Menu_Switch_Tag'Access, (1 => Switch));

      Create_Separator;

      Register_Action_Menu
        ("Status dir",
         -"Query the status of the current directory",
         -"_Query status for directory",
         Dir_Filter,
         On_Menu_Get_Status_Dir'Access, (1 => Status_Dir));

      Register_Action_Menu
        ("Update dir",
         -"Update the current directory",
         -"_Update directory",
         Dir_Filter,
         On_Menu_Update_Dir'Access, (1 => Update));

      Register_Action_Menu
        ("Status dir (recursively)",
         -"Query the status of the current directory recursively",
         -"Query _status for directory (recursively)",
         Dir_Filter,
         On_Menu_Get_Status_Dir_Recursive'Access, (1 => Status_Dir_Recursive));

      Register_Action_Menu
        ("Update dir (recursively)",
         -"Update the current directory (recursively)",
         -"Update _directory (recursively)",
         Dir_Filter,
         On_Menu_Update_Dir_Recursive'Access, (1 => Update));

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
         On_Menu_Get_Status_Project'Access,
         (Status_Files, Status_Dir));

      Register_Action_Menu
        ("Update project",
         -"Update the current project",
         -"_Update project",
         Prj_Filter,
         On_Menu_Update_Project'Access, (1 => Update));

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
         On_Menu_Get_Status_Project_Recursive'Access,
         (Status_Files, Status_Dir));

      Register_Action_Menu
        ("Update project (recursively)",
         -"Update the current project (recursively)",
         -"Update _project (recursively)",
         Prj_Filter,
         On_Menu_Update_Project_Recursive'Access, (1 => Update));
   end On_GPS_Started;

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
      when E : others => Trace (Exception_Handle, E);
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
      when E : others => Trace (Exception_Handle, E);
   end On_Open_Activities_Interface;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
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

   overriding procedure Append_To_Menu
     (Factory : access VCS_Contextual_Menu;
      Object  : access Glib.Object.GObject_Record'Class;
      Context : Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      pragma Unreferenced (Factory, Object);
   begin
      if Get_Creator (Context) /= Abstract_Module_ID (VCS_Module_ID)
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
      Child   : Gtk.Widget.Gtk_Widget)
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Module.all);
   begin
      if Child = Gtk_Widget (Get_Explorer (Kernel, False)) then
         Context := VCS_View_API.Context_Factory (Kernel, Child);
      else
         Context := VCS_Activities_View_API.Context_Factory (Kernel, Child);
      end if;
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
      VCS_Menu             : constant String := "/_" & (-"VCS");
      Tools_Menu           : constant String := -"Tools" & '/' & (-"Views");

      VCS_Action_Context   : constant Action_Filter := GPS.Kernel.Create;

      Filter               : Action_Filter;

   begin
      VCS_Module_ID := new VCS_Module_ID_Record;

      Register_Module
        (Module      => Module_ID (VCS_Module_ID),
         Kernel      => Kernel,
         Module_Name => VCS_Module_Name,
         Priority    => Default_Priority);

      Load_Activities (Kernel);

      Register_Desktop_Functions (Save_Desktop'Access, Load_Desktop'Access);

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

      Register_Menu
        (Kernel, VCS_Menu, -"_Explorer", "",
         On_Open_Interface'Access,
         Ref_Item   => -"Navigate",
         Add_Before => False);
      Register_Menu
        (Kernel, VCS_Menu, -"_Activities", "",
         On_Open_Activities_Interface'Access);

      Register_Menu
        (Kernel, Tools_Menu, -"VCS _Activities", "",
         On_Open_Activities_Interface'Access);
      Register_Menu
        (Kernel, Tools_Menu, -"_VCS Explorer", "",
         On_Open_Interface'Access);

      --  The creation of VCS menu is defferred after GPS is fully started.
      --  This is needed as we want to display in this menu only the items that
      --  are at least supported by a loaded VCS.

      Add_Hook (Kernel, GPS_Started_Hook,
                Wrapper (On_GPS_Started'Access),
                Name  => "vcs_module.gps_started");
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
   exception
      when E : others => Trace (Exception_Handle, E);
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
   exception
      when E : others => Trace (Exception_Handle, E);
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
   exception
      when E : others => Trace (Exception_Handle, E);
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
   exception
      when E : others => Trace (Exception_Handle, E);
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
   exception
      when E : others => Trace (Exception_Handle, E);
   end Revision_Parse_Handler;

   --------------------
   -- File_Edited_Cb --
   --------------------

   procedure File_Edited_Cb
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      D      : constant File_Hooks_Args := File_Hooks_Args (Data.all);
      Ref    : constant VCS_Access :=
                 Get_Current_Ref
                   (Kernel,
                    Get_Registry (Kernel).Tree.Info (D.File).Project (True));
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

   exception
      when E : others => Trace (Exception_Handle, E);
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
   exception
      when E : others => Trace (Exception_Handle, E);
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
      Project  : constant Project_Type :=
                   Get_Registry (Kernel).Tree.Info (D.File).Project;
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

   exception
      when E : others => Trace (Exception_Handle, E);
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
      end if;

      if Show and then M.Explorer_Child = null then
         Gtk_New (Child, M.Explorer,
                  Default_Width  => Gint (Default_Widget_Width.Get_Pref),
                  Default_Height => Gint (Default_Widget_Height.Get_Pref),
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
      if M.Explorer = null or else M.Explorer_Child = null then
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
                  Default_Width  => Gint (Default_Widget_Width.Get_Pref),
                  Default_Height => Gint (Default_Widget_Height.Get_Pref),
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
