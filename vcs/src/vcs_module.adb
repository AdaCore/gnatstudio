-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2001-2004                       --
--                            ACT-Europe                             --
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

with Glib;                        use Glib;
with Glib.Xml_Int;                use Glib.Xml_Int;
with Glib.Object;                 use Glib.Object;
with Gtk.Menu;                    use Gtk.Menu;
with Gtk.Menu_Item;               use Gtk.Menu_Item;
with Gtk.Widget;                  use Gtk.Widget;
with Gtkada.MDI;                  use Gtkada.MDI;

with Glide_Kernel.Contexts;       use Glide_Kernel.Contexts;
with Glide_Kernel.Console;        use Glide_Kernel.Console;
with Glide_Kernel.Hooks;          use Glide_Kernel.Hooks;
with Glide_Kernel.Modules;        use Glide_Kernel.Modules;
with Glide_Kernel.Preferences;    use Glide_Kernel.Preferences;
with Glide_Kernel.Project;        use Glide_Kernel.Project;
with Glide_Kernel.Scripts;        use Glide_Kernel.Scripts;
with Glide_Kernel.Standard_Hooks; use Glide_Kernel.Standard_Hooks;
with Glide_Kernel.Actions;        use Glide_Kernel.Actions;
with Glide_Intl;                  use Glide_Intl;

with Traces;                      use Traces;

with VCS;                         use VCS;
with VCS_View_API;                use VCS_View_API;
with VCS_View_Pkg;                use VCS_View_Pkg;
with Basic_Types;                 use Basic_Types;
with Commands.VCS;                use Commands.VCS;

with VCS.Unknown_VCS;             use VCS.Unknown_VCS;
with VCS.Generic_VCS;             use VCS.Generic_VCS;
with Ada.Exceptions;              use Ada.Exceptions;
with GNAT.OS_Lib;                 use GNAT.OS_Lib;
with Projects;                    use Projects;
with Projects.Registry;           use Projects.Registry;
with VFS;                         use VFS;

with String_List_Utils;
with Log_Utils;

package body VCS_Module is

   Auto_Detect  : constant String := "None";

   type VCS_Module_ID_Record is new Module_ID_Record with record
      VCS_List : Argument_List_Access;
      --  The list of all VCS systems recognized by the kernel.

      Explorer : VCS_View_Access;
      --  The VCS Explorer

      Explorer_Child : MDI_Child;
      --  The child containing the VCS Explorer.
   end record;
   type VCS_Module_ID_Access is access all VCS_Module_ID_Record'Class;

   procedure Destroy (Module : in out VCS_Module_ID_Record);
   --  Free the memory occupied by Module.



   type Has_VCS_Filter is new Action_Filter_Record with null record;
   function Filter_Matches_Primitive
     (Filter  : access Has_VCS_Filter;
      Context : access Selection_Context'Class) return Boolean;
   --  True when the current context is associated with a known VCS


   procedure VCS_Contextual_Menu
     (Object  : access Glib.Object.GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  Fill Menu with the contextual menu for the VCS module,
   --  if Context is appropriate.

   procedure On_Open_Interface
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  Display the VCS explorer

   procedure File_Edited_Cb
     (Kernel  : access Kernel_Handle_Record'Class;
      Data    : Hooks_Data'Class);
   --  Callback for the "file_edited" signal.

   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child;
   --  Restore the status of the explorer from a saved XML tree.

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
      return Node_Ptr;
   --  Save the status of the project explorer to an XML tree

   procedure Status_Parse_Handler
     (Data    : in out Glide_Kernel.Scripts.Callback_Data'Class;
      Command : String);
   --  Handler for the command "vcs_status_parse".

   procedure Annotations_Parse_Handler
     (Data    : in out Glide_Kernel.Scripts.Callback_Data'Class;
      Command : String);
   --  Handler for the command "VCS.annotations_parse".

   procedure VCS_Global_Menu
     (Kernel  : access Kernel_Handle_Record'Class;
      Context : Selection_Context_Access;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  Handle the dynamic global menu.

   procedure VCS_Command_Handler_No_Param
     (Data    : in out Glide_Kernel.Scripts.Callback_Data'Class;
      Command : String);
   --  Handler for VCS commands that take no parameter

   -----------------------
   -- On_Open_Interface --
   -----------------------

   procedure On_Open_Interface
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      Open_Explorer (Kernel, null);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Open_Interface;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   function Filter_Matches_Primitive
     (Filter  : access Has_VCS_Filter;
      Context : access Selection_Context'Class) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      return Context.all in File_Selection_Context'Class
        and then Get_Current_Ref (Selection_Context_Access (Context)) /=
          Unknown_VCS_Reference;
   end Filter_Matches_Primitive;

   -------------------------
   -- VCS_Contextual_Menu --
   -------------------------

   procedure VCS_Contextual_Menu
     (Object  : access Glib.Object.GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      use type VCS.VCS_Access;
      use type Gtk.Widget.Widget_List.Glist;
      pragma Unreferenced (Object);

      Item    : Gtk_Menu_Item;
      Submenu : Gtk_Menu;

   begin
      if Context.all in File_Selection_Context'Class
        and then Get_Current_Ref (Selection_Context_Access (Context)) /=
          Unknown_VCS_Reference
      then
         Gtk_New (Submenu);

         VCS_View_API.VCS_Contextual_Menu
           (Get_Kernel (Context),
            Selection_Context_Access (Context),
            Submenu,
            False);

         --  If the menu is empty, destroy it, otherwise set it in a submenu
         --  "version control".
         --  ??? Should the sub-menu be named after the VCS itself ?

         if Get_Children (Submenu) = Gtk.Widget.Widget_List.Null_List then
            Destroy (Submenu);
         else
            Gtk_New (Item, -"Version Control");
            Set_Submenu (Item, Submenu);
            Append (Menu, Item);
         end if;
      end if;
   end VCS_Contextual_Menu;

   ---------------------
   -- VCS_Global_Menu --
   ---------------------

   procedure VCS_Global_Menu
     (Kernel  : access Kernel_Handle_Record'Class;
      Context : Selection_Context_Access;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      Item : Gtk_Menu_Item;
   begin
      Gtk_New_With_Mnemonic (Item, -"_Explorer");
      Kernel_Callback.Connect
        (Item, "activate", On_Open_Interface'Access, Kernel_Handle (Kernel));
      Append (Menu, Item);

      --  ??? Should we use a vcs-specific vocable for the next two items ?

      Gtk_New_With_Mnemonic (Item, -"Update all _projects");
      Kernel_Callback.Connect
        (Item, "activate", Update_All'Access, Kernel_Handle (Kernel));
      Append (Menu, Item);

      Gtk_New_With_Mnemonic (Item, -"_Query status for all projects");
      Kernel_Callback.Connect
        (Item, "activate", Query_Status_For_Project'Access,
         Kernel_Handle (Kernel));
      Append (Menu, Item);

      Gtk_New (Item);
      Append (Menu, Item);

      VCS_Contextual_Menu (Kernel_Handle (Kernel), Context, Menu, True);
   end VCS_Global_Menu;

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
     (Data    : in out Glide_Kernel.Scripts.Callback_Data'Class;
      Command : String) is
   begin
      if Command = "supported_systems" then
         declare
            Systems : constant Argument_List := Get_VCS_List (VCS_Module_ID);
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
      M : constant VCS_Module_ID_Access :=
            VCS_Module_ID_Access (VCS_Module_ID);
      Explorer : VCS_View_Access;
      pragma Unreferenced (Explorer);
   begin
      if Node.Tag.all = "VCS_View_Record" then
         Explorer := Get_Explorer (User, True, True);
         return M.Explorer_Child;
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
      if Widget.all in VCS_View_Record'Class then
         N := new Node;
         N.Tag := new String'("VCS_View_Record");

         return N;
      end if;

      return null;
   end Save_Desktop;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      VCS_Class : constant Class_Type := New_Class (Kernel, "VCS");

      VCS_Root  : constant String := -"VCS";
      Command : Generic_Kernel_Command_Access;

      VCS_Action_Context : constant Action_Filter :=
        Action_Filter (Create);
      Filter : Action_Filter;
   begin
      VCS_Module_ID := new VCS_Module_ID_Record;
      Register_Module
        (Module                  => VCS_Module_ID,
         Kernel                  => Kernel,
         Module_Name             => VCS_Module_Name,
         Priority                => Default_Priority,
         Contextual_Menu_Handler => VCS_Contextual_Menu'Access,
         Default_Context_Factory => VCS_View_API.Context_Factory'Access);

      Glide_Kernel.Kernel_Desktop.Register_Desktop_Functions
        (Save_Desktop'Access, Load_Desktop'Access);

      Filter := new Has_VCS_Filter;
      Register_Filter (Kernel, Filter, "VCS");


      Log_Utils.Initialize (Kernel);

      Standard.VCS.Unknown_VCS.Register_Module (Kernel);
      Standard.VCS.Generic_VCS.Register_Module (Kernel);

      Add_Hook (Kernel, File_Edited_Hook, File_Edited_Cb'Access);

      --  Register VCS commands.

      Register_Command
        (Kernel, "supported_systems",
         Class         => VCS_Class,
         Static_Method => True,
         Handler      => VCS_Command_Handler_No_Param'Access);
      Register_Command
        (Kernel, "get_status",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Class         => VCS_Class,
         Static_Method => True,
         Handler       => VCS_Command_Handler'Access);
      Register_Command
        (Kernel, "update",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Class         => VCS_Class,
         Static_Method => True,
         Handler      => VCS_Command_Handler'Access);
      Register_Command
        (Kernel, "commit",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Class         => VCS_Class,
         Static_Method => True,
         Handler       => VCS_Command_Handler'Access);
      Register_Command
        (Kernel, "diff_head",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Class         => VCS_Class,
         Static_Method => True,
         Handler      => VCS_Command_Handler'Access);
      Register_Command
        (Kernel, "diff_working",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Class         => VCS_Class,
         Static_Method => True,
         Handler      => VCS_Command_Handler'Access);
      Register_Command
        (Kernel, "annotate",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Class         => VCS_Class,
         Static_Method => True,
         Handler      => VCS_Command_Handler'Access);
      Register_Command
        (Kernel, "remove_annotations",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Class         => VCS_Class,
         Static_Method => True,
         Handler      => VCS_Command_Handler'Access);
      Register_Command
        (Kernel, "log",
         Minimum_Args => 1,
         Maximum_Args => 2,
         Class         => VCS_Class,
         Static_Method => True,
         Handler      => VCS_Command_Handler'Access);
      Register_Command
        (Kernel, "status_parse",
         Minimum_Args => 4,
         Maximum_Args => 5,
         Class         => VCS_Class,
         Static_Method => True,
         Handler      => Status_Parse_Handler'Access);
      Register_Command
        (Kernel, "annotations_parse",
         Minimum_Args => 3,
         Maximum_Args => 3,
         Class         => VCS_Class,
         Static_Method => True,
         Handler      => Annotations_Parse_Handler'Access);

      Register_Dynamic_Menu
        (Kernel      => Kernel,
         Parent_Path => "/_" & VCS_Root,
         Text        => "",
         Factory     => VCS_Global_Menu'Access,
         Ref_Item => -"Navigate",
         Add_Before => False);

      --  Register the VCS actions.

      Register_Filter (Kernel, VCS_Action_Context, "VCS");

      Create (Command, Kernel_Handle (Kernel), On_Menu_Get_Status'Access);
      Register_Action
        (Kernel,
         "Status",
         Command,
         -"Query the status of the current selection",
         VCS_Action_Context);

      Create (Command, Kernel_Handle (Kernel), On_Menu_Open'Access);
      Register_Action
        (Kernel,
         "Open",
         Command,
         -"Open the current file for editing",
         VCS_Action_Context);

      Create (Command, Kernel_Handle (Kernel), On_Menu_Add'Access);
      Register_Action
        (Kernel,
         "Add",
         Command,
         -"Add the current file to repository",
         VCS_Action_Context);

      Create (Command, Kernel_Handle (Kernel), On_Menu_Remove'Access);
      Register_Action
        (Kernel,
         "Remove",
         Command,
         -"Remove the current file from repository",
         VCS_Action_Context);

      Create (Command, Kernel_Handle (Kernel), On_Menu_Revert'Access);
      Register_Action
        (Kernel,
         "Revert",
         Command,
         -"Revert the current file to repository revision",
         VCS_Action_Context);

      Create (Command, Kernel_Handle (Kernel), On_Menu_Annotate'Access);
      Register_Action
        (Kernel,
         "Annotate",
         Command,
         -"Annotate the current file",
         VCS_Action_Context);

      Create
        (Command, Kernel_Handle (Kernel), On_Menu_Remove_Annotate'Access);
      Register_Action
        (Kernel,
         "Remove Annotate",
         Command,
         -"Remove the annotations from current file",
         VCS_Action_Context);

      Create (Command, Kernel_Handle (Kernel), On_Menu_Diff'Access);
      Register_Action
        (Kernel,
         "Diff against head",
         Command,
         -"Compare current file with the most recent revision",
         VCS_Action_Context);

      Create (Command, Kernel_Handle (Kernel), On_Menu_Diff_Specific'Access);
      Register_Action
        (Kernel,
         "Diff against revision...",
         Command,
         -"Compare current file against a specified revision",
         VCS_Action_Context);

      Create (Command, Kernel_Handle (Kernel), On_Menu_View_Log'Access);
      Register_Action
        (Kernel,
         "History",
         Command,
         -"View the revision history for the current file",
         VCS_Action_Context);

      Create (Command, Kernel_Handle (Kernel), On_Menu_Update'Access);
      Register_Action
        (Kernel,
         "Update",
         Command,
         -"Update to the current repository revision",
         VCS_Action_Context);

      Create (Command, Kernel_Handle (Kernel), On_Menu_Edit_ChangeLog'Access);
      Register_Action
        (Kernel,
         "Edit global ChangeLog",
         Command,
         -"Edit the global ChangeLog for the current selection",
         VCS_Action_Context);

      Create (Command, Kernel_Handle (Kernel), On_Menu_Edit_Log'Access);
      Register_Action
        (Kernel,
         "Edit revision log",
         Command,
         -"Edit the revision log for the current file",
         VCS_Action_Context);

      Create (Command, Kernel_Handle (Kernel), On_Menu_Commit'Access);
      Register_Action
        (Kernel,
         "Commit",
         Command,
         -"Commit current file, or file corresponding to the current log",
         VCS_Action_Context);

      Create (Command, Kernel_Handle (Kernel), On_Menu_Remove_Log'Access);
      Register_Action
        (Kernel,
         "Remove revision log",
         Command,
         -"Remove the revision log corresponding to the current file",
         VCS_Action_Context);

      Create (Command, Kernel_Handle (Kernel), On_Menu_Get_Status_Dir'Access);
      Register_Action
        (Kernel,
         "Status dir",
         Command,
         -"Query the status of the current directory",
         VCS_Action_Context);

      Create (Command, Kernel_Handle (Kernel), On_Menu_Update_Dir'Access);
      Register_Action
        (Kernel,
         "Update dir",
         Command,
         -"Update the current directory",
         VCS_Action_Context);

      Create (Command, Kernel_Handle (Kernel),
              On_Menu_Get_Status_Project'Access);
      Register_Action
        (Kernel,
         "Status project",
         Command,
         -"Query the status of the current project",
         VCS_Action_Context);

      Create (Command, Kernel_Handle (Kernel), On_Menu_Update_Project'Access);
      Register_Action
        (Kernel,
         "Update project",
         Command,
         -"Update the current project",
         VCS_Action_Context);

      Create (Command, Kernel_Handle (Kernel),
              On_Menu_Get_Status_Dir_Recursive'Access);
      Register_Action
        (Kernel,
         "Status dir (recursively)",
         Command,
         -"Query the status of the current directory recursively",
         VCS_Action_Context);

      Create (Command, Kernel_Handle (Kernel),
              On_Menu_Update_Dir_Recursive'Access);
      Register_Action
        (Kernel,
         "Update dir (recursively)",
         Command,
         -"Update the current directory recursively",
         VCS_Action_Context);

      Create (Command, Kernel_Handle (Kernel),
              On_Menu_Get_Status_Project_Recursive'Access);
      Register_Action
        (Kernel,
         "Status project (recursively)",
         Command,
         -"Query the status of the current project recursively",
         VCS_Action_Context);

      Create (Command, Kernel_Handle (Kernel),
              On_Menu_Update_Project_Recursive'Access);
      Register_Action
        (Kernel,
         "Update project (recursively)",
         Command,
         -"Update the current project recursively",
         VCS_Action_Context);
   end Register_Module;

   --------------------------
   -- Status_Parse_Handler --
   --------------------------

   procedure Status_Parse_Handler
     (Data    : in out Glide_Kernel.Scripts.Callback_Data'Class;
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

   -------------------------------
   -- Annotations_Parse_Handler --
   -------------------------------

   procedure Annotations_Parse_Handler
     (Data    : in out Glide_Kernel.Scripts.Callback_Data'Class;
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
   end Annotations_Parse_Handler;

   --------------------
   -- File_Edited_Cb --
   --------------------

   procedure File_Edited_Cb
     (Kernel  : access Kernel_Handle_Record'Class;
      Data    : Hooks_Data'Class)
   is
      use String_List_Utils.String_List;
      D : constant File_Hooks_Args := File_Hooks_Args (Data);
      Files  : List;
      Ref    : VCS_Access;
      Status : File_Status_Record;
   begin
      Ref    := Get_Current_Ref
        (Get_Project_From_File (Get_Registry (Kernel).all, D.File, True));

      if Ref = null then
         return;
      end if;

      Status := Get_Cached_Status
        (Get_Explorer (Kernel_Handle (Kernel), False), D.File, Ref);

      if Status.File = VFS.No_File then
         Append (Files, Full_Name (D.File).all);
         Get_Status (Ref, Files, False, Local => True);
         Free (Files);
      else
         Display_Editor_Status
           (Kernel_Handle (Kernel), Ref, Status);
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end File_Edited_Cb;

   ------------------
   -- Get_Explorer --
   ------------------

   function Get_Explorer
     (Kernel      : Kernel_Handle;
      Raise_Child : Boolean := True;
      Show        : Boolean := False) return VCS_View_Access
   is
      M : constant VCS_Module_ID_Access :=
            VCS_Module_ID_Access (VCS_Module_ID);
   begin
      if M.Explorer = null then
         Gtk_New (M.Explorer, Kernel);
      end if;

      if Show
        and then M.Explorer_Child = null
      then
         M.Explorer_Child := Put
           (Kernel, M.Explorer,
            Default_Width  => Get_Pref (Kernel, Default_Widget_Width),
            Default_Height => Get_Pref (Kernel, Default_Widget_Height),
            Module         => VCS_Module_ID);

         Set_Focus_Child (M.Explorer_Child);
         Set_Title (M.Explorer_Child, -"VCS Explorer");
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
      M : constant VCS_Module_ID_Access :=
            VCS_Module_ID_Access (VCS_Module_ID);
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
      M : constant VCS_Module_ID_Access :=
            VCS_Module_ID_Access (VCS_Module_ID);
   begin
      return M.Explorer /= null
        and then M.Explorer_Child /= null;
   end Explorer_Is_Open;

end VCS_Module;
