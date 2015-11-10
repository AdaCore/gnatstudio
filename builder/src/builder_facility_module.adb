------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2008-2015, AdaCore                     --
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

with Ada.Containers.Ordered_Maps;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Unchecked_Deallocation;

with GNAT.OS_Lib;               use GNAT.OS_Lib;

with Glib;                      use Glib;
with Glib.Object;               use Glib.Object;

with Gtk.Handlers;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Toolbar;               use Gtk.Toolbar;
with Gtk.Tool_Button;           use Gtk.Tool_Button;
with Gtk.Tool_Item;             use Gtk.Tool_Item;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Window;                use Gtk.Window;
with Gtkada.Combo_Tool_Button;  use Gtkada.Combo_Tool_Button;
with Gtkada.MDI;                use Gtkada.MDI;

with Projects;                  use Projects;

with Commands;                    use Commands;
with Commands.Interactive;        use Commands.Interactive;

with Build_Configurations;        use Build_Configurations;
with Build_Configurations.Gtkada; use Build_Configurations.Gtkada;

with GPS.Customizable_Modules;  use GPS.Customizable_Modules;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Actions;        use GPS.Kernel.Actions;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.Messages;       use GPS.Kernel.Messages;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;     use GPS.Kernel.Modules.UI;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel.Search;         use GPS.Kernel.Search;
with GPS.Main_Window;           use GPS.Main_Window;
with GPS.Search;                use GPS.Search;
with GUI_Utils;                 use GUI_Utils;
with String_Utils;              use String_Utils;

with GNATCOLL.Traces;           use GNATCOLL.Traces;
with GNATCOLL.Projects;         use GNATCOLL.Projects;
with GNATCOLL.Any_Types;        use GNATCOLL.Any_Types;

with Builder_Facility_Module.Scripts;
with Build_Command_Manager;     use Build_Command_Manager;

with Interactive_Consoles;      use Interactive_Consoles;
with Commands.Builder;          use Commands.Builder;
with XML_Utils;                 use XML_Utils;

with GPS.Tools_Output;          use GPS.Tools_Output;

with Build_Command_Manager.Console_Writers;
with Build_Command_Manager.Location_Parsers;
with Build_Command_Manager.End_Of_Build;
with Builder_Facility_Module.Output_Choppers;
with Builder_Facility_Module.Text_Splitters;
with Builder_Facility_Module.UTF8_Converters;
with Commands.Builder.Progress_Parsers;
with Commands.Builder.Build_Output_Collectors;
with GPS.Core_Kernels;

package body Builder_Facility_Module is

   Me          : constant Trace_Handle := Create ("Builder_Facility_Module");
   Modes_Trace : constant Trace_Handle :=
                   Create ("Builder.Modes", GNATCOLL.Traces.Off);

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
      (Any_Type, Any_Type_Access);

   type Target_And_Main is new Gtkada.Combo_Tool_Button.User_Data_Record
   with record
      Target : Unbounded_String;
      Main   : Virtual_File;
   end record;

   package String_Callback is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record, Target_And_Main);

   package Combo_Callback is new Gtk.Handlers.Callback
     (Gtkada_Combo_Tool_Button_Record);

   package Buttons_List is new Ada.Containers.Doubly_Linked_Lists
     (Gtk_Tool_Item);

   type Model_And_Target_XML is record
      Model_Name : Unbounded_String;
      XML        : Node_Ptr;
      From_User  : Boolean;
   end record;

   package Target_XML_List is new Ada.Containers.Doubly_Linked_Lists
     (Model_And_Target_XML);

   use Target_XML_List;

   package Target_Map is new Ada.Containers.Ordered_Maps
     (Unbounded_String, Target_XML_List.List);
   --  The key in this map is a model name, and the element is a set of XML
   --  describing targets associated with this model.

   package Unbounded_String_List is new Ada.Containers.Doubly_Linked_Lists
     (Unbounded_String);

   type Builder_Module_ID_Record is
     new GPS.Kernel.Modules.Module_ID_Record
   with record
      Registry : Build_Config_Registry_Access;

      Buttons  : Buttons_List.List;
      --  The set of toolbar buttons

      Unregistered_Targets : Target_XML_List.List;
      --  This is a set of targets that could not be registered because their
      --  model was not registered at the time the module parsed this target.
      --  Every time a new model is registered, the module will look at this
      --  list of unregistered targets, and register all targets corresponding
      --  to the known models.
      --  Doing this means that targets can be passed to the module *before*
      --  their corresponding models. This way, there is no load order to
      --  maintain, and scripts can be executed in any order.

      Currently_Saving : Boolean := False;
      --  Whether the module is currently reacting to the File_Saved hook.
      --  See implementation of On_File_Save.

      Actions : Unbounded_String_List.List;
      --  The list of currently registered builder actions

      Menus : Unbounded_String_List.List;
      --  The set of menu items that need to be removed when reloading the
      --  targets

      Build_Count : Natural := 0;
      --  The number of builds currently running

      Prevent_Save_Reentry : Boolean := False;
      --  Used to prevent cases where a compilation is triggered "on file save"
      --  and the "file save" is caused by another compilation

      Output_Chopper   : aliased Output_Choppers.Output_Parser_Fabric;
      Text_Splitter    : aliased Text_Splitters.Output_Parser_Fabric;
      UTF8_Converter   : aliased UTF8_Converters.Output_Parser_Fabric;
      Progress_Parser  : aliased Progress_Parsers.Output_Parser_Fabric;
      Output_Collector : aliased Build_Output_Collectors.Output_Parser_Fabric;
      Console_Writer   : aliased Console_Writers.Output_Parser_Fabric;
      Location_Parser  : aliased Location_Parsers.Output_Parser_Fabric;
      Build_Hook       : aliased End_Of_Build.Output_Parser_Fabric;

      Builder          : aliased Builder_Context_Record;
   end record;

   type Builder_Module_ID_Access is access all Builder_Module_ID_Record'Class;
   --  Data stored with the module id

   overriding procedure Destroy (Module : in out Builder_Module_ID_Record);

   Builder_Module_ID : Builder_Module_ID_Access;

   type Builder_Contextual is new Submenu_Factory_Record with null record;
   overriding procedure Append_To_Menu
     (Builder : access Builder_Contextual;
      Context : Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);

   type Run_Contextual is new Submenu_Factory_Record with null record;
   overriding procedure Append_To_Menu
     (Builder : access Run_Contextual;
      Context : Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);

   ---------------
   -- Searching --
   ---------------

   type Target_Cursor_Access is access all Target_Cursor;
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
      (Target_Cursor, Target_Cursor_Access);

   type Builder_Search_Provider is new Kernel_Search_Provider with record
      Pattern : Search_Pattern_Access;
      Iter    : Target_Cursor_Access;
      Mains   : Any_Type_Access;
      Current_Main : Integer;
   end record;
   overriding procedure Free (Self : in out Builder_Search_Provider);
   overriding procedure Set_Pattern
      (Self     : not null access Builder_Search_Provider;
       Pattern  : not null access GPS.Search.Search_Pattern'Class;
       Limit    : Natural := Natural'Last);
   overriding procedure Next
      (Self     : not null access Builder_Search_Provider;
       Result   : out GPS.Search.Search_Result_Access;
       Has_Next : out Boolean);
   overriding function Display_Name
      (Self     : not null access Builder_Search_Provider) return String
      is (Provider_Builds);
   overriding function Documentation
      (Self     : not null access Builder_Search_Provider) return String;
   overriding function Complete_Suffix
     (Self      : not null access Builder_Search_Provider;
      Pattern   : not null access GPS.Search.Search_Pattern'Class)
      return String;

   procedure Setup
      (Self : not null access Builder_Search_Provider'Class);
   --  Preparate internal data for the current target

   type Builder_Search_Result is new Kernel_Search_Result with record
      Target : Target_Access;
      Main   : Virtual_File;
   end record;
   overriding procedure Execute
      (Self       : not null access Builder_Search_Result;
       Give_Focus : Boolean);

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Log (M : String; Mode : Message_Mode);
   --  Logger for the registry

   function Get_Kernel return Kernel_Handle;
   --  Utility function to get the kernel

   type Targets_Settings_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Targets_Settings_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Launch the build manager

   type Modes_Settings_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Modes_Settings_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Launch the mode manager

   type Shadow_Console_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Shadow_Console_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Launch the shadow console

   type Background_Builds_Console_Command
      is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Background_Builds_Console_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Open the background console

   procedure Auxiliary_Console
     (Kernel     : Kernel_Handle;
      Background : Boolean;
      Shadow     : Boolean);
   --  Code factorization between On_Shadow_Console and On_Background_Console

   overriding procedure Customize
     (Module : access Builder_Module_ID_Record;
      File   : GNATCOLL.VFS.Virtual_File;
      Node   : XML_Utils.Node_Ptr;
      Level  : Customization_Level);
   --  See inherited documentation

   procedure Clear_Toolbar_Buttons;
   --  Remove the build-related buttons from the main toolbar

   procedure Install_Toolbar_Buttons;
   --  Install the build-related buttons into the main toolbar

   procedure Install_Button_For_Target (Target : Target_Access);
   --  Install one button in the toolbar

   procedure Clear_Actions_And_Menus;
   --  Remove the target build menus

   function Get_Targets_File return GNATCOLL.VFS.Virtual_File;
   --  Return the file where user targets are stored

   procedure Attempt_Target_Register (XML : Node_Ptr; From_User : Boolean);
   --  Attempt to register target described in XML. If the model is not
   --  registered yet, add this target to the list of unregistered targets.
   --  From_User indicates whether the target is loaded from the user saved
   --  file.

   procedure On_Button_Or_Menu_Click
     (Widget : access Gtk_Widget_Record'Class;
      Data   : Target_And_Main);
   --  Called when a user clicks on a toolbar button.
   --  Name is the name of the target corresponding to that button.

   procedure On_Combo_Click
     (Widget : access Gtkada_Combo_Tool_Button_Record'Class);
   --  Called when a user clicks on a toolbar combo button

   procedure On_Combo_Selection
     (Widget : access Gtkada_Combo_Tool_Button_Record'Class);
   --  Called when a user selects a new item from the combo

   type On_File_Saved is new File_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_File_Saved;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File);
   --  Called when a file has been saved

   type On_Buffer_Modified is new File_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Buffer_Modified;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File);
   --  Called when a buffer has been modified

   procedure File_Saved_Or_Buffer_Modified
     (Kernel : access Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Saved  : Boolean);
   --  Factor code between On_File_Saved and On_Buffer_Modified

   procedure Clear_Compilation_Output
     (Kernel          : Kernel_Handle;
      Category        : String;
      Clear_Console   : Boolean;
      Clear_Locations : Boolean;
      Shadow          : Boolean;
      Background      : Boolean);
   --  Clear the compiler output, the console, and the locations view for
   --  Category.

   type On_Compilation_Starting is new Compilation_Hooks_Function
      with null record;
   overriding function Execute
      (Self   : On_Compilation_Starting;
       Kernel : not null access Kernel_Handle_Record'Class;
       Category : String;
       Quiet, Shadow, Background : Boolean) return Boolean;
   --  Called when the compilation is starting

   type On_Compilation_Finished is new Compilation_Finished_Hooks_Function
      with null record;
   overriding procedure Execute
      (Self   : On_Compilation_Finished;
       Kernel : not null access Kernel_Handle_Record'Class;
       Category, Target, Mode : String;
       Shadow, Background : Boolean;
       Status : Integer);
   --  Called when the compilation has ended

   type On_GPS_Started is new Simple_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_GPS_Started;
      Kernel : not null access Kernel_Handle_Record'Class);
   --  Called when GPS is starting

   type On_View_Changed is new Simple_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_View_Changed;
      Kernel : not null access Kernel_Handle_Record'Class);
   --  Called every time the project view has changed, ie potentially the list
   --  of main units.

   type On_Compute_Targets is new String_Return_Any_Hooks_Function
      with null record;
   overriding function Execute
      (Self   : On_Compute_Targets;
       Kernel : not null access Kernel_Handle_Record'Class;
       Kind   : String) return GNATCOLL.Any_Types.Any_Type;
   --  Called when computing build targets

   type On_Build_Mode_Changed is new String_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Build_Mode_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Mode   : String);
   --  Called when the build mode is being changed by the user

   procedure Add_Action_And_Menu_For_Target (Target : Target_Access);
   --  Register a Kernel Action to build T and create a menu item to build
   --  Target

   procedure Add_Actions_And_Menus_For_All_Targets;
   --  Register Kernel Actions for all targets

   procedure Parse_Mode_Node (XML : Node_Ptr);
   --  Parse XML node describing a mode. See spec for a description of the
   --  XML format.

   type Dynamic_Menu_Item_Record is new Gtk_Menu_Item_Record with null record;
   type Dynamic_Menu_Item is access all Dynamic_Menu_Item_Record'Class;
   --  So that items created for the dynamic Make and Run menus have a special
   --  type, and we only remove these when refreshing the menu

   type Contextual_Menu_Type is (Build_Targets, Run_Targets);
   procedure Append_To_Contextual_Menu
     (Menu_Type   : Contextual_Menu_Type;
      Context     : Selection_Context;
      Menu        : access Gtk.Menu.Gtk_Menu_Record'Class);

   -------------------------------
   -- Append_To_Contextual_Menu --
   -------------------------------

   procedure Append_To_Contextual_Menu
     (Menu_Type   : Contextual_Menu_Type;
      Context     : Selection_Context;
      Menu        : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      For_Files   : constant Boolean := Has_File_Information (Context);
      For_Project : constant Boolean := Has_Project_Information (Context)
        and then not For_Files
        and then not Has_Directory_Information (Context);

      C : Target_Cursor := Get_First_Target (Builder_Module_ID.Registry);
      T : Target_Access;

      procedure Add_Contextual (T : Target_Access);
      --  Add a contextual menu for T

      --------------------
      -- Add_Contextual --
      --------------------

      procedure Add_Contextual (T : Target_Access) is
         Mitem   : Dynamic_Menu_Item;
         Targets : Unbounded_String;
      begin
         Targets := Get_Properties (T).Target_Type;

         if Targets /= Null_Unbounded_String then
            declare
               Mains  : Any_Type :=
                  Compute_Build_Targets_Hook.Run
                     (Get_Kernel, Str => To_String (Targets));
            begin
               for J in 1 .. Mains.Length loop
                  if Mains.List (J).Length /= 0 then
                     declare
                        Display : constant String :=
                          Mains.List (J).Tuple (1).Str;
                        Full    : constant String :=
                          Mains.List (J).Tuple (2).Str;
                        Prj     : constant Virtual_File :=
                          Create (+Mains.List (J).Tuple (3).Str);
                     begin
                        if not Has_Project_Information (Context)
                          or else
                            Project_Information (Context).Project_Path = Prj
                        then
                           Mitem := new Dynamic_Menu_Item_Record;
                           Gtk.Menu_Item.Initialize
                             (Mitem, Get_Name (T) & ": " & Display);
                           Prepend (Menu, Mitem);
                           String_Callback.Connect
                             (Mitem, Signal_Activate,
                              On_Button_Or_Menu_Click'Access,
                              (To_Unbounded_String (Get_Name (T)),
                               Create (+Full)));
                        end if;
                     end;
                  end if;
               end loop;
               Free (Mains);
            end;

         else
            Mitem := new Dynamic_Menu_Item_Record;
            Gtk.Menu_Item.Initialize (Mitem, Get_Name (T));
            Prepend (Menu, Mitem);

            String_Callback.Connect
              (Mitem, Signal_Activate,
               On_Button_Or_Menu_Click'Access,
               (To_Unbounded_String (Get_Name (T)),
                No_File));
         end if;
      end Add_Contextual;

   begin
      loop
         T := Get_Target (C);
         exit when T = null;

         if (Menu_Type = Run_Targets and then Is_Run (T))
           or else (Menu_Type = Build_Targets and then not Is_Run (T))
         then
            if (For_Files
                and then Get_Properties (T).In_Contextual_Menu_For_Files
                and then Get_Properties (T).Visible)
              or else
                (For_Project
                 and then Get_Properties (T).In_Contextual_Menu_For_Projects
                 and then Get_Properties (T).Visible)
            then
               Add_Contextual (T);
            end if;
         end if;

         Next (C);
      end loop;
   end Append_To_Contextual_Menu;

   --------------------
   -- Append_To_Menu --
   --------------------

   overriding procedure Append_To_Menu
     (Builder : access Builder_Contextual;
      Context : Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      pragma Unreferenced (Builder);
      --  The filter guarantees we are on a File_Selection_Context
   begin
      Append_To_Contextual_Menu (Build_Targets, Context, Menu);
   end Append_To_Menu;

   --------------------
   -- Append_To_Menu --
   --------------------

   overriding procedure Append_To_Menu
     (Builder : access Run_Contextual;
      Context : Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      pragma Unreferenced (Builder);
      --  The filter guarantees we are on a File_Selection_Context
   begin
      Append_To_Contextual_Menu (Run_Targets, Context, Menu);
   end Append_To_Menu;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Module : in out Builder_Module_ID_Record) is
   begin
      Module.Builder.Destroy;
      Free (Module.Registry);
   end Destroy;

   ------------------------------------
   -- Add_Action_And_Menu_For_Target --
   ------------------------------------

   procedure Add_Action_And_Menu_For_Target (Target : Target_Access) is
      Kernel   : constant Kernel_Handle := Get_Kernel;
      C        : Build_Command_Access;
      M        : Build_Main_Command_Access;
      N        : constant String := Get_Name (Target);
      Action   : Unbounded_String;
      Category : constant String := Get_Category (Target);
      Targets  : constant Unbounded_String :=
        Get_Properties (Target).Target_Type;

      Toplevel_Menu : constant Boolean := Category (Category'First) = '_'
        and then Category (Category'Last) = '_';

      Cat_Path : constant String :=
        Strip_Single_Underscores (Get_Parent_Menu_Name (Target))
        & (if Toplevel_Menu then "" else Category & "/");
      --  For instance:  /Build/Project/

      procedure Menu_For_Action
        (Main         : Virtual_File;
         Project      : Project_Type;
         Menu_Name    : String;
         Action_Name  : String);
      --  Add a menu at Parent_Path for target named Name, with menu name
      --  Menu_Name

      procedure Menu_For_Action
        (Main         : Virtual_File;
         Project      : Project_Type;
         Menu_Name    : String;
         Action_Name  : String)
      is
         Mnemonics : constant Boolean := Main = No_File;
         --  Always protect underscores in menu name when dealing with file
         --  names, but not otherwise.

         Path : constant String :=
           Cat_Path
           & (if Project = No_Project
              then ""
              else Escape_Menu_Name (Escape_Underscore (Project.Name)) & '/')
           & Escape_Menu_Name
            ((if Mnemonics then Menu_Name else Escape_Underscore (Menu_Name)));
      begin
         --  Do nothing is the target is not supposed to be shown in the menu
         if not Get_Properties (Target).In_Menu
           or else not Get_Properties (Target).Visible
         then
            return;
         end if;

         Register_Menu
           (Kernel,
            Path          => Path,
            Action        => Action_Name,
            Ref_Item      => "Project");

         Builder_Module_ID.Menus.Prepend
           (To_Unbounded_String (Strip_Single_Underscores (Path)));
      end Menu_For_Action;

   begin
      if Targets /= Null_Unbounded_String then
         --  Register the "build main number x"-like actions

         declare
            Mains  : Any_Type :=
               Compute_Build_Targets_Hook.Run (Kernel, To_String (Targets));
            D : Dialog_Mode;

         begin
            if Mains.Length > 0
              and then Mains.T /= List_Type
            then
               Insert
                 (Kernel,
                  (-"The command for determining the target type of target " &
                   To_String (Targets) & (-" returned a ") & Mains.T'Img
                     & (-("but should return a LIST_TYPE "
                       & " (containing a pair display_name/full_name)"))),
                  Mode => Error);

            else
               case Get_Properties (Target).Launch_Mode is
                  when Manually | On_File_Save | In_Background =>
                     D := Default;
                  when Manually_With_Dialog =>
                     D := Force_Dialog;
                  when Manually_With_No_Dialog =>
                     D := Force_No_Dialog;
               end case;

               for J in 1 .. Mains.Length loop
                  if Mains.List (J).Length /= 0 then
                     Create (Item        => M,
                             Builder     => Builder_Module_ID.Builder'Access,
                             Target_Name => N,
                             Target_Type => To_String (Targets),
                             Main        => J,
                             Quiet       => False,
                             Dialog      => D);
                     Set_Unbounded_String (Action, N & (-" Number") & J'Img);
                     Unregister_Action (Kernel, To_String (Action));
                     Register_Action
                       (Kernel      => Kernel,
                        Name        => To_String (Action),
                        Command     => M,
                        Description => To_String (Action),
                        Icon_Name   => Get_Icon_Name (Target),
                        Category    => -"Build");
                     Builder_Module_ID.Actions.Append (Action);
                     Menu_For_Action
                       (Main         => Create (+Mains.List (J).Tuple (2).Str),
                        Action_Name  => To_String (Action),
                        Project      => Kernel.Registry.Tree.Project_From_Path
                          (Create (+Mains.List (J).Tuple (3).Str)),
                        Menu_Name    => Mains.List (J).Tuple (1).Str);
                  end if;
               end loop;

               --  Make sure we create at least those actions so that users
               --  can associate shortcuts to these.

               for J in Mains.Length + 1 .. 4 loop
                  Unregister_Action (Kernel, N & (-" Number") & J'Img);
                  Register_Action
                    (Kernel      => Kernel,
                     Name        => N & (-" Number") & J'Img,
                     Command     => null,
                     Category    => -"Build");
                  Builder_Module_ID.Actions.Append (Action);
               end loop;
            end if;

            Free (Mains);
         end;

      else
         Create
           (C, Builder_Module_ID.Builder'Access, N, No_File, False, Default);
         Unregister_Action (Kernel, N);
         Register_Action (Kernel      => Kernel,
                          Name        => N,
                          Command     => C,
                          Description => (-"Build target ") & N,
                          Icon_Name   => Get_Icon_Name (Target),
                          Category    => -"Build");
         Builder_Module_ID.Actions.Append (To_Unbounded_String (N));
         Menu_For_Action (Main         => No_File,
                          Project      => No_Project,
                          Menu_Name    => Get_Menu_Name (Target),
                          Action_Name  => N);
      end if;
   end Add_Action_And_Menu_For_Target;

   -------------------------------------------
   -- Add_Actions_And_Menus_For_All_Targets --
   -------------------------------------------

   procedure Add_Actions_And_Menus_For_All_Targets is
      C : Target_Cursor := Get_First_Target (Builder_Module_ID.Registry);
      T : Target_Access;
   begin
      loop
         T := Get_Target (C);
         exit when T = null;

         Add_Action_And_Menu_For_Target (T);
         Next (C);
      end loop;
   end Add_Actions_And_Menus_For_All_Targets;

   ----------------------
   -- Get_Targets_File --
   ----------------------

   function Get_Targets_File return GNATCOLL.VFS.Virtual_File is
   begin
      return Create_From_Dir (Get_Home_Dir (Get_Kernel), "targets.xml");
   end Get_Targets_File;

   ------------------
   -- Save_Targets --
   ------------------

   procedure Save_Targets is
      N       : Node_Ptr;
      Success : Boolean;
   begin
      N := Save_All_Targets_To_XML (Builder_Module_ID.Registry);
      Print (N, Get_Targets_File, Success);

      if not Success then
         Trace (Me, "Error when saving targets file");
      end if;
   end Save_Targets;

   ------------------
   -- Load_Targets --
   ------------------

   procedure Load_Targets is
      N : Node_Ptr;
      C : Node_Ptr;
   begin
      N := Parse (Get_Targets_File);

      if N = null then
         Trace (Me, "Error when loading targets file");
      else
         C := N.Child;

         while C /= null loop
            if C.Tag.all /= "target" then
               Trace (Me, "Error in targets file");
               return;
            end if;

            Attempt_Target_Register (C, True);

            C := C.Next;
         end loop;
      end if;

      Free (N);

      Refresh_Graphical_Elements;
   end Load_Targets;

   ------------------------------
   -- Clear_Compilation_Output --
   ------------------------------

   procedure Clear_Compilation_Output
     (Kernel          : Kernel_Handle;
      Category        : String;
      Clear_Console   : Boolean;
      Clear_Locations : Boolean;
      Shadow          : Boolean;
      Background      : Boolean)
   is
      Console : Interactive_Console;

   begin
      if Clear_Console then
         Console := Get_Build_Console (Kernel, Shadow, Background, False);

         if Console /= null then
            Clear (Console);
         end if;
      end if;

      if Clear_Locations
        and then not Background
      then
         Get_Messages_Container (Kernel).Remove_Category
           (Category, Builder_Message_Flags);
      end if;

      Builder_Module_ID.Builder.Clear_Build_Output (Shadow, Background);
   end Clear_Compilation_Output;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_GPS_Started;
      Kernel : not null access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Self, Kernel);
   begin
      Load_Targets;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
      (Self   : On_Compilation_Starting;
       Kernel : not null access Kernel_Handle_Record'Class;
       Category : String;
       Quiet, Shadow, Background : Boolean) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      --  Small issue here: if the user cancels the compilation in one of the
      --  custom hooks the user might have connected, then all changes done
      --  here (increase Build_Count) will not be undone since
      --  On_Compilation_Finished is not called.

      --  Ask for saving sources/projects before building.
      --  Do this before checking the project, in case we have a default
      --  project whose name is changed when saving

      Builder_Module_ID.Prevent_Save_Reentry := True;
      if not (Quiet or else Shadow)
        and then not Save_MDI_Children (Kernel, Force => Auto_Save.Get_Pref)
      then
         return False;
      end if;
      Builder_Module_ID.Prevent_Save_Reentry := False;

      Clear_Compilation_Output
        (Kernel_Handle (Kernel),
         Category        => Category,
         Clear_Console   => (not Quiet)
           and then (Shadow or else Builder_Module_ID.Build_Count = 0),
         Clear_Locations => (not Quiet)
           and then Builder_Module_ID.Build_Count = 0,
         Shadow          => Shadow,
         Background      => Background);

      Builder_Module_ID.Build_Count := Builder_Module_ID.Build_Count + 1;

      return True;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_View_Changed;
      Kernel : not null access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Self, Kernel);
   begin
      Refresh_Graphical_Elements;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
      (Self   : On_Compilation_Finished;
       Kernel : not null access Kernel_Handle_Record'Class;
       Category, Target, Mode : String;
       Shadow, Background : Boolean;
       Status : Integer)
   is
      pragma Unreferenced (Self, Kernel, Category, Target, Mode, Shadow);
      pragma Unreferenced (Background, Status);
   begin
      if Builder_Module_ID.Build_Count > 0 then
         Builder_Module_ID.Build_Count := Builder_Module_ID.Build_Count - 1;
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
      (Self   : On_Compute_Targets;
       Kernel : not null access Kernel_Handle_Record'Class;
       Kind   : String) return GNATCOLL.Any_Types.Any_Type
   is
      pragma Unreferenced (Self);
   begin
      if Kind = "main" then
         declare
            Mains  : constant Project_And_Main_Array :=
               Get_Mains (Get_Registry (Kernel_Handle (Kernel)));
            Result : Any_Type (List_Type, Mains'Length);
         begin
            for J in Mains'Range loop
               declare
                  Base : constant String := Mains (J).Main.Display_Base_Name;
                  Full : constant String := +Mains (J).Main.Full_Name;
                  P_Name : constant String :=
                    +Mains (J).Project.Project_Path.Full_Name;
                  Display_Name : constant Any_Type :=
                    (String_Type, Base'Length, Base);
                  Full_Name    : constant Any_Type :=
                    (String_Type, Full'Length, Full);
                  Project_Name : constant Any_Type :=
                    (String_Type, P_Name'Length, P_Name);
               begin
                  Result.List (1 + J - Mains'First) := new Any_Type'
                    ((Tuple_Type, 3,
                     Tuple => (1 => new Any_Type'(Display_Name),
                               2 => new Any_Type'(Full_Name),
                               3 => new Any_Type'(Project_Name))));
               end;
            end loop;

            return Result;
         end;

      elsif Kind = "executable" then
         declare
            Mains  : constant Project_And_Main_Array :=
               Get_Mains (Get_Registry (Kernel_Handle (Kernel)));
            Result : Any_Type (List_Type, Mains'Length);
         begin
            for J in Mains'Range loop
               if Mains (J).Project = No_Project then
                  --  This can happen when the project can not find the source
                  --  corresponding to the main file, for instance
                  --  badly-written user projects, or projects that are created
                  --  on-the-fly with the --debug= command line switch.
                  Trace
                    (Me,
                     (-"Could not find the project for """
                      & Mains (J).Main.Display_Full_Name & """"));

                  return Empty_Any_Type;
               elsif Executables_Directory (Mains (J).Project)
                 = GNATCOLL.VFS.No_File
               then
                  Log (-"Project """ & Mains (J).Project.Name
                       & """ has no exec_dir", Error);
                  return Empty_Any_Type;
               else
                  declare
                     Exec : constant Virtual_File :=
                       Create_From_Dir
                         (Executables_Directory (Mains (J).Project),
                          Mains (J).Project.Executable_Name
                          (Mains (J).Main.Full_Name));
                     Base : constant String := String (Exec.Base_Name);
                     Full : constant String := String (Exec.Full_Name.all);
                     P_Name : constant String :=
                       +Mains (J).Project.Project_Path.Full_Name;
                     Display_Name : constant Any_Type :=
                       (String_Type, Base'Length, Base);
                     Full_Name    : constant Any_Type :=
                       (String_Type, Full'Length, Full);
                     Project_Name : constant Any_Type :=
                       (String_Type, P_Name'Length, P_Name);

                  begin
                     Result.List (1 + J - Mains'First) := new Any_Type'
                       ((Tuple_Type, 3,
                        Tuple => (1 => new Any_Type'(Display_Name),
                                  2 => new Any_Type'(Full_Name),
                                  3 => new Any_Type'(Project_Name))));
                  end;
               end if;
            end loop;

            return Result;

         exception
            when GNATCOLL.VFS.VFS_Invalid_File_Error =>
               Log
                 (-"Could not determine executable names for the mains",
                  Error);

               return Empty_Any_Type;
         end;

      else
         return Empty_Any_Type;
      end if;
   end Execute;

   --------------------------------
   -- Refresh_Graphical_Elements --
   --------------------------------

   procedure Refresh_Graphical_Elements is
   begin
      --  Recreate the actions
      Clear_Actions_And_Menus;

      Add_Actions_And_Menus_For_All_Targets;

      Clear_Toolbar_Buttons;
      Install_Toolbar_Buttons;
   end Refresh_Graphical_Elements;

   -----------------------------------
   -- File_Saved_Or_Buffer_Modified --
   -----------------------------------

   procedure File_Saved_Or_Buffer_Modified
     (Kernel : access Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Saved  : Boolean)
   is
      C          : Build_Configurations.Target_Cursor :=
        Get_First_Target (Builder_Module_ID.Registry);
      T          : Target_Access;
      Background : constant Boolean := not Saved;
   begin
      --  If the current "save" operation is actually caused by the launch of
      --  a build operation, do not build in response to this save, as this
      --  would mean multiple builds (most likely of the same file) in
      --  parallel.
      if Saved
        and then Builder_Module_ID.Prevent_Save_Reentry
      then
         return;
      end if;

      if Background then
         --  If there is a category in the locations view that contains the
         --  build errors, do not launch a background build.

         if Has_Category
           (Get_Messages_Container (Kernel), Error_Category)
         then
            return;
         end if;
      end if;

      --  We run this hook only when a source file has changed.
      --  For other files (for instance revision logs), we do not want to
      --  compile.

      declare
         P      : Project_Type;
         F_Info : constant File_Info'Class :=
           File_Info'Class
             (Get_Registry (Kernel).Tree.Info_Set (File).First_Element);
      begin
         P := F_Info.Project;

         --  No project was found for the file: this is not a source file, so
         --  return now.
         if P = No_Project then
            Builder_Module_ID.Currently_Saving := False;
            return;
         end if;
      end;

      loop
         T := Get_Target (C);
         exit when T = null;

         if (Saved and then Get_Properties (T).Launch_Mode = On_File_Save)
           or else (not Saved
                    and then Get_Properties (T).Launch_Mode = In_Background)
         then
            Launch_Target (Builder      => Builder_Module_ID.Builder'Access,
                           Target_Name  => Get_Name (T),
                           Mode_Name    => "",
                           Force_File   => File,
                           Extra_Args   => null,
                           Quiet        => True,
                           Synchronous  => False,
                           Background   => Background,
                           Dialog       => Default,
                           Via_Menu     => False,
                           Main         => No_File);
            --  ??? Should we attempt to compute which is the "relevant" main
            --  in On_File_Save mode?
         end if;
         Next (C);
      end loop;
   end File_Saved_Or_Buffer_Modified;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_File_Saved;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File)
   is
      pragma Unreferenced (Self);
   begin
      --  Protect against the following recursion:
      --   a script connects the action Compilation_Starting to the saving
      --   of a file, and this causes Compile through this procedure.
      --  This should not happen in GPS, but we protect against this
      --  possibility occurring in user scripts.

      if Builder_Module_ID.Currently_Saving then
         return;
      end if;

      Builder_Module_ID.Currently_Saving := True;
      File_Saved_Or_Buffer_Modified (Kernel, File, Saved => True);
      Builder_Module_ID.Currently_Saving := False;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Buffer_Modified;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File)
   is
      pragma Unreferenced (Self);
   begin
      File_Saved_Or_Buffer_Modified (Kernel, File, Saved => False);
   end Execute;

   -----------------------------
   -- Attempt_Target_Register --
   -----------------------------

   procedure Attempt_Target_Register
     (XML       : Node_Ptr;
      From_User : Boolean)
   is
      Model_Name : Unbounded_String;
      T          : Target_Access;
      use Target_Map;

   begin
      Model_Name := To_Unbounded_String (Get_Attribute (XML, "model", ""));

      --  Sanity check
      if Model_Name = "" then
         Log (-"Error: target XML description has an empty model", Error);
         return;
      end if;

      --  Check whether the model is registered
      if Is_Registered_Model (Builder_Module_ID.Registry, Model_Name) then
         --  The model is registered: add the target immediately

         T := Load_Target_From_XML
           (Builder_Module_ID.Registry, XML, From_User);

         --  The target might already be registered, if we are calling this
         --  with Allow_Update (for example when a target coming from the
         --  targets.xml file overrides the default values).
         --  In this case, we do not want to create the actions, menus, and
         --  toolbars a second time.
         --  We test whether we have previously registered this target by
         --  looking it up in the registered actions.

         if T /= null
           and then not Builder_Module_ID.Actions.Contains
             (To_Unbounded_String (Get_Name (T)))
           and then (Length (Get_Properties (T).Target_Type) = 0
                     or else not Builder_Module_ID.Actions.Contains
                       (To_Unbounded_String (Get_Name (T) & (-" Number 1"))))
         then
            Add_Action_And_Menu_For_Target (T);
            Install_Button_For_Target (T);
         end if;

      else
         --  The model is not registered: add XML to the list of unregistered
         --  targets

         if From_User then
            Builder_Module_ID.Unregistered_Targets.Append
              (Model_And_Target_XML'(Model_Name, Deep_Copy (XML), From_User));
         else
            Builder_Module_ID.Unregistered_Targets.Prepend
              (Model_And_Target_XML'(Model_Name, Deep_Copy (XML), From_User));
         end if;
      end if;
   end Attempt_Target_Register;

   ---------
   -- Log --
   ---------

   procedure Log (M : String; Mode : Message_Mode) is
      Kernel  : constant Kernel_Handle := Get_Kernel (Builder_Module_ID.all);
      Message : constant String := (-"Build facility: ") & M;
   begin
      case Mode is
         when Info =>
            Insert (Kernel, Message, Mode => Info);
         when Error =>
            Insert (Kernel, Message, Mode => Error);
         when Trace =>
            Trace (Me, Message);
      end case;
   end Log;

   ----------------
   -- Get_Kernel --
   ----------------

   function Get_Kernel return Kernel_Handle is
   begin
      return Get_Kernel (Builder_Module_ID.all);
   end Get_Kernel;

   ---------------------------
   -- Clear_Toolbar_Buttons --
   ---------------------------

   procedure Clear_Toolbar_Buttons is
      Toolbar : constant Gtk_Toolbar := Get_Toolbar (Get_Kernel);
      use Buttons_List;

      C : Buttons_List.Cursor;
   begin
      if Toolbar /= null then
         --  Browse through the registry and remove already added buttons

         C := Builder_Module_ID.Buttons.First;

         while Has_Element (C) loop
            Remove (Toolbar, Element (C));
            Next (C);
         end loop;
      end if;

      --  Remove all buttons from registry

      Builder_Module_ID.Buttons.Clear;
   end Clear_Toolbar_Buttons;

   -----------------------------
   -- On_Button_Or_Menu_Click --
   -----------------------------

   procedure On_Button_Or_Menu_Click
     (Widget : access Gtk_Widget_Record'Class;
      Data   : Target_And_Main)
   is
      pragma Unreferenced (Widget);
   begin
      Launch_Target
        (Builder_Module_ID.Builder'Access,
         To_String (Data.Target),
         "",
         No_File,
         null, False, False, Default, False, Data.Main, False);
   exception
      when E : others =>
         Trace (Me, E);
   end On_Button_Or_Menu_Click;

   --------------------
   -- On_Combo_Click --
   --------------------

   procedure On_Combo_Click
     (Widget : access Gtkada_Combo_Tool_Button_Record'Class)
   is
      Data : constant Gtkada.Combo_Tool_Button.User_Data :=
               Get_Selected_Item_Data (Widget);
   begin
      if Data /= null then
         Launch_Target
           (Builder_Module_ID.Builder'Access,
            To_String (Target_And_Main (Data.all).Target),
            "",
            No_File,
            null, False, False, Default, False,
            Target_And_Main (Data.all).Main, False);
      end if;

   exception
      when E : others =>
         Trace (Me, E);
   end On_Combo_Click;

   ------------------------
   -- On_Combo_Selection --
   ------------------------

   procedure On_Combo_Selection
     (Widget : access Gtkada_Combo_Tool_Button_Record'Class)
   is
      Data : constant Gtkada.Combo_Tool_Button.User_Data :=
               Get_Selected_Item_Data (Widget);
   begin
      Set_Tooltip_Text
        (Widget,
         To_String (Target_And_Main (Data.all).Target) &
         ": " & Get_Selected_Item (Widget));
   end On_Combo_Selection;

   -------------------------------
   -- Install_Button_For_Target --
   -------------------------------

   procedure Install_Button_For_Target (Target : Target_Access) is
      Toolbar : constant Gtk_Toolbar   := Get_Toolbar (Get_Kernel);
      Pos     : constant Glib.Gint := Get_Toolbar_Section
        (Get_Kernel, null, "build");
      Button : Gtk.Tool_Item.Gtk_Tool_Item;

      procedure Button_For_Target
        (Name  : String;
         Mains : Any_Type);
      --  Create one button for target Name and main Main

      -----------------------
      -- Button_For_Target --
      -----------------------

      procedure Button_For_Target
        (Name  : String;
         Mains : Any_Type) is
      begin
         --  In case only one main is available, create a simple button
         if Mains.Length <= 1 then
            declare
               Widget : Gtk.Tool_Button.Gtk_Tool_Button;
               Main   : Virtual_File;
            begin
               Gtk_New (Widget);
               Widget.Set_Icon_Name (Get_Icon_Name (Target));
               Set_Label (Widget, Name);

               if Mains.Length = 0 then
                  Main := No_File;
                  Set_Tooltip_Text (Widget, Name);
               else
                  Main := Create (+Mains.List (1).Tuple (2).Str);
                  Set_Tooltip_Text
                    (Widget,
                     Name & ": " & Mains.List (1).Tuple (1).Str);
               end if;

               Set_Name (Widget, "toolbar_button_" & Name);

               String_Callback.Connect
                 (Widget, Gtk.Tool_Button.Signal_Clicked,
                  On_Button_Or_Menu_Click'Access,
                  (To_Unbounded_String (Name), Main));
               Button := Gtk_Tool_Item (Widget);
            end;

         else
            declare
               Widget : Gtkada.Combo_Tool_Button.Gtkada_Combo_Tool_Button;
            begin
               Gtk_New (Widget, Icon_Name => Get_Icon_Name (Target));

               --  Connect to this signal to automatically update the tooltips
               --  when a new main file is selected
               Combo_Callback.Connect
                 (Widget, Signal_Selection_Changed,
                  On_Combo_Selection'Access);

               for J in Mains.List'Range loop
                  Widget.Add_Item
                    (Mains.List (J).Tuple (2).Str, Get_Icon_Name (Target),
                     new Target_And_Main'
                       (To_Unbounded_String (Name),
                        Create (+Mains.List (J).Tuple (2).Str)));
               end loop;

               Combo_Callback.Connect
                 (Widget, Gtkada.Combo_Tool_Button.Signal_Clicked,
                  On_Combo_Click'Access);
               Button := Gtk_Tool_Item (Widget);
            end;
         end if;

         Builder_Module_ID.Buttons.Prepend (Button);
         Insert (Toolbar => Toolbar, Item => Button, Pos => Pos);
         Show_All (Button);
      end Button_For_Target;

      Targets : Unbounded_String;

   begin
      if Target = null
        or else Toolbar = null
        or else not Get_Properties (Target).In_Toolbar
        or else not Get_Properties (Target).Visible
      then
         return;
      end if;

      Targets := Get_Properties (Target).Target_Type;

      if Targets /= Null_Unbounded_String then
         declare
            Mains : Any_Type := Compute_Build_Targets_Hook.Run
               (Get_Kernel, To_String (Targets));
         begin
            --  Do not display if no main is available
            if Mains.Length > 0 then
               Button_For_Target (Get_Name (Target), Mains);
            end if;
            Free (Mains);
         end;

      else
         Button_For_Target (Get_Name (Target), Empty_Any_Type);
      end if;
   end Install_Button_For_Target;

   -----------------------------
   -- Clear_Actions_And_Menus --
   -----------------------------

   procedure Clear_Actions_And_Menus is
      use Unbounded_String_List;
      C : Unbounded_String_List.Cursor;
      M : Gtk_Menu_Item;
   begin
      C := Builder_Module_ID.Menus.First;

      while Has_Element (C) loop
         M := Find_Menu_Item (Get_Kernel, To_String (Element (C)));

         if M /= null then
            M.Destroy;
         end if;

         Next (C);
      end loop;

      loop
         C := Builder_Module_ID.Actions.First;
         exit when not Has_Element (C);
         Unregister_Action (Kernel => Get_Kernel,
                            Name   => To_String (Element (C)));
         Builder_Module_ID.Actions.Delete (C);
      end loop;
   end Clear_Actions_And_Menus;

   -----------------------------
   -- Install_Toolbar_Buttons --
   -----------------------------

   procedure Install_Toolbar_Buttons is
      C : Target_Cursor := Get_First_Target (Builder_Module_ID.Registry);
      T : Target_Access;

   begin
      loop
         T := Get_Target (C);
         exit when T = null;

         Install_Button_For_Target (T);

         Next (C);
      end loop;
   end Install_Toolbar_Buttons;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Targets_Settings_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel       : constant Kernel_Handle := Get_Kernel (Context.Context);
      Changes_Made : Boolean;

      procedure Set_Size (W : not null access Gtk_Window_Record'Class);
      --  Set the default size for the dialog

      procedure Set_Size (W : not null access Gtk_Window_Record'Class) is
      begin
         Set_Default_Size_From_History (W, "build-targets", Kernel, 800, 600);
      end Set_Size;

   begin
      Configuration_Dialog
        (Builder_Module_ID.Registry,
         Get_Main_Window (Kernel),
         Set_Size'Access,
         Changes_Made);

      if Changes_Made then
         Refresh_Graphical_Elements;

         --  Save the user-defined targets
         Save_Targets;
      end if;

      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Modes_Settings_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel       : constant Kernel_Handle := Get_Kernel (Context.Context);
      Changes_Made : Boolean;

      procedure Set_Size (W : not null access Gtk_Window_Record'Class);
      --  Set the default size for the dialog

      procedure Set_Size (W : not null access Gtk_Window_Record'Class) is
      begin
         Set_Default_Size_From_History (W, "build-modes", Kernel, 800, 600);
      end Set_Size;

   begin
      Modes_Dialog
        (Builder_Module_ID.Registry,
         Get_Main_Window (Kernel),
         Set_Size'Access,
         Changes_Made);

      pragma Warnings (Off);
      if Changes_Made then
         --  ???
         --  Reset mode combo box
         --  Save user-defined modes
         null;
      end if;
      pragma Warnings (On);

      return Commands.Success;
   end Execute;

   -----------------------
   -- Auxiliary_Console --
   -----------------------

   procedure Auxiliary_Console
     (Kernel     : Kernel_Handle;
      Background : Boolean;
      Shadow     : Boolean)
   is
      Console : Interactive_Console := Get_Build_Console
        (Kernel, True, False, False);

      C : Target_Outputs.Cursor;
   begin
      if Console = null then
         Console := Get_Build_Console (Kernel, Shadow, Background, True);

         C := Builder_Module_ID.Builder.Clear_All_Build_Output
           (Shadow, Background);

         while Target_Outputs.Has_Element (C) loop
            Insert (Console,
                    "***" & To_String (Target_Outputs.Key (C))
                    & "***" & ASCII.LF
                    & To_String (Target_Outputs.Element (C)));
            Target_Outputs.Next (C);
         end loop;

      else
         Raise_Child (Find_MDI_Child (Get_MDI (Kernel), Console));
      end if;
   end Auxiliary_Console;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Shadow_Console_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
   begin
      Auxiliary_Console (Kernel, Background => False, Shadow => True);
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Background_Builds_Console_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
   begin
      Auxiliary_Console (Kernel, Background => True, Shadow => False);
      return Commands.Success;
   end Execute;

   ---------------------
   -- Parse_Mode_Node --
   ---------------------

   procedure Parse_Mode_Node (XML : Node_Ptr) is
      Mode       : Mode_Record;
      pragma Unreferenced (Mode);
   begin
      --  Create the mode and add it to the list of supported modes

      Mode := Load_Mode_From_XML (Builder_Module_ID.Registry, XML);
   end Parse_Mode_Node;

   ---------------
   -- Customize --
   ---------------

   overriding procedure Customize
     (Module : access Builder_Module_ID_Record;
      File   : GNATCOLL.VFS.Virtual_File;
      Node   : XML_Utils.Node_Ptr;
      Level  : Customization_Level)
   is
      pragma Unreferenced (Module, File);
      From_User : Boolean;
      C         : Target_XML_List.Cursor;

   begin
      if Node.Tag.all = "target" then
         --  If the Customization_Level we are parsing is user-specific, this
         --  means that we are looking at a target saved by the user. In this
         --  case we allow the XML to update the values of a previously
         --  registered target.

         From_User := (Level = User_Specific);
         Attempt_Target_Register (Node, From_User);

      elsif Node.Tag.all = "target-model" then
         Create_Model_From_XML (Builder_Module_ID.Registry, Node);

         --  We have just added a model: register any target pending on this
         --  model.

         C := Builder_Module_ID.Unregistered_Targets.First;

         --  Browse through all targets until we have found a target with an
         --  unknown model.
         while Has_Element (C) loop
            if Is_Registered_Model
              (Registry => Builder_Module_ID.Registry,
               Name     => Element (C).Model_Name)
            then
               Attempt_Target_Register
                 (XML       => Element (C).XML,
                  From_User => Element (C).From_User);

               --  Free memory
               declare
                  Node_To_Free : Node_Ptr := Element (C).XML;
               begin
                  Free (Node_To_Free);
               end;

               Builder_Module_ID.Unregistered_Targets.Delete (C);
               C := Builder_Module_ID.Unregistered_Targets.First;
            else
               --  The target we are looking at has a model which is not
               --  known: exit.
               exit;
            end if;
         end loop;

      elsif Node.Tag.all = "builder-mode" then
         Parse_Mode_Node (Node);
      end if;
   end Customize;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      P       : Kernel_Search_Provider_Access;
   begin
      Builder_Module_ID := new Builder_Module_ID_Record;

      --  Initialise the registry
      Builder_Module_ID.Registry := Create (Log'Access);

      Initialize
        (Builder_Module_ID.Builder'Access,
         GPS.Core_Kernels.Core_Kernel (Kernel),
         Builder_Module_ID.Registry);

      Register_Module
        (Module      => Builder_Module_ID,
         Kernel      => Kernel,
         Module_Name => "Builder Facility");

      P := new Builder_Search_Provider;
      Register_Provider_And_Action (Kernel, P);
      if P.Kernel = null then
         raise Program_Error;
      end if;

      --  Register the menus

      Register_Action
        (Kernel, "Build open targets settings", new Targets_Settings_Command,
         Description => -"Open the Build Targets settings dialog");

      if Active (Modes_Trace) then
         Register_Action
           (Kernel, "Build open modes settings", new Modes_Settings_Command,
            Description => -"Open the Modes Targets settings dialog");
      end if;

      Register_Action
        (Kernel, "open Auxiliary Builds", new Shadow_Console_Command,
         Category => -"Views",
         Description => -"Open the Auxiliary Builds console");

      Register_Action
        (Kernel, "open Background Builds",
         new Background_Builds_Console_Command,
         Category => -"Views",
         Description => -"Open the Backgorund Builds console");

      Register_Contextual_Submenu
        (Kernel,
         Name    => "Build",
         Submenu => new Builder_Contextual);

      Register_Contextual_Submenu
        (Kernel,
         Name    => "Run",
         Submenu => new Run_Contextual);

      File_Saved_Hook.Add (new On_File_Saved);
      Buffer_Edited_Hook.Add (new On_Buffer_Modified);
      Compilation_Starting_Hook.Add (new On_Compilation_Starting);
      Compilation_Finished_Hook.Add (new On_Compilation_Finished);
      Project_View_Changed_Hook.Add (new On_View_Changed);
      Compute_Build_Targets_Hook.Add (new On_Compute_Targets);
      Build_Mode_Changed_Hook.Add (new On_Build_Mode_Changed);
      Gps_Started_Hook.Add (new On_GPS_Started);

      --  Register the shell commands

      Builder_Facility_Module.Scripts.Register_Commands
        (Kernel_Handle (Kernel));

      --  Load the user-defined targets

      Register_Output_Parser
        (Builder_Module_ID.Output_Chopper'Access, "output_chopper");
      Register_Output_Parser
        (Builder_Module_ID.UTF8_Converter'Access, "utf_converter");
      Register_Output_Parser
        (Builder_Module_ID.Text_Splitter'Access, "text_splitter");
      Register_Output_Parser
        (Builder_Module_ID.Console_Writer'Access, "console_writer");
      Register_Output_Parser
        (Builder_Module_ID.Location_Parser'Access, "location_parser");
      Register_Output_Parser
        (Builder_Module_ID.Output_Collector'Access, "output_collector");
      Register_Output_Parser
        (Builder_Module_ID.Build_Hook'Access, End_Of_Build_Name);

      Builder_Module_ID.Output_Collector.Set (Builder);
      Builder_Module_ID.Location_Parser.Set (Builder);
      Builder_Module_ID.Console_Writer.Set (Builder);
      Builder_Module_ID.Build_Hook.Set (Builder);

      declare
         Progress_Pattern : constant String :=
           "completed ([0-9]+) out of ([0-9]+) \(([^\n]*)%\)\.\.\.\n";
         --  ??? This is configurable in some cases (from XML for instance),
         --  so we should not have a hard coded regexp here.
      begin
         Register_Output_Parser
           (Builder_Module_ID.Progress_Parser'Access, "progress_parser");
         Builder_Module_ID.Progress_Parser.Set_Pattern (Progress_Pattern);
      end;

      Builder_Module_ID.UTF8_Converter.Set (Kernel);
   end Register_Module;

   --------------
   -- Registry --
   --------------

   function Registry
     return Build_Configurations.Build_Config_Registry_Access is
   begin
      return Builder_Module_ID.Registry;
   end Registry;

   -------------
   -- Builder --
   -------------

   function Builder return Builder_Context is
   begin
      return Builder_Module_ID.Builder'Access;
   end Builder;

   -------------------
   -- Activate_Mode --
   -------------------

   procedure Activate_Mode (Mode : String; Active : Boolean) is
      M : Mode_Record;
      U : constant Unbounded_String := To_Unbounded_String (Mode);
   begin
      if Contains_Mode (Builder_Module_ID.Registry, U) then
         M := Element_Mode (Builder_Module_ID.Registry, U);
         M.Active := Active;

         Replace_Mode (Builder_Module_ID.Registry, U, M);
      end if;
   end Activate_Mode;

   ----------------
   -- Set_Subdir --
   ----------------

   procedure Set_Subdir (Mode : String; Subdir : String) is
      M : Mode_Record;
      U : constant Unbounded_String := To_Unbounded_String (Mode);
   begin
      if Contains_Mode (Builder_Module_ID.Registry, U) then
         M := Element_Mode (Builder_Module_ID.Registry, U);
         M.Subdir := To_Unbounded_String (Subdir);

         Replace_Mode (Builder_Module_ID.Registry, U, M);
      end if;
   end Set_Subdir;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Build_Mode_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Mode   : String)
   is
      pragma Unreferenced (Self);
      Reg  : Project_Registry renames
               Project_Registry (Get_Registry (Kernel).all);
   begin
      if Reg.Environment.Object_Subdir /= Get_Mode_Subdir (Registry, Mode) then
         Reg.Environment.Set_Object_Subdir (Get_Mode_Subdir (Registry, Mode));
         Recompute_View (Get_Kernel);
      end if;
   end Execute;

   -----------------
   -- Set_Pattern --
   -----------------

   overriding procedure Set_Pattern
      (Self     : not null access Builder_Search_Provider;
       Pattern  : not null access GPS.Search.Search_Pattern'Class;
       Limit    : Natural := Natural'Last)
   is
      pragma Unreferenced (Limit);
   begin
      Self.Pattern := Search_Pattern_Access (Pattern);

      Unchecked_Free (Self.Iter);

      Self.Iter := new Target_Cursor'
         (Get_First_Target (Builder_Module_ID.Registry));
      Setup (Self);
   end Set_Pattern;

   -----------
   -- Setup --
   -----------

   procedure Setup
      (Self : not null access Builder_Search_Provider'Class)
   is
      T : constant Target_Access := Get_Target (Self.Iter.all);
   begin
      if Self.Mains /= null then
         Free (Self.Mains.all);
         Unchecked_Free (Self.Mains);
      end if;

      if T /= null then
         declare
            Targets : constant Unbounded_String :=
               Get_Properties (T).Target_Type;
         begin
            Self.Mains := new Any_Type'
               (Compute_Build_Targets_Hook.Run
                  (Self.Kernel, To_String (Targets)));
            Self.Current_Main := 1;
         end;
      else
         Self.Mains := null;
         Self.Current_Main := Integer'Last;
      end if;
   end Setup;

   ----------
   -- Next --
   ----------

   overriding procedure Next
      (Self     : not null access Builder_Search_Provider;
       Result   : out GPS.Search.Search_Result_Access;
       Has_Next : out Boolean)
   is
      T : constant Target_Access := Get_Target (Self.Iter.all);
      C : Search_Context;
   begin
      Result := null;
      if T = null or else Self.Mains = null then
         Has_Next := False;

      else
         if Self.Current_Main < Self.Mains.Length then
            declare
               Main : constant Virtual_File :=
                  Create (+Self.Mains.List (Self.Current_Main).Tuple (2).Str);
               Name : constant String :=
                  Get_Name (T) & " " & Main.Display_Base_Name;
            begin
               C := Self.Pattern.Start (Name);
               if C /= GPS.Search.No_Match then
                  Result := new Builder_Search_Result'
                     (Kernel   => Self.Kernel,
                      Provider => Self,
                       Score   => C.Score,
                       Short   => new String'
                          (Self.Pattern.Highlight_Match (Name, Context => C)),
                       Long    => null,
                       Id      => new String'("build-" & Name),
                       Main    => Main,
                       Target  => T);
                  Self.Adjust_Score (Result);
               end if;
            end;
         end if;

         Self.Current_Main := Self.Current_Main + 1;
         if Self.Current_Main > Self.Mains.Length then
            Next (Self.Iter.all);
            Setup (Self);
            Has_Next := Get_Target (Self.Iter.all) /= null;
         else
            Has_Next := True;
         end if;
      end if;
   end Next;

   ---------------------
   -- Complete_Suffix --
   ---------------------

   overriding function Complete_Suffix
     (Self      : not null access Builder_Search_Provider;
      Pattern   : not null access GPS.Search.Search_Pattern'Class)
      return String
   is
      Suffix      : Unbounded_String;
      Suffix_Last : Natural := 0;
      C           : Search_Context;
      T           : Target_Access;
   begin
      Self.Set_Pattern (Pattern);

      loop
         T := Get_Target (Self.Iter.all);
         exit when T = null or else Self.Mains = null;

         if Self.Current_Main < Self.Mains.Length then
            declare
               Main : constant Virtual_File :=
                  Create (+Self.Mains.List (Self.Current_Main).Tuple (2).Str);
               Name : constant String :=
                  Get_Name (T) & " " & Main.Display_Base_Name;
            begin
               C := Self.Pattern.Start (Name);
               if C /= GPS.Search.No_Match then
                  Self.Pattern.Compute_Suffix (C, Name, Suffix, Suffix_Last);
                  exit when Suffix_Last = 0;
               end if;
            end;
         end if;

         Self.Current_Main := Self.Current_Main + 1;
         if Self.Current_Main > Self.Mains.Length then
            Next (Self.Iter.all);
            Setup (Self);
         end if;
      end loop;

      return Slice (Suffix, 1, Suffix_Last);
   end Complete_Suffix;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Builder_Search_Provider) is
   begin
      if Self.Mains /= null then
         Free (Self.Mains.all);
         Unchecked_Free (Self.Mains);
      end if;

      Unchecked_Free (Self.Iter);

      Free (Kernel_Search_Provider (Self));
   end Free;

   -------------------
   -- Documentation --
   -------------------

   overriding function Documentation
      (Self     : not null access Builder_Search_Provider) return String
   is
      pragma Unreferenced (Self);
   begin
      return "Search amongst build targets";
   end Documentation;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
      (Self       : not null access Builder_Search_Result;
       Give_Focus : Boolean)
   is
      C : aliased Build_Command;
      Result : Command_Return_Type;
      pragma Unreferenced (Result, Give_Focus);
   begin
      C.Builder := Builder;
      C.Target_Name := To_Unbounded_String (Get_Name (Self.Target));
      C.Main    := Self.Main;
      C.Dialog  := Build_Command_Utils.Default;
      C.Quiet   := False;
      Result := C.Execute
        (Context => Create_Null_Context
           (New_Context (Kernel => Self.Kernel)));
   end Execute;

end Builder_Facility_Module;
