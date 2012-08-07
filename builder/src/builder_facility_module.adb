------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2008-2012, AdaCore                     --
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

with GNAT.OS_Lib;               use GNAT.OS_Lib;

with Glib;
with Glib.Object;               use Glib.Object;

with Gtk.Alignment;             use Gtk.Alignment;
with Gtk.Combo_Box;             use Gtk.Combo_Box;
with Gtk.Handlers;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Toolbar;               use Gtk.Toolbar;
with Gtk.Tool_Button;           use Gtk.Tool_Button;
with Gtk.Tool_Item;             use Gtk.Tool_Item;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Separator_Tool_Item;   use Gtk.Separator_Tool_Item;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Widget;                use Gtk.Widget;
with Gtkada.Combo_Tool_Button;  use Gtkada.Combo_Tool_Button;
with Gtkada.MDI;                use Gtkada.MDI;

with Projects;                  use Projects;

with Commands;                    use Commands;
with Commands.Interactive;        use Commands.Interactive;

with Build_Configurations;        use Build_Configurations;
with Build_Configurations.Gtkada; use Build_Configurations.Gtkada;

with Build_Command_Utils;           use Build_Command_Utils;

with GPS.Intl;                  use GPS.Intl;
with GPS.Properties;            use GPS.Properties;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Actions;        use GPS.Kernel.Actions;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.Messages;       use GPS.Kernel.Messages;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;     use GPS.Kernel.Modules.UI;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Console;        use GPS.Kernel.Console;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Properties;     use GPS.Kernel.Properties;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with Traces;                    use Traces;
with String_Utils;              use String_Utils;

with GNATCOLL.Traces;
with GNATCOLL.Projects;         use GNATCOLL.Projects;
with GNATCOLL.Any_Types;        use GNATCOLL.Any_Types;

with Builder_Facility_Module.Scripts;
with Build_Command_Manager;     use Build_Command_Manager;

with Interactive_Consoles;      use Interactive_Consoles;
with Commands.Builder;          use Commands.Builder;
with XML_Utils;                 use XML_Utils;

with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded.Hash;

with GPS.Kernel.Tools_Output;   use GPS.Kernel.Tools_Output;

with Builder_Facility_Module.Output_Choppers;
with Builder_Facility_Module.Text_Splitters;
with Builder_Facility_Module.UTF8_Converters;

package body Builder_Facility_Module is

   Me          : constant Debug_Handle := Create ("Builder_Facility_Module");
   Modes_Trace : constant Debug_Handle :=
                   Create ("Builder.Modes", GNATCOLL.Traces.Off);

   Main_Menu : constant String := '/' & (-"_Build") & '/';
   --  -"Build"

   Mode_Property : constant String := "Build-Mode";
   --  History to store which mode is selected

   Output_Chopper   : aliased Output_Choppers.Output_Parser_Fabric;
   Text_Splitter    : aliased Text_Splitters.Output_Parser_Fabric;
   UTF8_Converter   : aliased UTF8_Converters.Output_Parser_Fabric;

   type Target_And_Main is new Gtkada.Combo_Tool_Button.User_Data_Record
   with record
      Target : Unbounded_String;
      Main   : Virtual_File;
   end record;

   package String_Callback is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record, Target_And_Main);

   package Combo_Callback is new Gtk.Handlers.Callback
     (Gtkada_Combo_Tool_Button_Record);

   package Combo_Box_Callback is new Gtk.Handlers.Callback
     (Gtk_Combo_Box_Record);

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

   package Target_Outputs is new Ada.Containers.Hashed_Maps
     (Unbounded_String, Unbounded_String, Ada.Strings.Unbounded.Hash, "=");

   type Target_Output_Type is
     (Normal_Output, Background_Output, Shadow_Output);

   type Target_Output_Array is array (Target_Output_Type) of
     Target_Outputs.Map;

   package Files is new Ada.Containers.Hashed_Maps
     (Unbounded_String, Virtual_File, Ada.Strings.Unbounded.Hash, "=");

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

      Outputs : Target_Output_Array;

      Build_Count : Natural := 0;
      --  The number of builds currently running

      Modes_Toolbar_Item : Gtk_Tool_Item;
      Modes_Combo : Gtk_Combo_Box;
      --  The toolbar item containing the modes

      Browsing_For_Mode    : Unbounded_String := Null_Unbounded_String;
      --  The mode we are currently looking for when filling the combo,
      --  set to Null_Unbounded_String if we are not browsing.

      Background_Build_ID : Integer := 1;
      --  The ID of the current background build.

      Background_Build_Command : Scheduled_Command_Access;
      --  The command holding the background build.

      Prevent_Save_Reentry : Boolean := False;
      --  Used to prevent cases where a compilation is triggered "on file save"
      --  and the "file save" is caused by another compilation

      Last_Mains : Files.Map;
      --  The last launched main
   end record;

   type Builder_Module_ID_Access is access all Builder_Module_ID_Record'Class;
   --  Data stored with the module id

   overriding procedure Destroy (Module : in out Builder_Module_ID_Record);

   Builder_Module_ID : Builder_Module_ID_Access;

   type Builder_Contextual is new Submenu_Factory_Record with null record;
   overriding procedure Append_To_Menu
     (Builder : access Builder_Contextual;
      Object  : access GObject_Record'Class;
      Context : Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);

   type Run_Contextual is new Submenu_Factory_Record with null record;
   overriding procedure Append_To_Menu
     (Builder : access Run_Contextual;
      Object  : access GObject_Record'Class;
      Context : Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Log (M : String; Mode : Message_Mode);
   --  Logger for the registry

   function Get_Kernel return Kernel_Handle;
   --  Utility function to get the kernel

   procedure On_Build_Manager
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Launch the build manager

   procedure On_Modes_Manager
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Launch the mode manager

   procedure On_Shadow_Console
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Launch the shadow console

   procedure On_Background_Console
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
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

   procedure Clear_Menus;
   --  Remove the target build menus

   procedure Install_Menus;
   --  Install menus for all the targets

   procedure Add_Menu_For_Target (Target : Target_Access);
   --  Create a menu item to build Target

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

   procedure On_Mode_Changed
     (Widget : access Gtk_Combo_Box_Record'Class);
   --  Called when a user selects a mode from the Mode combo box

   procedure On_Combo_Selection
     (Widget : access Gtkada_Combo_Tool_Button_Record'Class);
   --  Called when a user selects a new item from the combo

   procedure On_File_Saved
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Called when a file has been saved

   procedure On_Buffer_Modified
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
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

   function On_Compilation_Starting
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class) return Boolean;
   --  Called when the compilation is starting

   procedure On_Compilation_Finished
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Called when the compilation has ended

   procedure On_GPS_Started
     (Kernel : access Kernel_Handle_Record'Class);
   --  Called when GPS is starting

   procedure On_View_Changed (Kernel : access Kernel_Handle_Record'Class);
   --  Called every time the project view has changed, ie potentially the list
   --  of main units.

   function On_Compute_Build_Targets
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class) return Any_Type;
   --  Called when computing build targets

   procedure Add_Action_For_Target (T : Target_Access);
   --  Register a Kernel Action to build T

   procedure Add_Actions_For_All_Targets;
   --  Register Kernel Actions for all targets

   procedure Remove_All_Actions;
   --  Unregister all previously registered Kernel Actions

   function Action_Name (T : Target_Access) return String;
   --  Return the name of the Kernel Action to build T

   procedure Parse_Mode_Node (XML : Node_Ptr);
   --  Parse XML node describing a mode. See spec for a description of the
   --  XML format.

   type Dynamic_Menu_Item_Record is new Gtk_Menu_Item_Record with null record;
   type Dynamic_Menu_Item is access all Dynamic_Menu_Item_Record'Class;
   --  So that items created for the dynamic Make and Run menus have a special
   --  type, and we only remove these when refreshing the menu

   procedure On_Project_Changed_Hook
     (Kernel : access Kernel_Handle_Record'Class);
   --  Called when project changed. Selects value in the build-mode combobox
   --  previously used for project

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

      procedure Add_Contextual (T : Target_Access) is
         Mitem   : Dynamic_Menu_Item;
         Targets : Unbounded_String;
      begin
         Targets := Get_Properties (T).Target_Type;

         if Length (Targets) /= 0 then
            declare
               Data   : aliased String_Hooks_Args :=
                 (Hooks_Data with
                  Length => Length (Targets),
                  Value  => To_String (Targets));
               Mains  : Any_Type :=
                 Run_Hook_Until_Not_Empty
                   (Get_Kernel,
                    Compute_Build_Targets_Hook,
                    Data'Unchecked_Access);
            begin
               for J in 1 .. Mains.Length loop
                  if Mains.List (J).Length /= 0 then
                     Mitem := new Dynamic_Menu_Item_Record;
                     Gtk.Menu_Item.Initialize
                       (Mitem,
                        Get_Name (T) & ": " & Mains.List (J).Tuple (1).Str);
                     Prepend (Menu, Mitem);
                     String_Callback.Connect
                       (Mitem, Signal_Activate,
                        On_Button_Or_Menu_Click'Access,
                        (To_Unbounded_String (Get_Name (T)),
                         Create (+Mains.List (J).Tuple (2).Str)));
                  end if;
               end loop;
               Destroy (Data);
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
      Object  : access GObject_Record'Class;
      Context : Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      pragma Unreferenced (Object, Builder);
      --  The filter guarantees we are on a File_Selection_Context
   begin
      Append_To_Contextual_Menu (Build_Targets, Context, Menu);
   end Append_To_Menu;

   --------------------
   -- Append_To_Menu --
   --------------------

   overriding procedure Append_To_Menu
     (Builder : access Run_Contextual;
      Object  : access GObject_Record'Class;
      Context : Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      pragma Unreferenced (Object, Builder);
      --  The filter guarantees we are on a File_Selection_Context
   begin
      Append_To_Contextual_Menu (Run_Targets, Context, Menu);
   end Append_To_Menu;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Module : in out Builder_Module_ID_Record) is
   begin
      for T in Target_Output_Type loop
         Module.Outputs (T).Clear;
      end loop;

      Free (Module.Registry);
   end Destroy;

   -----------------
   -- Action_Name --
   -----------------

   function Action_Name (T : Target_Access) return String is
   begin

      return Get_Name (T);
   end Action_Name;

   ---------------------------
   -- Add_Action_For_Target --
   ---------------------------

   procedure Add_Action_For_Target (T : Target_Access) is
      C      : Build_Command_Access;
      M      : Build_Main_Command_Access;
      N      : constant String := Get_Name (T);
      Name   : constant String := Action_Name (T);
      Action : Unbounded_String;

   begin
      if Length (Get_Properties (T).Target_Type) /= 0 then
         --  Register the "build main number x"-like actions

         for J in 1 .. 4 loop
            Create (M, Get_Kernel, Builder_Module_ID.Registry, N,
                    To_String (Get_Properties (T).Target_Type), J,
                    False, Default);
            Set_Unbounded_String (Action, N & (-" Number") & J'Img);
            Register_Action
              (Kernel      => Get_Kernel,
               Name        => To_String (Action),
               Command     => M,
               Description => To_String (Action),
               Filter      => null,
               Category    => -"Build",
               Defined_In  => GNATCOLL.VFS.No_File);
            Builder_Module_ID.Actions.Append (Action);
         end loop;
      else
         Create
           (C, Get_Kernel,
            Builder_Module_ID.Registry, N, No_File, False, Default);

         Register_Action (Kernel      => Get_Kernel,
                          Name        => Name,
                          Command     => C,
                          Description => (-"Build target ") & N,
                          Filter      => null,
                          Category    => -"Build",
                          Defined_In  => GNATCOLL.VFS.No_File);

         Builder_Module_ID.Actions.Append (To_Unbounded_String (Name));
      end if;
   end Add_Action_For_Target;

   ---------------------------------
   -- Add_Actions_For_All_Targets --
   ---------------------------------

   procedure Add_Actions_For_All_Targets is
      C : Target_Cursor := Get_First_Target (Builder_Module_ID.Registry);
      T : Target_Access;
   begin
      loop
         T := Get_Target (C);
         exit when T = null;

         Add_Action_For_Target (T);

         Next (C);
      end loop;
   end Add_Actions_For_All_Targets;

   ------------------------
   -- Remove_All_Actions --
   ------------------------

   procedure Remove_All_Actions is
      use Unbounded_String_List;
      C : Unbounded_String_List.Cursor;
   begin
      loop
         C := Builder_Module_ID.Actions.First;

         exit when not Has_Element (C);

         Unregister_Action (Kernel => Get_Kernel,
                            Name   => To_String (Element (C)));

         Builder_Module_ID.Actions.Delete (C);
      end loop;
   end Remove_All_Actions;

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

      if Shadow then
         Builder_Module_ID.Outputs (Shadow_Output).Clear;
      elsif Background then
         Builder_Module_ID.Outputs (Background_Output).Clear;
      else
         Builder_Module_ID.Outputs (Normal_Output).Clear;
      end if;
   end Clear_Compilation_Output;

   --------------------
   -- On_GPS_Started --
   --------------------

   procedure On_GPS_Started
     (Kernel : access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Kernel);
   begin
      Load_Targets;
   end On_GPS_Started;

   -----------------------------
   -- On_Compilation_Starting --
   -----------------------------

   function On_Compilation_Starting
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class) return Boolean
   is
      D : constant Compilation_Hooks_Args :=
        Compilation_Hooks_Args (Data.all);
   begin
      --  Small issue here: if the user cancels the compilation in one of the
      --  custom hooks the user might have connected, then all changes done
      --  here (increase Build_Count) will not be undone since
      --  On_Compilation_Finished is not called.

      --  Ask for saving sources/projects before building.
      --  Do this before checking the project, in case we have a default
      --  project whose name is changed when saving

      Builder_Module_ID.Prevent_Save_Reentry := True;
      if not (D.Quiet or else D.Shadow)
        and then not Save_MDI_Children (Kernel, Force => Auto_Save.Get_Pref)
      then
         return False;
      end if;
      Builder_Module_ID.Prevent_Save_Reentry := False;

      Clear_Compilation_Output
        (Kernel_Handle (Kernel),
         Category        => D.Value,
         Clear_Console   => (not D.Quiet)
           and then (D.Shadow or else Builder_Module_ID.Build_Count = 0),
         Clear_Locations => Builder_Module_ID.Build_Count = 0,
         Shadow          => D.Shadow,
         Background      => D.Background);

      Builder_Module_ID.Build_Count := Builder_Module_ID.Build_Count + 1;

      return True;
   end On_Compilation_Starting;

   ---------------------
   -- On_View_Changed --
   ---------------------

   procedure On_View_Changed (Kernel : access Kernel_Handle_Record'Class) is
      pragma Unreferenced (Kernel);
   begin
      --  Clear the items that might depend on the number of mains

      Refresh_Graphical_Elements;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_View_Changed;

   -----------------------------
   -- On_Compilation_Finished --
   -----------------------------

   procedure On_Compilation_Finished
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      pragma Unreferenced (Kernel, Data);
   begin
      if Builder_Module_ID.Build_Count > 0 then
         Builder_Module_ID.Build_Count := Builder_Module_ID.Build_Count - 1;
      end if;
   end On_Compilation_Finished;

   ------------------------------
   -- On_Compute_Build_Targets --
   ------------------------------

   function On_Compute_Build_Targets
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class) return Any_Type
   is
      Kind : constant String := String_Hooks_Args (Data.all).Value;
   begin
      if Kind = "main" then
         declare
            Mains  : constant File_Array :=
               Get_Mains (Get_Registry (Kernel_Handle (Kernel)));

            Result : Any_Type (List_Type, Mains'Length);
         begin
            for J in Mains'Range loop
               declare
                  Base         : constant String :=
                                   Mains (J).Display_Base_Name;
                  Full         : constant String := +Mains (J).Full_Name;
                  Display_Name : constant Any_Type :=
                                   (String_Type,
                                    Base'Length,
                                    Base);
                  Full_Name    : constant Any_Type :=
                                   (String_Type, Full'Length, Full);

               begin
                  Result.List (1 + J - Mains'First) := new Any_Type'
                    ((Tuple_Type, 2,
                     Tuple => (1 => new Any_Type'(Display_Name),
                               2 => new Any_Type'(Full_Name))));
               end;
            end loop;

            return Result;
         end;

      elsif Kind = "executable" then
         declare
            Mains  : constant File_Array :=
               Get_Mains (Get_Registry (Kernel_Handle (Kernel)));
            Result : Any_Type (List_Type, Mains'Length);
            P      : Project_Type;
         begin
            for J in Mains'Range loop
               --  ??? Not efficient: in Get_Mains, we started from the project
               --  to get the list of mains, and now for each of those main we
               --  are looking for the project.
               P := Get_Registry (Kernel).Tree.Info
                 (Get_Registry (Kernel).Tree.Create
                  (Mains (J).Full_Name)).Project;

               if P = No_Project then
                  --  This can happen when the project can not find the source
                  --  corresponding to the main file, for instance
                  --  badly-written user projects, or projects that are created
                  --  on-the-fly with the --debug= command line switch.
                  Trace
                    (Me,
                     (-"Could not find the project for """
                      & Mains (J).Display_Full_Name & """"));
                  return Empty_Any_Type;
               elsif Executables_Directory (P) = GNATCOLL.VFS.No_File then
                  Log (-"Project """ & P.Name & """ has no exec_dir", Error);
                  return Empty_Any_Type;
               else
                  declare
                     Exec : constant Virtual_File :=
                       Create_From_Dir
                         (Executables_Directory (P),
                          P.Executable_Name (Mains (J).Full_Name));
                     Base : constant String := String (Exec.Base_Name);
                     Full : constant String := String (Exec.Full_Name.all);
                     Display_Name : constant Any_Type :=
                       (String_Type, Base'Length, Base);
                     Full_Name    : constant Any_Type :=
                       (String_Type, Full'Length, Full);

                  begin
                     Result.List (1 + J - Mains'First) := new Any_Type'
                       ((Tuple_Type, 2,
                        Tuple => (1 => new Any_Type'(Display_Name),
                                  2 => new Any_Type'(Full_Name))));
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
   end On_Compute_Build_Targets;

   --------------------------------
   -- Refresh_Graphical_Elements --
   --------------------------------

   procedure Refresh_Graphical_Elements is
   begin
      Clear_Menus;
      Install_Menus;
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
           (Get_Messages_Container (Get_Kernel), Error_Category)
         then
            return;
         end if;
      end if;

      --  We run this hook only when a source file has changed.
      --  For other files (for instance revision logs), we do not want to
      --  compile.

      declare
         P : Project_Type;
      begin
         P := Get_Registry (Get_Kernel).Tree.Info (File).Project;

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
            Launch_Target (Kernel       => Kernel_Handle (Kernel),
                           Registry     => Builder_Module_ID.Registry,
                           Target_Name  => Get_Name (T),
                           Mode_Name    => "",
                           Force_File   => File,
                           Extra_Args   => null,
                           Quiet        => True,
                           Synchronous  => False,
                           Background   => Background,
                           Dialog       => Default,
                           Main         => No_File);
            --  ??? Should we attempt to compute which is the "relevant" main
            --  in On_File_Save mode?
         end if;
         Next (C);
      end loop;
   end File_Saved_Or_Buffer_Modified;

   -------------------
   -- On_File_Saved --
   -------------------

   procedure On_File_Saved
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      File_Data : constant File_Hooks_Args := File_Hooks_Args (Data.all);
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
      File_Saved_Or_Buffer_Modified (Kernel, File_Data.File, Saved => True);
      Builder_Module_ID.Currently_Saving := False;
   end On_File_Saved;

   ------------------------
   -- On_Buffer_Modified --
   ------------------------

   procedure On_Buffer_Modified
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      File_Data : constant File_Hooks_Args := File_Hooks_Args (Data.all);
   begin
      File_Saved_Or_Buffer_Modified (Kernel, File_Data.File, Saved => False);
   end On_Buffer_Modified;

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
            Add_Action_For_Target (T);

            Install_Button_For_Target (T);

            Add_Menu_For_Target (T);
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
      --  Browse through the registry and remove already added buttons

      C := Builder_Module_ID.Buttons.First;

      while Has_Element (C) loop
         Remove (Toolbar, Element (C));
         Next (C);
      end loop;

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
        (Get_Kernel,
         Builder_Module_ID.Registry,
         To_String (Data.Target),
         "",
         No_File,
         null, False, False, Default, Data.Main, False);
   exception
      when E : others =>
         Trace (Exception_Handle, E);
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
           (Get_Kernel,
            Builder_Module_ID.Registry,
            To_String (Target_And_Main (Data.all).Target),
            "",
            No_File,
            null, False, False, Default,
            Target_And_Main (Data.all).Main, False);
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
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

   ---------------------
   -- On_Mode_Changed --
   ---------------------

   procedure On_Mode_Changed (Widget : access Gtk_Combo_Box_Record'Class)
   is
      Mode : constant String := Get_Active_Text (Widget);
      Reg  : Project_Registry renames
               Project_Registry (Get_Registry (Get_Kernel).all);
      Prop : aliased String_Property_Access;

   begin
      --  Do not consider a change to be effective if we are just creating
      --  items or browsign through them.

      if Builder_Module_ID.Browsing_For_Mode /= Null_Unbounded_String
        and then Mode /= To_String (Builder_Module_ID.Browsing_For_Mode)
      then
         return;
      end if;

      if Reg.Environment.Object_Subdir /= Get_Mode_Subdir (Registry, Mode) then
         Reg.Environment.Set_Object_Subdir (Get_Mode_Subdir (Registry, Mode));
         Recompute_View (Get_Kernel);
      end if;

      if Mode /= "default" then
         Prop := new String_Property;
         Prop.Value := new String'(Mode);
         Set_Property
           (Get_Kernel,
            GPS.Kernel.Project.Get_Project (Get_Kernel),
            Mode_Property,
            Prop,
            True);

      else
         GPS.Kernel.Properties.Remove_Property
           (Get_Kernel,
            GPS.Kernel.Project.Get_Project (Get_Kernel),
            Mode_Property);
      end if;
   end On_Mode_Changed;

   -------------------------------
   -- Install_Button_For_Target --
   -------------------------------

   procedure Install_Button_For_Target (Target : Target_Access) is
      Toolbar : constant Gtk_Toolbar   := Get_Toolbar (Get_Kernel);
      Button : Gtk.Tool_Item.Gtk_Tool_Item;

      procedure Button_For_Target
        (Name  : String;
         Mains : Any_Type);
      --  Create one button for target Name and main Main

      procedure Button_For_Target
        (Name  : String;
         Mains : Any_Type)
      is
      begin
         --  In case only one main is available, create a simple button
         if Mains.Length <= 1 then
            declare
               Widget : Gtk.Tool_Button.Gtk_Tool_Button;
               Main   : Virtual_File;
            begin
               Gtk_New_From_Stock (Widget, Get_Icon (Target));
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
                 (Widget, Gtkada.Combo_Tool_Button.Signal_Clicked,
                  On_Button_Or_Menu_Click'Access,
                  (To_Unbounded_String (Name), Main));
               Button := Gtk_Tool_Item (Widget);
            end;
         else
            declare
               Widget : Gtkada.Combo_Tool_Button.Gtkada_Combo_Tool_Button;
            begin
               Gtk_New (Widget, Get_Icon (Target));
               --  Connect to this signal to automatically update the tooltips
               --  when a new main file is selected
               Combo_Callback.Connect
                 (Widget, Signal_Selection_Changed,
                  On_Combo_Selection'Access);

               for J in Mains.List'Range loop
                  Widget.Add_Item
                    (Mains.List (J).Tuple (2).Str, Get_Icon (Target),
                     new Target_And_Main'
                       (To_Unbounded_String (Name),
                        Create (+Mains.List (J).Tuple (2).Str)));
               end loop;

               Combo_Callback.Connect
                 (Widget, "clicked",
                  On_Combo_Click'Access);
               Button := Gtk_Tool_Item (Widget);
            end;
         end if;

         Builder_Module_ID.Buttons.Prepend (Button);
         Insert (Toolbar => Toolbar, Item    => Button);
         Show_All (Button);

      end Button_For_Target;

      Targets : Unbounded_String;

   begin
      if Target = null
        or else not Get_Properties (Target).In_Toolbar
        or else not Get_Properties (Target).Visible
      then
         return;
      end if;

      Targets := Get_Properties (Target).Target_Type;

      if Length (Targets) /= 0 then
         declare
            Data   : aliased String_Hooks_Args :=
              (Hooks_Data with
                 Length => Length (Targets),
                 Value  => To_String (Targets));
            Mains  : Any_Type :=
              Run_Hook_Until_Not_Empty
                (Get_Kernel,
                 Compute_Build_Targets_Hook,
                 Data'Unchecked_Access);

         begin
            --  Do not display if no main is available
            if Mains.Length > 0 then
               Button_For_Target (Get_Name (Target), Mains);
            end if;

            Free (Mains);
            Destroy (Data);
         end;
      else
         Button_For_Target (Get_Name (Target), Empty_Any_Type);
      end if;
   end Install_Button_For_Target;

   -------------------------
   -- Add_Menu_For_Target --
   -------------------------

   procedure Add_Menu_For_Target (Target : Target_Access) is
      Category : constant String := Get_Category (Target);
      Cat_Path : Unbounded_String :=
        To_Unbounded_String (Get_Parent_Menu_Name (Target));

      Toplevel_Menu : constant Boolean := Category (Category'First) = '_'
        and then Category (Category'Last) = '_';

      procedure Menu_For_Action
        (Parent_Path : String;
         Name        : String;
         Main        : Virtual_File;
         Menu_Name   : String);
      --  Add a menu at Parent_Path for target named Name, with menu name
      --  Menu_Name

      procedure Menu_For_Action
        (Parent_Path : String;
         Name        : String;
         Main        : Virtual_File;
         Menu_Name   : String)
      is
         C : Build_Command_Access;
      begin
         Create
           (C,
            Get_Kernel,
            Builder_Module_ID.Registry,
            Name,
            Main,
            False,
            Force_Dialog_Unless_Disabled_By_Target);

         Register_Menu (Kernel      => Get_Kernel,
                        Parent_Path => Parent_Path,
                        Text        => Menu_Name,
                        Stock_Image => Get_Icon (Target),
                        Callback    => null,
                        Command     => Interactive_Command_Access (C),
                        Ref_Item    => -"Run",
                        --  Do not use mnemonics if we are registering a
                        --  main, as this is a file name in this case.
                        Mnemonics   => Main = No_File);

         if Toplevel_Menu then
            Builder_Module_ID.Menus.Prepend
              (To_Unbounded_String (Parent_Path & Name));
         else
            Builder_Module_ID.Menus.Prepend (Cat_Path & "/" & Name);
         end if;
      end Menu_For_Action;

      Targets : Unbounded_String;

   begin
      --  Do nothing is the target is not supposed to be shown in the menu
      if not Get_Properties (Target).In_Menu
        or else not Get_Properties (Target).Visible
      then
         return;
      end if;

      if not Toplevel_Menu then
         Append (Cat_Path, Category);

         if not Builder_Module_ID.Menus.Contains (Cat_Path) then
            Builder_Module_ID.Menus.Append (Cat_Path);
         end if;
      end if;

      Targets := Get_Properties (Target).Target_Type;

      if Length (Targets) /= 0 then
         declare
            Data   : aliased String_Hooks_Args :=
              (Hooks_Data with
                 Length => Length (Targets),
                 Value  => To_String (Targets));
            Mains  : Any_Type :=
              Run_Hook_Until_Not_Empty
                 (Get_Kernel,
                  Compute_Build_Targets_Hook,
                  Data'Unchecked_Access);

         begin
            if Mains.Length > 0
              and then Mains.T /= List_Type
            then
               Insert
                 (Get_Kernel,
                  (-"The command for determining the target type of target " &
                   To_String (Targets) & (-" returned a ") & Mains.T'Img
                     & (-("but should return a LIST_TYPE "
                       & " (containing a pair display_name/full_name)"))),
                  Mode => Error);

            else
               for J in 1 .. Mains.Length loop
                  if Mains.List (J).Length /= 0 then
                     Menu_For_Action
                       (Parent_Path => To_String (Cat_Path),
                        Name        => Get_Name (Target),
                        Main        => Create (+Mains.List (J).Tuple (2).Str),
                        Menu_Name   => Mains.List (J).Tuple (1).Str);
                  end if;
               end loop;
            end if;

            Destroy (Data);
            Free (Mains);
         end;
      else
         Menu_For_Action (Parent_Path => To_String (Cat_Path),
                          Name        => Get_Name (Target),
                          Main        => No_File,
                          Menu_Name   => Get_Menu_Name (Target));
      end if;
   end Add_Menu_For_Target;

   -----------------
   -- Clear_Menus --
   -----------------

   procedure Clear_Menus is
      use Unbounded_String_List;
      C : Unbounded_String_List.Cursor;
      M : Gtk_Menu_Item;
   begin
      C := Builder_Module_ID.Menus.First;

      while Has_Element (C) loop
         --  Find_Menu_Item expects menu names stripped of their underscores,
         --  so call Strip_Single_Underscore here.
         declare
            Menu_Name : constant String :=
              Strip_Single_Underscores (To_String (Element (C)));
         begin
            M := Find_Menu_Item (Get_Kernel, Menu_Name);

            if M /= null then
               --  Always keep /Build/Run in place to have a menu item to
               --  reference when inserting new items.

               if Menu_Name = -"/Build/Run" then
                  Remove_Submenu (M);
               else
                  Destroy (M);
               end if;
            end if;
         end;

         Next (C);
      end loop;
   end Clear_Menus;

   -------------------
   -- Install_Menus --
   -------------------

   procedure Install_Menus is
      C : Target_Cursor := Get_First_Target (Builder_Module_ID.Registry);
      T : Target_Access;

   begin
      loop
         T := Get_Target (C);
         exit when T = null;

         Add_Menu_For_Target (T);
         Next (C);
      end loop;
   end Install_Menus;

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

   ----------------------
   -- On_Build_Manager --
   ----------------------

   procedure On_Build_Manager
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Changes_Made : Boolean;
   begin
      Configuration_Dialog
        (Builder_Module_ID.Registry,
         Get_Main_Window (Kernel),
         Changes_Made);

      if Changes_Made then
         --  Recreate the actions
         Remove_All_Actions;
         Add_Actions_For_All_Targets;

         Refresh_Graphical_Elements;

         --  Save the user-defined targets
         Save_Targets;
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Build_Manager;

   ----------------------
   -- On_Modes_Manager --
   ----------------------

   procedure On_Modes_Manager
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Changes_Made : Boolean;
   begin
      Modes_Dialog
        (Builder_Module_ID.Registry,
         Get_Main_Window (Kernel),
         Changes_Made);

      if Changes_Made then
         --  ???
         --  Reset mode combo box
         --  Save user-defined modes
         null;
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Modes_Manager;

   -----------------------------
   -- On_Project_Changed_Hook --
   -----------------------------

   procedure On_Project_Changed_Hook
     (Kernel : access Kernel_Handle_Record'Class)
   is
      use type Glib.Gint;

      Prop  : String_Property;
      Found : Boolean;

   begin
      Prop.Get_Property
        (GPS.Kernel.Project.Get_Project (Kernel), Mode_Property, Found);

      if Found then
         declare
            Mode : constant String := Prop.Value.all;
         begin
            --  Going in reverse order, so if unknown mode is specified in the
            --  property then 'default' mode will be selected

            Builder_Module_ID.Browsing_For_Mode := To_Unbounded_String (Mode);

            for J in reverse 0 ..
              Builder_Module_ID.Modes_Combo.Get_Model.N_Children - 1
            loop
               Builder_Module_ID.Modes_Combo.Set_Active (J);

               exit when Builder_Module_ID.Modes_Combo.Get_Active_Text = Mode;
            end loop;

            Builder_Module_ID.Browsing_For_Mode := Null_Unbounded_String;
         end;

      elsif Builder_Module_ID.Modes_Combo /= null then
         Builder_Module_ID.Modes_Combo.Set_Active (0);
      end if;
   end On_Project_Changed_Hook;

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

         if Shadow then
            C := Builder_Module_ID.Outputs (Shadow_Output).First;
         elsif Background then
            C := Builder_Module_ID.Outputs (Background_Output).First;
         else
            C := Builder_Module_ID.Outputs (Normal_Output).First;
         end if;

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

   -----------------------
   -- On_Shadow_Console --
   -----------------------

   procedure On_Shadow_Console
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      Auxiliary_Console (Kernel, Background => False, Shadow => True);
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Shadow_Console;

   ---------------------------
   -- On_Background_Console --
   ---------------------------

   procedure On_Background_Console
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      Auxiliary_Console (Kernel, Background => True, Shadow => False);
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Background_Console;

   ---------------------
   -- Parse_Mode_Node --
   ---------------------

   procedure Parse_Mode_Node (XML : Node_Ptr) is
      Mode       : Mode_Record;
      First_Mode : Boolean := False;
      Align      : Gtk_Alignment;

      use type Glib.Gint;

   begin
      --  Create the mode and add it to the list of supported modes

      Mode := Load_Mode_From_XML (Builder_Module_ID.Registry, XML);

      if Mode.Name = "" then
         return;
      end if;

      --  Add the mode to the combo if it is not a shadow mode

      if not Mode.Shadow then
         --  If the combo is not created, create it now

         if Builder_Module_ID.Modes_Combo = null then
            First_Mode := True;
            Gtk_New_Text (Builder_Module_ID.Modes_Combo);

            --  ... and add it to the toolbar

            Gtk_New (Builder_Module_ID.Modes_Toolbar_Item);
            Gtk_New (Align, 0.5, 0.5, 1.0, 0.6);
            Add (Align, Builder_Module_ID.Modes_Combo);

            Builder_Module_ID.Modes_Toolbar_Item.Add (Align);

            Insert (Get_Toolbar (Get_Kernel),
                    Builder_Module_ID.Modes_Toolbar_Item);
            Show_All (Builder_Module_ID.Modes_Toolbar_Item);
            Combo_Box_Callback.Connect
              (Builder_Module_ID.Modes_Combo,
               Signal_Changed,
               On_Mode_Changed'Access);
         end if;

         --  Now, insert the mode in the combo

         Append_Text (Builder_Module_ID.Modes_Combo, To_String (Mode.Name));

         if First_Mode then
            Set_Active (Builder_Module_ID.Modes_Combo, 0);
         end if;

         --  Regenerate the tooltips for the combo box

         Set_Tooltip_Text
           (Builder_Module_ID.Modes_Toolbar_Item,
           Get_Builder_Mode_Chooser_Tooltip (Builder_Module_ID.Registry));
      end if;
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
      Space : Gtk_Separator_Tool_Item;
   begin
      Builder_Module_ID := new Builder_Module_ID_Record;

      --  Initialise the registry
      Builder_Module_ID.Registry := Create (Log'Access);

      Register_Module
        (Module      => Builder_Module_ID,
         Kernel      => Kernel,
         Module_Name => "Builder Facility");

      --  Register the menus

      Register_Menu (Kernel, "/_" & (-"Build"), Ref_Item => -"Tools");
      Register_Menu (Kernel, Main_Menu & (-"_Run"));
      Register_Menu (Kernel, Main_Menu & (-"Se_ttings"), -"_Targets", "",
                     On_Build_Manager'Access);

      if Active (Modes_Trace) then
         Register_Menu (Kernel, Main_Menu & (-"Se_ttings"), -"_Modes", "",
                        On_Modes_Manager'Access);
      end if;

      Register_Menu (Kernel, -"/Tools/Consoles",
                     -"_Auxiliary Builds", "",
                     On_Shadow_Console'Access);

      Register_Menu (Kernel, -"/Tools/Consoles",
                     -"_Background Builds", "",
                     On_Background_Console'Access);

      Register_Contextual_Submenu
        (Kernel,
         Name    => "Build",
         Submenu => new Builder_Contextual);

      Register_Contextual_Submenu
        (Kernel,
         Name    => "Run",
         Submenu => new Run_Contextual);

      --  Connect to the File_Saved_Hook
      Add_Hook (Kernel, File_Saved_Hook,
                Wrapper (On_File_Saved'Access),
                Name  => "builder_facility_module.file_saved");

      Add_Hook (Kernel, Buffer_Modified_Hook,
                Wrapper (On_Buffer_Modified'Access),
                Name  => "builder_facility_module.file_buffer_modified");

      --  Connect to the Compilation_Starting_Hook

      Add_Hook (Kernel, Compilation_Starting_Hook,
                Wrapper (On_Compilation_Starting'Access),
                Name => "builder_facility_module.compilation_starting");

      Add_Hook (Kernel, Compilation_Finished_Hook,
                Wrapper (On_Compilation_Finished'Access),
                Name => "builder_facility_module.compilation_finished");

      Add_Hook
        (Kernel => Kernel,
         Hook   => Project_View_Changed_Hook,
         Func   => Wrapper (On_View_Changed'Access),
         Name   => "builder_facility_module.on_view_changed");

      Add_Hook (Kernel, Compute_Build_Targets_Hook,
                Wrapper (On_Compute_Build_Targets'Access),
                Name => "builder_facility_module.compute_build_targets");

      Add_Hook
        (Kernel => Kernel,
         Hook   => Project_Changed_Hook,
         Func   => Wrapper (On_Project_Changed_Hook'Access),
         Name   => "builder_facility_module.on_project_changed");

      --  Register the shell commands

      Builder_Facility_Module.Scripts.Register_Commands
        (Kernel_Handle (Kernel));

      --  Insert a separator in the toolbar

      Gtk_New (Space);
      Set_Draw (Space, True);
      Insert (Get_Toolbar (Kernel), Space);

      --  Load the user-defined targets

      Add_Hook (Kernel, GPS_Started_Hook,
                Wrapper (On_GPS_Started'Access),
                Name  => "builder_facility_module.gps_started");

      Register_Output_Parser (Output_Chopper'Access, Reserved - 15);
      Register_Output_Parser (UTF8_Converter'Access, Reserved - 5);
      Register_Output_Parser (Text_Splitter'Access, Line_By_Line);
      UTF8_Converter.Set (Kernel);
   end Register_Module;

   --------------
   -- Registry --
   --------------

   function Registry
     return Build_Configurations.Build_Config_Registry_Access is
   begin
      return Builder_Module_ID.Registry;
   end Registry;

   ----------------------------
   -- Append_To_Build_Output --
   ----------------------------

   procedure Append_To_Build_Output
     (Kernel     : access GPS.Kernel.Kernel_Handle_Record'Class;
      Line       : String;
      Target     : String;
      Shadow     : Boolean;
      Background : Boolean)
   is
      pragma Unreferenced (Kernel);

      Inserted : Boolean := True;
      C : Target_Outputs.Cursor;
      T : Target_Output_Type;
      use Target_Outputs;
   begin
      if Builder_Module_ID /= null then
         if Shadow then
            T := Shadow_Output;

         elsif Background then
            T := Background_Output;

         else
            T := Normal_Output;
         end if;

         C := Builder_Module_ID.Outputs (T).Find
           (To_Unbounded_String (Target));

         if C = Target_Outputs.No_Element then
            Builder_Module_ID.Outputs (T).Insert
              (Key       => To_Unbounded_String (Target),
               New_Item  => To_Unbounded_String (Line & ASCII.LF),
               Position  => C,
               Inserted  => Inserted);
         else
            declare
               procedure Local_Append (Key : Unbounded_String;
                                       E   : in out Unbounded_String);
               --  Auxiliary subprogram to append to an unbounded string
               --  in place in the container.

               ------------------
               -- Local_Append --
               ------------------

               procedure Local_Append (Key : Unbounded_String;
                                       E   : in out Unbounded_String)
               is
                  pragma Unreferenced (Key);
               begin
                  Append (E, Line & ASCII.LF);
               end Local_Append;
            begin
               Builder_Module_ID.Outputs (T).Update_Element
                 (C, Local_Append'Access);
            end;
         end if;
      end if;
   end Append_To_Build_Output;

   ----------------------
   -- Get_Build_Output --
   ----------------------

   function Get_Build_Output
     (Target     : String;
      Shadow     : Boolean;
      Background : Boolean) return Unbounded_String
   is
      Output : Target_Output_Type;
   begin
      if Shadow then
         Output := Shadow_Output;
      elsif Background then
         Output := Background_Output;
      else
         Output := Normal_Output;
      end if;

      if Target = "" then
         declare
            C : Target_Outputs.Cursor;
            R : Unbounded_String;
         begin
            C := Builder_Module_ID.Outputs (Output).First;

            while Target_Outputs.Has_Element (C) loop
               R := R & Target_Outputs.Element (C);
               Target_Outputs.Next (C);
            end loop;

            return R;
         end;
      else
         return Builder_Module_ID.Outputs (Output).Element
           (To_Unbounded_String (Target));
      end if;
   end Get_Build_Output;

   -----------------------
   -- Get_List_Of_Modes --
   -----------------------

   function Get_List_Of_Modes
     (Model : String) return GNAT.OS_Lib.Argument_List
   is
      Result : Argument_List
        (1 .. Number_Of_Modes (Builder_Module_ID.Registry));
      Index  : Natural;
      --  The first available element in Result;

      use Mode_Map;
      C : Mode_Map.Cursor;
      Mode : Mode_Record;
   begin
      if Builder_Module_ID.Modes_Combo = null
        or else Result'Length = 0
      then
         return (1 => new String'(""));
      end if;

      --  The first mode is the one selected in the combo

      Result (1) := new String'
        (Get_Active_Text (Builder_Module_ID.Modes_Combo));
      Index := 2;

      --  Find all the shadow modes

      C := First_Mode (Builder_Module_ID.Registry);

      while Has_Element (C) loop
         Mode := Element (C);

         if Mode.Shadow
           and then Mode.Active
         then
            declare
               use Model_List;
               C2 : Model_List.Cursor;
            begin
               C2 := Mode.Models.First;

               while Has_Element (C2) loop
                  if Element (C2).Model = Model then
                     Result (Index) := new String'(To_String (Mode.Name));
                     Index := Index + 1;
                  end if;

                  Next (C2);
               end loop;
            end;
         end if;

         Next (C);
      end loop;

      return Result (1 .. Index - 1);
   end Get_List_Of_Modes;

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

   --------------
   -- Get_Mode --
   --------------

   function Get_Mode return String is
   begin
      if Builder_Module_ID.Modes_Combo = null then
         return "";
      end if;

      return (Get_Active_Text (Builder_Module_ID.Modes_Combo));
   end Get_Mode;

   --------------
   -- Set_Mode --
   --------------

   procedure Set_Mode (Mode : String) is
      Model : Gtk_Tree_Model;
      Iter  : Gtk_Tree_Iter;
   begin
      if Builder_Module_ID.Modes_Combo = null then
         return;
      end if;

      Model := Builder_Module_ID.Modes_Combo.Get_Model;

      Iter := Model.Get_Iter_First;

      while Iter /= Null_Iter loop
         if Get_String (Model, Iter, 0) = Mode then
            Builder_Module_ID.Modes_Combo.Set_Active_Iter (Iter);
            return;
         end if;

         Next (Model, Iter);
      end loop;
   end Set_Mode;

   ----------------------------------
   -- Previous_Background_Build_Id --
   ----------------------------------

   function Previous_Background_Build_Id return String is
   begin
      return Integer'Image (Builder_Module_ID.Background_Build_ID - 1);
   end Previous_Background_Build_Id;

   ---------------------------------
   -- Current_Background_Build_Id --
   ---------------------------------

   function Current_Background_Build_Id return String is
   begin
      return Integer'Image (Builder_Module_ID.Background_Build_ID);
   end Current_Background_Build_Id;

   -------------------------------
   -- Background_Build_Finished --
   -------------------------------

   procedure Background_Build_Finished is
   begin
      if Builder_Module_ID.Background_Build_ID = Integer'Last then
         --  Very very unlikely, but just in case.
         Builder_Module_ID.Background_Build_ID := 1;
      else
         Builder_Module_ID.Background_Build_ID :=
           Builder_Module_ID.Background_Build_ID + 1;
      end if;

      Builder_Module_ID.Background_Build_Command := null;
   end Background_Build_Finished;

   ------------------------------
   -- Background_Build_Started --
   ------------------------------

   procedure Background_Build_Started (Command : Scheduled_Command_Access) is
   begin
      Builder_Module_ID.Background_Build_Command := Command;
   end Background_Build_Started;

   --------------------------------
   -- Interrupt_Background_Build --
   --------------------------------

   procedure Interrupt_Background_Build is
   begin
      if Builder_Module_ID.Background_Build_Command /= null then
         Get_Messages_Container (Builder_Module_ID.Get_Kernel).Remove_Category
           (Current_Background_Build_Id, Background_Message_Flags);

         Interrupt_Queue
           (Get_Kernel,
            Command_Access (Builder_Module_ID.Background_Build_Command));
         Builder_Module_ID.Background_Build_Command := null;
      end if;
   end Interrupt_Background_Build;

   -------------------
   -- Set_Last_Main --
   -------------------

   procedure Set_Last_Main (Target : String; Main : Virtual_File) is
      Key : constant Unbounded_String := To_Unbounded_String (Target);
   begin
      Builder_Module_ID.Last_Mains.Exclude (Key);
      Builder_Module_ID.Last_Mains.Insert (Key, Main);
   end Set_Last_Main;

   -------------------
   -- Get_Last_Main --
   -------------------

   function Get_Last_Main (Target : String) return Virtual_File is
      Key : constant Unbounded_String := To_Unbounded_String (Target);
      Cur : Files.Cursor;
      use Files;
   begin
      Cur := Builder_Module_ID.Last_Mains.Find (Key);
      if Cur /= Files.No_Element then
         return Element (Cur);
      else
         return No_File;
      end if;
   end Get_Last_Main;

end Builder_Facility_Module;
