-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2008, AdaCore                    --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Containers.Ordered_Maps;
with Ada.Containers.Doubly_Linked_Lists;

with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;

with GNAT.OS_Lib;               use GNAT.OS_Lib;

with Glib.Object;               use Glib.Object;
with Glib.Xml_Int;              use Glib.Xml_Int;

with Gtk.Handlers;
with Gtk.Toolbar;               use Gtk.Toolbar;
with Gtk.Tool_Button;           use Gtk.Tool_Button;
with Gtk.Tool_Item;             use Gtk.Tool_Item;
with Gtk.Separator_Tool_Item;   use Gtk.Separator_Tool_Item;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtkada.Combo_Tool_Button;  use Gtkada.Combo_Tool_Button;

with Projects;                  use Projects;
with Projects.Registry;         use Projects.Registry;

with Commands.Interactive;        use Commands.Interactive;

with Build_Configurations;        use Build_Configurations;
with Build_Configurations.Gtkada; use Build_Configurations.Gtkada;

with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Actions;        use GPS.Kernel.Actions;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Console;        use GPS.Kernel.Console;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GPS.Location_View;         use GPS.Location_View;
with Traces;                    use Traces;
with String_Utils;              use String_Utils;

with GNATCOLL.VFS;              use GNATCOLL.VFS;

with Builder_Facility_Module.Scripts;
with Build_Command_Manager;     use Build_Command_Manager;

with Commands.Builder;          use Commands.Builder;

package body Builder_Facility_Module is

   Me        : constant Debug_Handle := Create ("Builder_Facility_Module");
   Main_Menu : constant String := '/' & ("_Build") & '/';
   --  -"Build"

   type Target_And_Main is new Gtkada.Combo_Tool_Button.User_Data_Record
   with record
      Target : Unbounded_String;
      Main   : Unbounded_String;
   end record;

   package String_Callback is new Gtk.Handlers.User_Callback
     (Gtk_Tool_Button_Record, Target_And_Main);

   package Combo_Callback is new Gtk.Handlers.Callback
     (Gtkada_Combo_Tool_Button_Record);

   package Buttons_List is new Ada.Containers.Doubly_Linked_Lists
     (Gtk_Tool_Item);

   package String_List is new Ada.Containers.Doubly_Linked_Lists
     (Unbounded_String);

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

      Actions : String_List.List;
      --  The list of currently registered builder actions

      Menus : String_List.List;
      --  The set of menu items that need to be removed when reloading the
      --  targets

      Output     : String_List_Utils.String_List.List;
      --  The last build output
   end record;

   type Builder_Module_ID_Access is access all Builder_Module_ID_Record'Class;
   --  Data stored with the module id

   Builder_Module_ID : Builder_Module_ID_Access;

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Log (M : String; Mode : Message_Mode);
   --  Logger for the registry.

   function Get_Kernel return Kernel_Handle;
   --  Utility function to get the kernel

   procedure On_Build_Manager
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Launch the build manager

   overriding procedure Customize
     (Module : access Builder_Module_ID_Record;
      File   : GNATCOLL.VFS.Virtual_File;
      Node   : Glib.Xml_Int.Node_Ptr;
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
   --  Return the file where user targets are stored.

   procedure Attempt_Target_Register (XML : Node_Ptr; From_User : Boolean);
   --  Attempt to register target described in XML. If the model is not
   --  registered yet, add this target to the list of unregistered targets.
   --  From_User indicates whether the target is loaded from the user saved
   --  file.

   procedure On_Button_Click
     (Widget : access Gtk_Tool_Button_Record'Class;
      Data   : Target_And_Main);
   --  Called when a user clicks on a toolbar button.
   --  Name is the name of the target corresponding to that button.

   procedure On_Combo_Click
     (Widget : access Gtkada_Combo_Tool_Button_Record'Class);
   --  Called when a user clicks on a toolbar combo button.

   procedure Save_Targets;
   procedure Load_Targets;
   --  Save/Load the targets in the user-defined XML

   procedure On_File_Saved
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Called when a file has been saved

   procedure Clear_Compilation_Output
     (Kernel          : Kernel_Handle;
      Category        : String;
      Clear_Console   : Boolean;
      Clear_Locations : Boolean);
   --  Clear the compiler output, the console, and the locations view for
   --  Category.

   function On_Compilation_Starting
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class) return Boolean;
   --  Called when a file has been saved

   procedure On_GPS_Started
     (Kernel : access Kernel_Handle_Record'Class);
   --  Called when GPS is starting

   procedure Add_Action_For_Target (T : Target_Access);
   --  Register a Kernel Action to build T

   procedure Add_Actions_For_All_Targets;
   --  Register Kernel Actions for all targets

   procedure Remove_All_Actions;
   --  Unregister all previously registered Kernel Actions

   function Action_Name (T : Target_Access) return String;
   --  Return the name of the Kernel Action to build T

   procedure Free (Ar : in out Argument_List);
   --  Free memory associated to Ar.

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
      C    : Build_Command_Access;
      N    : constant String := Get_Name (T);
      Name : constant String := Action_Name (T);
   begin
      --  We do not add actions for targets that represent mains, this is
      --  handled via the <item1>, <item2> (...) mechanism.
      if Get_Properties (T).Represents_Mains then
         return;
      end if;

      Create (C, Get_Kernel, Builder_Module_ID.Registry, N, "", False, False);

      Register_Action (Kernel      => Get_Kernel,
                       Name        => Name,
                       Command     => C,
                       Description => (-"Build target ") & N,
                       Filter      => null,
                       Category    => -"Build",
                       Defined_In  => GNATCOLL.VFS.No_File);

      Builder_Module_ID.Actions.Append (To_Unbounded_String (Name));
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
      use String_List;
      C : String_List.Cursor;
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
      Filename : constant String := Get_Home_Dir (Get_Kernel) & "targets.xml";
   begin
      return Create (Filename);
   end Get_Targets_File;

   ------------------
   -- Save_Targets --
   ------------------

   procedure Save_Targets is
      N       : Node_Ptr;
      Success : Boolean;
   begin
      N := Save_All_Targets_To_XML (Builder_Module_ID.Registry);
      Print (N, Full_Name (Get_Targets_File).all, Success);

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
      N := Parse (Full_Name (Get_Targets_File).all);

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

      Clear_Menus;
      Install_Menus;
      Clear_Toolbar_Buttons;
      Install_Toolbar_Buttons;
   end Load_Targets;

   ------------------------------
   -- Clear_Compilation_Output --
   ------------------------------

   procedure Clear_Compilation_Output
     (Kernel          : Kernel_Handle;
      Category        : String;
      Clear_Console   : Boolean;
      Clear_Locations : Boolean)
   is
      pragma Unreferenced (Category);
   begin
      if Clear_Console then
         Console.Clear (Kernel);
      end if;

      if Clear_Locations then
         Remove_Location_Category (Kernel, Error_Category);
      end if;

      String_List_Utils.String_List.Free (Builder_Module_ID.Output);
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
      D : constant String_Boolean_Hooks_Args :=
        String_Boolean_Hooks_Args (Data.all);
      Quiet : constant Boolean := D.Bool;
      --  Whether the
   begin
      --  Small issue here: if the user cancels the compilation in one of the
      --  custom hooks the user might have connected, then all changes done
      --  here (increase Build_Count) will not be undone since
      --  On_Compilation_Finished is not called.

      --  Ask for saving sources/projects before building.
      --  Do this before checking the project, in case we have a default
      --  project whose name is changed when saving

      if not Quiet
        and then not Save_MDI_Children (Kernel, Force => Auto_Save.Get_Pref)
      then
         return False;
      end if;

      Clear_Compilation_Output
        (Kernel_Handle (Kernel), D.Value,
         Clear_Console   => not Quiet,
         Clear_Locations => True);

      --  ??? need to add support for this
--        Interrupt_Xrefs_Loading (Kernel);

      if not Quiet then
         Console.Raise_Console (Kernel);
      end if;

      return True;
   end On_Compilation_Starting;

   -------------------
   -- On_File_Saved --
   -------------------

   procedure On_File_Saved
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      File_Data : constant File_Hooks_Args := File_Hooks_Args (Data.all);
      C         : Build_Configurations.Target_Cursor :=
        Get_First_Target (Builder_Module_ID.Registry);
      T         : Target_Access;
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

      --  We run this hook only when a source file has changed.
      --  For other files (for instance revision logs), we do not want to
      --  compile.

      declare
         P : Project_Type;
      begin
         P := Get_Project_From_File
           (Registry          => Project_Registry
              (Get_Registry (Get_Kernel).all),
            Source_Filename   => File_Data.File,
            Root_If_Not_Found => False);

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

         if Get_Properties (T).Launch_Mode = On_File_Save then
            Launch_Target (Kernel       => Kernel_Handle (Kernel),
                           Registry     => Builder_Module_ID.Registry,
                           Target_Name  => Get_Name (T),
                           Force_File   => No_File,
                           --  We do not pass File_Data.File as Force_File,
                           --  as we do not want to compile, for instance,
                           --  when the current file has no associated project.
                           Extra_Args   => null,
                           Quiet        => True,
                           Synchronous  => False,
                           Force_Dialog => False,
                           Main         => "");
            --  ??? Should we attempt to a "relevant" main when
            --  in On_File_Save mode?
         end if;
         Next (C);
      end loop;

      Builder_Module_ID.Currently_Saving := False;
   end On_File_Saved;

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

   ---------------------
   -- On_Button_Click --
   ---------------------

   procedure On_Button_Click
     (Widget : access Gtk_Tool_Button_Record'Class;
      Data   : Target_And_Main)
   is
      pragma Unreferenced (Widget);
   begin
      Launch_Target
        (Get_Kernel,
         Builder_Module_ID.Registry,
         To_String (Data.Target),
         No_File,
         null, False, False, False, To_String (Data.Main));
   exception
      when E : others =>
         Trace (Exception_Handle, E);
   end On_Button_Click;

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
            No_File,
            null, False, False, False,
            To_String (Target_And_Main (Data.all).Main));
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
   end On_Combo_Click;

   ----------
   -- Free --
   ----------

   procedure Free (Ar : in out Argument_List) is
   begin
      for A in Ar'Range loop
         Free (Ar (A));
      end loop;
   end Free;

   -------------------------------
   -- Install_Button_For_Target --
   -------------------------------

   procedure Install_Button_For_Target (Target : Target_Access) is
      Toolbar : constant Gtk_Toolbar   := Get_Toolbar (Get_Kernel);
      Button : Gtk.Tool_Item.Gtk_Tool_Item;

      procedure Button_For_Target
        (Name  : String;
         Mains : Argument_List);
      --  Create one button for target Name and main Main

      procedure Button_For_Target
        (Name  : String;
         Mains : Argument_List)
      is
      begin
         if Mains'Length = 0 then
            declare
               Widget : Gtk.Tool_Button.Gtk_Tool_Button;
            begin
               Gtk_New_From_Stock (Widget, Get_Icon (Target));
               Set_Label (Widget, Name);
               String_Callback.Connect
                 (Widget, "clicked",
                  On_Button_Click'Access,
                  (To_Unbounded_String (Name), Null_Unbounded_String));
               Button := Gtk_Tool_Item (Widget);
            end;
         else
            declare
               Widget : Gtkada.Combo_Tool_Button.Gtkada_Combo_Tool_Button;
            begin
               Gtk_New (Widget, Get_Icon (Target));
               for J in Mains'Range loop
                  Widget.Add_Item
                    (Mains (J).all, Get_Icon (Target),
                     new Target_And_Main'
                       (To_Unbounded_String (Name),
                        To_Unbounded_String (Mains (J).all)));
               end loop;

               Combo_Callback.Connect
                 (Widget, "clicked",
                  On_Combo_Click'Access);
               Button := Gtk_Tool_Item (Widget);
            end;
         end if;

         Builder_Module_ID.Buttons.Prepend (Button);
         Insert (Toolbar => Toolbar, Item    => Button);
         Set_Tooltip (Button, Get_Tooltips (Get_Kernel), Name);
         Show_All (Button);

      end Button_For_Target;

   begin
      if Target = null
        or else not Get_Properties (Target).In_Toolbar
      then
         return;
      end if;

      if Get_Properties (Target).Represents_Mains then
         declare
            Mains  : Argument_List :=
              Get_Attribute_Value
                (Get_Root_Project (Get_Registry (Get_Kernel).all),
                 Attribute => Main_Attribute);
         begin
            Button_For_Target (Get_Name (Target), Mains);
            Free (Mains);
         end;
      else
         Button_For_Target (Get_Name (Target), (1 .. 0 => <>));
      end if;
   end Install_Button_For_Target;

   -------------------------
   -- Add_Menu_For_Target --
   -------------------------

   procedure Add_Menu_For_Target (Target : Target_Access) is
      Category : constant String := Get_Category (Target);
      Cat_Path : Unbounded_String := To_Unbounded_String (Main_Menu);

      Toplevel_Menu : constant Boolean := Category (Category'First) = '_'
        and then Category (Category'Last) = '_';

      procedure Menu_For_Action
        (Parent_Path : String;
         Name        : String;
         Main        : String;
         Menu_Name   : String);
      --  Add a menu at Parent_Path for target named Name, with menu name
      --  Menu_Name

      procedure Menu_For_Action
        (Parent_Path : String;
         Name        : String;
         Main        : String;
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
            Get_Properties (Target).Launch_Mode = Manually);

         Register_Menu (Kernel      => Get_Kernel,
                        Parent_Path => Parent_Path,
                        Text        => Menu_Name,
                        Stock_Image => Get_Icon (Target),
                        Callback    => null,
                        Command     => Interactive_Command_Access (C),
                        Ref_Item    => -"Run",
                        --  Do not use mnemonics if we are registering a
                        --  main, as this is a file name in this case.
                        Mnemonics   => Main = "");

         if Toplevel_Menu then
            Builder_Module_ID.Menus.Prepend
              (To_Unbounded_String (Main_Menu & Name));
         else
            Builder_Module_ID.Menus.Prepend (Cat_Path & "/" & Name);
         end if;
      end Menu_For_Action;

   begin
      --  Do nothing is the target is not supposed to be shown in the menu
      if not Get_Properties (Target).In_Menu then
         return;
      end if;

      if not Toplevel_Menu then
         Append (Cat_Path, Category);

         --  Find the menu for the category
         if Find_Menu_Item
           (Get_Kernel,
            Strip_Single_Underscores (To_String (Cat_Path))) = null
         then
            --  We have not found a menu item: this means we are about to
            --  create it, so add it to the list of menu items
            Builder_Module_ID.Menus.Append (Cat_Path);
         end if;
      end if;

      if Get_Properties (Target).Represents_Mains then
         declare
            Mains  : Argument_List :=
              Get_Attribute_Value
                (Get_Root_Project (Get_Registry (Get_Kernel).all),
                 Attribute => Main_Attribute);
         begin
            for J in Mains'Range loop
               if Mains (J) /= null then
                  Menu_For_Action (Parent_Path => To_String (Cat_Path),
                                   Name        => Get_Name (Target),
                                   Main        => Mains (J).all,
                                   Menu_Name   => Mains (J).all);
               end if;
            end loop;

            Free (Mains);
         end;
      else
         Menu_For_Action (Parent_Path => To_String (Cat_Path),
                          Name        => Get_Name (Target),
                          Main        => "",
                          Menu_Name   => Get_Menu_Name (Target));
      end if;
   end Add_Menu_For_Target;

   -----------------
   -- Clear_Menus --
   -----------------

   procedure Clear_Menus is
      use String_List;
      C : String_List.Cursor;
      M : Gtk_Menu_Item;
   begin
      C := Builder_Module_ID.Menus.First;

      while Has_Element (C) loop
         --  Find_Menu_Item expects menu names stripped of their underscores,
         --  so call Strip_Single_Underscore here.
         M := Find_Menu_Item
           (Get_Kernel,
            Strip_Single_Underscores (To_String (Element (C))));

         if M /= null then
            Destroy (M);
         else
            Trace (Me, "Menu not found: "
                   & Strip_Single_Underscores (To_String (Element (C))));
         end if;

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
         Get_Tooltips (Kernel),
         Changes_Made);

      if Changes_Made then
         --  Handle the toolbar
         Clear_Toolbar_Buttons;
         Install_Toolbar_Buttons;

         --  Recreate the actions
         Remove_All_Actions;
         Add_Actions_For_All_Targets;

         --  Recreate the menu
         Clear_Menus;
         Install_Menus;

         --  Save the user-defined targets
         Save_Targets;
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Build_Manager;

   ---------------
   -- Customize --
   ---------------

   overriding procedure Customize
     (Module : access Builder_Module_ID_Record;
      File   : GNATCOLL.VFS.Virtual_File;
      Node   : Glib.Xml_Int.Node_Ptr;
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

      Register_Menu (Kernel, "/_" & (-"Build"), Ref_Item => -"Tools");
      Register_Menu (Kernel, Main_Menu & (-"Se_ttings"), -"_Targets", "",
                     On_Build_Manager'Access);

      --  Connect to the File_Saved_Hook
      Add_Hook (Kernel, File_Saved_Hook,
                Wrapper (On_File_Saved'Access),
                Name  => "builder_facility_module.file_saved");

      --  Connect to the Compilation_Starting_Hook

      Add_Hook (Kernel, Compilation_Starting_Hook,
                Wrapper (On_Compilation_Starting'Access),
                Name => "builder_facility_module.compilation_starting");

      --  Register the shell commands

      Builder_Facility_Module.Scripts.Register_Commands
        (Kernel_Handle (Kernel));

      --  Insert a separator in the toolbar

      Gtk_New (Space);
      Set_Draw (Space, True);
      Insert (Get_Toolbar (Kernel), Space);

      --  Load the user-defined targets.
      Add_Hook (Kernel, "gps_started",
                Wrapper (On_GPS_Started'Access),
                Name  => "builder_facility_module.gps_started");
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
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Line   : String)
   is
      pragma Unreferenced (Kernel);
   begin
      if Builder_Module_ID /= null then
         String_List_Utils.String_List.Append (Builder_Module_ID.Output, Line);
      end if;
   end Append_To_Build_Output;

   ----------------------
   -- Get_Build_Output --
   ----------------------

   function Get_Build_Output return String_List_Utils.String_List.List is
   begin
      return Builder_Module_ID.Output;
   end Get_Build_Output;

end Builder_Facility_Module;
