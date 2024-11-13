------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2001-2024, AdaCore                     --
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

with Ada.Exceptions;             use Ada.Exceptions;
with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with GNAT.Directory_Operations;  use GNAT.Directory_Operations;
with GNATCOLL.Python;            use GNATCOLL.Python;
with GNATCOLL.Python.State;
with GNATCOLL.Scripts;           use GNATCOLL.Scripts;
with GNATCOLL.Scripts.Python;    use GNATCOLL.Scripts.Python;
with GNATCOLL.Traces;            use GNATCOLL.Traces;
with GNAT.Strings;               use GNAT.Strings;

with XML_Utils;                  use XML_Utils;

with Pango.Font;                 use Pango.Font;
with Glib.Object;                use Glib.Object;
with Gdk.RGBA;                   use Gdk.RGBA;
with Gtk.Check_Menu_Item;        use Gtk.Check_Menu_Item;
with Gtk.Color_Selection_Dialog; use Gtk.Color_Selection_Dialog;
with Gtk.Dialog;                 use Gtk.Dialog;
with Gtk.Menu_Item;              use Gtk.Menu_Item;
with Gtk.Radio_Menu_Item;        use Gtk.Radio_Menu_Item;
with Gtk.Tree_Model;             use Gtk.Tree_Model;
with Gtk.Tree_View_Column;       use Gtk.Tree_View_Column;
with Gtk.Widget;                 use Gtk.Widget;

with Config;
with Defaults;
with Default_Preferences.Enums;  use Default_Preferences.Enums;
with Default_Preferences.GUI;    use Default_Preferences.GUI;
with Dialog_Utils;               use Dialog_Utils;
with GPS.Customizable_Modules;   use GPS.Customizable_Modules;
with GPS.Intl;                   use GPS.Intl;
with GPS.Kernel.Charsets;        use GPS.Kernel.Charsets;
with GPS.Kernel.Hooks;           use GPS.Kernel.Hooks;
with GPS.Kernel.Modules;         use GPS.Kernel.Modules;
with GPS.Kernel.Scripts;         use GPS.Kernel.Scripts;
with Language;                   use Language;

package body GPS.Kernel.Preferences is
   Me : constant Trace_Handle := Create
     ("GPS.KERNEL.PREFERENCES");

   Experimental_LAL : constant Trace_Handle := Create
     ("GPS.INTERNAL.LAL_EXPERIMENTAL_FEATURES",
      Default => Off);
   --  This trace is used to activate LAL features that are ready
   --  for experimentation by developers.

   Cygwin_Window_Manager : constant Trace_Handle := Create
     ("GPS.INTERNAL.CYGWIN_WINDOW_MANAGER",
      Default => Off);
   --  The default window manager for cygwin x server poorly handles
   --  the menu tooltip and will close the menu window too soon.
   --  Thus generating a Storage_Error: this is not a Gtk bug so disable the
   --  local config menu tooltips in this case.

   use type Config.Host_Type;

   procedure Get_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Get preference command handler

   procedure Preferences_Page_Commands_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handler for the commands related with preferences pages.

   type Preferences_Module is new Module_ID_Record with null record;
   overriding procedure Customize
     (Module : access Preferences_Module;
      File   : GNATCOLL.VFS.Virtual_File;
      Node   : XML_Utils.Node_Ptr;
      Level  : Customization_Level);
   --  Handle GNAT Studio customization files for this module

   type Python_Preferences_Page_Record is new Default_Preferences_Page_Record
     with
      record
         Get_Python_Widget  : Subprogram_Type;
         --  Subprogram returning a newly created PyGtk widget for this page.
      end record;
   type Python_Preferences_Path is
     access all Python_Preferences_Page_Record'Class;
   --  Type used to represent preferences pages that have been registered
   --  in python plugins.
   overriding procedure Free (Self : in out Python_Preferences_Page_Record);

   overriding function Get_Widget
     (Self    : not null access Python_Preferences_Page_Record;
      Manager : not null Preferences_Manager) return Gtk.Widget.Gtk_Widget;

   ----------------
   -- Get_Widget --
   ----------------

   overriding function Get_Widget
     (Self    : not null access Python_Preferences_Page_Record;
      Manager : not null Preferences_Manager) return Gtk.Widget.Gtk_Widget
   is
      function Widget_From_PyObject (Object : PyObject) return System.Address;
      pragma Import (C, Widget_From_PyObject, "ada_widget_from_pyobject");

      Lock          : GNATCOLL.Python.State.Ada_GIL_Lock with Unreferenced;
      Script        : constant Scripting_Language  :=
                        Get_Script (Self.Get_Python_Widget.all);
      Args          : Callback_Data'Class := Create (Script, 0);
      Page_View     : constant Preferences_Page_View :=
                        new Preferences_Page_View_Record;
      Stub          : GObject_Record;
      Python_Widget : GObject;
      pragma Unreferenced (Manager);
   begin
      Dialog_Utils.Initialize (Page_View);

      --  Retrieve the PyGtkWidget created on the python side and add it to
      --  the Path view.
      Python_Widget := Get_User_Data
        (Obj  => Widget_From_PyObject
           (Get_PyObject
                (Execute (Self.Get_Python_Widget, Args))),
         Stub => Stub);
      Page_View.Append
        (Gtk_Widget (Python_Widget), Expand => True, Fill => True);
      Free (Args);
      return Gtk_Widget (Page_View);
   end Get_Widget;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Python_Preferences_Page_Record) is
   begin
      Free (Self.Get_Python_Widget);
      Free (Default_Preferences_Page_Record (Self));
   end Free;

   ----------------
   -- Set_Kernel --
   ----------------

   procedure Set_Kernel
     (Self   : not null access GPS_Preferences_Manager_Record;
      Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Self.Kernel := Kernel_Handle (Kernel);
   end Set_Kernel;

   ----------------
   -- Get_Kernel --
   ----------------

   function Get_Kernel
     (Self   : not null access GPS_Preferences_Manager_Record)
      return access GPS.Kernel.Kernel_Handle_Record'Class is
      (Self.Kernel);

   -------------------------
   -- Notify_Pref_Changed --
   -------------------------

   overriding procedure Notify_Pref_Changed
     (Self : not null access GPS_Preferences_Manager_Record;
      Pref : not null access Preference_Record'Class)
   is
      Font : Pango_Font_Description;
   begin
      Self.Nested_Pref_Changed := Self.Nested_Pref_Changed + 1;

      if Pref = Default_Font then
         Font := Copy (Default_Font.Get_Pref);
         Set_Size (Font, Gint (Float (Get_Size (Font)) * 0.8));
         Default_Preferences.Set_Pref
           (Small_Font, Self.Kernel.Preferences, Font);
         Free (Font);
      end if;

      if not Self.Is_Loading_Preferences then
         Trace (Me, "Preference changed: " & Pref.Get_Name);
         Preferences_Changed_Hook.Run
            (Self.Kernel, Default_Preferences.Preference (Pref));

         if Self.Nested_Pref_Changed = 1 then
            Save_Preferences (Self.Kernel);
         end if;
      end if;

      Self.Nested_Pref_Changed := Self.Nested_Pref_Changed - 1;

   exception
      when others =>
         Self.Nested_Pref_Changed := Self.Nested_Pref_Changed - 1;
   end Notify_Pref_Changed;

   ---------------------------------------
   -- Preferences_Page_Commands_Handler --
   ---------------------------------------

   procedure Preferences_Page_Commands_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
      Name   : constant String := Nth_Arg (Data, 1);
      pragma Unreferenced (Command);
   begin
      --  Don't register the preferences Path if the given name is an empty
      --  string.
      if Name = "" then
         return;
      end if;

      declare
         Page          : constant Python_Preferences_Path :=
                           new Python_Preferences_Page_Record'
                             (Default_Preferences_Page_Record with
                              Get_Python_Widget => Data.Nth_Arg (2));
         Priority      : constant Integer := Data.Nth_Arg (3, Default => -1);
         Is_Integrated : constant Boolean :=
                           Data.Nth_Arg (4, Default => False);
      begin
         Kernel.Get_Preferences.Register_Page
           (Name             => Name,
            Page             => Preferences_Page (Page),
            Priority         => Priority,
            Page_Type        =>
              (if Is_Integrated then Integrated_Page else Visible_Page),
            Replace_If_Exist => False);
      end;
   end Preferences_Page_Commands_Handler;

   -------------------------
   -- Get_Command_Handler --
   -------------------------

   procedure Get_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
      Class  : constant Class_Type := New_Class (Kernel, "Preference");
      Inst   : constant Class_Instance := Nth_Arg (Data, 1, Class);
   begin
      if Command = Constructor_Method then
         Set_Data (Inst, Class, String'(Nth_Arg (Data, 2)));

      elsif Command = "get" then
         declare
            Name : constant String     := Get_Data (Inst, Class);
            Pref : constant Preference :=
                      Get_Pref_From_Name (Kernel.Preferences, Name, False);
         begin
            if Pref = null then
               Set_Error_Msg (Data, -"Unknown preference " & Name);

            elsif Pref.all in Integer_Preference_Record'Class then
               Set_Return_Value
                 (Data, Integer'(Get_Pref (Integer_Preference (Pref))));

            elsif Pref.all in Boolean_Preference_Record'Class then
               Set_Return_Value
                 (Data, Boolean'(Get_Pref (Boolean_Preference (Pref))));

            elsif Pref.all in String_Preference_Record'Class
              or else Pref.all in Color_Preference_Record'Class
              or else Pref.all in Font_Preference_Record'Class
              or else Pref.all in Style_Preference_Record'Class
              or else Pref.all in Enum_Preference_Record'Class
              or else Pref.all in Theme_Preference_Record'Class
            then
               Set_Return_Value (Data, Get_Pref (Pref));

            else
               Set_Error_Msg (Data, -"Preference type not supported");
            end if;
         exception
            when others =>
               Set_Error_Msg (Data, -"Wrong parameters");
         end;

      elsif Command = "set" then
         declare
            Name : constant String     := Get_Data (Inst, Class);
            Pref : constant Preference :=
                     Get_Pref_From_Name (Kernel.Preferences, Name, False);
         begin
            if Pref = null then
               Set_Error_Msg (Data, -"Unknown preference " & Name);
            elsif Pref.all in Integer_Preference_Record'Class then
               Set_Pref
                 (Integer_Preference (Pref),
                  Kernel.Preferences,
                  Integer'(Nth_Arg (Data, 2)));
            elsif Pref.all in Boolean_Preference_Record'Class then
               Set_Pref
                 (Boolean_Preference (Pref),
                  Kernel.Preferences,
                  Boolean'(Nth_Arg (Data, 2)));
            elsif Pref.all in String_Preference_Record'Class
              or else Pref.all in Font_Preference_Record'Class
              or else Pref.all in Color_Preference_Record'Class
              or else Pref.all in Style_Preference_Record'Class
              or else Pref.all in Enum_Preference_Record'Class
              or else Pref.all in Theme_Preference_Record'Class
            then
               Set_Pref (Pref, Kernel.Preferences, String'(Nth_Arg (Data, 2)));

            else
               Set_Error_Msg (Data, -"Preference not supported");
            end if;

         exception
            when E : Invalid_Parameter =>
               Set_Error_Msg (Data, Exception_Message (E));
            when E : others =>
               Trace (Me, E);
         end;

      elsif Command = "create_style" then

         declare
            Path               : constant String := Get_Data (Inst, Class);
            Label              : constant String := Nth_Arg (Data, 2);
            Doc                : constant String := Nth_Arg (Data, 3, "");
            Default_Fg         : constant String := Nth_Arg (Data, 4, "");
            Default_Bg         : constant String := Nth_Arg
              (Data, 5, "rgba(0,0,0,0)");
            Default_Font_Style : constant String :=
              To_Lower (Nth_Arg (Data, 6, "default"));
            Default_Variant    : Variant_Enum;
            Pref               : Preference;
            pragma Unreferenced (Pref);
         begin
            if Default_Font_Style = "default"
              or else Default_Font_Style = "normal"
              or else Default_Font_Style = "italic"
              or else Default_Font_Style = "bold"
              or else Default_Font_Style = "bold_italic"
            then
               Default_Variant := Variant_Enum'Value (Default_Font_Style);
            else
               Set_Error_Msg (Data,
                              -"Wrong value for default_font_style parameter");
               return;
            end if;

            Pref := Preference
              (Create (Manager         => Kernel.Preferences,
                       Path            => Dir_Name (Path),
                       Name            => Path,
                       Label           => Label,
                       Doc             => Doc,
                       Default_Bg      => Default_Bg,
                       Default_Fg      => Default_Fg,
                       Default_Variant => Default_Variant,
                       Base            => Default_Style));
         end;

      elsif Command = "create" or else Command = "create_with_priority" then
         declare
            Has_Prio      : constant Boolean := Command /= "create";
            Doc_Param_Idx : constant Positive := (if Has_Prio then 5 else 4);
            Path          : constant String := Get_Data (Inst, Class);
            Label         : constant String := Nth_Arg (Data, 2);
            Typ           : constant String := Nth_Arg (Data, 3);
            Doc           : constant String :=
                              Nth_Arg (Data, Doc_Param_Idx, "");
            Priority      : constant Integer :=
                              (if Has_Prio then Data.Nth_Arg (4) else -1);
            Pref          : Preference;
            pragma Unreferenced (Pref);
         begin
            if Typ = "integer" then
               Pref := Preference (Create
                 (Manager  => Kernel.Preferences,
                  Path     => Dir_Name (Path),
                  Name     => Path,
                  Label    => Label,
                  Doc      => Doc,
                  Default  => Nth_Arg (Data, Doc_Param_Idx + 1, 0),
                  Minimum  => Nth_Arg (Data, Doc_Param_Idx + 2, Integer'First),
                  Maximum  => Nth_Arg (Data, Doc_Param_Idx + 3, Integer'Last),
                  Priority => Priority));

            elsif Typ = "boolean" then
               Pref := Preference (Boolean_Preference'(Create
                 (Manager  => Kernel.Preferences,
                  Path     => Dir_Name (Path),
                  Name     => Path,
                  Label    => Label,
                  Doc      => Doc,
                  Default  => Nth_Arg (Data, Doc_Param_Idx + 1, True),
                  Priority => Priority)));

            elsif Typ = "string" then
               Pref := Preference (String_Preference'(Create
                 (Manager  => Kernel.Preferences,
                  Path     => Dir_Name (Path),
                  Name     => Path,
                  Label    => Label,
                  Doc      => Doc,
                  Default  => Nth_Arg (Data, Doc_Param_Idx + 1, ""),
                  Priority => Priority)));

            elsif Typ = "multiline" then
               Pref := Preference (String_Preference'(Create
                 (Manager    => Kernel.Preferences,
                  Path       => Dir_Name (Path),
                  Name       => Path,
                  Label      => Label,
                  Doc        => Doc,
                  Multi_Line => True,
                  Default    => Nth_Arg (Data, Doc_Param_Idx + 1, ""),
                  Priority   => Priority)));

            elsif Typ = "color" then
               Pref := Preference (Color_Preference'(Create
                 (Manager  => Kernel.Preferences,
                  Path     => Dir_Name (Path),
                  Name     => Path,
                  Label    => Label,
                  Doc      => Doc,
                  Default  => Nth_Arg (Data, Doc_Param_Idx + 1, "black"),
                  Priority => Priority)));

            elsif Typ = "font" then
               Pref := Preference (Font_Preference'(Create
                 (Manager  => Kernel.Preferences,
                  Path     => Dir_Name (Path),
                  Name     => Path,
                  Label    => Label,
                  Doc      => Doc,
                  Default  =>
                    Nth_Arg
                      (Data, Doc_Param_Idx + 1, Defaults.Default_Font),
                  Priority => Priority)));

            elsif Typ = "enum" then
               declare
                  Val : constant String_List_Access :=
                    new GNAT.Strings.String_List
                      (1 .. Number_Of_Arguments (Data) - (Doc_Param_Idx  + 1));
                  --  Freed when the preference is destroyed
               begin
                  for V in Val'Range loop
                     Val (V) :=
                       new String'(Nth_Arg (Data, Doc_Param_Idx + 1 + V));
                  end loop;

                  Pref := Preference (Choice_Preference'(Create
                    (Manager => Kernel.Preferences,
                     Path    => Dir_Name (Path),
                     Name    => Path,
                     Label   => Label,
                     Doc     => Doc,
                     Choices => Val,
                     Default => Nth_Arg (Data, Doc_Param_Idx + 1))));
               end;

            else
               Set_Error_Msg (Data, -"Invalid preference type");
               return;
            end if;

            Set_Return_Value (Data, Inst);
         end;
      end if;
   end Get_Command_Handler;

   ---------------------------------
   -- Register_Global_Preferences --
   ---------------------------------

   procedure Register_Global_Preferences
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Manager : constant Preferences_Manager := Kernel.Get_Preferences;
      Page    : Preferences_Page;
      Group   : Preferences_Group;
   begin
      Manager.Set_Is_Loading_Prefs (True);

      --  Preferences Assistant --
      Manager.Register_Page
        (Name             => "Preferences Assistant General",
         Page             => new Default_Preferences_Page_Record,
         Page_Type        => Assistant_Page);

      --  Advanced  --
      System_Menus := Manager.Create_Invisible_Pref
        (Name     => "system-menus",
         Label    => -"System menus",
         Doc      =>
           -("Display menubar outside of the GNAT Studio window on systems"
             & " that support it (OSX and Unity). No effect on other"
             & " systems."),
         Default  => False);

      --  LibAdaLang --
      Use_LAL_In_Outline := Manager.Create_Invisible_Pref
        (Name     => "use-lal-in-outline",
         Label    => -"Use LAL in Outline",
         Doc      => -("Enable usage of libadalang in the Outline View."),
         Default  => Active (Experimental_LAL));

      Use_LAL_In_Shell := Manager.Create_Invisible_Pref
        (Name     => "use-lal-in-shell",
         Label    => -"Use LAL in Shell",
         Doc      => -("Enable usage of libadalang in GPS.SemanticTree " &
                        " shell commands."),
         Default  => Active (Experimental_LAL));

      Use_LAL_In_Info := Manager.Create_Invisible_Pref
        (Name     => "use-lal-in-info",
         Label    => -"Use LAL in menu",
         Doc      => -("Enable usage of libadalang to generate " &
                        " 'GNAT Runtime' menu."),
         Default  => Active (Experimental_LAL));

      Use_LAL_In_GNATHUB := Manager.Create_Invisible_Pref
        (Name     => "use-lal-in-gnathub",
         Label    => -"Use LAL in gnathub",
         Doc      => -("Enable usage of libadalang in gnathub module."),
         Default  => Active (Experimental_LAL));

      Use_LAL_In_COV := Manager.Create_Invisible_Pref
        (Name     => "use-lal-in-cov",
         Label    => -"Use LAL in cov",
         Doc      => -("Enable usage of libadalang in coverage module."),
         Default  => Active (Experimental_LAL));

      Use_LAL_In_Indent := Manager.Create_Invisible_Pref
        (Name     => "use-lal-in-indent",
         Label    => -"Use LAL in indent",
         Doc      => -("Enable usage of libadalang in indent."),
         Default  => False);

      Use_LAL_In_Highlight := Manager.Create_Invisible_Pref
        (Name     => "use-lal-in-highlight",
         Label    => -"Use LAL to highlight",
         Doc      => -("Enable usage of libadalang to highlight code."),
         Default  => Active (Experimental_LAL));

      -- General --

      Gtk_Theme := Manager.Create
        (Path  => -"General/Custom Styles:Theme",
         Name  => "GPS6-Gtk-Theme-Name",  --  synchronize with colorschemes.py
         Label => -"Theme",
         Doc   => -("Styles the tabs, tree views, buttons and UI elements."));

      Animations := Manager.Create
        (Path    => -"General/Custom Styles:Animations",
         Default => False,
         Name    => "Enable-Animations",
         Label   => "Enable animations",
         Doc     => -("Enables UI animations and smooth scrolling." & ASCII.LF
           & "(Changing this does not affect views and editors that"
           & " are currently open.)"));

      Default_Font := Manager.Create
        (Path    => -"General/Custom Styles:Fonts",
         Name    => "General-Default-Font",
         Default => Defaults.Default_Font,
         Doc     => -("Font in menus, browsers,..."),
         Label   => -"Default font");

      Small_Font := Manager.Create
        (Path    => -":Fonts & Colors",
         Name    => "General-Small-Font",
         Default => Defaults.Default_Font,
         Doc     =>
           -("Used by GNAT Studio to display secondary information."),
         Label   => -"Small font");

      View_Fixed_Font := Manager.Create
        (Path    => -"General/Custom Styles:Fonts",
         Name    => "General-Fixed-Font",
         Default => Defaults.Default_Fixed_Font,
         Doc     => -("Fixed-size font used in most views "
                      & "(Outline, Locations, Messages, ...)."),
         Label   => -"Monospace font");

      Command_Windows_Bg_Color := Manager.Create
        (Path    => -"General/Custom Styles:Other",
         Name    => "Command-Windows-Background-Color",
         Label   => -"Command windows background",
         Doc     => -("Background color for command windows "
           & "(isearch, zap to char, ...)."),
         Default => "#FFFFFF");

      Use_Native_Dialogs := Manager.Create
        (Path    => ":Windows",
         Name    => "General-Use-Native-Dialogs",
         Label   => -"Native dialogs",
         Doc     =>
           -"Use OS native dialogs instead of GNAT Studio-specific ones.",
         Default => True);

      Splash_Screen := Manager.Create
        (Path     => -"General:Behavior",
         Name     => "General-Splash-Screen",
         Label    => -"Display splash screen",
         Doc      => -"Display a splash screen while GNAT Studio starts.",
         Default => True);

      Display_Welcome := Manager.Create
        (Path    => -"General:Behavior",
         Name    => "General-Display-Welcome",
         Label   => -"Display welcome window",
         Doc     =>
            -"Show dialog to select the project, when none was specified.",
         Default => True);

      Auto_Save := Manager.Create
        (Path    => -"General:Behavior",
         Name    => "General-Auto-Save",
         Label   => -"Auto save",
         Doc     =>
           -("Save files and projects automatically before running tools"
             & " and compiling."),
         Default => True);

      Desktop_Backup_Save := Manager.Create
        (Path    => -"General:Behavior",
         Name    => "General-Desktop-Backup-Save",
         Label   => -"Perform backup saves in case of crash/freeze.",
         Doc     =>
            -("Save size and position of views in a backup file. Use this file"
              & " in the next session to restore the desktop."),
         Default => False);

      Save_Desktop_On_Exit := Manager.Create
        (Path    => -"General:Behavior",
         Name    => "General-Save-Desktop-On-Exit",
         Label   => -"Save desktop on exit",
         Doc     =>
            -("Save size and position of views on exit. Ignored"
              & " when you work with a default project."),
         Default => True);

      Hyper_Mode := Manager.Create
        (Path    => -"General:Behavior",
         Name    => "Hyper-Mode",
         Default => True,
         Doc     =>
            -"Display hyper links in editors when you press Control.",
         Label   => -"Hyper links");

      Multi_Language_Builder := Multi_Language_Builder_Policy_Prefs.Create
        (Manager => Manager,
         Name    => "General-Default-Builder",
         Label   => -"Default builder",
         Doc     =>
           -("Select the builder to use when compiling sources. Gprbuild is "
             & "the recommend solution especially for multi-language builds."),
         Path    => -"General:Behavior",
         Default => Default_Builder);

      Save_Editor_Desktop := Editor_Desktop_Policy_Prefs.Create
        (Manager => Manager,
         Name    => "General-Editor-Desktop-Policy",
         Label   => "Save editor in desktop",
         Doc     =>
            -("Select which editors to save in the desktop and restore"
              & " on startup."),
         Path    => -"General:Behavior",
         Default => From_Project);

      Max_Nb_Of_Log_Files := Manager.Create
        (Name    => "max-nb-of-log-files",
         Minimum => 1,
         Default => 10,
         Maximum => 500,
         Label   => "Maximum number of log files",
         Doc     =>
           -("The maximum number of log files preserved by GNAT Studio in "
           &  "the <b>GNATSTUDIO_HOME/.gnatstudio/log/</b> directory."),
         Path    => -"General:Behavior");

      Pref_Toolbar_Style := Toolbar_Icons_Size_Preferences.Create
        (Manager,
         Path    => -"General/Custom Styles:Other",
         Name    => "GPS6-General-Toolbar-Style",
         Label   => -"Toolbar style",
         Doc     => -"Style the toolbar.",
         Default => Small_Icons);

      GPS.Kernel.Charsets.Register_Preferences (Kernel);

      -- Source Editor --

      Display_Subprogram_Names := Manager.Create
        (Name    => "Src-Editor-Display-Subprogram_Names",
         Default => True,
         Doc => -"Show the name of the current subprogram in the status line.",
         Label   => -"Display subprogram names",
         Path    => -"Editor:Display");

      Display_Tooltip := Manager.Create
        (Name    => "Src-Editor-Display-Tooltip",
         Default => True,
         Doc     => -"Show tooltips with information on selected entity.",
         Label   => -"Tooltips",
         Path    => -"Editor:Display");

      Current_Line_Highlighting := Current_Line_Highlighting_Prefs.Create
        (Manager => Manager,
         Name    => "Src-Editor-Current-Line-Highlighting",
         Default => Gutter_Only,
         Doc     =>
            -"Select the way GNAT Studio will highlight the current line.",
         Label   => -"Current line highlighting",
         Path    => -"Editor:Highlighting");

      Alter_Bg_For_RO_Files := Manager.Create
        (Name    => "Alter-Bg-For-RO-Files",
         Label   => -"Change background of read-only editors",
         Doc     =>
            -"Alter the editor background color for read-only files.",
         Default => True,
         Path    => -"Editor:Display");

      Display_Line_Numbers := Line_Number_Policy_Prefs.Create
        (Manager => Manager,
         Name    => "GPS6-Src-Editor-Display-Line_Numbers",
         Default => All_Lines,
         Doc     => -"Display line numbers on the side of editors.",
         Label   => -"Display line numbers",
         Path    => -"Editor:Display");

      Highlight_Column := Manager.Create
        (Name    => "Src-Editor-Highlight-Column",
         Minimum => 0,
         Maximum => 255,
         Default => 80,
         Doc     =>
            -("Draw a vertical line based on line length. This also"
              & " impacts the refill command. Set to 0 to disable."),
         Label   => -"Right margin",
         Path    => -"Editor:Display");

      Gutter_Right_Margin := Manager.Create
        (Name    => "Src-Editor-Gutter-Right-Margin",
         Minimum => 0,
         Maximum => 100,
         Default => 0,
         Doc     =>
           -("The space that will be added between the text contained in an "
           & "editor and its gutter (i.e: where line numbers are drawn "
           & "etc.)."),
         Label   => -"Gutter right margin",
         Path    => -"Editor:Display");

      Highlight_Delimiters := Manager.Create
        (Name    => "Src-Editor-Highlight-Delimiters",
         Default => True,
         Doc     => -"Highlight matching delimiters: (){}[]",
         Label   => -"Highlight delimiters",
         Path    => -"Editor:Highlighting");

      Block_Highlighting := Manager.Create
        (Name    => "Src-Editor-Block-Highlighting",
         Default => True,
         Doc     =>
            -("Highlight current block on the side of editors: procedures,"
              & " loops, if statements,..."),
         Label   => -"Block highlighting",
         Path    => -"Editor:Highlighting");

      Strip_Blanks := Strip_Trailing_Blanks_Policy_Prefs.Create
        (Manager => Manager,
         Name    => "Src-Editor-Strip-Trailing-Blanks",
         Label   => -"Strip blanks",
         Doc     => -"Remove trailing blanks at end of lines when saving.",
         Default => Autodetect,
         Path    => -"Editor:On Save");

      Strip_Lines := Strip_Trailing_Blanks_Policy_Prefs.Create
        (Manager => Manager,
         Name    => "Src-Editor-Strip-Trailing-Lines",
         Label   => -"Strip lines",
         Doc     => -"Remove trailing blank lines when saving.",
         Default => Autodetect,
         Path    => -"Editor:On Save");

      Line_Terminator := Line_Terminators_Prefs.Create
        (Manager => Manager,
         Name  => "Src-Editor-Line-Terminator",
         Label => -"Line terminator",
         Doc   => -"Override line terminators when saving.",
         Default => Unchanged,
         Path    => -"Editor:On Save");

      Auto_Indent_On_Paste := Manager.Create
        (Name    => "Src-Editor-Indent-On-Paste",
         Default => False,
         Doc     => -"Auto-indent new contents when pasting.",
         Label   => -"Auto indent on paste",
         Path    => -"Editor:Behavior");

      Transient_Mark := Manager.Create
        (Name    => "Src-Editor-Transient-Mark",
         Default => False,
         Doc     =>
           -("The selected region normally remains selected when the"
           & " clipboard is modified by a Cut/Copy/Paste operation. If this"
           & " option is set, the selection will become unset when the"
           & " clipboard is modified. This is similar to the Emacs mode"
           & " with the same name."),
         Label   => -"Transient mark",
         Path    => -"Editor:Behavior");

      Periodic_Save := Manager.Create
        (Name     => "Src-Editor-Periodic-Save",
         Minimum  => 0,
         Maximum  => 3600,
         Default  => 60,
         Doc      =>
            -("Autosave delay in seconds (0 to disable). Files are saved with"
              & " special name .#filename#."),
         Label    => -"Autosave delay",
         Path     => -"Editor:Behavior",
         Priority => -2);

      Automatic_Syntax_Check := Manager.Create
        (Name    => "Src-Editor-Automatic-Syntax-Check",
         Default => False,
         Doc     => -"Check syntax in the background.",
         Label   => -"Automatic syntax check",
         Path    => ":Editor");

      if Config.Host = Config.Windows then
         Use_ACL := Manager.Create
           (Name    => "Src-Editor-Use-ACL",
            Label   => -"Use Windows ACL",
            Doc     =>
               -"Use access control lists to change read/write permissions.",
            Default => False,
            Path    => -"Editor:Behavior");
      end if;

      Block_Folding := Manager.Create
        (Name    => "Src-Editor-Block-Folding",
         Default => True,
         Doc     => -"Enable block folding: subprograms, if statements,...",
         Label   => -"Block folding",
         Path    => -"Editor:Folding");

      Fold_With_Use_Blocks := Manager.Create
        (Name    => "Src-Editor-Fold-With-Use-Blocks",
         Label   => -"Fold with/use blocks",
         Doc     => -"Automatically fold 'with' and 'use' blocks when a " &
           "block has more lines than the setting (0 to disable).",
         Minimum => 0,
         Maximum => 999,
         Default => 0,
         Path    => -"Editor:Folding");

      Fold_Comments := Manager.Create
        (Name    => "Src-Editor-Fold-Comments",
         Default => False,
         Label   => -"Fold comments",
         Doc     => -"Calculate/show folding for comment blocks.",
         Path    => -"Editor:Folding");

      Autofold_Comment_Blocks := Manager.Create
        (Name    => "Src-Editor-Autofold-Comment-Blocks",
         Label   => -"Autofold comment blocks",
         Doc     => -"Automatically fold comment blocks when a block has " &
           " more lines than the setting (0 to disable).",
         Minimum => 0,
         Maximum => 999,
         Default => 0,
         Path    => -"Editor:Folding");

      Fold_Comment_Reg1 := Manager.Create
        (Name     => "Src-Editor-Fold-Comment-reg1",
         Label    => -"Fold comment regexp 1",
         Doc      => -"Automatically fold comment blocks when a block " &
           "contents match the regular expression.",
         Default  => "",
         Path     => -"Editor:Folding");

      Fold_Comment_Reg2 := Manager.Create
        (Name     => "Src-Editor-Fold-Comment-reg2",
         Label    => -"Fold comment regexp 2",
         Doc      => -"Automatically fold comment blocks when a block " &
           "contents match the regular expression.",
         Default  => "",
         Path     => -"Editor:Folding");

      Fold_Comment_Reg3 := Manager.Create
        (Name     => "Src-Editor-Fold-Comment-reg3",
         Label    => -"Fold comment regexp 3",
         Doc      => -"Automatically fold comment blocks when a block " &
           "contents match the regular expression.",
         Default  => "",
         Path     => -"Editor:Folding");

      Default_Style := Manager.Create
        (Name         => "Src-Editor-Reference-Style",
         Label        => -"Default",
         Doc          => -"Default font and background color for editors.",
         Default_Font => Defaults.Default_Fixed_Font,
         Default_Fg   => "black",
         Path         => -"Editor/Fonts & Colors:General");

      Blocks_Style := Manager.Create
        (Name            => "Src-Editor-Block-Variant",
         Label           => -"Block Highlighting",
         Base            => Default_Style,
         Doc             => "",
         Default_Variant => Default,
         Default_Fg      => "#60615F",
         Path            => -"Editor/Fonts & Colors:General");

      Types_Style := Manager.Create
        (Name         => "Src-Editor-Type-Variant",
         Label        => -"Types",
         Base            => Default_Style,
         Default_Variant => Default,
         Doc             => "",
         Default_Fg   => "#009900",
         Path         => -"Editor/Fonts & Colors:General");

      Keywords_Style := Manager.Create
        (Name         => "Src-Editor-Keywords-Variant",
         Label        => -"Keywords",
         Base            => Default_Style,
         Default_Variant => Default,
         Doc             => "",
         Default_Fg      => "#0000E6",
         Path            => -"Editor/Fonts & Colors:General");

      Comments_Style := Manager.Create
        (Name         => "Src-Editor-Comments-Variant",
         Label        => -"Comments",
         Base            => Default_Style,
         Doc             => "",
         Default_Variant => Default,
         Default_Fg   => "#969696",
         Path         => -"Editor/Fonts & Colors:General");

      Strings_Style := Manager.Create
        (Name         => "Src-Editor-Strings-Variant",
         Label        => -"Strings",
         Base            => Default_Style,
         Doc             => "",
         Default_Variant => Default,
         Default_Fg   => "#CE7B00",
         Path         => -"Editor/Fonts & Colors:General");

      Numbers_Style := Manager.Create
        (Name         => "Src-Editor-Numbers-Variant",
         Label        => -"Numbers",
         Base            => Default_Style,
         Doc             => "",
         Default_Variant => Default,
         Default_Fg   => "#FF3333",
         Path         => -"Editor/Fonts & Colors:General");

      Hyper_Links_Style := Manager.Create
        (Name         => "Src-Editor-Hyper-Links-Variant",
         Label        => -"Hyper links",
         Base            => Default_Style,
         Doc             => "",
         Default_Variant => Default,
         Default_Fg   => "blue",
         Path            => -"Editor/Fonts & Colors:General");

      Code_Annotations_Style := Manager.Create
        (Name            => "Src-Editor-Code-Annotations-Variant-18",
         Label           => -"Code annotations",
         Base            => Default_Style,
         Default_Variant => Default,
         Default_Fg      => "black",
         Default_Bg      => "rgba(226,226,226,0.4)",
         Doc             => "",
         Path            => -"Editor/Fonts & Colors:General");

      ---------
      -- LSP --
      ---------

      LSP_Namespace_Style := Manager.Create
        (Name         => "LSP-Semantic-Namespace",
         Label        => -"Namespace",
         Base            => Default_Style,
         Doc             => "",
         Default_Variant => Default,
         Default_Fg   => "#310142",
         Path            => -"Editor/Fonts & Colors:LSP semantic");

      LSP_Interface_Style := Manager.Create
        (Name         => "LSP-Semantic-Interface",
         Label        => -"Interface",
         Base            => Default_Style,
         Doc             => "",
         Default_Variant => Default,
         Default_Fg   => "#8900ba",
         Path            => -"Editor/Fonts & Colors:LSP semantic");

      LSP_Class_Style := Manager.Create
        (Name         => "LSP-Semantic-Class",
         Label        => -"Class",
         Base            => Default_Style,
         Doc             => "",
         Default_Variant => Default,
         Default_Fg   => "#550073",
         Path            => -"Editor/Fonts & Colors:LSP semantic");

      LSP_Function_Style := Manager.Create
        (Name         => "LSP-Semantic-Function",
         Label        => -"Subprogram",
         Base            => Default_Style,
         Doc             => "",
         Default_Variant => Default,
         Default_Fg   => "#b500f5",
         Path            => -"Editor/Fonts & Colors:LSP semantic");

      LSP_Struct_Style := Manager.Create
        (Name         => "LSP-Semantic-Struct",
         Label        => -"Structure",
         Base            => Default_Style,
         Doc             => "",
         Default_Variant => Default,
         Default_Fg   => "#015878",
         Path            => -"Editor/Fonts & Colors:LSP semantic");

      LSP_TypeParameter_Style := Manager.Create
        (Name         => "LSP-Semantic-TypeParameter",
         Label        => -"TypeParameter",
         Base            => Default_Style,
         Doc             => "",
         Default_Variant => Default,
         Default_Fg   => "#590034",
         Path            => -"Editor/Fonts & Colors:LSP semantic");

      LSP_Property_Style := Manager.Create
        (Name         => "LSP-Semantic-Property",
         Label        => -"Property",
         Base            => Default_Style,
         Doc             => "",
         Default_Variant => Default,
         Default_Fg   => "#bf0270",
         Path            => -"Editor/Fonts & Colors:LSP semantic");

      LSP_Enum_Style := Manager.Create
        (Name         => "LSP-Semantic-Enum",
         Label        => -"Enum",
         Base            => Default_Style,
         Doc             => "",
         Default_Variant => Default,
         Default_Fg   => "#a38a27",
         Path            => -"Editor/Fonts & Colors:LSP semantic");

      LSP_EnumMember_Style := Manager.Create
        (Name         => "LSP-Semantic-EnumMember",
         Label        => -"EnumMember",
         Base            => Default_Style,
         Doc             => "",
         Default_Variant => Default,
         Default_Fg   => "#c4a321",
         Path            => -"Editor/Fonts & Colors:LSP semantic");

      LSP_Parameter_Style := Manager.Create
        (Name         => "LSP-Semantic-Parameter",
         Label        => -"Parameter",
         Base            => Default_Style,
         Doc             => "",
         Default_Variant => Default,
         Default_Fg   => "#008cbf",
         Path            => -"Editor/Fonts & Colors:LSP semantic");

      LSP_Variable_Style := Manager.Create
        (Name         => "LSP-Semantic-Variable",
         Label        => -"Variable",
         Base            => Default_Style,
         Doc             => "",
         Default_Variant => Default,
         Default_Fg   => "#c93c00",
         Path            => -"Editor/Fonts & Colors:LSP semantic");

      LSP_Modifier_Style := Manager.Create
        (Name         => "LSP-Semantic-Modifier",
         Label        => -"Modifier",
         Base            => Default_Style,
         Doc             => "",
         Default_Variant => Default,
         Default_Fg   => "#00873f",
         Path            => -"Editor/Fonts & Colors:LSP semantic");

      LSP_Operator_Style := Manager.Create
        (Name         => "LSP-Semantic-Operator",
         Label        => -"Operator",
         Base            => Default_Style,
         Doc             => "",
         Default_Variant => Default,
         Default_Fg   => "#005427",
         Path            => -"Editor/Fonts & Colors:LSP semantic");

      LSP_Missing_Style := Manager.Create
        (Name         => "LSP-Semantic-Missing",
         Label        => -"Deprecated or missing",
         Base            => Default_Style,
         Doc             => "",
         Default_Variant => Default,
         Default_Fg   => "red",
         Path            => -"Editor/Fonts & Colors:LSP semantic");

      LSP_Readonly_Bg := Manager.Create
        (Name     => "LSP-Semantic-Readonly",
         Default  => "#f2ffed",
         Label    => -"Background for readonly",
         Doc      => "",
         Path     => -"Editor/Fonts & Colors:LSP semantic");

      Current_Block_Color := Manager.Create
        (Name     => "Src-Editor-Current-Block-Color",
         Default  => "#9C9CFF",
         Label    => -"Current block color",
         Path     => -"Editor/Fonts & Colors:General",
         Doc      => "",
         Priority => -2);

      Current_Line_Color := Manager.Create
        (Name     => "Src-Editor-Current-Line-Color",
         Default  => "rgba(226,226,226,0.4)",
         Label    => -"Current line color",
         Path     => -"Editor/Fonts & Colors:General",
         Doc      => "",
         Priority => -2);

      Annotated_Comments_Style := Manager.Create
        (Name         => "Src-Editor-Annotated-Comments-Variant",
         Label        => -"SPARK Annotations (--#)",
         Base            => Default_Style,
         Default_Variant => Default,
         Default_Fg   => "#60615F",
         Doc          => "",
         Path         => -"Editor/Fonts & Colors:SPARK");

      Aspects_Style := Manager.Create
        (Name         => "Src-Editor-Aspects-Variant",
         Label        => -"Ada/SPARK aspects",
         Base            => Default_Style,
         Default_Variant => Default,
         Default_Fg   => "#60615F",
         Doc          => "",
         Path         => -"Editor/Fonts & Colors:SPARK");

      Aspects_Blocks_Style := Manager.Create
        (Name            => "Src-Editor-Aspects-Block-Variant",
         Label           => -"Ghost names",
         Base            => Default_Style,
         Doc             => "",
         Default_Variant => Default,
         Default_Fg      => "#60615F",
         Path            => -"Editor/Fonts & Colors:SPARK");

      Aspects_Types_Style := Manager.Create
        (Name         => "Src-Editor-Aspects-Type-Variant",
         Label        => -"Types in ghost",
         Base            => Default_Style,
         Default_Variant => Default,
         Doc             => "",
         Default_Fg   => "#009900",
         Path         => -"Editor/Fonts & Colors:SPARK");

      Aspects_Keywords_Style := Manager.Create
        (Name         => "Src-Editor-Aspects-Keywords-Variant",
         Label        => -"Keywords in aspect",
         Base            => Default_Style,
         Default_Variant => Default,
         Doc             => "",
         Default_Fg      => "#0000E6",
         Path            => -"Editor/Fonts & Colors:SPARK");

      Aspects_Comments_Style := Manager.Create
        (Name         => "Src-Editor-Aspects-Comments-Variant",
         Label        => -"Comments in aspect",
         Base            => Default_Style,
         Doc             => "",
         Default_Variant => Default,
         Default_Fg   => "#969696",
         Path         => -"Editor/Fonts & Colors:SPARK");

      Aspects_Strings_Style := Manager.Create
        (Name         => "Src-Editor-Aspects-Strings-Variant",
         Label        => -"Strings in aspect",
         Base            => Default_Style,
         Doc             => "",
         Default_Variant => Default,
         Default_Fg   => "#CE7B00",
         Path         => -"Editor/Fonts & Colors:SPARK");

      Aspects_Numbers_Style := Manager.Create
        (Name         => "Src-Editor-Aspects-Numbers-Variant",
         Label        => -"Numbers in aspect",
         Base            => Default_Style,
         Doc             => "",
         Default_Variant => Default,
         Default_Fg   => "#FF3333",
         Path         => -"Editor/Fonts & Colors:SPARK");

      Ephemeral_Highlighting_Simple := Manager.Create
        (Name            => "Src-Editor-Ephemeral-Simple",
         Label           => -"Ephemeral highlighting (simple)",
         Doc             => -(
           "Style used for ephemeral highlighting in the editor for simple"
           & " cases, such as highlighting text-based matches."),
         Base            => Default_Style,
         Default_Variant => Default,
         Default_Fg      => "rgba(0,0,0,0.0)",
         Default_Bg      => "rgba(134,134,134,0.35)",
         Path            => -"Editor/Fonts & Colors:Ephemeral highlighting");

      Ephemeral_Highlighting_Smart := Manager.Create
        (Name            => "Src-Editor-Ephemeral-Smart",
         Label           => -"Ephemeral highlighting (smart)",
         Doc             => -(
           "Style used for ephemeral highlighting of context-sensitive"
           & " information, such as highlighting of matching entities."),
         Base            => Default_Style,
         Default_Variant => Default,
         Default_Fg      => "rgba(0,0,0,0.0)",
         Default_Bg      => "rgba(252,172,79,0.4)",
         Path            => -"Editor/Fonts & Colors:Ephemeral highlighting");

      Bookmark_Color            := Manager.Create
        (Name            => "Src-Editor-Bookmarks",
         Label           => -"Lines with a bookmark",
         Doc             => -"Highlight color for lines with a bookmark",
         Default         => "rgba(205,0,255,0.3)",
         Path            => -"Editor/Fonts & Colors:Bookmarks");

      -- Refactoring --

      Add_Subprogram_Box := Manager.Create
        (Name    => "Refactoring-Subprogram-Box",
         Default => True,
         Doc     =>
            -("Add a comment box with the subprogram name when"
              & " creating new subprograms."),
         Label   => -"Subprogram Box",
         Path    => -"Refactoring:Subprograms");

      Add_In_Keyword := Manager.Create
        (Name    => "Refactoring-In-Keyword",
         Default => False,
         Doc     =>
            -("Add ""in"" keyword in parameter lists when creating new"
              & " subprograms."),
         Label   => -"Add ""in"" Keyword",
         Path    => -"Refactoring:Subprograms");

      Create_Subprogram_Decl  := Manager.Create
        (Name    => "Refactoring-Subprogram-Spec",
         Default => True,
         Doc     =>
            -"Create a separate declaration when creating new subprograms.",
         Label   => -"Create Subprogram Declarations",
         Path    => -"Refactoring:Subprograms");

      -- Browsers --

      Browsers_Bg_Color := Manager.Create
        (Name    => "Browsers-Bg-Color",
         Default => "#FFFFFF",
         Doc     => -"Background of the browsers.",
         Label   => -"Background",
         Path    => -"Browsers:Colors");

      Selected_Link_Color := Manager.Create
        (Name    => "browsers-link-to-selected-color",
         Default => "rgba(230,50,50,0.7)",
         Doc     => -"Links between selected items.",
         Label   => -"Selected links",
         Path    => -"Browsers:Colors");

      Unselected_Link_Color := Manager.Create
        (Name    => "browsers-link-color",
         Default => "rgba(180,180,180,0.7)",
         Doc     => -"Links between unselected items.",
         Label   => -"Links",
         Path    => -"Browsers:Colors");

      Parent_Linked_Item_Color := Manager.Create
        (Name    => "browsers-linked-item-outline",
         Default => "rgba(0,168,180,0.4)",
         Doc     => -"Background of the items linked to selected items.",
         Label   => -"Ancestor items",
         Path    => -"Browsers:Colors");

      Child_Linked_Item_Color := Manager.Create
        (Name    => "Browsers-Child-Linked-Item-Color",
         Default => "#DDDDDD",
         Doc     => -"Background of the items linked from selected items.",
         Label   => -"Offspring items",
         Path    => -"Browsers:Colors");

      Selected_Item_Color := Manager.Create
        (Name    => "browsers-selected-item-outline",
         Default => "rgba(138,226,52,0.7)",
         Doc     => -"Color to use to draw the selected item.",
         Label   => -"Selected items",
         Path    => -"Browsers:Colors");

      Title_Color := Manager.Create
        (Name     => "Browsers-Title-Color",
         Label    => -"Title background",
         Doc      => -"Item title background",
         Path     => ":Browsers",
         Default  => "#BEBEBE");

      Browsers_Vertical_Layout := Manager.Create
        (Name    => "Browsers-Vertical-Layout",
         Default => False,
         Doc     =>
            -("General direction for layout of items. Does not apply to"
              & " entities browser."),
         Label   => -"Vertical layout",
         Path    => -"Browsers:Display");

      -- Diff_Utils --

      Diff_Mode := Vdiff_Modes_Prefs.Create
        (Manager => Manager,
         Name    => "Diff-Utils-Mode",
         Label   => "Mode",
         Doc     =>
         -("Unified: show differences directly in the editor" & ASCII.LF
           & "Side By Side: show differences in separate editors."),
         Default => Side_By_Side,
         Path    => -"Visual diff:General");

      Diff_Cmd := Manager.Create
        (Name    => "Diff-Utils-Diff",
         Label   => -"Diff command",
         Doc     =>
            -"Command and arguments to compute differences between two files.",
         Default => Config.Default_Diff_Cmd,
         Path    => -"Visual diff:General");

      Patch_Cmd := Manager.Create
        (Name    => "Diff-Utils-Patch",
         Label   => -"Patch command",
         Doc     => -"Command and arguments to apply a patch.",
         Default => Config.Default_Patch_Cmd,
         Path    => -"Visual diff:General");

      -- Messages --

      Message_Highlight := Manager.Create
        (Name    => "Messages-Highlight-Color",
         Label   => -"GNAT Studio error messages",
         Doc     =>
           -"Color for the GNAT Studio error messages displayed in the "
         & "Messages view.",
         Default => "#FF0000",
         Path    => -"Messages:GPS & Editors");

      Search_Src_Highlight := Manager.Create
        (Name    => "Search-Src-Highlight-Color",
         Label   => -"Search highlighting",
         Doc     => -"Color for search results.",
         Default => "#BDD7FF",
         Path    => -"Messages:GPS & Editors");

      High_Messages_Highlight := Manager.Create
        (Name    => "High-Importance-Messages-Highlight",
         Label   => -"High importance messages",
         Path    => -"Messages:GPS & Editors",
         Doc     => -("Color for high priority messages "
           & "(e.g: compiler errors). This preference is also used by "
           & " external tools integrated in GNAT Studio (e.g: GNAT SAS)."),
         Default => "#FFB7B7");

      Medium_Messages_Highlight := Manager.Create
        (Name    => "Medium-Importance-Messages-Highlight",
         Label   => -"Medium importance messages",
         Path    => -"Messages:GPS & Editors",
         Doc     => -("Color for medium priority messages (e.g: compiler "
           & "warnings). This preference is also used by external tools "
           & "integrated in GNAT Studio (e.g: GNAT SAS)."),
         Default => "#FFCC9C");

      Low_Messages_Highlight := Manager.Create
        (Name    => "Low-Importance-Messages-Highlight",
         Label   => -"Low importance messages",
         Path    => -"Messages:GPS & Editors",
         Doc     => -("Color for low priority messages (e.g: style errors). "
           & "This preference is also used by external tools integrated in "
           & "GNAT Studio (e.g: GNAT SAS)."),
         Default => "#FFFFF0");

      Info_Messages_Highlight := Manager.Create
        (Name    => "Info-Messages-Highlight",
         Label   => -"Informational messages",
         Path    => -"Messages:GPS & Editors",
         Doc     => -("Color for informational messages (e.g: compiler "
           & "infos). This preference is also used by external tools "
           & "integrated in GNAT Studio (e.g: GNAT SAS)."),
         Default => "#BDE5F8");

      Annotation_Messages_Highlight := Manager.Create
        (Name    => "Annotation-Messages-Highlight",
         Label   => -"Annotation messages",
           Path    => -"Messages:GPS & Editors",
         Doc     => -("Color for annotation messages. This preference is "
           & "also used by external tools integrated in GNAT Studio "
           & "(e.g: GNAT SAS)."),
         Default => "#E0E0E0");

      File_Pattern := Manager.Create
        (Name    => "Messages-File-Regpat-1",
         Label   => -"File pattern",
         Doc     =>
           -"Pattern used to detect file locations (e.g error messages)",
         Default =>
           "^([^:]:?[^:]*):(\d+):((\d+):)? " &
           "(((medium )?warning|medium:)?(info|Note|check)?" &
           "(\(style|low:|low warning:)?.*)",
         Path => ":Compiler messages");

      File_Pattern_Index := Manager.Create
        (Name    => "Messages-File-Regexp-Index",
         Minimum => 1,
         Maximum => 99,
         Default => 1,
         Doc     => -"Index of filename in the pattern",
         Label   => -"File index",
         Path => ":Compiler messages");

      Line_Pattern_Index := Manager.Create
        (Name    => "Messages-Line-Regexp-Index",
         Minimum => 1,
         Maximum => 99,
         Default => 2,
         Doc     => -"Index of line number in the pattern",
         Label   => -"Line index",
         Path => ":Compiler messages");

      Column_Pattern_Index := Manager.Create
        (Name    => "Messages-Column-Regexp-Index",
         Minimum => 0,
         Maximum => 99,
         Default => 4,
         Doc     => -"Index of column number in the pattern, 0 if none",
         Label   => -"Column index",
         Path => ":Compiler messages");

      Message_Pattern_Index := Manager.Create
        (Name    => "Messages-Message-Regexp-Index",
         Minimum => 0,
         Maximum => 99,
         Default => 5,
         Doc     => -"Index of message in the pattern, 0 if none",
         Label   => -"Message index",
         Path => ":Compiler messages");

      Console_Max_Length := Manager.Create
        (Name    => "Consoles-Buffer-Maximum-Length",
         Minimum => -1,
         Maximum => 100000,
         Default => 2000,
         Doc     =>
           -("Number of lines to display in the console. "
           & "Older lines are removed when the text exceeds this. "
           & "Set to -1 for unlimited."),
         Label   => -"Console maximum length",
         Path    => ":Consoles");

      Console_Max_Width := Manager.Create
        (Name    => "Consoles-Buffer-Maximum-Width",
         Minimum => -1,
         Maximum => 10000,
         Default => 500,
         Doc     =>
           -("The maximum character length to accept in outside output. "
           & "Lines exceeding this length are split at that mark. "
           & "Set to -1 for unlimited."),
         Label   => -"Console maximum width",
         Path    => ":Consoles");

      Warning_Pattern_Index := Manager.Create
        (Name    => "Messages-Warning-Regexp-Index",
         Minimum => 0,
         Maximum => 99,
         Default => 6,
         Doc     => -"Index of warning indication in the pattern, 0 if none",
         Label   => -"Warning index",
         Path => ":Compiler messages");

      Info_Pattern_Index := Manager.Create
        (Name    => "Messages-Info-Regexp-Index",
         Minimum => 0,
         Maximum => 99,
         Default => 8,
         Doc     => -"Index of compiler info in the pattern, 0 if none",
         Label   => -"Info index",
         Path => ":Compiler messages");

      Style_Pattern_Index := Manager.Create
        (Name    => "Messages-Style-Regexp-Index-1",
         Minimum => 0,
         Maximum => 99,
         Default => 9,
         Doc     => -"Index of style indication in the pattern, 0 if none",
         Label   => -"Style index",
         Path => ":Compiler messages");

      Secondary_File_Pattern := Manager.Create
        (Name    => "GPS6-Messages-Secondary-File-Regexp",
         Label   => -"Secondary File pattern",
         Doc     =>
           -"Pattern used to detect secondary file locations in messages",
         Default => "(([^:( ]+):(\d+)(:(\d+):?)?)",
         Path => ":Compiler messages");

      Secondary_File_Pattern_Index := Manager.Create
        (Name    => "GPS6-Messages-Secondary-File-Regexp-Index",
         Minimum => 1,
         Maximum => 99,
         Default => 2,
         Doc     => -"Index of secondary filename in the pattern",
         Label   => -"Secondary File index",
         Path => ":Compiler messages");

      Secondary_Line_Pattern_Index := Manager.Create
        (Name    => "GPS6-Messages-Secondary-Line-Regexp-Index",
         Minimum => 1,
         Maximum => 99,
         Default => 3,
         Doc     => -"Index of secondary location line number in the pattern",
         Label   => -"Secondary Line index",
         Path => ":Compiler messages");

      Secondary_Column_Pattern_Index := Manager.Create
        (Name    => "GPS6-Messages-Secondary-Column-Regexp-Index",
         Minimum => 0,
         Maximum => 99,
         Default => 5,
         Doc     =>
         -"Index of secondary column number in the pattern, 0 if none",
         Label   => -"Secondary Column index",
         Path => ":Compiler messages");

      Alternate_Secondary_Pattern := Manager.Create
        (Name    => "GPS6-Messages-Alternate-Secondary-Regpat",
         Label   => -"Alternate secondary pattern",
         Doc     =>
           -"Pattern used to detect alternate secondary locations in messages",
         Default => "(at line (\d+))",
         Path => ":Compiler messages");

      Alternate_Secondary_Line_Index := Manager.Create
        (Name    => "GPS6-Messages-Alternate-Secondary-Line",
         Label   => -"Alternate secondary line index",
         Doc     =>
           -"Index of secondary location line number in the alternate pattern",
         Minimum => 1,
         Maximum => 99,
         Default => 2,
         Path => ":Compiler messages");

      Preserve_Messages := Kernel.Get_Preferences.Create_Invisible_Pref
        ("locations-preserve-messages", True,
         Label => -"Preserve messages",
         Doc => -"Keep build messages for files that are not being compiled");

      Location_Only_High_Messages :=
        Kernel.Get_Preferences.Create_Invisible_Pref
        ("locations-only-high-messages", False,
         Label => -"Show only errors",
         Doc =>
           -"Only show the messages of high importance in the Locations view");

      -- Project Editor --

      Default_Switches_Color := Manager.Create
        (Name    => "Prj-Editor-Default-Switches-Color",
         Default => "#777777",
         Doc     => -("Color to display switches that are set"
                      & " as default for all the files in the project"),
         Label   => -"Default switches color",
         Path    => ":Switches editor");

      Switches_Editor_Title_Font := Manager.Create
        (Name    => "Prj-Editor-Title-Font",
         Default => "sans bold oblique 14",
         Doc     => -"Font to use for the switches editor dialog",
         Label   => -"Title font",
         Path    => ":Switches editor");

      Variable_Ref_Background := Manager.Create
        (Name    => "Prj-Editor-Var-Ref-Bg",
         Default => "#AAAAAA",
         Doc     => -("Color to use for the background of variable"
                      & " references in the value editor"),
         Label   => -"Variable reference color",
         Path    => ":Scenario editor");

      Invalid_Variable_Ref_Background := Manager.Create
        (Name    => "Prj-Editor-Invalid-Var-Ref-Bg",
         Default => "#AA0000",
         Doc     => -("Color to use for the foreground of invalid variable"
                      & " references"),
         Label   => -"Invalid references color",
         Path    => ":Scenario editor");

      Generate_Relative_Paths := Manager.Create
        (Name    => "Prj-Editor-Generate-Relative-Paths",
         Default => True,
         Doc     => -"Save relative paths in projects, not absolute paths.",
         Label   => -"Relative project paths",
         Path    => -"Project:General");

      Trusted_Mode := Manager.Create
        (Name    => "Prj-Editor-Trusted-Mode",
         Default => True,
         Doc     =>
            -("Assume projects and files do not use symbolic links to speed"
              & " up loading of projects."),
         Label   => -"Fast Project Loading",
         Path    => -"Project:General");

      Hidden_Files_Pattern := Manager.Create
        (Name  => "Project-Hidden-Directories-Regexp",
         Label => -"Hidden files pattern",
         Doc   => -"Match basenames of files to hide in the GUI.",
         Default => "^((\.[^\.]+.*)|CVS)$",
         Path    => -":Views");

      Show_Hidden_Files := Manager.Create
        (Name   => "explorer-show-hidden-directories",
         Label  => -"Show hidden files",
         Doc    => -"Hide files that match the Hide Files Pattern preference",
         Default => False,
         Path    => -":Views");

      Show_Ellipsis := Manager.Create
        (Name    => "explorer-show-ellipsis",
         Default => False,
         Label   => -"Ellipsize long file names in views",
         Doc     =>
           -("Long file names are truncated to fit available space,"
             & "  instead of using horizontal scrolling"),
         Path    => -":Views");

      -- Wizards --

      Wizard_Title_Font := Manager.Create
        (Name    => "Wizard-Title-Font",
         Default => "sans bold oblique 10",
         Doc     => -"Font to use for the title of the pages in the wizard",
         Label   => -"Title font",
         Path    => ":Fonts & Colors");

      -- External Commands --

      List_Processes := Manager.Create
        (Name     => "Helpers-List-Processes",
         Label    => -"List processes",
         Doc      => -"Command to list processes running on the machine.",
         Default  => Config.Default_Ps,
         Path     => -"External Commands:General");

      Execute_Command := Manager.Create
        (Name    => "Helpers-Execute-Command",
         Label   => -"Execute command",
         Doc     => -"Program to execute commands externally.",
         Default => Config.Exec_Command,
         Path    => -"External Commands:General");

      if Config.Host /= Config.Windows then
         --  Preference not used under Windows

         Html_Browser := Manager.Create
           (Name    => "Helpers-HTML-Browser",
            Label   => -"HTML browser",
            Doc     =>
              -"Override the system's default browser. Use %u for the URL.",
            Default => "",
            Path    => -"External Commands:Browser");
      end if;

      Print_Command := Manager.Create
        (Name    => "Helpers-Print-Command",
          Label  => -"Print command",
          Doc    =>
            -"Command to print files. On Windows, defaults to built-in.",
         Default => Config.Default_Print_Cmd,
         Path    => -"External Commands:General");

      Max_Output_Length := Manager.Create
        (Name    => "Max-Output-Length",
         Label   => -"Maximum output length",
         Doc     => -"Maximum size of output read by GNAT Studio, in bytes.",
         Minimum => 1_000,
         Maximum => Integer'Last,
         Default => 10_000_000,
         Path    => ":Commands");

      -- Windows --

      Doc_Search_Before_First := Manager.Create
        (Name    => "Doc-Search-Before-First",
         Label   => -"Leading documentation",
         Doc     =>
           -("Extract documentation"
           & " for an entity by first looking at the leading comments, and"
           & " fallback to the comments after the entity declaration if not"
           & " found (reversed when preference is disabled)."),
         Default => False,
         Path    => -"Documentation:General");

      -- Debugger --

      Debugger_Current_Line_Color := Manager.Create
        (Name      => "Debugger-Editor-Current-Line",
         Label     => -"Debugger Current line",
         Doc       =>
           -"Color to highlight the debugger current line in editors.",
         Path      => -"Debugger:Editors",
         Default   => "rgba(125,236,57,0.6)");

      Breakpoint_Color := Manager.Create
        (Name      => "Debugger-Line-With-Breakpoint",
         Label     => -"Line with breakpoint",
         Doc       => -"Color to highlight lines with breakpoints.",
         Path      => -"Debugger:Editors",
         Default   => "rgba(0,0,255,0.3)");

      Conditional_Breakpoint_Color := Manager.Create
        (Name      => "Debugger-Line-With-Conditional-Breakpoint",
         Label     => -"Line with conditional breakpoint",
         Doc      => -"Color to highlight lines with conditional breakpoints.",
         Path      => -"Debugger:Editors",
         Default   => "rgba(0,255,0,0.3)");

      Disabled_Breakpoint_Color := Manager.Create
        (Name      => "Debugger-Line-With-Disabled-Breakpoint",
         Label     => -"Line with disabled breakpoint",
         Doc       => -"Color to highlight lines with disabled breakpoints.",
         Path      => -"Debugger:Editors",
         Default   => "rgba(255,0,0,0.3)");

      GNAThub_Semantic_Pass := Manager.Create
        (Name    => "GNAThub-Semantic-Pass",
         Label   => -"Extra Semantic Pass",
         Doc     =>
           -("When retrieving the data from the gnathub.db, "
           & "do a semantic pass to correct the locations and add extra "
           & "information like the entity type icon. Disabling this preference"
           & " will speedup the process at the cost of imprecise data."),
         Default => True,
         Path    => -"GNAThub");

      Explicit_Default_Value := Manager.Create_Invisible_Pref
        (Name    => "Scenario-Explicit-Default-Value",
         Label   => -"Always pass on command line",
         Doc     =>
           -("When False, only add -X switches when a scenario variable is "
             & "not set to its default value."),
         Default => True);

      LSP_Use_Snippets := Kernel.Get_Preferences.Create
        (Name    => "LSP-Completion-Use-Snippets",
         Default => False,
         Label   => "Use snippets",
         Doc     => "Control whether snippets should be "
         & "inserted when completing (i.e: for subprograms with "
         & "parameters, aggregates etc.). The language "
         & "server should be restarted manually "
         & "('Navigate/Restart Language Server...' menu) to take the "
         & "new preference value into account.",
         Path    => "LSP:Completion");

      LSP_Use_Signatures := Kernel.Get_Preferences.Create
        (Name    => "LSP-Use-Signatures",
         Default => True,
         Label   => "Enable Signature Help",
         Doc     => "Control whether the signature help window is displayed "
         & "in the editors.",
         Path    => "LSP:Completion");

      LSP_Limit_Formatting := Kernel.Get_Preferences.Create
        (Name    => "LSP-Limit-Formatting",
         Default => False,
         Label   => "Limit LSP formatting",
         Doc     => "Limit LSP formatting request to the current selection: "
         & "it prevents overzealous formatting affecting unselected lines.",
         Path    => "LSP:Formatting");

      LSP_Ada_Insert_With_Clauses := Kernel.Get_Preferences.Create
          (Name    => "LSP-Ada-Insert-With-Clauses",
           Default => True,
           Label   => "Insert with clauses",
           Doc     => "Insert missing with-clauses when "
           & "accepting completion for invisible symbols.",
           Path    => "Editor/Ada:Completion");

      LSP_Ada_Diagnostics := Kernel.Get_Preferences.Create
        (Name    => "LSP-Ada-Diagnostics",
         Default => True,
         Label   => "Enable diagnostics",
         Doc     => "Enable live diagnostics when editing Ada code "
         & "(e.g: syntax errors).",
         Path    => "Editor/Ada:Diagnostics");

      LSP_Ada_Project_Diagnostics := Kernel.Get_Preferences.Create
        (Name    => "LSP-Ada-Project-Diagnostics",
         Default => True,
         Label   => "Enable Project diagnostics",
         Doc     => "Enable project related diagnostics in Ada code. "
         & """Enable diagnotics"" must be enabled for this preference to "
         & "apply and the Server must be restarted",
         Path    => "Editor/Ada:Diagnostics");

      LSP_Ada_Param_Threshold := Kernel.Get_Preferences.Create
        (Name    => "LSP-Ada-Param-Naming-Threshold",
         Label   => -"Ada named parameters threshold",
         Doc     => -"Threshold before using the named parameters notation."
         & " Use 0 to never use the named parameters notation."
         & "(Server must be restarted for this preference to apply)",
         Minimum => 0,
         Maximum => Integer'Last,
         Default => 3,
         Path    => "Editor/Ada:Completion");

      Use_LSP_In_Highlight := Kernel.Get_Preferences.Create
        (Name     => "use-lsp-in-highlight",
         Label    => -"LSP Semantic Highlighting",
         Doc      => -"Enable LSP semantic highlighting in opened editors.",
         Default  => False,
         Path     => "LSP:Highlight");

      LSP_Diagnostics_Display := LSP_Diagnostics_Display_Policy_Prefs.Create
        (Manager  => Kernel.Get_Preferences,
         Name     => "LSP-Diagnostics-Display",
         Path     => "LSP:Display diagnostics",
         Label    => "Display diagnostics",
         Doc      => "Choose the display policy for diagnostics coming from"
         & " LSP servers. Diagnostics with several locations accross multiple"
         & " files will still appear in the Locations view for proper"
         & " navigation.",
         Default  => Editor_And_Locations);

      Page := Manager.Get_Registered_Page
        (Name             => "Preferences Assistant General",
         Create_If_Needed => False);

      Group := new Preferences_Group_Record;
      Page.Register_Group
        (Name     => "Behavior",
         Group    => Group,
         Priority => 0);

      Group.Add_Pref
        (Manager => Manager,
         Pref    => Preference (Auto_Save));
      Group.Add_Pref
        (Manager => Manager,
         Pref    => Preference (Save_Desktop_On_Exit));
      Group.Add_Pref
        (Manager => Manager,
         Pref    => Preference (Transient_Mark));

      Kernel.Preferences.Set_Is_Loading_Prefs (False);
   end Register_Global_Preferences;

   ---------------
   -- Customize --
   ---------------

   overriding procedure Customize
     (Module : access Preferences_Module;
      File   : GNATCOLL.VFS.Virtual_File;
      Node   : XML_Utils.Node_Ptr;
      Level  : Customization_Level)
   is
      pragma Unreferenced (File, Level);
      Kernel      : constant Kernel_Handle := Get_Kernel (Module.all);
      Child       : XML_Utils.Node_Ptr;
      Child_Count : Natural;
   begin
      if Node.Tag.all = "preference" then
         declare
            Name    : constant String := Get_Attribute_S (Node, "name", "");
            Path    : constant String :=
                        Get_Attribute_S (Node, "page", "General");
            Default : constant String := Get_Attribute_S (Node, "default", "");
            Tooltip : constant String := Get_Attribute_S (Node, "tip", "");
            Label   : constant String := Get_Attribute_S (Node, "label", "");
            Typ     : constant String := Get_Attribute_S (Node, "type", "");
            Min     : constant String :=
              Get_Attribute_S (Node, "minimum", "0");
            Max     : constant String :=
              Get_Attribute_S (Node, "maximum", "10");
            Pref    : Preference;
            pragma Unreferenced (Pref);
            Minimum, Maximum, Def : Integer;
            Bool_Def : Boolean;
         begin
            if Name = "" or else Typ = "" or else Label = "" then
               Insert
                 (Kernel,
                  -("<preference> must have ""name"", ""type"" and"
                    & " ""label"" attributes"),
                  Mode => Error);
               return;
            end if;

            for N in Name'Range loop
               if Name (N) = '_' or else Name (N) = ' ' then
                  Insert
                    (Kernel,
                     -("<preference>: ""name"" attribute mustn't contain"
                       & " '_' or ' ' characters"),
                     Mode => Error);
                  return;
               end if;
            end loop;

            if Typ = "boolean" then
               if Default = "" then
                  Bool_Def := True;
               else
                  Bool_Def := Boolean'Value (Default);
               end if;
               Pref := Preference (Boolean_Preference'(Create
                 (Manager => Kernel.Preferences,
                  Name    => Name,
                  Label   => Label,
                  Path    => Path,
                  Doc     => Tooltip,
                  Default => Bool_Def)));

            elsif Typ = "integer" then
               Minimum := Integer'Value (Min);
               Maximum := Integer'Value (Max);
               if Default = "" then
                  Def := 0;
               else
                  Def     := Integer'Value (Default);
               end if;

               if Minimum > Maximum then
                  Insert
                    (Kernel,
                     -"Minimum value greater than maximum for preference "
                     & Name,
                     Mode => Error);
                  Maximum := Minimum;
               end if;

               if Minimum > Def  then
                  Insert
                    (Kernel,
                     -"Minimum value greater than default for preference "
                     & Name,
                     Mode => Error);
                  Minimum := Def;
               end if;

               if Def > Maximum then
                  Insert
                    (Kernel,
                     -"Default value greater than maximum for preference "
                     & Name,
                     Mode => Error);
                  Maximum := Def;
               end if;

               Pref := Preference (Integer_Preference'(Create
                 (Manager => Kernel.Preferences,
                  Name    => Name,
                  Label   => Label,
                  Doc     => Tooltip,
                  Minimum => Minimum,
                  Maximum => Maximum,
                  Default => Def,
                  Path    => Path)));

            elsif Typ = "string" then
               Pref := Preference (String_Preference'(Create
                 (Manager => Kernel.Preferences,
                  Name    => Name,
                  Label   => Label,
                  Doc     => Tooltip,
                  Default => Default,
                  Path    => Path)));

            elsif Typ = "color" then
               Pref := Preference (Color_Preference'(Create
                 (Manager => Kernel.Preferences,
                  Name    => Name,
                  Label   => Label,
                  Path    => Path,
                  Doc     => Tooltip,
                  Default => Default)));

            elsif Typ = "font" then
               Pref := Preference (Font_Preference'(Create
                 (Manager => Kernel.Preferences,
                  Name    => Name,
                  Label   => Label,
                  Doc     => Tooltip,
                  Default => Default,
                  Path    => Path)));

            elsif Typ = "choices" then
               Child := Node.Child;
               Child_Count := 0;
               while Child /= null loop
                  Child_Count := Child_Count + 1;
                  Child := Child.Next;
               end loop;

               declare
                  Val : constant String_List_Access :=
                    new GNAT.Strings.String_List (1 .. Child_Count);
                  --  Freed when the preference is destroyed
               begin
                  Child := Node.Child;
                  Child_Count := 1;
                  while Child /= null loop
                     Val (Child_Count) := new String'(Child.Value.all);
                     Child_Count := Child_Count + 1;
                     Child := Child.Next;
                  end loop;

                  if Default = "" then
                     Def := 1;
                  else
                     Def := Integer'Value (Default);
                  end if;

                  Pref := Preference (Choice_Preference'(Create
                    (Manager => Kernel.Preferences,
                     Name      => Name,
                     Label     => Label,
                     Path      => Path,
                     Doc       => Tooltip,
                     Choices   => Val,
                     Default   => Def)));
               end;

            else
               Insert
                 (Kernel,
                  -"Invalid ""type"" attribute for <preference>",
                  Mode => Error);
               return;
            end if;

         exception
            when Constraint_Error =>
               Insert
                 (Kernel,
                  -("Invalid attribute value for <preference>, ignoring"
                    & " preference ") & Name,
                  Mode => Error);
         end;
      end if;
   end Customize;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Pref_Class             : constant Class_Type :=
                                 New_Class (Kernel, "Preference");
      Preferences_Page_Class : constant Class_Type :=
                                 New_Class (Kernel, "PreferencesPage");
      Module                 : Module_ID;
   begin
      Module := new Preferences_Module;
      GPS.Kernel.Modules.Register_Module
        (Module      => Module,
         Kernel      => Kernel,
         Module_Name => "Preferences");

      --  Register the commands associated to the Preference class
      Register_Command
        (Kernel, Constructor_Method,
         Minimum_Args => 1,
         Maximum_Args => 1,
         Class        => Pref_Class,
         Handler      => Get_Command_Handler'Access);

      Register_Command
        (Kernel, "get",
         Class   => Pref_Class,
         Handler => Get_Command_Handler'Access);

      Register_Command
        (Kernel.Scripts, "set",
         Params => (1 => Param ("value"),
                    2 => Param ("save", Optional => True)),
         Class        => Pref_Class,
         Handler      => Get_Command_Handler'Access);

      Register_Command
        (Kernel, "create",
         Minimum_Args => 2,
         Maximum_Args => Integer'Last,
         Class        => Pref_Class,
         Handler      => Get_Command_Handler'Access);

      Register_Command
        (Kernel, "create_with_priority",
         Minimum_Args => 3,
         Maximum_Args => Integer'Last,
         Class        => Pref_Class,
         Handler      => Get_Command_Handler'Access);

      Register_Command
        (Kernel.Scripts, "create_style",
         Params       =>
           (1 => Param ("label"),
            2 => Param ("doc", Optional => True),
            3 => Param ("default_font_style", Optional => True),
            4 => Param ("default_bg", Optional => True),
            5 => Param ("default_fg", Optional => True)),
         Class        => Pref_Class,
         Handler      => Get_Command_Handler'Access);

      --  Register the commands associated to the PreferencesPath class
      Register_Command
        (Kernel.Scripts, "create",
         Params        => (1 => Param ("name"),
                           2 => Param ("get_widget"),
                           3 => Param ("priority", Optional => True),
                           4 => Param ("is_integrated", Optional => True)),
         Class         => Preferences_Page_Class,
         Static_Method => True,
         Handler       => Preferences_Page_Commands_Handler'Access);
   end Register_Module;

   ----------------------
   -- Save_Preferences --
   ----------------------

   procedure Save_Preferences (Kernel : access Kernel_Handle_Record'Class) is
      File_Name : constant Virtual_File := Kernel.Preferences_File;
      Success : Boolean;
   begin
      if not Default_Preferences.Is_Frozen (Kernel.Preferences) then
         Trace (Me, "Saving preferences in " & File_Name.Display_Full_Name);
         Save_Preferences (Kernel.Preferences, File_Name, Success);

         if not Success then
            Report_Preference_File_Error (Kernel, File_Name);
         end if;
      end if;
   end Save_Preferences;

   ----------
   -- Thaw --
   ----------

   overriding procedure Thaw
     (Self : not null access GPS_Preferences_Manager_Record) is
   begin
      Thaw (Preferences_Manager_Record (Self.all)'Access);  --  inherited
      if not Self.Is_Frozen then
         Save_Preferences (Self.Kernel);
      end if;
   end Thaw;

   --------------
   -- Set_Pref --
   --------------

   procedure Set_Pref
     (Pref   : Boolean_Preference;
      Kernel : access Kernel_Handle_Record'Class;
      Value  : Boolean) is
   begin
      Set_Pref (Pref, Kernel.Preferences, Value);
   end Set_Pref;

   --------------
   -- Set_Pref --
   --------------

   procedure Set_Pref
     (Pref   : Integer_Preference;
      Kernel : access Kernel_Handle_Record'Class;
      Value  : Integer) is
   begin
      Set_Pref (Pref, Kernel.Preferences, Value);
   end Set_Pref;

   --------------
   -- Set_Pref --
   --------------

   procedure Set_Pref
     (Pref   : Preference;
      Kernel : access Kernel_Handle_Record'Class;
      Value  : String) is
   begin
      Set_Pref (Pref, Kernel.Preferences, Value);
   end Set_Pref;

   -------------------------
   -- Set_Font_And_Colors --
   -------------------------

   procedure Set_Font_And_Colors
     (Widget     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Fixed_Font : Boolean;
      Pref       : Default_Preferences.Preference := null)
   is
   begin
      if Pref = null
        or else Pref = Preference (Default_Font)
        or else (Fixed_Font and then Pref = Preference (View_Fixed_Font))
      then
         if Fixed_Font then
            Modify_Font (Widget, View_Fixed_Font.Get_Pref);
         else
            Modify_Font (Widget, Default_Font.Get_Pref);
         end if;
      end if;
   end Set_Font_And_Colors;

   type Check_Menu_Item_Pref_Record is new Gtk_Check_Menu_Item_Record with
      record
         Kernel : access Kernel_Handle_Record'Class;
         Pref   : Boolean_Preference;
      end record;
   type Check_Menu_Item_Pref is access all Check_Menu_Item_Pref_Record'Class;
   procedure On_Check_Menu_Item_Changed
     (Check : access Gtk_Check_Menu_Item_Record'Class);

   type Pref_Changed_For_Menu_Item is new Preferences_Hooks_Function
   with record
      Check : Check_Menu_Item_Pref;
   end record;
   overriding procedure Execute
     (Self   : Pref_Changed_For_Menu_Item;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference);

   --------------------------------
   -- On_Check_Menu_Item_Changed --
   --------------------------------

   procedure On_Check_Menu_Item_Changed
     (Check : access Gtk_Check_Menu_Item_Record'Class)
   is
      C : constant Check_Menu_Item_Pref := Check_Menu_Item_Pref (Check);
   begin
      if C.Pref.Get_Pref /= C.Get_Active then
         Set_Pref (C.Pref, C.Kernel.Get_Preferences, C.Get_Active);
      end if;
   end On_Check_Menu_Item_Changed;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : Pref_Changed_For_Menu_Item;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference)
   is
      pragma Unreferenced (Kernel);
      V : Boolean;
   begin
      if Pref = Preference (Self.Check.Pref) then
         V := Self.Check.Pref.Get_Pref;
         if V /= Self.Check.Get_Active then
            Self.Check.Set_Active (V);
         end if;
      end if;
   end Execute;

   -----------------
   -- Append_Menu --
   -----------------

   procedure Append_Menu
     (Menu    : not null access Gtk_Menu_Record'Class;
      Kernel  : not null access Kernel_Handle_Record'Class;
      Pref    : Boolean_Preference)
   is
      C   : constant Check_Menu_Item_Pref := new Check_Menu_Item_Pref_Record;
      Doc : constant String := Pref.Get_Doc;

   begin
      Gtk.Check_Menu_Item.Initialize (C, Pref.Get_Label);
      C.Kernel := Kernel;
      C.Pref := Pref;
      Menu.Add (C);

      if Doc /= "" and then not Active (Cygwin_Window_Manager) then
         C.Set_Tooltip_Text (Doc);
      end if;

      C.Set_Active (Pref.Get_Pref);
      C.On_Toggled (On_Check_Menu_Item_Changed'Access);

      Preferences_Changed_Hook.Add
        (Obj   =>
            new Pref_Changed_For_Menu_Item'(Hook_Function with Check => C),
         Watch => C);
   end Append_Menu;

   --------------------------
   -- Color_Menu_Item_Pref --
   --------------------------

   type Color_Menu_Item_Pref_Record is new Gtk_Menu_Item_Record with
      record
         Kernel : access Kernel_Handle_Record'Class;
         Pref   : Color_Preference;
      end record;
   type Color_Menu_Item_Pref is access all Color_Menu_Item_Pref_Record'Class;

   procedure On_Color_Menu_Item_Activated
     (Item : access Gtk_Menu_Item_Record'Class);

   -------------------------
   -- Enum_Menu_Item_Pref --
   -------------------------

   type Enum_Menu_Item_Pref_Record is new Gtk_Radio_Menu_Item_Record with
      record
         Kernel : access Kernel_Handle_Record'Class;
         Pref   : Enum_Preference;
         Value  : Unbounded_String;
      end record;
   type Enum_Menu_Item_Pref is access all Enum_Menu_Item_Pref_Record'Class;

   procedure On_Enum_Menu_Item_Activated
     (Item : access Gtk_Menu_Item_Record'Class);
   --  Called when a enum menu item is activated. Used to change the
   --  associated enum preference value.

   ---------------------------------
   -- On_Enum_Menu_Item_Activated --
   ---------------------------------

   procedure On_Enum_Menu_Item_Activated
     (Item : access Gtk_Menu_Item_Record'Class)
   is
      Menu_Item : constant Enum_Menu_Item_Pref := Enum_Menu_Item_Pref (Item);
   begin
      Set_Pref
        (Menu_Item.Pref,
         Menu_Item.Kernel.Get_Preferences,
         To_String (Menu_Item.Value));
   end On_Enum_Menu_Item_Activated;

   -----------------
   -- Append_Menu --
   -----------------

   procedure Append_Menu
     (Menu    : not null access Gtk_Menu_Record'Class;
      Kernel  : not null access Kernel_Handle_Record'Class;
      Pref    : Color_Preference)
   is
      C   : constant Color_Menu_Item_Pref := new Color_Menu_Item_Pref_Record;
      Doc : constant String := Pref.Get_Doc;
   begin
      Initialize_With_Label (C, Pref.Get_Label);
      C.Kernel := Kernel;
      C.Pref   := Pref;

      Menu.Add (C);

      if Doc /= "" then
         C.Set_Tooltip_Text (Doc);
      end if;

      C.On_Activate (On_Color_Menu_Item_Activated'Access);
   end Append_Menu;

   -----------------
   -- Append_Menu --
   -----------------

   procedure Append_Enum_To_Menu
     (Menu    : not null access Gtk_Menu_Record'Class;
      Kernel  : not null access Kernel_Handle_Record'Class;
      Pref    : Enum_Preference)
   is
      Menu_Item : Enum_Menu_Item_Pref;
      Group     : Widget_SList.GSlist := Widget_SList.Null_List;
   begin
      for J in Enumeration loop
         declare
            Value : constant String := Enumeration'Image (J);
         begin
            Menu_Item := new Enum_Menu_Item_Pref_Record;
            Gtk.Radio_Menu_Item.Initialize
              (Radio_Menu_Item => Menu_Item,
               Group           => Group,
               Label           => Default_Preferences.Enums.Enum_Value_To_Label
                 (Value));
            Group := Menu_Item.Get_Group;
            Menu_Item.Kernel := Kernel;
            Menu_Item.Pref := Pref;
            Menu_Item.Value := To_Unbounded_String (Value);
            Menu.Add (Menu_Item);
            Menu_Item.On_Activate (On_Enum_Menu_Item_Activated'Access);
         end;
      end loop;
   end Append_Enum_To_Menu;

   ----------------------------------
   -- On_Color_Menu_Item_Activated --
   ----------------------------------

   procedure On_Color_Menu_Item_Activated
     (Item : access Gtk_Menu_Item_Record'Class)
   is
      It     : constant Color_Menu_Item_Pref := Color_Menu_Item_Pref (Item);
      Dialog : Gtk_Color_Selection_Dialog;
      Color  : Gdk_RGBA;
   begin
      Dialog := Gtk_Color_Selection_Dialog_New (It.Pref.Get_Label);
      Dialog.Get_Color_Selection.Set_Current_Rgba (It.Pref.Get_Pref);

      if Dialog.Run = Gtk_Response_OK then
         Dialog.Get_Color_Selection.Get_Current_Rgba (Color);
         if Color /= It.Pref.Get_Pref then
            Set_Pref (It.Pref, It.Kernel.Get_Preferences, To_String (Color));
         end if;
      end if;

      Dialog.Destroy;
   end On_Color_Menu_Item_Activated;

   -------------------------
   -- Get_Line_Terminator --
   -------------------------

   function Get_Line_Terminator return String is
   begin
      if Line_Terminator.Get_Pref = Windows then
         return Ada.Characters.Latin_1.CR & Ada.Characters.Latin_1.LF;
      else
         return "" & Ada.Characters.Latin_1.LF;
      end if;
   end Get_Line_Terminator;

end GPS.Kernel.Preferences;
