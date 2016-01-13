------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2016, AdaCore                     --
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

with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Characters.Handling;   use Ada.Characters.Handling;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNATCOLL.Python;           use GNATCOLL.Python;
with GNATCOLL.Scripts;          use GNATCOLL.Scripts;
with GNATCOLL.Scripts.Python;   use GNATCOLL.Scripts.Python;
with GNATCOLL.Traces;           use GNATCOLL.Traces;
with GNAT.Strings;              use GNAT.Strings;

with XML_Utils;                 use XML_Utils;

with Pango.Font;                use Pango.Font;
with Glib.Object;               use Glib.Object;
with Gtk.Check_Menu_Item;       use Gtk.Check_Menu_Item;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;
with Gtk.Widget;                use Gtk.Widget;

with Config;
with Defaults;
with Default_Preferences.Enums; use Default_Preferences.Enums;
with Default_Preferences.GUI;   use Default_Preferences.GUI;
with GPS.Customizable_Modules;  use GPS.Customizable_Modules;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Charsets;       use GPS.Kernel.Charsets;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with Language;                  use Language;

package body GPS.Kernel.Preferences is
   Me : constant Trace_Handle := Create ("GPS_KERNEL");

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
   --  Handle GPS customization files for this module

   type Python_Preferences_Page_Record is new Default_Preferences_Page_Record
     with
      record
         Get_Python_Widget  : Subprogram_Type;
         --  Subprogram returning a newly created PyGtk widget for this page.
      end record;
   type Python_Preferences_Page is
     access all Python_Preferences_Page_Record'Class;
   --  Type used to represent preferences pages that have been registered
   --  in python plugins.

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

      Script        : constant Scripting_Language  :=
                        Get_Script (Self.Get_Python_Widget.all);
      Args          : constant Callback_Data'Class := Create (Script, 0);
      Page_View     : constant Preferences_Page_View :=
                        new Preferences_Page_View_Record;
      Stub          : GObject_Record;
      Python_Widget : GObject;
      pragma Unreferenced (Manager);
   begin
      Default_Preferences.GUI.Initialize (Page_View);

      --  Retrieve the PyGtkWidget created on the python side and add it to
      --  the page view.
      Python_Widget := Get_User_Data
        (Obj  => Widget_From_PyObject
           (Get_PyObject
                (Execute (Self.Get_Python_Widget, Args))),
         Stub => Stub);
      Page_View.Add (Gtk_Widget (Python_Widget));

      return Gtk_Widget (Page_View);
   end Get_Widget;

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
         Font := Copy (Default_Font.Get_Pref_Font);
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
      --  Don't register the preferences page if the given name is an empty
      --  string.
      if Name = "" then
         return;
      end if;

      declare
         Page : constant Python_Preferences_Page :=
                  new Python_Preferences_Page_Record'
                    (Default_Preferences_Page_Record with
                     Get_Python_Widget => Nth_Arg (Data, 2));
         Priority   : constant Integer := Nth_Arg (Data, 3, Default => -1);
      begin
         --  Register the page in the manager
         Kernel.Get_Preferences.Register_Page
           (Name             => Name,
            Page             => Preferences_Page (Page),
            Priority         => Priority,
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
            Default_Bg         : constant String := Nth_Arg (Data, 5, "white");
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
            end if;

            Pref := Preference
              (Create (Manager => Kernel.Preferences,
                       Name => Path,
                       Label => Label,
                       Page => Dir_Name (Path),
                       Doc => Doc,
                       Default_Bg => Default_Bg,
                       Default_Fg => Default_Fg,
                       Default_Variant => Default_Variant,
                       Base => Default_Style));
         end;

      elsif Command = "create" then
         declare
            Path  : constant String := Get_Data (Inst, Class);
            Label : constant String := Nth_Arg (Data, 2);
            Typ   : constant String := Nth_Arg (Data, 3);
            Doc   : constant String := Nth_Arg (Data, 4, "");
            Pref  : Preference;
            pragma Unreferenced (Pref);
         begin
            if Typ = "integer" then
               Pref := Preference (Create
                 (Manager => Kernel.Preferences,
                  Name    => Path,
                  Label   => Label,
                  Doc     => Doc,
                  Page    => Dir_Name (Path),
                  Default => Nth_Arg (Data, 5, 0),
                  Minimum => Nth_Arg (Data, 6, Integer'First),
                  Maximum => Nth_Arg (Data, 7, Integer'Last)));

            elsif Typ = "boolean" then
               Pref := Preference (Boolean_Preference'(Create
                 (Manager => Kernel.Preferences,
                  Name    => Path,
                  Label   => Label,
                  Doc     => Doc,
                  Page    => Dir_Name (Path),
                  Default => Nth_Arg (Data, 5, True))));

            elsif Typ = "string" then
               Pref := Preference (String_Preference'(Create
                 (Manager => Kernel.Preferences,
                  Name    => Path,
                  Label   => Label,
                  Page    => Dir_Name (Path),
                  Doc     => Doc,
                  Default => Nth_Arg (Data, 5, ""))));

            elsif Typ = "multiline" then
               Pref := Preference (String_Preference'(Create
                 (Manager => Kernel.Preferences,
                  Name    => Path,
                  Label   => Label,
                  Page    => Dir_Name (Path),
                  Doc     => Doc,
                  Multi_Line => True,
                  Default => Nth_Arg (Data, 5, ""))));

            elsif Typ = "color" then
               Pref := Preference (Color_Preference'(Create
                 (Manager => Kernel.Preferences,
                  Name    => Path,
                  Label   => Label,
                  Doc     => Doc,
                  Page    => Dir_Name (Path),
                  Default => Nth_Arg (Data, 5, "black"))));

            elsif Typ = "font" then
               Pref := Preference (Font_Preference'(Create
                 (Manager => Kernel.Preferences,
                  Name    => Path,
                  Label   => Label,
                  Page    => Dir_Name (Path),
                  Doc     => Doc,
                  Default => Nth_Arg (Data, 5, Defaults.Default_Font))));

            elsif Typ = "enum" then
               declare
                  Val : constant String_List_Access :=
                    new GNAT.Strings.String_List
                      (1 .. Number_Of_Arguments (Data) - 5);
                  --  Freed when the preference is destroyed
               begin
                  for V in Val'Range loop
                     Val (V) := new String'(Nth_Arg (Data, 5 + V));
                  end loop;

                  Pref := Preference (Choice_Preference'(Create
                    (Manager => Kernel.Preferences,
                     Name      => Path,
                     Label     => Label,
                     Page      => Dir_Name (Path),
                     Doc       => Doc,
                     Choices   => Val,
                     Default   => Nth_Arg (Data, 5))));
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
     (Kernel : access Kernel_Handle_Record'Class) is
   begin
      Kernel.Preferences.Set_Is_Loading_Prefs (True);

      -- General --
      Gtk_Theme := Create
        (Kernel.Preferences,
         Name  => "GPS6-Gtk-Theme-Name",  --  synchronize with colorschemes.py
         Label => -"Theme",
         Page  => -"General:Appearance",
         Doc   => -("Select a theme from the list to change the general "
                     & "appearance of GPS"));

      Default_Font := Create
        (Manager => Kernel.Preferences,
         Name    => "General-Default-Style",
         Default_Font => Defaults.Default_Font,
         Default_Fg   => "black",
         Default_Bg   => "white",
         Doc     => -("The default style used in GPS. The color indicates the"
           & " what should be used for the background color of windows (for"
           & " editors check the Editor/Colors preference page)."),
         Page    => -"General:Appearance",
         Label   => -"Default font");

      Small_Font := Create
        (Manager => Kernel.Preferences,
         Name    => "General-Small-Font",
         Default => Defaults.Default_Font,
         Doc     => -("The font used by GPS to display less important"
           & " information"),
         Page    => -"",
         Label   => -"Small font");

      View_Fixed_Font := Create
        (Manager => Kernel.Preferences,
         Name    => "General-Fixed-Font",
         Default => Defaults.Default_Fixed_Font,
         Doc     => -("Fixed pitch (monospace) font used in the various views "
                      & "(Outline View, Clipboard View, Messages, ...)"),
         Label   => -"Fixed view font",
         Page    => -"General:Appearance");

      Use_Native_Dialogs := Create
        (Manager => Kernel.Preferences,
         Name    => "General-Use-Native-Dialogs",
         Label   => -"Native dialogs",
         Doc     =>
         -"Use OS native dialogs if enabled, portable dialogs otherwise",
         Default => True,
         Page    => "");

      Splash_Screen := Create
        (Manager  => Kernel.Preferences,
         Name     => "General-Splash-Screen",
         Label    => -"Display splash screen",
         Doc      =>
         -"Whether a splash screen should be displayed when starting GPS",
         Default => True,
         Page    => -"General:Behavior");

      Display_Welcome := Create
        (Manager => Kernel.Preferences,
         Name    => "General-Display-Welcome",
         Label   => -"Display welcome window",
         Doc     => -("Enabled when GPS should display the welcome window"
                      & " for the selection of the project"),
         Default => True,
         Page    => -"General:Behavior");

      Auto_Save := Create
        (Manager => Kernel.Preferences,
         Name    => "General-Auto-Save",
         Label   => -"Auto save",
         Doc     => -("Whether unsaved files/projects should be saved"
                      & " automatically before calling external tools"),
         Default => True,
         Page    => -"General:Behavior");

      Save_Desktop_On_Exit := Create
        (Manager => Kernel.Preferences,
         Name    => "General-Save-Desktop-On-Exit",
         Label   => -"Save desktop on exit",
         Doc     => -"Whether the desktop should be saved when exiting GPS",
         Default => True,
         Page    => -"General:Behavior");

      Hyper_Mode := Create
        (Manager => Kernel.Preferences,
         Name    => "Hyper-Mode",
         Default => True,
         Doc     =>
         -("Whether to allow hyper links to appear in editors when the"
          & " Control key is pressed."),
         Label   => -"Hyper links",
         Page    => -"General:Behavior");

      Tip_Of_The_Day := Create
        (Manager => Kernel.Preferences,
         Name    => "General/Display-Tip-Of-The-Day",
         Default => True,
         Doc     => -("Whether GPS should display the Tip of the Day dialog"),
         Label   => -"Tip of the Day",
         Page    => -"General:Behavior");

      Multi_Language_Builder := Multi_Language_Builder_Policy_Prefs.Create
        (Manager => Kernel.Preferences,
         Name    => "General-Default-Builder",
         Label   => -"Default builder",
         Doc     =>
         -("GPS default builder choice:" & ASCII.LF &
           "  - gprbuild" & ASCII.LF &
           "  - gnatmake (not recommended, not supported for "
           & "multi-language builds)"),
         Page    => -"General:Behavior",
         Default => Default_Builder);

      Save_Editor_Desktop := Editor_Desktop_Policy_Prefs.Create
        (Manager => Kernel.Preferences,
         Name    => "General-Editor-Desktop-Policy",
         Label   => "Save editor in desktop",
         Doc     => -"When to save source editors in the desktop",
         Page    => -"General:Behavior",
         Default => From_Project);

      GPS.Kernel.Charsets.Register_Preferences (Kernel);

      -- Source Editor --

      Display_Subprogram_Names := Create
        (Manager => Kernel.Preferences,
         Name    => "Src-Editor-Display-Subprogram_Names",
         Default => True,
         Doc     =>
           -"Whether the subprogram names should be displayed in status lines",
         Label   => -"Display subprogram names",
         Page    => -"Editor:Display");

      Display_Tooltip := Create
        (Manager => Kernel.Preferences,
         Name    => "Src-Editor-Display-Tooltip",
         Default => True,
         Doc     => -"Whether tooltips should be displayed automatically",
         Label   => -"Tooltips",
         Page    => -"Editor:Display");

      Current_Line_Thin := Create
        (Manager => Kernel.Preferences,
         Name    => "Src-Editor-Current-Line-Thin",
         Default => False,
         Doc     => -("Whether to use a thin line rather than full background"
           & ASCII.LF & " highlighting on the current line."),
         Label   => -"Draw current line as a thin line",
         Page    => -"Editor:Display");

      Alter_Bg_For_RO_Files := Create
        (Manager => Kernel.Preferences,
         Name    => "Alter-Bg-For-RO-Files",
         Label   => -"Change background of read-only editors",
         Doc     => -("When active, change the background color"
           & " of read-only editors."),
         Default => True,
         Page    => -"Editor:Display");

      Display_Line_Numbers := Line_Number_Policy_Prefs.Create
        (Manager => Kernel.Preferences,
         Name    => "GPS6-Src-Editor-Display-Line_Numbers",
         Default => All_Lines,
         Doc     =>
           -"Whether the line numbers should be displayed in file editors",
         Label   => -"Display line numbers",
         Page    => -"Editor:Display");

      Highlight_Column := Create
        (Manager => Kernel.Preferences,
         Name    => "Src-Editor-Highlight-Column",
         Minimum => 0,
         Maximum => 255,
         Default => 80,
         Doc     => -("The right margin to highlight. 0 if none. This value "
                      & "is also used to implement the Edit->Refill command"),
         Label   => -"Right margin",
         Page    => -"Editor:Display");

      Highlight_Delimiters := Create
        (Manager => Kernel.Preferences,
         Name    => "Src-Editor-Highlight-Delimiters",
         Default => True,
         Doc     => -"Whether delimiters should be highlighted: (){}[]",
         Label   => -"Highlight delimiters",
         Page    => -"Editor:Highlighting");

      Block_Highlighting := Create
        (Manager => Kernel.Preferences,
         Name    => "Src-Editor-Block-Highlighting",
         Default => True,
         Doc     =>
           -"Should the editor enable block highlighting",
         Label   => -"Block highlighting",
         Page    => -"Editor:Highlighting");

      Strip_Blanks := Strip_Trailing_Blanks_Policy_Prefs.Create
        (Manager => Kernel.Preferences,
         Name    => "Src-Editor-Strip-Trailing-Blanks",
         Label   => -"Strip blanks",
         Doc     =>
           -"Should the editor remove trailing blanks when saving files",
         Default => Autodetect,
         Page    => -"Editor:On Save");

      Strip_Lines := Strip_Trailing_Blanks_Policy_Prefs.Create
        (Manager => Kernel.Preferences,
         Name    => "Src-Editor-Strip-Trailing-Lines",
         Label   => -"Strip lines",
         Doc     =>
           -"Should the editor remove trailing blank lines when saving files",
         Default => Autodetect,
         Page    => -"Editor:On Save");

      Line_Terminator := Line_Terminators_Prefs.Create
        (Manager => Kernel.Preferences,
         Name  => "Src-Editor-Line-Terminator",
         Label => -"Line terminator",
         Doc   => -"Line terminator style to use when saving files",
         Default => Unchanged,
         Page    => -"Editor:On Save");

      Auto_Indent_On_Paste := Create
        (Manager => Kernel.Preferences,
         Name    => "Src-Editor-Indent-On-Paste",
         Default => False,
         Doc     =>
           -"Whether content pasted in the source editors "
           & "should be auto indented",
         Label   => -"Auto indent on paste",
         Page    => -"Editor:Behavior");

      Block_Folding := Create
        (Manager => Kernel.Preferences,
         Name    => "Src-Editor-Block-Folding",
         Default => True,
         Doc     => -"Should the editor enable block folding",
         Label   => -"Block folding",
         Page    => -"Editor:Behavior");

      Periodic_Save := Create
        (Manager  => Kernel.Preferences,
         Name     => "Src-Editor-Periodic-Save",
         Minimum  => 0,
         Maximum  => 3600,
         Default  => 60,
         Doc      => -("The period (in seconds) after which a source editor"
           & " is automatically saved. 0 if none."),
         Label    => -"Autosave delay",
         Page     => -"Editor:Behavior",
         Priority => -2);

      Automatic_Syntax_Check := Create
        (Manager => Kernel.Preferences,
         Name    => "Src-Editor-Automatic-Syntax-Check",
         Default => False,
         Doc     => -"Enable/Disable automatic syntax check",
         Label   => -"Automatic syntax check",
         Page    => "");

      if Config.Host = Config.Windows then
         Use_ACL := Create
           (Manager => Kernel.Preferences,
            Name    => "Src-Editor-Use-ACL",
            Label   => -"Use Windows ACL",
            Doc     =>
            -"Whether GPS should use ACL when changing the "
            & "read/write permissions",
            Default => False,
            Page    => -"Editor:Behavior");
      end if;

      Default_Style := Create
        (Manager      => Kernel.Preferences,
         Name         => "Src-Editor-Reference-Style",
         Label        => -"Default",
         Doc          => -("Default style used in the source editors."
           & " The background color defined here also defines the background"
           & " color of all editors."),
         Default_Font => Defaults.Default_Fixed_Font,
         Default_Fg   => "black",
         Default_Bg   => "white",
         Page         => -"Editor/Fonts & Colors:General");

      Blocks_Style := Create
        (Manager      => Kernel.Preferences,
         Name         => "Src-Editor-Block-Variant",
         Label        => -"Blocks",
         Doc          => -("Style to use when displaying blocks (subprograms,"
           & "tasks, entries, ...) in declarations."),
         Base            => Default_Style,
         Default_Variant => Default,
         Default_Fg      => "#60615F",
         Default_Bg      => "white",
         Page            => -"Editor/Fonts & Colors:General");

      Types_Style := Create
        (Manager      => Kernel.Preferences,
         Name         => "Src-Editor-Type-Variant",
         Label        => -"Types",
         Doc          => -("Style to use when displaying types in "
           & "declarations."),
         Base            => Default_Style,
         Default_Variant => Default,
         Default_Fg   => "#009900",
         Default_Bg   => "white",
         Page         => -"Editor/Fonts & Colors:General");

      Keywords_Style := Create
        (Manager      => Kernel.Preferences,
         Name         => "Src-Editor-Keywords-Variant",
         Label        => -"Keywords",
         Doc          => -("Style to use when displaying keywords."
           & " The background color will be that of the default if left"
           & " to white"),
         Base            => Default_Style,
         Default_Variant => Default,
         Default_Fg      => "#0000E6",
         Default_Bg      => "white",
         Page            => -"Editor/Fonts & Colors:General");

      Comments_Style := Create
        (Manager      => Kernel.Preferences,
         Name         => "Src-Editor-Comments-Variant",
         Label        => -"Comments",
         Doc          => -"Style to use when displaying comments."
           & " The background color will be that of the default if left"
           & " to white",
         Base            => Default_Style,
         Default_Variant => Default,
         Default_Fg   => "#969696",
         Default_Bg   => "white",
         Page         => -"Editor/Fonts & Colors:General");

      Strings_Style := Create
        (Manager      => Kernel.Preferences,
         Name         => "Src-Editor-Strings-Variant",
         Label        => -"Strings",
         Doc          => -"Style to use when displaying strings."
           & " The background color will be that of the default if left"
           & " to white",
         Base            => Default_Style,
         Default_Variant => Default,
         Default_Fg   => "#CE7B00",
         Default_Bg   => "white",
         Page         => -"Editor/Fonts & Colors:General");

      Numbers_Style := Create
        (Manager      => Kernel.Preferences,
         Name         => "Src-Editor-Numbers-Variant",
         Label        => -"Numbers",
         Doc          => -"Style to use when displaying numbers."
           & " The background color will be that of the default if left"
           & " to white",
         Base            => Default_Style,
         Default_Variant => Default,
         Default_Fg   => "#FF3333",
         Default_Bg   => "white",
         Page         => -"Editor/Fonts & Colors:General");

      Hyper_Links_Style := Create
        (Manager      => Kernel.Preferences,
         Name         => "Src-Editor-Hyper-Links-Variant",
         Label        => -"Hyper links",
         Doc          => -"Style to use when displaying hyper-links."
           & " The background color will be that of the default if left"
           & " to white",
         Base            => Default_Style,
         Default_Variant => Default,
         Default_Fg   => "blue",
         Default_Bg   => "white",
         Page         => -"Editor/Fonts & Colors:General");

      Ephemeral_Highlighting_Smart := Create
        (Manager         => Kernel.Preferences,
         Name            => "Src-Editor-Ephemeral-Smart",
         Label           => -"Ephemeral highlighting (smart)",
         Doc             => -(
           "Style used for ephemeral highlighting of context-sensitive"
           & " information, such as highlighting of matching entities."),
         Base            => Default_Style,
         Default_Variant => Default,
         Default_Fg      => "rgba(0,0,0,0.0)",
         Default_Bg      => "rgba(252,172,79,0.4)",
         Page            => -"Editor/Fonts & Colors:General");

      Ephemeral_Highlighting_Simple := Create
        (Manager         => Kernel.Preferences,
         Name            => "Src-Editor-Ephemeral-Simple",
         Label           => -"Ephemeral highlighting (simple)",
         Doc             => -(
           "Style used for ephemeral highlighting in the editor for simple"
           & " cases, such as highlighting text-based matches."),
         Base            => Default_Style,
         Default_Variant => Default,
         Default_Fg      => "rgba(0,0,0,0.0)",
         Default_Bg      => "rgba(134,134,134,0.35)",
         Page            => -"Editor/Fonts & Colors:General");

      Current_Block_Color := Create
        (Manager  => Kernel.Preferences,
         Name     => "Src-Editor-Current-Block-Color",
         Default  => "#9C9CFF",
         Doc      => -"Color for highlighting the current block",
         Label    => -"Current block color",
         Page     => -"Editor/Fonts & Colors:General",
         Priority => -2);

      Current_Line_Color := Create
        (Manager  => Kernel.Preferences,
         Name     => "Src-Editor-Current-Line-Color",
         Default  => "rgba(226,226,226,0.4)",
         Doc      => -("Color for highlighting the current line. White means"
           & " transparent"),
         Label    => -"Current line color",
         Page     => -"Editor/Fonts & Colors:General",
         Priority => -2);

      Annotated_Comments_Style := Create
        (Manager      => Kernel.Preferences,
         Name         => "Src-Editor-Annotated-Comments-Variant",
         Label        => -"SPARK Annotations",
         Doc          => -"Style to use when displaying SPARK annotations "
         & "within Ada comments (starting with --#)."
         & " The background color will be that of the default if left"
         & " to white",
         Base            => Default_Style,
         Default_Variant => Default,
         Default_Fg   => "#60615F",
         Default_Bg   => "white",
         Page         => -"Editor/Fonts & Colors:Ada");

      Aspects_Style := Create
        (Manager      => Kernel.Preferences,
         Name         => "Src-Editor-Aspects-Variant",
         Label        => -"Ada/SPARK aspects",
         Doc          => -"Style to use when displaying Ada 2012 or SPARK 2014"
         & " aspects. The background color will be that of the default if left"
         & " to white",
         Base            => Default_Style,
         Default_Variant => Default,
         Default_Fg   => "#60615F",
         Default_Bg   => "white",
         Page         => -"Editor/Fonts & Colors:Ada");

      -- Refactoring --

      Add_Subprogram_Box := Create
        (Manager => Get_Preferences (Kernel),
         Name    => "Refactoring-Subprogram-Box",
         Default => True,
         Doc     => -(
           "This preference forces GPS to add a comment before bodies when it"
           & " creates new subprograms. This comment is a three line comment"
           & " box, containing the name of the subprogram, as in" & ASCII.LF
           & "----------------" & ASCII.LF
           & "-- Subprogram --" & ASCII.LF
           & "----------------" & ASCII.LF),
         Label   => -"Subprogram Box",
         Page    => -"Refactoring:Subprograms");

      Add_In_Keyword := Create
        (Manager => Get_Preferences (Kernel),
         Name    => "Refactoring-In-Keyword",
         Default => False,
         Doc     => -(
           "Whether the keyword ""in"" should be added when creating new"
           & " subprograms, as in" & ASCII.LF
           & "    procedure Proc (A : in Integer);" & ASCII.LF
           & " as opposed to" & ASCII.LF
           & "    procedure Proc (A : Integer);"),
         Label   => -"Add ""in"" Keyword",
         Page    => -"Refactoring:Subprograms");

      Create_Subprogram_Decl  := Create
        (Manager => Get_Preferences (Kernel),
         Name    => "Refactoring-Subprogram-Spec",
         Default => True,
         Doc     => -(
          "Whether GPS should create a declaration when extracting a"
           & " subprogram from existing code. If"
           & " set to False, only the body of the subprogram will be created"),
         Label   => -"Create Subprogram Declarations",
         Page    => -"Refactoring:Subprograms");

      -- Browsers --

      Browsers_Bg_Color := Create
        (Manager => Kernel.Preferences,
         Name    => "Browsers-Bg-Color",
         Default => "#FFFFFF",
         Doc     => -"Color used to draw the background of the browsers",
         Label   => -"Background color",
         Page    => -"Browsers:Colors");

      Browsers_Hyper_Link_Color := Create
        (Manager => Kernel.Preferences,
         Name    => "Browsers-Hyper-Link-Color",
         Default => "#0000FF",
         Doc     => -"Color used to draw the hyper links in the items",
         Label   => -"Hyper link color",
         Page    => -"Browsers:Colors");

      Selected_Link_Color := Create
        (Manager => Kernel.Preferences,
         Name    => "browsers-link-to-selected-color",
         Default => "rgba(230,50,50,0.7)",
         Doc     => -"Color to use for links between selected items",
         Label   => -"Selected link color",
         Page    => -"Browsers:Colors");

      Unselected_Link_Color := Create
        (Manager => Kernel.Preferences,
         Name    => "browsers-link-color",
         Default => "rgba(180,180,180,0.7)",
         Doc     => -"Color to use for links between unselected items",
         Label   => -"Default link color",
         Page    => -"Browsers:Colors");

      Parent_Linked_Item_Color := Create
        (Manager => Kernel.Preferences,
         Name    => "browsers-linked-item-outline",
         Default => "rgba(0,168,180,0.4)",
         Doc     => -("Color to use for the background of the items linked"
                      & " to the selected item"),
         Label   => -"Ancestor items color",
         Page    => -"Browsers:Colors");

      Child_Linked_Item_Color := Create
        (Manager => Kernel.Preferences,
         Name    => "Browsers-Child-Linked-Item-Color",
         Default => "#DDDDDD",
         Doc     => -("Color to use for the background of the items linked"
                      & " from the selected item"),
         Label   => -"Offspring items color",
         Page    => -"Browsers:Colors");

      Selected_Item_Color := Create
        (Manager => Kernel.Preferences,
         Name    => "browsers-selected-item-outline",
         Default => "rgba(138,226,52,0.7)",
         Doc     => -"Color to use to draw the selected item",
         Label   => -"Selected item color",
         Page    => -"Browsers:Colors");

      Title_Color := Create
        (Manager => Kernel.Preferences,
         Name     => "Browsers-Title-Color",
         Label    => -"Title background",
         Doc      => -"Color used for the background of the title",
         Page     => "",
         Default  => "#BEBEBE");

      Browsers_Vertical_Layout := Create
        (Manager => Kernel.Preferences,
         Name    => "Browsers-Vertical-Layout",
         Default => False,
         Doc     => -("If enabled, the boxes in the browsers will be"
           & " organized into layers displayed one below the other. The"
           & " graph will tend to grow vertically when you open new boxes."
           & " This setting does not affect the entities browser, though,"
           & " where the layout is always vertical."),
         Label   => -"Vertical layout",
         Page    => -"Browsers:Display");

      -- VCS --

      Implicit_Status := Create
        (Manager => Kernel.Preferences,
         Name    => "VCS-Implicit-Status",
         Default => True,
         Doc     => -("If disabled, the status command will never be called"
           & " implicitly as part of another VCS action. For example after"
           & " an update the status is requested from the repository. This"
           & " may take some time depending on the network connection speed."),
         Label   => -"Implicit status",
         Page    => -"VCS:General");

      -- Diff_Utils --

      Old_Vdiff := Create
        (Manager   => Kernel.Preferences,
         Name      => "Diff-Utils-Old-Vdiff",
         Label     => -"Use old diff (requires restart)",
         Doc       => -("Use the old version of visual differences."
           & " Changing this parameter requires restarting GPS."),
         Default   => False,
         Page      => -"Visual diff:General",
         Priority  => -2);

      Diff_Mode := Vdiff_Modes_Prefs.Create
        (Manager => Kernel.Preferences,
         Name    => "Diff-Utils-Mode",
         Label   => "Mode",
         Doc     => -("How diffs are represented in GPS:" & ASCII.LF
           & " - Unified: the differences are shown directly in the editor,"
           & ASCII.LF
           & " - Side_By_Side: the differences are shown in a separate editor."
          ),
         Default => Side_By_Side,
         Page    => -"Visual diff:General");

      Diff_Cmd := Create
        (Manager => Kernel.Preferences,
         Name    => "Diff-Utils-Diff",
         Label   => -"Diff command",
         Doc     => -("Command used to compute differences between two files."
                      & " Arguments can also be specified"),
         Default => Config.Default_Diff_Cmd,
         Page    => -"Visual diff:General");

      Patch_Cmd := Create
        (Manager => Kernel.Preferences,
         Name    => "Diff-Utils-Patch",
         Label   => -"Patch command",
         Doc     =>
           -"Command used to apply a patch. Arguments can also be specified",
         Default => Config.Default_Patch_Cmd,
         Page    => -"Visual diff:General");

      -- Messages --

      Message_Highlight := Create
        (Manager => Kernel.Preferences,
         Name    => "Messages-Highlight-Color",
         Label   => -"Color highlighting",
         Doc     => -"Color used to highlight text in the messages window",
         Default => "#FF0000",
         Page    => -"Messages");

      Error_Src_Highlight := Create
        (Manager => Kernel.Preferences,
         Name    => "Errors-Src-Highlight-Color",
         Label   => -"Errors highlighting",
         Doc     => -"Color used to highlight errors in the source editors",
         Default => "#FFB7B7",
         Page    => -"Messages");

      Warning_Src_Highlight := Create
        (Manager => Kernel.Preferences,
         Name    => "Warnings-Src-Highlight-Color",
         Label   => -"Warnings highlighting",
         Doc     => -"Color used to highlight warnings in the source editors",
         Default => "#FFCC9C",
         Page    => -"Messages");

      Style_Src_Highlight := Create
        (Manager => Kernel.Preferences,
         Name    => "Style-Src-Highlight-Color",
         Label   => -"Style errors highlighting",
         Doc     =>
           -"Color used to highlight style errors in the source editors",
         Default => "#FFFFAD",
         Page    => -"Messages");

      Info_Src_Highlight := Create
        (Manager => Kernel.Preferences,
         Name    => "Info-Src-Highlight-Color",
         Label   => -"Compiler info highlighting",
         Doc     =>
           -"Color used to highlight compiler info in the source editors",
         Default => "#ADFFC2",
         Page    => -"Messages");

      Search_Src_Highlight := Create
        (Manager => Kernel.Preferences,
         Name    => "Search-Src-Highlight-Color",
         Label   => -"Search highlighting",
         Doc     =>
             -"Color used to highlight search results in the source editors",
         Default => "#BDD7FF",
         Page    => -"Messages");

      File_Pattern := Create
        (Manager => Kernel.Preferences,
         Name    => "Messages-File-Regpat-1",
         Label   => -"File pattern",
         Doc     =>
           -"Pattern used to detect file locations (e.g error messages)",
         Default =>
           "^([^:]:?[^:]*):(\d+):((\d+):)? " &
           "(((medium )?warning|medium:)?(info|Note|check)?" &
           "(\(style|low:|low warning:)?.*)",
         Page => -"Messages");

      File_Pattern_Index := Create
        (Manager => Kernel.Preferences,
         Name    => "Messages-File-Regexp-Index",
         Minimum => 1,
         Maximum => 99,
         Default => 1,
         Doc     => -"Index of filename in the pattern",
         Label   => -"File index",
         Page    => -"Messages");

      Line_Pattern_Index := Create
        (Manager => Kernel.Preferences,
         Name    => "Messages-Line-Regexp-Index",
         Minimum => 1,
         Maximum => 99,
         Default => 2,
         Doc     => -"Index of line number in the pattern",
         Label   => -"Line index",
         Page    => -"Messages");

      Column_Pattern_Index := Create
        (Manager => Kernel.Preferences,
         Name    => "Messages-Column-Regexp-Index",
         Minimum => 0,
         Maximum => 99,
         Default => 4,
         Doc     => -"Index of column number in the pattern, 0 if none",
         Label   => -"Column index",
         Page    => -"Messages");

      Message_Pattern_Index := Create
        (Manager => Kernel.Preferences,
         Name    => "Messages-Message-Regexp-Index",
         Minimum => 0,
         Maximum => 99,
         Default => 5,
         Doc     => -"Index of message in the pattern, 0 if none",
         Label   => -"Message index",
         Page    => -"Messages");

      Warning_Pattern_Index := Create
        (Manager => Kernel.Preferences,
         Name    => "Messages-Warning-Regexp-Index",
         Minimum => 0,
         Maximum => 99,
         Default => 6,
         Doc     => -"Index of warning indication in the pattern, 0 if none",
         Label   => -"Warning index",
         Page    => -"Messages");

      Info_Pattern_Index := Create
        (Manager => Kernel.Preferences,
         Name    => "Messages-Info-Regexp-Index",
         Minimum => 0,
         Maximum => 99,
         Default => 8,
         Doc     => -"Index of compiler info in the pattern, 0 if none",
         Label   => -"Info index",
         Page    => -"Messages");

      Style_Pattern_Index := Create
        (Manager => Kernel.Preferences,
         Name    => "Messages-Style-Regexp-Index-1",
         Minimum => 0,
         Maximum => 99,
         Default => 9,
         Doc     => -"Index of style indication in the pattern, 0 if none",
         Label   => -"Style index",
         Page    => -"Messages");

      Secondary_File_Pattern := Create
        (Manager => Kernel.Preferences,
         Name    => "GPS6-Messages-Secondary-File-Regexp",
         Label   => -"Secondary File pattern",
         Doc     =>
           -"Pattern used to detect secondary file locations in messages",
         Default => "(([^:( ]+):(\d+)(:(\d+):?)?)",
         Page    => -"Messages");

      Secondary_File_Pattern_Index := Create
        (Manager => Kernel.Preferences,
         Name    => "GPS6-Messages-Secondary-File-Regexp-Index",
         Minimum => 1,
         Maximum => 99,
         Default => 2,
         Doc     => -"Index of secondary filename in the pattern",
         Label   => -"Secondary File index",
         Page    => -"Messages");

      Secondary_Line_Pattern_Index := Create
        (Manager => Kernel.Preferences,
         Name    => "GPS6-Messages-Secondary-Line-Regexp-Index",
         Minimum => 1,
         Maximum => 99,
         Default => 3,
         Doc     => -"Index of secondary location line number in the pattern",
         Label   => -"Secondary Line index",
         Page    => -"Messages");

      Secondary_Column_Pattern_Index := Create
        (Manager => Kernel.Preferences,
         Name    => "GPS6-Messages-Secondary-Column-Regexp-Index",
         Minimum => 0,
         Maximum => 99,
         Default => 5,
         Doc     =>
         -"Index of secondary column number in the pattern, 0 if none",
         Label   => -"Secondary Column index",
         Page    => -"Messages");

      Alternate_Secondary_Pattern := Create
        (Manager => Kernel.Preferences,
         Name    => "GPS6-Messages-Alternate-Secondary-Regpat",
         Label   => -"Alternate secondary pattern",
         Doc     =>
           -"Pattern used to detect alternate secondary locations in messages",
         Default => "(at line (\d+))",
         Page    => -"Messages");

      Alternate_Secondary_Line_Index := Create
        (Manager => Kernel.Preferences,
         Name    => "GPS6-Messages-Alternate-Secondary-Line",
         Label   => -"Alternate secondary line index",
         Doc     =>
           -"Index of secondary location line number in the alternate pattern",
         Minimum => 1,
         Maximum => 99,
         Default => 2,
         Page    => -"Messages");

      -- Project Editor --

      Default_Switches_Color := Create
        (Manager => Kernel.Preferences,
         Name    => "Prj-Editor-Default-Switches-Color",
         Default => "#777777",
         Doc     => -("Color to use when displaying switches that are set"
                      & " as default for all the files in the project"),
         Label   => -"Default switches color",
         Page    => "");

      Switches_Editor_Title_Font := Create
        (Manager => Kernel.Preferences,
         Name    => "Prj-Editor-Title-Font",
         Default => "sans bold oblique 14",
         Doc     => -"Font to use for the switches editor dialog",
         Label   => -"Title font",
         Page    => "");

      Variable_Ref_Background := Create
        (Manager => Kernel.Preferences,
         Name    => "Prj-Editor-Var-Ref-Bg",
         Default => "#AAAAAA",
         Doc     => -("Color to use for the background of variable"
                      & " references in the value editor"),
         Label   => -"Variable reference color",
         Page    => "");

      Invalid_Variable_Ref_Background := Create
        (Manager => Kernel.Preferences,
         Name    => "Prj-Editor-Invalid-Var-Ref-Bg",
         Default => "#AA0000",
         Doc     => -("Color to use for the foreground of invalid variable"
                      & " references"),
         Label   => -"Invalid references color",
         Page    => "");

      Generate_Relative_Paths := Create
        (Manager => Kernel.Preferences,
         Name    => "Prj-Editor-Generate-Relative-Paths",
         Default => True,
         Doc     => -("If enabled, use relative paths when the projects are " &
                      "modified, use absolute paths otherwise"),
         Label   => -"Relative project paths",
         Page    => -"Project");

      Trusted_Mode := Create
        (Manager => Kernel.Preferences,
         Name    => "Prj-Editor-Trusted-Mode",
         Default => True,
         Doc     => -("Whether a fast algorithm should be used to load Ada"
                      & " projects. This algorithm assumes the following "
                      & "about your project:" & ASCII.LF
                      & "   - no symbolic links are used to point to other"
                      & " files in the project" & ASCII.LF
                      & "   - no directory has a name which is a valid source"
                      & " file name according to the naming scheme"),
         Label   => -"Fast Project Loading",
         Page    => -"Project");

      Hidden_Directories_Pattern := Create
        (Manager => Kernel.Preferences,
         Name  => "Project-Hidden-Directories-Regexp",
         Label => -"Hidden directories pattern",
         Doc   =>
         -"Directories matching this pattern are removed from the project"
         & " view. This preference is really OS dependent, for"
         & " example on UNIX based systems, files and directories"
         & " starting with a dot are considered as hidden. This regular"
         & " expression is also used to remove VCS specific directories"
         & " like CVS.",
         Default => "(^|\\|/)((\.[^\.]+.*)|CVS)$",
         Page    => -"Project");

      -- Wizards --

      Wizard_Title_Font := Create
        (Manager => Kernel.Preferences,
         Name    => "Wizard-Title-Font",
         Default => "sans bold oblique 10",
         Doc     => -"Font to use for the title of the pages in the wizard",
         Label   => -"Title font",
         Page    => "");

      -- VCS --

      Hide_Up_To_Date := Create
        (Manager => Kernel.Preferences,
         Name    => "VCS-Hide-Up-To-Date",
         Default => False,
         Page    => "VCS:General",
         Doc     => -"Whether up to date files should be hidden by default",
         Label   => -"Hide up-to-date files");

      Hide_Not_Registered := Create
        (Manager => Kernel.Preferences,
         Name    => "VCS-Hide-Not-Registered",
         Default => False,
         Page    => "VCS:General",
         Doc     => -"Whether unregistered files should be hidden by default",
         Label   => -"Hide non registered files");

      Default_VCS := Create
        (Manager => Kernel.Preferences,
         Name    => "Default-VCS",
         Default => "Auto",
         Page    => -"VCS:General",
         Doc     =>
         -"The default VCS to use when no VCS is defined in the project",
         Label   => -"Default VCS");

      -- CVS --

      CVS_Command := Create
        (Manager => Kernel.Preferences,
         Name    => "CVS-Command",
         Default => "cvs",
         Doc     => -"General CVS command",
         Page    => "",
         Label   => -"CVS command");

      -- ClearCase --

      ClearCase_Command := Create
        (Manager => Kernel.Preferences,
         Name    => "ClearCase-Command",
         Default => "cleartool",
         Doc     => -"General ClearCase command",
         Page    => "",
         Label   => -"ClearCase command");

      -- External Commands --

      List_Processes := Create
        (Manager => Kernel.Preferences,
         Name     => "Helpers-List-Processes",
         Label    => -"List processes",
         Doc      =>
         -("Command used to list processes running on the machine." & ASCII.LF
           & "On Unix machines, you should surround the command with"
           & " triple-quotes similar to what python uses, and execute the"
           & " command through sh -c so that environment variables and"
           & " output redirection are properly executed"),
         Default  => Config.Default_Ps,
         Page     => -"External Command");

      Execute_Command := Create
        (Manager => Kernel.Preferences,
         Name    => "Helpers-Execute-Command",
         Label   => -"Execute command",
         Doc     => -"Program used to execute commands externally",
         Default => Config.Exec_Command,
         Page    => -"External Command");

      if Config.Host /= Config.Windows then
         --  Preference not used under Windows

         Html_Browser := Create
           (Manager => Kernel.Preferences,
            Name    => "Helpers-HTML-Browser",
            Label   => -"HTML browser",
            Doc     =>
            -("Program used to browse HTML pages. " &
              "No value means automatically try to find a suitable browser."
              & ASCII.LF
              & "The special parameter %u will be replaced by the URL. If it"
              & " isn't specified, the URL will be appended at the end of"
              & " the command."
              & ASCII.LF
              & "If you wish to automatically open a new tab in the firefox"
              & " browser, instead of replacing the current one, you could set"
              & " this command to" & ASCII.LF
              & "    firefox -remote ""openURL(%u,new-tab)"""),
            Default => "",
            Page    => -"External Command");
      end if;

      Print_Command := Create
        (Manager => Kernel.Preferences,
         Name    => "Helpers-Print-Command",
          Label  => -"Print command",
          Doc    => -("Program used to print files. No value means use " &
                       "the built-in printing capability (available under " &
                       "Windows only)"),
         Default => Config.Default_Print_Cmd,
         Page    => -"External Command");

      Max_Output_Length := Create
        (Manager => Kernel.Preferences,
         Name    => "Max-Output-Length",
         Label   => -"Maximum output length",
         Doc     => -("Maximum output length of output taken into account by"
           & "GPS, in bytes."),
         Minimum => 1_000,
         Maximum => Integer'Last,
         Default => 10_000_000,
         Page    => "");

      Tooltips_Background := Create
        (Manager => Kernel.Preferences,
         Name    => "Tooltips-Background-Color",
         Label   => -"Tooltips background",
         Doc     =>
            -("Color used for the background of tooltip windows. The default"
              & " is to use the color set by the gtk+ theme (this is also"
              & " the color used if you set this preference to full white)"),
         Default => "#FFFFFF",
         Page    => -"Windows");

      System_Menus := Create_Invisible_Pref
        (Manager  => Kernel.Preferences,
         Name     => "system-menus",
         Label    => -"System menus",
         Doc      =>
           -("If set, this will display the menubar outside of the main GPS"
           & " window on systems that sypport it (OSX and Unity). It has no"
           & " effect on other systems. Requires a restart of GPS."),
         Default  => False);

      Doc_Search_Before_First := Create
        (Manager => Kernel.Preferences,
         Name    => "Doc-Search-Before-First",
         Label   => -"Leading documentation",
         Doc     =>
           -("If this preference is set, GPS will extract the documentation"
           & " for an entity by first looking at the leading comments, and"
           & " fallback to the comments after the entity declaration if not"
           & " found. If the preference is unset, the search order is"
           & " reversed."),
         Default => True,
         Page    => -"Documentation");

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
            Name    : constant String := Get_Attribute (Node, "name", "");
            Page    : constant String :=
                        Get_Attribute (Node, "page", "General");
            Default : constant String := Get_Attribute (Node, "default", "");
            Tooltip : constant String := Get_Attribute (Node, "tip", "");
            Label   : constant String := Get_Attribute (Node, "label", "");
            Typ     : constant String := Get_Attribute (Node, "type", "");
            Min     : constant String := Get_Attribute (Node, "minimum", "0");
            Max     : constant String := Get_Attribute (Node, "maximum", "10");
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
                  Page    => Page,
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
                  Page    => Page)));

            elsif Typ = "string" then
               Pref := Preference (String_Preference'(Create
                 (Manager => Kernel.Preferences,
                  Name    => Name,
                  Label   => Label,
                  Doc     => Tooltip,
                  Default => Default,
                  Page    => Page)));

            elsif Typ = "color" then
               Pref := Preference (Color_Preference'(Create
                 (Manager => Kernel.Preferences,
                  Name    => Name,
                  Label   => Label,
                  Page    => Page,
                  Doc     => Tooltip,
                  Default => Default)));

            elsif Typ = "font" then
               Pref := Preference (Font_Preference'(Create
                 (Manager => Kernel.Preferences,
                  Name    => Name,
                  Label   => Label,
                  Doc     => Tooltip,
                  Default => Default,
                  Page    => Page)));

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
                     Page      => Page,
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
        (Kernel.Scripts, "create_style",
         Params       =>
           (1 => Param ("label"),
            2 => Param ("doc", Optional => True),
            3 => Param ("default_font_style", Optional => True),
            4 => Param ("default_bg", Optional => True),
            5 => Param ("default_fg", Optional => True)),
         Class        => Pref_Class,
         Handler      => Get_Command_Handler'Access);

      --  Register the commands associated to the PreferencesPage class
      Register_Command
        (Kernel.Scripts, "create",
         Params        => (1 => Param ("name"),
                           2 => Param ("get_widget"),
                           3 => Param ("priority", Optional => True)),
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
            Modify_Font (Widget, Default_Font.Get_Pref_Font);
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
      C : constant Check_Menu_Item_Pref := new Check_Menu_Item_Pref_Record;
      P : access Pref_Changed_For_Menu_Item;
      Doc : constant String := Pref.Get_Doc;
   begin
      Gtk.Check_Menu_Item.Initialize (C, Pref.Get_Label);
      C.Kernel := Kernel;
      C.Pref := Pref;
      Menu.Add (C);

      if Doc /= "" then
         C.Set_Tooltip_Text (Doc);
      end if;

      C.Set_Active (Pref.Get_Pref);
      C.On_Toggled (On_Check_Menu_Item_Changed'Access);

      P := new Pref_Changed_For_Menu_Item;
      P.Check := C;
      Preferences_Changed_Hook.Add (P, Watch => C);
   end Append_Menu;

end GPS.Kernel.Preferences;
