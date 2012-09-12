------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
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
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNATCOLL.Scripts;          use GNATCOLL.Scripts;
with GNATCOLL.Traces;           use GNATCOLL.Traces;
with GNAT.Strings;              use GNAT.Strings;

with XML_Utils;              use XML_Utils;

with Config;
with Default_Preferences.Enums; use Default_Preferences.Enums;
with Entities.Queries;          use Entities.Queries;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Charsets;       use GPS.Kernel.Charsets;
with GPS.Kernel.Console;        use GPS.Kernel.Console;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with Language;                  use Language;
with Traces;

package body GPS.Kernel.Preferences is
   Me : constant Trace_Handle := Create ("GPS_KERNEL");

   use type Config.Host_Type;

   Preferences_Pages : Preferences_Page_Array_Access;
   --  ??? To be included in the kernel

   Value_Cst : aliased constant String := "value";
   Set_Cmd_Parameters : constant Cst_Argument_List := (1 => Value_Cst'Access);

   procedure Get_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Get preference command handler

   type Preferences_Module is new Module_ID_Record with null record;
   overriding procedure Customize
     (Module : access Preferences_Module;
      File   : GNATCOLL.VFS.Virtual_File;
      Node   : XML_Utils.Node_Ptr;
      Level  : Customization_Level);
   --  Handle GPS customization files for this module

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
         Name_Parameters (Data, Set_Cmd_Parameters);
         declare
            Name : constant String     := Get_Data (Inst, Class);
            Pref : constant Preference :=
                     Get_Pref_From_Name (Kernel.Preferences, Name, False);
            Done : Boolean := True;
         begin
            if Pref = null then
               Set_Error_Msg (Data, -"Unknown preference " & Name);
               Done := False;

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
            then
               Set_Pref (Pref, Kernel.Preferences, String'(Nth_Arg (Data, 2)));

            else
               Done := False;
               Set_Error_Msg (Data, -"Preference not supported");
            end if;

            if Done and then Nth_Arg (Data, 3, True) then
               Save_Preferences
                 (Kernel,
                  Create_From_Dir (Get_Home_Dir (Kernel), "preferences"));
               Run_Hook (Kernel, Preferences_Changed_Hook);
            end if;

         exception
            when E : Invalid_Parameter =>
               Set_Error_Msg (Data, Exception_Message (E));
            when E : others => Trace (Traces.Exception_Handle, E);
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
                  Default => Nth_Arg (Data, 5, Config.Default_Font))));

            elsif Typ = "enum" then
               declare
                  Val : constant String_List_Access :=
                    new String_List (1 .. Number_Of_Arguments (Data) - 5);
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
         end;
      end if;
   end Get_Command_Handler;

   ---------------------------------
   -- Register_Global_Preferences --
   ---------------------------------

   procedure Register_Global_Preferences
     (Kernel : access Kernel_Handle_Record'Class) is
   begin
      -- General --
      Gtk_Theme := Create
        (Kernel.Preferences,
         Name  => "Gtk-Theme-Name",
         Label => -"Theme",
         Page  => -"General",
         Doc   => -("Select a theme from the list to change the general "
                     & "appearance of GPS"));

      Default_Font := Create
        (Manager => Kernel.Preferences,
         Name    => "General-Default-Style",
         Default_Font => Config.Default_Font,
         Default_Fg   => "black",
         Default_Bg   => "white",
         Doc     => -("The default style used in GPS. The color indicates the"
           & " what should be used for the background color of windows (for"
           & " editors check the Editor/Colors preference page)."),
         Page    => -"General",
         Label   => -"Default font");

      View_Fixed_Font := Create
        (Manager => Kernel.Preferences,
         Name    => "General-Fixed-Font",
         Default => Config.Default_Fixed_Font,
         Doc     => -("Fixed pitch (monospace) font used in the various views "
                      & "(Outline View, Clipboard View, Messages, ...)"),
         Label   => -"Fixed view font",
         Page    => -"General");

      GPS.Kernel.Charsets.Register_Preferences (Kernel);

      Default_Widget_Width := Create
        (Manager => Kernel.Preferences,
         Name    => "General-Default-Widget-Width",
         Label   => -"Default width",
         Doc     => -"Default width for all the newly created windows",
         Minimum => 50,
         Maximum => 2000,
         Default => 200,
         Page    => "");

      Default_Widget_Height := Create
        (Manager => Kernel.Preferences,
         Name    => "General-Default-Widget-Height",
         Label   => -"Default height",
         Doc     => -"Default height for all the newly created windows",
         Minimum => 50,
         Maximum => 2000,
         Default => 200,
         Page    => "");

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
         Page    => -"General");

      Display_Welcome := Create
        (Manager => Kernel.Preferences,
         Name    => "General-Display-Welcome",
         Label   => -"Display welcome window",
         Doc     => -("Enabled when GPS should display the welcome window"
                      & " for the selection of the project"),
         Default => True,
         Page    => -"General");

      Toolbar_Show_Text := Create
        (Manager => Kernel.Preferences,
         Name    => "General-Toolbar-Text",
         Label   => -"Show text in tool bar",
         Doc     => -("Enabled if tool bar should show both text and icons,"
                      & " Disabled if it should only show icons"),
         Default => False,
         Page    => -"General");

      Submenu_For_Dispatching_Calls := Dispatching_Menu_Policy_Prefs.Create
        (Manager => Kernel.Preferences,
         Name  => "Submenu-For-Dispatching",
         Label => -"Submenu for dispatching calls",
         Doc   => -("If you are using a GNAT version more recent than"
           & " 2007-09-21, cross-references on dispatching calls can"
           & " list all the subprograms that might be called at run time."
           & " However, computing this info might take some time, and the"
           & " preference lets you chose how GPS should behave:"
           & ASCII.LF
           & "Never: no special submenu is displayed for dispatching calls"
           & ASCII.LF
           & "From Memory: only the information already available in memory"
              & " is used. Some possible subprograms will not be listed"
           & ASCII.LF
           & "Accurate: GPS will reload the cross-references information"
           & " from the disk as needed. This might result in long delays"
           & " (up to several seconds) if lots of information needs to be"
           & " loaded. This method is always used when computing"
           & " dispatching information in call graphs."),
         Page    => -"Editor",
         Default => From_Memory);

      Auto_Save := Create
        (Manager => Kernel.Preferences,
         Name    => "General-Auto-Save",
         Label   => -"Auto save",
         Doc     => -("Whether unsaved files/projects should be saved"
                      & " automatically before calling external tools"),
         Default => True,
         Page    => -"General");

      Save_Desktop_On_Exit := Create
        (Manager => Kernel.Preferences,
         Name    => "General-Save-Desktop-On-Exit",
         Label   => -"Save desktop on exit",
         Doc     => -"Whether the desktop should be saved when exiting GPS",
         Default => True,
         Page    => -"General");

      Save_Editor_Desktop := Editor_Desktop_Policy_Prefs.Create
        (Manager => Kernel.Preferences,
         Name    => "General-Editor-Desktop-Policy",
         Label   => "Save editor in desktop",
         Doc     => -"When to save source editors in the desktop",
         Page    => -"General",
         Default => From_Project);

      Multi_Language_Builder := Multi_Language_Builder_Policy_Prefs.Create
        (Manager => Kernel.Preferences,
         Name    => "General-Default-Builder",
         Label   => -"Default builder",
         Doc     =>
         -("GPS default builder choice:" & ASCII.LF &
           "  - Auto (gnatmake for Ada projects, gprbuild otherwise)" &
           ASCII.LF &
           "  - Gnatmake (will disable multi-language builds)" & ASCII.LF &
           "  - Gprbuild (force use of gprbuild even for Ada projects)"
          ),
         Page    => -"General",
         Default => Default_Builder);

      Auto_Jump_To_First := Create
        (Manager => Kernel.Preferences,
         Name    => "Auto-Jump-To-First",
         Default => True,
         Doc     =>
         -("Whether GPS should automatically jump to the first location"
           & " when entries are added to the Location window (error"
           & " messages, find results, ...)"),
         Label   => -"Jump to first location",
         Page    => -"General");

      Locations_Wrap := Create
        (Manager => Kernel.Preferences,
         Name    => "Locations-Wrap",
         Label   => -"Wrap around on next/previous",
         Doc     => -("Whether using the Next Tag and Previous Tag actions "
           & " should wrap around to the beginning when reaching the end of "
           & " the category."),
         Default => True,
         Page    => "General");

      Locations_Auto_Close := Create
        (Manager => Kernel.Preferences,
         Name    => "Locations-Auto-Close",
         Label   => -"Auto close Locations view",
         Doc     => -("Whether the Locations view should be closed "
             & "automatically when it becomes empty."),
         Default => False,
         Page    => "General");

      Hyper_Mode := Create
        (Manager => Kernel.Preferences,
         Name    => "Hyper-Mode",
         Default => True,
         Doc     =>
         -("Whether to allow hyper links to appear in editors when the"
          & " Control key is pressed."),
         Label   => -"Hyper links",
         Page    => -"General");

      Tooltip_Color := Create
        (Manager => Kernel.Preferences,
         Name    => "General-Tooltip-Color",
         Default => "#FFFFEE",
         Doc     => -"Color to use for the tooltips background",
         Label   => -"Tooltip color",
         Page    => "");

      Tip_Of_The_Day := Create
        (Manager => Kernel.Preferences,
         Name    => "General/Display-Tip-Of-The-Day",
         Default => True,
         Doc     => -("Whether GPS should display the Tip of the Day dialog"),
         Label   => -"Tip of the Day",
         Page    => -"General");

      SQLITE_Db := Create
        (Manager => Kernel.Preferences,
         Name    => "Internal-Use-Sqlite",
         Label   => -"",
         Doc     => -"",
         Default => Active (Entities.SQLITE),
         Page    => "");

      -- Source Editor --

      Strip_Blanks := Strip_Trailing_Blanks_Policy_Prefs.Create
        (Manager => Kernel.Preferences,
         Name    => "Src-Editor-Strip-Trailing-Blanks",
         Label   => -"Strip blanks",
         Doc     =>
           -"Should the editor remove trailing blanks when saving files",
         Default => Autodetect,
         Page    => -"Editor");

      Strip_Lines := Strip_Trailing_Blanks_Policy_Prefs.Create
        (Manager => Kernel.Preferences,
         Name    => "Src-Editor-Strip-Trailing-Lines",
         Label   => -"Strip lines",
         Doc     =>
           -"Should the editor remove trailing blank lines when saving files",
         Default => Autodetect,
         Page    => -"Editor");

      Line_Terminator := Line_Terminators_Prefs.Create
        (Manager => Kernel.Preferences,
         Name  => "Src-Editor-Line-Terminator",
         Label => -"Line terminator",
         Doc   => -"Line terminator style to use when saving files",
         Default => Unchanged,
         Page    => -"Editor");

      Display_Line_Numbers := Create
        (Manager => Kernel.Preferences,
         Name    => "Src-Editor-Display-Line_Numbers",
         Default => True,
         Doc     =>
           -"Whether the line numbers should be displayed in file editors",
         Label   => -"Display line numbers",
         Page    => -"Editor");

      Display_Subprogram_Names := Create
        (Manager => Kernel.Preferences,
         Name    => "Src-Editor-Display-Subprogram_Names",
         Default => True,
         Doc     =>
           -"Whether the subprogram names should be displayed in status lines",
         Label   => -"Display subprogram names",
         Page    => -"Editor");

      Display_Tooltip := Create
        (Manager => Kernel.Preferences,
         Name    => "Src-Editor-Display-Tooltip",
         Default => True,
         Doc     => -"Whether tooltips should be displayed automatically",
         Label   => -"Tooltips",
         Page    => -"Editor");

      Tooltip_Timeout := Create
        (Manager => Kernel.Preferences,
         Name    => "Src-Editor-Tooltip-Timeout",
         Minimum => 0,
         Maximum => 10000,
         Default => 600,
         Doc     => -"Time (in milliseconds) before displaying tooltips",
         Label   => -"Tooltips timeout",
         Page    => -"Editor");

      Highlight_Delimiters := Create
        (Manager => Kernel.Preferences,
         Name    => "Src-Editor-Highlight-Delimiters",
         Default => True,
         Doc     => -"Whether delimiters should be highlighted: (){}[]",
         Label   => -"Highlight delimiters",
         Page    => -"Editor");

      Periodic_Save := Create
        (Manager => Kernel.Preferences,
         Name    => "Src-Editor-Periodic-Save",
         Minimum => 0,
         Maximum => 3600,
         Default => 60,
         Doc     => -("The period (in seconds) after which a source editor"
                      & " is automatically saved. 0 if none."),
         Label   => -"Autosave delay",
         Page    => -"Editor");

      Highlight_Column := Create
        (Manager => Kernel.Preferences,
         Name    => "Src-Editor-Highlight-Column",
         Minimum => 0,
         Maximum => 255,
         Default => 80,
         Doc     => -("The right margin to highlight. 0 if none. This value "
                      & "is also used to implement the Edit->Refill command"),
         Label   => -"Right margin",
         Page    => -"Editor");

      Block_Highlighting := Create
        (Manager => Kernel.Preferences,
         Name    => "Src-Editor-Block-Highlighting",
         Default => True,
         Doc     =>
           -"Should the editor enable block highlighting",
         Label   => -"Block highlighting",
         Page    => -"Editor");

      Block_Folding := Create
        (Manager => Kernel.Preferences,
         Name    => "Src-Editor-Block-Folding",
         Default => True,
         Doc     => -"Should the editor enable block folding",
         Label   => -"Block folding",
         Page    => -"Editor");

      Automatic_Syntax_Check := Create
        (Manager => Kernel.Preferences,
         Name    => "Src-Editor-Automatic-Syntax-Check",
         Default => False,
         Doc     => -"Enable/Disable automatic syntax check",
         Label   => -"Automatic syntax check",
         Page    => "");

      Speed_Column_Policy := Speed_Column_Policy_Prefs.Create
        (Manager => Kernel.Preferences,
         Name    => "Src-Editor-Speed-Column-Policy",
         Label   => -"Speed column policy",
         Doc     => -"When the speed column should be displayed",
         Default => Automatic,
         Page    => -"Editor");

      if Config.Host = Config.Windows then
         Use_ACL := Create
           (Manager => Kernel.Preferences,
            Name    => "Src-Editor-Use-ACL",
            Label   => -"Use Windows ACL",
            Doc     =>
            -"Whether GPS should use ACL when changing the "
            & "read/write permissions",
            Default => False,
            Page    => -"Editor");
      end if;

      Default_Style := Create
        (Manager      => Kernel.Preferences,
         Name         => "Src-Editor-Reference-Style",
         Label        => -"Default",
         Doc          => -("Default style used in the source editors."
           & " The background color defined here also defines the background"
           & " color of all editors."),
         Default_Font => Config.Default_Fixed_Font,
         Default_Fg   => "black",
         Default_Bg   => "white",
         Page         => -"Editor/Fonts & Colors");

      Block_Style := Create
        (Manager      => Kernel.Preferences,
         Name         => "Src-Editor-Block-Variant",
         Label        => -"Block",
         Doc          => -("Style to use when displaying blocks (subprograms,"
           & "tasks, entries, ...) in declarations."),
         Base            => Default_Style,
         Default_Variant => Default,
         Default_Fg      => "#A600D9",
         Default_Bg      => "white",
         Page            => -"Editor/Fonts & Colors");

      Type_Style := Create
        (Manager      => Kernel.Preferences,
         Name         => "Src-Editor-Type-Variant",
         Label        => -"Type",
         Doc          => -("Style to use when displaying types in "
           & "declarations."),
         Base            => Default_Style,
         Default_Variant => Default,
         Default_Fg   => "#009CD9",
         Default_Bg   => "white",
         Page         => -"Editor/Fonts & Colors");

      Keywords_Style := Create
        (Manager      => Kernel.Preferences,
         Name         => "Src-Editor-Keywords-Variant",
         Label        => -"Keywords",
         Doc          => -("Style to use when displaying keywords."
           & " The background color will be that of the default if left"
           & " to white"),
         Base            => Default_Style,
         Default_Variant => Bold,
         Default_Fg      => "black",
         Default_Bg      => "white",
         Page            => -"Editor/Fonts & Colors");

      Comments_Style := Create
        (Manager      => Kernel.Preferences,
         Name         => "Src-Editor-Comments-Variant",
         Label        => -"Comments",
         Doc          => -"Style to use when displaying comments."
           & " The background color will be that of the default if left"
           & " to white",
         Base            => Default_Style,
         Default_Variant => Italic,
         Default_Fg   => "blue",
         Default_Bg   => "white",
         Page         => -"Editor/Fonts & Colors");

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
         Default_Fg   => "#21A9DE",
         Default_Bg   => "white",
         Page         => -"Editor/Fonts & Colors");

      Strings_Style := Create
        (Manager      => Kernel.Preferences,
         Name         => "Src-Editor-Strings-Variant",
         Label        => -"Strings",
         Doc          => -"Style to use when displaying strings."
           & " The background color will be that of the default if left"
           & " to white",
         Base            => Default_Style,
         Default_Variant => Default,
         Default_Fg   => "brown",
         Default_Bg   => "white",
         Page         => -"Editor/Fonts & Colors");

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
         Page         => -"Editor/Fonts & Colors");

      Current_Line_Color := Create
        (Manager => Kernel.Preferences,
         Name    => "Src-Editor-Current-Line-Color",
         Default => "#D1DCFC",
         Doc     => -("Color for highlighting the current line. White means"
                      & " transparent"),
         Label   => -"Current line color",
         Page    => -"Editor/Fonts & Colors");

      Current_Line_Thin := Create
        (Manager => Kernel.Preferences,
         Name    => "Src-Editor-Current-Line-Thin",
         Default => True,
         Doc     => -("Whether to use a thin line rather than full background"
           & ASCII.LF & " highlighting on the current line."),
         Label   => -"Draw current line as a thin line",
         Page    => -"Editor/Fonts & Colors");

      Current_Block_Color := Create
        (Manager => Kernel.Preferences,
         Name    => "Src-Editor-Current-Block-Color",
         Default => "#9C9CFF",
         Doc     => -"Color for highlighting the current block",
         Label   => -"Current block color",
         Page    => -"Editor/Fonts & Colors");

      Delimiter_Color := Create
        (Manager => Kernel.Preferences,
         Name    => "Src-Editor-Highlight-Delimiters-Color",
         Default => "cyan",
         Doc     => -"Color for highlighting delimiters",
         Label   => -"Delimiter highlighting color",
         Page    => -"Editor/Fonts & Colors");

      Search_Results_Color := Create
        (Manager => Kernel.Preferences,
         Name    => "Src-Editor-Search_Results-Color",
         Default => "light blue",
         Doc     => -"Color for highlighting search results",
         Label   => -"Search results highlighting",
         Page    => -"Editor/Fonts & Colors");

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
         Page    => -"Refactoring");

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
         Page    => -"Refactoring");

      Create_Subprogram_Decl  := Create
        (Manager => Get_Preferences (Kernel),
         Name    => "Refactoring-Subprogram-Spec",
         Default => True,
         Doc     => -(
           "Whether GPS should create a declaration for the subprogram. If"
           & " set to False, only the body of the subprogram will be created"),
         Label   => -"Create Subprogram Declarations",
         Page    => -"Refactoring");

      -- Browsers --

      Browsers_Bg_Color := Create
        (Manager => Kernel.Preferences,
         Name    => "Browsers-Bg-Color",
         Default => "#BBBBBB",
         Doc     => -"Color used to draw the background of the browsers",
         Label   => -"Background color",
         Page    => -"Browsers");

      Browsers_Bg_Image := Create
        (Manager => Kernel.Preferences,
         Name    => "Browsers-Bg-Image",
         Label   => -"Background image",
         Page    => "",
         Doc     =>
           -("Image to draw in the background of browsers. If left empty,"
             & " no image is drawn. Using a large image will slow down"
             & " performances"),
         Default => "");

      Browsers_Draw_Grid := Create
        (Manager => Kernel.Preferences,
         Name    => "Browsers-Draw-Grid",
         Default => True,
         Doc     => -"Whether a grid should be displayed in the browsers",
         Page    => "",
         Label   => -"Draw grid");

      Browsers_Hyper_Link_Color := Create
        (Manager => Kernel.Preferences,
         Name    => "Browsers-Hyper-Link-Color",
         Default => "#0000FF",
         Doc     => -"Color used to draw the hyper links in the items",
         Label   => -"Hyper link color",
         Page    => -"Browsers");

      Selected_Link_Color := Create
        (Manager => Kernel.Preferences,
         Name    => "Browsers-Selected-Link-Color",
         Default => "#FF0000",
         Doc     => -"Color to use for links between selected items",
         Label   => -"Selected link color",
         Page    => -"Browsers");

      Unselected_Link_Color := Create
        (Manager => Kernel.Preferences,
         Name    => "Browsers-Unselected-Link-Color",
         Default => "#000000",
         Doc     => -"Color to use for links between unselected items",
         Label   => -"Default link color",
         Page    => -"Browsers");

      Parent_Linked_Item_Color := Create
        (Manager => Kernel.Preferences,
         Name    => "Browsers-Linked-Item-Color",
         Default => "#AAAAAA",
         Doc     => -("Color to use for the background of the items linked"
                      & " to the selected item"),
         Label   => -"Ancestor items color",
         Page    => -"Browsers");

      Child_Linked_Item_Color := Create
        (Manager => Kernel.Preferences,
         Name    => "Browsers-Child-Linked-Item-Color",
         Default => "#DDDDDD",
         Doc     => -("Color to use for the background of the items linked"
                      & " from the selected item"),
         Label   => -"Offspring items color",
         Page    => -"Browsers");

      Selected_Item_Color := Create
        (Manager => Kernel.Preferences,
         Name    => "Browsers-Selected-Item-Color",
         Default => "#888888",
         Doc     => -"Color to use to draw the selected item",
         Label   => -"Selected item color",
         Page    => -"Browsers");

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
         Page    => -"Browsers");

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
         Page    => -"VCS");

      -- Diff_Utils --

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
         Page    => -"Visual diff");

      Diff_Cmd := Create
        (Manager => Kernel.Preferences,
         Name    => "Diff-Utils-Diff",
         Label   => -"Diff command",
         Doc     => -("Command used to compute differences between two files."
                      & " Arguments can also be specified"),
         Default => Config.Default_Diff_Cmd,
         Page    => -"Visual diff");

      Patch_Cmd := Create
        (Manager => Kernel.Preferences,
         Name    => "Diff-Utils-Patch",
         Label   => -"Patch command",
         Doc     =>
           -"Command used to apply a patch. Arguments can also be specified",
         Default => Config.Default_Patch_Cmd,
         Page    => -"Visual diff");

      Old_Vdiff := Create
        (Manager => Kernel.Preferences,
         Name    => "Diff-Utils-Old-Vdiff",
         Label   => -"Use old diff (requires restart)",
         Doc     => -("Use the old version of visual differences."
                      & " Changing this parameter requires restarting GPS."),
         Default => False,
         Page    => -"Visual diff");

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
         Default => "#A2B6FF",
         Page    => -"Messages");

      File_Pattern := Create
        (Manager => Kernel.Preferences,
         Name    => "Messages-File-Regpat-1",
         Label   => -"File pattern",
         Doc     =>
           -"Pattern used to detect file locations (e.g error messages)",
         Default =>
           "^([^:]:?[^:]*):(\d+):((\d+):)? ((warning)?(info|Note|check)?" &
           "(\(style)?.*)",
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
         Default => 7,
         Doc     => -"Index of compiler info in the pattern, 0 if none",
         Label   => -"Info index",
         Page    => -"Messages");

      Style_Pattern_Index := Create
        (Manager => Kernel.Preferences,
         Name    => "Messages-Style-Regexp-Index-1",
         Minimum => 0,
         Maximum => 99,
         Default => 8,
         Doc     => -"Index of style indication in the pattern, 0 if none",
         Label   => -"Style index",
         Page    => -"Messages");

      Secondary_File_Pattern := Create
        (Manager => Kernel.Preferences,
         Name    => "Messages-Secondary-File-Regpat",
         Label   => -"Secondary File pattern",
         Doc     =>
           -"Pattern used to detect secondary file locations in messages",
         Default => "(([^:( ]+):(\d+)(:(\d+):?)?)",
         Page    => -"Messages");

      Secondary_File_Pattern_Index := Create
        (Manager => Kernel.Preferences,
         Name    => "Messages-Secondary-File-Regexp-Index",
         Minimum => 1,
         Maximum => 99,
         Default => 2,
         Doc     => -"Index of secondary filename in the pattern",
         Label   => -"Secondary File index",
         Page    => -"Messages");

      Secondary_Line_Pattern_Index := Create
        (Manager => Kernel.Preferences,
         Name    => "Messages-Secondary-Line-Regexp-Index",
         Minimum => 1,
         Maximum => 99,
         Default => 3,
         Doc     => -"Index of secondary location line number in the pattern",
         Label   => -"Secondary Line index",
         Page    => -"Messages");

      Secondary_Column_Pattern_Index := Create
        (Manager => Kernel.Preferences,
         Name    => "Messages-Secondary-Column-Regexp-Index",
         Minimum => 0,
         Maximum => 99,
         Default => 5,
         Doc     =>
         -"Index of secondary column number in the pattern, 0 if none",
         Label   => -"Secondary Column index",
         Page    => -"Messages");

      Alternate_Secondary_Pattern := Create
        (Manager => Kernel.Preferences,
         Name    => "Messages-Alternate-Secondary-Regpat",
         Label   => -"Alternate secondary pattern",
         Doc     =>
           -"Pattern used to detect alternate secondary locations in messages",
         Default => "(at line (\d+))",
         Page    => -"Messages");

      Alternate_Secondary_Line_Index := Create
        (Manager => Kernel.Preferences,
         Name    => "Messages-Alternate-Secondary-Line",
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

      Automatic_Xrefs_Load := Create
        (Manager => Kernel.Preferences,
         Name    => "Load-Xref-Info-At-Startup",
         Default => False,
         Doc     => -("Whether to load the Xref info in memory whenever a"
           & " new project is loaded into memory, or a new file is"
           & " compiled."),
         Label   => -"Load Xref info automatically",
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
         Default => "^((\..+)|CVS)$",
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
         Page    => "",
         Doc     => -"Whether up to date files should be hidden by default",
         Label   => -"Hide up-to-date files");

      Hide_Not_Registered := Create
        (Manager => Kernel.Preferences,
         Name    => "VCS-Hide-Not-Registered",
         Default => False,
         Page    => "",
         Doc     => -"Whether unregistered files should be hidden by default",
         Label   => -"Hide non registered files");

      Default_VCS := Create
        (Manager => Kernel.Preferences,
         Name    => "Default-VCS",
         Default => "Auto",
         Page    => -"VCS",
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
                    new String_List (1 .. Child_Count);
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
      Pref_Class : constant Class_Type := New_Class (Kernel, "Preference");
      Module     : Module_ID;
   begin
      Module := new Preferences_Module;
      GPS.Kernel.Modules.Register_Module
        (Module      => Module,
         Kernel      => Kernel,
         Module_Name => "Preferences");
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
        (Kernel, "set",
         Minimum_Args => 1,
         Maximum_Args => 2,
         Class        => Pref_Class,
         Handler      => Get_Command_Handler'Access);
      Register_Command
        (Kernel, "create",
         Minimum_Args => 2,
         Maximum_Args => Integer'Last,
         Class        => Pref_Class,
         Handler      => Get_Command_Handler'Access);
   end Register_Module;

   ----------------------
   -- Edit_Preferences --
   ----------------------

   procedure Edit_Preferences (Kernel : access Kernel_Handle_Record'Class) is
      procedure On_Changed (Manager : access Preferences_Manager_Record'Class);
      --  Called when the preferences have been changed

      ----------------
      -- On_Changed --
      ----------------

      procedure On_Changed
        (Manager : access Preferences_Manager_Record'Class)
      is
         pragma Unreferenced (Manager);
      begin
         Run_Hook (Kernel, Preferences_Changed_Hook);
      end On_Changed;

   begin
      if Preferences_Pages = null then
         Preferences_Pages := new Preferences_Page_Array (1 .. 0);
      end if;

      Edit_Preferences
        (Manager      => Kernel.Preferences,
         Parent       => Get_Main_Window (Kernel),
         On_Changed   => On_Changed'Unrestricted_Access,
         Custom_Pages => Preferences_Pages.all);

      Save_Preferences
        (Kernel, Create_From_Dir (Get_Home_Dir (Kernel), "preferences"));
   end Edit_Preferences;

   ----------------------
   -- Save_Preferences --
   ----------------------

   procedure Save_Preferences
     (Kernel    : access Kernel_Handle_Record'Class;
      File_Name : Virtual_File)
   is
      Success : Boolean;

   begin
      Trace (Me, "Saving preferences in " & File_Name.Display_Full_Name);
      Save_Preferences (Kernel.Preferences, File_Name, Success);

      if not Success then
         Report_Preference_File_Error (Kernel, File_Name);
      end if;
   end Save_Preferences;

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

   ----------------------
   -- Save_Preferences --
   ----------------------

   procedure Save_Preferences
     (Kernel : access Kernel_Handle_Record'Class;
      Saved  : out Default_Preferences.Saved_Prefs_Data) is
   begin
      Save_Preferences (Kernel.Preferences, Saved);
   end Save_Preferences;

   -------------------
   -- Register_Page --
   -------------------

   procedure Register_Page
     (Kernel : access Kernel_Handle_Record'Class;
      Page   : access Preferences_Page_Record'Class)
   is
      pragma Unreferenced (Kernel);
      Tmp : Preferences_Page_Array_Access := Preferences_Pages;
   begin
      if Tmp = null then
         Preferences_Pages := new Preferences_Page_Array (1 .. 1);
      else
         Preferences_Pages := new Preferences_Page_Array (1 .. Tmp'Last + 1);
         Preferences_Pages (Tmp'Range) := Tmp.all;
      end if;

      Preferences_Pages (Preferences_Pages'Last) := Preferences_Page (Page);
      Unchecked_Free (Tmp);
   end Register_Page;

end GPS.Kernel.Preferences;
