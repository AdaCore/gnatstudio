-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2001-2008, AdaCore             --
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
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with Ada.Strings.Maps;          use Ada.Strings.Maps;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNATCOLL.Scripts;          use GNATCOLL.Scripts;
with GNATCOLL.Traces;           use GNATCOLL.Traces;
with Interfaces.C.Strings;      use Interfaces.C, Interfaces.C.Strings;

with Gdk.Color;               use Gdk.Color;
with Gdk.Types;               use Gdk.Types;

with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Properties;         use Glib.Properties;
with Glib.Xml_Int;            use Glib.Xml_Int;
with Gtk.Style;

with Pango.Font;              use Pango.Font;

with Case_Handling;           use Case_Handling;
with Config;
with Entities.Queries;        use Entities.Queries;
with GPS.Intl;                use GPS.Intl;
with GPS.Kernel.Charsets;     use GPS.Kernel.Charsets;
with GPS.Kernel.Console;      use GPS.Kernel.Console;
with GPS.Kernel.Hooks;        use GPS.Kernel.Hooks;
with GPS.Kernel.Modules;      use GPS.Kernel.Modules;
with GPS.Kernel.Scripts;      use GPS.Kernel.Scripts;
with Language;                use Language;
with Traces;

package body GPS.Kernel.Preferences is
   Me : constant Trace_Handle := Create ("GPS_KERNEL");

   use type Config.Host_Type;

   package Line_Terminators_Properties is new Generic_Enumeration_Property
     ("Line_Terminators", Line_Terminators);

   package Speed_Column_Policy_Properties is new Generic_Enumeration_Property
     ("Speed_Column_Policies", Speed_Column_Policies);

   package Editor_Desktop_Policy_Properties is new Generic_Enumeration_Property
     ("Editor_Desktop_Policy", Editor_Desktop_Policy);

   package Dispatching_Menu_Policy_Properties is new
     Generic_Enumeration_Property
       ("Dispatching_Menu_Policy", Entities.Queries.Dispatching_Menu_Policy);

   package Multi_Language_Builder_Policy_Properties is new
     Generic_Enumeration_Property
       ("Multi_Language_Builder_Policy", Multi_Language_Builder_Policy);

   Preferences_Pages : Preferences_Page_Array_Access;
   --  ??? To be included in the kernel

   Value_Cst : aliased constant String := "value";
   Set_Cmd_Parameters : constant Cst_Argument_List := (1 => Value_Cst'Access);

   procedure Get_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Get preference command handler

   function Get_Index
     (Enum  : Param_Spec_Enum;
      Value : String) return Gint;
   --  Return string representation of value's index in Enum

   function Get_Value
     (Enum  : Param_Spec_Enum;
      Index : Guint) return String;
   --  Return enum value for Index

   type Preferences_Module is new Module_ID_Record with null record;
   overriding procedure Customize
     (Module : access Preferences_Module;
      File   : GNATCOLL.VFS.Virtual_File;
      Node   : Glib.Xml_Int.Node_Ptr;
      Level  : Customization_Level);
   --  Handle GPS customization files for this module

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
     (Enum  : Param_Spec_Enum;
      Index : Guint) return String
   is
      E_Klass : constant Enum_Class := Enumeration (Enum);
      Val     : Enum_Value;
   begin
      Val := Nth_Value (E_Klass, Index);

      if Val = null then
         raise Constraint_Error;
      end if;

      declare
         S : String := Nick (Val);
      begin
         Mixed_Case (S);
         return S;
      end;
   end Get_Value;

   ---------------
   -- Get_Index --
   ---------------

   function Get_Index
     (Enum  : Param_Spec_Enum;
      Value : String) return Gint
   is
      L_Value : constant String     := To_Upper (Value);
      --  Enumeration value are returned all upper case
      E_Klass : constant Enum_Class := Enumeration (Enum);
      Val     : Enum_Value;
      K       : Gint := 0;
   begin
      loop
         Val := Nth_Value (E_Klass, Guint (K));
         exit when Val = null;

         if Nick (Val) = L_Value then
            return K;
         end if;

         K := K + 1;
      end loop;
      raise Constraint_Error;
   end Get_Index;

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
            Pref  : constant String     :=
              Translate (Get_Data (Inst, Class), To_Mapping ("/", "-"));
            Param : constant Param_Spec :=
              Get_Pref_From_Name (Kernel.Preferences, Pref, False);
            Typ   : GType;
         begin
            if Param = null then
               Set_Error_Msg (Data, -"Unknown preference " & Pref);
               return;
            else
               Typ := Value_Type (Param);
            end if;

            if Typ = GType_Int then
               Set_Return_Value
                 (Data, Integer (Get_Pref (Param_Spec_Int (Param))));

            elsif Typ = GType_Boolean then
               Set_Return_Value
                 (Data, (Get_Pref (Param_Spec_Boolean (Param))));

            elsif Typ = GType_String then
               Set_Return_Value
                 (Data, Get_Pref (Param_Spec_String (Param)));

            elsif Typ = Gdk.Color.Gdk_Color_Type then
               Set_Return_Value
                 (Data, To_String (Get_Pref (Param_Spec_Color (Param))));

            elsif Typ = Pango.Font.Get_Type then
               Set_Return_Value
                 (Data, To_String (Get_Pref (Param_Spec_Font (Param))));

            elsif Typ = Gtk.Style.Get_Type then
               Set_Return_Value
                 (Data, Get_Pref (Param_Spec_String (Param)));

            elsif Fundamental (Typ) = GType_Enum then
               Set_Return_Value
                 (Data,
                  Get_Value
                    (Param_Spec_Enum (Param),
                     Guint (Get_Pref (Param_Spec_Enum (Param)))));

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
            Pref  : constant String     :=
              Translate (Get_Data (Inst, Class), To_Mapping ("/", "-"));
            Param : constant Param_Spec :=
              Get_Pref_From_Name (Kernel.Preferences, Pref, False);
            Typ   : GType;
            Done  : Boolean := True;
         begin
            if Param = null then
               Set_Error_Msg (Data, -"Unknown preference " & Pref);
               return;
            else
               Typ := Value_Type (Param);
            end if;

            if Typ = GType_Int then
               Set_Pref
                 (Kernel.Preferences,
                  Param_Spec_Int (Param),
                  Gint (Integer'(Nth_Arg (Data, 2))));

            elsif Typ = GType_String
              or else Typ = Pango.Font.Get_Type
              or else Typ = Gdk_Color_Type
            then
               Set_Pref
                 (Kernel.Preferences,
                  Param_Spec_String (Param), String'(Nth_Arg (Data, 2)));

            elsif Typ = GType_Boolean then
               Set_Pref
                 (Kernel.Preferences, Param_Spec_Boolean (Param),
                  Nth_Arg (Data, 2));

            elsif Typ = Gtk.Style.Get_Type then
               Set_Pref
                 (Kernel.Preferences,
                  Param_Spec_String (Param),
                  String'(Nth_Arg (Data, 2)));

            elsif Fundamental (Typ) = GType_Enum then
               Set_Pref
                 (Kernel.Preferences,
                  Param_Spec_Int (Param),
                  Get_Index
                    (Param_Spec_Enum (Param), String'(Nth_Arg (Data, 2))));

            else
               Done := False;
               Set_Error_Msg (Data, -"Preference not supported");
            end if;

            if Done and then Nth_Arg (Data, 3, True) then
               Save_Preferences
                 (Kernel, Get_Home_Dir (Kernel) & "preferences");
               Run_Hook (Kernel, Preferences_Changed_Hook);
            end if;

         exception
            when E : Invalid_Parameter =>
               Set_Error_Msg (Data, Exception_Message (E));
            when E : others => Trace (Traces.Exception_Handle, E);
         end;

      elsif Command = "create" then
         declare
            Pref  : constant String := Get_Data (Inst, Class);
            Name  : constant String := Translate (Pref, To_Mapping ("/", "-"));
            Nick  : constant String := Nth_Arg (Data, 2);
            Typ   : constant String := Nth_Arg (Data, 3);
            Doc   : constant String := Nth_Arg (Data, 4, "");
            Param : Param_Spec;
         begin
            if Typ = "integer" then
               Param := Gnew_Int
                 (Name    => Name,
                  Nick    => Nick,
                  Blurb   => Doc,
                  Default => Gint (Nth_Arg (Data, 5, 0)),
                  Minimum => Gint (Nth_Arg (Data, 6, Integer'First)),
                  Maximum => Gint (Nth_Arg (Data, 7, Integer'Last)));

            elsif Typ = "boolean" then
               Param := Gnew_Boolean
                 (Name    => Name,
                  Nick    => Nick,
                  Blurb   => Doc,
                  Default => Nth_Arg (Data, 5, True));

            elsif Typ = "string" then
               Param := Gnew_String
                 (Name    => Name,
                  Nick    => Nick,
                  Blurb   => Doc,
                  Default => Nth_Arg (Data, 5, ""));

            elsif Typ = "color" then
               Param := Param_Spec (Gnew_Color
                 (Name    => Name,
                  Nick    => Nick,
                  Blurb   => Doc,
                  Default => Nth_Arg (Data, 5, "black")));

            elsif Typ = "font" then
               Param := Param_Spec (Gnew_Font
                 (Name    => Name,
                  Nick    => Nick,
                  Blurb   => Doc,
                  Default => Nth_Arg (Data, 5, Config.Default_Font)));
            else
               Set_Error_Msg (Data, -"Invalid preference type");
               return;
            end if;

            Register_Property (Kernel.Preferences, Param, Dir_Name (Pref));
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

      Default_Font := Param_Spec_Font (Gnew_Font
        (Name    => "General-Default-Font",
         Default => Config.Default_Font,
         Blurb   => -"The default font used in GPS",
         Nick    => -"Default font"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Default_Font), -"General");

      View_Fixed_Font := Param_Spec_Font (Gnew_Font
        (Name    => "General-Fixed-View-Font",
         Default => "Courier 10",
         Blurb   => -("Fixed pitch (monospace) font used in the various views "
                      & "(Outline View, Clipboard View, Messages, ...)"),
         Nick    => -"Fixed view font"));
      Register_Property
        (Kernel.Preferences, Param_Spec (View_Fixed_Font), -"General");

      GPS.Kernel.Charsets.Register_Preferences (Kernel);

      Default_Widget_Width := Param_Spec_Int (Gnew_Int
        (Name    => "General-Default-Widget-Width",
         Nick    => -"Default width",
         Blurb   => -"Default width for all the newly created windows",
         Minimum => 50,
         Maximum => 2000,
         Default => 200,
         Flags   => Param_Readable));
      Register_Property
        (Kernel.Preferences, Param_Spec (Default_Widget_Width), -"General");

      Default_Widget_Height := Param_Spec_Int (Gnew_Int
        (Name    => "General-Default-Widget-Height",
         Nick    => -"Default height",
         Blurb   => -"Default height for all the newly created windows",
         Minimum => 50,
         Maximum => 2000,
         Default => 200,
         Flags   => Param_Readable));
      Register_Property
        (Kernel.Preferences, Param_Spec (Default_Widget_Height), -"General");

      Use_Native_Dialogs := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "General-Use-Native-Dialogs",
         Nick    => -"Native dialogs",
         Blurb   =>
           -"Use OS native dialogs if enabled, portable dialogs otherwise",
         Default => True,
         Flags   => Param_Readable));
      Register_Property
        (Kernel.Preferences, Param_Spec (Use_Native_Dialogs), -"General");

      Splash_Screen := Param_Spec_Boolean
        (Gnew_Boolean
           (Name    => "General-Splash-Screen",
            Nick    => -"Display splash screen",
            Blurb   =>
              -"Whether a splash screen should be displayed when starting GPS",
         Default => True));
      Register_Property
        (Kernel.Preferences, Param_Spec (Splash_Screen), -"General");

      Display_Welcome := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "General-Display-Welcome",
         Nick    => -"Display welcome window",
         Blurb   => -("Enabled when GPS should display the welcome window"
                      & " for the selection of the project"),
         Default => True));
      Register_Property
        (Kernel.Preferences, Param_Spec (Display_Welcome), -"General");

      Toolbar_Show_Text := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "General-Toolbar-Text",
         Nick    => -"Show text in tool bar",
         Blurb   => -("Enabled if tool bar should show both text and icons,"
                      & " Disabled if it should only show icons"),
         Default => False));
      Register_Property
        (Kernel.Preferences, Param_Spec (Toolbar_Show_Text), -"General");

      Submenu_For_Dispatching_Calls := Param_Spec_Enum
        (Dispatching_Menu_Policy_Properties.Gnew_Enum
           (Name => "Submenu-For-Dispatching",
            Nick => -"Submenu for dispatching calls",
            Blurb => -("If you are using a GNAT version more recent than"
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
            Default => From_Memory));
      Register_Property
           (Kernel.Preferences,
            Param_Spec (Submenu_For_Dispatching_Calls), -"Editor");

      Auto_Save := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "General-Auto-Save",
         Nick    => -"Auto save",
         Blurb   => -("Whether unsaved files/projects should be saved"
                      & " automatically before calling external tools"),
         Default => True));
      Register_Property
        (Kernel.Preferences, Param_Spec (Auto_Save), -"General");

      Save_Desktop_On_Exit := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "General-Save-Desktop-On-Exit",
         Nick    => -"Save project-specific desktop on exit",
         Blurb   => -("Whether the desktop should be saved when exiting GPS."
             & " This only applies to project-specific desktops, never to"
             & " the default desktop"),
         Default => True));
      Register_Property
        (Kernel.Preferences, Param_Spec (Save_Desktop_On_Exit), -"General");

      Save_Editor_Desktop := Param_Spec_Enum
        (Editor_Desktop_Policy_Properties.Gnew_Enum
           (Name    => "General-Editor-Desktop-Policy",
            Nick    => "Save editor in desktop",
            Blurb   => -"When to save source editors in the desktop",
            Default => From_Project));
      Register_Property
        (Kernel.Preferences, Param_Spec (Save_Editor_Desktop), -"General");

      Multi_Language_Build := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "General-Multi-Language-Build",
         Nick    => -"Multi language build",
         Blurb   =>
         -("Whether GPS should build more than just Ada " &
           "sources for projects containing Ada and other (e.g. C) languages"),
         Default => False));
      Register_Property
        (Kernel.Preferences, Param_Spec (Multi_Language_Build), -"General");

      Multi_Language_Builder := Param_Spec_Enum
        (Multi_Language_Builder_Policy_Properties.Gnew_Enum
           (Name    => "General-Multi-Language-Builder",
            Nick    => -"Multi language builder",
            Blurb   =>
            -("Whether GPS should build multi-language projects using " &
              "gprbuild (the new option) or gprmake (the old option)"),
            Default => Gprbuild));
      Register_Property
        (Kernel.Preferences, Param_Spec (Multi_Language_Builder), -"General");

      Auto_Jump_To_First := Param_Spec_Boolean
        (Gnew_Boolean
          (Name    => "Auto-Jump-To-First",
           Default => True,
           Blurb   =>
             -("Whether GPS should automatically jump to the first location"
               & " when entries are added to the Location window (error"
               & " messages, find results, ...)"),
           Nick    => -"Jump to first location"));
      Register_Property
        (Kernel, Param_Spec (Auto_Jump_To_First), -"General");

      Tooltip_Color := Param_Spec_Color (Gnew_Color
        (Name    => "General-Tooltip-Color",
         Default => "#FFFFEE",
         Blurb   => -"Color to use for the tooltips background",
         Nick    => -"Tooltip color",
         Flags   => Param_Readable));
      Register_Property
        (Kernel, Param_Spec (Tooltip_Color), -"General");

      -- MDI --

      MDI_Opaque := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "MDI-Opaque",
         Default => Config.Default_Opaque_MDI,
         Blurb   => -("Whether items will be resized or moved opaquely when"
                      & " not maximized"),
         Nick    => -"Opaque"));
      Register_Property
        (Kernel.Preferences, Param_Spec (MDI_Opaque), -"Windows");

      MDI_Destroy_Floats := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "MDI-Destroy-Floats",
         Default => False,
         Blurb   =>
           -("If disabled, closing the window associated with a floating"
             & " item will put the item back in the main GPS window,"
             & " but will not destroy it. If enabled, the item is"
             & " destroyed"),
         Nick    => -"Destroy floats"));
      Register_Property
        (Kernel.Preferences, Param_Spec (MDI_Destroy_Floats), -"Windows");

      MDI_All_Floating := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "MDI-All-Floating",
         Default => False,
         Blurb   =>
           -("If enabled, all windows will be set as floating, and put"
             & " under control of your window manager. Otherwise, a"
             & " multiple document interface is used."),
         Nick    => -"All floating"));
      Register_Property
        (Kernel.Preferences, Param_Spec (MDI_All_Floating), -"Windows");

      MDI_Float_Short_Title := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "MDI-Float-Short-Title",
         Default => False,
         Blurb   =>
           -("If enabled, all floating windows will have a short title. In"
           & " particular, base file names will be used for editors."),
         Nick    => -"Short titles for floats"));
      Register_Property
        (Kernel.Preferences, Param_Spec (MDI_Float_Short_Title), -"Windows");

      MDI_Background_Color := Param_Spec_Color (Gnew_Color
        (Name    => "MDI-Background-Color",
         Default => "#666666",
         Blurb   => -"Color to use for the background of the MDI",
         Nick    => -"Background color"));
      Register_Property
        (Kernel.Preferences, Param_Spec (MDI_Background_Color), -"Windows");

      MDI_Title_Bar_Color := Param_Spec_Color (Gnew_Color
        (Name    => "MDI-Title-Bar-Color",
         Default => "#AAAAAA",
         Blurb   => -"Color to use for the title bar of unselected items",
         Nick    => -"Title bar color"));
      Register_Property
        (Kernel.Preferences, Param_Spec (MDI_Title_Bar_Color), -"Windows");

      MDI_Focus_Title_Color := Param_Spec_Color (Gnew_Color
        (Name    => "MDI-Focus-Title-Color",
         Default => "#6297C5",
         Blurb   => -"Color to use for the title bar of selected items",
         Nick    => -"Selected title bar color"));
      Register_Property
        (Kernel.Preferences, Param_Spec (MDI_Focus_Title_Color), -"Windows");

      -- Source Editor --

      Strip_Blanks := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "Src-Editor-Strip-Blanks",
         Default => True,
         Blurb   =>
           -"Should the editor remove trailing blanks when saving files",
         Nick    => -"Strip blanks"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Strip_Blanks), -"Editor");

      Line_Terminator := Param_Spec_Enum (Line_Terminators_Properties.Gnew_Enum
        (Name  => "Src-Editor-Line-Terminator",
         Nick  => -"Line terminator",
         Blurb => -"Line terminator style to use when saving files",
         Default => Unchanged));
      Register_Property
        (Kernel.Preferences, Param_Spec (Line_Terminator), -"Editor");

      Display_Line_Numbers := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "Src-Editor-Display-Line_Numbers",
         Default => True,
         Blurb   =>
           -"Whether the line numbers should be displayed in file editors",
         Nick    => -"Display line numbers"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Display_Line_Numbers),
         -"Editor");

      Display_Subprogram_Names := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "Src-Editor-Display-Subprogram_Names",
         Default => True,
         Blurb   =>
           -"Whether the subprogram names should be displayed in status lines",
         Nick    => -"Display subprogram names"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Display_Subprogram_Names),
         -"Editor");

      Display_Tooltip := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "Src-Editor-Display-Tooltip",
         Default => True,
         Blurb   => -"Whether tooltips should be displayed automatically",
         Nick    => -"Tooltips"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Display_Tooltip), -"Editor");

      Tooltip_Timeout := Param_Spec_Int (Gnew_Int
        (Name    => "Src-Editor-Tooltip-Timeout",
         Minimum => 0,
         Maximum => 10000,
         Default => 600,
         Blurb   => -"Time (in milliseconds) before displaying tooltips",
         Nick    => -"Tooltips timeout"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Tooltip_Timeout), -"Editor");

      Highlight_Delimiters := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "Src-Editor-Highlight-Delimiters",
         Default => True,
         Blurb   => -"Whether delimiters should be highlighted: (){}[]",
         Nick    => -"Highlight delimiters"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Highlight_Delimiters),
         -"Editor");

      Periodic_Save := Param_Spec_Int (Gnew_Int
        (Name    => "Src-Editor-Periodic-Save",
         Minimum => 0,
         Maximum => 3600,
         Default => 60,
         Blurb   => -("The period (in seconds) after which a source editor"
                      & " is automatically saved. 0 if none."),
         Nick    => -"Autosave delay"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Periodic_Save), -"Editor");

      Highlight_Column := Param_Spec_Int (Gnew_Int
        (Name    => "Src-Editor-Highlight-Column",
         Minimum => 0,
         Maximum => 255,
         Default => 80,
         Blurb   => -("The right margin to highlight. 0 if none. This value "
                      & "is also used to implement the Edit->Refill command"),
         Nick    => -"Right margin"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Highlight_Column), -"Editor");

      Tab_Width := Param_Spec_Int (Gnew_Int
        (Name    => "Src-Editor-Tab-Width",
         Minimum => 1,
         Maximum => 16,
         Default => 8,
         Blurb   => -"The width of a tabulation character, in characters",
         Nick    => -"Tabulation width",
         Flags   => Param_Readable));
      Register_Property
        (Kernel.Preferences, Param_Spec (Tab_Width), -"Editor");

      Block_Highlighting := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "Src-Editor-Block-Highlighting",
         Default => True,
         Blurb   =>
           -"Should the editor enable block highlighting",
         Nick    => -"Block highlighting"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Block_Highlighting), -"Editor");

      Block_Folding := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "Src-Editor-Block-Folding",
         Default => True,
         Blurb   =>
           -"Should the editor enable block folding",
         Nick    => -"Block folding"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Block_Folding), -"Editor");

      Automatic_Syntax_Check := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "Src-Editor-Automatic-Syntax-Check",
         Default => False,
         Blurb   =>
           -"Enable/Disable automatic syntax check",
         Nick    => -"Automatic syntax check",
         Flags   => Param_Readable));
      Register_Property
        (Kernel.Preferences, Param_Spec (Automatic_Syntax_Check), -"Editor");

      Speed_Column_Policy := Param_Spec_Enum
        (Speed_Column_Policy_Properties.Gnew_Enum
           (Name    => "Src-Editor-Speed-Column-Policy",
            Nick    => -"Speed column policy",
            Blurb   => -"When the speed column should be displayed",
            Default => Automatic));
      Register_Property
        (Kernel.Preferences, Param_Spec (Speed_Column_Policy), -"Editor");

      Default_Style := Gnew_Style
        (Name    => "Src-Editor-Default-Style",
         Nick    => -"Default",
         Blurb   => -"Default style used in the source editors",
         Default_Font => "Courier 10",
         Default_Fg   => "black",
         Default_Bg   => "white");
      Register_Property
        (Kernel.Preferences, Param_Spec (Default_Style),
         -"Editor/Fonts & Colors");

      Keywords_Style := Gnew_Style
        (Name    => "Src-Editor-Keywords-Style",
         Nick    => -"Keywords",
         Blurb   => -"Style to use when displaying keywords",
         Default_Font => "Courier Bold 10",
         Default_Fg   => "black",
         Default_Bg   => "white");
      Register_Property
        (Kernel.Preferences, Param_Spec (Keywords_Style),
         -"Editor/Fonts & Colors");

      Comments_Style := Gnew_Style
        (Name    => "Src-Editor-Comments-Style",
         Nick    => -"Comments",
         Blurb   => -"Style to use when displaying comments",
         Default_Font => "Courier Medium Oblique 10",
         Default_Fg   => "blue",
         Default_Bg   => "white");
      Register_Property
        (Kernel.Preferences, Param_Spec (Comments_Style),
         -"Editor/Fonts & Colors");

      Annotated_Comments_Style := Gnew_Style
        (Name    => "Src-Editor-Annotated-Comments-Style",
         Nick    => -"Annotated Comments",
         Blurb   => -"Style to use when displaying annotated comments",
         Default_Font => "Courier Medium Oblique 10",
         Default_Fg   => "#21A9DE",
         Default_Bg   => "white");
      Register_Property
        (Kernel.Preferences, Param_Spec (Annotated_Comments_Style),
         -"Editor/Fonts & Colors");

      Strings_Style := Gnew_Style
        (Name    => "Src-Editor-Strings-Style",
         Nick    => -"Strings",
         Blurb   => -"Style to use when displaying strings",
         Default_Font => "Courier 10",
         Default_Fg   => "brown",
         Default_Bg   => "white");
      Register_Property
        (Kernel.Preferences, Param_Spec (Strings_Style),
         -"Editor/Fonts & Colors");

      Current_Line_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Src-Editor-Current-Line-Color",
         Default => "#FFC38D",
         Blurb   => -"Color for highlighting the current line",
         Nick    => -"Current line color"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Current_Line_Color),
         -"Editor/Fonts & Colors");

      Current_Block_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Src-Editor-Current-Block-Color",
         Default => "#BBBBFF",
         Blurb   => -"Color for highlighting the current block",
         Nick    => -"Current block color"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Current_Block_Color),
         -"Editor/Fonts & Colors");

      Delimiter_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Src-Editor-Highlight-Delimiters-Color",
         Default => "cyan",
         Blurb   => -"Color for highlighting delimiters",
         Nick    => -"Delimiter highlighting color"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Delimiter_Color),
         -"Editor/Fonts & Colors");

      Search_Results_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Src-Editor-Search_Results-Color",
         Default => "light blue",
         Blurb   => -"Color for highlighting search results",
         Nick    => -"Search results highlighting"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Search_Results_Color),
         -"Editor/Fonts & Colors");

      -- Browsers --

      Browsers_Bg_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Browsers-Bg-Color",
         Default => "#BBBBBB",
         Blurb   => -"Color used to draw the background of the browsers",
         Nick    => -"Background color"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Browsers_Bg_Color),
         -"Browsers");

      Browsers_Bg_Image := Param_Spec_String (Gnew_String
        (Name    => "Browsers-Bg-Image",
         Nick    => -"Background image",
         Flags   => Param_Readable,
         Blurb   =>
           -("Image to draw in the background of browsers. If left empty,"
             & " no image is drawn. Using a large image will slow down"
             & " performances"),
         Default => ""));
      Register_Property
        (Kernel.Preferences, Param_Spec (Browsers_Bg_Image),
         -"Browsers");

      Browsers_Draw_Grid := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "Browsers-Draw-Grid",
         Default => True,
         Blurb   => -"Whether a grid should be displayed in the browsers",
         Flags   => Param_Readable,
         Nick    => -"Draw grid"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Browsers_Draw_Grid),
         -"Browsers");

      Browsers_Hyper_Link_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Browsers-Hyper-Link-Color",
         Default => "#0000FF",
         Blurb   => -"Color used to draw the hyper links in the items",
         Nick    => -"Hyper link color"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Browsers_Hyper_Link_Color),
         -"Browsers");

      Selected_Link_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Browsers-Selected-Link-Color",
         Default => "#FF0000",
         Blurb   => -"Color to use for links between selected items",
         Nick    => -"Selected link color"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Selected_Link_Color),
         -"Browsers");

      Unselected_Link_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Browsers-Unselected-Link-Color",
         Default => "#000000",
         Blurb   => -"Color to use for links between unselected items",
         Nick    => -"Default link color"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Unselected_Link_Color),
         -"Browsers");

      Parent_Linked_Item_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Browsers-Linked-Item-Color",
         Default => "#AAAAAA",
         Blurb   => -("Color to use for the background of the items linked"
                      & " to the selected item"),
         Nick    => -"Ancestor items color"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Parent_Linked_Item_Color),
         -"Browsers");

      Child_Linked_Item_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Browsers-Child-Linked-Item-Color",
         Default => "#DDDDDD",
         Blurb   => -("Color to use for the background of the items linked"
                      & " from the selected item"),
         Nick    => -"Offspring items color"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Child_Linked_Item_Color),
         -"Browsers");

      Selected_Item_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Browsers-Selected-Item-Color",
         Default => "#888888",
         Blurb   => -"Color to use to draw the selected item",
         Nick    => -"Selected item color"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Selected_Item_Color), -"Browsers");

      Title_Color := Param_Spec_Color (Gnew_Color
        (Name     => "Browsers-Title-Color",
         Nick     => -"Title background",
         Blurb    => -"Color used for the background of the title",
         Flags    => Param_Readable,
         Default  => "#BEBEBE"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Title_Color), -"Browsers");

      Browsers_Vertical_Layout := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "Browsers-Vertical-Layout",
         Default => False,
         Blurb   => -("If enabled, the boxes in the browsers will be"
           & " organized into layers displayed one below the other. The"
           & " graph will tend to grow vertically when you open new boxes."
           & " This setting does not affect the entities browser, though,"
           & " where the layout is always vertical."),
         Nick    => -"Vertical layout"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Browsers_Vertical_Layout),
         -"Browsers");

      -- Diff_Utils --

      Diff_Cmd := Param_Spec_String (Gnew_String
        (Name  => "Diff-Utils-Diff",
         Nick  => -"Diff command",
         Blurb => -("Command used to compute differences between two files."
                    & " Arguments can also be specified"),
         Default => Config.Default_Diff_Cmd));
      Register_Property
        (Kernel.Preferences, Param_Spec (Diff_Cmd), -"Visual diff");

      Patch_Cmd := Param_Spec_String (Gnew_String
        (Name    => "Diff-Utils-Patch",
         Nick    => -"Patch command",
         Blurb   =>
           -"Command used to apply a patch. Arguments can also be specified",
         Default => Config.Default_Patch_Cmd));
      Register_Property
        (Kernel.Preferences, Param_Spec (Patch_Cmd), -"Visual diff");

      Old_Vdiff := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "Diff-Utils-Old-Vdiff",
         Nick    => -"Use old diff (requires restart)",
         Blurb   => -("Use the old version of visual differences."
                      & " Changing this parameter requires restarting GPS."),
         Default => False));
      Register_Property
        (Kernel.Preferences, Param_Spec (Old_Vdiff), -"Visual diff");
      -- Messages --

      Message_Highlight := Param_Spec_Color (Gnew_Color
        (Name    => "Messages-Highlight-Color",
         Nick    => -"Color highlighting",
         Blurb   => -"Color used to highlight text in the messages window",
         Default => "#FF0000"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Message_Highlight), -"Messages");

      Error_Src_Highlight := Param_Spec_Color (Gnew_Color
        (Name    => "Errors-Src-Highlight-Color",
         Nick    => -"Errors highlighting",
         Blurb   => -"Color used to highlight errors in the source editors",
         Default => "#FF6D6D"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Error_Src_Highlight), -"Messages");

      Warning_Src_Highlight := Param_Spec_Color (Gnew_Color
        (Name    => "Warnings-Src-Highlight-Color",
         Nick    => -"Warnings highlighting",
         Blurb   => -"Color used to highlight warnings in the source editors",
         Default => "#FFB46D"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Warning_Src_Highlight), -"Messages");

      Style_Src_Highlight := Param_Spec_Color (Gnew_Color
        (Name    => "Style-Src-Highlight-Color",
         Nick    => -"Style errors highlighting",
         Blurb   =>
           -"Color used to highlight style errors in the source editors",
         Default => "#EEFF6D"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Style_Src_Highlight), -"Messages");

      Search_Src_Highlight := Param_Spec_Color (Gnew_Color
        (Name    => "Search-Src-Highlight-Color",
         Nick    => -"Search highlighting",
         Blurb   =>
             -"Color used to highlight search results in the source editors",
         Default => "#A2B6FF"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Search_Src_Highlight), -"Messages");

      File_Pattern := Param_Spec_String
        (Gnew_String
           (Name  => "Messages-File-Regpat",
            Nick  => -"File pattern",
            Blurb =>
              -"Pattern used to detect file locations (e.g error messages)",
            Default =>
              "^([^:]:?[^:]*):(\d+):((\d+):)? ((warning)?(\(style)?.*)"));
      Register_Property
        (Kernel.Preferences, Param_Spec (File_Pattern), -"Messages");

      File_Pattern_Index := Param_Spec_Int (Gnew_Int
        (Name    => "Messages-File-Regexp-Index",
         Minimum => 1,
         Maximum => 99,
         Default => 1,
         Blurb   => -"Index of filename in the pattern",
         Nick    => -"File index"));
      Register_Property
        (Kernel.Preferences, Param_Spec (File_Pattern_Index), -"Messages");

      Line_Pattern_Index := Param_Spec_Int (Gnew_Int
        (Name    => "Messages-Line-Regexp-Index",
         Minimum => 1,
         Maximum => 99,
         Default => 2,
         Blurb   => -"Index of line number in the pattern",
         Nick    => -"Line index"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Line_Pattern_Index), -"Messages");

      Column_Pattern_Index := Param_Spec_Int (Gnew_Int
        (Name    => "Messages-Column-Regexp-Index",
         Minimum => 0,
         Maximum => 99,
         Default => 4,
         Blurb   => -"Index of column number in the pattern, 0 if none",
         Nick    => -"Column index"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Column_Pattern_Index), -"Messages");

      Message_Pattern_Index := Param_Spec_Int (Gnew_Int
        (Name    => "Messages-Message-Regexp-Index",
         Minimum => 0,
         Maximum => 99,
         Default => 5,
         Blurb   => -"Index of message in the pattern, 0 if none",
         Nick    => -"Message index"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Message_Pattern_Index), -"Messages");

      Warning_Pattern_Index := Param_Spec_Int (Gnew_Int
        (Name    => "Messages-Warning-Regexp-Index",
         Minimum => 0,
         Maximum => 99,
         Default => 6,
         Blurb   => -"Index of warning indication in the pattern, 0 if none",
         Nick    => -"Warning index"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Warning_Pattern_Index), -"Messages");

      Style_Pattern_Index := Param_Spec_Int (Gnew_Int
        (Name    => "Messages-Style-Regexp-Index",
         Minimum => 0,
         Maximum => 99,
         Default => 7,
         Blurb   => -"Index of style indication in the pattern, 0 if none",
         Nick    => -"Style index"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Style_Pattern_Index), -"Messages");

      Secondary_File_Pattern := Param_Spec_String
        (Gnew_String
           (Name  => "Messages-Secondary-File-Regpat",
            Nick  => -"Secondary File pattern",
            Blurb =>
            -"Pattern used to detect secondary file locations in messages",
            Default =>
              "([^: ]+):(\d+)(:(\d+):)?"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Secondary_File_Pattern), -"Messages");

      Secondary_File_Pattern_Index := Param_Spec_Int (Gnew_Int
        (Name    => "Messages-Secondary-File-Regexp-Index",
         Minimum => 1,
         Maximum => 99,
         Default => 1,
         Blurb   => -"Index of secondary filename in the pattern",
         Nick    => -"Secondary File index"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Secondary_File_Pattern_Index),
         -"Messages");

      Secondary_Line_Pattern_Index := Param_Spec_Int (Gnew_Int
        (Name    => "Messages-Secondary-Line-Regexp-Index",
         Minimum => 1,
         Maximum => 99,
         Default => 2,
         Blurb   => -"Index of secondary location line number in the pattern",
         Nick    => -"Secondary Line index"));
      Register_Property
        (Kernel.Preferences,
         Param_Spec (Secondary_Line_Pattern_Index), -"Messages");

      Secondary_Column_Pattern_Index := Param_Spec_Int (Gnew_Int
        (Name    => "Messages-Secondary-Column-Regexp-Index",
         Minimum => 0,
         Maximum => 99,
         Default => 3,
         Blurb   =>
         -"Index of secondary column number in the pattern, 0 if none",
         Nick    => -"Secondary Column index"));
      Register_Property
        (Kernel.Preferences,
         Param_Spec (Secondary_Column_Pattern_Index), -"Messages");

      -- Project Editor --

      Default_Switches_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Prj-Editor-Default-Switches-Color",
         Default => "#777777",
         Blurb   => -("Color to use when displaying switches that are set"
                      & " as default for all the files in the project"),
         Nick    => -"Default switches color",
         Flags   => Param_Readable));
      Register_Property
        (Kernel.Preferences, Param_Spec (Default_Switches_Color), -"Project");

      Switches_Editor_Title_Font := Param_Spec_Font (Gnew_Font
        (Name    => "Prj-Editor-Title-Font",
         Default => "sans bold oblique 14",
         Blurb   => -"Font to use for the switches editor dialog",
         Nick    => -"Title font",
         Flags   => Param_Readable));
      Register_Property
        (Kernel.Preferences, Param_Spec (Switches_Editor_Title_Font),
         -"Project");

      Variable_Ref_Background := Param_Spec_Color (Gnew_Color
        (Name    => "Prj-Editor-Var-Ref-Bg",
         Default => "#AAAAAA",
         Blurb   => -("Color to use for the background of variable"
                      & " references in the value editor"),
         Nick    => -"Variable reference color",
         Flags   => Param_Readable));
      Register_Property
        (Kernel.Preferences, Param_Spec (Variable_Ref_Background), -"Project");

      Invalid_Variable_Ref_Background := Param_Spec_Color (Gnew_Color
        (Name    => "Prj-Editor-Invalid-Var-Ref-Bg",
         Default => "#AA0000",
         Blurb   => -("Color to use for the foreground of invalid variable"
                      & " references"),
         Nick    => -"Invalid references color",
         Flags   => Param_Readable));
      Register_Property
        (Kernel.Preferences, Param_Spec (Invalid_Variable_Ref_Background),
         -"Project");

      Generate_Relative_Paths := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "Prj-Editor-Generate-Relative-Paths",
         Default => True,
         Blurb   => -("If enabled, use relative paths when the projects are " &
                      "modified, use absolute paths otherwise"),
         Nick    => -"Relative project paths"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Generate_Relative_Paths), -"Project");

      Trusted_Mode := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "Prj-Editor-Trusted-Mode",
         Default => True,
         Blurb   => -("Whether a fast algorithm should be used to load Ada"
                      & " projects. This algorithm assumes the following "
                      & "about your project:" & ASCII.LF
                      & "   - no symbolic links are used to point to other"
                      & " files in the project" & ASCII.LF
                      & "   - no directory has a name which is a valid source"
                      & " file name according to the naming scheme"),
         Nick    => -"Fast Project Loading"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Trusted_Mode), -"Project");

      Automatic_Xrefs_Load := Param_Spec_Boolean
        (Gnew_Boolean
           (Name    => "Load-Xref-Info-At-Startup",
            Default => False,
            Blurb   => -("Whether to load the Xref info in memory whenever a"
              & " new project is loaded into memory, or a new file is"
              & " compiled."),
            Nick    => -"Load Xref info automatically"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Automatic_Xrefs_Load), -"Project");

      Hidden_Directories_Pattern := Param_Spec_String
        (Gnew_String
           (Name  => "Project-Hidden-Directories-Regexp",
            Nick  => -"Hidden directories pattern",
            Blurb =>
              -"Directories matching this pattern are removed from the project"
              & " view. This preference is really OS dependent, for"
              & " example on UNIX based systems, files and directories"
              & " starting with a dot are considered as hidden. This regular"
              & " expression is also used to remove VCS specific directories"
              & " like CVS.",
            Default =>
              "^((\..+)|CVS)$"));
      Register_Property
        (Kernel.Preferences,
         Param_Spec (Hidden_Directories_Pattern), -"Project");

      -- Wizards --

      Wizard_Title_Font := Param_Spec_Font (Gnew_Font
        (Name    => "Wizard-Title-Font",
         Default => "sans bold oblique 10",
         Blurb   => -"Font to use for the title of the pages in the wizard",
         Nick    => -"Title font",
         Flags   => Param_Readable));
      Register_Property
        (Kernel.Preferences, Param_Spec (Wizard_Title_Font),
         -"Project wizard");

      -- VCS --

      Hide_Up_To_Date := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "VCS-Hide-Up-To-Date",
         Default => False,
         Flags   => Param_Readable,
         Blurb   => -"Whether up to date files should be hidden by default",
         Nick    => -"Hide up-to-date files"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Hide_Up_To_Date), -"VCS");

      Hide_Not_Registered := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "VCS-Hide-Not-Registered",
         Default => False,
         Flags   => Param_Readable,
         Blurb   => -"Whether unregistered files should be hidden by default",
         Nick    => -"Hide non registered files"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Hide_Not_Registered), -"VCS");

      -- CVS --

      CVS_Command := Param_Spec_String (Gnew_String
        (Name    => "CVS-Command",
         Default => "cvs",
         Blurb   => -"General CVS command",
         Flags   => Param_Readable,
         Nick    => -"CVS command"));
      Register_Property
        (Kernel.Preferences, Param_Spec (CVS_Command), -"VCS:CVS");

      -- ClearCase --

      ClearCase_Command := Param_Spec_String (Gnew_String
        (Name    => "ClearCase-Command",
         Default => "cleartool",
         Blurb   => -"General ClearCase command",
         Flags   => Param_Readable,
         Nick    => -"ClearCase command"));
      Register_Property
        (Kernel.Preferences, Param_Spec (ClearCase_Command), -"VCS:ClearCase");

      -- External Commands --

      List_Processes := Param_Spec_String (Gnew_String
        (Name     => "Helpers-List-Processes",
         Nick     => -"List processes",
         Blurb    =>
         -("Command used to list processes running on the machine." & ASCII.LF
           & "On Unix machines, you should surround the command with"
           & " triple-quotes similar to what python uses, and execute the"
           & " command through sh -c so that environment variables and"
           & " output redirection are properly executed"),
         Default  => Config.Default_Ps));
      Register_Property
        (Kernel.Preferences, Param_Spec (List_Processes), -"External Command");

      Execute_Command := Param_Spec_String (Gnew_String
         (Name    => "Helpers-Execute-Command",
          Nick    => -"Execute command",
          Blurb   => -"Program used to execute commands externally",
          Default => Config.Exec_Command));
      Register_Property
        (Kernel.Preferences,
         Param_Spec (Execute_Command), -"External Command");

      if Config.Host /= Config.Windows then
         --  Preference not used under Windows

         Html_Browser := Param_Spec_String (Gnew_String
           (Name  => "Helpers-HTML-Browser",
            Nick  => -"HTML browser",
            Blurb =>
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
            Default => ""));
         Register_Property
           (Kernel.Preferences,
            Param_Spec (Html_Browser), -"External Command");
      end if;

      Print_Command := Param_Spec_String (Gnew_String
         (Name    => "Helpers-Print-Command",
          Nick    => -"Print command",
          Blurb   => -("Program used to print files. No value means use " &
                       "the built-in printing capability (available under " &
                       "Windows only)"),
          Default => Config.Default_Print_Cmd));
      Register_Property
        (Kernel.Preferences, Param_Spec (Print_Command), -"External Command");

      Max_Output_Length := Param_Spec_Int (Gnew_Int
         (Name    => "Max-Output-Length",
          Nick    => -"Maximum output length",
          Blurb   => -("Maximum output length of output taken into account by"
            & "GPS, in bytes."),
          Minimum => 1_000,
          Maximum => Gint'Last,
          Default => 1_000_000,
          Flags   => Param_Readable));
      Register_Property
        (Kernel.Preferences, Param_Spec (Max_Output_Length), "");
   end Register_Global_Preferences;

   ---------------
   -- Customize --
   ---------------

   overriding procedure Customize
     (Module : access Preferences_Module;
      File   : GNATCOLL.VFS.Virtual_File;
      Node   : Glib.Xml_Int.Node_Ptr;
      Level  : Customization_Level)
   is
      pragma Unreferenced (File, Level);
      Kernel : constant Kernel_Handle := Get_Kernel (Module.all);
      Child : Glib.Xml_Int.Node_Ptr;
      Child_Count : Natural;
      Flags : Param_Flags;
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
            Min   : constant String := Get_Attribute (Node, "minimum", "0");
            Max   : constant String := Get_Attribute (Node, "maximum", "10");
            Pspec   : Param_Spec;
            Minimum, Maximum, Def : Gint;
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

            Flags := Param_Readable or Param_Writable;
            if Page = "" then
               Flags := Param_Readable;
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
               Pspec := Gnew_Boolean
                 (Name    => Name,
                  Nick    => Label,
                  Blurb   => Tooltip,
                  Default => Bool_Def);

            elsif Typ = "integer" then
               Minimum := Gint'Value (Min);
               Maximum := Gint'Value (Max);
               if Default = "" then
                  Def := 0;
               else
                  Def     := Gint'Value (Default);
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

               Pspec := Gnew_Int
                 (Name    => Name,
                  Nick    => Label,
                  Blurb   => Tooltip,
                  Minimum => Minimum,
                  Maximum => Maximum,
                  Default => Def,
                  Flags   => Flags);
            elsif Typ = "string" then
               Pspec := Gnew_String
                 (Name    => Name,
                  Nick    => Label,
                  Blurb   => Tooltip,
                  Default => Default,
                  Flags   => Flags);
            elsif Typ = "color" then
               Pspec := Param_Spec (Gnew_Color
                                      (Name    => Name,
                                       Nick    => Label,
                                       Blurb   => Tooltip,
                                       Default => Default,
                                       Flags   => Flags));
            elsif Typ = "font" then
               Pspec := Param_Spec (Gnew_Font
                                      (Name    => Name,
                                       Nick    => Label,
                                       Blurb   => Tooltip,
                                       Default => Default,
                                       Flags   => Flags));

            elsif Typ = "choices" then
               Child := Node.Child;
               Child_Count := 0;
               while Child /= null loop
                  Child_Count := Child_Count + 1;
                  Child := Child.Next;
               end loop;

               declare
                  Typ : GType;
                  Val : chars_ptr_array (1 .. size_t (Child_Count));
               begin
                  Child := Node.Child;
                  Child_Count := 1;
                  while Child /= null loop
                     Val (size_t (Child_Count)) := New_String
                       (Child.Value.all);
                     Child_Count := Child_Count + 1;
                     Child := Child.Next;
                  end loop;

                  Typ := Register_Static_Enum (Name, Val);

                  for V in Val'Range loop
                     Free (Val (V));
                  end loop;

                  if Default = "" then
                     Def := 1;
                  else
                     Def := Gint'Value (Default);
                  end if;

                  Pspec := Gnew_Enum
                    (Name      => Name,
                     Nick      => Label,
                     Blurb     => Tooltip,
                     Enum_Type => Typ,
                     Default   => Def,
                     Flags     => Flags);
               end;

            else
               Insert
                 (Kernel,
                  -"Invalid ""type"" attribute for <preference>",
                  Mode => Error);
               return;
            end if;

            Register_Property (Kernel.Preferences, Pspec, Page);

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
        (Module                => Module,
         Kernel                => Kernel,
         Module_Name           => "Preferences");
      Register_Command
        (Kernel, Constructor_Method,
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => Pref_Class,
         Handler       => Get_Command_Handler'Access);
      Register_Command
        (Kernel, "get",
         Class         => Pref_Class,
         Handler       => Get_Command_Handler'Access);
      Register_Command
        (Kernel, "set",
         Minimum_Args  => 1,
         Maximum_Args  => 2,
         Class         => Pref_Class,
         Handler       => Get_Command_Handler'Access);
      Register_Command
        (Kernel, "create",
         Minimum_Args  => 2,
         Maximum_Args  => Integer'Last,
         Class         => Pref_Class,
         Handler       => Get_Command_Handler'Access);
   end Register_Module;

   ----------------------
   -- Edit_Preferences --
   ----------------------

   procedure Edit_Preferences (Kernel : access Kernel_Handle_Record'Class) is
      procedure On_Changed (Manager : access Preferences_Manager_Record'Class);
      --  Called when the preferences have been changed.

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
        (Manager           => Kernel.Preferences,
         Parent            => Get_Main_Window (Kernel),
         On_Changed        => On_Changed'Unrestricted_Access,
         Custom_Pages      => Preferences_Pages.all);

      Save_Preferences (Kernel, Get_Home_Dir (Kernel) & "preferences");
   end Edit_Preferences;

   ----------------------
   -- Save_Preferences --
   ----------------------

   procedure Save_Preferences
     (Kernel : access Kernel_Handle_Record'Class; File_Name : String)
   is
      Success : Boolean;

   begin
      Trace (Me, "Saving preferences in " & File_Name);
      Save_Preferences (Kernel.Preferences, File_Name, Success);

      if not Success then
         Report_Preference_File_Error (Kernel, File_Name);
      end if;
   end Save_Preferences;

   --------------
   -- Set_Pref --
   --------------

   procedure Set_Pref
     (Kernel : access Kernel_Handle_Record'Class;
      Pref   : Param_Spec_Boolean;
      Value  : Boolean) is
   begin
      Set_Pref (Kernel.Preferences, Pref, Value);
   end Set_Pref;

   --------------
   -- Set_Pref --
   --------------

   procedure Set_Pref
     (Kernel : access Kernel_Handle_Record'Class;
      Pref   : Param_Spec_Int;
      Value  : Glib.Gint) is
   begin
      Set_Pref (Kernel.Preferences, Pref, Value);
   end Set_Pref;

   --------------
   -- Set_Pref --
   --------------

   procedure Set_Pref
     (Kernel : access Kernel_Handle_Record'Class;
      Pref   : Param_Spec_String;
      Value  : String) is
   begin
      Set_Pref (Kernel.Preferences, Pref, Value);
   end Set_Pref;

   -----------------------
   -- Register_Property --
   -----------------------

   procedure Register_Property
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Param  : Glib.Param_Spec;
      Page   : String) is
   begin
      Register_Property (Kernel.Preferences, Param, Page);
   end Register_Property;

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
