-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2002-2003                       --
--                            ACT-Europe                             --
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

with Glib;                      use Glib;
with Glib.Object;               use Glib.Object;
with Glide_Intl;                use Glide_Intl;
with Glide_Kernel.Console;      use Glide_Kernel.Console;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with Glide_Kernel.Preferences;  use Glide_Kernel.Preferences;
with Glide_Kernel.Project;      use Glide_Kernel.Project;
with Glide_Kernel;              use Glide_Kernel;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Check_Button;          use Gtk.Check_Button;
with Gtkada.Dialogs;            use Gtkada.Dialogs;
with Gtkada.File_Selector;      use Gtkada.File_Selector;
with Gtkada.Handlers;           use Gtkada.Handlers;
with Gtk.Combo;                 use Gtk.Combo;
with Gtk.Dialog;                use Gtk.Dialog;
with Gtk.Button;                use Gtk.Button;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Event_Box;             use Gtk.Event_Box;
with Gtk.Frame;                 use Gtk.Frame;
with Gtk.GEntry;                use Gtk.GEntry;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Notebook;              use Gtk.Notebook;
with Gtk.Object;                use Gtk.Object;
with Gtk.Size_Group;            use Gtk.Size_Group;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Tooltips;              use Gtk.Tooltips;
with Gtk.Widget;                use Gtk.Widget;
with Projects.Editor;           use Projects, Projects.Editor;
with Projects.Registry;         use Projects.Registry;
with String_Utils;              use String_Utils;
with Basic_Types;               use Basic_Types;
with Language_Handlers;         use Language_Handlers;
with Ada.Unchecked_Deallocation;
with Scenario_Selectors;        use Scenario_Selectors;
with Traces;                    use Traces;
with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Exceptions;            use Ada.Exceptions;
with Project_Viewers;           use Project_Viewers;
with Languages_Lists;           use Languages_Lists;

package body Project_Properties is
   use Widget_List;

   Me : constant Debug_Handle := Create ("Project_Properties");

   type Widget_Array is array (Natural range <>) of Gtk_Widget;
   type Widget_Array_Access is access Widget_Array;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Widget_Array, Widget_Array_Access);

   type Properties_Editor_Record is new Gtk.Dialog.Gtk_Dialog_Record with
   record
      Name               : Gtk.GEntry.Gtk_Entry;
      Path               : Gtk.GEntry.Gtk_Entry;
      Debugger           : Gtk.GEntry.Gtk_Entry;
      Global_Pragmas     : Gtk.GEntry.Gtk_Entry;
      Local_Pragmas      : Gtk.GEntry.Gtk_Entry;
      Gnatls             : Gtk.GEntry.Gtk_Entry;
      Compilers          : Widget_Array_Access;
      Languages          : Languages_Lists.Languages_List;
      Use_Relative_Paths : Gtk.Check_Button.Gtk_Check_Button;
      Tools_Host         : Gtk.GEntry.Gtk_Entry;
      Program_Host       : Gtk.GEntry.Gtk_Entry;
      Protocol           : Gtk.GEntry.Gtk_Entry;

      Selector     : Scenario_Selector;
      Prj_Selector : Project_Selector;

      Pages              : Widget_Array_Access;
      --  The pages that have been registered.

      Project            : Project_Type;
      Kernel             : Kernel_Handle;
   end record;
   type Properties_Editor is access all Properties_Editor_Record'Class;

   procedure Gtk_New
     (Editor  : out Properties_Editor;
      Project : Project_Type;
      Kernel  : access Kernel_Handle_Record'Class);
   --  Create a new properties editor

   procedure Initialize
     (Editor  : access Properties_Editor_Record'Class;
      Project : Project_Type;
      Kernel  : access Kernel_Handle_Record'Class);
   --  Internal initialization function

   procedure Command_Set_Sensitive
     (Check : access Glib.Object.GObject_Record'Class;
      Ent   : GObject);
   --  Set Ent to sensitive or insensitive state, depending on whether Check is
   --  active or not.

   procedure Destroyed (Editor : access Gtk_Widget_Record'Class);
   --  Called when the editor is destroyed

   function Create_General_Page
     (Editor  : access Properties_Editor_Record'Class;
      Project : Project_Type;
      Kernel  : access Kernel_Handle_Record'Class)
      return Gtk.Widget.Gtk_Widget;
   --  Create the "General" page for the project properties

   procedure Switch_Page
     (Notebook : access GObject_Record'Class; Editor : GObject);
   --  Called when a new page is selected in the notebook

   function Paths_Are_Relative
     (Kernel : access Kernel_Handle_Record'Class; Project : Project_Type)
      return Boolean;
   --  Return True if the paths in the project should be relative paths

   type Project_Edition_Type is (Do_Not_Edit,
                                 Edit_File,
                                 Edit_Properties);

   function Warning_On_View_Incomplete
     (Kernel  : access Kernel_Handle_Record'Class;
      Project : Project_Type)
      return Project_Edition_Type;
   --  Display a warning dialog to indicate that the current view is
   --  incomplete, and it might be dangereous to edit the properties.

   ------------------------
   -- Paths_Are_Relative --
   ------------------------

   function Paths_Are_Relative
     (Kernel : access Kernel_Handle_Record'Class; Project : Project_Type)
      return Boolean
   is
   begin
      case Get_Paths_Type (Project) is
         when Relative  => return True;
         when Absolute  => return False;
         when From_Pref => return Get_Pref (Kernel, Generate_Relative_Paths);
      end case;
   end Paths_Are_Relative;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Editor  : out Properties_Editor;
      Project : Project_Type;
      Kernel  : access Kernel_Handle_Record'Class) is
   begin
      Editor := new Properties_Editor_Record;
      Initialize (Editor, Project, Kernel);
   end Gtk_New;

   ---------------
   -- Destroyed --
   ---------------

   procedure Destroyed (Editor : access Gtk_Widget_Record'Class) is
      E : Properties_Editor := Properties_Editor (Editor);
   begin
      Unchecked_Free (E.Compilers);
      Unchecked_Free (E.Pages);
   end Destroyed;

   ---------------------------
   -- Command_Set_Sensitive --
   ---------------------------

   procedure Command_Set_Sensitive
     (Check : access Glib.Object.GObject_Record'Class;
      Ent   : GObject) is
   begin
      Set_Sensitive (Gtk_Widget (Ent), Get_Active (Gtk_Check_Button (Check)));
   end Command_Set_Sensitive;

   -------------------------
   -- Create_General_Page --
   -------------------------

   function Create_General_Page
     (Editor  : access Properties_Editor_Record'Class;
      Project : Project_Type;
      Kernel  : access Kernel_Handle_Record'Class)
      return Gtk.Widget.Gtk_Widget
   is
      Button2      : Gtk_Button;
      Label        : Gtk_Label;
      Ent     : Gtk_GEntry;
      Combo        : Gtk_Combo;
      Items        : Gtk.Enums.String_List.Glist;
      Frame        : Gtk_Frame;
      Group        : Gtk_Size_Group;
      Vbox, Box, Hbox : Gtk_Box;
      Event        : Gtk_Event_Box;
      Ada_Check    : Gtk_Check_Button;
      Languages : Argument_List := Known_Languages
        (Get_Language_Handler (Kernel));
      Project_Languages : Argument_List :=  Get_Languages (Project);

      use Gtk.Enums.String_List;

   begin
      Gtk_New_Vbox (Vbox, Homogeneous => False);

      Gtk_New (Group, Both);

      --  Name and location frame

      Gtk_New (Frame, -"Name & Location");
      Set_Border_Width (Frame, 5);
      Pack_Start (Vbox, Frame, Expand => False);

      Gtk_New_Vbox (Box, Homogeneous => True);
      Add (Frame, Box);

      Gtk_New_Hbox (Hbox, Homogeneous => False);
      Pack_Start (Box, Hbox);

      --  Name

      Gtk_New (Event);
      Pack_Start (Hbox, Event, Expand => False);
      Gtk_New (Label, -"Name:");
      Set_Alignment (Label, 0.0, 0.5);
      Add (Event, Label);
      Add_Widget (Group, Label);
      Set_Tip (Get_Tooltips (Kernel), Event,
               (-"Name of the project. ") &
               (-"Only applies to the project you selected initially"));

      Gtk_New (Editor.Name);
      Set_Width_Chars (Editor.Name, 0);
      Set_Text (Editor.Name, Project_Name (Project));
      Pack_Start (Hbox, Editor.Name, Expand => True);

      Gtk_New_Hbox (Hbox, Homogeneous => False);
      Pack_Start (Box, Hbox);

      --  Path

      Gtk_New (Event);
      Pack_Start (Hbox, Event, Expand => False);
      Gtk_New (Label, -"Path:");
      Set_Alignment (Label, 0.0, 0.5);
      Add (Event, Label);
      Add_Widget (Group, Label);
      Set_Tip (Get_Tooltips (Kernel), Event,
               -("Directory containing the project file. Changing this field"
                 & " will move the project file. This field only applies to"
                 & " the project you selected initially"));

      Gtk_New (Editor.Path);
      Set_Width_Chars (Editor.Path, 0);
      Set_Text (Editor.Path, Project_Directory (Project));
      Pack_Start (Hbox, Editor.Path, Expand => True);

      Gtk_New (Button2, -"Browse");
      Pack_Start (Hbox, Button2, Expand => False);
      Widget_Callback.Object_Connect
        (Button2, "clicked",
         Widget_Callback.To_Marshaller (Browse_Location'Access),
         Slot_Object => Editor.Path);

      Gtk_New (Editor.Use_Relative_Paths, -"Paths should be relative paths");
      Set_Active
        (Editor.Use_Relative_Paths, Paths_Are_Relative (Kernel, Project));
      Pack_Start (Box, Editor.Use_Relative_Paths);
      Set_Tip (Get_Tooltips (Kernel), Editor.Use_Relative_Paths,
               -("If this field is activated, then all the path information in"
                 & " the project (source and build directories, dependencies"
                 & " between projects,...) will be stored as paths relative"
                 & " to the location of the project file. It will thus be"
                 & " easier to move the project file to another directory"));

      --  Languages frame

      Gtk_New (Editor.Languages, Kernel, Project);
      Pack_Start (Vbox, Editor.Languages, Expand => False);

      --  Tools frame

      Gtk_New (Frame, -"Tools");
      Set_Border_Width (Frame, 5);
      Pack_Start (Vbox, Frame, Expand => False);

      Gtk_New_Vbox (Box, Homogeneous => True);
      Add (Frame, Box);

      Editor.Compilers := new Widget_Array (Languages'Range);

      for L in Languages'Range loop
         Gtk_New_Hbox (Hbox, Homogeneous => False);
         Pack_Start (Box, Hbox);

         declare
            S : String := Languages (L).all;
         begin
            Mixed_Case (S);
            Gtk_New (Event);
            Pack_Start (Hbox, Event, Expand => False);

            Gtk_New (Label, S & ' ' & (-"compiler:"));
            Set_Alignment (Label, 0.0, 0.5);
            Add (Event, Label);
            Add_Widget (Group, Label);
            Set_Tip (Get_Tooltips (Kernel), Event,
                     -"Name of the compiler to use for the language " & S);
         end;

         if To_Lower (Languages (L).all) = "ada" then
            Gtk_New (Combo);
            Set_Width_Chars (Get_Entry (Combo), 0);
            Pack_Start (Hbox, Combo);
            Append (Items, "gnatmake");
            Append (Items, "powerpc-wrs-vxworks-gnatmake");
            Append (Items, "powerpc-wrs-vxworksae-gnatmake");
            Append (Items, "powerpc-elf-gnatmake");
            Append (Items, "i386-wrs-vxworks-gnatmake");
            Append (Items, "m68k-wrs-vxworks-gnatmake");
            Append (Items, "mips-wrs-vxworks-gnatmake");
            Append (Items, "sparc-wrs-vxworks-gnatmake");
            Append (Items, "sparc64-wrs-vxworks-gnatmake");
            Append (Items, "xscale-wrs-vxworks-gnatmake");
            Append (Items, "powerpc-xcoff-lynxos-gnatmake");
            Append (Items, "gnaampmake");
            Set_Popdown_Strings (Combo, Items);
            Free_String_List (Items);

            Ent := Get_Entry (Combo);
            Set_Text
              (Ent,
               Get_Attribute_Value
                 (Project, Compiler_Command_Attribute,
                  Default => "gnatmake",
                  Index => Languages (L).all));

         else
            Gtk_New (Ent);
            Pack_Start (Hbox, Ent);
            Set_Text
              (Ent,
               Get_Attribute_Value
                 (Project, Compiler_Command_Attribute,
                  Default => "gcc",
                  Index => Languages (L).all));
         end if;

         Editor.Compilers (L) := Gtk_Widget (Ent);

         Set_Sensitive
           (Ent, Contains (Project_Languages, Languages (L).all));

         Object_User_Callback.Connect
           (Get_Check_Button (Editor.Languages, Languages (L).all),
            "toggled",
            Object_User_Callback.To_Marshaller (Command_Set_Sensitive'Access),
            User_Data => GObject (Ent));
      end loop;

      --  gnatls

      --  ??? Would be nice to specify the list of available cross compilers
      --  using a configuration file

      Ada_Check := Get_Check_Button (Editor.Languages, "ada");

      Gtk_New_Hbox (Hbox, Homogeneous => False);
      Pack_Start (Box, Hbox);

      Gtk_New (Event);
      Pack_Start (Hbox, Event, Expand => False);
      Gtk_New (Label, -"Gnatls:");
      Add_Widget (Group, Label);
      Set_Alignment (Label, 0.0, 0.5);
      Add (Event, Label);
      Set_Tip (Get_Tooltips (Kernel), Event,
               -("Name of the external tool to use to find the location of"
                 & " the standard Ada library"));

      Gtk_New (Combo);
      Set_Width_Chars (Get_Entry (Combo), 0);
      Pack_Start (Hbox, Combo, Expand => True);

      Append (Items, "gnatls");
      Append (Items, "powerpc-wrs-vxworks-gnatls");
      Append (Items, "powerpc-wrs-vxworksae-gnatls");
      Append (Items, "powerpc-elf-gnatls");
      Append (Items, "i386-wrs-vxworks-gnatls");
      Append (Items, "m68k-wrs-vxworks-gnatls");
      Append (Items, "mips-wrs-vxworks-gnatls");
      Append (Items, "sparc-wrs-vxworks-gnatls");
      Append (Items, "sparc64-wrs-vxworks-gnatls");
      Append (Items, "xscale-wrs-vxworks-gnatls");
      Append (Items, "powerpc-xcoff-lynxos-gnatls");
      Append (Items, "gnaampls");
      Set_Popdown_Strings (Combo, Items);
      Free_String_List (Items);
      Editor.Gnatls := Get_Entry (Combo);
      Set_Text
        (Editor.Gnatls,
         Get_Attribute_Value
         (Project, Gnatlist_Attribute, Default => "gnatls"));

      Set_Sensitive (Editor.Gnatls, Get_Active (Ada_Check));
      Object_User_Callback.Connect
        (Ada_Check, "toggled",
         Object_User_Callback.To_Marshaller (Command_Set_Sensitive'Access),
         User_Data => GObject (Editor.Gnatls));

      --  Debugger

      Gtk_New_Hbox (Hbox, Homogeneous => False);
      Pack_Start (Box, Hbox);

      Gtk_New (Event);
      Pack_Start (Hbox, Event, Expand => False);
      Gtk_New (Label, -"Debugger:");
      Add_Widget (Group, Label);
      Set_Alignment (Label, 0.0, 0.5);
      Add (Event, Label);
      Set_Tip (Get_Tooltips (Kernel), Event,
               -"Name and location of the debugger to use");

      Gtk_New (Combo);
      Set_Width_Chars (Get_Entry (Combo), 0);
      Pack_Start (Hbox, Combo, Expand => True);

      Append (Items, "gdb");
      Append (Items, "powerpc-wrs-vxworks-gdb");
      Append (Items, "powerpc-wrs-vxworksae-gdb");
      Append (Items, "powerpc-elf-gdb");
      Append (Items, "i386-wrs-vxworks-gdb");
      Append (Items, "m68k-wrs-vxworks-gdb");
      Append (Items, "mips-wrs-vxworks-gdb");
      Append (Items, "sparc-wrs-vxworks-gdb");
      Append (Items, "sparc64-wrs-vxworks-gdb");
      Append (Items, "xscale-wrs-vxworks-gdb");
      Append (Items, "powerpc-xcoff-lynxos-gdb");
      Set_Popdown_Strings (Combo, Items);
      Free_String_List (Items);
      Editor.Debugger := Get_Entry (Combo);
      Set_Text
        (Editor.Debugger,
         Get_Attribute_Value
         (Project, Debugger_Command_Attribute, Default => "gdb"));

      --  Configuration frame

      Gtk_New (Frame, -"External configuration");
      Set_Border_Width (Frame, 5);
      Pack_Start (Vbox, Frame, Expand => False);

      Gtk_New_Vbox (Box, Homogeneous => True);
      Add (Frame, Box);

      Gtk_New_Hbox (Hbox, Homogeneous => False);
      Pack_Start (Box, Hbox);

      Gtk_New (Event);
      Pack_Start (Hbox, Event, Expand => False);
      Gtk_New (Label, -"Global pragmas:");
      Add_Widget (Group, Label);
      Set_Alignment (Label, 0.0, 0.5);
      Add (Event, Label);
      Set_Tip (Get_Tooltips (Kernel), Event,
               -("External file that contains the configuration pragmas to use"
                 & " for Ada sources. This file will be used both for this"
                 & " project and all its imported projects"));

      Gtk_New (Editor.Global_Pragmas);
      Set_Width_Chars (Editor.Global_Pragmas, 0);
      Pack_Start (Hbox, Editor.Global_Pragmas, Expand => True);
      Set_Text
        (Editor.Global_Pragmas,
         Get_Attribute_Value
         (Project, Global_Pragmas_Attribute,
          Default => ""));

      Gtk_New (Button2, -"Browse");
      Pack_Start (Hbox, Button2, Expand => False);
      Widget_Callback.Object_Connect
        (Button2, "clicked",
         Widget_Callback.To_Marshaller (Browse_Location'Access),
         Slot_Object => Editor.Global_Pragmas);

      Gtk_New_Hbox (Hbox, Homogeneous => False);
      Pack_Start (Box, Hbox);

      Gtk_New (Event);
      Pack_Start (Hbox, Event, Expand => False);
      Gtk_New (Label, -"Local pragmas:");
      Add_Widget (Group, Label);
      Set_Alignment (Label, 0.0, 0.5);
      Add (Event, Label);
      Set_Tip (Get_Tooltips (Kernel), Event,
               -("External file that contains the configuration pragmas to use"
                 & " for Ada sources of this project. This is combined with"
                 & " the pragmas found in the Global Pragmas section of the"
                 & " root project." & ASCII.NUL
                 & "This file isn't used for imported projects"));

      Gtk_New (Editor.Local_Pragmas);
      Set_Width_Chars (Editor.Local_Pragmas, 0);
      Pack_Start (Hbox, Editor.Local_Pragmas, Expand => True);
      Set_Text
        (Editor.Local_Pragmas,
         Get_Attribute_Value
         (Project, Local_Pragmas_Attribute,
          Default => ""));

      Gtk_New (Button2, -"Browse");
      Pack_Start (Hbox, Button2, Expand => False);
      Widget_Callback.Object_Connect
        (Button2, "clicked",
         Widget_Callback.To_Marshaller (Browse_Location'Access),
         Slot_Object => Editor.Local_Pragmas);

      --  Cross environment frame

      Gtk_New (Frame, -"Cross environment");
      Set_Border_Width (Frame, 5);
      Pack_Start (Vbox, Frame, Expand => False);

      Gtk_New_Vbox (Box, Homogeneous => True);
      Add (Frame, Box);

      --  Tools host

      Gtk_New_Hbox (Hbox, Homogeneous => False);
      Pack_Start (Box, Hbox);

      Gtk_New (Event);
      Pack_Start (Hbox, Event, Expand => False);
      Gtk_New (Label, -"Tools host:");
      Add_Widget (Group, Label);
      Set_Alignment (Label, 0.0, 0.5);
      Add (Event, Label);
      Set_Tip (Get_Tooltips (Kernel), Event,
               -("Name or IP address of the machine on which the application"
                 & " should be compiled, debugged and run. It is recommended"
                 & " that you always start GPS locally, and work remotely"
                 & " on your application. Leave this field blank when working"
                 & " locally"));


      Gtk_New (Editor.Tools_Host);
      Set_Width_Chars (Editor.Tools_Host, 0);
      Pack_Start (Hbox, Editor.Tools_Host, Expand => True);
      Set_Text
        (Editor.Tools_Host,
         Get_Attribute_Value
           (Project, Remote_Host_Attribute, Default => ""));

      --  Program host

      Gtk_New_Hbox (Hbox, Homogeneous => False);
      Pack_Start (Box, Hbox);

      Gtk_New (Event);
      Pack_Start (Hbox, Event, Expand => False);
      Gtk_New (Label, -"Program host:");
      Add_Widget (Group, Label);
      Add (Event, Label);
      Set_Alignment (Label, 0.0, 0.5);
      Set_Tip (Get_Tooltips (Kernel), Event,
               -("Name or IP address of the embedded target. This field"
                 & " should be left blank if you are not working on an"
                 & " embedded application"));

      Gtk_New (Editor.Program_Host);
      Set_Width_Chars (Editor.Tools_Host, 0);
      Pack_Start (Hbox, Editor.Program_Host, Expand => True);
      Set_Text
        (Editor.Program_Host,
         Get_Attribute_Value
         (Project, Program_Host_Attribute, Default => ""));

      --  Protocol

      Gtk_New_Hbox (Hbox, Homogeneous => False);
      Pack_Start (Box, Hbox);

      Gtk_New (Event);
      Pack_Start (Hbox, Event, Expand => False);
      Gtk_New (Label, -"Protocol:");
      Add_Widget (Group, Label);
      Add (Event, Label);
      Set_Alignment (Label, 0.0, 0.5);
      Set_Tip (Get_Tooltips (Kernel), Event,
               -("Protocol used to connect to the embedded target. This"
                 & " field should be left blank if you are not working"
                 & " on an embedded application"));

      Gtk_New (Combo);
      Set_Width_Chars (Get_Entry (Combo), 0);
      Pack_Start (Hbox, Combo, Expand => True);
      Append (Items, "wtx");
      Append (Items, "vxworks");
      Append (Items, "remote");
      Set_Popdown_Strings (Combo, Items);
      Free_String_List (Items);
      Editor.Protocol := Get_Entry (Combo);
      Set_Text
        (Editor.Protocol,
         Get_Attribute_Value
         (Project, Protocol_Attribute, Default => ""));

      Free (Languages);
      Free (Project_Languages);

      return Gtk_Widget (Vbox);
   end Create_General_Page;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Editor  : access Properties_Editor_Record'Class;
      Project : Project_Type;
      Kernel  : access Kernel_Handle_Record'Class)
  is
      Label        : Gtk_Label;
      Main_Note    : Gtk_Notebook;
      Button       : Gtk_Widget;
      Page         : Project_Editor_Page;
      Box          : Gtk_Box;
      Main_Box     : Gtk_Box;

   begin
      Gtk.Dialog.Initialize
        (Dialog => Editor,
         Title  => -"Properties for " & Project_Name (Project),
         Parent => Get_Main_Window (Kernel),
         Flags  => Modal or Destroy_With_Parent);
      Set_Policy
        (Editor,
         Allow_Shrink => False,
         Allow_Grow   => True,
         Auto_Shrink  => True);
      Realize (Editor);

      Widget_Callback.Connect
        (Editor, "destroy",
         Widget_Callback.To_Marshaller (Destroyed'Access));

      Gtk_New_Hbox (Main_Box);
      Pack_Start (Get_Vbox (Editor), Main_Box, Expand => True, Fill => True);

      Gtk_New (Main_Note);
      Set_Tab_Pos (Main_Note, Pos_Left);
      Pack_Start (Main_Box, Main_Note, Expand => True, Fill => True);

      Gtk_New (Label, -"General");
      Append_Page
        (Main_Note, Create_General_Page (Editor, Project, Kernel), Label);

      Gtk_New_Vbox (Box, Homogeneous => False);
      Pack_Start (Main_Box, Box, Expand => True, Fill => True);

      Gtk_New (Label, -"Apply changes to:");
      Set_Alignment (Label, 0.0, 0.0);
      Pack_Start (Box, Label, Expand => False);

      Gtk_New (Editor.Prj_Selector, Kernel, Project);
      Pack_Start (Box, Editor.Prj_Selector, Expand => True, Fill => True);

      Gtk_New (Editor.Selector, Kernel);
      Pack_Start (Box, Editor.Selector, Expand => True, Fill => True);

      Editor.Project := Project;
      Editor.Kernel  := Kernel_Handle (Kernel);
      Editor.Pages   := new Widget_Array
        (1 .. Project_Editor_Pages_Count (Kernel));

      Show_All (Editor);

      --  We used to create the pages dynamically, in Switch_Page. However,
      --  this means that the pages that haven't been visited by the user will
      --  not generate a project on exit, which is a problem when copying a
      --  scenario to another one for instance
      for E in Editor.Pages'Range loop
         Page := Get_Nth_Project_Editor_Page (Kernel, E);

         Gtk_New (Label, Get_Label (Page));
         Editor.Pages (E) := Widget_Factory
           (Page, Project, Project_Path (Project), Editor.Kernel);
         Append_Page (Main_Note, Editor.Pages (E), Label);
      end loop;

      --  Connect this only once we have created the pages
      Object_User_Callback.Connect
        (Main_Note, "switch_page",
         Object_User_Callback.To_Marshaller (Switch_Page'Access),
         User_Data => GObject (Editor),
         After => True);
      Button := Add_Button (Editor, Stock_Ok, Gtk_Response_OK);
      Show (Button);
      Button := Add_Button (Editor, Stock_Cancel, Gtk_Response_Cancel);
      Show (Button);
   end Initialize;

   -----------------
   -- Switch_Page --
   -----------------

   procedure Switch_Page
     (Notebook : access GObject_Record'Class;
      Editor   : GObject)
   is
      Note  : constant Gtk_Notebook := Gtk_Notebook (Notebook);
      Ed    : constant Properties_Editor := Properties_Editor (Editor);
      Page  : constant Integer := Integer (Get_Current_Page (Note));
      Flags : Selector_Flags;

   begin
      if Page >= 1
        and then not Gtk.Object.In_Destruction_Is_Set (Ed)
      then
         declare
            Languages : Argument_List := Get_Languages (Ed.Languages);
         begin
            Refresh
              (Page         => Get_Nth_Project_Editor_Page (Ed.Kernel, Page),
               Widget       => Ed.Pages (Page),
               Project      => Ed.Project,
               Languages    => Languages);
            Free (Languages);
         end;

         Flags := Get_Flags (Get_Nth_Project_Editor_Page (Ed.Kernel, Page));

         Set_Sensitive
           (Ed.Prj_Selector, (Flags and Multiple_Projects) /= 0);
         Set_Sensitive
           (Ed.Selector, (Flags and Multiple_Scenarios) /= 0);

      elsif Page = 0 then
         Set_Sensitive (Ed.Prj_Selector, True);
         Set_Sensitive (Ed.Selector, True);
      end if;
   end Switch_Page;

   --------------------------------
   -- Warning_On_View_Incomplete --
   --------------------------------

   function Warning_On_View_Incomplete
     (Kernel  : access Kernel_Handle_Record'Class;
      Project : Project_Type)
      return Project_Edition_Type
   is
      D : Gtk_Dialog;
      B : Gtk_Widget;
      L : Gtk_Label;
      C : Gtk_Check_Button;
      pragma Unreferenced (B);
   begin
      Gtk_New (D,
               Title  => -"Project had errors",
               Parent => Get_Main_Window (Kernel),
               Flags  => Modal);

      Gtk_New
        (L,
         -"The project """
         & Project_Name (Project)
         & (-(""" contained errors, and was incorrectly"
              & ASCII.LF
              & "loaded by GPS. Editing it through the project properties"
              & ASCII.LF
              & "dialog might result in a lose of data.")));
      Set_Alignment (L, 0.0, 0.5);
      Pack_Start (Get_Vbox (D), L, Expand => True, Fill => True);

      Gtk_New (C, -"Edit the project file");
      Set_Active (C, True);
      Pack_End (Get_Vbox (D), C, Expand => False);

      B := Add_Button (D, Stock_Open,   Gtk_Response_OK);
      B := Add_Button (D, Stock_Cancel, Gtk_Response_Cancel);

      Show_All (D);

      case Run (D) is
         when Gtk_Response_OK =>
            if Get_Active (C) then
               Destroy (D);
               return Edit_File;
            else
               Destroy (D);
               return Edit_Properties;
            end if;

         when others =>
            Destroy (D);
            return Do_Not_Edit;
      end case;
   end Warning_On_View_Incomplete;

   ---------------------
   -- Edit_Properties --
   ---------------------

   procedure Edit_Properties
     (Project : Project_Type;
      Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Languages : Argument_List := Known_Languages
        (Get_Language_Handler (Kernel));

      procedure Report_Error (Msg : String);
      --  Report an error to the console

      function Process_General_Page
        (Editor : Properties_Editor;
         Project : Project_Type;
         Scenario_Variables : Scenario_Variable_Array;
         Project_Renamed_Or_Moved : Boolean)
         return Boolean;
      --  Modify the attributes set on the general page

      ------------------
      -- Report_Error --
      ------------------

      procedure Report_Error (Msg : String) is
      begin
         Insert (Kernel, Msg);
      end Report_Error;

      --------------------------
      -- Process_General_Page --
      --------------------------

      function Process_General_Page
        (Editor : Properties_Editor;
         Project : Project_Type;
         Scenario_Variables : Scenario_Variable_Array;
         Project_Renamed_Or_Moved : Boolean)
         return Boolean
      is
         Changed  : Boolean := False;
         Ent      : Gtk_GEntry;
         Relative : Boolean := Get_Active (Editor.Use_Relative_Paths);
      begin
         --  If we are moving the project through the GUI, then we need to
         --  convert the paths to absolute or the semantics changes.

         if Project_Renamed_Or_Moved then
            Relative := False;
         end if;

         --  Convert the paths if necessary
         if Relative /= Paths_Are_Relative (Kernel, Project) then
            if Relative then
               Set_Paths_Type (Project, Projects.Relative);
            else
               Set_Paths_Type (Project, Absolute);
            end if;

            Changed := Changed
              or Convert_Paths (Project                => Project,
                                Use_Relative_Paths     => Relative,
                                Update_With_Statements => True);

            if Changed then
               Trace (Me, "Paths have changed relative/absolute");
            end if;
         end if;

         if Project = No_Project
           or else Get_Text (Editor.Gnatls) /= Get_Attribute_Value
           (Project, Gnatlist_Attribute, Default => "gnatls")
         then
            Update_Attribute_Value_In_Scenario
              (Project            => Project,
               Scenario_Variables => Scenario_Variables,
               Attribute          => Gnatlist_Attribute,
               Value              => Get_Text (Editor.Gnatls));
            Changed := True;
            Trace (Me, "gnatls changed");
         end if;

         if Project = No_Project
           or else Get_Text (Editor.Debugger) /= Get_Attribute_Value
           (Project, Debugger_Command_Attribute, Default => "gdb")
         then
            Update_Attribute_Value_In_Scenario
              (Project            => Project,
               Scenario_Variables => Scenario_Variables,
               Attribute          => Debugger_Command_Attribute,
               Value              => Get_Text (Editor.Debugger));
            Changed := True;
            Trace (Me, "debugger command changed");
         end if;

         declare
            New_Languages : Argument_List := Get_Languages (Editor.Languages);
         begin
            if Project /= No_Project then

               declare
                  Project_Languages : Argument_List :=
                    Get_Languages (Project);
                  Different : Boolean;
               begin
                  for J in Languages'Range loop
                     Ent   := Gtk_GEntry (Editor.Compilers (J));

                     if Is_Selected (Editor.Languages, Languages (J).all) then
                        Different := False;
                        if To_Lower (Languages (J).all) = "ada" then
                           Different := Get_Attribute_Value
                             (Project, Compiler_Command_Attribute,
                              Default => "gnatmake",
                              Index => Languages (J).all) /= Get_Text (Ent);
                        else
                           Different := Get_Attribute_Value
                             (Project, Compiler_Command_Attribute,
                              Default => "gcc",
                              Index => Languages (J).all) /= Get_Text (Ent);
                        end if;

                        if Different then
                           Update_Attribute_Value_In_Scenario
                             (Project  => Project,
                              Scenario_Variables => Scenario_Variables,
                              Attribute => Compiler_Command_Attribute,
                              Value => Get_Text (Ent),
                              Attribute_Index => Languages (J).all);
                           Changed := True;
                           Trace (Me, "Compiler changed for "
                                  & Languages (J).all);
                        end if;
                     end if;
                  end loop;

                  if not Is_Equal
                    (New_Languages, Project_Languages, Case_Sensitive => False)
                  then
                     Changed := True;
                     Update_Attribute_Value_In_Scenario
                       (Project            => Project,
                        Scenario_Variables => Scenario_Variables,
                        Attribute          => Languages_Attribute,
                        Values             => New_Languages);
                     Trace (Me, "List of languages changed");
                  end if;

                  Free (Project_Languages);
               end;

            else
               Changed := True;
               Trace (Me, "No project view initially");
               Update_Attribute_Value_In_Scenario
                 (Project            => Project,
                  Scenario_Variables => Scenario_Variables,
                  Attribute          => Languages_Attribute,
                  Values             => New_Languages);

               for J in Languages'Range loop
                  Ent := Gtk_GEntry (Editor.Compilers (J));
                  Update_Attribute_Value_In_Scenario
                    (Project            => Project,
                     Scenario_Variables => Scenario_Variables,
                     Attribute          => Compiler_Command_Attribute,
                     Value              => Get_Text (Ent),
                     Attribute_Index    => Languages (J).all);
               end loop;

            end if;

            Free (New_Languages);
         end;

         if Project = No_Project
           or else Get_Text (Editor.Tools_Host) /= Get_Attribute_Value
           (Project, Remote_Host_Attribute, Default => "")
         then
            Changed := True;
            Trace (Me, "Remote_Host changed");

            if Get_Text (Editor.Tools_Host) /= "" then
               Update_Attribute_Value_In_Scenario
                 (Project            => Project,
                  Scenario_Variables => Scenario_Variables,
                  Attribute          => Remote_Host_Attribute,
                  Value              => Get_Text (Editor.Tools_Host));
            else
               Delete_Attribute
                 (Project            => Project,
                  Scenario_Variables => Scenario_Variables,
                  Attribute          => Remote_Host_Attribute);
            end if;
         end if;

         if Project = No_Project
           or else Get_Text (Editor.Program_Host) /= Get_Attribute_Value
           (Project, Program_Host_Attribute, Default => "")
         then
            Changed := True;
            Trace (Me, "Program_Host changed");

            if Get_Text (Editor.Program_Host) /= "" then
               Update_Attribute_Value_In_Scenario
                 (Project            => Project,
                  Scenario_Variables => Scenario_Variables,
                  Attribute          => Program_Host_Attribute,
                  Value              => Get_Text (Editor.Program_Host));
            else
               Delete_Attribute
                 (Project            => Project,
                  Scenario_Variables => Scenario_Variables,
                  Attribute          => Program_Host_Attribute);
            end if;
         end if;

         if Project = No_Project
           or else Get_Text (Editor.Protocol) /= Get_Attribute_Value
           (Project, Protocol_Attribute, Default => "")
         then
            Changed := True;
            Trace (Me, "Protocol changed");

            if Get_Text (Editor.Protocol) /= "" then
               Update_Attribute_Value_In_Scenario
                 (Project            => Project,
                  Scenario_Variables => Scenario_Variables,
                  Attribute          => Protocol_Attribute,
                  Value              => Get_Text (Editor.Protocol));
            else
               Delete_Attribute
                 (Project            => Project,
                  Scenario_Variables => Scenario_Variables,
                  Attribute          => Protocol_Attribute);
            end if;
         end if;

         if Project = No_Project
           or else Get_Text (Editor.Global_Pragmas) /= Get_Attribute_Value
           (Project, Global_Pragmas_Attribute, Default => "")
         then
            Changed := True;
            Trace (Me, "global pragmas changed");

            if Get_Text (Editor.Global_Pragmas) /= "" then
               Update_Attribute_Value_In_Scenario
                 (Project            => Project,
                  Scenario_Variables => Scenario_Variables,
                  Attribute          => Global_Pragmas_Attribute,
                  Value              => Get_Text (Editor.Global_Pragmas));
            else
               Delete_Attribute
                 (Project            => Project,
                  Scenario_Variables => Scenario_Variables,
                  Attribute          => Global_Pragmas_Attribute);
            end if;
         end if;

         if Project = No_Project
           or else Get_Text (Editor.Local_Pragmas) /= Get_Attribute_Value
           (Project, Local_Pragmas_Attribute, Default => "")
         then
            Changed := True;
            Trace (Me, "local pragmas changed");

            if Get_Text (Editor.Local_Pragmas) /= "" then
               Update_Attribute_Value_In_Scenario
                 (Project            => Project,
                  Scenario_Variables => Scenario_Variables,
                  Attribute          => Local_Pragmas_Attribute,
                  Value              => Get_Text (Editor.Local_Pragmas));
            else
               Delete_Attribute
                 (Project            => Project,
                  Scenario_Variables => Scenario_Variables,
                  Attribute          => Local_Pragmas_Attribute);
            end if;
         end if;

         return Changed;
      end Process_General_Page;


      Editor  : Properties_Editor;
      Changed : Boolean := False;
      Response : Gtk_Response_Type;
      Response2 : Message_Dialog_Buttons;
      Project_Renamed_Or_Moved : Boolean := False;

   begin
      if not View_Is_Complete (Project) then
         case Warning_On_View_Incomplete (Kernel, Project) is
            when Do_Not_Edit =>
               return;

            when Edit_File =>
               Open_File_Editor
                 (Kernel,
                  Project_Path (Project),
                  From_Path => False);
               return;

            when Edit_Properties =>
               null;
         end case;
      end if;

      Gtk_New (Editor, Project, Kernel);

      loop
         Response := Run (Editor);

         exit when Response /= Gtk_Response_OK;

         if not Is_Valid_Project_Name (Get_Text (Editor.Name)) then
            Response2 := Message_Dialog
              (Msg     => (-"Invalid name for the project ") &
                          (-"(only letters, digits and underscores)"),
               Buttons => Button_OK,
               Dialog_Type => Error,
               Title   => -"Error",
               Parent  => Get_Main_Window (Kernel));

         elsif not Is_Directory
           (Name_As_Directory (Get_Text (Editor.Path)))
         then
            Response2 := Message_Dialog
              (Msg     => Name_As_Directory (Get_Text (Editor.Path))
               & (-" is not a valid directory"),
               Buttons => Button_OK,
               Dialog_Type => Error,
               Title   => -"Error",
               Parent  => Get_Main_Window (Kernel));

         else
            declare
               New_Name : constant String := Get_Text (Editor.Name);
               New_File : constant String := To_File_Name (New_Name);
               New_Path : constant String :=
                 Name_As_Directory (Get_Text (Editor.Path));
            begin
               if (New_Name /= Project_Name (Project)
                   or else New_Path /= Project_Directory (Project))
                 and then Is_Regular_File
                 (New_Path & New_File & Project_File_Extension)
               then
                  Response2 := Message_Dialog
                    (Msg => New_Path & New_File & Project_File_Extension
                     & (-" already exists. Do you want to overwrite ?"),
                     Buttons => Button_Yes or Button_No,
                     Dialog_Type => Error,
                     Title   => -"Error",
                     Parent  => Get_Main_Window (Kernel));

                  if Response2 = Button_Yes then
                     exit;
                  end if;
               else
                  exit;
               end if;
            end;
         end if;
      end loop;

      if Response = Gtk_Response_OK then
         declare
            Vars         : constant Scenario_Variable_Array :=
              Scenario_Variables (Kernel);
            Saved_Values : Argument_List := Get_Current_Scenario (Vars);
            Prj_Iter     : Project_Iterator := Start (Editor.Prj_Selector);
            Ed           : Project_Editor_Page;
         begin
            while Current (Prj_Iter) /= No_Project loop
               declare
                  Scenar_Iter : Scenario_Iterator := Start (Editor.Selector);
               begin
                  while not At_End (Scenar_Iter) loop
                     --  Set the scenario
                     declare
                        Curr   : Argument_List := Current (Scenar_Iter);
                        Is_Env : Boolean := True;
                     begin
                        Set_Environment (Vars, Curr);

                        for V in Vars'Range loop
                           Is_Env := Is_Env and then Curr (V).all =
                             Saved_Values (V).all;
                        end loop;

                        Free (Curr);
                     end;

                     if Process_General_Page
                       (Editor, Current (Prj_Iter), Vars,
                        Project_Renamed_Or_Moved)
                     then
                        Trace (Me, "General page modified the project");
                        Changed := True;
                     end if;

                     --  Modify each projects

                     for P in Editor.Pages'Range loop
                        Ed := Get_Nth_Project_Editor_Page (Kernel, P);

                        --  If the project is either the one the user clicked
                        --  on or the page might apply to multiple projects
                        if ((Get_Flags (Ed) and Multiple_Projects) /= 0
                            or else Current (Prj_Iter) = Project)
                        then
                           if Project_Editor
                             (Ed, Current (Prj_Iter),
                              Kernel, Editor.Pages (P),
                              Vars,
                              Ref_Project => Project)
                           then
                              Trace (Me, "Project modified on page " & P'Img);
                              Changed := True;
                           end if;
                        end if;
                     end loop;

                     Next (Scenar_Iter);
                  end loop;
               end;

               Next (Prj_Iter);
            end loop;

            --  Restore the scenario
            Set_Environment (Vars, Saved_Values);
            Free (Saved_Values);
         end;


         --  Rename the project last, since we need to recompute the view
         --  immediately afterward before anything else can be done with the
         --  project.

         declare
            New_Name : constant String := Get_Text (Editor.Name);
            New_Path : constant String :=
              Name_As_Directory (Get_Text (Editor.Path));
         begin
            if New_Name /= Project_Name (Project)
              or else New_Path /= Project_Directory (Project)
            then
               Project_Renamed_Or_Moved := True;

               Rename_And_Move
                 (Root_Project  => Get_Project (Kernel),
                  Project       => Project,
                  New_Name      => New_Name,
                  New_Path      => New_Path,
                  Report_Errors => Report_Error'Unrestricted_Access);

               --  Since we actually changed the project hierarchy (all modules
               --  that stored the name of the projects are now obsolete), we
               --  act as if a new project had been loaded.

               Project_Changed (Kernel);

               Changed := True;
               Trace (Me, "Project was renamed or moved");
            end if;
         end;

         if Changed then
            Recompute_View (Kernel);
         end if;
      end if;

      Destroy (Editor);
      Free (Languages);
   end Edit_Properties;

   -----------------------------
   -- Edit_Project_Properties --
   -----------------------------

   procedure Edit_Project_Properties
     (Widget  : access Glib.Object.GObject_Record'Class;
      Context : Glide_Kernel.Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
      C : constant File_Selection_Context_Access :=
        File_Selection_Context_Access (Context);
   begin
      Edit_Properties (Project_Information (C), Get_Kernel (Context));

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Edit_Project_Properties;

end Project_Properties;
