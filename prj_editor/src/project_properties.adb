-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                        Copyright (C) 2002                         --
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
with Glide_Kernel.Project;      use Glide_Kernel.Project;
with Glide_Kernel;              use Glide_Kernel;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
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
with Gtk.Handlers;              use Gtk.Handlers;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Notebook;              use Gtk.Notebook;
with Gtk.Object;                use Gtk.Object;
with Gtk.Size_Group;            use Gtk.Size_Group;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Tooltips;              use Gtk.Tooltips;
with Gtk.Widget;                use Gtk.Widget;
with Prj;                       use Prj;
with Prj.Tree;                  use Prj.Tree;
with Prj_API;                   use Prj_API;
with String_Utils;              use String_Utils;
with Basic_Types;               use Basic_Types;
with Language_Handlers;         use Language_Handlers;
with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Unchecked_Deallocation;
with Prj_Normalize;             use Prj_Normalize;
with Scenario_Selectors;        use Scenario_Selectors;

package body Project_Properties is
   use Widget_List;

   type Widget_Array is array (Natural range <>) of Gtk_Widget;
   type Widget_Array_Access is access Widget_Array;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Widget_Array, Widget_Array_Access);

   type Properties_Editor_Record is new Gtk.Dialog.Gtk_Dialog_Record with
   record
      Name               : Gtk.GEntry.Gtk_Entry;
      Path               : Gtk.GEntry.Gtk_Entry;
      Gnatls             : Gtk.GEntry.Gtk_Entry;
      Compiler           : Gtk.GEntry.Gtk_Entry;
      Debugger           : Gtk.GEntry.Gtk_Entry;
      Compilers          : Widget_Array_Access;
      Languages          : Widget_Array_Access;
      Use_Relative_Paths : Gtk.Check_Button.Gtk_Check_Button;
      Tools_Host         : Gtk.GEntry.Gtk_Entry;
      Program_Host       : Gtk.GEntry.Gtk_Entry;
      Protocol           : Gtk.GEntry.Gtk_Entry;

      Pages              : Widget_Array_Access;
      --  The pages that have been registered.

      Project_View       : Prj.Project_Id;
      Kernel             : Kernel_Handle;
   end record;
   type Properties_Editor is access all Properties_Editor_Record'Class;

   procedure Gtk_New
     (Editor       : out Properties_Editor;
      Project_View : Prj.Project_Id;
      Kernel       : access Kernel_Handle_Record'Class);
   --  Create a new properties editor

   procedure Initialize
     (Editor       : access Properties_Editor_Record'Class;
      Project_View : Prj.Project_Id;
      Kernel       : access Kernel_Handle_Record'Class);
   --  Internal initialization function

   procedure Browse_Location (Editor : access Gtk_Widget_Record'Class);
   --  Open a directory selector for the new location of the project file

   procedure Command_Set_Sensitive
     (Check : access Glib.Object.GObject_Record'Class;
      Ent   : GObject);
   --  Set Ent to sensitive or insensitive state, depending on whether Check is
   --  active or not.

   procedure Destroyed (Editor : access Gtk_Widget_Record'Class);
   --  Called when the editor is destroyed

   function Create_General_Page
     (Editor       : access Properties_Editor_Record'Class;
      Project_View : Prj.Project_Id;
      Kernel       : access Kernel_Handle_Record'Class)
      return Gtk.Widget.Gtk_Widget;
   --  Create the "General" page for the project properties

   procedure Switch_Page
     (Notebook : access GObject_Record'Class; Editor : GObject);
   --  Called when a new page is selected in the notebook

   function Get_Languages (Editor : access Properties_Editor_Record'Class)
      return Argument_List;
   --  Return the list of supported languages selected graphically by the
   --  user. The caller must free the returned array

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Editor       : out Properties_Editor;
      Project_View : Prj.Project_Id;
      Kernel       : access Kernel_Handle_Record'Class) is
   begin
      Editor := new Properties_Editor_Record;
      Initialize (Editor, Project_View, Kernel);
   end Gtk_New;

   ---------------
   -- Destroyed --
   ---------------

   procedure Destroyed (Editor : access Gtk_Widget_Record'Class) is
      E : Properties_Editor := Properties_Editor (Editor);
   begin
      Unchecked_Free (E.Languages);
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
     (Editor       : access Properties_Editor_Record'Class;
      Project_View : Prj.Project_Id;
      Kernel       : access Kernel_Handle_Record'Class)
      return Gtk.Widget.Gtk_Widget
   is
      Button2      : Gtk_Button;
      Label        : Gtk_Label;
      Check        : Gtk_Check_Button;
      Languages : Argument_List := Known_Languages
        (Get_Language_Handler (Kernel));
      Project_Languages : Argument_List :=  Get_Languages (Project_View);
      Ent     : Gtk_GEntry;
      Project : constant Project_Node_Id := Get_Project_From_View
        (Project_View);
      Combo        : Gtk_Combo;
      Items        : Gtk.Enums.String_List.Glist;
      Frame        : Gtk_Frame;
      Group        : Gtk_Size_Group;
      Vbox, Box, Hbox : Gtk_Box;
      Ada_Check    : Gtk_Check_Button;
      Event        : Gtk_Event_Box;

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
               -("Name of the project, and name of the file. This field is"
                 & " case insensitive. This field only applies to the project"
                 & " you selected initially"));

      Gtk_New (Editor.Name);
      Set_Text (Editor.Name, Project_Name (Project_View));
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
      Set_Text (Editor.Path, Dir_Name (Project_Path (Project_View)));
      Pack_Start (Hbox, Editor.Path, Expand => True);

      Gtk_New (Button2, -"Browse");
      Pack_Start (Hbox, Button2, Expand => False);
      Widget_Callback.Object_Connect
        (Button2, "clicked",
         Widget_Callback.To_Marshaller (Browse_Location'Access),
         Slot_Object => Editor);

      Gtk_New (Editor.Use_Relative_Paths, -"Paths should be relative paths");
      Set_Active
        (Editor.Use_Relative_Paths,
         Project_Uses_Relative_Paths (Kernel, Project));
      Pack_Start (Box, Editor.Use_Relative_Paths);
      Set_Tip (Get_Tooltips (Kernel), Editor.Use_Relative_Paths,
               -("If this field is activated, then all the path information in"
                 & " the project (source and build directories, dependencies"
                 & " between projects,...) will be stored as paths relative"
                 & " to the location of the project file. It will thus be"
                 & " easier to move the project file to another directory"));

      --  Languages frame

      Gtk_New (Frame, -"Languages");
      Set_Border_Width (Frame, 5);
      Pack_Start (Vbox, Frame, Expand => False);

      Gtk_New_Vbox (Box, Homogeneous => True);
      Add (Frame, Box);

      Editor.Languages := new Widget_Array (Languages'Range);

      for L in Languages'Range loop
         declare
            S : String := Languages (L).all;
         begin
            Mixed_Case (S);
            Gtk_New (Check, S);
         end;
         Pack_Start (Box, Check);

         Editor.Languages (L) := Gtk_Widget (Check);
         Set_Active
           (Check, Contains
            (Project_Languages, Languages (L).all, Case_Sensitive => False));
      end loop;

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
            Ada_Check := Gtk_Check_Button (Editor.Languages (L));

            Gtk_New (Combo);
            Pack_Start (Hbox, Combo);
            Append (Items, "gnatmake");
            Append (Items, "powerpc-wrs-vxworks-gnatmake");
            Append (Items, "powerpc-wrs-vxworksae-gnatmake");
            Append (Items, "i386-wrs-vxworks-gnatmake");
            Append (Items, "m68k-wrs-vxworks-gnatmake");
            Append (Items, "mips-wrs-vxworks-gnatmake");
            Append (Items, "sparc-wrs-vxworks-gnatmake");
            Append (Items, "sparc64-wrs-vxworks-gnatmake");
            Append (Items, "strongarm-wrs-vxworks-gnatmake");
            Append (Items, "alpha-dec-vxworks-gnatmake");
            Append (Items, "powerpc-xcoff-lynxos-gnatmake");
            Append (Items, "gnaampmake");
            Append (Items, "jgnatmake");
            Set_Popdown_Strings (Combo, Items);
            Free_String_List (Items);

            Ent := Get_Entry (Combo);
            Set_Text
              (Ent,
               Get_Attribute_Value
                 (Project_View, Compiler_Command_Attribute,
                  Ide_Package, Default => "gnatmake",
                  Index => Languages (L).all));

         else
            Gtk_New (Ent);
            Pack_Start (Hbox, Ent);
            Set_Text
              (Ent,
               Get_Attribute_Value
                 (Project_View, Compiler_Command_Attribute,
                  Ide_Package, Default => "gcc",
                  Index => Languages (L).all));
         end if;

         Editor.Compilers (L) := Gtk_Widget (Ent);

         Set_Sensitive
           (Ent, Get_Active (Gtk_Check_Button (Editor.Languages (L))));

         Object_User_Callback.Connect
           (Editor.Languages (L), "toggled",
            Object_User_Callback.To_Marshaller (Command_Set_Sensitive'Access),
            User_Data => GObject (Ent));
      end loop;

      --  gnatls

      --  ??? Would be nice to specify the list of available cross compilers
      --  using a configuration file

      if Ada_Check /= null then
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
         Pack_Start (Hbox, Combo, Expand => True);

         Append (Items, "gnatls");
         Append (Items, "powerpc-wrs-vxworks-gnatls");
         Append (Items, "powerpc-wrs-vxworksae-gnatls");
         Append (Items, "i386-wrs-vxworks-gnatls");
         Append (Items, "m68k-wrs-vxworks-gnatls");
         Append (Items, "mips-wrs-vxworks-gnatls");
         Append (Items, "sparc-wrs-vxworks-gnatls");
         Append (Items, "sparc64-wrs-vxworks-gnatls");
         Append (Items, "strongarm-wrs-vxworks-gnatls");
         Append (Items, "alpha-dec-vxworks-gnatls");
         Append (Items, "powerpc-xcoff-lynxos-gnatls");
         Append (Items, "gnaampls");
         Append (Items, "jgnatls");
         Set_Popdown_Strings (Combo, Items);
         Free_String_List (Items);
         Editor.Gnatls := Get_Entry (Combo);
         Set_Text
           (Editor.Gnatls,
            Get_Attribute_Value
            (Project_View, Gnatlist_Attribute,
             Ide_Package, Default => "gnatls"));

         Set_Sensitive (Editor.Gnatls, Get_Active (Ada_Check));
         Object_User_Callback.Connect
           (Ada_Check, "toggled",
            Object_User_Callback.To_Marshaller (Command_Set_Sensitive'Access),
            User_Data => GObject (Editor.Gnatls));
      end if;

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
      Pack_Start (Hbox, Combo, Expand => True);

      Append (Items, "gdb");
      Append (Items, "powerpc-wrs-vxworks-gdb");
      Append (Items, "powerpc-wrs-vxworksae-gdb");
      Append (Items, "i386-wrs-vxworks-gdb");
      Append (Items, "m68k-wrs-vxworks-gdb");
      Append (Items, "mips-wrs-vxworks-gdb");
      Append (Items, "sparc-wrs-vxworks-gdb");
      Append (Items, "sparc64-wrs-vxworks-gdb");
      Append (Items, "strongarm-wrs-vxworks-gdb");
      Append (Items, "alpha-dec-vxworks-gdb");
      Append (Items, "powerpc-xcoff-lynxos-gdb");
      Set_Popdown_Strings (Combo, Items);
      Free_String_List (Items);
      Editor.Debugger := Get_Entry (Combo);
      Set_Text
        (Editor.Debugger,
         Get_Attribute_Value
         (Project_View, Debugger_Command_Attribute,
          Ide_Package, Default => "gdb"));

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
      Pack_Start (Hbox, Editor.Tools_Host, Expand => True);
      Set_Text
        (Editor.Tools_Host,
         Get_Attribute_Value
         (Project_View, Remote_Host_Attribute,
          Ide_Package, Default => ""));

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
      Pack_Start (Hbox, Editor.Program_Host, Expand => True);
      Set_Text
        (Editor.Program_Host,
         Get_Attribute_Value
         (Project_View, Program_Host_Attribute,
          Ide_Package, Default => ""));

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
         (Project_View, Protocol_Attribute,
          Ide_Package, Default => ""));

      Free (Languages);
      Free (Project_Languages);

      return Gtk_Widget (Vbox);
   end Create_General_Page;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Editor       : access Properties_Editor_Record'Class;
      Project_View : Prj.Project_Id;
      Kernel       : access Kernel_Handle_Record'Class)
   is
      Label        : Gtk_Label;
      Main_Note    : Gtk_Notebook;
      Button       : Gtk_Widget;
      Page         : Project_Editor_Page;
      Box          : Gtk_Box;
      Main_Box     : Gtk_Box;
      Selector     : Scenario_Selector;
      Prj_Selector : Project_Selector;

   begin
      Gtk.Dialog.Initialize
        (Dialog => Editor,
         Title  => -"Properties for "
           & Project_Name (Project_View),
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
        (Main_Note, Create_General_Page (Editor, Project_View, Kernel), Label);

      Gtk.Handlers.Add_Watch
        (Object_User_Callback.Connect
         (Main_Note, "switch_page",
          Object_User_Callback.To_Marshaller (Switch_Page'Access),
          User_Data => GObject (Editor),
          After => True),
         Editor);

      Editor.Project_View := Project_View;
      Editor.Kernel       := Kernel_Handle (Kernel);
      Editor.Pages        := new Widget_Array
        (1 .. Project_Editor_Pages_Count (Kernel));

      for E in Editor.Pages'Range loop
         Page := Get_Nth_Project_Editor_Page (Kernel, E);

         --  Pages' widgets are created dynamically, when the user switches to
         --  the page.
         Gtk_New_Vbox (Box, Homogeneous => False);

         Gtk_New (Label, Get_Label (Page));
         Append_Page (Main_Note, Box, Label);
      end loop;

      Gtk_New_Vbox (Box, Homogeneous => False);
      Pack_Start (Main_Box, Box, Expand => True, Fill => True);

      Gtk_New (Label, -"Apply changes to:");
      Set_Alignment (Label, 0.0, 0.0);
      Pack_Start (Box, Label, Expand => False);

      Gtk_New (Prj_Selector, Kernel, Get_Project_From_View (Project_View));
      Pack_Start (Box, Prj_Selector, Expand => True, Fill => True);

      Gtk_New (Selector, Kernel);
      Pack_Start (Box, Selector, Expand => True, Fill => True);

      Button := Add_Button (Editor, Stock_Ok, Gtk_Response_OK);
      Button := Add_Button (Editor, Stock_Cancel, Gtk_Response_Cancel);
   end Initialize;

   -----------------
   -- Switch_Page --
   -----------------

   procedure Switch_Page
     (Notebook : access GObject_Record'Class;
      Editor   : GObject)
   is
      Note : Gtk_Notebook := Gtk_Notebook (Notebook);
      Ed : Properties_Editor := Properties_Editor (Editor);
      Page : Integer := Integer (Get_Current_Page (Note));
   begin
      if Page >= 1
        and then not Gtk.Object.In_Destruction_Is_Set (Ed)
      then
         if Ed.Pages (Page) = null then
            Ed.Pages (Page) := Widget_Factory
              (Get_Nth_Project_Editor_Page (Ed.Kernel, Page),
               Ed.Project_View, Ed.Kernel);
            Pack_Start
              (Gtk_Box (Get_Nth_Page (Note, Gint (Page))), Ed.Pages (Page));

         else
            declare
               Languages : Argument_List := Get_Languages (Ed);
            begin
               Refresh
                 (Page        => Get_Nth_Project_Editor_Page (Ed.Kernel, Page),
                  Widget       => Ed.Pages (Page),
                  Project_View => Ed.Project_View,
                  Languages    => Get_Languages (Ed));
               Free (Languages);
            end;
         end if;
      end if;
   end Switch_Page;

   ---------------------
   -- Browse_Location --
   ---------------------

   procedure Browse_Location (Editor : access Gtk_Widget_Record'Class) is
      Ed : constant Properties_Editor := Properties_Editor (Editor);
      Name : constant String := Select_Directory
        (-"Select project file location",
         Base_Directory => Name_As_Directory (Get_Text (Ed.Path)));
   begin
      if Name /= "" then
         Set_Text (Ed.Path, Name);
      end if;
   end Browse_Location;

   -------------------
   -- Get_Languages --
   -------------------

   function Get_Languages (Editor : access Properties_Editor_Record'Class)
      return Argument_List
   is
      New_Languages : Argument_List (Editor.Languages'Range);
      Num_Languages : Natural := New_Languages'First;
      Check         : Gtk_Check_Button;
      Ent           : Gtk_Entry;
      Languages     : Argument_List := Known_Languages
        (Get_Language_Handler (Editor.Kernel));

   begin
      for J in Editor.Languages'Range loop
         Check := Gtk_Check_Button (Editor.Languages (J));
         Ent   := Gtk_GEntry (Editor.Compilers (J));

         if Get_Active (Check) then
            New_Languages (Num_Languages) := new String' (Languages (J).all);
            Num_Languages := Num_Languages + 1;
         end if;
      end loop;
      Free (Languages);
      return New_Languages (New_Languages'First .. Num_Languages - 1);
   end Get_Languages;

   ---------------------
   -- Edit_Properties --
   ---------------------

   procedure Edit_Properties
     (Project_View : Prj.Project_Id;
      Kernel       : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      procedure Report_Error (Msg : String);
      --  Report an error to the console

      ------------------
      -- Report_Error --
      ------------------

      procedure Report_Error (Msg : String) is
      begin
         Insert (Kernel, Msg);
      end Report_Error;

      Editor  : Properties_Editor;
      Changed : Boolean := False;
      Project : constant Project_Node_Id :=
        Get_Project_From_View (Project_View);
      Project_Languages : Argument_List := Get_Languages (Project_View);
      Languages : Argument_List := Known_Languages
        (Get_Language_Handler (Kernel));
      Ent : Gtk_GEntry;
      Check : Gtk_Check_Button;
      Response : Gtk_Response_Type;
      Response2 : Message_Dialog_Buttons;

   begin
      Gtk_New (Editor, Project_View, Kernel);
      Show_All (Editor);

      loop
         Response := Run (Editor);
         exit when Response /= Gtk_Response_OK
           or else Is_Directory (Name_As_Directory (Get_Text (Editor.Path)));

         Response2 := Message_Dialog
           (Msg     => Name_As_Directory (Get_Text (Editor.Path))
            & (-" is not a valid directory"),
            Buttons => Button_OK,
            Title   => -"Error",
            Parent  => Get_Main_Window (Kernel));
      end loop;

      if Response = Gtk_Response_OK then
         declare
            New_Name : constant String := Get_Text (Editor.Name);
            New_Path : constant String :=
              Name_As_Directory (Get_Text (Editor.Path));
            Relative : Boolean := Get_Active
              (Editor.Use_Relative_Paths);

         begin
            --  We normalize the project automatically. If the project is not
            --  modified after all, it doesn't matter since we are not going to
            --  save the project.
            if not Has_Been_Normalized (Project) then
               Normalize (Project, Recurse => False);
            end if;

            --  If we are moving the project through the GUI, then we need to
            --  convert the paths to absolute or the semantics changes.
            if New_Name /= Project_Name (Project_View)
              or else New_Path /= Dir_Name (Project_Path (Project_View))
            then
               Relative := False;
            end if;

            --  Convert the paths if necessary
            if Relative /= Project_Uses_Relative_Paths (Kernel, Project) then
               Set_Project_Uses_Relative_Paths (Kernel, Project, Relative);
               Changed := Changed
                 or Convert_Paths (Project                => Project,
                                   Use_Relative_Paths     => Relative,
                                   Update_With_Statements => True);
            end if;

            if New_Name /= Project_Name (Project_View)
              or else New_Path /= Dir_Name (Project_Path (Project_View))
            then
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
            end if;

            if Get_Text (Editor.Gnatls) /= Get_Attribute_Value
              (Project_View, Gnatlist_Attribute, Ide_Package)
            then
               Update_Attribute_Value_In_Scenario
                 (Project            => Project,
                  Pkg_Name           => Ide_Package,
                  Scenario_Variables => Scenario_Variables (Kernel),
                  Attribute_Name     => Gnatlist_Attribute,
                  Value              => Get_Text (Editor.Gnatls));
               Changed := True;
            end if;

            if Get_Text (Editor.Debugger) /= Get_Attribute_Value
              (Project_View, Debugger_Command_Attribute, Ide_Package)
            then
               Update_Attribute_Value_In_Scenario
                 (Project            => Project,
                  Pkg_Name           => Ide_Package,
                  Scenario_Variables => Scenario_Variables (Kernel),
                  Attribute_Name     => Debugger_Command_Attribute,
                  Value              => Get_Text (Editor.Debugger));
               Changed := True;
            end if;

            declare
               New_Languages : constant Argument_List :=
                 Get_Languages (Editor);
            begin
               for J in Editor.Languages'Range loop
                  Check := Gtk_Check_Button (Editor.Languages (J));
                  Ent   := Gtk_GEntry (Editor.Compilers (J));

                  if Get_Active (Check)
                    and then Get_Attribute_Value
                      (Project_View, Compiler_Command_Attribute,
                       Ide_Package, Default => "",
                       Index => Languages (J).all) /= Get_Text (Ent)
                  then
                     Update_Attribute_Value_In_Scenario
                       (Project  => Project,
                        Pkg_Name => Ide_Package,
                        Scenario_Variables => Scenario_Variables (Kernel),
                        Attribute_Name => Compiler_Command_Attribute,
                        Value => Get_Text (Ent),
                        Attribute_Index => Languages (J).all);
                     Changed := True;
                  end if;
               end loop;

               if not Is_Equal
                 (New_Languages, Project_Languages, Case_Sensitive => False)
               then
                  Changed := True;
                  Update_Attribute_Value_In_Scenario
                    (Project           => Project,
                     Pkg_Name          => "",
                     Scenario_Variables => Scenario_Variables (Kernel),
                     Attribute_Name     => Languages_Attribute,
                     Values             => New_Languages);
               end if;
            end;

            if Get_Text (Editor.Tools_Host) /= Get_Attribute_Value
              (Project_View, Remote_Host_Attribute,
               Ide_Package, Default => "")
            then
               Changed := True;
               Update_Attribute_Value_In_Scenario
                 (Project            => Project,
                  Pkg_Name           => Ide_Package,
                  Scenario_Variables => Scenario_Variables (Kernel),
                  Attribute_Name     => Remote_Host_Attribute,
                  Value              => Get_Text (Editor.Tools_Host));
            end if;

            if Get_Text (Editor.Program_Host) /= Get_Attribute_Value
              (Project_View, Program_Host_Attribute,
               Ide_Package, Default => "")
            then
               Changed := True;
               Update_Attribute_Value_In_Scenario
                 (Project            => Project,
                  Pkg_Name           => Ide_Package,
                  Scenario_Variables => Scenario_Variables (Kernel),
                  Attribute_Name     => Program_Host_Attribute,
                  Value              => Get_Text (Editor.Program_Host));
            end if;

            if Get_Text (Editor.Protocol) /= Get_Attribute_Value
              (Project_View, Protocol_Attribute,
               Ide_Package, Default => "")
            then
               Changed := True;
               Update_Attribute_Value_In_Scenario
                 (Project            => Project,
                  Pkg_Name           => Ide_Package,
                  Scenario_Variables => Scenario_Variables (Kernel),
                  Attribute_Name     => Protocol_Attribute,
                  Value              => Get_Text (Editor.Protocol));
            end if;
         end;

         for P in Editor.Pages'Range loop
            if Editor.Pages (P) /= null then
               Changed := Changed or Project_Editor
                 (Get_Nth_Project_Editor_Page (Kernel, P),
                  Project, Project_View, Kernel, Editor.Pages (P));
            end if;
         end loop;

         if Changed then
            Set_Project_Modified (Kernel, Project, True);
            Recompute_View (Kernel);
         end if;
      end if;

      Destroy (Editor);
      Free (Languages);
      Free (Project_Languages);
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
   end Edit_Project_Properties;

end Project_Properties;
