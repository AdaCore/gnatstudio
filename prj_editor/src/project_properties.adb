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
with Gtk.GEntry;                use Gtk.GEntry;
with Gtk.Handlers;              use Gtk.Handlers;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Notebook;              use Gtk.Notebook;
with Gtk.Object;                use Gtk.Object;
with Gtk.Paned;                 use Gtk.Paned;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Table;                 use Gtk.Table;
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
         Name        : Gtk.GEntry.Gtk_Entry;
         Path        : Gtk.GEntry.Gtk_Entry;
         Gnatls      : Gtk.GEntry.Gtk_Entry;
         Compiler    : Gtk.GEntry.Gtk_Entry;
         Debugger    : Gtk.GEntry.Gtk_Entry;
         Compilers   : Widget_Array_Access;
         Languages   : Widget_Array_Access;
         Use_Relative_Paths : Gtk.Check_Button.Gtk_Check_Button;

         Pages       : Widget_Array_Access;
         --  The pages that have been registered.

         Project_View : Prj.Project_Id;
         Kernel       : Kernel_Handle;
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
      Table, Lang  : Gtk_Table;
      Languages : Argument_List := Known_Languages
        (Get_Language_Handler (Kernel));
      Project_Languages : Argument_List :=  Get_Languages (Project_View);
      Ent     : Gtk_GEntry;
      Project : constant Project_Node_Id := Get_Project_From_View
        (Project_View);
      Combo        : Gtk_Combo;
      Items        : Gtk.Enums.String_List.Glist;

      use Gtk.Enums.String_List;

   begin
      Gtk_New (Table, Rows => 6, Columns => 3, Homogeneous => False);

      Gtk_New (Label, -"Name:");
      Set_Alignment (Label, 0.0, 0.0);
      Attach (Table, Label, 0, 1, 0, 1, Xoptions => Fill, Yoptions => 0);
      Gtk_New (Editor.Name);
      Attach (Table, Editor.Name, 1, 3, 0, 1, Yoptions => 0);

      Set_Text (Editor.Name, Project_Name (Project_View));

      Gtk_New (Label, -"Path:");
      Set_Alignment (Label, 0.0, 0.0);
      Attach (Table, Label, 0, 1, 1, 2, Xoptions => Fill, Yoptions => 0);
      Gtk_New (Editor.Path);
      Attach (Table, Editor.Path, 1, 2, 1, 2, Yoptions => 0);
      Set_Width_Chars (Editor.Path, 20);
      Gtk_New (Button2, -"Browse");
      Attach (Table, Button2, 2, 3, 1, 2, Xoptions => 0, Yoptions => 0);

      Set_Text (Editor.Path, Dir_Name (Project_Path (Project_View)));

      Widget_Callback.Object_Connect
        (Button2, "clicked",
         Widget_Callback.To_Marshaller (Browse_Location'Access),
         Slot_Object => Editor);

      --  ??? Should be a toggle button, with "relative" or "absolute"
      Gtk_New (Editor.Use_Relative_Paths, -"Paths should be relative paths");
      Set_Active
        (Editor.Use_Relative_Paths,
         Project_Uses_Relative_Paths (Kernel, Project));
      Attach (Table, Editor.Use_Relative_Paths, 1, 2, 2, 3, Yoptions => 0);

      Gtk_New (Label, -"Gnatls:");
      Set_Alignment (Label, 0.0, 0.0);
      Attach (Table, Label, 0, 1, 3, 4, Xoptions => Fill, Yoptions => 0);

      Gtk_New (Combo);

      --  ??? Would be nice to specify the list of available cross compilers
      --  using a configuration file

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
      Attach (Table, Combo, 1, 3, 3, 4, Yoptions => 0);
      Editor.Gnatls := Get_Entry (Combo);
      Set_Text
        (Editor.Gnatls,
         Get_Attribute_Value
         (Project_View, Gnatlist_Attribute,
          Ide_Package, Default => "gnatls"));

      Gtk_New (Label, -"Debugger:");
      Set_Alignment (Label, 0.0, 0.0);
      Attach (Table, Label, 0, 1, 4, 5, Xoptions => Fill, Yoptions => 0);

      Gtk_New (Combo);
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
      Attach (Table, Combo, 1, 3, 4, 5, Yoptions => 0);
      Editor.Debugger := Get_Entry (Combo);
      Set_Text
        (Editor.Debugger,
         Get_Attribute_Value
         (Project_View, Debugger_Command_Attribute,
          Ide_Package, Default => "gdb"));

      Gtk_New (Label, (-"Languages") & " & " & ASCII.LF & (-"Compilers:"));
      Set_Alignment (Label, 0.0, 0.0);
      Attach (Table, Label, 0, 1, 5, 6, Xoptions => Fill, Yoptions => 0);
      Gtk_New (Lang, Rows => Languages'Length, Columns => 2,
               Homogeneous => False);
      Attach (Table, Lang, 1, 3, 5, 6, Yoptions => 0);

      Editor.Compilers := new Widget_Array (Languages'Range);
      Editor.Languages := new Widget_Array (Languages'Range);

      for L in Languages'Range loop
         Gtk_New (Check, Languages (L).all);
         Attach
           (Lang, Check, 0, 1,
            Guint (L - Languages'First),
            Guint (L - Languages'First + 1),
            Xoptions => Fill);

         if To_Lower (Languages (L).all) = "ada" then
            Gtk_New (Combo);
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
            Attach (Lang, Combo, 1, 2,
                    Guint (L - Languages'First),
                    Guint (L - Languages'First + 1));
            Ent := Get_Entry (Combo);
            Set_Text
              (Ent,
               Get_Attribute_Value
               (Project_View, Compiler_Command_Attribute,
                Ide_Package, Default => "gnatmake",
                Index => Languages (L).all));

         else
            Gtk_New (Ent);
            Set_Sensitive (Ent, False);
            Attach (Lang, Ent, 1, 2,
                    Guint (L - Languages'First),
                    Guint (L - Languages'First + 1));
            Set_Text
              (Ent,
               Get_Attribute_Value
               (Project_View, Compiler_Command_Attribute,
                Ide_Package, Default => "gcc",
                Index => Languages (L).all));
         end if;

         Editor.Languages (L) := Gtk_Widget (Check);
         Editor.Compilers (L) := Gtk_Widget (Ent);

         Object_User_Callback.Connect
           (Check, "toggled",
            Object_User_Callback.To_Marshaller (Command_Set_Sensitive'Access),
            User_Data => GObject (Ent));

         Set_Active (Check, False);

         for PL in Project_Languages'Range loop
            if Project_Languages (PL).all = To_Lower (Languages (L).all) then
               Set_Active (Check, True);
            end if;
         end loop;
      end loop;

      Free (Languages);
      Free (Project_Languages);

      return Gtk_Widget (Table);
   end Create_General_Page;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Editor       : access Properties_Editor_Record'Class;
      Project_View : Prj.Project_Id;
      Kernel       : access Kernel_Handle_Record'Class)
   is
      Label     : Gtk_Label;
      Main_Note : Gtk_Notebook;
      Button    : Gtk_Widget;
      Page      : Project_Editor_Page;
      Box       : Gtk_Box;
      Paned     : Gtk_Paned;
      Selector  : Scenario_Selector;
      Prj_Selector : Project_Selector;

   begin
      Gtk.Dialog.Initialize
        (Dialog => Editor,
         Title  => -"Properties for "
           & Project_Name (Project_View),
         Parent => Get_Main_Window (Kernel),
         Flags  => Modal or Destroy_With_Parent);
      Set_Policy (Editor,
                  Allow_Shrink => True,
                  Allow_Grow   => True,
                  Auto_Shrink  => True);
      Set_Default_Size (Editor, -1, 470);
      Realize (Editor);

      Widget_Callback.Connect
        (Editor, "destroy",
         Widget_Callback.To_Marshaller (Destroyed'Access));

      Gtk_New_Hpaned (Paned);
      Pack_Start (Get_Vbox (Editor), Paned, Expand => True, Fill => True);

      Gtk_New (Main_Note);
      Set_Tab_Pos (Main_Note, Pos_Left);
      Pack1 (Paned, Main_Note, Resize => True);

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
      Editor.Kernel := Kernel_Handle (Kernel);
      Editor.Pages := new Widget_Array
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
      Pack2 (Paned, Box, Resize => True);

      Gtk_New (Label, -"Apply changes to:");
      Set_Alignment (Label, 0.0, 0.0);
      Pack_Start (Box, Label, Expand => False, Fill => True);

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
            Show_All (Ed.Pages (Page));

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
