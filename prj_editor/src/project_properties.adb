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
with Glib.Values;               use Glib.Values;
with Glide_Intl;                use Glide_Intl;
with Glide_Kernel.Console;      use Glide_Kernel.Console;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with Glide_Kernel.Project;      use Glide_Kernel.Project;
with Glide_Kernel;              use Glide_Kernel;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Button;                use Gtk.Button;
with Gtk.Cell_Renderer_Text;    use Gtk.Cell_Renderer_Text;
with Gtk.Check_Button;          use Gtk.Check_Button;
with Gtkada.Dialogs;            use Gtkada.Dialogs;
with Gtkada.File_Selector;      use Gtkada.File_Selector;
with Gtkada.Handlers;           use Gtkada.Handlers;
with Gtk.Combo;                 use Gtk.Combo;
with Gtk.Dialog;                use Gtk.Dialog;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.GEntry;                use Gtk.GEntry;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Table;                 use Gtk.Table;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Tree_Selection;        use Gtk.Tree_Selection;
with Gtk.Tree_View;             use Gtk.Tree_View;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;
with Gtk.List_Store;            use Gtk.List_Store;
with Gtk.Vbutton_Box;           use Gtk.Vbutton_Box;
with Gtk.Widget;                use Gtk.Widget;
with Prj;                       use Prj;
with Prj.Tree;                  use Prj.Tree;
with Prj_API;                   use Prj_API;
with String_Utils;              use String_Utils;
with Basic_Types;               use Basic_Types;
with Language_Handlers;         use Language_Handlers;
with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Unchecked_Deallocation;

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
      Executables : Gtk_List_Store;
      Tree_View   : Gtk_Tree_View;
      Gnatls      : Gtk.GEntry.Gtk_Entry;
      Compiler    : Gtk.GEntry.Gtk_Entry;
      Debugger    : Gtk.GEntry.Gtk_Entry;
      Convert     : Gtk.Check_Button.Gtk_Check_Button;
      Compilers   : Widget_Array_Access;
      Languages   : Widget_Array_Access;
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

   procedure Add_Main_Unit (Editor : access Gtk_Widget_Record'Class);
   --  Add a main unit to the list of main units for the edited project

   procedure Remove_Main_Unit (Editor : access Gtk_Widget_Record'Class);
   --  Remove the selected main units.

   procedure Add_Main_File
     (Editor : access Properties_Editor_Record'Class;
      File   : String);
   --  Add a new file entry in the list of main units.

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

   -------------------
   -- Add_Main_File --
   -------------------

   procedure Add_Main_File
     (Editor : access Properties_Editor_Record'Class;
      File   : String)
   is
      Val : GValue;
      Iter : Gtk_Tree_Iter;
   begin
      Init (Val, GType_String);
      Set_String (Val, File);
      Append (Editor.Executables, Iter);
      Set_Value
        (Editor.Executables,
         Iter   => Iter,
         Column => 0,
         Value  => Val);
      Unset (Val);
   end Add_Main_File;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Editor       : access Properties_Editor_Record'Class;
      Project_View : Prj.Project_Id;
      Kernel       : access Kernel_Handle_Record'Class)
   is
      Button       : Gtk_Widget;
      Button2      : Gtk_Button;
      Label        : Gtk_Label;
      Check        : Gtk_Check_Button;
      Table, Lang  : Gtk_Table;
      Languages    : String_Array := Known_Languages
        (Get_Language_Handler (Kernel));
      Project_Languages : Argument_List := Get_Languages (Project_View);
      Ent          : Gtk_GEntry;
      Box          : Gtk_Box;
      Bbox         : Gtk_Vbutton_Box;
      Column       : Gtk_Tree_View_Column;
      Renderer     : Gtk_Cell_Renderer_Text;
      Col          : Gint;
      Scrolled     : Gtk_Scrolled_Window;
      Combo        : Gtk_Combo;
      Items        : Gtk.Enums.String_List.Glist;

      use Gtk.Enums.String_List;

   begin
      Gtk.Dialog.Initialize
        (Dialog => Editor,
         Title  => -"Properties for " & Project_Name (Project_View),
         Parent => Get_Main_Window (Kernel),
         Flags  => Modal or Destroy_With_Parent);
      Set_Policy (Editor,
                  Allow_Shrink => False,
                  Allow_Grow   => True,
                  Auto_Shrink  => False);

      Widget_Callback.Connect
        (Editor, "destroy",
         Widget_Callback.To_Marshaller (Destroyed'Access));

      Gtk_New (Table, Rows => 7, Columns => 3, Homogeneous => False);
      Pack_Start
        (Get_Vbox (Editor), Table, Expand => True, Fill => True);

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

      Widget_Callback.Object_Connect
        (Button2, "clicked",
         Widget_Callback.To_Marshaller (Browse_Location'Access),
         Slot_Object => Editor);

      Gtk_New (Editor.Convert, -"Convert paths to absolute");
      Attach (Table, Editor.Convert, 1, 2, 2, 3, Yoptions => 0);

      Set_Active (Editor.Convert, True);
      Set_Text (Editor.Path, Dir_Name (Project_Path (Project_View)));

      Gtk_New (Label, -"Main files:");
      Set_Alignment (Label, 0.0, 0.0);
      Attach (Table, Label, 0, 1, 3, 4, Xoptions => Fill);

      Gtk_New_Hbox (Box, Homogeneous => False);
      Attach (Table, Box, 1, 3, 3, 4);

      --  Create the module and the associated view
      Gtk_New (Editor.Executables, (1 => GType_String));
      Gtk_New (Editor.Tree_View, Model => Editor.Executables);
      Set_Mode (Get_Selection (Editor.Tree_View), Selection_Multiple);
      Set_Headers_Visible (Editor.Tree_View, False);
      Gtk_New (Renderer);
      Gtk_New (Column);
      Pack_Start (Column, Renderer, Expand => True);
      Add_Attribute (Column, Renderer, "text", 0);
      Col := Append_Column (Editor.Tree_View, Column);

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
      Set_Shadow_Type (Scrolled, Shadow_In);
      Add (Scrolled, Editor.Tree_View);
      Pack_Start (Box, Scrolled, Expand => True, Fill => True);
      Gtk_New (Bbox);
      Set_Layout (Bbox, Buttonbox_Start);
      Pack_Start (Box, Bbox, Expand => False, Fill => False);

      Gtk_New_From_Stock (Button2, Stock_Add);
      Pack_Start (Bbox, Button2);
      Widget_Callback.Object_Connect
        (Button2, "clicked",
         Widget_Callback.To_Marshaller (Add_Main_Unit'Access),
         Slot_Object => Editor);

      Gtk_New_From_Stock (Button2, Stock_Remove);
      Pack_Start (Bbox, Button2);
      Widget_Callback.Object_Connect
        (Button2, "clicked",
         Widget_Callback.To_Marshaller (Remove_Main_Unit'Access),
         Slot_Object => Editor);

      declare
         Mains : Argument_List := Get_Attribute_Value
           (Project_View,
            Attribute_Name => Main_Attribute);
      begin
         for M in Mains'Range loop
            Add_Main_File (Editor, Mains (M).all);
         end loop;

         Free (Mains);
      end;

      Gtk_New (Label, -"Gnatls:");
      Set_Alignment (Label, 0.0, 0.0);
      Attach (Table, Label, 0, 1, 4, 5, Xoptions => Fill, Yoptions => 0);

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
      Attach (Table, Combo, 1, 3, 4, 5, Yoptions => 0);
      Editor.Gnatls := Get_Entry (Combo);
      Set_Text
        (Editor.Gnatls,
         Get_Attribute_Value
           (Project_View, Gnatlist_Attribute,
            Ide_Package, Default => "gnatls"));

      Gtk_New (Label, -"Debugger:");
      Set_Alignment (Label, 0.0, 0.0);
      Attach (Table, Label, 0, 1, 5, 6, Xoptions => Fill, Yoptions => 0);

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
      Attach (Table, Combo, 1, 3, 5, 6, Yoptions => 0);
      Editor.Debugger := Get_Entry (Combo);
      Set_Text
        (Editor.Debugger,
         Get_Attribute_Value
           (Project_View, Debugger_Command_Attribute,
            Ide_Package, Default => "gdb"));

      Button := Add_Button (Editor, Stock_Ok, Gtk_Response_OK);
      Button := Add_Button (Editor, Stock_Cancel, Gtk_Response_Cancel);

      Gtk_New (Label, (-"Languages") & " & " & ASCII.LF & (-"Compilers:"));
      Set_Alignment (Label, 0.0, 0.0);
      Attach (Table, Label, 0, 1, 6, 7, Xoptions => Fill, Yoptions => 0);
      Gtk_New (Lang, Rows => Languages'Length, Columns => 2,
               Homogeneous => False);
      Attach (Table, Lang, 1, 3, 6, 7, Yoptions => 0);

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
   end Initialize;

   -------------------
   -- Add_Main_Unit --
   -------------------

   procedure Add_Main_Unit (Editor : access Gtk_Widget_Record'Class) is
      File : constant String := Select_File
        (Title => -"Select the main unit to add");
      --  ??? Would be nice to allow the selection of multiple files from the
      --  same dialog.
   begin
      if File /= "" then
         Add_Main_File (Properties_Editor (Editor), Base_Name (File));
      end if;
   end Add_Main_Unit;

   ----------------------
   -- Remove_Main_Unit --
   ----------------------

   procedure Remove_Main_Unit (Editor : access Gtk_Widget_Record'Class) is
      Ed : constant Properties_Editor := Properties_Editor (Editor);
      Iter, Tmp : Gtk_Tree_Iter;
      Selection : constant Gtk_Tree_Selection := Get_Selection (Ed.Tree_View);
   begin
      Iter := Get_Iter_First (Ed.Executables);
      while Iter /= Null_Iter loop
         Tmp := Iter;
         Next (Ed.Executables, Iter);

         if Iter_Is_Selected (Selection, Tmp) then
            Remove (Ed.Executables, Tmp);
         end if;
      end loop;
   end Remove_Main_Unit;

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
      Project : Project_Node_Id;
      Project_Languages : Argument_List := Get_Languages (Project_View);
      Num_Languages : Natural;
      Languages : String_Array := Known_Languages
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
         exit when Response /= Gtk_Response_OK;

         if not Is_Directory (Name_As_Directory (Get_Text (Editor.Path))) then
            Response2 := Message_Dialog
              (Msg     => Name_As_Directory (Get_Text (Editor.Path))
                 & (-" is not a valid directory"),
               Buttons => Button_OK,
               Title   => -"Error",
               Parent  => Get_Main_Window (Kernel));
         else
            exit;
         end if;

      end loop;

      if Response = Gtk_Response_OK then
         declare
            New_Name : constant String := Get_Text (Editor.Name);
            New_Path : constant String :=
              Name_As_Directory (Get_Text (Editor.Path));

         begin
            Project := Get_Project_From_View (Project_View);

            if New_Name /= Project_Name (Project_View)
              or else New_Path /= Dir_Name (Project_Path (Project_View))
            then
               if Get_Active (Editor.Convert) then
                  --  The with statements will be automatically updated by the
                  --  call to Rename_And_Move.
                  Convert_Paths_To_Absolute (Project, False);
               end if;
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
               Num_Children : constant Gint := N_Children (Editor.Executables);
               New_Mains : Argument_List (1 .. Integer (Num_Children));
               Iter : Gtk_Tree_Iter := Get_Iter_First (Editor.Executables);
               N : Natural := New_Mains'First;
               Old_Mains : Argument_List := Get_Attribute_Value
                 (Project_View,
                  Attribute_Name => Main_Attribute);
            begin
               --  First get the list of main files
               while Iter /= Null_Iter loop
                  New_Mains (N) := new String'
                    (Get_String (Editor.Executables, Iter, 0));
                  N := N + 1;
                  Next (Editor.Executables, Iter);
               end loop;

               if not Is_Equal (Old_Mains, New_Mains) then
                  Changed := True;
                  Update_Attribute_Value_In_Scenario
                    (Project  => Project,
                     Pkg_Name => "",
                     Scenario_Variables => Scenario_Variables (Kernel),
                     Attribute_Name => Main_Attribute,
                     Values => New_Mains);
               end if;

               Free (Old_Mains);
               Free (New_Mains);
            end;

            declare
               New_Languages : Argument_List (Editor.Languages'Range);
            begin
               Num_Languages := New_Languages'First;
               for J in Editor.Languages'Range loop
                  Check := Gtk_Check_Button (Editor.Languages (J));
                  Ent   := Gtk_GEntry (Editor.Compilers (J));

                  if Get_Active (Check) then
                     New_Languages (Num_Languages) := GNAT.OS_Lib.String_Access
                       (Languages (J));
                     Num_Languages := Num_Languages + 1;

                     if Get_Attribute_Value
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
                  end if;
               end loop;

               if not Is_Equal
                 (New_Languages (New_Languages'First .. Num_Languages - 1),
                  Project_Languages,
                  Case_Sensitive => False)
               then
                  Changed := True;
                  Update_Attribute_Value_In_Scenario
                    (Project           => Project,
                     Pkg_Name          => "",
                     Scenario_Variables => Scenario_Variables (Kernel),
                     Attribute_Name     => Languages_Attribute,
                     Values             => New_Languages
                       (New_Languages'First .. Num_Languages - 1));
               end if;
            end;
         end;

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
