-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                        Copyright (C) 2002                         --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
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

with Glib;                      use Glib;
with Glib.Object;               use Glib.Object;
with Glide_Intl;                use Glide_Intl;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with Glide_Kernel.Project;      use Glide_Kernel.Project;
with Glide_Kernel;              use Glide_Kernel;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Button;                use Gtk.Button;
with Gtk.Check_Button;          use Gtk.Check_Button;
with Gtkada.File_Selector;      use Gtkada.File_Selector;
with Gtkada.Handlers;           use Gtkada.Handlers;
with Gtk.Dialog;                use Gtk.Dialog;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.GEntry;                use Gtk.GEntry;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Text;                  use Gtk.Text;
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

package body Project_Properties is

   type Widget_Array is array (Natural range <>) of Gtk_Widget;
   type Widget_Array_Access is access Widget_Array;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Widget_Array, Widget_Array_Access);

   type Properties_Editor_Record is new Gtk.Dialog.Gtk_Dialog_Record with
      record
         Name        : Gtk.GEntry.Gtk_Entry;
         Path        : Gtk.GEntry.Gtk_Entry;
         Executables : Gtk.Text.Gtk_Text;
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

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Editor       : access Properties_Editor_Record'Class;
      Project_View : Prj.Project_Id;
      Kernel       : access Kernel_Handle_Record'Class)
   is
      Button  : Gtk_Widget;
      Button2 : Gtk_Button;
      Label   : Gtk_Label;
      Check   : Gtk_Check_Button;
      Table, Lang  : Gtk_Table;
      Languages : String_Array := Known_Languages
        (Get_Language_Handler (Kernel));
      Project_Languages : Argument_List :=
        Get_Attribute_Value (Project_View, Languages_Attribute);
      Ent     : Gtk_GEntry;
   begin
      Gtk.Dialog.Initialize
        (Dialog => Editor,
         Title  => -"Properties for "
           & Project_Name (Project_View),
         Parent => Get_Main_Window (Kernel),
         Flags  => Modal or Destroy_With_Parent);

      Widget_Callback.Connect
        (Editor, "destroy",
         Widget_Callback.To_Marshaller (Destroyed'Access));

      Gtk_New (Table, Rows => 7, Columns => 3, Homogeneous => False);
      Pack_Start
        (Get_Vbox (Editor), Table, Expand => True, Fill => True);

      Gtk_New (Label, -"Name:");
      Set_Alignment (Label, 0.0, 0.0);
      Attach (Table, Label, 0, 1, 0, 1, Xoptions => Fill);
      Gtk_New (Editor.Name);
      Attach (Table, Editor.Name, 1, 3, 0, 1);

      Set_Text (Editor.Name, Project_Name (Project_View));

      Gtk_New (Label, -"Path:");
      Set_Alignment (Label, 0.0, 0.0);
      Attach (Table, Label, 0, 1, 1, 2, Xoptions => Fill);
      Gtk_New (Editor.Path);
      Attach (Table, Editor.Path, 1, 2, 1, 2);
      Set_Width_Chars (Editor.Path, 40);
      Gtk_New (Button2, -"Browse");
      Attach (Table, Button2, 2, 3, 1, 2, Xoptions => 0);

      Widget_Callback.Object_Connect
        (Button2, "clicked",
         Widget_Callback.To_Marshaller (Browse_Location'Access),
         Slot_Object => Editor);

      Gtk_New (Editor.Convert, "Convert paths to absolute");
      Attach (Table, Editor.Convert, 1, 2, 2, 3);

      Set_Active (Editor.Convert, True);
      Set_Text (Editor.Path, Dir_Name (Project_Path (Project_View)));

      Gtk_New (Label, -"Main files:");
      Set_Alignment (Label, 0.0, 0.0);
      Attach (Table, Label, 0, 1, 3, 4, Xoptions => Fill);
      Gtk_New (Editor.Executables);
      Attach (Table, Editor.Executables, 1, 3, 3, 4);

      Set_Sensitive (Editor.Executables, False);
      Set_Sensitive (Label, False);

      Gtk_New (Label, -"Gnatls:");
      Set_Alignment (Label, 0.0, 0.0);
      Attach (Table, Label, 0, 1, 4, 5, Xoptions => Fill);
      Gtk_New (Editor.Gnatls);
      Attach (Table, Editor.Gnatls, 1, 3, 4, 5);
      Set_Text
        (Editor.Gnatls,
         Get_Attribute_Value (Project_View, Gnatlist_Attribute, Ide_Package));

      Gtk_New (Label, -"Debugger:");
      Set_Alignment (Label, 0.0, 0.0);
      Attach (Table, Label, 0, 1, 5, 6, Xoptions => Fill);
      Gtk_New (Editor.Debugger);
      Attach (Table, Editor.Debugger, 1, 3, 5, 6);
      Set_Text
        (Editor.Debugger,
         Get_Attribute_Value
           (Project_View, Debugger_Command_Attribute, Ide_Package));

      Button := Add_Button (Editor, Stock_Ok, Gtk_Response_OK);
      Button := Add_Button (Editor, Stock_Cancel, Gtk_Response_Cancel);

      Gtk_New (Label, (-"Languages") & " & " & ASCII.LF & (-"Compilers:"));
      Set_Alignment (Label, 0.0, 0.0);
      Attach (Table, Label, 0, 1, 6, 7, Xoptions => Fill);
      Gtk_New (Lang, Rows => Languages'Length, Columns => 2,
               Homogeneous => False);
      Attach (Table, Lang, 1, 3, 6, 7);

      Editor.Compilers := new Widget_Array (Languages'Range);
      Editor.Languages := new Widget_Array (Languages'Range);

      for L in Languages'Range loop
         Gtk_New (Check, Languages (L).all);
         Attach (Lang, Check, 0, 1,
                 Guint (L - Languages'First),
                 Guint (L - Languages'First + 1),
                 Xoptions => Fill);

         Gtk_New (Ent);
         Set_Sensitive (Ent, False);
         Attach (Lang, Ent, 1, 2,
                 Guint (L - Languages'First),
                 Guint (L - Languages'First + 1));
         Set_Text (Ent,
                   Get_Attribute_Value
                     (Project_View, Compiler_Command_Attribute,
                      Ide_Package, Default => "",
                      Index => Languages (L).all));

         Editor.Languages (L) := Gtk_Widget (Check);
         Editor.Compilers (L) := Gtk_Widget (Ent);

         Object_User_Callback.Connect
           (Check, "toggled",
            Object_User_Callback.To_Marshaller (Command_Set_Sensitive'Access),
            User_Data => GObject (Ent));

         Set_Active (Check, False);

         for PL in Project_Languages'Range loop
            if To_Lower (Project_Languages (PL).all) =
              To_Lower (Languages (L).all)
            then
               Set_Active (Check, True);
            end if;
         end loop;
      end loop;

      Free (Languages);
      Free (Project_Languages);
   end Initialize;

   ---------------------
   -- Browse_Location --
   ---------------------

   procedure Browse_Location (Editor : access Gtk_Widget_Record'Class) is
      Ed : Properties_Editor := Properties_Editor (Editor);
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
      Editor  : Properties_Editor;
      Changed : Boolean := False;
      Project : Project_Node_Id;
      Project_Languages : Argument_List :=
        Get_Attribute_Value (Project_View, Languages_Attribute);
      Num_Languages : Natural;
      Languages : String_Array := Known_Languages
        (Get_Language_Handler (Kernel));
      Ent : Gtk_GEntry;
      Check : Gtk_Check_Button;

   begin
      Gtk_New (Editor, Project_View, Kernel);
      Show_All (Editor);

      if Run (Editor) = Gtk_Response_OK then
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
                 (Root_Project => Get_Project (Kernel),
                  Project      => Project,
                  New_Name     => New_Name,
                  New_Path     => New_Path);
               Changed := True;
            end if;

            if Get_Text (Editor.Gnatls) /= Get_Attribute_Value
              (Project_View, Gnatlist_Attribute, Ide_Package)
            then
               Update_Attribute_Value_In_Scenario
                 (Project            => Get_Project_From_View (Project_View),
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
                 (Project            => Get_Project_From_View (Project_View),
                  Pkg_Name           => Ide_Package,
                  Scenario_Variables => Scenario_Variables (Kernel),
                  Attribute_Name     => Debugger_Command_Attribute,
                  Value              => Get_Text (Editor.Debugger));
               Changed := True;
            end if;

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
                          (Project  => Get_Project_From_View (Project_View),
                           Pkg_Name => Ide_Package,
                           Scenario_Variables => Scenario_Variables (Kernel),
                           Attribute_Name => Compiler_Command_Attribute,
                           Value => Get_Text (Ent),
                           Attribute_Index => Languages (J).all);
                        Changed := True;
                     end if;
                  end if;
               end loop;

               if Num_Languages - New_Languages'First /=
                 Project_Languages'Length
               then
                  Changed := True;
               else
                  for N in New_Languages'First .. Num_Languages - 1 loop
                     if New_Languages (N).all /= Project_Languages
                       (N + Project_Languages'First - New_Languages'First).all
                     then
                        Changed := True;
                        exit;
                     end if;
                  end loop;
               end if;

               if Changed then
                  Update_Attribute_Value_In_Scenario
                    (Project           => Get_Project_From_View (Project_View),
                     Pkg_Name          => "",
                     Scenario_Variables => Scenario_Variables (Kernel),
                     Attribute_Name     => Languages_Attribute,
                     Values             => New_Languages
                       (New_Languages'First .. Num_Languages - 1));
               end if;
            end;
         end;

         if Changed then
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
      C : File_Selection_Context_Access :=
        File_Selection_Context_Access (Context);
   begin
      Edit_Properties (Project_Information (C), Get_Kernel (Context));
   end Edit_Project_Properties;

end Project_Properties;
