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

with Ada.Unchecked_Deallocation;
with GNAT.Case_Util;            use GNAT.Case_Util;
with GNAT.Strings;              use GNAT.Strings;
with GNATCOLL.Utils;            use GNATCOLL.Utils;

with Glib;                      use Glib;
with Glib.Object;               use Glib.Object;

with Gtk.Box;                   use Gtk.Box;
with Gtk.Button;                use Gtk.Button;
with Gtk.Dialog;                use Gtk.Dialog;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Widget;                use Gtk.Widget;

with Gtkada.File_Selector;      use Gtkada.File_Selector;
with Gtkada.Handlers;           use Gtkada.Handlers;

with Commands.Interactive;      use Commands, Commands.Interactive;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel;                use GPS.Kernel;
with Language_Handlers;         use Language_Handlers;
with Scenario_Selectors;        use Scenario_Selectors;
with Switches_Chooser.Gtkada;   use Switches_Chooser, Switches_Chooser.Gtkada;
with Traces;                    use Traces;
with Histories;                 use Histories;
with GNATCOLL.Arg_Lists;        use GNATCOLL.Arg_Lists;
with GNATCOLL.VFS;              use GNATCOLL.VFS;

package body Switches_Editors is

   Me : constant Debug_Handle := Create ("Switches_Editors");

   -----------------------
   -- Local subprograms --
   -----------------------

   function Close_Switch_Editor
     (Switches  : access Switches_Edit_Record'Class;
      Project   : Project_Type;
      Files     : File_Array;
      Scenario  : access Scenario_Selector_Record'Class) return Boolean;
   --  Called when the user has closed a switch editor for a specific file.
   --  This modifies the edited project to reflect the changes done in the
   --  dialog.
   --  File_Name is the name of the file whose switches we are changing, or ""
   --  if we are changing the default switches.
   --  Return True if the switches were modified

   procedure Revert_To_Default (Switches : access Gtk_Widget_Record'Class);
   --  Revert to the default switches in the editor

   function Get_Switches
     (Switches : access Switches_Edit_Record'Class;
      Tool     : Tool_Properties_Record;
      Files    : File_Array;
      Use_Initial_Value : Boolean := True) return GNAT.Strings.String_List;
   --  Return the list of switches for Files, found in the package Pkg_Name,
   --  for a specific language, and for a specific list of switches. The
   --  returned array must be freed by the caller.

   procedure Fill_Editor
     (Switches  : access Switches_Edit_Record'Class;
      Project   : Project_Type;
      Files     : File_Array);
   --  Fill the editor

   procedure Page_Destroyed (Page : access Gtk_Widget_Record'Class);
   --  Called when a page is destroyed

   procedure Set_Visible_Pages
     (Editor    : access Switches_Edit_Record'Class;
      Languages : GNAT.Strings.String_List;
      Show_Only : Boolean;
      File_Specific : Boolean);
   --  Same as the public version, except that the pages are never hidden, only
   --  shown depending on the languages.
   --  File_Specific should be True if the editor is open for a specific set
   --  of files, as opposed to a project-wide setting.

   function Has_Supported_Language
     (Tool                : Tool_Properties_Record;
      Supported_Languages : GNAT.Strings.String_List) return Boolean;
   --  Return True if Page applies to one of the languages in
   --  Supported_Language

   --------------
   -- Get_Page --
   --------------

   function Get_Page
     (Editor : access Switches_Edit_Record'Class;
      Title  : String) return Switches_Editor_Page is
   begin
      if Editor.Pages /= null then
         for Num in Editor.Pages'Range loop
            if Editor.Pages (Num) /= null
              and then Editor.Pages (Num).Tool_Name.all = Title
            then
               return Editor.Pages (Num);
            end if;
         end loop;
      end if;

      return null;
   end Get_Page;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Page             : out Switches_Editor_Page;
      In_Editor        : Switches_Edit;
      Kernel           : access GPS.Kernel.Kernel_Handle_Record'Class;
      Tool             : GPS.Kernel.Tool_Properties_Record)
   is
      pragma Unreferenced (Kernel);
   begin
      Page := new Switches_Editor_Page_Record;
      Initialize
        (Editor             => Page,
         Config             => Tool.Config,
         Use_Native_Dialogs => Use_Native_Dialogs.Get_Pref,
         History            => null,
         Key                => No_Key);
      Widget_Callback.Connect (Page, Signal_Destroy, Page_Destroyed'Access);

      Page.Switches  := In_Editor;
      Page.Tool_Name := new String'(Tool.Tool_Name.all);
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Editor : out Switches_Edit;
      Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Tab    : Gtk_Label;
      T      : Integer;
      Tools  : constant Tool_Properties_Array := Get_All_Tools (Kernel);

   begin
      Editor := new Switches_Edit_Record;
      Gtk.Notebook.Initialize (Editor);
      Set_Scrollable (Editor);

      Editor.Kernel := Kernel_Handle (Kernel);
      Editor.Pages := new Pages_Array (1 .. Tools'Length);

      for P in Editor.Pages'Range loop
         T := P - Editor.Pages'First + 1;
         if Tools (T).Config /= null then
            Gtk_New
              (Page           => Editor.Pages (P),
               In_Editor      => Editor,
               Kernel         => Kernel,
               Tool           => Tools (T));
            Gtk_New (Tab, Editor.Pages (P).Tool_Name.all);
            Append_Page (Editor, Editor.Pages (P), Tab);
         end if;
      end loop;

      Set_Current_Page (Editor, 0);
   end Gtk_New;

   --------------------
   -- Page_Destroyed --
   --------------------

   procedure Page_Destroyed (Page : access Gtk_Widget_Record'Class) is
      P : constant Switches_Editor_Page := Switches_Editor_Page (Page);
   begin
      Free (P.Tool_Name);
   end Page_Destroyed;

   -----------------------
   -- Set_Visible_Pages --
   -----------------------

   procedure Set_Visible_Pages
     (Editor    : access Switches_Edit_Record;
      Languages : GNAT.Strings.String_List) is
   begin
      Set_Visible_Pages
        (Editor, Languages, Show_Only => False, File_Specific => False);
   end Set_Visible_Pages;

   -----------------------
   -- Set_Visible_Pages --
   -----------------------

   procedure Set_Visible_Pages
     (Editor    : access Switches_Edit_Record'Class;
      Languages : GNAT.Strings.String_List;
      Show_Only : Boolean;
      File_Specific : Boolean)
   is
      Current : Gint := Get_Current_Page (Editor);
      Tool    : Tool_Properties_Record;
   begin
      for P in Editor.Pages'Range loop
         if Editor.Pages (P) /= null then
            Tool := Get_Tool_Properties
              (Editor.Kernel, Editor.Pages (P).Tool_Name.all);

            if Has_Supported_Language (Tool, Languages)
              and then (not File_Specific
                        or else Tool.Project_Package.all /= Ide_Package)
            then
               Show (Editor.Pages (P));
            elsif not Show_Only then
               Hide (Editor.Pages (P));
            end if;
         end if;
      end loop;

      --  Work around an apparent bug in gtk+: when the contents of a page is
      --  hidden, and the shown again, it is always displayed on top of the
      --  current page in the notebook. We thus see the contents of two or more
      --  pages at the same time...
      if Current = -1 then
         Current := 0;
      end if;
      Set_Current_Page (Editor, Current);
   end Set_Visible_Pages;

   -----------------------
   -- Revert_To_Default --
   -----------------------

   procedure Revert_To_Default
     (Switches : access Gtk_Widget_Record'Class)
   is
      S : constant Switches_Edit := Switches_Edit (Switches);
   begin
      for P in S.Pages'Range loop
         if S.Pages (P) /= null then
            declare
               Tool : constant Tool_Properties_Record :=
                 Get_Tool_Properties (S.Kernel, S.Pages (P).Tool_Name.all);
               List : GNAT.Strings.String_List := Get_Switches
                 (S,
                  Tool,
                  Files => (1 .. 0 => GNATCOLL.VFS.No_File),
                  Use_Initial_Value => True);
            begin
               Set_Command_Line (S.Pages (P), List);
               Free (List);
            end;
         end if;
      end loop;

   exception
      when E : others => Trace (Exception_Handle, E);
   end Revert_To_Default;

   ----------------------------
   -- Has_Supported_Language --
   ----------------------------

   function Has_Supported_Language
     (Tool                : Tool_Properties_Record;
      Supported_Languages : GNAT.Strings.String_List) return Boolean
   is
   begin
      if Tool.Languages /= null then
         for L in Tool.Languages'Range loop
            for PL in Supported_Languages'Range loop
               if Equal
                 (Tool.Languages (L).all,
                  Supported_Languages (PL).all,
                  Case_Sensitive => False)
               then
                  return True;
               end if;
            end loop;
         end loop;
      else
         --  No filter => always save those switches
         return True;
      end if;
      return False;
   end Has_Supported_Language;

   ----------------------
   -- Generate_Project --
   ----------------------

   function Generate_Project
     (Switches           : access Switches_Edit_Record'Class;
      Project            : Project_Type;
      Languages          : GNAT.Strings.String_List;
      Scenario_Variables : Scenario_Variable_Array;
      Files              : File_Array) return Boolean
   is
      Changed : Boolean := False;

      procedure Change_Switches
        (Page      : access Switches_Editor_Page_Record'Class;
         Tool      : Tool_Properties_Record;
         File_Name : Virtual_File);
      --  Changes the switches for a specific package and tool

      procedure Process_File (File_Name : Virtual_File);
      --  Generate the switches for a specific file (or the default switches if
      --  File_Name is the empty string). Return True if the project was
      --  changed.

      ---------------------
      -- Change_Switches --
      ---------------------

      procedure Change_Switches
        (Page      : access Switches_Editor_Page_Record'Class;
         Tool      : Tool_Properties_Record;
         File_Name : Virtual_File)
      is
         Is_Default_Value : Boolean;
         To_Remove        : Boolean := False;
      begin
         --  Language not supported => Ignore the attribute.
         --  We shouldn't remove it, since it might have been added by another
         --  page for a different language (Compiler'Switches is modified by
         --  several pages, for instance).

         if not Has_Supported_Language (Tool, Languages) then
            return;
         end if;

         --  Check if we in fact have the initial value

         declare
            Args : String_List_Access :=
              Get_Command_Line (Page, Expanded => False);
         begin
            if Project = No_Project then
               Is_Default_Value := False;

            else
               --  If the switches are exactly the ones set by default for the
               --  language, remove the file-specific attribute
               declare
                  Default_Args : GNAT.Strings.String_List :=
                    Get_Switches
                      (Project,
                       Tool => Tool,
                       Use_Initial_Value => True);
               begin
                  Is_Default_Value := Page = Default_Args;
                  if not Is_Default_Value and then Active (Me) then
                     Trace (Me, "Switches are not the default value");
                  end if;

                  Free (Default_Args);
               end;

               if Is_Default_Value then
                  To_Remove := File_Name /= GNATCOLL.VFS.No_File;

               else
                  --  If the switches are the ones already set for the file,
                  --  no change has been done in the dialog

                  declare
                     Default_Args : GNAT.Strings.String_List :=
                       Get_Switches
                         (Project, Tool,
                          File => File_Name,
                          Use_Initial_Value => Is_Default_Value);
                  begin
                     Is_Default_Value := Page = Default_Args;
                     if not Is_Default_Value and then Active (Me) then
                        Trace (Me, "Switches changed by user");
                     end if;

                     Free (Default_Args);
                  end;
               end if;
            end if;

            if Tool.Project_Attribute.all = "default_switches" then
               --  Tool's attribute is not defined in tool's descriptor,
               --  default handling of switches using "default_switches" and
               --  "switches" is used.

               if To_Remove then
                  if File_Name /= GNATCOLL.VFS.No_File then
                     Trace (Me, "Removing file-specific switches for "
                            & (+Base_Name (File_Name)));
                     Project.Delete_Attribute
                       (Scenario  => Scenario_Variables,
                        Attribute => Attribute_Pkg_List'(Build
                          (Tool.Project_Package.all, "switches")),
                        Index     => +Base_Name (File_Name));
                     Changed := True;
                  end if;

               elsif not Is_Default_Value then
                  if File_Name /= GNATCOLL.VFS.No_File then
                     if Args'Length /= 0 then
                        Trace (Me, "Changing switches for "
                               & (+Base_Name (File_Name)));
                        Project.Set_Attribute
                          (Scenario  => Scenario_Variables,
                           Attribute => Attribute_Pkg_List'(Build
                             (Tool.Project_Package.all, "switches")),
                           Values    => Args.all,
                           Index     => +Base_Name (File_Name),
                           Prepend   => False);
                        Changed := True;
                     else
                        Trace (Me, "Removing switches for "
                               & (+Base_Name (File_Name)));
                        Project.Delete_Attribute
                          (Scenario  => Scenario_Variables,
                           Attribute => Attribute_Pkg_List'(Build
                             (Tool.Project_Package.all, "switches")),
                           Index     => +Base_Name (File_Name));
                        Changed := True;
                     end if;

                  elsif Args'Length /= 0 then
                     Trace (Me, "Changing default switches for "
                            & Tool.Project_Package.all
                            & " " & Tool.Project_Index.all);
                     Project.Set_Attribute
                       (Scenario  => Scenario_Variables,
                        Attribute => Attribute_Pkg_List'(Build
                          (Tool.Project_Package.all, "default_switches")),
                        Values    => Args.all,
                        Index     => Tool.Project_Index.all,
                        Prepend   => False);
                     Changed := True;

                  else
                     Trace (Me, "Removing default switches for "
                            & Tool.Project_Package.all & " "
                            & Tool.Project_Index.all);
                     Project.Delete_Attribute
                       (Scenario  => Scenario_Variables,
                        Attribute => Attribute_Pkg_List'(Build
                          (Tool.Project_Package.all, "default_switches")),
                        Index     => Tool.Project_Index.all);
                     Changed := True;
                  end if;
               end if;

            else
               --  Tool's attribute is defined in tool's descriptor.

               if Args'Length /= 0 and then not Is_Default_Value then
                  Trace (Me, "Now has switches for '"
                         & Tool.Project_Package.all & "."
                         & Tool.Project_Attribute.all & "' when we had none");
                  Project.Set_Attribute
                    (Scenario  => Scenario_Variables,
                     Attribute =>
                       Attribute_Pkg_List'
                         (Build
                              (Tool.Project_Package.all,
                               Tool.Project_Attribute.all)),
                     Values    => Args.all,
                     Index     => Tool.Project_Index.all,
                     Prepend   => False);
                  Changed := True;

               elsif To_Remove then
                  Trace (Me, "No more switches for '"
                         & Tool.Project_Package.all & "."
                         & Tool.Project_Attribute.all & "'");
                  Project.Delete_Attribute
                    (Scenario  => Scenario_Variables,
                     Attribute =>
                       Attribute_Pkg_List'
                         (Build
                              (Tool.Project_Package.all,
                               Tool.Project_Attribute.all)),
                     Index     => Tool.Project_Index.all);
                  Changed := True;
               end if;
            end if;

            Free (Args);
         end;
      end Change_Switches;

      ------------------
      -- Process_File --
      ------------------

      procedure Process_File (File_Name : Virtual_File) is
         Tool : Tool_Properties_Record;
      begin
         for P in Switches.Pages'Range loop
            if Switches.Pages (P) /= null then
               Tool := Get_Tool_Properties
                 (Switches.Kernel, Switches.Pages (P).Tool_Name.all);
               if Tool /= No_Tool then
                  Change_Switches (Switches.Pages (P), Tool, File_Name);
               end if;
            end if;
         end loop;
      end Process_File;

   begin
      pragma Assert (Project /= No_Project);

      if Files'Length = 0 then
         Process_File (GNATCOLL.VFS.No_File);
      else
         for F in Files'Range loop
            Process_File (Files (F));
         end loop;
      end if;

      return Changed;
   end Generate_Project;

   -------------------------
   -- Close_Switch_Editor --
   -------------------------

   function Close_Switch_Editor
     (Switches     : access Switches_Edit_Record'Class;
      Project      : Project_Type;
      Files        : GNATCOLL.VFS.File_Array;
      Scenario     : access Scenario_Selector_Record'Class) return Boolean
   is
      Scenar    : Scenario_Iterator := Start (Scenario);
      Modified  : Boolean := False;
      Languages : GNAT.Strings.String_List := Project.Languages;

   begin
      --  No scenario variables ?
      while not At_End (Scenar) loop
         Modified := Modified or Generate_Project
           (Switches           => Switches,
            Languages          => Languages,
            Scenario_Variables => Current (Scenar),
            Project            => Project,
            Files              => Files);
         Next (Scenar);
      end loop;

      Free (Languages);

      --  ??? Need this to update the icon in the project explorer
      if Modified then
         Recompute_View (Switches.Kernel);
      end if;

      return Modified;
   end Close_Switch_Editor;

   ------------------
   -- Set_Switches --
   ------------------

   procedure Set_Switches
     (Editor : access Switches_Edit_Record; Project : Project_Type) is
   begin
      Fill_Editor (Editor, Project, Files => (1 .. 0 => GNATCOLL.VFS.No_File));
   end Set_Switches;

   ------------------
   -- Get_Switches --
   ------------------

   function Get_Switches
     (Switches          : access Switches_Edit_Record'Class;
      Tool              : Tool_Properties_Record;
      Files             : File_Array;
      Use_Initial_Value : Boolean := True) return GNAT.Strings.String_List
   is
      procedure Free is
        new Ada.Unchecked_Deallocation
          (GNAT.Strings.String_List, GNAT.Strings.String_List_Access);

      Value : String_List_Access;

   begin
      if Tool.Project_Attribute.all = "default_switches" then
         --  Tool's attribute is not defined in tool's descriptor, default
         --  handling of switches using "default_switches" and "switches" is
         --  used.

         if Files'Length = 0 then
            return Get_Switches
              (Switches.Project, Tool, GNATCOLL.VFS.No_File,
               Use_Initial_Value => Use_Initial_Value);

         else
            --  ??? Should we merge all the switches ?
            return Get_Switches
              (Switches.Project, Tool, Files (Files'First),
               Use_Initial_Value => Use_Initial_Value);
         end if;

      else
         --  Tool's attribute is defined in tool's descriptor, request value
         --  of the tool specific attribute and index.

         Value := Switches.Project.Attribute_Value
           (Attribute =>
              Attribute_Pkg_List'
              (Build
                 (Tool.Project_Package.all, Tool.Project_Attribute.all)),
            Index     => Tool.Project_Index.all);

         if Value = null then
            return (1 .. 0 => null);
         else
            declare
               Result : constant GNAT.Strings.String_List := Value.all;
            begin
               Free (Value);
               return Result;
            end;
         end if;
      end if;
   end Get_Switches;

   -----------------
   -- Fill_Editor --
   -----------------

   procedure Fill_Editor
     (Switches  : access Switches_Edit_Record'Class;
      Project   : Project_Type;
      Files     : File_Array) is
   begin
      Switches.Project := Project;

      --  Project might be null when we are in the project wizard. In this
      --  case, we fall back on switches for the default project.

      if Project /= No_Project then
         if Files'Length = 0 then
            declare
               Langs : GNAT.Strings.String_List := Project.Languages;
            begin
               Set_Visible_Pages (Switches, Langs);
               Free (Langs);
            end;

         else
            for F in Files'Range loop
               declare
                  Lang : aliased String := Get_Language_From_File
                    (Get_Language_Handler (Switches.Kernel), Files (F));
               begin
                  To_Lower (Lang);

                  Set_Visible_Pages
                    (Editor    => Switches,
                     Languages => (1 => Lang'Unchecked_Access),
                     Show_Only => F /= Files'First,
                     File_Specific => True);
               end;
            end loop;
         end if;
      end if;

      --  Set the switches for all the pages

      for P in Switches.Pages'Range loop
         if Switches.Pages (P) /= null then
            declare
               Tool : constant Tool_Properties_Record :=
                 Get_Tool_Properties (Switches.Kernel,
                                      Switches.Pages (P).Tool_Name.all);
               List : GNAT.Strings.String_List := Get_Switches
                 (Switches          => Switches,
                  Tool              => Tool,
                  Files             => Files,
                  Use_Initial_Value => False);
            begin
               Set_Command_Line
                 (Switches.Pages (P), Argument_List_To_String (List));
               Free (List);
            end;
         end if;
      end loop;

   exception
      when E : others => Trace (Exception_Handle, E);
   end Fill_Editor;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Edit_Switches_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Ignore : Boolean;
      pragma Unreferenced (Command, Ignore);
   begin
      Assert (Me, Has_Project_Information (Context.Context),
              "Project unknown when editing switches");

      if Has_File_Information (Context.Context) then
         Ignore := Edit_Switches_For_Files
           (Get_Kernel (Context.Context),
            Project_Information (Context.Context),
            (1 => File_Information (Context.Context)));
      else
         Ignore := Edit_Switches_For_Files
           (Get_Kernel (Context.Context),
            Project_Information (Context.Context),
            (1 .. 0 => GNATCOLL.VFS.No_File));
      end if;
      return Success;
   end Execute;

   -----------------------------
   -- Edit_Switches_For_Files --
   -----------------------------

   function Edit_Switches_For_Files
     (Kernel       : access GPS.Kernel.Kernel_Handle_Record'Class;
      Project      : Project_Type;
      Files        : File_Array) return Boolean
   is
      Switches  : Switches_Edit;
      Dialog    : Gtk_Dialog;
      Button    : Gtk_Widget;
      Ignore    : Gtk_Widget;
      B         : Gtk_Button;
      Box       : Gtk_Box;
      Selector  : Scenario_Selector;
      Modified  : Boolean;
      pragma Unreferenced (Ignore);

   begin
      Run_Hook (Kernel, Project_Editor_Hook);

      if Files'Length > 1 then
         Gtk_New (Dialog,
                  Title  => -"Editing switches for multiple files",
                  Parent => Get_Current_Window (Kernel),
                  Flags  => Modal or Destroy_With_Parent);

      elsif Files'Length = 1 then
         Gtk_New (Dialog,
                  Title  => -"Editing switches for specific file",
                  Parent => Get_Current_Window (Kernel),
                  Flags  => Modal or Destroy_With_Parent);

      else
         Gtk_New (Dialog,
                  Title  => (-"Editing default switches for project ")
                    & Project.Name,
                  Parent => Get_Current_Window (Kernel),
                  Flags  => Modal or Destroy_With_Parent);
      end if;

      Gtk_New_Hbox (Box, Homogeneous => False);
      Pack_Start (Get_Vbox (Dialog), Box, Fill => True, Expand => True);

      Gtk_New (Switches, Kernel);
      Switches.Kernel := Kernel_Handle (Kernel);
      Pack_Start (Box, Switches, Fill => True, Expand => True);

      Gtk_New (Selector, Kernel);
      Pack_Start (Box, Selector, Expand => False);

      Show_All (Dialog);

      --  Unrestricted_Access is safe, since Switches is a local variable
      --  destroyed when the dialog is destroyed at the end of this procedure.

      Switches.Files := Files'Unrestricted_Access;

      Fill_Editor (Switches, Project, Files);

      Ignore := Add_Button (Dialog, Stock_Ok, Gtk_Response_OK);

      if Files'Length /= 0 then
         Gtk_New_From_Stock (B, Stock_Revert_To_Saved);
         Pack_Start (Get_Action_Area (Dialog), B);
         Widget_Callback.Object_Connect
           (B, Signal_Clicked, Revert_To_Default'Access,
            Slot_Object => Switches);
         Show_All (B);
      end if;

      Button := Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);
      Show_All (Button);

      --  Note: if the dialog is no longer modal, then we need to create a copy
      --  of the context for storing in the callback, since the current context
      --  will be automatically freed by the kernel at some point in the life
      --  of this dialog.

      if Run (Dialog) = Gtk_Response_OK then
         Modified := Close_Switch_Editor
           (Switches, Project, Files, Selector);
      else
         Modified := False;
      end if;

      Destroy (Dialog);
      return Modified;
   end Edit_Switches_For_Files;

   ----------------------
   -- Get_Tool_By_Name --
   ----------------------

   overriding function Get_Tool_By_Name
     (Editor    : Switches_Editor_Page_Record;
      Tool_Name : String)
      return Gtk_Switches_Editors.Root_Switches_Editor_Access is
   begin
      if Editor.Switches /= null then
         for P in Editor.Switches.Pages'Range loop
            if Editor.Switches.Pages (P) /= null
              and then Editor.Switches.Pages (P).Tool_Name.all = Tool_Name
            then
               return Gtk_Switches_Editors.Root_Switches_Editor_Access
                 (Editor.Switches.Pages (P));
            end if;
         end loop;
      end if;
      return null;
   end Get_Tool_By_Name;

end Switches_Editors;
