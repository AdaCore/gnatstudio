------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2017, AdaCore                     --
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

with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with GNAT.Strings;              use GNAT.Strings;
with GNATCOLL.Utils;            use GNATCOLL.Utils;

with Glib;                      use Glib;

with Gtk.Box;                   use Gtk.Box;
with Gtk.Button;                use Gtk.Button;
with Gtk.Dialog;                use Gtk.Dialog;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Widget;                use Gtk.Widget;

with Gtkada.Handlers;           use Gtkada.Handlers;

with Commands;                  use Commands;
with Dialog_Utils;              use Dialog_Utils;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Main_Window;           use GPS.Main_Window;
with Scenario_Selectors;        use Scenario_Selectors;
with Switches_Chooser.Gtkada;   use Switches_Chooser, Switches_Chooser.Gtkada;
with GNATCOLL.Traces;           use GNATCOLL.Traces;
with Histories;                 use Histories;
with GNATCOLL.Arg_Lists;        use GNATCOLL.Arg_Lists;

package body Switches_Editors is

   Me : constant Trace_Handle := Create ("Switches_Editors");

   ---------------------
   -- Switches editor --
   ---------------------

   type Multi_Page_Switches_Editor_Record is
     new Switches_Chooser.Gtkada.Switches_Editor_Record
     with record
        Tool_From_Name : Tool_From_Name_Getter;
     end record;
   type Multi_Page_Switches_Editor is
     access all Multi_Page_Switches_Editor_Record'Class;
   overriding function Get_Tool_By_Name
     (Self      : Multi_Page_Switches_Editor_Record;
      Tool_Name : String)
      return Gtk_Switches_Editors.Root_Switches_Editor_Access;
   --  A special child of the switches editor, so that dependencies between
   --  tools' switches are properly handled.

   --------------------------
   -- Project editor pages --
   --------------------------

   type Switches_Editor_Page_Record is new Project_Editor_Page_Record
     (Flags => Multiple_Projects or Multiple_Scenarios)
   with record
      Switches : Multi_Page_Switches_Editor;
      Tool     : access GPS.Kernel.Tool_Properties_Record;
      Files    : File_Array_Access;
      Tool_From_Name : Tool_From_Name_Getter;
   end record;
   type Switches_Editor_Page is access all Switches_Editor_Page_Record'Class;
   overriding procedure Destroy (Self : in out Switches_Editor_Page_Record);
   overriding procedure Initialize
     (Self         : not null access Switches_Editor_Page_Record;
      Kernel       : not null access Kernel_Handle_Record'Class;
      Read_Only    : Boolean;
      Project      : Project_Type := No_Project);
   overriding function Edit_Project
     (Self               : not null access Switches_Editor_Page_Record;
      Project            : Project_Type;
      Kernel             : not null access Kernel_Handle_Record'Class;
      Languages          : GNAT.Strings.String_List;
      Scenario_Variables : Scenario_Variable_Array) return Boolean;
   overriding function Is_Visible
     (Self               : not null access Switches_Editor_Page_Record;
      Languages          : GNAT.Strings.String_List) return Boolean;

   function Get_Switches
     (Self     : not null access Switches_Editor_Page_Record'Class;
      Project  : Project_Type;
      Use_Initial_Value : Boolean := True) return GNAT.Strings.String_List;
   --  Return the list of switches for Files, found in the package Pkg_Name,
   --  for a specific language, and for a specific list of switches. The
   --  returned array must be freed by the caller.

   type All_Tools_Switch_Editor_Record is new Project_Editor_Multi_Page_Record
   with record
      Project        : Project_Type;
   end record;
   type All_Tools_Switch_Editor is
     access all All_Tools_Switch_Editor_Record'Class;

   procedure For_All_Switches_Pages
     (Data     : access GObject_Record'Class;
      Callback : Page_Iterator_Callback);
   --  Iterate over all nested pages of a All_Tools_Switch_Editor.
   --  The profile is compatible with a Page_Iterator, and is used for a
   --  Tool_From_Name_Getter.

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Revert_To_Default (Switches : access Gtk_Widget_Record'Class);
   --  Revert to the default switches in the editor

   function Has_Supported_Language
     (Tool                : Tool_Properties_Record;
      Supported_Languages : GNAT.Strings.String_List) return Boolean;
   --  Return True if Tool applies to one of the languages in
   --  Supported_Language

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Self : in out Switches_Editor_Page_Record) is
   begin
      Unchecked_Free (Self.Files);
   end Destroy;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (Self         : not null access Switches_Editor_Page_Record;
      Kernel       : not null access Kernel_Handle_Record'Class;
      Read_Only    : Boolean;
      Project      : Project_Type := No_Project)
   is
      pragma Unreferenced (Kernel);
      List : GNAT.Strings.String_List := Self.Get_Switches
        (Project           => Project,
         Use_Initial_Value => False);
   begin
      Dialog_Utils.Initialize (Self);

      Self.Switches := new Multi_Page_Switches_Editor_Record;
      Self.Switches.Tool_From_Name := Self.Tool_From_Name;
      Initialize
        (Editor             => Self.Switches,
         Config             => Self.Tool.Config,
         Use_Native_Dialogs => Use_Native_Dialogs.Get_Pref,
         Read_Only          => Read_Only,
         History            => null,
         Key                => No_Key,
         Cmd_Line_Tooltip   =>
           "The switches that will be added to the corresponding package.");
      Self.Append (Self.Switches, Fill => True, Expand => True);

      Set_Command_Line (Self.Switches, Argument_List_To_String (List));
      Free (List);
   end Initialize;

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

   ----------------
   -- Is_Visible --
   ----------------

   overriding function Is_Visible
     (Self      : not null access Switches_Editor_Page_Record;
      Languages : GNAT.Strings.String_List) return Boolean is
   begin
      return Has_Supported_Language (Self.Tool.all, Languages);
   end Is_Visible;

   ------------------
   -- Edit_Project --
   ------------------

   overriding function Edit_Project
     (Self               : not null access Switches_Editor_Page_Record;
      Project            : Project_Type;
      Kernel             : not null access Kernel_Handle_Record'Class;
      Languages          : GNAT.Strings.String_List;
      Scenario_Variables : Scenario_Variable_Array) return Boolean
   is
      pragma Unreferenced (Kernel);

      Changed : Boolean := False;

      procedure Change_Switches (File_Name : Virtual_File);
      --  Changes the switches for a specific package and tool

      ---------------------
      -- Change_Switches --
      ---------------------

      procedure Change_Switches (File_Name : Virtual_File) is
         Is_Default_Value : Boolean;
         To_Remove        : Boolean := False;
         Attr_Name        : Unbounded_String;
         Index            : constant String :=
                              (if File_Name /= GNATCOLL.VFS.No_File then
                                  File_Name.Display_Base_Name
                               else
                                  To_String (Self.Tool.Project_Index));
      begin
         --  Language not supported => Ignore the attribute.
         --  We shouldn't remove it, since it might have been added by another
         --  page for a different language (Compiler'Switches is modified by
         --  several pages, for instance).

         if not Has_Supported_Language (Self.Tool.all, Languages) then
            return;
         end if;

         --  Check if we in fact have the initial value

         declare
            Args : String_List_Access :=
              Self.Switches.Get_Command_Line (Expanded => False);
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
                       Tool              => Self.Tool.all,
                       Use_Initial_Value => True);
               begin
                  Is_Default_Value := Self.Switches = Default_Args;
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
                         (Project, Self.Tool.all,
                          File              => File_Name,
                          Use_Initial_Value => Is_Default_Value);
                  begin
                     Is_Default_Value := Self.Switches = Default_Args;
                     if not Is_Default_Value and then Active (Me) then
                        Trace (Me, "Switches changed by user");
                     end if;

                     Free (Default_Args);
                  end;
               end if;
            end if;

            if Self.Tool.Project_Attribute = "default_switches" then
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
                          (To_String (Self.Tool.Project_Package), "switches")),
                        Index     => Index);
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
                             (To_String (Self.Tool.Project_Package),
                              "switches")),
                           Values    => Args.all,
                           Index     => Index,
                           Prepend   => False);
                        Changed := True;
                     else
                        Trace (Me, "Removing switches for "
                               & (+Base_Name (File_Name)));
                        Project.Delete_Attribute
                          (Scenario  => Scenario_Variables,
                           Attribute => Attribute_Pkg_List'(Build
                             (To_String (Self.Tool.Project_Package),
                              "switches")),
                           Index     => Index);
                        Changed := True;
                     end if;

                  else
                     --  Use Switches, unless the user was already using
                     --  Default_Switches.

                     Attr_Name := To_Unbounded_String ("switches");
                     if Project.Has_Attribute
                       (Attribute_Pkg_List'
                          (Build
                            (To_String (Self.Tool.Project_Package),
                             "default_switches")),
                        Index => Index)
                     then
                        Attr_Name := To_Unbounded_String ("default_switches");
                     end if;

                     if Args'Length /= 0 then
                        Trace (Me, "Changing default switches for "
                               & To_String (Self.Tool.Project_Package)
                               & " " & To_String (Self.Tool.Project_Index));
                        Project.Set_Attribute
                          (Scenario  => Scenario_Variables,
                           Attribute => Attribute_Pkg_List'(Build
                             (To_String (Self.Tool.Project_Package),
                                  To_String (Attr_Name))),
                           Values    => Args.all,
                           Index     => Index,
                           Prepend   => False);
                        Changed := True;

                     else
                        Trace (Me, "Removing default switches for "
                               & To_String (Self.Tool.Project_Package) & " "
                               & To_String (Self.Tool.Project_Index));
                        Project.Delete_Attribute
                          (Scenario  => Scenario_Variables,
                           Attribute => Attribute_Pkg_List'(Build
                             (To_String (Self.Tool.Project_Package),
                              To_String (Attr_Name))),
                           Index     => Index);
                        Changed := True;
                     end if;
                  end if;
               end if;

            else
               --  Tool's attribute is defined in tool's descriptor.
               if Args'Length /= 0 and then not Is_Default_Value then
                  Trace (Me, "Now has switches for '"
                         & To_String (Self.Tool.Project_Package) & "."
                         & To_String (Self.Tool.Project_Attribute)
                         & "' when we had none");
                  Project.Set_Attribute
                    (Scenario  => Scenario_Variables,
                     Attribute =>
                       Attribute_Pkg_List'
                         (Build
                              (To_String (Self.Tool.Project_Package),
                               To_String (Self.Tool.Project_Attribute))),
                     Values    => Args.all,
                     Index     => Index,
                     Prepend   => False);
                  Changed := True;

               elsif not Is_Default_Value then
                  --  Args'Length = 0 and diffs from old value, so drop it
                  Trace (Me, "No more switches for '"
                         & To_String (Self.Tool.Project_Package) & "."
                         & To_String (Self.Tool.Project_Attribute) & "'");
                  Project.Delete_Attribute
                    (Scenario  => Scenario_Variables,
                     Attribute =>
                       Attribute_Pkg_List'
                         (Build
                              (To_String (Self.Tool.Project_Package),
                               To_String (Self.Tool.Project_Attribute))),
                     Index     => Index);
                  Changed := True;
               end if;
            end if;

            Free (Args);
         end;
      end Change_Switches;

   begin
      pragma Assert (Project /= No_Project);

      if Self.Files = null or else Self.Files'Length = 0 then
         Change_Switches (GNATCOLL.VFS.No_File);
      else
         for F in Self.Files'Range loop
            Change_Switches (Self.Files (F));
         end loop;
      end if;

      return Changed;
   end Edit_Project;

   ------------------
   -- Get_Switches --
   ------------------

   function Get_Switches
     (Self     : not null access Switches_Editor_Page_Record'Class;
      Project  : Project_Type;
      Use_Initial_Value : Boolean := True) return GNAT.Strings.String_List
   is
      Files : constant File_Array :=
        (if Use_Initial_Value or else Self.Files = null
         then Empty_File_Array
         else Self.Files.all);

      procedure Free is
        new Ada.Unchecked_Deallocation
          (GNAT.Strings.String_List, GNAT.Strings.String_List_Access);

      Value : String_List_Access;

   begin
      if Self.Tool.Project_Attribute = "default_switches" then
         --  Tool's attribute is not defined in tool's descriptor, default
         --  handling of switches using "default_switches" and "switches" is
         --  used.

         if Files'Length = 0 then
            return Get_Switches
              (Project, Self.Tool.all, GNATCOLL.VFS.No_File,
               Use_Initial_Value => Use_Initial_Value);

         else
            --  ??? Should we merge all the switches ?
            return Get_Switches
              (Project, Self.Tool.all, Files (Files'First),
               Use_Initial_Value => Use_Initial_Value);
         end if;

      else
         declare
            Index : constant String :=
                      (if Files'Length = 0 then
                          To_String (Self.Tool.Project_Index)
                       else
                          Files (Files'First).Display_Base_Name);
         begin
            --  Tool's attribute is defined in tool's descriptor, request value
            --  of the tool specific attribute and index.

            Value := Project.Attribute_Value
              (Attribute =>
                 Attribute_Pkg_List'
                   (Build
                        (To_String (Self.Tool.Project_Package),
                         To_String (Self.Tool.Project_Attribute))),
               Index     => Index);

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
         end;
      end if;
   end Get_Switches;

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

   -----------------------
   -- Revert_To_Default --
   -----------------------

   procedure Revert_To_Default
     (Switches : access Gtk_Widget_Record'Class)
   is
      Self : constant All_Tools_Switch_Editor :=
        All_Tools_Switch_Editor (Switches);

      procedure Callback
        (Page : not null access Project_Editor_Page_Record'Class);
      procedure Callback
        (Page : not null access Project_Editor_Page_Record'Class)
      is
         S : constant Switches_Editor_Page := Switches_Editor_Page (Page);
         List : GNAT.Strings.String_List := Get_Switches
           (S,
            Project           => Self.Project,
            Use_Initial_Value => True);
      begin
         Set_Command_Line (S.Switches, List);
         Free (List);
      end Callback;

   begin
      Self.For_Each_Page (Callback'Unrestricted_Access);
   end Revert_To_Default;

   -----------------------------
   -- Edit_Switches_For_Files --
   -----------------------------

   function Edit_Switches_For_Files
     (Kernel       : access GPS.Kernel.Kernel_Handle_Record'Class;
      Project      : Project_Type;
      Files        : File_Array) return Boolean
   is
      Page      : All_Tools_Switch_Editor;
      Dialog    : Gtk_Dialog;
      Button    : Gtk_Widget;
      Ignore    : Gtk_Widget;
      B         : Gtk_Button;
      Box       : Gtk_Box;
      Selector  : Scenario_Selector;
      Modified  : Boolean;
      pragma Unreferenced (Ignore);

   begin
      Project_Editor_Hook.Run (Kernel);

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

      Set_Default_Size_From_History
         (Dialog, "switches-editor", Kernel, 1024, 800);

      Gtk_New_Hbox (Box, Homogeneous => False);
      Dialog.Get_Content_Area.Pack_Start (Box, Fill => True, Expand => True);

      Page := All_Tools_Switch_Editor
        (Switches_Editor_For_All_Tools_Factory (Kernel, Files));
      Page.Project := Project;
      Page.Initialize (Kernel, Read_Only => False, Project => Project);
      Box.Pack_Start (Page, Fill => True, Expand => True);

      Gtk_New (Selector, Kernel);
      Box.Pack_Start (Selector, Expand => False);

      Dialog.Show_All;

      Ignore := Dialog.Add_Button ("Save", Gtk_Response_OK);

      if Files'Length /= 0 then
         Gtk_New_From_Stock (B, Stock_Revert_To_Saved);
         Dialog.Get_Action_Area.Pack_Start (B);
         Widget_Callback.Object_Connect
           (B, Signal_Clicked, Revert_To_Default'Access,
            Slot_Object => Page);
         B.Show_All;
      end if;

      Button := Dialog.Add_Button (Stock_Cancel, Gtk_Response_Cancel);
      Button.Show_All;

      --  Note: if the dialog is no longer modal, then we need to create a copy
      --  of the context for storing in the callback, since the current context
      --  will be automatically freed by the kernel at some point in the life
      --  of this dialog.

      Modified := False;

      --  Compute which pages should be visible

      if Files'Length /= 0 then
         declare
            List : GNAT.Strings.String_List (Files'Range);
         begin
            for L in List'Range loop
               List (L) := new String'
                 (Kernel.Get_Language_Handler.Get_Language_From_File
                    (Files (L)));
            end loop;

            Modified := Page.Is_Visible (List);
            Free (List);
         end;
      else
         declare
            List : GNAT.Strings.String_List := Project.Languages;
         begin
            Modified := Page.Is_Visible (List);
            Free (List);
         end;
      end if;

      if Run (Dialog) = Gtk_Response_OK then
         declare
            Scenarios : constant Scenario_Variable_Array :=
                          Selector.Get_Scenarios;
            Languages : GNAT.Strings.String_List := Project.Languages;
            Success   : Boolean;
         begin
            Modified := Modified or Page.Edit_Project
              (Kernel             => Kernel,
               Languages          => Languages,
               Scenario_Variables => Scenarios,
               Project            => Project);

            Free (Languages);

            if Modified then
               Project.Set_Modified (True);
               Recompute_View (Kernel);
               Success := Save_Project
                 (Kernel    => Kernel,
                  Project   => Project,
                  Recursive => True);

               if not Success then
                  Trace (Me, "Failed to save project: " & Project.Name);
               end if;
            end if;
         end;
      end if;

      Destroy (Dialog);
      return Modified;
   end Edit_Switches_For_Files;

   --------------------------------------
   -- Switches_Editor_For_Tool_Factory --
   --------------------------------------

   function Switches_Editor_For_Tool_Factory
     (Tool           : not null access GPS.Kernel.Tool_Properties_Record;
      Files          : File_Array := Empty_File_Array;
      Tool_From_Name : Tool_From_Name_Getter)
      return Project_Editor_Page
   is
      Result : access Switches_Editor_Page_Record;
   begin
      Result := new Switches_Editor_Page_Record;
      Result.Tool_From_Name := Tool_From_Name;
      Result.Tool := Tool;

      if Files /= Empty_File_Array then
         Result.Files := new File_Array'(Files);
      end if;
      return Project_Editor_Page (Result);
   end Switches_Editor_For_Tool_Factory;

   ----------------------------
   -- For_All_Switches_Pages --
   ----------------------------

   procedure For_All_Switches_Pages
     (Data     : access GObject_Record'Class;
      Callback : Page_Iterator_Callback)
   is
      P : constant All_Tools_Switch_Editor := All_Tools_Switch_Editor (Data);
   begin
      P.For_Each_Page (Callback);
   end For_All_Switches_Pages;

   -------------------------------------------
   -- Switches_Editor_For_All_Tools_Factory --
   -------------------------------------------

   function Switches_Editor_For_All_Tools_Factory
     (Kernel         : not null access Kernel_Handle_Record'Class;
      Files          : File_Array := Empty_File_Array)
      return Project_Editor_Page
   is
      Result : constant access All_Tools_Switch_Editor_Record :=
        new All_Tools_Switch_Editor_Record;
      Tools : constant Tool_Properties_Array := Get_All_Tools (Kernel);
   begin
      for T in Tools'Range loop
         Result.Add_Page
           (Page  => Switches_Editor_For_Tool_Factory
              (Tool           => Tools (T),
               Files          => Files,
               Tool_From_Name =>
                 (Data     => Result,
                  Iterator => For_All_Switches_Pages'Access)),
            Title => Tools (T).Tool_Name);
      end loop;

      return Project_Editor_Page (Result);
   end Switches_Editor_For_All_Tools_Factory;

   ----------------------
   -- Get_Tool_By_Name --
   ----------------------

   overriding function Get_Tool_By_Name
     (Self      : Multi_Page_Switches_Editor_Record;
      Tool_Name : String)
      return Gtk_Switches_Editors.Root_Switches_Editor_Access
   is
      Found : Gtk_Switches_Editors.Root_Switches_Editor_Access;

      procedure Callback
        (Page : not null access Project_Editor_Page_Record'Class);

      --------------
      -- Callback --
      --------------

      procedure Callback
        (Page : not null access Project_Editor_Page_Record'Class) is
      begin
         if Page.all in Switches_Editor_Page_Record'Class
           and then Switches_Editor_Page (Page).Tool.Tool_Name = Tool_Name
         then
            Found := Gtk_Switches_Editors.Root_Switches_Editor_Access
              (Switches_Editor_Page (Page).Switches);
         end if;
      end Callback;

   begin
      if Self.Tool_From_Name.Iterator /= null then
         Self.Tool_From_Name.Iterator
           (Self.Tool_From_Name.Data, Callback'Unrestricted_Access);
      end if;
      return Found;
   end Get_Tool_By_Name;

end Switches_Editors;
