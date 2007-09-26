-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2001-2007, AdaCore             --
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
with GNAT.Case_Util;            use GNAT.Case_Util;
with GNAT.Strings;              use GNAT.Strings;

with Glib;                      use Glib;
with Glib.Object;               use Glib.Object;

with Gtk.Box;                   use Gtk.Box;
with Gtk.Button;                use Gtk.Button;
with Gtk.Dialog;                use Gtk.Dialog;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Object;                use Gtk.Object;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Tooltips;              use Gtk.Tooltips;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Window;                use Gtk.Window;

with Gtkada.File_Selector;      use Gtkada.File_Selector;
with Gtkada.Handlers;           use Gtkada.Handlers;

with Basic_Types;               use Basic_Types;
with Commands.Interactive;      use Commands, Commands.Interactive;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel;                use GPS.Kernel;
with Language_Handlers;         use Language_Handlers;
with Prj;
with Project_Viewers;           use Project_Viewers;
with Projects.Editor;           use Projects.Editor;
with Projects;                  use Projects;
with Scenario_Selectors;        use Scenario_Selectors;
with Snames;                    use Snames;
with String_Utils;              use String_Utils;
with Switches_Chooser.Gtkada;   use Switches_Chooser, Switches_Chooser.Gtkada;
with Traces;                    use Traces;
with Namet;                     use Namet;
with VFS;                       use VFS;

package body Switches_Editors is

   Me : constant Debug_Handle := Create ("Switches_Editors");

   ------------------
   -- Dependencies --
   ------------------

--     type Dependency_Data is record
--        Master_Status  : Boolean;
--        Slave_Switch   : Switch_Check_Widget_Access;
--        Slave_Activate : Boolean;
--     end record;
--
--     package Dependency_Callback is new Gtk.Handlers.User_Callback
--       (Gtk_Widget_Record, Dependency_Data);
--
--     procedure Check_Dependency
--       (Check : access Gtk_Widget_Record'Class;
--        Data  : Dependency_Data);
--     procedure Check_Field_Dependency
--       (Field : access Gtk_Widget_Record'Class;
--        Data  : Dependency_Data);
   --  Callback to handle the dependencies between two items

   -----------------------
   -- Local subprograms --
   -----------------------

--     procedure Unchecked_Free is new Ada.Unchecked_Deallocation
--       (Pages_Array, Page_Array_Access);
--     procedure Unchecked_Free is new Ada.Unchecked_Deallocation
--       (GNAT.Strings.String_List, String_List_Access);

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

--     procedure Activate_Dependencies
--       (Page   : access Switches_Editor_Page_Record'Class;
--        Editor : access Switches_Edit_Record'Class);
   --  Activate all the dependencies defined for page

   function Get_Switches
     (Switches : access Switches_Edit_Record'Class;
      Pkg_Name : String;
      Language : String;
      Files    : File_Array;
      Use_Initial_Value : Boolean := True) return GNAT.Strings.String_List;
   --  Return the list of switches for Files, found in the package Pkg_Name,
   --  for a specific language, and for a specific list of switches. The
   --  returned array must be freed by the caller.

   procedure Fill_Editor
     (Switches  : access Switches_Edit_Record'Class;
      Project   : Project_Type;
      Files     : File_Array);
   --  Fill the editor.

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

--     function Filter_Matches
--       (Page : access Switches_Editor_Page_Record'Class;
--        Language : String) return Boolean;
   --  Return True if Page applies to Language.

   function Has_Supported_Language
     (Page                : access Switches_Editor_Page_Record'Class;
      Supported_Languages : GNAT.Strings.String_List) return Boolean;
   --  Return True if Page applies to one of the languages in
   --  Supported_Language

--     function Get_Parameter
--       (Switch          : String;
--        Separator       : String;
--        List            : GNAT.Strings.String_List;
--        Index_In_List   : Natural;
--        Skip_Next_Index : access Boolean) return String;
   --  Get the parameter for Switch, assuming it was first seen at position
   --  Index_In_List in List.

--     function Switch_Matches
--       (Switch        : String;
--        Separator     : String;
--        Candidate     : GNAT.Strings.String_Access) return Boolean;
   --  Whether Candidate matches Switch & Separator (properly take into
   --  account the case where Separator is a space, and the argument appears
   --  in a separate item in the argument_list

   --------------------
   -- Switch_Matches --
   --------------------

--     function Switch_Matches
--       (Switch        : String;
--        Separator     : String;
--        Candidate     : GNAT.Strings.String_Access) return Boolean is
--     begin
--        if Candidate = null then
--           return False;
--
--        elsif Separator'Length > 0
--          and then Separator (Separator'First) = ' '
--        then
--           return Candidate.all = Switch;
--
--        else
--           return Candidate'Length >= Switch'Length + Separator'Length
--             and then Candidate
--             (Candidate'First ..
--                Candidate'First + Switch'Length + Separator'Length - 1) =
--               Switch & Separator;
--        end if;
--     end Switch_Matches;

   -------------------
   -- Get_Parameter --
   -------------------

--     function Get_Parameter
--       (Switch          : String;
--        Separator       : String;
--        List            : GNAT.Strings.String_List;
--        Index_In_List   : Natural;
--        Skip_Next_Index : access Boolean) return String is
--     begin
--        if Separator'Length >= 1
--          and then Separator (Separator'First) = ' '
--        then
--           if Index_In_List < List'Last
--             and then List (Index_In_List + 1) /= null
--           then
--              Skip_Next_Index.all := True;
--              return List (Index_In_List + 1).all;
--           else
--              Skip_Next_Index.all := False;
--              return "";
--           end if;
--
--        else
--           Skip_Next_Index.all := False;
--           return List (Index_In_List)
--             (List (Index_In_List)'First + Switch'Length + Separator'Length
--              .. List (Index_In_List)'Last);
--        end if;
--     end Get_Parameter;

   ----------------------------
   -- Check_Field_Dependency --
   ----------------------------

--     procedure Check_Field_Dependency
--       (Field : access Gtk_Widget_Record'Class;
--        Data  : Dependency_Data)
--     is
--        Has_Text : constant Boolean := Get_Text (Gtk_Entry (Field)) /= "";
--     begin
--        if (Has_Text and then Data.Master_Status)
--          or else (not Has_Text and then not Data.Master_Status)
--        then
--           Set_Sensitive (Data.Slave_Switch.Check, False);
--           Set_Active (Data.Slave_Switch.Check, Data.Slave_Activate);
--        else
--           Set_Sensitive (Data.Slave_Switch.Check, True);
--           Set_Active (Data.Slave_Switch.Check, not Data.Slave_Activate);
--        end if;
--     end Check_Field_Dependency;

   ----------------------
   -- Check_Dependency --
   ----------------------

--     procedure Check_Dependency
--       (Check : access Gtk_Widget_Record'Class;
--        Data  : Dependency_Data) is
--     begin
--        if Get_Active (Gtk_Check_Button (Check)) = Data.Master_Status then
--           Set_Sensitive (Data.Slave_Switch.Check, False);
--           Set_Active (Data.Slave_Switch.Check, Data.Slave_Activate);
--
--        else
--           Set_Sensitive (Data.Slave_Switch.Check, True);
--           Set_Active (Data.Slave_Switch.Check, not Data.Slave_Activate);
--        end if;
--     end Check_Dependency;

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
              and then Editor.Pages (Num).Title.all = Title
            then
               return Editor.Pages (Num);
            end if;
         end loop;
      end if;

      return null;
   end Get_Page;

   --------------------
   -- Add_Dependency --
   --------------------

   procedure Add_Dependency
     (Page           : access Switches_Editor_Page_Record;
      Master_Page    : String;
      Master_Switch  : String;
      Master_Status  : Boolean;
      Slave_Page     : String;
      Slave_Switch   : String;
      Slave_Activate : Boolean := True) is
   begin
      Page.Dependencies := new Dependency_Description'
        (Next          => Page.Dependencies,
         Master_Page   => new String'(Master_Page),
         Master_Switch => new String'(Master_Switch),
         Master_Status => Master_Status,
         Slave_Page    => new String'(Slave_Page),
         Slave_Switch  => new String'(Slave_Switch),
         Slave_Status  => Slave_Activate);
   end Add_Dependency;

   ---------------------------
   -- Activate_Dependencies --
   ---------------------------

--     procedure Activate_Dependencies
--       (Page   : access Switches_Editor_Page_Record'Class;
--        Editor : access Switches_Edit_Record'Class)
--     is
--        Master_Page, Slave_Page : Switches_Editor_Page;
--        S1, S2  : Switch_Basic_Widget;
--        Dep : Dependency_Description_Access := Page.Dependencies;
--     begin
--        while Dep /= null loop
--           Master_Page := Get_Page (Editor, Dep.Master_Page.all);
--           Slave_Page  := Get_Page (Editor, Dep.Slave_Page.all);
--
--           if Master_Page /= null and then Slave_Page /= null then
--              S1 := Get_Switch_Widget (Master_Page, Dep.Master_Switch.all);
--              S2 := Get_Switch_Widget (Slave_Page, Dep.Slave_Switch.all);
--              if S1 = null
--                or else S2 = null
--                or else (S1.all not in Switch_Check_Widget'Class
--                         and then S1.all not in Switch_Field_Widget'Class)
--                or else S2.all not in Switch_Check_Widget'Class
--              then
--                 Insert
--                   (Page.Kernel,
--                  "Can only add dependencies between check button switches "
--                    & Master_Page.Title.all & ' ' & Dep.Master_Switch.all
--                   & ' ' & Slave_Page.Title.all & ' ' & Dep.Slave_Switch.all,
--                    Mode => GPS.Kernel.Console.Error);
--              else
--                 if S1.all in Switch_Check_Widget'Class then
--                    Dependency_Callback.Connect
--                      (Switch_Check_Widget_Access (S1).Check, Signal_Toggled,
--                       Check_Dependency'Access,
--                       (Dep.Master_Status,
--                        Switch_Check_Widget_Access (S2),
--                        Dep.Slave_Status));
--                    Check_Dependency
--                      (Switch_Check_Widget_Access (S1).Check,
--                       (Dep.Master_Status,
--                        Switch_Check_Widget_Access (S2),
--                        Dep.Slave_Status));
--                 else
--                    Dependency_Callback.Connect
--                      (Switch_Field_Widget_Access (S1).Field, Signal_Changed,
--                       Check_Field_Dependency'Access,
--                       (Dep.Master_Status,
--                        Switch_Check_Widget_Access (S2),
--                        Dep.Slave_Status));
--                    Check_Field_Dependency
--                      (Switch_Field_Widget_Access (S1).Field,
--                       (Dep.Master_Status,
--                        Switch_Check_Widget_Access (S2),
--                        Dep.Slave_Status));
--                 end if;
--              end if;
--           end if;
--
--           Dep := Dep.Next;
--        end loop;
--     end Activate_Dependencies;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Page            : out Switches_Editor_Page;
      Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      Title           : String;
      Project_Package : String;
      Attribute_Index : String := "";
      Config          : Switches_Editor_Config) is
   begin
      Page := new Switches_Editor_Page_Record;
      Initialize
        (Editor             => Page,
         Config             => Config,
         Tooltips           => Get_Tooltips (Kernel),
         Use_Native_Dialogs => Get_Pref (Use_Native_Dialogs));
      Widget_Callback.Connect (Page, Signal_Destroy, Page_Destroyed'Access);

      Page.Kernel := Kernel_Handle (Kernel);
      Page.Lang_Filter := null;

      Page.Attribute_Index := new String'(Attribute_Index);
      Page.Title := new String'(Title);
      Page.Pkg   := new String'(To_Lower (Project_Package));
   end Gtk_New;

   ------------------
   -- Add_Language --
   ------------------

   procedure Add_Language
     (Page : access Switches_Editor_Page_Record;
      Language_Filter : String) is
   begin
      Append (Page.Lang_Filter, (1 => new String'(Language_Filter)));
      To_Lower (Page.Lang_Filter (Page.Lang_Filter'Last).all);
   end Add_Language;

   --------------------
   -- Filter_Matches --
   --------------------

--     function Filter_Matches
--       (Page : access Switches_Editor_Page_Record'Class;
--        Language : String) return Boolean
--     is
--     begin
--        if Page.Lang_Filter = null then
--           return True;
--        end if;
--
--        for F in Page.Lang_Filter'Range loop
--           if Language = Page.Lang_Filter (F).all then
--              return True;
--           end if;
--        end loop;
--
--        return False;
--     end Filter_Matches;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Editor : out Switches_Edit;
      Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Tab    : Gtk_Label;
      Config : Switches_Editor_Config;
      Tool   : String_Access;
      Props  : Tool_Properties_Record;
      Lang   : String_List_Access;

   begin
      Editor := new Switches_Edit_Record;
      Gtk.Notebook.Initialize (Editor);
      Set_Scrollable (Editor);

      Editor.Kernel := Kernel_Handle (Kernel);
      Editor.Pages := new Pages_Array (1 .. Switches_Page_Count (Kernel));

      for P in Editor.Pages'Range loop
         Get_Nth_Switches_Page
           (Kernel,
            Num       => P,
            Config    => Config,
            Tool      => Tool,
            Languages => Lang);
         Props := Get_Tool_Properties
           (Kernel    => Kernel,
            Tool_Name => Tool.all);

         Gtk_New
           (Page            => Editor.Pages (P),
            Kernel          => Kernel,
            Title           => Tool.all,
            Project_Package => Props.Project_Package.all,
            Attribute_Index => Props.Project_Index.all,
            Config          => Config);
         Gtk_New (Tab, Editor.Pages (P).Title.all);
         Append_Page (Editor, Editor.Pages (P), Tab);

         if Lang /= null then
            for L in Lang'Range loop
               Add_Language (Editor.Pages (P), Lang (L).all);
            end loop;
         end if;

      end loop;

      --  Then once all the pages have been created, setup the dependencies
--        for P in Editor.Pages'Range loop
--           Activate_Dependencies (Editor.Pages (P), Editor);
--        end loop;

      Set_Current_Page (Editor, 0);
   end Gtk_New;

   --------------------
   -- Page_Destroyed --
   --------------------

   procedure Page_Destroyed (Page : access Gtk_Widget_Record'Class) is
      P : constant Switches_Editor_Page := Switches_Editor_Page (Page);
   begin
      Free (P.Lang_Filter);
      Free (P.Attribute_Index);
      Free (P.Title);
      Free (P.Pkg);
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
   begin
      for P in Editor.Pages'Range loop
         if (Editor.Pages (P).Lang_Filter = null
             or else Has_Supported_Language (Editor.Pages (P), Languages))
           and then (not File_Specific
                     or else Editor.Pages (P).Pkg.all /= Ide_Package)
         then
            Show (Editor.Pages (P));
         elsif not Show_Only then
            Hide (Editor.Pages (P));
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
         declare
            List : GNAT.Strings.String_List := Get_Switches
              (S,
               S.Pages (P).Pkg.all,
               S.Pages (P).Attribute_Index.all,
               Files => (1 .. 0 => VFS.No_File),
               Use_Initial_Value => True);
         begin
            Set_Command_Line (S.Pages (P).Switches, List);
            Free (List);
         end;
      end loop;

   exception
      when E : others => Trace (Exception_Handle, E);
   end Revert_To_Default;

   ----------------------------
   -- Has_Supported_Language --
   ----------------------------

   function Has_Supported_Language
     (Page                : access Switches_Editor_Page_Record'Class;
      Supported_Languages : GNAT.Strings.String_List) return Boolean is
   begin
      if Page.Lang_Filter /= null then
         for L in Page.Lang_Filter'Range loop
            for PL in Supported_Languages'Range loop
               if To_Lower (Page.Lang_Filter (L).all) =
                 To_Lower (Supported_Languages (PL).all)
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
         File_Name : Virtual_File);
      --  Changes the switches for a specific package and tool.

      procedure Process_File (File_Name : Virtual_File);
      --  Generate the switches for a specific file (or the default switches if
      --  File_Name is the empty string). Return True if the project was
      --  changed.

      ---------------------
      -- Change_Switches --
      ---------------------

      procedure Change_Switches
        (Page      : access Switches_Editor_Page_Record'Class;
         File_Name : Virtual_File)
      is
         Language : constant Name_Id := Get_String (Page.Attribute_Index.all);
         Value    : Prj.Variable_Value;
         Is_Default_Value : Boolean;
         Rename_Prj : Project_Type;
         To_Remove : Boolean := False;
      begin
         Rename_Prj := Find_Project_Of_Package (Project, Page.Pkg.all);

         --  Language not supported => Ignore the attribute.
         --  We shouldn't remove it, since it might have been added by another
         --  page for a different language (Compiler'Switches is modified by
         --  several pages, for instance).

         if not Has_Supported_Language (Page, Languages) then
            return;
         end if;

         --  Check if we in fact have the initial value.

         declare
            Args : String_List_Access :=
              Get_Command_Line (Page, Expanded => False);
         begin
            if Project = No_Project then
               Is_Default_Value := False;

            else
               --  If the switches are exactly the ones set by default for the
               --  language, remove the file-specific attribute

               Get_Switches
                 (Project          => Rename_Prj,
                  In_Pkg           => Page.Pkg.all,
                  File             => VFS.No_File,
                  Language         => Language,
                  Value            => Value,
                  Is_Default_Value => Is_Default_Value);
               declare
                  Default_Args : GNAT.Strings.String_List :=
                    To_Argument_List (Get_Tree (Project), Value);
               begin
                  Is_Default_Value := Page = Default_Args;
                  if not Is_Default_Value and then Active (Me) then
                     Trace (Me, "Switches are not the default value");
                  end if;

                  Free (Default_Args);
               end;

               if Is_Default_Value then
                  To_Remove := File_Name /= VFS.No_File;

               else
                  --  If the switches are the ones already set for the file,
                  --  no change has been done in the dialog

                  Get_Switches
                    (Project          => Rename_Prj,
                     In_Pkg           => Page.Pkg.all,
                     File             => File_Name,
                     Language         => Language,
                     Value            => Value,
                     Is_Default_Value => Is_Default_Value);
                  declare
                     Default_Args : GNAT.Strings.String_List :=
                       To_Argument_List (Get_Tree (Project), Value);
                  begin
                     Is_Default_Value := Page = Default_Args;
                     if not Is_Default_Value and then Active (Me) then
                        Trace (Me, "Switches changed by user");
                     end if;

                     Free (Default_Args);
                  end;
               end if;
            end if;

            if To_Remove then
               if File_Name /= VFS.No_File then
                  Trace (Me, "Removing file-specific switches for "
                         & Base_Name (File_Name));
                  Delete_Attribute
                    (Project            => Rename_Prj,
                     Scenario_Variables => Scenario_Variables,
                     Attribute          => Build
                       (Page.Pkg.all, Get_String (Name_Switches)),
                     Attribute_Index    => Base_Name (File_Name));
                  Changed := True;
               end if;

            elsif not Is_Default_Value then
               if File_Name /= VFS.No_File then
                  if Args'Length /= 0 then
                     Trace (Me, "Changing switches for "
                            & Base_Name (File_Name));
                     Update_Attribute_Value_In_Scenario
                       (Project            => Rename_Prj,
                        Scenario_Variables => Scenario_Variables,
                        Attribute          =>
                          Build (Page.Pkg.all, Get_String (Name_Switches)),
                        Values             => Args.all,
                        Attribute_Index    => Base_Name (File_Name),
                        Prepend            => False);
                     Changed := True;
                  else
                     Trace (Me, "Removing switches for "
                            & Base_Name (File_Name));
                     Delete_Attribute
                       (Project            => Rename_Prj,
                        Scenario_Variables => Scenario_Variables,
                        Attribute          =>
                          Build (Page.Pkg.all, Get_String (Name_Switches)),
                        Attribute_Index    => Base_Name (File_Name));
                     Changed := True;
                  end if;

               elsif Args'Length /= 0 then
                  Trace (Me, "Changing default switches for "
                         & Page.Pkg.all & " " & Page.Attribute_Index.all);
                  Update_Attribute_Value_In_Scenario
                    (Project            => Rename_Prj,
                     Scenario_Variables => Scenario_Variables,
                     Attribute          =>
                      Build (Page.Pkg.all, Get_String (Name_Default_Switches)),
                     Values             => Args.all,
                     Attribute_Index    => Page.Attribute_Index.all,
                     Prepend            => False);
                  Changed := True;

               else
                  Trace (Me, "Removing default switches for "
                         & Page.Pkg.all & " " & Page.Attribute_Index.all);
                  Delete_Attribute
                    (Project            => Rename_Prj,
                     Scenario_Variables => Scenario_Variables,
                     Attribute          =>
                      Build (Page.Pkg.all, Get_String (Name_Default_Switches)),
                     Attribute_Index    => Page.Attribute_Index.all);
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
      begin
         for P in Switches.Pages'Range loop
            Change_Switches (Switches.Pages (P), File_Name);
         end loop;
      end Process_File;

   begin
      pragma Assert (Project /= No_Project);

      if Files'Length = 0 then
         Process_File (VFS.No_File);
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
      Files        : VFS.File_Array;
      Scenario     : access Scenario_Selector_Record'Class) return Boolean
   is
      Saved     : GNAT.Strings.String_List := Get_Current_Scenario
        (Scenario_Variables (Switches.Kernel));
      Scenar    : Scenario_Iterator := Start (Scenario);
      Modified  : Boolean := False;
      Languages : GNAT.Strings.String_List := Get_Languages (Project);

   begin
      --  No scenario variables ?
      while not At_End (Scenar) loop
         declare
            Cur : GNAT.Strings.String_List := Current (Scenar);
         begin
            Set_Environment (Scenario_Variables (Switches.Kernel), Cur);
            Modified := Modified or Generate_Project
              (Switches           => Switches,
               Languages          => Languages,
               Scenario_Variables => Scenario_Variables (Switches.Kernel),
               Project            => Project,
               Files              => Files);
            Free (Cur);
         end;

         Next (Scenar);
      end loop;

      Free (Languages);

      Set_Environment (Scenario_Variables (Switches.Kernel), Saved);
      Free (Saved);

      --  ??? Need this to update the icon in the project explorer...
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
      Fill_Editor (Editor, Project, Files => (1 .. 0 => VFS.No_File));
   end Set_Switches;

   ------------------
   -- Get_Switches --
   ------------------

   function Get_Switches
     (Switches          : access Switches_Edit_Record'Class;
      Pkg_Name          : String;
      Language          : String;
      Files             : File_Array;
      Use_Initial_Value : Boolean := True) return GNAT.Strings.String_List is
   begin
      if Files'Length = 0 then
         return Get_Switches
           (Switches.Kernel,
            Switches.Project, Pkg_Name, VFS.No_File,
            Language, Use_Initial_Value => Use_Initial_Value);
      else
         --  ??? Should we merge all the switches ?
         return Get_Switches
           (Switches.Kernel,
            Switches.Project, Pkg_Name, Files (Files'First),
            Language, Use_Initial_Value => Use_Initial_Value);
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
               Langs : GNAT.Strings.String_List := Get_Languages (Project);
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
         declare
            List : GNAT.Strings.String_List := Get_Switches
              (Switches,
               Switches.Pages (P).Pkg.all,
               Switches.Pages (P).Attribute_Index.all,
               Files,
               Use_Initial_Value => False);
         begin
            Set_Command_Line
              (Switches.Pages (P), Argument_List_To_String (List));
            Free (List);
         end;
      end loop;

   exception
      when E : others => Trace (Exception_Handle, E);
   end Fill_Editor;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Edit_Switches_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Modified : Boolean;
      pragma Unreferenced (Command, Modified);
   begin
      Assert (Me, Has_Project_Information (Context.Context),
              "Project unknown when editing switches");

      if Has_File_Information (Context.Context) then
         Modified := Edit_Switches_For_Files
           (Get_Kernel (Context.Context),
            Project_Information (Context.Context),
            (1 => File_Information (Context.Context)));
      else
         Modified := Edit_Switches_For_Files
           (Get_Kernel (Context.Context),
            Project_Information (Context.Context),
            (1 .. 0 => VFS.No_File));
      end if;
      return Success;
   end Execute;

   -----------------------------
   -- Edit_Switches_For_Files --
   -----------------------------

   function Edit_Switches_For_Files
     (Kernel       : access GPS.Kernel.Kernel_Handle_Record'Class;
      Project      : Projects.Project_Type;
      Files        : File_Array) return Boolean
   is
      Switches  : Switches_Edit;
      Dialog    : Gtk_Dialog;
      Button, Button_OK    : Gtk_Widget;
      B         : Gtk_Button;
      Box       : Gtk_Box;
      Selector  : Scenario_Selector;
      Modified  : Boolean;
      pragma Unreferenced (Button_OK);

   begin
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
                    & Project_Name (Project),
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

      Button_OK := Add_Button (Dialog, Stock_Ok, Gtk_Response_OK);

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

end Switches_Editors;
