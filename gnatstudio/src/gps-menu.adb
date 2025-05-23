------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2001-2025, AdaCore                     --
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

with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;

with VSS.Strings.Conversions;
with VSS.Strings.Formatters.Strings;
with VSS.Strings.Templates;
with VSS.String_Vectors;

with GNATCOLL.Projects;      use GNATCOLL.Projects;
with GNATCOLL.Tribooleans;
with GNATCOLL.Utils;         use GNATCOLL.Utils;
with GNATCOLL.VFS;           use GNATCOLL.VFS;
with GNATCOLL.VFS.VSS_Utils;

with Glib.Object;            use Glib.Object;
with Glib.Types;             use Glib.Types;
with Glib;                   use Glib;

with Gtk.Editable;           use Gtk.Editable;
with Gtk.Label;              use Gtk.Label;
with Gtk.Text_View;          use Gtk.Text_View;
with Gtk.Widget;             use Gtk.Widget;
with Gtk.Window;             use Gtk.Window;

with Gtkada.File_Selector;   use Gtkada.File_Selector;

with Commands.Interactive;   use Commands, Commands.Interactive;
with GPS.Intl;               use GPS.Intl;

with GPS.Kernel.Actions;     use GPS.Kernel.Actions;
with GPS.Kernel.Clipboard;   use GPS.Kernel.Clipboard;
with GPS.Kernel.Hooks;       use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;         use GPS.Kernel.MDI;
with GPS.Kernel.Modules.UI;  use GPS.Kernel.Modules, GPS.Kernel.Modules.UI;
with GPS.Kernel.Task_Manager;
with GPS.Kernel.Preferences; use GPS.Kernel.Preferences;
with GPS.Kernel.Project;     use GPS.Kernel.Project;
with GPS.Main_Window;        use GPS.Main_Window;
with Histories;              use Histories;
with File_Utils;             use File_Utils;
with GUI_Utils;              use GUI_Utils;
with Interactive_Consoles;
with Tooltips;               use Tooltips;

package body GPS.Menu is

   type Menu_Module_Record is new Module_ID_Record with record
      Recent_Project_Actions : Action_Lists.List;
      --  The list of actions that open recent projects. These actions
      --  are displayed in the 'Open Recent Pproject...' menu.

      Is_Alire_Available : GNATCOLL.Tribooleans.Triboolean :=
        GNATCOLL.Tribooleans.Indeterminate;
      --  True if the Alire executable ('alr') is available in the PATH
      --  at startup, False otherwise. Using a triboolean to cache the
      --  value once it's computed the first time.

      Alire_Crate_Loaded : Boolean := False;
      --  True when a project has been loaded through an Alire crate.
      --  When it's the case we don't want to consider the .gpr file that
      --  gets finally loaded as a recent project, just the Alire manifest.
   end record;
   Menu_Module : aliased Menu_Module_Record :=
     (Module_ID_Record with others => <>);
   --  ??? Should be registered as standard module

   type On_Project_Changing is new File_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Project_Changing;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File);
   --  Called when project is about to change

   type On_Project_Changed is new Simple_Hooks_Function with null record;
   overriding
   procedure Execute
     (Self   : On_Project_Changed;
      Kernel : not null access Kernel_Handle_Record'Class);
   --  Called when the project has just changed

   type On_Open_Recent is new Interactive_Command with record
      File : GNATCOLL.VFS.Virtual_File;
   end record;
   overriding
   function Execute
     (Self : access On_Open_Recent; Context : Interactive_Command_Context)
      return Command_Return_Type;
   --  Called to reopen a project file

   type Clipboard_Kind is (Cut, Copy, Paste, Paste_Previous);
   type Clipboard_Command is new Interactive_Command with record
      Kind : Clipboard_Kind;
   end record;
   overriding
   function Execute
     (Command : access Clipboard_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Perform the various actions associated with the clipboard

   type Has_Alire_Filter_Record is new GPS.Kernel.Action_Filter_Record
   with null record;
   overriding
   function Filter_Matches_Primitive
     (Self    : access Has_Alire_Filter_Record;
      Context : GPS.Kernel.Selection_Context) return Boolean;
   --  Used to filter Alire related actions/menus

   type Clipboard_Action_Context is new GPS.Kernel.Action_Filter_Record
   with record
      Kind : Clipboard_Kind;
   end record;
   overriding
   function Filter_Matches_Primitive
     (Self    : access Clipboard_Action_Context;
      Context : GPS.Kernel.Selection_Context) return Boolean;
   --  Used to filter the clipboard actions

   type Save_Desktop_Command is new Interactive_Command with null record;
   overriding
   function Execute
     (Command : access Save_Desktop_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  File->Save Desktop menu

   type Change_Dir_Command is new Interactive_Command with null record;
   overriding
   function Execute
     (Command : access Change_Dir_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  File->Change Directory... menu

   type Save_All_Command is new Interactive_Command with null record;
   overriding
   function Execute
     (Command : access Save_All_Command; Context : Interactive_Command_Context)
      return Command_Return_Type;
   --  File->Save All menu

   type Exit_Command is new Interactive_Command with null record;
   overriding
   function Execute
     (Command : access Exit_Command; Context : Interactive_Command_Context)
      return Command_Return_Type;
   --  File->Exit menu

   type Open_Project_Command is new Interactive_Command with null record;
   overriding
   function Execute
     (Command : access Open_Project_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Project->Open menu

   type Open_Alire_Crate_Command is new Interactive_Command with null record;
   overriding
   function Execute
     (Command : access Open_Alire_Crate_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Project->Open Alire Crate

   type Reload_Project_Command is new Interactive_Command with null record;
   overriding
   function Execute
     (Command : access Reload_Project_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Callback for the Project->Recompute Project menu

   type Recompute_Recent_Menus_Command is new Root_Command with record
      Kernel : Kernel_Handle;
   end record;
   overriding
   function Execute
     (Command : access Recompute_Recent_Menus_Command)
      return Command_Return_Type;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding
   function Filter_Matches_Primitive
     (Self    : access Has_Alire_Filter_Record;
      Context : GPS.Kernel.Selection_Context) return Boolean
   is
      use GNATCOLL.Tribooleans;
   begin
      if Menu_Module.Is_Alire_Available = Indeterminate then
         Menu_Module.Is_Alire_Available :=
           To_TriBoolean (Is_Alire_Available (Get_Kernel (Context)));
      end if;

      return To_Boolean (Menu_Module.Is_Alire_Available);
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding
   function Filter_Matches_Primitive
     (Self    : access Clipboard_Action_Context;
      Context : GPS.Kernel.Selection_Context) return Boolean
   is
      Kernel               : constant Kernel_Handle := Get_Kernel (Context);
      Tooltip_Focus_Widget : constant Gtk_Widget :=
        Get_Tooltip_Clipboard_Widget;
      Widget               : constant Gtk_Widget :=
        (if Tooltip_Focus_Widget /= null then Tooltip_Focus_Widget
         else Get_Current_Focus_Widget (Kernel));

      package Implements_Editable is new
        Glib.Types.Implements
          (Gtk.Editable.Gtk_Editable,
           Gtk_Widget_Record,
           Gtk_Widget);
      function "+"
        (Widget : access Gtk_Widget_Record'Class)
         return Gtk.Editable.Gtk_Editable
      renames Implements_Editable.To_Interface;

   begin
      --  Return False is there is no focus widget
      if Widget = null then
         return False;
      end if;

      --  Verify that the currently focused widget is relevant for clipboard
      --  operations.

      if Is_A (Widget.Get_Type, Gtk.Editable.Get_Type) then
         declare
            Edit_Obj      : constant Gtk_Editable := +Widget;
            First, Last   : Gint;
            Has_Selection : Boolean;
         begin
            Get_Selection_Bounds
              (Editable      => Edit_Obj,
               Start_Pos     => First,
               End_Pos       => Last,
               Has_Selection => Has_Selection);

            case Self.Kind is
               when Copy =>
                  return Has_Selection;

               when Cut =>
                  return Has_Selection and then Get_Editable (Edit_Obj);

               when others =>
                  return Get_Editable (Edit_Obj);
            end case;
         end;

      elsif Widget.all in Gtk_Text_View_Record'Class then
         if Self.Kind = Paste then
            if Interactive_Consoles.Find_Interactive_Console (Widget) /= null
            then
               --  Disable Paste for interactive consoles because we have
               --  separate "Paste to console" action for this
               return False;
            end if;
         end if;

         declare
            Text_View : constant Gtk_Text_View := Gtk_Text_View (Widget);
         begin
            case Self.Kind is
               when Copy =>
                  return Text_View.Get_Buffer.Get_Has_Selection;

               when Cut =>
                  return
                    Text_View.Get_Buffer.Get_Has_Selection
                    and then Text_View.Get_Editable;

               when others =>
                  return Text_View.Get_Editable;
            end case;
         end;

      elsif Widget.all in Gtk_Label_Record'Class then
         declare
            Label         : constant Gtk_Label := Gtk_Label (Widget);
            First, Last   : Gint;
            Has_Selection : Boolean;
         begin
            Label.Get_Selection_Bounds
              (Start         => First,
               The_End       => Last,
               Has_Selection => Has_Selection);

            case Self.Kind is
               when Copy =>
                  return Has_Selection;

               when others =>
                  return False;
            end case;
         end;
      else
         return False;
      end if;
   end Filter_Matches_Primitive;

   -------------
   -- Execute --
   -------------

   overriding
   function Execute
     (Command : access Exit_Command; Context : Interactive_Command_Context)
      return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
   begin
      GPS.Main_Window.Quit (GPS_Window (Get_Main_Window (Kernel)));
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding
   function Execute
     (Command : access Change_Dir_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      Dir    : Virtual_File;
   begin
      Dir :=
        Select_Directory
          (-"Select a directory",
           History           => Get_History (Kernel),
           Base_Directory    => Get_Current_Dir,
           Use_Native_Dialog => Use_Native_Dialogs.Get_Pref,
           Parent            => Get_Current_Window (Kernel));

      if Dir /= No_File then
         Change_Dir (Dir);
      end if;
      return Commands.Success;

   exception
      when VFS_Directory_Error =>
         Kernel.Insert
           ("Cannot change to directory: " & Dir.Display_Full_Name,
            Mode => Error);
         return Commands.Failure;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding
   function Execute
     (Command : access Save_All_Command; Context : Interactive_Command_Context)
      return Command_Return_Type
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      Ignore : Boolean;
      pragma Unreferenced (Command, Ignore);
   begin
      Ignore := Save_MDI_Children (Kernel, Force => False);
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding
   function Execute
     (Command : access Save_Desktop_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
   begin
      Save_Desktop (Kernel);
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding
   function Execute
     (Command : access Clipboard_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel    : constant Kernel_Handle := Get_Kernel (Context.Context);
      Widget    : constant Gtk_Widget := Get_Current_Focus_Widget (Kernel);
      Clipboard : constant Clipboard_Access := Get_Clipboard (Kernel);

   begin
      if Widget = null or else Clipboard = null then
         return Commands.Failure;
      end if;

      if Widget /= null then
         case Command.Kind is
            when Cut =>
               Cut_Clipboard (Clipboard, Widget);

            when Copy =>
               Copy_Clipboard (Clipboard, Widget);

            when Paste =>
               Paste_Clipboard (Clipboard);

            when Paste_Previous =>
               Paste_Previous_Clipboard (Clipboard, Widget);
         end case;
         return Commands.Success;

      else
         return Commands.Failure;
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding
   function Execute
     (Self : access On_Open_Recent; Context : Interactive_Command_Context)
      return Command_Return_Type is
   begin
      Load_Project (Get_Kernel (Context.Context), Self.File);
      return Standard.Commands.Success;
   end Execute;

      -------------
      -- Execute --
      -------------

   overriding
   procedure Execute
     (Self   : On_Project_Changing;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File)
   is
      Filename : constant String := UTF8_Full_Name (File);
   begin
      --  We are loading an Alire crate: store the Alire manifest
      --  (alire.toml) as a recent project in the history.
      if GNATCOLL.Utils.Ends_With (Filename, "alire.toml") then
         Add_To_History (Kernel, Project_Files_History_Key, Filename);
         Menu_Module.Alire_Crate_Loaded := True;
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute
     (Self   : On_Project_Changed;
      Kernel : not null access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Self);
      Project : constant Project_Type := Get_Project (Kernel);
      Path    : constant Virtual_File := Project_Path (Project);
      Command : Command_Access;
   begin
      if Path = No_File then
         return;
      end if;

      --  Store the project file that just got loaded as a recent project
      --  only if it has not been loaded through an Alire crate.
      if not Menu_Module.Alire_Crate_Loaded then
         Add_To_History
           (Kernel, Project_Files_History_Key, UTF8_Full_Name (Path));
      end if;

      --  We cannot recompute the old menus and actions immediately as
      --  the project has changed because of a saw-the-branch-you're-on
      --  problem: this function can be called from within the very command
      --  that we're removing.
      --  To solve this, do this in a timeout.

      Command :=
        new Recompute_Recent_Menus_Command'
          (Root_Command with Kernel => Kernel_Handle (Kernel));
      GPS.Kernel.Task_Manager.Launch_Background_Command
        (Kernel            => Kernel,
         Command           => Command,
         Active            => False,
         Show_Bar          => False,
         Block_Exit        => False,
         Start_Immediately => False);
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding
   function Execute
     (Command : access Recompute_Recent_Menus_Command)
      return Command_Return_Type
   is
      Kernel          : constant Kernel_Handle := Command.Kernel;
      Hist            : constant History := Kernel.Get_History;
      Name_List       : VSS.String_Vectors.Virtual_String_Vector :=
        Get_History (Kernel.Get_History.all, Project_Files_History_Key);
      Max_Length      : constant Integer :=
        Get_Max_Length (Hist.all, Project_Files_History_Key);
      File_Menu_Paths : constant Unbounded_String_Array :=
        Split
          (To_String (Menu_List_For_Action ("open project dialog")),
           On => '/');
      Project_File    : constant Virtual_File :=
        Project_Path (Get_Project (Kernel));
      Project_Path    : constant VSS.Strings.Virtual_String :=
        GNATCOLL.VFS.VSS_Utils.Full_Name (Project_File);
      Is_Full         : Boolean := False;

      function Action_Name (Name : VSS.Strings.Virtual_String) return String;
      --  Name formatting

      procedure Create_New_Menu
        (File : Virtual_File; Prepend : Boolean := True);
      --  Create new action and menu for a Open Recent Project

      -----------------
      -- Action_Name --
      -----------------

      function Action_Name (Name : VSS.Strings.Virtual_String) return String is
         Template : VSS.Strings.Templates.Virtual_String_Template :=
           "open recent project: {}";

      begin
         return
           VSS.Strings.Conversions.To_UTF_8_String
             (Template.Format (VSS.Strings.Formatters.Strings.Image (Name)));
      end Action_Name;

      ---------------------
      -- Create_New_Menu --
      ---------------------

      procedure Create_New_Menu
        (File : Virtual_File; Prepend : Boolean := True)
      is
         Name : constant VSS.Strings.Virtual_String :=
           GNATCOLL.VFS.VSS_Utils.Full_Name (File);

      begin
         --  Add new menus.
         --
         --  We retrieve the toplevel menu from the 'open project dialog'
         --  action: this ensures that the 'open recent project' and the
         --  'open file' actions are always in the same toplevel menu,
         --  even when menus.xml has been changed by the user.
         Register_Action
           (Kernel,
            Name        => Action_Name (Name),
            Command     =>
              new On_Open_Recent'(Interactive_Command with File => File),
            Description => Action_Name (Name),
            Category    => "Internal");
         Register_Menu
           (Kernel,
            Path    =>
              To_String (File_Menu_Paths (File_Menu_Paths'First))
              & "/Open Recent Projects/"
              & Escape_Underscore (File.Display_Base_Name),
            Action  => Action_Name (Name),
            Prepend => Prepend);

         if Prepend then
            Menu_Module.Recent_Project_Actions.Prepend (Action_Name (Name));
         else
            Menu_Module.Recent_Project_Actions.Append (Action_Name (Name));
         end if;
      end Create_New_Menu;

   begin
      if Menu_Module.Recent_Project_Actions.Is_Empty then

         --  Get the recently loaded projects using the dedicated history key
         Name_List :=
           Get_History (Kernel.Get_History.all, Project_Files_History_Key);

         for Name of Name_List loop
            declare
               File : constant Virtual_File :=
                 GNATCOLL.VFS.VSS_Utils.Create (Name);

            begin
               --  Create a menu if the project actually exists. Remove
               --  it from the history otherwise.

               if File.Is_Regular_File then
                  Create_New_Menu (File, Prepend => False);

               else
                  Remove_From_History
                    (Hist            => Kernel.Get_History.all,
                     Key             => Project_Files_History_Key,
                     Entry_To_Remove => Name);
               end if;
            end;
         end loop;

      else
         Is_Full := Name_List.Length = Max_Length;

         if Menu_Module.Recent_Project_Actions.Contains
           (Action_Name (Project_Path))
         then
            --  The action already exist: do nothing
            return Success;
         end if;

         --  If necessary pop the oldest item to make place
         if Is_Full then
            declare
               Name : constant String :=
                 Action_Name (Name_List.Last_Element);
               C    : Action_Lists.Cursor :=
                 Menu_Module.Recent_Project_Actions.Find (Name);
            begin
               Unregister_Action (Kernel, Name, True);
               Menu_Module.Recent_Project_Actions.Delete (C);
            end;
         end if;

         --  Create a menu item for the currently loaded project
         --  only if it has not been loaded through Alire.
         if not Menu_Module.Alire_Crate_Loaded then
            Create_New_Menu (Project_File, Prepend => True);
         end if;
         Menu_Module.Alire_Crate_Loaded := False;
      end if;
      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding
   function Execute
     (Command : access Reload_Project_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
   begin
      Reload_Project_If_Needed (Kernel);
      Recompute_View (Kernel);
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding
   function Execute
     (Command : access Open_Project_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel  : constant Kernel_Handle := Get_Kernel (Context.Context);
      Success : Boolean
      with Unreferenced;
   begin
      Success :=
        Display_Open_Project_Dialog
          (Kernel, Parent => Get_Current_Window (Kernel));
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding
   function Execute
     (Command : access Open_Alire_Crate_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel  : constant Kernel_Handle := Get_Kernel (Context.Context);
      Success : Boolean
      with Unreferenced;
   begin
      Success :=
        Display_Open_Alire_Crate_Dialog
          (Kernel, Parent => Get_Current_Window (Kernel));
      return Commands.Success;
   end Execute;

   ---------------------------
   -- Register_Common_Menus --
   ---------------------------

   procedure Register_Common_Menus
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Command : Interactive_Command_Access;
      Has_Alire_Filter : Action_Filter;
   begin
      Register_Action
        (Kernel,
         "open project dialog",
         new Open_Project_Command,
         Icon_Name   => "gps-open-project-symbolic",
         Description => -"Open the Open Project dialog");

      Has_Alire_Filter := new Has_Alire_Filter_Record;
      Register_Filter
        (Kernel => Kernel,
         Filter => Has_Alire_Filter,
         Name   => "is_alire_available");

      Register_Action
        (Kernel,
         "open alire crate dialog",
         new Open_Alire_Crate_Command,
         Icon_Name   => "alr-logo-symbolic",
         Filter      => Has_Alire_Filter,
         Description => -"Open the Open Alire Crate dialog");

      Project_Changing_Hook.Add (new On_Project_Changing);
      Project_Changed_Hook.Add (new On_Project_Changed);

      Register_Action
        (Kernel,
         "reload project",
         new Reload_Project_Command,
         Description =>
           -("Recompute the list of source files for the project. This should"
             & " be used whenever you create or remove files outside of GNAT"
             & " Studio. Can also be used to try to reload the previous"
             & " invalid project."),
         Icon_Name   => "gps-refresh-symbolic");

      Register_Action
        (Kernel,
         "save files and projects",
         new Save_All_Command,
         Description => -("Save all modified files and projects"));

      Register_Action
        (Kernel,
         "save desktop",
         new Save_Desktop_Command,
         Description =>
           -("Save the layout of the desktop to a file, so that it is"
             & " restored when GNAT Studio is restarted later with the same"
             & " project"));

      Register_Action
        (Kernel,
         "change directory",
         new Change_Dir_Command,
         Description => -"Change the current directory");

      Register_Action
        (Kernel,
         "exit",
         new Exit_Command,
         -"Exit GNAT Studio, after confirming whether to save modified"
         & " files");

      Command := new Clipboard_Command;
      Clipboard_Command (Command.all).Kind := Cut;
      Register_Action
        (Kernel,
         "Cut to Clipboard",
         Command,
         Description => -"Cut the current selection to the clipboard",
         Icon_Name   => "gps-cut-symbolic",
         Filter      =>
           new Clipboard_Action_Context'
             (Action_Filter_Record with Kind => Cut));

      Command := new Clipboard_Command;
      Clipboard_Command (Command.all).Kind := Copy;
      Register_Action
        (Kernel,
         "Copy to Clipboard",
         Command,
         Description => -"Copy the current selection to the clipboard",
         Icon_Name   => "gps-copy-symbolic",
         Filter      =>
           new Clipboard_Action_Context'
             (Action_Filter_Record with Kind => Copy));

      Command := new Clipboard_Command;
      Clipboard_Command (Command.all).Kind := Paste;
      Register_Action
        (Kernel,
         "Paste From Clipboard",
         Command,
         Description =>
           -"Paste the contents of the clipboard into the current text area",
         Icon_Name   => "gps-paste-symbolic",
         Filter      =>
           new Clipboard_Action_Context'
             (Action_Filter_Record with Kind => Paste));

      Command := new Clipboard_Command;
      Clipboard_Command (Command.all).Kind := Paste_Previous;
      Register_Action
        (Kernel,
         -"Paste Previous From Clipboard",
         Command,
         -("Cancel the previous Paste operation, and instead insert the text"
           & " copied before through Copy To Clipboard"),
         Icon_Name => "gps-paste-symbolic",
         Filter    =>
           new Clipboard_Action_Context'
             (Action_Filter_Record with Kind => Paste_Previous));
   end Register_Common_Menus;

end GPS.Menu;
