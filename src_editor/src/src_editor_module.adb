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

with Ada.Containers.Vectors;
with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.IO_Exceptions;                 use Ada.IO_Exceptions;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Default_Preferences.Enums;
with GNAT.OS_Lib;                       use GNAT.OS_Lib;
with GNAT.Regpat;

with VSS.String_Vectors;

with GNATCOLL.Projects;                 use GNATCOLL.Projects;
with GNATCOLL.Traces;                   use GNATCOLL.Traces;
with GNATCOLL.Utils;                    use GNATCOLL.Utils;
with GNATCOLL.VFS.VSS_Utils;

with Gdk.Event;                         use Gdk.Event;
with Glib.Object;                       use Glib.Object;
with Glib.Unicode;                      use Glib.Unicode;
with Glib.Values;

with Gtk.Label;                         use Gtk.Label;
with Gtk.Menu;
with Gtk.Menu_Item;                     use Gtk.Menu_Item;
with Gtk.Separator_Menu_Item;           use Gtk.Separator_Menu_Item;
with Gtk.Style_Context;                 use Gtk.Style_Context;
with Gtk.Window;                        use Gtk.Window;

with Gtkada.File_Selector;              use Gtkada.File_Selector;
with Gtkada.Handlers;                   use Gtkada.Handlers;

with Pango.Layout;                      use Pango.Layout;

with Aliases_Module;                    use Aliases_Module;
with Casing_Exceptions;                 use Casing_Exceptions;
with Case_Handling;                     use Case_Handling;
with Commands;
with Commands.Controls;                 use Commands.Controls;
with Commands.Interactive;              use Commands, Commands.Interactive;
with Completion_Module;
with Config;                            use Config;
with Default_Preferences;               use Default_Preferences;
with Find_Utils;                        use Find_Utils;
with File_Utils;                        use File_Utils;
with GPS.Default_Styles;                use GPS.Default_Styles;
with GPS.Intl;                          use GPS.Intl;
with GPS.Editors.Line_Information;      use GPS.Editors.Line_Information;
with GPS.Kernel.Actions;                use GPS.Kernel.Actions;
with GPS.Kernel.Contexts;               use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;                  use GPS.Kernel.Hooks;
with GPS.Kernel.Messages;               use GPS.Kernel.Messages;
with GPS.Kernel.Modules.UI;             use GPS.Kernel.Modules.UI;
with GPS.Kernel.Project;                use GPS.Kernel.Project;
with GPS.Kernel.Scripts;                use GPS.Kernel.Scripts;
with GPS.Kernel.Task_Manager;           use GPS.Kernel.Task_Manager;

with GUI_Utils;                         use GUI_Utils;
with Histories;                         use Histories;
with Projects;                          use Projects;
with Src_Contexts;                      use Src_Contexts;
with Src_Editor_Box;                    use Src_Editor_Box;
with Src_Editor_Buffer.Buffer_Commands; use Src_Editor_Buffer.Buffer_Commands;
with Src_Editor_Buffer.Line_Information;
use Src_Editor_Buffer.Line_Information;
with Src_Editor_Buffer.Text_Handling;   use Src_Editor_Buffer.Text_Handling;
with Src_Editor_Module.Line_Highlighting;
with Src_Editor_Module.Editors;         use Src_Editor_Module.Editors;
with Src_Editor_Module.Markers;         use Src_Editor_Module.Markers;
with Src_Editor_Module.Shell;           use Src_Editor_Module.Shell;
with Src_Editor_Module.Commands;        use Src_Editor_Module.Commands;
with Src_Editor_Module.Messages;

with Src_Editor_View.Commands;          use Src_Editor_View.Commands;
with Src_Editor_View;                   use Src_Editor_View;
with String_Utils;                      use String_Utils;
with Vsearch;                           use Vsearch;
with VSS.Strings;                       use VSS.Strings;
with VSS.Strings.Conversions;           use VSS.Strings.Conversions;
with VSS.Transformers.Casing;           use VSS.Transformers.Casing;
with Xref;                              use Xref;

package body Src_Editor_Module is

   use type Basic_Types.Visible_Column_Type;
   use type Pango.Font.Pango_Font_Description;

   Me : constant Trace_Handle := Create ("GPS.SOURCE_EDITOR.MODULE");

   Hist_Key : constant History_Key := "reopen_files";
   --  Key to use in the kernel histories to store the most recently opened
   --  files.

   Underscore : constant Gunichar := UTF8_Get_Char ("_");
   Space      : constant Gunichar := UTF8_Get_Char (" ");
   Backspace  : constant Gunichar := 8;

   File_Properties_Contextual_Group : constant Integer := Integer'Last;
   --  Always keep the "Properties..." contextual menu at the last position

   Range_Format_Name_Prefix   : constant String := "Editor-Range-Formatter-";
   On_Type_Format_Name_Prefix : constant String := "Editor-On-Type-Formatter-";
   Move_Cursor_Format_Prefix  : constant String :=
     "Editor-Move-Cursor-Formatter-";
   Limit_LSP_Format_Prefix    : constant String := "Limit-LSP-Formatting-";
   Disabled_Pref_Choice       : constant String := "Disabled";

   type Editor_Child_Record is new GPS_MDI_Child_Record with null record;

   overriding procedure Tab_Contextual
     (Child : access Editor_Child_Record;
      Menu  : access Gtk.Menu.Gtk_Menu_Record'Class);
   overriding function Get_Tooltip
     (Self  : not null access Editor_Child_Record) return String;
   overriding function Get_Tooltip_Is_Markup
     (Self  : not null access Editor_Child_Record) return Boolean is (True);
   overriding function Get_Command_Queue
     (Child : access Editor_Child_Record)
      return Standard.Commands.Command_Queue;
   overriding function Dnd_Data
     (Child : access Editor_Child_Record; Copy : Boolean) return MDI_Child;
   overriding function Save_Desktop
     (Self  : not null access Editor_Child_Record) return Node_Ptr;
   overriding function Get_Child_Class
     (Self  : not null access Editor_Child_Record) return Class_Type;
   overriding procedure Reload
     (Self : not null access Editor_Child_Record);
   overriding function Needs_To_Be_Saved
     (Self : not null access Editor_Child_Record) return Boolean;
   overriding function Report_Deleted_File
     (Self   : not null access Editor_Child_Record;
      Exists : Boolean) return Boolean;
   overriding function Build_Context
     (Self  : not null access Editor_Child_Record;
      Event : Gdk.Event.Gdk_Event := null)
      return Selection_Context;
   overriding function Has_Menu_Bar_When_Floating
     (Child : not null access Editor_Child_Record) return Boolean
      is (True) with Inline;
   --  See inherited documentation

   procedure Update_Editor_Tabs_Status
     (Kernel : not null access Kernel_Handle_Record'Class;
      Files  : Basic_Types.File_Sets.Set);
   --  Update the the tab status of the editor views opened for the given
   --  files.

   type On_Open_File is new Open_File_Hooks_Function with null record;
   overriding function Execute
     (Self             : On_Open_File;
      Kernel           : not null access Kernel_Handle_Record'Class;
      File             : Virtual_File;
      Line             : Integer;
      Column, Column_End : Basic_Types.Visible_Column_Type;
      Enable_Navigation, New_File, Force_Reload, Focus : Boolean;
      Project          : Project_Type;
      Group            : Child_Group;
      Initial_Position : Child_Position;
      Areas            : Allowed_Areas;
      Title            : String;
      Is_Load_Desktop  : Boolean) return Boolean;
   --  Reacts to the Open_File_Action_Hook

   type On_File_Line_Action is new Line_Info_Hooks_Function with null record;
   overriding procedure Execute
     (Self       : On_File_Line_Action;
      Kernel     : not null access Kernel_Handle_Record'Class;
      Identifier : String;
      File       : Virtual_File;
      Every_Line : Boolean;
      Tooltip    : String;
      Info       : access Line_Information_Array;
      Icon_Name  : String);
   --  Reacts to the File_Line_Action_Hook

   type On_Open_Recent is new Interactive_Command with record
      File  : GNATCOLL.VFS.Virtual_File;
   end record;
   overriding function Execute
      (Self    : access On_Open_Recent;
       Context : Interactive_Command_Context) return Command_Return_Type;
   --  Called to reopen a source file

   type On_Word_Added is new Character_Hooks_Function with null record;
   overriding procedure Execute
     (Self        : On_Word_Added;
      Kernel      : not null access Kernel_Handle_Record'Class;
      File        : Virtual_File;
      Char        : Glib.Gunichar := 0;
      Interactive : Boolean := True);
   --  Reacts to the word_added Hook

   type On_Character_Added is new Character_Hooks_Function with null record;
   overriding procedure Execute
     (Self        : On_Character_Added;
      Kernel      : not null access Kernel_Handle_Record'Class;
      File        : Virtual_File;
      Character   : Glib.Gunichar;
      Interactive : Boolean);
   --  Reacts to the character_added Hook

   type On_Project_Changed is new Simple_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Project_Changed;
      Kernel : not null access Kernel_Handle_Record'Class);
   --  Called when the project has changed

   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child;
   --  Support functions for the MDI

   type Select_All_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Select_All_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Edit->Select All menu

   type Insert_File_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Insert_File_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Edit->Insert File... menu

   type On_File_Edited is new File_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_File_Edited;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File);
   --  Callback for the "file_edited" hook

   type On_File_Reloaded is new File_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_File_Reloaded;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File);
   --  Callback for the "file_reloaded" hook

   type On_File_Changed_On_Disk is new File_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_File_Changed_On_Disk;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File);
   --  Callback for the "file_changed_on_disk" hook

   type On_File_Renamed is new File2_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_File_Renamed;
      Kernel : not null access Kernel_Handle_Record'Class;
      File, Renamed : Virtual_File);
   --  Callback for the "file_renamed" hook

   type On_File_Saved is new File_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_File_Saved;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File);
   --  Callback for the "file_saved" hook

   type On_Cursor_Stopped is new File_Location_Hooks_Function
      with null record;
   overriding procedure Execute
     (Self   : On_Cursor_Stopped;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Line, Column : Integer;
      Project : Project_Type);
   --  Callback for the "cursor_stopped" hook

   type On_Semantic_Tree_Updated is new File_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Semantic_Tree_Updated;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File);

   type On_Pref_Changed is new Preferences_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference);
   --  Called when the preferences have changed

   type On_Deleting is new File_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Deleting;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File);
   --  Callback for the "file_deleting" hook

   procedure Add_To_Recent_Menu
     (Kernel : access Kernel_Handle_Record'Class; File : Virtual_File);
   --  Add an entry for File to the Recent menu, if needed
   --  Remove the older menu if the history is full.

   function Get_File_Identifier (Child  : MDI_Child) return Virtual_File;
   --  Return the file identifier if Child is a file editor

   function Expand_Aliases_Entities
     (Kernel    : access Kernel_Handle_Record'Class;
      Expansion : String;
      Special   : Character) return String;
   --  Does the expansion of special entities in the aliases

   procedure On_Editor_Destroy
     (Widget : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues);
   --  Callback to call when an editor is about to be destroyed

   procedure On_Lock_Or_Unlock_Tab_Contextual_Menu
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  Called when the user clicks on the 'Lock or unlock editor' tab
   --  contextual menu.

   procedure On_Lock_Or_Unlock_Split_Tab_Contextual_Menu
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  Called when the user clicks on the 'Lock or unlock editor (split)'
   --  tab contextual menu.

   procedure Update_Cache_On_Focus
     (Child : access Gtk_Widget_Record'Class);
   --  Make sure that the last view for a file is reflected in the cache, so
   --  that we always use that one by default when looking for the last editor
   --  for a given file.

   function Goto_Other_File_Label_Factory
     (Context : Selection_Context) return String;
   --  Return "implementation" if the current file is a specification one, or
   --  "specification" if it's an implementation one.
   --  Used to have a proper label for the "goto other file" contextual
   --  menu.

   type Is_Not_Makefile_Context is new GPS.Kernel.Action_Filter_Record
      with null record;
   overriding function Filter_Matches_Primitive
     (Context : access Is_Not_Makefile_Context;
      Ctxt    : GPS.Kernel.Selection_Context) return Boolean;
   --  Filter which passes when the context contains a file which is not
   --  a Makefile.

   type Is_Ada_File_Context is new GPS.Kernel.Action_Filter_Record
      with null record;
   overriding function Filter_Matches_Primitive
     (Context : access Is_Ada_File_Context;
      Ctxt    : GPS.Kernel.Selection_Context) return Boolean;
   --  Filter which passes when the context contains an Ada file.

   type Undo_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Undo_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Redo_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Redo_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Has_Undo_Filter is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Has_Undo_Filter;
      Context : Selection_Context) return Boolean;

   type Has_Redo_Filter is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Has_Redo_Filter;
      Context : Selection_Context) return Boolean;

   type Has_Selection_Filter is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Has_Selection_Filter;
      Context : Selection_Context) return Boolean;

   procedure Create_New_Recent_Menu
     (Kernel  : access Kernel_Handle_Record'Class;
      File    : GNATCOLL.VFS.Virtual_File;
      Prepend : Boolean);
   --  Create a submenu for File in "Open Recent Files"

   procedure Regenerate_Recent_Files_Menu
     (Kernel : access Kernel_Handle_Record'Class);
   --  Regenerate the "Open Recent Files" menu, based on the contents of the
   --  history key.

   function Get_Lang_Formatter_Pref
     (Lang : Language.Language_Access; Prefix : String) return Preference;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Has_Undo_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter, Context);
      UR : constant Undo_Redo :=
        Source_Editor_Module (Src_Editor_Module_Id).Undo_Redo;
   begin
      return Can_Undo (UR);
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Has_Redo_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter, Context);
      UR : constant Undo_Redo :=
        Source_Editor_Module (Src_Editor_Module_Id).Undo_Redo;
   begin
      return Can_Redo (UR);
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Has_Selection_Filter;
      Context : Selection_Context) return Boolean is
      pragma Unreferenced (Filter);
   begin
      return Has_Area_Information (Context);
   end Filter_Matches_Primitive;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Undo_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command, Context);
      UR : constant Undo_Redo :=
        Source_Editor_Module (Src_Editor_Module_Id).Undo_Redo;
   begin
      Undo (UR);
      return Standard.Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Redo_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command, Context);
      UR : constant Undo_Redo :=
        Source_Editor_Module (Src_Editor_Module_Id).Undo_Redo;
   begin
      Redo (UR);
      return Standard.Commands.Success;
   end Execute;

   -------------------------
   -- Get_Undo_Redo_Queue --
   -------------------------

   function Get_Undo_Redo_Queue return Standard.Commands.Command_Queue is
      UR : Undo_Redo;
   begin
      if Src_Editor_Module_Id = null then
         return Null_Command_Queue;
      end if;

      UR := Source_Editor_Module (Src_Editor_Module_Id).Undo_Redo;

      if UR = null then
         return Null_Command_Queue;
      end if;

      return Get_Undo_Redo_Queue (UR);
   end Get_Undo_Redo_Queue;

   --------------
   -- Can_Undo --
   --------------

   function Can_Undo return Boolean is
   begin
      return Src_Editor_Module_Id /= null and then Can_Undo
        (Source_Editor_Module (Src_Editor_Module_Id).Undo_Redo);
   end Can_Undo;

   ------------------------
   -- Set_Global_Command --
   ------------------------

   procedure Set_Global_Command (Command : Command_Access) is
   begin
      if Src_Editor_Module_Id /= null then
         Set_Global_Command
           (Source_Editor_Module (Src_Editor_Module_Id).Undo_Redo, Command);
      end if;
   end Set_Global_Command;

   ----------------------------
   -- Execute_Global_Command --
   ----------------------------

   function Execute_Global_Command
     return Standard.Commands.Command_Return_Type is
   begin
      if Src_Editor_Module_Id /= null then
         return Execute_Global_Command
           (Source_Editor_Module (Src_Editor_Module_Id).Undo_Redo);
      end if;
      return Standard.Commands.Success;
   end Execute_Global_Command;

   ----------------------------
   -- Change_Undo_Redo_Queue --
   ----------------------------

   procedure Change_Undo_Redo_Queue (Queue : Command_Queue) is
      UR : Undo_Redo;
   begin
      if Src_Editor_Module_Id /= null then
         UR := Source_Editor_Module (Src_Editor_Module_Id).Undo_Redo;
         if Queue = Null_Command_Queue then
            Unset_Undo_Redo_Queue (UR);
         else
            Set_Undo_Redo_Queue (UR, Queue);
         end if;
      end if;
   end Change_Undo_Redo_Queue;

   -----------------------
   -- On_Editor_Destroy --
   -----------------------

   procedure On_Editor_Destroy
     (Widget : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues)
   is
      pragma Unreferenced (Params);
      Id : constant Source_Editor_Module :=
             Source_Editor_Module (Src_Editor_Module_Id);
      Child : constant MDI_Child := MDI_Child (Widget);
      New_Map : Editors_Hash.Map;
   begin
      if Id /= null then
         if Id.Last_Focused_Editor = Child then
            Id.Last_Focused_Editor := null;
         end if;

         for E of Id.Editors loop
            if E.Child /= Child then
               New_Map.Insert (Get_Filename (E.Child), E);
            end if;
         end loop;
         Id.Editors := New_Map;
      end if;
   exception
      when E : others => Trace (Me, E);
   end On_Editor_Destroy;

   -------------------------------------------
   -- On_Lock_Or_Unlock_Tab_Contextual_Menu --
   -------------------------------------------

   procedure On_Lock_Or_Unlock_Tab_Contextual_Menu
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Command : Interactive_Command_Access;
      Proxy   : Command_Access;
   begin
      Command :=
        new Lock_Or_Unlock_Commmand'(Root_Command with Split => False);

      Proxy := Create_Proxy
        (Command,
         Create_Null_Context (New_Context (Kernel, Src_Editor_Module_Id)));
      Launch_Background_Command
        (Kernel          => Kernel,
         Command         => Proxy,
         Active          => True,
         Show_Bar        => False,
         Block_Exit      => False);
   end On_Lock_Or_Unlock_Tab_Contextual_Menu;

   -------------------------------------------------
   -- On_Lock_Or_Unlock_Split_Tab_Contextual_Menu --
   -------------------------------------------------

   procedure On_Lock_Or_Unlock_Split_Tab_Contextual_Menu
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Command : Interactive_Command_Access;
      Proxy   : Command_Access;
   begin
      Command :=
        new Lock_Or_Unlock_Commmand'(Root_Command with Split => True);

      Proxy := Create_Proxy
        (Command,
         Create_Null_Context (New_Context (Kernel, Src_Editor_Module_Id)));
      Launch_Background_Command
        (Kernel          => Kernel,
         Command         => Proxy,
         Active          => True,
         Show_Bar        => False,
         Block_Exit      => False);
   end On_Lock_Or_Unlock_Split_Tab_Contextual_Menu;

   --------------
   -- Dnd_Data --
   --------------

   overriding function Dnd_Data
     (Child : access Editor_Child_Record; Copy : Boolean) return MDI_Child
   is
      Editor : Source_Editor_Box;
      Kernel : Kernel_Handle;
   begin
      if Copy then
         Editor := Get_Source_Box_From_MDI (MDI_Child (Child));
         Kernel := Get_Kernel (Editor);
         return Find_MDI_Child
           (Get_MDI (Kernel),
            New_View
              (Kernel, Editor, Get_Project (Editor))); --  for same project
      else
         return MDI_Child (Child);
      end if;
   end Dnd_Data;

   -------------------------
   -- Report_Deleted_File --
   -------------------------

   overriding function Report_Deleted_File
     (Self : not null access Editor_Child_Record;
      Exists : Boolean) return Boolean
   is
      Title   : constant String := Self.Get_Short_Title;
      Del_Tag : constant String := " (deleted)";
      Has_Tag : constant Boolean := Ends_With (Title, Del_Tag);
--        Ada.Strings.Fixed.Index (Title, Del_Tag) > 0;
   begin
      --  Indicate in the title that the file has been deleted, the user
      --  can save the buffer to recreate it or close it to remove the
      --  buffered data.
      if not Exists and not Has_Tag then
         Self.Set_Title (Title & Del_Tag);
      elsif Exists and Has_Tag then
         Self.Set_Title (Title (Title'First .. Title'Last - Del_Tag'Length));
      end if;

      return Exists;
   end Report_Deleted_File;

   ------------
   -- Reload --
   ------------

   overriding procedure Reload (Self : not null access Editor_Child_Record) is
      Editor  : constant Source_Editor_Box :=
        Get_Source_Box_From_MDI (MDI_Child (Self));
      Buffer  : constant Source_Buffer := Get_Buffer (Editor);
      File    : constant Virtual_File := Buffer.Get_Filename;
      Success : Boolean;
      Line    : Editable_Line_Type;
      Column  : Character_Offset_Type;
   begin
      if not File.Is_Regular_File then
         Open_File_Action_Hook.Run
            (Editor.Get_Kernel,
             File => File,
             Project => No_Project,
             Line => -1);   --  close editors for File
      else
         Get_Cursor_Position (Buffer, Line, Column);

         --  Avoid moving the cursor around when reloading the file: we will
         --  replace the cursor at its original position right after anyway.

         Set_Avoid_Cursor_Move_On_Changes (Buffer, True);
         Load_File
           (Buffer,
            Filename        => File,
            Lang_Autodetect => True,
            Success         => Success);
         Set_Avoid_Cursor_Move_On_Changes (Buffer, False);

         --  Replace the cursor to its original position. Don't perform
         --  synchronous scrolling since the editor may need to be refreshed
         --  in some idle functions after reloading the file.

         if Is_Valid_Position (Buffer, Line) then
            Set_Cursor_Location
              (Editor,
               Line,
               Column,
               False,
               Centering             => Center,
               Synchronous_Scrolling => False);
         end if;
      end if;
   end Reload;

   -----------------------
   -- Needs_To_Be_Saved --
   -----------------------

   overriding function Needs_To_Be_Saved
     (Self : not null access Editor_Child_Record) return Boolean
   is
      Buffer : constant Source_Buffer :=
        Get_Buffer (Get_Source_Box_From_MDI (MDI_Child (Self)));
   begin
      return Buffer.Needs_To_Be_Saved;
   end Needs_To_Be_Saved;

   -------------------------
   -- Get_File_Identifier --
   -------------------------

   function Get_File_Identifier (Child : MDI_Child) return Virtual_File is
   begin
      if Child /= null
        and then Get_Widget (Child) /= null
        and then Get_Widget (Child).all in Source_Editor_Box_Record'Class
      then
         return Get_File_Identifier
           (Get_Buffer (Source_Editor_Box (Get_Widget (Child))));
      else
         return GNATCOLL.VFS.No_File;
      end if;
   end Get_File_Identifier;

   ------------------
   -- Get_Filename --
   ------------------

   function Get_Filename
     (Child : not null access MDI_Child_Record'Class)
      return GNATCOLL.VFS.Virtual_File
   is
   begin
      if Child /= null
        and then Get_Widget (Child) /= null
        and then Get_Widget (Child).all in Source_Editor_Box_Record'Class
      then
         return Get_Filename (Source_Editor_Box (Get_Widget (Child)));
      else
         return GNATCOLL.VFS.No_File;
      end if;
   end Get_Filename;

   -----------------
   -- Get_Project --
   -----------------

   function Get_Project
     (Child : not null access MDI_Child_Record'Class) return Project_Type is
   begin
      if Child /= null
        and then Get_Widget (Child) /= null
        and then Get_Widget (Child).all in Source_Editor_Box_Record'Class
      then
         return Get_Project (Source_Editor_Box (Get_Widget (Child)).Get_View);
      else
         return No_Project;
      end if;
   end Get_Project;

   -------------------
   -- For_All_Views --
   -------------------

   procedure For_All_Views
     (Kernel   : not null access Kernel_Handle_Record'Class;
      File     : Virtual_File;
      Callback : not null access procedure
        (Child : not null access GPS_MDI_Child_Record'Class))
   is
      Iter  : Child_Iterator := Get_MDI (Kernel).First_Child;
      Child : MDI_Child;
      Box   : Source_Editor_Box;

      package Child_Vectors is new Ada.Containers.Vectors
        (Positive, GPS_MDI_Child);
      Childs : Child_Vectors.Vector;

   begin
      loop
         Child := Get (Iter);
         exit when Child = null;

         if Get_Widget (Child).all in Source_Editor_Box_Record'Class then
            Box := Source_Editor_Box (Get_Widget (Child));

            if File = GNATCOLL.VFS.No_File or else File = Box.Get_Filename then
               Childs.Append (GPS_MDI_Child (Child));
            end if;
         end if;

         Next (Iter);
      end loop;

      for Child of Childs loop
         Callback (Child);
      end loop;
   end For_All_Views;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_File_Edited;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File)
   is
      pragma Unreferenced (Self);
      Id    : constant Source_Editor_Module :=
        Source_Editor_Module (Src_Editor_Module_Id);

      procedure On_View (Child : not null access GPS_MDI_Child_Record'Class);
      procedure On_View (Child : not null access GPS_MDI_Child_Record'Class) is
         File : constant Virtual_File := Get_Filename (Child);
      begin
         Child.Update_File_Info;
         --  Do this gymnastics so that the most recent view is the one
         --  found first through the cache
         Id.Editors.Include (File, (Child => MDI_Child (Child)));
      end On_View;
   begin
      Reset_Markers_For_File (Kernel, File);
      For_All_Views (Kernel, File, On_View'Access);
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_File_Reloaded;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File)
   is
      procedure On_View (Child : not null access GPS_MDI_Child_Record'Class);
      procedure On_View (Child : not null access GPS_MDI_Child_Record'Class) is
      begin
         Child.Update_File_Info;
      end On_View;
   begin
      Reset_Markers_For_File (Kernel, File);
      For_All_Views (Kernel, File, On_View'Access);
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_File_Changed_On_Disk;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File)
   is
      pragma Unreferenced (Self);
      procedure On_View (Child : not null access GPS_MDI_Child_Record'Class);
      procedure On_View (Child : not null access GPS_MDI_Child_Record'Class) is
      begin
         Check_Writable (Source_Editor_Box (Get_Widget (Child)));
      end On_View;

      Ignored : Boolean;
   begin
      Ignored := Check_Monitored_Files (Kernel, Interactive => False);
      For_All_Views (Kernel, File, On_View'Access);
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_File_Renamed;
      Kernel : not null access Kernel_Handle_Record'Class;
      File, Renamed : Virtual_File)
   is
      pragma Unreferenced (Kernel, Self);
      Id : constant Source_Editor_Module :=
             Source_Editor_Module (Src_Editor_Module_Id);
      E  : Element := No_Element;
   begin
      while Id.Editors.Contains (File) loop
         E := Id.Editors.Element (File);
         Id.Editors.Delete (File);
      end loop;

      if E /= No_Element then
         --  Monitor the new filename now that the file has been renamed.
         Monitor_File (GPS_MDI_Child (E.Child), Renamed);
         Id.Editors.Include (Renamed, E);
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_File_Saved;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File)
   is
      pragma Unreferenced (Self);

      procedure On_View (Child : not null access GPS_MDI_Child_Record'Class);
      procedure On_View (Child : not null access GPS_MDI_Child_Record'Class) is
      begin
         Child.Update_File_Info;
      end On_View;

   begin
      --  Insert the saved file in the Recent menu

      if File /= GNATCOLL.VFS.No_File
        and then not Is_Auto_Save (File)
      then
         Add_To_Recent_Menu (Kernel, File);
      end if;

      For_All_Views (Kernel, File, On_View'Access);
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Semantic_Tree_Updated;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File)
   is
      pragma Unreferenced (Self);
      Id  : constant Source_Editor_Module :=
        Source_Editor_Module (Src_Editor_Module_Id);
      C   : constant MDI_Child := Find_Editor (Kernel, File, No_Project);
      Box : constant Source_Editor_Box :=
        Get_Source_Box_From_MDI (C);
   begin
      if Box /= null then
         if Id.Show_Subprogram_Names then
            Update_Subprogram_Name (Box);
         end if;
         Box.Get_View.Queue_Draw;
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Cursor_Stopped;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Line, Column : Integer;
      Project : Project_Type)
   is
      pragma Unreferenced (Self, Line, Column);
      C   : constant MDI_Child := Find_Editor (Kernel, File, Project);
      Id  : constant Source_Editor_Module :=
        Source_Editor_Module (Src_Editor_Module_Id);
      Box : constant Source_Editor_Box :=
        Get_Source_Box_From_MDI (C);
   begin
      if Box /= null then
         if Id.Show_Subprogram_Names then
            Update_Subprogram_Name (Box);
         end if;
         Box.Get_View.Queue_Draw;
      end if;
   end Execute;

   ------------------
   -- Load_Desktop --
   ------------------

   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child
   is
      Src         : Source_Editor_Box;
      F           : Virtual_File;
      Str         : XML_Utils.String_Ptr;
      Line        : Editable_Line_Type := 1;
      Column      : Visible_Column_Type := 1;
      Child       : MDI_Child;
      Project     : Project_Type;
      pragma Unreferenced (MDI);

      Dummy  : Boolean;
      pragma Unreferenced (Dummy);
   begin
      if Node.Tag.all = "Source_Editor" then
         F := Get_File_Child (Node, "File");

         if F /= No_File then
            Str := Get_Field (Node, "Line");

            if Str /= null then
               Line := Editable_Line_Type'Value (Str.all);
            end if;

            Str := Get_Field (Node, "Column");

            if Str /= null then
               Column := Visible_Column_Type'Value (Str.all);
            end if;

            Str := Get_Field (Node, "project");
            if Str /= null then
               Project := Get_Registry (User).Tree.Project_From_Path
                 (Create (+Str.all));
            end if;

            Child := Find_Editor (User, F, No_Project);  --  any project
            if Child = null then
               --  If the file no longer exists on the disk, we still want
               --  to create an empty editor : this preserves the user's
               --  desktop and points out possible disk issues...

               Src := Open_File
                 (User, F,
                  Project         => Project,
                  Focus           => False,
                  Line            => Line,
                  Create_New      => True,
                  Column          => Column,
                  Column_End      => Column,
                  Is_Load_Desktop => True);
               if Src /= null then
                  Child := Find_Child (User, Src);
               end if;
            else
               Src := New_View
                 (User, Get_Source_Box_From_MDI (Child), Project);
               Child := Find_Child (User, Src);
            end if;
         end if;
      end if;

      return Child;

   exception
      when E : others =>
         Trace (Me, E);
         return null;
   end Load_Desktop;

   ---------------------
   -- Get_Child_Class --
   ---------------------

   overriding function Get_Child_Class
     (Self  : not null access Editor_Child_Record) return Class_Type is
   begin
      return New_Class (Self.Kernel, "EditorView");
   end Get_Child_Class;

   ------------------
   -- Save_Desktop --
   ------------------

   overriding function Save_Desktop
     (Self  : not null access Editor_Child_Record) return Node_Ptr
   is
      N, Child : Node_Ptr;
      Line     : Editable_Line_Type;
      Column   : Character_Offset_Type;
      File     : Virtual_File;
      Pref     : Editor_Desktop_Policy;
      Editor   : constant Source_Editor_Box :=
        Source_Editor_Box (Self.Get_Widget);
      Kernel   : constant Kernel_Handle := Self.Kernel;

   begin
      begin
         Pref := Save_Editor_Desktop.Get_Pref;
      exception
         when Constraint_Error =>
            Pref := From_Project;
      end;

      if Pref = Never then
         return null;
      end if;

      File   := Get_Filename (Editor);

      --  ??? For now, save with desktop only local files

      if not Is_Local (File) then
         return null;
      end if;

      if Pref = From_Project and then
        (Get_Registry (Kernel).Tree.Status /= From_File or else
         Get_Registry (Kernel).Tree.Info_Set (File).Is_Empty)
      then
         return null;
      end if;

      if File = No_File or else not Is_Regular_File (File) then
         return null;
      end if;

      N := new Node;
      N.Tag := new String'("Source_Editor");

      Add_File_Child (N, "File", File);

      Get_Cursor_Position (Get_Buffer (Editor), Line, Column);

      Child := new Node;
      Child.Tag := new String'("Line");
      Child.Value := new String'(Image (Integer (Line)));
      Add_Child (N, Child);

      Child := new Node;
      Child.Tag := new String'("Column");
      Child.Value := new String'(Image (Integer (Column)));
      Add_Child (N, Child);

      Child := new Node;
      Child.Tag := new String'("Column_End");
      Child.Value := new String'(Image (Integer (Column)));
      Add_Child (N, Child);

      Child := new Node;
      Child.Tag := new String'("project");
      Child.Value := new String'
        (Editor.Get_View.Get_Project.Project_Path.Display_Full_Name);
      Add_Child (N, Child);

      return N;
   end Save_Desktop;

   -----------------------------
   -- Get_Source_Box_From_MDI --
   -----------------------------

   function Get_Source_Box_From_MDI
     (Child : MDI_Child) return Source_Editor_Box is
   begin
      if Child = null then
         return null;
      else
         return Source_Editor_Box (Get_Widget (Child));
      end if;
   end Get_Source_Box_From_MDI;

   -------------------
   -- Is_Source_Box --
   -------------------

   function Is_Source_Box (Child : Gtkada.MDI.MDI_Child) return Boolean is
   begin
      if Child = null then
         return False;
      end if;

      return Get_Widget (Child).all in Source_Editor_Box_Record'Class;
   end Is_Source_Box;

   -------------------------
   -- Find_Current_Editor --
   -------------------------

   function Find_Current_Editor
     (Kernel          : access Kernel_Handle_Record'Class;
      Only_If_Focused : Boolean := False) return MDI_Child
   is
      Id : constant Source_Editor_Module :=
        Source_Editor_Module (Src_Editor_Module_Id);
      Child : constant MDI_Child := Get_Focus_Child (Get_MDI (Kernel));
   begin
      if Is_Source_Box (Child) then
         return Child;
      elsif Only_If_Focused then
         return null;
      elsif Id /= null and then Id.Last_Focused_Editor /= null then
         return Id.Last_Focused_Editor;
      end if;

      return Find_MDI_Child_By_Tag
        (Get_MDI (Kernel), Source_Editor_Box_Record'Tag);
   end Find_Current_Editor;

   ---------------------------
   -- Update_Cache_On_Focus --
   ---------------------------

   procedure Update_Cache_On_Focus
     (Child : access Gtk_Widget_Record'Class)
   is
      Id  : constant Source_Editor_Module :=
        Source_Editor_Module (Src_Editor_Module_Id);
      Box : constant Source_Editor_Box :=
        Get_Source_Box_From_MDI (MDI_Child (Child));
   begin
      --  Update the cache, so that the view is used when possible, since it
      --  was the last open in any case.

      if Id = null then
         return;
      end if;
      Id.Editors.Include (Get_Filename (Box), (Child => MDI_Child (Child)));
   end Update_Cache_On_Focus;

   -----------------------------------
   -- Goto_Other_File_Label_Factory --
   -----------------------------------

   function Goto_Other_File_Label_Factory
     (Context : Selection_Context) return String is
   begin
      if Has_File_Information (Context) then
         declare
            Kernel     : constant Kernel_Handle := Get_Kernel (Context);
            File       : constant Virtual_File := File_Information (Context);
            Info       : constant GNATCOLL.Projects.File_Info'Class :=
                           File_Info'Class
                             (Get_Registry
                                (Kernel).Tree.Info_Set (File).First_Element);
         begin
            if Info.Unit_Part in Unit_Body | Unit_Separate then
               return "Specification";
            else
               return "Implementation";
            end if;
         end;
      end if;

      return "";
   end Goto_Other_File_Label_Factory;

   --------------
   -- New_View --
   --------------

   function New_View
     (Kernel  : access Kernel_Handle_Record'Class;
      Current : Source_Editor_Box;
      Project : GNATCOLL.Projects.Project_Type) return Source_Editor_Box
   is
      Editor : Source_Editor_Box;
      Child  : GPS_MDI_Child;
      Num    : Natural;
      P      : constant Project_Type :=
        (if Project = No_Project and then Current /= null
         then Current.Get_View.Get_Project
         else Project);

   begin
      if Current = null then
         return null;
      end if;

      declare
         Title : constant Virtual_File := Get_Filename (Current);
         P_Full_Name : constant String :=
           (if P /= No_Project and then
               Get_Registry (Kernel).Tree.Root_Project.Is_Aggregate_Project
            then " - Project: " & P.Project_Path.Display_Full_Name
            else "");
      begin
         Create_New_View
           (Editor,
            P,
            Kernel,
            Current);

         Child := new Editor_Child_Record;
         Initialize
           (Child, Editor,
            Kernel         => Kernel,
            Flags          => All_Buttons,
            Focus_Widget   => Gtk_Widget (Get_View (Editor)),
            Module         => Src_Editor_Module_Id,
            Areas          => Central_Only);

         --  Find the first free view number
         Num := 2;
         while Find_MDI_Child_By_Name
           (Get_MDI (Kernel),
            Display_Base_Name (Title) & " <" & Image (Num) & ">") /=
           null
         loop
            Num := Num + 1;
         end loop;

         --  Put before we set the title, to make sure the appropriate short
         --  title will be used
         Put (Get_MDI (Kernel), Child, Initial_Position => Position_Automatic);

         declare
            Im : constant String := Image (Num);
         begin
            Set_Title
              (Child,
               Display_Full_Name (Title) & " <" & Im & ">" & P_Full_Name,
               Display_Base_Name (Title) & " <" & Im & ">");
         end;

         Set_Child (Get_View (Editor), Child);

         Widget_Callback.Connect
           (Child, Signal_Selected, Update_Cache_On_Focus'Access);

         Set_Focus_Child (Child);

         Widget_Callback.Connect
           (Child, Signal_Destroy,
            On_Editor_Destroy'Access);

         --  Emit a "status_changed" signal on the editor, to initialize the
         --  MDI icon and status.
         Status_Changed (Get_Buffer (Editor));
      end;

      return Editor;
   end New_View;

   -------------------
   -- Save_Function --
   -------------------

   overriding function Save_Function
     (Module       : access Source_Editor_Module_Record;
      Child        : Glib.Object.GObject;
      Mode         : Save_Function_Mode;
      Single_Child : Boolean;
      Force        : Boolean) return Boolean
   is
      pragma Unreferenced (Module);
      Success : Boolean;
      Box     : constant Source_Editor_Box := Source_Editor_Box (Child);
   begin
      case Mode is
         when Query =>
            return Needs_To_Be_Saved (Box, Single_Child);

         when Action =>
            if Needs_To_Be_Saved (Box, Single_Child) then
               Save_To_File (Box, Success => Success, Force => Force);
               return Success;
            else
               --  Nothing to do => success
               return True;
            end if;
      end case;
   end Save_Function;

   ------------------------
   -- Create_File_Editor --
   ------------------------

   function Create_File_Editor
     (Kernel          : access Kernel_Handle_Record'Class;
      File            : GNATCOLL.VFS.Virtual_File;
      Project         : GNATCOLL.Projects.Project_Type;
      Dir             : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Create_New      : Boolean := True;
      Is_Load_Desktop : Boolean := False)
      return Src_Editor_Box.Source_Editor_Box
   is
      Success     : Boolean;
      Editor      : Source_Editor_Box;
      File_Exists : constant Boolean := Is_Regular_File (File);
      Writable    : Writable_File;
      Is_Writable : Boolean;
   begin
      --  Create a new editor only if the file exists or we are asked to
      --  create a new empty one anyway.

      if File_Exists then
         Gtk_New
           (Box             => Editor,
            Project         => Project,
            Kernel          => Kernel_Handle (Kernel),
            Filename        => File,
            Is_Load_Desktop => Is_Load_Desktop);

      elsif Create_New then
         --  Do not create the file if we know we won't be able to save it
         --  anyway (for instance a remote file for which we couldn't establish
         --  the connection)

         if File = GNATCOLL.VFS.No_File or else File.Is_Directory then
            Is_Writable := True;

         else
            Writable := Write_File (File);
            Is_Writable := Writable /= Invalid_File;

            if Writable /= Invalid_File then
               begin
                  Close (Writable);
                  Delete (File, Success);
                  if not Success then
                     Is_Writable := False;
                  end if;
               exception
                  when Use_Error =>
                     Is_Writable := False;
               end;
            end if;
         end if;

         if Is_Writable then
            Gtk_New
              (Box             => Editor,
               Project         => Project,
               Kernel          => Kernel_Handle (Kernel),
               Filename        => File,
               Is_Load_Desktop => Is_Load_Desktop);

            if File = GNATCOLL.VFS.No_File
              and then Dir /= GNATCOLL.VFS.No_File
            then
               Editor.Get_Buffer.Set_Initial_Dir (Dir);
            end if;

         end if;
      end if;

      return Editor;
   end Create_File_Editor;

   ----------------------------
   -- Create_New_Recent_Menu --
   ----------------------------

   procedure Create_New_Recent_Menu
     (Kernel  : access Kernel_Handle_Record'Class;
      File    : GNATCOLL.VFS.Virtual_File;
      Prepend : Boolean)
   is
      M                : constant Source_Editor_Module :=
        Source_Editor_Module (Src_Editor_Module_Id);
      Name             : constant String := File.Display_Full_Name;
      Prefix_Menu_Name : constant String := "open recent file: ";
      File_Menu_Paths  : constant Unbounded_String_Array := Split
        (To_String (Menu_List_For_Action ("open file")),
         On => '/');
   begin
      Register_Action
        (Kernel,
         Name        => Prefix_Menu_Name & Name,
         Command     => new On_Open_Recent'
           (Interactive_Command with File => File),
         Description => "Reopen the file " & Name,
         Category    => "Internal");
      Register_Menu
        (Kernel,
         Path     => To_String (File_Menu_Paths (File_Menu_Paths'First))
         & "/Open Recent Files/"
         & Escape_Underscore (File.Display_Base_Name),
         Action   => Prefix_Menu_Name & Name,
         Prepend  => Prepend);

      if Prepend then
         M.Recent_File_Actions.Prepend (Prefix_Menu_Name & Name);
      else
         M.Recent_File_Actions.Append (Prefix_Menu_Name & Name);
      end if;
   end Create_New_Recent_Menu;

   ----------------------------------
   -- Regenerate_Recent_Files_Menu --
   ----------------------------------

   procedure Regenerate_Recent_Files_Menu
     (Kernel : access Kernel_Handle_Record'Class)
   is
      M : constant Source_Editor_Module :=
        Source_Editor_Module (Src_Editor_Module_Id);
      V : constant VSS.String_Vectors.Virtual_String_Vector :=
        Get_History (Kernel.Get_History.all, Hist_Key);
      F : Virtual_File;
   begin
      --  Remove old menus and actions

      for Action of M.Recent_File_Actions loop
         Unregister_Action (Kernel, Action, Remove_Menus_And_Toolbars => True);
      end loop;
      M.Recent_File_Actions.Clear;

      --  Regenerate the menus and actions.
      --
      --  We retrieve the toplevel menu from the 'open file' action: this
      --  ensures that the 'open recent files' and the 'open file' actions are
      --  always in the same toplevel menu, even when menus.xml has been
      --  changed by the user.

      for N of V loop
         F := GNATCOLL.VFS.VSS_Utils.Create (N);
         Create_New_Recent_Menu (Kernel, F, Prepend => False);
      end loop;
   end Regenerate_Recent_Files_Menu;

   ------------------------
   -- Add_To_Recent_Menu --
   ------------------------

   procedure Add_To_Recent_Menu
     (Kernel : access Kernel_Handle_Record'Class; File : Virtual_File)
   is
      M        : constant Source_Editor_Module :=
        Source_Editor_Module (Src_Editor_Module_Id);
      Is_Full  : Boolean := False;
   begin
      --  Make sure we won't add duplicate entries in the history.
      --  This loop is not very optimal, but we have to do this since
      --  histories only store strings and we do not have a way of
      --  normalizing capitalization under Windows.
      declare
         Hist      : constant History := Kernel.Get_History;
         Name_List : constant VSS.String_Vectors.Virtual_String_Vector :=
           Get_History (Hist.all, Hist_Key);
         F         : Virtual_File;

      begin
         Is_Full := Name_List.Length = Get_Max_Length (Hist.all, Hist_Key);

         for Name of Name_List loop
            F := GNATCOLL.VFS.VSS_Utils.Create (Name);

            if F = File then
               --  The menu and actions are already there, nothing to do.

               return;
            end if;
         end loop;
      end;

      --  If we reach this, it means the menu doesn't already exist.

      if Is_Full then
         --  Max capacity: Pop the oldest menu
         Unregister_Action
           (Kernel                    => Kernel,
            Name                      => M.Recent_File_Actions.Last_Element,
            Remove_Menus_And_Toolbars => True);
         M.Recent_File_Actions.Delete_Last;
      end if;

      Add_To_History (Kernel, Hist_Key, UTF8_Full_Name (File));

      Create_New_Recent_Menu (Kernel, File, Prepend => True);
   end Add_To_Recent_Menu;

   -------------
   -- Execute --
   -------------

   overriding function Execute
      (Self    : access On_Open_Recent;
       Context : Interactive_Command_Context) return Command_Return_Type is
   begin
      Open_File_Action_Hook.Run
        (Get_Kernel (Context.Context),
         File    => Self.File,
         Project => No_Project);  --  will choose one at random ???
      return Standard.Commands.Success;
   end Execute;

   ---------------
   -- Open_File --
   ---------------

   function Open_File
     (Kernel           : access Kernel_Handle_Record'Class;
      File             : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Project          : GNATCOLL.Projects.Project_Type;
      Create_New       : Boolean := True;
      Focus            : Boolean := True;
      Line             : Editable_Line_Type;
      Column           : Visible_Column_Type;
      Column_End       : Visible_Column_Type;
      Group            : Gtkada.MDI.Child_Group := Gtkada.MDI.Group_Default;
      Initial_Position : Gtkada.MDI.Child_Position :=
        Gtkada.MDI.Position_Automatic;
      Initial_Dir      : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Areas            : Gtkada.MDI.Allowed_Areas := Gtkada.MDI.Central_Only;
      Title            : String := "";
      Is_Load_Desktop  : Boolean := False)
      return Source_Editor_Box
   is
      Id      : constant Source_Editor_Module :=
                  Source_Editor_Module (Src_Editor_Module_Id);
      No_Name : constant String := -"Untitled";
      MDI     : constant MDI_Window := Get_MDI (Kernel);
      Editor  : Source_Editor_Box;
      Child   : GPS_MDI_Child;
      Child2  : MDI_Child;

      procedure Jump_To_Location_And_Give_Focus
        (Child     : not null access MDI_Child_Record'Class;
         Is_Opened : Boolean);
      --  Jump to the location given in parameter to Open_File

      -------------------------------------
      -- Jump_To_Location_And_Give_Focus --
      -------------------------------------

      procedure Jump_To_Location_And_Give_Focus
        (Child     : not null access MDI_Child_Record'Class;
         Is_Opened : Boolean)
      is
         Real_Column, Real_Column_End : Character_Offset_Type;
      begin
         if Line /= 0
           and then Is_Valid_Position (Get_Buffer (Editor), Line)
         then
            Real_Column := Collapse_Tabs
              (Get_Buffer (Editor), Line, Column);

            --  Will emit context_changed if Focus is true
            Set_Cursor_Location
              (Editor, Line, Real_Column, Force_Focus => False,
               Centering => With_Margin);
            if Focus then
               Child.Raise_Child (Focus);
            end if;

            Set_Position_Set_Explicitely (Get_View (Editor));

            if Column_End /= 0
              and then Column < Column_End
              and then Is_Valid_Position
                (Get_Buffer (Editor), Line, Column_End)
            then
               Real_Column_End := Collapse_Tabs
                 (Get_Buffer (Editor), Line, Column_End);

               Select_Region
                 (Get_Buffer (Editor),
                  Line,
                  Real_Column,
                  Line,
                  Real_Column_End);
            end if;

         else
            if not Is_Opened then
               --  Place the cursor if we are opening the file
               Set_Cursor_Location (Editor, 1, 1, Force_Focus => False);
            end if;
            if Focus then
               --  Gives the focus, thus child_selected, thus context_changed).
               Raise_Child (Child, Focus);
            end if;
         end if;
      end Jump_To_Location_And_Give_Focus;

   begin
      if Active (Me) then
         Trace (Me, "Open file " & File.Display_Full_Name
                & " Project=" & Project.Project_Path.Display_Full_Name
                & " Focus=" & Focus'Img
                & " Line=" & Line'Img);
      end if;

      if File /= GNATCOLL.VFS.No_File and then not File.Is_Directory then
         Child2 := Find_Editor
           (Kernel        => Kernel,
            File          => File,
            Project       => Project,
            Unlocked_Only => True);

         if Child2 /= null then
            Trace (Me, "Reusing existing editor for same project");

            --  An editor exists: raise the child and Present its
            --  toplevel window. This is useful for instance when
            --  invoking the "Open From Project" omnisearch from a
            --  floating window.

            Raise_Child (Child2, Focus);

            declare
               TL : constant Gtk_Widget := Child2.Get_Toplevel;
            begin
               if TL.all in Gtk_Window_Record'Class then
                  Present (Gtk_Window (TL));
               end if;
            end;

            Editor := Source_Editor_Box (Get_Widget (Child2));

            Freeze_Context (Get_Buffer (Editor));
            Jump_To_Location_And_Give_Focus (Child2, Is_Opened => True);
            Thaw_Context (Get_Buffer (Editor));

            return Editor;
         end if;
      end if;

      --  Do we have the file opened for another project ? If yes, create a new
      --  view

      Child2 := Find_Editor
        (Kernel  => Kernel,
         File    => File,
         Project => No_Project);

      if Child2 /= null then
         Trace (Me, "Create new view for existing file, wrong project");
         Create_New_View
           (Box             => Editor,
            Project         => Project,
            Kernel          => Kernel,
            Source          => Get_Source_Box_From_MDI (Child2),
            Is_Load_Desktop => Is_Load_Desktop);

      else
         Increase_Indent (Me, "About to Create_File_Editor");
         Editor := Create_File_Editor
           (Kernel          => Kernel,
            File            => File,
            Project         => Project,
            Dir             => Initial_Dir,
            Create_New      => Create_New,
            Is_Load_Desktop => Is_Load_Desktop);
         Decrease_Indent (Me);
      end if;

      --  If we have created an editor, put it into a box, and give it
      --  to the MDI to handle

      if Editor /= null then
         Freeze_Context (Get_Buffer (Editor));
         Child := new Editor_Child_Record;
         Initialize
           (Child, Editor,
            Kernel         => Kernel,
            Flags          => All_Buttons,
            Focus_Widget   => Gtk_Widget (Get_View (Editor)),
            Group          => Group,
            Module         => Src_Editor_Module_Id,
            Areas          => Areas);
         Child.Monitor_File (File);

         Put
           (Get_MDI (Kernel),
            Child,
            Initial_Position => Initial_Position);
         Set_Child (Get_View (Editor), Child);

         if not Is_Load_Desktop then
            Save_Backup_Desktop (Kernel);
         end if;

         Check_Writable (Editor);

         if Get_Status (Get_Buffer (Editor)) = Modified then
            Child.Set_Icon_Name
              (File_Modified_Pixbuf
               & (if Editor.Is_Locked then Locked_Suffix else ""));
         elsif Get_Status (Get_Buffer (Editor)) = Unsaved then
            Child.Set_Icon_Name
              (File_Unsaved_Pixbuf
               & (if Editor.Is_Locked then Locked_Suffix else ""));
         else
            Child.Set_Icon_Name
              (File_Pixbuf
               & (if Editor.Is_Locked then Locked_Suffix else ""));
         end if;

         Widget_Callback.Connect
           (Child, Signal_Selected, Update_Cache_On_Focus'Access);

         --  Add child to the hash table of editors

         Id.Editors.Include (File, (Child => MDI_Child (Child)));

         --  Make sure the entry in the hash table is removed when the editor
         --  is destroyed.

         Widget_Callback.Connect
           (Child, Signal_Destroy, On_Editor_Destroy'Access);

         if File /= GNATCOLL.VFS.No_File and then not File.Is_Directory then
            if Title /= "" then
               Editor.Get_Buffer.Set_Title (Title);
            end if;

            --  Force update of MDI titles (thus emits context_changed)
            Editor.Get_Buffer.Filename_Changed;

            --  Build a context from the newly created editor, in particular
            --  to change the current context's file information before
            --  running the File_Edited hook.
            Kernel.Refresh_Context;

            --  Update the caches (timestamp+checksum) for this file
            File_Edited_Hook.Run (Kernel, Get_Filename (MDI_Child (Child)));
         else
            --  Determine the number of "Untitled" files open

            declare
               Iterator    : Child_Iterator := First_Child (MDI);
               The_Child   : MDI_Child;
               Nb_Untitled : Integer := -1;
               Ident       : Virtual_File;
            begin
               The_Child := Get (Iterator);

               while The_Child /= null loop
                  if The_Child /= MDI_Child (Child)
                    and then Get_Widget (The_Child).all in
                    Source_Editor_Box_Record'Class
                    and then Get_Filename (The_Child) = GNATCOLL.VFS.No_File
                  then
                     declare
                        Ident : constant String := Display_Base_Name
                          (Get_File_Identifier (The_Child));
                        Index : Natural;
                     begin
                        if Ident = No_Name then
                           Nb_Untitled := Natural'Max (Nb_Untitled, 0);
                        else
                           Index := Natural'Value
                             (Ident (Ident'First + No_Name'Length + 2
                                     .. Ident'Last - 1));
                           Nb_Untitled := Natural'Max (Nb_Untitled, Index);
                        end if;

                     exception
                        when Constraint_Error =>
                           null;
                     end;
                  end if;

                  Next (Iterator);
                  The_Child := Get (Iterator);
               end loop;

               --  The identifier is set as an absolute path to avoid
               --  conversions later on, for instance through shell commands,
               --  that would otherwise receive a relative path name and try
               --  to resolve it.

               if Nb_Untitled = -1 then
                  if Title /= "" then
                     Child.Set_Title (Title);
                  else
                     Set_Title (Child, No_Name);
                  end if;
                  Ident := Create (+('/' & No_Name));

               else
                  declare
                     Identifier : constant String :=
                                    No_Name & " ("
                                      & Image (Nb_Untitled + 1) & ")";
                  begin
                     if Title /= "" then
                        Child.Set_Title (Title);
                     else
                        Set_Title (Child, Identifier);
                     end if;
                     Ident := Create (+('/' & Identifier));
                  end;
               end if;

               Set_File_Identifier (Get_Buffer (Editor), Ident);
               Set_Filename
                 (Get_Buffer (Editor), Get_Filename (MDI_Child (Child)));
               File_Edited_Hook.Run (Kernel, Ident);
            end;
         end if;

         --  Change location only at the end, since other calls above might
         --  reset it.

         Jump_To_Location_And_Give_Focus (Child, Is_Opened => False);

         if File /= GNATCOLL.VFS.No_File then
            Add_To_Recent_Menu (Kernel, File);
         end if;

         Thaw_Context (Get_Buffer (Editor));

         --  Update the editor tabs' status for the file being opened
         Update_Editor_Tabs_Status (Kernel, [File]);
      else
         Kernel.Insert
           ((-"Cannot open file ") & "'" & Display_Full_Name (File)
            & "'",
            Add_LF => True,
            Mode   => Error);
      end if;

      return Editor;
   end Open_File;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Select_All_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      Source : constant Source_Editor_Box :=
                 Get_Source_Box_From_MDI (Find_Current_Editor (Kernel));
   begin
      if Source /= null then
         Select_All (Get_Buffer (Source));
      end if;
      return Standard.Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Insert_File_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      Source : constant Source_Editor_Box :=
                 Get_Source_Box_From_MDI (Find_Current_Editor (Kernel));
   begin
      if Source /= null then
         declare
            F      : constant Virtual_File :=
              Select_File
                (Title             => -"Insert File",
                 Parent            => Get_Current_Window (Kernel),
                 Use_Native_Dialog => Use_Native_Dialogs.Get_Pref,
                 Kind              => Open_File,
                 File_Pattern      => "*;*.ad?;{*.c,*.h,*.cpp,*.cc,*.C}",
                 Pattern_Name      => -"All files;Ada files;C/C++ files",
                 History           => Get_History (Kernel));
            Buffer : GNAT.Strings.String_Access;
            Line   : Editable_Line_Type;
            Column : Character_Offset_Type;

         begin
            if F /= GNATCOLL.VFS.No_File then
               Buffer := Read_File (F);
               Get_Cursor_Position (Get_Buffer (Source), Line, Column);
               Insert (Get_Buffer (Source), Line, Column, Buffer.all);
               Free (Buffer);
            end if;
         end;
      end if;
      return Standard.Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self             : On_Open_File;
      Kernel           : not null access Kernel_Handle_Record'Class;
      File             : Virtual_File;
      Line             : Integer;
      Column, Column_End : Basic_Types.Visible_Column_Type;
      Enable_Navigation, New_File, Force_Reload, Focus : Boolean;
      Project          : Project_Type;
      Group            : Child_Group;
      Initial_Position : Child_Position;
      Areas            : Allowed_Areas;
      Title            : String;
      Is_Load_Desktop  : Boolean) return Boolean
   is
      pragma Unreferenced (Self);
      Child  : MDI_Child;
      Source : Source_Editor_Box;
      Tmp    : Boolean;

   begin
      if Line = -1 then
         --  Close all file editors corresponding to File

         loop
            Child := Find_Editor (Kernel, File, Project);
            exit when Child = null;
            Close_Child (Child);
         end loop;

         return True;

      elsif File.Is_Regular_File and then not File.Is_Readable then
         --  Don't open a file which can't be read

         Kernel.Get_Messages_Window.Insert_Error
           ((-"Cannot open file ") & "'" & Display_Full_Name (File)
            & "', the file is not readable" & ASCII.LF);
         return False;

      else
         Source := Open_File
           (Kernel, File,
            Project          => Project,
            Create_New       => New_File,
            Focus            => Focus,
            Group            => Group,
            Initial_Position => Initial_Position,
            Line             => Editable_Line_Type (Line),
            Column           => Column,
            Column_End       => Column_End,
            Areas            => Areas,
            Title            => Title,
            Is_Load_Desktop  => Is_Load_Desktop);

         if Force_Reload then
            Source.Get_Buffer.Load_File
              (Filename        => File,
               Success         => Tmp,
               Is_Load_Desktop => Is_Load_Desktop);
         end if;

         --  This used to be done in Open_File_Editor itself, before we call
         --  the Hook, but then we wouldn't have access to Create_File_Marker.
         --  Another module that deals with this hook would likely want its own
         --  type of Marker anyway...
         if Enable_Navigation then
            Push_Marker_In_History
              (Kernel => Kernel,
               Marker => Create_File_Marker
                 (Kernel  => Kernel,
                  File    => File,
                  Project => Project,
                  Line    => Convert (Line),
                  Column  => Column));
         end if;

         return Source /= null;
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self       : On_File_Line_Action;
      Kernel     : not null access Kernel_Handle_Record'Class;
      Identifier : String;
      File       : Virtual_File;
      Every_Line : Boolean;
      Tooltip    : String;
      Info       : access Line_Information_Array;
      Icon_Name  : String)
   is
      pragma Unreferenced (Self);

      procedure Process_File (F : Virtual_File);

      procedure Process_File (F : Virtual_File) is
         Child : constant MDI_Child := Find_Editor (Kernel, F, No_Project);
      begin
         if Child /= null then
            if Info = null or else Info'Length = 0 then
               Remove_Line_Information_Column
                 (Source_Editor_Box (Get_Widget (Child)), Identifier);
            elsif Info'First = 0 then
               Create_Line_Information_Column
                 (Source_Editor_Box (Get_Widget (Child)).Get_Buffer,
                  Identifier,
                  Every_Line,
                  Info (0));
            elsif Info'Last < 0 then
               --  This how the hook data encodes extra information
               Add_Extra_Information
                 (Get_Buffer (Source_Editor_Box (Get_Widget (Child))),
                  Identifier => Identifier,
                  Info       => Info,
                  Icon       => Icon_Name,
                  Tooltip    => Tooltip);
            else
               --  ??? Source duplicated in
               --  src_editor_buffer-line_information (Add_Blank_Lines)
               Add_Side_Information
                 (Source_Editor_Box (Get_Widget (Child)).Get_Buffer,
                  Identifier, Info.all);
            end if;
         end if;
      end Process_File;

   begin
      if File /= No_File then
         Process_File (File);
      else
         for F of Kernel.Opened_Files loop
            Process_File (F);
         end loop;
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self        : On_Character_Added;
      Kernel      : not null access Kernel_Handle_Record'Class;
      File        : Virtual_File;
      Character   : Glib.Gunichar;
      Interactive : Boolean)
   is
      pragma Unreferenced (Self);
   begin
      if File = GNATCOLL.VFS.No_File or else not Interactive then
         return;
      end if;

      declare
         --  get the most recent editor for this file, for any project
         Box    : constant Source_Editor_Box :=
           Get_Source_Box_From_MDI
             (Find_Editor (Kernel, File, No_Project));  -- any project
         Buffer : Source_Buffer;

      begin
         if Box = null then
            --  It happens when editing a buffer without view
            return;
         end if;

         Buffer := Get_Buffer (Box);

         if Is_Alnum (Character)
           or else Character = Underscore
           or else Character = Backspace
         then
            if Is_Alnum (Character) then
               Add_Typed_Char (Buffer, Character);
            elsif Character = Backspace then
               Delete_Last_Typed_Char (Buffer);
            end if;

            if Get_View (Box).As_Is_Enabled then
               Get_View (Box).Reset_As_Is_Mode;
            else
               Autocase_Text
                 (Buffer    => Buffer,
                  Character => Character,
                  Casing    => On_The_Fly);
            end if;
         elsif Character = Space then
            Get_View (Box).Reset_As_Is_Mode;
         end if;
      end;
   end Execute;

   -------------------------------------
   -- Update_Belong_To_Project_Status --
   -------------------------------------

   procedure Update_Editor_Tabs_Status
     (Kernel : not null access Kernel_Handle_Record'Class;
      Files  : Basic_Types.File_Sets.Set)
   is

      procedure Set_Belongs_To_Project
        (Child : not null access GPS_MDI_Child_Record'Class);
      --  Set/unset the CSS class specifying if the editor's file belongs
      --  to the project or not.

      ----------------------------
      -- Set_Belongs_To_Project --
      ----------------------------

      procedure Set_Belongs_To_Project
        (Child : not null access GPS_MDI_Child_Record'Class)
      is
         Src_Box : constant Source_Editor_Box :=
           Get_Source_Box_From_MDI (MDI_Child (Child));
         File    : constant Virtual_File := Src_Box.Get_Filename;
         Project : constant Project_Type := Get_Project_For_File
           (Kernel.Registry.Tree, File);
         Tab_Label : constant Gtk_Label := Child.Get_Tab_Label;
      begin
         --  Return immediately if there is no tab label (floating editors).
         if Tab_Label = null then
            return;
         end if;

         if Project /= No_Project then
            Get_Style_Context (Child.Get_Tab_Label).Remove_Class
              ("not-from-project");
         else
            Get_Style_Context (Child.Get_Tab_Label).Add_Class
              ("not-from-project");
         end if;
      end Set_Belongs_To_Project;

   begin
      for File of Files loop
         For_All_Views
           (Kernel   => Kernel,
            File     => File,
            Callback => Set_Belongs_To_Project'Unrestricted_Access);
      end loop;
   end Update_Editor_Tabs_Status;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Project_Changed;
      Kernel : not null access Kernel_Handle_Record'Class) is
   begin
      Update_Editor_Tabs_Status (Kernel, Kernel.Opened_Files);
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self        : On_Word_Added;
      Kernel      : not null access Kernel_Handle_Record'Class;
      File        : Virtual_File;
      Char        : Glib.Gunichar := 0;
      Interactive : Boolean := True)
   is
      pragma Unreferenced (Self);
      Buffer    : Source_Buffer;
   begin
      if File /= GNATCOLL.VFS.No_File then
         --  Get the most recent editor for this file, for any project
         Buffer := Get_Buffer
           (Get_Source_Box_From_MDI (Find_Editor (Kernel, File, No_Project)));
         Autocase_Text
           (Buffer => Buffer, Character => Char, Casing => End_Of_Word);
      end if;
   end Execute;

   -------------------
   -- Build_Context --
   -------------------

   overriding function Build_Context
     (Self  : not null access Editor_Child_Record;
      Event : Gdk.Event.Gdk_Event := null)
      return Selection_Context
   is
      Editor : constant Source_Editor_Box :=
        Source_Editor_Box (GPS_MDI_Child (Self).Get_Actual_Widget);
   begin
      return Build_Editor_Context
        (View     => Get_View (Editor),
         Location => Location_Event,
         Event    => Event);
   end Build_Context;

   -----------------------------
   -- Expand_Aliases_Entities --
   -----------------------------

   function Expand_Aliases_Entities
     (Kernel    : access Kernel_Handle_Record'Class;
      Expansion : String;
      Special   : Character) return String
   is
      Box    : Source_Editor_Box;
      W      : Gtk_Widget := Get_Current_Focus_Widget (Kernel);
      Line   : Editable_Line_Type;
      Column : Character_Offset_Type;
      Prj    : Project_Type;
   begin
      if W.all in Source_View_Record'Class then
         W := Get_Parent (W);
         while W.all not in Source_Editor_Box_Record'Class loop
            W := Get_Parent (W);
         end loop;
         Box := Source_Editor_Box (W);

         case Special is
            when 'l' =>
               Get_Cursor_Position (Get_Buffer (Box), Line, Column);
               return Expansion & Image (Integer (Line));

            when 'c' =>
               Get_Cursor_Position (Get_Buffer (Box), Line, Column);
               return Expansion & Image (Integer (Column));

            when 'f' =>
               return Expansion & (+Base_Name (Get_Filename (Box)));

            when 'd' =>
               return Expansion & (+Dir_Name (Get_Filename (Box)));

            when 'p' =>
               Prj := Get_Project (Box);
               if Prj = No_Project then
                  Prj := Get_Registry (Kernel).Tree.Root_Project;
               end if;

               return Expansion & Prj.Name;

            when 'P' =>
               Prj := Get_Project (Box);
               if Prj = No_Project then
                  Prj := Get_Registry (Kernel).Tree.Root_Project;
               end if;

               return Expansion & (+Prj.Project_Path.Full_Name);

            when others =>
               return Invalid_Expansion;
         end case;

      else
         return Invalid_Expansion;
      end if;
   end Expand_Aliases_Entities;

   ----------------------
   -- Bookmark_Handler --
   ----------------------

   overriding function Bookmark_Handler
     (Module : access Source_Editor_Module_Record;
      Load   : XML_Utils.Node_Ptr := null;
      JSON   : JSON_Value := JSON_Null) return Location_Marker is
   begin
      return Src_Editor_Module.Markers.Load
        (Get_Kernel (Module.all), Load, JSON);
   end Bookmark_Handler;

   ---------------
   -- Customize --
   ---------------

   overriding procedure Customize
     (Module : access Source_Editor_Module_Record;
      File   : GNATCOLL.VFS.Virtual_File;
      Node   : XML_Utils.Node_Ptr;
      Level  : Customization_Level) is
   begin
      Casing_Customize (Get_Kernel (Module.all), File, Node, Level);
   end Customize;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding
   function Filter_Matches_Primitive
     (Context : access Is_Not_Makefile_Context;
      Ctxt    : GPS.Kernel.Selection_Context) return Boolean
   is
      pragma Unreferenced (Context);
      File : Virtual_File;
   begin
      if not Has_File_Information (Ctxt) then
         return False;
      end if;

      File := File_Information (Ctxt);

      return not Starts_With (To_Lower (+File.Base_Name), "makefile");
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Context : access Is_Ada_File_Context;
      Ctxt    : GPS.Kernel.Selection_Context) return Boolean is
   begin
      if not Has_File_Information (Ctxt) then
         return False;
      end if;

      return Ada.Characters.Handling.To_Lower
        (Get_Kernel
           (Ctxt).Get_Language_Handler.Get_Language_From_File
             (File_Information (Ctxt)).Get_Name) = "ada";
   end Filter_Matches_Primitive;

   -----------------------------
   -- Get_Lang_Formatter_Pref --
   -----------------------------

   function Get_Lang_Formatter_Pref
     (Lang : Language.Language_Access; Prefix : String)
      return Preference
   is
      Id          : constant Source_Editor_Module :=
        Source_Editor_Module (Src_Editor_Module_Id);
      Lang_Name   : constant String := To_Lower (Lang.Get_Name);
      Pref_Suffix : constant String :=
        (if Lang_Name = "c" or else Lang_Name = "c++" or else Lang_Name = "cpp"
         --  Merge C and C++ together
         then "c++"
         else Lang_Name);
   begin
      return Id.Get_Kernel.Get_Preferences.Get_Pref_From_Name
           (Name                => Prefix & Pref_Suffix,
            Create_If_Necessary => False);
   end Get_Lang_Formatter_Pref;

   -----------------------------------
   -- Get_Range_Formatting_Provider --
   -----------------------------------

   function Get_Range_Formatting_Provider
     (Lang : Language.Language_Access)
      return Editor_Formatting_Provider_Access
   is
      Id       : constant Source_Editor_Module :=
        Source_Editor_Module (Src_Editor_Module_Id);
      Pref     : constant Preference :=
        Get_Lang_Formatter_Pref (Lang, Range_Format_Name_Prefix);
      Selected : constant String :=
        (if Pref = null
         then Id.Formatting_Providers.First_Element.Get_Name
         else Pref.Get_Pref);
      Value    : constant Virtual_String :=
        To_Lowercase.Transform (To_Virtual_String (Selected));
   begin
      for Provider of Id.Formatting_Providers loop
         declare
            Name : constant Virtual_String :=
              To_Lowercase.Transform (To_Virtual_String (Provider.Get_Name));
         begin
            if Name = Value then
               return Provider;
            end if;
         end;
      end loop;

      return null;
   end Get_Range_Formatting_Provider;

   -------------------------------------
   -- Get_On_Type_Formatting_Provider --
   -------------------------------------

   function Get_On_Type_Formatting_Provider
     (Lang : Language.Language_Access)
     return Editor_Formatting_Provider_Access
   is
      Id       : constant Source_Editor_Module :=
        Source_Editor_Module (Src_Editor_Module_Id);
      Pref     : constant Preference :=
        Get_Lang_Formatter_Pref (Lang, On_Type_Format_Name_Prefix);
      Selected : constant String :=
        (if Pref = null
         then Id.Formatting_Providers.First_Element.Get_Name
         else Pref.Get_Pref);
      Value    : constant Virtual_String :=
        To_Lowercase.Transform (To_Virtual_String (Selected));
   begin
      for Provider of Id.Formatting_Providers loop
         declare
            Name : constant Virtual_String :=
              To_Lowercase.Transform (To_Virtual_String (Provider.Get_Name));
         begin
            if Name = Value then
               return Provider;
            end if;
         end;
      end loop;

      return null;
   end Get_On_Type_Formatting_Provider;

   -------------------------------------
   -- Get_Move_Cursor_When_Formatting --
   -------------------------------------

   function Get_Move_Cursor_When_Formatting
     (Lang : Language.Language_Access) return Boolean
   is
      Pref : constant Preference :=
        Get_Lang_Formatter_Pref (Lang, Move_Cursor_Format_Prefix);
   begin
      if Pref /= null then
         return Boolean_Preference (Pref).Get_Pref;
      else
         return False;
      end if;
   end Get_Move_Cursor_When_Formatting;

   ------------------------------
   -- Get_Limit_LSP_Formatting --
   ------------------------------

   function Get_Limit_LSP_Formatting
     (Lang : Language.Language_Access) return Boolean
   is
      Pref : constant Preference :=
        Get_Lang_Formatter_Pref (Lang, Limit_LSP_Format_Prefix);
   begin
      if Pref /= null then
         return Boolean_Preference (Pref).Get_Pref;
      else
         return False;
      end if;
   end Get_Limit_LSP_Formatting;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      UR                          : constant Undo_Redo :=
                                      new Undo_Redo_Information;
      Module                      : Search_Module;
      Selector                    : Simple_Scope_Selector;
      Extra                       : Files_Extra_Scope;
      Default_Options_Mask        : constant Search_Options_Mask :=
                                      All_Options
                                          and not Search_Backward
                                              and not Supports_Incremental;
      Command                     : Interactive_Command_Access;
      Line_Numbers_Area_Filter    : Action_Filter;

      Has_Entity_Name             : constant Action_Filter :=
                                      new Has_Entity_Name_Filter;
      Src_Action_Context          : constant Action_Filter :=
                                      new Src_Editor_Action_Context;
      No_Completion               : constant Action_Filter :=
                                      new Completion_Context
                                         (Is_Line_Movement => True);
      Writable_Src_Action_Context : constant Action_Filter :=
                                      new Writable_Src_Editor_Action_Context;
      Is_Not_Makefile             : constant Action_Filter :=
                                      new Is_Not_Makefile_Context;
      Last_Editor_Context         : constant Action_Filter :=
                                      new Last_Editor_Action_Context;
      Context_Has_Selection       : constant Action_Filter :=
                                      new Has_Selection_Filter;
      Is_Ada_File_Filter          : constant Action_Filter :=
                                      new Is_Ada_File_Context;
      --  Memory is never freed, but this is needed for the whole life of
      --  the application.

      Steps : constant array (1 .. 2) of Integer := (1, -1);

   begin
      Src_Editor_Module_Id := new Source_Editor_Module_Record;
      Source_Editor_Module (Src_Editor_Module_Id).Undo_Redo := UR;

      --  Register source editor common filters

      Register_Filter
        (Kernel,
         Src_Action_Context,
         "Source editor");

      Register_Filter
        (Kernel,
         No_Completion,
         "No completion");

      Register_Filter
        (Kernel, Writable_Src_Action_Context, "Writable source editor");

      Register_Filter
        (Kernel, Is_Not_Makefile, "Is not Makefile");

      Register_Filter
        (Kernel, Context_Has_Selection, "Has selection");

      Register_Filter
        (Kernel, Has_Entity_Name, "Has entity name");

      --  Commands

      for Kind in Movement_Type loop
         for Step of Steps loop
            for Extend_Selection in Boolean loop
               declare
                  Step_Str        : constant String :=
                    (if Step = 1 then "next" else "previous");
                  Kind_Str        : constant String := To_Lower (Kind'Img);
                  ESel_Str        : constant String :=
                    (if Extend_Selection then " (extend selection)" else "");
                  When_Completion : constant Action_Filter :=
                    new Completion_Context (Is_Line_Movement => Kind = Line);
                  When_Signature  : constant Action_Filter :=
                    new Signature_Context;
               begin
                  Command := new Move_Command;
                  Move_Command (Command.all).Kind := Kind;
                  Move_Command (Command.all).Step := Step;
                  Move_Command (Command.all).Extend_Selection :=
                    Extend_Selection;
                  Register_Action
                    (Kernel,
                     "Move to " & Step_Str & " " & Kind_Str & ESel_Str,
                     Command,
                     -"Move to the " & Step_Str & " " & Kind_Str
                     & " in the current source editor" & ESel_Str,
                     Category       => "Editor",
                     Filter         =>
                       (Src_Action_Context
                        and When_Completion
                        and When_Signature),
                     Log_On_Execute => False);
               end;
            end loop;
         end loop;
      end loop;

      Register_Action
        (Kernel, "Center cursor on screen", new Scroll_Command,
           -"Scroll the current source editor so that the cursor is centered",
         Category => "Editor",
         Filter   => Src_Action_Context);

      Command := new Delete_Command;
      Delete_Command (Command.all).Kind := Word;
      Delete_Command (Command.all).Count := 1;
      Register_Action
        (Kernel, "Delete word forward", Command,
           -"Delete the word following the current cursor position",
         Category => "Editor",
         Filter       => Writable_Src_Action_Context,
         For_Learning => True);

      Command := new Delete_Command;
      Delete_Command (Command.all).Kind := Word;
      Delete_Command (Command.all).Count := -1;
      Register_Action
        (Kernel, "Delete word backward", Command,
           -"Delete the word preceding the current cursor position or"
         & " all spaces from the current line.",
         Category => "Editor",
         Filter       => Writable_Src_Action_Context,
         For_Learning => True);

      Line_Numbers_Area_Filter := new In_Line_Numbers_Area_Filter;

      Register_Action
        (Kernel, "Lock or unlock current editor",
         new Lock_Or_Unlock_Commmand'(Root_Command with Split => False),
         -("Lock or unlock the current editor. Locked editors can be used to "
           & "maintain a given position in an editor. This is done by making "
           & "editors insensitive to 'jump to code' events such as navigation "
           & "actions, clicks on messages in the Locations view etc. Instead, "
           & "a new unlocked editor will be opened for the same file."),
         Category => "Editor",
         Filter   => Src_Action_Context);

      Register_Action
        (Kernel, "Lock or unlock current editor (split)",
         new Lock_Or_Unlock_Commmand'(Root_Command with Split => True),
         -("Lock or unlock the current editor, and put it in separate "
           & "notebook. This will split the MDI in two if needed "
           & "(i.e: when there is only one notebook available)."),
         Category => "Editor",
         Filter   => Src_Action_Context);

      Register_Contextual_Menu
        (Kernel,
         Action => "goto declaration",
         Label  => "Go To Declaration",
         Filter => Has_Entity_Name,
         Group  => Navigation_Contextual_Group);

      Register_Contextual_Menu
        (Kernel,
         Action => "goto other file",
         Name   => "goto other file",
         Label  => "Jump to %C File",
         Custom => Goto_Other_File_Label_Factory'Access,
         Filter => not Context_Has_Selection
         and Lookup_Filter (Kernel, "Source editor"),
         Group  => Navigation_Contextual_Group);

      Register_Contextual_Menu
        (Kernel,
         Action => "goto body",
         Label  => "Go To Body or Full Declaration",
         Filter => Has_Entity_Name,
         Group  => Navigation_Contextual_Group);

      Register_Contextual_Menu
        (Kernel,
         Action => "goto type of entity",
         Label  => -"Go To Type Declaration",
         Group  => Navigation_Contextual_Group);

      Register_Action
        (Kernel, "display type hierarchy of entity",
         Command    => new Type_Hierarchy_Command,
         Filter     => Has_Entity_Name);

      Register_Action
        (Kernel, "edit file",
         Command     => new Edit_File_Command,
         Description => -"Open an editor for the selected file",
         Filter      => Lookup_Filter (Kernel, "File")
            and not Create (Module => Src_Editor_Module_Name));
      Register_Contextual_Menu
        (Kernel,
         Label  => "Open %f",
         Action => "edit file",
         Group  => Navigation_Contextual_Group);

      Register_Action
        (Kernel, "edit file properties",
         Command     => new Editor_Properties_Command,
         Description =>
           -"Open a dialog to edit file properties (charset, language,...)",
         Filter   => Create (Module => Src_Editor_Module_Name),
         Category    => -"Editor");

      Register_Contextual_Menu
        (Kernel,
         Label  => "Properties...",
         Action => "edit file properties",
         Group  => File_Properties_Contextual_Group);

      Command := new Control_Command;
      Control_Command (Command.all).Mode := As_Is;
      Register_Action
        (Kernel, "No casing/indentation on next key",
         Command, -"Disable the casing and indentation on next key",
         Category   => "Editor",
         Filter     => Last_Editor_Context);

      Command := new Control_Command;
      Control_Command (Command.all).Mode := Sticky_As_Is;
      Register_Action
        (Kernel, "Toggle auto casing/indentation",
         Command, -"Disable or enable the casing and indentation",
         Category => "Editor",
         Filter     => Src_Action_Context);

      Register_Action
        (Kernel, "Insert TAB with spaces",
         new Tab_As_Space_Command,
         -("Insert spaces until a column multiple of the indentation level"
           & " as set in the Preferences for the corresponding language"),
         Category => "Editor",
         Filter   => Writable_Src_Action_Context);

      Register_Action
        (Kernel, "Delete TAB with spaces",
         new Delete_Tab_Command,
         -("Delete spaces according to indentation level set in the "
           & "Preferences for the corresponding language"),
         Category => "Editor",
         Filter   => Writable_Src_Action_Context);

      Command := new Add_String_Comment_Command;
      Register_Action
        (Kernel, "Wrap string/comment", Command,
           -"Wrap string or comment using the cursor location",
         Category => "Editor",
         Filter   => Is_Ada_File_Filter and Writable_Src_Action_Context);

      Command := new Split_String_Command;
      Register_Action
        (Kernel, "Insert in string template", Command,
         -("Template splitting the string at cursor location using " &
           "concatenation operators"),
         Category => "Editor",
         Filter   => Is_Ada_File_Filter and Writable_Src_Action_Context);

      Command := new Paste_Into_String_Command;
      Register_Action
        (Kernel, "Paste into string literal", Command,
         -("Paste clipboard at cursor location inside a string using" &
             " concatenation operators"),
         Category => "Editor",
         Filter   => Is_Ada_File_Filter and Writable_Src_Action_Context);

      Register_Module
        (Module      => Src_Editor_Module_Id,
         Kernel      => Kernel,
         Module_Name => Src_Editor_Module_Name,
         Priority    => Default_Priority);
      Register_Desktop_Functions (null, Load_Desktop'Access);

      Open_File_Action_Hook.Add (new On_Open_File);
      File_Line_Action_Hook.Add (new On_File_Line_Action);
      Word_Added_Hook.Add (new On_Word_Added);
      Character_Added_Hook.Add (new On_Character_Added);
      Project_View_Changed_Hook.Add (new On_Project_Changed);
      Gps_Started_Hook.Add (new On_Project_Changed);

      --  Menus

      Register_Action
        (Kernel, New_File_Command_Name, new New_File_Command,
         Description => -"Create a new empty editor",
         Icon_Name   => "gps-new-document-symbolic");

      Register_Action
        (Kernel, "new view", new New_View_Command,
         Description => -"Create a new view for the selected editor");

      Register_Action
        (Kernel, Save_Command_Name, new Save_Command,
         Icon_Name   => "gps-save-symbolic",
         Description => -"Save the current editor");

      Register_Action
        (Kernel, "save as", new Save_As_Command,
         Description => -"Save the current editor with a different name");

      Register_Action
        (Kernel, "print", new Src_Editor_Module.Commands.Print_Command,
         Icon_Name   => "gps-print-symbolic",
         Description => -"Print the current editor");

      --  Note: callbacks for the Undo/Redo menu items will be added later
      --  by each source editor.

      Register_Action
        (Kernel, "undo", new Undo_Command,
         Description => -"Undo the last command",
         Icon_Name   => "gps-undo-symbolic",
         Category    => -"Editor",
         Filter      => new Has_Undo_Filter);

      Register_Action
        (Kernel, "redo", new Redo_Command,
         Description => -"Redo the last command that was undone",
         Icon_Name   => "gps-redo-symbolic",
         Category    => -"Editor",
         Filter      => new Has_Redo_Filter);

      Register_Action
        (Kernel, "select all", new Select_All_Command,
         -"Select the whole contents of the editor",
         Category => -"Editor",
         Filter   => Src_Action_Context);

      Register_Action
        (Kernel, "insert file", new Insert_File_Command,
         -"Insert the contents of the file into the current editor",
         Category => -"Editor",
         Filter   => Src_Action_Context);

      Register_Action
        (Kernel, "refill", new Refill_Command,
         Description   =>
           -("Reformat selected lines or current paragraph so that the list"
           & " are shorter than the grey line on the right"),
         Category      => -"Editor",
         Filter        => Writable_Src_Action_Context,
         For_Learning  => True);

      Register_Action
        (Kernel, "print selection", new Print_Selection_Command,
         Description   => -"Print the current selection",
         Category      => -"Editor",
         Filter        => Writable_Src_Action_Context);

      Register_Action
        (Kernel, "format selection", new Formatting_Command,
         -"Format the current line or selection",
         Category => "Editor",
         Filter   => Writable_Src_Action_Context and Is_Not_Makefile);

      Command := new Formatting_Command;
      Formatting_Command (Command.all).Indent := True;
      Register_Action
        (Kernel, "indent selection", Command,
         -"Indent the current line or selection",
         Category => "Editor",
         Filter   => Writable_Src_Action_Context and Is_Not_Makefile);

      Register_Action
        (Kernel, "fold all blocks", new Fold_All_Blocks_Command,
         -"Fold all blocks (if, loops,...)",
         Category => -"Editor",
         Filter   => Src_Action_Context);

      Register_Action
        (Kernel, "fold all similar blocks",
         new Fold_All_Blocks_Command (Similar => True),
         -"Fold all blocks of current block type",
         Category => -"Editor",
         Filter   => Src_Action_Context);

      Register_Action
        (Kernel, "unfold all blocks", new Unfold_All_Blocks_Command,
         -"Unfold all blocks (if, loops,...)",
         Category => -"Editor",
         Filter   => Src_Action_Context);

      Register_Action
        (Kernel, "unfold all similar blocks",
         new Unfold_All_Blocks_Command (Similar => True),
         -"Unfold all blocks of current block type",
         Category => -"Editor",
         Filter   => Src_Action_Context);

      Register_Action
        (Kernel, "goto line",
         Command      => new Goto_Line_Command,
         Description  => -"Open a dialog to select a line to go to",
         Category     => -"Editor",
         For_Learning => True,
         Filter       => Src_Action_Context);
      Register_Contextual_Menu
        (Kernel,
         Label   => -"Goto line...",
         Action  => "Go To/Line",
         Filter  => Line_Numbers_Area_Filter);

      Register_Action
        (Kernel, "jump to matching delimiter", new Jump_To_Delimiter_Command,
         -"Jump to the matching delimiter ()[]{}",
         Category   => "Editor",
         Filter     => Src_Action_Context);

      Register_Action
        (Kernel,
         "Show Code Actions",
         new Activate_Code_Actions_Menu_Command,
         "Pop-up a menu with Code Actions applicable" &
           " to the current editor location");

      File_Renamed_Hook.Add (new On_File_Renamed);
      File_Saved_Hook.Add (new On_File_Saved);
      Location_Changed_Hook.Add_Debounce (new On_Cursor_Stopped);
      Semantic_Tree_Updated_Hook.Add (new On_Semantic_Tree_Updated);
      Preferences_Changed_Hook.Add (new On_Pref_Changed);
      File_Edited_Hook.Add (new On_File_Edited);
      File_Reloaded_Hook.Add (new On_File_Reloaded);
      File_Changed_On_Disk_Hook.Add (new On_File_Changed_On_Disk);
      File_Deleting_Hook.Add (new On_Deleting);

      Register_Commands (Kernel);

      Set_Buffer_Factory
        (Kernel, new Src_Editor_Buffer_Factory'
           (Src_Editor_Module.Editors.Create (Kernel_Handle (Kernel))));

      Set_Editor_Tooltip_Handler_Factory
        (Default_Editor_Tooltip_Handler_Factory'Access);

      Set_Hyper_Mode_Click_Callback
        (Default_Hyper_Mode_Click_Callback'Access);

      --  Register the search modules

      Selector := new Simple_Scope_Selector_Record;
      Initialize (Selector, Kernel);

      Extra := new Files_Extra_Scope_Record;
      Initialize (Extra, Kernel);

      Module := new Files_From_Project_Search_Module;
      Initialize
        (Module,
         Label    => -"Files From Projects",
         Selector => Selector,
         Id       => Src_Editor_Module_Id,
         Mask     => Default_Options_Mask);

      Register_Search_Function
        (Kernel     => Kernel,
         Module     => Module,
         Is_Default => True);

      Module := new Files_From_Root_Project_Search_Module;
      Initialize
        (Module,
         Label    => -"Files From Project '%p'",
         Selector => Selector,
         Id       => Src_Editor_Module_Id,
         Mask     => Default_Options_Mask);

      Register_Search_Function
        (Kernel     => Kernel,
         Module     => Module);

      Module := new Runtime_Files_Search_Module;
      Initialize
        (Module,
         Label    => -"Files From Runtime",
         Selector => Selector,
         Id       => Src_Editor_Module_Id,
         Mask     => Default_Options_Mask);

      Register_Search_Function
        (Kernel => Kernel,
         Module => Module);

      Module := new Files_Search_Module;
      Initialize
        (Module,
         Label    => -"Files...",
         Selector => Extra,
         Id       => Src_Editor_Module_Id,
         Mask     => Default_Options_Mask);

      Register_Search_Function
        (Kernel => Kernel,
         Module => Module);

      Module := new Open_Files_Search_Module;
      Initialize
        (Module,
         Label     => -"Open Files",
         Selector  => Selector,
         Id        => Src_Editor_Module_Id,
         Mask      => Default_Options_Mask);

      Register_Search_Function
        (Kernel => Kernel,
         Module => Module);

      Module := new Current_Selection_Search_Module;
      Initialize
        (Module,
         Label        => -"Current Selection",
         Selector     => Selector,
         Id           => Src_Editor_Module_Id,
         Mask         => All_Options and not Supports_Incremental,
         In_Selection => True);

      Register_Search_Function
        (Kernel => Kernel,
         Module => Module);

      Module := new Current_File_Search_Module;
      Initialize
        (Module,
         Label    => -"Current File",
         Selector => Selector,
         Id       => Src_Editor_Module_Id,
         Mask     => All_Options);

      Register_Search_Function
        (Kernel => Kernel,
         Module => Module);

      --  Register the aliases special entities

      Register_Special_Alias_Entity
        (Kernel, -"Current line",   'l', Expand_Aliases_Entities'Access);
      Register_Special_Alias_Entity
        (Kernel, -"Current column", 'c', Expand_Aliases_Entities'Access);
      Register_Special_Alias_Entity
        (Kernel, -"Current file",   'f', Expand_Aliases_Entities'Access);
      Register_Special_Alias_Entity
        (Kernel, -"Project for the current file", 'p',
         Expand_Aliases_Entities'Access);
      Register_Special_Alias_Entity
        (Kernel, -"Full path of project for the current file", 'P',
         Expand_Aliases_Entities'Access);
      Register_Special_Alias_Entity
        (Kernel, -"Directory of current file", 'd',
         Expand_Aliases_Entities'Access);

      Completion_Module.Register_Module (Kernel);

      --  Register the message listener for editors
      Src_Editor_Module.Messages.Register (Kernel);

      --  Create highlighting categories preemptively for builder styles, so
      --  that errors always have higher priority than warnings, etc..

      Line_Highlighting.Add_Category (Messages_Styles (High));
      Line_Highlighting.Add_Category (Messages_Styles (Medium));
      Line_Highlighting.Add_Category (Messages_Styles (Low));
      Line_Highlighting.Add_Category (Messages_Styles (Informational));
      Line_Highlighting.Add_Category (Debugger_Current_Line_Style);
      Line_Highlighting.Add_Category (Bookmark_Default_Style);

      Regenerate_Recent_Files_Menu (Kernel);
   end Register_Module;

   ------------------------
   -- Create_Preferences --
   ------------------------

   procedure Create_Preferences
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Id      : constant Source_Editor_Module :=
        Source_Editor_Module (Src_Editor_Module_Id);
      Choices : VSS.String_Vectors.Virtual_String_Vector;
      Langs   : constant VSS.String_Vectors.Virtual_String_Vector :=
        ["Ada", "C++"];
   begin
      for Provider of Id.Formatting_Providers loop
         declare
            Choice : constant VSS.Strings.Virtual_String :=
              VSS.Strings.Conversions.To_Virtual_String (Provider.Get_Name);
         begin
            Choices.Append (Choice);
         end;
      end loop;
      --  Add an empty choice
      Choices.Append
        (VSS.Strings.Conversions.To_Virtual_String (Disabled_Pref_Choice));

      for Lang of Langs loop
         declare
            Menu_Name : constant String :=
              (if Lang = "C++"
               then "C & C++"
               else VSS.Strings.Conversions.To_UTF_8_String (Lang));
            Suffix_Name : constant String :=
              To_Lower (VSS.Strings.Conversions.To_UTF_8_String (Lang));
            --  Use lower case for consistency
            Path        : constant String :=
              "Editor/" & Menu_Name & ":Formatting";
            Default     : constant VSS.Strings.Virtual_String :=
              (if Lang = "Ada"
               then VSS.Strings.Conversions.To_Virtual_String
                 (Legacy_Formatter_Value)
               else VSS.Strings.Conversions.To_Virtual_String
                 (LSP_Formatter_Value));
            Dummy       : Default_Preferences.Enums.Choice_Preference;
            Ignore      : Default_Preferences.Boolean_Preference;
         begin
            Dummy :=
              Default_Preferences.Enums.Create
                (Manager         => Kernel.Get_Preferences,
                 Name            => Range_Format_Name_Prefix & Suffix_Name,
                 Choices         => Choices,
                 Default         => Default,
                 Label           => "Formatter for range formatting",
                 Doc             =>
                   "Choose which formatter should be used when "
                   & "formatting a range. Legacy is GNAT Studio's own "
                   & "formatter which will be baselined in favor of the "
                   & "LSP formatter.",
                 Path            => Path,
                 Combo_Threshold => 1);
            Dummy :=
              Default_Preferences.Enums.Create
                (Manager         => Kernel.Get_Preferences,
                 Name            => On_Type_Format_Name_Prefix & Suffix_Name,
                 Choices         => Choices,
                 Default         => Default,
                 Label           => "Formatter on enter",
                 Doc             =>
                   "Choose which formatter should be used when pressing "
                   & "enter. Legacy is GNAT Studio's own formatter "
                   & "which will be baselined in favor of the LSP formatter.",
                 Path            => Path,
                 Combo_Threshold => 1);
            Ignore :=
              Kernel.Get_Preferences.Create
                (Name    => Move_Cursor_Format_Prefix & Suffix_Name,
                 Default => True,
                 Label   => "Move cursor when formatting",
                 Doc     =>
                   "Should the cursor move to the end of the formatting edit.",
                 Path    => Path);
            Ignore :=
              Kernel.Get_Preferences.Create
                (Name    => Limit_LSP_Format_Prefix & Suffix_Name,
                 Default => False,
                 Label   => "Limit LSP formatting",
                 Doc     =>
                   "Limit LSP formatting request to the current selection: "
                 & "it prevents overzealous formatting affecting unselected"
                 & "lines.",
                 Path    => Path);
         end;
      end loop;
   end Create_Preferences;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference)
   is
      pragma Unreferenced (Self);
      Pref_Display_Line_Numbers     : constant Boolean :=
                                        Display_Line_Numbers.Get_Pref /= Never;
      Pref_Display_Subprogram_Names : constant Boolean :=
                                        Display_Subprogram_Names.Get_Pref;

      Font_Has_Changed : Boolean := False;
      Line_Display_Has_Changed : Boolean := False;

      Id : constant Source_Editor_Module :=
             Source_Editor_Module (Src_Editor_Module_Id);

      Runtime_Use_ACL : C.int;
      pragma Import (C, Runtime_Use_ACL, "__gnat_use_acl");

      Iter  : Child_Iterator;
      Child : MDI_Child;
   begin
      Line_Highlighting.Add_Category (Search_Results_Style);

      if Pref_Display_Subprogram_Names /= Id.Show_Subprogram_Names then
         --  The preference for showing the subprogram name has changed:
         --  we need either to show or to hide the name on all open editors.

         Iter := First_Child (Get_MDI (Kernel));
         loop
            Child := Get (Iter);
            exit when Child = null;

            if Get_Widget (Child) /= null
              and then Get_Widget (Child).all in Source_Editor_Box_Record'Class
            then
               Source_Editor_Box (Get_Widget (Child)).Update_Subprogram_Name;
            end if;

            Next (Iter);
         end loop;

         Id.Show_Subprogram_Names := Pref_Display_Subprogram_Names;
      end if;

      if Id.Font /= Default_Style.Get_Pref_Font then
         Id.Font := Default_Style.Get_Pref_Font;
         Font_Has_Changed := True;
      end if;

      if Pref_Display_Line_Numbers /= Id.Display_Line_Numbers then
         Id.Display_Line_Numbers := Pref_Display_Line_Numbers;

         Line_Display_Has_Changed := True;
      end if;

      if Pref = Preference (Gutter_Right_Margin) then
         Line_Display_Has_Changed := True;
      end if;

      if Font_Has_Changed or else Line_Display_Has_Changed then
         if Id.Display_Line_Numbers then
            --  Recompute the width of the character
            declare
               Layout : constant Pango_Layout := Create_Pango_Layout
                 (Get_Main_Window (Kernel));
               Height : Gint;
               Width  : Gint;
            begin
               Set_Font_Description (Layout, Default_Style.Get_Pref_Font);
               Set_Markup (Layout, "0000");
               Get_Pixel_Size (Layout, Width, Height);
               Id.Character_Width :=
                 Gint'Max (Width / 4, Minimum_Character_Width);
               Unref (Layout);
            end;

            --  Tell the editors to refresh their side columns
            Iter  := First_Child (Get_MDI (Kernel));

            loop
               Child := Get (Iter);
               exit when Child = null;

               if Get_Widget (Child).all in Source_Editor_Box_Record'Class then
                  Refresh_Side_Column
                    (Get_Buffer (Source_Editor_Box (Get_Widget (Child))));
               end if;

               Next (Iter);
            end loop;
         else
            Id.Character_Width := Minimum_Character_Width;
         end if;
      end if;

      --  Set ACL usage
      if Config.Host = Config.Windows then
         if Use_ACL.Get_Pref then
            Runtime_Use_ACL := 1;
         else
            Runtime_Use_ACL := 0;
         end if;
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Deleting;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File)
   is
      pragma Unreferenced (Self);

      procedure Close_View
        (Child : not null access GPS_MDI_Child_Record'Class);

      ----------------
      -- Close_View --
      ----------------

      procedure Close_View
        (Child : not null access GPS_MDI_Child_Record'Class)
      is
         Box : constant Source_Editor_Box :=
           Source_Editor_Box (Get_Widget (Child));
      begin
         if Box.Get_Buffer.Needs_To_Be_Saved then
            --  undo all changes to prevent save dialog showing
            Standard.Commands.Empty_Queue (Box.Get_Buffer.Get_Command_Queue);
         end if;
         Close (Get_MDI (Kernel), Box, True);
      end Close_View;

   begin
      For_All_Views (Kernel, File, Close_View'Access);
   end Execute;

   ----------
   -- Free --
   ----------

   procedure Free (Categories : in out Highlighting_Category_Array) is
   begin
      for C in Categories'Range loop
         Unchecked_Free (Categories (C));
      end loop;
   end Free;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Id : in out Source_Editor_Module_Record) is
      High_Iter : List_Of_Highlighters.Cursor;
      H         : Highlighter_Record;
   begin
      Id.Stored_Marks.Clear;

      Free (Id.Search_Pattern);

      --  Post_It_Note_GC and Blank_Lines_GC are initialized only when the
      --  main window is mapped. Therefore, if the window was never displayed,
      --  these values are not initialized.

      if Id.Categories /= null then
         Free (Id.Categories.all);
         Unchecked_Free (Id.Categories);
      end if;

      High_Iter := Id.Highlighters.First;
      while List_Of_Highlighters.Has_Element (High_Iter) loop
         H := List_Of_Highlighters.Element (High_Iter);
         Free (H);
         List_Of_Highlighters.Next (High_Iter);
      end loop;

      Id.Editors.Clear;

      Destroy (Src_Editor_Buffer_Factory
               (Get_Buffer_Factory (Get_Kernel (Id)).all));

      Free (Id.Undo_Redo);
      Src_Editor_Module.Messages.Unregister (Get_Kernel (Id));

      Src_Editor_Module_Id := null;
   end Destroy;

   -----------------
   -- Find_Editor --
   -----------------

   function Find_Editor
     (Kernel        : access GPS.Kernel.Kernel_Handle_Record'Class;
      File          : GNATCOLL.VFS.Virtual_File;
      Project       : GNATCOLL.Projects.Project_Type;
      Unlocked_Only : Boolean := False) return Gtkada.MDI.MDI_Child
   is
      Id    : constant Source_Editor_Module :=
                Source_Editor_Module (Src_Editor_Module_Id);
      Iter  : Child_Iterator;
      Child : MDI_Child;
      Full  : GNATCOLL.VFS.Virtual_File;

      function Project_Matches (Child : MDI_Child) return Boolean is
        (Project = No_Project
         or else Get_Project (Child) = No_Project
         or else Get_Project (Child) = Project);
      --  Whether the project associated with child matches the expected one.
      --  This could be because we allow any project (Project=No_Project), or
      --  because the child is not associated with a project yet, or because it
      --  indeed is the same project.

   begin
      if File = GNATCOLL.VFS.No_File or else Get_MDI (Kernel) = null then
         return null;
      end if;

      --  Check whether the currently selected child corresponds to
      --  the file (this will be the case in a vast majority of calls to
      --  this subprogram)

      Child := Get_Focus_Child (Get_MDI (Kernel));

      if Child /= null
        and then Get_Filename (Child) = File
        and then
          (not Unlocked_Only
           or else not Get_Source_Box_From_MDI (Child).Is_Locked)
      then
         return Child;
      end if;

      --  Attempt to find the editor in the cache

      if Id.Editors.Contains (File) then
         Child := Id.Editors.Element (File).Child;
      end if;

      --  Verify that the child corresponds to the wanted filename.
      --  (It could have changed, for example if "save as..." was used)

      if Child /= null then
         if Get_Filename (Child) = File
           and then
             (not Unlocked_Only
              or else not Get_Source_Box_From_MDI (Child).Is_Locked)
         then
            return Child;
         else
            if Id.Editors.Contains (File) then
               Id.Editors.Delete (File);
            end if;
         end if;
      end if;

      --  The editor could not be found in the hash table, find it by cycling
      --  through the editors.

      if Is_Absolute_Path (File) then
         Full := File;
      else
         Full := Get_Registry (Kernel).Tree.Create
           (File.Full_Name, Use_Object_Path => False);
         if Full = No_File then
            return null;
         end if;
      end if;

      Iter := First_Child (Get_MDI (Kernel));

      loop
         Child := Get (Iter);

         exit when Child = null
           or else
             ((Get_Filename (Child) = Full
               or else Get_File_Identifier (Child) = Full
               --  Handling of file identifiers
               or else Get_Title (Child) = Display_Full_Name (File))
              and then Project_Matches (Child)
              and then
                (not Unlocked_Only
                 or else not Get_Source_Box_From_MDI (Child).Is_Locked));

         Next (Iter);
      end loop;

      return Child;
   end Find_Editor;

   ---------
   -- Get --
   ---------

   function Get
     (Kernel : access Kernel_Handle_Record'Class;
      File   : Virtual_File) return Source_Buffer
   is
      Child : MDI_Child;
      Box   : Source_Editor_Box;
   begin
      if File /= GNATCOLL.VFS.No_File then
         Child := Find_Editor
           (Kernel, File,
            GNATCOLL.Projects.No_Project);  --  ??? any project
      end if;

      if Child /= null then
         Box := Get_Source_Box_From_MDI (Child);
         return Get_Buffer (Box);
      end if;

      return null;
   end Get;

   -----------------------
   -- Find_Other_Editor --
   -----------------------

   function Find_Other_Editor
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      View   : Gtk_Text_View;
      Buffer : Gtk_Text_Buffer) return Src_Editor_Box.Source_Editor_Box
   is
      Iter   : Child_Iterator := First_Child (Get_MDI (Kernel));
      Editor : Src_Editor_Box.Source_Editor_Box;
      Child  : MDI_Child;
      Source : Source_Buffer;

   begin
      Child := Get (Iter);

      while Child /= null loop
         if Get_Widget (Child).all in Source_Editor_Box_Record'Class then
            Editor := Source_Editor_Box (Get_Widget (Child));

            Source := Get_Buffer (Editor);

            if Gtk_Text_Buffer (Source) = Buffer
              and then Gtk_Text_View (Get_View (Editor)) /= View
            then
               return Editor;
            end if;
         end if;

         Next (Iter);
         Child := Get (Iter);
      end loop;

      return null;
   end Find_Other_Editor;

   ----------------
   -- Find_Child --
   ----------------

   function Find_Child
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Editor : access Src_Editor_Box.Source_Editor_Box_Record'Class)
      return Gtkada.MDI.MDI_Child
   is
      pragma Unreferenced (Kernel);
   begin
      return MDI_Child (Get_View (Editor).Get_Child);
   end Find_Child;

   ---------------------------------
   -- Line_Number_Character_Width --
   ---------------------------------

   function Line_Number_Character_Width return Gint is
      Id : constant Source_Editor_Module :=
             Source_Editor_Module (Src_Editor_Module_Id);
   begin
      if Id = null then
         return Minimum_Character_Width;
      else
         return Id.Character_Width;
      end if;
   end Line_Number_Character_Width;

   --------------------
   -- Tab_Contextual --
   --------------------

   overriding procedure Tab_Contextual
     (Child : access Editor_Child_Record;
      Menu  : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      Source_Box : constant Source_Editor_Box := Source_Editor_Box
        (Child.Get_Widget);
      Menu_Name  : constant String :=
        (if Source_Box.Is_Locked then
            "Unlock "
         else
            "Lock ") & "current editor";
      Item       : Gtk_Menu_Item;
      Sep        : Gtk_Separator_Menu_Item;
   begin
      --  Call the parent's primitive first
      Tab_Contextual (GPS_MDI_Child_Record (Child.all)'Access, Menu);

      --  Add a contextual menu for the 'Lock or unlock current editor'
      Gtk_New (Item, Menu_Name);
      Menu.Insert (Item, 3);

      Kernel_Callback.Connect
        (Item, Gtk.Menu_Item.Signal_Activate,
         On_Lock_Or_Unlock_Tab_Contextual_Menu'Access,
         Child.Kernel);

      --  Unlocking a split editor is done by the 'Lock or unlock current
      --  editor' action.
      if not Source_Box.Is_Locked then
         Gtk_New (Item, Menu_Name & " (split)");
         Menu.Insert (Item, 4);

         Kernel_Callback.Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            On_Lock_Or_Unlock_Split_Tab_Contextual_Menu'Access,
            Child.Kernel);
         Gtk_New (Sep);
         Menu.Insert (Sep, 5);
      else
         Gtk_New (Sep);
         Menu.Insert (Sep, 4);
      end if;
   end Tab_Contextual;

   -----------------
   -- Get_Tooltip --
   -----------------

   overriding function Get_Tooltip
     (Self  : not null access Editor_Child_Record) return String is
   begin
      return Get_Tooltip_For_File
        (Kernel    => Self.Kernel,
         File      => Get_Filename (Self),
         Project   => Get_Project (Self));
   end Get_Tooltip;

   -----------------------
   -- Get_Command_Queue --
   -----------------------

   overriding function Get_Command_Queue
     (Child : access Editor_Child_Record)
      return Standard.Commands.Command_Queue
   is
      Box : constant Source_Editor_Box :=
        Get_Source_Box_From_MDI (MDI_Child (Child));
   begin
      return Get_Command_Queue (Get_Buffer (Box));
   end Get_Command_Queue;

   --------------------------
   -- Register_Highlighter --
   --------------------------

   procedure Register_Highlighter (Highlighter : Highlighter_Record) is
      Id : constant Source_Editor_Module :=
             Source_Editor_Module (Src_Editor_Module_Id);
   begin
      Id.Highlighters.Append (Highlighter);
   end Register_Highlighter;

   ----------------------------
   -- Unregister_Highlighter --
   ----------------------------

   procedure Unregister_Highlighter (Highlighter : Highlighter_Record) is
      Id     : constant Source_Editor_Module :=
                 Source_Editor_Module (Src_Editor_Module_Id);
      Cursor : List_Of_Highlighters.Cursor;
      use List_Of_Highlighters;
   begin
      Cursor := Id.Highlighters.First;

      while Has_Element (Cursor) loop
         if List_Of_Highlighters.Element (Cursor).Pattern_String.all =
           Highlighter.Pattern_String.all
         then
            declare
               H : Highlighter_Record;
            begin
               H := List_Of_Highlighters.Element (Cursor);
               Free (H);
            end;

            Id.Highlighters.Delete (Cursor);
            return;
         end if;

         Cursor := Next (Cursor);
      end loop;
   end Unregister_Highlighter;

   ------------------------
   -- Register_Formatter --
   ------------------------

   procedure Register_Formatter (Provider : Editor_Formatting_Provider_Access)
   is
      Id : constant Source_Editor_Module :=
        Source_Editor_Module (Src_Editor_Module_Id);
   begin
      if Provider /= null then
         Id.Formatting_Providers.Append (Provider);
      end if;
   end Register_Formatter;

   -----------------------------------
   -- Set_Hyper_Mode_Click_Callback --
   -----------------------------------

   procedure Set_Hyper_Mode_Click_Callback
     (Callback : not null Hyper_Mode_Click_Callback_Type)
   is
      Id : constant Source_Editor_Module :=
             Source_Editor_Module (Src_Editor_Module_Id);
   begin
      Id.Hyper_Mode_Click_Cb := Callback;
   end Set_Hyper_Mode_Click_Callback;

   -----------------------------------
   -- Get_Hyper_Mode_Click_Callback --
   -----------------------------------

   function Get_Hyper_Mode_Click_Callback
     return Hyper_Mode_Click_Callback_Type is
      Id : constant Source_Editor_Module :=
             Source_Editor_Module (Src_Editor_Module_Id);
   begin
      return Id.Hyper_Mode_Click_Cb;
   end Get_Hyper_Mode_Click_Callback;

   ----------------------------------------
   -- Set_Editor_Tooltip_Handler_Factory --
   ----------------------------------------

   procedure Set_Editor_Tooltip_Handler_Factory
     (Tooltip_Factory : not null Editor_Tooltip_Handler_Factory_Access)
   is
      Id : constant Source_Editor_Module :=
             Source_Editor_Module (Src_Editor_Module_Id);
   begin
      Id.Tooltip_Factory := Tooltip_Factory;
   end Set_Editor_Tooltip_Handler_Factory;

   ----------------------------------------
   -- Get_Editor_Tooltip_Handler_Factory --
   ----------------------------------------

   function Get_Editor_Tooltip_Handler_Factory
     return Editor_Tooltip_Handler_Factory_Access
   is
      Id : constant Source_Editor_Module :=
             Source_Editor_Module (Src_Editor_Module_Id);
   begin
      return Id.Tooltip_Factory;
   end Get_Editor_Tooltip_Handler_Factory;

   ---------------------------
   -- On_Ed_View_Focus_Lost --
   ---------------------------

   procedure On_Ed_View_Focus_Lost (Child : MDI_Child; File : Virtual_File) is
      Id : constant Source_Editor_Module :=
        Source_Editor_Module (Src_Editor_Module_Id);
   begin
      Id.Editors.Include (File, (Child => Child));
      Id.Last_Focused_Editor := Child;
   end On_Ed_View_Focus_Lost;

   ----------------------
   -- Get_Highlighters --
   ----------------------

   function Get_Highlighters return List_Of_Highlighters.List is
      Id : constant Source_Editor_Module :=
             Source_Editor_Module (Src_Editor_Module_Id);
   begin
      return Id.Highlighters;
   end Get_Highlighters;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Highlighter_Record) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (GNAT.Regpat.Pattern_Matcher, GNAT.Expect.Pattern_Matcher_Access);
   begin
      Free (Self.Pattern_String);
      Free (Self.Action);
      Free (Self.Alternate);
      Unchecked_Free (Self.Pattern);
   end Free;

   ---------------------------------------
   -- Default_Hyper_Mode_Click_Callback --
   ---------------------------------------

   procedure Default_Hyper_Mode_Click_Callback
     (Kernel      : not null Kernel_Handle;
      Buffer      : GPS.Editors.Editor_Buffer'Class;
      Project     : GNATCOLL.Projects.Project_Type;
      Line        : Editable_Line_Type;
      Column      : Visible_Column_Type;
      Entity_Name : String;
      Alternate   : Boolean)
   is
      Closest  : Root_Entity_Reference_Ref;
      Decl     : General_Entity_Declaration;
      Location : General_Location;
      Current  : General_Location;
      File     : constant Virtual_File := Buffer.File;
      Entity   : constant Root_Entity'Class :=
                   Kernel.Databases.Get_Entity
                     (Loc         =>
                        (File         => File,
                         Project_Path => Project.Project_Path,
                         Line         => Integer (Line),
                         Column       => Column),
                      Name        => Entity_Name,
                      Closest_Ref => Closest);
   begin
      if Entity = No_Root_Entity then
         return;
      end if;

      Decl := Get_Declaration (Entity);
      Location := Decl.Loc;

      --  Do not check the column: it is unlikely to have both spec and
      --  body on the same line, and this works around an issue in the
      --  constructs where going to the body for a child package
      --  declaration goes in fact to the name of the parent
      --  package on the child package declaration line.

      if Alternate
        or else
          (Location.Line = Natural (Line)
           and then Location.File = File)
      then
         --  We asked for the alternate behavior, or we are already on
         --  the spec: in this case, go to the body
         Current :=
           (File         => File,
            Project_Path => Project.Project_Path,
            Line         => Integer (Line),
            Column       => Column);
         Location := Get_Body (Entity, After => Current);
         if Location = No_Location then
            Location := Decl.Loc;
         end if;
      end if;

      if Location /= No_Location then
         --  Open editor when Location is valid

         Go_To_Closest_Match
           (Kernel,
            Location.File,
            Get_Project (Location),
            Editable_Line_Type (Location.Line),
            Location.Column, Entity);
      end if;
   end Default_Hyper_Mode_Click_Callback;

end Src_Editor_Module;
