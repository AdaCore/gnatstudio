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

with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.IO_Exceptions;                 use Ada.IO_Exceptions;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with GNAT.Directory_Operations;         use GNAT.Directory_Operations;
with GNAT.OS_Lib;                       use GNAT.OS_Lib;
with GNAT.Regpat;                       use GNAT.Regpat;
with GNATCOLL.Projects;                 use GNATCOLL.Projects;
with GNATCOLL.VFS_Utils;                use GNATCOLL.VFS_Utils;

with Gdk.Types.Keysyms;                 use Gdk.Types.Keysyms;
with Gdk.Types;                         use Gdk.Types;
with Gdk;                               use Gdk;

with Glib.Object;                       use Glib.Object;
with Glib.Unicode;                      use Glib.Unicode;
with Glib.Values;                       use Glib.Values;

with Gtk.Box;                           use Gtk.Box;
with Gtk.Check_Button;                  use Gtk.Check_Button;
with Gtk.Combo_Box;                     use Gtk.Combo_Box;
with Gtk.Dialog;                        use Gtk.Dialog;
with Gtk.Enums;                         use Gtk.Enums;
with Gtk.GEntry;                        use Gtk.GEntry;
with Gtk.Handlers;                      use Gtk.Handlers;
with Gtk.Label;                         use Gtk.Label;
with Gtk.Main;                          use Gtk.Main;
with Gtk.Menu_Item;                     use Gtk.Menu_Item;
with Gtk.Rc;                            use Gtk.Rc;
with Gtk.Size_Group;                    use Gtk.Size_Group;
with Gtk.Stock;                         use Gtk.Stock;
with Gtk.Separator_Menu_Item;           use Gtk.Separator_Menu_Item;
with Gtk.Separator_Tool_Item;           use Gtk.Separator_Tool_Item;
with Gtk.Tool_Button;                   use Gtk.Tool_Button;
with Gtk.Toolbar;                       use Gtk.Toolbar;
with Gtk.Window;                        use Gtk.Window;

with Gtkada.Entry_Completion;           use Gtkada.Entry_Completion;
with Gtkada.File_Selector;              use Gtkada.File_Selector;
with Gtkada.Handlers;                   use Gtkada.Handlers;
with Gtkada.Types;                      use Gtkada.Types;

with Pango.Layout;                      use Pango.Layout;

with Aliases_Module;                    use Aliases_Module;
with Casing_Exceptions;                 use Casing_Exceptions;
with Case_Handling;                     use Case_Handling;
with Commands.Interactive;              use Commands, Commands.Interactive;
with Completion_Module;                 use Completion_Module;
with Config;                            use Config;
with Default_Preferences;               use Default_Preferences;
with Find_Utils;                        use Find_Utils;
with GPS.Intl;                          use GPS.Intl;
with GPS.Editors;                       use GPS.Editors;
with GPS.Editors.Line_Information;      use GPS.Editors.Line_Information;
with GPS.Kernel.Actions;                use GPS.Kernel.Actions;
with GPS.Kernel.Charsets;               use GPS.Kernel.Charsets;
with GPS.Kernel.Console;                use GPS.Kernel.Console;
with GPS.Kernel.Contexts;               use GPS.Kernel.Contexts;
with GPS.Kernel.MDI;                    use GPS.Kernel.MDI;
with GPS.Kernel.Modules.UI;             use GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences;            use GPS.Kernel.Preferences;
with GPS.Kernel.Project;                use GPS.Kernel.Project;
with GPS.Kernel.Standard_Hooks;         use GPS.Kernel.Standard_Hooks;
with Histories;                         use Histories;
with Language;                          use Language;
with Language_Handlers;                 use Language_Handlers;
with Language_Handlers.GUI;             use Language_Handlers.GUI;
with Projects;                          use Projects;
with Remote;                            use Remote;
with Src_Contexts;                      use Src_Contexts;
with Src_Editor_Box;                    use Src_Editor_Box;
with Src_Editor_Buffer.Buffer_Commands; use Src_Editor_Buffer.Buffer_Commands;
with Src_Editor_Buffer.Hooks;           use Src_Editor_Buffer.Hooks;
with Src_Editor_Buffer.Line_Information;
use Src_Editor_Buffer.Line_Information;
with Src_Editor_Buffer.Text_Handling;   use Src_Editor_Buffer.Text_Handling;
with Src_Editor_Module.Line_Highlighting;
with Src_Editor_Module.Editors;         use Src_Editor_Module.Editors;
with Src_Editor_Module.Markers;         use Src_Editor_Module.Markers;
with Src_Editor_Module.Shell;           use Src_Editor_Module.Shell;
with Src_Editor_Module.Messages;        use Src_Editor_Module.Messages;

with Src_Editor_View.Commands;          use Src_Editor_View.Commands;
with Src_Editor_View;                   use Src_Editor_View;
with Src_Printing.Fabric;
with String_Utils;                      use String_Utils;
with UTF8_Utils;                        use UTF8_Utils;
with Traces;                            use Traces;
with Vsearch;                           use Vsearch;

package body Src_Editor_Module is

   Me : constant Debug_Handle := Create ("Src_Editor_Module");

   Hist_Key : constant History_Key := "reopen_files";
   --  Key to use in the kernel histories to store the most recently opened
   --  files.

   Open_From_Path_History : constant History_Key := "open-from-project";
   --  Key used to store the most recently open files in the Open From Project
   --  dialog.

   Underscore : constant Gunichar := UTF8_Get_Char ("_");
   Space      : constant Gunichar := UTF8_Get_Char (" ");
   Backspace  : constant Gunichar := 8;

   fold_block_xpm : aliased Chars_Ptr_Array (0 .. 0);
   pragma Import (C, fold_block_xpm, "fold_block_xpm");
   unfold_block_xpm  : aliased Chars_Ptr_Array (0 .. 0);
   pragma Import (C, unfold_block_xpm, "unfold_block_xpm");
   close_block_xpm  : aliased Chars_Ptr_Array (0 .. 0);
   pragma Import (C, close_block_xpm, "close_block_xpm");

   Cursor_Color        : Color_Preference;
   Cursor_Aspect_Ratio : Integer_Preference;

   type Editor_Child_Record is new GPS_MDI_Child_Record with null record;

   overriding function Get_Command_Queue
     (Child : access Editor_Child_Record) return Commands.Command_Queue;
   overriding function Dnd_Data
     (Child : access Editor_Child_Record; Copy : Boolean) return MDI_Child;
   --  See inherited documentation

   function Source_File_Hook
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class) return Boolean;
   --  Reacts to the Open_File_Action_Hook

   function File_Line_Hook
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class) return Boolean;
   --  Reacts to the File_Line_Action_Hook

   procedure Word_Added_Hook
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Reacts to the word_added Hook

   procedure User_Character_Added_Hook
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Reacts to the character_added Hook

   procedure Save_To_File
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      Name    : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Success : out Boolean);
   --  Save the current editor to Name, or its associated filename if Name is
   --  null.

   type Location_Idle_Data is record
      Edit  : Source_Editor_Box;
      Line  : Editable_Line_Type;
      Column, Column_End : Character_Offset_Type;
      Kernel : Kernel_Handle;
      Focus  : Boolean;
   end record;

   function File_Edit_Callback (D : Location_Idle_Data) return Boolean;
   --  Emit the File_Edited signal

   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child;
   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      User   : Kernel_Handle)
      return Node_Ptr;
   --  Support functions for the MDI

   procedure On_Open_File
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  File->Open menu

   procedure On_Open_From_Path
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  File->Open From Path menu

   procedure On_Open_Remote_File
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  File->Open Remote menu

   procedure On_New_View
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  File->New View menu

   procedure On_New_File
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  File->New menu

   procedure On_Save
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  File->Save menu

   procedure On_Save_As
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  File->Save As... menu

   procedure On_Print
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  File->Print menu

   procedure On_Print_Selection
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Edit->Selection->Print Selection menu

   procedure On_Select_All
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Edit->Select All menu

   procedure On_Insert_File
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Edit->Insert File... menu

   procedure On_Goto_Declaration
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Navigate->Goto Declaration menu
   --  Goto the declaration of the entity under the cursor in the current
   --  editor.

   procedure On_Goto_Body
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Navigate->Goto Body menu
   --  Goto the next body of the entity under the cursor in the current
   --  editor.

   procedure On_Comment_Lines
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Edit->Comment Lines menu

   procedure On_Uncomment_Lines
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Edit->Uncomment Lines menu

   procedure On_Fold_Blocks
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Edit->Fold all blocks menu

   procedure On_Unfold_Blocks
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Edit->Unfold all blocks menu

   procedure On_Refill
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Edit->Refill

   procedure Comment_Uncomment
     (Kernel : Kernel_Handle; Comment : Boolean);
   --  Comment or uncomment the current selection, if any.
   --  Auxiliary procedure for On_Comment_Lines and On_Uncomment_Lines.

   type File_Completion_Factory is new Completions_Factory with record
      File1, File2 : File_Array_Access;
   end record;
   overriding function Completion
     (Factory : File_Completion_Factory; Index : Positive) return String;
   overriding function Description
     (Factory : File_Completion_Factory; Index : Positive) return String;
   --  See doc from inherited subprogram

   type Edit_File_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Edit_File_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  See doc for inherited subprogram
   --  Edit a file (from a contextual menu)

   type Editor_Properties_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Editor_Properties_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  See doc for inherited subprogram
   --  Edit the properties of a file (from a contextual menu)

   type Close_Command is new Interactive_Command with record
      Kernel    : Kernel_Handle;
      Close_All : Boolean;
   end record;
   overriding function Execute
     (Command : access Close_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Close the current window (or all windows if Close_All is True)

   procedure New_View
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Create a new view for the current editor and add it in the MDI.
   --  The current editor is the focus child in the MDI. If the focus child
   --  is not an editor, nothing happens.

   procedure File_Edited_Cb
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Callback for the "file_edited" hook

   procedure File_Changed_On_Disk_Cb
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Callback for the "file_changed_on_disk" hook

   procedure File_Renamed_Cb
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Callback for the "file_renamed" hook

   procedure File_Saved_Cb
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Callback for the "file_saved" hook

   procedure Cursor_Stopped_Cb
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Callback for the "cursor_stopped" hook

   procedure Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class);
   --  Called when the preferences have changed

   procedure Add_To_Recent_Menu
     (Kernel : access Kernel_Handle_Record'Class; File : Virtual_File);
   --  Add an entry for File to the Recent menu, if needed

   function Get_Filename (Child : MDI_Child) return GNATCOLL.VFS.Virtual_File;
   --  If Child is a file editor, return the corresponding filename,
   --  otherwise return an empty string.

   function Get_File_Identifier (Child  : MDI_Child) return Virtual_File;
   --  Return the file identifier if Child is a file editor

   function Expand_Aliases_Entities
     (Kernel    : access Kernel_Handle_Record'Class;
      Expansion : String;
      Special   : Character) return String;
   --  Does the expansion of special entities in the aliases

   type On_Recent is new Menu_Callback_Record with record
      Kernel : Kernel_Handle;
   end record;
   overriding procedure Activate (Callback : access On_Recent; Item : String);

   procedure Toolbar_Destroy_Cb
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Called when we are about to destroy the toolbar which contains the
   --  undo/redo items.

   procedure On_Editor_Destroy
     (Widget : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues);
   --  Callback to call when an editor is about to be destroyed

   procedure Update_Cache_On_Focus
     (Child : access Gtk_Widget_Record'Class);
   --  Make sure that the last view for a file is reflected in the cache, so
   --  that we always use that one by default when looking for the last editor
   --  for a given file.

   procedure Create_Files_Pixbufs_If_Needed
     (Handle : access Kernel_Handle_Record'Class);
   --  Create File_Pixbuf and File_Modified_Pixbuf if needed

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
      I  : Editors_Hash.Cursor;
      E  : Element;
   begin
      if Id /= null then
         Editors_Hash.Get_First (Id.Editors, I);
         E := Editors_Hash.Get_Element (I);

         while E /= No_Element loop
            if E.Child = Child then
               Editors_Hash.Remove_And_Get_Next (Id.Editors, I);
            else
               Editors_Hash.Get_Next (Id.Editors, I);
            end if;

            E := Editors_Hash.Get_Element (I);
         end loop;
      end if;
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Editor_Destroy;

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
           (Get_MDI (Kernel), New_View (Kernel, Editor));
      else
         return MDI_Child (Child);
      end if;
   end Dnd_Data;

   ------------------------
   -- Toolbar_Destroy_Cb --
   ------------------------

   procedure Toolbar_Destroy_Cb
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      UR : Undo_Redo;
      pragma Unreferenced (Widget);
   begin
      UR := Undo_Redo_Data.Get (Kernel, Undo_Redo_Id);
      UR.Undo_Button_Handler_ID.Id := Null_Handler_Id;
      UR.Undo_Button := null;
      UR.Redo_Button := null;
      UR.Undo_Menu_Item := null;
      UR.Redo_Menu_Item := null;
   exception
      when E : others => Trace (Exception_Handle, E);
   end Toolbar_Destroy_Cb;

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

   function Get_Filename (Child : MDI_Child) return Virtual_File is
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

   --------------------
   -- File_Edited_Cb --
   --------------------

   procedure File_Edited_Cb
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      D : constant File_Hooks_Args := File_Hooks_Args (Data.all);
   begin
      Reset_Markers_For_File (Kernel, D.File);
   end File_Edited_Cb;

   -----------------------------
   -- File_Changed_On_Disk_Cb --
   -----------------------------

   procedure File_Changed_On_Disk_Cb
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      D     : constant File_Hooks_Args := File_Hooks_Args (Data.all);
      Iter  : Child_Iterator := First_Child (Get_MDI (Kernel));
      Child : MDI_Child;
      Box   : Source_Editor_Box;
   begin
      loop
         Child := Get (Iter);

         exit when Child = null;

         if Get_Widget (Child).all in Source_Editor_Box_Record'Class then
            Box := Source_Editor_Box (Get_Widget (Child));

            if D.File = GNATCOLL.VFS.No_File
              or else D.File = Get_Filename (Box)
            then
               Check_Timestamp_And_Reload
                 (Box,
                  Interactive   => False,
                  Always_Reload => False);

               Check_Writable (Box);
            end if;
         end if;

         Next (Iter);
      end loop;
   end File_Changed_On_Disk_Cb;

   ---------------------
   -- File_Renamed_Cb --
   ---------------------

   procedure File_Renamed_Cb
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      pragma Unreferenced (Kernel);
      Id : constant Source_Editor_Module :=
             Source_Editor_Module (Src_Editor_Module_Id);
      D  : constant Files_2_Hooks_Args := Files_2_Hooks_Args (Data.all);
      E  : Element;
   begin
      E := Editors_Hash.Get (Id.Editors, D.File);

      if E /= No_Element then
         Editors_Hash.Remove (Id.Editors, D.File);
         Editors_Hash.Set (Id.Editors, D.Renamed, E);
      end if;
   end File_Renamed_Cb;

   -------------------
   -- File_Saved_Cb --
   -------------------

   procedure File_Saved_Cb
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      Id    : constant Source_Editor_Module :=
                Source_Editor_Module (Src_Editor_Module_Id);
      D     : constant File_Hooks_Args := File_Hooks_Args (Data.all);
      Iter  : Child_Iterator := First_Child (Get_MDI (Kernel));
      Child : MDI_Child;
      Box   : Source_Editor_Box;

      I     : Editors_Hash.Cursor;
      E     : Element;
   begin
      --  Insert the saved file in the Recent menu

      if D.File /= GNATCOLL.VFS.No_File
        and then not Is_Auto_Save (D.File)
      then
         Add_To_Recent_Menu (Kernel, D.File);
      end if;

      loop
         Child := Get (Iter);

         exit when Child = null;

         if Get_Widget (Child).all in Source_Editor_Box_Record'Class then
            Box := Source_Editor_Box (Get_Widget (Child));

            if D.File = GNATCOLL.VFS.No_File
              or else D.File = Get_Filename (Box)
            then
               Editors_Hash.Get_First (Id.Editors, I);
               E := Editors_Hash.Get_Element (I);

               while E /= No_Element loop
                  if E.Child = Child then
                     Editors_Hash.Remove_And_Get_Next (Id.Editors, I);
                  else
                     Editors_Hash.Get_Next (Id.Editors, I);
                  end if;

                  E := Editors_Hash.Get_Element (I);
               end loop;

               Editors_Hash.Set (Id.Editors, D.File, (Child => Child));
            end if;
         end if;

         Next (Iter);
      end loop;
   exception
      when E : others => Trace (Exception_Handle, E);
   end File_Saved_Cb;

   -----------------------
   -- Cursor_Stopped_Cb --
   -----------------------

   procedure Cursor_Stopped_Cb
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      D   : constant File_Location_Hooks_Args_Access :=
              File_Location_Hooks_Args_Access (Data);
      Id  : constant Source_Editor_Module :=
              Source_Editor_Module (Src_Editor_Module_Id);
      Box : Source_Editor_Box;

   begin
      if Id.Show_Subprogram_Names then
         Box := Get_Source_Box_From_MDI (Find_Editor (Kernel, D.File));

         if Box /= null then
            Show_Subprogram_Name (Box, Get_Subprogram_Name (Box));
         end if;
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end Cursor_Stopped_Cb;

   ------------------------
   -- File_Edit_Callback --
   ------------------------

   function File_Edit_Callback (D : Location_Idle_Data) return Boolean is
   begin
      if Is_Valid_Position (Get_Buffer (D.Edit), D.Line) then
         Set_Cursor_Location (D.Edit, D.Line, D.Column, Force_Focus => False);

         if D.Column_End /= 0
           and then Is_Valid_Position
             (Get_Buffer (D.Edit), D.Line, D.Column_End)
         then
            Select_Region
              (Get_Buffer (D.Edit),
               D.Line,
               D.Column,
               D.Line,
               D.Column_End);
         end if;
      end if;

      File_Edited (Get_Kernel (D.Edit), Get_Filename (D.Edit));

      return False;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return False;
   end File_Edit_Callback;

   ------------------------------------
   -- Create_Files_Pixbufs_If_Needed --
   ------------------------------------

   procedure Create_Files_Pixbufs_If_Needed
     (Handle : access Kernel_Handle_Record'Class) is
   begin
      if File_Pixbuf = Null_Pixbuf then
         File_Pixbuf := Render_Icon
           (Get_Main_Window (Handle), "gps-file", Icon_Size_Menu);
      end if;

      if File_Modified_Pixbuf = Null_Pixbuf then
         File_Modified_Pixbuf := Render_Icon
           (Get_Main_Window (Handle), "gps-file-modified", Icon_Size_Menu);
      end if;

      if File_Unsaved_Pixbuf = Null_Pixbuf then
         File_Unsaved_Pixbuf := Render_Icon
           (Get_Main_Window (Handle), "gtk-new", Icon_Size_Menu);
      end if;
   end Create_Files_Pixbufs_If_Needed;

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
      Id          : Idle_Handler_Id;
      Line        : Editable_Line_Type := 1;
      Column      : Visible_Column_Type := 1;
      Real_Column : Character_Offset_Type;
      Child       : MDI_Child;
      pragma Unreferenced (Id, MDI);

      Dummy  : Boolean;
      pragma Unreferenced (Dummy);
   begin
      if Node.Tag.all = "Source_Editor" then
         Create_Files_Pixbufs_If_Needed (User);
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

            if not Is_Open (User, F) then
               Src := Open_File
                 (User, F, False,
                  Line   => Line,
                  Column => Column,
                  Column_End => Column);
               Child := Find_Editor (User, F);
            else
               Child := Find_Editor (User, F);
               declare
                  Edit  : constant Source_Editor_Box :=
                    Get_Source_Box_From_MDI (Child);
               begin
                  Src := New_View (User, Edit);
               end;
            end if;

            if Src /= null then
               Real_Column := Collapse_Tabs (Get_Buffer (Src), Line, Column);
               Dummy := File_Edit_Callback
                 ((Src, Line,
                   Real_Column, 0, User, False));
            end if;
         end if;
      end if;

      return Child;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return null;
   end Load_Desktop;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Hook   : Lines_Revealed_Hook_Record;
      Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Data   : access GPS.Kernel.Hooks.Hooks_Data'Class)
   is
      pragma Unreferenced (Hook);
      D : constant Context_Hooks_Args := Context_Hooks_Args (Data.all);
      Infos        : Line_Information_Data;
      Line1, Line2 : Integer;

   begin
      if Has_Area_Information (D.Context) then
         Get_Area (D.Context, Line1, Line2);

         --  ??? This is probably unnecessary if not Has_File_Information
         --  (Area_Context), see below.
         Infos := new Line_Information_Array (Line1 .. Line2);

         for J in Infos'Range loop
            Infos (J).Text := new String'(Image (J));
         end loop;

         if Has_File_Information (D.Context) then
            Add_Line_Information
              (Kernel,
               File_Information (D.Context),
               Src_Editor_Module_Name,
               Infos,
               Normalize => False);
         end if;

         Unchecked_Free (Infos);
      end if;
   end Execute;

   ------------------
   -- Save_Desktop --
   ------------------

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      User   : Kernel_Handle) return Node_Ptr
   is
      N, Child : Node_Ptr;
      Line     : Editable_Line_Type;
      Column   : Character_Offset_Type;
      Editor   : Source_Editor_Box;
      File     : Virtual_File;
      Pref     : Editor_Desktop_Policy;

   begin
      if Widget.all not in Source_Editor_Box_Record'Class then
         return null;
      end if;

      begin
         Pref := Save_Editor_Desktop.Get_Pref;
      exception
         when Constraint_Error =>
            Pref := From_Project;
      end;

      if Pref = Never then
         return null;
      end if;

      Editor := Source_Editor_Box (Widget);
      File   := Get_Filename (Editor);

      --  ??? For now, save with desktop only local files

      if not Is_Local (File) then
         return null;
      end if;

      if Pref = From_Project and then
        (Get_Registry (User).Tree.Status /= From_File
         or else Get_Registry (User).Tree.Info (File).Project = No_Project)
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
     (Kernel : access Kernel_Handle_Record'Class) return MDI_Child is
   begin
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

      Editors_Hash.Set
        (Id.Editors, Get_Filename (Box), (Child => MDI_Child (Child)));
   end Update_Cache_On_Focus;

   --------------
   -- New_View --
   --------------

   function New_View
     (Kernel  : access Kernel_Handle_Record'Class;
      Current : Source_Editor_Box) return Source_Editor_Box
   is
      Editor : Source_Editor_Box;
      Child  : GPS_MDI_Child;
      Num    : Natural;
   begin
      if Current = null then
         return null;
      end if;

      declare
         Title : constant Virtual_File := Get_Filename (Current);
      begin
         Create_New_View (Editor, Kernel, Current);

         Child := new Editor_Child_Record;
         Initialize
           (Child, Editor,
            Flags          => All_Buttons,
            Focus_Widget   => Gtk_Widget (Get_View (Editor)),
            Default_Width  => Gint (Default_Widget_Width.Get_Pref),
            Default_Height => Gint (Default_Widget_Height.Get_Pref),
            Module         => Src_Editor_Module_Id);

         --  Find the first free view number
         Num := 2;
         while Find_MDI_Child_By_Name
           (Get_MDI (Kernel),
            Display_Base_Name (Title) & " <" & Image (Num) & ">") /=
           null
         loop
            Num := Num + 1;
         end loop;

         declare
            Im : constant String := Image (Num);
         begin
            Set_Title
              (Child,
               Display_Full_Name (Title) & " <" & Im & ">",
               Display_Base_Name (Title) & " <" & Im & ">");
         end;

         Put (Get_MDI (Kernel), Child, Initial_Position => Position_Automatic);
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

   procedure New_View
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Current : constant Source_Editor_Box :=
                  Get_Source_Box_From_MDI (Find_Current_Editor (Kernel));
      Ignore  : Source_Editor_Box;
      pragma Unreferenced (Ignore);

   begin
      if Current /= null then
         Ignore := New_View (Kernel, Current);
      end if;
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
     (Kernel     : access Kernel_Handle_Record'Class;
      File       : GNATCOLL.VFS.Virtual_File;
      Dir        : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Create_New : Boolean := True) return Source_Editor_Box
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
         Gtk_New (Editor, Kernel_Handle (Kernel));
         Load_File (Editor, File,
                    Force_Focus => True,
                    Success     => Success);

         if not Success then
            Destroy (Editor);
            Editor := null;
         end if;

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
            Gtk_New (Editor, Kernel_Handle (Kernel));
            Load_Empty_File (Editor, File, Dir, Get_Language_Handler (Kernel));
         end if;
      end if;

      return Editor;
   end Create_File_Editor;

   ------------------------
   -- Add_To_Recent_Menu --
   ------------------------

   procedure Add_To_Recent_Menu
     (Kernel : access Kernel_Handle_Record'Class; File : Virtual_File) is
   begin
      Add_To_History (Kernel, Hist_Key, +Full_Name (File));
   end Add_To_Recent_Menu;

   ---------------
   -- Open_File --
   ---------------

   function Open_File
     (Kernel           : access Kernel_Handle_Record'Class;
      File             : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Create_New       : Boolean := True;
      Focus            : Boolean := True;
      Force            : Boolean := False;
      Line             : Editable_Line_Type;
      Column           : Visible_Column_Type;
      Column_End       : Visible_Column_Type;
      Group            : Gtkada.MDI.Child_Group := Gtkada.MDI.Group_Default;
      Initial_Position : Gtkada.MDI.Child_Position :=
        Gtkada.MDI.Position_Automatic;
      Initial_Dir      : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File)
      return Source_Editor_Box
   is
      Id      : constant Source_Editor_Module :=
                  Source_Editor_Module (Src_Editor_Module_Id);
      No_Name : constant String := -"Untitled";
      MDI     : constant MDI_Window := Get_MDI (Kernel);
      Editor  : Source_Editor_Box;
      Child   : GPS_MDI_Child;
      Child2  : MDI_Child;

      procedure Jump_To_Location;
      --  Jump to the location given in parameter to Open_File

      ----------------------
      -- Jump_To_Location --
      ----------------------

      procedure Jump_To_Location is
         Real_Column, Real_Column_End : Character_Offset_Type;
      begin
         if Line /= 0
           and then Is_Valid_Position (Get_Buffer (Editor), Line)
         then
            Real_Column := Collapse_Tabs
              (Get_Buffer (Editor), Line, Column);

            Set_Cursor_Location
              (Editor, Line, Real_Column, Focus,
               Centering => With_Margin);

            Set_Position_Set_Explicitely (Get_Buffer (Editor));

            if Column_End /= 0
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
         end if;
      end Jump_To_Location;

   begin
      Create_Files_Pixbufs_If_Needed (Kernel);

      if Active (Me) then
         Trace (Me, "Open file " & Display_Full_Name (File)
                & " Focus=" & Focus'Img);
      end if;

      if File /= GNATCOLL.VFS.No_File and then not File.Is_Directory then
         Child2 := Find_Editor (Kernel, File);

         if Child2 /= null then
            Check_Timestamp_And_Reload
              (Source_Editor_Box (Get_Widget (Child2)),
               Interactive   => not Force,
               Always_Reload => Force);

            Raise_Child (Child2, Focus);
            Editor := Source_Editor_Box (Get_Widget (Child2));

            Jump_To_Location;

            return Editor;
         end if;
      end if;

      Editor := Create_File_Editor (Kernel, File, Initial_Dir, Create_New);

      --  If we have created an editor, put it into a box, and give it
      --  to the MDI to handle

      if Editor /= null then
         Child := new Editor_Child_Record;
         Initialize
           (Child, Editor,
            Flags          => All_Buttons,
            Focus_Widget   => Gtk_Widget (Get_View (Editor)),
            Group          => Group,
            Default_Width  => Gint (Default_Widget_Width.Get_Pref),
            Default_Height => Gint (Default_Widget_Height.Get_Pref),
            Module         => Src_Editor_Module_Id);
         Put (Get_MDI (Kernel), Child, Initial_Position => Initial_Position);
         Set_Child (Get_View (Editor), Child);

         --  ??? Consider enabling this code
         --  if MDI_Editors_Floating.Get_Pref then
         --     Float_Child (Child, True);
         --  end if;

         Check_Writable (Editor);

         if Get_Status (Get_Buffer (Editor)) = Modified then
            if File_Modified_Pixbuf /= null then
               Set_Icon (Child, File_Modified_Pixbuf);
               Ref (File_Modified_Pixbuf);
            end if;

         elsif Get_Status (Get_Buffer (Editor)) = Unsaved then
            if File_Unsaved_Pixbuf /= null then
               Set_Icon (Child, File_Unsaved_Pixbuf);
               Ref (File_Unsaved_Pixbuf);
            end if;

         else
            if File_Pixbuf /= null then
               Set_Icon (Child, File_Pixbuf);
               Ref (File_Pixbuf);
            end if;
         end if;

         Widget_Callback.Connect
           (Child, Signal_Selected, Update_Cache_On_Focus'Access);

         --  Add child to the hash table of editors

         Editors_Hash.Set (Id.Editors, File, (Child => MDI_Child (Child)));

         --  Make sure the entry in the hash table is removed when the editor
         --  is destroyed.

         Widget_Callback.Connect
           (Child, Signal_Destroy, On_Editor_Destroy'Access);

         Raise_Child (Child, Focus);

         Jump_To_Location;

         if File /= GNATCOLL.VFS.No_File and then not File.Is_Directory then
            if Is_Local (File) then
               Set_Title
                 (Child, Display_Full_Name (File), Display_Base_Name (File));
            else
               Set_Title
                 (Child,
                  Get_Host (File) & ":|" & Display_Full_Name (File),
                  Display_Base_Name (File));
            end if;

            File_Edited (Kernel, Get_Filename (MDI_Child (Child)));

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
                  Set_Title (Child, No_Name);
                  Ident := Create (+('/' & No_Name));

               else
                  declare
                     Identifier : constant String :=
                                    No_Name & " ("
                                      & Image (Nb_Untitled + 1) & ")";
                  begin
                     Set_Title (Child, Identifier);
                     Ident := Create (+('/' & Identifier));
                  end;
               end if;

               Set_File_Identifier (Get_Buffer (Editor), Ident);
               Set_Filename
                 (Get_Buffer (Editor), Get_Filename (MDI_Child (Child)));
               File_Edited (Kernel, Ident, Force_Hook => True);
            end;
         end if;

         if File /= GNATCOLL.VFS.No_File then
            Add_To_Recent_Menu (Kernel, File);
         end if;

      else
         Console.Insert
           (Kernel, (-"Cannot open file ") & "'" & Display_Full_Name (File)
            & "'",
            Add_LF => True,
            Mode   => Error);
      end if;

      return Editor;
   end Open_File;

   ------------------
   -- Save_To_File --
   ------------------

   procedure Save_To_File
     (Kernel  : access Kernel_Handle_Record'Class;
      Name    : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Success : out Boolean)
   is
      Child  : constant MDI_Child := Find_Current_Editor (Kernel);
      Source : Source_Editor_Box;
   begin
      if Child = null then
         Success := False;
         return;
      end if;

      Source := Source_Editor_Box (Get_Widget (Child));
      Save_To_File (Source, Name, Success);
   end Save_To_File;

   ------------------
   -- On_Open_File --
   ------------------

   procedure On_Open_File
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Context : Selection_Context;
      Dir     : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
   begin
      Context := Get_Current_Context (Kernel);

      if Has_Directory_Information (Context) then
         Dir := Directory_Information (Context);
      end if;

      declare
         Filename : constant Virtual_File := Select_File
           (Title             => -"Open File",
            Base_Directory    => Dir,
            Parent            => Get_Current_Window (Kernel),
            Use_Native_Dialog => Use_Native_Dialogs.Get_Pref,
            Kind              => Open_File,
            File_Pattern      => "*;*.ad?;{*.c,*.h,*.cpp,*.cc,*.C}",
            Pattern_Name      => -"All files;Ada files;C/C++ files",
            History           => Get_History (Kernel));
      begin
         if Filename /= GNATCOLL.VFS.No_File then
            Open_File_Editor (Kernel, Filename);
         end if;
      end;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Open_File;

   -------------------------
   -- On_Open_Remote_File --
   -------------------------

   procedure On_Open_Remote_File
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      declare
         Filename : constant Virtual_File := Select_File
           (Title             => -"Open Remote File",
            Parent            => Get_Current_Window (Kernel),
            Remote_Browsing   => True,
            Use_Native_Dialog => False,
            Kind              => Open_File,
            File_Pattern      => "*;*.ad?;{*.c,*.h,*.cpp,*.cc,*.C}",
            Pattern_Name      => -"All files;Ada files;C/C++ files",
            History           => Get_History (Kernel));

      begin
         if Filename /= GNATCOLL.VFS.No_File then
            Open_File_Editor (Kernel, Filename);
         end if;
      end;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Open_Remote_File;

   ----------------
   -- Completion --
   ----------------

   overriding function Completion
     (Factory : File_Completion_Factory; Index : Positive) return String
   is
      File : GNATCOLL.VFS.Virtual_File;
   begin
      if Index > Factory.File1'Length then
         if Index - Factory.File1'Length > Factory.File2'Length then
            return "";
         else
            File := Factory.File2
              (Factory.File2'First + Index - Factory.File1'Length - 1);
         end if;
      else
         File := Factory.File1 (Factory.File1'First + Index - 1);
      end if;

      if File = GNATCOLL.VFS.No_File then
         return "@$#$";  --  Unlikely string, will not match any suffix
      else
         return +Base_Name (File);
      end if;
   end Completion;

   -----------------
   -- Description --
   -----------------

   overriding function Description
     (Factory : File_Completion_Factory; Index : Positive) return String
   is
      File : GNATCOLL.VFS.Virtual_File;
   begin
      if Index > Factory.File1'Length then
         if Index - Factory.File1'Length > Factory.File2'Length then
            return "";
         else
            File := Factory.File2
              (Factory.File2'First + Index - Factory.File1'Length - 1);
         end if;
      else
         File := Factory.File1 (Factory.File1'First + Index - 1);
      end if;

      if File = GNATCOLL.VFS.No_File then
         return "";
      else
         return +Full_Name (File);
      end if;
   end Description;

   -----------------------
   -- On_Open_From_Path --
   -----------------------

   procedure On_Open_From_Path
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Label  : Gtk_Label;
      Ignore : Gtk_Widget;
      pragma Unreferenced (Widget, Ignore);

      Open_File_Dialog : Gtk_Dialog;
      Open_File_Entry  : Gtkada_Entry;
      Hist             : constant String_List_Access :=
                           Get_History
                             (Get_History (Kernel).all,
                              Open_From_Path_History);
      List1            : File_Array_Access :=
                           Get_Project (Kernel).Source_Files
                             (Recursive => True);
      List2            : File_Array_Access := new File_Array'
                           (Get_Registry (Kernel).Environment
                            .Predefined_Source_Files);
      Compl            : File_Completion_Factory;

   begin
      Push_State (Kernel,  Busy);
      Gtk_New (Open_File_Dialog,
               Title  => -"Open file from project",
               Parent => Get_Current_Window (Kernel),
               Flags  => Modal or Destroy_With_Parent);
      Set_Default_Size (Open_File_Dialog, 600, 400);
      Set_Position (Open_File_Dialog, Win_Pos_Mouse);

      Gtk_New (Label, -"Enter file name (use <tab> for completion):");
      Pack_Start (Get_Vbox (Open_File_Dialog), Label, Expand => False);

      --  Do not use a combo box, so that users can easily navigate to the list
      --  of completions through the keyboard (C423-005)
      Gtk_New (Open_File_Entry,
               Case_Sensitive =>
                 Is_Case_Sensitive (Get_Nickname (Build_Server)));
      Set_Activates_Default (Get_Entry (Open_File_Entry), True);
      Pack_Start (Get_Vbox (Open_File_Dialog), Open_File_Entry,
                  Fill => True, Expand => True);

      if Hist /= null then
         Set_Text (Get_Entry (Open_File_Entry),
                   Base_Name (Hist (Hist'First).all));
         Select_Region (Get_Entry (Open_File_Entry), 0, -1);
      end if;

      Ignore := Add_Button (Open_File_Dialog, Stock_Ok, Gtk_Response_OK);
      Ignore := Add_Button
        (Open_File_Dialog, Stock_Cancel, Gtk_Response_Cancel);
      Set_Default_Response (Open_File_Dialog, Gtk_Response_OK);

      Grab_Focus (Get_Entry (Open_File_Entry));
      Show_All (Open_File_Dialog);

      Compl.File1 := List1;
      Compl.File2 := List2;

      Set_Completions (Open_File_Entry, Compl);

      Pop_State (Kernel);

      if Run (Open_File_Dialog) = Gtk_Response_OK then

         --  Look for the file in the project. If the file cannot be found,
         --  display an error message in the console.

         declare
            Complet : constant Integer :=
                        Current_Completion (Open_File_Entry);
            Text    : constant String :=
                        Get_Text (Get_Entry (Open_File_Entry));
            --  ??? What if the filesystem path is non-UTF8?
            Full    : Virtual_File;
         begin
            --  Close the dialog first, so that the current context becomes the
            --  one of the editor while running hooks
            Destroy (Open_File_Dialog);

            if Complet /= 0 then
               if Complet > List1'Length then
                  Full := List2 (List2'First + Complet - List1'Length - 1);
               else
                  Full := List1 (List1'First + Complet - 1);
               end if;
            else
               Full := Create (+Text, Kernel, Use_Object_Path => False);
            end if;

            Add_To_History
              (Get_History (Kernel).all, Open_From_Path_History,
               +Full_Name (Full));

            if Is_Regular_File (Full) then
               Open_File_Editor
                 (Kernel, Full,
                  Enable_Navigation => True,
                  New_File          => False,
                  Line              => 0,
                  Column            => 0);

            else
               Insert
                 (Kernel,
                    -"Could not find source file """ & Text &
                      (-""" in currently loaded project."),
                  Mode => Error);
            end if;
         end;
      else
         Destroy (Open_File_Dialog);
      end if;

      Unchecked_Free (List1);
      Unchecked_Free (List2);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Open_From_Path;

   --------------
   -- Activate --
   --------------

   overriding procedure Activate
     (Callback : access On_Recent; Item : String) is
   begin
      Open_File_Editor (Callback.Kernel, Create (Full_Filename => +Item));

   exception
      when E : others => Trace (Exception_Handle, E);
   end Activate;

   -----------------
   -- On_New_File --
   -----------------

   procedure On_New_File
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Context : constant Selection_Context := Get_Current_Context (Kernel);
      Ignore  : Source_Editor_Box;
      pragma Unreferenced (Widget, Ignore);
      Dir     : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;

   begin
      if Has_Directory_Information (Context) then
         Dir := Directory_Information (Context);
      end if;

      Ignore := Open_File
        (Kernel,
         File => GNATCOLL.VFS.No_File,
         Line => 1, Column => 1, Column_End => 1,
         Initial_Dir => Dir);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_New_File;

   -------------
   -- On_Save --
   -------------

   procedure On_Save
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Success : Boolean;
      pragma Warnings (Off, Success);
   begin
      Save_To_File (Kernel, Success => Success);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Save;

   ----------------
   -- On_Save_As --
   ----------------

   procedure On_Save_As
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Success : Boolean;
      Source  : constant Source_Editor_Box :=
                  Get_Source_Box_From_MDI (Find_Current_Editor (Kernel));
   begin
      if Source /= null then
         declare
            Old_Name : constant Virtual_File := Get_Filename (Source);
            New_Name : constant Virtual_File := Select_File
              (Title             => -"Save File As",
               Parent            => Get_Current_Window (Kernel),
               Base_Directory    => Dir (Old_Name),
               Default_Name      => Base_Name (Old_Name),
               Remote_Browsing   => not Is_Local (Old_Name),
               Use_Native_Dialog => Use_Native_Dialogs.Get_Pref,
               Kind              => Save_File,
               File_Pattern      => "*;*.ad?;{*.c,*.h,*.cpp,*.cc,*.C}",
               Pattern_Name      => -"All files;Ada files;C/C++ files",
               History           => Get_History (Kernel));

         begin
            if New_Name /= GNATCOLL.VFS.No_File then
               Save_To_File (Kernel, New_Name, Success);
            end if;
         end;
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Save_As;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Close_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Context);
      MDI   : MDI_Window;
      Child : MDI_Child;

   begin
      if Command.Close_All then
         if Save_MDI_Children (Command.Kernel) then
            Close_All_Children (Command.Kernel);
         end if;
      else
         MDI := Get_MDI (Command.Kernel);
         Child := Get_Focus_Child (MDI);

         if Child /= null then
            Close (MDI, Get_Widget (Child));
         end if;
      end if;

      return Commands.Success;
   end Execute;

   --------------
   -- On_Print --
   --------------

   procedure On_Print
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Child            : constant MDI_Child := Find_Current_Editor (Kernel);
      Source           : Source_Editor_Box;
   begin
      if Get_Focus_Child (Get_MDI (Kernel)) /= Child then
         Console.Insert (Kernel, "No source file selected", Mode => Error);
         return;
      end if;

      Source := Get_Source_Box_From_MDI (Child);

      if Source = null then
         return;
      end if;

      Src_Printing.Fabric.Create.Print (Source);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Print;

   ------------------------
   -- On_Print_Selection --
   ------------------------

   procedure On_Print_Selection
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Child            : constant MDI_Child := Find_Current_Editor (Kernel);
      Source           : Source_Editor_Box;

      Context    : constant Selection_Context := Get_Current_Context (Kernel);
      Start_Line : Editable_Line_Type;
      End_Line   : Editable_Line_Type;
   begin
      if Has_Area_Information (Context) then
         Get_Area (Context, Natural (Start_Line), Natural (End_Line));
      else
         Console.Insert (Kernel, "No selection", Mode => Error);
         return;
      end if;

      if Get_Focus_Child (Get_MDI (Kernel)) /= Child then
         Console.Insert (Kernel, "No source file selected", Mode => Error);
         return;
      end if;

      Source := Get_Source_Box_From_MDI (Child);

      if Source = null then
         return;
      end if;

      Src_Printing.Fabric.Create.Print (Source, Start_Line, End_Line);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Print_Selection;

   -------------------
   -- On_Select_All --
   -------------------

   procedure On_Select_All
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Source : constant Source_Editor_Box :=
                 Get_Source_Box_From_MDI (Find_Current_Editor (Kernel));
   begin
      if Source /= null then
         Select_All (Get_Buffer (Source));
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Select_All;

   --------------------
   -- On_Insert_File --
   --------------------

   procedure On_Insert_File
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
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

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Insert_File;

   -----------------
   -- On_New_View --
   -----------------

   procedure On_New_View
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      New_View (Kernel);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_New_View;

   -------------------------
   -- On_Goto_Declaration --
   -------------------------

   procedure On_Goto_Declaration
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Editor : constant Source_Editor_Box :=
                 Get_Source_Box_From_MDI (Find_Current_Editor (Kernel));
   begin
      if Editor = null then
         return;
      end if;

      Goto_Declaration_Or_Body
        (Kernel,
         To_Body => False,
         Editor  => Editor,
         Context => Get_Current_Context (Kernel));

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Goto_Declaration;

   ------------------
   -- On_Goto_Body --
   ------------------

   procedure On_Goto_Body
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Editor : constant Source_Editor_Box :=
                 Get_Source_Box_From_MDI (Find_Current_Editor (Kernel));
   begin
      if Editor = null then
         return;
      end if;

      Goto_Declaration_Or_Body
        (Kernel,
         To_Body => True,
         Editor  => Editor,
         Context => Get_Current_Context (Kernel));

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Goto_Body;

   -----------------------
   -- Comment_Uncomment --
   -----------------------

   procedure Comment_Uncomment
     (Kernel : Kernel_Handle; Comment : Boolean)
   is
      Context    : constant Selection_Context := Get_Current_Context (Kernel);
      Start_Line : Editable_Line_Type;
      End_Line   : Editable_Line_Type;
      Buffer     : Source_Buffer;
   begin
      if Has_File_Information (Context)
        and then Has_Directory_Information (Context)
      then
         declare
            Lang  : Language_Access;
            File  : constant Virtual_File := File_Information (Context);
            Block : Unbounded_String := Null_Unbounded_String;
         begin
            Buffer := Get_Buffer
              (Get_Source_Box_From_MDI (Find_Editor (Kernel, File)));

            if not Get_Writable (Buffer) then
               return;
            end if;

            if Has_Area_Information (Context) then
               Get_Area (Context, Natural (Start_Line), Natural (End_Line));

            elsif Has_Line_Information (Context) then
               Start_Line := Editable_Line_Type
                 (Contexts.Line_Information (Context));
               End_Line   := Start_Line;
            else
               return;
            end if;

            Lang := Get_Language_From_File
              (Get_Language_Handler (Kernel), File);

            --  Create a String representing the selected block

            for J in Start_Line .. End_Line loop
               Append
                 (Block,
                  Get_Chars (Buffer => Buffer, Line => J, Column => 1));
            end loop;

            Replace_Slice
              (Buffer,
               Text   => Comment_Block (Lang, To_String (Block), Comment),
               Line   => Start_Line,
               Column => 1,
               Before => 0,
               After  => Length (Block));
         end;
      end if;
   end Comment_Uncomment;

   ----------------------
   -- On_Comment_Lines --
   ----------------------

   procedure On_Comment_Lines
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      Comment_Uncomment (Kernel, Comment => True);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Comment_Lines;

   --------------------
   -- On_Fold_Blocks --
   --------------------

   procedure On_Fold_Blocks
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Current : constant Source_Editor_Box :=
                  Get_Source_Box_From_MDI (Find_Current_Editor (Kernel));
   begin
      if Current /= null then
         Src_Editor_Buffer.Line_Information.Fold_All (Get_Buffer (Current));
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Fold_Blocks;

   ----------------------
   -- On_Unfold_Blocks --
   ----------------------

   procedure On_Unfold_Blocks
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Current : constant Source_Editor_Box :=
                  Get_Source_Box_From_MDI (Find_Current_Editor (Kernel));
   begin
      if Current /= null then
         Src_Editor_Buffer.Line_Information.Unfold_All (Get_Buffer (Current));
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Unfold_Blocks;

   ------------------------
   -- On_Uncomment_Lines --
   ------------------------

   procedure On_Uncomment_Lines
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

   begin
      Comment_Uncomment (Kernel, Comment => False);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Uncomment_Lines;

   ---------------
   -- On_Refill --
   ---------------

   procedure On_Refill
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Current : constant Source_Editor_Box :=
                  Get_Source_Box_From_MDI (Find_Current_Editor (Kernel));
      Ignore  : Boolean;
      pragma Unreferenced (Ignore);
   begin
      if Current /= null then
         Ignore := Do_Refill (Get_Buffer (Current));
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Refill;

   ----------------------
   -- Source_File_Hook --
   ----------------------

   function Source_File_Hook
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class) return Boolean
   is
      D      : constant Source_File_Hooks_Args :=
                 Source_File_Hooks_Args (Data.all);
      Child  : MDI_Child;
      Source : Source_Editor_Box;
      Edit   : Source_Editor_Box;
      Tmp    : Boolean;
      pragma Unreferenced (Tmp);

   begin
      if D.Line = -1 then
         --  Close all file editors corresponding to File

         loop
            Child := Find_Editor (Kernel, D.File);

            exit when Child = null;

            Close_Child (Child);
         end loop;

         return True;

      else
         Source := Open_File
           (Kernel, D.File,
            Create_New       => D.New_File,
            Focus            => D.Focus,
            Force            => D.Force_Reload,
            Group            => D.Group,
            Initial_Position => D.Initial_Position,
            Line             => Editable_Line_Type (D.Line),
            Column           => D.Column,
            Column_End       => D.Column_End);

         --  This used to be done in Open_File_Editor itself, before we call
         --  the Hook, but then we wouldn't have access to Create_File_Marker.
         --  Another module that deals with this hook would likely want its own
         --  type of Marker anyway...
         if D.Enable_Navigation then
            Push_Marker_In_History
              (Kernel => Kernel,
               Marker => Create_File_Marker
                 (Kernel => Kernel,
                  File   => D.File,
                  Line   => Convert (D.Line),
                  Column => D.Column));
         end if;

         if Source /= null then
            Edit := Source;
         end if;

         if D.Title /= "" then
            Child := Find_Editor (Kernel, D.File);
            Set_Title (Child, Title => D.Title, Short_Title => D.Title);
         end if;

         return Edit /= null;
      end if;
   end Source_File_Hook;

   --------------------
   -- File_Line_Hook --
   --------------------

   function File_Line_Hook
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class) return Boolean
   is
      D     : constant File_Line_Hooks_Args := File_Line_Hooks_Args (Data.all);
      Child : constant MDI_Child := Find_Editor (Kernel, D.File);
   begin
      if Child /= null then
         if D.Info'First = 0 then
            Create_Line_Information_Column
              (Source_Editor_Box (Get_Widget (Child)).Get_Buffer,
               D.Identifier,
               D.Every_Line,
               D.Info (0));

         elsif D.Info'Length = 0 then
            Remove_Line_Information_Column
              (Source_Editor_Box (Get_Widget (Child)), D.Identifier);
         else
            if D.Info'Last < 0 then
               --  This how the hook data encodes extra information

               Add_Extra_Information
                 (Get_Buffer
                    (Source_Editor_Box
                       (Get_Widget (Child))), D.Identifier, D.Info);
            else
               --  ??? Source duplicated in src_editor_buffer-line_information
               --  (Add_Blank_Lines)

               Add_Side_Information
                 (Source_Editor_Box (Get_Widget (Child)).Get_Buffer,
                  D.Identifier, D.Info.all, 0);
            end if;
         end if;

         return True;
      end if;

      return False;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return False;
   end File_Line_Hook;

   -------------------------------
   -- User_Character_Added_Hook --
   -------------------------------

   procedure User_Character_Added_Hook
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      File_Data : constant File_Edition_Hooks_Args :=
                    File_Edition_Hooks_Args (Data.all);
   begin
      if File_Data.File = GNATCOLL.VFS.No_File
        or else not File_Data.Interactive
      then
         return;
      end if;

      if Is_Alnum (File_Data.Character)
        or else File_Data.Character = Underscore
        or else File_Data.Character = Backspace
      then
         declare
            Box    : constant Source_Editor_Box :=
                       Get_Source_Box_From_MDI
                         (Find_Editor (Kernel, File_Data.File));
            Buffer : constant Source_Buffer := Get_Buffer (Box);
         begin
            if Is_Alnum (File_Data.Character) then
               Add_Typed_Char (Buffer, File_Data.Character);
            elsif File_Data.Character = Backspace then
               Delete_Last_Typed_Char (Buffer);
            end if;

            if Get_View (Box).As_Is_Enabled then
               Get_View (Box).Reset_As_Is_Mode;
            else
               Autocase_Text (Get_Buffer (Box), Casing => On_The_Fly);
            end if;
         end;

      elsif File_Data.Character = Space then
         Get_View
           (Get_Source_Box_From_MDI
              (Find_Editor (Kernel, File_Data.File))).Reset_As_Is_Mode;
      end if;
   end User_Character_Added_Hook;

   ---------------------
   -- Word_Added_Hook --
   ---------------------

   procedure Word_Added_Hook
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      File_Data : constant File_Hooks_Args := File_Hooks_Args (Data.all);
      Buffer    : Source_Buffer;
   begin
      if File_Data.File = GNATCOLL.VFS.No_File then
         return;
      end if;

      Buffer := Get_Buffer
        (Get_Source_Box_From_MDI (Find_Editor (Kernel, File_Data.File)));
      Autocase_Text (Buffer, Casing => End_Of_Word);
   end Word_Added_Hook;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Edit_File_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Line : Natural;
   begin
      Trace (Me, "On_Edit_File: "
             & (+Full_Name (File_Information (Context.Context))));

      if Has_Line_Information (Context.Context) then
         Line := Contexts.Line_Information (Context.Context);
      else
         Line := 1;
      end if;

      Open_File_Editor
        (Get_Kernel (Context.Context),
         Filename  => File_Information (Context.Context),
         Line      => Line,
         Column    => Column_Information (Context.Context));
      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Editor_Properties_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      File   : constant GNATCOLL.VFS.Virtual_File :=
        File_Information (Context.Context);
      Kernel  : constant Kernel_Handle := Get_Kernel (Context.Context);
      Dialog  : Gtk_Dialog;
      Label   : Gtk_Label;
      Lang    : Gtk_Combo_Box;
      Charset : Gtk_Combo_Box;
      Strip   : Gtk_Check_Button;
      Strip_Lines : Gtk_Check_Button;
      Box     : Gtk_Box;
      Size    : Gtk_Size_Group;
      Buffer  : Source_Buffer;
      Ignore  : Gtk_Widget;
      pragma Unreferenced (Ignore);

   begin
      Buffer := Get_Buffer
        (Get_Source_Box_From_MDI (Get_Focus_Child (Get_MDI (Kernel))));
      if Buffer = null then
         return Failure;
      end if;

      Gtk_New (Dialog,
               Title  => -"Properties for " & Display_Full_Name (File),
               Parent => Get_Main_Window (Kernel),
               Flags  => Destroy_With_Parent);
      Set_Default_Size (Dialog, 400, 200);

      Gtk_New (Size);

      --  Base name

      Gtk_New_Hbox (Box, Homogeneous => False);
      Pack_Start (Get_Vbox (Dialog), Box, Expand => True);
      Gtk_New (Label, -"File:");
      Set_Alignment (Label, 0.0, 0.5);
      Add_Widget (Size, Label);
      Pack_Start (Box, Label, Expand => False);
      Gtk_New (Label, Display_Base_Name (File));
      Set_Alignment (Label, 0.0, 0.5);
      Pack_Start (Box, Label, Expand => False);

      --  Directory

      Gtk_New_Hbox (Box, Homogeneous => False);
      Pack_Start (Get_Vbox (Dialog), Box, Expand => True);
      Gtk_New (Label, -"Directory:");
      Set_Alignment (Label, 0.0, 0.5);
      Add_Widget (Size, Label);
      Pack_Start (Box, Label, Expand => False);
      Gtk_New (Label, Unknown_To_UTF8 (+Dir_Name (File)));
      Set_Alignment (Label, 0.0, 0.5);
      Pack_Start (Box, Label, Expand => False);

      --  Language

      Gtk_New_Hbox (Box, Homogeneous => False);
      Pack_Start (Get_Vbox (Dialog), Box, Expand => True);

      Gtk_New (Label, -"Language: ");
      Set_Alignment (Label, 0.0, 0.5);
      Add_Widget (Size, Label);
      Pack_Start (Box, Label, Expand => False);

      Lang := Create_Language_Combo
        (Get_Language_Handler (Kernel), File,
         Default => Get_Name (Get_Language (Buffer)));
      Pack_Start (Box, Lang, Expand => True, Fill => True);

      --  Charset

      Gtk_New_Hbox (Box, Homogeneous => False);
      Pack_Start (Get_Vbox (Dialog), Box, Expand => True);

      Gtk_New (Label, -"Character set: ");
      Set_Alignment (Label, 0.0, 0.5);
      Add_Widget (Size, Label);
      Pack_Start (Box, Label, Expand => False);

      Charset := Create_Charset_Combo
        (File, Default => Get_Charset (Buffer));
      Pack_Start (Box, Charset, Expand => True, Fill => True);

      --  Trailing spaces

      Gtk_New_Hbox (Box, Homogeneous => False);
      Pack_Start (Get_Vbox (Dialog), Box, Expand => True);

      Gtk_New (Label, -"Strip blanks: ");
      Set_Alignment (Label, 0.0, 0.5);
      Add_Widget (Size, Label);
      Pack_Start (Box, Label, Expand => False);

      Gtk_New (Strip, "");
      Strip.Set_Active (Get_Strip_Trailing_Blanks (Buffer));
      Pack_Start (Box, Strip, Expand => True, Fill => True);

      --  Trailing blank lines

      Gtk_New_Hbox (Box, Homogeneous => False);
      Pack_Start (Get_Vbox (Dialog), Box, Expand => True);

      Gtk_New (Label, -"Strip lines: ");
      Set_Alignment (Label, 0.0, 0.5);
      Add_Widget (Size, Label);
      Pack_Start (Box, Label, Expand => False);

      Gtk_New (Strip_Lines, "");
      Strip_Lines.Set_Active (Get_Strip_Trailing_Lines (Buffer));
      Pack_Start (Box, Strip_Lines, Expand => True, Fill => True);

      Ignore := Add_Button (Dialog, Stock_Ok, Gtk_Response_OK);
      Grab_Default (Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel));

      Show_All (Dialog);

      if Run (Dialog) = Gtk_Response_OK then
         declare
            Text   : constant String := Get_Active_Text (Lang);
            Header : constant String := -"(From project) ";
            Index  : Natural := Text'First;
         begin
            if Text'Length >= Header'Length
              and then Text (Index .. Index + Header'Length - 1) = Header
            then
               Index := Index + Header'Length;
            end if;

            Set_Language
              (Buffer, Get_Language_By_Name
                 (Get_Language_Handler (Kernel),
                  Text (Index .. Text'Last)));
            Set_Charset (Buffer, Selected_Charset (Charset));

            Set_Strip_Trailing_Blanks (Buffer, Strip.Get_Active);
            Set_Strip_Trailing_Lines (Buffer, Strip_Lines.Get_Active);
         end;
      end if;

      Destroy (Dialog);
      return Success;
   end Execute;

   -----------------------------
   -- Default_Context_Factory --
   -----------------------------

   overriding procedure Default_Context_Factory
     (Module  : access Source_Editor_Module_Record;
      Context : in out Selection_Context;
      Child   : Glib.Object.GObject) is
   begin
      Get_Contextual_Menu
        (Context, Get_Kernel (Module.all),
         Source_Editor_Box (Child),
         Location => Location_Cursor);
   end Default_Context_Factory;

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
               Prj := Get_Registry (Kernel).Tree.Info
                 (Get_Filename (Box)).Project;
               if Prj = No_Project then
                  Prj := Get_Registry (Kernel).Tree.Root_Project;
               end if;

               return Expansion & Prj.Name;

            when 'P' =>
               Prj := Get_Registry (Kernel).Tree.Info
                 (Get_Filename (Box)).Project;
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
      Load   : XML_Utils.Node_Ptr := null) return Location_Marker is
   begin
      return Src_Editor_Module.Markers.Load (Get_Kernel (Module.all), Load);
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

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      File                     : constant String := '/' & (-"File") & '/';
      Edit                     : constant String := '/' & (-"Edit") & '/';
      Selection                : constant String :=
                                   Edit & (-"S_election") & '/';
      Navigate                 : constant String := '/' & (-"Navigate") & '/';
      Sep                      : Gtk_Separator_Menu_Item;
      Toolbar                  : constant Gtk_Toolbar := Get_Toolbar (Kernel);
      UR                       : constant Undo_Redo :=
                                   new Undo_Redo_Information;
      Selector                 : Scope_Selector;
      Extra                    : Files_Extra_Scope;
      Recent_Menu_Item         : Gtk_Menu_Item;
      Command                  : Interactive_Command_Access;
      Filter                   : Action_Filter;
      Label                    : Contextual_Menu_Label_Creator;
      Line_Numbers_Area_Filter : Action_Filter;
      Submenu                  : Submenu_Factory;

      Has_Type                 : constant Action_Filter := new Has_Type_Filter;
      Is_Access                : constant Action_Filter :=
                                   new Is_Access_Type_Filter;
      Is_Dispatching           : constant Action_Filter :=
                                   new Is_Dispatching_Filter;
      Src_Action_Context       : constant Action_Filter :=
                                   new Src_Editor_Action_Context;
      --  Memory is never freed, but this is needed for the whole life of
      --  the application.

   begin
      Src_Editor_Module_Id := new Source_Editor_Module_Record;
      Register_Filter (Kernel, Src_Action_Context, "Source editor");

      --  Commands

      Command := new Move_Command;
      Move_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Move_Command (Command.all).Kind := Word;
      Move_Command (Command.all).Step := 1;
      Register_Action
        (Kernel, "Move to next word", Command,
           -"Move to the next word in the current source editor",
         Category => "Editor",
         Filter   => Src_Action_Context);

      Command := new Move_Command;
      Move_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Move_Command (Command.all).Kind := Word;
      Move_Command (Command.all).Step := -1;
      Register_Action
        (Kernel, "Move to previous word", Command,
           -"Move to the previous word in the current source editor",
         Category => "Editor",
         Filter   => Src_Action_Context);

      Command := new Move_Command;
      Move_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Move_Command (Command.all).Kind := Line;
      Move_Command (Command.all).Step := 1;
      Register_Action
        (Kernel, "Move to next line", Command,
           -"Move to the next line in the current source editor",
         Category => "Editor",
         Filter   => Src_Action_Context);

      Command := new Move_Command;
      Move_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Move_Command (Command.all).Kind := Line;
      Move_Command (Command.all).Step := -1;
      Register_Action
        (Kernel, "Move to previous line", Command,
           -"Move to the previous line in the current source editor",
         Category => "Editor",
         Filter   => Src_Action_Context);

      Command := new Move_Command;
      Move_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Move_Command (Command.all).Kind := Char;
      Move_Command (Command.all).Step := 1;
      Register_Action
        (Kernel, "Move to next character", Command,
           -"Move to the next character in the current source editor",
         Category => "Editor",
         Filter   => Src_Action_Context);

      Command := new Move_Command;
      Move_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Move_Command (Command.all).Kind := Char;
      Move_Command (Command.all).Step := -1;
      Register_Action
        (Kernel, "Move to previous character", Command,
           -"Move to the previous character in the current source editor",
         Category => "Editor",
         Filter   => Src_Action_Context);

      Command := new Move_Command;
      Move_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Move_Command (Command.all).Kind := Paragraph;
      Move_Command (Command.all).Step := -1;
      Register_Action
        (Kernel, "Move to previous sentence", Command,
           -"Move to the previous sentence in the current source editor",
         Category => "Editor",
         Filter   => Src_Action_Context);

      Command := new Move_Command;
      Move_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Move_Command (Command.all).Kind := Paragraph;
      Move_Command (Command.all).Step := 1;
      Register_Action
        (Kernel, "Move to next sentence", Command,
           -"Move to the next sentence in the current source editor",
         Category => "Editor",
         Filter   => Src_Action_Context);

      Command := new Move_Command;
      Move_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Move_Command (Command.all).Kind := Page;
      Move_Command (Command.all).Step := -1;
      Register_Action
        (Kernel, "Move to previous page", Command,
           -"Move to the previous page in the current source editor",
         Category => "Editor",
         Filter   => Src_Action_Context);

      Command := new Move_Command;
      Move_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Move_Command (Command.all).Kind := Page;
      Move_Command (Command.all).Step := 1;
      Register_Action
        (Kernel, "Move to next page", Command,
           -"Move to the next page in the current source editor",
         Category => "Editor",
         Filter   => Src_Action_Context);

      Command := new Scroll_Command;
      Scroll_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Register_Action
        (Kernel, "Center cursor on screen", Command,
           -"Scroll the current source editor so that the cursor is centered",
         Category => "Editor",
         Filter   => Src_Action_Context);

      Command := new Delete_Command;
      Delete_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Delete_Command (Command.all).Kind := Word;
      Delete_Command (Command.all).Count := 1;
      Register_Action
        (Kernel, "Delete word forward", Command,
           -"Delete the word following the current cursor position",
         Category => "Editor",
         Filter   => Src_Action_Context);

      Command := new Delete_Command;
      Delete_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Delete_Command (Command.all).Kind := Word;
      Delete_Command (Command.all).Count := -1;
      Register_Action
        (Kernel, "Delete word backward", Command,
           -"Delete the word preceding the current cursor position",
         Category => "Editor",
         Filter   => Src_Action_Context);

      Line_Numbers_Area_Filter := new In_Line_Numbers_Area_Filter;

      Command := new Goto_Declaration_Command;
      Register_Contextual_Menu
        (Kernel, "Goto declaration of entity",
         Action => Command,
         Label  => -"Goto declaration of %e",
         Filter => (not Is_Dispatching)
            and ((not Line_Numbers_Area_Filter
              and Create (Module => Src_Editor_Module_Name))
              or Has_Type));

      Submenu := new Goto_Dispatch_Declaration_Submenu;
      Register_Contextual_Submenu
        (Kernel, "Goto dispatching declaration of entity",
         Label   => -"Goto declarations of %e",
         Submenu => Submenu,
         Filter  => Is_Dispatching
            and ((not Line_Numbers_Area_Filter
              and Create (Module => Src_Editor_Module_Name))
              or Has_Type));

      Command := new Goto_Next_Body_Command;
      Filter  := new Has_Body_Filter;
      Label   := new Goto_Body_Menu_Label;
      Register_Contextual_Menu
        (Kernel, "Goto body of entity",
         Action => Command,
         Label  => Label,
         Filter => (not Is_Dispatching) and Filter);

      Submenu := new Goto_Dispatch_Body_Submenu;
      Register_Contextual_Submenu
        (Kernel, "Goto dispatching bodies of entity",
         Label   => "Goto bodies of %e",
         Submenu => Submenu,
         Filter  => Is_Dispatching);

      Command := new Goto_Type_Command;
      Register_Contextual_Menu
        (Kernel, "Goto type of entity",
         Action => Command,
         Label  => -"Goto type declaration of %e",
         Filter => Has_Type);

      Command := new Type_Hierarchy_Command;
      Register_Contextual_Menu
        (Kernel, "Display type hierarchy of entity",
         Action     => Command,
         Label      => -"Display type hierarchy for %e",
         Filter     => Has_Type or Is_Access);

      Command := new Goto_Other_File_Command;
      Filter  := new Has_Other_File_Filter;
      Register_Contextual_Menu
        (Kernel, "Goto file spec<->body",
         Action     => Command,
         Label      => -"Goto file spec<->body",
         Filter     => Filter and Src_Action_Context);

      Command := new Edit_File_Command;
      Register_Contextual_Menu
        (Kernel,
         Name   => "Edit file",
         Label  => "Edit %f",
         Action => Command,
         Filter => Lookup_Filter (Kernel, "File")
           and not Create (Module => Src_Editor_Module_Name));

      Command := new Editor_Properties_Command;
      Register_Contextual_Menu
        (Kernel,
         Name   => "Editor properties",
         Label  => "Properties...",
         Action => Command,
         Filter => Create (Module => Src_Editor_Module_Name),
         Group  => Integer'Last); --  Always keep last

      Command := new Control_Command;
      Control_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Control_Command (Command.all).Mode := As_Is;
      Register_Action
        (Kernel, "No casing/indentation on next key",
         Command, -"Disable the casing and indentation on next key",
         Category => "Editor",
         Filter   => Src_Action_Context);
      Bind_Default_Key
        (Kernel      => Kernel,
         Action      => "No casing/indentation on next key",
         Default_Key => "control-q");

      Command := new Control_Command;
      Control_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Control_Command (Command.all).Mode := Sticky_As_Is;
      Register_Action
        (Kernel, "Toggle auto casing/indentation",
         Command, -"Disable or enable the casing and indentation",
         Category => "Editor",
         Filter   => Src_Action_Context);
      Bind_Default_Key
        (Kernel      => Kernel,
         Action      => "Toggle auto casing/indentation",
         Default_Key => "alt-q");

      Command := new Tab_As_Space_Command;
      Tab_As_Space_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Register_Action
        (Kernel, "Insert TAB with spaces",
         Command,
         -("Insert spaces until a column multiple of the indentation level"
           & " as set in the Preferences for the corresponding language"),
         Category => "Editor",
         Filter => Src_Action_Context);
      Bind_Default_Key
        (Kernel      => Kernel,
         Action      => "Insert TAB with spaces",
         Default_Key => "");

      Register_Module
        (Module      => Src_Editor_Module_Id,
         Kernel      => Kernel,
         Module_Name => Src_Editor_Module_Name,
         Priority    => Default_Priority);
      Register_Desktop_Functions (Save_Desktop'Access, Load_Desktop'Access);

      Add_Hook (Kernel, Open_File_Action_Hook,
                Wrapper (Source_File_Hook'Access),
                Name => "src_editor.open_file");
      Add_Hook (Kernel, File_Line_Action_Hook,
                Wrapper (File_Line_Hook'Access),
                Name => "src_editor.file_line");
      Add_Hook (Kernel, Src_Editor_Buffer.Hooks.Word_Added_Hook,
                Wrapper (Word_Added_Hook'Access),
                Name => "src_editor.word_added");
      Add_Hook (Kernel, Src_Editor_Buffer.Hooks.Character_Added_Hook,
                Wrapper (User_Character_Added_Hook'Access),
                Name => "src_editor.after_character_added");

      --  Menus

      Register_Menu
        (Kernel, File, -"_New", Stock_New, On_New_File'Access,
         Ref_Item => -"Save More");
      Register_Menu
        (Kernel, File, -"New _View", "", On_New_View'Access,
         Ref_Item => -"Save More");
      Register_Menu
        (Kernel, File, -"_Open...",  Stock_Open,
         On_Open_File'Access, null, GDK_F3,
         Ref_Item => -"Save More");
      Register_Menu
        (Kernel, File, -"Open _From Project...",  Stock_Open,
         On_Open_From_Path'Access, null,
         GDK_F3, Shift_Mask,
         Ref_Item => -"Save More");
      Register_Menu
        (Kernel, File, -"Open From _Host...",  Stock_Open,
         On_Open_Remote_File'Access, null, GDK_F3, Control_Mask,
         Ref_Item => -"Save More");

      Recent_Menu_Item := Register_Menu
        (Kernel, File, -"_Recent", "", null, Ref_Item => -"Save More");
      Associate (Get_History (Kernel).all,
                 Hist_Key,
                 Recent_Menu_Item,
                 new On_Recent'(Menu_Callback_Record with
                                Kernel => Kernel_Handle (Kernel)));

      Register_Menu
        (Kernel, File, -"_Save", Stock_Save,
         On_Save'Access, null,
         GDK_LC_s, Control_Mask,
         Ref_Item => -"Save More");
      Register_Menu
        (Kernel, File, -"Save _As...", Stock_Save_As,
         On_Save_As'Access,
         Ref_Item => -"Save More");

      Register_Menu (Kernel, File, -"_Print", Stock_Print, On_Print'Access,
                     Ref_Item => "Exit");

      Command := new Close_Command;
      Close_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Close_Command (Command.all).Close_All := False;
      Register_Action
        (Kernel, "Close current window", Command,
         -"Close the currently selected window",
        Category => "MDI");

      Register_Menu
        (Kernel,
         Parent_Path => File,
         Text        => -"_Close",
         Ref_Item    => -"Exit",
         Stock_Image => Stock_Close,
         Callback    => null,
         Command     => Command,
         Accel_Key   => GDK_LC_w,
         Accel_Mods  => Control_Mask);

      Command := new Close_Command;
      Close_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Close_Command (Command.all).Close_All := True;
      Register_Action
        (Kernel, "Close all windows", Command,
         -"Close all open windows, asking for confirmation when relevant",
         Category => "MDI");

      Register_Menu
        (Kernel,
         Parent_Path => File,
         Text        => -"Close _All",
         Ref_Item    => -"Close",
         Callback    => null,
         Command     => Command,
         Add_Before  => False);

      Gtk_New (Sep);
      Register_Menu (Kernel, File, Sep, Ref_Item => -"Exit");

      Gtk_New (Sep);
      Register_Menu (Kernel, File, Sep, Ref_Item => -"Close");

      Gtk_New (Sep);
      Register_Menu (Kernel, File, Sep, Ref_Item => -"Print");

      Gtk_New (Sep);
      Register_Menu (Kernel, File, Sep, Ref_Item => -"Change Directory...");

      --  Note: callbacks for the Undo/Redo menu items will be added later
      --  by each source editor.

      UR.Undo_Menu_Item :=
        Register_Menu (Kernel, Edit, -"_Undo", Stock_Undo,
                       null, null,
                       GDK_LC_z, Control_Mask,
                       Ref_Item  => -"Paste Previous",
                       Add_Before => False,
                       Sensitive => False);
      UR.Redo_Menu_Item :=
        Register_Menu (Kernel, Edit, -"_Redo", Stock_Redo,
                       null, null,
                       GDK_LC_r, Control_Mask,
                       Ref_Item   => -"Undo",
                       Add_Before => False,
                       Sensitive  => False);

      declare
         Space : Gtk_Separator_Tool_Item;
      begin
         Gtk_New (Space);
         Set_Draw (Space, True);
         Insert (Toolbar, Space);

         Gtk_New_From_Stock (UR.Undo_Button, Stock_Undo);
         Set_Tooltip_Text (UR.Undo_Button, -"Undo Previous Action");
         Set_Sensitive (UR.Undo_Button, False);
         Insert (Toolbar, UR.Undo_Button);

         Gtk_New_From_Stock (UR.Redo_Button, Stock_Redo);
         Set_Tooltip_Text (UR.Redo_Button, -"Redo Previous Action");
         Set_Sensitive (UR.Redo_Button, False);
         Insert (Toolbar, UR.Redo_Button);
      end;

      Kernel_Callback.Connect
        (Toolbar, Signal_Destroy,
         Toolbar_Destroy_Cb'Access,
         Kernel_Handle (Kernel));

      --  ??? This should be bound to Ctrl-A, except this would interfer with
      --  Emacs keybindings for people who want to use them.
      Register_Menu (Kernel, Edit, -"_Select All", "",
                     On_Select_All'Access, Ref_Item => -"Redo",
                     Add_Before => False,
                     Filter => Src_Action_Context);

      Gtk_New (Sep);
      Register_Menu
        (Kernel, Edit, Sep, Ref_Item => "Redo", Add_Before => False);

      Register_Menu (Kernel, Edit, -"Insert _File...",  "",
                     On_Insert_File'Access, Ref_Item => -"Select All",
                     Add_Before => False,
                     Filter => Src_Action_Context);

      Gtk_New (Sep);
      Register_Menu (Kernel, Edit, Sep, Ref_Item => -"Select All",
                     Add_Before => False);

      Register_Menu (Kernel, Selection, -"Comment _Lines", "",
                     On_Comment_Lines'Access, null,
                     GDK_minus, Control_Mask, Ref_Item => -"More Completion",
                     Add_Before => False,
                     Filter => Src_Action_Context);

      Register_Menu (Kernel, Selection, -"Uncomment L_ines", "",
                     On_Uncomment_Lines'Access, null,
                     GDK_underscore, Control_Mask,
                     Ref_Item   => -"Comment Lines",
                     Add_Before => False,
                     Filter     => Src_Action_Context);
      Register_Menu (Kernel, Selection, -"R_efill", "",
                     On_Refill'Access, null,
                     GDK_equal, Control_Mask,
                     Ref_Item   => "Expand Alias",
                     Add_Before => False,
                     Filter     => Src_Action_Context);
      Register_Menu (Kernel, Selection, -"Print Selection",
                     Callback   => On_Print_Selection'Access,
                     Ref_Item   => "Refill",
                     Add_Before => True,
                     Filter     => Src_Action_Context);

      Gtk_New (Sep);
      Register_Menu (Kernel, Edit, Sep, Ref_Item => -"Selection",
                     Add_Before => False);

      Command := new Indentation_Command;
      Indentation_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Register_Menu
        (Kernel, Edit, -"Format Selectio_n", "",
         null, Command, GDK_Tab, Control_Mask,
         Ref_Item   => "Insert File...",
         Add_Before => False,
         Filter     => Src_Action_Context);
      Register_Action
        (Kernel, "Format selection",
         Command, -"Format the current line or selection",
         Category => "Editor",
         Filter   => Src_Action_Context);

      Gtk_New (Sep);
      Register_Menu (Kernel, Edit, Sep, Ref_Item => -"Insert File...",
                     Add_Before => False);

      Register_Menu (Kernel, Edit, -"_Fold all blocks", "",
                     On_Fold_Blocks'Access, null,
                     0, 0, Ref_Item => -"Selection",
                     Add_Before     => False,
                     Filter         => Src_Action_Context);

      Gtk_New (Sep);
      Register_Menu (Kernel, Edit, Sep, Ref_Item => -"Fold all blocks",
                     Add_Before => True);

      Register_Menu (Kernel, Edit, -"Unfold all _blocks", "",
                     On_Unfold_Blocks'Access, null,
                     0, 0, Ref_Item => -"Fold all blocks",
                     Add_Before     => False,
                     Filter         => Src_Action_Context);

      Command := new Goto_Line_Command;
      Goto_Line_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Register_Menu (Kernel,
                     Parent_Path => Navigate,
                     Text        => -"Goto _Line...",
                     Stock_Image => Stock_Jump_To,
                     Command     => Command,
                     Callback    => null,
                     Accel_Key   => GDK_G,
                     Accel_Mods  => Control_Mask,
                     Ref_Item    => -"Goto File Spec<->Body");
      Register_Contextual_Menu
        (Kernel, -"Goto line...",
         Action => Command,
         Filter => Line_Numbers_Area_Filter);

      Register_Menu (Kernel, Navigate, -"Goto _Declaration", Stock_Home,
                     On_Goto_Declaration'Access, Ref_Item => -"Goto Line...");
      Register_Menu (Kernel, Navigate, -"Goto _Body", "",
                     On_Goto_Body'Access, Ref_Item => -"Goto Line...");

      Command := new Jump_To_Delimiter_Command;
      Jump_To_Delimiter_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Register_Action
        (Kernel, "Jump to matching delimiter", Command,
         -"Jump to the matching delimiter ()[]{}",
         Category => "Editor",
         Filter => Src_Action_Context);
      Register_Menu (Kernel, Navigate, -"Goto Matching _Delimiter",
                     Ref_Item   => -"Goto Body",
                     Callback   => null,
                     Add_Before => False,
                     Accel_Key  => GDK_apostrophe,
                     Accel_Mods => Control_Mask,
                     Command    => Command,
                     Filter     => Src_Action_Context);

      --  Toolbar buttons

      declare
         Button : Gtk_Tool_Button;
      begin
         Gtk_New_From_Stock (Button, Stock_New);
         Set_Tooltip_Text (Button, -"Create a New File");
         Insert (Toolbar, Button, 0);
         Kernel_Callback.Connect
           (Button, Signal_Clicked,
            On_New_File'Access, Kernel_Handle (Kernel));

         Gtk_New_From_Stock (Button, Stock_Open);
         Set_Tooltip_Text (Button, -"Open a File");
         Insert (Toolbar, Button, 1);
         Kernel_Callback.Connect
           (Button, Signal_Clicked,
            On_Open_File'Access, Kernel_Handle (Kernel));

         Gtk_New_From_Stock (Button, Stock_Save);
         Set_Tooltip_Text (Button, -"Save Current File");
         Insert (Toolbar, Button, 2);
         Kernel_Callback.Connect
           (Button, Signal_Clicked, On_Save'Access, Kernel_Handle (Kernel));
      end;

      Add_Hook (Kernel, File_Renamed_Hook,
                Wrapper (File_Renamed_Cb'Access),
                Name => "src_editor.file_renamed");
      Add_Hook (Kernel, File_Saved_Hook,
                Wrapper (File_Saved_Cb'Access),
                Name => "src_editor.file_saved");
      Add_Hook (Kernel, Location_Changed_Hook,
                Wrapper (Cursor_Stopped_Cb'Access),
                Name => "src_editor.location_changed");

      Undo_Redo_Data.Set (Kernel, UR, Undo_Redo_Id);

      Add_Hook (Kernel, Preferences_Changed_Hook,
                Wrapper (Preferences_Changed'Access),
                Name => "src_editor.preferences_changed");
      Add_Hook (Kernel, File_Edited_Hook,
                Wrapper (File_Edited_Cb'Access),
                Name => "src_editor.file_edited");
      Add_Hook (Kernel, File_Changed_On_Disk_Hook,
                Wrapper (File_Changed_On_Disk_Cb'Access),
                Name => "src_editor.file_changed_on_disk");

      Register_Commands (Kernel);

      Set_Buffer_Factory
        (Kernel, new Src_Editor_Buffer_Factory'
           (Src_Editor_Module.Editors.Create (Kernel_Handle (Kernel))));

      --  Register the search functions

      Gtk_New (Selector, Kernel);
      Gtk_New (Extra, Kernel);

      Register_Search_Function
        (Kernel            => Kernel,
         Label             => -"Files From Runtime",
         Factory           => Files_From_Runtime_Factory'Access,
         Extra_Information => Selector,
         Id                => Src_Editor_Module_Id,
         Mask              => All_Options and not Search_Backward);
      Register_Search_Function
        (Kernel            => Kernel,
         Label             => -"Files From Current Project",
         Factory           => Files_From_Root_Project_Factory'Access,
         Extra_Information => Selector,
         Id                => Src_Editor_Module_Id,
         Mask              => All_Options and not Search_Backward);
      Register_Search_Function
        (Kernel            => Kernel,
         Label             => -"Files From Project",
         Factory           => Files_From_Project_Factory'Access,
         Extra_Information => Selector,
         Id                => Src_Editor_Module_Id,
         Mask              => All_Options and not Search_Backward);
      Register_Search_Function
        (Kernel            => Kernel,
         Label             => -"Files...",
         Factory           => Files_Factory'Access,
         Extra_Information => Extra,
         Id                => Src_Editor_Module_Id,
         Mask              => All_Options and not Search_Backward);
      Register_Search_Function
        (Kernel => Kernel,
         Label             => -"Open Files",
         Factory           => Open_Files_Factory'Access,
         Extra_Information => Selector,
         Id                => Src_Editor_Module_Id,
         Mask              => All_Options and not Search_Backward);
      Register_Search_Function
        (Kernel => Kernel,
         Label             => -"Current File",
         Factory           => Current_File_Factory'Access,
         Extra_Information => Selector,
         Id                => Src_Editor_Module_Id,
         Mask              => All_Options);

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

      Register_Editor_Hooks (Kernel);

      Remove_Blank_Lines_Pixbuf := Gdk_New_From_Xpm_Data (close_block_xpm);
      Hide_Block_Pixbuf   := Gdk_New_From_Xpm_Data (fold_block_xpm);
      Unhide_Block_Pixbuf := Gdk_New_From_Xpm_Data (unfold_block_xpm);

      --  Register preferences

      Cursor_Color := Create
        (Get_Preferences (Kernel),
         Name    => "Editor-Cursor-Color",
         Default => "black",
         Page    => -"Editor/Fonts & Colors",
         Doc     => -"Color to use for the cursor in editors",
         Label   => -"Cursor color");

      Cursor_Aspect_Ratio := Create
        (Get_Preferences (Kernel),
         Name    => "Editor-Cursor-Aspect-Ratio",
         Default => 10,
         Minimum => 1,
         Maximum => 100,
         Page    => -"Editor/Fonts & Colors",
         Label   => -"Cursor aspect ratio",
         Doc     => -("Size of the cursor, proportionaly to one character. 100"
                      & "means the same size as a character"));

      Completion_Module.Register_Module (Kernel);

      Register_Hook_No_Return (Kernel, Buffer_Modified_Hook, File_Hook_Type);

      --  Register the message listener for editors
      Src_Editor_Module.Messages.Register (Kernel);
   end Register_Module;

   -------------------------
   -- Preferences_Changed --
   -------------------------

   procedure Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Pref_Display_Line_Numbers     : constant Boolean :=
                                        Display_Line_Numbers.Get_Pref;
      Pref_Display_Subprogram_Names : constant Boolean :=
                                        Display_Subprogram_Names.Get_Pref;

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

         declare
            Files : constant GNATCOLL.VFS.File_Array := Open_Files (Kernel);
         begin
            for Node in Files'Range loop
               declare
                  Box : constant Source_Editor_Box :=
                    Get_Source_Box_From_MDI
                      (Find_Editor (Kernel, Files (Node)));
               begin
                  if Pref_Display_Subprogram_Names then
                     Show_Subprogram_Name (Box, Get_Subprogram_Name (Box));
                  else
                     Clear_Subprogram_Name (Box);
                  end if;
               end;
            end loop;
         end;

         Id.Show_Subprogram_Names := Pref_Display_Subprogram_Names;
      end if;

      Parse_String ("style ""gps-style"" { " & ASCII.LF
                    & "GtkTextView::cursor-color="""
                    & Cursor_Color.Get_Pref
                    & """" & ASCII.LF
                    & "GtkTextView::cursor-aspect-ratio="
                    & Float'Image
                      (Float (Cursor_Aspect_Ratio.Get_Pref) / 100.0)
                    & ASCII.LF
                    & "}" & ASCII.LF
                    & "class ""GtkTextView"" style ""gps-style""");

      if Pref_Display_Line_Numbers /= Id.Display_Line_Numbers then
         Id.Display_Line_Numbers := Pref_Display_Line_Numbers;

         if Pref_Display_Line_Numbers then
            declare
               Layout : constant Pango_Layout := Create_Pango_Layout
                 (Get_Main_Window (Kernel));
               Height : Gint;
               Width  : Gint;
            begin
               Set_Font_Description (Layout, Default_Style.Get_Pref_Font);
               Set_Markup (Layout, "0000");
               Get_Pixel_Size (Layout, Width, Height);
               Id.Character_Width := Width / 4;
               Unref (Layout);
            end;
         else
            Id.Character_Width := 0;
         end if;

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
      end if;

      --  Set ACL usage
      if Config.Host = Config.Windows then
         if Use_ACL.Get_Pref then
            Runtime_Use_ACL := 1;
         else
            Runtime_Use_ACL := 0;
         end if;
      end if;
   end Preferences_Changed;

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
      Marker_List.Free (Id.Stored_Marks);

      Free (Id.Search_Pattern);

      --  Post_It_Note_GC and Blank_Lines_GC are initialized only when the
      --  main window is mapped. Therefore, if the window was never displayed,
      --  these values are not initialized.

      --  Destroy graphics
      if Remove_Blank_Lines_Pixbuf /= null then
         Unref (Remove_Blank_Lines_Pixbuf);
      end if;

      if Hide_Block_Pixbuf /= null then
         Unref (Hide_Block_Pixbuf);
      end if;

      if Unhide_Block_Pixbuf /= null then
         Unref (Unhide_Block_Pixbuf);
      end if;

      if File_Pixbuf /= null then
         Unref (File_Pixbuf);
      end if;

      if File_Modified_Pixbuf /= null then
         Unref (File_Modified_Pixbuf);
      end if;

      if File_Unsaved_Pixbuf /= null then
         Unref (File_Unsaved_Pixbuf);
      end if;

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

      Editors_Hash.Reset (Id.Editors);

      Destroy (Src_Editor_Buffer_Factory
               (Get_Buffer_Factory (Get_Kernel (Id)).all));

      Src_Editor_Module.Messages.Unregister (Get_Kernel (Id));

      Src_Editor_Module_Id := null;
   end Destroy;

   -----------------
   -- Find_Editor --
   -----------------

   function Find_Editor
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File) return Gtkada.MDI.MDI_Child
   is
      Id    : constant Source_Editor_Module :=
                Source_Editor_Module (Src_Editor_Module_Id);
      Iter  : Child_Iterator;
      Child : MDI_Child;
      Full  : GNATCOLL.VFS.Virtual_File;

   begin
      if File = GNATCOLL.VFS.No_File or else Get_MDI (Kernel) = null then
         return null;
      end if;

      --  Check whether the currently selected child corresponds to
      --  the file (this will be the case in a vast majority of calls to
      --  this subprogram)

      Child := Get_Focus_Child (Get_MDI (Kernel));

      if Get_Filename (Child) = File then
         return Child;
      end if;

      --  Attempt to find the editor in the cache

      Child := Editors_Hash.Get (Id.Editors, File).Child;

      --  Verify that the child corresponds to the wanted filename.
      --  (It could have changed, for example if "save as..." was used)

      if Child /= null then
         if Get_Filename (Child) = File then
            return Child;
         else
            Editors_Hash.Remove (Id.Editors, File);
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
           or else Get_Filename (Child) = Full
           or else Get_File_Identifier (Child) = Full

            --  Handling of file identifiers
           or else Get_Title (Child) = Display_Full_Name (File);

         Next (Iter);
      end loop;

      return Child;
   end Find_Editor;

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
      Iter  : Child_Iterator := First_Child (Get_MDI (Kernel));
      Child : MDI_Child;

   begin
      loop
         Child := Get (Iter);

         exit when Child = null
           or else (Get_Widget (Child).all in Source_Editor_Box_Record'Class
                    and then Source_Editor_Box (Get_Widget (Child)) =
                      Source_Editor_Box (Editor));
         Next (Iter);
      end loop;

      return Child;
   end Find_Child;

   ----------
   -- Hash --
   ----------

   function Hash is new String_Utils.Hash (Header_Num);

   function Hash (F : Virtual_File) return Header_Num is
   begin
      if Is_Case_Sensitive (Get_Nickname (Build_Server)) then
         return Hash (+Full_Name (F));
      else
         return Hash (To_Lower (+Full_Name (F)));
      end if;
   end Hash;

   -----------
   -- Equal --
   -----------

   function Equal (F1, F2 : Virtual_File) return Boolean is
   begin
      return F1 = F2;
   end Equal;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Element) is
      pragma Unreferenced (X);
   begin
      null;
   end Free;

   --------------------
   -- Autosaved_File --
   --------------------

   function Autosaved_File (File : Virtual_File) return Virtual_File is
   begin
      --  Implementation must be in sync with Is_Auto_Save below
      return Create_From_Dir (Dir (File), ".#" & Base_Name (File) & "#");
   end Autosaved_File;

   ------------------
   -- Is_Auto_Save --
   ------------------

   function Is_Auto_Save (File : GNATCOLL.VFS.Virtual_File) return Boolean is
      Base : constant String := +Base_Name (File);
   begin
      return Base'Length >= 2
        and then Base (Base'First .. Base'First + 1) = ".#"
        and then Base (Base'Last) = '#';
   end Is_Auto_Save;

   ---------------------------------
   -- Line_Number_Character_Width --
   ---------------------------------

   function Line_Number_Character_Width return Gint is
      Id : constant Source_Editor_Module :=
             Source_Editor_Module (Src_Editor_Module_Id);
   begin
      if Id = null then
         return 0;
      else
         return Id.Character_Width;
      end if;
   end Line_Number_Character_Width;

   -----------------------
   -- Get_Command_Queue --
   -----------------------

   overriding function Get_Command_Queue
     (Child : access Editor_Child_Record) return Commands.Command_Queue
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

end Src_Editor_Module;
