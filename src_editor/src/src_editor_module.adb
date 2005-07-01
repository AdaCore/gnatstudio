-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2005                       --
--                              AdaCore                              --
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

with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;
with Ada.Characters.Handling;     use Ada.Characters.Handling;
with Ada.Exceptions;              use Ada.Exceptions;
with Ada.IO_Exceptions;           use Ada.IO_Exceptions;

with GNAT.Directory_Operations;   use GNAT.Directory_Operations;
with GNAT.OS_Lib;                 use GNAT.OS_Lib;
with Glib.Xml_Int;                use Glib.Xml_Int;
with Glib.Convert;                use Glib.Convert;
with Gdk;                         use Gdk;
with Gdk.Color;                   use Gdk.Color;
with Gdk.GC;                      use Gdk.GC;
with Gdk.Types;                   use Gdk.Types;
with Gdk.Types.Keysyms;           use Gdk.Types.Keysyms;
with Glib;                        use Glib;
with Glib.Object;                 use Glib.Object;
with Glib.Values;                 use Glib.Values;
with GPS.Intl;                    use GPS.Intl;
with GPS.Main_Window;             use GPS.Main_Window;
with GPS.Kernel;                  use GPS.Kernel;
with GPS.Kernel.Actions;          use GPS.Kernel.Actions;
with GPS.Kernel.Console;          use GPS.Kernel.Console;
with GPS.Kernel.Contexts;         use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;            use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;              use GPS.Kernel.MDI;
with GPS.Kernel.Modules;          use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;      use GPS.Kernel.Preferences;
with GPS.Kernel.Project;          use GPS.Kernel.Project;
with GPS.Kernel.Scripts;          use GPS.Kernel.Scripts;
with GPS.Kernel.Timeout;          use GPS.Kernel.Timeout;
with GPS.Kernel.Standard_Hooks;   use GPS.Kernel.Standard_Hooks;
with Language;                    use Language;
with Language_Handlers;           use Language_Handlers;
with Basic_Types;                 use Basic_Types;
with Gtk.Box;                     use Gtk.Box;
with Gtk.Button;                  use Gtk.Button;
with Gtk.Dialog;                  use Gtk.Dialog;
with Gtk.Enums;                   use Gtk.Enums;
with Gtk.GEntry;                  use Gtk.GEntry;
with Gtk.Handlers;
with Gtk.Label;                   use Gtk.Label;
with Gtk.Menu_Item;               use Gtk.Menu_Item;
with Gtk.Main;                    use Gtk.Main;
with Gtk.Rc;                      use Gtk.Rc;
with Gtk.Stock;                   use Gtk.Stock;
with Gtk.Toolbar;                 use Gtk.Toolbar;
with Gtk.Widget;                  use Gtk.Widget;
with Gtk.Text_Iter;               use Gtk.Text_Iter;
with Gtk.Text_Mark;               use Gtk.Text_Mark;
with Gtk.Window;                  use Gtk.Window;
with Gtkada.Entry_Completion;     use Gtkada.Entry_Completion;
with Gtkada.Handlers;             use Gtkada.Handlers;
with Gtkada.MDI;                  use Gtkada.MDI;
with Gtkada.File_Selector;        use Gtkada.File_Selector;
with Src_Editor_Box;              use Src_Editor_Box;
with Src_Editor_Buffer;           use Src_Editor_Buffer;
with Src_Editor_Buffer.Line_Information;
use Src_Editor_Buffer.Line_Information;
with Src_Editor_View;             use Src_Editor_View;
with Src_Editor_View.Commands;    use Src_Editor_View.Commands;
with String_List_Utils;           use String_List_Utils;
with String_Utils;                use String_Utils;
with Traces;                      use Traces;
with Projects.Registry;           use Projects, Projects.Registry;
with Src_Contexts;                use Src_Contexts;
with Find_Utils;                  use Find_Utils;
with Histories;                   use Histories;
with Aliases_Module;              use Aliases_Module;
with Commands.Interactive;        use Commands, Commands.Interactive;
with VFS;                         use VFS;
with Casing_Exceptions;           use Casing_Exceptions;
with Default_Preferences;         use Default_Preferences;
with Glib.Properties.Creation;    use Glib.Properties.Creation;

with Gtkada.Types;                use Gtkada.Types;
with Gdk.Pixbuf;                  use Gdk.Pixbuf;

with Generic_List;
with File_Utils;                  use File_Utils;

with Src_Editor_Module.Line_Highlighting;
with Src_Editor_Buffer.Buffer_Commands; use Src_Editor_Buffer.Buffer_Commands;
with Src_Editor_Buffer.Line_Information;
with Src_Editor_Buffer.Hooks;           use Src_Editor_Buffer.Hooks;
with Src_Editor_Buffer.Text_Handling;   use Src_Editor_Buffer.Text_Handling;

with Src_Printing;
with Pango.Font;
with Pango.Layout; use Pango.Layout;
with Pango.Enums;

package body Src_Editor_Module is

   Me : constant Debug_Handle := Create ("Src_Editor_Module");

   Hist_Key : constant History_Key := "reopen_files";
   --  Key to use in the kernel histories to store the most recently opened
   --  files.

   Open_From_Path_History : constant History_Key := "open-from-project";
   --  Key used to store the most recently open files in the Open From Project
   --  dialog.

   editor_xpm : aliased Chars_Ptr_Array (0 .. 0);
   pragma Import (C, editor_xpm, "mini_page_xpm");
   fold_block_xpm : aliased Chars_Ptr_Array (0 .. 0);
   pragma Import (C, fold_block_xpm, "fold_block_xpm");
   unfold_block_xpm  : aliased Chars_Ptr_Array (0 .. 0);
   pragma Import (C, unfold_block_xpm, "unfold_block_xpm");
   close_block_xpm  : aliased Chars_Ptr_Array (0 .. 0);
   pragma Import (C, close_block_xpm, "close_block_xpm");

   Filename_Cst          : aliased constant String := "filename";
   Line_Cst              : aliased constant String := "line";
   Col_Cst               : aliased constant String := "column";
   Length_Cst            : aliased constant String := "length";
   Pattern_Cst           : aliased constant String := "pattern";
   Case_Cst              : aliased constant String := "case_sensitive";
   Regexp_Cst            : aliased constant String := "regexp";
   Scope_Cst             : aliased constant String := "scope";
   Force_Cst             : aliased constant String := "force";
   All_Cst               : aliased constant String := "all";
   Interactive_Cst       : aliased constant String := "interactive";
   Current_Line_Only_Cst : aliased constant String := "current_line_only";
   Before_Cst            : aliased constant String := "before";
   After_Cst             : aliased constant String := "after";
   Name_Cst              : aliased constant String := "name";
   First_Line_Cst        : aliased constant String := "first_line";
   Start_Column_Cst      : aliased constant String := "start_column";
   Last_Line_Cst         : aliased constant String := "last_line";
   End_Column_Cst        : aliased constant String := "end_column";
   Writable_Cst          : aliased constant String := "writable";
   Position_Cst          : aliased constant String := "position";

   Edit_Cmd_Parameters : constant Cst_Argument_List :=
     (1 => Filename_Cst'Access,
      2 => Line_Cst'Access,
      3 => Col_Cst'Access,
      4 => Length_Cst'Access,
      5 => Force_Cst'Access,
      6 => Position_Cst'Access);
   Create_Mark_Parameters : constant Cst_Argument_List :=
     (1 => Filename_Cst'Access,
      2 => Line_Cst'Access,
      3 => Col_Cst'Access,
      4 => Length_Cst'Access);
   File_Search_Parameters : constant Cst_Argument_List :=
     (1 => Pattern_Cst'Access,
      2 => Case_Cst'Access,
      3 => Regexp_Cst'Access,
      4 => Scope_Cst'Access);
   Save_Cmd_Parameters : constant Cst_Argument_List :=
     (1 => Interactive_Cst'Access,
      2 => All_Cst'Access);
   Indent_Cmd_Parameters : constant Cst_Argument_List :=
     (1 => Current_Line_Only_Cst'Access);
   Get_Chars_Args : constant Cst_Argument_List :=
     (1 => Filename_Cst'Access,
      2 => Line_Cst'Access,
      3 => Col_Cst'Access,
      4 => Before_Cst'Access,
      5 => After_Cst'Access);
   Case_Exception_Cmd_Parameters : constant Cst_Argument_List :=
     (1 => Name_Cst'Access);
   Set_Writable_Cmd_Parameters : constant Cst_Argument_List :=
     (1 => Name_Cst'Access,
      2 => Writable_Cst'Access);
   Select_Text_Cmd_Parameters : constant Cst_Argument_List :=
     (1 => First_Line_Cst'Access,
      2 => Last_Line_Cst'Access,
      3 => Start_Column_Cst'Access,
      4 => End_Column_Cst'Access);

   Cursor_Color        : Param_Spec_Color;
   Cursor_Aspect_Ratio : Param_Spec_Int;

   type Editor_Child_Record is new GPS_MDI_Child_Record with null record;

   function Dnd_Data
     (Child : access Editor_Child_Record; Copy : Boolean) return MDI_Child;
   --  See inherited documentation

   procedure Gtk_New
     (Box : out Source_Box; Editor : Source_Editor_Box);
   --  Create a new source box.

   procedure Initialize
     (Box : access Source_Box_Record'Class; Editor : Source_Editor_Box);
   --  Internal initialization function.

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

   procedure Save_To_File
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      Name    : VFS.Virtual_File := VFS.No_File;
      Success : out Boolean);
   --  Save the current editor to Name, or its associated filename if Name is
   --  null.

   function Open_File
     (Kernel     : access Kernel_Handle_Record'Class;
      File       : VFS.Virtual_File := VFS.No_File;
      Create_New : Boolean := True;
      Focus      : Boolean := True;
      Force      : Boolean := False;
      Position   : Gtkada.MDI.Child_Position :=
        Gtkada.MDI.Position_Default) return Source_Box;
   --  Open a file and return the handle associated with it.
   --  If Add_To_MDI is set to True, the box will be added to the MDI window.
   --  If Focus is True, the box will be raised if it is in the MDI.
   --  See Create_File_Exitor.
   --  Position indicates the position to give to the editor in the MDI.
   --  If Force is true, then the file is reloaded without asking confirmation
   --  from the user

   function Create_File_Editor
     (Kernel     : access Kernel_Handle_Record'Class;
      File       : VFS.Virtual_File;
      Create_New : Boolean := True) return Source_Editor_Box;
   --  Create a new text editor that edits File.
   --  If File is the empty string, or the file doesn't exist and Create_New is
   --  True, then an empty editor is created.
   --  No check is done to make sure that File is not already edited
   --  elsewhere. The resulting editor is not put in the MDI window.

   function Save_Function
     (Kernel : access Kernel_Handle_Record'Class;
      Child  : Gtk.Widget.Gtk_Widget;
      Mode   : Save_Function_Mode) return Boolean;
   --  Save the text editor.
   --  If Force is False, then offer a choice to the user before doing so.

   type Location_Idle_Data is record
      Edit  : Source_Editor_Box;
      Line  : Editable_Line_Type;
      Column, Column_End : Natural;
      Kernel : Kernel_Handle;
      Focus  : Boolean;
   end record;

   function Location_Callback (D : Location_Idle_Data) return Boolean;
   --  Idle callback used to scroll the source editors.

   function File_Edit_Callback (D : Location_Idle_Data) return Boolean;
   --  Emit the File_Edited signal.

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

   procedure On_Save_All
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  File->Save All menu

   procedure On_Save_Desktop
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  File->Save Desktop menu

   procedure On_Save_Default_Desktop
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  File->Save Default Desktop menu

   procedure On_Change_Dir
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  File->Change Directory... menu

   procedure On_Exit
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  File->Exit menu

   procedure On_Print
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  File->Print menu

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
   function Completion
     (Factory : File_Completion_Factory; Index : Positive) return String;
   function Description
     (Factory : File_Completion_Factory; Index : Positive) return String;
   --  See doc from inherited subprogram

   type Edit_File_Command is new Interactive_Command with null record;
   function Execute
     (Command : access Edit_File_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  See doc for inherited subprogram
   --  Edit a file (from a contextual menu)

   type Close_Command is new Interactive_Command with record
      Kernel    : Kernel_Handle;
      Close_All : Boolean;
   end record;
   function Execute
     (Command : access Close_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Close the current window (or all windows if Close_All is True).

   function Default_Factory
     (Kernel : access Kernel_Handle_Record'Class;
      Child  : Gtk.Widget.Gtk_Widget) return Selection_Context_Access;
   --  Create the current context for GPS.Kernel.Get_Current_Context

   function Default_Factory
     (Kernel : access Kernel_Handle_Record'Class;
      Editor : access Source_Editor_Box_Record'Class)
      return Selection_Context_Access;
   --  Same as above.

   function New_View
     (Kernel  : access Kernel_Handle_Record'Class;
      Current : Source_Editor_Box) return Source_Box;
   --  Create a new view for Current and add it in the MDI.
   --  The current editor is the focus child in the MDI.
   --  If Add is True, the Box is added to the MDI.

   procedure New_View
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Create a new view for the current editor and add it in the MDI.
   --  The current editor is the focus child in the MDI. If the focus child
   --  is not an editor, nothing happens.

   function Delete_Callback
     (Widget : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues) return Boolean;
   --  Callback for the "delete_event" signal.

   procedure File_Edited_Cb
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Callback for the "file_edited" hook.

   procedure File_Closed_Cb
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Callback for the "file_closed" hook.

   procedure File_Changed_On_Disk_Cb
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Callback for the "file_changed_on_disk" hook.

   procedure File_Saved_Cb
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Callback for the "file_saved" hook.

   procedure Cursor_Stopped_Cb
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Callback for the "cursor_stopped" hook.

   type Child_Triplet is array (1 .. 3) of MDI_Child;
   type Child_Triplet_Access is access Child_Triplet;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Child_Triplet, Child_Triplet_Access);

   package Child_Triplet_Callback is new Gtk.Handlers.User_Callback
     (Widget_Type => Gtk_Widget_Record,
      User_Type   => Child_Triplet_Access);

   procedure On_Raise_Child
     (Child   : access Gtk_Widget_Record'Class;
      Triplet : Child_Triplet_Access);
   --  Called when synchronized editor Child in Triplet is raised.

   procedure On_Delete_Child
     (Child   : access Gtk_Widget_Record'Class;
      Triplet : Child_Triplet_Access);
   --  Called when synchronized editor Child in Triplet is deleted.

   procedure Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class);
   --  Called when the preferences have changed.

   procedure Edit_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Interactive command handler for the source editor module.

   procedure Current_Search_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   procedure File_Search_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   procedure Project_Search_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   procedure Common_Search_Command_Handler
     (Data  : in out Callback_Data'Class;
      Files : VFS.File_Array_Access);
   --  Interactive command handler for the source editor module (Search part)

   procedure Add_To_Recent_Menu
     (Kernel : access Kernel_Handle_Record'Class; File : Virtual_File);
   --  Add an entry for File to the Recent menu, if needed.

   function Find_Mark (Identifier : String) return Mark_Identifier_Record;
   --  Find the mark corresponding to Identifier, or return an empty
   --  record.

   procedure Fill_Marks (Kernel : Kernel_Handle; File : VFS.Virtual_File);
   --  Create the marks on the buffer corresponding to File, if File has just
   --  been open.

   function Get_Filename (Child : MDI_Child) return VFS.Virtual_File;
   --  If Child is a file editor, return the corresponding filename,
   --  otherwise return an empty string.

   function Get_File_Identifier (Child  : MDI_Child) return VFS.Virtual_File;
   --  Return the file identifier if Child is a file editor

   function Expand_Aliases_Entities
     (Kernel    : access Kernel_Handle_Record'Class;
      Expansion : String;
      Special   : Character) return String;
   --  Does the expansion of special entities in the aliases.

   type On_Recent is new Menu_Callback_Record with record
      Kernel : Kernel_Handle;
   end record;
   procedure Activate (Callback : access On_Recent; Item : String);

   procedure Toolbar_Destroy_Cb
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Called when we are about to destroy the toolbar which contains the
   --  undo/redo items.

   procedure Map_Cb (Widget : access Gtk_Widget_Record'Class);
   --  Create the module-wide GCs.

   package File_Callback is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record, Virtual_File);

   procedure On_Editor_Destroy
     (Widget : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues;
      User   : Virtual_File);
   --  Callback to call when an editor is about to be destroyed.

   procedure Update_Cache_On_Focus
     (Child : access Gtk_Widget_Record'Class);
   --  Make sure that the last view for a file is reflected in the cache, so
   --  that we always used that one by default when search for the last editor
   --  for a given file

   -----------------------
   -- On_Editor_Destroy --
   -----------------------

   procedure On_Editor_Destroy
     (Widget : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues;
      User   : Virtual_File)
   is
      pragma Unreferenced (Widget, Params);
      Id : constant Source_Editor_Module :=
        Source_Editor_Module (Src_Editor_Module_Id);
   begin
      if Id /= null then
         Editors_Hash.Remove (Id.Editors, User);
      end if;
   end On_Editor_Destroy;

   --------------
   -- Dnd_Data --
   --------------

   function Dnd_Data
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
      UR.Undo_Button_Handler_ID.Signal := Null_Signal_Id;
      UR.Undo_Button := null;
      UR.Redo_Button := null;
      UR.Undo_Menu_Item := null;
      UR.Redo_Menu_Item := null;
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Toolbar_Destroy_Cb;

   ------------
   -- Map_Cb --
   ------------

   procedure Map_Cb (Widget : access Gtk_Widget_Record'Class) is
      Color   : Gdk_Color;
      Success : Boolean;
      Id      : constant Source_Editor_Module :=
        Source_Editor_Module (Src_Editor_Module_Id);
   begin
      Gdk_New (Id.Blank_Lines_GC, Get_Window (Widget));
      Gdk_New (Id.Post_It_Note_GC, Get_Window (Widget));

      --  ??? Should this be a preference ?
      Color := Parse ("#AAAAAA");
      Alloc_Color (Get_Default_Colormap, Color, False, True, Success);

      if Success then
         Set_Foreground
           (Id.Blank_Lines_GC, Color);
      else
         Set_Foreground
           (Id.Blank_Lines_GC,
            Black (Get_Default_Colormap));
      end if;

      --  ??? This should be a preference !
      Color := Parse ("#FFFF88");
      Alloc_Color (Get_Default_Colormap, Color, False, True, Success);

      if Success then
         Set_Foreground
           (Id.Post_It_Note_GC, Color);
      else
         Set_Foreground
           (Id.Post_It_Note_GC,
            Black (Get_Default_Colormap));
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Map_Cb;

   -------------------------
   -- Get_File_Identifier --
   -------------------------

   function Get_File_Identifier (Child  : MDI_Child) return VFS.Virtual_File is
   begin
      if Child /= null
        and then Get_Widget (Child).all in Source_Box_Record'Class
      then
         return Get_File_Identifier
           (Get_Buffer (Source_Box (Get_Widget (Child)).Editor));
      else
         return VFS.No_File;
      end if;
   end Get_File_Identifier;

   ------------------
   -- Get_Filename --
   ------------------

   function Get_Filename (Child : MDI_Child) return VFS.Virtual_File is
   begin
      if Child /= null
        and then Get_Widget (Child).all in Source_Box_Record'Class
      then
         return Get_Filename (Source_Box (Get_Widget (Child)).Editor);
      else
         return VFS.No_File;
      end if;
   end Get_Filename;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Mark_Identifier_Record) is
      pragma Unreferenced (X);
   begin
      null;
   end Free;

   ---------------
   -- Find_Mark --
   ---------------

   function Find_Mark (Identifier : String) return Mark_Identifier_Record is
      use type Mark_Identifier_List.List_Node;

      Id          : constant Source_Editor_Module :=
        Source_Editor_Module (Src_Editor_Module_Id);
      Mark_Node   : Mark_Identifier_List.List_Node;
      Mark_Record : Mark_Identifier_Record;
   begin
      Mark_Node := Mark_Identifier_List.First (Id.Stored_Marks);

      while Mark_Node /= Mark_Identifier_List.Null_Node loop
         Mark_Record := Mark_Identifier_List.Data (Mark_Node);

         if Image (Mark_Record.Id) = Identifier then
            return Mark_Record;
         end if;

         Mark_Node := Mark_Identifier_List.Next (Mark_Node);
      end loop;

      return Mark_Identifier_Record'
        (Id     => 0,
         File   => VFS.No_File,
         Mark   => null,
         Line   => 0,
         Column => 0,
         Length => 0);
   end Find_Mark;

   -----------------------------------
   -- Common_Search_Command_Handler --
   -----------------------------------

   procedure Common_Search_Command_Handler
     (Data    : in out Callback_Data'Class;
      Files   : File_Array_Access)
   is
      Kernel  : constant Kernel_Handle := Get_Kernel (Data);
      Context : Files_Project_Context_Access;
      Pattern : constant String  := Nth_Arg (Data, 2);
      Casing  : constant Boolean := Nth_Arg (Data, 3, False);
      Regexp  : constant Boolean := Nth_Arg (Data, 4, False);
      Scope   : constant String  := Nth_Arg (Data, 5, "whole");
      S       : Search_Scope;

      function Callback (Match : Match_Result) return Boolean;
      --  Store the result of the match in Data

      --------------
      -- Callback --
      --------------

      function Callback (Match : Match_Result) return Boolean is
      begin
         Set_Return_Value
           (Data,
            Create_File_Location
              (Get_Script (Data),
               Create_File (Get_Script (Data), Current_File (Context)),
               Match.Line,
               Match.Column));
         return True;
      end Callback;

   begin
      if Scope = "whole" then
         S := Whole;
      elsif Scope = "comments" then
         S := Comments_Only;
      elsif Scope = "strings" then
         S := Strings_Only;
      elsif Scope = "code" then
         S := All_But_Comments;
      else
         S := Whole;
      end if;

      Context := Files_From_Project_Factory
        (Scope           => S,
         All_Occurrences => True);
      Set_File_List (Context, Files);
      Set_Context
        (Context,
         Look_For => Pattern,
         Options  => (Case_Sensitive => Casing,
                      Whole_Word     => False,
                      Regexp         => Regexp));

      Set_Return_Value_As_List (Data);

      while Search
        (Context  => Context,
         Handler  => Get_Language_Handler (Kernel),
         Kernel   => Kernel,
         Callback => Callback'Unrestricted_Access)
      loop
         --  No need to delay, since the search is done in same process.
         null;
      end loop;
   end Common_Search_Command_Handler;

   ------------------------------------
   -- Current_Search_Command_Handler --
   ------------------------------------

   procedure Current_Search_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      pragma Unreferenced (Command);
      Kernel  : constant Kernel_Handle := Get_Kernel (Data);

      Id      : constant Source_Editor_Module :=
         Source_Editor_Module (Src_Editor_Module_Id);

      Inst    : constant Class_Instance :=
                 Nth_Arg (Data, 1, Get_File_Class (Kernel));
      File    : constant Virtual_File := Get_File (Get_Data (Inst));
      Pattern : constant String := Nth_Arg (Data, 2);
      Casing  : constant Boolean := Nth_Arg (Data, 3, False);
      Regexp  : constant Boolean := Nth_Arg (Data, 4, False);

      Dummy   : Boolean;
      pragma Unreferenced (Dummy);

      function Callback (Match : Match_Result) return Boolean;
      --  Store the result of the match in Data

      --------------
      -- Callback --
      --------------

      function Callback (Match : Match_Result) return Boolean is
      begin
         Set_Return_Value
           (Data,
            Create_File_Location
              (Get_Script (Data),
               Create_File (Get_Script (Data), File),
               Match.Line,
               Match.Column));
         return True;
      end Callback;

   begin
      if Id.Search_Context = null
        or else Id.Search_Pattern = null
        or else Id.Search_Pattern.all /= Pattern
        or else Id.Search_File /= File
      then
         Free (Id.Search_Pattern);
         Id.Search_Pattern := new String'(Pattern);
         Id.Search_File    := File;
         Id.Search_Context := Files_From_Project_Factory (Whole, False);
         Set_File_List (Id.Search_Context, new File_Array'(1 => File));

         Set_Context
           (Id.Search_Context,
            Look_For => Pattern,
            Options  => (Case_Sensitive => Casing,
                         Whole_Word     => False,
                         Regexp         => Regexp));
      end if;

      Dummy := Search
        (Context  => Id.Search_Context,
         Handler  => Get_Language_Handler (Kernel),
         Kernel   => Kernel,
         Callback => Callback'Unrestricted_Access);
   end Current_Search_Command_Handler;

   ---------------------------------
   -- File_Search_Command_Handler --
   ---------------------------------

   procedure File_Search_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
      Inst   : constant Class_Instance :=
        Nth_Arg (Data, 1, Get_File_Class (Kernel));
      Info   : constant File_Info := Get_Data (Inst);
   begin
      Name_Parameters (Data, File_Search_Parameters);
      Common_Search_Command_Handler
        (Data, new File_Array'(1 => Get_File (Info)));
   end File_Search_Command_Handler;

   ------------------------------------
   -- Project_Search_Command_Handler --
   ------------------------------------

   procedure Project_Search_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      pragma Unreferenced (Command);
      Project   : constant Project_Type := Get_Data (Data, 1);
      Recursive : Boolean;
   begin
      Name_Parameters (Data, File_Search_Parameters);
      Recursive := Nth_Arg (Data, 5, True);
      Common_Search_Command_Handler
        (Data, Get_Source_Files (Project, Recursive));
   end Project_Search_Command_Handler;

   --------------------------
   -- Edit_Command_Handler --
   --------------------------

   procedure Edit_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
      Id     : constant Source_Editor_Module :=
        Source_Editor_Module (Src_Editor_Module_Id);
      Length : Natural := 0;
      Line   : Natural := 1;
      Column : Natural := 1;
      Force  : Boolean;

   begin
      if Command = "edit" or else Command = "create_mark" then
         if Command = "edit" then
            Name_Parameters (Data, Edit_Cmd_Parameters);
         else
            Name_Parameters (Data, Create_Mark_Parameters);
         end if;

         declare
            File : constant Virtual_File :=
              Create (Nth_Arg (Data, 1), Kernel, Use_Source_Path => True);
            Position : Natural;
         begin
            Line   := Nth_Arg (Data, 2, Default => 1);
            Column := Nth_Arg (Data, 3, Default => 1);
            Length := Nth_Arg (Data, 4, Default => 0);

            if File /= VFS.No_File then
               if Command = "edit" then
                  Force := Nth_Arg (Data, 5, Default => False);
                  Position := Nth_Arg
                    (Data, 6, Default => Natural (Position_Default));

                  if Length = 0 then
                     Open_File_Editor
                       (Kernel,
                        File,
                        Line,
                        Column,
                        Enable_Navigation => False,
                        Force_Reload => Force,
                        Position => Child_Position (Position));
                  else
                     Open_File_Editor
                       (Kernel,
                        File,
                        Line,
                        Column,
                        Column + Length,
                        Enable_Navigation => False,
                        Force_Reload => Force);
                  end if;

               elsif Command = "create_mark" then
                  declare
                     Box         : Source_Box;
                     Child       : MDI_Child;
                     Mark_Record : Mark_Identifier_Record;
                  begin
                     Child := Find_Editor (Kernel, File);

                     --  Create a new mark record and insert it in the list.

                     Mark_Record.File := File;
                     Mark_Record.Id   := Id.Next_Mark_Id;
                     Mark_Record.Line := Line;
                     Id.Next_Mark_Id  := Id.Next_Mark_Id + 1;

                     Mark_Record.Length := Length;

                     if Child /= null then
                        Box := Source_Box (Get_Widget (Child));
                        Mark_Record.Mark :=
                          Create_Mark
                            (Get_Buffer (Box.Editor),
                             Editable_Line_Type (Line),
                             Column);
                     else
                        Mark_Record.Line := Line;
                        Mark_Record.Column := Column;
                        Add_Unique_Sorted
                          (Id.Unopened_Files, Full_Name (File).all);
                     end if;

                     Mark_Identifier_List.Append
                       (Id.Stored_Marks, Mark_Record);

                     Set_Return_Value (Data, Image (Mark_Record.Id));
                  end;
               end if;
            end if;
         end;

      elsif Command = "indent" then
         Name_Parameters (Data, Indent_Cmd_Parameters);
         declare
            Current_Line_Only : constant Boolean := Nth_Arg (Data, 1, False);
            Child : constant MDI_Child := Find_Current_Editor (Kernel);
            Box   : Source_Box;
         begin
            if Child /= null then
               Box := Source_Box (Get_Widget (Child));

               if not Get_Editable (Get_View (Box.Editor))
                 or else not Do_Indentation
                   (Get_Buffer (Box.Editor), Current_Line_Only)
               then
                  Set_Error_Msg (Data, -"Could not indent selection");
               end if;
            end if;
         end;

      elsif Command = "indent_buffer" then
         declare
            Child    : constant MDI_Child := Find_Current_Editor (Kernel);
            Box      : Source_Box;
            Buffer   : Source_Buffer;
            From, To : Gtk_Text_Iter;

         begin
            if Child /= null then
               Box := Source_Box (Get_Widget (Child));
               Buffer := Get_Buffer (Box.Editor);

               Get_Start_Iter (Buffer, From);
               Get_End_Iter (Buffer, To);

               if not Get_Editable (Get_View (Box.Editor))
                 or else not Do_Indentation
                   (Get_Buffer (Box.Editor), From, To)
               then
                  Set_Error_Msg (Data, -"Could not indent buffer");
               end if;
            end if;
         end;

      elsif Command = "refill" then
         declare
            Child : constant MDI_Child := Find_Current_Editor (Kernel);
            Box   : Source_Box;

         begin
            if Child /= null then
               Box := Source_Box (Get_Widget (Child));

               if not Get_Editable (Get_View (Box.Editor))
                 or else not Do_Refill (Get_Buffer (Box.Editor))
               then
                  Set_Error_Msg (Data, -"Could not refill buffer");
               end if;
            end if;
         end;

      elsif Command = "cut"
        or else Command = "copy"
        or else Command = "paste"
        or else Command = "select_all"
      then
         declare
            Source : constant Source_Editor_Box :=
                       Get_Source_Box_From_MDI
                         (Find_Current_Editor (Kernel));

         begin
            if Source /= null then
               if Command = "cut" then
                  Cut_Clipboard (Source);
                  External_End_Action (Get_Buffer (Source));
               elsif Command = "copy" then
                  Copy_Clipboard (Source);
               elsif Command = "paste" then
                  Paste_Clipboard (Source);
                  External_End_Action (Get_Buffer (Source));
               else
                  Select_All (Get_Buffer (Source));
               end if;
            end if;
         end;

      elsif Command = "select_text" then
         Name_Parameters (Data, Select_Text_Cmd_Parameters);

         declare
            Child        : constant MDI_Child := Find_Current_Editor (Kernel);
            Buffer       : Source_Buffer;
            First_Line   : constant Natural := Nth_Arg (Data, 1);
            Start_Column : constant Natural := Nth_Arg (Data, 3, Default => 1);
            Last_Line    : Natural := Nth_Arg (Data, 2);
            End_Column   : Natural := Nth_Arg (Data, 4, Default => 0);
         begin
            if Child /= null then
               if End_Column = 0 then
                  --  End column not specified, in this case select the
                  --  whole line
                  End_Column := 1;
                  Last_Line  := Last_Line + 1;
               end if;

               Buffer := Get_Buffer (Source_Box (Get_Widget (Child)).Editor);

               if Is_Valid_Position
                 (Buffer, Gint (First_Line - 1), Gint (Start_Column - 1))
               then
                  Select_Region
                    (Buffer,
                     Editable_Line_Type (First_Line),
                     Start_Column,
                     Editable_Line_Type (Last_Line),
                     End_Column);
               end if;
            end if;
         end;

      elsif Command = "close"
        or else Command = "undo"
        or else Command = "redo"
      then
         declare
            Filename : constant Virtual_File :=
              Create (Full_Filename => Nth_Arg (Data, 1));
         begin
            if Command = "close" then
               if Is_Absolute_Path (Filename) then
                  Close_File_Editors (Kernel, Filename);
               else
                  Close_File_Editors
                    (Kernel,
                     Create
                       (Get_Full_Path_From_File
                          (Get_Registry (Kernel).all,
                           Full_Name (Filename).all,
                           True, False)));
               end if;
            else
               declare
                  Child : MDI_Child;
                  Box   : Source_Box;
               begin
                  Child := Find_Editor (Kernel, Filename);

                  if Child = null then
                     Set_Error_Msg (Data, -"file not open");
                  else
                     Box := Source_Box (Get_Widget (Child));

                     if Command = "redo" then
                        Redo (Box.Editor);
                     elsif Command = "undo" then
                        Undo (Box.Editor);
                     end if;
                  end if;
               end;
            end if;
         end;

      elsif Command = "goto_mark" then
         declare
            Identifier  : constant String := Nth_Arg (Data, 1);
            Mark_Record : constant Mark_Identifier_Record :=
              Find_Mark (Identifier);

            Child       : constant MDI_Child :=
              Find_Editor (Kernel, Mark_Record.File);
         begin
            if Child /= null then
               Raise_Child (Child);
               Set_Focus_Child (Child);
               Grab_Focus (Source_Box (Get_Widget (Child)).Editor);

               --  If the Length is null, we set the length to 1, otherwise
               --  the cursor is not visible.

               Scroll_To_Mark
                 (Source_Box (Get_Widget (Child)).Editor,
                  Mark_Record.Mark,
                  Mark_Record.Length);

            else
               if Mark_Record.File /= VFS.No_File
                 and then Is_In_List
                 (Id.Unopened_Files, Full_Name (Mark_Record.File).all)
               then
                  Open_File_Editor (Kernel,
                                    Mark_Record.File,
                                    Mark_Record.Line,
                                    Mark_Record.Column,
                                    Mark_Record.Column + Mark_Record.Length);

                  --  At this point, Open_File_Editor should have caused the
                  --  propagation of the File_Edited signal, which provokes a
                  --  call to Fill_Marks in File_Edited_Cb.
                  --  Therefore the Mark_Record might not be valid beyond this
                  --  point.
               end if;
            end if;
         end;

      elsif Command = "delete_mark" then
         declare
            Identifier  : constant String := Nth_Arg (Data, 1);
            Mark_Record : constant Mark_Identifier_Record :=
              Find_Mark (Identifier);
            Node        : Mark_Identifier_List.List_Node;
            Prev        : Mark_Identifier_List.List_Node;
            Child       : constant MDI_Child :=
              Find_Editor (Kernel, Mark_Record.File);

            use Mark_Identifier_List;
         begin
            if Child /= null
              and then Mark_Record.Mark /= null
            then
               Delete_Mark
                 (Get_Buffer
                    (Source_Box (Get_Widget (Child)).Editor),
                  Mark_Record.Mark);
            end if;

            Node := First (Id.Stored_Marks);

            if Mark_Identifier_List.Data (Node).Id = Mark_Record.Id then
               Next (Id.Stored_Marks);
            else
               Prev := Node;
               Node := Next (Node);

               while Node /= Null_Node loop
                  if Mark_Identifier_List.Data (Node).Id
                    = Mark_Record.Id
                  then
                     Remove_Nodes (Id.Stored_Marks, Prev, Node);
                     exit;
                  end if;

                  Prev := Node;
                  Node := Next (Node);
               end loop;
            end if;
         end;

      elsif Command = "get_chars" then
         Name_Parameters (Data, Get_Chars_Args);

         declare
            File   : constant String  := Nth_Arg (Data, 1);
            Line   : constant Integer := Nth_Arg (Data, 2, 0);
            Column : constant Integer := Nth_Arg (Data, 3, 1);
            Before : constant Integer := Nth_Arg (Data, 4, Default => -1);
            After  : constant Integer := Nth_Arg (Data, 5, Default => -1);
            Child  : constant MDI_Child :=
              Find_Editor (Kernel, Create (File, Kernel));
         begin
            Set_Return_Value
              (Data,
               Get_Chars
                 (Get_Buffer (Source_Box (Get_Widget (Child)).Editor),
                  Editable_Line_Type (Line),
                  Natural (Column),
                  Before, After));
         end;

      elsif Command = "replace_text" then
         declare
            File   : constant String  := Nth_Arg (Data, 1);
            Line   : constant Integer := Nth_Arg (Data, 2);
            Column : constant Integer := Nth_Arg (Data, 3);
            Text   : constant String  := Nth_Arg (Data, 4);
            Before : constant Integer := Nth_Arg (Data, 5, Default => -1);
            After  : constant Integer := Nth_Arg (Data, 6, Default => -1);
            Editor : constant Source_Box := Open_File
              (Kernel, Create (File, Kernel), Create_New => False);
         begin
            if Editor /= null then
               if Get_Writable (Editor.Editor) then
                  Replace_Slice
                    (Get_Buffer (Editor.Editor),
                     Text,
                     Editable_Line_Type (Line), Natural (Column),
                     Before, After);
               else
                  Set_Error_Msg
                    (Data,
                     -("Attempting to edit a non-writable file: ") & File);
               end if;
            else
               Set_Error_Msg (Data, -"file not open");
            end if;

         end;

      elsif Command = "insert_text" then
         declare
            Child  : constant MDI_Child := Find_Current_Editor (Kernel);
            Buffer : Source_Buffer;
            Text   : constant String  := Nth_Arg (Data, 1);
            Line   : Editable_Line_Type;
            Column : Positive;
         begin
            if Child /= null then
               Buffer := Get_Buffer (Source_Box (Get_Widget (Child)).Editor);

               Get_Cursor_Position (Buffer, Line, Column);
               Insert (Buffer, Line, Column, Text);
            end if;
         end;

      elsif Command = "get_line"
        or else Command = "get_column"
        or else Command = "get_file"
      then
         declare
            Identifier  : constant String := Nth_Arg (Data, 1);
            Mark_Record : constant Mark_Identifier_Record :=
              Find_Mark (Identifier);
            Buffer      : Source_Buffer;
            Child       : constant MDI_Child :=
              Find_Editor (Kernel, Mark_Record.File);
         begin
            if Mark_Record.File = VFS.No_File then
               Set_Error_Msg (Data, -"mark not found");
            else
               if Child /= null then
                  Buffer := Get_Buffer
                    (Source_Box (Get_Widget (Child)).Editor);
               end if;

               if Command = "get_line" then
                  if Buffer /= null then
                     Set_Return_Value
                       (Data,
                        Integer (Src_Editor_Buffer.Line_Information.Get_Line
                                   (Buffer, Mark_Record.Mark)));
                  else
                     Set_Return_Value (Data, Mark_Record.Line);
                  end if;
               elsif Command = "get_column" then
                  if Buffer /= null then
                     Set_Return_Value
                       (Data,
                        Src_Editor_Buffer.Line_Information.Get_Column
                          (Buffer, Mark_Record.Mark));
                  else
                     Set_Return_Value (Data, Mark_Record.Column);
                  end if;
               else
                  Set_Return_Value (Data, Full_Name (Mark_Record.File).all);
               end if;
            end if;
         end;

      elsif Command = "get_last_line" then
         declare
            File  : constant Virtual_File :=
              Create (Nth_Arg (Data, 1), Kernel);
            Child : constant MDI_Child := Find_Editor (Kernel, File);
         begin
            if Child = null then
               declare
                  A : GNAT.OS_Lib.String_Access := Read_File (File);
                  N : Natural := 0;
               begin
                  if A /= null then
                     for J in A'Range loop
                        if A (J) = ASCII.LF then
                           N := N + 1;
                        end if;
                     end loop;

                     Free (A);

                     if N = 0 then
                        N := 1;
                     end if;

                     Set_Return_Value (Data, N);
                  else
                     Set_Error_Msg (Data, -"file not found or not opened");
                  end if;
               end;
            else
               Set_Return_Value
                 (Data,
                  Get_Last_Line (Source_Box (Get_Widget (Child)).Editor));
            end if;
         end;

      elsif Command = "block_get_start"
        or else Command = "block_get_end"
        or else Command = "block_get_name"
        or else Command = "block_get_type"
        or else Command = "block_get_level"
        or else Command = "subprogram_name"
      then
         declare
            File   : constant Virtual_File :=
              Create (Nth_Arg (Data, 1), Kernel);
            Child  : constant MDI_Child := Find_Editor (Kernel, File);
            Line   : constant Editable_Line_Type :=
              Editable_Line_Type (Natural'(Nth_Arg (Data, 2)));

         begin
            if Child = null then
               Set_Error_Msg
                 (Data,
                    -("Attempting to get block information for non" &
                      " open file : ") & Base_Name (File));
            else
               if Command = "block_get_start" then
                  Set_Return_Value
                    (Data,
                     Get_Block_Start
                       (Source_Box (Get_Widget (Child)).Editor, Line));
               elsif Command = "block_get_end" then
                  Set_Return_Value
                    (Data,
                     Get_Block_End
                       (Source_Box (Get_Widget (Child)).Editor, Line));
               elsif Command = "block_get_name" then
                  Set_Return_Value
                    (Data,
                     Get_Block_Name
                       (Source_Box (Get_Widget (Child)).Editor, Line));
               elsif Command = "block_get_type" then
                  Set_Return_Value
                    (Data,
                     Get_Block_Type
                       (Source_Box (Get_Widget (Child)).Editor, Line));
               elsif Command = "block_get_level" then
                  Set_Return_Value
                    (Data,
                     Get_Block_Level
                       (Source_Box (Get_Widget (Child)).Editor, Line));
               else
                  --  subprogram_name
                  Set_Return_Value
                    (Data,
                     Get_Subprogram_Name
                       (Source_Box (Get_Widget (Child)).Editor, Line));
               end if;
            end if;
         end;

      elsif Command = "cursor_get_line"
        or else Command = "cursor_get_column"
      then
         declare
            File  : constant Virtual_File :=
              Create (Nth_Arg (Data, 1), Kernel);
            Child : constant MDI_Child := Find_Editor (Kernel, File);
         begin
            if Child = null then
               Set_Error_Msg
                 (Data,
                    -("Attempting to get cursor position for non open file: ")
                  & Base_Name (File));
            else
               declare
                  Line   : Editable_Line_Type;
                  Column : Positive;
               begin
                  Get_Cursor_Position
                    (Get_Buffer
                       (Source_Box (Get_Widget (Child)).Editor), Line, Column);

                  if Command = "cursor_get_line" then
                     Set_Return_Value (Data, Integer (Line));
                  else
                     Set_Return_Value (Data, Column);
                  end if;
               end;
            end if;
         end;

      elsif Command = "cursor_set_position" then
         declare
            File   : constant Virtual_File :=
              Create (Nth_Arg (Data, 1), Kernel);
            Child  : constant MDI_Child := Find_Editor (Kernel, File);
            Line   : constant Editable_Line_Type :=
              Editable_Line_Type (Integer'(Nth_Arg (Data, 2)));
            Column : Natural := Nth_Arg (Data, 3, Default => 0);
         begin
            if Child = null then
               Set_Error_Msg
                 (Data,
                    -("Attempting to set cursor position for non open file: ")
                  & Base_Name (File));
            else
               if Column = 0 then
                  --  Column has not been specified, set it to the first non
                  --  white space character.
                  --  Do we really always want this behavior ???

                  declare
                     Chars : constant String :=
                       Get_Chars
                         (Get_Buffer (Source_Box (Get_Widget (Child)).Editor),
                          Line);
                  begin
                     --  Set the column to 1, if line is empty we want to set
                     --  the cursor on the first column.

                     Column := 1;

                     for K in Chars'Range loop
                        Column := K;
                        exit when Chars (K) /= ' '
                          and then Chars (K) /= ASCII.HT;
                     end loop;

                     if Column /= 1 then
                        --  Adjust column number.
                        Column := Column - Chars'First + 1;
                     end if;
                  end;
               end if;

               Set_Cursor_Position
                 (Get_Buffer (Source_Box (Get_Widget (Child)).Editor),
                  Line, Column);
            end if;
         end;

      elsif Command = "cursor_center" then
         declare
            File   : constant Virtual_File :=
              Create (Nth_Arg (Data, 1), Kernel);
            Child  : constant MDI_Child := Find_Editor (Kernel, File);
         begin
            Scroll_To_Cursor_Location
              (Get_View (Source_Box (Get_Widget (Child)).Editor),
               Center => True);
         end;

      elsif Command = "get_buffer" then
         declare
            File  : constant Virtual_File :=
              Create (Nth_Arg (Data, 1), Kernel);
            Child : constant MDI_Child := Find_Editor (Kernel, File);
            A     : GNAT.OS_Lib.String_Access;

         begin
            if Child /= null then
               A := Src_Editor_Buffer.Get_String
                 (Get_Buffer (Source_Box (Get_Widget (Child)).Editor));

               Set_Return_Value (Data, A.all);

               Free (A);
            else
               --  The buffer is not currently open, read directly from disk.

               A := Read_File (File);

               if A /= null then
                  declare
                     Length        : constant Integer := A'Length;
                     Result_String : String (1 .. Length * 2 + 1);
                     Ignore, Bytes : Natural;
                  begin
                     Glib.Convert.Convert
                       (A.all,
                        "UTF-8", Get_Pref (Kernel, Default_Charset),
                        Ignore, Bytes, Result => Result_String);
                     Set_Return_Value (Data, Result_String (1 .. Bytes));
                  end;

                  Free (A);
               else
                  Set_Error_Msg (Data, -"file not found");
               end if;
            end if;
         end;

      elsif Command = "save_buffer" then
         declare
            File    : constant Virtual_File :=
              Create (Nth_Arg (Data, 1), Kernel);
            Child   : constant MDI_Child := Find_Editor (Kernel, File);
            To_File : Virtual_File := VFS.No_File;
            Result  : Boolean;
         begin
            if Number_Of_Arguments (Data) >= 2 then
               To_File := Create (Nth_Arg (Data, 2), Kernel);
            end if;

            if Child /= null then
               if To_File /= VFS.No_File then
                  Save_To_File
                    (Get_Buffer (Source_Box (Get_Widget (Child)).Editor),
                     To_File,
                     Result,
                     True);

               else
                  Save_To_File
                    (Get_Buffer (Source_Box (Get_Widget (Child)).Editor),
                     File,
                     Result,
                     False);
               end if;
            else
               Set_Error_Msg (Data, -"file not open");
            end if;
         end;

      elsif Command = "save" then
         Name_Parameters (Data, Save_Cmd_Parameters);
         declare
            Interactive : constant Boolean :=
              Nth_Arg (Data, 1, Default => True);
            All_Save : constant Boolean := Nth_Arg (Data, 2, Default => True);
            Child    : MDI_Child;
         begin
            if All_Save then
               if not Save_MDI_Children (Kernel, Force => not Interactive) then
                  Set_Error_Msg (Data, -"cancelled");
               end if;
            else
               Child := Find_Current_Editor (Kernel);
               if Child = null then
                  Set_Error_Msg (Data, -"no file selected");
               elsif not Save_MDI_Children
                 (Kernel, Children => (1 => Child), Force => not Interactive)
               then
                  Set_Error_Msg (Data, -"cancelled");
               end if;
            end if;
         end;

      elsif Command = "add_blank_lines" then
         declare
            Filename    : constant Virtual_File :=
              Create (Nth_Arg (Data, 1), Kernel);
            Line        : constant Integer := Nth_Arg (Data, 2);
            Number      : constant Integer := Nth_Arg (Data, 3);
            Child       : MDI_Child;
            Box         : Source_Box;
            Mark_Record : Mark_Identifier_Record;
            Highlight_Category : Natural := 0;
         begin
            Child := Find_Editor (Kernel, Filename);

            if Number_Of_Arguments (Data) >= 4 then
               Highlight_Category :=
                 Line_Highlighting.Lookup_Category (Nth_Arg (Data, 4));
            end if;

            if Child /= null then
               Box := Source_Box (Get_Widget (Child));

               if Line >= 0 and then Number > 0 then
                  --  Create a new mark record and insert it in the list.
                  Mark_Record.Line := 0;
                  Mark_Record.File := Filename;
                  Mark_Record.Id := Id.Next_Mark_Id;

                  Id.Next_Mark_Id := Id.Next_Mark_Id + 1;
                  Mark_Record.Length := 0;
                  Mark_Record.Mark :=
                    Add_Blank_Lines
                      (Get_Buffer (Box.Editor),
                       Editable_Line_Type (Line),
                       Highlight_Category, "", Number);
                  Mark_Identifier_List.Append (Id.Stored_Marks, Mark_Record);
                  Set_Return_Value (Data, Image (Mark_Record.Id));
               end if;
            else
               Set_Error_Msg (Data, -"file not open");
            end if;
         end;

      elsif Command = "remove_blank_lines" then
         declare
            Identifier  : constant String := Nth_Arg (Data, 1);
            Mark_Record : constant Mark_Identifier_Record :=
              Find_Mark (Identifier);
            Child       : MDI_Child;
            Number      : Integer := 0;
            Box         : Source_Box;
         begin
            Child := Find_Editor (Kernel, Mark_Record.File);

            if Number_Of_Arguments (Data) >= 3 then
               Number := Nth_Arg (Data, 2);
            end if;

            if Child /= null then
               Box := Source_Box (Get_Widget (Child));

               Src_Editor_Buffer.Line_Information.Remove_Blank_Lines
                 (Get_Buffer (Box.Editor), Mark_Record.Mark, Number);
            else
               Set_Error_Msg (Data, -"file not found or not open");
            end if;
         end;

      elsif Command = "block_fold" then
         declare
            Filename    : constant Virtual_File :=
              Create (Nth_Arg (Data, 1), Kernel);
            Line        : constant Integer := Nth_Arg (Data, 2, 0);
            Child       : MDI_Child;
            Box         : Source_Box;
         begin
            Child := Find_Editor (Kernel, Filename);

            if Child /= null then
               Box := Source_Box (Get_Widget (Child));

               if Line = 0 then
                  Src_Editor_Buffer.Line_Information.Fold_All
                    (Get_Buffer (Box.Editor));
               else
                  Src_Editor_Buffer.Line_Information.Fold_Block
                    (Get_Buffer (Box.Editor), Editable_Line_Type (Line));
               end if;
            else
               Set_Error_Msg (Data, -"file not found or not open");
            end if;
         end;

      elsif Command = "block_unfold" then
         declare
            Filename    : constant Virtual_File :=
              Create (Nth_Arg (Data, 1), Kernel);
            Line        : constant Integer := Nth_Arg (Data, 2, 0);
            Child       : MDI_Child;
            Box         : Source_Box;
         begin
            Child := Find_Editor (Kernel, Filename);

            if Child /= null then
               Box := Source_Box (Get_Widget (Child));

               if Line = 0 then
                  Src_Editor_Buffer.Line_Information.Fold_All
                    (Get_Buffer (Box.Editor));
               else
                  Src_Editor_Buffer.Line_Information.Unfold_Line
                    (Get_Buffer (Box.Editor), Editable_Line_Type (Line));
               end if;
            else
               Set_Error_Msg (Data, -"file not found or not open");
            end if;
         end;

      elsif Command = "set_background_color" then
         declare
            Filename    : constant Virtual_File :=
              Create (Nth_Arg (Data, 1), Kernel);
            Color       : constant String := Nth_Arg (Data, 2);
            Box         : Source_Box;
            Child       : MDI_Child;
         begin
            Child := Find_Editor (Kernel, Filename);

            if Child /= null then
               Box := Source_Box (Get_Widget (Child));
               Modify_Base
                 (Get_View (Box.Editor), State_Normal, Parse (Color));
            end if;
         end;

      elsif Command = "set_synchronized_scrolling" then
         declare
            Filename_1 : constant Virtual_File :=
              Create (Nth_Arg (Data, 1), Kernel);
            Filename_2 : constant Virtual_File :=
              Create (Nth_Arg (Data, 2), Kernel);
            Child_1    : MDI_Child;
            Child_2    : MDI_Child;
            use Child_Triplet_Callback;
            Triplet    : Child_Triplet_Access;
         begin
            Child_1 := Find_Editor (Kernel, Filename_1);
            Child_2 := Find_Editor (Kernel, Filename_2);

            if Child_1 /= null and then Child_2 /= null then
               Triplet := new Child_Triplet'(Child_1, Child_2, null);

               Set_Synchronized_Editor
                 (Get_View (Source_Box (Get_Widget (Child_1)).Editor),
                  Get_View (Source_Box (Get_Widget (Child_2)).Editor));

               if Number_Of_Arguments (Data) > 2 then
                  declare
                     Filename_3 : constant Virtual_File :=
                       Create (Nth_Arg (Data, 3), Kernel);
                     Child_3 : constant MDI_Child :=
                       Find_Editor (Kernel, Filename_3);
                  begin
                     if Child_3 /= null then
                        Set_Synchronized_Editor
                          (Get_View
                             (Source_Box (Get_Widget (Child_2)).Editor),
                           Get_View
                             (Source_Box (Get_Widget (Child_3)).Editor));

                        Set_Synchronized_Editor
                          (Get_View
                             (Source_Box (Get_Widget (Child_3)).Editor),
                           Get_View
                             (Source_Box (Get_Widget (Child_1)).Editor));
                     end if;

                     Triplet (3) := Child_3;
                  end;

               else
                  Set_Synchronized_Editor
                    (Get_View (Source_Box (Get_Widget (Child_2)).Editor),
                     Get_View (Source_Box (Get_Widget (Child_1)).Editor));
               end if;

               for C in Triplet'Range loop
                  if Triplet (C) /= null then
                     Connect
                       (Triplet (C), "grab_focus",
                        Marshallers.Void_Marshaller.To_Marshaller
                          (On_Raise_Child'Access),
                        User_Data => Triplet);
                     Connect
                       (Triplet (C), "destroy",
                        Marshallers.Void_Marshaller.To_Marshaller
                          (On_Delete_Child'Access),
                        User_Data => Triplet);
                  end if;
               end loop;
            end if;
         end;

      elsif Command = "add_case_exception"
        or else Command = "remove_case_exception"
      then
         Name_Parameters (Data, Case_Exception_Cmd_Parameters);

         declare
            Name : constant String := Nth_Arg (Data, 1);
         begin
            if Command = "add_case_exception" then
               Add_Exception (Name);
            else
               Remove_Exception (Name);
            end if;
         end;

      elsif Command = "set_writable" then
         Name_Parameters (Data, Set_Writable_Cmd_Parameters);

         declare
            Filename : constant Virtual_File :=
              Create (Nth_Arg (Data, 1), Kernel);
            Write    : constant Boolean := Nth_Arg (Data, 2);
            Child    : MDI_Child;
         begin
            Child := Find_Editor (Kernel, Filename);

            if Child /= null then
               Set_Writable (Source_Box (Get_Widget (Child)).Editor, Write);
            end if;
         end;
      end if;
   end Edit_Command_Handler;

   ----------------
   -- Fill_Marks --
   ----------------

   procedure Fill_Marks
     (Kernel : Kernel_Handle;
      File   : VFS.Virtual_File)
   is
      Id : constant Source_Editor_Module :=
        Source_Editor_Module (Src_Editor_Module_Id);

      use Mark_Identifier_List;

      Box         : Source_Box;
      Child       : MDI_Child;
      Node        : List_Node;
      Mark_Record : Mark_Identifier_Record;
   begin
      if Is_In_List (Id.Unopened_Files, Full_Name (File).all) then
         Child := Find_Editor (Kernel, File);

         if Child = null then
            return;
         end if;

         Box := Source_Box (Get_Widget (Child));
         Remove_From_List (Id.Unopened_Files, Full_Name (File).all);

         Node := First (Id.Stored_Marks);

         while Node /= Null_Node loop
            Mark_Record := Data (Node);

            if Mark_Record.File = File then
               Set_Data
                 (Node,
                  Mark_Identifier_Record'
                    (Id     => Mark_Record.Id,
                     File   => File,
                     Line   => Mark_Record.Line,
                     Mark   =>
                       Create_Mark
                         (Get_Buffer (Box.Editor),
                          Editable_Line_Type (Mark_Record.Line),
                          Mark_Record.Column),
                     Column => Mark_Record.Column,
                     Length => Mark_Record.Length));
            end if;

            Node := Next (Node);
         end loop;
      end if;
   end Fill_Marks;

   --------------------
   -- File_Edited_Cb --
   --------------------

   procedure File_Edited_Cb
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      D : constant File_Hooks_Args := File_Hooks_Args (Data.all);
   begin
      Fill_Marks (Kernel_Handle (Kernel), D.File);
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
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
      Box   : Source_Box;
      Dummy : Boolean;
      pragma Unreferenced (Dummy);
   begin
      loop
         Child := Get (Iter);

         exit when Child = null;

         if Get_Widget (Child).all in Source_Box_Record'Class then
            Box := Source_Box (Get_Widget (Child));

            if D.File = VFS.No_File
              or else D.File = Get_Filename (Box.Editor)
            then
               Dummy := Check_Timestamp_And_Reload
                 (Box.Editor,
                  Interactive   => False,
                  Always_Reload => False);

               Check_Writable (Box.Editor);
            end if;
         end if;

         Next (Iter);
      end loop;
   end File_Changed_On_Disk_Cb;

   --------------------
   -- File_Closed_Cb --
   --------------------

   procedure File_Closed_Cb
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      use Mark_Identifier_List;

      D           : constant File_Hooks_Args := File_Hooks_Args (Data.all);
      Id          : constant Source_Editor_Module :=
                      Source_Editor_Module (Src_Editor_Module_Id);
      Node        : List_Node;
      Mark_Record : Mark_Identifier_Record;
      Added       : Boolean := False;
      Box         : Source_Box;
      Child       : MDI_Child;

   begin
      --  If the file has marks, store their location.

      if Id = null then
         Node := Null_Node;
      else
         Node := First (Id.Stored_Marks);
      end if;

      while Node /= Null_Node loop
         Mark_Record := Mark_Identifier_List.Data (Node);
         if Mark_Record.File = D.File then
            if Mark_Record.Mark /= null
              and then Mark_Record.Line /= 0
            then
               Child := Find_Editor (Kernel, Mark_Record.File);
               Box := Source_Box (Get_Widget (Child));

               Mark_Record.Line :=
                 Natural (Src_Editor_Buffer.Line_Information.Get_Line
                            (Get_Buffer (Box.Editor), Mark_Record.Mark));
               Mark_Record.Column :=
                 Src_Editor_Buffer.Line_Information.Get_Column
                   (Get_Buffer (Box.Editor), Mark_Record.Mark);

               Set_Data
                 (Node,
                  Mark_Identifier_Record'
                    (Id     => Mark_Record.Id,
                     File   => D.File,
                     Line   => Mark_Record.Line,
                     Mark   => null,
                     Column => Mark_Record.Column,
                     Length => Mark_Record.Length));

               if not Added then
                  Add_Unique_Sorted
                    (Id.Unopened_Files, Full_Name (D.File).all);
                  Added := True;
               end if;
            end if;
         end if;

         Node := Next (Node);
      end loop;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end File_Closed_Cb;

   -------------------
   -- File_Saved_Cb --
   -------------------

   procedure File_Saved_Cb
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      D    : constant File_Hooks_Args := File_Hooks_Args (Data.all);
   begin
      --  Insert the saved file in the Recent menu.

      if D.File /= VFS.No_File
        and then not Is_Auto_Save (D.File)
      then
         Add_To_Recent_Menu (Kernel, D.File);
      end if;
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end File_Saved_Cb;

   ---------------------
   -- On_Delete_Child --
   ---------------------

   procedure On_Delete_Child
     (Child   : access Gtk_Widget_Record'Class;
      Triplet : Child_Triplet_Access)
   is
      All_Null : Boolean := True;
   begin
      for C in Triplet'Range loop
         if Triplet (C) = MDI_Child (Child) then
            Triplet (C) := null;
         end if;

         if Triplet (C) /= null then
            All_Null := False;
         end if;
      end loop;

      if All_Null then
         --  All editors in Triplet are closed: free memory associated to it
         declare
            X : Child_Triplet_Access := Triplet;
         begin
            Unchecked_Free (X);
         end;
      end if;
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Delete_Child;

   --------------------
   -- On_Raise_Child --
   --------------------

   procedure On_Raise_Child
     (Child   : access Gtk_Widget_Record'Class;
      Triplet : Child_Triplet_Access) is
   begin
      for C in Triplet'Range loop
         if Triplet (C) /= null
           and then Triplet (C) /= MDI_Child (Child)
           and then not Is_Floating (Triplet (C))
           and then not Is_Raised (Triplet (C))
           and then Get_Parent (Triplet (C)) /= Get_Parent (Child)
         then
            Raise_Child (Triplet (C), False);
         end if;
      end loop;
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Raise_Child;

   -----------------------
   -- Cursor_Stopped_Cb --
   -----------------------

   procedure Cursor_Stopped_Cb
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      D      : constant File_Location_Hooks_Args_Access :=
        File_Location_Hooks_Args_Access (Data);
      Id     : constant Source_Editor_Module :=
        Source_Editor_Module (Src_Editor_Module_Id);
      Box    : Source_Editor_Box;

   begin
      if Id.Show_Subprogram_Names then
         Box := Get_Source_Box_From_MDI (Find_Editor (Kernel, D.File));

         if Box /= null then
            Show_Subprogram_Name (Box, Get_Subprogram_Name (Box));
         end if;
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Cursor_Stopped_Cb;

   ---------------------
   -- Delete_Callback --
   ---------------------

   function Delete_Callback
     (Widget : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues) return Boolean
   is
      pragma Unreferenced (Params);
      Kernel : constant Kernel_Handle :=
        Get_Kernel (Source_Box (Widget).Editor);
   begin
      --  ??? How come we need to access this low-level information ?
      return Get_Ref_Count (Get_Buffer (Source_Box (Widget).Editor)) = 1
        and then not Save_MDI_Children
          (Kernel,
           Children => (1 => Find_MDI_Child (Get_MDI (Kernel), Widget)),
           Force => False);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         return False;
   end Delete_Callback;

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
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         return False;
   end File_Edit_Callback;

   ------------------
   -- Load_Desktop --
   ------------------

   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child
   is
      Src    : Source_Box := null;
      File   : Glib.String_Ptr;
      F      : Virtual_File;
      Str    : Glib.String_Ptr;
      Id     : Idle_Handler_Id;
      Line   : Positive := 1;
      Column : Positive := 1;
      Child  : MDI_Child;
      pragma Unreferenced (Id, MDI);

      Dummy  : Boolean;
      pragma Unreferenced (Dummy);
   begin
      if Node.Tag.all = "Source_Editor" then
         File := Get_Field (Node, "File");

         if File /= null and then File.all /= "" then
            Str := Get_Field (Node, "Line");

            if Str /= null then
               Line := Positive'Value (Str.all);
            end if;

            Str := Get_Field (Node, "Column");

            if Str /= null then
               Column := Positive'Value (Str.all);
            end if;

            F := Create (Full_Filename => File.all);
            if not Is_Open (User, F) then
               Src := Open_File (User, F, False);
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
               Dummy := File_Edit_Callback
                 ((Src.Editor,
                   Editable_Line_Type (Line),
                   Column, 0, User, False));

               --  Add the location in the navigations button.
               declare
                  Args : Argument_List :=
                    (new String'("Editor.edit"),
                     new String'(Full_Name (F).all),
                     new String'(Image (Line)),
                     new String'(Image (Column)));
               begin
                  Execute_GPS_Shell_Command
                    (User, "add_location_command", Args);
                  Free (Args);
               end;
            end if;
         end if;
      end if;

      return Child;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         return null;
   end Load_Desktop;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Hook   : Lines_Revealed_Hook_Record;
      Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Data   : access GPS.Kernel.Hooks.Hooks_Data'Class)
   is
      pragma Unreferenced (Hook);
      D : constant Context_Hooks_Args := Context_Hooks_Args (Data.all);
      Area_Context : File_Area_Context_Access;
      Infos        : Line_Information_Data;
      Line1, Line2 : Integer;

   begin
      if D.Context.all in File_Area_Context'Class then
         Area_Context := File_Area_Context_Access (D.Context);

         Get_Area (Area_Context, Line1, Line2);

         --  ??? This is probably unnecessary if not Has_File_Information
         --  (Area_Context), see below.
         Infos := new Line_Information_Array (Line1 .. Line2);

         for J in Infos'Range loop
            Infos (J).Text := new String'(Image (J));
         end loop;

         if Has_File_Information (Area_Context) then
            Add_Line_Information
              (Kernel,
               File_Information (Area_Context),
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
      pragma Unreferenced (User);
      N, Child     : Node_Ptr;
      Line, Column : Positive;
      Editor       : Source_Editor_Box;

   begin
      if Widget.all in Source_Box_Record'Class then
         Editor := Source_Box (Widget).Editor;

         declare
            Filename : constant String :=
              Full_Name (Get_Filename (Editor)).all;
         begin
            if Filename = ""
              or else not Is_Regular_File (Get_Filename (Editor))
            then
               return null;
            end if;

            N := new Node;
            N.Tag := new String'("Source_Editor");

            Child := new Node;
            Child.Tag := new String'("File");
            Child.Value := new String'(Filename);
            Add_Child (N, Child);

            Get_Cursor_Location (Editor, Line, Column);

            Child := new Node;
            Child.Tag := new String'("Line");
            Child.Value := new String'(Image (Line));
            Add_Child (N, Child);

            Child := new Node;
            Child.Tag := new String'("Column");
            Child.Value := new String'(Image (Column));
            Add_Child (N, Child);

            Child := new Node;
            Child.Tag := new String'("Column_End");
            Child.Value := new String'(Image (Column));
            Add_Child (N, Child);

            return N;
         end;
      end if;

      return null;
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
         return Source_Box (Get_Widget (Child)).Editor;
      end if;
   end Get_Source_Box_From_MDI;

   -------------------------
   -- Find_Current_Editor --
   -------------------------

   function Find_Current_Editor
     (Kernel : access Kernel_Handle_Record'Class) return MDI_Child is
   begin
      return Find_MDI_Child_By_Tag (Get_MDI (Kernel), Source_Box_Record'Tag);
   end Find_Current_Editor;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Box    : out Source_Box;
      Editor : Source_Editor_Box) is
   begin
      Box := new Source_Box_Record;
      Initialize (Box, Editor);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Box    : access Source_Box_Record'Class;
      Editor : Source_Editor_Box) is
   begin
      Gtk.Box.Initialize_Hbox (Box);
      Box.Editor := Editor;
   end Initialize;

   ---------------------------
   -- Update_Cache_On_Focus --
   ---------------------------

   procedure Update_Cache_On_Focus
     (Child : access Gtk_Widget_Record'Class)
   is
      Id    : constant Source_Editor_Module :=
        Source_Editor_Module (Src_Editor_Module_Id);
      Box : constant Source_Editor_Box :=
        Get_Source_Box_From_MDI (MDI_Child (Child));
   begin
      --  Update the cache, so that the view is used when possible, since it
      --  was the last open in any case
      Editors_Hash.Set
        (Id.Editors, Get_Filename (Box), (Child => MDI_Child (Child)));
   end Update_Cache_On_Focus;

   --------------
   -- New_View --
   --------------

   function New_View
     (Kernel  : access Kernel_Handle_Record'Class;
      Current : Source_Editor_Box) return Source_Box
   is
      Editor : Source_Editor_Box;
      Box    : Source_Box;
      Child  : MDI_Child;
   begin
      if Current = null then
         return null;
      end if;

      declare
         Title : constant Virtual_File := Get_Filename (Current);
      begin
         Create_New_View (Editor, Kernel, Current);
         Gtk_New (Box, Editor);
         Attach (Editor, Box);

         Child := new Editor_Child_Record;
         Initialize (Child, Box, All_Buttons);
         Child := Put
           (Kernel, Child,
            Focus_Widget   => Gtk_Widget (Get_View (Editor)),
            Default_Width  => Get_Pref (Kernel, Default_Widget_Width),
            Default_Height => Get_Pref (Kernel, Default_Widget_Height),
            Module         => Src_Editor_Module_Id);
         Set_Child (Get_View (Editor), Child);

         Widget_Callback.Connect
           (Child, "selected", Update_Cache_On_Focus'Access);

         Set_Icon (Child, Gdk_New_From_Xpm_Data (editor_xpm));
         Set_Focus_Child (Child);

         declare
            Im : constant String := Image
              (Get_Total_Ref_Count (Get_Buffer (Editor)));
         begin
            Set_Title
              (Child,
               Full_Name (Title).all & " <" & Im & ">",
               Base_Name (Title) & " <" & Im & ">");
         end;

         Gtkada.Handlers.Return_Callback.Object_Connect
           (Box,
            "delete_event",
            Delete_Callback'Access,
            Gtk_Widget (Box),
            After => False);

         File_Callback.Connect
           (Child, "destroy", On_Editor_Destroy'Access,
            User_Data => Title);
      end;

      return Box;
   end New_View;

   procedure New_View
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Current : constant Source_Editor_Box :=
        Get_Source_Box_From_MDI (Find_Current_Editor (Kernel));
      Box     : Source_Box;
      pragma Unreferenced (Box);

   begin
      if Current /= null then
         Box := New_View (Kernel, Current);
      end if;
   end New_View;

   -------------------
   -- Save_Function --
   -------------------

   function Save_Function
     (Kernel : access Kernel_Handle_Record'Class;
      Child  : Gtk.Widget.Gtk_Widget;
      Mode   : Save_Function_Mode) return Boolean
   is
      pragma Unreferenced (Kernel);
      Success        : Boolean;
      Containing_Box : constant Source_Box := Source_Box (Child);
      Box            : constant Source_Editor_Box := Containing_Box.Editor;
   begin
      case Mode is
         when Query =>
            return Needs_To_Be_Saved (Box);

         when Action =>
            if Needs_To_Be_Saved (Box) then
               Save_To_File (Box, Success => Success);
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
      File       : VFS.Virtual_File;
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

         if File = VFS.No_File then
            Is_Writable := True;
         else
            Writable := Write_File (File);
            Is_Writable := Writable /= Invalid_File;

            if Writable /= Invalid_File then
               begin
                  Close (Writable);
                  Delete (File);
               exception
                  when Use_Error =>
                     Is_Writable := False;
               end;
            end if;
         end if;

         if Is_Writable then
            Gtk_New (Editor, Kernel_Handle (Kernel));
            Load_Empty_File (Editor, File, Get_Language_Handler (Kernel));
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
      Add_To_History (Kernel, Hist_Key, Full_Name (File).all);
   end Add_To_Recent_Menu;

   ---------------
   -- Open_File --
   ---------------

   function Open_File
     (Kernel     : access Kernel_Handle_Record'Class;
      File       : VFS.Virtual_File := VFS.No_File;
      Create_New : Boolean := True;
      Focus      : Boolean := True;
      Force      : Boolean := False;
      Position   : Gtkada.MDI.Child_Position :=
        Gtkada.MDI.Position_Default) return Source_Box
   is
      No_Name : constant String := -"Untitled";
      MDI     : constant MDI_Window := Get_MDI (Kernel);
      Editor  : Source_Editor_Box;
      Box     : Source_Box;
      Child   : MDI_Child;
      Dummy   : Boolean;
      Id      : constant Source_Editor_Module :=
        Source_Editor_Module (Src_Editor_Module_Id);
      pragma Unreferenced (Dummy);

   begin
      Trace (Me, "Open file " & Full_Name (File).all
             & " Focus=" & Focus'Img);

      if File /= VFS.No_File then
         Child := Find_Editor (Kernel, File);

         if Child /= null then
            Dummy := Check_Timestamp_And_Reload
              (Source_Box (Get_Widget (Child)).Editor,
               Interactive   => False,
               Always_Reload => Force);

            Raise_Child (Child, Focus);

            return Source_Box (Get_Widget (Child));
         end if;
      end if;

      Editor := Create_File_Editor (Kernel, File, Create_New);

      --  If we have created an editor, put it into a box, and give it
      --  to the MDI to handle

      if Editor /= null then
         Gtk_New (Box, Editor);
         Attach (Editor, Box);

         Child := new Editor_Child_Record;
         Initialize (Child, Box, All_Buttons);
         Child := Put
           (Kernel, Child,
            Focus_Widget   => Gtk_Widget (Get_View (Editor)),
            Default_Width  => Get_Pref (Kernel, Default_Widget_Width),
            Default_Height => Get_Pref (Kernel, Default_Widget_Height),
            Module         => Src_Editor_Module_Id,
            Position       => Position);
         Set_Child (Get_View (Editor), Child);
         Set_Icon (Child, Gdk_New_From_Xpm_Data (editor_xpm));
         Widget_Callback.Connect
           (Child, "selected", Update_Cache_On_Focus'Access);

         --  Add child to the hash table of editors.
         Editors_Hash.Set (Id.Editors, File, (Child => Child));

         --  Make sure the entry in the hash table is removed when the editor
         --  is destroyed.

         File_Callback.Connect
           (Child, "destroy", On_Editor_Destroy'Access,
            User_Data => File);

         Raise_Child (Child, Focus);

         if File /= VFS.No_File then
            Set_Title (Child, Full_Name (File).all, Base_Name (File));
            File_Edited (Kernel, Get_Filename (Child));

         else
            --  Determine the number of "Untitled" files open.

            declare
               Iterator    : Child_Iterator := First_Child (MDI);
               The_Child   : MDI_Child;
               Nb_Untitled : Integer := -1;
               Ident       : Virtual_File;
            begin
               The_Child := Get (Iterator);

               while The_Child /= null loop
                  if The_Child /= Child
                    and then Get_Widget (The_Child).all in
                    Source_Box_Record'Class
                    and then Get_Filename (The_Child) = VFS.No_File
                  then
                     declare
                        Ident : constant String := Base_Name
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
                  Ident := Create (Full_Filename => '/' & No_Name);
               else
                  declare
                     Identifier : constant String :=
                       No_Name & " (" & Image (Nb_Untitled + 1) & ")";
                  begin
                     Set_Title (Child, Identifier);
                     Ident := Create (Full_Filename => '/' & Identifier);
                  end;
               end if;

               Set_File_Identifier (Get_Buffer (Editor), Ident);
               Set_Filename (Get_Buffer (Editor), Get_Filename (Child));
               File_Edited (Kernel, Ident);
            end;
         end if;

         Gtkada.Handlers.Return_Callback.Object_Connect
           (Box,
            "delete_event",
            Delete_Callback'Access,
            Gtk_Widget (Box),
            After => False);

         if File /= VFS.No_File then
            Add_To_Recent_Menu (Kernel, File);
         end if;

      else
         Console.Insert
           (Kernel, (-"Cannot open file ") & "'" & Full_Name (File).all & "'",
            Add_LF => True,
            Mode   => Error);
      end if;

      return Box;
   end Open_File;

   -----------------------
   -- Location_Callback --
   -----------------------

   function Location_Callback (D : Location_Idle_Data) return Boolean is
   begin
      if D.Line /= 0
        and then Is_Valid_Position (Get_Buffer (D.Edit), D.Line)
      then
         Set_Cursor_Location (D.Edit, D.Line, D.Column, D.Focus);

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

      return False;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         return False;
   end Location_Callback;

   ------------------
   -- Save_To_File --
   ------------------

   procedure Save_To_File
     (Kernel  : access Kernel_Handle_Record'Class;
      Name    : VFS.Virtual_File := VFS.No_File;
      Success : out Boolean)
   is
      Child  : constant MDI_Child := Find_Current_Editor (Kernel);
      Source : Source_Editor_Box;

   begin
      if Child = null then
         Success := False;
         return;
      end if;

      Source := Source_Box (Get_Widget (Child)).Editor;

      declare
         Old_Name : constant Virtual_File := Get_Filename (Source);
      begin
         Save_To_File (Source, Name, Success);

         declare
            New_Name : constant Virtual_File := Get_Filename (Source);
         begin
            --  Update the title, in case "save as..." was used.

            if Old_Name /= New_Name then
               Set_Title
                 (Child, Full_Name (New_Name).all, Base_Name (New_Name));
               Recompute_View (Kernel);
            end if;
         end;
      end;
   end Save_To_File;

   ------------------
   -- On_Open_File --
   ------------------

   procedure On_Open_File
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      declare
         Filename : constant Virtual_File :=
           Select_File
             (Title             => -"Open File",
              Parent            => Get_Current_Window (Kernel),
              Use_Native_Dialog => Get_Pref (Kernel, Use_Native_Dialogs),
              Kind              => Open_File,
              History           => Get_History (Kernel));

      begin
         if Filename /= VFS.No_File then
            Open_File_Editor (Kernel, Filename);
         end if;
      end;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Open_File;

   ----------------
   -- Completion --
   ----------------

   function Completion
     (Factory : File_Completion_Factory; Index : Positive) return String
   is
      File : VFS.Virtual_File;
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

      if File = VFS.No_File then
         return "@$#$";  --  Unlikely string, will not match any suffix
      else
         return Base_Name (File);
      end if;
   end Completion;

   -----------------
   -- Description --
   -----------------

   function Description
     (Factory : File_Completion_Factory; Index : Positive) return String
   is
      File : VFS.Virtual_File;
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

      if File = VFS.No_File then
         return "";
      else
         return Full_Name (File).all;
      end if;
   end Description;

   -----------------------
   -- On_Open_From_Path --
   -----------------------

   procedure On_Open_From_Path
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Label  : Gtk_Label;
      Button : Gtk_Widget;
      pragma Unreferenced (Widget, Button);

      Open_File_Dialog : Gtk_Dialog;
      Open_File_Entry  : Gtkada_Entry;
      Hist             : constant String_List_Access :=
        Get_History (Get_History (Kernel).all, Open_From_Path_History);
      List1 : File_Array_Access := Get_Source_Files
        (Project   => Get_Project (Kernel),
         Recursive => True);
      List2 : File_Array_Access :=
        Get_Predefined_Source_Files (Get_Registry (Kernel).all);
      Compl : File_Completion_Factory;

   begin
      Push_State (Kernel,  Busy);
      Gtk_New (Open_File_Dialog,
               Title  => -"Open file from project",
               Parent => Get_Current_Window (Kernel),
               Flags  => Modal or Destroy_With_Parent);
      Set_Default_Size (Open_File_Dialog, 300, 200);
      Set_Position (Open_File_Dialog, Win_Pos_Mouse);

      Gtk_New (Label, -"Enter file name (use <tab> for completion):");
      Pack_Start (Get_Vbox (Open_File_Dialog), Label, Expand => False);

      --  Do not use a combo box, so that users can easily navigate to the list
      --  of completions through the keyboard (C423-005)
      Gtk_New (Open_File_Entry,
               Use_Combo => False,
               Case_Sensitive => Filenames_Are_Case_Sensitive);
      Set_Activates_Default (Get_Entry (Open_File_Entry), True);
      Pack_Start (Get_Vbox (Open_File_Dialog), Open_File_Entry,
                  Fill => True, Expand => True);

      if Hist /= null then
         Set_Text (Get_Entry (Open_File_Entry),
                   Base_Name (Hist (Hist'First).all));
         Select_Region (Get_Entry (Open_File_Entry), 0, -1);
      end if;

      Button := Add_Button (Open_File_Dialog, Stock_Ok, Gtk_Response_OK);
      Button := Add_Button
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
            Text : constant String :=
              Get_Text (Get_Entry (Open_File_Entry));
            Full : Virtual_File;
         begin
            if Complet /= 0 then
               if Complet > List1'Length then
                  Full := List2 (List2'First + Complet - List1'Length - 1);
               else
                  Full := List1 (List1'First + Complet - 1);
               end if;
            else
               Full := Create (Text, Kernel, Use_Object_Path => False);
            end if;

            Add_To_History
              (Get_History (Kernel).all, Open_From_Path_History,
               Full_Name (Full).all);

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
      end if;

      Destroy (Open_File_Dialog);

      Unchecked_Free (List1);
      Unchecked_Free (List2);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Open_From_Path;

   --------------
   -- Activate --
   --------------

   procedure Activate (Callback : access On_Recent; Item : String) is
   begin
      Open_File_Editor (Callback.Kernel, Create (Full_Filename => Item));

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Activate;

   -----------------
   -- On_New_File --
   -----------------

   procedure On_New_File
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Editor : Source_Box;
      pragma Unreferenced (Widget, Editor);
   begin
      Editor := Open_File (Kernel, File => VFS.No_File);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_New_File;

   -------------
   -- On_Save --
   -------------

   procedure On_Save
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Success : Boolean;
   begin
      Save_To_File (Kernel, Success => Success);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
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
            New_Name : constant Virtual_File :=
              Select_File
                (Title             => -"Save File As",
                 Parent            => Get_Current_Window (Kernel),
                 Base_Directory    => Dir_Name (Old_Name).all,
                 Default_Name      => Base_Name (Old_Name),
                 Use_Native_Dialog => Get_Pref (Kernel, Use_Native_Dialogs),
                 Kind              => Save_File,
                 History           => Get_History (Kernel));

         begin
            if New_Name /= VFS.No_File then
               Save_To_File (Kernel, New_Name, Success);
            end if;
         end;
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Save_As;

   -----------------
   -- On_Save_All --
   -----------------

   procedure On_Save_All
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Ignore : Boolean;
      pragma Unreferenced (Widget, Ignore);

   begin
      Ignore := Save_MDI_Children (Kernel, Force => False);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Save_All;

   -------------------
   -- On_Change_Dir --
   -------------------

   procedure On_Change_Dir
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Dir    : constant String := Select_Directory
        (-"Select a directory",
         History => Get_History (Kernel),
         Parent  => Gtk_Window (Get_Current_Window (Kernel)));

   begin
      if Dir /= "" then
         Change_Dir (Dir);
      end if;
   end On_Change_Dir;

   ---------------------
   -- On_Save_Desktop --
   ---------------------

   procedure On_Save_Desktop
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      Save_Desktop (Kernel, As_Default_Desktop => False);
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Save_Desktop;

   -----------------------------
   -- On_Save_Default_Desktop --
   -----------------------------

   procedure On_Save_Default_Desktop
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      Save_Desktop (Kernel, As_Default_Desktop => True);
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Save_Default_Desktop;

   -------------
   -- Execute --
   -------------

   function Execute
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

   -------------
   -- On_Exit --
   -------------

   procedure On_Exit
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      GPS.Main_Window.Quit (GPS_Window (Get_Main_Window (Kernel)));

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Exit;

   --------------
   -- On_Print --
   --------------

   procedure On_Print
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      use Pango.Font, Pango.Enums;

      Success          : Boolean;
      Child            : constant MDI_Child := Find_Current_Editor (Kernel);
      Source           : Source_Editor_Box;
      Print_Helper     : constant String := Get_Pref (Kernel, Print_Command);
      Source_Font      : constant Pango_Font_Description :=
        Get_Pref_Font (Kernel, Default_Style);
      Source_Font_Name : constant String := Get_Family (Source_Font);
      Source_Font_Size : constant Gint := To_Pixels (Get_Size (Source_Font));

   begin
      if Get_Focus_Child (Get_MDI (Kernel)) /= Child then
         Console.Insert (Kernel, "No source file selected", Mode => Error);
         return;
      end if;

      Source := Get_Source_Box_From_MDI (Child);

      if Source = null then
         return;
      end if;

      if Print_Helper = "" then
         --  Use our internal facility

         Src_Printing.Print
           (Source,
            Font_Name  => Source_Font_Name,
            Font_Size  => Integer (Source_Font_Size),
            Bold       => False,
            Italicized => False);

      else
         --  Use helper

         if Save_MDI_Children
           (Kernel,
            Children => (1 => Child),
            Force    => Get_Pref (Kernel, Auto_Save))
         then
            declare
               Cmd : Argument_List_Access := Argument_String_To_List
                 (Print_Helper & " " & Full_Name (Get_Filename (Source)).all);
            begin
               Launch_Process
                 (Kernel,
                  Command   => Cmd (Cmd'First).all,
                  Arguments => Cmd (Cmd'First + 1 .. Cmd'Last),
                  Console   => Get_Console (Kernel),
                  Success   => Success);
               Free (Cmd);
            end;
         end if;
      end if;
   end On_Print;

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
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
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
                 Use_Native_Dialog => Get_Pref (Kernel, Use_Native_Dialogs),
                 Kind              => Open_File,
                 History           => Get_History (Kernel));
            Buffer : GNAT.OS_Lib.String_Access;
            Line   : Editable_Line_Type;
            Column : Natural;

         begin
            if F /= VFS.No_File then
               Buffer := Read_File (F);
               Get_Cursor_Position (Get_Buffer (Source), Line, Column);
               Insert (Get_Buffer (Source), Line, Column, Buffer.all);
               Free (Buffer);
            end if;
         end;
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
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
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
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
         Context => Entity_Selection_Context_Access
           (Default_Factory (Kernel, Editor)));

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
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
         Context => Entity_Selection_Context_Access
           (Default_Factory (Kernel, Editor)));

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Goto_Body;

   -----------------------
   -- Comment_Uncomment --
   -----------------------

   procedure Comment_Uncomment
     (Kernel : Kernel_Handle; Comment : Boolean)
   is
      Context      : constant Selection_Context_Access :=
        Get_Current_Context (Kernel);

      Area         : File_Area_Context_Access;
      File_Context : File_Selection_Context_Access;
      Start_Line   : Integer;
      End_Line     : Integer;

      use String_List_Utils.String_List;
   begin
      if Context /= null
        and then Context.all in File_Selection_Context'Class
        and then Has_File_Information
          (File_Selection_Context_Access (Context))
        and then Has_Directory_Information
          (File_Selection_Context_Access (Context))
      then
         File_Context := File_Selection_Context_Access (Context);

         declare
            Lang   : Language_Access;
            File   : constant Virtual_File := File_Information (File_Context);
            Block  : Unbounded_String := Null_Unbounded_String;
         begin
            if Context.all in File_Area_Context'Class then
               Area := File_Area_Context_Access (Context);
               Get_Area (Area, Start_Line, End_Line);

            elsif Context.all in Entity_Selection_Context'Class
              and then Has_Line_Information
                (Entity_Selection_Context_Access (Context))
            then
               Start_Line := Contexts.Line_Information
                 (Entity_Selection_Context_Access (Context));

               End_Line := Start_Line;
            else
               return;
            end if;

            Lang := Get_Language_From_File
              (Get_Language_Handler (Kernel), File);

            --  Create a String representing the selected block

            for J in Start_Line .. End_Line loop
               declare
                  Args : Argument_List :=
                    (1 => new String'(Full_Name (File).all),
                     2 => new String'(Image (J)),
                     3 => new String'("1"));
                  Line : constant String := Execute_GPS_Shell_Command
                    (Kernel, "Editor.get_chars", Args);
               begin
                  Free (Args);
                  Append (Block, Line);
               end;
            end loop;

            declare
               Args : Argument_List :=
                 (1 => new String'(Full_Name (File).all),
                  2 => new String'(Image (Start_Line)),
                  3 => new String'("1"), --  column
                  4 => new String'
                    (Comment_Block (Lang, To_String (Block), Comment)),
                  5 => new String'("0"), --  before
                  6 => new String'(Image (To_String (Block)'Length))); -- after
            begin
               Execute_GPS_Shell_Command
                    (Kernel, "Editor.replace_text", Args);
               Free (Args);
            end;
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
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
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
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
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
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
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
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
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
      Result  : Boolean;
      pragma Unreferenced (Result);
   begin
      if Current /= null then
         Result := Do_Refill (Get_Buffer (Current));
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Refill;

   ----------------------
   -- Source_File_Hook --
   ----------------------

   function Source_File_Hook
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class) return Boolean
   is
      D : constant Source_File_Hooks_Args := Source_File_Hooks_Args (Data.all);
      Iter        : Child_Iterator := First_Child (Get_MDI (Kernel));
      Child       : MDI_Child;
      No_Location : Boolean := False;
      Column      : Integer := D.Column;
      Source      : Source_Box;
      Edit        : Source_Editor_Box;
      Tmp         : Boolean;
      pragma Unreferenced (Tmp);
   begin
      if D.Line = -1 then
         --  Close all file editors corresponding to File.

         loop
            Child := Get (Iter);

            exit when Child = null;

            if Get_Widget (Child).all in Source_Box_Record'Class
              and then Get_Filename (Child) = D.File
            then
               Close_Child (Child);
            end if;

            Next (Iter);
         end loop;

         return True;

      else
         if D.Line = 0 and then D.Column = 0 then
            No_Location := True;
         end if;

         Source := Open_File
           (Kernel, D.File,
            Create_New => D.New_File,
            Focus      => D.Focus,
            Force      => D.Force_Reload,
            Position   => D.Position);

         if Source /= null then
            Edit := Source.Editor;
         end if;

         if Column = 0 then
            Column := 1;
         end if;

         if Edit /= null
           and then not No_Location
         then
            Trace (Me, "Setup editor to go to line,col="
                   & D.Line'Img & Column'Img);
            Tmp := Location_Callback
              ((Edit,
                Editable_Line_Type (D.Line),
                Natural (Column),
                Natural (D.Column_End),
                Kernel_Handle (Kernel),
                D.Focus));
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
      D : constant File_Line_Hooks_Args := File_Line_Hooks_Args (Data.all);
      Child : constant MDI_Child := Find_Editor (Kernel, D.File);
   begin
      if Child /= null then
         if D.Info'First = 0 then
            Create_Line_Information_Column
              (Source_Box (Get_Widget (Child)).Editor,
               D.Identifier,
               D.Every_Line);

         elsif D.Info'Length = 0 then
            Remove_Line_Information_Column
              (Source_Box (Get_Widget (Child)).Editor, D.Identifier);

         else
            Add_File_Information
              (Source_Box (Get_Widget (Child)).Editor, D.Identifier, D.Info);
         end if;
         return True;
      end if;

      return False;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         return False;
   end File_Line_Hook;

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
      if File_Data.File = VFS.No_File then
         return;
      end if;

      Buffer := Get_Buffer
        (Get_Source_Box_From_MDI (Find_Editor (Kernel, File_Data.File)));
      Autocase_Last_Word (Buffer);
   end Word_Added_Hook;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Edit_File_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      File : constant File_Selection_Context_Access :=
        File_Selection_Context_Access (Context.Context);
      Line : Natural;
   begin
      Trace (Me, "On_Edit_File: " & Full_Name (File_Information (File)).all);

      if Has_Line_Information (File) then
         Line := Contexts.Line_Information (File);
      else
         Line := 1;
      end if;

      Open_File_Editor
        (Get_Kernel (Context.Context),
         Filename  => File_Information (File),
         Line      => Line,
         Column    => Column_Information (File));
      return Success;
   end Execute;

   ---------------------
   -- Default_Factory --
   ---------------------

   function Default_Factory
     (Kernel : access Kernel_Handle_Record'Class;
      Editor : access Source_Editor_Box_Record'Class)
      return Selection_Context_Access is
   begin
      return Get_Contextual_Menu (Kernel, Editor, null, null);
   end Default_Factory;

   ---------------------
   -- Default_Factory --
   ---------------------

   function Default_Factory
     (Kernel : access Kernel_Handle_Record'Class;
      Child  : Gtk.Widget.Gtk_Widget) return Selection_Context_Access
   is
      C : constant Source_Box := Source_Box (Child);
   begin
      return Default_Factory (Kernel, C.Editor);
   end Default_Factory;

   -----------------------------
   -- Expand_Aliases_Entities --
   -----------------------------

   function Expand_Aliases_Entities
     (Kernel    : access Kernel_Handle_Record'Class;
      Expansion : String;
      Special   : Character) return String
   is
      Box          : Source_Editor_Box;
      W            : Gtk_Widget := Get_Current_Focus_Widget (Kernel);
      Line, Column : Positive;
   begin
      if W.all in Source_View_Record'Class then
         W := Get_Parent (W);
         while W.all not in Source_Box_Record'Class loop
            W := Get_Parent (W);
         end loop;
         Box := Source_Box (W).Editor;

         case Special is
            when 'l' =>
               Get_Cursor_Location (Box, Line, Column);
               return Expansion & Image (Line);

            when 'c' =>
               Get_Cursor_Location (Box, Line, Column);
               return Expansion & Image (Column);

            when 'f' =>
               return Expansion & Base_Name (Get_Filename (Box));

            when 'd' =>
               return Expansion & Dir_Name (Get_Filename (Box)).all;

            when 'p' =>
               return Expansion & Project_Name
                 (Get_Project_From_File
                  (Get_Registry (Kernel).all,
                   Get_Filename (Box),
                   Root_If_Not_Found => True));

            when 'P' =>
               return Expansion & Project_Path
                 (Get_Project_From_File
                  (Get_Registry (Kernel).all,
                   Get_Filename (Box),
                   Root_If_Not_Found => True));

            when others =>
               return Invalid_Expansion;
         end case;

      else
         return Invalid_Expansion;
      end if;
   end Expand_Aliases_Entities;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      File               : constant String := '/' & (-"File") & '/';
      Save               : constant String := File & (-"Save M_ore") & '/';
      Edit               : constant String := '/' & (-"Edit") & '/';
      Navigate           : constant String := '/' & (-"Navigate") & '/';
      Mitem              : Gtk_Menu_Item;
      Button             : Gtk_Button;
      Toolbar            : constant Gtk_Toolbar := Get_Toolbar (Kernel);
      UR                 : constant Undo_Redo := new Undo_Redo_Information;
      Selector           : Scope_Selector;
      Extra              : Files_Extra_Scope;
      Recent_Menu_Item   : Gtk_Menu_Item;
      Command            : Interactive_Command_Access;
      Editor_Class       : constant Class_Type := New_Class (Kernel, "Editor");
      Filter             : Action_Filter;
      Label              : Contextual_Menu_Label_Creator;
      Line_Numbers_Area_Filter : Action_Filter;

      Src_Action_Context : constant Action_Filter :=
        new Src_Editor_Action_Context;
      --  Memory is never freed, but this is needed for the whole life of
      --  the application

   begin
      Src_Editor_Module_Id := new Source_Editor_Module_Record;
      Register_Filter (Kernel, Src_Action_Context, "Source editor");

      Command := new Indentation_Command;
      Indentation_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Register_Action
        (Kernel, "Format selection",
         Command, -"Format the current line or selection",
         Src_Action_Context);
      Bind_Default_Key
        (Kernel      => Kernel,
         Action      => "Format selection",
         Default_Key => "control-Tab");

      Command := new Completion_Command;
      Completion_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Register_Action
        (Kernel, "Complete identifier", Command,
         -("Complete current identifier based on the contents of the editor"),
         Src_Action_Context);
      Bind_Default_Key
        (Kernel      => Kernel,
         Action      => "Complete identifier",
         Default_Key => "control-slash");

      Command := new Jump_To_Delimiter_Command;
      Jump_To_Delimiter_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Register_Action
        (Kernel, "Jump to matching delimiter", Command,
         -"Jump to the matching delimiter ()[]{}",
         Src_Action_Context);
      Bind_Default_Key
        (Kernel      => Kernel,
         Action      => "Jump to matching delimiter",
         Default_Key => "control-apostrophe");

      Command := new Move_Command;
      Move_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Move_Command (Command.all).Kind := Word;
      Move_Command (Command.all).Step := 1;
      Register_Action
        (Kernel, "Move to next word", Command,
           -"Move to the next word in the current source editor",
         Src_Action_Context);

      Command := new Move_Command;
      Move_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Move_Command (Command.all).Kind := Word;
      Move_Command (Command.all).Step := -1;
      Register_Action
        (Kernel, "Move to previous word", Command,
           -"Move to the previous word in the current source editor",
         Src_Action_Context);

      Command := new Move_Command;
      Move_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Move_Command (Command.all).Kind := Line;
      Move_Command (Command.all).Step := 1;
      Register_Action
        (Kernel, "Move to next line", Command,
           -"Move to the next line in the current source editor",
         Src_Action_Context);

      Command := new Move_Command;
      Move_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Move_Command (Command.all).Kind := Line;
      Move_Command (Command.all).Step := -1;
      Register_Action
        (Kernel, "Move to previous line", Command,
           -"Move to the previous line in the current source editor",
         Src_Action_Context);

      Command := new Move_Command;
      Move_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Move_Command (Command.all).Kind := Char;
      Move_Command (Command.all).Step := 1;
      Register_Action
        (Kernel, "Move to next character", Command,
           -"Move to the next character in the current source editor",
         Src_Action_Context);

      Command := new Move_Command;
      Move_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Move_Command (Command.all).Kind := Char;
      Move_Command (Command.all).Step := -1;
      Register_Action
        (Kernel, "Move to previous character", Command,
           -"Move to the previous character in the current source editor",
         Src_Action_Context);

      Command := new Move_Command;
      Move_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Move_Command (Command.all).Kind := Paragraph;
      Move_Command (Command.all).Step := -1;
      Register_Action
        (Kernel, "Move to previous sentence", Command,
           -"Move to the previous sentence in the current source editor",
         Src_Action_Context);

      Command := new Move_Command;
      Move_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Move_Command (Command.all).Kind := Paragraph;
      Move_Command (Command.all).Step := 1;
      Register_Action
        (Kernel, "Move to next sentence", Command,
           -"Move to the next sentence in the current source editor",
         Src_Action_Context);

      Command := new Scroll_Command;
      Scroll_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Register_Action
        (Kernel, "Center cursor on screen", Command,
           -"Scroll the current source editor so that the cursor is centered",
         Src_Action_Context);

      Command := new Delete_Command;
      Delete_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Delete_Command (Command.all).Kind := Word;
      Delete_Command (Command.all).Count := 1;
      Register_Action
        (Kernel, "Delete word forward", Command,
           -"Delete the word following the current cursor position",
         Src_Action_Context);

      Command := new Delete_Command;
      Delete_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Delete_Command (Command.all).Kind := Word;
      Delete_Command (Command.all).Count := -1;
      Register_Action
        (Kernel, "Delete word backward", Command,
           -"Delete the word preceding the current cursor position",
         Src_Action_Context);

      Line_Numbers_Area_Filter := new In_Line_Numbers_Area_Filter;

      Command := new Goto_Line_Command;
      Goto_Line_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Register_Contextual_Menu
        (Kernel, -"Goto line...",
         Action => Command,
         Filter => Line_Numbers_Area_Filter);

      Command := new Goto_Declaration_Command;
      Register_Contextual_Menu
        (Kernel, "Goto declaration of entity",
         Action     => Command,
         Label      => -"Goto declaration of %e",
         Ref_Item   => "Examine entity",
         Add_Before => True,
         Filter     => Action_Filter
           (not Line_Numbers_Area_Filter
            and Create (Module => Src_Editor_Module_Name)));

      Command := new Goto_Next_Body_Command;
      Filter  := new Has_Body_Filter;
      Label   := new Goto_Body_Menu_Label;
      Register_Contextual_Menu
        (Kernel, "Goto body of entity",
         Action     => Command,
         Label      => Label,
         Ref_Item   => "Goto declaration of entity",
         Add_Before => False,
         Filter     => Filter);

      Command := new Goto_Type_Command;
      Filter  := new Has_Type_Filter;
      Register_Contextual_Menu
        (Kernel, "Goto type of entity",
         Action     => Command,
         Label      => -"Goto type declaration of %e",
         Ref_Item   => "Goto body of entity",
         Add_Before => False,
         Filter     => Filter);

      Command := new Goto_Other_File_Command;
      Register_Contextual_Menu
        (Kernel, "Goto file spec<->body",
         Action     => Command,
         Label      => -"Goto file spec<->body",
         Ref_Item   => "Goto type of entity",
         Add_Before => False,
         Filter => Action_Filter
           (not Line_Numbers_Area_Filter
            and Create (Module => Src_Editor_Module_Name)));

      Register_Contextual_Submenu
        (Kernel, "References",
         Ref_Item   => "Goto file spec<->body",
         Add_Before => False);

      Command := new Edit_File_Command;
      Register_Contextual_Menu
        (Kernel,
         Name   => "Edit file",
         Label  => "Edit %f",
         Action => Command,
         Filter => Action_Filter (Lookup_Filter (Kernel, "File")
            and not Create (Module => Src_Editor_Module_Name)));

      Command := new Control_Command;
      Control_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Control_Command (Command.all).Mode := As_Is;
      Register_Action
        (Kernel, "No casing/indentation on next key",
         Command, -"Disable the casing and indentation on next key",
         Src_Action_Context);
      Bind_Default_Key
        (Kernel      => Kernel,
         Action      => "No casing/indentation on next key",
         Default_Key => "control-q");

      Register_Module
        (Module                  => Src_Editor_Module_Id,
         Kernel                  => Kernel,
         Module_Name             => Src_Editor_Module_Name,
         Priority                => Default_Priority,
         Default_Context_Factory => Default_Factory'Access,
         Save_Function           => Save_Function'Access,
         Customization_Handler   => Casing_Customize'Access);
      GPS.Kernel.Kernel_Desktop.Register_Desktop_Functions
        (Save_Desktop'Access, Load_Desktop'Access);

      Add_Hook (Kernel, Open_File_Action_Hook, Source_File_Hook'Access);
      Add_Hook (Kernel, File_Line_Action_Hook, File_Line_Hook'Access);
      Add_Hook
        (Kernel, Src_Editor_Buffer.Hooks.Word_Added_Hook,
         Word_Added_Hook'Access);

      --  Menus

      Register_Menu
        (Kernel, File, -"_New", Stock_New, On_New_File'Access,
         Ref_Item => -"Messages");
      Register_Menu
        (Kernel, File, -"New _View", "", On_New_View'Access,
         Ref_Item => -"Messages");
      Register_Menu
        (Kernel, File, -"_Open...",  Stock_Open,
         On_Open_File'Access, null, GDK_F3,
         Ref_Item => -"Messages");
      Register_Menu
        (Kernel, File, -"Open _From Project...",  Stock_Open,
         On_Open_From_Path'Access, null,
         GDK_F3, Shift_Mask,
         Ref_Item => -"Messages");

      Recent_Menu_Item := Register_Menu
        (Kernel, File, -"_Recent", "", null, Ref_Item => -"Messages");
      Associate (Get_History (Kernel).all,
                 Hist_Key,
                 Recent_Menu_Item,
                 new On_Recent'(Menu_Callback_Record with
                                Kernel => Kernel_Handle (Kernel)));

      Register_Menu
        (Kernel, File, -"_Save", Stock_Save,
         On_Save'Access, null,
         GDK_S, Control_Mask,
         Ref_Item => -"Messages");
      Register_Menu
        (Kernel, File, -"Save _As...", Stock_Save_As,
         On_Save_As'Access,
         Ref_Item => -"Messages");

      Register_Menu
        (Kernel, Save, -"_All", "",
         On_Save_All'Access,
         Ref_Item => -"Messages");
      Register_Menu (Kernel, Save, -"_Desktop", "", On_Save_Desktop'Access);
      Register_Menu
        (Kernel, Save, -"D_efault Desktop", "",
         On_Save_Default_Desktop'Access);
      Gtk_New (Mitem);
      Register_Menu (Kernel, File, Mitem, Ref_Item => -"Messages");

      Register_Menu
        (Kernel, File, -"Change _Directory...", "",
         On_Change_Dir'Access, Ref_Item => -"Messages");

      Register_Menu (Kernel, File, -"_Print", Stock_Print, On_Print'Access);
      Gtk_New (Mitem);
      Register_Menu (Kernel, File, Mitem);

      Command := new Close_Command;
      Close_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Close_Command (Command.all).Close_All := False;
      Register_Action
        (Kernel, "Close current window", Command,
           -"Close the currently selected window");

      Register_Menu
        (Kernel,
         Parent_Path => File,
         Text        => -"_Close",
         Stock_Image => Stock_Close,
         Callback    => null,
         Command     => Commands.Command_Access (Command),
         Accel_Key   => GDK_LC_w,
         Accel_Mods  => Control_Mask);

      Command := new Close_Command;
      Close_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Close_Command (Command.all).Close_All := True;
      Register_Action
        (Kernel, "Close all windows", Command,
           -"Close all open windows, asking for confirmation when relevant");

      Register_Menu
        (Kernel,
         Parent_Path => File,
         Text        => -"Close _All",
         Callback    => null,
         Command     => Commands.Command_Access (Command),
         Ref_Item    => -"Close",
         Add_Before  => False);

      Gtk_New (Mitem);
      Register_Menu (Kernel, File, Mitem);

      Register_Menu (Kernel, File, -"_Exit", "", On_Exit'Access);

      --  Note: callbacks for the Undo/Redo menu items will be added later
      --  by each source editor.

      UR.Undo_Menu_Item :=
        Register_Menu (Kernel, Edit, -"_Undo", Stock_Undo,
                       null, null,
                       GDK_Z, Control_Mask,
                       Ref_Item  => -"Preferences",
                       Sensitive => False);
      UR.Redo_Menu_Item :=
        Register_Menu (Kernel, Edit, -"_Redo", Stock_Redo,
                       null, null,
                       GDK_R, Control_Mask,
                       Ref_Item  => -"Preferences",
                       Sensitive => False);

      Gtk_New (Mitem);
      Register_Menu
        (Kernel, Edit, Mitem, Ref_Item => "Redo", Add_Before => False);

      Insert_Space (Toolbar, Position => 3);
      UR.Undo_Button := Insert_Stock
        (Toolbar, Stock_Undo, -"Undo Previous Action", Position => 4);
      Set_Sensitive (UR.Undo_Button, False);
      UR.Redo_Button := Insert_Stock
        (Toolbar, Stock_Redo, -"Redo Previous Action", Position => 5);
      Set_Sensitive (UR.Redo_Button, False);

      Kernel_Callback.Connect
        (Toolbar, "destroy",
         Toolbar_Destroy_Cb'Access,
         Kernel_Handle (Kernel));

      Append_Space (Toolbar);

      --  ??? This should be bound to Ctrl-A, except this would interfer with
      --  Emacs keybindings for people who want to use them.
      Register_Menu (Kernel, Edit, -"_Select All",  "",
                     On_Select_All'Access, Ref_Item => -"Preferences");

      Gtk_New (Mitem);
      Register_Menu (Kernel, Edit, Mitem, Ref_Item => -"Preferences");
      Register_Menu (Kernel, Edit, -"Insert _File...",  "",
                     On_Insert_File'Access, Ref_Item => -"Preferences");

      Gtk_New (Mitem);
      Register_Menu (Kernel, Edit, Mitem, Ref_Item => -"Preferences");

      Register_Menu (Kernel, Edit, -"Comment _Lines", "",
                     On_Comment_Lines'Access, null,
                     GDK_minus, Control_Mask, Ref_Item => -"Preferences");
      Register_Menu (Kernel, Edit, -"Uncomment L_ines", "",
                     On_Uncomment_Lines'Access, null,
                     GDK_underscore, Control_Mask, Ref_Item => -"Preferences");
      Register_Menu (Kernel, Edit, -"R_efill", "",
                     On_Refill'Access, null,
                     GDK_equal, Control_Mask, Ref_Item => -"Preferences");

      Gtk_New (Mitem);
      Register_Menu (Kernel, Edit, Mitem, Ref_Item => -"Preferences");

      Register_Menu (Kernel, Edit, -"_Fold all blocks", "",
                     On_Fold_Blocks'Access, null,
                     0, 0, Ref_Item => -"Preferences");
      Register_Menu (Kernel, Edit, -"Unfold all _blocks", "",
                     On_Unfold_Blocks'Access, null,
                     0, 0, Ref_Item => -"Preferences");

      Gtk_New (Mitem);
      Register_Menu (Kernel, Edit, Mitem, Ref_Item => -"Preferences");

      Command := new Goto_Line_Command;
      Goto_Line_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Register_Menu (Kernel,
                     Parent_Path => Navigate,
                     Text        => -"Goto _Line...",
                     Stock_Image => Stock_Jump_To,
                     Command     => Command_Access (Command),
                     Callback    => null,
                     Accel_Key   => GDK_G,
                     Accel_Mods  => Control_Mask,
                     Ref_Item    => -"Goto File Spec<->Body");
      Register_Menu (Kernel, Navigate, -"Goto _Declaration", Stock_Home,
                     On_Goto_Declaration'Access, Ref_Item => -"Goto Line...");
      Register_Menu (Kernel, Navigate, -"Goto _Body", "",
                     On_Goto_Body'Access, Ref_Item => -"Goto Line...");

      --  Toolbar buttons

      Button := Insert_Stock
        (Toolbar, Stock_New, -"Create a New File", Position => 0);
      Kernel_Callback.Connect
        (Button, "clicked", On_New_File'Access, Kernel_Handle (Kernel));

      Button := Insert_Stock
        (Toolbar, Stock_Open, -"Open a File", Position => 1);
      Kernel_Callback.Connect
        (Button, "clicked", On_Open_File'Access, Kernel_Handle (Kernel));

      Button := Insert_Stock
        (Toolbar, Stock_Save, -"Save Current File", Position => 2);
      Kernel_Callback.Connect
        (Button, "clicked", On_Save'Access, Kernel_Handle (Kernel));

      Add_Hook (Kernel, File_Saved_Hook, File_Saved_Cb'Access);
      Add_Hook (Kernel, Location_Changed_Hook, Cursor_Stopped_Cb'Access);

      Undo_Redo_Data.Set (Kernel, UR, Undo_Redo_Id);

      Add_Hook (Kernel, Preferences_Changed_Hook, Preferences_Changed'Access);
      Add_Hook (Kernel, File_Closed_Hook, File_Closed_Cb'Access);
      Add_Hook (Kernel, File_Edited_Hook, File_Edited_Cb'Access);
      Add_Hook
        (Kernel, File_Changed_On_Disk_Hook, File_Changed_On_Disk_Cb'Access);

      --  Commands

      Register_Command
        (Kernel, "edit",
         Class         => Editor_Class,
         Static_Method => True,
         Minimum_Args  => 1,
         Maximum_Args  => 6,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "create_mark",
         Minimum_Args  => 1,
         Maximum_Args  => 4,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "highlight",
         Minimum_Args  => 2,
         Maximum_Args  => 3,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Line_Highlighting.Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "add_blank_lines",
         Minimum_Args  => 3,
         Maximum_Args  => 4,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "remove_blank_lines",
         Minimum_Args  => 1,
         Maximum_Args  => 2,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "block_fold",
         Minimum_Args  => 1,
         Maximum_Args  => 2,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "block_unfold",
         Minimum_Args  => 1,
         Maximum_Args  => 2,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "unhighlight",
         Minimum_Args  => 2,
         Maximum_Args  => 3,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Line_Highlighting.Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "highlight_range",
         Minimum_Args  => 2,
         Maximum_Args  => 5,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Line_Highlighting.Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "unhighlight_range",
         Minimum_Args  => 2,
         Maximum_Args  => 5,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Line_Highlighting.Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "register_highlighting",
         Minimum_Args  => 2,
         Maximum_Args  => 3,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Line_Highlighting.Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "set_background_color",
         Minimum_Args  => 2,
         Maximum_Args  => 2,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "goto_mark",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "delete_mark",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "get_chars",
         Minimum_Args  => 1,
         Maximum_Args  => 5,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "get_line",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "get_column",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "get_file",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "get_last_line",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "block_get_start",
         Minimum_Args  => 2,
         Maximum_Args  => 2,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "block_get_end",
         Minimum_Args  => 2,
         Maximum_Args  => 2,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "block_get_name",
         Minimum_Args  => 2,
         Maximum_Args  => 2,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "block_get_type",
         Minimum_Args  => 2,
         Maximum_Args  => 2,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "block_get_level",
         Minimum_Args  => 2,
         Maximum_Args  => 2,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "subprogram_name",
         Minimum_Args  => 2,
         Maximum_Args  => 2,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "cursor_get_line",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "cursor_get_column",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "cursor_set_position",
         Minimum_Args  => 2,
         Maximum_Args  => 3,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "cursor_center",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "get_buffer",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "save_buffer",
         Minimum_Args  => 1,
         Maximum_Args  => 2,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "replace_text",
         Minimum_Args  => 4,
         Maximum_Args  => 6,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "indent",
         Minimum_Args  => Indent_Cmd_Parameters'Length - 1,
         Maximum_Args  => Indent_Cmd_Parameters'Length,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "indent_buffer",
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "refill",
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "cut",
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "copy",
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "paste",
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "select_all",
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "select_text",
         Minimum_Args  => 2,
         Maximum_Args  => 4,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "insert_text",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "undo",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "redo",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "close",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "save",
         Maximum_Args  => Save_Cmd_Parameters'Length,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "search",
         Minimum_Args => 1,
         Maximum_Args => 3,
         Class        => Get_File_Class (Kernel),
         Handler      => File_Search_Command_Handler'Access);
      Register_Command
        (Kernel, "search_next",
         Minimum_Args => 1,
         Maximum_Args => 3,
         Class        => Get_File_Class (Kernel),
         Handler      => Current_Search_Command_Handler'Access);

      Register_Command
        (Kernel, "search",
         Minimum_Args => 1,
         Maximum_Args => 5,
         Class        => Get_Project_Class (Kernel),
         Handler      => Project_Search_Command_Handler'Access);

      Register_Command
        (Kernel, "set_synchronized_scrolling",
         Minimum_Args  => 2,
         Maximum_Args  => 3,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "add_case_exception",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "remove_case_exception",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);

      Register_Command
        (Kernel, "set_writable",
         Minimum_Args  => 2,
         Maximum_Args  => 2,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);

      --  Register the search functions

      Gtk_New (Selector, Kernel);
      Gtk_New (Extra, Kernel);

      declare
         Name  : constant String := -"Current File";
         Name2 : constant String := -"Files From Project";
         Name3 : constant String := -"Files...";
         Name4 : constant String := -"Open Files";

      begin
         Register_Search_Function
           (Kernel => Kernel,
            Data   =>
              (Length            => Name'Length,
               Label             => Name,
               Factory           => Current_File_Factory'Access,
               Extra_Information => Gtk_Widget (Selector),
               Id                => Src_Editor_Module_Id,
               Mask              => All_Options));
         Register_Search_Function
           (Kernel => Kernel,
            Data   =>
              (Length            => Name2'Length,
               Label             => Name2,
               Factory           => Files_From_Project_Factory'Access,
               Extra_Information => Gtk_Widget (Selector),
               Id                => null,
               Mask              => All_Options and not Search_Backward));
         Register_Search_Function
           (Kernel => Kernel,
            Data   =>
              (Length            => Name3'Length,
               Label             => Name3,
               Factory           => Files_Factory'Access,
               Extra_Information => Gtk_Widget (Extra),
               Id                => null,
               Mask              => All_Options and not Search_Backward));
         Register_Search_Function
           (Kernel => Kernel,
            Data   =>
              (Length            => Name4'Length,
               Label             => Name4,
               Factory           => Open_Files_Factory'Access,
               Extra_Information => Gtk_Widget (Selector),
               Id                => null,
               Mask              => All_Options and not Search_Backward));
      end;

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

      --  Create the module-wide GCs.
      --  We need to do that in a callback to "map"

      if not Mapped_Is_Set (Get_Main_Window (Kernel)) then
         Widget_Callback.Connect
           (Get_Main_Window (Kernel), "map", Map_Cb'Access, After => True);

      else
         Map_Cb (Get_Main_Window (Kernel));
      end if;

      Remove_Blank_Lines_Pixbuf := Gdk_New_From_Xpm_Data (close_block_xpm);
      Hide_Block_Pixbuf   := Gdk_New_From_Xpm_Data (fold_block_xpm);
      Unhide_Block_Pixbuf := Gdk_New_From_Xpm_Data (unfold_block_xpm);

      --  Register preferences

      Cursor_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Editor-Cursor-Color",
         Default => "black",
         Blurb   => -"Color to use for the cursor in editors",
         Nick    => -"Cursor color"));
      Register_Property
        (Kernel, Param_Spec (Cursor_Color), -"Editor:Fonts & Colors");

      Cursor_Aspect_Ratio := Param_Spec_Int (Gnew_Int
        (Name    => "Editor-Cursor-Aspect-Ratio",
         Default => 10,
         Minimum => 1,
         Maximum => 100,
         Nick    => -"Cursor aspect ratio",
         Blurb   => -("Size of the cursor, proportionaly to one character. 100"
                      & "means the same size as a character")));
      Register_Property
        (Kernel, Param_Spec (Cursor_Aspect_Ratio), -"Editor:Fonts & Colors");
   end Register_Module;

   -------------------------
   -- Preferences_Changed --
   -------------------------

   procedure Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Pref_Display_Line_Numbers     : constant Boolean :=
        Get_Pref (Kernel, Display_Line_Numbers);
      Pref_Display_Subprogram_Names : constant Boolean :=
        Get_Pref (Kernel, Display_Subprogram_Names);

      Id : constant Source_Editor_Module :=
        Source_Editor_Module (Src_Editor_Module_Id);

      Color   : Gdk_Color;
      Success : Boolean;
      Iter    : Child_Iterator;
      Child   : MDI_Child;
   begin
      Color := Get_Pref (Kernel, Search_Results_Color);
      Alloc_Color (Get_Default_Colormap, Color, False, True, Success);

      Line_Highlighting.Add_Category
        (Search_Result_Highlighting, Color, Mark_In_Speedbar => True);

      if Pref_Display_Subprogram_Names /= Id.Show_Subprogram_Names then
         --  The preference for showing the subprogram name has changed:
         --  we need either to show or to hide the name on all open editors.

         declare
            Files : constant VFS.File_Array := Open_Files (Kernel);
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
                    & Get_Pref (Kernel, Cursor_Color)
                    & """" & ASCII.LF
                    & "GtkTextView::cursor-aspect-ratio="
                    & Float'Image
                      (Float (Get_Pref (Kernel, Cursor_Aspect_Ratio))
                       / 100.0)
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
               Set_Font_Description
                 (Layout, Get_Pref_Font (Kernel, Default_Style));
               Set_Markup (Layout, "0000");
               Get_Pixel_Size (Layout, Width, Height);
               Id.Character_Width := Width / 4;
               Unref (Layout);
            end;
         else
            Id.Character_Width := 0;
         end if;

         --  Tell the editors to refresh their side columns.
         Iter  := First_Child (Get_MDI (Kernel));

         loop
            Child := Get (Iter);

            exit when Child = null;

            if Get_Widget (Child).all in Source_Box_Record'Class then
               Refresh_Side_Column
                 (Get_Buffer (Source_Box (Get_Widget (Child)).Editor));
            end if;

            Next (Iter);
         end loop;
      end if;
   end Preferences_Changed;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Id : in out Source_Editor_Module_Record) is
   begin
      String_List_Utils.String_List.Free (Id.Unopened_Files);
      Mark_Identifier_List.Free (Id.Stored_Marks);

      Free (Id.Search_Pattern);

      --  Post_It_Note_GC and Blank_Lines_GC are initialized only when the
      --  main window is mapped. Therefore, if the window was never displayed,
      --  these values are not initialized.

      if Id.Post_It_Note_GC /= null then
         Unref (Id.Post_It_Note_GC);
      end if;

      if Id.Blank_Lines_GC /= null then
         Unref (Id.Blank_Lines_GC);
      end if;

      --  Destroy graphics
      Unref (Remove_Blank_Lines_Pixbuf);
      Unref (Hide_Block_Pixbuf);
      Unref (Unhide_Block_Pixbuf);
      Src_Editor_Module_Id := null;
   end Destroy;

   -----------------
   -- Find_Editor --
   -----------------

   function Find_Editor
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : VFS.Virtual_File) return Gtkada.MDI.MDI_Child
   is
      Iter  : Child_Iterator;
      Child : MDI_Child;
      Full  : VFS.Virtual_File;
      Id    : constant Source_Editor_Module :=
        Source_Editor_Module (Src_Editor_Module_Id);

   begin
      if File = VFS.No_File then
         return null;
      end if;

      --  Check whether the currently selected child corresponds to
      --  the file (this will be the case in a vast majority of calls to
      --  this subprogram)

      Child := Get_Focus_Child (Get_MDI (Kernel));

      if Get_Filename (Child) = File then
         return Child;
      end if;

      --  Attempt to find the editor in the cache.

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
         Full := Create
           (Get_Full_Path_From_File
              (Get_Registry (Kernel).all, Full_Name (File).all, True, False));
      end if;

      Iter  := First_Child (Get_MDI (Kernel));

      loop
         Child := Get (Iter);

         exit when Child = null
           or else Get_Filename (Child) = Full
           or else Get_File_Identifier (Child) = Full

            --  Handling of file identifiers
           or else Get_Title (Child) = Full_Name (File).all;

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
         if Get_Widget (Child).all in Source_Box_Record'Class then
            Editor := Source_Box (Get_Widget (Child)).Editor;

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
           or else (Get_Widget (Child).all in Source_Box_Record'Class
                    and then Source_Box (Get_Widget (Child)).Editor =
                      Source_Editor_Box (Editor));
         Next (Iter);
      end loop;

      return Child;
   end Find_Child;

   ----------
   -- Hash --
   ----------

   function Hash is new HTables.Hash (Header_Num);

   function Hash (F : Virtual_File) return Header_Num is
   begin
      if Filenames_Are_Case_Sensitive then
         return Hash (Full_Name (F).all);
      else
         return Hash (To_Lower (Full_Name (F).all));
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

   function Autosaved_File (File : VFS.Virtual_File) return VFS.Virtual_File is
   begin
      --  Implementation must be in sync with Is_Auto_Save below.
      return Create
        (Full_Filename => Dir_Name (File).all & ".#" & Base_Name (File) & "#");
   end Autosaved_File;

   ------------------
   -- Is_Auto_Save --
   ------------------

   function Is_Auto_Save (File : VFS.Virtual_File) return Boolean is
      Base : constant String := Base_Name (File);
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
      return Id.Character_Width;
   end Line_Number_Character_Width;

   ---------------------
   -- Post_It_Note_GC --
   ---------------------

   function Post_It_Note_GC return Gdk.GC.Gdk_GC is
      Id : constant Source_Editor_Module :=
        Source_Editor_Module (Src_Editor_Module_Id);
   begin
      return Id.Post_It_Note_GC;
   end Post_It_Note_GC;

end Src_Editor_Module;
