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









with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Exceptions;                    use Ada.Exceptions;
with Ada.IO_Exceptions;                 use Ada.IO_Exceptions;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with GNAT.Directory_Operations;         use GNAT.Directory_Operations;
with GNAT.OS_Lib;                       use GNAT.OS_Lib;

with Gdk.Color;                         use Gdk.Color;
with Gdk.GC;                            use Gdk.GC;
with Gdk.Types.Keysyms;                 use Gdk.Types.Keysyms;
with Gdk.Types;                         use Gdk.Types;
with Gdk;                               use Gdk;

with Glib.Object;                       use Glib.Object;
with Glib.Properties.Creation;          use Glib.Properties.Creation;
with Glib.Values;                       use Glib.Values;

with Gtk.Box;                           use Gtk.Box;
with Gtk.Button;                        use Gtk.Button;
with Gtk.Dialog;                        use Gtk.Dialog;
with Gtk.Enums;                         use Gtk.Enums;
with Gtk.GEntry;                        use Gtk.GEntry;
with Gtk.Handlers;
with Gtk.Label;                         use Gtk.Label;
with Gtk.Main;                          use Gtk.Main;
with Gtk.Menu_Item;                     use Gtk.Menu_Item;
with Gtk.Rc;                            use Gtk.Rc;
with Gtk.Stock;                         use Gtk.Stock;
with Gtk.Toolbar;                       use Gtk.Toolbar;
with Gtk.Window;                        use Gtk.Window;

with Gtkada.Entry_Completion;           use Gtkada.Entry_Completion;
with Gtkada.File_Selector;              use Gtkada.File_Selector;
with Gtkada.Handlers;                   use Gtkada.Handlers;
with Gtkada.Types;                      use Gtkada.Types;

with Pango.Enums;
with Pango.Font;
with Pango.Layout;                      use Pango.Layout;

with Aliases_Module;                    use Aliases_Module;
with Basic_Types;                       use Basic_Types;
with Casing_Exceptions;                 use Casing_Exceptions;
with Commands.Interactive;              use Commands, Commands.Interactive;
with Completion_Module;                 use Completion_Module;
with Default_Preferences;
with File_Utils;                        use File_Utils;
with Find_Utils;                        use Find_Utils;
with Generic_List;
with GPS.Intl;                          use GPS.Intl;
with GPS.Kernel.Actions;                use GPS.Kernel.Actions;
with GPS.Kernel.Console;                use GPS.Kernel.Console;
with GPS.Kernel.Contexts;               use GPS.Kernel.Contexts;
with GPS.Kernel.MDI;                    use GPS.Kernel.MDI;
with GPS.Kernel.Preferences;            use GPS.Kernel.Preferences;
with GPS.Kernel.Project;                use GPS.Kernel.Project;
with GPS.Kernel.Standard_Hooks;         use GPS.Kernel.Standard_Hooks;
with GPS.Kernel.Timeout;                use GPS.Kernel.Timeout;
with GPS.Main_Window;                   use GPS.Main_Window;
with Histories;                         use Histories;
with Language;                          use Language;
with Language_Handlers;                 use Language_Handlers;
with Projects.Registry;                 use Projects, Projects.Registry;
with Src_Contexts;                      use Src_Contexts;
with Src_Editor_Box;                    use Src_Editor_Box;
with Src_Editor_Buffer.Buffer_Commands; use Src_Editor_Buffer.Buffer_Commands;
with Src_Editor_Buffer.Hooks;           use Src_Editor_Buffer.Hooks;
with Src_Editor_Buffer.Line_Information;
use Src_Editor_Buffer.Line_Information;
with Src_Editor_Buffer.Text_Handling;   use Src_Editor_Buffer.Text_Handling;
with Src_Editor_Buffer;                 use Src_Editor_Buffer;
with Src_Editor_Module.Line_Highlighting;
with Src_Editor_Module.Markers;         use Src_Editor_Module.Markers;
with Src_Editor_Module.Shell;           use Src_Editor_Module.Shell;
with Src_Editor_View.Commands;          use Src_Editor_View.Commands;
with Src_Editor_View;                   use Src_Editor_View;
with Src_Printing;
with String_Utils;                      use String_Utils;
with Traces;                            use Traces;

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

   Cursor_Color        : Default_Preferences.Param_Spec_Color;
   Cursor_Aspect_Ratio : Param_Spec_Int;

   type Editor_Child_Record is new GPS_MDI_Child_Record with null record;

   function Dnd_Data
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

   procedure Save_To_File
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      Name    : VFS.Virtual_File := VFS.No_File;
      Success : out Boolean);
   --  Save the current editor to Name, or its associated filename if Name is
   --  null.

   function Create_File_Editor
     (Kernel     : access Kernel_Handle_Record'Class;
      File       : VFS.Virtual_File;
      Create_New : Boolean := True) return Source_Editor_Box;
   --  Create a new text editor that edits File.
   --  If File is the empty string, or the file doesn't exist and Create_New is
   --  True, then an empty editor is created.
   --  No check is done to make sure that File is not already edited
   --  elsewhere. The resulting editor is not put in the MDI window.

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
      Editor : access Source_Editor_Box_Record'Class)
      return Selection_Context_Access;
   --  Same as above.

   procedure New_View
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Create a new view for the current editor and add it in the MDI.
   --  The current editor is the focus child in the MDI. If the focus child
   --  is not an editor, nothing happens.

   procedure File_Edited_Cb
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Callback for the "file_edited" hook.

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

   procedure Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class);
   --  Called when the preferences have changed.

   procedure Add_To_Recent_Menu
     (Kernel : access Kernel_Handle_Record'Class; File : Virtual_File);
   --  Add an entry for File to the Recent menu, if needed.

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
   --  that we always use that one by default when looking for the last editor
   --  for a given file.

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

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
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
        and then Get_Widget (Child).all in Source_Editor_Box_Record'Class
      then
         return Get_File_Identifier
           (Get_Buffer (Source_Editor_Box (Get_Widget (Child))));
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
        and then Get_Widget (Child).all in Source_Editor_Box_Record'Class
      then
         return Get_Filename (Source_Editor_Box (Get_Widget (Child)));
      else
         return VFS.No_File;
      end if;
   end Get_Filename;

   ----------------
   -- Do_Nothing --
   ----------------

   procedure Do_Nothing (X : in out Location_Marker) is
      pragma Unreferenced (X);
   begin
      null;
   end Do_Nothing;

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
      Dummy : Boolean;
      pragma Unreferenced (Dummy);
   begin
      loop
         Child := Get (Iter);

         exit when Child = null;

         if Get_Widget (Child).all in Source_Editor_Box_Record'Class then
            Box := Source_Editor_Box (Get_Widget (Child));

            if D.File = VFS.No_File
              or else D.File = Get_Filename (Box)
            then
               Dummy := Check_Timestamp_And_Reload
                 (Box,
                  Interactive   => False,
                  Always_Reload => False);

               Check_Writable (Box);
            end if;
         end if;

         Next (Iter);
      end loop;
   end File_Changed_On_Disk_Cb;

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
      Src    : Source_Editor_Box;
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
                 ((Src,
                   Editable_Line_Type (Line),
                   Column, 0, User, False));
               Push_Marker_In_History
                 (Kernel  => User,
                  Marker  => Create_File_Marker
                    (Kernel => User,
                     File   => F,
                     Line   => Line,
                     Column => Column));
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
      if Widget.all in Source_Editor_Box_Record'Class then
         Editor := Source_Editor_Box (Widget);

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
         return Source_Editor_Box (Get_Widget (Child));
      end if;
   end Get_Source_Box_From_MDI;

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
      Current : Source_Editor_Box) return Source_Editor_Box
   is
      Editor : Source_Editor_Box;
      Child  : MDI_Child;
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
         Initialize (Child, Editor, All_Buttons);
         Child := Put
           (Kernel, Child,
            Focus_Widget   => Gtk_Widget (Get_View (Editor)),
            Default_Width  => Get_Pref (Default_Widget_Width),
            Default_Height => Get_Pref (Default_Widget_Height),
            Module         => Src_Editor_Module_Id);
         Set_Child (Get_View (Editor), Child);

         Widget_Callback.Connect
           (Child, "selected", Update_Cache_On_Focus'Access);

         Set_Icon (Child, Gdk_New_From_Xpm_Data (editor_xpm));
         Set_Focus_Child (Child);

         --  Find the first free view number
         Num := 2;
         while Find_MDI_Child_By_Name
           (Get_MDI (Kernel), Base_Name (Title) & " <" & Image (Num) & ">") /=
           null
         loop
            Num := Num + 1;
         end loop;

         declare
            Im : constant String := Image (Num);
         begin
            Set_Title
              (Child,
               Full_Name (Title).all & " <" & Im & ">",
               Base_Name (Title) & " <" & Im & ">");
         end;

         File_Callback.Connect
           (Child, "destroy", On_Editor_Destroy'Access, User_Data => Title);
      end;

      return Editor;
   end New_View;

   procedure New_View
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Current : constant Source_Editor_Box :=
        Get_Source_Box_From_MDI (Find_Current_Editor (Kernel));
      Box     : Source_Editor_Box;
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
     (Module : access Source_Editor_Module_Record;
      Child  : Gtk.Widget.Gtk_Widget;
      Mode   : Save_Function_Mode) return Boolean
   is
      pragma Unreferenced (Module);
      Success        : Boolean;
      Box            : constant Source_Editor_Box := Source_Editor_Box (Child);
   begin
      case Mode is
         when Query =>
            return Needs_To_Be_Saved (Get_Buffer (Box));

         when Action =>
            if Needs_To_Be_Saved (Get_Buffer (Box)) then
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
        Gtkada.MDI.Position_Default) return Source_Editor_Box
   is
      No_Name : constant String := -"Untitled";
      MDI     : constant MDI_Window := Get_MDI (Kernel);
      Editor  : Source_Editor_Box;
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
              (Source_Editor_Box (Get_Widget (Child)),
               Interactive   => False,
               Always_Reload => Force);

            Raise_Child (Child, Focus);

            return Source_Editor_Box (Get_Widget (Child));
         end if;
      end if;

      Editor := Create_File_Editor (Kernel, File, Create_New);

      --  If we have created an editor, put it into a box, and give it
      --  to the MDI to handle

      if Editor /= null then
         Child := new Editor_Child_Record;
         Initialize (Child, Editor, All_Buttons);
         Child := Put
           (Kernel, Child,
            Focus_Widget   => Gtk_Widget (Get_View (Editor)),
            Default_Width  => Get_Pref (Default_Widget_Width),
            Default_Height => Get_Pref (Default_Widget_Height),
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
                    Source_Editor_Box_Record'Class
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

         if File /= VFS.No_File then
            Add_To_Recent_Menu (Kernel, File);
         end if;

      else
         Console.Insert
           (Kernel, (-"Cannot open file ") & "'" & Full_Name (File).all & "'",
            Add_LF => True,
            Mode   => Error);
      end if;

      return Editor;
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

      Source := Source_Editor_Box (Get_Widget (Child));

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
              Use_Native_Dialog => Get_Pref (Use_Native_Dialogs),
              Kind              => Open_File,
              File_Pattern      => "*.*;{*.ads,*.adb};{*.c,*.h,*.cpp}",
              Pattern_Name      => -"All files;GNAT Ada default;C/C++ files",
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
                           Get_History
                             (Get_History (Kernel).all,
                              Open_From_Path_History);
      List1            : File_Array_Access :=
                           Get_Source_Files
                             (Project   => Get_Project (Kernel),
                              Recursive => True);
      List2            : File_Array_Access :=
                           Get_Predefined_Source_Files
                             (Get_Registry (Kernel).all);
      Compl            : File_Completion_Factory;

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
            Text    : constant String :=
                        Get_Text (Get_Entry (Open_File_Entry));
            Full    : Virtual_File;
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
      Editor : Source_Editor_Box;
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
                 Base_Directory    => Dir (Old_Name),
                 Default_Name      => Base_Name (Old_Name),
                 Use_Native_Dialog => Get_Pref (Use_Native_Dialogs),
                 Kind              => Save_File,
                 File_Pattern      => "*.*;{*.ads,*.adb};{*.c,*.h,*.cpp}",
                 Pattern_Name      =>
                   -"All files;GNAT Ada default;C/C++ files",
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
      Dir    : Virtual_File;

   begin
      Dir :=  Select_Directory
        (-"Select a directory",
         History => Get_History (Kernel),
         Parent  => Gtk_Window (Get_Current_Window (Kernel)));

      if Dir /= No_File then
         Change_Dir (Dir);
      end if;

   exception
      when VFS_Directory_Error =>
         GPS.Kernel.Console.Insert
           (Kernel,
            "Cannot change to directory: " &
            Full_Name (Dir).all,
            Mode => Error);
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
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
      Print_Helper     : constant String := Get_Pref (Print_Command);
      Source_Font      : constant Pango_Font_Description :=
        Get_Pref_Font (Default_Style);
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
            Force    => Get_Pref (Auto_Save))
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

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
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
                 Use_Native_Dialog => Get_Pref (Use_Native_Dialogs),
                 Kind              => Open_File,
                 File_Pattern      => "*.*;{*.ads,*.adb};{*.c,*.h,*.cpp}",
                 Pattern_Name      =>
                   -"All files;GNAT Ada default;C/C++ files",
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
      Buffer       : Source_Buffer;
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
            Lang  : Language_Access;
            File  : constant Virtual_File := File_Information (File_Context);
            Block : Unbounded_String := Null_Unbounded_String;
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

            Buffer := Get_Buffer
              (Get_Source_Box_From_MDI (Find_Editor (Kernel, File)));

            for J in Start_Line .. End_Line loop
               Append (Block, Get_Chars
                         (Buffer   => Buffer,
                          Line     => Editable_Line_Type (J),
                          Column   => 1));
            end loop;

            Replace_Slice
              (Buffer,
               Text   => Comment_Block (Lang, To_String (Block), Comment),
               Line   => Editable_Line_Type (Start_Line),
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
      Source      : Source_Editor_Box;
      Edit        : Source_Editor_Box;
      Tmp         : Boolean;
      pragma Unreferenced (Tmp);
   begin
      if D.Line = -1 then
         --  Close all file editors corresponding to File.

         loop
            Child := Get (Iter);

            exit when Child = null;

            if Get_Widget (Child).all in Source_Editor_Box_Record'Class
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
                  Line   => D.Line,
                  Column => D.Column));
         end if;

         if Source /= null then
            Edit := Source;
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
              (Source_Editor_Box (Get_Widget (Child)),
               D.Identifier,
               D.Every_Line);

         elsif D.Info'Length = 0 then
            Remove_Line_Information_Column
              (Source_Editor_Box (Get_Widget (Child)), D.Identifier);

         else
            Add_File_Information
              (Source_Editor_Box (Get_Widget (Child)), D.Identifier, D.Info);
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

   -----------------------------
   -- Default_Context_Factory --
   -----------------------------

   function Default_Context_Factory
     (Module : access Source_Editor_Module_Record;
      Child  : Gtk.Widget.Gtk_Widget) return Selection_Context_Access
   is
   begin
      return Default_Factory
        (Get_Kernel (Module.all), Source_Editor_Box (Child));
   end Default_Context_Factory;

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
         while W.all not in Source_Editor_Box_Record'Class loop
            W := Get_Parent (W);
         end loop;
         Box := Source_Editor_Box (W);

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

   ----------------------
   -- Bookmark_Handler --
   ----------------------

   function Bookmark_Handler
     (Module : access Source_Editor_Module_Record;
      Load   : Glib.Xml_Int.Node_Ptr := null) return Location_Marker is
   begin
      return Src_Editor_Module.Markers.Load (Get_Kernel (Module.all), Load);
   end Bookmark_Handler;

   ---------------
   -- Customize --
   ---------------

   procedure Customize
     (Module : access Source_Editor_Module_Record;
      File   : VFS.Virtual_File;
      Node   : Glib.Xml_Int.Node_Ptr;
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
         Priority                => Default_Priority);
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
         Command     => Command,
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
         Command     => Command,
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

      --  ??? This should be bound to Ctrl-A, except this would interfer with
      --  Emacs keybindings for people who want to use them.
      Register_Menu (Kernel, Edit, -"_Select All", "",
                     On_Select_All'Access, Ref_Item => -"Preferences",
                     Filter => Src_Action_Context);

      Gtk_New (Mitem);
      Register_Menu (Kernel, Edit, Mitem, Ref_Item => -"Preferences");
      Register_Menu (Kernel, Edit, -"Insert _File...",  "",
                     On_Insert_File'Access, Ref_Item => -"Preferences",
                     Filter => Src_Action_Context);

      Gtk_New (Mitem);
      Register_Menu (Kernel, Edit, Mitem, Ref_Item => -"Preferences");

      Register_Menu (Kernel, Edit, -"Comment _Lines", "",
                     On_Comment_Lines'Access, null,
                     GDK_minus, Control_Mask, Ref_Item => -"Preferences",
                     Filter => Src_Action_Context);
      Register_Menu (Kernel, Edit, -"Uncomment L_ines", "",
                     On_Uncomment_Lines'Access, null,
                     GDK_underscore, Control_Mask, Ref_Item => -"Preferences",
                     Filter => Src_Action_Context);
      Register_Menu (Kernel, Edit, -"R_efill", "",
                     On_Refill'Access, null,
                     GDK_equal, Control_Mask, Ref_Item => -"Preferences",
                     Filter => Src_Action_Context);

      Gtk_New (Mitem);
      Register_Menu (Kernel, Edit, Mitem, Ref_Item => -"Preferences");

      Register_Menu (Kernel, Edit, -"_Fold all blocks", "",
                     On_Fold_Blocks'Access, null,
                     0, 0, Ref_Item => -"Preferences",
                     Filter => Src_Action_Context);
      Register_Menu (Kernel, Edit, -"Unfold all _blocks", "",
                     On_Unfold_Blocks'Access, null,
                     0, 0, Ref_Item => -"Preferences",
                     Filter => Src_Action_Context);

      Gtk_New (Mitem);
      Register_Menu (Kernel, Edit, Mitem, Ref_Item => -"Preferences");

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
      Register_Menu (Kernel, Navigate, -"Goto _Declaration", Stock_Home,
                     On_Goto_Declaration'Access, Ref_Item => -"Goto Line...");
      Register_Menu (Kernel, Navigate, -"Goto _Body", "",
                     On_Goto_Body'Access, Ref_Item => -"Goto Line...");

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
      Register_Menu (Kernel, Navigate, -"Goto Matching _Delimiter",
                     Ref_Item   => -"Goto Body",
                     Callback   => null,
                     Add_Before => False,
                     Accel_Key  => GDK_apostrophe,
                     Accel_Mods => Control_Mask,
                     Command    => Command,
                     Filter     => Src_Action_Context);

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
      Add_Hook (Kernel, File_Edited_Hook, File_Edited_Cb'Access);
      Add_Hook
        (Kernel, File_Changed_On_Disk_Hook, File_Changed_On_Disk_Cb'Access);

      Register_Commands (Kernel);

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
               Id                => Abstract_Module_ID (Src_Editor_Module_Id),
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

      Cursor_Color := Default_Preferences.Param_Spec_Color
        (Default_Preferences.Gnew_Color
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

      Completion_Module.Register_Module (Kernel);
   end Register_Module;

   -------------------------
   -- Preferences_Changed --
   -------------------------

   procedure Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Pref_Display_Line_Numbers     : constant Boolean :=
        Get_Pref (Display_Line_Numbers);
      Pref_Display_Subprogram_Names : constant Boolean :=
        Get_Pref (Display_Subprogram_Names);

      Id : constant Source_Editor_Module :=
        Source_Editor_Module (Src_Editor_Module_Id);

      Iter  : Child_Iterator;
      Child : MDI_Child;
   begin
      Line_Highlighting.Add_Category
        (Search_Results_Style,
         Mark_In_Speedbar => True);

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
                    & Get_Pref (Cursor_Color)
                    & """" & ASCII.LF
                    & "GtkTextView::cursor-aspect-ratio="
                    & Float'Image
                      (Float (Get_Pref (Cursor_Aspect_Ratio))
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
                 (Layout, Get_Pref_Font (Default_Style));
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

            if Get_Widget (Child).all in Source_Editor_Box_Record'Class then
               Refresh_Side_Column
                 (Get_Buffer (Source_Editor_Box (Get_Widget (Child))));
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
      Marker_List.Free (Id.Stored_Marks);

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

      Iter := First_Child (Get_MDI (Kernel));

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
      if Id = null then
         return 0;
      else
         return Id.Character_Width;
      end if;
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
