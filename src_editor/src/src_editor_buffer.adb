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

with Ada.Calendar;                        use Ada.Calendar;
with Ada.Characters.Conversions;
with Ada.Text_IO;
with System.Address_To_Access_Conversions;

pragma Warnings (Off, ".*is an internal GNAT unit");
with Ada.Strings.Unbounded.Aux;
pragma Warnings (On, ".*is an internal GNAT unit");
with Ada.Strings.Maps;                    use Ada.Strings.Maps;

with Interfaces.C.Strings;                use Interfaces.C.Strings;
with System.Address_Image;
with GNAT.Expect;                         use GNAT.Expect;
with GNAT.Regpat;                         use GNAT.Regpat;
with GNAT.SHA1;

with GNATCOLL.Arg_Lists;                  use GNATCOLL.Arg_Lists;
with GNATCOLL.Paragraph_Filling;          use GNATCOLL.Paragraph_Filling;
with GNATCOLL.Projects;                   use GNATCOLL.Projects;
with GNATCOLL.Symbols;                    use GNATCOLL.Symbols;
with GNATCOLL.Traces;                     use GNATCOLL.Traces;
with GNATCOLL.Utils;                      use GNATCOLL.Utils;
with GNATCOLL.VFS;                        use GNATCOLL.VFS;

with VSS.Strings.Conversions;

with Gdk.Keyval;
with Gdk.RGBA;                            use Gdk.RGBA;
with Gdk.Types.Keysyms;                   use Gdk.Types.Keysyms;
with Glib.Convert;
with Glib.Error;                          use Glib.Error;
with Glib.Object;                         use Glib.Object;
with Glib.Properties;                     use Glib.Properties;
with Glib.Unicode;                        use Glib.Unicode;
with Glib.Values;                         use Glib.Values;

with Gtk;                                 use Gtk;
with Gtk.Enums;                           use Gtk.Enums;
with Gtk.Handlers;                        use Gtk.Handlers;
with Gtk.Text_Buffer;                     use Gtk.Text_Buffer;
with Gtk.Text_Iter;                       use Gtk.Text_Iter;
with Gtk.Text_Tag;                        use Gtk.Text_Tag;
with Gtk.Text_Tag_Table;                  use Gtk.Text_Tag_Table;
with Gtk.Text_View;                       use Gtk.Text_View;

with Gtkada.Dialogs;                      use Gtkada.Dialogs;
with Gtkada.MDI;                          use Gtkada.MDI;
with Gtkada.Types;                        use Gtkada.Types;

with Pango.Enums;                         use Pango.Enums;

with Commands.Editor;                     use Commands.Editor;
with Completion_Module;                   use Completion_Module;
with Default_Preferences;                 use Default_Preferences;
with GPS.Default_Styles;                  use GPS.Default_Styles;
with GPS.Dialogs;                         use GPS.Dialogs;
with GPS.Intl;                            use GPS.Intl;
with GPS.Kernel;                          use GPS.Kernel;
with GPS.Kernel.Charsets;                 use GPS.Kernel.Charsets;
with GPS.Kernel.Clipboard;                use GPS.Kernel.Clipboard;
with GPS.Kernel.Contexts;                 use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;                    use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;                      use GPS.Kernel.MDI;
with GPS.Kernel.Messages;                 use GPS.Kernel.Messages;
with GPS.Kernel.Messages.Simple;          use GPS.Kernel.Messages.Simple;
with GPS.Kernel.Modules;                  use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;              use GPS.Kernel.Preferences;
with GPS.Kernel.Properties;               use GPS.Kernel.Properties;
with GPS.Kernel.Project;                  use GPS.Kernel.Project;
with GPS.Kernel.Scripts;                  use GPS.Kernel.Scripts;
with GPS.Kernel.Task_Manager;
with GPS.Properties;
with GUI_Utils;                           use GUI_Utils;
with Language;                            use Language;
with Language.Unknown;                    use Language.Unknown;
with Language_Handlers;                   use Language_Handlers;
with Src_Editor_Box;                      use Src_Editor_Box;
with Src_Editor_Buffer.Blocks;
with Src_Editor_Buffer.Line_Information;
with Src_Editor_Buffer.Hooks;             use Src_Editor_Buffer.Hooks;
with Src_Editor_Buffer.Cursors;           use Src_Editor_Buffer.Cursors;
with Src_Editor_Buffer.Text_Handling;
with Src_Editor_Module;                   use Src_Editor_Module;
with Src_Editor_Module.Editors;           use Src_Editor_Module.Editors;
with Src_Editor_Module.Line_Highlighting;
with Src_Editor_Status_Bar;               use Src_Editor_Status_Bar;
with Src_Highlighting;                    use Src_Highlighting;
with String_Utils;                        use String_Utils;
with Gtk.Window;                          use Gtk.Window;

package body Src_Editor_Buffer is

   type Gtk_TB_Access is access all Gtk_Text_Buffer_Record;

   use Src_Editor_Buffer.Blocks;
   use Src_Editor_Module.Line_Highlighting;
   use Src_Editor_Buffer.Line_Information;

   use type Basic_Types.Visible_Column_Type;
   use type Glib.Main.G_Source_Id;
   use type GNAT.Strings.String_Access;
   use type System.Address;

   type Src_Editor_Factory_Access is
     access all Src_Editor_Module.Editors.Src_Editor_Buffer_Factory;

   Editors_Factory : Src_Editor_Factory_Access;

   Me                  : constant Trace_Handle :=
     Create ("GPS.Source_Editor.Buffer");

   Me_Formatters       : constant Trace_Handle :=
     Create ("GPS.SOURCE_EDITOR.BUFFER.FORMATTERS", Off);

   Prevent_Align       : constant Trace_Handle :=
     Create ("GPS.INTERNAL.PREVENT_ALIGN_ON_TAB", On);

   Auto_Save_No_Reload : constant Trace_Handle :=
     Create ("DIALOG_AUTO_SAVE_NO_RELOAD", Off);

   Auto_Save_Reload    : constant Trace_Handle :=
     Create ("DIALOG_AUTO_SAVE_RELOAD", Off);

   pragma Unreferenced (Prevent_Align);
   --  This trace is setup here for the benefit of tab.py

   Buffer_Recompute_Interval : constant Guint := 200;
   --  The interval at which to check whether the buffer should be reparsed,
   --  in milliseconds.

   Buffer_Recompute_Delay    : constant Duration := 1.0;
   --  The delay between the last edit and the re-parsing of the buffer,
   --  in seconds.

   Src_Editor_Message_Flags  : constant Message_Flags :=
     Side_And_Locations;

   package Buffer_Timeout is new Glib.Main.Generic_Sources (Source_Buffer);

   function Strlen
     (Str : Gtkada.Types.Chars_Ptr) return Interfaces.C.size_t;
   pragma Import (C, Strlen);
   --  Import Strlen directly, for efficiency

   type Delimiter_Type is (Opening, Closing);
   --  ??? missing doc

   Delimiters : constant array (1 .. 3, Delimiter_Type'Range) of Character
     := (('(', ')'),
         ('[', ']'),
         ('{', '}'));
   --  ??? missing doc
   --  ??? Should we get that from the language ?

   Strip_Blanks_Property_Name : constant String := "strip-blanks";
   Strip_Lines_Property_Name  : constant String := "strip-blanks-lines";

   package Iter_Access_Address_Conversions is
     new System.Address_To_Access_Conversions (Gtk_Text_Iter);

   --------------------
   -- Signal Support --
   --------------------

   Class_Record : Ada_GObject_Class := Uninitialized_Class;
   --  A pointer to the 'class record'

   Signals : constant Interfaces.C.Strings.chars_ptr_array :=
          (1 => New_String (String (Signal_Cursor_Position_Changed)),
           2 => New_String (String (Signal_Side_Column_Changed)),
           3 => New_String (String (Signal_Side_Column_Configuration_Changed)),
           4 => New_String (String (Signal_Line_Highlights_Changed)),
           5 => New_String (String (Signal_Status_Changed)),
           6 => New_String (String (Signal_Filename_Changed)),
           7 => New_String (String (Signal_Buffer_Information_Changed)),
           8 => New_String (String (Signal_Closed)));
   --  The list of new signals supported by this GObject

   Signal_Parameters : constant Glib.Object.Signal_Parameter_Types :=
                         (1 => (GType_Int, GType_Int),
                          2 => (GType_None, GType_None),
                          3 => (GType_None, GType_None),
                          4 => (GType_None, GType_None),
                          5 => (GType_None, GType_None),
                          6 => (GType_None, GType_None),
                          7 => (GType_None, GType_None),
                          8 => (GType_None, GType_None));
   --  The parameters associated to each new signal

   package Buffer_Callback is new Gtk.Handlers.Callback
     (Widget_Type => Source_Buffer_Record);

   --------------------------
   -- Forward declarations --
   --------------------------

   generic
      with function Line_Length (Iter : Gtk_Text_Iter) return Gint;
      with procedure Set_Pos (Iter : in out Gtk_Text_Iter; Pos : Gint);
   procedure Generic_Valid_Position
     (Buffer : Source_Buffer;
      Iter   : out Gtk_Text_Iter;
      Found  : out Boolean;
      Line   : Gint;
      Column : Gint := 0);
   --  Generic version of Is_Valid_Position

   procedure Changed_Handler
     (Buffer : access Source_Buffer_Record'Class);
   --  This procedure is used to signal to the clients that the insert
   --  cursor position may have changed by emitting the
   --  "cursor_position_changed" signal.

   procedure Mark_Set_Handler
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues);
   --  This procedure is used to signal to the clients that the insert
   --  cursor position has changed by emitting the "cursor_position_changed"
   --  signal. This signal is emitted only when the mark changed is the
   --  Insert_Mark.

   procedure Insert_Text_Cb
     (Buffer          : access Source_Buffer_Record'Class;
      End_Insert_Iter : Gtk.Text_Iter.Gtk_Text_Iter);
   --  This procedure recomputes the syntax-highlighting of the buffer
   --  in a semi-optimized manor, based on syntax-highlighting already
   --  done before the insertion and the text added.
   --
   --  This procedure assumes that the language has been set.

   procedure After_Insert_Text
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues);
   --  This procedure is just a proxy between the "insert_text" signal
   --  and the Insert_Text_Cb callback. It extracts the parameters from
   --  Params and then call Insert_Text_Cb. For efficiency reasons, the
   --  signal is processed only when Lang is not null and the language
   --  supports syntax highlighting.
   --
   --  Note that this handler is designed to be connected "after", in which
   --  case the Insert_Iter iterator is located at the end of the inserted
   --  text.

   procedure Before_Insert_Text
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues);
   --  First handler connected to the "insert_text" signal

   procedure Delete_Range_Cb
     (Buffer : access Source_Buffer_Record'Class;
      Iter   : Gtk_Text_Iter);
   --  This procedure recomputes the syntax-highlighting of the buffer
   --  in a semi-optimized manor, based on syntax-highlighting already
   --  done before the deletion and the location of deletion.
   --
   --  This procedure assumes that the language has been set.

   procedure Before_Delete_Range
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues);
   --  Handler connected to the "delete_range" signal, but which occurs before
   --  the actual deletion.

   procedure After_Delete_Range
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues);
   --  This procedure is just a proxy between the "delete_range" signal
   --  and the Delete_Range_Cb callback. It extracts the parameters from
   --  Params and then call Delete_Range_Cb. For efficiency reasons, the
   --  signal is processed only when Lang is not null and the language
   --  supports syntax highlighting.
   --
   --  Note that this handler is designed to be connected "after", in which
   --  case the Start and End iterators are equal, since the text between
   --  these iterators have already been deleted.

   procedure Buffer_Destroy (Data : System.Address; Buf : System.Address);
   pragma Convention (C, Buffer_Destroy);
   --  Called when the buffer is being destroyed

   function Automatic_Save (Buffer : Source_Buffer) return Boolean;
   --  Handle automatic save of the buffer, using a timeout

   procedure Internal_Save_To_File
     (Buffer   : Source_Buffer;
      Filename : GNATCOLL.VFS.Virtual_File;
      Internal : Boolean;
      Success  : out Boolean;
      Force    : Boolean := False);
   --  Low level save function. Only writes the buffer contents on disk,
   --  with no modification on the buffer's settings.
   --  If Internal is True, do not emit kernel signals. This is used notably
   --  for automatic saves.

   type On_Pref_Changed is new Preferences_Hooks_Function with record
      Buffer : Source_Buffer;
   end record;
   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference);
   --  Called when the preferences have changed

   type On_Project_Changed is new Simple_Hooks_Function with record
      Buffer : Source_Buffer;
   end record;
   overriding procedure Execute
     (Self   : On_Project_Changed;
      Kernel : not null access Kernel_Handle_Record'Class);
   --  Called when the project has changed

   type On_Loc_Changed is new File_Location_Hooks_Function with record
      Buffer : Source_Buffer;
   end record;
   overriding procedure Execute
     (Self         : On_Loc_Changed;
      Kernel       : not null access Kernel_Handle_Record'Class;
      File         : Virtual_File;
      Line, Column : Integer;
      Project      : Project_Type);
   --  Called when the current editor reaches a new location

   procedure Cursor_Move_Hook (Buffer : access Source_Buffer_Record'Class);
   --  Actions that must be executed whenever the cursor moves

   procedure User_Edit_Hook (Buffer : access Source_Buffer_Record'Class);
   --  Actions that must be executed whenever the user inserts or deletes text

   procedure Edit_Hook (Buffer : access Source_Buffer_Record'Class);
   --  Actions that must be executed whenever the buffer text is changed

   procedure Lines_Remove_Hook_Before
     (Buffer     : access Source_Buffer_Record'Class;
      Start_Line : Buffer_Line_Type;
      Count      : Buffer_Line_Type);
   --  Actions that must be executed whenever a line is removed.
   --  This is called before the lines are actually removed from the buffer.

   procedure Lines_Remove_Hook_After
     (Buffer     : access Source_Buffer_Record'Class;
      Start_Line : Buffer_Line_Type;
      End_Line   : Buffer_Line_Type);
   --  Same as above, but occurs after the lines have been removed

   procedure Lines_Add_Hook
     (Buffer : access Source_Buffer_Record'Class;
      Start  : Buffer_Line_Type;
      Number : Buffer_Line_Type);
   --  Actions that must be executed whenever a line is added.
   --  Start is the number of the line just before the lines were inserted,
   --  Number is the number of lines that were inserted.

   procedure End_Action_Hook (Buffer : access Source_Buffer_Record'Class);
   --  Actions that must be executed whenever an action is ended

   procedure Destroy_Hook (Buffer : access Source_Buffer_Record'Class);
   --  Actions that must be executed when the buffer is destroyed

   procedure Initialize_Hook (Buffer : access Source_Buffer_Record'Class);
   --  Actions that must be executed after initialization

   procedure Get_Iter_At_Screen_Position
     (Buffer : access Source_Buffer_Record;
      Iter   : out Gtk_Text_Iter;
      Line   : Gint;
      Column : Visible_Column_Type);
   --  Return the iter at position (Line, Column), tab expansion included.
   --  ??? This function should be removed in the long term, replaced by
   --  the version of Get_Iter_At_Screen_Position that supports blank lines.

   function Edition_Timeout (Buffer : Source_Buffer) return Boolean;
   --  Timeout called in a timeout after the user has finished editing

   procedure Free_Column_Info
     (Column_Info : Columns_Config_Access);
   --  Free the info contained in Column_Info

   procedure Get_Selection_Bounds
     (Buffer       : access Source_Buffer_Record;
      Start_Line   : out Gint;
      Start_Column : out Gint;
      End_Line     : out Gint;
      End_Column   : out Gint;
      Found        : out Boolean);
   --  If a portion of the buffer is currently selected, then return the
   --  position of the beginning and the end of the selection. Otherwise,
   --  Found is set to False and the positions returned both point to the
   --  begining of the buffer.

   procedure Insert
     (Buffer      : access Source_Buffer_Record;
      Line        : Gint;
      Column      : Gint;
      Text        : String;
      Enable_Undo : Boolean := True);
   --  Insert the given text in at the specified position.
   --
   --  The validity of the given position must be verified before invoking this
   --  procedure. An incorrect position  will cause an Assertion_Failure when
   --  compiled with assertion checks, or an undefined behavior
   --  otherwise.
   --  If Enable_Undo is True, then the insertion action will be
   --  stored in the undo/redo queue.

   procedure Insert_Text
     (Buffer          : access Source_Buffer_Record;
      From_File       : Virtual_File;
      Lang_Autodetect : Boolean;
      Is_Auto_Save    : Boolean;
      File_Is_New     : Boolean;
      Success         : out Boolean);
   --  Replace the buffer with the contents of the file. This is undoable.

   function Compare_SHA1
     (File : Virtual_File; Autosave : Virtual_File) return Boolean;
   --  Compare the SHA1 of File and its autosaved file.
   --  Delete the autosaved file and return False if they are the same.

   procedure Replace_Slice_Real
     (Buffer       : access Source_Buffer_Record;
      Start_Line   : Gint;
      Start_Column : Gint;
      End_Line     : Gint;
      End_Column   : Gint;
      Text         : String;
      Enable_Undo  : Boolean := True);
   --  Replace the text between the start and end positions by Text.
   --
   --  The validity of the given positions must be verified before invoking
   --  this procedure. An incorrect position will cause an Assertion_Failure
   --  when compiled with assertion checks, or an undefined behavior otherwise.

   procedure Delete
     (Buffer      : access Source_Buffer_Record;
      Line        : Gint;
      Column      : Gint;
      Length      : Gint;
      Enable_Undo : Boolean := True);
   --  Delete Length caracters after the specified position.
   --
   --  The validity of the given position must be verified before invoking this
   --  procedure. An incorrect position  will cause an Assertion_Failure when
   --  compiled with assertion checks, or an undefined behavior
   --  otherwise.
   --  If Enable_Undo is True, then the deletion action will be
   --  stored in the undo/redo queue.

   function Conversion_Error_Message
     (Charset : String) return VSS.Strings.Virtual_String;
   --  Return the location category corresponding to errors when converting
   --  to Charset.

   procedure On_Paste_Done (Buffer : access Source_Buffer_Record'Class);
   --  Disable the "paste-done" signal introduced in gtk+ 2.18, which breaks
   --  the handling of multiple views

   procedure Update_Logical_Timestamp
     (Buffer : access Source_Buffer_Record'Class);
   pragma Inline (Update_Logical_Timestamp);
   --  Update the logical timestamp

   procedure Update_All_Column_Memory
     (Buffer : access Source_Buffer_Record'Class);
   --  Update column memory for every cursor, the main one and every existing
   --  multi cursor

   function Get_Current_Command
     (Buffer : access Source_Buffer_Record'Class) return Editor_Command
   is
     (if Buffer.Cursors_Sync.Mode = Manual_Slave
      then Editor_Command (Buffer.Cursors_Sync.MC.Current_Command)
      else Editor_Command (Buffer.Current_Command));

   procedure Set_Current_Command
     (Buffer : access Source_Buffer_Record'Class;
      Command : Editor_Command);

   function Source_Lines_Context
     (Buffer     : access Source_Buffer_Record;
      Project    : GNATCOLL.Projects.Project_Type;
      Start_Line : Editable_Line_Type;
      End_Line   : Editable_Line_Type) return Selection_Context;
   --  Create a selection context for the given lines

   procedure Set_Trailing_Space_Policy
     (Buffer : access Source_Buffer_Record;
      File   : GNATCOLL.VFS.Virtual_File;
      Trailing_Spaces_Found : Boolean);
   --  Detect and set strip trailing space policy for Buffer

   procedure Set_Trailing_Lines_Policy
     (Buffer               : access Source_Buffer_Record;
      File                 : GNATCOLL.VFS.Virtual_File;
      Trailing_Lines_Found : Boolean);
   --  Detect and set strip trailing empty lines policy for Buffer

   function Autosaved_File
     (File : GNATCOLL.VFS.Virtual_File) return GNATCOLL.VFS.Virtual_File;
   --  Return the autosaved file corresponding to File.
   --  See also Is_Auto_Save

   function Is_Editor (Ctxt : Selection_Context) return Boolean;
   --  Return True if the context was created from a source editor

   function Get_First_Non_Blank_Column
     (Buffer : access Source_Buffer_Record;
      Line   : Editable_Line_Type)
      return Visible_Column_Type;
   --  Return the first column with a non-whitespace character

   -----------
   -- Hooks --
   -----------

   type On_File_Deleted is new File_Hooks_Function with record
      Buffer : Source_Buffer;
   end record;
   overriding procedure Execute
     (Self   : On_File_Deleted;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File);
   --  Callback for the "file_deleted" hook

   type On_File_Renamed is new File2_Hooks_Function with record
      Buffer : Source_Buffer;
   end record;
   overriding procedure Execute
     (Self   : On_File_Renamed;
      Kernel : not null access Kernel_Handle_Record'Class;
      File, Renamed : Virtual_File);
   --  Callback for the "file_renamed" hook

   type On_Semantic_Tree_Updated is new File_Hooks_Function with record
      Buffer : Source_Buffer;
   end record;
   overriding procedure Execute
      (Self   : On_Semantic_Tree_Updated;
       Kernel : not null access Kernel_Handle_Record'Class;
       File   : GNATCOLL.VFS.Virtual_File);

   procedure Reset_Slave_Cursors_Commands
     (Buffer : Source_Buffer);

   procedure Emit_File_Edited
     (Buffer   : not null access Source_Buffer_Record'Class;
      Filename : Virtual_File);
   --  Emit the File_Edited hook and call File_Edited on the listeners

   procedure Emit_File_Closed
     (Buffer   : not null access Source_Buffer_Record'Class;
      Filename : Virtual_File);
   --  Emit the File_Closed hook and call File_Closed on the listeners

   procedure Emit_File_Renamed
     (Buffer : not null access Source_Buffer_Record'Class;
      From   : Virtual_File;
      To     : Virtual_File);
   --  Emit the File_Renamed hook and call File_Renamed on the listeners

   procedure Adjust_Tab_Width
     (Buffer : access Source_Buffer_Record'Class);
   --  Set Tab_Width based on the Language preferences

   procedure Save_Cursor
     (Buffer        : Source_Buffer;
      Mark          : Gtk_Text_Mark;
      Cursor_Line   : out Gint;
      Cursor_Offset : out Gint);
   --  Save the location of the Mark

   procedure Place_Cursor
     (Buffer        : Source_Buffer;
      Mark          : Gtk_Text_Mark;
      Cursor_Line   : Gint;
      Cursor_Offset : Gint);
   --  Move the mark to the new location

   procedure Range_Formatting
     (Buffer   : Source_Buffer;
      Mark     : Gtk_Text_Mark;
      From, To : Gtk_Text_Iter;
      Force    : Boolean := False);
   --  Execute provider selected in the preferences to do range formatting

   procedure On_Type_Formatting
     (Buffer   : Source_Buffer;
      Mark     : Gtk_Text_Mark;
      From, To : Gtk_Text_Iter;
      Force    : Boolean := False);
   --  Execute provider selected in the preferences to do formatting on enter

   -----------
   -- Utils --
   -----------

   procedure Unchecked_Free is
     new Ada.Unchecked_Deallocation
       (Source_Highlighter_Record'Class, Source_Highlighter);

   ----------------------
   -- Adjust_Tab_Width --
   ----------------------

   procedure Adjust_Tab_Width
     (Buffer : access Source_Buffer_Record'Class) is
   begin
      if Buffer.Lang /= null then
         Buffer.Tab_Width := Buffer.Lang.Get_Indentation_Level;
      end if;
   end Adjust_Tab_Width;

   -------------------------
   -- Set_Current_Command --
   -------------------------

   procedure Set_Current_Command
     (Buffer : access Source_Buffer_Record'Class;
      Command : Editor_Command) is
   begin
      if Buffer.Cursors_Sync.Mode = Manual_Slave then
         Buffer.Cursors_Sync.MC.Current_Command :=
           Command_Access (Command);
      else
         Buffer.Current_Command := Command_Access (Command);
      end if;
   end Set_Current_Command;

   ----------------------------------
   -- Reset_Slave_Cursors_Commands --
   ----------------------------------

   procedure Reset_Slave_Cursors_Commands
     (Buffer : Source_Buffer) is
   begin
      for Cursor of Buffer.Slave_Cursors_List loop
         if not Is_Null_Command (Editor_Command (Cursor.Current_Command)) then
            Cursor.Current_Command := null;
         end if;
      end loop;
   end Reset_Slave_Cursors_Commands;

   ---------------------
   -- Paste_Clipboard --
   ---------------------

   overriding procedure Paste_Clipboard
     (Buffer      : not null access Source_Buffer_Record;
      Clipboard   : not null access Gtk.Clipboard.Gtk_Clipboard_Record'Class;
      Default_Editable : Boolean := True)
   is
      Iter     : Gtk_Text_Iter;
      S        : Loc_T;
      From, To : Gtk_Text_Iter;
      Success  : Boolean;
      pragma Unreferenced (Success);

      G : Group_Block := New_Group (Buffer.Queue);
   begin
      --  This procedure is called from GPS.Kernel.Clipboard.Paste_Clipboard
      if Buffer.Has_MC_Clipboard then
         for C of Get_Cursors (Source_Buffer (Buffer)) loop
            if not C.Is_Main_Cursor then
               Set_Manual_Sync (C);
               Buffer.Get_Iter_At_Mark (Iter, Get_Mark (C));
               Buffer.Insert (Iter, To_String (C.Cursor.Clipboard));
            end if;
         end loop;

         Set_Manual_Sync
           (Get_Main_Cursor (Source_Buffer (Buffer)));
      end if;

      Get_Mark_Position (Source_Buffer (Buffer), Buffer.Get_Insert, S);
      Paste_Clipboard (Gtk_TB_Access (Buffer), Clipboard, Default_Editable);
      Get_Iter_At_Screen_Position (Buffer, From, S.Line, S.Col);
      Buffer.Get_Iter_At_Mark (To, Buffer.Get_Insert);

      Set_Manual_Sync (Get_Main_Cursor (+Buffer));

      if Get_Line (From) /= Get_Line (To)
        and then Auto_Indent_On_Paste.Get_Pref
      then
         Success := On_Indent_Action (Source_Buffer (Buffer), From, To);
      end if;

      Set_Cursors_Auto_Sync (Source_Buffer (Buffer));
   end Paste_Clipboard;

   -------------------
   -- Cut_Clipboard --
   -------------------

   overriding procedure Cut_Clipboard
     (Buffer     : not null access Source_Buffer_Record;
      Clipboard  : not null access Gtk.Clipboard.Gtk_Clipboard_Record'Class;
      Default_Editable : Boolean)
   is
      Start_Iter, End_Iter : Gtk_Text_Iter;
   begin
      Set_Manual_Sync (Get_Main_Cursor (+Buffer));
      Cut_Clipboard (Gtk_TB_Access (Buffer), Clipboard, Default_Editable);

      for C of Get_Cursors (Source_Buffer (Buffer)) loop
         if not C.Is_Main_Cursor then
            Buffer.Has_MC_Clipboard := True;

            Set_Manual_Sync (C);
            Buffer.Get_Iter_At_Mark (Start_Iter, C.Cursor.Sel_Mark);
            Buffer.Get_Iter_At_Mark (End_Iter, C.Cursor.Mark);
            C.Cursor.Clipboard := To_Unbounded_String
              (Buffer.Get_Text
                 (Start_Iter,
                  End_Iter,
                  Include_Hidden_Chars => True));
            Buffer.Delete (Start_Iter, End_Iter);
         end if;
      end loop;

      Update_MC_Selection (Source_Buffer (Buffer));

      Set_Cursors_Auto_Sync (Source_Buffer (Buffer));
   end Cut_Clipboard;

   --------------------
   -- Copy_Clipboard --
   --------------------

   overriding procedure Copy_Clipboard
     (Buffer    : not null access Source_Buffer_Record;
      Clipboard : not null access Gtk.Clipboard.Gtk_Clipboard_Record'Class)
   is
      Start_Iter, End_Iter : Gtk_Text_Iter;
      Success              : Boolean;
   begin
      Set_Manual_Sync (Get_Main_Cursor (+Buffer));
      Buffer.Get_Selection_Bounds
        (Start   => Start_Iter,
         The_End => End_Iter,
         Result  => Success);

      if Success then
         Clipboard.Set_Text
           (Text => Buffer.Get_Text
              (Start                => Start_Iter,
               The_End              => End_Iter,
               Include_Hidden_Chars => True));
      end if;

      for C of Get_Cursors (Source_Buffer (Buffer)) loop
         if not C.Is_Main_Cursor then
            Buffer.Has_MC_Clipboard := True;
            Set_Manual_Sync (C);
            Buffer.Get_Iter_At_Mark (Start_Iter, C.Cursor.Sel_Mark);
            Buffer.Get_Iter_At_Mark (End_Iter, C.Cursor.Mark);
            C.Cursor.Clipboard := To_Unbounded_String
              (Buffer.Get_Text
                 (Start_Iter,
                  End_Iter,
                  Include_Hidden_Chars => True));
         end if;
      end loop;

      Set_Cursors_Auto_Sync (Source_Buffer (Buffer));
   end Copy_Clipboard;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_File_Deleted;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File)
   is
      pragma Unreferenced (Kernel);
      Edited      : constant GNATCOLL.VFS.Virtual_File := Self.Buffer.Filename;
      Need_Action : Boolean := False;
   begin
      if Edited /= GNATCOLL.VFS.No_File then
         if Is_Directory (File) and then Is_Parent (File, Edited) then
            Need_Action := True;
         elsif not Is_Directory (File) and then File = Edited then
            Need_Action := True;
         end if;
      end if;

      if Need_Action then
         Self.Buffer.Saved_Position := -1;
         Self.Buffer.Status_Changed;
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Semantic_Tree_Updated;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File)
   is
      pragma Unreferenced (Kernel);
      Edited  : constant GNATCOLL.VFS.Virtual_File := Self.Buffer.Filename;
   begin
      if Edited /= GNATCOLL.VFS.No_File
        and then File = Edited
      then
         --  The semantic tree for this buffer has been updated.
         --  Parse the blocks.

         if Self.Buffer.Parse_Blocks then
            Compute_Blocks (Self.Buffer, Immediate => False);
            Self.Buffer.Blocks_Exact := True;
         end if;
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_File_Renamed;
      Kernel : not null access Kernel_Handle_Record'Class;
      File, Renamed : Virtual_File)
   is
      pragma Unreferenced (Kernel);
      Edited : constant Virtual_File := Self.Buffer.Filename;
      Dest   : GNATCOLL.VFS.Virtual_File;
   begin
      --  If we are renaming a directory (and not a file), we need to update
      --  the internal name of the file to take the new directory name into
      --  account.

      if Is_Directory (File) then
         if Is_Parent (File, Edited) then
            Dest := Create_From_Dir
              (Renamed.Dir, Relative_Path (Edited, File));
            Self.Buffer.Filename := Dest;
            Self.Buffer.Filename_Changed;
         end if;

      --  Else if we are renaming the current file from elsewhere, we need
      --  to update the buffer filename

      elsif Edited /= GNATCOLL.VFS.No_File and then Edited = File then
         Self.Buffer.Filename := Renamed;
         Self.Buffer.Filename_Changed;
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self         : On_Loc_Changed;
      Kernel       : not null access Kernel_Handle_Record'Class;
      File         : Virtual_File;
      Line, Column : Integer;
      Project      : Project_Type)
   is
      pragma Unreferenced (Kernel, File, Line, Column, Project);
   begin
      --  Highlight the cursor delimiters
      Self.Buffer.Highlighter.Highlight_Parenthesis;
   end Execute;

   ----------------------
   -- Free_Column_Info --
   ----------------------

   procedure Free_Column_Info (Column_Info : Columns_Config_Access) is
   begin
      if Column_Info /= null then
         if Column_Info.all /= null then
            for J in Column_Info.all'Range loop
               GNAT.Strings.Free (Column_Info.all (J).Identifier);
               Unchecked_Free (Column_Info.all (J));
            end loop;

            Unchecked_Free (Column_Info.all);
         end if;

         Column_Info.all := null;
      end if;
   end Free_Column_Info;

   ----------
   -- Free --
   ----------

   procedure Free (S : in out Src_String) is
      function To_chars_ptr is new Ada.Unchecked_Conversion
        (Unchecked_String_Access, Gtkada.Types.Chars_Ptr);
   begin
      if not S.Read_Only then
         --  was returned by Gtk.Text_Buffer.Get_Text
         --  should be released using Gtkada.Types.g_free
         g_free (To_chars_ptr (S.Contents));
      end if;
      S.Contents := null;
   end Free;

   -----------------------
   -- Reset_Blocks_Info --
   -----------------------

   procedure Reset_Blocks_Info (Buffer : access Source_Buffer_Record'Class) is
   begin
      Buffer.Blocks_Exact := False;
   end Reset_Blocks_Info;

   ---------------------
   -- Get_String_Line --
   ---------------------

   function Get_String_At_Line
     (Buffer               : Source_Buffer;
      Line                 : Editable_Line_Type;
      Start_Column         : Character_Offset_Type := 1;
      End_Column           : Character_Offset_Type := 0;
      Include_Hidden_Chars : Boolean := True;
      Include_Last         : Boolean := False)
      return Src_String
   is
      Start_Iter, End_Iter : Gtk_Text_Iter;
      Success              : Boolean;
      Result               : Src_String;
      Chars                : Gtkada.Types.Chars_Ptr;

   begin
      if Line not in 1 .. Buffer.Last_Editable_Line
        or else Start_Column = End_Column
      then
         return Result;
      end if;

      Get_Iter_At_Line_Offset
        (Buffer,
         Start_Iter,
         Gint (Buffer.Editable_Lines (Line) - 1),
         Gint (Start_Column - 1));

      if End_Column /= 0 then
         Get_Iter_At_Line_Offset
           (Buffer,
            End_Iter,
            Gint (Buffer.Editable_Lines (Line) - 1),
            Gint (End_Column - 1));
      else
         Copy (Start_Iter, End_Iter);
         Forward_To_Line_End (End_Iter, Success);
      end if;

      if Get_Line (Start_Iter) /= Get_Line (End_Iter) then
         --  If Line is empty then Forward_To_Line_End will move
         --  End_Iter to the end of Line + 1.
         --  In this case End_Iter should just point to Start_Iter.
         Copy (Start_Iter, End_Iter);
      end if;

      if Include_Last then
         Forward_Char (End_Iter, Success);
      end if;

      Chars :=
        Get_Text (Buffer, Start_Iter, End_Iter, Include_Hidden_Chars);
      Result.Contents := To_Unchecked_String (Chars);
      Result.Length := Integer (Strlen (Chars));

      return Result;
   end Get_String_At_Line;

   --------------
   -- Get_Text --
   --------------

   function Get_Text
     (Buffer               : access Source_Buffer_Record;
      Start_Line           : Editable_Line_Type := 1;
      Start_Column         : Character_Offset_Type := 1;
      End_Line             : Editable_Line_Type := 0;
      End_Column           : Character_Offset_Type := 0;
      Include_Hidden_Chars : Boolean := True;
      Include_Last         : Boolean := False)
      return GNAT.Strings.String_Access
   is
      Start_Iter : Gtk_Text_Iter;
      End_Iter   : Gtk_Text_Iter;
      Result     : GNAT.Strings.String_Access;
      Success    : Boolean;

   begin
      if Lines_Are_Real (Buffer) then
         Get_Iter_At_Line_Offset
           (Buffer,
            Start_Iter,
            Gint (Get_Buffer_Line (Buffer, Start_Line) - 1),
            Gint (Start_Column - 1));

         if End_Line /= 0 then
            if End_Column /= 0 then
               Get_Iter_At_Line_Offset
                 (Buffer,
                  End_Iter,
                  Gint (Get_Buffer_Line (Buffer, End_Line) - 1),
                  Gint (End_Column - 1));
            else
               Get_Iter_At_Line
                 (Buffer,
                  End_Iter,
                  Gint (Get_Buffer_Line (Buffer, End_Line) - 1));
               Forward_To_Line_End (End_Iter, Success);
            end if;
         else
            Get_End_Iter (Buffer, End_Iter);
         end if;

         if Include_Last then
            Forward_Char (End_Iter, Success);
         end if;

         Result := GUI_Utils.Get_Text
             (Buffer, Start_Iter, End_Iter, Include_Hidden_Chars);

         return Result;

      else
         if End_Line /= 0 then
            return Get_Buffer_Lines
              (Buffer               => Buffer,
               Start_Line           => Start_Line,
               End_Line             => End_Line,
               Start_Column         => Start_Column,
               End_Column           => End_Column,
               Include_Hidden_Chars => Include_Hidden_Chars,
               Include_Last         => Include_Last);
         else
            return
              Get_Buffer_Lines
                (Buffer               => Buffer,
                 Start_Line           => Start_Line,
                 End_Line             => Buffer.Last_Editable_Line,
                 Start_Column         => Start_Column,
                 Include_Hidden_Chars => Include_Hidden_Chars,
                 Include_Last         => Include_Last);
         end if;
      end if;
   end Get_Text;

   ---------------
   -- To_String --
   ---------------

   function To_String (S : Src_String) return String is
   begin
      if S.Length = 0 then
         return "";
      else
         return S.Contents (1 .. S.Length);
      end if;
   end To_String;

   -------------------------------
   -- Get_Strip_Trailing_Blanks --
   -------------------------------

   function Get_Strip_Trailing_Blanks
     (Buffer : access Source_Buffer_Record) return Boolean is
   begin
      return Buffer.Strip_Trailing_Blanks;
   end Get_Strip_Trailing_Blanks;

   -------------------------------
   -- Set_Strip_Trailing_Blanks --
   -------------------------------

   procedure Set_Strip_Trailing_Blanks
     (Buffer : access Source_Buffer_Record;
      Value  : Boolean) is
   begin
      Buffer.Strip_Trailing_Blanks := Value;

      if Buffer.Filename /= GNATCOLL.VFS.No_File then
         Set_Property
           (Kernel     => Buffer.Kernel,
            File       => Buffer.Filename,
            Name       => Strip_Blanks_Property_Name,
            Property   =>
               new GPS.Properties.Boolean_Property'(Value => Value),
            Persistent => True);
      end if;
   end Set_Strip_Trailing_Blanks;

   ------------------------------
   -- Get_Strip_Trailing_Lines --
   ------------------------------

   function Get_Strip_Trailing_Lines
     (Buffer : access Source_Buffer_Record) return Boolean is
   begin
      return Buffer.Strip_Trailing_Lines;
   end Get_Strip_Trailing_Lines;

   ------------------------------
   -- Set_Strip_Trailing_Lines --
   ------------------------------

   procedure Set_Strip_Trailing_Lines
     (Buffer : access Source_Buffer_Record;
      Value  : Boolean) is
   begin
      Buffer.Strip_Trailing_Lines := Value;

      if Buffer.Filename /= GNATCOLL.VFS.No_File then
         Set_Property
           (Kernel     => Buffer.Kernel,
            File       => Buffer.Filename,
            Name       => Strip_Lines_Property_Name,
            Property   =>
               new GPS.Properties.Boolean_Property'(Value => Value),
            Persistent => True);
      end if;
   end Set_Strip_Trailing_Lines;

   ----------------------
   -- Get_Buffer_Lines --
   ----------------------

   function Get_Buffer_Lines
     (Buffer               : access Source_Buffer_Record'Class;
      Start_Line           : Editable_Line_Type;
      End_Line             : Editable_Line_Type;
      Start_Column         : Character_Offset_Type := 1;
      End_Column           : Character_Offset_Type := 0;
      Include_Hidden_Chars : Boolean := True;
      Include_Last         : Boolean := False)
      return GNAT.Strings.String_Access
   is
      --  If the caller is retrieving the text between two iters and there are
      --  special lines. Then End_Line can be greater than Last_Editable_Line
      --  => An extra ASCII.LF will be added at the end for each of the special
      --  lines.
      Real_End : constant Editable_Line_Type :=
        Editable_Line_Type'Min (Buffer.Last_Editable_Line, End_Line);
      Lines    : array (Start_Line .. Real_End) of Src_String;
      Len      : Integer := 0;
      Index    : Integer := 1;
      Output   : GNAT.Strings.String_Access;
   begin
      for J in Lines'Range loop
         --  Retrieve the content of the lines: handle the special case
         --  of the first and last lines.
         Lines (J) :=
           Get_String_At_Line
             (Source_Buffer (Buffer),
              Line                 => J,
              Start_Column         => (if J = Lines'First
                                       then Start_Column
                                       else 1),
              End_Column           => (if J = Lines'Last
                                       then End_Column
                                       else 0),
              Include_Hidden_Chars => Include_Hidden_Chars,
              Include_Last         =>  J /= Lines'Last or else Include_Last);
         Len := Len + Lines (J).Length;
      end loop;

      Output := new String (1 .. Len);

      for J in Lines'Range loop
         Len := Lines (J).Length;

         if Len /= 0 then
            Output (Index .. Index + Len - 1) := Lines (J).Contents (1 .. Len);
         end if;

         Index := Index + Len;

         Free (Lines (J));
      end loop;

      return Output;
   end Get_Buffer_Lines;

   --------------------
   -- Get_Byte_Index --
   --------------------

   function Get_Byte_Index (Iter : Gtk_Text_Iter) return Natural is
      Index : Natural := 0;
   begin
      for J in 0 ..
        Get_Editable_Line
          (Source_Buffer (Get_Buffer (Iter)),
           Buffer_Line_Type (Get_Line (Iter))) - 1
      loop
         --  Increment the index by the size of the string + 1 (for EOL).
         --  Gtk lines are 0-based, Editable_Lines 1-based, hence the J + 1

         declare
            Str : Src_String :=
              Get_String_At_Line (Source_Buffer (Get_Buffer (Iter)), J + 1);
         begin
            Index := Index + Str.Length;
            Index := Index + 1;

            Free (Str);
         end;
      end loop;

      Index := Index + Natural (Get_Line_Index (Iter));

      return Index;
   end Get_Byte_Index;

   ---------------------
   -- Edition_Timeout --
   ---------------------

   function Edition_Timeout (Buffer : Source_Buffer) return Boolean is
      CL : Arg_List;
      Start_Iter : Gtk_Text_Iter;
      End_Iter   : Gtk_Text_Iter;

      ------------------------------
      -- Run_Highlight_Range_Hook --
      ------------------------------

      procedure Run_Highlight_Range_Hook;

      procedure Run_Highlight_Range_Hook is
      begin
         Get_Iter_At_Mark
           (Buffer, Start_Iter, Buffer.Highlighter.First_Highlight_Mark);
         Get_Iter_At_Mark
           (Buffer, End_Iter, Buffer.Highlighter.Last_Highlight_Mark);
         Highlight_Range_Hook.Run
           (Kernel    => Buffer.Kernel,
            File      => Buffer.Filename,
            From_Line => Natural (Get_Line (Start_Iter) + 1),
            To_Line   => Natural (Get_Line (End_Iter) + 1));
      end Run_Highlight_Range_Hook;

   begin
      if Buffer.In_Destruction
        or else Clock < Buffer.Blocks_Request_Timestamp +
          Buffer_Recompute_Delay
      then
         return True;
      end if;

      --  Re-highlight the highlight region if needed
      if Buffer.Highlighter.Use_Highlighting_Hook then
         Run_Highlight_Range_Hook;

      else
         Buffer.Highlighter.Highlight_Region;
      end if;

      --  Perform on-the-fly style check

      if Buffer.Auto_Syntax_Check then
         CL := Create ("File");
         Append_Argument (CL, +Full_Name (Buffer.Filename), One_Arg);
         Execute_GPS_Shell_Command
           (Buffer.Kernel, CL);
         Execute_GPS_Shell_Command
           (Buffer.Kernel,
            Parse_String ("File.shadow_check_syntax %1", Separate_Args));
      end if;

      --  Emit the Buffer_Modifed hook
      if Buffer.Get_Version > 0 then
         Buffer_Modified (Buffer);
      end if;

      --  Request an asynchronous update of the semantic tree
      if Buffer.Filename /= No_File then
         declare
            Command : constant Update_Async_Access := new Update_Async_Record'
              (Root_Command with
               Kernel => Buffer.Kernel,
               Filename => Buffer.Filename);
         begin
            GPS.Kernel.Task_Manager.Launch_Background_Command
              (Buffer.Kernel,
               Command_Access (Command),
               Active     => True,
               Show_Bar   => False,
               Queue_Id   => "semantic tree",
               Block_Exit => False);
         end;
      end if;

      --  Unregister the timeout

      Buffer.Blocks_Timeout_Registered := False;
      Buffer.Blocks_Timeout := Glib.Main.No_Source_Id;
      return False;

   exception
      when E : others =>
         Trace (Me, E);
         Buffer.Blocks_Timeout_Registered := False;
         Buffer.Blocks_Timeout := Glib.Main.No_Source_Id;
         return False;
   end Edition_Timeout;

   --------------------
   -- Get_Delimiters --
   --------------------

   procedure Get_Delimiters
     (Buffer           : access Source_Buffer_Record;
      On_Cursor_Iter   : Gtk_Text_Iter;
      First_Delim_Iter : out Gtk_Text_Iter;
      Last_Delim_Iter  : out Gtk_Text_Iter;
      Found            : out Natural;
      Counter_Max      : Natural := 16_384)
   is
      Current      : Gtk_Text_Iter;

      Success      : Boolean;
      Counter      : Natural;

      Stack        : Natural;
      String_Tag   : Boolean;
      C            : Character;

      Delimiter    : Integer;

      Language     : constant Language_Access := Get_Language (Buffer);

      Highlight_Within_Comment : Boolean := False;
      --  Set to True if the cursor is in a comment. In this case, we want to
      --  highlight matching parentheses only within the current comment block.

      function Check_Char (Forward : Boolean) return Boolean;
      --  Check current character (C) and update current procedure state.
      --  Returns False if parsing must stop (end of file reached for example)

      ----------------
      -- Check_Char --
      ----------------

      function Check_Char (Forward : Boolean) return Boolean is
         Val     : constant array (Boolean) of Integer := (1, -1);
         Tmp     : Gtk_Text_Iter;
         C2      : Character;
         Success : Boolean;

         procedure Move_Char;
         pragma Inline (Move_Char);
         --  Move one character backward or forward

         ---------------
         -- Move_Char --
         ---------------

         procedure Move_Char is
         begin
            if Forward then
               Forward_Char (Tmp, Success);
            else
               Backward_Char (Tmp, Success);
            end if;
         end Move_Char;

         In_Comment : constant Boolean :=
                        Is_In_Comment (Source_Buffer (Buffer), Current);

      begin
         --  If we are looking to highlight only within the current comment,
         --  and the character we are looking at is not in a comment, exit.

         if Highlight_Within_Comment
           and then not (Is_Blank (C) or else In_Comment)
         then
            return False;
         end if;

         if C = Delimiters (Delimiter, Closing)
           and then not String_Tag
           and then (not In_Comment or else Highlight_Within_Comment)
         then
            Stack := Stack + Val (Forward);

         elsif C = Delimiters (Delimiter, Opening)
           and then not String_Tag
           and then (not In_Comment or else Highlight_Within_Comment)
         then
            Stack := Stack - Val (Forward);

         elsif C = '"' then
            String_Tag := not String_Tag;

         elsif C = ''' then
            --  Check if this is a character
            Copy (Current, Tmp);

            Move_Char;

            if not Success then
               return False;
            end if;

            Move_Char;

            if not Success then
               return False;
            end if;

            C2 := Get_Char (Tmp);

            if C2 = ''' then
               Copy (Tmp, Current);
            end if;
         end if;

         return True;
      end Check_Char;

   begin
      --  Find a closing delimiter

      Found := 0;
      Delimiter := -1;
      Copy (On_Cursor_Iter, Current);
      Backward_Char (Current, Success);

      if Language /= null
        and then Language /= Unknown_Lang
        and then Is_In_Comment (Source_Buffer (Buffer), Current)
      then
         --  The current character is in a comment: set the corresponding flag
         Highlight_Within_Comment := True;
      end if;

      if Success then
         C := Get_Char (Current);

         for J in Delimiters'Range (1) loop
            if Delimiters (J, Closing) = C then
               Delimiter := J;
               exit;
            end if;
         end loop;
      end if;

      if Delimiter in Delimiters'Range (1) then
         Counter    := 0;
         Stack      := 1;
         String_Tag := False;

         Backward_Char (Current, Success);

         while Success and then Counter < Counter_Max loop
            C := Get_Char (Current);

            Success := Check_Char (Forward => False);
            exit when not Success;

            if Stack = 0 and then not String_Tag then
               Copy (Current, First_Delim_Iter);
               Copy (On_Cursor_Iter, Last_Delim_Iter);

               Found := Found + 1;
               exit;
            end if;

            Counter := Counter + 1;
            Backward_Char (Current, Success);
         end loop;
      end if;

      --  Highlight next parenthesis, if necessary

      Delimiter := -1;
      Copy (On_Cursor_Iter, Current);
      C := Get_Char (On_Cursor_Iter);

      for J in Delimiters'Range (1) loop
         if Delimiters (J, Opening) = C then
            Delimiter := J;
            exit;
         end if;
      end loop;

      if Delimiter in Delimiters'Range (1) then
         Counter    := 0;
         Stack      := 1;
         String_Tag := False;

         Forward_Char (Current, Success);

         while Success and then Counter < Counter_Max loop
            C := Get_Char (Current);

            Success := Check_Char (Forward => True);
            exit when not Success;

            if Stack = 0 and then not String_Tag then
               if Found = 0 then
                  Copy (On_Cursor_Iter, First_Delim_Iter);
               end if;

               Forward_Char (Current, Success);
               Copy (Current, Last_Delim_Iter);

               Found := Found + 1;
               exit;
            end if;

            Counter := Counter + 1;
            Forward_Char (Current, Success);
         end loop;
      end if;
   end Get_Delimiters;

   ---------------------------
   -- Register_Edit_Timeout --
   ---------------------------

   procedure Register_Edit_Timeout
     (Buffer : access Source_Buffer_Record'Class)
   is
      Timeout : Gint;
   begin
      Buffer.Blocks_Request_Timestamp := Clock;

      if Buffer.In_Destruction then
         return;
      end if;

      if not Buffer.Blocks_Timeout_Registered
        and then Buffer.Blocks_Timeout = Glib.Main.No_Source_Id
      then
         Buffer.Blocks_Timeout_Registered := True;
         Buffer.Blocks_Timeout := Buffer_Timeout.Timeout_Add
           (Buffer_Recompute_Interval,
            Edition_Timeout'Access,
            Source_Buffer (Buffer));
      end if;

      Timeout := Gint (Integer'(Periodic_Save.Get_Pref));
      if not Buffer.Timeout_Registered
        and then Timeout > 0
      then
         Buffer.Timeout_Id := Buffer_Timeout.Timeout_Add
           (Guint (Timeout) * 1000,  Automatic_Save'Access, Buffer.all'Access);
         Buffer.Timeout_Registered := True;
      end if;
   end Register_Edit_Timeout;

   ---------------------
   -- Get_Buffer_Line --
   ---------------------

   function Get_Buffer_Line
     (Buffer : access Source_Buffer_Record;
      Line   : Editable_Line_Type) return Buffer_Line_Type is
   begin
      if not Buffer.Original_Text_Inserted then
         return Buffer_Line_Type (Line);
      end if;

      if Buffer.Editable_Lines /= null
        and then Line in Buffer.Editable_Lines'Range
      then
         return Buffer.Editable_Lines (Line);
      end if;

      return 0;
   end Get_Buffer_Line;

   -----------------------
   -- Get_Editable_Line --
   -----------------------

   function Get_Editable_Line
     (Buffer : access Source_Buffer_Record'Class;
      Line   : Buffer_Line_Type) return Editable_Line_Type is
   begin
      if Buffer.Line_Data /= null
        and then Line in Buffer.Line_Data'Range
      then
         return Buffer.Line_Data (Line).Editable_Line;
      end if;

      return 0;
   end Get_Editable_Line;

   --------------------
   -- Automatic_Save --
   --------------------

   function Automatic_Save (Buffer : Source_Buffer) return Boolean
   is
      Success : Boolean;
   begin
      if Buffer.Modified_Auto
        and then Buffer.Filename /= No_File
      then
         Internal_Save_To_File
           (Buffer,
            Autosaved_File (Buffer.Filename),
            True,
            Success);
         Buffer.Modified_Auto := False;
      end if;

      --  timeout will be restarted when the bufer is changed
      Buffer.Timeout_Registered := False;
      Buffer.Timeout_Id := Glib.Main.No_Source_Id;
      return False;
   end Automatic_Save;

   ----------------------
   -- Cursor_Move_Hook --
   ----------------------

   procedure Cursor_Move_Hook (Buffer : access Source_Buffer_Record'Class) is
   begin
      --  Clear the completion data if we are not already completing

      if not Buffer.Inserting then
         Reset_Completion_Data;
         End_Action (Buffer);
         Reset_Slave_Cursors_Commands (Source_Buffer (Buffer));
      end if;

      Location_Changed (Source_Buffer (Buffer));
   end Cursor_Move_Hook;

   ---------------
   -- Edit_Hook --
   ---------------

   procedure Edit_Hook (Buffer : access Source_Buffer_Record'Class) is
   begin
      --  Request re-parsing of the blocks

      if Buffer.Modifying_Editable_Lines then
         Buffer.Reset_Blocks_Info;
      end if;

      Register_Edit_Timeout (Buffer);
   end Edit_Hook;

   --------------------
   -- User_Edit_Hook --
   --------------------

   procedure User_Edit_Hook (Buffer : access Source_Buffer_Record'Class) is
      pragma Unreferenced (Buffer);
   begin
      Reset_Completion_Data;
   end User_Edit_Hook;

   ------------------
   -- Destroy_Hook --
   ------------------

   procedure Destroy_Hook (Buffer : access Source_Buffer_Record'Class) is
   begin
      --  ??? Must remove the line information column

      --  Unregister the blocks timeout

      if Buffer.Blocks_Timeout /= Glib.Main.No_Source_Id then
         Buffer.Blocks_Timeout_Registered := False;
         Glib.Main.Remove (Buffer.Blocks_Timeout);
         Buffer.Blocks_Timeout := Glib.Main.No_Source_Id;
      end if;

      if Buffer.Hightlight_Messages_Idle /= Glib.Main.No_Source_Id then
         Glib.Main.Remove (Buffer.Hightlight_Messages_Idle);
         Buffer.Hightlight_Messages_Idle := Glib.Main.No_Source_Id;
      end if;

      GNAT.Strings.Free (Buffer.Forced_Title);
   end Destroy_Hook;

   ----------
   -- Free --
   ----------

   procedure Free (Info : in out Extra_Information_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Extra_Information_Record, Extra_Information_Access);
   begin
      if Info /= null then
         GNAT.Strings.Free (Info.Identifier);
         Free (Info.Info);
         GNAT.Strings.Free (Info.Tooltip);
         GNAT.Strings.Free (Info.Icon);
         Unchecked_Free (Info);
      end if;
   end Free;

   --------------------
   -- Buffer_Destroy --
   --------------------

   procedure Buffer_Destroy (Data : System.Address; Buf : System.Address) is

      procedure Free (X : in out Line_Info_Width_Array);
      --  Free memory associated to X

      Stub    : Source_Buffer_Record;
      pragma Unreferenced (Data);
      pragma Warnings (Off, Stub);

      Buffer  : constant Source_Buffer :=
                  Source_Buffer (Get_User_Data (Buf, Stub));

      ----------
      -- Free --
      ----------

      procedure Free (X : in out Line_Info_Width_Array) is
      begin
         for J in X'Range loop
            Free (Buffer, X (J), Free_Messages => False);
         end loop;
      end Free;

   begin
      Trace (Me, "Destroying Buffer buffer="
             & System.Address_Image (Buffer.all'Address)
             & " widget="
             & System.Address_Image (Buf));
      Buffer.In_Destruction := True;

      --  Destroying listeners

      while not Buffer.Listeners.Is_Empty loop
         declare
            procedure Unchecked_Free is
              new Ada.Unchecked_Deallocation
                (Editor_Listener'Class, Editor_Listener_Access);

            Listener : Editor_Listener_Access :=
                         Buffer.Listeners.First_Element;

         begin
            Listener.Finalize;
            Unchecked_Free (Listener);
            Buffer.Listeners.Delete_First;
         end;
      end loop;

      --  We do not free memory associated to Buffer.Current_Command, since
      --  this command is already freed when freeing Buffer.Queue.

      Destroy_Hook (Buffer);

      if Buffer.Timeout_Registered then
         Glib.Main.Remove (Buffer.Timeout_Id);
         Buffer.Timeout_Registered := False;

         if Buffer.Filename /= GNATCOLL.VFS.No_File then
            Buffer.Delete_Autosaved_File (Buffer.Filename);
         end if;
      end if;

      --  Remove the undo redo queue if it is indeed the currently registered
      --  queue. It may happen that the focus is given to another editor
      --  before closing this one, in which case we do not want to reset
      --  the undo/redo here.

      if Get_Undo_Redo_Queue = Buffer.Queue then
         Remove_Controls (Buffer);
      end if;
      Empty_Queue (Buffer.Queue);

      Free_File_Information (Buffer);

      Free_Column_Info (Buffer.Editable_Line_Info_Columns);
      Unchecked_Free (Buffer.Editable_Line_Info_Columns);

      if Buffer.Editable_Lines /= null then
         Reset_Blocks_Info (Buffer);
         Unchecked_Free (Buffer.Editable_Lines);
      end if;

      for J in Buffer.Line_Data'Range loop
         for K in Highlight_Location loop
            Unchecked_Free (Buffer.Line_Data (J).Highlighting (K).Enabled);
         end loop;

         if Buffer.Line_Data (J).Side_Info_Data /= null then
            Free (Buffer.Line_Data (J).Side_Info_Data.all);
            Unchecked_Free (Buffer.Line_Data (J).Side_Info_Data);
         end if;
      end loop;

      Unchecked_Free (Buffer.Line_Data);
      GNAT.Strings.Free (Buffer.Charset);

      Unref (Buffer.Non_Editable_Tag);
      Unchecked_Free (Buffer.Highlighter);
      Unref (Buffer.Hyper_Mode_Tag);

      Reset_Completion_Data;
   end Buffer_Destroy;

   ---------------------
   -- Changed_Handler --
   ---------------------

   procedure Changed_Handler (Buffer : access Source_Buffer_Record'Class) is
   begin
      Emit_New_Cursor_Position (Buffer);

      if Buffer.Modifying_Editable_Lines then
         Buffer.Modified_Auto := True;
      end if;

   exception
      when E : others =>
         Trace (Me, E);
   end Changed_Handler;

   ----------------------
   -- Mark_Set_Handler --
   ----------------------

   procedure Mark_Set_Handler
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues)
   is
      Mark : constant Gtk_Text_Mark :=
               Get_Text_Mark (Glib.Values.Nth (Params, 2));
      Iter : Gtk_Text_Iter;
   begin
      --  Emit the new cursor position if it is the Insert_Mark that was
      --  changed.

      if Buffer.Setting_Mark then
         return;
      end if;

      Buffer.Setting_Mark := True;

      --  If the mark being moved corresponds to the selection_bound (which is
      --  moved each time the insert cursor or the selection changes), emit
      --  the corresponding signal.

      if Mark = Buffer.Insert_Mark then

         if Buffer.Cursors_Sync.Mode = Auto then
            Remove_All_Slave_Cursors (Source_Buffer (Buffer));
         end if;

         --  If we are not currently in an undo/redo group, moving the
         --  cursor should break the grouping of actions. This way, if you
         --  are typing 'a' then clicking then typing 'b', GNAT Studio should
         --  require two undos to remove 'a' then 'b'.
         if not Buffer.Inserting then
            Change_Group (Buffer.Queue);
         end if;

         Emit_New_Cursor_Position (Buffer);
         Cursor_Move_Hook (Buffer);
         Buffer.Get_Iter_At_Mark (Iter, Mark);
         Buffer.Cursor_Column_Memory := Get_Line_Offset (Iter);

         declare
            Editable_Line  : Editable_Line_Type;
            Visible_Column : Visible_Column_Type;
         begin
            Get_Iter_Position
              (Source_Buffer (Buffer), Iter, Editable_Line, Visible_Column);

            for Listener of Buffer.Listeners loop
               Listener.After_Cursor_Moved
                 (Buffer.Editor_Buffer.New_Location
                    (Integer (Editable_Line),
                     Visible_Column),
                  not Buffer.Inserting);
            end loop;
         end;
      end if;

      for Cursor of Buffer.Slave_Cursors_List loop
         if Cursor.Mark = Mark then
            Buffer.Get_Iter_At_Mark (Iter, Mark);
            Cursor.Column_Memory := Get_Line_Offset (Iter);
         end if;
      end loop;

      Buffer.Setting_Mark := False;

   exception
      when E : others =>
         Trace (Me, E);
   end Mark_Set_Handler;

   --------------------
   -- Insert_Text_Cb --
   --------------------

   procedure Insert_Text_Cb
     (Buffer          : access Source_Buffer_Record'Class;
      End_Insert_Iter : Gtk.Text_Iter.Gtk_Text_Iter) is
   begin
      if not Buffer.Modifying_Editable_Lines then
         return;
      end if;

      Source_Buffer (Buffer).Highlighter.Update_Highlight_Region
        (End_Insert_Iter);
   end Insert_Text_Cb;

   -----------------------
   -- After_Insert_Text --
   -----------------------

   procedure After_Insert_Text
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues)
   is
      Text   : constant Unchecked_String_Access :=
                 To_Unchecked_String (Get_Chars (Nth (Params, 2)));
      Length : constant Integer := Integer (Get_Int (Nth (Params, 3)));
      Start  : Buffer_Line_Type;
      Iter   : Gtk_Text_Iter;
      Start_Iter   : Gtk_Text_Iter;
      Start_Line   : Editable_Line_Type;
      Start_Column : Visible_Column_Type;
      Ignored      : Boolean;
      Number       : Buffer_Line_Type := 0;
   begin
      Update_Logical_Timestamp (Buffer);

      Get_Text_Iter (Nth (Params, 1), Iter);

      --  Get the coordinates of the start
      Copy (Iter, Start_Iter);
      Backward_Chars
        (Start_Iter, Gint (UTF8_Strlen (Text (1 .. Length))),
         Ignored);
      Get_Iter_Position
        (Source_Buffer (Buffer), Start_Iter, Start_Line, Start_Column);

      --  After a text insertion, both the selection mark and the insertion
      --  mark are at the location where the insertion ended. Update the
      --  corresponding editor command.

      declare
         C : constant Editor_Command := Get_Current_Command (Buffer);
      begin
         if C /= null
           and then not Buffer.Inserting
           and then Buffer.Cursors_Sync.Mode = Manual_Slave
         then
            C.Set_End_Location (Iter);
         end if;
      end;

      --  Call Add_Lines, to compute added lines for the side column

      Start := Buffer_Line_Type (Get_Line (Iter) + 1);

      for J in 1 .. Length loop
         if Text (J) = ASCII.LF then
            Number := Number + 1;
         end if;
      end loop;

      if Number > 0 then
         Lines_Add_Hook (Buffer, Start - Number, Number);

         Emit_New_Cursor_Position (Buffer);
         --  This is already done when the cursor is moved but it is too early
         --  when new lines are inserted at the end of the buffer: the fact
         --  that lines have been added as to be reflected in the buffer data
         --  before the status bar is refereshed.
         --  When indentation is enabled "cursor_position_changed" is emitted
         --  twice (once after line information has been recomputed) and the
         --  status bar is properly refreshed. This is not the case when
         --  auto indentation is not performed (preference disabled or
         --  buffer language unknown). As a result, the status bar report a
         --  cursor on line 0.
      end if;

      --  Perform insertion for every multi cursor
      --  If we are in auto mode
      if Buffer.Cursors_Sync.Mode = Auto then
         declare
            Iter : Gtk_Text_Iter;
            G : Group_Block := Current_Group (Buffer.Queue);
         begin
            for C of Get_Cursors (Source_Buffer (Buffer)) loop
               if not C.Is_Main_Cursor then
                  --  Perform insertion for the multi cursor
                  Set_Manual_Sync (C);
                  Buffer.Get_Iter_At_Mark (Iter, C.Cursor.Mark);
                  Buffer.Insert (Iter, Text (1 .. Length));
               end if;
            end loop;

            Set_Cursors_Auto_Sync (Source_Buffer (Buffer));

            declare
               Iter_Acc : Gtk_Text_Iter renames
                 Iter_Access_Address_Conversions.To_Pointer
                   (Get_Address (Nth (Params, 1))).all;

            begin
               Buffer.Get_Iter_At_Mark (Iter_Acc, Buffer.Insert_Mark);
            end;
         end;
      end if;

      Update_All_Column_Memory (Buffer);

      if Buffer.Modifying_Editable_Lines then
         for Listener of Buffer.Listeners loop
            Listener.After_Insert_Text
              (Buffer.Editor_Buffer.New_Location
                 (Integer (Start_Line), Start_Column),
               Text (1 .. Length),
               not Buffer.Inserting);
         end loop;
      end if;

      --  Emit the Character_Added hook. Do this only if we are appending only
      --  one character. Eliminate the obvious cases when we are writing more
      --  than one character, so as not to have to perform UTF8 computations
      --  in these cases.

      if Number = 0 and then Length < 4 then
         declare
            Index : Natural;
         begin
            Index := UTF8_Find_Next_Char (Text (1 .. Length), Text'First);

            if Index > Length then
               Character_Added
                 (Source_Buffer (Buffer),
                  UTF8_Get_Char (Text (1 .. Length)),
                  Interactive => not Buffer.Inserting);
            end if;
         end;
      end if;

      --  We might have invalidate the original iter, so retrieve a new one
      --  at the insert mark's location, forwarding it by the number of
      --  chars that are being added, to highlight the inserted text.

      declare
         End_Iter : Gtk_Text_Iter;
         Success  : Boolean := False;
      begin
         Src_Editor_Buffer.Text_Handling.Get_Iter
           (Buffer => Buffer,
            Iter   => End_Iter,
            Line   => Start_Line,
            Column => Collapse_Tabs
              (Buffer => Buffer,
               Line   => Start_Line,
               Column => Start_Column));
         Forward_Chars
           (Iter   => End_Iter,
            Count  => Gint (Length),
            Result => Success);

         if Buffer.Lang /= null
           and then Get_Language_Context (Buffer.Lang).Syntax_Highlighting
         then
            Insert_Text_Cb
              (Buffer          => Buffer,
               End_Insert_Iter => End_Iter);
         end if;
      end;

      declare
         Address  : constant System.Address := Get_Address (Nth (Params, 1));
         Insert_Iter : Gtk_Text_Iter;
         Original : Gtk_Text_Iter
           with Import, Address => Address;
      begin
         Buffer.Get_Iter_At_Mark (Insert_Iter, Buffer.Insert_Mark);

         --  From Gtk "insert-text" signal documentation:
         --  if your handler runs before the default handler it must not
         --  invalidate the location iter (or has to revalidate it).
         --  Let's revalidate original TextIter:
         Copy (Insert_Iter, Dest => Original);
      end;

   exception
      when E : others =>
         Trace (Me, E);
   end After_Insert_Text;

   ------------------------
   -- Before_Insert_Text --
   ------------------------

   procedure Before_Insert_Text
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues)
   is

      procedure Update_Insert_Command
        (Buffer : Source_Buffer;
         User_Action : Action_Type;
         Command : out Editor_Command;
         Pos : Gtk_Text_Iter;
         Sel_Pos : Gtk_Text_Iter;
         Text : String;
         Is_Main_Action : Boolean := True);
      --  Update the command with the given action

      procedure Update_Insert_Command
        (Buffer         : Source_Buffer;
         User_Action    : Action_Type;
         Command        : out Editor_Command;
         Pos            : Gtk_Text_Iter;
         Sel_Pos        : Gtk_Text_Iter;
         Text           : String;
         Is_Main_Action : Boolean := True)
      is
         Line, Sel_Line : Editable_Line_Type;
         Col, Sel_Col   : Character_Offset_Type;

         procedure End_Action;
         procedure End_Action is
         begin
            if Is_Main_Action then
               End_Action (Buffer);
            end if;
         end End_Action;

         procedure Create_And_Enqueue_Command;
         procedure Create_And_Enqueue_Command is
            C : constant Src_Editor_Buffer.Cursors.Cursor :=
              (if Buffer.Cursors_Sync.Mode = Manual_Slave
               then Src_Editor_Buffer.Cursors.Create
                 (Buffer.Cursors_Sync.MC, Buffer)
               else Get_Main_Cursor (+Buffer));
         begin
            Create
              (Item          => Command,
               Mode          => Insertion,
               Buffer        => Buffer,
               User_Executed => False,
               Cursor_Loc    => (Line, Col),
               Sel_Loc       => (Sel_Line, Sel_Col),
               C             => C);
            Enqueue (Buffer, Command_Access (Command), User_Action);
         end Create_And_Enqueue_Command;

      begin
         Get_Iter_Position (Buffer, Pos, Line, Col);
         Get_Iter_Position (Buffer, Sel_Pos, Sel_Line, Sel_Col);

         if Is_Null_Command (Command) then
            Create_And_Enqueue_Command;
         elsif Get_Mode (Command) = Insertion then
            if (User_Action = Insert_Spaces
                and then Buffer.Last_User_Action /= Insert_Spaces)
              or else
                (User_Action = Insert_Line
                 and then Buffer.Last_User_Action /= Insert_Line)
            then
               End_Action;
               Create_And_Enqueue_Command;
            end if;
         else
            End_Action;
            Create_And_Enqueue_Command;
         end if;

         Add_Text (Command, Text);

         Buffer.Set_Current_Command (Command);
      end Update_Insert_Command;

      Text         : constant Unchecked_String_Access :=
        To_Unchecked_String (Get_Chars (Nth (Params, 2)));
      Length       : constant Integer := Integer (Get_Int (Nth (Params, 3)));
      Pos, Sel_Pos : Gtk_Text_Iter;
      Command      : Editor_Command := Get_Current_Command (Buffer);
      Line         : Editable_Line_Type;
      User_Action  : Action_Type;
      Sel_Mark     : Gtk_Text_Mark := Buffer.Get_Selection_Bound;
      Cursor_Previously_Held : Boolean;

   begin
      if Buffer.Inserting_Count > 0 then
         --  Setting Insertion_Position by default (old behavior),
         --  in other words do not use settings from previous
         --  (non internal) insert.
         Buffer.Inserting_Position := At_Begin;
         return;
      end if;

      --  If in multi cursors manual slave mode, update corresponding command
      --  and sel mark
      if Buffer.Cursors_Sync.Mode = Manual_Slave then
         Sel_Mark := Buffer.Cursors_Sync.MC.Sel_Mark;
      end if;

      Get_Text_Iter (Nth (Params, 1), Pos);
      Line := Get_Editable_Line
        (Buffer, Buffer_Line_Type (Get_Line (Pos) + 1));

      if Starts_Line (Pos) then
         Buffer.Inserting_Position := At_Begin;

      elsif Ends_Line (Pos) then
         Buffer.Inserting_Position := At_End;

      else
         Buffer.Inserting_Position := Other;
      end if;

      --  Move mark of start of re-highlight area into insertion position
      Move_Mark (Buffer, Buffer.Highlighter.First_Highlight_Mark, Pos);

      if Line = 0 then
         --  In a special line: we simply stop propagation
         Emit_Stop_By_Name (Object => Buffer, Name => "insert_text");
         return;
      end if;

      if Buffer.Prevent_CR_Insertion then
         Buffer.Prevent_CR_Insertion := False;

         declare
            T        : String (1 .. Length);
            Last     : Integer := 1;
            CR_Found : Boolean := False;
            Ignore   : Boolean;
            pragma Unreferenced (Ignore);

         begin
            --  Copy Text in T, replacing CRLF by LF and CR by LF
            for J in 1 .. Length loop
               if Text (J) = ASCII.CR then
                  CR_Found := True;

                  if J = Length or else Text (J + 1) /= ASCII.LF then
                     T (Last) := ASCII.LF;
                     Last := Last + 1;
                  end if;
               else
                  T (Last) := Text (J);
                  Last := Last + 1;
               end if;
            end loop;

            Last := Last - 1;

            if CR_Found then
               --  If we have found a CR in the text, block the current
               --  insertion and write the stripped text instead.

               Emit_Stop_By_Name (Object => Buffer, Name => "insert_text");
               Ignore := Insert_Interactive_At_Cursor
                 (Buffer, T (1 .. Last), True);
               return;
            end if;
         end;
      end if;

      --  We are editing characters on a line: unfold the block below, so
      --  that the folding data remains in sync even if we remove the
      --  information that justified the folding.
      --
      --  For instance, if the text is
      --
      --    1  procedure hello is
      --    2  begin
      --    3     null;
      --    3  end hello;
      --
      --  If the block is folded and the user inserts a line break in line 1,
      --  there is no longer a reason that there should be folded data
      --  below this line.

      if not Lines_Are_Real (Buffer) then
         declare
            Result, Ignore : Boolean := False;
            pragma Unreferenced (Ignore);
         begin
            --  The line which receives the insert_text event is necessarily
            --  folded: we want to unfold the text starting with the following
            --  line to effectively unfold the block.
            if Line < Buffer.Last_Editable_Line then
               Result := Fold_Unfold_Line (Buffer, Line + 1, False);
            end if;

            if Result then
               --  We did unfold a block: stop propagation and insert the
               --  new text at the cursor position.
               Emit_Stop_By_Name (Object => Buffer, Name => "insert_text");

               Ignore := Insert_Interactive_At_Cursor
                 (Buffer, Text (1 .. Length), True);
               return;
            end if;
         end;
      end if;

      Edit_Hook (Buffer);
      Cursor_Move_Hook (Buffer);

      if Buffer.Inserting then
         return;
      end if;

      --  Past this point, we know we are dealing with an user action

      User_Edit_Hook (Buffer);

      if Length = 1
        and then (Text (1) = ' ' or else Text (1) = ASCII.HT)
      then
         User_Action := Insert_Spaces;
      elsif Length = 1 and then Text (1) = ASCII.LF then
         User_Action := Insert_Line;
      else
         User_Action := Insert_Text;
      end if;

      Cursor_Previously_Held := Buffer.No_Cursor_Move_On_Changes;

      --  If there is a selection and we are inserting, this might be a drag
      --  and drop action - in this case, prevent cursor changes in reaction to
      --  insertion, as they would result in losing the selection, and Gtk+
      --  needs the selection to know which text to delete after the drop.
      if Buffer.Selection_Exists then
         Buffer.No_Cursor_Move_On_Changes := True;
      end if;

      Buffer.Get_Iter_At_Mark (Sel_Pos, Sel_Mark);
      Update_Insert_Command
        (Source_Buffer (Buffer),
         User_Action, Command, Pos, Sel_Pos, Text (1 .. Length),
         Is_Main_Action => Buffer.Cursors_Sync.Mode /= Manual_Slave);

      Buffer.No_Cursor_Move_On_Changes := Cursor_Previously_Held;

   exception
      when E : others =>
         Trace (Me, E);
   end Before_Insert_Text;

   ---------------------
   -- Delete_Range_Cb --
   ---------------------

   procedure Delete_Range_Cb
     (Buffer : access Source_Buffer_Record'Class;
      Iter   : Gtk_Text_Iter) is
   begin
      Source_Buffer (Buffer).Highlighter.Update_Highlight_Region (Iter);
   end Delete_Range_Cb;

   ------------------------
   -- After_Delete_Range --
   ------------------------

   procedure After_Delete_Range
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues)
   is
      Start_Iter : Gtk_Text_Iter;
      End_Iter   : Gtk_Text_Iter;
   begin
      Update_Logical_Timestamp (Buffer);

      Get_Text_Iter (Nth (Params, 1), Start_Iter);
      Get_Text_Iter (Nth (Params, 2), End_Iter);

      --  Move mark of start of re-highlight area into insertion position

      Move_Mark (Buffer, Buffer.Highlighter.First_Highlight_Mark, Start_Iter);

      declare
         C : constant Editor_Command := Get_Current_Command (Buffer);
      begin
         if C /= null
           and then
             (not Buffer.Inserting
              or else Buffer.Cursors_Sync.Mode = Manual_Slave)
         then
            C.Set_End_Location (Start_Iter);
         end if;
      end;

      if Buffer.Lang /= null
        and then Get_Language_Context (Buffer.Lang).Syntax_Highlighting
      then
         Delete_Range_Cb (Buffer, Start_Iter);
      end if;

      if Buffer.First_Removed_Line > 0 then
         Lines_Remove_Hook_After
           (Buffer, Buffer.First_Removed_Line, Buffer.Last_Removed_Line);
         Buffer.First_Removed_Line := 0;
      end if;

      if Buffer.Modifying_Editable_Lines then
         for Listener of Buffer.Listeners loop
            Listener.After_Delete_Range (not Buffer.Inserting);
         end loop;
      end if;

      Character_Added
        (Source_Buffer (Buffer), 8,
         Interactive => not Buffer.Inserting);

      if Buffer.Cursors_Delete_Offset /= 0 and then
        Buffer.Cursors_Sync.Mode = Auto
      then
         declare
            Iter_1, Iter_2 : Gtk_Text_Iter;
            G : Group_Block := Current_Group (Buffer.Queue);
         begin
            for C of Get_Cursors (Source_Buffer (Buffer)) loop
               if not C.Is_Main_Cursor then
                  Set_Manual_Sync (C);
                  Buffer.Get_Iter_At_Mark (Iter_1, C.Cursor.Mark);
                  Buffer.Get_Iter_At_Offset
                    (Iter_2,
                     Get_Offset (Iter_1) + Buffer.Cursors_Delete_Offset);
                  Buffer.Delete (Iter_1, Iter_2);
               end if;
            end loop;
            Set_Cursors_Auto_Sync (Source_Buffer (Buffer));
         end;
      end if;

      Update_All_Column_Memory (Buffer);

      Buffer.Get_Iter_At_Mark (Start_Iter, Buffer.Insert_Mark);

      if Buffer_Line_Type (Get_Line (Start_Iter)) in Buffer.Line_Data'Range
        and then not Starts_Line (Start_Iter)
        and then not Ends_Line (Start_Iter)
      then
         --  Start line was not removed completely, check if all messages
         --  still have corresponding locations in source code

         declare
            use Message_Reference_List;
            Src  : Line_Info_Width_Array_Access renames
              Buffer.Line_Data (Buffer_Line_Type
                                (Get_Line (Start_Iter))).Side_Info_Data;

            Msg          : Message_Access;
            For_Deleting : Message_Reference_List.List;

         begin
            for Index in Src'Range loop
               for Ref of Src (Index).Messages loop
                  Msg := Message (Ref);
                  if Msg /= null
                    and then not Msg.Get_Editor_Mark.Is_Present
                  then
                     For_Deleting.Append (Ref);
                  end if;
               end loop;
            end loop;

            while not For_Deleting.Is_Empty loop
               Msg := Message (For_Deleting.First_Element);
               if Msg /= null then
                  Remove (Msg);
               end if;
               For_Deleting.Delete_First;
            end loop;
         end;
      end if;

   exception
      when E : others =>
         Trace (Me, E);
   end After_Delete_Range;

   -------------------------
   -- Before_Delete_Range --
   -------------------------

   procedure Before_Delete_Range
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues)
   is
      Start_Iter          : Gtk_Text_Iter;
      End_Iter            : Gtk_Text_Iter;
      Command             : Editor_Command := Get_Current_Command (Buffer);
      Direction           : Direction_Type := Extended;
      Line, Column        : Gint;
      Line_Start          : Gint;
      Column_Start        : Gint;
      Line_End            : Gint;
      Column_End          : Gint;
      Line_Count          : Gint;
      Editable_Line_Start : Editable_Line_Type;
      Editable_Line_End   : Editable_Line_Type;
      Delete_Offset       : Gint := 0;
      Sel_Mark            : Gtk_Text_Mark := Buffer.Get_Selection_Bound;
      Sel_Pos_Iter        : Gtk_Text_Iter;
      Sel_Pos             : Loc_T;

      First_Buffer_Line_To_Remove : Buffer_Line_Type;
      Last_Buffer_Line_To_Remove  : Buffer_Line_Type;

      procedure Get_Current_Cursor_Position
        (Line   : out Gint;
         Column : out Gint);
      --  Same as get cursor position, but takes multi-cursors into account

      Cursor_Pos : Loc_T;

      procedure Get_Current_Cursor_Position
        (Line   : out Gint;
         Column : out Gint)
      is
         Mark : Gtk_Text_Mark;
         Iter : Gtk_Text_Iter;
      begin

         if Buffer.Cursors_Sync.Mode = Manual_Slave then
            Mark := Buffer.Cursors_Sync.MC.Mark;
         else
            Mark := Buffer.Insert_Mark;
         end if;

         Get_Iter_At_Mark (Buffer, Iter, Mark);
         Line   := Get_Line (Iter);
         Column := Get_Line_Offset (Iter);

         Get_Iter_Position
           (Source_Buffer (Buffer),
            Iter, Cursor_Pos.Line, Cursor_Pos.Col);
      end Get_Current_Cursor_Position;

   begin
      --  If in multi cursors manual slave mode, update corresponding command
      if Buffer.Cursors_Sync.Mode = Manual_Slave then
         Sel_Mark := Buffer.Cursors_Sync.MC.Sel_Mark;
      end if;

      Get_Text_Iter (Nth (Params, 1), Start_Iter);
      Get_Text_Iter (Nth (Params, 2), End_Iter);

      --  Determine the direction mode for the delete action

      Get_Current_Cursor_Position (Line, Column);

      Line_Start   := Get_Line (Start_Iter);
      Column_Start := Get_Line_Offset (Start_Iter);
      Line_End     := Get_Line (End_Iter);
      Column_End   := Get_Line_Offset (End_Iter);

      if Line = Line_Start and then Column = Column_Start then
         Direction := Backward;
      end if;

      if Line = Line_End and then Column = Column_End then
         Direction := Forward;
      end if;

      if Buffer.Cursors_Sync.Mode = Auto then
         Delete_Offset := (Get_Offset (End_Iter) - Get_Offset (Start_Iter));
         Delete_Offset := Delete_Offset * (case Direction is
                                           when Forward => -1,
                                           when Backward => 1,
                                           when Extended => 0);
         Buffer.Cursors_Delete_Offset := Delete_Offset;
      end if;

      Editable_Line_Start :=
        Get_Editable_Line (Buffer, Buffer_Line_Type (Line_Start + 1));

      Editable_Line_End :=
        Get_Editable_Line (Buffer, Buffer_Line_Type (Line_End + 1));

      Buffer.Get_Iter_At_Mark (Sel_Pos_Iter, Sel_Mark);
      Get_Iter_Position
        (Source_Buffer (Buffer), Sel_Pos_Iter, Sel_Pos.Line, Sel_Pos.Col);

      --  If there are non-editable lines in the range, intercept the deletion

      if not Buffer.Inserting
        and then Has_Special_Lines
          (Buffer,
           Buffer_Line_Type (Line_Start + 1),
           Buffer_Line_Type (Line_End + 1))
      then
         --  Intercept default propagation, we want to flatten the area
         --  before continuing.
         Emit_Stop_By_Name (Buffer, "delete_range");

         --  If we are just deleting in one special line, ignore

         if Editable_Line_Start = 0 and then Editable_Line_End = 0 then
            return;
         end if;

         while Editable_Line_Start = 0 loop
            Line_Start := Line_Start - 1;
            exit when Line_Start <= 0;

            Editable_Line_Start :=
              Get_Editable_Line (Buffer, Buffer_Line_Type (Line_Start + 1));
         end loop;

         if Editable_Line_Start = 0 then
            First_Buffer_Line_To_Remove := 1;
            Editable_Line_Start := 1;
         else
            First_Buffer_Line_To_Remove := Buffer_Line_Type (Line_Start + 1);
         end if;

         Line_Count := Buffer.Get_Line_Count;

         while Editable_Line_End = 0 loop
            Line_End := Line_End + 1;

            Editable_Line_End :=
              Get_Editable_Line (Buffer, Buffer_Line_Type (Line_End + 1));

            exit when Line_End = Line_Count;
         end loop;

         Last_Buffer_Line_To_Remove := Buffer_Line_Type (Line_End + 1);

         declare
            Expanded : Boolean;
            M_Start, M_End : Gtk_Text_Mark;
            I_Start, I_End : Gtk_Text_Iter;
            Ignored  : Boolean;
         begin
            M_Start := Buffer.Create_Mark (Where => Start_Iter);
            M_End   := Buffer.Create_Mark (Where => End_Iter);

            Expanded := Flatten_Area
              (Buffer            => Buffer,
               Start_Line        => Editable_Line_Start,
               End_Line          => Editable_Line_End,
               Start_Buffer_Line => First_Buffer_Line_To_Remove,
               End_Buffer_Line   => Last_Buffer_Line_To_Remove);

            Buffer.Get_Iter_At_Mark (I_Start, M_Start);
            Buffer.Get_Iter_At_Mark (I_End, M_End);

            Delete_Mark (Buffer, M_Start);
            Delete_Mark (Buffer, M_End);

            --  Re-launch the deletion unless we are expanding a folded line,
            --  in which case do nothing.
            if not Expanded then
               Delete_Interactive (Buffer           => Buffer,
                                   Start_Iter       => I_Start,
                                   End_Iter         => I_End,
                                   Default_Editable => True,
                                   Result           => Ignored);
            end if;
            return;
         end;
      end if;

      if Buffer.Modifying_Editable_Lines then
         for Listener of Buffer.Listeners loop
            Listener.Before_Delete_Range
              (Buffer.Editor_Buffer.New_Location
                 (Integer (Editable_Line_Start),
                  Visible_Column_Type (Column_Start + 1)),
               Buffer.Editor_Buffer.New_Location
                 (Integer (Editable_Line_End),
                  Visible_Column_Type (Column_End + 1)),
               not Buffer.Inserting);
         end loop;
      end if;

      --  Remove the lines in the side information column
      declare
         From, To, Count : Buffer_Line_Type;
      begin
         From := Buffer_Line_Type (Line_Start + 1);
         To   := Buffer_Line_Type (Line_End + 1);

         if From /= To then
            Count := To - From;

            if Starts_Line (Start_Iter) then
               --  The Start_Iter is on the start of line, so we need
               --  to remove this line.
               From := From - 1;
            end if;

            Lines_Remove_Hook_Before (Buffer, From, Count);

         else
            --  We are editing characters on a line: unfold the block below, so
            --  that the folding data remains in sync even if we remove the
            --  information that justified the folding.
            --
            --  For instance, if the text is
            --
            --    1  procedure hello is
            --    2  begin
            --    3     null;
            --    3  end hello;
            --
            --  If the block is folded and the user removes the "is" in line 1,
            --  there is no longer a reason that there should be folded data
            --  below this line.

            if not Lines_Are_Real (Buffer) then
               declare
                  Result : Boolean;
                  M1     : Gtk_Text_Mark;
                  M2     : Gtk_Text_Mark;
               begin
                  M1 := Create_Mark (Buffer, "", Start_Iter);
                  M2 := Create_Mark (Buffer, "", End_Iter);

                  Result := Fold_Unfold_Line
                    (Buffer, Editable_Line_Start, False);
                  if Result then
                     --  We have changed the buffer:
                     --  stop propagation and reemit
                     Emit_Stop_By_Name (Buffer, "delete_range");

                     Get_Iter_At_Mark (Buffer, Start_Iter, M1);
                     Get_Iter_At_Mark (Buffer, End_Iter, M2);

                     Delete_Mark (Buffer, M1);
                     Delete_Mark (Buffer, M2);
                     Delete (Buffer, Start_Iter, End_Iter);
                     return;
                  end if;
               end;
            end if;
         end if;
      end;

      Edit_Hook (Buffer);
      Cursor_Move_Hook (Buffer);

      if Buffer.Inserting then
         return;
      end if;

      User_Edit_Hook (Buffer);

      if not Is_Null_Command (Command)
        and then (Get_Mode (Command) /= Deletion
                  or else Get_Direction (Command) /= Direction)
      then
         End_Action (Buffer);
         Command := Get_Current_Command (Buffer);
      end if;

      declare
         Slice        : constant Basic_Types.UTF8_String :=
           Get_Slice (Buffer, Start_Iter, End_Iter);
         User_Action  : Action_Type;
      begin

         if Slice = "" & ASCII.LF then
            User_Action := Delete_Line;
         elsif Slice = " " or else Slice = "" & ASCII.HT then
            User_Action := Delete_Spaces;
         else
            User_Action := Delete_Text;
         end if;

         if Is_Null_Command (Command) then
            Create
              (Command,
               Deletion,
               Source_Buffer (Buffer),
               True,
               Cursor_Loc  => Cursor_Pos,
               Sel_Loc     => Sel_Pos,
               Direction   => Direction,
               C           =>
                 (if Buffer.Cursors_Sync.Mode = Manual_Slave
                  then Create
                    (Buffer.Cursors_Sync.MC, Source_Buffer (Buffer))
                  else Get_Main_Cursor (+Buffer)));

            Enqueue (Buffer, Command_Access (Command), User_Action);

            Add_Text (Command, Slice);
         else
            if Direction = Forward then
               Add_Text
                 (Command,
                  Slice,
                  Get_Editable_Line
                    (Buffer, Buffer_Line_Type (Line_Start + 1)),
                  Character_Offset_Type (Column_Start + 1));
            else
               Add_Text (Command, Slice);
            end if;
         end if;
      end;

      Buffer.Set_Current_Command (Command);

   exception
      when E : others =>
         Trace (Me, E);
   end Before_Delete_Range;

   -----------------------------
   -- Line_Highlights_Changed --
   -----------------------------

   procedure Line_Highlights_Changed
     (Buffer : access Source_Buffer_Record'Class) is
   begin
      Emit_By_Name
        (Get_Object (Buffer), Signal_Line_Highlights_Changed & ASCII.NUL);
   end Line_Highlights_Changed;

   --------------------------------
   -- Buffer_Information_Changed --
   --------------------------------

   procedure Buffer_Information_Changed
     (Buffer : access Source_Buffer_Record'Class) is
   begin
      Emit_By_Name
        (Get_Object (Buffer), Signal_Buffer_Information_Changed & ASCII.NUL);
   end Buffer_Information_Changed;

   --------------------
   -- Status_Changed --
   --------------------

   procedure Status_Changed
     (Buffer : access Source_Buffer_Record'Class) is
   begin
      Emit_By_Name (Get_Object (Buffer), Signal_Status_Changed & ASCII.NUL);
   end Status_Changed;

   ----------------------
   -- Filename_Changed --
   ----------------------

   procedure Filename_Changed
     (Buffer : access Source_Buffer_Record'Class) is
   begin
      Emit_By_Name (Get_Object (Buffer), Signal_Filename_Changed & ASCII.NUL);
   end Filename_Changed;

   ---------------------
   -- Set_Last_Status --
   ---------------------

   procedure Set_Last_Status
     (Buffer : access Source_Buffer_Record'Class;
      Status : Status_Type) is
   begin
      if Status /= Buffer.Current_Status then
         Buffer.Current_Status := Status;
         Status_Changed (Buffer);
      end if;
   end Set_Last_Status;

   ------------------------------
   -- Emit_New_Cursor_Position --
   ------------------------------

   procedure Emit_New_Cursor_Position
     (Buffer : access Source_Buffer_Record'Class)
   is
      procedure Emit_By_Name
        (Object : System.Address;
         Name   : String;
         Line   : Gint;
         Column : Gint);
      pragma Import (C, Emit_By_Name, "ada_g_signal_emit_by_name_int_int");

      L, C : Gint;
   begin
      if Buffer.Do_Not_Move_Cursor then
         return;
      end if;

      Get_Screen_Position (Buffer, L, C);

      Emit_By_Name
        (Get_Object (Buffer), "cursor_position_changed" & ASCII.NUL,
         Line   => Gint (Get_Editable_Line (Buffer, Buffer_Line_Type (L + 1))),
         Column => C + 1);

      Buffer.Highlighter.Remove_Delimiters_Highlighting;
   end Emit_New_Cursor_Position;

   ----------------------------
   -- Generic_Valid_Position --
   ----------------------------

   procedure Generic_Valid_Position
     (Buffer : Source_Buffer;
      Iter   : out Gtk_Text_Iter;
      Found  : out Boolean;
      Line   : Gint;
      Column : Gint := 0) is
   begin
      --  First check that Line does not exceed the number of lines
      --  in the buffer.

      if Column < 0
        or else Line >= Get_Line_Count (Buffer)
      then
         Found := False;
         return;
      end if;

      Get_Iter_At_Line_Offset (Buffer, Iter, Line, 0);

      if Column = 0 then
         Found := True;
      elsif Line_Length (Iter) >= Column then
         Set_Pos (Iter, Column);
         Found := True;
      else
         Found := False;
      end if;
   end Generic_Valid_Position;

   ------------------
   -- Is_Valid_Pos --
   ------------------

   procedure Is_Valid_Pos is new
     Generic_Valid_Position (Get_Chars_In_Line, Set_Line_Offset);
   --  Column should be given in characters, not in bytes

   -----------------------
   -- Is_Valid_Position --
   -----------------------

   function Is_Valid_Position
     (Buffer : access Source_Buffer_Record;
      Line   : Gint;
      Column : Gint := 0) return Boolean
   is
      Iter  : Gtk_Text_Iter;
      Found : Boolean;
   begin
      Is_Valid_Pos (Source_Buffer (Buffer), Iter, Found, Line, Column);
      return Found;
   end Is_Valid_Position;

   function Is_Valid_Position
     (Buffer : access Source_Buffer_Record;
      Line   : Editable_Line_Type;
      Column : Character_Offset_Type := 1) return Boolean
   is
      Buffer_Line : constant Buffer_Line_Type :=
                      Get_Buffer_Line (Buffer, Line);

   begin
      if Buffer_Line = 0 then
         if Line in 1 .. Buffer.Last_Editable_Line then
            return True;
         else
            Trace (Me, "Invalid Buffer Line");
            return False;
         end if;

      else
         return Is_Valid_Position
           (Buffer, Gint (Buffer_Line - 1), Gint (Column - 1));
      end if;
   end Is_Valid_Position;

   function Is_Valid_Position
     (Buffer : access Source_Buffer_Record;
      Line   : Editable_Line_Type;
      Column : Visible_Column_Type) return Boolean is
   begin
      return Is_Valid_Position
        (Buffer, Line, Collapse_Tabs (Buffer, Line, Column));
   end Is_Valid_Position;

   ---------------------------
   -- Ensure_Valid_Position --
   ---------------------------

   procedure Ensure_Valid_Position
     (Buffer : access Source_Buffer_Record;
      Line   : Editable_Line_Type;
      Column : Character_Offset_Type := 1) is
   begin
      if not Is_Valid_Position (Buffer, Line, Column) then
         raise Location_Exception with (-"Invalid Buffer Line");
      end if;
   end Ensure_Valid_Position;

   procedure Ensure_Valid_Position
     (Buffer : access Source_Buffer_Record;
      Line   : Editable_Line_Type;
      Column : Visible_Column_Type) is
   begin
      if not Is_Valid_Position (Buffer, Line, Column) then
         raise Location_Exception with (-"Invalid Buffer Line");
      end if;
   end Ensure_Valid_Position;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Buffer : out Source_Buffer;
      Kernel : GPS.Kernel.Kernel_Handle;
      Lang   : Language.Language_Access := null) is
   begin
      Buffer := new Source_Buffer_Record;
      Initialize (Buffer, Kernel, Lang);
   end Gtk_New;

   ---------------------
   -- Initialize_Hook --
   ---------------------

   procedure Initialize_Hook (Buffer : access Source_Buffer_Record'Class) is
      Iter : Gtk_Text_Iter;
   begin
      Reset_Completion_Data;

      Get_Start_Iter (Buffer, Iter);

      --  Initialize the data for timeout highlighting

      Buffer.Highlighter.First_Highlight_Mark :=
        Create_Mark (Buffer, "", Iter);
      Buffer.Highlighter.Last_Highlight_Mark  :=
        Create_Mark (Buffer, "", Iter, False);

      --  Initialize the line info

      Buffer.Editable_Lines := new Editable_Line_Array (1 .. 1);
      Buffer.Editable_Lines (1) := 1;

      --  ??? create line info (above)

      Buffer.Line_Data := new Line_Data_Array (1 .. 1);
      Buffer.Line_Data (1) := New_Line_Data;
      Buffer.Line_Data (1).Editable_Line := 1;
      Create_Side_Info (Buffer, 1);

      --  Compute the block information

      Register_Edit_Timeout (Buffer);

      --  Show the line number information, if needed
      Recalculate_Side_Column_Width (Buffer);
   end Initialize_Hook;

   -------------------
   -- On_Paste_Done --
   -------------------

   procedure On_Paste_Done (Buffer : access Source_Buffer_Record'Class) is
   begin
      --  Workaround a bug in gtk+ 2.18: when a paste was completed, the
      --  paste-done signal is emitted, and results in scrolling _all_ views
      --  to the cursor location (which breaks the handling of multiple views).
      --  The simplest workaround is simply to not emit that signal at all
      --  (GNAT Studio takes care of the scrolling for the active view anyway).
      --  The RH for this signal in gtk+ is:
      --      Add a "paste-done" signal and use it to properly scroll the
      --      view at the end of the pasted text in the case of an async paste

      Emit_Stop_By_Name (Buffer, "paste-done");

      On_Paste_Done (Get_Clipboard (Buffer.Kernel), Buffer);
   end On_Paste_Done;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Buffer : access Source_Buffer_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle;
      Lang   : Language.Language_Access := null)
   is
      use Pango.Enums.Underline_Properties;

      Command : Check_Modified_State;
      P_Hook  : Preferences_Hooks_Function_Access;

   begin
      Glib.Object.Initialize_Class_Record
        (Ancestor     => Gtkada.Text_Buffer.Get_Type,
         Signals      => Signals,
         Class_Record => Class_Record,
         Type_Name    => "GPSSourceBuffer",
         Parameters   => Signal_Parameters);
      Glib.Object.G_New (Buffer, Class_Record);
      Gtkada.Text_Buffer.Initialize (Buffer);

      Buffer.Lang   := Lang;
      Buffer.Kernel := Kernel;

      --  Create the Hidden_Text_Tag and save it into the source buffer tag
      --  table.
      Gtk_New (Buffer.Hidden_Text_Tag, "hidden-text");
      Set_Property
        (Buffer.Hidden_Text_Tag, Invisible_Property, True);

      --  Create the Hyper Mode Tag

      Gtk_New (Buffer.Hyper_Mode_Tag);
      Set_Property
        (Buffer.Hyper_Mode_Tag,
         Gtk.Text_Tag.Underline_Property,
         Pango_Underline_Single);
      Set_Property (Buffer.Hyper_Mode_Tag, Foreground_Rgba_Property,
                    Hyper_Links_Style.Get_Pref_Fg);

      if Hyper_Links_Style.Get_Pref_Bg /= Gdk.RGBA.White_RGBA then
         Set_Property (Buffer.Hyper_Mode_Tag, Background_Rgba_Property,
                       Hyper_Links_Style.Get_Pref_Bg);
      end if;

      --  Create the Non Editable Tag

      Gtk_New (Buffer.Non_Editable_Tag);
      Set_Property
        (Buffer.Non_Editable_Tag, Gtk.Text_Tag.Editable_Property, False);

      Buffer.Highlighter := new Source_Highlighter_Record
        (Source_Buffer (Buffer));

      --  Preference changed hook

      P_Hook :=
        new On_Pref_Changed'
          (Hook_Function with Buffer => Source_Buffer (Buffer));
      Preferences_Changed_Hook.Add (Obj => P_Hook, Watch => Buffer);
      P_Hook.Execute (Kernel, null);

      --  Project recomputed hook
      Project_View_Changed_Hook.Add
        (Obj   =>
            new On_Project_Changed'
           (Hook_Function with Buffer => Source_Buffer (Buffer)),
         Watch => Buffer);

      --  File hooks
      File_Deleted_Hook.Add
        (Obj   =>
            new On_File_Deleted'
           (Hook_Function with Buffer => Source_Buffer (Buffer)),
         Watch => Buffer);

      --  Renamed_Hook.Execute will change the buffer's filename:
      --  Add it with Last=>True so that other modules have a chance to react
      --  on the editor before it is renamed
      File_Renamed_Hook.Add
        (Obj   =>
            new On_File_Renamed'
           (Hook_Function with Buffer => Source_Buffer (Buffer)),
         Watch => Buffer,
         Last  => True);

      Semantic_Tree_Updated_Hook.Add
        (Obj   =>
            new On_Semantic_Tree_Updated'
           (Hook_Function with Buffer => Source_Buffer (Buffer)),
         Watch => Buffer);

      Location_Changed_Hook.Add_Debounce
        (Obj   =>
            new On_Loc_Changed'
           (Hook_Function with Buffer => Source_Buffer (Buffer)),
         Watch => Buffer);

      --  Save the insert mark for fast retrievals, since we will need to
      --  access it very often.

      Buffer.Insert_Mark := Get_Insert (Buffer);

      --  Initialize the queue for editor commands

      Buffer.Queue := New_Queue;

      --  Workaround a bug in gtk+ 2.18: when a paste was completed, the
      --  paste-done signal is emitted, and results in scrolling _all_ views
      --  to the cursor location (which breaks the handling of multiple views).
      --  The simplest workaround is simply to not emit that signal at all
      --  (GNAT Studio takes care of the scrolling for the active view anyway).
      --  The RH for this signal in gtk+ is:
      --      Add a "paste-done" signal and use it to properly scroll the
      --      view at the end of the pasted text in the case of an async paste

      Buffer_Callback.Connect
        (Buffer, "paste-done",
         Marsh => Buffer_Callback.To_Marshaller (On_Paste_Done'Access),
         After => False);

      --  And finally, connect ourselves to the interesting signals

      Weak_Ref (Buffer, Buffer_Destroy'Access);

      Buffer_Callback.Connect
        (Buffer, Signal_Changed, Changed_Handler'Access, After => True);
      Buffer_Callback.Connect
        (Buffer, Signal_Mark_Set,
         Cb => Mark_Set_Handler'Access, After => True);
      Buffer_Callback.Connect
        (Buffer, Signal_Insert_Text,
         Cb => Before_Insert_Text'Access);
      Buffer_Callback.Connect
        (Buffer, Signal_Insert_Text,
         Cb => After_Insert_Text'Access,
         After => True);
      Buffer_Callback.Connect
        (Buffer, Signal_Delete_Range,
         Cb => After_Delete_Range'Access,
         After => True);
      Buffer_Callback.Connect
        (Buffer, Signal_Delete_Range,
         Cb    => Before_Delete_Range'Access,
         After => False);

      Buffer.Editable_Line_Info_Columns :=
        new Line_Info_Display_Array_Access'(null);

      Initialize_Hook (Buffer);

      --  Create the queue change hook that will be called every
      --  time the state of the queue associated to the buffer changes.

      Create (Command, Source_Buffer (Buffer));

      Add_Queue_Change_Hook
        (Buffer.Queue, Command_Access (Command), "State_Check");

      Buffer.First_Removed_Line := 0;

      End_Action (Buffer);

      --  Create the default column for line information (block folding,
      --  compiler error messages, etc).
      Create_Line_Information_Column
        (Buffer, Default_Column, False, Empty_Line_Information);
      Buffer.Block_Highlighting_Column :=
        Buffer.Editable_Line_Info_Columns.all'Last;

      if Editors_Factory = null then
         Editors_Factory :=
           new Src_Editor_Module.Editors.Src_Editor_Buffer_Factory'
             (Src_Editor_Module.Editors.Create (Kernel));
      end if;

      Buffer.Editor_Buffer := new GPS.Editors.Editor_Buffer'Class'
          (Editors_Factory.Get (Buffer));

      --  Initialize every listener from factories
      for Factory of Listener_Factories loop
         Buffer.Listeners.Append
           (Factory.Create
              (Buffer.Editor_Buffer.all,
               Editor_Buffer_Factory (Editors_Factory.all),
               Core_Kernel (Kernel)));
      end loop;

      Buffer.Folding_Provider := Folding_Provider;
   end Initialize;

   -------------------
   -- Is_In_Comment --
   -------------------

   function Is_In_Comment
     (Buffer : Source_Buffer;
      Iter   : Gtk_Text_Iter) return Boolean
   is
      Pos     : Gtk_Text_Iter;
      Success : Boolean;
   begin
      Copy (Iter, Pos);

      --  If the position is past the end of buffer move backward one char
      --  otherwise the Has_Tag check won't work properly.

      if Is_End (Pos) and not Is_Start (Pos) then
         Backward_Char (Pos, Success);
      end if;

      return Buffer.Highlighter.Is_Comment_Tag (Pos);
   end Is_In_Comment;

   -------------------
   -- Is_In_Comment --
   -------------------

   function Is_In_Comment
     (Buffer : Source_Buffer;
      Line   : Editable_Line_Type;
      Column : Visible_Column_Type) return Boolean
   is
      Iter : Gtk_Text_Iter;
   begin
      Buffer.Get_Iter_At_Screen_Position
        (Iter   => Iter,
         Line   => Line,
         Column => Column);

      return Is_In_Comment (Buffer, Iter);
   end Is_In_Comment;

   ------------------
   -- Is_In_String --
   ------------------

   function Is_In_String
     (Buffer              : Source_Buffer;
      Iter                : Gtk_Text_Iter;
      Added_Character     : Glib.Gunichar := 0;
      Check_Interpolation : Boolean := False) return Boolean
   is
      Lang         : constant Language_Access := Buffer.Lang;
      Lang_Context : Language_Context_Access;
      C1, C2       : Character;
      Added_C      : Character;
      Pos          : Gtk_Text_Iter;
      Quoted       : Boolean := False;
      Result       : Boolean;
      Skip_First   : Boolean := False;
   begin
      if Lang = null then
         return False;
      end if;

      Lang_Context := Get_Language_Context (Lang);

      Copy (Iter, Pos);
      Backward_Char (Pos, Result);

      if not Result then
         --  Start of buffer
         return Quoted;
      end if;

      C2 := Get_Char (Pos);

      if Added_Character /= 0 then
         declare
            The_Char : String (1 .. 6);
            --  Unichar_To_UTF8 requires a buffer of size 6
            Last     : Natural;
         begin
            Unichar_To_UTF8 (Added_Character, The_Char, Last);
            Added_C := The_Char (Last);
            Skip_First := True;
         end;
      end if;

      if Check_Interpolation
        and then Lang.Is_Interpolation_Char
          (Ada.Characters.Conversions.To_Wide_Wide_Character (C2))
      then
         --  In case of interpolation character, consider that we are in
         --  a string if the next character is in string.
         Forward_Char (Pos, Result);
         if Result then
            return Is_In_String (Buffer, Pos) xor
              (Added_Character /= 0
               and then C2 /= Lang_Context.Quote_Character
               and then Added_C = Lang_Context.String_Delimiter);
         end if;
      end if;

      if Added_Character /= 0 then
         --  For the first loop iteration use the added character
         --  which is not in the buffer yet
         C2 := Added_C;
         Skip_First := True;
      end if;

      while C2 /= ASCII.LF loop
         if Skip_First then
            Skip_First := False;
         else
            Backward_Char (Pos, Result);
            exit when not Result;
         end if;

         C1 := C2;
         C2 := Get_Char (Pos);

         --  Take care of '"' case
         if C1 = Lang_Context.Constant_Character
           and then C2 = Lang_Context.String_Delimiter
         then
            Backward_Char (Pos, Result);

            if not Result then
               --  The string delimiter is the first character
               Quoted := not Quoted;
               exit;
            end if;

            C2 := Get_Char (Pos);

            --  Ignore string delimiters in Constant_Character quotes
            if C2 /= Lang_Context.Constant_Character
              and then C2 /= Lang_Context.Quote_Character
            then
               Quoted := not Quoted;
            end if;

         elsif C1 = Lang_Context.String_Delimiter
           and then not (C2 = Lang_Context.Quote_Character)
         then
            Quoted := not Quoted;
         end if;
      end loop;

      return Quoted;
   end Is_In_String;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Project_Changed;
      Kernel : not null access Kernel_Handle_Record'Class)
   is
      Buffer : constant Source_Buffer := Self.Buffer;
   begin
      --  The project has changed: if this buffer has a file and the language
      --  is unknown, it is possible that the new project knows which language
      --  this file is.

      if (Buffer.Lang = null or else Buffer.Lang = Unknown_Lang)
        and then Buffer.Filename /= No_File
      then
         Set_Language
           (Buffer, Get_Language_From_File
              (Get_Language_Handler (Kernel), Buffer.Filename));
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference)
   is
      pragma Unreferenced (Kernel, Pref);
      B                 : constant Source_Buffer := Self.Buffer;
      Prev              : Boolean;
      Prev_Highlighting : GPS.Kernel.Preferences.External_Highlighting;
      Prev_Folding      : Folding_Preferences_Values;

   begin
      --  Connect timeout, to handle automatic saving of buffer

      B.Highlighter.Update_Use_Highlighting_Hook;

      Prev := B.Block_Highlighting;
      B.Block_Highlighting := Block_Highlighting.Get_Pref;

      if Prev /= B.Block_Highlighting then
         Register_Edit_Timeout (B);
      end if;

      Prev_Highlighting := B.LSP_Highlighting;
      B.LSP_Highlighting := Use_External_Highlighting.Get_Pref;
      if Prev_Highlighting /= B.LSP_Highlighting then
         Register_Edit_Timeout (B);
      end if;

      Prev_Folding := B.Block_Folding;
      B.Block_Folding :=
        (Block_Folding.Get_Pref,
         Fold_With_Use_Blocks.Get_Pref,
         Fold_Comments.Get_Pref,
         Autofold_Comment_Blocks.Get_Pref,
         To_Unbounded_String (Fold_Comment_Reg1.Get_Pref),
         To_Unbounded_String (Fold_Comment_Reg2.Get_Pref),
         To_Unbounded_String (Fold_Comment_Reg3.Get_Pref));

      if Prev_Folding /= B.Block_Folding then
         Register_Edit_Timeout (B);
      end if;

      if not B.Block_Folding.Block_Folding
        and then Prev_Folding.Block_Folding
      then
         Unfold_All (B);
         Remove_Block_Folding_Commands (B);
      end if;

      Prev := B.Parse_Blocks;
      B.Parse_Blocks := B.Block_Folding.Block_Folding
        or else B.Block_Highlighting
        or else Display_Subprogram_Names.Get_Pref;

      if Prev /= B.Parse_Blocks then
         Buffer_Information_Changed (B);
      end if;

      if not Prev and then B.Parse_Blocks then
         Register_Edit_Timeout (B);
      end if;

      B.Auto_Syntax_Check := Automatic_Syntax_Check.Get_Pref;
      B.Adjust_Tab_Width;

      B.Highlighter.Highlight_Delimiters :=
        Highlight_Delimiters.Get_Pref;
   end Execute;

   ---------------------
   -- Load_Empty_File --
   ---------------------

   procedure Load_Empty_File (Buffer : access Source_Buffer_Record) is
   begin
      Buffer.Original_Text_Inserted := True;
      Set_Trailing_Lines_Policy (Buffer, No_File, False);
      Set_Trailing_Space_Policy (Buffer, No_File, False);
   end Load_Empty_File;

   -------------------------------
   -- Set_Trailing_Space_Policy --
   -------------------------------

   procedure Set_Trailing_Space_Policy
     (Buffer : access Source_Buffer_Record;
      File   : GNATCOLL.VFS.Virtual_File;
      Trailing_Spaces_Found : Boolean)
   is
      Found   : Boolean;
      Prop    : GPS.Properties.Boolean_Property;
      Default : constant Strip_Trailing_Blanks_Policy :=
        Strip_Blanks.Get_Pref;
   begin
      if File /= GNATCOLL.VFS.No_File then
         GPS.Properties.Get_Property
           (Prop, File, Strip_Blanks_Property_Name, Found);

         if Found then
            Buffer.Strip_Trailing_Blanks := Prop.Value;

            return;
         end if;
      end if;

      case Default is
         when Always =>
            Set_Strip_Trailing_Blanks (Buffer, True);
         when Never =>
            Set_Strip_Trailing_Blanks (Buffer, False);
         when Autodetect =>
            Set_Strip_Trailing_Blanks
              (Buffer, not Trailing_Spaces_Found);
      end case;
   end Set_Trailing_Space_Policy;

   -------------------------------
   -- Set_Trailing_Lines_Policy --
   -------------------------------

   procedure Set_Trailing_Lines_Policy
     (Buffer               : access Source_Buffer_Record;
      File                 : GNATCOLL.VFS.Virtual_File;
      Trailing_Lines_Found : Boolean)
   is
      Found   : Boolean;
      Prop    : GPS.Properties.Boolean_Property;
      Default : constant Strip_Trailing_Blanks_Policy :=
        Strip_Lines.Get_Pref;
   begin
      if File /= GNATCOLL.VFS.No_File then
         GPS.Properties.Get_Property
           (Prop, File, Strip_Lines_Property_Name, Found);

         if Found then
            Buffer.Strip_Trailing_Lines := Prop.Value;
            return;
         end if;
      end if;

      case Default is
         when Always =>
            Set_Strip_Trailing_Lines (Buffer, True);
         when Never =>
            Set_Strip_Trailing_Lines (Buffer, False);
         when Autodetect =>
            Set_Strip_Trailing_Lines
              (Buffer, not Trailing_Lines_Found);
      end case;
   end Set_Trailing_Lines_Policy;

   --------------------
   -- Autosaved_File --
   --------------------

   function Autosaved_File (File : Virtual_File) return Virtual_File is
   begin
      --  Implementation must be in sync with Is_Auto_Save below
      return Create_From_Dir (Dir (File), ".#" & Base_Name (File) & "#");
   exception
      when VFS_Invalid_File_Error =>
         Trace
           (Me,
            "Failed to create Autosaved File for " & Display_Full_Name (File));
         return No_File;
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

   ----------------------------
   -- Check_Auto_Saved_Files --
   ----------------------------

   procedure Check_Auto_Saved_Files (Kernel : GPS.Kernel.Kernel_Handle) is
      Saved_Files : File_Sets.Set;
      Dialog      : GPS.Dialogs.GPS_Dialog;
      Ignore      : Boolean;
   begin
      if Active (Testsuite_Handle) and then Active (Auto_Save_No_Reload) then
         return;
      end if;

      for File of Kernel.Opened_Files loop
         declare
            Saved : constant Virtual_File := Autosaved_File (File);
         begin
            if Saved.Is_Regular_File and then Compare_SHA1 (File, Saved) then
               Saved_Files.Include (File);
            end if;
         end;
      end loop;

      if Saved_Files.Is_Empty then
         return;
      end if;

      Reload_Files_Dialog
        (Kernel       => Kernel,
         To_Update    => Saved_Files,
         Title        => "Autosaved files",
         Description  => "Select the autosaved files which will be loaded",
         Extra_Widget => null,
         Force        =>
           Active (Testsuite_Handle) and then Active (Auto_Save_Reload),
         Monitored    => False,
         Dialog       => Dialog);

      if Dialog /= null then
         Dialog.Destroy;
      end if;

      for File of Saved_Files loop
         declare
            Buffer : constant access Source_Buffer_Record :=
              Get (Kernel, File);
         begin
            if Buffer /= null then
               --  Replace File by its autosaved counterpart
               Insert_Text
                 (Buffer          => Buffer,
                  From_File       => Autosaved_File (File),
                  Lang_Autodetect => True,
                  Is_Auto_Save    => True,
                  File_Is_New     => False,
                  Success         => Ignore);
            end if;
         end;
      end loop;
   end Check_Auto_Saved_Files;

   -----------------
   -- Insert_Text --
   -----------------

   procedure Insert_Text
     (Buffer          : access Source_Buffer_Record;
      From_File       : Virtual_File;
      Lang_Autodetect : Boolean;
      Is_Auto_Save    : Boolean;
      File_Is_New     : Boolean;
      Success         : out Boolean)
   is

      procedure Reset_Buffer (Buffer : access Source_Buffer_Record);
      --  Reset all data associated with Buffer

      ------------------
      -- Reset_Buffer --
      ------------------

      procedure Reset_Buffer (Buffer : access Source_Buffer_Record) is
      begin
         --  Clear the buffer

         --  The presence of folded lines stops propagation of delete_range:
         --  unfold all lines before clearing.
         Unfold_All (Buffer);
         Clear (Buffer);

         Buffer.Original_Text_Inserted := False;

         if Buffer.Editable_Lines /= null then
            Unchecked_Free (Buffer.Editable_Lines);
         end if;

         for J in Buffer.Line_Data'Range loop
            for K in Highlight_Location loop
               Unchecked_Free (Buffer.Line_Data (J).Highlighting (K).Enabled);
            end loop;

            if Buffer.Line_Data (J).Side_Info_Data /= null then
               for I in Buffer.Line_Data (J).Side_Info_Data'Range loop
                  Free (Buffer, Buffer.Line_Data (J).Side_Info_Data (I),
                        Free_Messages => False);
               end loop;
               Unchecked_Free (Buffer.Line_Data (J).Side_Info_Data);
            end if;
         end loop;

         Unchecked_Free (Buffer.Line_Data);

         Initialize_Hook (Buffer);

         Buffer.First_Removed_Line := 0;
         Buffer.Last_Removed_Line  := 0;
         Buffer.Last_Editable_Line := 1;

         --  Unregister the blocks timeout

         if Buffer.Blocks_Timeout /= Glib.Main.No_Source_Id then
            Buffer.Blocks_Timeout_Registered := False;
            Glib.Main.Remove (Buffer.Blocks_Timeout);
            Buffer.Blocks_Timeout := Glib.Main.No_Source_Id;
         end if;

         --  Unregister the cursor timeout

         Buffer.Blank_Lines := 0;
         Buffer.Hidden_Lines := 0;
         Buffer.Block_Highlighting_Column := -1;
      end Reset_Buffer;

      Text  : VSS.Strings.Virtual_String;
      Props : File_Props;

   begin
      Trace (Me, "Loading " & From_File.Display_Full_Name
             & " as autosave ? " & Is_Auto_Save'Img
             & " is new ? " & File_Is_New'Img);
      Success := True;
      Read_File_With_Charset (From_File, Text, Props);

      if Text.Is_Null then
         --  The file does not exist on disk, this is a new file that has
         --  never been saved.
         Trace (Me, "Load_File: Couldn't read contents of "
                & From_File.Display_Full_Name);
         Success := False;
         return;
      end if;

      if Props.Bidirectional_Unicode then
         Buffer.Kernel.Insert
           ("Warning: the file contains bidirectional Unicode text that may be"
            & " interpreted or compiled differently than what it looks in the"
            & " editor.",
           Mode => GPS.Kernel.Error);
      end if;

      if Props.NUL_Found then
         Buffer.Kernel.Insert
           ((-"Warning: NUL characters stripped from ")
            & From_File.Display_Full_Name, Mode => GPS.Kernel.Error);
      end if;

      if Props.Invalid_UTF8 then
         Buffer.Kernel.Insert
           ((-"Warning: invalid characters stripped from ")
            & From_File.Display_Full_Name, Mode => GPS.Kernel.Error);
      end if;

      declare
         G : Group_Block := New_Group (Buffer.Queue);
      begin
         if not Is_Auto_Save then
            if not File_Is_New then
               Emit_By_Name (Get_Object (Buffer),
                             Signal_Closed & ASCII.NUL);
               Reset_Buffer (Buffer);
            else
               Buffer.Start_Inserting;  --  no undo should be available
            end if;

            if Lang_Autodetect then
               Set_Language
                 (Buffer, Get_Language_From_File
                    (Get_Language_Handler (Buffer.Kernel), From_File));
            end if;

            Set_Charset (Buffer, Get_File_Charset (From_File));
            Set_Trailing_Space_Policy
              (Buffer, From_File, Props.Trailing_Spaces_Found);
            Set_Trailing_Lines_Policy
              (Buffer, From_File, Props.Trailing_Lines_Found);

            if Props.CR_Found then
               Buffer.Line_Terminator := CR_LF;
            else
               Buffer.Line_Terminator := LF;
            end if;

         else
            Reset_Buffer (Buffer);
         end if;

         --  Insert the new text

         declare
            use type Ada.Strings.Unbounded.Aux.Big_String_Access;

            U : constant Unbounded_String :=
              VSS.Strings.Conversions.To_Unbounded_UTF_8_String (Text);
            S : Ada.Strings.Unbounded.Aux.Big_String_Access;
            L : Natural;

         begin
            Ada.Strings.Unbounded.Aux.Get_String (U, S, L);

            if S /= null then
               Insert_At_Cursor (Buffer, S (1 .. L));
            end if;

         exception
            when E : others =>
               Trace (Me, E);
         end;

         if not Is_Auto_Save and then File_Is_New then
            Buffer.End_Inserting;  --  reenable undo
         end if;
      end;

      if Is_Auto_Save then
         Buffer.Saved_Position := 0;
         Buffer.Set_Last_Status (Modified);
      elsif File_Is_New then
         Buffer.Saved_Position := 0;
      else
         Buffer.Saved_Position := Get_Position (Buffer.Queue);
         Buffer.Set_Last_Status (Saved);
      end if;
   end Insert_Text;

   ------------------
   -- Compare_SHA1 --
   ------------------

   function Compare_SHA1
     (File : Virtual_File; Autosave : Virtual_File) return Boolean
   is
      Str1, Str2 : GNAT.Strings.String_Access;
      Success    : Boolean;
   begin
      Str1 := File.Read_File;
      Str2 := Autosave.Read_File;

      --  Return immediately if one of the files to be compared
      --  can't be read.
      if Str1 = null or else Str2 = null then
         return True;
      end if;

      if GNAT.SHA1.Digest (Str1.all) = GNAT.SHA1.Digest (Str2.all) then
         --  same sha1, don't bother the user
         Trace (Me, "Auto-save file has same sha1");
         GNAT.Strings.Free (Str1);
         GNAT.Strings.Free (Str2);
         Autosave.Delete (Success);
         return False;
      end if;

      GNAT.Strings.Free (Str1);
      GNAT.Strings.Free (Str2);
      return True;
   end Compare_SHA1;

   ---------------
   -- Load_File --
   ---------------

   procedure Load_File
     (Buffer          : access Source_Buffer_Record;
      Filename        : GNATCOLL.VFS.Virtual_File;
      Lang_Autodetect : Boolean := True;
      Success         : out Boolean;
      Is_Load_Desktop : Boolean := False)
   is
      File_Is_New : constant Boolean := not Buffer.Original_Text_Inserted;

      procedure Check_Auto_Saved_File;
      --  Chech whether there exists an auto-saved file, and whether the user
      --  would like to load it.
      --  Loads the auto-save file if requested

      ---------------------------
      -- Check_Auto_Saved_File --
      ---------------------------

      procedure Check_Auto_Saved_File is
         Autosave   : constant Virtual_File := Autosaved_File (Filename);
         Buttons    : Message_Dialog_Buttons;
      begin
         if Autosave.Is_Regular_File then

            if not Compare_SHA1 (Filename, Autosave) then
               return;
            end if;

            Trace (Me, "Found auto-save file " & Autosave.Display_Full_Name);

            if Active (Testsuite_Handle) then
               --  In the testsuite, we test for two specific handles to
               --  control the behavior of this dialog:
               if Active (Auto_Save_Reload) then
                  Buttons := Button_Yes;
               elsif Active (Auto_Save_No_Reload) then
                  Buttons := Button_No;
               else
                  Buttons := Button_No;
                  --  test is not expecting the dialog, we should report it as
                  --  an error, but no need to block with a dialog
                  Trace
                    (Testsuite_Handle,
                     "Would have displayed the dialog about auto-saved file");
               end if;
            else
               Buttons := GPS_Message_Dialog
                 (Msg            =>
                    -"Found an auto-saved file named "
                  & Autosave.Display_Base_Name & ASCII.LF
                  & (-"This usually means that your previous GNAT Studio"
                  & " session ") & ASCII.LF
                  & (-"terminated unexpectedly with unsaved changes.")
                  & ASCII.LF & ASCII.LF
                  & (-"Do you want to recover the contents of ")
                  & Filename.Display_Base_Name & ASCII.LF
                  & (-"from this auto-saved file "
                    & " (this operation can be undone) ?"),
                  Dialog_Type    => Warning,
                  Buttons        => Button_Yes or Button_No,
                  Default_Button => Button_Yes,
                  Title          => -"Found auto-saved file",
                  Justification  => Justify_Left,
                  Parent         => Get_Current_Window (Buffer.Kernel));
            end if;

            if Buttons = Button_Yes then
               Insert_Text
                 (Buffer          => Buffer,
                  From_File       => Autosave,
                  Lang_Autodetect => Lang_Autodetect,
                  Is_Auto_Save    => True,
                  File_Is_New     => File_Is_New,
                  Success         => Success);
            end if;

            --  Do not delete the auto-save file: it will be removed when the
            --  user saves the file (or another auto-save takes place). In the
            --  meantime, should GNAT Studio crash, we still want the user to
            --  be able to restore it next time.
         end if;
      end Check_Auto_Saved_File;

   begin
      Insert_Text
        (Buffer          => Buffer,
         From_File       => Filename,
         Lang_Autodetect => Lang_Autodetect,
         Is_Auto_Save    => False,
         File_Is_New     => File_Is_New,
         Success         => Success);

      if not Success then
         Buffer.Save_Complete := False;
         return;
      end if;

      --  If there is an auto-save file that the user wants to reload, we do
      --  this in a separate step, so that this operation can be undone.
      --  We don't want to spawn multiple dialogs while loading a project's
      --  desktop: a dialog with all the autosaved files will be opened later.

      if not Is_Load_Desktop and then File_Is_New then
         Check_Auto_Saved_File;
      end if;

      Buffer.Highlighter.On_Load_File;

      Buffer.Modified_Auto := False;

      --  Do not empty the undo/redo queue, so that reloading a file can be
      --  undone.
      --      Empty_Queue (Buffer.Queue);
      --      Buffer.Current_Command := null;

      Buffer.Set_Filename (Filename);
      Buffer.Writable := Filename.Is_Writable;

      --  Emit the "file_reloaded" hook so that clients know that this file
      --  has just been reloaded from disk - for instance to reset file
      --  markers that have just been sent to location 1,1 during the
      --  buffer reset above.
      if not File_Is_New then
         File_Reloaded_Hook.Run (Buffer.Kernel, Filename);
      end if;

   exception
      when E : others =>
         Trace (Me, E);
         Success := False;
   end Load_File;

   ------------------------------
   -- Conversion_Error_Message --
   ------------------------------

   function Conversion_Error_Message
     (Charset : String) return VSS.Strings.Virtual_String is
   begin
      return
        VSS.Strings.Conversions.To_Virtual_String
          (-"Error converting from UTF8 to " & Charset);
   end Conversion_Error_Message;

   ---------------------------
   -- Internal_Save_To_File --
   ---------------------------

   procedure Internal_Save_To_File
     (Buffer   : Source_Buffer;
      Filename : GNATCOLL.VFS.Virtual_File;
      Internal : Boolean;
      Success  : out Boolean;
      Force    : Boolean := False)
   is
      FD          : Writable_File;
      Terminator  : Line_Terminator_Style := Buffer.Line_Terminator;
      --  Whether the file mode has been forced to writable
      U_Buffer    : Unbounded_String;
      Has_Errors  : Boolean := False;
      Buttons     : Message_Dialog_Buttons := Button_None;

      procedure New_Line;
      --  Append a new line on U_Buffer

      --------------
      -- New_Line --
      --------------

      procedure New_Line is
      begin
         case Terminator is
            when CR_LF        => Append (U_Buffer, (ASCII.CR & ASCII.LF));
            when CR           => Append (U_Buffer, ASCII.CR);
            when Unknown | LF => Append (U_Buffer, ASCII.LF);
         end case;
      end New_Line;

   begin
      Trace (Me, "Internal_Save_To_File " & Filename.Display_Full_Name);
      Success := True;

      if not Needs_To_Be_Saved (Buffer)
         and then Filename = Buffer.Get_Filename
         and then Filename.Is_Regular_File
      then
         Trace (Me, "File not modified, nothing to do");
         return;
      end if;

      --  When the user requested a save, interrupt the current action, so
      --  that the "check_modified" command, which is called when the undo/redo
      --  queue changes, has a chance to refresh the modified state.
      End_Action (Buffer);

      if not Internal then
         --  Run the "before_file_saved" hook
         Before_File_Saved_Hook.Run (Buffer.Kernel, Filename);
      end if;

      declare
         Terminator_Pref : constant Line_Terminators :=
                             Line_Terminator.Get_Pref;
         Bytes_Written   : Integer;
         pragma Unreferenced (Bytes_Written);

         Index           : Natural;
         Error           : GError_Access := new GError'(null);

         procedure Unchecked_Free is new Ada.Unchecked_Deallocation
           (GError, GError_Access);

         Last_Line : Editable_Line_Type := 0;
      begin
         case Terminator_Pref is
            when Unix =>
               Terminator := LF;
            when Windows =>
               Terminator := CR_LF;
            when Unchanged =>
               null;
         end case;

         if Buffer.Strip_Trailing_Lines then
            for Line in reverse
              Buffer.Editable_Lines'First .. Buffer.Last_Editable_Line
            loop
               declare
                  Str  : constant Src_String :=
                    Get_String_At_Line (Buffer, Line);
               begin
                  if Str.Contents /= null
                    and then not Is_Blank_Line (Str.Contents (1 .. Str.Length))
                  then
                     Last_Line := Line;
                     exit;
                  end if;
               end;
            end loop;
         else
            Last_Line := Buffer.Last_Editable_Line;
         end if;

         for Line in Buffer.Editable_Lines'First .. Last_Line loop
            declare
               Str : Src_String := Get_String_At_Line (Buffer, Line);
            begin
               if Str.Length = 0 then
                  if Line /= Buffer.Last_Editable_Line then
                     New_Line;
                  end if;

               else
                  Index := Str.Length;

                  if Buffer.Strip_Trailing_Blanks then
                     --  Safe to use 1 here as Str.Contents'First as this is
                     --  the type definition
                     for J in reverse 1 .. Str.Length loop
                        --  No need for special utf8 handling, as we are just
                        --  looking for spaces.

                        exit when Str.Contents (J) /= ' '
                          and then Str.Contents (J) /= ASCII.HT;

                        --  J points to a blank character. Let's set Index to
                        --  the previous character in the string.
                        Index := J - 1;
                     end loop;
                  end if;

                  declare
                     Contents : constant String := Glib.Convert.Convert
                       (Str.Contents (Str.Contents'First .. Index),
                        Buffer.Charset.all, "UTF-8", Error);

                  begin
                     if Error.all = null then
                        Append (U_Buffer, Contents);
                     else
                        --  An error has occurred on this line
                        Has_Errors := True;

                        Create_Simple_Message
                          (Get_Messages_Container (Buffer.Kernel),
                           Conversion_Error_Message (Buffer.Charset.all),
                           Buffer.Filename,
                           Positive (Line),
                           1,
                           VSS.Strings.Conversions.To_Virtual_String
                             (Get_Message (Error.all)),
                           High,
                           Src_Editor_Message_Flags);

                        Error_Free (Error.all);
                        Error.all := null;
                     end if;
                  end;

                  New_Line;
               end if;

               Free (Str);
            end;
         end loop;

         Unchecked_Free (Error);
      end;

      --  If we observed UTF-8 conversion errors, warn the user

      if Has_Errors and then not Internal then
         Buttons := GPS_Message_Dialog
           (Msg            =>
              -("This buffer contains UTF-8 characters which"
              & " could not be translated to ") & Buffer.Charset.all
            & "." & ASCII.LF & ASCII.LF &
            (-("Some data may be missing in the saved file: check the"
               & " Locations View."))
            & ASCII.LF & ASCII.LF &
            (-("You may change the character set of this file through"
               & " the ""Properties..."" contextual menu.")),
            Dialog_Type    => Warning,
            Buttons        => Button_OK,
            Default_Button => Button_OK,
            Title          => -"Warning: Conversion Incomplete",
            Justification  => Justify_Left,
            Parent         => Get_Current_Window (Buffer.Kernel));
      end if;

      --  The file could not be opened, check whether it is read-only

      if Is_Regular_File (Filename) and then not Is_Writable (Filename) then
         if not Force then
            Buttons := GPS_Message_Dialog
              (Msg            => -"The file "
               & Display_Base_Name (Filename) & ASCII.LF
               & (-"is read-only. Do you want to overwrite it ?"),
               Dialog_Type    => Confirmation,
               Buttons        => Button_Yes or Button_No,
               Default_Button => Button_No,
               Title          => -"File is read-only",
               Justification  => Justify_Left,
               Parent         => Get_Current_Window (Buffer.Kernel));
         end if;

         if Force or else Buttons = Button_Yes then
            Make_File_Writable (Buffer.Kernel, Filename, True);
            Mark_Buffer_Writable (Buffer, True);
         end if;
      end if;

      declare
         use type Ada.Strings.Unbounded.Aux.Big_String_Access;

         S : Ada.Strings.Unbounded.Aux.Big_String_Access;
         L : Natural;

      begin
         FD := Write_File (Filename);

         if FD = Invalid_File then
            Buffer.Kernel.Insert
              (-"Could not open file for writing: "
               & Display_Full_Name (Filename),
               Mode => GPS.Kernel.Error);
            Buffer.Kernel.Insert
              (To_String (Error_String (FD)),
               Mode => GPS.Kernel.Error);
            Success := False;
            return;
         end if;

         Ada.Strings.Unbounded.Aux.Get_String (U_Buffer, S, L);

         if S /= null then
            Write (FD, S (1 .. L));
         end if;

         U_Buffer := Null_Unbounded_String;

         Close (FD);

      exception
         when E : Ada.Text_IO.Use_Error =>
            Trace (Me, E);

            if not Internal then
               Buttons := GPS_Message_Dialog
                 (Msg            => -"The file "
                  & Display_Base_Name (Filename) & ASCII.LF
                  & " could not be saved. This might be a transient disk"
                  & " problem.",
                  Dialog_Type    => Warning,
                  Buttons        => Button_OK,
                  Default_Button => Button_OK,
                  Title          => -"File could not be saved",
                  Justification  => Justify_Left,
                  Parent         => Get_Current_Window (Buffer.Kernel));
            end if;
            return;
      end;

      --  If the file could be saved, emit the corresponding signal.
      --  Emit the signal only if we are really saving to the buffer's file,
      --  not to another filename (which happens for example when doing
      --  automatic saves.

      if not Internal then
         Buffer.Save_Complete := not Has_Errors;

         if Buffer.Filename /= Filename then
            --  If we "save as" the buffer, we emit a closed for the previous
            --  name, unless the file was an unnamed buffer
            if Buffer.Filename /= GNATCOLL.VFS.No_File then
               Emit_File_Closed (Buffer, Buffer.File_Identifier);
            end if;

            declare
               Old : constant Virtual_File := Buffer.Filename;
            begin
               Buffer.Filename := Filename;

               --  If we renamed the file, emit the corresponding hook
               --  and notify listeners
               if Old /= Buffer.Filename  then
                  Emit_File_Renamed (Buffer, Old, Buffer.Filename);
               end if;
            end;
         end if;

         File_Saved_Hook.Run (Buffer.Kernel, Filename);

         Buffer.Saved_Position := Get_Position (Buffer.Queue);
         Buffer.Set_Last_Status (Saved);
      end if;

   exception
      when E : others =>
         Trace (Me, E);

         --  To avoid consuming up all File Descriptors, we catch all
         --  exceptions here, and close the current file descriptor.

         if FD /= Invalid_File then
            Close (FD);
         end if;
   end Internal_Save_To_File;

   ------------------
   -- Save_To_File --
   ------------------

   procedure Save_To_File
     (Buffer   : access Source_Buffer_Record;
      Filename : GNATCOLL.VFS.Virtual_File;
      Success  : out Boolean;
      Internal : Boolean := False;
      Force    : Boolean := False)
   is
      --  When a file is created through "Goto spec<->body", the file won't
      --  exist on the disk yet, but Buffer.Filename will already be set. So
      --  we need both tests below
      Name_Changed      : constant Boolean :=
        Buffer.Filename /= Filename
        or else not Buffer.Filename.Is_Regular_File;

      Original_Filename : constant Virtual_File := Buffer.Filename;
   begin
      if not Internal then
         Remove_Completion;
      end if;

      Internal_Save_To_File
        (Source_Buffer (Buffer), Filename, Internal, Success,
         Force => Force);

      if Success and then not Internal then
         if Name_Changed then
            --  Force an update of the persistent properties if need be
            Set_Charset  (Buffer, Get_Charset (Buffer));
            Set_Language (Buffer, Get_Language (Buffer));

            --  ??? The following is expensive, it would be nice to have a
            --  simpler way to report a possible change in the list of sources
            --  of a project.
            Recompute_View (Buffer.Kernel);

            --  Change the language when we have reparsed the project, so that
            --  the naming scheme is correctly taken into account
            Set_Language
              (Buffer,
               Get_Language_From_File
                 (Get_Language_Handler (Buffer.Kernel), Buffer.Filename));

            --  Emit the "filename_changed" signal
            Buffer.Filename_Changed;
         end if;

         if Original_Filename /= GNATCOLL.VFS.No_File then
            Buffer.Delete_Autosaved_File (Original_Filename);
         end if;

         if Filename /= Original_Filename then
            --  We have just "saved as" with a new file name: tell GNAT Studio
            --  that this file is now open
            Emit_File_Edited (Buffer, Filename);
         end if;

         Buffer.Modified_Auto := False;
      end if;

   exception
      when E : others =>
         Trace (Me, E);
   end Save_To_File;

   ---------------------------
   -- Delete_Autosaved_File --
   ---------------------------

   procedure Delete_Autosaved_File
     (Buffer : access Source_Buffer_Record;
      File   : GNATCOLL.VFS.Virtual_File)
   is
      Autosaved : constant Virtual_File := Autosaved_File (File);
      Dummy     : Boolean;
   begin
      if Active (Me) then
         Trace (Me, "Delete autosave file " &
                  Autosaved.Display_Full_Name);
      end if;

      if Is_Regular_File (Autosaved) then
         if not Is_Writable (Autosaved) then
            Make_File_Writable (Buffer.Kernel, Autosaved, True);
         end if;

         Delete (Autosaved, Dummy);
      end if;
   exception
      when E : others =>
         Me.Trace (E, "When deleting autosave file " &
                     Autosaved.Display_Full_Name);
   end Delete_Autosaved_File;

   ------------------
   -- Set_Language --
   ------------------

   procedure Set_Language
     (Buffer : access Source_Buffer_Record;
      Lang   : Language.Language_Access)
   is
      Buffer_Start_Iter : Gtk_Text_Iter;
      Buffer_End_Iter   : Gtk_Text_Iter;
   begin
      if Buffer.Lang /= Lang then
         Buffer.Lang := Lang;
         Get_Bounds (Buffer, Buffer_Start_Iter, Buffer_End_Iter);

         Buffer.Highlighter.On_Set_Language
           (Buffer_Start_Iter, Buffer_End_Iter);

         Register_Edit_Timeout (Buffer);

         Buffer_Information_Changed (Buffer);
      end if;

      if Buffer.Filename /= GNATCOLL.VFS.No_File then
         if Lang /= Get_Language_From_File
           (Get_Language_Handler (Buffer.Kernel), Buffer.Filename)
         then
            if Lang = Language.Unknown.Unknown_Lang
              or else Lang = Get_Language_From_File
              (Get_Language_Handler (Buffer.Kernel), Buffer.Filename,
               From_Project_Only => True)
            then
               --  If this is the same as the project => Do not save, so that
               --  changing in the project correctly impacts this file
               Set_Language_From_File
                 (Buffer.Kernel, Buffer.Filename, "");
            else
               --  Note for the future which language should be used
               Set_Language_From_File
                 (Buffer.Kernel, Buffer.Filename, Get_Name (Lang));
            end if;
         end if;
      end if;
      Buffer.Adjust_Tab_Width;
   end Set_Language;

   -----------------
   -- Set_Charset --
   -----------------

   procedure Set_Charset
     (Buffer : access Source_Buffer_Record; Charset : String)
   is
      Success : Boolean;
      Ignore  : Message_Dialog_Buttons;
      pragma Unreferenced (Ignore);

   begin
      if Buffer.Filename /= GNATCOLL.VFS.No_File then
         if Charset /= Get_File_Charset (Buffer.Filename) then
            if Charset = Get_File_Charset (GNATCOLL.VFS.No_File) then
               --  Since we are using the default charset, do not save in the
               --  properties
               Set_File_Charset (Buffer.Kernel, Buffer.Filename, "");
            else
               --  Else note for the future which charset should be used
               Set_File_Charset (Buffer.Kernel, Buffer.Filename, Charset);
            end if;
         end if;
      end if;

      if Buffer.Charset /= null
        and then Buffer.Charset.all /= Charset
      then
         --  The charset is being changed: remove from the Locations View
         --  the category listing the conversion errors from that charset for
         --  this file.

         Get_Messages_Container (Buffer.Kernel).Remove_File
           (Conversion_Error_Message (Buffer.Charset.all),
            Buffer.Filename,
            Src_Editor_Message_Flags);

         GNAT.Strings.Free (Buffer.Charset);
         Buffer.Charset := new String'(Charset);

         if Get_Status (Buffer) = Modified
           or else Get_Status (Buffer) = Unsaved
         then
            Ignore := GPS_Message_Dialog
              (Msg => -("The character set has been modified."
               & ASCII.LF
               & "Since the file is currently modified, the new"
               & ASCII.LF
               & "character set will only apply when the file is"
               & ASCII.LF
               & " saved, the file will not be reloaded automatically"),
               Dialog_Type => Warning,
               Buttons     => Button_OK,
               Title       => -"Warning: charset modified",
               Parent      => Get_Main_Window (Buffer.Kernel));

         else
            --  If the user has tried to save Buffer and got errors, do not
            --  reload the file from the disk, as the user would then lose his
            --  modifications that could not be saved.
            if Buffer.Save_Complete then
               Load_File (Buffer, Buffer.Filename, Success => Success);
            end if;
         end if;
      else
         GNAT.Strings.Free (Buffer.Charset);
         Buffer.Charset := new String'(Charset);
      end if;
   end Set_Charset;

   -----------------
   -- Get_Charset --
   -----------------

   function Get_Charset (Buffer : access Source_Buffer_Record) return String is
   begin
      if Buffer.Charset = null then
         return Get_File_Charset (GNATCOLL.VFS.No_File);
      else
         return Buffer.Charset.all;
      end if;
   end Get_Charset;

   ------------------
   -- Get_Language --
   ------------------

   function Get_Language
     (Buffer : access Source_Buffer_Record) return Language.Language_Access is
   begin
      return Buffer.Lang;
   end Get_Language;

   -----------------------------
   -- Should_Extend_Selection --
   -----------------------------

   function Should_Extend_Selection
     (Buffer           : not null access Source_Buffer_Record;
      Extend_Selection : Boolean) return Boolean is
   begin
      return Extend_Selection
        or else Buffer.Extend_Existing_Selection;
   end Should_Extend_Selection;

   -----------------------------------
   -- Set_Extend_Existing_Selection --
   -----------------------------------

   procedure Set_Extend_Existing_Selection
     (Buffer : not null access Source_Buffer_Record;
      Extend : Boolean) is
   begin
      Buffer.Extend_Existing_Selection := Extend;
   end Set_Extend_Existing_Selection;

   -------------------------------
   -- Extend_Existing_Selection --
   -------------------------------

   function Extend_Existing_Selection
     (Buffer : not null access Source_Buffer_Record) return Boolean is
   begin
      return Buffer.Extend_Existing_Selection;
   end Extend_Existing_Selection;

   -------------------------
   -- Set_Cursor_Position --
   -------------------------

   procedure Set_Cursor_Position
     (Buffer    : access Source_Buffer_Record;
      Line      : Gint;
      Column    : Gint;
      Internal  : Boolean;
      Extend_Selection : Boolean := False)
   is
      Iter : Gtk_Text_Iter;
   begin
      if not Is_Valid_Position (Buffer, Line, Column) then
         Trace (Me, "invalid position for Set_Cursor_Position "
                & Get_Filename (Buffer).Display_Full_Name
                & Line'Img & Column'Img);
         return;
      end if;

      if not Internal then
         --  Any explicit (ie, forced programatically, as opposed to through
         --  user typing) cursor movement should end the current action, and
         --  destroy the smart completion window.
         End_Action (Buffer);
         Remove_Completion;
      end if;

      --  At this point, we know that the (Line, Column) position is
      --  valid, so we can safely get the iterator at this position.

      Get_Iter_At_Line_Offset (Buffer, Iter, Line, Column);

      if Buffer.Should_Extend_Selection (Extend_Selection) then
         Move_Mark (Buffer, Buffer.Insert_Mark, Iter);
      else
         Place_Cursor (Buffer, Iter);
      end if;
   end Set_Cursor_Position;

   procedure Set_Cursor_Position
     (Buffer    : access Source_Buffer_Record;
      Line      : Editable_Line_Type;
      Column    : Character_Offset_Type;
      Internal  : Boolean;
      Extend_Selection : Boolean := False)
   is
      Buffer_Line : Buffer_Line_Type;
   begin
      if Buffer.Do_Not_Move_Cursor then
         return;
      end if;

      --  If the line is in a non-visible line, make the line visible

      Unfold_Line (Buffer, Line);
      Buffer_Line := Get_Buffer_Line (Buffer, Line);

      Set_Cursor_Position
        (Buffer, Gint (Buffer_Line - 1), Gint (Column - 1),
         Internal => Internal,
         Extend_Selection => Extend_Selection);
   end Set_Cursor_Position;

   ---------------------------------
   -- Get_Iter_At_Screen_Position --
   ---------------------------------

   procedure Get_Iter_At_Screen_Position
     (Buffer : access Source_Buffer_Record;
      Iter   : out Gtk_Text_Iter;
      Line   : Gint;
      Column : Visible_Column_Type)
   is
      Tab_Len    : constant Positive := Buffer.Tab_Width;
      The_Column : constant Gint := Gint (Column - 1);
      Result     : Boolean := True;
      Current    : Gint := 0;
   begin
      if Is_Valid_Position (Buffer, Line, 0) then
         Get_Iter_At_Line_Offset (Buffer, Iter, Line, 0);
      else
         Trace (Me, "Invalid position for Set_Screen_Position "
                & Get_Filename (Buffer).Display_Full_Name & Line'Img);
         Get_End_Iter (Buffer, Iter);
      end if;

      --  We have to test Result, in case Iter was pointing after the end of
      --  the buffer.

      while Result and then Current < The_Column loop
         if Get_Char (Iter) = ASCII.HT then
            Current := Current + Gint (Tab_Len) - (Current mod Gint (Tab_Len));
         else
            Current := Current + 1;
         end if;

         exit when Ends_Line (Iter);

         Forward_Char (Iter, Result);
      end loop;
   end Get_Iter_At_Screen_Position;

   ---------------------------------
   -- Get_Iter_At_Screen_Position --
   ---------------------------------

   procedure Get_Iter_At_Screen_Position
     (Buffer : access Source_Buffer_Record;
      Iter   : out Gtk_Text_Iter;
      Line   : Editable_Line_Type;
      Column : Visible_Column_Type)
   is
      Buffer_Line : Buffer_Line_Type;
   begin
      Buffer_Line := Get_Buffer_Line (Buffer, Line);

      if Buffer_Line /= 0 then
         Get_Iter_At_Screen_Position
           (Buffer, Iter,
            Gint (Buffer_Line - 1),
            Column);
      else
         Get_Iter_At_Line_Offset (Buffer, Iter, 0, 0);
      end if;
   end Get_Iter_At_Screen_Position;

   procedure Get_Iter_At_Screen_Position
     (Buffer : access Source_Buffer_Record;
      Iter   : out Gtk.Text_Iter.Gtk_Text_Iter;
      Line   : Editable_Line_Type;
      Column : Character_Offset_Type)
   is
      Buffer_Line : Buffer_Line_Type;
   begin
      Buffer_Line := Get_Buffer_Line (Buffer, Line);

      if Buffer_Line /= 0 then
         Get_Iter_At_Line_Offset
           (Buffer, Iter,
            Gint (Buffer_Line - 1),
            Gint (Column - 1));
      else
         Get_Iter_At_Line_Offset (Buffer, Iter, 0, 0);
      end if;
   end Get_Iter_At_Screen_Position;

   -------------------------
   -- Get_Screen_Position --
   -------------------------

   procedure Get_Screen_Position
     (Buffer : access Source_Buffer_Record;
      Iter   : Gtk_Text_Iter;
      Line   : out Gint;
      Column : out Gint)
   is
      Start   : Gtk_Text_Iter;
      Result  : Boolean := True;
      Tab_Len : constant Positive := Buffer.Tab_Width;

   begin
      Line   := Get_Line (Iter);
      Column := 0;
      Get_Iter_At_Line_Offset (Buffer, Start, Line, 0);

      --  We have to test Result, in case Iter was pointing after the end of
      --  the buffer.

      while Result and then not Equal (Start, Iter) loop
         if Get_Char (Start) = ASCII.HT then
            Column := Column + Gint (Tab_Len) - (Column mod Gint (Tab_Len));
         else
            Column := Column + 1;
         end if;

         Forward_Char (Start, Result);
      end loop;
   end Get_Screen_Position;

   procedure Get_Screen_Position
     (Buffer : access Source_Buffer_Record;
      Line   : out Gint;
      Column : out Gint)
   is
      Iter : Gtk_Text_Iter;
   begin
      Get_Iter_At_Mark (Buffer, Iter, Buffer.Insert_Mark);
      Get_Screen_Position (Buffer, Iter, Line, Column);
   end Get_Screen_Position;

   -------------------------
   -- Get_Cursor_Position --
   -------------------------

   procedure Get_Cursor_Position
     (Buffer : access Source_Buffer_Record;
      Line   : out Gint;
      Column : out Gint)
   is
      Iter : Gtk_Text_Iter;
   begin
      Get_Iter_At_Mark (Buffer, Iter, Buffer.Insert_Mark);
      Line   := Get_Line (Iter);
      Column := Get_Line_Offset (Iter);
   end Get_Cursor_Position;

   -----------------------
   -- Get_Iter_Position --
   -----------------------

   procedure Get_Iter_Position
     (Buffer : Source_Buffer;
      Iter   : Gtk_Text_Iter;
      Line   : out Editable_Line_Type;
      Column : out Character_Offset_Type) is
   begin
      Line := Get_Editable_Line
        (Buffer, Buffer_Line_Type (Get_Line (Iter) + 1));
      Column := Character_Offset_Type (Get_Line_Offset (Iter) + 1);
   end Get_Iter_Position;

   -----------------------
   -- Get_Iter_Position --
   -----------------------

   procedure Get_Iter_Position
     (Buffer : Source_Buffer;
      Iter   : Gtk.Text_Iter.Gtk_Text_Iter;
      Line   : out Editable_Line_Type;
      Column : out Visible_Column_Type)
   is
      Col : Character_Offset_Type;
   begin
      Get_Iter_Position (Buffer, Iter, Line, Col);
      Column := Expand_Tabs (Buffer, Line, Col);
   end Get_Iter_Position;

   -----------------------
   -- Get_Iter_Position --
   -----------------------

   procedure Get_Iter_Position
     (Buffer : Source_Buffer;
      Iter   : Gtk.Text_Iter.Gtk_Text_Iter;
      Loc    : out Loc_T)
   is
   begin
      Get_Iter_Position (Buffer, Iter, Loc.Line, Loc.Col);
   end Get_Iter_Position;

   -----------------------
   -- Get_Mark_Position --
   -----------------------

   procedure Get_Mark_Position
     (Buffer : Source_Buffer;
      Mark   : Gtk.Text_Mark.Gtk_Text_Mark;
      Loc    : out Loc_T)
   is
      I : Gtk_Text_Iter;
   begin
      Buffer.Get_Iter_At_Mark (I, Mark);
      Get_Iter_Position (Buffer, I, Loc);
   end Get_Mark_Position;

   -------------------------
   -- Get_Cursor_Position --
   -------------------------

   procedure Get_Cursor_Position
     (Buffer : access Source_Buffer_Record;
      Iter   : out Gtk.Text_Iter.Gtk_Text_Iter) is
   begin
      Get_Iter_At_Mark (Buffer, Iter, Buffer.Insert_Mark);
   end Get_Cursor_Position;

   -------------------------
   -- Get_Cursor_Position --
   -------------------------

   procedure Get_Cursor_Position
     (Buffer : access Source_Buffer_Record;
      Line   : out Editable_Line_Type;
      Column : out Character_Offset_Type)
   is
      Iter : Gtk_Text_Iter;
   begin
      Get_Cursor_Position (Buffer, Iter);
      Get_Iter_Position (Source_Buffer (Buffer), Iter, Line, Column);
   end Get_Cursor_Position;

   procedure Get_Cursor_Position
     (Buffer : access Source_Buffer_Record;
      Line   : out Editable_Line_Type;
      Column : out Visible_Column_Type)
   is
      Col : Character_Offset_Type;
   begin
      Get_Cursor_Position (Buffer, Line, Col);
      Column := Expand_Tabs (Buffer, Line, Col);
   end Get_Cursor_Position;

   ---------------
   -- Ends_Word --
   ---------------

   function Ends_Word
     (Buffer : access Source_Buffer_Record;
      Iter : Gtk_Text_Iter)
      return Boolean
   is
      Next : Gtk_Text_Iter;
      Res  : Boolean;
   begin
      if not Inside_Word (Buffer, Iter) then
         return False;
      end if;

      Copy (Iter, Next);
      Forward_Char (Next, Res);

      if Res then
         return not Inside_Word (Buffer, Next);
      else
         return True;
      end if;
   end Ends_Word;

   -----------------
   -- Starts_Word --
   -----------------

   function Starts_Word
     (Buffer : access Source_Buffer_Record;
      Iter   : Gtk.Text_Iter.Gtk_Text_Iter)
      return Boolean
   is
      Prev : Gtk_Text_Iter;
      Res  : Boolean;
   begin
      if not Inside_Word (Buffer, Iter) then
         return False;
      end if;

      Copy (Iter, Prev);
      Backward_Char (Prev, Res);

      if Res then
         return not Inside_Word (Buffer, Prev);
      else
         return True;
      end if;
   end Starts_Word;

   -----------------
   -- Inside_Word --
   -----------------

   function Inside_Word
     (Buffer : access Source_Buffer_Record;
      Iter   : Gtk.Text_Iter.Gtk_Text_Iter)
      return Boolean is
   begin
      if Buffer.Lang /= null then
         return Buffer.Lang.Is_Word_Char
           (Wide_Wide_Character'Val
              (Gunichar'(Get_Char (Iter))));
      else
         return False;
      end if;
   end Inside_Word;

   -------------------------
   -- Select_Current_Word --
   -------------------------

   procedure Select_Current_Word (Buffer : access Source_Buffer_Record) is
      Success                    : Boolean;
      Start_Iter, End_Iter, Iter : Gtk_Text_Iter;

   begin
      Get_Selection_Bounds (Buffer, Start_Iter, End_Iter, Success);

      if Success then
         --  Check the current selection: if more than a single word is
         --  selected then return.

         Copy (Start_Iter, Iter);
         Forward_Char (Iter, Success);

         loop
            exit when Equal (Iter, End_Iter);

            if not Inside_Word (Iter) then
               return;
            end if;

            Forward_Char (Iter, Success);
            exit when not Success;
         end loop;

         Backward_Char (End_Iter, Success);

      elsif not Inside_Word (Start_Iter)
        and then Get_Char (Start_Iter) /= '_'
      then
         return;
      end if;

      Success := True;
      while not Ends_Word (Buffer, End_Iter) loop
         Forward_Char (End_Iter, Success);
         exit when not Success;
      end loop;

      if Success then
         Forward_Char (End_Iter, Success);
      end if;

      while not Starts_Word (Buffer, Start_Iter) loop
         Backward_Char (Start_Iter, Success);
         exit when not Success;
      end loop;

      Buffer.Select_Range (End_Iter, Start_Iter);
   end Select_Current_Word;

   --------------
   -- Get_Text --
   --------------

   function Get_Text
     (Buffer               : Source_Buffer;
      Start_Line           : Gint;
      Start_Column         : Gint;
      End_Line             : Gint;
      End_Column           : Gint;
      Include_Hidden_Chars : Boolean := True;
      Include_Last         : Boolean := False)
      return Unbounded_String
   is
   begin
      return Get_Text
        (Buffer               => Buffer,
         Start_Line           => Editable_Line_Type (Start_Line + 1),
         Start_Column         => Character_Offset_Type (Start_Column + 1),
         End_Line             => Editable_Line_Type (End_Line + 1),
         End_Column           => Character_Offset_Type (End_Column + 1),
         Include_Hidden_Chars => Include_Hidden_Chars,
         Include_Last         => Include_Last);
   end Get_Text;

   --------------------------
   -- Get_Selection_Bounds --
   --------------------------

   procedure Get_Selection_Bounds
     (Buffer       : access Source_Buffer_Record;
      Start_Line   : out Gint;
      Start_Column : out Gint;
      End_Line     : out Gint;
      End_Column   : out Gint;
      Found        : out Boolean)
   is
      Start_Iter : Gtk_Text_Iter;
      End_Iter   : Gtk_Text_Iter;
   begin
      Get_Selection_Bounds (Buffer, Start_Iter, End_Iter, Found);

      if Found then
         Start_Line   := Get_Line (Start_Iter);
         Start_Column := Get_Line_Offset (Start_Iter);
         End_Line     := Get_Line (End_Iter);
         End_Column   := Get_Line_Offset (End_Iter);
      else
         Start_Line   := 0;
         Start_Column := 0;
         End_Line     := 0;
         End_Column   := 0;
      end if;
   end Get_Selection_Bounds;

   procedure Get_Selection_Bounds
     (Buffer       : access Source_Buffer_Record;
      Start_Line   : out Editable_Line_Type;
      Start_Column : out Character_Offset_Type;
      End_Line     : out Editable_Line_Type;
      End_Column   : out Character_Offset_Type;
      Found        : out Boolean)
   is
      SL, SC, EL, EC : Gint;
   begin
      Get_Selection_Bounds (Buffer, SL, SC, EL, EC, Found);

      Start_Line := Get_Editable_Line (Buffer, Buffer_Line_Type (SL + 1));
      End_Line := Get_Editable_Line (Buffer, Buffer_Line_Type (EL + 1));
      Start_Column := Character_Offset_Type (SC + 1);
      End_Column   := Character_Offset_Type (EC + 1);
   end Get_Selection_Bounds;

   -------------------
   -- Get_Selection --
   -------------------

   function Get_Selection
     (Buffer : access Source_Buffer_Record) return String
   is
      Start_Iter : Gtk_Text_Iter;
      End_Iter   : Gtk_Text_Iter;
      Found      : Boolean;

   begin
      Get_Selection_Bounds (Buffer, Start_Iter, End_Iter, Found);

      if Found then
         return To_String
           (Get_Text
              (Source_Buffer (Buffer),
               Get_Line (Start_Iter),
               Get_Line_Offset (Start_Iter),
               Get_Line (End_Iter),
               Get_Line_Offset (End_Iter),
               Include_Last => True));
      else
         return "";
      end if;
   end Get_Selection;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Buffer      : access Source_Buffer_Record;
      Line        : Gint;
      Column      : Gint;
      Text        : String;
      Enable_Undo : Boolean := True)
   is
      Iter : Gtk_Text_Iter;
   begin
      pragma Assert (Is_Valid_Position (Buffer, Line, Column));

      if not Buffer.Writable then
         End_Action (Buffer);
         return;
      end if;

      if not Enable_Undo then
         Buffer.Start_Inserting;
      end if;

      Get_Iter_At_Line_Offset (Buffer, Iter, Line, Column);
      Insert (Buffer, Iter, Text);

      if not Enable_Undo then
         Buffer.End_Inserting;
      end if;

      Register_Edit_Timeout (Buffer);
   end Insert;

   procedure Insert
     (Buffer      : access Source_Buffer_Record;
      Line        : Editable_Line_Type;
      Column      : Character_Offset_Type;
      Text        : String;
      Enable_Undo : Boolean := True)
   is
      Buffer_Line : Buffer_Line_Type;
   begin
      if not Buffer.Writable then
         End_Action (Buffer);
         return;
      end if;

      Unfold_Line (Buffer, Line);
      Buffer_Line := Get_Buffer_Line (Buffer, Line);

      if Buffer_Line = 0 then
         Trace (Me, "invalid buffer line");
         return;
      end if;

      Insert (Buffer, Gint (Buffer_Line - 1), Gint (Column - 1), Text,
              Enable_Undo);
   end Insert;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Buffer      : access Source_Buffer_Record;
      Line        : Gint;
      Column      : Gint;
      Length      : Gint;
      Enable_Undo : Boolean := True)
   is
      Iter                     : Gtk_Text_Iter;
      End_Iter                 : Gtk_Text_Iter;
      Result                   : Boolean;
   begin
      pragma Assert (Is_Valid_Position (Buffer, Line, Column));

      if not Buffer.Inserting then
         End_Action (Buffer);
      end if;

      if not Enable_Undo then
         Buffer.Start_Inserting;
      end if;

      Get_Iter_At_Line_Offset (Buffer, Iter, Line, Column);
      Copy (Iter, End_Iter);

      --  If the file contains blank lines, we need to move step by step here
      --  and ignore blank lines.

      if Buffer.Blank_Lines = 0 then
         Forward_Chars (End_Iter, Length, Result);

      else
         declare
            Remaining : Gint := Length;
            Success   : Boolean;
            Buff_Line : Gint;
         begin
            Buff_Line := Get_Line (End_Iter);

            Advance_Char :
            loop
               exit Advance_Char when Remaining = 0;

               Forward_Char (End_Iter, Success);
               exit Advance_Char when not Success;

               if Get_Line (End_Iter) /= Buff_Line then
                  Buff_Line := Get_Line (End_Iter);

                  --  We have moved the End_Iter to another line. We need to
                  --  check whether this line is a blank line, and in this case
                  --  we move to the next line until we have found a non-blank
                  --  line.

                  while Get_Editable_Line
                      (Buffer, Buffer_Line_Type (Buff_Line + 1)) = 0
                  loop
                     Forward_Line (End_Iter, Success);
                     exit Advance_Char when not Success;
                  end loop;
               end if;

               Remaining := Remaining - 1;
            end loop Advance_Char;
         end;
      end if;

      Delete_Interactive (Buffer, Iter, End_Iter, True, Result);

      if not Enable_Undo then
         Buffer.End_Inserting;
      end if;

      Register_Edit_Timeout (Buffer);
   end Delete;

   procedure Delete
     (Buffer      : access Source_Buffer_Record;
      Line        : Editable_Line_Type;
      Column      : Character_Offset_Type;
      Length      : Natural;
      Enable_Undo : Boolean := True)
   is
      Buffer_Line : Buffer_Line_Type;

   begin
      if not Buffer.Writable then
         End_Action (Buffer);
         return;
      end if;

      Unfold_Line (Buffer, Line);
      Buffer_Line := Get_Buffer_Line (Buffer, Line);

      if Buffer_Line = 0 then
         Trace (Me, "invalid buffer line");
         return;
      end if;

      Delete (Buffer, Gint (Buffer_Line - 1), Gint (Column - 1), Gint (Length),
              Enable_Undo);
   end Delete;

   -------------------------
   -- Delete_Tab_Backward --
   -------------------------

   procedure Delete_Tab_Backward (Buffer : access Source_Buffer_Record)
   is
      Cursor_Mark : Gtk_Text_Mark;
      Iter, To    : Gtk_Text_Iter;
      Result      : Boolean;
   begin
      for Cursor of Get_Cursors (Source_Buffer (Buffer)) loop
         --  For all cursors

         Cursor_Mark := Get_Mark (Cursor);
         Set_Manual_Sync (Cursor);
         Get_Iter_At_Mark (Buffer, To, Cursor_Mark);

         if Get_Line_Offset (To) /= 0 then
            --  Not at the beginning of the line

            Copy (Source => To, Dest => Iter);
            Backward_Char (Iter, Result);
            for Pos in 1 .. Buffer.Tab_Width - 1 loop
               --  Move backward until we reach Tab_Width or find not HT/space
               exit when not Result
                 or else Get_Line_Offset (Iter) = 0
                 or else Get_Char (Iter) /= ' ';
               Backward_Char (Iter, Result);
            end loop;

            if Get_Char (Iter) /= Character'Pos (ASCII.HT)
              and then Get_Char (Iter) /= ' '
            then
               --  Move forward when cursor is not on the space or <tab>
               Forward_Char (Iter, Result);
            end if;
            Delete (Buffer, Iter, To);
         end if;
      end loop;
   end Delete_Tab_Backward;

   ------------------------
   -- Replace_Slice_Real --
   ------------------------

   procedure Replace_Slice_Real
     (Buffer       : access Source_Buffer_Record;
      Start_Line   : Gint;
      Start_Column : Gint;
      End_Line     : Gint;
      End_Column   : Gint;
      Text         : String;
      Enable_Undo  : Boolean := True)
   is
      Start_Iter               : Gtk_Text_Iter;
      End_Iter                 : Gtk_Text_Iter;

   begin
      Assert (Me, Is_Valid_Position (Buffer, Start_Line, Start_Column),
              "Invalid start position " & Start_Line'Img & Start_Column'Img);
      Assert (Me, Is_Valid_Position (Buffer, End_Line, End_Column),
              "Invalid end position " & End_Line'Img & End_Column'Img);

      if not Buffer.Inserting then
         End_Action (Buffer);
      end if;

      if not Enable_Undo then
         Buffer.Start_Inserting;
      end if;

      Get_Iter_At_Line_Offset (Buffer, Start_Iter, Start_Line, Start_Column);
      Get_Iter_At_Line_Offset (Buffer, End_Iter, End_Line, End_Column);

      --  Currently, Gtk_Text_Buffer does not export a service to replace
      --  some text, so we delete the slice first, then insert the text...
      Delete (Buffer, Start_Iter, End_Iter);

      Get_Iter_At_Line_Offset (Buffer, Start_Iter, Start_Line, Start_Column);

      Insert (Buffer, Start_Iter, Text);

      if not Enable_Undo then
         Buffer.End_Inserting;
      end if;

      Register_Edit_Timeout (Buffer);
   end Replace_Slice_Real;

   -------------------
   -- Replace_Slice --
   -------------------

   procedure Replace_Slice
     (Buffer       : access Source_Buffer_Record;
      Start_Line   : Editable_Line_Type;
      Start_Column : Character_Offset_Type;
      End_Line     : Editable_Line_Type;
      End_Column   : Character_Offset_Type;
      Text         : String;
      Enable_Undo  : Boolean := True)
   is
      Buffer_Start_Line, Buffer_End_Line : Buffer_Line_Type;
   begin
      if not Buffer.Writable then
         End_Action (Buffer);
         return;
      end if;

      for J in Start_Line .. End_Line loop
         Unfold_Line (Buffer, J);
      end loop;

      Buffer_Start_Line := Get_Buffer_Line (Buffer, Start_Line);
      Buffer_End_Line := Get_Buffer_Line (Buffer, End_Line);

      if Buffer_Start_Line = 0 or else Buffer_End_Line = 0 then
         Trace (Me, "invalid buffer line");
         return;
      end if;

      Replace_Slice_Real
        (Buffer,
         Gint (Buffer_Start_Line - 1),
         Gint (Start_Column - 1),
         Gint (Buffer_End_Line - 1),
         Gint (End_Column - 1),
         Text,
         Enable_Undo);
   end Replace_Slice;

   ----------------
   -- Select_All --
   ----------------

   procedure Select_All (Buffer : access Source_Buffer_Record) is
      Start_Iter : Gtk_Text_Iter;
      End_Iter   : Gtk_Text_Iter;
   begin
      End_Action (Buffer);
      Get_Start_Iter (Buffer, Start_Iter);
      Get_End_Iter (Buffer, End_Iter);
      Select_Range (Buffer, Ins => End_Iter, Bound => Start_Iter);
   end Select_All;

   -------------------
   -- Select_Region --
   -------------------

   procedure Select_Region
     (Buffer      : access Source_Buffer_Record;
      Cursor_Iter : Gtk.Text_Iter.Gtk_Text_Iter;
      Bound_Iter  : Gtk.Text_Iter.Gtk_Text_Iter)
   is
   begin
      End_Action (Buffer);
      Select_Range (Buffer, Ins => Cursor_Iter, Bound => Bound_Iter);
   end Select_Region;

   -------------------
   -- Select_Region --
   -------------------

   procedure Select_Region
     (Buffer       : access Source_Buffer_Record;
      Start_Line   : Gint;
      Start_Column : Gint;
      End_Line     : Gint;
      End_Column   : Gint)
   is
      Start_Iter : Gtk_Text_Iter;
      End_Iter   : Gtk_Text_Iter;
   begin
      if Start_Line = End_Line and then Start_Column = End_Column then
         End_Action (Buffer);
         Get_Iter_At_Mark (Buffer, Start_Iter, Get_Insert (Buffer));
         Place_Cursor (Buffer, Start_Iter);

      else
         if not Is_Valid_Position (Buffer, Start_Line, Start_Column) then
            Trace (Me, "invalid start position in Select_Region, aborting:"
                   & Start_Line'Img & Start_Column'Img);
            return;

         elsif not Is_Valid_Position (Buffer, End_Line, End_Column) then
            Trace (Me, "invalid end position in Select_Region, aborting:"
                   & End_Line'Img & End_Column'Img);
            return;
         end if;

         End_Action (Buffer);
         Get_Iter_At_Line_Offset
           (Buffer, Start_Iter, Start_Line, Start_Column);
         Get_Iter_At_Line_Offset (Buffer, End_Iter, End_Line, End_Column);
         Select_Range (Buffer, Ins => End_Iter, Bound => Start_Iter);
      end if;
   end Select_Region;

   -------------------
   -- Select_Region --
   -------------------

   procedure Select_Region
     (Buffer       : access Source_Buffer_Record;
      Start_Line   : Editable_Line_Type;
      Start_Column : Character_Offset_Type;
      End_Line     : Editable_Line_Type;
      End_Column   : Character_Offset_Type) is
   begin
      for J in Start_Line .. End_Line loop
         Unfold_Line (Buffer, J);
      end loop;

      Select_Region
        (Buffer,
         Gint (Get_Buffer_Line (Buffer, Start_Line) - 1),
         Gint (Start_Column - 1),
         Gint (Get_Buffer_Line (Buffer, End_Line) - 1),
         Gint (End_Column - 1));
   end Select_Region;

   -------------------
   -- Select_Region --
   -------------------

   procedure Select_Region
     (Buffer       : access Source_Buffer_Record;
      Start_Line   : Editable_Line_Type;
      Start_Column : Visible_Column_Type;
      End_Line     : Editable_Line_Type;
      End_Column   : Visible_Column_Type)
   is
      Start_Col, End_Col : Character_Offset_Type;
   begin
      Start_Col := Collapse_Tabs (Buffer, Start_Line, Start_Column);
      End_Col := Collapse_Tabs (Buffer, End_Line, End_Column);

      Select_Region (Buffer, Start_Line, Start_Col, End_Line, End_Col);
   end Select_Region;

   --------------------
   -- Lines_Add_Hook --
   --------------------

   procedure Lines_Add_Hook
     (Buffer : access Source_Buffer_Record'Class;
      Start  : Buffer_Line_Type;
      Number : Buffer_Line_Type) is
   begin
      --  Resynchronize the arrays that need to be synchronized with line
      --  numbers.
      Add_Lines (Buffer, Start, Number);

      --  Parse the block information
      Register_Edit_Timeout (Buffer);
   end Lines_Add_Hook;

   ------------------------------
   -- Lines_Remove_Hook_Before --
   ------------------------------

   procedure Lines_Remove_Hook_Before
     (Buffer     : access Source_Buffer_Record'Class;
      Start_Line : Buffer_Line_Type;
      Count      : Buffer_Line_Type) is
   begin
      Buffer_Before_Delete_Lines_Hook.Run
        (Buffer.Kernel, Buffer.Filename,
         Integer (Start_Line + 1), Integer (Count));

      --  Resynchronize the arrays that need to be synchronized with line
      --  numbers.
      Remove_Lines (Buffer, Start_Line, Count);

      --  It is necessary to set the fields Buffer.First_Removed_Line and
      --  Buffer.Last_Removed_Line because the parameters for the
      --  "delete_range" signal are invalid when we connect after the actual
      --  deletion has been done.

      Buffer.First_Removed_Line := Start_Line + 1;
      Buffer.Last_Removed_Line  := Start_Line + Count;
   end Lines_Remove_Hook_Before;

   -----------------------------
   -- Lines_Remove_Hook_After --
   -----------------------------

   procedure Lines_Remove_Hook_After
     (Buffer     : access Source_Buffer_Record'Class;
      Start_Line : Buffer_Line_Type;
      End_Line   : Buffer_Line_Type)
   is
      pragma Unreferenced (Start_Line, End_Line);
   begin
      --  Parse the block information
      Register_Edit_Timeout (Buffer);
   end Lines_Remove_Hook_After;

   ---------------------
   -- End_Action_Hook --
   ---------------------

   procedure End_Action_Hook (Buffer : access Source_Buffer_Record'Class) is
      pragma Unreferenced (Buffer);
   begin
      Reset_Completion_Data;
   end End_Action_Hook;

   ----------------
   -- End_Action --
   ----------------

   procedure End_Action (Buffer : access Source_Buffer_Record'Class) is
      Command : constant Editor_Command :=
                  Editor_Command (Buffer.Current_Command);
   begin
      End_Action_Hook (Buffer);

      if not Is_Null_Command (Command) then
         Buffer.Current_Command := null;
      end if;
   end End_Action;

   -------------------------
   -- External_End_Action --
   -------------------------

   procedure External_End_Action (Buffer : access Source_Buffer_Record) is
      Command : constant Editor_Command :=
                  Editor_Command (Buffer.Current_Command);

   begin
      if not Is_Null_Command (Command) then
         End_Action (Buffer);
      end if;
   end External_End_Action;

   ----------
   -- Redo --
   ----------

   procedure Redo (Buffer : access Source_Buffer_Record) is
      Command : constant Editor_Command :=
                  Editor_Command (Buffer.Current_Command);
   begin
      if not Is_Null_Command (Command) then
         End_Action (Buffer);
      end if;

      Redo (Buffer.Queue);

      --  Undo and Redo clear the current group: start a new group
      Change_Group (Buffer.Queue);
   end Redo;

   ----------
   -- Undo --
   ----------

   procedure Undo (Buffer : access Source_Buffer_Record) is
      Command : constant Editor_Command :=
                  Editor_Command (Buffer.Current_Command);
   begin
      if not Is_Null_Command (Command) then
         End_Action (Buffer);
      end if;

      Undo (Buffer.Queue);

      --  Undo and Redo clear the current group: start a new group
      Change_Group (Buffer.Queue);
   end Undo;

   --------------
   -- Can_Undo --
   --------------

   function Can_Undo (Buffer : access Source_Buffer_Record) return Boolean is
   begin
      return Can_Undo (Buffer.Queue);
   end Can_Undo;

   -------------
   -- Enqueue --
   -------------

   procedure Enqueue
     (Buffer      : access Source_Buffer_Record;
      Command     : Command_Access;
      User_Action : Action_Type)
   is
      Drag_N_Drop_Deletion : constant Boolean :=
        Buffer.In_Text_Drag_N_Drop and then User_Action = Delete_Text;
   begin
      Buffer.Start_Inserting;

      if Buffer.Saved_Position > Get_Position (Buffer.Queue) then
         Buffer.Saved_Position := -1;
      end if;

      Buffer.Current_Command := null;

      --  Decide whether we should group this action with the previous actions.
      --  Don't change the group if we are enqueuing a drag n drop deletion: we
      --  want it to be in the same group as the drag n drop insertion.
      if User_Action /= Buffer.Last_User_Action
        and then not Drag_N_Drop_Deletion
      then
         Change_Group (Buffer.Queue);
      end if;

      if User_Action in No_Action .. Delete_Line then
         Buffer.Last_User_Action := User_Action;
      end if;

      Src_Editor_Module.Set_Global_Command (null);
      Enqueue (Buffer.Queue, Command);
      Buffer.End_Inserting;

      --  Reset the drag n drop flag when we know that it's finished
      if Drag_N_Drop_Deletion then
         Buffer.In_Text_Drag_N_Drop := False;
      end if;
   end Enqueue;

   -----------------------------
   -- Notify_Text_Drag_N_Drop --
   -----------------------------

   procedure Notify_Text_Drag_N_Drop
     (Buffer : not null access Source_Buffer_Record) is
   begin
      Buffer.In_Text_Drag_N_Drop := True;
   end Notify_Text_Drag_N_Drop;

   ----------------
   -- Get_Kernel --
   ----------------

   function Get_Kernel
     (Buffer : access Source_Buffer_Record) return GPS.Kernel.Kernel_Handle is
   begin
      return Buffer.Kernel;
   end Get_Kernel;

   ------------------
   -- Get_Filename --
   ------------------

   function Get_Filename
     (Buffer : access Source_Buffer_Record) return GNATCOLL.VFS.Virtual_File is
   begin
      return Buffer.Filename;
   end Get_Filename;

   ------------------
   -- Set_Filename --
   ------------------

   procedure Set_Filename
     (Buffer : access Source_Buffer_Record; Name : Virtual_File)
   is
      Old : constant Virtual_File := Buffer.Filename;
   begin
      Buffer.Filename := Name;
      if not Is_Regular_File (Name) then
         Buffer.Saved_Position := -1;
         Buffer.Save_Complete := False;
      end if;
      if Old /= Name then
         --  Listeners are created before the editor is given
         --  a filename: here is their chance to get the new
         --  filename.
         for Listener of Buffer.Listeners loop
            Listener.File_Renamed (Old, Name);
         end loop;
      end if;
   end Set_Filename;

   ---------------------
   -- Set_Initial_Dir --
   ---------------------

   procedure Set_Initial_Dir
     (Buffer : access Source_Buffer_Record;
      Name   : GNATCOLL.VFS.Virtual_File) is
   begin
      Buffer.Initial_Dir := Name;
   end Set_Initial_Dir;

   ---------------------
   -- Get_Initial_Dir --
   ---------------------

   function Get_Initial_Dir
     (Buffer : access Source_Buffer_Record) return GNATCOLL.VFS.Virtual_File is
   begin
      return Buffer.Initial_Dir;
   end Get_Initial_Dir;

   -------------------------
   -- Get_File_Identifier --
   -------------------------

   function Get_File_Identifier
     (Buffer : access Source_Buffer_Record) return GNATCOLL.VFS.Virtual_File is
   begin
      return Buffer.File_Identifier;
   end Get_File_Identifier;

   -------------------------
   -- Set_File_Identifier --
   -------------------------

   procedure Set_File_Identifier
     (Buffer : access Source_Buffer_Record; Name : Virtual_File) is
   begin
      Buffer.File_Identifier := Name;
   end Set_File_Identifier;

   -------------------------
   -- Source_Lines_Folded --
   -------------------------

   procedure Source_Lines_Folded
     (Buffer     : access Source_Buffer_Record;
      Start_Line : Editable_Line_Type;
      End_Line   : Editable_Line_Type)
   is
      Context : constant Selection_Context := Source_Lines_Context
        (Buffer, No_Project, Start_Line, End_Line);
   begin
      Source_Lines_Folded_Hook.Run
        (Buffer.Kernel, Context,
         Natural (Start_Line),
         Natural (End_Line));
   end Source_Lines_Folded;

   ---------------------------
   -- Source_Lines_Unfolded --
   ---------------------------

   procedure Source_Lines_Unfolded
     (Buffer     : access Source_Buffer_Record;
      Start_Line : Editable_Line_Type;
      End_Line   : Editable_Line_Type)
   is
      Context : constant Selection_Context := Source_Lines_Context
        (Buffer, No_Project, Start_Line, End_Line);
   begin
      Source_Lines_Unfolded_Hook.Run
        (Buffer.Kernel, Context,
         Natural (Start_Line),
         Natural (End_Line));
   end Source_Lines_Unfolded;

   --------------------------
   -- Source_Lines_Context --
   --------------------------

   function Source_Lines_Context
     (Buffer     : access Source_Buffer_Record;
      Project    : GNATCOLL.Projects.Project_Type;
      Start_Line : Editable_Line_Type;
      End_Line   : Editable_Line_Type) return Selection_Context
   is
      Context     : Selection_Context :=
        New_Context (Buffer.Kernel, Src_Editor_Module_Id);
      The_Project : Project_Type;
   begin
      if Project = No_Project then
         declare
            Child : MDI_Child;
         begin
            Child := Find_Editor (Kernel  => Buffer.Kernel,
                                  File    => Buffer.Filename,
                                  Project => No_Project);

            if Child /= null then
               The_Project := Src_Editor_Module.Get_Project (Child);
            end if;
         end;
      end if;

      if Buffer.Filename /= GNATCOLL.VFS.No_File then
         Set_File_Information
           (Context,
            Files           => (1 => Buffer.Filename),
            Project         => The_Project,
            Publish_Project => False);

      elsif Buffer.File_Identifier /= GNATCOLL.VFS.No_File then
         Set_File_Information
           (Context,
            Files           => (1 => Buffer.File_Identifier),
            Project         => The_Project,
            Publish_Project => False);
      end if;

      Set_Area_Information
        (Context, "",
         Integer (Start_Line),
         Integer (End_Line));

      return Context;
   end Source_Lines_Context;

   -------------------
   -- Register_View --
   -------------------

   procedure Register_View
     (Buffer : access Source_Buffer_Record; Add : Boolean) is
   begin
      if Add then
         Buffer.Number_Of_Views := Buffer.Number_Of_Views + 1;
      else
         Buffer.Number_Of_Views := Buffer.Number_Of_Views - 1;
      end if;

      if Buffer.Number_Of_Views = 0 then
         Emit_By_Name (Get_Object (Buffer), Signal_Closed & ASCII.NUL);

         if Buffer.Filename /= GNATCOLL.VFS.No_File then
            Emit_File_Closed (Buffer, Buffer.Filename);

         elsif Buffer.File_Identifier /= GNATCOLL.VFS.No_File then
            Emit_File_Closed (Buffer, Buffer.File_Identifier);
         end if;
      end if;
   end Register_View;

   ----------------------------------
   -- Avoid_Cursor_Move_On_Changes --
   ----------------------------------

   function Avoid_Cursor_Move_On_Changes
     (Buffer : access Source_Buffer_Record) return Boolean
   is
   begin
      return Buffer.No_Cursor_Move_On_Changes;
   end Avoid_Cursor_Move_On_Changes;

   --------------------------------------
   -- Set_Avoid_Cursor_Move_On_Changes --
   --------------------------------------

   procedure Set_Avoid_Cursor_Move_On_Changes
     (Buffer : access Source_Buffer_Record; Value : Boolean)
   is
   begin
      Buffer.No_Cursor_Move_On_Changes := Value;
   end Set_Avoid_Cursor_Move_On_Changes;

   ------------------
   -- Add_Controls --
   ------------------

   procedure Add_Controls (Buffer : access Source_Buffer_Record) is
   begin
      Change_Undo_Redo_Queue (Buffer.Queue);
   end Add_Controls;

   ---------------------
   -- Remove_Controls --
   ---------------------

   procedure Remove_Controls (Buffer : access Source_Buffer_Record) is
      pragma Unreferenced (Buffer);
   begin
      Change_Undo_Redo_Queue (Null_Command_Queue);
   end Remove_Controls;

   ----------------------------
   -- Get_Total_Column_Width --
   ----------------------------

   function Get_Total_Column_Width
     (Buffer : access Source_Buffer_Record) return Natural is
   begin
      return Buffer.Total_Column_Width;
   end Get_Total_Column_Width;

   ------------------------
   -- Line_Needs_Refresh --
   ------------------------

   function Line_Needs_Refresh
     (Buffer : access Source_Buffer_Record;
      Line   : Buffer_Line_Type) return Boolean is
   begin
      if Buffer.Line_Data (Line).Side_Info_Data /= null then
         for J in Buffer.Line_Data (Line).Side_Info_Data'Range loop
            if not Buffer.Line_Data (Line).Side_Info_Data (J).Set then
               return True;
            end if;
         end loop;
      end if;

      return False;
   end Line_Needs_Refresh;

   ----------------------
   -- Create_Side_Info --
   ----------------------

   procedure Create_Side_Info
     (Buffer : access Source_Buffer_Record;
      Line   : Buffer_Line_Type)
   is
      Columns_Config : Line_Info_Display_Array_Access
        renames Buffer.Editable_Line_Info_Columns.all;
   begin
      if Columns_Config /= null then
         if Buffer.Line_Data (Line).Side_Info_Data = null then
            Buffer.Line_Data (Line).Side_Info_Data := new
              Line_Info_Width_Array (Columns_Config'Range);

            for K in Columns_Config'Range loop
               Buffer.Line_Data (Line).Side_Info_Data (K) :=
                 (Message_Reference_List.Empty_List,
                  Action => null,
                  Set    => not Columns_Config (K).Every_Line);
            end loop;
         end if;
      end if;
   end Create_Side_Info;

   -----------------------
   -- Needs_To_Be_Saved --
   -----------------------

   function Needs_To_Be_Saved
     (Buffer : access Source_Buffer_Record'Class) return Boolean is
   begin
      --  Only modified and non-empty unsaved buffers need to be saved
      return Get_Status (Buffer) = Modified
        or else (Get_Status (Buffer) = Unsaved
                 and then Get_Char_Count (Buffer) > 0);
   end Needs_To_Be_Saved;

   --------------------
   -- Has_Been_Saved --
   --------------------

   function Has_Been_Saved
     (Buffer : access Source_Buffer_Record'Class) return Boolean is
   begin
      return Buffer.Save_Complete;
   end Has_Been_Saved;

   ----------------
   -- Get_Status --
   ----------------

   function Get_Status
     (Buffer : access Source_Buffer_Record) return Status_Type is
   begin
      --  If the buffer has an empty queue (no modifications performed),
      --  and a saved position = 0 (loaded from a file) => unmodified
      --  Else, if saved position = current queue position => saved
      --  Else, if the queue is not empty => modified
      --  Else => unsaved (queue empty, and not loaded from a file, not saved).
      if (Undo_Queue_Empty (Buffer.Queue)
          and then Redo_Queue_Empty (Buffer.Queue)
          and then Buffer.Saved_Position /= -1)
        or else
          (Buffer.Saved_Position = Get_Position (Buffer.Queue)
           and then Buffer.Saved_Position = 0)
      then
         return Unmodified;

      elsif Buffer.Saved_Position = Get_Position (Buffer.Queue) then
         return Saved;

      elsif not Undo_Queue_Empty (Buffer.Queue)
        or else not Redo_Queue_Empty (Buffer.Queue)
      then
         return Modified;

      else
         return Unsaved;
      end if;
   end Get_Status;

   ---------------------------
   -- Get_Extra_Information --
   ---------------------------

   function Get_Extra_Information
     (Buffer : Source_Buffer)
      return Extra_Information_Array_Access is
   begin
      return Buffer.Extra_Information;
   end Get_Extra_Information;

   ---------------------
   -- Get_Highlighter --
   ---------------------

   function Get_Highlighter
     (Editor : access Source_Buffer_Record)
      return Source_Highlighter is
   begin
      return Editor.Highlighter;
   end Get_Highlighter;

   --------------------------------
   -- Get_First_Non_Blank_Column --
   --------------------------------

   function Get_First_Non_Blank_Column
     (Buffer : access Source_Buffer_Record;
      Line   : Editable_Line_Type)
      return Visible_Column_Type
   is
      Str   : Src_String := Get_String_At_Line (Source_Buffer (Buffer), Line);
      Index : Natural    := 1;
   begin
      if Str.Contents /= null and then
        not Is_Blank_Line (Str.Contents (1 .. Str.Length))
      then
         Skip_Blanks (Str.Contents (1 .. Str.Length), Index);
      end if;
      Free (Str);
      return Visible_Column_Type (Index);
   end Get_First_Non_Blank_Column;

   ---------------
   -- Get_Block --
   ---------------

   function Get_Block
     (Editor             : access Source_Buffer_Record;
      Line               : Editable_Line_Type;
      Update_Immediately : Boolean;
      Filter             : Language.Tree.Category_Array :=
        Language.Tree.Null_Category_Array;
      Column             : Visible_Column_Type := 0) return Block_Record is
   begin
      if Line = 0 then
         return New_Block;
      end if;

      --  If we are inserting internally (for instance blank lines) then
      --  we should not update the contents at this stage: the editor does
      --  not know yet that the lines that we have just inserted are
      --  special
      if Editor.Inserting then
         return New_Block;
      end if;

      declare
         Tree : Semantic_Tree'Class :=
           Editor.Kernel.Get_Abstract_Tree_For_File ("EDIT", Editor.Filename);

      begin
         if Update_Immediately then
            Tree.Update;
         end if;

         if Tree.Is_Ready
           or else Update_Immediately
         then
            --  Take the first possible project. This should not impact block
            --  computation, which does not need xref information
            declare
               Real_Column : constant Visible_Column_Type :=
                 (if Column /= 0
                  then Column
                  else Get_First_Non_Blank_Column (Editor, Line));
               Node : constant Semantic_Node'Class := Tree.Node_At
                    ((Line   => Integer (Line),
                      Column => Real_Column,
                      others => <>),
                  Filter);
            begin
               if Node = No_Semantic_Node then
                  return New_Block;
               else
                  return Block_Record'
                    (Indentation_Level => 0,
                     Offset_Start      => Integer (Node.Sloc_Start.Column),
                     Stored_Offset     => 0,
                     First_Line => Editable_Line_Type (Node.Sloc_Start.Line),
                     Last_Line  => Editable_Line_Type (Node.Sloc_End.Line),
                     Name              => Node.Name,
                     Block_Type        => Node.Category,
                     Color             => Null_RGBA);
               end if;
            end;
         else
            return New_Block;
         end if;
      end;
   end Get_Block;

   -----------------------
   -- Get_Current_Block --
   -----------------------

   function Get_Current_Block
     (Editor   : access Source_Buffer_Record;
      Absolute : Boolean := False)
      return Block_Record
   is
      Line   : Editable_Line_Type;
      Column : Visible_Column_Type;
   begin
      Editor.Get_Cursor_Position (Line, Column);
      return Editor.Get_Block
        (Line               => Line,
         Update_Immediately => False,
         Column             => (if Absolute then Column else 0));
   end Get_Current_Block;

   --------------------------
   -- Get_Subprogram_Block --
   --------------------------

   function Get_Subprogram_Block
     (Editor : access Source_Buffer_Record;
      Line   : Editable_Line_Type;
      Update_Tree : Boolean := False) return Block_Record is
   begin
      return Get_Block
        (Editor, Line, Update_Tree,
         Filter =>
           (Cat_Package, Cat_Namespace, Cat_Task, Cat_Procedure,
            Cat_Function, Cat_Constructor, Cat_Method, Cat_Destructor,
            Cat_Protected, Cat_Entry, Cat_Class, Cat_Structure, Cat_Union));
   end Get_Subprogram_Block;

   -------------------------
   -- Get_Subprogram_Name --
   -------------------------

   function Get_Subprogram_Name
     (Editor : access Source_Buffer_Record;
      Line   : Editable_Line_Type) return String
   is
      Block : constant Block_Record :=
        Get_Subprogram_Block (Editor, Line);
   begin
      if Block.Name /= No_Symbol then
         return Get (Block.Name).all;
      else
         return "";
      end if;
   end Get_Subprogram_Name;

   ---------------------------
   -- Has_Block_Information --
   ---------------------------

   function Has_Block_Information
     (Editor : access Source_Buffer_Record) return Boolean is
   begin
      return Editor.Parse_Blocks;
   end Has_Block_Information;

   -----------------
   -- Save_Cursor --
   -----------------

   procedure Save_Cursor
     (Buffer        : Source_Buffer;
      Mark          : Gtk_Text_Mark;
      Cursor_Line   : out Gint;
      Cursor_Offset : out Gint)
   is
      Cursor_Iter : Gtk_Text_Iter;
   begin
      Buffer.Do_Not_Move_Cursor := True;
      Get_Iter_At_Mark (Buffer, Cursor_Iter, Mark);
      Cursor_Line := Get_Line (Cursor_Iter);
      Cursor_Offset := Get_Line_Offset (Cursor_Iter);
   end Save_Cursor;

   ------------------
   -- Place_Cursor --
   ------------------

   procedure Place_Cursor
     (Buffer        : Source_Buffer;
      Mark          : Gtk_Text_Mark;
      Cursor_Line   : Gint;
      Cursor_Offset : Gint)
   is
      Cursor_Move : Gint := Cursor_Offset;
      Result      : Boolean := True;
      Offset      : Gint := 0;
      Iter        : Gtk_Text_Iter;
   begin
      --  Can't move the cursor if the mark was deleted
      if not Get_Move_Cursor_When_Formatting (Buffer.Get_Language)
        or else Mark.Get_Deleted
      then
         return;
      end if;

      --  If the cursor was located before the first non-blank character,
      --  move it to that character. This is more usual for Emacs users,
      --  and more user friendly generally.
      Get_Iter_At_Line (Buffer, Iter, Cursor_Line);
      Set_Line_Offset (Iter, 0);

      while Result
        and then not Ends_Line (Iter)
        and then Glib.Unicode.Is_Space (Get_Char (Iter))
      loop
         Forward_Char (Iter, Result);
         Offset := Offset + 1;
      end loop;

      Cursor_Move := Cursor_Move - Offset;

      if Cursor_Move > 0 then
         Forward_Chars (Iter, Cursor_Move, Result);
      end if;

      Buffer.Do_Not_Move_Cursor := False;
      if Buffer.Insert_Mark = Mark then
         --  Move the main cursor
         Place_Cursor (Buffer, Iter);
      else
         --  Move multiline cursor
         Move_Mark (Buffer, Mark, Iter);
      end if;
   end Place_Cursor;

   ----------------------
   -- Range_Formatting --
   ----------------------

   procedure Range_Formatting
     (Buffer   : Source_Buffer;
      Mark     : Gtk_Text_Mark;
      From, To : Gtk_Text_Iter;
      Force    : Boolean := False)
   is
      pragma Unreferenced (Force);
      G : Group_Block := New_Group (Buffer.Queue);

      Cursor_Line   : Gint;
      Cursor_Offset : Gint;

      From_Column : Visible_Column_Type;
      To_Column   : Visible_Column_Type;

      From_Line : Editable_Line_Type;
      To_Line   : Editable_Line_Type;

      Provider : constant Editor_Formatting_Provider_Access :=
        Src_Editor_Module.Get_Range_Formatting_Provider (Buffer.Get_Language);
   begin
      if Provider = null then
         Trace
           (Me_Formatters,
            "rangeFormatting Provider is not defined.");
         return;
      else
         Trace
           (Me_Formatters,
            "Found rangeFormatting Provider: " & Provider.Get_Name);
      end if;

      Save_Cursor
        (Buffer        => Buffer,
         Mark          => Mark,
         Cursor_Line   => Cursor_Line,
         Cursor_Offset => Cursor_Offset);

      if Src_Editor_Buffer.Line_Information.Lines_Are_Real (Buffer) then
         Get_Iter_Position (Buffer, From, From_Line, From_Column);
         Get_Iter_Position (Buffer, To, To_Line, To_Column);
      else
         Trace (Me_Formatters, "Unreal lines found when formatting");
         declare
            Search_Line : Gint;
            Start_Line  : Gint;
            End_Line    : Gint;
         begin
            Start_Line := Get_Line (From);
            End_Line := Get_Line (To);

            --  Gint and Buffer_Line_Type are off by 1
            From_Line :=
              Get_Editable_Line (Buffer, Buffer_Line_Type (Start_Line + 1));
            To_Line :=
              Get_Editable_Line (Buffer, Buffer_Line_Type (End_Line + 1));

            --  Get_Editable_Line will return 0 for non-editable line.
            --  Search for the first and last editable lines between
            --  Start_Line and End_Line.

            --  Search upward for the first editable line
            --  We have tested Start_Line + 1, the next one is Start_Line + 2
            Search_Line := Start_Line + 2;
            while From_Line = 0 and then Search_Line <= End_Line loop
               From_Line :=
                 Get_Editable_Line
                   (Buffer, Buffer_Line_Type (Search_Line));
               Search_Line := Search_Line + 1;
            end loop;

            --  Search downward for the last editable line
            --  We have tested End_Line + 1, the previous one is End_Line
            Search_Line := End_Line;
            while To_Line = 0 and then Search_Line >= Start_Line loop
               To_Line :=
                 Get_Editable_Line
                   (Buffer, Buffer_Line_Type (Search_Line));
               Search_Line := Search_Line - 1;
            end loop;

            Trace (Me, "End_Line: " & End_Line'Image);
            Trace (Me, "Search_Line: " & Search_Line'Image);
            Trace (Me, "To_Line: " & To_Line'Image);

            --  Include all characters from the last line
            To_Column :=
              Buffer.Editor_Buffer.New_Location
                (Line   => Integer (To_Line),
                 Column => 1).End_Of_Line.Column;
         end;
      end if;

      if not Provider.On_Range_Formatting
               (Buffer.Editor_Buffer.New_Location (Integer (From_Line), 1),
                Buffer.Editor_Buffer.New_Location
                  (Integer (To_Line), To_Column),
                Cursor_Line => Natural (Cursor_Line) + 1,
                Cursor_Move => Integer (Cursor_Offset))
      then
         Trace (Me_Formatters, "Formatting failed");
      end if;

      Place_Cursor
        (Buffer        => Buffer,
         Mark          => Mark,
         Cursor_Line   => Cursor_Line,
         Cursor_Offset => Cursor_Offset);
   end Range_Formatting;

   ------------------------
   -- On_Type_Formatting --
   ------------------------

   procedure On_Type_Formatting
     (Buffer   : Source_Buffer;
      Mark     : Gtk_Text_Mark;
      From, To : Gtk_Text_Iter;
      Force    : Boolean := False)
   is
      pragma Unreferenced (Force);
      Cursor_Line   : Gint;
      Cursor_Offset : Gint;

      Start_Line, End_Line     : Editable_Line_Type;
      Start_Column, End_Column : Visible_Column_Type;

      Provider : constant Editor_Formatting_Provider_Access :=
        Src_Editor_Module.Get_On_Type_Formatting_Provider
          (Buffer.Get_Language);
   begin
      if Provider = null then
         Trace
           (Me_Formatters,
            "onTypeFormatting Provider is not defined.");
         return;
      else
         Trace
           (Me_Formatters,
            "Found onTypeFormatting Provider: " & Provider.Get_Name);
      end if;

      Save_Cursor
        (Buffer        => Buffer,
         Mark          => Mark,
         Cursor_Line   => Cursor_Line,
         Cursor_Offset => Cursor_Offset);

      Get_Iter_Position (Buffer, From, Start_Line, Start_Column);
      Get_Iter_Position (Buffer, To, End_Line, End_Column);

      if not Provider.On_Type_Formatting
               (Buffer.Editor_Buffer.New_Location
                  (Integer (Start_Line), Start_Column),
                Buffer.Editor_Buffer.New_Location
                  (Integer (End_Line), End_Column),
                Cursor_Line => Natural (Cursor_Line) + 1)
      then
         Trace (Me_Formatters, "Fails to format on new line");
      end if;

      Place_Cursor
        (Buffer        => Buffer,
         Mark          => Mark,
         Cursor_Line   => Cursor_Line,
         Cursor_Offset => Cursor_Offset);
   end On_Type_Formatting;

   -------------------
   -- Should_Indent --
   -------------------

   function Should_Indent (Buffer : Source_Buffer) return Boolean is
      Lang : constant Language_Access := Get_Language (Buffer);
   begin
      return
        Get_Language_Context (Lang).Can_Indent
        and then Lang.Get_Indentation_Style /= None;
   end Should_Indent;

   --------------------
   -- Do_Indentation --
   --------------------

   function On_Indent_Action
     (Buffer            : Source_Buffer;
      Current_Line_Only : Boolean := False;
      Force             : Boolean := False) return Boolean
   is
      Iter, End_Pos : Gtk_Text_Iter;
      Result        : Boolean;
   begin
      if not Buffer.Writable then
         End_Action (Buffer);
         return False;
      end if;

      Get_Selection_Bounds (Buffer, Iter, End_Pos, Result);

      if not Current_Line_Only and then Result then
         --  Do not consider a line selected if only the first character
         --  is selected.

         if Get_Line_Offset (End_Pos) = 0 then
            Backward_Char (End_Pos, Result);
         end if;

         --  Do not consider a line selected if only the last character is
         --  selected.

         if Ends_Line (Iter) then
            Forward_Char (Iter, Result);
         end if;

      else
         Get_Iter_At_Mark (Buffer, Iter, Buffer.Insert_Mark);
         Copy (Iter, Dest => End_Pos);
      end if;

      return On_Indent_Action (Buffer, Iter, End_Pos, Force);
   end On_Indent_Action;

   --------------------
   -- Do_Indentation --
   --------------------

   function On_Indent_Action
     (Buffer     : Source_Buffer;
      From, To   : Gtk_Text_Iter;
      Force      : Boolean := False) return Boolean
   is
      Lang          : constant Language_Access := Get_Language (Buffer);
      Indent_Style  : Indentation_Kind;
      End_Pos       : Gtk_Text_Iter;
      Iter          : Gtk_Text_Iter;
      pragma Suppress (Access_Check, Slice);
      Result        : Boolean;
      Indent_Params : Indent_Parameters;
   begin
      if not Buffer.Writable then
         Trace (Me, "Buffer not writable");
         End_Action (Buffer);
         return False;
      end if;

      if not Get_Language_Context (Lang).Can_Indent then
         Trace (Me, "Language Can't Indent");
         return False;
      end if;

      Get_Indentation_Parameters
        (Lang         => Lang,
         Params       => Indent_Params,
         Indent_Style => Indent_Style);

      if Indent_Style = None then
         return False;
      end if;

      --  What should we indent (current line or current selection) ?

      Copy (From, Dest => Iter);
      Set_Line_Offset (Iter, 0);
      Copy (To, Dest => End_Pos);

      if not Ends_Line (End_Pos) then
         Forward_To_Line_End (End_Pos, Result);
      end if;

      End_Action (Buffer);

      Range_Formatting
        (Buffer => Buffer,
         Mark   => Buffer.Insert_Mark,
         From   => Iter,
         To     => End_Pos,
         Force  => Force);

      return True;

   exception
      when E : others =>
         --  Stop propagation of exception, since doing nothing
         --  in this callback is harmless.

         Buffer.Do_Not_Move_Cursor := False;

         Trace (Me, E);
         return False;
   end On_Indent_Action;

   ------------------------
   -- Newline_And_Indent --
   ------------------------

   procedure Newline_And_Indent
     (Buffer : access Source_Buffer_Record; As_Is : Boolean)
   is
      Ignore, Result : Boolean;

      procedure Strip_Blanks_From_Previous_Line;
      --  Removes blanks from the end of the previous line

      -------------------------------------
      -- Strip_Blanks_From_Previous_Line --
      -------------------------------------

      procedure Strip_Blanks_From_Previous_Line is
         Iter         : Gtk.Text_Iter.Gtk_Text_Iter;
         End_Iter     : Gtk.Text_Iter.Gtk_Text_Iter;
         Line         : Gint;
         Start_Offset : Gint;
         End_Offset   : Gint;
         Char         : Character;
         Result       : Boolean;
      begin
         --  Retrieve the cursor position (after insertion of the newline) and
         --  backward from one char to retrieve the end of the previous line.
         Buffer.Get_Cursor_Position (Iter);
         Backward_Char (Iter, Result);

         if Iter = Null_Text_Iter or else not Result then
            return;
         end if;

         End_Offset := Get_Line_Offset (Iter);

         --  Backward the iter while we find trailing blanks
         while not Starts_Line (Iter) loop
            Backward_Char (Iter, Result);
            Char := Get_Char (Iter);
            exit when Char /= ' ' and then Char /= ASCII.HT;
         end loop;

         Start_Offset := Get_Line_Offset (Iter);

         --  Delete the trailing blanks, if any
         if Start_Offset /= 0
           and then End_Offset /= Start_Offset + 1
         then
            Line := Get_Line (Iter);
            Buffer.Get_Iter_At_Line_Offset (Iter, Line, Start_Offset + 1);
            Buffer.Get_Iter_At_Line_Offset (End_Iter, Line, End_Offset);
            Buffer.Delete_Interactive (Iter, End_Iter, True, Result);
         end if;

      exception
         when E : others =>
            Trace (Me, E);
      end Strip_Blanks_From_Previous_Line;

      G : Group_Block := New_Group (Buffer.Queue);
   begin
      if not As_Is then
         Word_Added
           (Buffer      => Source_Buffer (Buffer),
            Character   => Gdk.Keyval.To_Unicode (GDK_Return),
            Interactive => not Buffer.Inserting);
      end if;

      --  If there is a selection, delete it
      if Selection_Exists (Buffer) then
         Ignore := Delete_Selection (Buffer, True, True);

         External_End_Action (Buffer);
      end if;

      --  Insert the newline
      Result :=
        Insert_Interactive_At_Cursor (Buffer, (1 => ASCII.LF), True);

      --  Now that the newline is inserted, strip the trailing blanks from the
      --  previous line, if any.
      if Buffer.Strip_Trailing_Blanks then
         Strip_Blanks_From_Previous_Line;
      end if;

      if Should_Indent (+Buffer) and then Result then
         declare
            Params       : Indent_Parameters;
            Indent_Style : Indentation_Kind;

            Current_Sync_Mode : constant Cursors_Sync_Type :=
              Get_Cursors_Sync (+Buffer);
            procedure Indent_Cursor (M : Gtk_Text_Mark);
            procedure Indent_Cursor
              (M : Gtk_Text_Mark)
            is
               S, L : Gtk_Text_Iter;
            begin
               Get_Iter_At_Mark (Buffer, L, M);
               Copy (L, S);

               if not As_Is then
                  Backward_Line (S, Ignore);
               end if;

               if not Ends_Line (L) then
                  Forward_To_Line_End (L, Ignore);
               end if;

               On_Type_Formatting
                 (Buffer => Source_Buffer (Buffer),
                  Mark   => M,
                  From   => S,
                  To     => L);
            end Indent_Cursor;

         begin
            Buffer.Get_Language.Get_Indentation_Parameters
              (Params, Indent_Style);

            for Cursor of Get_Cursors (+Buffer) loop
               Set_Manual_Sync (Cursor);
               Indent_Cursor (Get_Mark (Cursor));
            end loop;
            Set_Cursors_Sync (+Buffer, Current_Sync_Mode);
         end;
      end if;
   end Newline_And_Indent;

   ---------------
   -- Do_Refill --
   ---------------

   function Do_Refill (Buffer : Source_Buffer) return Boolean is
      type Comment_Kind is (None, Single_Line, Multiple_Lines);
      --  We classify programming language comments in two main kinds:
      --  (1) Single-line comments: they have a Begin-Comment delimiter and
      --      implicitly terminate at newline.
      --  (2) Multiple-line comments: they have Begin-Comment (BC) and
      --      End-Comment (EC) delimiters.
      --  There are languages which simultaneously support both kinds of
      --  comments (for example, C++).

      Single_Line_BC_Len        : Natural := 0;
      Single_Line_BC_Pattern    : Pattern_Matcher_Access;

      Multiple_Lines_BC_Len     : Natural := 0;
      Multiple_Lines_BC_Pattern : Pattern_Matcher_Access;

      Multiple_Lines_EC_Len     : Natural := 0;
      Multiple_Lines_EC_Pattern : Pattern_Matcher_Access;

      Lang_Context              : constant Language_Context_Access :=
        (if Buffer.Lang /= null
         then Get_Language_Context (Buffer.Lang)
         else null);

      function Is_Empty (Line : Src_String) return Boolean;
      --  Return True if Line is empty (no contents it has only spaces/HT)

      procedure Refill_Comments
        (From_Line : Editable_Line_Type;
         To_Line   : Editable_Line_Type);
      --  Scan the Buffer in the range From_Line .. To_Line and refill the
      --  comments. Lines not containing comments are left unmodified.

      procedure Refill_Plain_Text
        (From_Line : Editable_Line_Type;
         To_Line   : Editable_Line_Type);
      --  Refill the contents of the buffer. It assumes that the contents of
      --  the buffer is plain text.

      procedure Scan_Comment
        (Line : String;
         Kind : out Comment_Kind;
         Last : out Natural);
      --  Search in Line for the Begin-Comment pattern of single-line and
      --  multiple-line comment. If the pattern of a single line comment
      --  is found then Kind is set to Single_Line; if the pattern of a
      --  multiple-line comment is found then Kind is set to Multiple_Lines.
      --  In both cases Last returns the position of the last character of
      --  the comment prefix. Example: for " -- xxx", returns 4, pointing just
      --  before the first x. If no comment is found then Kind is None and Last
      --  is 0.
      --
      --  Special case: If the pattern to start a multiple-line comment and the
      --  pattern to terminate the comment are simultaneously found in Line,
      --  and the pattern to terminate the comment is found after the last
      --  position of the pattern to begin the comment, then the comment is
      --  not considered a candidate to be refilled and Kind is set to None
      --  and Last is 0.

      procedure Setup_Comment_Regexps;
      --  Check if the language for the current file is known, and if it is
      --  setup the regexps to use to find the bounds of comments.

      procedure Find_Paragraph_Bounds
        (From_Line, To_Line : out Editable_Line_Type);
      --  Find the bounds of the region to refill (it will be the user
      --  selection if available, or guessed automatically).

      procedure Unchecked_Free is new
        Ada.Unchecked_Deallocation (Pattern_Matcher, Pattern_Matcher_Access);

      --------------
      -- Is_Empty --
      --------------

      function Is_Empty (Line : Src_String) return Boolean is

         function Only_Spaces return Boolean;
         --  Return True is Line contains only spaces or HT

         -----------------
         -- Only_Spaces --
         -----------------

         function Only_Spaces return Boolean is
            Result : Boolean := True;

         begin
            for K in 1 .. Line.Length loop
               if Line.Contents (K) /= ' '
                 and then Line.Contents (K) /= ASCII.HT
               then
                  Result := False;
               end if;
            end loop;

            return Result;
         end Only_Spaces;

      begin
         return Line.Contents = null or else Only_Spaces;
      end Is_Empty;

      ---------------------
      -- Refill_Comments --
      ---------------------

      procedure Refill_Comments
        (From_Line : Editable_Line_Type;
         To_Line   : Editable_Line_Type)
      is
         Max_Line_Length : constant Positive := Highlight_Column.Get_Pref;
         Tab_Width       : constant Integer := Integer (Buffer.Tab_Width);
         Comment         : Unbounded_String;
         In_ML_Comment   : Boolean := False;
         Max_Line        : Natural := 0;
         New_Text        : Unbounded_String := To_Unbounded_String ("");
         Prefix          : Unbounded_String;

         function ML_End_Comment_Last (Line : String) return Natural;
         --  If the pattern that terminates a comment of a multi-line comment
         --  is found then return the position of its last character. Example,
         --  for " */" return 3. Return 0 if the pattern is not found.

         procedure Refill_One_Comment;
         --  Refill the current comment

         -------------------------
         -- ML_End_Comment_Last --
         -------------------------

         function ML_End_Comment_Last (Line : String) return Natural is
            Matches : Match_Array (0 .. 0);

         begin
            pragma Assert (Multiple_Lines_EC_Len > 0);
            Match (Multiple_Lines_EC_Pattern.all, Line, Matches);

            if Matches (0) /= No_Match then
               return Matches (0).Last;
            end if;

            return 0;
         end ML_End_Comment_Last;

         ------------------------
         -- Refill_One_Comment --
         ------------------------

         procedure Refill_One_Comment is
            Blanks_Prefix : constant String (1 .. Length (Prefix)) :=
                              (others => ' ');
            Search_For    : constant Character_Set := To_Set (ASCII.LF);
            From          : Positive;
            Is_First_Line : Boolean;
            Len           : Natural;
            New_Comment   : Unbounded_String;
            Pos           : Natural;

         begin
            --  Terminate the last line of the current comment

            Append (Comment, ASCII.LF);

            --  Reformat this comment

            New_Comment := Pretty_Fill (To_String (Comment), Max_Line);

            --  For single-line comments add Prefix before each line of the
            --  refilled comment. For multi-line comments Prefix is added only
            --  to the first line of the comment and blanks are added for
            --  subsequent lines. For example the C-style comment:
            --
            --         /* This is an example of refilling text */
            --
            --  can be refilled as follows (assumming Max_Line = 24):
            --
            --         /* This is an example
            --            of refilling text */

            Is_First_Line := True;
            Len  := Length (New_Comment);
            From := 1;

            while From <= Len loop
               Pos := Index (New_Comment, Search_For, From);

               if In_ML_Comment
                 and then not Is_First_Line
               then
                  Append (New_Text, Blanks_Prefix);
               else
                  Append (New_Text, To_String (Prefix));
               end if;

               Append (New_Text, Slice (New_Comment, From, Pos));

               Is_First_Line := False;
               From := Pos + 1;
            end loop;
         end Refill_One_Comment;

         --  Local variables

         In_Comment : Boolean := False;
         BC_Last    : Natural;
         EC_Last    : Natural;
         Line       : Src_String;
         To_Length  : Character_Offset_Type;

      begin  --  Refill_Comments
         for K in From_Line .. To_Line loop
            Line := Get_String_At_Line (Buffer, K);

            if K = To_Line then
               To_Length := Character_Offset_Type (Line.Length);
            end if;

            --  Handle continuation of multi-line comment

            if In_ML_Comment then

               --  Empty line in multi-line comment; continue accumulating
               --  all the text of the current comment

               if Is_Empty (Line) then
                  Append (Comment, ASCII.LF);

               --  Non-empty line. Check if this line has the end-comment
               --  delimiter

               else
                  declare
                     S : String renames Line.Contents (1 .. Line.Length);

                  begin
                     EC_Last := ML_End_Comment_Last (S);

                     --  End of multi-line comment not found yet; continue
                     --  acumulating all the text of the current comment

                     if EC_Last = 0 then
                        Append (Comment,
                          To_Unbounded_String (ASCII.LF & S));

                     --  End of multi-line comment found

                     else
                        --  Add the last line of this comment (including the
                        --  end-comment delimiter) to the buffer of acumulated
                        --  comments and refill the whole comment

                        Append (Comment,
                          To_Unbounded_String (ASCII.LF & S (1 .. EC_Last)));
                        Refill_One_Comment;

                        --  If there is some text after the end-comment
                        --  delimiter then move it to the next line

                        if EC_Last < Line.Length then
                           Append (New_Text,
                             S (EC_Last + 1 .. Line.Length) & ASCII.LF);
                        end if;

                        In_ML_Comment := False;
                        In_Comment := False;
                     end if;
                  end;
               end if;

            --  Empty line

            elsif Is_Empty (Line) then

               --  For single-line comments an empty line means that we must
               --  stop acumulating comments and we must refill the text of
               --  the acumulated comments

               if In_Comment then
                  pragma Assert (not In_ML_Comment);
                  Refill_One_Comment;
                  In_Comment := False;
               end if;

               Append (New_Text, ASCII.LF);

            --  Line containing text

            else
               declare
                  S    : String renames Line.Contents (1 .. Line.Length);
                  Kind : Comment_Kind;

               begin
                  Scan_Comment (S, Kind, BC_Last);

                  --  The line does not have a begin-comment delimiter

                  if Kind = None then
                     pragma Assert (not In_ML_Comment);

                     --  If we were acumulating single-line comments and this
                     --  line does not have a begin-comment delimiter then it
                     --  is time to refill the accumulated comment.

                     if In_Comment then
                        Refill_One_Comment;
                        In_Comment := False;
                     end if;

                     Append (New_Text, To_Unbounded_String (S));
                     Append (New_Text, ASCII.LF);

                  --  The line has some begin-comment delimiter: let's start
                  --  acumulating comments.

                  elsif not In_Comment then

                     --  Calculate the length of the prefix taking into account
                     --  spaces and horizontal tabs

                     declare
                        Prefix_Length : Natural := 0;

                     begin
                        for K in 1 .. BC_Last loop
                           if S (K) = ASCII.HT then
                              Prefix_Length := Prefix_Length + Tab_Width;
                           else
                              Prefix_Length := Prefix_Length + 1;
                           end if;
                        end loop;

                        --  Calculate the maximum line length to refill

                        Max_Line := Max_Line_Length - Prefix_Length + 1;
                     end;

                     Prefix := To_Unbounded_String (S (1 .. BC_Last - 1));
                     In_ML_Comment := Kind = Multiple_Lines;

                     if In_ML_Comment then
                        EC_Last :=
                          ML_End_Comment_Last (S (BC_Last .. Line.Length));
                     else
                        EC_Last := 0;
                     end if;

                     --  Check if this line has the end-comment delimiter

                     if EC_Last = 0 then
                        Comment :=
                          To_Unbounded_String (S (BC_Last .. Line.Length));
                        In_Comment := True;
                     else
                        Comment :=
                          To_Unbounded_String (S (BC_Last .. EC_Last));
                        Refill_One_Comment;

                        --  If there is some text after the end-comment
                        --  delimiter then move it to the next line

                        if EC_Last < Line.Length then
                           Append (New_Text,
                             S (EC_Last + 1 .. Line.Length) & ASCII.LF);
                        end if;
                     end if;

                  --  We are acumulating text of single-line comments; let's
                  --  continue acumulating comments

                  else
                     Append (Comment,
                       To_Unbounded_String
                         (ASCII.LF & S (BC_Last .. Line.Length)));
                  end if;
               end;
            end if;
         end loop;

         if In_Comment then
            Refill_One_Comment;
         end if;

         --  Replace selected text with the new one

         if Get_Line_Count (Buffer) >= Gint (To_Line + 1) then
            Replace_Slice
              (Buffer, From_Line, 1, To_Line + 1, 1, To_String (New_Text));
         else
            Replace_Slice
              (Buffer, From_Line, 1, To_Line, To_Length,
               To_String (New_Text));
         end if;

         declare
            Iter : Gtk_Text_Iter;
            Dummy : Boolean;
         begin
            Buffer.Get_Iter_At_Mark (Iter, Buffer.Get_Insert);
            Backward_Line (Iter, Dummy);
            Forward_To_Line_End (Iter, Dummy);
            Buffer.Place_Cursor (Iter);
         end;

      exception
         when E : others =>
            Trace (Me, E);
      end Refill_Comments;

      -----------------------
      -- Refill_Plain_Text --
      -----------------------

      procedure Refill_Plain_Text
        (From_Line : Editable_Line_Type;
         To_Line   : Editable_Line_Type)
      is
         Max_Line_Length : constant Positive := Highlight_Column.Get_Pref;
         Tab_Width       : constant Integer := Integer (Buffer.Tab_Width);
         Comment         : Unbounded_String;
         Max_Line        : Natural := 0;
         New_Text        : Unbounded_String := To_Unbounded_String ("");
         Prefix          : Unbounded_String;

         procedure Refill_Text;
         --  Refill the acumulated text

         -----------------
         -- Refill_Text --
         -----------------

         procedure Refill_Text is
            Search_For  : constant Character_Set := To_Set (ASCII.LF);
            From        : Positive;
            Len         : Natural;
            New_Comment : Unbounded_String;
            Pos         : Natural;

         begin
            --  Terminate the last line of the current comment

            Append (Comment, ASCII.LF);

            --  Reformat this comment

            New_Comment := Pretty_Fill (To_String (Comment), Max_Line);

            --  Add Prefix before each line. Thus we ensure that all the
            --  refilled text has the same left margin.

            Len  := Length (New_Comment);
            From := 1;

            while From <= Len loop
               Pos := Index (New_Comment, Search_For, From);
               Append (New_Text, To_String (Prefix));
               Append (New_Text, Slice (New_Comment, From, Pos));

               From := Pos + 1;
            end loop;
         end Refill_Text;

         --  Local variables

         Acumulating : Boolean := False;
         Line        : Src_String;

      begin  --  Refill_Plain_Text
         for K in From_Line .. To_Line loop
            Line := Get_String_At_Line (Buffer, K);

            --  Empty line

            if Is_Empty (Line) then

               --  For single-line comments an empty line means that we must
               --  stop acumulating comments and we must refill the text of
               --  the acumulated comments

               if Acumulating then
                  Refill_Text;
                  Acumulating := False;
               end if;

               Append (New_Text, ASCII.LF);

            --  Line containing text

            else
               declare
                  S    : String renames Line.Contents (1 .. Line.Length);

               begin
                  --  If we are acumulating text then let's continue
                  --  acumulating text to refill

                  if Acumulating then
                     Append (Comment, To_Unbounded_String (ASCII.LF & S));

                  else
                     --  Calculate the length of the prefix taking into account
                     --  spaces and horizontal tabs

                     declare
                        Prefix_Length : Natural  := 0;
                        J             : Positive := 1;

                     begin
                        while S (J) = ASCII.HT or else S (J) = ' ' loop
                           if S (J) = ASCII.HT then
                              Prefix_Length := Prefix_Length + Tab_Width;
                           else
                              Prefix_Length := Prefix_Length + 1;
                           end if;

                           J := J + 1;
                        end loop;

                        --  Calculate the maximum line length to refill

                        Max_Line := Max_Line_Length - Prefix_Length;
                        Prefix   := To_Unbounded_String (S (1 .. J - 1));
                        Comment  := To_Unbounded_String (S (J .. Line.Length));
                     end;

                     Acumulating := True;
                  end if;
               end;
            end if;
         end loop;

         if Acumulating then
            Refill_Text;
         end if;

         --  Replace selected text by the new one

         if Get_Line_Count (Buffer) >= Gint (To_Line + 1) then
            Replace_Slice
              (Buffer, From_Line, 1, To_Line + 1, 1, To_String (New_Text));
         else
            Replace_Slice
              (Buffer, From_Line, 1,
               To_Line, Character_Offset_Type (Length (New_Text)),
               To_String (New_Text));
         end if;

      exception
         when E : others =>
            Trace (Me, E);
      end Refill_Plain_Text;

      ------------------
      -- Scan_Comment --
      ------------------

      procedure Scan_Comment
        (Line : String;
         Kind : out Comment_Kind;
         Last : out Natural)
      is
         Matches        : Match_Array (0 .. 0);
         Single_Line_BC : constant GNAT.Strings.String_Access :=
           (if Lang_Context /= null
            then Lang_Context.Syntax.New_Line_Comment_Start
            else null);
      begin
         Kind := None;
         Last := 0;

         --  If the language supports single-line comments then search for its
         --  pattern in the current line

         if Single_Line_BC_Pattern /= null then
            Match (Single_Line_BC_Pattern.all, Line, Matches);

            if Matches (0) /= No_Match then
               pragma Assert (Matches (0).First = Line'First);

               --  Single-line comments that end with the characters sequence
               --  used to begin the comment are not considered comments to be
               --  refilled. Such comments are probably part of the copyright
               --  header, which we don't want to reformat. For example:

               --      This line is an example of a non-refilled comment    --

               if Single_Line_BC = null
                 or else Line (Line'Last - Single_Line_BC_Len + 1 .. Line'Last)
                    /= Single_Line_BC.all
               then
                  Kind := Single_Line;
                  Last := Matches (0).Last;
               end if;

               return;
            end if;
         end if;

         --  If the language supports multiple-line comments then search for
         --  the pattern that begin a comment in the current line

         if Multiple_Lines_BC_Len /= 0 then
            Match (Multiple_Lines_BC_Pattern.all, Line, Matches);

            if Matches (0) /= No_Match then
               Kind := Multiple_Lines;
               Last := Matches (0).Last;
            end if;
         end if;
      end Scan_Comment;

      ---------------------------
      -- Setup_Comment_Regexps --
      ---------------------------

      procedure Setup_Comment_Regexps is
         Start_Comment_Pattern : constant String := "^\s*";
         --  Start of line, followed by zero or more spaces

         End_Comment_Pattern   : constant String := "\s\s?[^\s]";
         --  One or two spaces, followed by a non-space. If there are more than
         --  two spaces after the comment marker, then we don't recognize it as
         --  a comment line (it's an indented comment, which should not be
         --  reformatted). We also don't recognize it as a comment line if
         --  there is no space after the command marker.

         S : Unbounded_String;

         function Filter_Metachars (S : String) return String;
         --  Filter regexp metacharacters. Currently only '*' needs to be
         --  filtered.

         ----------------------
         -- Filter_Metachars --
         ----------------------

         function Filter_Metachars (S : String) return String is
            Result : String (1 .. 2 * S'Length);
            Pos    : Natural := 0;

         begin
            for J in S'Range loop
               if S (J) = '*' then
                  Pos := Pos + 1;
                  Result (Pos) := '\';
               end if;

               Pos := Pos + 1;
               Result (Pos) := S (J);
            end loop;

            return Result (1 .. Pos);
         end Filter_Metachars;

         --  Local variables

         Single_Line_BC : constant GNAT.Strings.String_Access :=
           (if Lang_Context /= null
            then Lang_Context.Syntax.New_Line_Comment_Start
            else null);
      begin

         --  Handle single-line comments

         --  If the language comment start dectection has been provided via
         --  a regexp, use it directly
         if Lang_Context /= null
           and then Lang_Context.Syntax.New_Line_Comment_Start_Regexp /= null
           and then Lang_Context.Syntax.New_Line_Comment_Start_Regexp.all
             /= Never_Match
         then
            Single_Line_BC_Pattern :=
              new Pattern_Matcher'
                (Lang_Context.Syntax.New_Line_Comment_Start_Regexp.all);
         end if;

         --  If the language comment start dectection has been provided via
         --  a simple string, build the regexp from it instead.
         if Single_Line_BC_Pattern = null
           and then Single_Line_BC /= null
         then
            Single_Line_BC_Len := Single_Line_BC'Length;

            S := To_Unbounded_String (Start_Comment_Pattern);
            Append (S, Filter_Metachars (Single_Line_BC.all));
            Append (S, End_Comment_Pattern);

            Single_Line_BC_Pattern :=
              new Pattern_Matcher'
                (Compile (To_String (S), GNAT.Regpat.Single_Line));
         end if;

         --  Handle multi-line comments

         if Lang_Context /= null
           and then Lang_Context.Syntax.Comment_Start /= null
         then
            pragma Assert (Lang_Context.Syntax.Comment_End /= null);

            Multiple_Lines_BC_Len := Lang_Context.Syntax.Comment_Start'Length;
            Multiple_Lines_EC_Len := Lang_Context.Syntax.Comment_End'Length;

            S := To_Unbounded_String (Start_Comment_Pattern);
            Append
              (S, Filter_Metachars (Lang_Context.Syntax.Comment_Start.all));
            Append (S, End_Comment_Pattern);

            Multiple_Lines_BC_Pattern :=
              new Pattern_Matcher'(Compile (To_String (S)));

            S := To_Unbounded_String
                   (Filter_Metachars (Lang_Context.Syntax.Comment_End.all));
            Multiple_Lines_EC_Pattern :=
              new Pattern_Matcher'(Compile (To_String (S)));
         end if;
      end Setup_Comment_Regexps;

      ---------------------------
      -- Find_Paragraph_Bounds --
      ---------------------------

      procedure Find_Paragraph_Bounds
        (From_Line, To_Line : out Editable_Line_Type)
      is
         Success  : Boolean;
         Current  : Editable_Line_Type;
         Column   : Character_Offset_Type;
         From, To : Gtk_Text_Iter;
      begin
         Get_Selection_Bounds (Buffer, From, To, Success);

         if not Success then
            --  No user selection. If the current file knows about comments,
            --  we try and find the bounds of the current comment paragraph by
            --  searching forward and backward for empty lines (or non-comment
            --  lines); if we are formatting a plain text file, we always
            --  refill the current line.

            Get_Cursor_Position (Buffer, Current, Column);
            Find_Current_Comment_Paragraph
              (Buffer, Current, From_Line, To_Line);

         else
            --  Do not consider a line selected if only the first character is
            --  selected.

            if Get_Line_Offset (To) = 0 then
               Backward_Char (To, Success);
            end if;

            --  Do not consider a line selected if only the last character is
            --  selected.

            if Ends_Line (From) then
               Forward_Char (From, Success);
            end if;

            --  Get editable lines

            From_Line :=
              Get_Editable_Line
                (Buffer, Buffer_Line_Type (Get_Line (From) + 1));

            To_Line :=
              Get_Editable_Line
                (Buffer, Buffer_Line_Type (Get_Line (To) + 1));
         end if;
      end Find_Paragraph_Bounds;

      --  Local variables

      From_Line : Editable_Line_Type;
      To_Line   : Editable_Line_Type;

   --  Start of processing for Do_Refill

   begin
      if not Buffer.Writable then
         End_Action (Buffer);
         return False;
      end if;

      declare
         G : Group_Block := New_Group (Buffer.Queue);
      begin
         Setup_Comment_Regexps;
         Find_Paragraph_Bounds (From_Line, To_Line);

         if Single_Line_BC_Pattern /= null
           or else Multiple_Lines_BC_Pattern /= null
         then
            --  We have a known syntax for comments
            Refill_Comments (From_Line, To_Line);
         else
            Refill_Plain_Text (From_Line, To_Line);
         end if;

         --  Free allocated memory

         if Single_Line_BC_Pattern /= null then
            Unchecked_Free (Single_Line_BC_Pattern);
         end if;

         if Multiple_Lines_BC_Pattern /= null then
            Unchecked_Free (Multiple_Lines_BC_Pattern);
            Unchecked_Free (Multiple_Lines_EC_Pattern);
         end if;

         End_Action (Buffer);
      end;

      return True;
   end Do_Refill;

   ------------------------------------
   -- Find_Current_Comment_Paragraph --
   ------------------------------------

   procedure Find_Current_Comment_Paragraph
     (Buffer : not null access Source_Buffer_Record;
      Line   : Editable_Line_Type;
      Start_Line, End_Line : out Editable_Line_Type)
   is
      Lang_Context : constant Language_Context_Access :=
        (if Buffer.Lang /= null
         then Get_Language_Context (Buffer.Lang)
         else null);
      Single_Line_BC : constant GNAT.Strings.String_Access :=
        (if Lang_Context /= null
         then Lang_Context.Syntax.New_Line_Comment_Start
         else null);
      Non_Empty_Comment_Re : Pattern_Matcher_Access;

      Is_Empty_Re : Pattern_Matcher_Access;
      Comment_Start_End_Re : Pattern_Matcher_Access;

      function Is_Comment_Line (Line : Editable_Line_Type) return Boolean;
      --  Whether Pos is anywhere on a comment line (not necessarily within the
      --  comment itself).

      function Is_Boundary (Line : Editable_Line_Type) return Boolean;
      --  Whether Line is a boundary for a pagraph

      ---------------------
      -- Is_Comment_Line --
      ---------------------

      function Is_Comment_Line (Line : Editable_Line_Type) return Boolean is
         L : Src_String;
      begin
         if Non_Empty_Comment_Re = null then
            return False;
         end if;

         L := Get_String_At_Line (Source_Buffer (Buffer), Line);
         return L.Contents /= null and then Match
           (Non_Empty_Comment_Re.all, L.Contents (1 .. L.Length));
      end Is_Comment_Line;

      -----------------
      -- Is_Boundary --
      -----------------

      function Is_Boundary (Line : Editable_Line_Type) return Boolean is
         L : constant Src_String :=
           Get_String_At_Line (Source_Buffer (Buffer), Line);
      begin
         return L.Contents = null
           or else Match (Is_Empty_Re.all, L.Contents (1 .. L.Length))
           or else
             (Non_Empty_Comment_Re /= null
              and then Match  --  in a single line comment
                (Non_Empty_Comment_Re.all, L.Contents (1 .. L.Length)))
           or else    --  boundary of comment block
             (Comment_Start_End_Re /= null
              and then Match (Comment_Start_End_Re.all,
                              L.Contents (1 .. L.Length)));
      end Is_Boundary;

   begin
      if Single_Line_BC /= null then
         Non_Empty_Comment_Re := new Pattern_Matcher'
           (Compile ("^\s*" & Single_Line_BC.all & "\s*\S"));
      end if;

      Start_Line := Line;
      End_Line  := Line;

      if not Is_Comment_Line (Line) then
         --  If we are not in a single line comment, we should simply search
         --  for empty lines before and after to find the bounds of the
         --  paragraph. We should also stop when we find a line that marks the
         --  beginning of end of a comment.

         Is_Empty_Re := new Pattern_Matcher'(Compile ("^\s*$"));

         if Lang_Context /= null
           and then Lang_Context.Syntax.Comment_Start /= null
         then
            Comment_Start_End_Re := new Pattern_Matcher'
              (Compile ("(" & Quote (Lang_Context.Syntax.Comment_Start.all)
               & "|" & Quote (Lang_Context.Syntax.Comment_End.all) & ")"));
         end if;

         while Start_Line > 1
           and then not Is_Boundary (Start_Line - 1)
         loop
            Start_Line := Start_Line - 1;
         end loop;

         while End_Line < Editable_Line_Type (Buffer.Get_Line_Count)
           and then not Is_Boundary (End_Line + 1)
         loop
            End_Line := End_Line + 1;
         end loop;

      else
         while Start_Line > 1
           and then Is_Comment_Line (Start_Line - 1)
         loop
            Start_Line := Start_Line - 1;
         end loop;

         while End_Line < Editable_Line_Type (Buffer.Get_Line_Count)
           and then Is_Comment_Line (End_Line + 1)
         loop
            End_Line := End_Line + 1;
         end loop;
      end if;

      Trace (Me, "Bounds" & Start_Line'Img & End_Line'Img
             & " started from" & Line'Img);

      Unchecked_Free (Non_Empty_Comment_Re);
      Unchecked_Free (Is_Empty_Re);
      Unchecked_Free (Comment_Start_End_Re);
   end Find_Current_Comment_Paragraph;

   ---------------
   -- Is_Editor --
   ---------------

   function Is_Editor (Ctxt : Selection_Context) return Boolean
   is
   begin
      --  Do not check the current focus widget ourselves. Instead, we know
      --  it has been properly checked when the context was created, and we
      --  just check the current module from there.
      if Get_Kernel (Ctxt).Get_Contextual_Menu_Open then
         return False;
      else
         declare
            Creator : constant Module_ID := Module_ID (Get_Creator (Ctxt));
         begin
            return Creator = Src_Editor_Module_Id;
         end;
      end if;
   end Is_Editor;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Context : access Src_Editor_Action_Context;
      Ctxt    : Selection_Context) return Boolean
   is
      pragma Unreferenced (Context);
   begin
      return Is_Editor (Ctxt);
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matched_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Context : access Completion_Context;
      Ctxt    : GPS.Kernel.Selection_Context) return Boolean
   is
      pragma Unreferenced (Ctxt);
   begin
      --  Disable Move to next/previous line when in completion
      return not (Context.Is_Line_Movement
        and then Completion_Module.In_Smart_Completion);
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matched_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Context : access Signature_Context;
      Ctxt    : GPS.Kernel.Selection_Context) return Boolean
   is
      pragma Unreferenced (Context);
   begin
      --  Disable Move to next/previous line when in Signature Help window
      return not Get_Kernel (Ctxt).In_Signature_Help;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Context : access Writable_Src_Editor_Action_Context;
      Ctxt    : GPS.Kernel.Selection_Context) return Boolean
   is
      pragma Unreferenced (Context);
      Box : Source_Editor_Box;
   begin
      if not Is_Editor (Ctxt)
        or else Completion_Module.In_Smart_Completion
      then
         return False;
      end if;

      Box := Get_Source_Box_From_MDI (Find_Current_Editor (Get_Kernel (Ctxt)));

      return Box /= null and then Box.Get_Buffer.Get_Writable;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Context : access Last_Editor_Action_Context;
      Ctxt    : GPS.Kernel.Selection_Context) return Boolean
   is
      pragma Unreferenced (Context);
      Editor : constant MDI_Child := Find_Current_Editor (Get_Kernel (Ctxt));
   begin
      return Editor /= null;
   end Filter_Matches_Primitive;

   ----------
   -- Free --
   ----------

   procedure Free
     (Buffer        : access Source_Buffer_Record;
      X             : in out Line_Info_Width;
      Free_Messages : Boolean) is
   begin
      Free (X.Action);

      if Free_Messages then
         for Reference of X.Messages loop
            Remove_Message (Buffer, Reference);
         end loop;
         X.Messages.Clear;
      end if;
   end Free;

   ----------------------
   -- Forward_Position --
   ----------------------

   procedure Forward_Position
     (Buffer       : access Source_Buffer_Record;
      Start_Line   : Editable_Line_Type;
      Start_Column : Character_Offset_Type;
      Length       : Integer;
      End_Line     : out Editable_Line_Type;
      End_Column   : out Character_Offset_Type)
   is
      Iter    : Gtk_Text_Iter;
      Success : Boolean;

      Amount  : Integer := Length;

      Found   : Boolean;
      Forward : constant Boolean := Length > 0;
   begin
      End_Line   := Start_Line;
      End_Column := Start_Column;

      if Length < 0 then
         Amount := -Length;
      end if;

      Is_Valid_Pos (Buffer => Source_Buffer (Buffer),
                    Iter   => Iter,
                    Found  => Found,
                    Line   => Gint (Get_Buffer_Line (Buffer, Start_Line) - 1),
                    Column => Gint (Start_Column - 1));

      if not Found then
         return;
      end if;

      for J in 1 .. Amount loop
         if Forward then
            Forward_Char (Iter, Success);
         else
            Backward_Char (Iter, Success);
         end if;

         exit when not Success
           or else Buffer_Line_Type (Get_Line (Iter) + 1) not in
             Buffer.Line_Data'Range;

         while Buffer.Line_Data
           (Buffer_Line_Type (Get_Line (Iter) + 1)).Editable_Line = 0
         loop
            --  ??? Could be optimized by moving line by line
            if Forward then
               Forward_Char (Iter, Success);
            else
               Backward_Char (Iter, Success);
            end if;

            exit when not Success
              or else Buffer_Line_Type (Get_Line (Iter) + 1) not in
                Buffer.Line_Data'Range;
         end loop;
      end loop;

      if Buffer_Line_Type (Get_Line (Iter) + 1) not in
        Buffer.Line_Data'Range
      then
         return;
      end if;

      End_Line := Get_Editable_Line
        (Buffer, Buffer_Line_Type (Get_Line (Iter) + 1));

      End_Column := Character_Offset_Type (Get_Line_Offset (Iter) + 1);
   end Forward_Position;

   --------------
   -- Get_Text --
   --------------

   function Get_Text
     (Buffer               : access Source_Buffer_Record;
      Start_Line           : Editable_Line_Type;
      Start_Column         : Character_Offset_Type;
      End_Line             : Editable_Line_Type := 0;
      End_Column           : Character_Offset_Type := 0;
      Include_Hidden_Chars : Boolean := True;
      Include_Last         : Boolean := False)
      return Unbounded_String
   is
      Text_Access : GNAT.Strings.String_Access :=
        Get_Text
           (Buffer               => Buffer,
            Start_Line           => Start_Line,
            Start_Column         => Start_Column,
            End_Line             => End_Line,
            End_Column           => End_Column,
            Include_Hidden_Chars => Include_Hidden_Chars,
            Include_Last         => Include_Last);
   begin
      return To_Unbounded_String (Text_Access);
   end Get_Text;

   --------------
   -- Get_Text --
   --------------

   function Get_Text
     (Buffer               : access Source_Buffer_Record;
      Start_Line           : Editable_Line_Type;
      Start_Column         : Character_Offset_Type;
      End_Line             : Editable_Line_Type := 0;
      End_Column           : Character_Offset_Type := 0;
      Include_Hidden_Chars : Boolean := True;
      Include_Last         : Boolean := False)
      return VSS.Strings.Virtual_String
   is
      Text_Access : GNAT.Strings.String_Access :=
        Get_Text
           (Buffer               => Buffer,
            Start_Line           => Start_Line,
            Start_Column         => Start_Column,
            End_Line             => End_Line,
            End_Column           => End_Column,
            Include_Hidden_Chars => Include_Hidden_Chars,
            Include_Last         => Include_Last);
   begin
      return Result : constant VSS.Strings.Virtual_String :=
        VSS.Strings.Conversions.To_Virtual_String (Text_Access.all)
      do
         GNAT.Strings.Free (Text_Access);
      end return;
   end Get_Text;

   -------------------------
   -- Refresh_Side_Column --
   -------------------------

   procedure Refresh_Side_Column (Buffer : access Source_Buffer_Record) is
   begin
      Recalculate_Side_Column_Width (Buffer);
      Side_Column_Configuration_Changed (Buffer);
      Side_Column_Changed (Buffer);
   end Refresh_Side_Column;

   --------------------
   -- In_Destruction --
   --------------------

   function In_Destruction
     (Buffer : access Source_Buffer_Record'Class) return Boolean is
   begin
      return Buffer.In_Destruction;
   end In_Destruction;

   -----------------------
   -- Get_Command_Queue --
   -----------------------

   function Get_Command_Queue
     (Buffer : access Source_Buffer_Record'Class) return Command_Queue is
   begin
      return Buffer.Queue;
   end Get_Command_Queue;

   -------------
   -- Convert --
   -------------

   function Convert (L : Natural) return Editable_Line_Type is
   begin
      return Editable_Line_Type (L);
   end Convert;

   function Convert (L : Editable_Line_Type) return Natural is
   begin
      return Natural (L);
   end Convert;

   -----------------
   -- Expand_Tabs --
   -----------------

   function Expand_Tabs
     (Buffer : access Source_Buffer_Record;
      Line   : Editable_Line_Type;
      Column : Character_Offset_Type) return Visible_Column_Type
   is
      Iter    : Gtk_Text_Iter;
      Count   : Character_Offset_Type := 1;
      Current : Visible_Column_Type := 1;
      Result  : Boolean := True;
      Tab_Len : constant Visible_Column_Type :=
                  Visible_Column_Type (Buffer.Tab_Width);
   begin
      if Buffer.Editable_Lines = null
        or else Line not in 1 .. Buffer.Last_Editable_Line
      then
         return Current;
      end if;

      Get_Iter_At_Line
        (Buffer, Iter,
         Gint (Buffer.Editable_Lines (Line) - 1));

      while Result and then Count < Column loop
         if Get_Char (Iter) = ASCII.HT then
            Current := Current + (Tab_Len - (Current - 1) mod Tab_Len);
         else
            Current := Current + 1;
         end if;

         Count := Count + 1;
         Forward_Char (Iter, Result);
      end loop;

      return Current;
   end Expand_Tabs;

   -------------------
   -- Collapse_Tabs --
   -------------------

   function Collapse_Tabs
     (Buffer : access Source_Buffer_Record;
      Line   : Editable_Line_Type;
      Column : Visible_Column_Type) return Character_Offset_Type
   is
      Iter        : Gtk_Text_Iter;
      Current     : Visible_Column_Type := 1;
      Count       : Character_Offset_Type := 1;
      Result      : Boolean := True;
      Tab_Len     : constant Visible_Column_Type :=
                      Visible_Column_Type (Buffer.Tab_Width);
      Buffer_Line : constant Buffer_Line_Type := Buffer.Get_Buffer_Line
        (Line);
   begin
      if Column = 0 or else Line not in 1 .. Buffer.Last_Editable_Line then
         return 0;
      end if;

      if Buffer_Line /= 0 then
         Get_Iter_At_Line
           (Buffer, Iter,
            Gint (Buffer_Line - 1));

         while Result and then Current < Column  loop
            if Get_Char (Iter) = ASCII.HT then
               Current := Current + Tab_Len - (Current - 1) mod Tab_Len;
            else
               Current := Current + 1;
            end if;

            Count := Count + 1;
            Forward_Char (Iter, Result);
         end loop;
      end if;

      return Count;
   end Collapse_Tabs;

   --------------
   -- Get_Tree --
   --------------

   function Get_Tree
     (Buffer : access Source_Buffer_Record) return Semantic_Tree'Class is
   begin
      return Buffer.Kernel.Get_Abstract_Tree_For_File
        ("EDIT", Buffer.Filename);
   end Get_Tree;

   ----------------------
   -- Blocks_Are_Exact --
   ----------------------

   function Blocks_Are_Exact
     (Buffer : access Source_Buffer_Record) return Boolean is
   begin
      return Buffer.Blocks_Exact;
   end Blocks_Are_Exact;

   --------------------------
   -- Mark_Buffer_Writable --
   --------------------------

   procedure Mark_Buffer_Writable
     (Buffer   : not null access Source_Buffer_Record;
      Writable : Boolean)
   is
      Views : constant Views_Array := Get_Views (Source_Buffer (Buffer));
   begin
      Buffer.Writable := Writable;

      if Writable then
         Add_Controls (Buffer);
      else
         Remove_Controls (Buffer);
      end if;

      for V in Views'Range loop
         Set_Editable (Gtk_Text_View (Views (V).Get_Source_View), Writable);
         Update_Status (Views (V).Get_Status_Bar);

         --  Changing the class does not take into account the CSS
         --  background-color, for some reason, although it does take other
         --  attributes like "color" into account.
         Views (V).Get_Source_View.Set_Background_Color;
      end loop;
   end Mark_Buffer_Writable;

   ------------------
   -- Get_Writable --
   ------------------

   function Get_Writable
     (Buffer : not null access Source_Buffer_Record) return Boolean is
   begin
      return Buffer.Writable;
   end Get_Writable;

   --------------------------
   -- Prevent_CR_Insertion --
   --------------------------

   procedure Prevent_CR_Insertion
     (Buffer  : access Source_Buffer_Record'Class;
      Prevent : Boolean := True) is
   begin
      Buffer.Prevent_CR_Insertion := Prevent;
   end Prevent_CR_Insertion;

   -----------------------
   -- Set_In_Completion --
   -----------------------

   procedure Set_In_Completion
     (Buffer        : Source_Buffer;
      In_Completion : Boolean) is
   begin
      Buffer.In_Completion := In_Completion;
   end Set_In_Completion;

   -------------------
   -- In_Completion --
   -------------------

   function In_Completion (Buffer : Source_Buffer) return Boolean is
   begin
      return Buffer.In_Completion;
   end In_Completion;

   -----------------
   -- Buffer_List --
   -----------------

   function Buffer_List
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Source_Buffer_Array
   is
      Iter        : Child_Iterator;
      Child_Count : Natural := 0;
   begin
      if Get_MDI (Kernel) = null then
         return (1 .. 0 => <>);
      end if;

      Iter := First_Child (Get_MDI (Kernel));
      while Get (Iter) /= null loop
         Child_Count := Child_Count + 1;
         Next (Iter);
      end loop;

      declare
         Buffers : Source_Buffer_Array (1 .. Child_Count);
         Buffer  : Source_Buffer;
         Index   : Integer := Buffers'First - 1;
         Found   : Boolean;
      begin
         Iter := First_Child (Get_MDI (Kernel));
         while Get (Iter) /= null loop
            if Is_Source_Box (Get (Iter)) then
               Buffer := Get_Buffer (Get_Source_Box_From_MDI (Get (Iter)));
               Found := False;
               for J in Buffers'First .. Index loop
                  if Buffers (J) = Buffer then
                     Found := True;
                     exit;
                  end if;
               end loop;

               if not Found then
                  Index := Index + 1;
                  Buffers (Index) := Buffer;
               end if;
            end if;
            Next (Iter);
         end loop;

         return Buffers (Buffers'First .. Index);
      end;
   end Buffer_List;

   --------------------
   -- Add_Typed_Char --
   --------------------

   procedure Add_Typed_Char
     (Buffer : access Source_Buffer_Record'Class;
      C      : Gunichar) is
   begin
      if Buffer.Typed_Char_Index < Buffer.Typed_Chars'Last then
         Buffer.Typed_Char_Index := Buffer.Typed_Char_Index + 1;
         Buffer.Typed_Chars (Buffer.Typed_Char_Index) := C;
      end if;
   end Add_Typed_Char;

   ----------------------------
   -- Delete_Last_Typed_Char --
   ----------------------------

   procedure Delete_Last_Typed_Char
     (Buffer : access Source_Buffer_Record'Class) is
   begin
      if Buffer.Typed_Char_Index >= Buffer.Typed_Chars'First then
         Buffer.Typed_Char_Index := Buffer.Typed_Char_Index - 1;
      end if;
   end Delete_Last_Typed_Char;

   -----------------------
   -- Clear_Typed_Chars --
   -----------------------

   procedure Clear_Typed_Chars (Buffer : access Source_Buffer_Record'Class) is
   begin
      Buffer.Typed_Char_Index := 0;
   end Clear_Typed_Chars;

   ---------------------
   -- Get_Typed_Chars --
   ---------------------

   function Get_Typed_Chars
     (Buffer : access Source_Buffer_Record'Class;
      N      : Positive) return Basic_Types.UTF8_String is
   begin
      if N > Buffer.Typed_Char_Index then
         --  No enough characters in buffer, we cannot support conservative
         --  casing.
         return "";

      else
         declare
            S    : Basic_Types.UTF8_String (1 .. N * 6);
            --  An UTF-8 character expand of max 6 bytes
            Last : Natural := 0;
         begin
            for K in Buffer.Typed_Char_Index - N + 1
              .. Buffer.Typed_Char_Index
            loop
               Unichar_To_UTF8
                 (Buffer.Typed_Chars (K), S (Last + 1 .. S'Last), Last);
            end loop;
            return S (1 .. Last);
         end;
      end if;
   end Get_Typed_Chars;

   -------------------
   -- Get_Version --
   -------------------

   function Get_Version
     (Buffer : access Source_Buffer_Record'Class) return Integer is
   begin
      return Buffer.Version;
   end Get_Version;

   ------------------------------
   -- Update_Logical_Timestamp --
   ------------------------------

   procedure Update_Logical_Timestamp
     (Buffer : access Source_Buffer_Record'Class) is
   begin
      if Buffer.Version = Integer'Last then
         Buffer.Version := 0;
      else
         Buffer.Version := Buffer.Version + 1;
      end if;
   end Update_Logical_Timestamp;

   ------------------------------
   -- Update_All_Column_Memory --
   ------------------------------

   procedure Update_All_Column_Memory
     (Buffer : access Source_Buffer_Record'Class)
   is
      Iter : Gtk_Text_Iter;
   begin

      Buffer.Get_Iter_At_Mark (Iter, Buffer.Insert_Mark);
      Buffer.Cursor_Column_Memory := Get_Line_Offset (Iter);

      for Cursor of Buffer.Slave_Cursors_List loop
         Buffer.Get_Iter_At_Mark (Iter, Cursor.Mark);
         Cursor.Column_Memory := Get_Line_Offset (Iter);
      end loop;

   end Update_All_Column_Memory;
   pragma Unreferenced (Update_All_Column_Memory);

   ----------------------------------
   -- Update_Use_Highlighting_Hook --
   ----------------------------------

   procedure Update_Use_Highlighting_Hook
     (Self : access Source_Highlighter_Record)
   is
      Prev : constant Boolean := Self.Use_Highlighting_Hook;
   begin
      Self.Use_Highlighting_Hook :=
        Self.Buffer.Lang /= null
          and then Self.Buffer.Lang.Get_Name = "Ada"
          and then Use_External_Highlighting.Get_Pref /= None;

      if Prev /= Self.Use_Highlighting_Hook
        and then Prev
      then
         --  We don't use hook anymore, so should clear old highlighting
         --  styles prodused by code connected to highlighting hook

         Self.Call_Clear_Highlighting := True;
      end if;
   end Update_Use_Highlighting_Hook;

   ---------------
   -- Inserting --
   ---------------

   function Inserting
     (Buffer : access Source_Buffer_Record'Class) return Boolean is
   begin
      return Buffer.Inserting_Count >= 1;
   end Inserting;

   -----------------------------
   -- Is_Inserting_Internally --
   -----------------------------

   function Is_Inserting_Internally
     (Buffer  : access Source_Buffer_Record) return Boolean
   is (Buffer.Inserting);

   ---------------------
   -- Start_Inserting --
   ---------------------

   procedure Start_Inserting
     (Buffer : access Source_Buffer_Record'Class) is
   begin
      Buffer.Inserting_Count := Buffer.Inserting_Count + 1;
   end Start_Inserting;

   -------------------
   -- End_Inserting --
   -------------------

   procedure End_Inserting
     (Buffer : access Source_Buffer_Record'Class) is
   begin
      Buffer.Inserting_Count := Buffer.Inserting_Count - 1;
   end End_Inserting;

   ----------------------
   -- Start_Undo_Group --
   ----------------------

   procedure Start_Undo_Group (Buffer : access Source_Buffer_Record'Class) is
   begin
      End_Action (Buffer);
      Start_Group (Buffer.Queue);
   end Start_Undo_Group;

   -----------------------
   -- Finish_Undo_Group --
   -----------------------

   procedure Finish_Undo_Group (Buffer : access Source_Buffer_Record'Class) is
   begin
      End_Action (Buffer);
      End_Group (Buffer.Queue);
   end Finish_Undo_Group;

   ------------------------
   -- Current_Undo_Group --
   ------------------------

   function Current_Undo_Group
     (Buffer : access Source_Buffer_Record'Class) return Group_Block is
   begin
      End_Action (Buffer);
      return Current_Group (Buffer.Queue);
   end Current_Undo_Group;

   --------------------
   -- New_Undo_Group --
   --------------------

   function New_Undo_Group
     (Buffer : access Source_Buffer_Record'Class) return Group_Block is
   begin
      End_Action (Buffer);
      return New_Group (Buffer.Queue);
   end New_Undo_Group;

   --------------------------
   -- Add_Listener_Factory --
   --------------------------

   procedure Add_Listener_Factory
     (Factory : Editor_Listener_Factory_Access) is
   begin
      Listener_Factories.Append (Factory);
   end Add_Listener_Factory;

   --------------------------
   -- Set_Folding_Provider --
   --------------------------

   procedure Set_Folding_Provider
     (Provider : Editor_Folding_Provider_Access) is
   begin
      Folding_Provider := Provider;
   end Set_Folding_Provider;

   -----------------------
   -- Get_Editor_Buffer --
   -----------------------

   function Get_Editor_Buffer
     (Buffer : access Source_Buffer_Record'Class) return Editor_Buffer_Access
   is
   begin
      return Buffer.Editor_Buffer;
   end Get_Editor_Buffer;

   --------------------------------------
   -- Get_Global_Editor_Buffer_Factory --
   --------------------------------------

   function Get_Global_Editor_Buffer_Factory
     return access GPS.Editors.Editor_Buffer_Factory'Class is
   begin
      return Editors_Factory;
   end Get_Global_Editor_Buffer_Factory;

   ---------------
   -- Set_Title --
   ---------------

   procedure Set_Title
      (Buffer : not null access Source_Buffer_Record;
       Title  : String)
   is
   begin
      GNAT.Strings.Free (Buffer.Forced_Title);
      Buffer.Forced_Title := new String'(Title);
   end Set_Title;

   ---------------
   -- Get_Title --
   ---------------

   function Get_Title
      (Buffer : not null access Source_Buffer_Record) return String is
   begin
      if Buffer.Forced_Title /= null then
         return Buffer.Forced_Title.all;
      else
         return "";
      end if;
   end Get_Title;

   --------------------
   -- Freeze_Context --
   --------------------

   procedure Freeze_Context
     (Self : not null access Source_Buffer_Record'Class) is
   begin
      Self.Context_Frozen := Self.Context_Frozen + 1;
   end Freeze_Context;

   ------------------
   -- Thaw_Context --
   ------------------

   procedure Thaw_Context
     (Self : not null access Source_Buffer_Record'Class) is
   begin
      Self.Context_Frozen := Self.Context_Frozen - 1;
      Assert (Me, Self.Context_Frozen >= 0,
              "Calls to Thaw_Context doesn't match Freeze_Context");
   end Thaw_Context;

   -----------------------
   -- Context_Is_Frozen --
   -----------------------

   function Context_Is_Frozen
     (Self  : not null access Source_Buffer_Record'Class)
      return Boolean
     is (Self.Context_Frozen > 0);

   -------------------------------
   -- Source_Highlighter_Record --
   -------------------------------

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Self : in out Source_Highlighter_Record)
   is
      Tags : Gtk_Text_Tag_Table;
   begin
      --  Save the newly created highlighting tags into the source buffer
      --  tag table.

      Tags := Get_Tag_Table (Self.Buffer);

      for Entity_Kind in Standout_Language_Entity'Range loop
         Self.Syntax_Tags (Entity_Kind) := Get_Tag
           (Language_Styles (Entity_Kind));
         Text_Tag_Table.Add (Tags, Self.Syntax_Tags (Entity_Kind));
      end loop;

      --  Create Delimiter_Tag and save it into the source buffer tag table
      Self.Delimiter_Tag := Get_Tag (Editor_Ephemeral_Highlighting_Simple);

      Add (Tags, Self.Delimiter_Tag);
      Add (Tags, Self.Buffer.Hyper_Mode_Tag);
      Add (Tags, Self.Buffer.Non_Editable_Tag);
      Add (Tags, Self.Buffer.Hidden_Text_Tag);
   end Initialize;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Source_Highlighter_Record) is
   begin
      Unref (Self.Syntax_Tags);
      if Self.Delimiter_Tag /= null then
         Unref (Self.Delimiter_Tag);
      end if;
   end Finalize;

   -------------------------
   -- Enable_Highlighting --
   -------------------------

   procedure Enable_Highlighting
     (Self : access Source_Highlighter_Record) is
   begin
      Self.Auto_Highlight_Enabled := True;
   end Enable_Highlighting;

   --------------------------
   -- Disable_Highlighting --
   --------------------------

   procedure Disable_Highlighting
     (Self : access Source_Highlighter_Record) is
   begin
      Self.Auto_Highlight_Enabled := False;
   end Disable_Highlighting;

   ---------------------------
   -- Set_Line_Highlighting --
   ---------------------------

   procedure Set_Line_Highlighting
     (Self         : access Source_Highlighter_Record;
      Line         : Buffer_Line_Type;
      Style        : not null Style_Access;
      Set          : Boolean;
      Highlight_In : Highlight_Location_Array)
   is
      procedure Set_Highlighting
        (Data     : in out Highlighting_Data_Record;
         Category : Natural;
         Enabled  : Boolean);
      --  Sets highligting state and recompute active highlighting

      ----------------------
      -- Set_Highlighting --
      ----------------------

      procedure Set_Highlighting
        (Data     : in out Highlighting_Data_Record;
         Category : Natural;
         Enabled  : Boolean)
      is
         Last_Index : constant Natural := Get_Last_Index;

      begin
         --  Reallocate data when necessary

         if Data.Enabled = null then
            --  If we are removing a highlight where no highlight is defined,
            --  we can exit immediately.

            if not Set then
               return;
            end if;

            Data.Enabled := new Boolean_Array (1 .. Last_Index);
            Data.Enabled.all := (others => False);

         elsif Data.Enabled'Last < Last_Index then
            declare
               Aux : Boolean_Array_Access;

            begin
               Aux := new Boolean_Array (1 .. Last_Index);
               Aux (1 .. Data.Enabled'Last) := Data.Enabled.all;
               Aux (Data.Enabled'Last + 1 .. Last_Index) := (others => False);
               Unchecked_Free (Data.Enabled);
               Data.Enabled := Aux;
            end;
         end if;

         --  Set new state of highlighting

         Data.Enabled (Category) := Enabled;

         --  Find out which category has priority for highlighting

         for J in Data.Enabled'Range loop
            if Data.Enabled (J) then
               Data.Active := J;

               return;
            end if;
         end loop;

         --  If we reach this stage, no highlighting was found

         Data.Active := 0;
      end Set_Highlighting;

      Category : Natural;

   begin
      if Line = 0 then
         return;
      end if;

      Category := Lookup_Category (Style);

      if Category = 0 then
         Trace (Me, "Set_Line_Highlight Id=" & Get_Name (Style)
                & " couldn't identify category, nothing done");
         --  Could not identify highlighting category
         return;
      end if;

      if Line < Self.Buffer.Line_Data'First
        or else Line > Self.Buffer.Line_Data'Last
      then
         Trace (Me, "Wrong line number: " & Image (Integer (Line)));
         return;
      end if;

      for K in Highlight_Location loop
         if Highlight_In (K) then
            Set_Highlighting
              (Self.Buffer.Line_Data (Line).Highlighting (K), Category, Set);
         end if;
      end loop;
   end Set_Line_Highlighting;

   ---------------------------
   -- Add_Line_Highlighting --
   ---------------------------

   procedure Add_Line_Highlighting
     (Self         : access Source_Highlighter_Record;
      Line         : Editable_Line_Type;
      Style        : not null Style_Access;
      Highlight_In : Highlight_Location_Array)
   is
      The_Line : Buffer_Line_Type;
   begin
      if Line = 0 then
         for J in Self.Buffer.Line_Data'Range loop
            Self.Set_Line_Highlighting (J, Style, True, Highlight_In);
         end loop;
      else
         The_Line := Get_Buffer_Line (Self.Buffer, Line);

         if The_Line /= 0 then
            Self.Set_Line_Highlighting (The_Line, Style, True, Highlight_In);
         end if;
      end if;

      Line_Highlights_Changed (Self.Buffer);
   end Add_Line_Highlighting;

   ---------------------
   -- Highlight_Range --
   ---------------------

   procedure Highlight_Range
     (Self      : access Source_Highlighter_Record;
      Style     : Style_Access;
      Line      : Editable_Line_Type;
      Start_Col : Visible_Column_Type;
      End_Col   : Visible_Column_Type;
      Remove    : Boolean := False)
   is
      Start_Iter, End_Iter : Gtk_Text_Iter;
      Result               : Boolean;
      The_Line             : Gint;
   begin
      --  Here we test whether the buffer is in destruction. If it is the case
      --  we simply return since it is not worth taking care of unhighlighting
      --  lines. Furthermore this prevents GNAT Studio from crashing when
      --  we close a source file used in a visual diff while the reference
      --  file is still being displayed.

      if Self.Buffer.In_Destruction then
         return;
      end if;

      --  Get the boundaries of text to (un)highlight

      if Line = 0 then
         Get_Bounds (Self.Buffer, Start_Iter, End_Iter);

      else
         The_Line :=
           Gint (Get_Buffer_Line (Self.Buffer, Line) - 1);

         if The_Line < 0 then
            return;
         end if;

         if Start_Col <= 0
           or else not Is_Valid_Position (Self.Buffer, Line, Start_Col)
         then
            Get_Iter_At_Line (Self.Buffer, Start_Iter, The_Line);
         else
            Get_Iter_At_Screen_Position
              (Self.Buffer, Start_Iter, Line, Start_Col);

            if Ends_Line (Start_Iter) then
               Backward_Char (Start_Iter, Result);
            end if;
         end if;

         if End_Col <= 0
           or else not Is_Valid_Position (Self.Buffer, Line, End_Col)
         then
            Copy (Start_Iter, End_Iter);
            Forward_To_Line_End (End_Iter, Result);
         else
            Get_Iter_At_Screen_Position
              (Self.Buffer, End_Iter, Line, End_Col);
         end if;
      end if;

      Self.Highlight_Range (Style      => Style,
                            Line       => Line,
                            Start_Iter => Start_Iter,
                            End_Iter   => End_Iter,
                            Remove     => Remove);
   end Highlight_Range;

   ---------------------
   -- Highlight_Slice --
   ---------------------

   procedure Highlight_Slice
     (Self       : access Source_Highlighter_Record;
      Style      : Style_Access;
      Start_Line : Editable_Line_Type;
      Start_Col  : Visible_Column_Type;
      End_Line   : Editable_Line_Type;
      End_Col    : Visible_Column_Type;
      Remove     : Boolean := False)
   is
      Start_Iter, End_Iter : Gtk_Text_Iter;
   begin
      Get_Iter_At_Screen_Position
        (Self.Buffer, Start_Iter, Start_Line, Start_Col);
      Get_Iter_At_Screen_Position
        (Self.Buffer, End_Iter, End_Line, End_Col);

      Highlight_Slice (Self, Style, Start_Iter, End_Iter, Remove);
   end Highlight_Slice;

   ---------------------
   -- Highlight_Slice --
   ---------------------

   procedure Highlight_Slice
     (Self       : access Source_Highlighter_Record;
      Style      : Style_Access;
      Start_Iter : Gtk.Text_Iter.Gtk_Text_Iter;
      End_Iter   : Gtk.Text_Iter.Gtk_Text_Iter;
      Remove     : Boolean := False)
   is
      Tag : Gtk_Text_Tag;
   begin
      --  Get the text tag, create it if necessary
      Tag := Lookup (Get_Tag_Table (Self.Buffer), Get_Name (Style));

      if Tag = null then
         if Remove then
            return;
         else
            --  Create the tag from the style
            Tag := Get_Tag (Style);
            Add (Get_Tag_Table (Self.Buffer), Tag);
         end if;
      end if;

      declare
         Start_Line : constant Gint := Get_Line (Start_Iter);
         End_Line   : constant Gint := Get_Line (End_Iter);
      begin

         --  Highlight/Unhighlight the text
         if Remove then
            Remove_Tag (Self.Buffer, Tag, Start_Iter, End_Iter);
         else
            Apply_Tag (Self.Buffer, Tag, Start_Iter, End_Iter);
         end if;

         if Get_In_Speedbar (Style) then
            for Line in Start_Line .. End_Line loop
               if Remove then
                  Self.Remove_Line_Highlighting
                    (Editable_Line_Type (Line), Style);
               else
                  Self.Add_Line_Highlighting
                    (Editable_Line_Type (Line), Style,
                     Highlight_In => (Highlight_Speedbar => True,
                                      others             => False));
               end if;
            end loop;
         end if;
      end;
   end Highlight_Slice;

   ---------------------
   -- Highlight_Range --
   ---------------------

   procedure Highlight_Range
     (Self       : access Source_Highlighter_Record;
      Style      : Style_Access;
      Line       : Editable_Line_Type;
      Start_Iter : Gtk_Text_Iter;
      End_Iter   : Gtk_Text_Iter;
      Remove     : Boolean := False)
   is
      Tag : Gtk_Text_Tag;
   begin
      --  Get the text tag, create it if necessary

      Tag := Lookup (Get_Tag_Table (Self.Buffer), Get_Name (Style));

      if Tag = null then
         if Remove then
            return;
         else
            --  Create the tag from the style
            Tag := Get_Tag (Style);
            Add (Get_Tag_Table (Self.Buffer), Tag);
         end if;
      end if;

      --  Highlight/Unhighlight the text

      if Remove then
         Remove_Tag (Self.Buffer, Tag, Start_Iter, End_Iter);
      else
         Apply_Tag (Self.Buffer, Tag, Start_Iter, End_Iter);
      end if;

      if Line /= 0 then
         if Get_In_Speedbar (Style) then
            if Remove then
               Self.Remove_Line_Highlighting (Line, Style);
            else
               Self.Add_Line_Highlighting
                 (Line, Style,
                  Highlight_In => (Highlight_Speedbar => True,
                                   others             => False));
            end if;
         end if;
      end if;
   end Highlight_Range;

   ---------------------
   -- Highlight_Slice --
   ---------------------

   procedure Highlight_Slice
     (Self       : access Source_Highlighter_Record;
      Start_Iter : Gtk.Text_Iter.Gtk_Text_Iter;
      End_Iter   : Gtk.Text_Iter.Gtk_Text_Iter)
   is
      procedure Is_Valid_Index is new
        Generic_Valid_Position (Get_Bytes_In_Line, Set_Line_Index);
      --  Column should be given in bytes, not characters

      function Highlight_Cb
        (Entity         : Language_Entity;
         Sloc_Start     : Source_Location;
         Sloc_End       : Source_Location;
         Partial_Entity : Boolean) return Boolean;
      --  Function called by Language.Parse_Entities for each entity found

      procedure Local_Highlight;
      --  Highlight the region exactly located between Entity_Start and
      --  Entity_End.
      --  After this procedure is run, some variables are positioned to
      --  the following values:
      --    - Last_Entity is equal to the incomplete entity kind found inside
      --      the given region, or to Normal_Text if all entities were complete
      --    - Entity_Start is set to the begining of the incomplete region
      --      found in the given buffer slice, if any.

      Highlight_Complete : Boolean := False;
      Entity_Start       : Gtk_Text_Iter;
      Entity_End         : Gtk_Text_Iter;
      Slice_Offset_Line  : Buffer_Line_Type;
      --  Offset between the beginning of the Source_Buffer and the beginning
      --  of the string slice passed to Parse_Entities.

      Tmp_Start, Tmp_End  : Gtk_Text_Iter;
      Slice_Offset_Column : Gint;
      Result              : Boolean;
      Ignored             : Boolean;
      Entity_Kind         : Language_Entity;
      Slice               : Unchecked_String_Access;
      pragma Suppress (Access_Check, Slice);

      ------------------
      -- Highlight_Cb --
      ------------------

      function Highlight_Cb
        (Entity         : Language_Entity;
         Sloc_Start     : Source_Location;
         Sloc_End       : Source_Location;
         Partial_Entity : Boolean) return Boolean
      is
         Success         : Boolean;
         Start           : Natural;
         Col, Line       : Gint;
         Offset          : Gint;
         Buffer_Line     : Buffer_Line_Type;
         End_Buffer_Line : Buffer_Line_Type;

      begin
         if Partial_Entity then
            Highlight_Complete := False;
         end if;

         if Entity not in Standout_Language_Entity then
            return False;
         end if;

         --  Some parsers currently leave line numbers to 0. Don't highlight in
         --  this case, since we cannot use from the byte index due to
         --  limitations in gtk+

         if Sloc_Start.Line < 1 then
            return False;
         end if;

         --  Don't need to take into account the offset column, unless we are
         --  still on the same line that we started at.

         if Sloc_Start.Line = 1 then
            Offset := Slice_Offset_Column;
         else
            Offset := 0;
         end if;

         Col := Gint (Sloc_Start.Column) + Offset - 1;
         Buffer_Line := Buffer_Line_Type (Sloc_Start.Line) + Slice_Offset_Line;

         End_Buffer_Line :=
           Buffer_Line_Type (Sloc_End.Line) + Slice_Offset_Line;

         --  If the column is 0, the entity really ended on the end of the
         --  previous line.

         if End_Buffer_Line = 0 then
            return False;
         end if;

         Line := Gint (Buffer_Line - 1);
         Is_Valid_Index (Self.Buffer, Entity_Start, Success, Line, Col);

         if not Success then
            Trace (Me, "invalid position");
            return False;
         end if;

         Line := Gint (End_Buffer_Line - 1);

         if Sloc_End.Column = 0 then
            Get_Iter_At_Line_Index (Self.Buffer, Entity_End, Line, 0);
            Backward_Char (Entity_End, Success);

         else
            if Gint (Sloc_End.Line) = 1 then
               Offset := Slice_Offset_Column;
            else
               Offset := 0;
            end if;

            if Slice (Sloc_End.Index) /= ASCII.LF then
               Col := Gint (Sloc_End.Column) + Offset - 1;

               --  Is_Valid_Index requires an index at the start of a character
               --  while Sloc_End.Index points to the end of a character, so
               --  adjust if needed.

               Start := UTF8_Find_Prev_Char
                 (Slice (1 .. Sloc_End.Index + 1), Sloc_End.Index + 1);

               if Start /= Sloc_End.Index then
                  Col := Col - Gint (Sloc_End.Index - Start);
               end if;

               Is_Valid_Index (Self.Buffer, Entity_End, Success, Line, Col);

               if not Success then
                  Trace (Me, "invalid position """
                         & Self.Buffer.Filename.Display_Full_Name & """"
                         & Line'Img & Col'Img);
                  return False;
               end if;

               Forward_Char (Entity_End, Success);

            else
               Is_Valid_Index (Self.Buffer, Entity_End, Success, Line, 0);
               if not Success then
                  Trace (Me, "invalid position """
                         & Self.Buffer.Filename.Display_Full_Name & """"
                         & Line'Img & " 0--");
                  return False;
               end if;

               if not Ends_Line (Entity_End) then
                  Forward_To_Line_End (Entity_End, Success);
               end if;
            end if;
         end if;

         if Entity in Aspect_Keyword_Text .. Aspect_Text then
            --  We have an aspect here, store information about this for
            --  lines
            declare
               Line    : Buffer_Line_Type :=
                 Buffer_Line_Type (Get_Line (Entity_Start) + 1);
               The_End : constant Buffer_Line_Type :=
                 Buffer_Line_Type (Get_Line (Entity_End) + 1);
            begin
               while Line <= The_End
                 and then Line in Self.Buffer.Line_Data'Range
               loop
                  Self.Buffer.Line_Data (Line).Has_Aspect := True;
                  Line := Line + 1;
               end loop;
            end;
         end if;

         Apply_Tag
           (Self.Buffer, Self.Syntax_Tags (Entity), Entity_Start, Entity_End);

         return False;
      end Highlight_Cb;

      ---------------------
      -- Local_Highlight --
      ---------------------

      procedure Local_Highlight is
         UTF8   : constant Gtkada.Types.Chars_Ptr :=
           Get_Slice (Entity_Start, Entity_End);

         --  Can't use Get_Offset (Entity_End) - Get_Offset (Entity_Start)
         --  since this would give the number of chars, not bytes.

         Length : constant Integer := Integer (Strlen (UTF8));

      begin
         Slice               := To_Unchecked_String (UTF8);
         Highlight_Complete  := True;
         Slice_Offset_Line   := Buffer_Line_Type (Get_Line (Entity_Start));
         Slice_Offset_Column := Get_Line_Index (Entity_Start);

         --  First, un-apply all the style tags...

         Self.Kill_Highlighting (Entity_Start, Entity_End);

         --  Now re-highlight the text...

         if Self.Buffer.Lang /= null then
            Parse_Entities
              (Self.Buffer.Lang,
               Slice (1 .. Length),
               Highlight_Cb'Unrestricted_Access);
         end if;

         Slice := null;
         g_free (UTF8);
      end Local_Highlight;

   begin
      Copy (Source => Start_Iter, Dest => Entity_Start);
      Copy (Source => End_Iter, Dest => Entity_End);

      --  Checking whether we have an aspect on the first range line
      --  and enlarging the range to the first aspect's line if needed
      Slice_Offset_Line := Buffer_Line_Type (Get_Line (Start_Iter) + 1);
      while Slice_Offset_Line > 1
        and then Slice_Offset_Line in Self.Buffer.Line_Data'Range
        and then Self.Buffer.Line_Data (Slice_Offset_Line).Has_Aspect
      loop
         Slice_Offset_Line := Slice_Offset_Line - 1;
      end loop;

      if Slice_Offset_Line < Buffer_Line_Type (Get_Line (Start_Iter) + 1) then
         Self.Buffer.Get_Iter_At_Line
           (Entity_Start, Gint (Slice_Offset_Line));
      else
         --  Start from the beginning of the current line to handle special
         --  language semantics requiring information from previous characters,
         --  such as x.all'address in Ada...

         Set_Line_Offset (Entity_Start, 0);

         if Get_Line (Entity_Start) /= Get_Line (Start_Iter) then
            Copy (Source => Start_Iter, Dest => Entity_Start);
         end if;
      end if;

      if Self.Buffer.Lang /= null
        and then Get_Language_Context (Self.Buffer.Lang).Use_Semicolon
      then
         --  ...and highlight from the previous semicolon, to handle multiple
         --  line constructs such as C comments or Ada aspect clauses. Note:
         --  this is a heuristic, since we could find a semicolon in the middle
         --  of a comment, which wouldn't help us that much.

         Backward_Search
           (Iter => Entity_Start, Str => ";", Flags => 0,
            Match_Start => Tmp_Start, Match_End => Tmp_End,
            Result => Result);

         if Result then
            Forward_Char (Tmp_Start, Result);
            Entity_Start := Tmp_Start;
         else
            Set_Offset (Entity_Start, 0);
         end if;
      end if;

      --  Highlight to the end of line, to avoid missing most of the partial
      --  entities (strings, characters, ...). In case we have started typing
      --  a string for instance, that provides a nice optimization over
      --  rehighlighting the whole buffer...

      Forward_To_Line_End (Entity_End, Result);

      if Self.Buffer.Lang /= null
        and then Get_Language_Context (Self.Buffer.Lang).Use_Semicolon
      then
         --  ...and go to next semicolon if any
         Forward_Search
           (Iter => Entity_End, Str => ";", Flags => 0,
            Match_Start => Tmp_Start, Match_End => Tmp_End,
            Result => Result);

         if Result then
            Entity_End := Tmp_Start;
         else
            Forward_To_End (Entity_End);
         end if;
      end if;

      --  Search the initial minimum area to re-highlight...

      Entity_Kind := Normal_Text;

      Entity_Kind_Search_Loop :
      for Current_Entity in Standout_Language_Entity loop
         if Has_Tag (Entity_Start, Self.Syntax_Tags (Current_Entity)) then
            --  This means that we are in a highlighted region. The minimum
            --  region to re-highlight starts from the begining of the
            --  current region to the end of the following region.

            Entity_Kind := Current_Entity;

            Backward_To_Tag_Toggle
              (Entity_Start,
               Self.Syntax_Tags (Current_Entity),
               Result => Ignored);
            Forward_To_Tag_Toggle
              (Entity_End, Self.Syntax_Tags (Entity_Kind), Ignored);
            Forward_To_Tag_Toggle (Entity_End, Result => Ignored);

            exit Entity_Kind_Search_Loop;

         elsif Begins_Tag (Entity_End, Self.Syntax_Tags (Current_Entity))
           or else Ends_Tag (Entity_Start, Self.Syntax_Tags (Current_Entity))
         then
            --  Case Begins_Tag:
            --    This means that we inserted right at the begining of
            --    a highlighted region... The minimum region to re-highlight
            --    starts from the begining of the previous region to the
            --    end of the current region.
            --  Case Ends_Tag:
            --    This means that we inserted right at the end of a
            --    highlighted region. In this case, the minimum region to
            --    re-highlight starts from the begining of the previous
            --    region to the end of the current region.
            --  In both cases, the processing is the same...

            Entity_Kind := Current_Entity;
            Backward_To_Tag_Toggle (Entity_Start, Result => Ignored);
            Forward_To_Tag_Toggle (Entity_End, Result => Ignored);

            exit Entity_Kind_Search_Loop;
         end if;
      end loop Entity_Kind_Search_Loop;

      if Entity_Kind = Normal_Text then
         --  We are inside a normal text region. Just re-highlight this
         --  region.

         Backward_To_Tag_Toggle (Entity_Start, Result => Ignored);
         Forward_To_Tag_Toggle (Entity_End, Result => Ignored);
      end if;

      Local_Highlight;

      if not Highlight_Complete then
         --  In this case, we are in the middle of e.g a multi-line comment,
         --  and we re-highlight the whole buffer since we do not know where
         --  the comment started.
         --  ??? would be nice to optimize here. We could for instance
         --  highlight only from the beginning of the section instead of from
         --  the start

         Set_Offset (Entity_Start, 0);
         Forward_To_End (Entity_End);
         Local_Highlight;
      end if;
   end Highlight_Slice;

   -----------------------------
   -- Update_Highlight_Region --
   -----------------------------

   procedure Update_Highlight_Region
     (Self : access Source_Highlighter_Record;
      Iter : Gtk_Text_Iter)
   is
      First_Mark_Iter : Gtk_Text_Iter;
      Last_Mark_Iter  : Gtk_Text_Iter;

   begin
      if not Self.Highlight_Needed then
         Self.Highlight_Needed := True;
         Move_Mark (Self.Buffer, Self.Last_Highlight_Mark, Iter);

      else
         Get_Iter_At_Mark
           (Self.Buffer, First_Mark_Iter, Self.First_Highlight_Mark);
         Get_Iter_At_Mark
           (Self.Buffer, Last_Mark_Iter, Self.Last_Highlight_Mark);

         if Get_Offset (First_Mark_Iter) > Get_Offset (Iter) then
            Move_Mark (Self.Buffer, Self.First_Highlight_Mark, Iter);
         end if;

         if Get_Offset (Last_Mark_Iter) < Get_Offset (Iter) then
            Move_Mark (Self.Buffer, Self.Last_Highlight_Mark, Iter);
         end if;
      end if;

      if not Self.Buffer.Inserting
        and then Self.Auto_Highlight_Enabled
      then
         Self.Highlight_Region;
      end if;
   end Update_Highlight_Region;

   ----------------------
   -- Highlight_Region --
   ----------------------

   procedure Highlight_Region
     (Self : access Source_Highlighter_Record)
   is
      Start_Iter : Gtk_Text_Iter;
      End_Iter   : Gtk_Text_Iter;
   begin
      if Self.Call_Clear_Highlighting then
         Self.Call_Clear_Highlighting := False;

         Clear_Highlighting_Hook.Run
           (Kernel => Self.Buffer.Kernel,
            File   => Self.Buffer.Filename);
      end if;

      if not Self.Highlight_Needed then
         return;
      end if;

      Get_Iter_At_Mark (Self.Buffer, Start_Iter, Self.First_Highlight_Mark);
      Get_Iter_At_Mark (Self.Buffer, End_Iter, Self.Last_Highlight_Mark);
      Self.Highlight_Slice (Start_Iter, End_Iter);

      Self.Highlight_Needed := False;
   end Highlight_Region;

   ------------------
   -- On_Load_File --
   ------------------

   procedure On_Load_File
     (Self : access Source_Highlighter_Record)
   is
      F, L : Gtk_Text_Iter;
   begin
      --  Highlight the newly inserted text
      if Self.Buffer.Lang = null
        or else not Get_Language_Context
          (Self.Buffer.Lang).Syntax_Highlighting
      then
         return;
      end if;

      Get_Bounds (Self.Buffer, F, L);
      Self.Highlight_Needed := True;
      Move_Mark (Self.Buffer, Self.First_Highlight_Mark, F);
      Move_Mark (Self.Buffer, Self.Last_Highlight_Mark, L);
      Self.Highlight_Region;
   end On_Load_File;

   ---------------------
   -- On_Set_Language --
   ---------------------

   procedure On_Set_Language
     (Self       : access Source_Highlighter_Record;
      Start_Iter : Gtk_Text_Iter;
      End_Iter   : Gtk_Text_Iter) is
   begin
      Self.Update_Use_Highlighting_Hook;

      --  Do not try to highlight an empty buffer
      if not Is_End (Start_Iter) then
         if Self.Buffer.Lang /= null
           and then not Get_Language_Context
             (Self.Buffer.Lang).Syntax_Highlighting
         then
            Self.Kill_Highlighting (Start_Iter, End_Iter);

         elsif Self.Use_Highlighting_Hook then
            Highlight_Range_Hook.Run
              (Kernel    => Self.Buffer.Kernel,
               File      => Self.Buffer.Filename,
               From_Line => Natural (Get_Line (Start_Iter) + 1),
               To_Line   => Natural (Get_Line (End_Iter) + 1));
         else
            Self.Highlight_Slice (Start_Iter, End_Iter);
         end if;
      end if;
   end On_Set_Language;

   ------------------------------
   -- Remove_Line_Highlighting --
   ------------------------------

   procedure Remove_Line_Highlighting
     (Self   : access Source_Highlighter_Record;
      Line   : Editable_Line_Type;
      Style  : not null Style_Access)
   is
      The_Line : Buffer_Line_Type;
   begin
      if Self.Buffer.In_Destruction then
         return;
      end if;

      if Line = 0 then
         --  This procedure is called by Highlight_Range, but not when Line=0,
         --  so there are no recursive calls here
         Self.Highlight_Range (Style, 0, 1, 1, True);

         for J in Self.Buffer.Line_Data'Range loop
            Self.Set_Line_Highlighting (J, Style, False, (others => True));
         end loop;
      else
         The_Line := Get_Buffer_Line (Self.Buffer, Line);

         if The_Line /= 0 then
            Self.Set_Line_Highlighting
              (The_Line, Style, False, (others => True));
         end if;
      end if;

      Line_Highlights_Changed (Self.Buffer);
   end Remove_Line_Highlighting;

   -------------------------
   -- Remove_Highlighting --
   -------------------------

   procedure Remove_Highlighting
     (Self      : access Source_Highlighter_Record;
      Style     : Style_Access;
      From_Line : Editable_Line_Type;
      To_Line   : Editable_Line_Type)
   is
      Start_Iter, End_Iter : Gtk_Text_Iter;
      Ignore               : Boolean;
      One                  : constant Character_Offset_Type := 1;
      Tag                  : Gtk_Text_Tag;
   begin
      Tag := Lookup (Get_Tag_Table (Self.Buffer), Get_Name (Style));

      --  Remove tag-based highlighting
      if Tag /= null then
         Get_Iter_At_Screen_Position
           (Self.Buffer, Start_Iter, From_Line, One);
         Get_Iter_At_Screen_Position
           (Self.Buffer, End_Iter, To_Line, One);

         if not Ends_Line (End_Iter) then
            Forward_To_Line_End (End_Iter, Ignore);
         end if;

         Remove_Tag (Self.Buffer, Tag, Start_Iter, End_Iter);
      end if;

      --  Remove line-based highlighting
      if Get_In_Speedbar (Style) then
         for Line in From_Line .. To_Line loop
            Self.Remove_Line_Highlighting (Line, Style);
         end loop;
      end if;
   end Remove_Highlighting;

   -----------------------
   -- Kill_Highlighting --
   -----------------------

   procedure Kill_Highlighting
     (Self : access Source_Highlighter_Record;
      From : Gtk_Text_Iter;
      To   : Gtk_Text_Iter)
   is
      Line    : Buffer_Line_Type := Buffer_Line_Type (Get_Line (From) + 1);
      The_End : constant Buffer_Line_Type :=
        Buffer_Line_Type (Get_Line (To) + 1);
   begin
      while Line <= The_End
        and then Line in Self.Buffer.Line_Data'Range
      loop
         --  Clearing 'aspect' information for lines
         Self.Buffer.Line_Data (Line).Has_Aspect := False;
         Line := Line + 1;
      end loop;

      for Entity_Kind in Standout_Language_Entity loop
         Remove_Tag (Self.Buffer, Self.Syntax_Tags (Entity_Kind), From, To);
      end loop;
   end Kill_Highlighting;

   -------------------------
   -- Get_Highlight_Color --
   -------------------------

   function Get_Highlight_Color
     (Self    : access Source_Highlighter_Record;
      Line    : Buffer_Line_Type;
      Context : Highlight_Location) return Gdk_RGBA is
   begin
      if Line = 0 then
         return Null_RGBA;
      end if;

      if Self.Buffer.Line_Data /= null
        and then Line <= Self.Buffer.Line_Data'Last
        and then Self.Buffer.Line_Data
          (Line).Highlighting (Context).Active /= 0
      then
         return Get_Color
           (Self.Buffer.Line_Data (Line).Highlighting (Context).Active);

      else
         return Null_RGBA;
      end if;
   end Get_Highlight_Color;

   --------------------
   -- Is_Comment_Tag --
   --------------------

   function Is_Comment_Tag
     (Self : access Source_Highlighter_Record;
      Pos  : Gtk.Text_Iter.Gtk_Text_Iter)
      return Boolean is
   begin
      return Has_Tag (Pos, Self.Syntax_Tags (Comment_Text))
        or else Has_Tag (Pos, Self.Syntax_Tags (Aspect_Comment_Text))
        or else Has_Tag (Pos, Self.Syntax_Tags (Annotated_Comment_Text))
        or else Has_Tag (Pos, Self.Syntax_Tags (Annotated_Keyword_Text));
   end Is_Comment_Tag;

   ---------------------------
   -- Highlight_Parenthesis --
   ---------------------------

   procedure Highlight_Parenthesis (Self : access Source_Highlighter_Record)
   is
      On_Cursor_Iter       : Gtk_Text_Iter;
      First_Highlight_Iter : Gtk_Text_Iter;
      Last_Highlight_Iter  : Gtk_Text_Iter;
      Current              : Gtk_Text_Iter;
      Found                : Natural;
      Success              : Boolean;
   begin
      --  Highlight the cursor delimiters
      if not Self.Highlight_Delimiters then
         return;
      end if;

      Get_Iter_At_Mark (Self.Buffer, On_Cursor_Iter, Self.Buffer.Insert_Mark);
      Get_Delimiters
        (Self.Buffer,
         On_Cursor_Iter,
         First_Highlight_Iter, Last_Highlight_Iter,
         Found);

      if Found >= 1 then
         Copy (First_Highlight_Iter, Current);
         Forward_Char (Current, Success);
         if Success then
            Apply_Tag
              (Self.Buffer,
               Self.Delimiter_Tag,
               First_Highlight_Iter,
               Current);
         end if;

         Copy (Last_Highlight_Iter, Current);
         Backward_Char (Current, Success);
         if Success then
            Apply_Tag
              (Self.Buffer,
               Self.Delimiter_Tag,
               Current,
               Last_Highlight_Iter);
         end if;

         if Found = 2 then
            Copy (On_Cursor_Iter, Current);
            Backward_Char (Current, Success);
            if Success then
               Forward_Char (On_Cursor_Iter, Success);
               if Success then
                  Apply_Tag
                    (Self.Buffer,
                     Self.Delimiter_Tag,
                     Current,
                     On_Cursor_Iter);
               end if;
            end if;
         end if;

         Self.Start_Delimiters_Highlight := Create_Mark
           (Self.Buffer, "", First_Highlight_Iter);
         Self.End_Delimiters_Highlight := Create_Mark
           (Self.Buffer, "", Last_Highlight_Iter);

         Self.Has_Delimiters_Highlight := True;
      end if;
   end Highlight_Parenthesis;

   ------------------------------------
   -- Remove_Delimiters_Highlighting --
   ------------------------------------

   procedure Remove_Delimiters_Highlighting
     (Self : access Source_Highlighter_Record) is
   begin
      if not Self.Has_Delimiters_Highlight then
         return;
      end if;

      declare
         From : Gtk_Text_Iter;
         To   : Gtk_Text_Iter;
      begin
         Get_Iter_At_Mark
           (Self.Buffer, From, Self.Start_Delimiters_Highlight);
         Get_Iter_At_Mark (Self.Buffer, To, Self.End_Delimiters_Highlight);

         Delete_Mark (Self.Buffer, Self.Start_Delimiters_Highlight);
         Delete_Mark (Self.Buffer, Self.End_Delimiters_Highlight);

         Remove_Tag (Self.Buffer, Self.Delimiter_Tag, From, To);
      end;

      Self.Has_Delimiters_Highlight := False;
   end Remove_Delimiters_Highlighting;

   ----------------------
   -- Emit_File_Edited --
   ----------------------

   procedure Emit_File_Edited
     (Buffer   : not null access Source_Buffer_Record'Class;
      Filename : Virtual_File) is
   begin
      File_Edited_Hook.Run (Buffer.Kernel, Filename);
      for Listener of Buffer.Listeners loop
         Listener.File_Edited (Filename);
      end loop;
   end Emit_File_Edited;

   ----------------------
   -- Emit_File_Closed --
   ----------------------

   procedure Emit_File_Closed
     (Buffer   : not null access Source_Buffer_Record'Class;
      Filename : Virtual_File) is
   begin
      File_Closed_Hook.Run (Buffer.Kernel, Filename);
      for Listener of Buffer.Listeners loop
         Listener.File_Closed (Filename);
      end loop;
   end Emit_File_Closed;

   -----------------------
   -- Emit_File_Renamed --
   -----------------------

   procedure Emit_File_Renamed
     (Buffer : not null access Source_Buffer_Record'Class;
      From   : Virtual_File;
      To     : Virtual_File)
   is
   begin
      File_Renamed_Hook.Run (Buffer.Kernel, From, To);
      for Listener of Buffer.Listeners loop
         Listener.File_Renamed (From, To);
      end loop;
   end Emit_File_Renamed;

   ------------------------------
   -- Set_Opened_On_LSP_Server --
   ------------------------------

   procedure Set_Opened_On_LSP_Server
     (This  : access Source_Buffer_Record;
      Value : Boolean) is
   begin
      This.LSP_Opened := Value;
   end Set_Opened_On_LSP_Server;

   -----------------------------
   -- Is_Opened_On_LSP_Server --
   -----------------------------

   function Is_Opened_On_LSP_Server
     (This : access Source_Buffer_Record) return Boolean is
   begin
      return This.LSP_Opened;
   end Is_Opened_On_LSP_Server;

end Src_Editor_Buffer;
