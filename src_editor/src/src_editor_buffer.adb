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

with Ada.Calendar;                        use Ada.Calendar;
with Ada.Characters.Handling;             use Ada.Characters.Handling;
with Ada.Strings.Unbounded;               use Ada.Strings.Unbounded;

pragma Warnings (Off);
with Ada.Strings.Unbounded.Aux;           use Ada.Strings.Unbounded.Aux;
pragma Warnings (On);
with Ada.Strings.Maps;                    use Ada.Strings.Maps;

with Interfaces.C.Strings;                use Interfaces.C.Strings;
with System.Address_Image;

with GNAT.Expect;                         use GNAT.Expect;
with GNAT.Regpat;                         use GNAT.Regpat;

with GNATCOLL.Arg_Lists;                  use GNATCOLL.Arg_Lists;
with GNATCOLL.Paragraph_Filling;          use GNATCOLL.Paragraph_Filling;
with GNATCOLL.Symbols;                    use GNATCOLL.Symbols;
with GNATCOLL.Traces;                     use GNATCOLL.Traces;
with GNATCOLL.Utils;                      use GNATCOLL.Utils;
with GNATCOLL.VFS;                        use GNATCOLL.VFS;

with Gdk.Color;                           use Gdk.Color;

with Glib.Convert;                        use Glib.Convert;
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
with Gtk.Text_Mark;                       use Gtk.Text_Mark;
with Gtk.Text_Tag;                        use Gtk.Text_Tag;
with Gtk.Text_Tag_Table;                  use Gtk.Text_Tag_Table;
with Gtk.Widget;

with Gtkada.Dialogs;                      use Gtkada.Dialogs;
with Gtkada.MDI;                          use Gtkada.MDI;
with Gtkada.Types;                        use Gtkada.Types;

with Pango.Font;                          use Pango.Font;
with Pango.Enums;                         use Pango.Enums;

with Casing_Exceptions;                   use Casing_Exceptions;
with Case_Handling;                       use Case_Handling;
with Commands.Editor;                     use Commands.Editor;
with Commands.Controls;                   use Commands.Controls;
with Completion_Module;                   use Completion_Module;
with GPS.Intl;                            use GPS.Intl;
with GPS.Kernel;                          use GPS.Kernel;
with GPS.Kernel.Charsets;                 use GPS.Kernel.Charsets;
with GPS.Kernel.Console;                  use GPS.Kernel.Console;
with GPS.Kernel.Contexts;                 use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;                    use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;                      use GPS.Kernel.MDI;
with GPS.Kernel.Messages.Simple;          use GPS.Kernel.Messages.Simple;
with GPS.Kernel.Modules;                  use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;              use GPS.Kernel.Preferences;
with GPS.Kernel.Properties;               use GPS.Kernel.Properties;
with GPS.Kernel.Project;                  use GPS.Kernel.Project;
with GPS.Kernel.Scripts;                  use GPS.Kernel.Scripts;
with GPS.Properties;
with Language;                            use Language;
with Language.Unknown;                    use Language.Unknown;
with Language_Handlers;                   use Language_Handlers;
with Src_Editor_Box;                      use Src_Editor_Box;
with Src_Editor_Buffer.Blocks;
with Src_Editor_Buffer.Line_Information;
with Src_Editor_Buffer.Hooks;             use Src_Editor_Buffer.Hooks;
with Src_Editor_Module;                   use Src_Editor_Module;
with Src_Editor_Module.Line_Highlighting;
with Src_Highlighting;                    use Src_Highlighting;
with String_Utils;                        use String_Utils;
with Traces;

package body Src_Editor_Buffer is

   use Src_Editor_Buffer.Blocks;
   use Src_Editor_Module.Line_Highlighting;
   use Src_Editor_Buffer.Line_Information;

   use type System.Address;
   use type GNAT.Strings.String_Access;

   Me                   : constant Trace_Handle :=
                            Create ("Source_Editor_Buffer");
   Indent_On_Block_Info : constant Trace_Handle :=
                            Create
                              ("Source_Editor_Buffer.Indent_On_Block_Info",
                               Default => Off);

   Buffer_Recompute_Interval : constant Guint := 200;
   --  The interval at which to check whether the buffer should be reparsed,
   --  in milliseconds.

   Cursor_Stop_Interval      : constant Guint := 100;
   --  The interval after which we consider that the cursor has stopped

   Buffer_Recompute_Delay    : constant Duration := 1.0;
   --  The delay between the last edit and the re-parsing of the buffer,
   --  in seconds.

   Cursor_Reactivity_Delay   : constant Duration := 0.2;
   --  The timeout between the last time the cursor moves and the time it is
   --  considered as having stopped.

   Src_Editor_Message_Flags : constant Message_Flags :=
     (Editor_Side => True,
      Locations   => True);

   package Buffer_Timeout is new Glib.Main.Generic_Sources (Source_Buffer);

   function Strlen
     (Str : Interfaces.C.Strings.chars_ptr) return Interfaces.C.size_t;
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

   --------------------
   -- Signal Support --
   --------------------

   Class_Record : GObject_Class := Uninitialized_Class;
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

   procedure Line_Highlights_Changed
     (Buffer : access Source_Buffer_Record'Class);
   --  Emit the "Line_Highlights_Changed" signal

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

   procedure Insert_Text_Handler
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

   procedure First_Insert_Text
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

   procedure Delete_Range_Before_Handler
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues);
   --  Handler connected to the "delete_range" signal, but which occurs before
   --  the actual deletion.

   procedure Delete_Range_Handler
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

   procedure Kill_Highlighting
     (Buffer : access Source_Buffer_Record'Class;
      From   : Gtk_Text_Iter;
      To     : Gtk_Text_Iter);
   --  Remove all highlighting tags for the given region

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

   type Preferences_Changed_Hook_Record is new Function_No_Args with record
      Buffer : Source_Buffer;
   end record;
   type Preferences_Hook is access all Preferences_Changed_Hook_Record'Class;
   overriding procedure Execute
     (Hook   : Preferences_Changed_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class);
   --  Called when the preferences have changed

   type Project_Changed_Hook_Record is new Function_No_Args with record
      Buffer : Source_Buffer;
   end record;
   type Project_Hook is access all Project_Changed_Hook_Record'Class;
   overriding procedure Execute
     (Hook   : Project_Changed_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class);
   --  Called when the project has changed

   procedure Cursor_Move_Hook (Buffer : access Source_Buffer_Record'Class);
   --  Actions that must be executed whenever the cursor moves

   procedure User_Edit_Hook (Buffer : access Source_Buffer_Record'Class);
   --  Actions that must be executed whenever the user inserts or deletes text

   procedure Edit_Hook (Buffer : access Source_Buffer_Record'Class);
   --  Actions that must be executed whenever the buffer text is changed

   procedure Lines_Remove_Hook_Before
     (Buffer     : access Source_Buffer_Record'Class;
      Start_Line : Buffer_Line_Type;
      End_Line   : Buffer_Line_Type);
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

   procedure Register_Cursor_Timeout
     (Buffer : access Source_Buffer_Record'Class);
   --  Indicate that the cursor has moved, and that a timeout should be
   --  registered to call the corresponding "after-timeout" hook.

   function Edition_Timeout (Buffer : Source_Buffer) return Boolean;
   --  Timeout called in a timeout after the user has finished editing

   function Cursor_Stop_Hook (Buffer : Source_Buffer) return Boolean;
   --  Hook called after the cursor has stopped moving

   procedure Free_Column_Info
     (Column_Info : Columns_Config_Access);
   --  Free the info contained in Column_Info

   procedure C_Free (S : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, C_Free, "free");
   --  Binding to the C "free" function

   function Get_Slice
     (Buffer       : Source_Buffer;
      Start_Line   : Gint;
      Start_Column : Gint;
      End_Line     : Gint := -1;
      End_Column   : Gint := -1) return String;
   --  Return the text located between (Start_Line, Start_Column) and
   --  (End_Line, End_Column). The first line is 0, the first column is 0
   --  If End_Line = -1, contents are taken until the end of the buffer.
   --
   --  The text returned is UTF8-encoded.
   --
   --  The validity of both start and end positions must be verified before
   --  invoking this function. An incorrect position will cause an
   --  Assertion_Failure when compiled with assertion checks, or an undefined
   --  behavior otherwise.

   function Get_Slice
     (Buffer       : Source_Buffer;
      Start_Line   : Gint;
      Start_Column : Gint;
      End_Line     : Gint := -1;
      End_Column   : Gint := -1) return Gtkada.Types.Chars_Ptr;
   --  Same as above but return the C pointer directly for efficiency.
   --  The caller is responsible for freeing the memory (with g_free).
   --  The returned string is UTF8-encoded.

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

   procedure Update_Highlight_Region
     (Buffer : Source_Buffer;
      Iter   : Gtk_Text_Iter);
   --  Update the region to be highlighted in the next highlighting timeout

   procedure Process_Highlight_Region
     (Buffer : Source_Buffer);
   --  Highlight the region marked by the highlight marks in the editor

   procedure Highlight_Parenthesis (Buffer : Source_Buffer);
   --  Highlight the matching parenthesis that are next to the cursor, if any

   function Conversion_Error_Message (Charset : String) return UTF8_String;
   --  Return the location category corresponding to errors when converting
   --  to Charset.

   procedure On_Paste_Done (Buffer : access Source_Buffer_Record'Class);
   --  Disable the "paste-done" signal introduced in gtk+ 2.18, which breaks
   --  the handling of multiple views

   procedure Update_Logical_Timestamp
     (Buffer : access Source_Buffer_Record'Class);
   pragma Inline (Update_Logical_Timestamp);
   --  Update the logical timestamp

   -----------
   -- Hooks --
   -----------

   type Internal_Hook_Record is abstract new Function_With_Args with record
      Buffer : Source_Buffer;
   end record;

   type File_Deleted_Hook_Record is new Internal_Hook_Record with null record;
   type File_Deleted_Hook is access File_Deleted_Hook_Record'Class;

   type File_Renamed_Hook_Record is new Internal_Hook_Record with null record;
   type File_Renamed_Hook is access File_Renamed_Hook_Record'Class;

   overriding procedure Execute
     (Hook   : File_Deleted_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Callback for the "file_deleted" hook

   overriding procedure Execute
     (Hook   : File_Renamed_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Callback for the "file_renamed" hook

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Hook   : File_Deleted_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      pragma Unreferenced (Kernel);
      File        : constant GNATCOLL.VFS.Virtual_File :=
                      File_Hooks_Args (Data.all).File;
      Edited      : constant GNATCOLL.VFS.Virtual_File := Hook.Buffer.Filename;
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
         Hook.Buffer.Saved_Position := -1;
         Hook.Buffer.Status_Changed;
      end if;

   exception
      when E : others =>
         Trace (Traces.Exception_Handle, E);
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Hook   : File_Renamed_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      pragma Unreferenced (Kernel);
      File   : constant Virtual_File := Files_2_Hooks_Args (Data.all).File;
      Edited : constant Virtual_File := Hook.Buffer.Filename;
      Dest   : GNATCOLL.VFS.Virtual_File;
   begin
      if Edited /= GNATCOLL.VFS.No_File then
         if Is_Directory (File) and then Is_Parent (File, Edited) then
            Dest := Create_From_Dir
              (Files_2_Hooks_Args (Data.all).Renamed,
               Relative_Path (Edited, File));
            Hook.Buffer.Filename := Dest;
            Hook.Buffer.Filename_Changed;

         elsif not Is_Directory (File) and then Edited = File then
            Hook.Buffer.Filename := Files_2_Hooks_Args (Data.all).Renamed;
            Hook.Buffer.Filename_Changed;
         end if;
      end if;

   exception
      when E : others =>
         Trace (Traces.Exception_Handle, E);
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
   begin
      if S.Read_Only then
         S.Contents := null;
      else
         Free (S.Contents);
      end if;
   end Free;

   -----------------------
   -- Reset_Blocks_Info --
   -----------------------

   procedure Reset_Blocks_Info (Buffer : access Source_Buffer_Record'Class) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Block_Record, Block_Access);
      Block : Block_Access;
   begin
      if Buffer.Editable_Lines /= null then
         --  ??? Should we loop till Buffer.Last_Editable_Line ?
         for Line in Buffer.Editable_Lines'Range loop
            Block := Buffer.Editable_Lines (Line).Block;

            --  Block is shared so we need to ensure that no other block
            --  references it. We only free the block on its last line
            --  (which is always associated with this block, that hasn't
            --  changed) rather than iterate through all the lines looking
            --  for similar blocks. That avoids a O(n^2) algorithm

            if Block /= null
              and then Block.Last_Line = Line
            then
               Unchecked_Free (Block);
            end if;

            Buffer.Editable_Lines (Line).Block := null;
         end loop;
      end if;

      Buffer.Blocks_Exact := False;
   end Reset_Blocks_Info;

   ----------------
   -- Get_String --
   ----------------

   --  ??? See whether these two functions could be optimized further

   function Get_String
     (Buffer : Source_Buffer;
      Line   : Editable_Line_Type) return Src_String
   is
      Start_Iter, End_Iter : Gtk_Text_Iter;
      Success              : Boolean;
      Result               : Src_String;
      Chars                : Interfaces.C.Strings.chars_ptr;

   begin
      if Line not in Buffer.Editable_Lines'Range then
         return Result;
      end if;

      case Buffer.Editable_Lines (Line).Where is
         when In_Buffer =>
            Get_Iter_At_Line
              (Buffer,
               Start_Iter,
               Gint (Buffer.Editable_Lines (Line).Buffer_Line - 1));
            Copy (Start_Iter, End_Iter);
            Forward_To_Line_End (End_Iter, Success);

            if Get_Line (Start_Iter) /= Get_Line (End_Iter) then
               return Result;
            end if;

            Chars := Get_Text (Buffer, Start_Iter, End_Iter, True);
            Result.Contents := To_Unchecked_String (Chars);
            Result.Length := Integer (Strlen (Chars));

         when In_Mark =>
            if Buffer.Editable_Lines (Line).Text /= null then
               Result.Read_Only := True;
               Result.Contents  := To_Unchecked_String
                 (Buffer.Editable_Lines (Line).Text.all'Address);
               Result.Length    := Buffer.Editable_Lines (Line).Text'Length;
            end if;
      end case;

      return Result;
   end Get_String;

   function Get_String
     (Buffer : access Source_Buffer_Record'Class)
      return GNAT.Strings.String_Access
   is
      Start, The_End : Gtk_Text_Iter;
      Result         : GNAT.Strings.String_Access;
      Chars          : Interfaces.C.Strings.chars_ptr;
      C_Str          : Unchecked_String_Access;

   begin
      if Lines_Are_Real (Buffer) then
         Get_Bounds (Buffer, Start, The_End);
         Chars  := Get_Text (Buffer, Start, The_End, True);
         C_Str  := To_Unchecked_String (Chars);
         Result := new String'(C_Str (1 .. Integer (Strlen (Chars))));
         C_Free (Chars);
         --  We need to use the C function to free Chars, since memory was
         --  allocated in C code.

         return Result;

      else
         return Get_Buffer_Lines (Buffer, 1, Buffer.Last_Editable_Line);
      end if;
   end Get_String;

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
      Value  : Boolean)
   is
      Prop : GPS.Properties.String_Property_Access;
   begin
      Buffer.Strip_Trailing_Blanks := Value;

      if Buffer.Filename /= GNATCOLL.VFS.No_File then
         Prop := new GPS.Properties.String_Property'
           (Value => new String'(Boolean'Image (Value)));

         Set_Property
           (Buffer.Kernel,
            Buffer.Filename,
            Strip_Blanks_Property_Name,
            Prop,
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
      Value  : Boolean)
   is
      Prop : GPS.Properties.String_Property_Access;
   begin
      Buffer.Strip_Trailing_Lines := Value;

      if Buffer.Filename /= GNATCOLL.VFS.No_File then
         Prop := new GPS.Properties.String_Property'
           (Value => new String'(Boolean'Image (Value)));

         Set_Property
           (Buffer.Kernel,
            Buffer.Filename,
            Strip_Lines_Property_Name,
            Prop,
            Persistent => True);
      end if;
   end Set_Strip_Trailing_Lines;

   ----------------------
   -- Get_Buffer_Lines --
   ----------------------

   function Get_Buffer_Lines
     (Buffer     : access Source_Buffer_Record'Class;
      Start_Line : Editable_Line_Type;
      End_Line   : Editable_Line_Type) return GNAT.Strings.String_Access
   is
      A      : array (Start_Line .. End_Line) of Src_String;
      Len    : Integer := 0;
      Index  : Integer := 1;
      Output : GNAT.Strings.String_Access;
      Last   : Editable_Line_Type;
   begin
      for J in A'Range loop
         A (J) := Get_String (Source_Buffer (Buffer), J);
         Len := Len + A (J).Length;
      end loop;

      --  If we are looking at the last line, and the last line happens to
      --  be an empty line, ignore it, since we are already adding the last
      --  ASCII.LF of the file as part of the loop below.

      if End_Line = Buffer.Last_Editable_Line
        and then A (End_Line).Length = 0
      then
         Len := Len - 1;
         Last := A'Last - 1;
         Free (A (A'Last));
      else
         Last := A'Last;
      end if;

      Output := new String (1 .. Len + A'Length);

      for J in A'First .. Last loop
         Len := A (J).Length;

         if Len /= 0 then
            Output (Index .. Index + Len - 1) := A (J).Contents (1 .. Len);
         end if;

         Output (Index + Len) := ASCII.LF;
         Index := Index + Len + 1;
         Free (A (J));
      end loop;

      return Output;
   end Get_Buffer_Lines;

   ---------------------
   --  Get_Byte_Index --
   ---------------------

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
                    Get_String (Source_Buffer (Get_Buffer (Iter)), J + 1);
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

   begin
      if Clock < Buffer.Blocks_Request_Timestamp + Buffer_Recompute_Delay then
         return True;
      end if;

      --  Parse the blocks

      if Buffer.Parse_Blocks then
         Compute_Blocks (Buffer);
      end if;

      --  Re-highlight the highlight region if needed

      Process_Highlight_Region (Buffer);

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

      Buffer_Modified (Buffer);

      --  Unregister the timeout

      Buffer.Blocks_Timeout_Registered := False;
      return False;

   exception
      when E : others =>
         Trace (Traces.Exception_Handle, E);
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
   -- Highlight_Parenthesis --
   ---------------------------

   procedure Highlight_Parenthesis (Buffer : Source_Buffer) is
      On_Cursor_Iter       : Gtk_Text_Iter;
      First_Highlight_Iter : Gtk_Text_Iter;
      Last_Highlight_Iter  : Gtk_Text_Iter;
      Current              : Gtk_Text_Iter;
      Found                : Natural;
      Success              : Boolean;
   begin
      Get_Iter_At_Mark (Buffer, On_Cursor_Iter, Buffer.Insert_Mark);
      Get_Delimiters
        (Buffer,
         On_Cursor_Iter,
         First_Highlight_Iter, Last_Highlight_Iter,
         Found);

      if Found >= 1 then
         Copy (First_Highlight_Iter, Current);
         Forward_Char (Current, Success);
         Apply_Tag
           (Buffer,
            Buffer.Delimiter_Tag,
            First_Highlight_Iter,
            Current);

         Copy (Last_Highlight_Iter, Current);
         Backward_Char (Current, Success);
         Apply_Tag
           (Buffer,
            Buffer.Delimiter_Tag,
            Current,
            Last_Highlight_Iter);

         if Found = 2 then
            Copy (On_Cursor_Iter, Current);
            Backward_Char (Current, Success);
            Forward_Char (On_Cursor_Iter, Success);
            Apply_Tag
              (Buffer,
               Buffer.Delimiter_Tag,
               Current,
               On_Cursor_Iter);
         end if;

         Buffer.Start_Delimiters_Highlight := Create_Mark
           (Buffer, "", First_Highlight_Iter);
         Buffer.End_Delimiters_Highlight := Create_Mark
           (Buffer, "", Last_Highlight_Iter);

         if Get_Language_Context (Buffer.Lang).Syntax_Highlighting then
            Backward_To_Tag_Toggle (First_Highlight_Iter, null, Success);
            Forward_To_Tag_Toggle (Last_Highlight_Iter, null, Success);

            Highlight_Slice
              (Buffer, First_Highlight_Iter, Last_Highlight_Iter);
         end if;

         Buffer.Has_Delimiters_Highlight := True;
      end if;
   end Highlight_Parenthesis;

   ----------------------
   -- Cursor_Stop_Hook --
   ----------------------

   function Cursor_Stop_Hook (Buffer : Source_Buffer) return Boolean is
   begin
      if Clock < Buffer.Cursor_Timestamp + Cursor_Reactivity_Delay then
         return True;
      end if;

      --  Emit the hook
      Location_Changed (Buffer);

      --  Highlight the cursor delimiters
      if Buffer.Highlight_Delimiters then
         Highlight_Parenthesis (Buffer);
      end if;

      Buffer.Cursor_Timeout_Registered := False;
      return False;
   exception
      when E : others =>
         Trace (Traces.Exception_Handle, E);
         return False;
   end Cursor_Stop_Hook;

   -----------------------------
   -- Register_Cursor_Timeout --
   -----------------------------

   procedure Register_Cursor_Timeout
     (Buffer : access Source_Buffer_Record'Class) is
   begin
      Buffer.Cursor_Timestamp := Clock;

      if not Buffer.Cursor_Timeout_Registered then
         Buffer.Cursor_Timeout_Registered := True;
         Buffer.Cursor_Timeout := Buffer_Timeout.Timeout_Add
           (Cursor_Stop_Interval,
            Cursor_Stop_Hook'Access,
            Source_Buffer (Buffer));
      end if;
   end Register_Cursor_Timeout;

   ---------------------------
   -- Register_Edit_Timeout --
   ---------------------------

   procedure Register_Edit_Timeout
     (Buffer : access Source_Buffer_Record'Class) is
   begin
      Buffer.Blocks_Request_Timestamp := Clock;

      if not Buffer.Blocks_Timeout_Registered then
         Buffer.Blocks_Timeout_Registered := True;
         Buffer.Blocks_Timeout := Buffer_Timeout.Timeout_Add
           (Buffer_Recompute_Interval,
            Edition_Timeout'Access,
            Source_Buffer (Buffer));
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

      if Buffer.Editable_Lines /= null then
         if Line in Buffer.Editable_Lines'Range then
            if Buffer.Editable_Lines (Line).Where = In_Buffer then
               return Buffer.Editable_Lines (Line).Buffer_Line;
            end if;
         end if;
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

   -----------------------
   -- Get_Editable_Line --
   -----------------------

   function Get_Editable_Line
     (Buffer : access Source_Buffer_Record;
      Line   : File_Line_Type) return Editable_Line_Type is
   begin
      for J in Buffer.Line_Data'Range loop
         if Buffer.Line_Data (J).File_Line = Line then
            return Buffer.Line_Data (J).Editable_Line;
         end if;

         --  ??? This optimization assumes that the lines are ordered, is it
         --  true ?
         if Buffer.Line_Data (J).File_Line > Line then
            return 0;
         end if;
      end loop;

      return 0;
   end Get_Editable_Line;

   ---------------------
   -- Get_Buffer_Line --
   ---------------------

   function Get_Buffer_Line
     (Buffer : access Source_Buffer_Record;
      Line   : File_Line_Type) return Buffer_Line_Type is
   begin
      for J in Buffer.Line_Data'Range loop
         if Buffer.Line_Data (J).File_Line = Line then
            return J;
         end if;

         --  ??? This optimization assumes that the lines are ordered, is it
         --  true ?
         if Buffer.Line_Data (J).File_Line > Line then
            return 0;
         end if;
      end loop;

      return 0;
   end Get_Buffer_Line;

   --------------------
   -- Automatic_Save --
   --------------------

   function Automatic_Save (Buffer : Source_Buffer) return Boolean is
      Success : Boolean;
   begin
      if not Buffer.Modified_Auto or else Buffer.Filename = No_File then
         return True;
      end if;

      Internal_Save_To_File
        (Buffer,
         Autosaved_File (Buffer.Filename),
         True,
         Success);
      Buffer.Modified_Auto := False;

      return True;
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
      end if;

      Register_Cursor_Timeout (Buffer);
   end Cursor_Move_Hook;

   ---------------
   -- Edit_Hook --
   ---------------

   procedure Edit_Hook (Buffer : access Source_Buffer_Record'Class) is
   begin
      --  Request re-parsing of the blocks

      if Buffer.Constructs_State > Approximate then
         Buffer.Constructs_State := Approximate;
      end if;

      Buffer.Blocks_Exact := False;

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

      if Buffer.Blocks_Timeout_Registered then
         Glib.Main.Remove (Buffer.Blocks_Timeout);
      end if;

      if Buffer.Cursor_Timeout_Registered then
         Glib.Main.Remove (Buffer.Cursor_Timeout);
      end if;
   end Destroy_Hook;

   ----------
   -- Free --
   ----------

   procedure Free (Info : in out Line_Information_Access) is
   begin
      if Info = null then
         return;
      end if;

      if Info.Text /= null then
         GNAT.Strings.Free (Info.Text);
      end if;

      if Info.Tooltip_Text /= null then
         GNAT.Strings.Free (Info.Tooltip_Text);
      end if;

      if Info.Associated_Command /= null then
         Unref (Info.Associated_Command);
      end if;

      Unchecked_Free (Info);
   end Free;

   --------------------
   -- Buffer_Destroy --
   --------------------

   procedure Buffer_Destroy (Data : System.Address; Buf : System.Address) is

      procedure Free (X : in out Line_Info_Width_Array);
      --  Free memory associated to X

      Stub    : Source_Buffer_Record;
      Success : Boolean;
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

      --  We do not free memory associated to Buffer.Current_Command, since
      --  this command is already freed when freeing Buffer.Queue.

      Destroy_Hook (Buffer);

      if Buffer.Timeout_Registered then
         Glib.Main.Remove (Buffer.Timeout_Id);
         Buffer.Timeout_Registered := False;

         if Buffer.Filename /= GNATCOLL.VFS.No_File then
            Delete (Autosaved_File (Buffer.Filename), Success);
         end if;
      end if;

      if Buffer.Controls_Command /= null then
         Remove_Controls (Buffer);
      end if;

      Free_Queue (Buffer.Queue);
      Free_File_Information (Buffer);

      Free_Column_Info (Buffer.Editable_Line_Info_Columns);
      Unchecked_Free (Buffer.Editable_Line_Info_Columns);

      if Buffer.Editable_Lines /= null then
         Reset_Blocks_Info (Buffer);

         for J in Buffer.Editable_Lines'Range loop
            if Buffer.Editable_Lines (J).Where = In_Mark then
               GNAT.Strings.Free (Buffer.Editable_Lines (J).Text);
            end if;
         end loop;

         Unchecked_Free (Buffer.Editable_Lines);
      end if;

      for J in Buffer.Line_Data'Range loop
         if Buffer.Line_Data (J).Enabled_Highlights /= null then
            Unchecked_Free (Buffer.Line_Data (J).Enabled_Highlights);
            --  Block already freed by the call to Reset_Blocks_Info
         end if;

         if Buffer.Line_Data (J).Side_Info_Data /= null then
            Free (Buffer.Line_Data (J).Side_Info_Data.all);
            Unchecked_Free (Buffer.Line_Data (J).Side_Info_Data);
         end if;
      end loop;

      Free (Buffer.Constructs);

      Unchecked_Free (Buffer.Line_Data);
      GNAT.Strings.Free (Buffer.Charset);

      Unref (Buffer.Delimiter_Tag);
      Unref (Buffer.Non_Editable_Tag);
      Unref (Buffer.Syntax_Tags);
      Unref (Buffer.Hyper_Mode_Tag);

      Reset_Completion_Data;
   end Buffer_Destroy;

   ---------------------
   -- Changed_Handler --
   ---------------------

   procedure Changed_Handler (Buffer : access Source_Buffer_Record'Class) is
   begin
      Emit_New_Cursor_Position (Buffer);
      Buffer.Modified_Auto := True;

   exception
      when E : others =>
         Trace (Traces.Exception_Handle, E);
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

   begin
      --  Emit the new cursor position if it is the Insert_Mark that was
      --  changed.

      if Buffer.Setting_Mark then
         return;
      end if;

      Buffer.Setting_Mark := True;

      --  If the mark being moved corresponds to the cursor, emit the
      --  corresponding signal.

      if Mark = Buffer.Insert_Mark then
         if Buffer.Insert_In_Current_Group = 0
           and then not Buffer.Inserting
         then
            End_Group (Buffer.Queue);
            Start_Group (Buffer.Queue);
         end if;
         Emit_New_Cursor_Position (Buffer);
         Cursor_Move_Hook (Buffer);
      end if;

      Buffer.Setting_Mark := False;

   exception
      when E : others =>
         Trace (Traces.Exception_Handle, E);
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

      Update_Highlight_Region (Source_Buffer (Buffer), End_Insert_Iter);

   exception
      when E : others =>
         Trace (Traces.Exception_Handle, E);
   end Insert_Text_Cb;

   -------------------------
   -- Insert_Text_Handler --
   -------------------------

   procedure Insert_Text_Handler
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues)
   is
      Text   : constant Unchecked_String_Access :=
                 To_Unchecked_String (Get_Chars (Nth (Params, 2)));
      Length : constant Integer := Integer (Get_Int (Nth (Params, 3)));
      Start  : Buffer_Line_Type;
      Iter   : Gtk_Text_Iter;
      Number : Buffer_Line_Type := 0;
   begin
      Update_Logical_Timestamp (Buffer);

      Get_Text_Iter (Nth (Params, 1), Iter);

      if Get_Language_Context (Buffer.Lang).Syntax_Highlighting then
         Insert_Text_Cb (Buffer, Iter);
      end if;

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

   exception
      when E : others =>
         Trace (Traces.Exception_Handle, E);
   end Insert_Text_Handler;

   -----------------------
   -- First_Insert_Text --
   -----------------------

   procedure First_Insert_Text
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues)
   is
      Text        : constant Unchecked_String_Access :=
                      To_Unchecked_String (Get_Chars (Nth (Params, 2)));
      Length      : constant Integer := Integer (Get_Int (Nth (Params, 3)));
      Pos         : Gtk_Text_Iter;
      Command     : Editor_Command := Editor_Command (Buffer.Current_Command);
      Line        : Editable_Line_Type;
      User_Action : Action_Type;
      Cursor_Previously_Held : Boolean;
   begin
      Get_Text_Iter (Nth (Params, 1), Pos);
      Line := Get_Editable_Line
        (Buffer, Buffer_Line_Type (Get_Line (Pos) + 1));

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

      if not Lines_Are_Real (Buffer)
        and not Buffer.Inserting
      then
         declare
            Result, Ignore : Boolean;
            pragma Unreferenced (Ignore);
         begin
            Result := Fold_Unfold_Line (Buffer, Line, False);

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

      if Is_Null_Command (Command) then
         Create
           (Command,
            Insertion,
            Source_Buffer (Buffer),
            False, Line,
            Character_Offset_Type (Get_Line_Offset (Pos) + 1));

         Enqueue (Buffer, Command_Access (Command), User_Action);

         Add_Text (Command, Text (1 .. Length));
         Buffer.Current_Command := Command_Access (Command);

      elsif Get_Mode (Command) = Insertion then
         if (User_Action = Insert_Spaces
              and then Buffer.Last_User_Action /= Insert_Spaces)
           or else (User_Action = Insert_Line
                     and then Buffer.Last_User_Action /= Insert_Line)
         then
            End_Action (Buffer);
            Create
              (Command,
               Insertion,
               Source_Buffer (Buffer),
               False, Line,
               Character_Offset_Type (Get_Line_Offset (Pos) + 1));

            Enqueue (Buffer, Command_Access (Command), User_Action);
         end if;

         Add_Text (Command, Text (1 .. Length));
         Buffer.Current_Command := Command_Access (Command);

      else
         End_Action (Buffer);
         Create
           (Command,
            Insertion,
            Source_Buffer (Buffer),
            False,
            Line,
            Character_Offset_Type (Get_Line_Offset (Pos) + 1));
         Enqueue (Buffer, Command_Access (Command), User_Action);
         Add_Text (Command, Text (1 .. Length));
         Buffer.Current_Command := Command_Access (Command);
      end if;

      Buffer.No_Cursor_Move_On_Changes := Cursor_Previously_Held;
   exception
      when E : others =>
         Trace (Traces.Exception_Handle, E);
   end First_Insert_Text;

   ---------------------
   -- Delete_Range_Cb --
   ---------------------

   procedure Delete_Range_Cb
     (Buffer : access Source_Buffer_Record'Class;
      Iter   : Gtk_Text_Iter) is
   begin
      Update_Highlight_Region (Source_Buffer (Buffer), Iter);
   end Delete_Range_Cb;

   --------------------------
   -- Delete_Range_Handler --
   --------------------------

   procedure Delete_Range_Handler
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues)
   is
      Start_Iter : Gtk_Text_Iter;
   begin
      Update_Logical_Timestamp (Buffer);

      Get_Text_Iter (Nth (Params, 1), Start_Iter);

      if Get_Language_Context (Buffer.Lang).Syntax_Highlighting then
         Delete_Range_Cb (Buffer, Start_Iter);
      end if;

      if Buffer.First_Removed_Line > 0 then
         Lines_Remove_Hook_After
           (Buffer, Buffer.First_Removed_Line, Buffer.Last_Removed_Line);
         Buffer.First_Removed_Line := 0;
      end if;

      Character_Added
        (Source_Buffer (Buffer), 8,
         Interactive => not Buffer.Inserting);

   exception
      when E : others =>
         Trace (Traces.Exception_Handle, E);
   end Delete_Range_Handler;

   ---------------------------------
   -- Delete_Range_Before_Handler --
   ---------------------------------

   procedure Delete_Range_Before_Handler
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues)
   is
      Start_Iter          : Gtk_Text_Iter;
      End_Iter            : Gtk_Text_Iter;
      Command             : Editor_Command :=
                              Editor_Command (Buffer.Current_Command);
      Direction           : Direction_Type := Extended;
      Line, Column        : Gint;
      Line_Start          : Gint;
      Column_Start        : Gint;
      Line_End            : Gint;
      Column_End          : Gint;
      Line_Count          : Gint;
      Editable_Line_Start : Editable_Line_Type;
      Editable_Line_End   : Editable_Line_Type;
      First_Buffer_Line_To_Remove : Buffer_Line_Type;
      Last_Buffer_Line_To_Remove  : Buffer_Line_Type;

   begin
      Get_Text_Iter (Nth (Params, 1), Start_Iter);
      Get_Text_Iter (Nth (Params, 2), End_Iter);

      --  Determine the direction mode for the delete action

      Get_Cursor_Position (Buffer, Line, Column);

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

      Editable_Line_Start :=
        Get_Editable_Line (Buffer, Buffer_Line_Type (Line_Start + 1));

      Editable_Line_End :=
        Get_Editable_Line (Buffer, Buffer_Line_Type (Line_End + 1));

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
            pragma Unreferenced (Ignored);
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

      --  Remove the lines in the side information column

      if Line_Start /= Line_End then
         Lines_Remove_Hook_Before
           (Buffer,
            Buffer_Line_Type (Line_Start + 1),
            Buffer_Line_Type (Line_End + 1));

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

               Result := Fold_Unfold_Line (Buffer, Editable_Line_Start, False);
               if Result then
                  --  We have changed the buffer: stop propagation and reemit
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

      Edit_Hook (Buffer);
      Cursor_Move_Hook (Buffer);

      if Buffer.Inserting then
         return;
      end if;

      User_Edit_Hook (Buffer);

      if not Is_Null_Command (Command)
        and then (Get_Mode (Command) /= Deletion
                  or else (Get_Direction (Command) /= Direction))
      then
         End_Action (Buffer);
         Command := Editor_Command (Buffer.Current_Command);

         pragma Assert (Is_Null_Command (Command));
      end if;

      declare
         Slice       : constant UTF8_String :=
           Get_Slice (Buffer, Start_Iter, End_Iter);
         User_Action : Action_Type;
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
               Editable_Line_Start,
               Character_Offset_Type (Column_Start + 1),
               Direction,
               Editable_Line_End,
               Character_Offset_Type (Column + 1));

            Enqueue (Buffer, Command_Access (Command), User_Action);

            Add_Text
              (Command,
               Slice,
               Get_Editable_Line (Buffer, Buffer_Line_Type (Line_Start + 1)),
               Character_Offset_Type (Column_Start + 1));
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

      Buffer.Current_Command := Command_Access (Command);

      if Direction = Extended
        or else Line_Start /= Line_End
      then
         End_Action (Buffer);
      end if;

   exception
      when E : others =>
         Trace (Traces.Exception_Handle, E);
   end Delete_Range_Before_Handler;

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
   -- Get_Last_Status --
   ---------------------

   function Get_Last_Status
     (Buffer : access Source_Buffer_Record'Class) return Status_Type is
   begin
      return Buffer.Current_Status;
   end Get_Last_Status;

   ---------------------
   -- Set_Last_Status --
   ---------------------

   procedure Set_Last_Status
     (Buffer : access Source_Buffer_Record'Class;
      Status : Status_Type) is
   begin
      Buffer.Current_Status := Status;
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

      --  Remove delimiters highlight

      if Buffer.Has_Delimiters_Highlight then
         declare
            From : Gtk_Text_Iter;
            To   : Gtk_Text_Iter;
         begin
            Get_Iter_At_Mark
              (Buffer, From, Buffer.Start_Delimiters_Highlight);
            Get_Iter_At_Mark (Buffer, To, Buffer.End_Delimiters_Highlight);

            Delete_Mark (Buffer, Buffer.Start_Delimiters_Highlight);
            Delete_Mark (Buffer, Buffer.End_Delimiters_Highlight);

            Remove_Tag (Buffer, Buffer.Delimiter_Tag, From, To);
         end;

         Buffer.Has_Delimiters_Highlight := False;
      end if;
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

   -----------------------
   -- Is_Valid_Position --
   -----------------------

   procedure Is_Valid_Index is new
     Generic_Valid_Position (Get_Bytes_In_Line, Set_Line_Index);
   --  Column should be given in bytes, not characters

   procedure Is_Valid_Pos   is new
     Generic_Valid_Position (Get_Chars_In_Line, Set_Line_Offset);
   --  Column should be given in characters, not in bytes

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
         if Line in Buffer.Editable_Lines'Range
           and then Buffer.Editable_Lines (Line).Where = In_Mark
           and then Buffer.Editable_Lines (Line).Text /= null
           and then Buffer.Editable_Lines (Line).Text'Length >= Column - 1
         then
            return True;

         else
            Trace (Me, "invalid buffer line");
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

   ---------------------
   -- Highlight_Slice --
   ---------------------

   procedure Highlight_Slice
     (Buffer     : access Source_Buffer_Record'Class;
      Start_Iter : Gtk.Text_Iter.Gtk_Text_Iter;
      End_Iter   : Gtk.Text_Iter.Gtk_Text_Iter)
   is
      Highlight_Complete : Boolean := False;
      Entity_Start       : Gtk_Text_Iter;
      Entity_End         : Gtk_Text_Iter;
      Tags               : Highlighting_Tags renames Buffer.Syntax_Tags;
      Slice_Offset_Line  : Buffer_Line_Type;
      --  Offset between the beginning of the Source_Buffer and the beginning
      --  of the string slice passed to Parse_Entities.

      Slice_Offset_Column : Gint;
      Result              : Boolean;
      Ignored             : Boolean;
      Entity_Kind         : Language_Entity;
      Slice               : Unchecked_String_Access;
      pragma Suppress (Access_Check, Slice);

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

         if Sloc_Start.Line = 0 then
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

         if Buffer_Line = 0 then
            return False;
         end if;

         End_Buffer_Line :=
           Buffer_Line_Type (Sloc_End.Line) + Slice_Offset_Line;

         --  If the column is 0, the entity really ended on the end of the
         --  previous line.

         if End_Buffer_Line = 0 then
            return False;
         end if;

         Line := Gint (Buffer_Line - 1);
         Is_Valid_Index
           (Source_Buffer (Buffer), Entity_Start, Success, Line, Col);

         if not Success then
            Trace (Me, "invalid position");
            return False;
         end if;

         Line := Gint (End_Buffer_Line - 1);

         if Sloc_End.Column = 0 then
            Get_Iter_At_Line_Index (Buffer, Entity_End, Line, 0);
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

               Is_Valid_Index
                 (Source_Buffer (Buffer), Entity_End, Success, Line, Col);

               if not Success then
                  Trace (Me, "invalid position """
                         & Buffer.Filename.Display_Full_Name & """"
                         & Line'Img & Col'Img);
                  return False;
               end if;

               Forward_Char (Entity_End, Success);

            else
               Is_Valid_Index
                 (Source_Buffer (Buffer), Entity_End, Success, Line, 0);
               if not Success then
                  Trace (Me, "invalid position """
                         & Buffer.Filename.Display_Full_Name & """"
                         & Line'Img & " 0--");
                  return False;
               end if;

               if not Ends_Line (Entity_End) then
                  Forward_To_Line_End (Entity_End, Success);
               end if;
            end if;
         end if;

         Apply_Tag (Buffer, Tags (Entity), Entity_Start, Entity_End);
         return False;
      end Highlight_Cb;

      ---------------------
      -- Local_Highlight --
      ---------------------

      procedure Local_Highlight is
         UTF8   : constant Interfaces.C.Strings.chars_ptr :=
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

         Kill_Highlighting (Buffer, Entity_Start, Entity_End);

         --  Now re-highlight the text...

         Parse_Entities
           (Buffer.Lang,
            Slice (1 .. Length),
            Highlight_Cb'Unrestricted_Access);
         Slice := null;
         g_free (UTF8);
      end Local_Highlight;

   begin
      Copy (Source => Start_Iter, Dest => Entity_Start);
      Copy (Source => End_Iter, Dest => Entity_End);

      --  Highlight from the beginning of the current line, to handle
      --  special language semantics requiring information from previous
      --  characters, such as x.all'address in Ada.

      Set_Line_Offset (Entity_Start, 0);

      if Get_Line (Entity_Start) /= Get_Line (Start_Iter) then
         Copy (Source => Start_Iter, Dest => Entity_Start);
      end if;

      --  Highlight to the end of line, to avoid missing most of the
      --  partial entities (strings, characters, ...).
      --  In case we have started typing a string for instance, that
      --  provides a nice optimization over rehighlighting the whole buffer.

      Forward_To_Line_End (Entity_End, Result);

      --  Search the initial minimum area to re-highlight...

      Entity_Kind := Normal_Text;

      Entity_Kind_Search_Loop :
      for Current_Entity in Standout_Language_Entity loop
         if Has_Tag (Entity_Start, Tags (Current_Entity)) then
            --  This means that we are in a highlighted region. The minimum
            --  region to re-highlight starts from the begining of the current
            --  region to the end of the following region.

            Entity_Kind := Current_Entity;

            Backward_To_Tag_Toggle
              (Entity_Start, Tags (Current_Entity), Result => Ignored);
            Forward_To_Tag_Toggle (Entity_End, Tags (Entity_Kind), Ignored);
            Forward_To_Tag_Toggle (Entity_End, Result => Ignored);

            exit Entity_Kind_Search_Loop;

         elsif Begins_Tag (Entity_End, Tags (Current_Entity))
           or else Ends_Tag (Entity_Start, Tags (Current_Entity))
         then
            --  Case Begins_Tag:
            --    This means that we inserted right at the begining of
            --    a highlighted region... The minimum region to re-highlight
            --    starts from the begining of the previous region to the
            --    end of the current region.
            --  Case Ends_Tag:
            --    This means that we inserted right at the end of a highlighted
            --    region. In this case, the minimum region to re-highlight
            --    starts from the begining of the previous region to the end of
            --    the current region.
            --  In both cases, the processing is the same...

            Entity_Kind := Current_Entity;
            Backward_To_Tag_Toggle (Entity_Start, Result => Ignored);
            Forward_To_Tag_Toggle (Entity_End, Result => Ignored);

            exit Entity_Kind_Search_Loop;
         end if;
      end loop Entity_Kind_Search_Loop;

      if Entity_Kind = Normal_Text then
         --  We are inside a normal text region. Just re-highlight this region

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

   -----------------------
   -- Kill_Highlighting --
   -----------------------

   procedure Kill_Highlighting
     (Buffer : access Source_Buffer_Record'Class;
      From   : Gtk_Text_Iter;
      To     : Gtk_Text_Iter) is
   begin
      for Entity_Kind in Standout_Language_Entity loop
         Remove_Tag (Buffer, Buffer.Syntax_Tags (Entity_Kind), From, To);
      end loop;
   end Kill_Highlighting;

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

      Buffer.First_Highlight_Mark := Create_Mark (Buffer, "", Iter);
      Buffer.Last_Highlight_Mark  := Create_Mark (Buffer, "", Iter, False);

      --  Initialize the line info

      Buffer.Editable_Lines := new Editable_Line_Array (1 .. 1);
      Buffer.Editable_Lines (1) :=
        (Where          => In_Buffer,
         Buffer_Line    => 1,
         Stored_Lines   => Lines_List.Empty_List,
         Block          => null,
         Stored_Editable_Lines => 0);

      --  ??? create line info (above)

      Buffer.Line_Data := new Line_Data_Array (1 .. 1);
      Buffer.Line_Data (1) := New_Line_Data;
      Buffer.Line_Data (1).Editable_Line := 1;

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
      Emit_Stop_By_Name (Buffer, "paste-done");
   end On_Paste_Done;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Buffer : access Source_Buffer_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle;
      Lang   : Language.Language_Access := null)
   is
      Tags         : Gtk_Text_Tag_Table;
      Command      : Check_Modified_State;
      P_Hook       : Preferences_Hook;
      Prj_Hook     : Project_Hook;
      Deleted_Hook : File_Deleted_Hook;
      Renamed_Hook : File_Renamed_Hook;

      use Pango.Enums.Underline_Properties;
   begin
      Gtkada.Text_Buffer.Initialize (Buffer);

      Glib.Object.Initialize_Class_Record
        (Buffer, Signals, Class_Record, "GPSSourceBuffer",
         Signal_Parameters);

      Buffer.Lang := Lang;
      Buffer.Kernel := Kernel;

      --  Save the newly created highlighting tags into the source buffer
      --  tag table.

      Tags := Get_Tag_Table (Buffer);

      --  Preference changed hook

      P_Hook := new Preferences_Changed_Hook_Record'
        (Function_No_Args with Buffer => Source_Buffer (Buffer));
      Add_Hook
        (Kernel, Preferences_Changed_Hook, P_Hook,
         Name => "src_editor_buffer.preferences_changed",
         Watch => GObject (Buffer));
      Execute (P_Hook.all, Kernel);

      --  Project recomputed hook
      Prj_Hook := new Project_Changed_Hook_Record'
        (Function_No_Args with Buffer => Source_Buffer (Buffer));
      Add_Hook
        (Kernel => Kernel,
         Hook   => Project_View_Changed_Hook,
         Func   => Prj_Hook,
         Name   => "src_editor_buffer.on_project_changed",
         Watch  => GObject (Buffer));

      --  File hooks
      Deleted_Hook := new File_Deleted_Hook_Record;
      Deleted_Hook.Buffer := Source_Buffer (Buffer);
      Add_Hook
        (Kernel, GPS.Kernel.File_Deleted_Hook, Deleted_Hook,
         Name  => "project_explorers_files.file_deleted",
         Watch => GObject (Buffer));

      --  Renamed_Hook.Execute will change the buffer's filename:
      --  Add it with Last=>True so that other modules have a chance to react
      --  on the editor before it is renamed
      Renamed_Hook := new File_Renamed_Hook_Record;
      Renamed_Hook.Buffer := Source_Buffer (Buffer);
      Add_Hook
        (Kernel, GPS.Kernel.File_Renamed_Hook, Renamed_Hook,
         Name  => "project_explorers_files.file_renamed",
         Watch => GObject (Buffer),
         Last  => True);

      for Entity_Kind in Standout_Language_Entity'Range loop
         Text_Tag_Table.Add (Tags, Buffer.Syntax_Tags (Entity_Kind));
      end loop;

      --  Create Delimiter_Tag and save it into the source buffer tag table

      Create_Highlight_Line_Tag
        (Buffer.Delimiter_Tag, Delimiter_Color.Get_Pref);
      Text_Tag_Table.Add (Tags, Buffer.Delimiter_Tag);

      --  Create the Hyper Mode Tag

      Gtk_New (Buffer.Hyper_Mode_Tag);
      Set_Property
        (Buffer.Hyper_Mode_Tag,
         Gtk.Text_Tag.Underline_Property,
         Pango_Underline_Single);
      Set_Property (Buffer.Hyper_Mode_Tag, Foreground_Gdk_Property,
                    Hyper_Links_Style.Get_Pref_Fg);

      if Hyper_Links_Style.Get_Pref_Bg
        /= White (Gtk.Widget.Get_Default_Colormap)
      then
         Set_Property (Buffer.Hyper_Mode_Tag, Background_Gdk_Property,
                       Hyper_Links_Style.Get_Pref_Bg);
      end if;

      Add (Tags, Buffer.Hyper_Mode_Tag);

      Gtk_New (Buffer.Non_Editable_Tag);
      Set_Property (Buffer.Non_Editable_Tag, Editable_Property, False);
      Add (Tags, Buffer.Non_Editable_Tag);

      --  Save the insert mark for fast retrievals, since we will need to
      --  access it very often.

      Buffer.Insert_Mark := Get_Insert (Buffer);

      --  Initialize the queue for editor commands

      Buffer.Queue := New_Queue;

      --  Workaround a bug in gtk+ 2.18: when a paste was completed, the
      --  paste-done signal is emitted, and results in scrolling _all_ views
      --  to the cursor location (which breaks the handling of multiple views).
      --  The simplest workaround is simply to not emit that signal at all
      --  (GPS takes care of the scrolling for the active view anyway).
      --  The RH for this signal in gtk+ is:
      --      Add a "paste-done" signal and use it to properly scroll the
      --      view at the end of the pasted text in the case of an async paste

      if Glib.Object.Lookup
        (Get_Type (Buffer), "paste-done") /= Null_Signal_Id
      then
         Buffer_Callback.Connect
           (Buffer, "paste-done",
            Marsh => Buffer_Callback.To_Marshaller (On_Paste_Done'Access),
            After => False);
      end if;

      --  And finally, connect ourselves to the interesting signals

      Weak_Ref (Buffer, Buffer_Destroy'Access);

      Buffer_Callback.Connect
        (Buffer, Signal_Changed, Changed_Handler'Access, After => True);
      Buffer_Callback.Connect
        (Buffer, Signal_Mark_Set,
         Cb => Mark_Set_Handler'Access, After => True);
      Buffer_Callback.Connect
        (Buffer, Signal_Insert_Text,
         Cb => First_Insert_Text'Access);
      Buffer_Callback.Connect
        (Buffer, Signal_Insert_Text,
         Cb => Insert_Text_Handler'Access,
         After => True);
      Buffer_Callback.Connect
        (Buffer, Signal_Delete_Range,
         Cb => Delete_Range_Handler'Access,
         After => True);
      Buffer_Callback.Connect
        (Buffer, Signal_Delete_Range,
         Cb    => Delete_Range_Before_Handler'Access,
         After => False);

      Buffer.Editable_Line_Info_Columns :=
        new Line_Info_Display_Array_Access'(null);

      Initialize_Hook (Buffer);

      --  Create the queue change hook that will be called every
      --  time the state of the queue associated to the buffer changes.

      Create (Command, Source_Buffer (Buffer), Buffer.Queue);

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
   end Initialize;

   -------------------
   -- Is_In_Comment --
   -------------------

   function Is_In_Comment
     (Buffer : Source_Buffer;
      Iter   : Gtk_Text_Iter) return Boolean is
   begin
      return Has_Tag (Iter, Buffer.Syntax_Tags (Comment_Text))
        or else Has_Tag (Iter, Buffer.Syntax_Tags (Annotated_Comment_Text))
        or else Has_Tag (Iter, Buffer.Syntax_Tags (Annotated_Keyword_Text));
   end Is_In_Comment;

   ------------------
   -- Is_In_String --
   ------------------

   function Is_In_String
     (Buffer : Source_Buffer;
      Iter   : Gtk_Text_Iter) return Boolean
   is
      Lang         : constant Language_Access := Buffer.Lang;
      Lang_Context : constant Language_Context_Access :=
                       Get_Language_Context (Lang);
      C1, C2       : Character;
      Pos          : Gtk_Text_Iter;
      Quoted       : Boolean := False;
      Result       : Boolean;
   begin
      Copy (Iter, Pos);
      Backward_Char (Pos, Result);

      if not Result then
         --  Start of buffer
         return Quoted;
      end if;

      C2 := Get_Char (Pos);

      while C2 /= ASCII.LF loop
         Backward_Char (Pos, Result);
         exit when not Result;

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
     (Hook   : Project_Changed_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class)
   is
      Buffer : constant Source_Buffer := Hook.Buffer;
   begin
      --  The project has changed: if this buffer has a file and the language
      --  is unknown, it is possible that the new project knows which language
      --  this file is.

      if Buffer.Lang = Unknown_Lang
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
     (Hook   : Preferences_Changed_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Kernel);
      B       : constant Source_Buffer := Hook.Buffer;
      Timeout : Gint;
      Prev    : Boolean;

      Keyword_Font           : constant Pango.Font.Pango_Font_Description :=
                                 Keywords_Style.Get_Pref_Font;
      Block_Font             : constant Pango.Font.Pango_Font_Description :=
                                 Block_Style.Get_Pref_Font;
      Type_Font              : constant Pango.Font.Pango_Font_Description :=
                                 Type_Style.Get_Pref_Font;
      Comment_Font           : constant Pango.Font.Pango_Font_Description :=
                                 Comments_Style.Get_Pref_Font;
      Annotated_Comment_Font : constant Pango.Font.Pango_Font_Description :=
                                 Annotated_Comments_Style.Get_Pref_Font;
      String_Font            : constant Pango.Font.Pango_Font_Description :=
                                 Strings_Style.Get_Pref_Font;

   begin
      --  Since we update the tags directly, gtk+ will automatically refresh
      --  the source view, we don't need to do anything for this.

      Create_Syntax_Tags
        (B.Syntax_Tags,
         Type_Color                  => Type_Style.Get_Pref_Fg,
         Type_Color_Bg               => Type_Style.Get_Pref_Bg,
         Type_Font_Desc              => Type_Font,
         Block_Color                 => Block_Style.Get_Pref_Fg,
         Block_Color_Bg              => Block_Style.Get_Pref_Bg,
         Block_Font_Desc             => Block_Font,
         Keyword_Color               => Keywords_Style.Get_Pref_Fg,
         Keyword_Color_Bg            => Keywords_Style.Get_Pref_Bg,
         Keyword_Font_Desc           => Keyword_Font,
         Comment_Color               => Comments_Style.Get_Pref_Fg,
         Comment_Color_Bg            => Comments_Style.Get_Pref_Bg,
         Comment_Font_Desc           => Comment_Font,
         Annotated_Comment_Color     => Annotated_Comments_Style.Get_Pref_Fg,
         Annotated_Comment_Color_Bg  => Annotated_Comments_Style.Get_Pref_Bg,
         Annotated_Comment_Font_Desc => Annotated_Comment_Font,
         Character_Color             => Strings_Style.Get_Pref_Fg,
         Character_Color_Bg          => Strings_Style.Get_Pref_Bg,
         Character_Font_Desc         => String_Font,
         String_Color                => Strings_Style.Get_Pref_Fg,
         String_Color_Bg             => Strings_Style.Get_Pref_Bg,
         String_Font_Desc            => String_Font);

      if B.Delimiter_Tag /= null then
         Set_Property
           (B.Delimiter_Tag,
            Background_Gdk_Property,
            Delimiter_Color.Get_Pref);
      end if;

      --  Connect timeout, to handle automatic saving of buffer

      if B.Timeout_Registered then
         Glib.Main.Remove (B.Timeout_Id);
         B.Timeout_Registered := False;
      end if;

      Timeout := Gint (Periodic_Save.Get_Pref);

      if Timeout > 0 then
         B.Timeout_Id := Buffer_Timeout.Timeout_Add
           (Guint (Timeout) * 1000,  Automatic_Save'Access, B.all'Access);
         B.Timeout_Registered := True;
      end if;

      Prev := B.Block_Highlighting;
      B.Block_Highlighting := Block_Highlighting.Get_Pref;

      if Prev /= B.Block_Highlighting then
         Register_Edit_Timeout (B);
      end if;

      Prev := B.Block_Folding;
      B.Block_Folding := Block_Folding.Get_Pref;

      if Prev /= B.Block_Folding then
         Register_Edit_Timeout (B);
      end if;

      if not B.Block_Folding and then Prev then
         Unfold_All (B);
         Remove_Block_Folding_Commands (B);
      end if;

      Prev := B.Parse_Blocks;
      B.Parse_Blocks := B.Block_Folding or else B.Block_Highlighting
        or else Display_Subprogram_Names.Get_Pref;

      if Prev /= B.Parse_Blocks then
         Buffer_Information_Changed (B);
      end if;

      if not Prev and then B.Parse_Blocks then
         Register_Edit_Timeout (B);
      end if;

      B.Auto_Syntax_Check    := Automatic_Syntax_Check.Get_Pref;
      B.Highlight_Delimiters := Highlight_Delimiters.Get_Pref;
      B.Tab_Width            := Gint (String_Utils.Tab_Width);
   end Execute;

   ---------------------
   -- Load_Empty_File --
   ---------------------

   procedure Load_Empty_File (Buffer : access Source_Buffer_Record) is
   begin
      Buffer.Original_Text_Inserted := True;
   end Load_Empty_File;

   ---------------
   -- Load_File --
   ---------------

   procedure Load_File
     (Buffer          : access Source_Buffer_Record;
      Filename        : GNATCOLL.VFS.Virtual_File;
      Lang_Autodetect : Boolean := True;
      Success         : out Boolean)
   is
      procedure Reset_Buffer (Buffer : access Source_Buffer_Record);
      --  Reset all data associated with Buffer

      procedure Set_Trailing_Space_Policy
        (Buffer : access Source_Buffer_Record;
         File   : GNATCOLL.VFS.Virtual_File;
         Trailing_Spaces_Found : Boolean);
      --  Detect and set strip trailing space policy for Buffer

      procedure Set_Trailing_Lines_Policy
        (Buffer  : access Source_Buffer_Record;
         File    : GNATCOLL.VFS.Virtual_File;
         Content : String);
      --  Detect and set strip trailing empty lines policy for Buffer

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
            for J in Buffer.Editable_Lines'Range loop
               if Buffer.Editable_Lines (J).Where = In_Mark then
                  GNAT.Strings.Free (Buffer.Editable_Lines (J).Text);
               end if;
            end loop;

            Unchecked_Free (Buffer.Editable_Lines);
         end if;

         for J in Buffer.Line_Data'Range loop
            if Buffer.Line_Data (J).Enabled_Highlights /= null then
               Unchecked_Free (Buffer.Line_Data (J).Enabled_Highlights);
            end if;
         end loop;

         Unchecked_Free (Buffer.Line_Data);

         Initialize_Hook (Buffer);

         Buffer.First_Removed_Line := 0;
         Buffer.Last_Removed_Line  := 0;
         Buffer.Last_Editable_Line := 1;

         --  Unregister the blocks timeout

         if Buffer.Blocks_Timeout_Registered then
            Glib.Main.Remove (Buffer.Blocks_Timeout);
            Buffer.Blocks_Timeout_Registered := False;
         end if;

         --  Unregister the cursor timeout

         if Buffer.Cursor_Timeout_Registered then
            Glib.Main.Remove (Buffer.Cursor_Timeout);
            Buffer.Cursor_Timeout_Registered := False;
         end if;

         Buffer.Blank_Lines := 0;
         Buffer.Hidden_Lines := 0;
         Buffer.Block_Highlighting_Column := -1;
      end Reset_Buffer;

      -------------------------------
      -- Set_Trailing_Space_Policy --
      -------------------------------

      procedure Set_Trailing_Space_Policy
        (Buffer : access Source_Buffer_Record;
         File   : GNATCOLL.VFS.Virtual_File;
         Trailing_Spaces_Found : Boolean)
      is
         Found   : Boolean;
         Prop    : GPS.Properties.String_Property;
         Default : constant Strip_Trailing_Blanks_Policy :=
           Strip_Blanks.Get_Pref;
      begin
         if File /= GNATCOLL.VFS.No_File then
            GPS.Properties.Get_Property
              (Prop, File, Strip_Blanks_Property_Name, Found);

            if Found then
               Buffer.Strip_Trailing_Blanks :=
                 Boolean'Value (Prop.Value.all);

               return;
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
         else
            Buffer.Strip_Trailing_Blanks := False;
         end if;
      end Set_Trailing_Space_Policy;

      -------------------------------
      -- Set_Trailing_Lines_Policy --
      -------------------------------

      procedure Set_Trailing_Lines_Policy
        (Buffer  : access Source_Buffer_Record;
         File    : GNATCOLL.VFS.Virtual_File;
         Content : String)
      is
         Found   : Boolean;
         Prop    : GPS.Properties.String_Property;
         Default : constant Strip_Trailing_Blanks_Policy :=
           Strip_Lines.Get_Pref;
         Trailing_Lines_Found : Boolean := False;
      begin
         if File /= GNATCOLL.VFS.No_File then
            GPS.Properties.Get_Property
              (Prop, File, Strip_Lines_Property_Name, Found);

            if Found then
               Buffer.Strip_Trailing_Blanks :=
                 Boolean'Value (Prop.Value.all);

               return;
            end if;

            case Default is
               when Always =>
                  Set_Strip_Trailing_Lines (Buffer, True);
               when Never =>
                  Set_Strip_Trailing_Lines (Buffer, False);
               when Autodetect =>
                  for J in reverse Content'Range loop
                     if Content (J) = ASCII.LF then
                        if J /= Content'Last then
                           Trailing_Lines_Found := True;
                           exit;
                        end if;
                     elsif Content (J) /= ' ' and Content (J) /= ASCII.HT then
                        exit;
                     end if;
                  end loop;

                  Set_Strip_Trailing_Lines (Buffer, not Trailing_Lines_Found);
            end case;
         end if;
      end Set_Trailing_Lines_Policy;

      Contents      : GNAT.Strings.String_Access;
      UTF8          : Gtkada.Types.Chars_Ptr;
      Ignore        : aliased Natural;
      Length        : aliased Natural;
      Last          : Natural;
      CR_Found      : Boolean := False;
      NUL_Found     : Boolean := False;
      F, L          : Gtk_Text_Iter;
      Valid         : Boolean;
      Recovering    : Boolean := False;
      First_Invalid : Natural;
      Buttons       : Message_Dialog_Buttons;
      File_Is_New   : constant Boolean := not Buffer.Original_Text_Inserted;

      Trailing_Spaces_Found : Boolean;
   begin
      Success := True;

      if File_Is_New then
         if Is_Regular_File (Autosaved_File (Filename)) then
            Buttons := Message_Dialog
              (Msg            => -"Found an auto-saved file named "
                 & Display_Base_Name (Autosaved_File (Filename)) & ASCII.LF
                 & (-"This usually means that your previous GPS session ")
                 & ASCII.LF
                 & (-"terminated unexpectedly with unsaved changes.")
                 & ASCII.LF & ASCII.LF
                 & (-"Do you want to recover the contents of ")
                 & Display_Base_Name (Filename) & ASCII.LF
                 & (-"from this auto-saved file ?"),
               Dialog_Type    => Warning,
               Buttons        => Button_Yes or Button_No,
               Default_Button => Button_Yes,
               Title          => -"Found auto-saved file",
               Justification  => Justify_Left,
               Parent         => Get_Current_Window (Buffer.Kernel));

            if Buttons = Button_Yes then
               Contents   := Read_File (Autosaved_File (Filename));
               Recovering := True;
            end if;
         end if;
      end if;

      if not Recovering then
         Contents := Read_File (Filename);
      end if;

      if Contents = null then
         Trace (Me, "Load_File: Couldn't read contents of "
                & Filename.Display_Full_Name);
         --  The file does not exist on disk, this is a new file that has never
         --  been saved.
         Buffer.Save_Complete := False;
         Success := False;
         return;
      end if;

      if not File_Is_New then
         Emit_By_Name (Get_Object (Buffer), Signal_Closed & ASCII.NUL);
         File_Closed (Buffer.Kernel, Filename);
         Reset_Buffer (Buffer);
      end if;

      --  Insert the new text

      Buffer.Start_Inserting;

      begin
         if Lang_Autodetect then
            Set_Language
              (Buffer, Get_Language_From_File
                 (Get_Language_Handler (Buffer.Kernel), Filename));
         end if;

         Strip_CR_And_NUL
           (Contents.all, Last, CR_Found, NUL_Found, Trailing_Spaces_Found);

         Set_Charset (Buffer, Get_File_Charset (Filename));

         Set_Trailing_Space_Policy (Buffer, Filename, Trailing_Spaces_Found);

         Set_Trailing_Lines_Policy
           (Buffer, Filename, Contents (Contents'First .. Last));

         if NUL_Found then
            Console.Insert
              (Buffer.Kernel,
               (-"Warning: NUL characters stripped from ")
               & Display_Full_Name (Filename), Mode => Console.Error);
         end if;

         UTF8 := Glib.Convert.Convert
           (Contents (Contents'First .. Last), "UTF-8",
            Buffer.Charset.all,
            Ignore'Unchecked_Access, Length'Unchecked_Access);

         if UTF8 = Gtkada.Types.Null_Ptr then
            --  In case conversion failed, use a default encoding so that we
            --  can at least show something in the editor
            Set_Charset (Buffer, "ISO-8859-1");
            UTF8 := Glib.Convert.Convert
              (Contents (Contents'First .. Last), "UTF-8",
               Buffer.Charset.all,
               Ignore'Unchecked_Access, Length'Unchecked_Access);
         end if;

         GNAT.Strings.Free (Contents);
         UTF8_Validate
           (To_Unchecked_String (UTF8) (1 .. Length), Valid, First_Invalid);

         if not Valid then
            Length := First_Invalid - 1;
            Console.Insert
              (Buffer.Kernel,
               (-"Warning: invalid characters stripped from ")
               & Display_Full_Name (Filename), Mode => Console.Error);
         end if;

         Insert_At_Cursor (Buffer, UTF8, Gint (Length));
         g_free (UTF8);

         --  Highlight the newly inserted text

         if Get_Language_Context (Buffer.Lang).Syntax_Highlighting then
            Get_Bounds (Buffer, F, L);
            Buffer.Highlight_Needed := True;
            Move_Mark (Buffer, Buffer.First_Highlight_Mark, F);
            Move_Mark (Buffer, Buffer.Last_Highlight_Mark, L);
            Process_Highlight_Region (Source_Buffer (Buffer));
         end if;

         if CR_Found then
            Buffer.Line_Terminator := CR_LF;
         else
            Buffer.Line_Terminator := LF;
         end if;

         if Recovering then
            Buffer.Saved_Position := -1;
            Buffer.Restored_From_Autosave := True;
         else
            Buffer.Saved_Position := 0;
         end if;

         Buffer.End_Inserting;
      exception
         when others =>
            Buffer.End_Inserting;
            raise;
      end;

      Buffer.Modified_Auto := False;

      Buffer.Timestamp := File_Time_Stamp (Filename);

      Empty_Queue (Buffer.Queue);
      Buffer.Current_Command := null;

      --  If the file was not new (ie the file was re-loaded from disk after
      --  some edition, this typically happens when editing with another file
      --  editor and choosing to "revert" the file), we need to emit a
      --  File_Edited signal at this point, to force a re-display of line
      --  information.
      --  We also need to emit a File_Closed signal, so that modules can reset
      --  properly the information relative to this file.

      if not File_Is_New then
         File_Edited (Buffer.Kernel, Filename);
      end if;

   exception
      when E : others =>
         Trace (Traces.Exception_Handle, E);
         Success := False;
   end Load_File;

   ------------------------------
   -- Conversion_Error_Message --
   ------------------------------

   function Conversion_Error_Message (Charset : String) return UTF8_String is
   begin
      return -"Error converting from UTF8 to " & Charset;
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
      Buffer_Line : Buffer_Line_Type;
      Force_Write : Boolean := False;
      --  Whether the file mode has been forced to writable
      U_Buffer    : Unbounded_String;
      S           : Ada.Strings.Unbounded.Aux.Big_String_Access;
      Length      : Natural;
      Has_Errors  : Boolean := False;

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
      Success := True;

      if not Internal then
         --  Run the "before_file_saved" hook
         Before_File_Saved (Buffer.Kernel, Filename);
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
                  Str  : constant Src_String := Get_String (Buffer, Line);
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
               Str : Src_String := Get_String (Buffer, Line);
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
                           Get_Message (Error.all),
                           0,
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
         declare
            Ignore : Message_Dialog_Buttons;
            pragma Unreferenced (Ignore);
         begin
            Ignore := Message_Dialog
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
         end;
      end if;

      --  The file could not be opened, check whether it is read-only

      if Is_Regular_File (Filename) and then not Is_Writable (Filename) then
         declare
            Buttons : Message_Dialog_Buttons;
         begin
            if not Force then
               Buttons := Message_Dialog
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
               Force_Write := True;
               Set_Writable (Filename, True);
            end if;
         end;
      end if;

      FD := Write_File (Filename);

      if FD = Invalid_File then
         Insert
           (Buffer.Kernel,
            -"Could not open file for writing: "
            & Display_Full_Name (Filename),
            Mode => GPS.Kernel.Console.Error);
         Success := False;
         return;
      end if;

      Get_String (U_Buffer, S, Length);

      if S /= null then
         Write (FD, S (1 .. Length));
      end if;

      U_Buffer := Null_Unbounded_String;

      Close (FD);

      --  If the file could be saved, emit the corresponding signal.
      --  Emit the signal only if we are really saving to the buffer's file,
      --  not to another filename (which happens for example when doing
      --  automatic saves.

      if not Internal then
         Buffer.Save_Complete := not Has_Errors;

         if Buffer.Filename /= Filename then
            --  If we "save as" the buffer, we emit a closed for the previous
            --  name
            if Buffer.Filename = GNATCOLL.VFS.No_File then
               File_Closed (Buffer.Kernel, Buffer.File_Identifier);
            end if;

            File_Renamed (Handle   => Buffer.Kernel,
                          File     => Buffer.Filename,
                          New_Path => Filename);

            Buffer.Filename := Filename;
         end if;

         File_Saved (Buffer.Kernel, Filename);

         for J in Buffer.Editable_Lines'Range loop
            Buffer_Line := Get_Buffer_Line (Buffer, J);

            if Buffer_Line /= 0
              and then Buffer_Line in Buffer.Line_Data'Range
            then
               Buffer.Line_Data (Buffer_Line).File_Line := File_Line_Type (J);
            end if;
         end loop;

         Buffer.Saved_Position := Get_Position (Buffer.Queue);
         Buffer.Current_Status := Saved;
         Status_Changed (Buffer);
      end if;

      --  If the file mode was forced to writable, reset it to read-only

      if Force_Write then
         Set_Writable (Filename, False);
      end if;

   exception
      when E : others =>
         Trace (Traces.Exception_Handle, E);

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

      Result            : Boolean;
      Original_Filename : constant Virtual_File := Buffer.Filename;
   begin
      if not Internal then
         Remove_Completion;
      end if;

      Internal_Save_To_File
        (Source_Buffer (Buffer), Filename, Internal, Success,
         Force => Force);

      if Success and then not Internal then
         Buffer.Timestamp := File_Time_Stamp (Get_Filename (Buffer));

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
            Delete (Autosaved_File (Original_Filename), Result);
         end if;

         Buffer.Modified_Auto := False;
      end if;

   exception
      when E : others =>
         Trace (Traces.Exception_Handle, E);
   end Save_To_File;

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

         --  Do not try to highlight an empty buffer
         if not Is_End (Buffer_Start_Iter) then
            if Get_Language_Context (Buffer.Lang).Syntax_Highlighting then
               Highlight_Slice (Buffer, Buffer_Start_Iter, Buffer_End_Iter);
            else
               Kill_Highlighting (Buffer, Buffer_Start_Iter, Buffer_End_Iter);
            end if;
         end if;

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
            Ignore := Message_Dialog
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

   -------------------------
   -- Set_Cursor_Position --
   -------------------------

   procedure Set_Cursor_Position
     (Buffer    : access Source_Buffer_Record;
      Line      : Gint;
      Column    : Gint;
      Centering : Centering_Type;
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

      if Centering /= Minimal
        and then not Buffer.Initial_Scroll_Has_Occurred
      then
         Buffer.Cursor_Set_Explicitely := True;
      end if;

      --  At this point, we know that the (Line, Column) position is
      --  valid, so we can safely get the iterator at this position.

      Get_Iter_At_Line_Offset (Buffer, Iter, Line, Column);

      if Extend_Selection then
         Move_Mark (Buffer, Buffer.Insert_Mark, Iter);
      else
         Place_Cursor (Buffer, Iter);
      end if;
   end Set_Cursor_Position;

   procedure Set_Cursor_Position
     (Buffer    : access Source_Buffer_Record;
      Line      : Editable_Line_Type;
      Column    : Character_Offset_Type;
      Centering : Centering_Type := Center;
      Internal  : Boolean;
      Extend_Selection : Boolean := False)
   is
      Buffer_Line : Buffer_Line_Type := Get_Buffer_Line (Buffer, Line);
   begin
      if Buffer.Do_Not_Move_Cursor then
         return;
      end if;

      --  If the line is in a non-visible line, make the line visible

      if Buffer_Line = 0 then
         Unfold_Line (Buffer, Line);
         Buffer_Line := Get_Buffer_Line (Buffer, Line);
      end if;

      Set_Cursor_Position
        (Buffer, Gint (Buffer_Line - 1), Gint (Column - 1),
         Centering, Internal => Internal,
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
      Tab_Len    : constant Gint := Buffer.Tab_Width;
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
            Current := Current + Tab_Len - (Current mod Tab_Len);
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
      Tab_Len : constant Gint := Buffer.Tab_Width;

   begin
      Line   := Get_Line (Iter);
      Column := 0;
      Get_Iter_At_Line_Offset (Buffer, Start, Line, 0);

      --  We have to test Result, in case Iter was pointing after the end of
      --  the buffer.

      while Result and then not Equal (Start, Iter) loop
         if Get_Char (Start) = ASCII.HT then
            Column := Column + Tab_Len - (Column mod Tab_Len);
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

   procedure Get_Iter_Position
     (Buffer : Source_Buffer;
      Iter   : Gtk_Text_Iter;
      Line   : out Editable_Line_Type;
      Column : out Character_Offset_Type) is
   begin
      Line := Get_Editable_Line
        (Buffer, Buffer_Line_Type (Get_Line (Iter) + 1));

      --  Default the Line to 1

      if Line = 0 then
         Line := 1;
      end if;

      Column := Character_Offset_Type (Get_Line_Offset (Iter) + 1);
   end Get_Iter_Position;

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

   function Ends_Word (Iter : Gtk_Text_Iter) return Boolean is
      Next : Gtk_Text_Iter;
      Res  : Boolean;
      N    : Character;
   begin
      --  ??? This implementation should be fixed to take the language into
      --  account
      Copy (Iter, Next);
      Forward_Char (Next, Res);

      if Res then
         N := Get_Char (Next);

         if N = '_' then
            return False;
         else
            return not Is_Alphanumeric (N);
         end if;
      else
         return True;
      end if;
   end Ends_Word;

   -----------------
   -- Starts_Word --
   -----------------

   function Starts_Word (Iter : Gtk_Text_Iter) return Boolean is
      Prev : Gtk_Text_Iter;
      Res  : Boolean;
      P    : Character;
   begin
      --  ??? This implementation should be fixed to take the language into
      --  account
      Copy (Iter, Prev);
      Backward_Char (Prev, Res);

      if Res then
         P := Get_Char (Prev);

         if P = '_' then
            return False;
         else
            return not Is_Alphanumeric (P);
         end if;
      else
         return True;
      end if;
   end Starts_Word;

   -----------------
   -- Inside_Word --
   -----------------

   function Inside_Word (Iter : Gtk.Text_Iter.Gtk_Text_Iter) return Boolean is
   begin
      --  ??? This implementation should be fixed to take the language into
      --  account
      return Gtk.Text_Iter.Inside_Word (Iter) or else Get_Char (Iter) = '_';
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
      while not Ends_Word (End_Iter) loop
         Forward_Char (End_Iter, Success);
         exit when not Success;
      end loop;

      if Success then
         Forward_Char (End_Iter, Success);
      end if;

      while not Starts_Word (Start_Iter) loop
         Backward_Char (Start_Iter, Success);
         exit when not Success;
      end loop;

      Move_Mark_By_Name (Buffer, "selection_bound", Start_Iter);
      Move_Mark_By_Name (Buffer, "insert", End_Iter);
   end Select_Current_Word;

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
         return Get_Slice
           (Source_Buffer (Buffer),
            Get_Line (Start_Iter), Get_Line_Offset (Start_Iter),
            Get_Line (End_Iter), Get_Line_Offset (End_Iter));
      else
         return "";
      end if;
   end Get_Selection;

   ---------------
   -- Get_Slice --
   ---------------

   function Get_Slice
     (Buffer       : Source_Buffer;
      Start_Line   : Gint;
      Start_Column : Gint;
      End_Line     : Gint := -1;
      End_Column   : Gint := -1) return Gtkada.Types.Chars_Ptr
   is
      Start_Iter : Gtk_Text_Iter;
      End_Iter   : Gtk_Text_Iter;
      Success    : Boolean;

   begin
      pragma Assert (Is_Valid_Position (Buffer, Start_Line, Start_Column));
      Get_Iter_At_Line_Offset (Buffer, Start_Iter, Start_Line, Start_Column);

      if End_Line = -1 then
         Get_End_Iter (Buffer, End_Iter);
      else
         pragma Assert (Is_Valid_Position (Buffer, End_Line, End_Column));
         Get_Iter_At_Line_Offset (Buffer, End_Iter, End_Line, End_Column);
      end if;

      --  Needed, since we expect Get_Slice to return a result [start, end] and
      --  Get_Text actually return a [start, end).
      Forward_Char (End_Iter, Success);

      return Get_Text (Buffer, Start_Iter, End_Iter, True);
   end Get_Slice;

   function Get_Slice
     (Buffer       : Source_Buffer;
      Start_Line   : Gint;
      Start_Column : Gint;
      End_Line     : Gint := -1;
      End_Column   : Gint := -1) return String
   is
      Str : constant Gtkada.Types.Chars_Ptr :=
              Get_Slice
                (Buffer, Start_Line, Start_Column, End_Line, End_Column);
      S   : constant String := Value (Str);

   begin
      g_free (Str);
      return S;
   end Get_Slice;

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
      Iter                     : Gtk_Text_Iter;
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

   ----------------------
   -- Selection_Region --
   ----------------------

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
      End_Line   : Buffer_Line_Type) is
   begin
      --  Resynchronize the arrays that need to be synchronized with line
      --  numbers.
      Remove_Lines (Buffer, Start_Line, End_Line);

      --  It is necessary to set the fields Buffer.First_Removed_Line and
      --  Buffer.Last_Removed_Line because the parameters for the
      --  "delete_range" signal are invalid when we connect after the actual
      --  deletion has been done.

      Buffer.First_Removed_Line := Start_Line;
      Buffer.Last_Removed_Line  := End_Line;
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

      --  Reset all high-level grouping when doing undo/redo
      Buffer.Insert_In_Current_Group := 0;

      Start_Group (Buffer.Queue);
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

      --  Reset all high-level grouping when doing undo/redo
      Buffer.Insert_In_Current_Group := 0;

      Start_Group (Buffer.Queue);
   end Undo;

   -------------
   -- Enqueue --
   -------------

   procedure Enqueue
     (Buffer      : access Source_Buffer_Record;
      Command     : Command_Access;
      User_Action : Action_Type) is
   begin
      Buffer.Start_Inserting;

      if Buffer.Saved_Position > Get_Position (Buffer.Queue) then
         Buffer.Saved_Position := -1;
      end if;

      --  Finish the current group and start a new one

      Buffer.Current_Command := null;

      --  Decide whether we should group this action with the previous actions
      if Buffer.Insert_In_Current_Group = 0
        and then (User_Action /= Buffer.Last_User_Action)
      then
         End_Group (Buffer.Queue);
         Start_Group (Buffer.Queue);
      end if;

      if User_Action in No_Action .. Delete_Line then
         Buffer.Last_User_Action := User_Action;
      end if;

      Enqueue (Buffer.Queue, Command);
      Buffer.End_Inserting;
   end Enqueue;

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
     (Buffer : access Source_Buffer_Record; Name : Virtual_File) is
   begin
      Buffer.Filename := Name;

      if not Is_Regular_File (Name) then
         Buffer.Saved_Position := -1;
         Buffer.Save_Complete := False;
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

   ---------------------------
   -- Source_Lines_Revealed --
   ---------------------------

   procedure Source_Lines_Revealed
     (Buffer     : access Source_Buffer_Record;
      Start_Line : Buffer_Line_Type;
      End_Line   : Buffer_Line_Type)
   is
      Context     : Selection_Context := New_Context;
      First, Last : Editable_Line_Type;
   begin
      Set_Context_Information
        (Context,
         Buffer.Kernel,
         Abstract_Module_ID (Src_Editor_Module_Id));

      if Buffer.Filename /= GNATCOLL.VFS.No_File then
         Set_File_Information (Context, (1 => Buffer.Filename));

      elsif Buffer.File_Identifier /= GNATCOLL.VFS.No_File then
         Set_File_Information (Context, (1 => Buffer.File_Identifier));
      end if;

      First := 0;
      Last  := 0;

      --  Find the editable boundaries

      for J in reverse 1 .. Start_Line loop
         First := Get_Editable_Line (Buffer, J);
         exit when First /= 0;
      end loop;

      for J in End_Line .. Buffer.Line_Data'Last loop
         Last := Get_Editable_Line (Buffer, J);
         exit when Last /= 0;
      end loop;

      if First = 0 then
         First := 1;
      end if;

      if Last = 0 then
         Last := Buffer.Last_Editable_Line;
      end if;

      Set_Area_Information (Context, "", Integer (First), Integer (Last));
      GPS.Kernel.Source_Lines_Revealed (Buffer.Kernel, Context);
   end Source_Lines_Revealed;

   ------------------------------
   -- Check_Timestamp_And_Diff --
   ------------------------------

   function Check_Timestamp_And_Diff
     (Buffer : access Source_Buffer_Record'Class;
      Update : Boolean := False) return Boolean
   is
      New_Timestamp : Ada.Calendar.Time;
      Result        : Boolean := True;
   begin
      if Buffer.Filename /= GNATCOLL.VFS.No_File then
         New_Timestamp := File_Time_Stamp (Buffer.Filename);

         --  If the file does not exist, we assume the editor is up-to-date

         Result := New_Timestamp = No_Time
           or else New_Timestamp = Buffer.Timestamp;

         if Update then
            Buffer.Timestamp := New_Timestamp;
         end if;

         --  If the timestamp changed, make sure the contents of the file has
         --  really changed. Otherwise, it might be for instance a VCS that
         --  modified the timestamps, and we don't need to bother the user with
         --  that.
         --  There is unfortunately no function in gtk + to get the number of
         --  bytes in a buffer which we could use to speed up the following.

         if not Result then
            declare
               Txt  : GNAT.Strings.String_Access := Get_String (Buffer);
               Disk : GNAT.Strings.String_Access :=
                        Read_File (Buffer.Filename);
            begin
               if Disk = null then
                  Result := True;
               else
                  Result := Txt.all = Disk.all;
               end if;

               Trace (Me, "Timestamps differ, files equal ? " & Result'Img);
               GNAT.Strings.Free (Disk);
               GNAT.Strings.Free (Txt);
            end;
         end if;
      end if;

      return Result;
   end Check_Timestamp_And_Diff;

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
            File_Closed (Buffer.Kernel, Buffer.Filename);

         elsif Buffer.File_Identifier /= GNATCOLL.VFS.No_File then
            File_Closed (Buffer.Kernel, Buffer.File_Identifier);
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
      if Buffer.Controls_Command = null then
         Buffer.Controls_Command := Set_Controls
           (Buffer.Queue, Undo_Redo_Data.Get (Buffer.Kernel, Undo_Redo_Id));
      end if;
   end Add_Controls;

   ---------------------
   -- Remove_Controls --
   ---------------------

   procedure Remove_Controls (Buffer : access Source_Buffer_Record) is
   begin
      if Buffer.Controls_Command /= null then
         Unset_Controls (Buffer.Controls_Command);
      end if;

      --  No need to Unref the command, this is done in the following call to
      --  Add_Controls.
      Buffer.Controls_Command := null;
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
                 (Message_List.Empty_List,
                  Action => null,
                  Set   => not Columns_Config (K).Every_Line);
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
         if Buffer.Restored_From_Autosave then
            return Modified;
         else
            return Unsaved;
         end if;
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

   ---------------------------
   -- Set_Line_Highlighting --
   ---------------------------

   procedure Set_Line_Highlighting
     (Editor       : access Source_Buffer_Record;
      Line         : Buffer_Line_Type;
      Style        : Style_Access;
      Set          : Boolean;
      Highlight_In : Highlight_Location_Array)
   is
      Category   : Natural;
      Last_Index : Natural;
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

      if Highlight_In (Highlight_Speedbar) then
         Editor.Line_Data (Line).Highlight_Category_Speedbar := Category;
      end if;

      if not Set then
         Editor.Line_Data (Line).Highlight_Category_Speedbar := 0;
      end if;

      if Line < Editor.Line_Data'First
        or else Line > Editor.Line_Data'Last
      then
         Trace (Me, "Wrong line number: " & Image (Integer (Line)));
         return;
      end if;

      Last_Index := Get_Last_Index;

      if Editor.Line_Data (Line).Enabled_Highlights = null then
         --  If we are removing a highlight where no highlight is defined,
         --  we can exit immediately.

         if not Set then
            return;
         end if;

         Editor.Line_Data (Line).Enabled_Highlights :=
           new Boolean_Array (1 .. Last_Index);
         Editor.Line_Data (Line).Enabled_Highlights.all := (others   => False);
         Editor.Line_Data (Line).Highlight_In := Highlight_In;

      elsif Editor.Line_Data (Line).Enabled_Highlights'Last < Last_Index then
         declare
            A : Boolean_Array_Access;
         begin
            A := new Boolean_Array (1 .. Last_Index);
            A (1 .. Editor.Line_Data (Line).Enabled_Highlights'Last)
              := Editor.Line_Data (Line).Enabled_Highlights.all;
            A (Editor.Line_Data (Line).Enabled_Highlights'Last + 1
                 .. Last_Index) := (others => False);
            Unchecked_Free (Editor.Line_Data (Line).Enabled_Highlights);
            Editor.Line_Data (Line).Enabled_Highlights := A;
         end;
      end if;

      --  Do not change Highlight_In unless we are setting the highlighting
      --  category, otherwise removing any category will unhighlight the whole
      --  line.
      if Set then
         Editor.Line_Data (Line).Highlight_In := Highlight_In;
      end if;

      --  Sets highlighting enabled flag only when highlighting of the hole
      --  line in the editor is requested.

      if Highlight_In (Highlight_Editor) then
         Editor.Line_Data (Line).Enabled_Highlights (Category) := Set;

      --  But resets it always when hide of highlighting is requested.

      elsif not Set then
         Editor.Line_Data (Line).Enabled_Highlights (Category) := False;
      end if;

      --  Find out which category has priority for highlighting

      for J in Editor.Line_Data (Line).Enabled_Highlights'Range loop
         if Editor.Line_Data (Line).Enabled_Highlights (J) then
            Editor.Line_Data (Line).Highlight_Category := J;
            Editor.Line_Data (Line).Highlight_Category_Speedbar := J;

            return;
         end if;
      end loop;

      --  If we reach this stage, no highlighting was found, therefore we
      --  remove the current GC.

      Editor.Line_Data (Line).Highlight_Category := 0;
   end Set_Line_Highlighting;

   --------------------------
   -- Add_Line_Higlighting --
   --------------------------

   procedure Add_Line_Highlighting
     (Editor       : access Source_Buffer_Record;
      Line         : Editable_Line_Type;
      Style        : Style_Access;
      Highlight_In : Highlight_Location_Array)
   is
      The_Line : Buffer_Line_Type;
   begin
      if Line = 0 then
         for J in Editor.Line_Data'Range loop
            Set_Line_Highlighting (Editor, J, Style, True, Highlight_In);
         end loop;
      else
         The_Line := Get_Buffer_Line (Editor, Line);

         if The_Line /= 0 then
            Set_Line_Highlighting
              (Editor, The_Line, Style, True, Highlight_In);
         end if;
      end if;

      Line_Highlights_Changed (Editor);
   end Add_Line_Highlighting;

   ------------------------------
   -- Remove_Line_Highlighting --
   ------------------------------

   procedure Remove_Line_Highlighting
     (Editor : access Source_Buffer_Record;
      Line   : Editable_Line_Type;
      Style  : Style_Access)
   is
      The_Line : Buffer_Line_Type;
   begin
      if Editor.In_Destruction then
         return;
      end if;

      if Line = 0 then
         --  This procedure is called by Highlight_Range, but not when Line=0,
         --  so there are no recursive calls here
         Highlight_Range (Editor, Style, 0, 1, 1, True);

         for J in Editor.Line_Data'Range loop
            Set_Line_Highlighting (Editor, J, Style, False, (others => False));
         end loop;
      else
         The_Line := Get_Buffer_Line (Editor, Line);

         if The_Line /= 0 then
            Set_Line_Highlighting
              (Editor, The_Line, Style, False, (others => False));
         end if;
      end if;

      Line_Highlights_Changed (Editor);
   end Remove_Line_Highlighting;

   -------------------------
   -- Get_Highlight_Color --
   -------------------------

   function Get_Highlight_Color
     (Editor  : access Source_Buffer_Record;
      Line    : Buffer_Line_Type;
      Context : Highlight_Location) return Gdk_Color is
   begin
      if Line = 0 then
         return Null_Color;
      end if;

      if Editor.Line_Data /= null
        and then Line <= Editor.Line_Data'Last
        and then Editor.Line_Data (Line).Highlight_In (Context)
      then
         if Context = Highlight_Speedbar then
            return Get_Color
              (Editor.Line_Data (Line).Highlight_Category_Speedbar);
         else
            return Get_Color (Editor.Line_Data (Line).Highlight_Category);
         end if;
      end if;

      return Null_Color;
   end Get_Highlight_Color;

   ---------------
   -- Get_Block --
   ---------------

   function Get_Block
     (Editor        : access Source_Buffer_Record;
      Line          : Editable_Line_Type;
      Force_Compute : Boolean := True) return Block_Record
   is
      Prev : Boolean;
   begin
      if Line = 0 then
         return New_Block;
      end if;

      --  If the editor hasn't calculated block information on-the-fly,
      --  calculate the block information now.

      if Force_Compute then
         --  We need to temporarily change the value of
         --  Editor.Block_Highlighting so that Compute_Blocks parses the
         --  information that we want.

         Prev := Editor.Block_Highlighting;
         Editor.Block_Highlighting := True;
         Compute_Blocks (Editor);
         Editor.Block_Highlighting := Prev;
      end if;

      if Editor.Editable_Lines /= null
        and then Line <= Editor.Editable_Lines'Last
      then
         if Editor.Editable_Lines (Line).Block = null then
            return New_Block;
         else
            return Editor.Editable_Lines (Line).Block.all;
         end if;
      end if;

      return New_Block;
   end Get_Block;

   ---------------------------
   -- Has_Block_Information --
   ---------------------------

   function Has_Block_Information
     (Editor : access Source_Buffer_Record) return Boolean is
   begin
      return Editor.Parse_Blocks;
   end Has_Block_Information;

   -------------------
   -- Should_Indent --
   -------------------

   function Should_Indent (Buffer : Source_Buffer) return Boolean is
      Lang          : constant Language_Access := Get_Language (Buffer);
      Indent_Params : Indent_Parameters;
      Indent_Style  : Indentation_Kind;
   begin
      if not Get_Language_Context (Lang).Can_Indent then
         return False;
      end if;

      Get_Indentation_Parameters
        (Lang         => Lang,
         Params       => Indent_Params,
         Indent_Style => Indent_Style);

      return Indent_Style /= None;
   end Should_Indent;

   --------------------
   -- Do_Indentation --
   --------------------

   function Do_Indentation
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

      return Do_Indentation (Buffer, Iter, End_Pos, Force);
   end Do_Indentation;

   function Do_Indentation
     (Buffer   : Source_Buffer;
      From, To : Gtk_Text_Iter;
      Force    : Boolean := False) return Boolean
   is
      Lang          : constant Language_Access := Get_Language (Buffer);
      Tab_Width     : constant Integer := Integer (Buffer.Tab_Width);
      Indent_Style  : Indentation_Kind;
      End_Pos       : Gtk_Text_Iter;
      Iter          : Gtk_Text_Iter;
      C_Str         : Gtkada.Types.Chars_Ptr := Gtkada.Types.Null_Ptr;
      Slice         : Unchecked_String_Access;
      pragma Suppress (Access_Check, Slice);
      Line          : Gint;
      Current_Line  : Gint;
      Cursor_Line   : Natural;
      Cursor_Offset : Gint;
      Indent_Offset : Integer := 0;
      Result        : Boolean;
      Buffer_Text   : GNAT.Strings.String_Access;
      Indent_Params : Indent_Parameters;
      From_Line     : Editable_Line_Type;
      To_Line       : Editable_Line_Type;
      Offset_Line   : Editable_Line_Type := 0;
      Block         : Block_Record;
      Char          : Gunichar;
      Len           : Integer;

      procedure Local_Format_Buffer
        (Lang          : Language_Access;
         Local_Buffer  : String;
         Replace       : Replace_Text_Callback;
         From, To      : Natural := 0;
         Indent_Params : Indent_Parameters);
      --  Wrapper around Format_Buffer to take into account Indent_Style

      procedure Replace_Text
        (L       : Natural;
         First   : Natural;
         Last    : Natural;
         Replace : String);
      --  Callback for Format_Buffer

      procedure Skip_To_First_Non_Blank (Iter : in out Gtk_Text_Iter);
      --  Move Iter to the first non-blank character on the line

      -----------------------------
      -- Skip_To_First_Non_Blank --
      -----------------------------

      procedure Skip_To_First_Non_Blank (Iter : in out Gtk_Text_Iter) is
         Result : Boolean := True;
         Offset : Gint := 0;
      begin
         Set_Line_Offset (Iter, 0);

         while Result
           and then not Ends_Line (Iter)
           and then Is_Space (Get_Char (Iter))
         loop
            Forward_Char (Iter, Result);
            Offset := Offset + 1;
         end loop;

         Cursor_Offset := Cursor_Offset - Offset;
      end Skip_To_First_Non_Blank;

      -------------------------
      -- Local_Format_Buffer --
      -------------------------

      procedure Local_Format_Buffer
        (Lang          : Language_Access;
         Local_Buffer  : String;
         Replace       : Replace_Text_Callback;
         From, To      : Natural := 0;
         Indent_Params : Indent_Parameters) is
      begin
         if Indent_Style = Language.Simple then
            Format_Buffer
              (Language_Root (Lang.all)'Access,
               Local_Buffer, Replace, From, To,
               Indent_Params, Indent_Offset, Get_Case_Exceptions);
         else
            Format_Buffer
              (Lang, Local_Buffer, Replace, From, To,
               Indent_Params, Indent_Offset, Get_Case_Exceptions);
         end if;
      end Local_Format_Buffer;

      ------------------
      -- Replace_Text --
      ------------------

      procedure Replace_Text
        (L       : Natural;
         First   : Natural;
         Last    : Natural;
         Replace : String)
      is
         Line         : Natural := L;
         Iter         : Gtk_Text_Iter;
         Buffer_Line  : Buffer_Line_Type;
         Start_Column : Character_Offset_Type;
         End_Column   : Character_Offset_Type;
         Replace_Cmd  : Editor_Replace_Slice;

      begin
         if Offset_Line /= 0 then
            Line := L + Natural (Offset_Line) - 1;
         end if;

         Unfold_Line (Buffer, Editable_Line_Type (Line));
         Buffer_Line := Get_Buffer_Line (Buffer, Editable_Line_Type (Line));

         if Buffer_Line = 0 then
            Trace (Me, "invalid buffer line");
            return;
         end if;

         --  Convert byte offsets into char counts

         Get_Iter_At_Line_Index
           (Buffer, Iter,
            Gint (Buffer_Line - 1), Gint (First - 1));
         Start_Column := Character_Offset_Type (Get_Line_Offset (Iter) + 1);
         Set_Line_Index (Iter, Gint (Last - 1));
         End_Column := Character_Offset_Type (Get_Line_Offset (Iter) + 1);

         --  Only replace if needed

         if Get_Text (Buffer,
                      Editable_Line_Type (Line), Start_Column,
                      Editable_Line_Type (Line), End_Column) /= Replace
         then
            if Line = Cursor_Line then
               --  ??? Need to handle folded lines
               Cursor_Offset := Cursor_Offset +
                 Gint (Replace'Length - (End_Column - Start_Column));
            end if;

            Enter_Current_Group (Buffer);

            Create
              (Replace_Cmd,
               Buffer,
               Editable_Line_Type (Line),
               Start_Column,
               Editable_Line_Type (Line),
               End_Column,
               Replace);
            Enqueue (Buffer, Command_Access (Replace_Cmd), External);
            Leave_Current_Group (Buffer);
         end if;
      end Replace_Text;

   begin  --  Do_Indentation
      if not Buffer.Writable then
         End_Action (Buffer);
         return False;
      end if;

      if not Get_Language_Context (Lang).Can_Indent then
         return False;
      end if;

      Get_Indentation_Parameters
        (Lang         => Lang,
         Params       => Indent_Params,
         Indent_Style => Indent_Style);

      --  Allow the "Format selection" command to indent the selected text even
      --  if the indentation style for normal text entry is Simple or None.

      if Force then
         Indent_Style := Extended;
      end if;

      if Indent_Style = None then
         return False;
      end if;

      --  Set proper casing policy, we want to disable the auto-casing here if
      --  we are using the on-the-fly casing policy and we are not in Force
      --  mode.

      if Indent_Params.Casing_Policy in End_Of_Word .. On_The_Fly
        and then not Force
      then
         Indent_Params.Casing_Policy := Disabled;
      end if;

      Buffer.Do_Not_Move_Cursor := True;

      --  Where is the cursor (so we can keep it on the same line)

      Get_Iter_At_Mark (Buffer, Iter, Buffer.Insert_Mark);
      Cursor_Line   := Natural (Get_Line (Iter)) + 1;
      Cursor_Offset := Get_Line_Offset (Iter);

      --  What should we indent (current line or current selection) ?

      Copy (From, Dest => Iter);
      Set_Line_Offset (Iter, 0);
      Current_Line := Get_Line (Iter);
      Copy (To, Dest => End_Pos);

      if not Ends_Line (End_Pos) then
         Forward_To_Line_End (End_Pos, Result);
      end if;

      Line := Get_Line (End_Pos);

      if Active (Indent_On_Block_Info) then
         if Line = Current_Line + 1
           and then Has_Block_Information (Buffer)
         then
            --  Take advantage of the precomputed block information to only
            --  compute indentation inside the current block, since this is
            --  much more efficient, in particular on big files.
            --  ??? Unfortunately, this does not work properly on some
            --  constructs, so disable it by default until these issues
            --  can be resolved (see TODO).

            Block := Get_Block
              (Buffer, Get_Editable_Line (Buffer, Buffer_Line_Type (Line + 1)),
               Force_Compute => False);
            Offset_Line := Block.First_Line;

            Get_Iter_At_Line_Offset
              (Buffer, Iter,
               Gint (Get_Buffer_Line (Buffer, Offset_Line) - 1), 0);

            loop
               exit when Ends_Line (Iter);

               Char := Get_Char (Iter);

               exit when not Is_Space (Char);

               if Char = Character'Pos (ASCII.HT) then
                  Indent_Offset :=
                    Indent_Offset + Tab_Width - (Indent_Offset mod Tab_Width);
               else
                  Indent_Offset := Indent_Offset + 1;
               end if;

               Forward_Char (Iter, Result);
            end loop;
         end if;
      end if;

      End_Action (Buffer);

      if Buffer.Insert_In_Current_Group = 0 then
         Start_Group (Buffer.Queue);
      end if;

      if Lines_Are_Real (Buffer) then
         if Offset_Line /= 0 then
            C_Str := Get_Slice
              (Buffer,
               Gint (Offset_Line) - 1, 0, Line, Get_Line_Offset (End_Pos));
            Slice := To_Unchecked_String (C_Str);
            Local_Format_Buffer
              (Lang,
               Slice (1 .. Integer (Strlen (C_Str))),
               Replace_Text'Unrestricted_Access,
               Integer (Current_Line - Gint (Offset_Line - 1) + 1),
               Integer (Line - Gint (Offset_Line - 1) + 1),
               Indent_Params);

         else
            C_Str := Get_Slice (Buffer, 0, 0, Line, Get_Line_Offset (End_Pos));
            Slice := To_Unchecked_String (C_Str);
            Len   := Integer (Strlen (C_Str));

            if Is_End (End_Pos) then
               --  Special case for end of buffer: we won't get an extra
               --  LF in this case, so need to add it manually.
               --  Note that it is fine to access Slice (Len + 1), since
               --  this is the location of the terminating ASCII.NUL character.

               Len := Len + 1;
               Slice (Len) := ASCII.LF;
            end if;

            Local_Format_Buffer
              (Lang,
               Slice (1 .. Len),
               Replace_Text'Unrestricted_Access,
               Integer (Current_Line + 1),
               Integer (Line + 1),
               Indent_Params);
         end if;

         g_free (C_Str);

      else
         From_Line :=
           Get_Editable_Line (Buffer, Buffer_Line_Type (Current_Line + 1));
         To_Line := Get_Editable_Line (Buffer, Buffer_Line_Type (Line + 1));

         declare
            Line_Cursor : Gint;
         begin
            Line_Cursor := Current_Line + 1;
            while From_Line = 0 and then Line_Cursor < Line loop
               Line_Cursor := Line_Cursor + 1;
               From_Line :=
                 Get_Editable_Line
                   (Buffer, Buffer_Line_Type (Line_Cursor + 1));
            end loop;

            Line_Cursor := Line + 1;
            while To_Line = 0 and then Line_Cursor > Current_Line loop
               Line_Cursor := Line_Cursor - 1;
               To_Line :=
                 Get_Editable_Line
                   (Buffer, Buffer_Line_Type (Line_Cursor + 1));
            end loop;
         end;

         if From_Line /= 0 and then To_Line /= 0 then
            --  There is at least one editable line in the selection of lines
            --  to be reformatted.

            if Offset_Line /= 0 then
               Buffer_Text := Get_Buffer_Lines (Buffer, Offset_Line, To_Line);
               From_Line := From_Line - Offset_Line + 1;
               To_Line := To_Line - Offset_Line + 1;
            else
               Buffer_Text := Get_Buffer_Lines (Buffer, 1, To_Line);
            end if;

            Local_Format_Buffer
              (Lang,
               Buffer_Text.all,
               Replace_Text'Unrestricted_Access,
               Integer (From_Line), Integer (To_Line), Indent_Params);
            GNAT.Strings.Free (Buffer_Text);
         end if;
      end if;

      End_Action (Buffer);
      End_Group (Buffer.Queue);

      --  If the cursor was located before the first non-blank character,
      --  move it to that character. This is more usual for Emacs users,
      --  and more user friendly generally.

      --  ??? Fix handling of folded lines
      Get_Iter_At_Line (Buffer, Iter, Gint (Cursor_Line - 1));
      Skip_To_First_Non_Blank (Iter);

      if Cursor_Offset > 0 then
         Forward_Chars (Iter, Cursor_Offset, Result);
      end if;

      Buffer.Do_Not_Move_Cursor := False;
      Place_Cursor (Buffer, Iter);

      return True;

   exception
      when E : others =>
         --  Stop propagation of exception, since doing nothing
         --  in this callback is harmless.

         End_Group (Buffer.Queue);

         Buffer.Do_Not_Move_Cursor := False;

         if Buffer_Text /= null then
            GNAT.Strings.Free (Buffer_Text);
         end if;

         if C_Str /= Gtkada.Types.Null_Ptr then
            g_free (C_Str);
         end if;

         Trace (Traces.Exception_Handle, E);
         return False;
   end Do_Indentation;

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
                                    Get_Language_Context (Buffer.Lang);

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

      procedure Unchecked_Free is new
        Ada.Unchecked_Deallocation (Pattern_Matcher, Pattern_Matcher_Access);

      --------------
      -- Is_Empty --
      --------------

      function Is_Empty (Line : Src_String) return Boolean is

         function Only_Spaces return Boolean;
         --  Return True is Line contains only spaces or HT

         function Only_Spaces return Boolean is
            Result : Boolean := True;

         begin
            for K in 1 .. Line.Length loop
               if Line.Contents (K) /= ' '
                 or else Line.Contents (K) /= ASCII.HT
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

      begin  --  Refill_Comments
         for K in From_Line .. To_Line loop
            Line := Get_String (Buffer, K);

            --  Handle continuation of multi-line comment

            if In_ML_Comment then

               --  Empty line in multi-line comment; continue acumulating
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
                     --  is time to refill the acumulated comment.

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
            Trace (Traces.Exception_Handle, E);
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
            Line := Get_String (Buffer, K);

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
            Trace (Traces.Exception_Handle, E);
      end Refill_Plain_Text;

      -------------------
      -- Scan_Commment --
      -------------------

      procedure Scan_Comment
        (Line : String;
         Kind : out Comment_Kind;
         Last : out Natural)
      is
         Matches        : Match_Array (0 .. 0);
         Single_Line_BC : constant GNAT.Strings.String_Access :=
                            Lang_Context.Syntax.New_Line_Comment_Start;
      begin
         Kind := None;
         Last := 0;

         --  If the language supports single-line comments then search for its
         --  pattern in the current line

         if Single_Line_BC_Len /= 0 then
            Match (Single_Line_BC_Pattern.all, Line, Matches);

            if Matches (0) /= No_Match then
               pragma Assert (Matches (0).First = Line'First);

               --  Single-line comments that end with the characters sequence
               --  used to begin the comment are not considered comments to be
               --  refilled. Such comments are probably part of the copyright
               --  header, which we don't want to reformat. For example:

               --      This line is an example of a non-refilled comment    --

               if Line (Line'Last - Single_Line_BC_Len + 1 .. Line'Last)
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

      --  Local variables

      From, To  : Gtk_Text_Iter;
      From_Line : Editable_Line_Type;
      To_Line   : Editable_Line_Type;
      Success   : Boolean;

   --  Start of processing for Do_Refill

   begin
      if not Buffer.Writable then
         End_Action (Buffer);
         return False;
      end if;

      Start_Group (Buffer.Queue);
      Enter_Current_Group (Buffer);

      Get_Selection_Bounds (Buffer, From, To, Success);

      --  No selection; get the current position

      if not Success then
         declare
            Dummy : Character_Offset_Type;
         begin
            Get_Cursor_Position (Buffer, From_Line, Dummy);
            To_Line := From_Line;
         end;

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

      declare
         Start_Comment_Pattern : constant String := "^\s*";
         --  Start of line, followed by zero or more spaces

         End_Comment_Pattern   : constant String := "\s\s?[^\s]";
         --  One or two spaces, followed by a non-space. If there are more than
         --  two spaces after the command marker, then we don't recognize it as
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
                            Lang_Context.Syntax.New_Line_Comment_Start;
      begin
         --  Handle single-line comments

         if Single_Line_BC /= null then
            Single_Line_BC_Len := Single_Line_BC'Length;

            S := To_Unbounded_String (Start_Comment_Pattern);
            Append (S, Filter_Metachars (Single_Line_BC.all));
            Append (S, End_Comment_Pattern);

            Single_Line_BC_Pattern :=
              new Pattern_Matcher'(Compile (To_String (S)));
         end if;

         --  Handle multi-line comments

         if Lang_Context.Syntax.Comment_Start /= null then
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
      end;

      --  Refill comments in selected lines

      if Single_Line_BC_Pattern /= null
        or else Multiple_Lines_BC_Pattern /= null
      then
         Refill_Comments (From_Line, To_Line);

      --  This is a text file

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
      End_Group (Buffer.Queue);
      Leave_Current_Group (Buffer);
      return True;
   end Do_Refill;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Context : access Src_Editor_Action_Context;
      Ctxt    : Selection_Context) return Boolean
   is
      pragma Unreferenced (Context);
   begin
      --  Do not check the current focus widget ourselves. Instead, we know
      --  it has been properly checked when the context was created, and we
      --  just check the current module from there.
      return not Completion_Module.In_Smart_Completion
        and then GPS.Kernel.Modules.Module_ID (Get_Creator (Ctxt)) =
        Src_Editor_Module_Id;
   end Filter_Matches_Primitive;

   ----------
   -- Free --
   ----------

   procedure Free
     (Buffer        : access Source_Buffer_Record;
      X             : in out Line_Info_Width;
      Free_Messages : Boolean)
   is
   begin
      Free (X.Action);

      if Free_Messages then

         --  We are about to remove messages from the editor. For safety, do
         --  not iterate on the messages list, since removing messages will
         --  affect this list. Instead, construct a Message_Array with all
         --  messages. This is also potentially more efficient, as
         --  Remove_Messages works on an array of messages.
         declare
            M : Message_Array (1 .. Integer (X.Messages.Length));
            J : Natural := 1;
            C : Message_List.Cursor;
         begin
            C := X.Messages.First;

            while Message_List.Has_Element (C) loop
               M (J) := Message_List.Element (C);
               J := J + 1;
               Message_List.Next (C);
            end loop;

            Remove_Messages (Buffer, M);
         end;

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
   begin
      Get_Iter_At_Line_Offset
        (Buffer,
         Iter,
         Gint (Get_Buffer_Line (Buffer, Start_Line) - 1),
         Gint (Start_Column - 1));

      for J in 1 .. Length loop
         Forward_Char (Iter, Success);
         exit when not Success;

         while Buffer.Line_Data
           (Buffer_Line_Type (Get_Line (Iter) + 1)).Editable_Line = 0
         loop
            Forward_Char (Iter, Success);
            exit when not Success;
         end loop;
      end loop;

      End_Line := Get_Editable_Line
        (Buffer, Buffer_Line_Type (Get_Line (Iter) + 1));

      End_Column := Character_Offset_Type (Get_Line_Offset (Iter) + 1);
   end Forward_Position;

   --------------
   -- Get_Text --
   --------------

   function Get_Text
     (Buffer       : access Source_Buffer_Record;
      Start_Line   : Editable_Line_Type;
      Start_Column : Character_Offset_Type;
      End_Line     : Editable_Line_Type := 0;
      End_Column   : Character_Offset_Type := 0) return String
   is
      Start_Iter, End_Iter : Gtk_Text_Iter;
      Start_End, End_Begin : Gtk_Text_Iter;
      Result               : Boolean;
      Real_End_Line        : Editable_Line_Type := End_Line;
   begin
      if not Lines_Are_Real (Buffer) then
         Unfold_Line (Buffer, Start_Line);

         if End_Line /= 0 then
            Unfold_Line (Buffer, End_Line);
         end if;

         --  ??? Not sufficient.
         --  middle lines won't be unfolded in the following case:
         --     start_line (folded)
         --     ...
         --     middle_line (folded)
         --     ...
         --     end_line (folded)
      end if;

      Get_Iter_At_Line_Offset
        (Buffer,
         Start_Iter,
         Gint (Get_Buffer_Line (Buffer, Start_Line) - 1),
         Gint (Start_Column - 1));

      if End_Line /= 0 then
         Get_Iter_At_Line_Offset
           (Buffer,
            End_Iter,
            Gint (Get_Buffer_Line (Buffer, End_Line) - 1),
            Gint (End_Column - 1));
      else
         Real_End_Line := Buffer.Last_Editable_Line;
         Get_End_Iter (Buffer, End_Iter);
      end if;

      if not Lines_Are_Real (Buffer)
        and then Real_End_Line - Start_Line > 1
      then
         --  If we are getting multiple lines of text, we need to get the
         --  potential hidden lines.

         Copy (Start_Iter, Start_End);

         if not Ends_Line (Start_End) then
            Forward_To_Line_End (Start_End, Result);

            if not Result then
               Copy (Start_Iter, Start_End);
            end if;
         end if;

         Copy (End_Iter, End_Begin);
         Set_Line_Offset (End_Begin, 0);

         declare
            A : GNAT.Strings.String_Access :=
              Get_Buffer_Lines (Buffer, Start_Line + 1, Real_End_Line - 1);
            S : constant String :=
              Get_Text (Buffer, Start_Iter, Start_End) & ASCII.LF
                & A.all & Get_Text (Buffer, End_Begin, End_Iter);
         begin
            GNAT.Strings.Free (A);
            return S;
         end;
      else
         return Get_Text (Buffer, Start_Iter, End_Iter, True);
      end if;
   end Get_Text;

   -----------------------------
   -- Update_Highlight_Region --
   -----------------------------

   procedure Update_Highlight_Region
     (Buffer : Source_Buffer;
      Iter   : Gtk_Text_Iter)
   is
      First_Mark_Iter : Gtk_Text_Iter;
      Last_Mark_Iter  : Gtk_Text_Iter;

   begin
      if not Buffer.Highlight_Needed then
         Buffer.Highlight_Needed := True;
         Move_Mark (Buffer, Buffer.First_Highlight_Mark, Iter);
         Move_Mark (Buffer, Buffer.Last_Highlight_Mark, Iter);

      else
         Get_Iter_At_Mark
           (Buffer, First_Mark_Iter, Buffer.First_Highlight_Mark);
         Get_Iter_At_Mark
           (Buffer, Last_Mark_Iter, Buffer.Last_Highlight_Mark);

         if Get_Offset (First_Mark_Iter) > Get_Offset (Iter) then
            Move_Mark (Buffer, Buffer.First_Highlight_Mark, Iter);
         end if;

         if Get_Offset (Last_Mark_Iter) < Get_Offset (Iter) then
            Move_Mark (Buffer, Buffer.Last_Highlight_Mark, Iter);
         end if;
      end if;

      if not Buffer.Inserting then
         Process_Highlight_Region (Buffer);
      end if;
   end Update_Highlight_Region;

   ------------------------------
   -- Process_Highlight_Region --
   ------------------------------

   procedure Process_Highlight_Region (Buffer : Source_Buffer) is
      Start_Iter  : Gtk_Text_Iter;
      End_Iter    : Gtk_Text_Iter;
   begin
      if not Buffer.Highlight_Needed then
         return;
      end if;

      Get_Iter_At_Mark (Buffer, Start_Iter, Buffer.First_Highlight_Mark);
      Get_Iter_At_Mark (Buffer, End_Iter, Buffer.Last_Highlight_Mark);

      Highlight_Slice (Buffer, Start_Iter, End_Iter);
      Buffer.Highlight_Needed := False;
   end Process_Highlight_Region;

   -------------------------
   -- Refresh_Side_Column --
   -------------------------

   procedure Refresh_Side_Column (Buffer : access Source_Buffer_Record) is
   begin
      Recalculate_Side_Column_Width (Buffer);
      Side_Column_Configuration_Changed (Buffer);
      Side_Column_Changed (Buffer);
   end Refresh_Side_Column;

   ------------------------------
   -- Position_Set_Explicitely --
   ------------------------------

   function Position_Set_Explicitely
     (Buffer : access Source_Buffer_Record;
      Reset  : Boolean) return Boolean
   is
      Set : constant Boolean := Buffer.Cursor_Set_Explicitely;
   begin
      if Reset then
         Buffer.Cursor_Set_Explicitely := False;
         Buffer.Initial_Scroll_Has_Occurred := True;
      end if;

      return Set;
   end Position_Set_Explicitely;

   ----------------------------------
   -- Set_Position_Set_Explicitely --
   ----------------------------------

   procedure Set_Position_Set_Explicitely
     (Buffer : access Source_Buffer_Record) is
   begin
      if not Buffer.Initial_Scroll_Has_Occurred then
         Buffer.Cursor_Set_Explicitely := True;
      end if;
   end Set_Position_Set_Explicitely;

   ---------------------------
   -- In_Destruction_Is_Set --
   ---------------------------

   function In_Destruction_Is_Set
     (Buffer : access Source_Buffer_Record'Class) return Boolean is
   begin
      return Buffer.In_Destruction;
   end In_Destruction_Is_Set;

   --------------------------
   -- Get_Subprogram_Block --
   --------------------------

   function Get_Subprogram_Block
     (Editor : access Source_Buffer_Record;
      Line   : Src_Editor_Buffer.Editable_Line_Type) return Block_Record
   is
      Empty_Block : constant Block_Record :=
        (0, 0, 0, 0, 0, No_Symbol, Language.Cat_Unknown, Null_Color);
      L           : Editable_Line_Type;
      New_L       : Editable_Line_Type;
      Block       : Block_Record;
   begin
      L := Line;

      Block := Get_Block (Editor, Line, Force_Compute => True);

      if Block.Block_Type = Cat_Unknown
        and then Block.Indentation_Level = 0
      then
         return Empty_Block;
      end if;

      while L > 1 loop
         if Block.Block_Type in Namespace_Category
           or else Block.Block_Type in Subprogram_Category
         then
            if Block.Name /= No_Symbol then
               return Block;
            else
               return Empty_Block;
            end if;
         end if;

         if Block.First_Line > 1 then
            New_L := Block.First_Line - 1;

            --  At this point, we have to check that we are not stuck on
            --  the same line, this can happen when block information is not
            --  up-to-date.

            if New_L < L then
               L := New_L;
            else
               L := L - 1;
            end if;
         else
            exit;
         end if;

         Block := Get_Block (Editor, L, Force_Compute => False);
      end loop;

      return Empty_Block;
   end Get_Subprogram_Block;

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

   function Convert (C : Natural) return Visible_Column_Type is
   begin
      return Visible_Column_Type (C);
   end Convert;

   function Convert (C : Natural) return Character_Offset_Type is
   begin
      return Character_Offset_Type (C);
   end Convert;

   function Convert (L : Natural) return Editable_Line_Type is
   begin
      return Editable_Line_Type (L);
   end Convert;

   function Convert (L : Editable_Line_Type) return Natural is
   begin
      return Natural (L);
   end Convert;

   function Convert (C : Character_Offset_Type) return Natural is
   begin
      return Natural (C);
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
      J       : Natural;
   begin
      if Buffer.Editable_Lines = null
        or else Line not in Buffer.Editable_Lines'Range
      then
         return Current;
      end if;

      case Buffer.Editable_Lines (Line).Where is
         when In_Buffer =>
            Get_Iter_At_Line
              (Buffer, Iter,
               Gint (Buffer.Editable_Lines (Line).Buffer_Line - 1));

            while Result and then Count < Column loop
               if Get_Char (Iter) = ASCII.HT then
                  Current := Current + (Tab_Len - (Current - 1) mod Tab_Len);
               else
                  Current := Current + 1;
               end if;

               Count := Count + 1;
               Forward_Char (Iter, Result);
            end loop;

         when In_Mark =>
            if Buffer.Editable_Lines (Line).Text /= null then
               J := Buffer.Editable_Lines (Line).Text'First;

               while J <  Buffer.Editable_Lines (Line).Text'Last
                 and then Count < Column
               loop
                  if Buffer.Editable_Lines (Line).Text (J) = ASCII.HT then
                     Current := Current +
                       (Tab_Len - (Current - 1) mod Tab_Len);
                  else
                     Current := Current + 1;
                  end if;

                  Count := Count + 1;
                  J := UTF8_Next_Char
                    (Buffer.Editable_Lines (Line).Text.all, J);
               end loop;
            end if;
      end case;

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
      Iter    : Gtk_Text_Iter;
      Current : Visible_Column_Type := 1;
      Count   : Character_Offset_Type := 1;
      Result  : Boolean := True;
      Tab_Len : constant Visible_Column_Type :=
                  Visible_Column_Type (Buffer.Tab_Width);
      J       : Natural;

   begin
      if Column = 0 or else Line not in Buffer.Editable_Lines'Range then
         return 0;
      end if;

      case Buffer.Editable_Lines (Line).Where is
         when In_Buffer =>
            Get_Iter_At_Line
              (Buffer, Iter,
               Gint (Buffer.Editable_Lines (Line).Buffer_Line - 1));

            while Result and then Current < Column  loop
               if Get_Char (Iter) = ASCII.HT then
                  Current := Current + Tab_Len - (Current - 1) mod Tab_Len;
               else
                  Current := Current + 1;
               end if;

               Count := Count + 1;
               Forward_Char (Iter, Result);
            end loop;

         when In_Mark =>
            if Buffer.Editable_Lines (Line).Text /= null then
               J := Buffer.Editable_Lines (Line).Text'First;

               while J <  Buffer.Editable_Lines (Line).Text'Last
                 and then Current < Column
               loop
                  if Buffer.Editable_Lines (Line).Text (J) = ASCII.HT then
                     Current := Current +
                       (Tab_Len - (Current - 1) mod Tab_Len);
                  else
                     Current := Current + 1;
                  end if;

                  Count := Count + 1;
                  J := UTF8_Next_Char
                    (Buffer.Editable_Lines (Line).Text.all, J);
               end loop;
            end if;
      end case;

      return Count;
   end Collapse_Tabs;

   --------------------
   -- Get_Constructs --
   --------------------

   function Get_Constructs
     (Buffer         : access Source_Buffer_Record;
      Required_Level : Constructs_State_Type) return Construct_List
   is
      Text : GNAT.Strings.String_Access;
   begin
      if Buffer.Constructs_State >= Required_Level then
         return Buffer.Constructs;
      end if;

      Free (Buffer.Constructs);

      Text := Get_String (Source_Buffer (Buffer));
      Parse_Constructs (Buffer.Lang, Text.all, Buffer.Constructs);
      GNAT.Strings.Free (Text);

      Buffer.Constructs_State := Exact;

      if Buffer.Constructs_Timestamp = Natural'Last then
         Buffer.Constructs_Timestamp := 0;
      else
         Buffer.Constructs_Timestamp := Buffer.Constructs_Timestamp + 1;
      end if;

      return Buffer.Constructs;
   end Get_Constructs;

   --------------------------
   -- Get_Constructs_State --
   --------------------------

   function Get_Constructs_State
     (Buffer : access Source_Buffer_Record) return Constructs_State_Type is
   begin
      return Buffer.Constructs_State;
   end Get_Constructs_State;

   ------------------------------
   -- Get_Constructs_Timestamp --
   ------------------------------

   function Get_Constructs_Timestamp
     (Buffer : access Source_Buffer_Record) return Natural is
   begin
      return Buffer.Constructs_Timestamp;
   end Get_Constructs_Timestamp;

   ------------------
   -- Set_Writable --
   ------------------

   procedure Set_Writable
     (Buffer   : not null access Source_Buffer_Record;
      Writable : Boolean;
      Explicit : Boolean) is
   begin
      Buffer.Writable := Writable;
      Buffer.Explicit_Writable_Set := Explicit;
   end Set_Writable;

   ------------------
   -- Get_Writable --
   ------------------

   function Get_Writable
     (Buffer : not null access Source_Buffer_Record) return Boolean is
   begin
      return Buffer.Writable;
   end Get_Writable;

   -------------------------------
   -- Get_Explicit_Writable_Set --
   -------------------------------

   function Get_Explicit_Writable_Set
     (Buffer : not null access Source_Buffer_Record) return Boolean is
   begin
      return Buffer.Explicit_Writable_Set;
   end Get_Explicit_Writable_Set;

   --------------------------
   -- Prevent_CR_Insertion --
   --------------------------

   procedure Prevent_CR_Insertion
     (Buffer  : access Source_Buffer_Record'Class;
      Prevent : Boolean := True) is
   begin
      Buffer.Prevent_CR_Insertion := Prevent;
   end Prevent_CR_Insertion;

   -------------------------
   -- Enter_Current_Group --
   -------------------------

   procedure Enter_Current_Group
     (Buffer : access Source_Buffer_Record'Class) is
   begin
      Buffer.Insert_In_Current_Group := Buffer.Insert_In_Current_Group + 1;
   end Enter_Current_Group;

   -------------------------
   -- Leave_Current_Group --
   -------------------------

   procedure Leave_Current_Group
     (Buffer : access Source_Buffer_Record'Class) is
   begin
      Buffer.Insert_In_Current_Group := Buffer.Insert_In_Current_Group - 1;
   end Leave_Current_Group;

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
      Iter        : Child_Iterator := First_Child (Get_MDI (Kernel));
      Child_Count : Natural := 0;
   begin
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
      if Buffer.Index < Buffer.Typed_Chars'Last then
         Buffer.Index := Buffer.Index + 1;
         Buffer.Typed_Chars (Buffer.Index) := C;
      end if;
   end Add_Typed_Char;

   ----------------------------
   -- Delete_Last_Typed_Char --
   ----------------------------

   procedure Delete_Last_Typed_Char
     (Buffer : access Source_Buffer_Record'Class) is
   begin
      if Buffer.Index >= Buffer.Typed_Chars'First then
         Buffer.Index := Buffer.Index - 1;
      end if;
   end Delete_Last_Typed_Char;

   -----------------------
   -- Clear_Typed_Chars --
   -----------------------

   procedure Clear_Typed_Chars (Buffer : access Source_Buffer_Record'Class) is
   begin
      Buffer.Index := 0;
   end Clear_Typed_Chars;

   ---------------------
   -- Get_Typed_Chars --
   ---------------------

   function Get_Typed_Chars
     (Buffer : access Source_Buffer_Record'Class;
      N      : Positive) return UTF8_String is
   begin
      if N > Buffer.Index then
         --  No enough characters in buffer, we cannot support conservative
         --  casing.
         return "";

      else
         declare
            S    : UTF8_String (1 .. N * 6);
            --  An UTF-8 character expand of max 6 bytes
            Last : Natural := 0;
         begin
            for K in Buffer.Index - N + 1 .. Buffer.Index loop
               Unichar_To_UTF8
                 (Buffer.Typed_Chars (K), S (Last + 1 .. S'Last), Last);
            end loop;
            return S (1 .. Last);
         end;
      end if;
   end Get_Typed_Chars;

   -------------------
   -- Get_Timestamp --
   -------------------

   function Get_Timestamp
     (Buffer : access Source_Buffer_Record'Class) return Integer is
   begin
      return Buffer.Logical_Timestamp;
   end Get_Timestamp;

   ------------------------------
   -- Update_Logical_Timestamp --
   ------------------------------

   procedure Update_Logical_Timestamp
     (Buffer : access Source_Buffer_Record'Class) is
   begin
      if Buffer.Logical_Timestamp = Integer'Last then
         Buffer.Logical_Timestamp := 0;
      else
         Buffer.Logical_Timestamp := Buffer.Logical_Timestamp + 1;
      end if;
   end Update_Logical_Timestamp;

   ---------------
   -- Inserting --
   ---------------

   function Inserting
     (Buffer : access Source_Buffer_Record'Class) return Boolean is
   begin
      return Buffer.Inserting_Count >= 1;
   end Inserting;

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
      Enter_Current_Group (Buffer);
   end Start_Undo_Group;

   -----------------------
   -- Finish_Undo_Group --
   -----------------------

   procedure Finish_Undo_Group (Buffer : access Source_Buffer_Record'Class) is
   begin
      End_Action (Buffer);
      End_Group (Buffer.Queue);
      Leave_Current_Group (Buffer);
   end Finish_Undo_Group;

end Src_Editor_Buffer;
