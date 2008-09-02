-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                Copyright (C) 2001-2008, AdaCore                   --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
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

with Ada.Calendar;                        use Ada.Calendar;
with Ada.Characters.Handling;             use Ada.Characters.Handling;
with Ada.Strings.Unbounded;               use Ada.Strings.Unbounded;
pragma Warnings (Off);
with Ada.Strings.Unbounded.Aux;           use Ada.Strings.Unbounded.Aux;
pragma Warnings (On);
with GNATCOLL.Traces;                     use GNATCOLL.Traces;
with GNATCOLL.Utils;                      use GNATCOLL.Utils;
with GNATCOLL.VFS;                        use GNATCOLL.VFS;
with Interfaces.C.Strings;                use Interfaces.C.Strings;
with System.Address_Image;

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
with Gtk.Main;                            use Gtk.Main;
with Gtk.Text_Buffer;                     use Gtk.Text_Buffer;
with Gtk.Text_Iter;                       use Gtk.Text_Iter;
with Gtk.Text_Mark;                       use Gtk.Text_Mark;
with Gtk.Text_Tag;                        use Gtk.Text_Tag;
with Gtk.Text_Tag_Table;                  use Gtk.Text_Tag_Table;

with Gtkada.Dialogs;                      use Gtkada.Dialogs;
with Gtkada.Types;                        use Gtkada.Types;

with Pango.Font;                          use Pango.Font;

with Casing_Exceptions;                   use Casing_Exceptions;
with Case_Handling;                       use Case_Handling;
with Commands.Editor;                     use Commands.Editor;
with Commands.Controls;                   use Commands.Controls;
with Completion_Module;                   use Completion_Module;
with GPS.Intl;                            use GPS.Intl;
with GPS.Location_View;                   use GPS.Location_View;
with GPS.Kernel;                          use GPS.Kernel;
with GPS.Kernel.Charsets;                 use GPS.Kernel.Charsets;
with GPS.Kernel.Console;                  use GPS.Kernel.Console;
with GPS.Kernel.Contexts;                 use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;                    use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;                      use GPS.Kernel.MDI;
with GPS.Kernel.Modules;                  use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;              use GPS.Kernel.Preferences;
with GPS.Kernel.Properties;               use GPS.Kernel.Properties;
with GPS.Kernel.Project;                  use GPS.Kernel.Project;
with GPS.Kernel.Scripts;                  use GPS.Kernel.Scripts;
with Language;                            use Language;
with Language.Unknown;                    use Language.Unknown;
with Language_Handlers;                   use Language_Handlers;
with Src_Editor_Buffer.Blocks;
with Src_Editor_Buffer.Line_Information;
with Src_Editor_Buffer.Hooks;             use Src_Editor_Buffer.Hooks;
with Src_Editor_Buffer.Text_Handling;     use Src_Editor_Buffer.Text_Handling;
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

   Me                        : constant Trace_Handle :=
     Create ("Source_Editor_Buffer");
   Indent_On_Block_Info      : constant Trace_Handle :=
     Create ("Source_Editor_Buffer.Indent_On_Block_Info", Default => Off);

   Buffer_Recompute_Interval : constant Guint32 := 200;
   --  The interval at which to check whether the buffer should be reparsed,
   --  in milliseconds.

   Cursor_Stop_Interval      : constant Guint32 := 100;
   --  The interval after which we consider that the cursor has stopped

   Buffer_Recompute_Delay    : constant Duration := 1.0;
   --  The delay between the last edit and the re-parsing of the buffer,
   --  in seconds.

   Cursor_Reactivity_Delay   : constant Duration := 0.2;
   --  The timeout between the last time the cursor moves and the time it is
   --  considered as having stopped.

   package Buffer_Timeout is new Gtk.Main.Timeout (Source_Buffer);

   type Delimiter_Type is (Opening, Closing);

   function Strlen
     (Str : Interfaces.C.Strings.chars_ptr) return Interfaces.C.size_t;
   pragma Import (C, Strlen);

   Delimiters : constant array (1 .. 3, Delimiter_Type'Range) of Character
     := (('(', ')'),
         ('[', ']'),
         ('{', '}'));
   --  ??? Should we get that from the language ?

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
           7 => New_String (String (Signal_Buffer_Information_Changed)));
   --  The list of new signals supported by this GObject

   Signal_Parameters : constant Glib.Object.Signal_Parameter_Types :=
     (1 => (GType_Int, GType_Int),
      2 => (GType_None, GType_None),
      3 => (GType_None, GType_None),
      4 => (GType_None, GType_None),
      5 => (GType_None, GType_None),
      6 => (GType_None, GType_None),
      7 => (GType_None, GType_None));
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

   procedure Set_Line_Highlighting
     (Editor : access Source_Buffer_Record;
      Line   : Buffer_Line_Type;
      Style  : Style_Access;
      Set    : Boolean;
      Highlight_In : Highlight_Location_Array);
   --  Common function for [Add|Remove]_Line_Highlighting

   procedure Register_Cursor_Timeout
     (Buffer : access Source_Buffer_Record'Class);
   --  Indicate that the cursor has moved, and that a timeout should be
   --  registered to call the corresponding "after-timeout" hook.

   function Check_Blocks (Buffer : Source_Buffer) return Boolean;
   --  Timeout that recomputes the blocks if needed

   function Cursor_Stop_Hook (Buffer : Source_Buffer) return Boolean;
   --  Hook called after the cursor has stopped moving

   type Src_String is record
      Contents  : Unchecked_String_Access;
      Length    : Natural := 0;
      Read_Only : Boolean := False;
   end record;
   --  Special purpose string type to avoid extra copies and string allocation
   --  as much as possible.
   --  The actual contents of a Src_String is represented by
   --  Contents (1 .. Length) (as utf8-encoded string)
   --  Never use Free (Contents) directly, use the Free procedure below.

   procedure Free (S : in out Src_String);
   --  Free the memory associated with S

   function Get_String
     (Buffer : Source_Buffer;
      Line   : Editable_Line_Type) return Src_String;
   --  Return the string at line Line, without the line terminator.
   --  Return null if the Line is not a valid line or there is no contents
   --  associated with the line.
   --  The caller is responsible for freeing the returned value..
   --  The returned string is UTF8-encoded

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
         if Is_Directory (File)
           and then Edited.Full_Name'Length > File.Full_Name'Length
           and then Create (Edited.Full_Name.all
               (Edited.Full_Name'First ..
                  Edited.Full_Name'First + File.Full_Name'Length - 1)) = File
         then
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
         if Is_Directory (File)
           and then Edited.Full_Name'Length > File.Full_Name'Length
           and then Create (Edited.Full_Name.all
               (Edited.Full_Name'First ..
                  Edited.Full_Name'First + File.Full_Name'Length - 1)) = File
         then
            Dest := Create
              (Files_2_Hooks_Args (Data.all).Renamed.Full_Name.all &
               Edited.Full_Name.all
                 (Edited.Full_Name'First + File.Full_Name'Length ..
                    Edited.Full_Name'Last));
            Hook.Buffer.Filename := Dest;
            Hook.Buffer.Filename_Changed;

         elsif not Is_Directory (File)
           and then Edited.Full_Name.all = File.Full_Name.all
         then
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

   procedure Free_Column_Info
     (Column_Info : Columns_Config_Access) is
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

   ----------------
   -- Free_Block --
   ----------------

   procedure Free_Block (Block : in out Block_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Block_Record, Block_Access);
   begin
      GNAT.Strings.Free (Block.Name);
      Unchecked_Free (Block);
   end Free_Block;

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
     (Buffer : Source_Buffer) return GNAT.Strings.String_Access
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

   ------------------
   -- Check_Blocks --
   ------------------

   function Check_Blocks (Buffer : Source_Buffer) return Boolean is
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
         Execute_GPS_Shell_Command
           (Buffer.Kernel,
            "File " & Full_Name (Buffer.Filename).all);
         Execute_GPS_Shell_Command
           (Buffer.Kernel, "File.shadow_check_syntax %1");
      end if;

      --  Unregister the timeout

      Buffer.Blocks_Timeout_Registered := False;
      return False;

   exception
      when E : others =>
         Trace (Traces.Exception_Handle, E);
         return False;
   end Check_Blocks;

   --------------------
   -- Get_Delimiters --
   --------------------

   procedure Get_Delimiters
     (Buffer           : access Source_Buffer_Record;
      On_Cursor_Iter   : Gtk_Text_Iter;
      First_Delim_Iter : out Gtk_Text_Iter;
      Last_Delim_Iter  : out Gtk_Text_Iter;
      Found            : out Natural)
   is
      Current      : Gtk_Text_Iter;

      Success      : Boolean;
      Counter      : Natural;
      Counter_Max  : constant := 8192;
      --  ??? Should that be a preference ?

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

      if Language /= null
        and then Language /= Unknown_Lang
        and then Is_In_Comment (Source_Buffer (Buffer), Current)
      then
         --  The current character is in a comment: set the corresponding flag.
         Highlight_Within_Comment := True;
      end if;

      Backward_Char (Current, Success);

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
         C          := ASCII.NUL;

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
         C          := ASCII.NUL;

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
         Buffer.Cursor_Timeout := Buffer_Timeout.Add
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
         Buffer.Blocks_Timeout := Buffer_Timeout.Add
           (Buffer_Recompute_Interval,
            Check_Blocks'Access,
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
      Line   : File_Line_Type) return Buffer_Line_Type
   is
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
         Timeout_Remove (Buffer.Blocks_Timeout);
      end if;

      if Buffer.Cursor_Timeout_Registered then
         Timeout_Remove (Buffer.Cursor_Timeout);
      end if;
   end Destroy_Hook;

   --------------------
   -- Buffer_Destroy --
   --------------------

   procedure Buffer_Destroy (Data : System.Address; Buf : System.Address) is
      Stub    : Source_Buffer_Record;
      Success : Boolean;
      pragma Unreferenced (Data);
      pragma Warnings (Off, Stub);

      Buffer : constant Source_Buffer := Source_Buffer
        (Get_User_Data (Buf, Stub));

      procedure Free (X : in out Line_Info_Width_Array);
      --  Free memory associated to X

      ----------
      -- Free --
      ----------

      procedure Free (X : in out Line_Info_Width_Array) is
      begin
         for J in X'Range loop
            if X (J).Info /= null then
               Free (X (J).Info.all);
            end if;

            Unchecked_Free (X (J).Info);
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
         Timeout_Remove (Buffer.Timeout_Id);
         Buffer.Timeout_Registered := False;

         if Buffer.Filename /= GNATCOLL.VFS.No_File then
            Delete (Autosaved_File (Buffer.Filename), Success);
         end if;
      end if;

      if Buffer.Controls_Set then
         Remove_Controls (Buffer);
      end if;

      Free_Queue (Buffer.Queue);

      Free_Column_Info (Buffer.Editable_Line_Info_Columns);
      Unchecked_Free (Buffer.Editable_Line_Info_Columns);

      if Buffer.Editable_Lines /= null then
         for J in Buffer.Editable_Lines'Range loop
            if Buffer.Editable_Lines (J).Where = In_Mark then
               GNAT.Strings.Free (Buffer.Editable_Lines (J).Text);
            end if;

            if Buffer.Editable_Lines (J).Side_Info_Data /= null then
               Free (Buffer.Editable_Lines (J).Side_Info_Data.all);
               Unchecked_Free (Buffer.Editable_Lines (J).Side_Info_Data);
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

      if Number = 0
        and then Length < 4
      then
         declare
            Index : Natural;
         begin
            Index := UTF8_Find_Next_Char (Text (1 .. Length), Text'First);

            if Index > Length then
               Character_Added
                 (Source_Buffer (Buffer),
                  UTF8_Get_Char (Text (1 .. Length)));
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
      Text    : constant Unchecked_String_Access :=
                  To_Unchecked_String (Get_Chars (Nth (Params, 2)));
      Length  : constant Integer := Integer (Get_Int (Nth (Params, 3)));
      Pos     : Gtk_Text_Iter;
      Command : Editor_Command := Editor_Command (Buffer.Current_Command);
   begin
      if Buffer.Prevent_CR_Insertion then
         Buffer.Prevent_CR_Insertion := False;

         declare
            T        : String (1 .. Length);
            Last     : Integer := 1;
            CR_Found : Boolean := False;
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

               Emit_Stop_By_Name (Object => Buffer, Name   => "insert_text");
               CR_Found := Insert_Interactive_At_Cursor
                 (Buffer, T (1 .. Last), True);
               return;
            end if;
         end;
      end if;

      Edit_Hook (Buffer);
      Cursor_Move_Hook (Buffer);

      if Buffer.Inserting then
         return;
      end if;

      User_Edit_Hook (Buffer);

      Get_Text_Iter (Nth (Params, 1), Pos);

      if Is_Null_Command (Command) then
         Create
           (Command,
            Insertion,
            Source_Buffer (Buffer),
            False,
            Get_Editable_Line (Buffer, Buffer_Line_Type (Get_Line (Pos) + 1)),
            Character_Offset_Type (Get_Line_Offset (Pos) + 1));

         Enqueue (Buffer, Command_Access (Command));

         Add_Text (Command, Text (1 .. Length));
         Buffer.Current_Command := Command_Access (Command);

      elsif Get_Mode (Command) = Insertion then
         if Length = 1
           and then (Text (1) = ASCII.LF or else Text (1) = ' ')
         then
            End_Action (Buffer);
            Create
              (Command,
               Insertion,
               Source_Buffer (Buffer),
               False,
               Get_Editable_Line
                 (Buffer, Buffer_Line_Type (Get_Line (Pos) + 1)),
               Character_Offset_Type (Get_Line_Offset (Pos) + 1));

            Enqueue (Buffer, Command_Access (Command));
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
            Get_Editable_Line (Buffer, Buffer_Line_Type (Get_Line (Pos) + 1)),
            Character_Offset_Type (Get_Line_Offset (Pos) + 1));
         Enqueue (Buffer, Command_Access (Command));
         Add_Text (Command, Text (1 .. Length));
         Buffer.Current_Command := Command_Access (Command);
      end if;

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
      Get_Text_Iter (Nth (Params, 1), Start_Iter);

      if Get_Language_Context (Buffer.Lang).Syntax_Highlighting then
         Delete_Range_Cb (Buffer, Start_Iter);
      end if;

      if Buffer.First_Removed_Line > 0 then
         Lines_Remove_Hook_After
           (Buffer, Buffer.First_Removed_Line, Buffer.Last_Removed_Line);
         Buffer.First_Removed_Line := 0;
      end if;

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
      Start_Iter   : Gtk_Text_Iter;
      End_Iter     : Gtk_Text_Iter;
      Command      : Editor_Command := Editor_Command (Buffer.Current_Command);
      Direction    : Direction_Type := Extended;
      Line, Column : Gint;
      Line_Start   : Gint;
      Column_Start : Gint;
      Line_End     : Gint;
      Column_End   : Gint;
      Editable_Line_Start : Editable_Line_Type;
      Editable_Line_End   : Editable_Line_Type;

   begin
      Get_Text_Iter (Nth (Params, 1), Start_Iter);
      Get_Text_Iter (Nth (Params, 2), End_Iter);

      --  Determine the direction mode for the delete action

      Get_Cursor_Position (Buffer, Line, Column);

      Line_Start   := Get_Line (Start_Iter);
      Column_Start := Get_Line_Offset (Start_Iter);
      Line_End     := Get_Line (End_Iter);
      Column_End   := Get_Line_Offset (End_Iter);

      if Line = Line_Start
        and then Column = Column_Start
      then
         Direction := Backward;
      end if;

      if Line = Line_End
        and then Column = Column_End
      then
         Direction := Forward;
      end if;

      Editable_Line_Start :=
        Get_Editable_Line (Buffer, Buffer_Line_Type (Line_Start + 1));

      Editable_Line_End :=
        Get_Editable_Line (Buffer, Buffer_Line_Type (Line_End + 1));

      --  If we are removing lines in a non-editable block, stop propagation
      --  of the handler.

      if not Lines_Are_Real (Buffer)
        and then not Buffer.Inserting
      then
         if Editable_Line_Start = 0
           or else Editable_Line_End = 0
         then
            Insert
              (Buffer.Kernel,
               -"Trying to delete a blank line",
               Mode => GPS.Kernel.Console.Error);
            Emit_Stop_By_Name (Buffer, "delete_range");
            return;
         end if;
      end if;

      --  Remove the lines in the side information column

      if Line_Start /= Line_End then
         --  Unfold all lines, remove blank lines before deleting multiple
         --  lines.
         if not Lines_Are_Real (Buffer)
           and then not Buffer.Inserting
         then
            if Flatten_Area
              (Buffer, Editable_Line_Start, Editable_Line_End)
            then
               --  We have modified the area. Stop propagation of this signal,
               --  and delete the new area.

               Emit_Stop_By_Name (Buffer, "delete_range");

               Replace_Slice
                 (Buffer,
                  "",
                  Editable_Line_Start,
                  Character_Offset_Type (Column_Start + 1),
                  Editable_Line_End,
                  Character_Offset_Type (Column_End + 1));
               return;
            end if;
         end if;

         Lines_Remove_Hook_Before
           (Buffer,
            Buffer_Line_Type (Line_Start + 1),
            Buffer_Line_Type (Line_End + 1));
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

         Enqueue (Buffer, Command_Access (Command));

         Add_Text
           (Command,
            Get_Slice (Buffer, Start_Iter, End_Iter),
            Get_Editable_Line (Buffer, Buffer_Line_Type (Line_Start + 1)),
            Character_Offset_Type (Column_Start + 1));
      else
         if Direction = Forward then
            Add_Text
              (Command,
               Get_Slice (Buffer, Start_Iter, End_Iter),
               Get_Editable_Line (Buffer, Buffer_Line_Type (Line_Start + 1)),
               Character_Offset_Type (Column_Start + 1));
         else
            Add_Text
              (Command,
               Get_Slice (Buffer, Start_Iter, End_Iter));
         end if;
      end if;

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
        (Get_Object (Buffer), Signal_Line_Highlights_Changed
         & ASCII.NUL);
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
     (Buffer : access Source_Buffer_Record'Class)
      return Status_Type is
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

               Is_Valid_Index
                 (Source_Buffer (Buffer), Entity_End, Success, Line, Col);
               if not Success then
                  Trace (Me, "invalid position """
                         & Full_Name (Buffer.Filename).all & """"
                         & Line'Img & Col'Img);
                  return False;
               end if;

               Forward_Char (Entity_End, Success);

            else
               Is_Valid_Index
                 (Source_Buffer (Buffer), Entity_End, Success, Line, 0);
               if not Success then
                  Trace (Me, "invalid position """
                         & Full_Name (Buffer.Filename).all & """"
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

      --  Highlight from the previous tag if on the same line, to handle
      --  special language semantics requiring information from previous
      --  characters, such as x.all'address in Ada.

      Backward_To_Tag_Toggle (Entity_Start, Result => Result);

      if Get_Line (Entity_Start) /= Get_Line (Start_Iter) then
         Copy (Source => Start_Iter, Dest => Entity_Start);
      end if;

      --  Highlight to the end of line, to avoid missing most of the
      --  partial entities (strings, characters, ...).
      --  In case we have started typing a string for instance, that
      --  provides a nice optimization over rehighlighting the whole buffer.

      Forward_To_Line_End (Entity_End, Result);
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
         Side_Info_Data => null);

      --  ??? create line info (above)

      Buffer.Line_Data := new Line_Data_Array (1 .. 1);
      Buffer.Line_Data (1) := New_Line_Data;
      Buffer.Line_Data (1).Editable_Line := 1;

      --  Compute the block information

      Register_Edit_Timeout (Buffer);

      --  Show the line number information, if needed
      Recalculate_Side_Column_Width (Buffer);
   end Initialize_Hook;

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
      Deleted_Hook : File_Deleted_Hook;
      Renamed_Hook : File_Renamed_Hook;
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

      --  File hooks
      Deleted_Hook := new File_Deleted_Hook_Record;
      Deleted_Hook.Buffer := Source_Buffer (Buffer);
      Add_Hook
        (Kernel, GPS.Kernel.File_Deleted_Hook, Deleted_Hook,
         Name  => "project_explorers_files.file_deleted",
         Watch => GObject (Buffer));
      Renamed_Hook := new File_Renamed_Hook_Record;
      Renamed_Hook.Buffer := Source_Buffer (Buffer);
      Add_Hook
        (Kernel, GPS.Kernel.File_Renamed_Hook, Renamed_Hook,
         Name  => "project_explorers_files.file_renamed",
         Watch => GObject (Buffer));

      for Entity_Kind in Standout_Language_Entity'Range loop
         Text_Tag_Table.Add (Tags, Buffer.Syntax_Tags (Entity_Kind));
      end loop;

      --  Create Delimiter_Tag and save it into the source buffer tag table

      Create_Highlight_Line_Tag
        (Buffer.Delimiter_Tag, Get_Pref (Delimiter_Color));
      Text_Tag_Table.Add (Tags, Buffer.Delimiter_Tag);

      Gtk_New (Buffer.Non_Editable_Tag);
      Set_Property (Buffer.Non_Editable_Tag, Editable_Property, False);
      Add (Tags, Buffer.Non_Editable_Tag);

      --  Save the insert mark for fast retrievals, since we will need to
      --  access it very often.

      Buffer.Insert_Mark := Get_Insert (Buffer);

      --  Initialize the queue for editor commands

      Buffer.Queue := New_Queue;

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
         Cb => Delete_Range_Before_Handler'Access,
         After => False);

      Buffer.Editable_Line_Info_Columns := new Line_Info_Display_Array_Access'
        (null);

      Initialize_Hook (Buffer);

      --  Create the queue change hook that will be called every
      --  time the state of the queue associated to the buffer changes.

      Create (Command, Source_Buffer (Buffer), Buffer.Queue);

      Add_Queue_Change_Hook
        (Buffer.Queue,
         Command_Access (Command),
         "State_Check");

      Buffer.First_Removed_Line := 0;
   end Initialize;

   -------------------
   -- Is_In_Comment --
   -------------------

   function Is_In_Comment
     (Buffer : Source_Buffer;
      Iter   : Gtk_Text_Iter)
      return Boolean is
   begin
      return Has_Tag (Iter, Buffer.Syntax_Tags (Comment_Text))
        or else Has_Tag (Iter, Buffer.Syntax_Tags (Annotated_Comment_Text));
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
     (Hook   : Preferences_Changed_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Kernel);
      B       : constant Source_Buffer := Hook.Buffer;
      Timeout : Gint;
      Prev    : Boolean;

      Keyword_Font : constant Pango.Font.Pango_Font_Description :=
                       Get_Pref_Font (Keywords_Style);
      Comment_Font : constant Pango.Font.Pango_Font_Description :=
                       Get_Pref_Font (Comments_Style);
      String_Font  : constant Pango.Font.Pango_Font_Description :=
                       Get_Pref_Font (Strings_Style);
   begin
      --  Since we update the tags directly, gtk+ will automatically refresh
      --  the source view, we don't need to do anything for this.

      Create_Syntax_Tags
        (B.Syntax_Tags,
         Keyword_Color               => Get_Pref_Fg (Keywords_Style),
         Keyword_Color_Bg            => Get_Pref_Bg (Keywords_Style),
         Keyword_Font_Desc           => Keyword_Font,
         Comment_Color               => Get_Pref_Fg (Comments_Style),
         Comment_Color_Bg            => Get_Pref_Bg (Comments_Style),
         Comment_Font_Desc           => Comment_Font,
         Annotated_Comment_Color     => Get_Pref_Fg (Annotated_Comments_Style),
         Annotated_Comment_Color_Bg  => Get_Pref_Bg (Annotated_Comments_Style),
         Annotated_Comment_Font_Desc => Comment_Font,
         Character_Color             => Get_Pref_Fg (Strings_Style),
         Character_Color_Bg          => Get_Pref_Bg (Strings_Style),
         Character_Font_Desc         => String_Font,
         String_Color                => Get_Pref_Fg (Strings_Style),
         String_Color_Bg             => Get_Pref_Bg (Strings_Style),
         String_Font_Desc            => String_Font);

      --  Connect timeout, to handle automatic saving of buffer

      if B.Timeout_Registered then
         Timeout_Remove (B.Timeout_Id);
         B.Timeout_Registered := False;
      end if;

      Timeout := Get_Pref (Periodic_Save);

      if Timeout > 0 then
         B.Timeout_Id := Buffer_Timeout.Add
           (Guint32 (Timeout) * 1000,  Automatic_Save'Access, B.all'Access);
         B.Timeout_Registered := True;
      end if;

      Prev := B.Block_Highlighting;
      B.Block_Highlighting := Get_Pref (Block_Highlighting);

      if Prev /= B.Block_Highlighting then
         Register_Edit_Timeout (B);
      end if;

      Prev := B.Block_Folding;
      B.Block_Folding := Get_Pref (Block_Folding);

      if Prev /= B.Block_Folding then
         Register_Edit_Timeout (B);
      end if;

      if not B.Block_Folding and then Prev then
         Unfold_All (B);
         Remove_Block_Folding_Commands (B);
      end if;

      Prev := B.Parse_Blocks;
      B.Parse_Blocks := B.Block_Folding or else B.Block_Highlighting
        or else Get_Pref (Display_Subprogram_Names);

      if Prev /= B.Parse_Blocks then
         Buffer_Information_Changed (B);
      end if;

      if not Prev and then B.Parse_Blocks then
         Register_Edit_Timeout (B);
      end if;

      B.Auto_Syntax_Check    := Get_Pref (Automatic_Syntax_Check);
      B.Highlight_Delimiters := Get_Pref (Highlight_Delimiters);
      B.Tab_Width            := Get_Pref (Tab_Width);
   end Execute;

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

      ------------------
      -- Reset_Buffer --
      ------------------

      procedure Reset_Buffer (Buffer : access Source_Buffer_Record) is
      begin
         --  Clear the buffer

         Clear (Buffer);

         --  Clear the side column information

         Buffer.Original_Text_Inserted := False;

         Free_Column_Info (Buffer.Editable_Line_Info_Columns);

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

         Buffer.Editable_Lines := new Editable_Line_Array (1 .. 1);
         Buffer.Editable_Lines (1) :=
           (Where          => In_Buffer,
            Buffer_Line    => 1,
            Side_Info_Data => null);

         Buffer.Line_Data := new Line_Data_Array (1 .. 1);
         Buffer.Line_Data (1) := New_Line_Data;
         Buffer.Line_Data (1).Editable_Line := 1;

         Buffer.First_Removed_Line := 0;
         Buffer.Last_Removed_Line  := 0;
         Buffer.Last_Editable_Line := 1;

         --  Unregister the blocks timeout

         if Buffer.Blocks_Timeout_Registered then
            Timeout_Remove (Buffer.Blocks_Timeout);
            Buffer.Blocks_Timeout_Registered := False;
         end if;

         --  Unregister the cursor timeout

         if Buffer.Cursor_Timeout_Registered then
            Timeout_Remove (Buffer.Cursor_Timeout);
            Buffer.Cursor_Timeout_Registered := False;
         end if;

         Buffer.Blank_Lines := 0;
         Buffer.Hidden_Lines := 0;
         Buffer.Block_Highlighting_Column := -1;

         --  Request the new blocks

         Register_Edit_Timeout (Buffer);
      end Reset_Buffer;

      Contents      : GNAT.Strings.String_Access;
      UTF8          : Gtkada.Types.Chars_Ptr;
      Ignore        : aliased Natural;
      Length        : aliased Natural;
      Last          : Natural;
      CR_Found      : Boolean := False;
      F, L          : Gtk_Text_Iter;
      Valid         : Boolean;
      Recovering    : Boolean := False;
      First_Invalid : Natural;
      Buttons       : Message_Dialog_Buttons;
      File_Is_New   : constant Boolean := not Buffer.Original_Text_Inserted;

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
                & Full_Name (Filename).all);
         Success := False;
         return;
      end if;

      if not File_Is_New then
         Reset_Buffer (Buffer);
      end if;

      --  Insert the new text

      Buffer.Inserting := True;

      if Lang_Autodetect then
         Set_Language
           (Buffer, Get_Language_From_File
              (Get_Language_Handler (Buffer.Kernel), Filename));
      end if;

      Strip_CR (Contents.all, Last, CR_Found);

      Set_Charset (Buffer, Get_File_Charset (Filename));

      UTF8 := Glib.Convert.Convert
        (Contents (Contents'First .. Last), "UTF-8",
         Buffer.Charset.all,
         Ignore'Unchecked_Access, Length'Unchecked_Access);

      if UTF8 = Gtkada.Types.Null_Ptr then
         --  In case conversion failed, use a default encoding so that we can
         --  at least show something in the editor
         Set_Charset (Buffer, "ISO-8859-1");
         UTF8 := Glib.Convert.Convert
           (Contents (Contents'First .. Last), "UTF-8",
            Buffer.Charset.all,
            Ignore'Unchecked_Access, Length'Unchecked_Access);
      end if;

      GNAT.Strings.Free (Contents);
      UTF8_Validate (To_Unchecked_String (UTF8) (1 .. Length),
                     Valid, First_Invalid);

      if not Valid then
         Length := First_Invalid - 1;
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
      else
         Buffer.Saved_Position := 0;
      end if;

      Buffer.Inserting := False;
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
         File_Closed (Buffer.Kernel, Filename);
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
      S           : Ada.Strings.Unbounded.String_Access;
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
                             Line_Terminators'Val (Get_Pref (Line_Terminator));
         Bytes_Written   : Integer;
         pragma Unreferenced (Bytes_Written);

         Strip_Blank     : Boolean;
         Index           : Natural;
         Error           : GError_Access := new GError'(null);

         procedure Unchecked_Free is new Ada.Unchecked_Deallocation
           (GError, GError_Access);

      begin
         case Terminator_Pref is
            when Unix =>
               Terminator := LF;
            when Windows =>
               Terminator := CR_LF;
            when Unchanged =>
               null;
         end case;

         Strip_Blank := Get_Pref (Strip_Blanks);

         for Line in
           Buffer.Editable_Lines'First .. Buffer.Last_Editable_Line
         loop
            declare
               Str : Src_String := Get_String (Buffer, Line);
            begin
               if Str.Length = 0 then
                  if Line /= Buffer.Last_Editable_Line then
                     New_Line;
                  end if;

               else
                  Index := Str.Length;

                  if Strip_Blank then
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

                        Insert_Location
                          (Kernel   => Buffer.Kernel,
                           Category => Conversion_Error_Message
                             (Buffer.Charset.all),
                           File     => Buffer.Filename,
                           Column   => 0,
                           Text     => Get_Message (Error.all),
                           Line     => Positive (Line));

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
            Buttons : Message_Dialog_Buttons;
            pragma Unreferenced (Buttons);
         begin
            Buttons := Message_Dialog
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

      FD := Write_File (Filename);

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

      if FD = Invalid_File then
         Insert
           (Buffer.Kernel,
            -"Could not open file for writing: " & Full_Name (Filename).all,
            Mode => GPS.Kernel.Console.Error);
         Success := False;
         return;
      end if;

      Get_String (U_Buffer, S, Length);

      if S /= null then
         Write (FD, S (1 .. Length));
      end if;

      Close (FD);

      --  If the file could be saved, emit the corresponding signal.
      --  Emit the signal only if we are really saving to the buffer's file,
      --  not to another filename (which happens for example when doing
      --  automatic saves.

      if Success and then not Internal then
         Buffer.Save_Complete := not Has_Errors;

         if Buffer.Filename /= Filename then
            --  If we "save as" the buffer, we emit a closed for the previous
            --  name
            if Buffer.Filename = GNATCOLL.VFS.No_File then
               File_Closed (Buffer.Kernel, Buffer.File_Identifier);
            end if;

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
      Name_Changed      : constant Boolean := Buffer.Filename /= Filename;
      Result            : Boolean;
      Original_Filename : constant Virtual_File := Buffer.Filename;
   begin
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
                 (Get_Language_Handler (Buffer.Kernel),
                  Buffer.Filename));

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
      Buttons : Message_Dialog_Buttons;
      pragma Unreferenced (Buttons);

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
         Remove_Location_Category
           (Buffer.Kernel,
            Conversion_Error_Message (Buffer.Charset.all),
            Buffer.Filename);

         GNAT.Strings.Free (Buffer.Charset);
         Buffer.Charset := new String'(Charset);

         if Get_Status (Buffer) = Modified
           or else Get_Status (Buffer) = Unsaved
         then
            Buttons := Message_Dialog
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
      Internal  : Boolean)
   is
      Iter : Gtk_Text_Iter;
   begin
      if not Is_Valid_Position (Buffer, Line, Column) then
         Trace (Me, "invalid position for Set_Cursor_Position "
                & Full_Name (Get_Filename (Buffer)).all
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

      if Centering /= Minimal then
         Buffer.Cursor_Set_Explicitely := True;
      end if;

      Place_Cursor (Buffer, Iter);
   end Set_Cursor_Position;

   procedure Set_Cursor_Position
     (Buffer    : access Source_Buffer_Record;
      Line      : Editable_Line_Type;
      Column    : Character_Offset_Type;
      Centering : Centering_Type := Center;
      Internal  : Boolean)
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
         Centering, Internal => Internal);
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
      Result  : Boolean := True;
      Current : Gint := 0;
      Tab_Len : constant Gint := Buffer.Tab_Width;

      The_Column : constant Gint := Gint (Column - 1);
   begin
      if Is_Valid_Position (Buffer, Line, 0) then
         Get_Iter_At_Line_Offset (Buffer, Iter, Line, 0);
      else
         Trace (Me, "Invalid position for Set_Screen_Position "
                & Full_Name (Get_Filename (Buffer)).all & Line'Img);
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

   -------------------------
   -- Select_Current_Word --
   -------------------------

   procedure Select_Current_Word (Buffer : access Source_Buffer_Record) is
      Success : Boolean := True;
      Start_Iter, End_Iter, Iter : Gtk_Text_Iter;

   begin
      Get_Iter_At_Mark (Buffer, Iter, Buffer.Insert_Mark);

      if not (Inside_Word (Iter) or else Get_Char (Iter) = '_') then
         return;
      end if;

      Copy (Iter, End_Iter);

      while Success and then not Ends_Word (End_Iter) loop
         Forward_Char (End_Iter, Success);
      end loop;

      if Success then
         Forward_Char (End_Iter, Success);
      end if;

      Copy (Iter, Start_Iter);

      Success := True;

      while Success and then not Starts_Word (Start_Iter) loop
         Backward_Char (Start_Iter, Success);
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
      Previous_Inserting_Value : constant Boolean := Buffer.Inserting;
   begin
      pragma Assert (Is_Valid_Position (Buffer, Line, Column));

      if not Buffer.Writable then
         End_Action (Buffer);
         return;
      end if;

      if not Enable_Undo then
         Buffer.Inserting := True;
      end if;

      Get_Iter_At_Line_Offset (Buffer, Iter, Line, Column);
      Insert (Buffer, Iter, Text);

      if not Enable_Undo then
         Buffer.Inserting := Previous_Inserting_Value;
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
      Previous_Inserting_Value : constant Boolean := Buffer.Inserting;
   begin
      pragma Assert (Is_Valid_Position (Buffer, Line, Column));

      if not Buffer.Inserting then
         End_Action (Buffer);
      end if;

      if not Enable_Undo then
         Buffer.Inserting := True;
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
         Buffer.Inserting := Previous_Inserting_Value;
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
      Previous_Inserting_Value : constant Boolean := Buffer.Inserting;

   begin
      Assert (Me, Is_Valid_Position (Buffer, Start_Line, Start_Column),
              "Invalid start position " & Start_Line'Img & Start_Column'Img);
      Assert (Me, Is_Valid_Position (Buffer, End_Line, End_Column),
              "Invalid end position " & End_Line'Img & End_Column'Img);

      if not Buffer.Inserting then
         End_Action (Buffer);
      end if;

      if not Enable_Undo then
         Buffer.Inserting := True;
      end if;

      Get_Iter_At_Line_Offset (Buffer, Start_Iter, Start_Line, Start_Column);
      Get_Iter_At_Line_Offset (Buffer, End_Iter, End_Line, End_Column);

      --  Currently, Gtk_Text_Buffer does not export a service to replace
      --  some text, so we delete the slice first, then insert the text...
      Delete (Buffer, Start_Iter, End_Iter);

      Get_Iter_At_Line_Offset (Buffer, Start_Iter, Start_Line, Start_Column);

      Insert (Buffer, Start_Iter, Text);

      if not Enable_Undo then
         Buffer.Inserting := Previous_Inserting_Value;
      end if;

      Register_Edit_Timeout (Buffer);
   end Replace_Slice_Real;

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
      Start_Iter     : Gtk_Text_Iter;
      End_Iter       : Gtk_Text_Iter;
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
     (Buffer       : access Source_Buffer_Record;
      Cursor_Iter  : Gtk.Text_Iter.Gtk_Text_Iter;
      Bound_Iter   : Gtk.Text_Iter.Gtk_Text_Iter)
   is
   begin
      End_Action (Buffer);
      Buffer.Cursor_Set_Explicitely := True;
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
         Buffer.Cursor_Set_Explicitely := True;
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
   begin
      Redo (Buffer.Queue);
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
   end Undo;

   -------------
   -- Enqueue --
   -------------

   procedure Enqueue
     (Buffer  : access Source_Buffer_Record;
      Command : Command_Access) is
   begin
      Buffer.Inserting := True;
      if Buffer.Saved_Position > Get_Position (Buffer.Queue) then
         Buffer.Saved_Position := -1;
      end if;

      Enqueue (Buffer.Queue, Command);
      Buffer.Inserting := False;
   end Enqueue;

   ----------------
   -- Get_Kernel --
   ----------------

   function Get_Kernel
     (Buffer : access Source_Buffer_Record)
     return GPS.Kernel.Kernel_Handle is
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
      end if;
   end Set_Filename;

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

   ---------------------
   -- Check_Timestamp --
   ---------------------

   function Check_Timestamp
     (Buffer : access Source_Buffer_Record;
      Update : Boolean := False) return Boolean
   is
      New_Timestamp : Ada.Calendar.Time;
      Result : Boolean := True;
   begin
      if Buffer.Filename /= GNATCOLL.VFS.No_File then
         New_Timestamp := File_Time_Stamp (Buffer.Filename);

         --  If the file does not exist, we assume the editor is up-to-date

         Result := New_Timestamp = No_Time
           or else New_Timestamp = Buffer.Timestamp;

         if Update then
            Buffer.Timestamp := New_Timestamp;
         end if;
      end if;

      return Result;
   end Check_Timestamp;

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
         if Buffer.Filename /= GNATCOLL.VFS.No_File then
            File_Closed (Buffer.Kernel, Buffer.Filename);

         elsif Buffer.File_Identifier /= GNATCOLL.VFS.No_File then
            File_Closed (Buffer.Kernel, Buffer.File_Identifier);
         end if;

      end if;
   end Register_View;

   ------------------
   -- Add_Controls --
   ------------------

   procedure Add_Controls (Buffer : access Source_Buffer_Record) is
   begin
      if not Buffer.Controls_Set then
         Set_Controls
           (Buffer.Queue, Undo_Redo_Data.Get (Buffer.Kernel, Undo_Redo_Id));
      end if;

      Buffer.Controls_Set := True;
   end Add_Controls;

   ---------------------
   -- Remove_Controls --
   ---------------------

   procedure Remove_Controls (Buffer : access Source_Buffer_Record) is
   begin
      if Buffer.Controls_Set then
         Unset_Controls (Buffer.Queue);
      end if;

      Buffer.Controls_Set := False;
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
      Line   : Buffer_Line_Type) return Boolean
   is
      EL : Editable_Line_Type;
   begin
      EL := Get_Editable_Line (Buffer, Line);

      if EL = 0 then
         return False;
      end if;

      if Buffer.Editable_Lines (EL).Side_Info_Data /= null then
         for J in Buffer.Editable_Lines (EL).Side_Info_Data'Range loop
            if not Buffer.Editable_Lines (EL).Side_Info_Data (J).Set then
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
      Line   : Editable_Line_Type)
   is
      Editable_Lines   : Editable_Line_Array_Access
        renames Buffer.Editable_Lines;
      Columns_Config : Line_Info_Display_Array_Access
        renames Buffer.Editable_Line_Info_Columns.all;
   begin
      if Columns_Config /= null then
         if Editable_Lines (Line).Side_Info_Data = null then
            Editable_Lines (Line).Side_Info_Data := new
              Line_Info_Width_Array (Columns_Config'Range);

            for K in Columns_Config'Range loop
               Editable_Lines (Line).Side_Info_Data (K) :=
                 (null,
                  Width => -1,
                  Set   => not Columns_Config (K).Every_Line);
            end loop;
         end if;
      end if;
   end Create_Side_Info;

   -----------------------
   -- Needs_To_Be_Saved --
   -----------------------

   function Needs_To_Be_Saved
     (Buffer : access Source_Buffer_Record'Class)
      return Boolean is
   begin
      --  Only modified and non-empty unsaved buffers need to be saved
      return Get_Status (Buffer) = Modified
        or else (Get_Status (Buffer) = Unsaved
                   and then Get_Char_Count (Buffer) > 0);
   end Needs_To_Be_Saved;

   ----------------
   -- Get_Status --
   ----------------

   function Get_Status
     (Buffer : access Source_Buffer_Record)
      return Status_Type is
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

      --  Else
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

   ---------------------------
   -- Set_Line_Highlighting --
   ---------------------------

   procedure Set_Line_Highlighting
     (Editor : access Source_Buffer_Record;
      Line   : Buffer_Line_Type;
      Style  : Style_Access;
      Set    : Boolean;
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

         if Set then
            Editor.Line_Data (Line).Enabled_Highlights :=
              new Boolean_Array (1 .. Last_Index);
            Editor.Line_Data (Line).Enabled_Highlights.all :=
              (others   => False);
            Editor.Line_Data (Line).Enabled_Highlights (Category) := Set;
            Editor.Line_Data (Line).Highlight_Category := Category;
            Editor.Line_Data (Line).Highlight_In       := Highlight_In;
         end if;

         return;
      end if;

      if Editor.Line_Data (Line).Enabled_Highlights'Last < Last_Index then
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

      Editor.Line_Data (Line).Enabled_Highlights (Category) := Set;

      --  Find out which category has priority for highlighting

      for J in Editor.Line_Data (Line).Enabled_Highlights'Range loop
         if Editor.Line_Data (Line).Enabled_Highlights (J) then
            Editor.Line_Data (Line).Highlight_Category := J;
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
     (Editor : access Source_Buffer_Record;
      Line   : Editable_Line_Type;
      Style  : Style_Access;
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

   ----------------------
   -- Get_Highlight_GC --
   ----------------------

   function Get_Highlight_GC
     (Editor  : access Source_Buffer_Record;
      Line    : Buffer_Line_Type;
      Context : Highlight_Location) return Gdk_GC is
   begin
      if Line = 0 then
         return null;
      end if;

      if Editor.Line_Data /= null
        and then Line <= Editor.Line_Data'Last
        and then Editor.Line_Data (Line).Highlight_In (Context)
      then
         return Get_GC (Editor.Line_Data (Line).Highlight_Category);
      end if;

      return null;
   end Get_Highlight_GC;

   ---------------
   -- Get_Block --
   ---------------

   function Get_Block
     (Editor        : access Source_Buffer_Record;
      Line          : Buffer_Line_Type;
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

      if Editor.Line_Data /= null
        and then Line <= Editor.Line_Data'Last
      then
         if Editor.Line_Data (Line).Block = null then
            return New_Block;
         else
            return Editor.Line_Data (Line).Block.all;
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
         Buffer        : String;
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
         Buffer        : String;
         Replace       : Replace_Text_Callback;
         From, To      : Natural := 0;
         Indent_Params : Indent_Parameters) is
      begin
         if Indent_Style = Simple then
            Format_Buffer
              (Language_Root (Lang.all)'Access,
               Buffer, Replace, From, To,
               Indent_Params, Indent_Offset, Get_Case_Exceptions);
         else
            Format_Buffer
              (Lang, Buffer, Replace, From, To,
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

            Create
              (Replace_Cmd,
               Buffer,
               Editable_Line_Type (Line),
               Start_Column,
               Editable_Line_Type (Line),
               End_Column,
               Replace);
            Enqueue (Buffer, Command_Access (Replace_Cmd));
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

      if Indent_Params.Casing_Policy = On_The_Fly
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
              (Buffer, Buffer_Line_Type (Line + 1), Force_Compute => False);
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
      Start_Group (Buffer.Queue);

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
      Tab_Width : constant Integer := Integer (Buffer.Tab_Width);
      Max       : constant Positive :=
                    Positive (Get_Pref (Highlight_Column));
      --  Right margin, wrap line if longer than Max character

      From, To  : Gtk_Text_Iter;
      From_Line : Editable_Line_Type;
      To_Line   : Editable_Line_Type;
      --  Block to work on

      B_Sep     : GNAT.Strings.String_Access; -- Spaces/TAB before comment line
      Sep       : GNAT.Strings.String_Access; -- Comment symbol
      After     : Natural;                    -- Spaces after comment symbol
      --  Note that if Sep is null we are not working on a comment, in this
      --  case Indent and After must be ignored.

      New_Text  : Unbounded_String;
      --  The new content, will replace the selection

      Is_First  : Boolean := True; -- True if first iteration (first line)
      Is_Start  : Boolean := True; -- True if at the start of a new line
      Line_Size : Natural := 0;    -- Current line size

      procedure Analyse_Line
        (Buffer : Source_Buffer;
         Line   : Editable_Line_Type);
      --  Analyse line content and set refill separator if comment

      procedure Add_Result (Str : String);
      --  Add Str to the result string, Last_Line is set to true if Str is
      --  the last line content.

      procedure Add (Str : String);
      pragma Inline (Add);
      --  Add Str to the result string

      procedure Add_EOL;
      --  Add LF to the result string

      function Is_Empty (Line : Src_String) return Boolean;
      --  Returns True if Line is empty (no content or only spaces/HT)

      ---------
      -- Add --
      ---------

      procedure Add (Str : String) is
      begin
         Append (New_Text, Str);

         for K in Str'Range loop
            if Str (K) = ASCII.HT then
               Line_Size := Line_Size + Tab_Width;
            else
               Line_Size := Line_Size + 1;
            end if;
         end loop;
      end Add;

      -------------
      -- Add_EOL --
      -------------

      procedure Add_EOL is
      begin
         Add ((1 => ASCII.LF));
         Is_Start := True;
         Line_Size := 0;
      end Add_EOL;

      ----------------
      -- Add_Result --
      ----------------

      procedure Add_Result (Str : String) is

         procedure Add_Word (Word : String);
         --  Add Word to the result string, properly insert a new-line if
         --  line goes over the right margin.

         --------------
         -- Add_Word --
         --------------

         procedure Add_Word (Word : String) is
            Space : constant String := (1 => ' ');
         begin
            --  Does this word fit on the line

            if Line_Size + Word'Length + 1 > Max then
               --  +1 for the space before the word
               Add_EOL;
            end if;

            if Is_Start then
               --  Add spaces before comment line

               if B_Sep /= null then
                  Add (B_Sep.all);
               end if;

               if Sep /= null then
                  if Is_First then
                     Is_First := False;
                     Add (Sep.all);

                  else
                     if Sep'Length > 1
                       and then Sep (Sep'First) /= Sep (Sep'First + 1)
                     then
                        --  Comment separator is composed of 2 different
                        --  characters, treat it a a multi-line comment, just
                        --  add Sep'length spaces

                        for K in Sep'Range loop
                           Add (Space);
                        end loop;

                     else
                        Add (Sep.all);
                     end if;
                  end if;

                  --  Add spaces after comment separator

                  for K in 1 .. After loop
                     Add (Space);
                  end loop;
               end if;

               Is_Start := False;

            else
               --  Add a space before this word, this is not done for the
               --  first word on the line.
               Add (Space);
            end if;

            Add (Word);
         end Add_Word;

         F, L : Natural;

      begin
         F := Str'First;

         --  Skip spaces and HT

         while F < Str'Last
           and then (Str (F) = ' ' or else Str (F) = ASCII.HT)
         loop
            F := F + 1;
         end loop;

         --  Skip comment separator

         if Sep /= null
           and then F + Sep'Length - 1 <= Str'Last
           and then Str (F .. F + Sep'Length - 1) = Sep.all
         then
            F := F + Sep'Length;
         end if;

         --  Is there something remaining

         declare
            Is_Empty : Boolean := True;
            --  True if only spaces or HT remaining on the line
            N        : Natural := 0;
            --  Number of spaces after the comment separator if any
         begin
            for K in F .. Str'Last loop
               if Str (K) /= ' ' and then Str (K) /= ASCII.HT then
                  Is_Empty := False;
                  exit;
               end if;
               N := N + 1;
            end loop;

            if Is_Empty then
               --  No word found, we want to keep the current empty comment
               --  line as-is.
               Add_EOL; --  Terminate current line
               Add_Word ("");
               Add_EOL;

            else
               --  There is something after, record the new indent level
               --  inside the comment.
               After := N;
            end if;
         end;

         --  Read all words on the line

         while F <= Str'Last loop
            --  Skip spaces

            while F < Str'Last and then Str (F) = ' ' loop
               F := F + 1;
            end loop;

            --  Get word

            L := F;
            while L < Str'Last and then Str (L) /= ' ' loop
               L := L + 1;
            end loop;

            if Str (L) = ' ' then
               Add_Word (Str (F .. L - 1));
            else
               Add_Word (Str (F .. L));
            end if;

            F := L + 1;
         end loop;
      end Add_Result;

      ------------------
      -- Analyse_Line --
      ------------------

      procedure Analyse_Line
        (Buffer : Source_Buffer;
         Line   : Editable_Line_Type)
      is
         F, L : Natural;

         procedure Detect_Start_Pattern
           (Line : Src_String; P : in out Natural);
         --  Detect possible patterns starting a line.
         --  Currently, only detect Ada, C, C++ and Shell comments
         --  Start at position P in Line. Set P to last character in pattern.

         --------------------------
         -- Detect_Start_Pattern --
         --------------------------

         procedure Detect_Start_Pattern
           (Line : Src_String; P : in out Natural) is
         begin
            if P < Line.Length + 1
              and then
                (Line.Contents (P + 1 .. P + 2) = "/*"
                 or else Line.Contents (P + 1 .. P + 2) = "//"
                 or else Line.Contents (P + 1 .. P + 2) = "--")
            then
               --  This is a 2 characters comment separator
               P := P + 2;

            elsif P < Line.Length
              and then Line.Contents (P + 1) = '#'
            then
               --  A single character comment separator
               P := P + 1;
            end if;
         end Detect_Start_Pattern;

      begin
         declare
            First_Line : constant Src_String := Get_String (Buffer, Line);
         begin
            if First_Line.Contents = null then
               --  Emptry line, nothing to do
               return;
            end if;

            --  Get current indent level based on the first line. Note that
            --  this routine will change the indentation level based on the
            --  first Line.

            F := 0;
            while F + 1 <= First_Line.Length
              and then (First_Line.Contents (F + 1) = ' '
                        or else First_Line.Contents (F + 1) = ASCII.HT)
            loop
               F := F + 1;
            end loop;

            if F > 0 then
               B_Sep := new String'(First_Line.Contents (1 .. F));
            end if;

            --  Get comment separator if any

            L := F;
            F := F + 1;

            Detect_Start_Pattern (First_Line, L);

            if L >= F then
               Sep := new String'(First_Line.Contents (F .. L));
            end if;

            --  Get spaces after comment separator

            if Sep /= null then
               After := 0;
               while L < First_Line.Length + 1
                 and then First_Line.Contents (L + 1) = ' '
               loop
                  L := L + 1;
                  After := After + 1;
               end loop;
            end if;
         end;
      end Analyse_Line;

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

   begin -- Do_Refill
      if not Buffer.Writable then
         End_Action (Buffer);
         return False;
      end if;

      declare
         Result : Boolean;
      begin
         Get_Selection_Bounds (Buffer, From, To, Result);

         if Result then
            --  Do not consider a line selected if only the first character
            --  is selected.

            if Get_Line_Offset (To) = 0 then
               Backward_Char (To, Result);
            end if;

            --  Do not consider a line selected if only the last character is
            --  selected.

            if Ends_Line (From) then
               Forward_Char (From, Result);
            end if;

            --  Get editable lines

            From_Line := Get_Editable_Line
              (Buffer, Buffer_Line_Type (Get_Line (From) + 1));
            To_Line := Get_Editable_Line
              (Buffer, Buffer_Line_Type (Get_Line (To) + 1));

         else
            --  No selection, get the current position
            declare
               Dummy : Character_Offset_Type;
            begin
               Get_Cursor_Position (Buffer, From_Line, Dummy);
               To_Line := From_Line;
            end;
         end if;
      end;

      Analyse_Line (Buffer, From_Line);

      --  Create new content

      for K in From_Line .. To_Line loop
         declare
            Line : constant Src_String := Get_String (Buffer, K);
         begin
            if Is_Empty (Line) then
               --  Terminate current line and skip one line
               Add_EOL;
               Add_EOL;
            else
               Add_Result (Line.Contents (1 .. Line.Length));
            end if;
         end;
      end loop;

      --  Add final LF

      Add_EOL;

      --  Replace text with the new one

      Replace_Slice
        (Buffer, From_Line, 1, To_Line + 1, 1, To_String (New_Text));

      GNAT.Strings.Free (B_Sep);
      GNAT.Strings.Free (Sep);

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

   procedure Free (X : in out Line_Info_Width) is
   begin
      if X.Info /= null then
         Free (X.Info.all);
         Unchecked_Free (X.Info);
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
      Iter      : Gtk_Text_Iter;
      Success   : Boolean;
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
      Start_Iter : Gtk_Text_Iter;
      End_Iter   : Gtk_Text_Iter;
      Ignored    : Boolean;

      Tags : Highlighting_Tags renames Buffer.Syntax_Tags;

      Entity_Kind : Language_Entity;

   begin
      if not Buffer.Highlight_Needed then
         return;
      end if;

      Get_Iter_At_Mark (Buffer, Start_Iter, Buffer.First_Highlight_Mark);
      Get_Iter_At_Mark (Buffer, End_Iter, Buffer.Last_Highlight_Mark);

      --  Search the initial minimum area to re-highlight...

      Entity_Kind := Normal_Text;

      Entity_Kind_Search_Loop :
      for Current_Entity in Standout_Language_Entity loop
         if Has_Tag (Start_Iter, Tags (Current_Entity)) then
            --  This means that we are in a highlighted region. The minimum
            --  region to re-highlight starts from the begining of the current
            --  region to the end of the following region.

            Entity_Kind := Current_Entity;

            Backward_To_Tag_Toggle
              (Start_Iter, Tags (Current_Entity), Result => Ignored);
            Forward_To_Tag_Toggle (End_Iter, Tags (Entity_Kind), Ignored);
            Forward_To_Tag_Toggle (End_Iter, Result => Ignored);

            exit Entity_Kind_Search_Loop;

         elsif Begins_Tag (End_Iter, Tags (Current_Entity))
           or else Ends_Tag (Start_Iter, Tags (Current_Entity))
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
            Backward_To_Tag_Toggle (Start_Iter, Result => Ignored);
            Forward_To_Tag_Toggle (End_Iter, Result => Ignored);

            exit Entity_Kind_Search_Loop;
         end if;
      end loop Entity_Kind_Search_Loop;

      if Entity_Kind = Normal_Text then
         --  We are inside a normal text region. Just re-highlight this region

         Backward_To_Tag_Toggle (Start_Iter, Result => Ignored);
         Forward_To_Tag_Toggle (End_Iter, Result => Ignored);
      end if;

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
      end if;

      return Set;
   end Position_Set_Explicitely;

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
                      (0, 0, 0, 0, 0, null, Language.Cat_Unknown, null);
      L           : Buffer_Line_Type;
      New_L       : Buffer_Line_Type;
      Block       : Block_Record;
   begin
      L := Get_Buffer_Line (Editor, Line);

      Block := Get_Block (Editor, L, Force_Compute => True);

      if Block.Block_Type = Cat_Unknown
        and then Block.Indentation_Level = 0
      then
         return Empty_Block;
      end if;

      while L > 1 loop
         if Block.Block_Type in Namespace_Category
           or else Block.Block_Type in Subprogram_Category
         then
            if Block.Name /= null then
               return Block;
            else
               return Empty_Block;
            end if;
         end if;

         if Block.First_Line > 1 then
            New_L := Get_Buffer_Line (Editor, Block.First_Line - 1);

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
      if Line not in Buffer.Editable_Lines'Range then
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
      Tab_Len : constant Visible_Column_Type := Visible_Column_Type
        (Buffer.Tab_Width);
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

end Src_Editor_Buffer;
