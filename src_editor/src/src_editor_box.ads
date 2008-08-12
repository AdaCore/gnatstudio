-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                   Copyright (C) 2001-2008, AdaCore                --
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

--  <description>
--  This package provides an object that represents the source editor.
--  The source editor is composed of the following entities:
--    - A Source_View, with vertical and horizontal scrollbars
--    - A status bar at the bottom containing
--        - the current file name
--        - the line and column number of the insert cursor
--  </description>

with Ada.Unchecked_Deallocation;

with Glib;
with Glib.Object;
with Gdk.Event;

with Gtk.Box;
with Gtk.Frame;
with Gtk.Label;
with Gtk.Menu;
with Gtk.Main;
with Gtk.Handlers;
with Gtk.Separator;
with Gtk.Text_Mark;

with Basic_Types;           use Basic_Types;
with Language;
with Language_Handlers;
with GPS.Kernel;
with GPS.Kernel.Modules;
with GPS.Kernel.Standard_Hooks;
with Src_Editor_Buffer;     use Src_Editor_Buffer;
with Src_Editor_View;
with GNATCOLL.VFS;
with Entities;
with Commands.Interactive;  use Commands, Commands.Interactive;

package Src_Editor_Box is

   type Source_Editor_Box_Record is new Gtk.Box.Gtk_Box_Record
     with private;
   type Source_Editor_Box is access all Source_Editor_Box_Record;

   procedure Gtk_New
     (Box    : out Source_Editor_Box;
      Kernel : GPS.Kernel.Kernel_Handle;
      Lang   : Language.Language_Access := null);
   --  Create a new Source_Editor_Box. It must be destroyed after use
   --  (see procedure Destroy below).

   procedure Initialize
     (Box    : access Source_Editor_Box_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle;
      Source : Source_Buffer := null;
      Lang   : Language.Language_Access);
   --  Perform the initialization of the given editor box. If Source_Buffer
   --  is null, then a new buffer will automatically be created. Otherwise,
   --  the editor creates a new editor for the same Source_Buffer.

   procedure Create_New_View
     (Box    : out Source_Editor_Box;
      Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Source : access Source_Editor_Box_Record);
   --  Create a new view of the given box.
   --  ??? Do we want to copy the font attributes as well, or do we want
   --  to add another parameter?

   function Get_Kernel
     (Box : access Source_Editor_Box_Record) return GPS.Kernel.Kernel_Handle;
   --  Accessor to the Kernel field.

   function Get_View (Editor : access Source_Editor_Box_Record)
      return Src_Editor_View.Source_View;
   --  Return the source view associated with the box

   function Get_Buffer (Editor : access Source_Editor_Box_Record)
      return Src_Editor_Buffer.Source_Buffer;
   --  Return the source buffer associated with the box

   procedure Set_Writable
     (Editor   : access Source_Editor_Box_Record;
      Writable : Boolean;
      Explicit : Boolean := False);
   --  Change the writable status of the editor (the underlying buffer
   --  actually). Explicit should be True when it is an explicit query from
   --  the customer.

   procedure Read_Only_By_Default (State : Boolean := True);
   --  If State if True, this will set the Writable state of new
   --  Editors to False, otherwise, the writable state will be set based on
   --  the file system flags.

   -----------
   -- Views --
   -----------

   type Views_Array is array (Natural range <>) of Source_Editor_Box;

   function Get_Views (Buffer : Source_Buffer) return Views_Array;
   --  Return the list of views for this buffer

   ------------------------------------
   -- Source_Buffer related services --
   ------------------------------------

   function Get_Filename
     (Editor : access Source_Editor_Box_Record)
      return GNATCOLL.VFS.Virtual_File;
   --  Return the filename associated the given Editor. Return the empty
   --  string if Editor does not have any filename.

   procedure Load_File
     (Editor          : access Source_Editor_Box_Record;
      Filename        : GNATCOLL.VFS.Virtual_File;
      Lang_Autodetect : Boolean := True;
      Force_Focus     : Boolean := True;
      Success         : out Boolean);
   --  Load the file into the buffer. If Lang_Autodetect is set to True, then
   --  the editor tries to automatically set the language based on the
   --  Filename. Otherwise, the language remains unchanged. After the file is
   --  loaded into the buffer, the buffer is syntax-highlighted if Lang is set.
   --  Filename is also stored in the Editor.
   --
   --  Note that if Lang_Autodetect is True, and the editor could not guess
   --  the language from the filename, then Lang will be unset, and syntax
   --  highlighting will be deactivated.

   procedure Load_Empty_File
     (Editor          : access Source_Editor_Box_Record;
      Filename        : GNATCOLL.VFS.Virtual_File;
      Lang_Handler    : Language_Handlers.Language_Handler;
      Lang_Autodetect : Boolean := True);
   --  Similar to Load_File, but assume that Filename is a new file that
   --  is not present on the file system.

   procedure Save_To_File
     (Editor   : access Source_Editor_Box_Record;
      Filename : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Success  : out Boolean;
      Force    : Boolean := False);
   --  Save the buffer to the given file.
   --  Success is set to false if the buffer could not be saved.
   --  If filename is null, use the filename associated with Editor.

   procedure Check_Timestamp_And_Reload
     (Editor        : access Source_Editor_Box_Record;
      Interactive   : Boolean;
      Always_Reload : Boolean);
   --  Check whether the timestamp changed on the disk.
   --  If yes, then
   --    if Interactive is True, display a dialog asking the user whether he
   --      wants to reload the file.
   --    if Interactive is False, reload the file without asking.
   --  In Always_Reload, then the file will always be reloaded.

   procedure Set_Cursor_Location
     (Editor      : access Source_Editor_Box_Record;
      Line        : Editable_Line_Type;
      Column      : Character_Offset_Type := 1;
      Force_Focus : Boolean := True;
      Centering   : Centering_Type := Minimal);
   --  Move the insert cursor to the given location. Success is set to False
   --  if the position is outside of the buffer.
   --  If Force_Focus is False, then the editor will not grab the focus
   --  before setting the cursor position.
   --  Centering indicates the behavior when scrolling the editor to reveal
   --  the cursor location.

   procedure Replace_Sliced
     (Editor       : access Source_Editor_Box_Record;
      Start_Line   : Positive;
      Start_Column : Positive;
      End_Line     : Positive;
      End_Column   : Positive;
      Text         : String);
   --  Replace the text between the start and end locations by Text.
   --
   --  The validity of the given locations must be verified before invoking
   --  this procedure. An incorrect location  will cause an Assertion_Failure
   --  when compiled with assertion checks, or an undefined behavior otherwise.

   procedure Goto_Declaration_Or_Body
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      To_Body : Boolean;
      Editor  : access Source_Editor_Box_Record'Class;
      Context : GPS.Kernel.Selection_Context);
   --  Perform a Declaration-or-Body cross-reference for the entity
   --  located at (Line, Column) in Editor, or in the current editor if
   --  Editor is null. If To_Body is True, then display the next body of the
   --  entity, otherwise display its declaration.
   --  If either Line or Column is null, then the position of the insert cursor
   --  is used instead. Highlight the entity found, opening a new editor if
   --  needed (this may depend on the user preferences).

   procedure Grab_Focus (Editor : access Source_Editor_Box_Record);
   --  Set the focus on the source view

   procedure Clear_Subprogram_Name
     (Editor : access Source_Editor_Box_Record);
   --  Clear the subprogram name label (set to empty string)

   procedure Show_Subprogram_Name
     (Box             : Source_Editor_Box;
      Subprogram_Name : String);
   --  Show the name of the current subprogram

   procedure Scroll_To_Mark
     (Editor : access Source_Editor_Box_Record;
      Mark   : Gtk.Text_Mark.Gtk_Text_Mark;
      Length : Natural := 0);
   --  Scroll Mark onscreen, and place the cursor on Mark.
   --  Lenght is the length of text that should be selected after
   --  Mark.

   function Get_Last_Line
     (Editor : access Source_Editor_Box_Record) return Positive;
   --  Return the number of the last line in the file

   function Get_Block_Start
     (Editor : access Source_Editor_Box_Record;
      Line   : Src_Editor_Buffer.Editable_Line_Type) return Natural;
   --  Return the line number where block enclosing Line starts. Returns 0
   --  if Line is not in a block.

   function Get_Block_End
     (Editor : access Source_Editor_Box_Record;
      Line   : Src_Editor_Buffer.Editable_Line_Type) return Natural;
   --  Return the line number where block enclosing Line ends. Returns 0
   --  if Line is not in a block.

   function Get_Block_Name
     (Editor : access Source_Editor_Box_Record;
      Line   : Src_Editor_Buffer.Editable_Line_Type) return String;
   --  Return the name for the block enclosing Line

   function Get_Block_Type
     (Editor : access Source_Editor_Box_Record;
      Line   : Src_Editor_Buffer.Editable_Line_Type) return String;
   --  Return the type for block enclosing Line. Returns 0 if Line is not
   --  in a block.

   function Get_Block_Level
     (Editor : access Source_Editor_Box_Record;
      Line   : Src_Editor_Buffer.Editable_Line_Type) return Natural;
   --  Return the line number where block enclosing Line ends. Returns 0
   --  if Line is not in a block.

   function Get_Buffer
     (Editor : access Source_Editor_Box_Record) return String;
   --  Return the contents of the entire buffer.

   function Get_Subprogram_Name
     (Editor : access Source_Editor_Box_Record;
      Line   : Src_Editor_Buffer.Editable_Line_Type :=
        Src_Editor_Buffer.Editable_Line_Type'Last) return String;
   --  Return the name for the subprogram enclosing Line.
   --  If Line is left to its default value, then the subprogram at the current
   --  line is computed.

   function Get_Subprogram
     (Editor : access Source_Editor_Box_Record;
      Line   : Src_Editor_Buffer.Editable_Line_Type :=
        Src_Editor_Buffer.Editable_Line_Type'Last)
      return Entities.Entity_Information;
   --  Same as above, but returns a pointer to the declaration of the
   --  subprogram.

   procedure Check_Writable (Editor : access Source_Editor_Box_Record);
   --  Check whether the file in Editor is writable, and update the read-only
   --  label accordingly.

   ---------------------
   -- Contextual menu --
   ---------------------

   type In_Line_Numbers_Area_Filter is new GPS.Kernel.Action_Filter_Record
      with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access In_Line_Numbers_Area_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean;
   --  True if the event currently processed was in an editor's line numbers
   --  area

   type Has_Body_Filter is new GPS.Kernel.Action_Filter_Record
     with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Has_Body_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean;
   --  True if the current entity has a body

   type Has_Type_Filter is new GPS.Kernel.Action_Filter_Record
     with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Has_Type_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean;
   --  True if the current entity has a type

   type Is_Dispatching_Filter is new GPS.Kernel.Action_Filter_Record
     with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Is_Dispatching_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean;
   --  True if the current entity has a type

   type Has_Other_File_Filter is new GPS.Kernel.Action_Filter_Record
     with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Has_Other_File_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean;
   --  True if the current file has a spec/body

   type Goto_Line_Command is new Interactive_Command with record
      Kernel : GPS.Kernel.Kernel_Handle;
   end record;
   overriding function Execute
     (Command : access Goto_Line_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Ask the user on which line to jump to

   type Goto_Other_File_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Goto_Other_File_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Go to the file spec/body, depending on what is currently open

   type Goto_Declaration_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Goto_Declaration_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Go to the declaration of the entity in the context

   type Goto_Next_Body_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Goto_Next_Body_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Go to the body of the entity in the context

   type Goto_Dispatch_Declaration_Submenu is new
     GPS.Kernel.Modules.Submenu_Factory_Record with null record;
   overriding procedure Append_To_Menu
     (Factory : access Goto_Dispatch_Declaration_Submenu;
      Object  : access Glib.Object.GObject_Record'Class;
      Context : GPS.Kernel.Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  Adds submenus to the "Goto dispatching declaration" contextual menu

   type Goto_Dispatch_Body_Submenu is new
     GPS.Kernel.Modules.Submenu_Factory_Record with null record;
   overriding procedure Append_To_Menu
     (Factory : access Goto_Dispatch_Body_Submenu;
      Object  : access Glib.Object.GObject_Record'Class;
      Context : GPS.Kernel.Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  Adds submenus to the "Goto dispatching body" contextual menu

   type Goto_Body_Menu_Label is new
     GPS.Kernel.Modules.Contextual_Menu_Label_Creator_Record
     with null record;
   overriding function Get_Label
     (Creator : access Goto_Body_Menu_Label;
      Context : GPS.Kernel.Selection_Context) return String;
   --  Return the label to use for the contextual menu "Goto body"

   type Goto_Type_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Goto_Type_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Go to the type declaration of the entity in the context

   procedure Get_Contextual_Menu
     (Context : in out GPS.Kernel.Selection_Context;
      Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      Object  : access Glib.Object.GObject_Record'Class;
      Event   : Gdk.Event.Gdk_Event;
      Menu    : Gtk.Menu.Gtk_Menu);
   --  Return the contextual menu to use for the source box.
   --  This function is also used to create the context for
   --  GPS.Kernel.Get_Current_Context, and might be called with Event and
   --  Menu set to null.

   ----------------------
   -- Line information --
   ----------------------

   procedure Add_File_Information
     (Editor     : access Source_Editor_Box_Record;
      Identifier : String;
      Info       : GPS.Kernel.Standard_Hooks.Line_Information_Data);
   --  See GPS.Kernel.Modules for more information.

   procedure Create_Line_Information_Column
     (Editor     : access Source_Editor_Box_Record;
      Identifier : String;
      Every_Line : Boolean);
   --  See GPS.Kernel.Modules for more information.

   procedure Remove_Line_Information_Column
     (Editor     : access Source_Editor_Box_Record;
      Identifier : String);
   --  See GPS.Kernel.Modules for more information.

   procedure Undo (Editor : access Source_Editor_Box_Record);
   procedure Redo (Editor : access Source_Editor_Box_Record);
   --  Undo/Redo last edit command.

   function Needs_To_Be_Saved
     (Box    : not null access Source_Editor_Box_Record;
      Single : Boolean) return Boolean;
   --  Return True in case the underlying buffer needs to be saved and there is
   --  either only one view or Single is False and the box is the last that had
   --  the focus. False is returned otherwise.

private

   function To_Box_Line
     (B    : Source_Buffer;
      Line : Glib.Gint) return Natural;
   pragma Inline (To_Box_Line);
   --  Convert a line number in the Source Buffer to a line number in the
   --  Source Box. This conversion is necessary because line numbers start
   --  from 1 in the Source Box (this is the natural numbering for humans),
   --  whereas it starts from 0 in the Source Box.

   function To_Box_Column (Col : Glib.Gint) return Character_Offset_Type;
   pragma Inline (To_Box_Column);
   --  Convert a column number in the Source Buffer to a column number
   --  in the Source Box. Same rationale as in To_Box_Line.

   type Timestamp_Check_Mode is (Checking, Check_At_Focus, Check_At_Modify);
   --  When should the source box test the timestamp of the file on disk ?
   --  - Checking: we are already asking the user whether he wants to edit
   --  - Check_At_Focus: check at next focus event only
   --  - Check_At_Modify: check the next time the buffer is modified only
   --  The goal of this type is to avoid asking the question multiple times to
   --  the user. If he answers no the first time, we forbid editing until he
   --  has said yes.

   type Frame_Separator is record
      Frame     : Gtk.Frame.Gtk_Frame;
      Separator : Gtk.Separator.Gtk_Separator;
   end record;

   type Frames_Array is array (Natural range <>) of Frame_Separator;
   type Frames_Array_Access is access Frames_Array;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Frames_Array, Frames_Array_Access);

   type Source_Editor_Box_Record is new Gtk.Box.Gtk_Box_Record with record
      Kernel               : GPS.Kernel.Kernel_Handle;

      Timestamp_Mode       : Timestamp_Check_Mode := Check_At_Focus;

      Source_View          : Src_Editor_View.Source_View;
      Source_Buffer        : Src_Editor_Buffer.Source_Buffer;

      Label_Box            : Gtk.Box.Gtk_Hbox;

      Current_Line         : Editable_Line_Type;
      --  Cache for the current line

      --  The status bar
      Function_Label       : Gtk.Label.Gtk_Label;
      Read_Only_Label      : Gtk.Label.Gtk_Label;
      Modified_Label       : Gtk.Label.Gtk_Label;
      Overwrite_Label      : Gtk.Label.Gtk_Label;
      Cursor_Loc_Label     : Gtk.Label.Gtk_Label;

      --  The non graphical attributes

      Overwrite            : Boolean := False;

      Default_GC, Bg_GC    : Gdk.Gdk_GC;

      Cursor_Handler       : Gtk.Handlers.Handler_Id;
      --  Handler connected to the signal "cursor_position_changed" in
      --  the Source_Buffer.

      Status_Handler       : Gtk.Handlers.Handler_Id;
      --  Handler connected to the signal "status_changed"
      --  from the source buffer.

      Buffer_Info_Handler  : Gtk.Handlers.Handler_Id;
      --  Handler connected to the signal "buffer_information_changed" from the
      --  source buffer.

      Buffer_Info_Frames   : Frames_Array_Access := null;

      Check_Timestamp_Registered : Boolean := False;
      Check_Timestamp_Id         : Gtk.Main.Idle_Handler_Id;
      --  Used to protect the idle handler from being called after the box is
      --  destroyed.
   end record;
   --  Note that it is straightforward to retrieve the Source_Buffer from
   --  the Source_View, thus making the Source_View field not absolutely
   --  necessary. But it is kept nonetheless for performance reasons, since
   --  we have to retrieve the buffer for lots of operation...
   --  ??? Is the latter true?

end Src_Editor_Box;
