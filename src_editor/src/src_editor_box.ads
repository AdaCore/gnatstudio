------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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

--  <description>
--  This package provides an object that represents the source editor.
--  The source editor is composed of the following entities:
--    - A Source_View, with vertical and horizontal scrollbars
--    - A status bar at the bottom containing
--        - the current file name
--        - the line and column number of the insert cursor
--  </description>

with Glib;

with Gtk.Box;
with Gtk.Event_Box;
with Gtk.Menu;
with Gtk.Handlers;
with Gtk.Text_Mark;

with Basic_Types;           use Basic_Types;
with GPS.Editors;           use GPS.Editors;
with GPS.Kernel;
with GPS.Kernel.Modules.UI;
with GPS.Kernel.Messages;   use GPS.Kernel.Messages;

with Src_Editor_Buffer;     use Src_Editor_Buffer;
with Src_Editor_Status_Bar; use Src_Editor_Status_Bar;
with GNATCOLL.Projects;
with Src_Editor_View;
with GNATCOLL.VFS;
with Xref;

package Src_Editor_Box is

   type Source_Editor_Box_Record is new Gtk.Event_Box.Gtk_Event_Box_Record
     with private;
   type Source_Editor_Box is access all Source_Editor_Box_Record;

   procedure Gtk_New
     (Box         : out Source_Editor_Box;
      Project     : GNATCOLL.Projects.Project_Type;
      Kernel      : GPS.Kernel.Kernel_Handle;
      Filename    : GNATCOLL.VFS.Virtual_File)
     with Pre => not Filename.Is_Directory;
   procedure Initialize
     (Box         : access Source_Editor_Box_Record'Class;
      Project     : GNATCOLL.Projects.Project_Type;
      Kernel      : GPS.Kernel.Kernel_Handle;
      Filename    : GNATCOLL.VFS.Virtual_File);
   --  Perform the initialization of the given editor box.
   --
   --  Project is the one controlling the file. There might be several
   --  possibilities when using aggregate projects, and we need to know the
   --  exact project to resolve things like cross-references.
   --
   --  If Filename points to an existin file:
   --    Load the file into the buffer. If Lang_Autodetect is set to True, then
   --    the editor tries to automatically set the language based on the
   --    Filename. Otherwise, the language remains unchanged. After the file is
   --    loaded into the buffer, the buffer is syntax-highlighted if Lang is
   --    set. Filename is also stored in the Editor.
   --  Otherwise:
   --     an empty file is loaded.
   --
   --  Note that if Lang_Autodetect is True, and the editor could not guess
   --  the language from the filename, then Lang will be unset, and syntax
   --  highlighting will be deactivated.

   procedure Create_New_View
     (Box     : out Source_Editor_Box;
      Project : GNATCOLL.Projects.Project_Type;
      Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      Source  : access Source_Editor_Box_Record);
   --  Create a new view of the given box.
   --  ??? Do we want to copy the font attributes as well, or do we want
   --  to add another parameter?

   function Get_Kernel
     (Box : access Source_Editor_Box_Record) return GPS.Kernel.Kernel_Handle;
   --  Accessor to the Kernel field

   function Get_View (Editor : access Source_Editor_Box_Record)
      return Src_Editor_View.Source_View;
   --  Return the source view associated with the box

   function Get_Buffer (Editor : access Source_Editor_Box_Record)
      return Src_Editor_Buffer.Source_Buffer;
   --  Return the source buffer associated with the box

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

   function Get_Project
     (Editor : access Source_Editor_Box_Record)
      return GNATCOLL.Projects.Project_Type;
   --  Return the project for this view. This is used in the case of
   --  aggregate projects

   procedure Save_To_File
     (Editor   : access Source_Editor_Box_Record;
      Filename : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Success  : out Boolean;
      Force    : Boolean := False);
   --  Save the buffer to the given file.
   --  Success is set to false if the buffer could not be saved.
   --  If filename is null, use the filename associated with Editor.

   procedure Set_Cursor_Location
     (Editor       : access Source_Editor_Box_Record;
      Line         : Editable_Line_Type;
      Column       : Character_Offset_Type := 1;
      Force_Focus  : Boolean := True;
      Raise_Child  : Boolean := False;
      Centering    : GPS.Editors.Centering_Type := GPS.Editors.Minimal;
      Extend_Selection : Boolean := False);
   --  Move the insert cursor to the given location. Success is set to False
   --  if the position is outside of the buffer.
   --  If Force_Focus is False, then the editor will not grab the focus
   --  before setting the cursor position.
   --  If Raise_Child, the MDI child for Editor will also get raised.
   --  Centering indicates the behavior when scrolling the editor to reveal
   --  the cursor location.
   --  If Extend_Selection is True, extend the selection from the current
   --  bound to the given position.

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

   procedure Update_Subprogram_Name
     (Box : not null access Source_Editor_Box_Record'Class);
   --  Update the name of the current subprogram

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
      Line   : Editable_Line_Type) return Natural;
   --  Return the line number where block enclosing Line starts. Returns 0
   --  if Line is not in a block.

   function Get_Block_End
     (Editor : access Source_Editor_Box_Record;
      Line   : Editable_Line_Type) return Natural;
   --  Return the line number where block enclosing Line ends. Returns 0
   --  if Line is not in a block.

   function Get_Block_Name
     (Editor : access Source_Editor_Box_Record;
      Line   : Editable_Line_Type) return String;
   --  Return the name for the block enclosing Line

   function Get_Block_Type
     (Editor : access Source_Editor_Box_Record;
      Line   : Editable_Line_Type) return String;
   --  Return the type for block enclosing Line. Returns 0 if Line is not
   --  in a block.

   function Get_Block_Level
     (Editor : access Source_Editor_Box_Record;
      Line   : Editable_Line_Type) return Natural;
   --  Return the line number where block enclosing Line ends. Returns 0
   --  if Line is not in a block.

   function Get_Buffer
     (Editor : access Source_Editor_Box_Record) return String;
   --  Return the contents of the entire buffer

   procedure Set_Writable
     (Editor   : access Source_Editor_Box_Record;
      Writable : Boolean;
      Explicit : Boolean := False);
   --  Change the writable status of the editor (the underlying buffer
   --  actually). Explicit should be True when it is an explicit query from
   --  the customer.

   function Get_Subprogram_Name
     (Editor : access Source_Editor_Box_Record;
      Line   : Editable_Line_Type) return String;
   --  Return the name for the subprogram enclosing Line.

   procedure Check_Writable (Editor : access Source_Editor_Box_Record);
   --  Check whether the file in Editor is writable, and update the read-only
   --  label accordingly.
   --  This only works on Editors which are displayed in an MDI child.

   ---------------------
   -- Contextual menu --
   ---------------------

   type Goto_Body_Menu_Label is new
     GPS.Kernel.Modules.UI.Contextual_Menu_Label_Creator_Record
     with null record;
   overriding function Get_Label
     (Creator : access Goto_Body_Menu_Label;
      Context : GPS.Kernel.Selection_Context) return String;
   --  Return the label to use for the contextual menu "Goto body"

   type Goto_Dispatch_Declaration_Submenu is new
     GPS.Kernel.Modules.UI.Submenu_Factory_Record with null record;
   overriding procedure Append_To_Menu
     (Factory : access Goto_Dispatch_Declaration_Submenu;
      Context : GPS.Kernel.Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  Adds submenus to the "Goto dispatching declaration" contextual menu

   type Goto_Dispatch_Body_Submenu is new
     GPS.Kernel.Modules.UI.Submenu_Factory_Record with null record;
   overriding procedure Append_To_Menu
     (Factory : access Goto_Dispatch_Body_Submenu;
      Context : GPS.Kernel.Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  Adds submenus to the "Goto dispatching body" contextual menu

   ----------------------
   -- Line information --
   ----------------------

   procedure Add_File_Information
     (Editor     : access Source_Editor_Box_Record;
      Identifier : String;
      Messages   : Message_Array);
   --  See GPS.Kernel.Modules for more information

   procedure Remove_Line_Information_Column
     (Editor     : access Source_Editor_Box_Record;
      Identifier : String);
   --  See GPS.Kernel.Modules for more information

   procedure Undo (Editor : access Source_Editor_Box_Record);
   procedure Redo (Editor : access Source_Editor_Box_Record);
   --  Undo/Redo last edit command

   function Needs_To_Be_Saved
     (Box    : not null access Source_Editor_Box_Record;
      Single : Boolean) return Boolean;
   --  Return True in case the underlying buffer needs to be saved and there is
   --  either only one view or Single is False and the box is the last that had
   --  the focus. False is returned otherwise.

   procedure Go_To_Closest_Match
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      Filename : GNATCOLL.VFS.Virtual_File;
      Project  : GNATCOLL.Projects.Project_Type;
      Line     : Editable_Line_Type;
      Column   : Visible_Column_Type;
      Entity   : Xref.Root_Entity'Class);
   --  Open an editor for Filename. Go to Line, Column, or the nearest
   --  occurrence of Entity close by.

   procedure Go_To_Closest_Match
     (Kernel      : access GPS.Kernel.Kernel_Handle_Record'Class;
      Filename    : GNATCOLL.VFS.Virtual_File;
      Project     : GNATCOLL.Projects.Project_Type;
      Line        : Editable_Line_Type;
      Column      : Visible_Column_Type;
      Entity_Name : String);
   --  Open an editor for Filename. Go to Line, Column, or the nearest
   --  occurrence of Entity_Name close by.

   function Has_Specification
     (Context : GPS.Kernel.Selection_Context) return Boolean;
   --  Whether the Entity referenced in context has a specification other than
   --  at the location described in Context

   function Has_Body (Context : GPS.Kernel.Selection_Context) return Boolean;
   --  Whether the Entity referenced in context has a body other than at the
   --  location described in Context

   procedure Add_Navigation_Location
     (Source : access Source_Editor_Box_Record'Class);
   --  Add a navigation command to mark the given location in the source
   --  editor. Used to remember the location before Xref navigation.

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

   type Source_Editor_Box_Record is new
     Gtk.Event_Box.Gtk_Event_Box_Record
   with record
      Box                  : Gtk.Box.Gtk_Box;
      Kernel               : GPS.Kernel.Kernel_Handle;

      Source_View          : Src_Editor_View.Source_View;
      Source_Buffer        : Src_Editor_Buffer.Source_Buffer;

      Status_Bar           : Source_Editor_Status_Bar;
      --  The status bar

      --  The non graphical attributes

      Status_Handler       : Gtk.Handlers.Handler_Id;
      --  Handler connected to the signal "status_changed"
      --  from the source buffer.
   end record;
   --  Note that it is straightforward to retrieve the Source_Buffer from
   --  the Source_View, thus making the Source_View field not absolutely
   --  necessary. But it is kept nonetheless for performance reasons, since
   --  we have to retrieve the buffer for lots of operation...
   --  ??? Is the latter true?

end Src_Editor_Box;
