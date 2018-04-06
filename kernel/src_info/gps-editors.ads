------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2008-2018, AdaCore                     --
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

--  An abstract definition of what editors are and what they can do.

with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Holders;
with Ada.Finalization;  use Ada.Finalization;
with Basic_Types;       use Basic_Types;
with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.Scripts;  use GNATCOLL.Scripts;
with GNATCOLL.VFS;      use GNATCOLL.VFS;
with GNATCOLL.Xref;     use GNATCOLL.Xref;
with GPS.Markers;       use GPS.Markers;
with Commands;          use Commands;
with Language;          use Language;
with System;
with XML_Utils;

package GPS.Editors is

   --  Declarations of types

   --  The following types & subprograms are used as an abstraction to the
   --  editor facilities by the GPS code. This avoid relying directly on Gtk
   --  internals, and open the door to possible alternate implementation, such
   --  as Eclipse for GNATbench.
   --
   --  No dependency on e.g. GTK or GPS.Kernel should be added in the spec or
   --  the body of this unit, in order to be able to separate it completely
   --  from the GPS UI.

   type Editor_Buffer_Factory is abstract new Controlled with null record;
   type Editor_Buffer_Factory_Access is access all Editor_Buffer_Factory'Class;

   type Editor_Location is abstract new Controlled with null record;
   Nil_Editor_Location : constant Editor_Location'Class;

   type Editor_Mark is abstract new Controlled with null record;
   Nil_Editor_Mark : constant Editor_Mark'Class;

   type Editor_Buffer is abstract new Controlled with null record;
   Nil_Editor_Buffer : constant Editor_Buffer'Class;

   type Editor_Cursor is abstract new Controlled with null record;
   Nil_Editor_Cursor : constant Editor_Cursor'Class;

   type Editor_View is abstract new Controlled with null record;
   Nil_Editor_View : constant Editor_View'Class;

   type Editor_Overlay is abstract new Controlled with null record;
   Nil_Editor_Overlay : constant Editor_Overlay'Class;

   type Editor_Listener is abstract new Controlled with null record;
   type Editor_Listener_Access is access all Editor_Listener'Class;

   Editor_Exception : exception;
   --  Exception raised by the subprograms below when the arguments are not
   --  expected (all kind of errors, the specific error is part of the
   --  exception's Error_Message).

   --------------------
   -- Editor_Overlay --
   --------------------
   --  Overlays (ie text and presentation properties) in an editor can be
   --  represented in two ways:
   --    * historically, GPS has used a Style_Access to represent the highlight
   --      style. This only provides basic support for styling, giving access
   --      to foreground and background colors only
   --    * a more complete (and complex) version via an Editor_Overlay, which
   --      more closely models what is available in GtkAda.
   --  We provide both interfaces here for backward compatibility, until the
   --  internal GPS code no longer uses the Style_Access.

   function Name (This : Editor_Overlay) return String is abstract;
   --  Return the name associated with this overlay

   function Get_Property
     (This : Editor_Overlay; Name : String) return String  is abstract;
   function Get_Property
     (This : Editor_Overlay; Name : String) return Boolean is abstract;
   function Get_Property
     (This : Editor_Overlay; Name : String) return Integer is abstract;
   --  Retrieve the value of specific properties.
   --  See the python documentation for a list of supported properties

   procedure Set_Property
     (This : Editor_Overlay; Name : String; Value : String) is abstract;
   procedure Set_Property
     (This : Editor_Overlay; Name : String; Value : Boolean) is abstract;
   procedure Set_Property
     (This : Editor_Overlay; Name : String; Value : Integer) is abstract;
   --  Set the value of specific properties.

   package Overlay_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists
     (Editor_Overlay'Class);

   ---------------------
   -- Editor_Location --
   ---------------------

   overriding function "=" (Left, Right : Editor_Location) return Boolean
     is abstract;

   function Beginning_Of_Line
     (This : Editor_Location) return Editor_Location'Class is abstract;
   --  Return a location located at the beginning of the line on which This is

   function End_Of_Line
     (This : Editor_Location) return Editor_Location'Class is abstract;
   --  Return a location located at the end of the line on which self is

   function Block_Start
     (This        : Editor_Location;
      Update_Tree : Boolean := True) return Editor_Location'Class is abstract;
   --  Return the location of the beginning of the current block.
   --  Update_Tree indicates whether the internal cache should first be
   --  refreshed.

   function Block_End
     (This        : Editor_Location;
      Update_Tree : Boolean := True) return Editor_Location'Class is abstract;
   --  Return the location of the end of the current block

   function Block_Type
     (This        : Editor_Location;
      Update_Tree : Boolean := True) return Language_Category is abstract;
   --  Return the type of the block surrounding the location. This type
   --  indicates whether the block is a subprogram, an if statement,...

   function Block_Name
     (This        : Editor_Location;
      Subprogram  : Boolean;
      Update_Tree : Boolean := True) return String is abstract;
   --  Return the name of the current block (if Subprogram is False) or the
   --  current subprogram (if Subprogram is True).

   procedure Block_Fold (This : Editor_Location) is abstract;
   procedure Block_Unfold (This : Editor_Location) is abstract;
   --  Fold the block containing the location, ie make it invisible on the
   --  screen, except for its first line. Clicking on the icon next to this
   --  first line will unfold the block and make it visible to the user

   function Block_Level
     (This : Editor_Location) return Natural is abstract;
   --  Return the nesting level of the block surrounding the location. The
   --  definition of a block depends on the specific programming language

   function Line (This : Editor_Location) return Integer is abstract;
   --  Return the line of the location

   function Column
     (This : Editor_Location) return Visible_Column_Type is abstract;
   --  Return the column of the location

   function Line_Offset
     (This : Editor_Location) return Natural is abstract;
   --  Return the line offset of the location

   function Offset (This : Editor_Location) return Natural is abstract;
   --  Offset in the file.

   subtype Compare_Result is Integer range -1 .. 1;
   function Compare
     (This : Editor_Location; To : Editor_Location) return Compare_Result;
   --  Compare two editor locations, and return -1 if This occurs before To,
   --  0 if they are at the same position, and 1 if This occurs after To.
   --  They must both be in the same editor, this isn't checked

   function Buffer
     (This : Editor_Location) return Editor_Buffer'Class is abstract;
   --  Return the editor in which the location is found

   function Create_Mark
     (This : Editor_Location;
      Name : String := "";
      Left_Gravity : Boolean := True)
      return Editor_Mark'Class is abstract;
   --  Create a mark at that location in the buffer. The mark will stay
   --  permanently at that location, and follows if the buffer is modified. If
   --  the name is specified, this creates a named mark, which can be retrieved
   --  through a call to GPS.EditorBuffer.get_mark. If a mark with the same
   --  name already exists, it is moved to the new location, and then returned.
   --  The mark remains valid until you call Delete, even if the editor is
   --  closed. It is not preserved across GPS sessions.

   function Forward_Char
     (This  : Editor_Location;
      Count : Integer) return Editor_Location'Class is abstract;
   --  Return a new location located count characters after self (which might
   --  be several bytes). If count is negative, the location is moved backward
   --  instead

   function Forward_Word
     (This  : Editor_Location;
      Count : Integer) return Editor_Location'Class is abstract;
   --  Return a new location located count words after self. If count is
   --  negative, the location is moved backward instead. The definition of a
   --  word depends on the language used

   function Forward_Line
     (This  : Editor_Location;
      Count : Integer) return Editor_Location'Class is abstract;
   --  Return a new location located count lines after self. The location is
   --  moved back to the beginning of the line. In case self is on the last
   --  line, the beginning of the last line is returned.

   function Starts_Word (This : Editor_Location) return Boolean is abstract;
   function Ends_Word (This : Editor_Location) return Boolean is abstract;
   function Inside_Word (This : Editor_Location) return Boolean is abstract;
   --  Whether the location is on a word boundary. The definition of a word
   --  depends on the language

   function Get_Char (This : Editor_Location) return Integer is abstract;
   --  Return the character at the current location. Returns the unicode value

   procedure Search
     (This              : Editor_Location;
      Pattern           : String;
      Backward          : Boolean := False;
      Case_Sensitive    : Boolean := False;
      Regexp            : Boolean := False;
      Whole_Word        : Boolean := False;
      Scope             : String := "Whole";
      Dialog_On_Failure : Boolean := True;
      Success           : out Boolean;
      Starts            : out Editor_Location;
      Ends              : out Editor_Location) is abstract;
   --  Returns a list of two GPS.EditorLocation
   --  This function searches for the next occurrence of Pattern in the editor,
   --  starting at the given location. If there is such a match, this function
   --  returns the two locations for the beginning of the match and the end of
   --  the match. Typically, these would be used to highlight the match in the
   --  editor. When no match is found, this function returns null.
   --  Additionally, if dialog_on_failure is true then a dialog is displayed to
   --  the user asking whether the search should restart at the beginning of
   --  the buffer

   function Get_Overlays
     (This    : Editor_Location) return Overlay_Lists.List is abstract;
   --  This function returns the list of all the overlays that apply at this
   --  specific location. The color and font of the text is composed through
   --  the contents of these overlays

   function Has_Overlay
     (This    : Editor_Location;
      Overlay : Editor_Overlay'Class) return Boolean is abstract;
   --  This function returns True if the given overlay applies to the character
   --  at that location

   function Forward_Overlay
     (This    : Editor_Location;
      Overlay : Editor_Overlay'Class) return Editor_Location'Class is abstract;
   function Backward_Overlay
     (This    : Editor_Location;
      Overlay : Editor_Overlay'Class) return Editor_Location'Class is abstract;
   --  Moves to the next change in the list of overlays applying to the
   --  character. If overlay is specified, go to the next change for this
   --  specific overlay (ie the next beginning or end of range where it
   --  applies), otherwise to the next change for any overlay. If there are no
   --  more changes, the location is left at the end of the buffer

   -----------------
   -- Editor_Mark --
   -----------------

   function Line (This : Editor_Mark) return Integer is abstract;
   --  Return the current line of the mark, without opening the buffer if not
   --  open.

   function Column (This : Editor_Mark) return Visible_Column_Type is abstract;
   --  Return the current column of the mark, without opening the buffer if not
   --  open.

   function Location
     (This : Editor_Mark;
      Open : Boolean := True) return Editor_Location'Class is abstract;
   --  Returns the current location of the mark. This location will vary
   --  depending on the changes that take place in the buffer.
   --  If no editor for the location exists, one will be open if Open is True.

   function Is_Present (This : Editor_Mark) return Boolean is abstract;
   --  Returns True if mark's location is still present in the buffer

   procedure Delete (This : in out Editor_Mark) is abstract;
   --  Deletes the physical mark from the buffer.

   function Name (This : Editor_Mark) return String is abstract;
   --  Return the name of the mark (if one was provided in the call to
   --  Create_Mark)

   procedure Move
     (This : Editor_Mark; Location : Editor_Location'Class) is abstract;
   --  Move the mark to a different location

   function Create_Instance
     (This   : Editor_Mark;
      Script : access Scripting_Language_Record'Class)
      return Class_Instance is abstract;
   --  Return an Class_Instance for the mark

   package Mark_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists
     (Editor_Mark'Class);

   -------------------
   -- Editor_Cursor --
   -------------------

   procedure Move
     (This : Editor_Cursor; Where : Editor_Location'Class;
      Extend_Selection : Boolean) is abstract;

   function Get_Insert_Mark
     (This : Editor_Cursor) return Editor_Mark'Class is abstract;

   function Get_Selection_Mark
     (This : Editor_Cursor) return Editor_Mark'Class is abstract;

   procedure Set_Manual_Sync
     (This : Editor_Cursor) is abstract;
   --  This sets the buffer in "slave manual mode" regarding multi cursor
   --  insertion, with the corresponding text mark as the multi-cursors mark.
   --  This should be called before the corresponding multi cursor's action is
   --  done. This basically means that in this mode, if any action is
   --  performed :
   --  - It wont impact any multi cursor
   --  - The main cursor will not move
   --  The action will be recorded as part of the same group as the main
   --  cursor's action regarding undo/redo groups.

   package Cursors_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists
     (Editor_Cursor'Class);

   function Create_Instance
     (This   : Editor_Location;
      Script : access Scripting_Language_Record'Class)
      return Class_Instance is abstract;
   --  Return an Class_Instance for the location

   -----------------
   -- Editor_View --
   -----------------

   procedure Set_Read_Only
     (This : Editor_View; Read_Only : Boolean) is abstract;
   function Is_Read_Only (This : Editor_View) return Boolean is abstract;
   --  Indicates whether the user should be able to edit interactively through
   --  this view. Setting a view Writable/Read Only will also modify the status
   --  of the other views of the same buffer.xx

   procedure Center
     (This     : Editor_View;
      Location : Editor_Location'Class := Nil_Editor_Location) is abstract;
   --  Scrolls the view so that the location is centered. By default, the
   --  editor is centered around the location of the cursor.

   type Centering_Type is (Minimal, Center, With_Margin);
   --  Indicates the behaviour when scrolling a text view to reveal the cursor:
   --    - Minimal indicates that minimal scrolling should be performed
   --    - Center indicates that the cursor should be placed in the exact
   --      middle of the view
   --    - With_Margin indicates that minimal scrolling should occur in order
   --      to place the cursor onscreen, with a margin above and below the
   --      cursor.

   procedure Cursor_Goto
     (This       : Editor_View;
      Location   : Editor_Location'Class;
      Raise_View : Boolean := False;
      Centering  : Centering_Type := With_Margin;
      Extend_Selection : Boolean := False) is abstract;
   --  Moves the cursor at the given location. Each view of a particular buffer
   --  has its own cursor position, which is where characters typed by the user
   --  will be inserted.
   --  The view is scrolled to make the cursor visible according to the
   --  specified policy
   --  If Extend_Selection is True, extend the selection from the current
   --  selection bound to the Location.

   function Cursor
     (This : Editor_View) return Editor_Location'Class is abstract;
   --  Return the current location of the cursor in this view

   function Title
     (This : Editor_View; Short : Boolean) return String is abstract;
   --  Return the title of the editor window (or the short title if Short
   --  is True).

   function Buffer (This : Editor_View) return Editor_Buffer'Class is abstract;
   --  Return the buffer that This is displaying

   function Get_MDI_Child
     (This : Editor_View) return System.Address is abstract;
   --  Return the MDI child created for this view.
   --  The address returned is a GtkAda.MDI.MDI_Child (the C pointer, so
   --  Get_User_Data needs to be used to convert to a MDI_Child -- this is
   --  to prevent the need for a No_Strict_Aliasing pragma on MDI_Child).

   package View_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists
     (Editor_View'Class);

   -------------------
   -- Editor_Buffer --
   -------------------

   function Version
     (This : Editor_Buffer) return Integer is abstract;

   function Has_Slave_Cursors
     (This : Editor_Buffer) return Boolean is abstract;

   function Get_Main_Cursor
     (This : Editor_Buffer) return Editor_Cursor'Class is abstract;

   procedure Newline_And_Indent
     (This : Editor_Buffer) is abstract;

   function New_Location
     (This   : Editor_Buffer;
      Line   : Integer;
      Column : Visible_Column_Type) return Editor_Location'Class is abstract;
   --  Return a new location

   function New_Location_Offset
     (This   : Editor_Buffer;
      Line   : Integer;
      Column : Character_Offset_Type) return Editor_Location'Class is abstract;

   function New_Location_At_Line
     (This   : Editor_Buffer;
      Line   : Integer) return Editor_Location'Class;

   function New_Location
     (This : Editor_Buffer;
      Offset : Natural) return Editor_Location'Class is abstract;

   function New_View
     (This : Editor_Buffer) return Editor_View'Class is abstract;
   --  Creates a new view for the given buffer, and insert it in the MDI

   function Open (This : Editor_Buffer) return Editor_View'Class is abstract;
   --  Opens a view for the given buffer. If the view is already exists, it
   --  will get the focus, otherwise a new view will be opened.

   procedure Close
     (This : Editor_Buffer; Force : Boolean := False) is abstract;
   --  Close all views and internal representations of the given buffer.
   --  If Force is false and the editor has been edited and not saved, an
   --  interactive dialog is displayed asking the user whether to save.

   function File (This : Editor_Buffer) return Virtual_File is abstract;
   --  Return the name of the edited file

   function Is_Modified (This : Editor_Buffer) return Boolean is abstract;
   --  Whether the buffer has been modified since it was opened or saved.

   function Current_View
     (This : Editor_Buffer) return Editor_View'Class is abstract;
   --  Returns the last view used for this buffer, ie the last view that had
   --  the focus and through which the user might have edited the buffer's
   --  contents

   function Views
     (This : Editor_Buffer) return View_Lists.List is abstract;
   --  Returns the list of all views currently editing the buffer. There is
   --  always at least one such view. When the last view is destroyed, the
   --  buffer itself is destroyed

   function Lines_Count (This : Editor_Buffer) return Integer is abstract;
   --  Returns the total number of lines in the buffer

   function Characters_Count (This : Editor_Buffer) return Natural is abstract;
   --  Return the number of characters in the buffer

   procedure Select_Text
     (This : Editor_Buffer;
      From : Editor_Location'Class := Nil_Editor_Location;
      To   : Editor_Location'Class := Nil_Editor_Location) is abstract;
   --  Selects an area in the buffer. The boundaries are included in the
   --  selection. The order of the boundaries is irrelevant, but the cursor
   --  will be left on to. By default, From is set to the beginning of the
   --  buffer, and to to the end.

   procedure Unselect (This : Editor_Buffer) is abstract;
   --  Remove any selection that might exist in the buffer

   function Selection_Start
     (This : Editor_Buffer) return Editor_Location'Class is abstract;
   function Selection_End
     (This : Editor_Buffer) return Editor_Location'Class is abstract;
   --  Return the bounds of the selection. The start will always be located
   --  before the end of the selection, no matter the order of parameters given
   --  to Select_Text were.

   function Get_Chars
     (This : Editor_Buffer;
      From : Editor_Location'Class := Nil_Editor_Location;
      To   : Editor_Location'Class := Nil_Editor_Location)
      return String is abstract;
   --  Returns the contents of the buffer between the two locations given in
   --  parameter. Modifying the returned value has no effect on the buffer

   procedure Insert
     (This : Editor_Buffer;
      From : Editor_Location'Class;
      Text : String) is abstract;
   --  Inserts some text in the buffer

   procedure Delete
     (This : Editor_Buffer;
      From : Editor_Location'Class := Nil_Editor_Location;
      To   : Editor_Location'Class := Nil_Editor_Location) is abstract;
   --  Delete the given range of text from the buffer

   procedure Indent
     (This : Editor_Buffer;
      From : Editor_Location'Class := Nil_Editor_Location;
      To   : Editor_Location'Class := Nil_Editor_Location) is abstract;
   --  Recompute the indentation of the given range of text. This feature is
   --  language-dependent. By default, from points to the beginning of the
   --  buffer and to to the end of the buffer.

   procedure Refill
     (This : Editor_Buffer;
      From : Editor_Location'Class := Nil_Editor_Location;
      To   : Editor_Location'Class := Nil_Editor_Location) is abstract;
   --  Refill the given range of text, ie cut long lines if necessary so that
   --  they fit in the limit specified in the GPS preferences

   procedure Copy
     (This   : Editor_Buffer;
      From   : Editor_Location'Class := Nil_Editor_Location;
      To     : Editor_Location'Class := Nil_Editor_Location;
      Append : Boolean := False) is abstract;
   procedure Cut
     (This   : Editor_Buffer;
      From   : Editor_Location'Class := Nil_Editor_Location;
      To     : Editor_Location'Class := Nil_Editor_Location;
      Append : Boolean := False) is abstract;
   procedure Paste
     (This   : Editor_Buffer;
      From   : Editor_Location'Class) is abstract;
   --  Copy/Paste management. If Append is true, the text is appended to the
   --  clipboard instead of replacing it. The range defaults to beginning and
   --  end of the buffer.

   function Beginning_Of_Buffer
     (This : Editor_Buffer) return Editor_Location'Class is abstract;
   --  Returns a location pointing to the first character in the buffer

   function End_Of_Buffer
     (This : Editor_Buffer) return Editor_Location'Class is abstract;
   --  Returns a location pointing to the last character in the buffer

   procedure Save
     (This        : Editor_Buffer;
      Interactive : Boolean := True;
      File        : Virtual_File := No_File;
      Internal    : Boolean := False) is abstract;
   --  Saves the buffer to the given file. If interactive is true, a dialog is
   --  open to ask for confirmation from the user first, which gives him a
   --  chance to cancel the saving. "interactive" is ignored if file is
   --  specified. When no file is specified, then the buffer will be saved
   --  in the same file as it's currently edited.
   --  If Internal is True, the file is saved but the editor is not changed.

   function Get_Mark
     (This : Editor_Buffer;
      Name : String) return Editor_Mark'Class is abstract;
   --  Check whether there is a mark with that name in the buffer, and return
   --  it. A Nil_Editor_Mark is returned if there is no such mark

   -------------------------
   --  Undo/Redo handling --
   -------------------------

   --  There are two APIs for handling undo/redo:
   --   - the Start_Undo_Group/Finish_Undo_Group pair: this should be called
   --     only from
   --        - the GPS Python API, to provide the high-level context manager
   --        - GNATbench
   --    - the rest of GPS should call Current_Undo_Group or New_Undo_Group

   procedure Start_Undo_Group (This : Editor_Buffer) is abstract;
   --  Starts grouping commands on the editor. All future editions will be
   --  considered as belonging to the same group. finish_undo_group should be
   --  called once for every call to start_undo_group.

   procedure Finish_Undo_Group (This : Editor_Buffer) is abstract;
   --  ancels the grouping of commands on the editor. See
   --  GPS.EditorBuffer.start_undo_group

   function Current_Undo_Group (This : Editor_Buffer) return Group_Block;
   pragma Annotate (AJIS, Bind, Current_Undo_Group, False);
   pragma Annotate (AJIS, Bind, Current_Undo_Group,
                    "Allow_Java_Creation_And_Child_Types");
   --  Enter the current undo/redo group. The group is in effect until
   --  Group_Block is finalized: see documentation in Commands.

   function New_Undo_Group (This : Editor_Buffer) return Group_Block;
   pragma Annotate (AJIS, Bind, New_Undo_Group, False);
   pragma Annotate (AJIS, Bind, New_Undo_Group,
                    "Allow_Java_Creation_And_Child_Types");
   --  Enter a new undo/redo group. The group is in effect until
   --  Group_Block is finalized: see documentation in Commands.

   procedure Undo (This : Editor_Buffer) is abstract;
   procedure Redo (This : Editor_Buffer) is abstract;
   --  Undo or redo the last command on the editor

   procedure Blocks_Fold (This : Editor_Buffer) is abstract;
   procedure Blocks_Unfold (This : Editor_Buffer) is abstract;
   --  Folds/Unfolds all the blocks in all the views of the buffer. Block
   --  folding is a language-dependent feature, whereby one can hide part of
   --  the source code temporarily, by keeping only the first line of the block
   --  (for instance the first line of a subprogram body, the rest is hidden).
   --  A small icon is displayed to the left of the first line so that it can
   --  be unfolded later on

   procedure Set_Read_Only
     (This : Editor_Buffer; Read_Only : Boolean) is abstract;
   function Is_Read_Only (This : Editor_Buffer) return Boolean is abstract;
   --  Indicates whether the user should be able to edit the buffer
   --  interactively (through any view).

   procedure Apply_Style
     (This  : Editor_Buffer;
      Style : String;
      Line  : Integer;
      From_Column, To_Column : Visible_Column_Type := -1) is abstract;
   --  Apply a specific style to part of a buffer.
   --  If From_Column and To_Column are equal, the highlighting is drawn so
   --  that the whole line including the trailing spaces appear selected.
   --  Otherwise only the actual characters will be styled.

   procedure Remove_Style
     (This  : Editor_Buffer;
      Style : String;
      Line  : Integer;
      From_Column, To_Column : Visible_Column_Type := -1) is abstract;
   --  Remove highlighting from a specific part of the text.
   --  If Line is 0, the removal is done on the whole buffer.

   function Create_Overlay
     (This : Editor_Buffer;
      Name : String := "") return Editor_Overlay'Class is abstract;
   --  Create a new overlay. Properties can be set on this overlay, which can
   --  then be applied to one or more ranges of text to changes its visual
   --  rendering or to associate user data with it. If name is specified, this
   --  function will return an existing overlay with the same name in this
   --  buffer if any can be found. If the name is not specified, a new overlay
   --  is created. Changing the properties of an existing overlay results in an
   --  immediate graphical update of the views associated with the buffer. A
   --  number of predefined overlay exit. Among these are the ones used for
   --  syntax highlighting by GPS itself, which are "keyword", "comment",
   --  "string", "character". You can use these to navigate from one comment
   --  section to the next for instance.

   procedure Apply_Overlay
     (This    : Editor_Buffer;
      Overlay : Editor_Overlay'Class;
      From    : Editor_Location'Class := Nil_Editor_Location;
      To      : Editor_Location'Class := Nil_Editor_Location) is abstract;
   --  Applies the overlay to the given range of text. This immediately changes
   --  the rendering of the text based on the properties of the overlay.
   --  Defaults to the whole buffer

   procedure Remove_Overlay
     (This    : Editor_Buffer;
      Overlay : Editor_Overlay'Class;
      From    : Editor_Location'Class := Nil_Editor_Location;
      To      : Editor_Location'Class := Nil_Editor_Location) is abstract;
   --  Removes all instances of the overlay in the given range of text. It
   --  isn't an error if the overlay is not applied to any of the character in
   --  the range, it just has no effect in that case

   procedure Add_Cursor
     (This     : Editor_Buffer;
      Location : Editor_Location'Class := Nil_Editor_Location) is abstract;
   --  Add a multi cursor at the specified location

   function Add_Cursor
     (This     : Editor_Buffer;
      Location : Editor_Location'Class := Nil_Editor_Location)
      return Editor_Cursor'Class is abstract;
   --  Add a multi cursor at the specified location

   procedure Delete_Cursor
     (This     : Editor_Buffer;
      Location : Editor_Location'Class := Nil_Editor_Location) is abstract;
   --  Delete a multi cursor at the specified location

   procedure Remove_All_Slave_Cursors (This : Editor_Buffer) is abstract;
   --  Remove all multi cursors from current buffer

   procedure Set_Cursors_Auto_Sync (Buffer : Editor_Buffer) is abstract;
   --  This sets the buffer in auto mode regarding multi cursor insertion.
   --  This means that every insert/delete will impact every active cursors
   --  in the buffer. Do not forget to set that back after a manual multi
   --  cursor operation !

   function Get_Cursors
     (This : Editor_Buffer) return Cursors_Lists.List is abstract;
   --  Get the list of all multi cursor's marks

   procedure Update_Cursors_Selection
     (This : Editor_Buffer) is abstract;
   --  Update the overlay corresponding to the multi cursors selection. *MUST*
   --  be called if you change any selection marks for multi cursors

   overriding function "="
     (This : Editor_Buffer; Buffer : Editor_Buffer) return Boolean;
   --     is abstract; --  ??? workaround, for J617-004
   --  Compare two buffers. Since an Editor_Buffer is just a wrapper and we
   --  recreate as many of them as we need even for the same widget
   --  object, we do the comparison on the widget itself.
   --  This always return False if any of the buffers is not associated with a
   --  live widget anymore.

   function Get_Language
     (Buffer : Editor_Buffer) return Language.Language_Access is (null);
   procedure Set_Language
      (Buffer : Editor_Buffer;
       Lang   : Language.Language_Access) is null;
   --  Set the language of the given buffer. The syntax highlighting is
   --  recomputed using the new language.

   procedure Set_Extend_Existing_Selection
     (This : Editor_Buffer; Extend : Boolean) is abstract;
   function Extend_Existing_Selection
     (This : Editor_Buffer) return Boolean is abstract;
   --  This mode controls the handling of selection when the cursor is
   --  moved:
   --  * for subprograms that receive an Extend_Selection parameter set to
   --    True, GPS will always create a new selection if none exists, and
   --    then extend it to include the new location of the cursor. This is
   --    the standard handling of shortcuts like shift-arrow keys
   --  * for subprograms that receive an Extend_Selection parameter set to
   --    False, the behavior depends on Always_Extend_Selection:
   --       - if False, then GPS always resets and cancels the current
   --         selection when the cursor moves. This is the standard behavior
   --         when using arrow keys for instance.
   --       - if True, GPS creates a new selection, and extend it.
   --         This is the standard Emacs mode where the mark is set
   --         independently (or vi's "v" mode).
   --         ??? This behavior requires that the emacs mode sets and unsets
   --         this flag when the mark is set or cancelled. Unfortunately, we
   --         can't simply look at whether there is a selection already,
   --         because in gtk having no selection or an empty selection is the
   --         same.

   function Has_Information_Column
      (This : Editor_Buffer; Id : String) return Boolean is abstract;
   --  Editors can display information on the side.
   --  This extra information is grouped into columns, each of which has a
   --  name. This function returns True if a column with the given Id exists

   package Buffer_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists
     (Editor_Buffer'Class);

   ---------------------------
   -- Editor_Buffer_Factory --
   ---------------------------

   function Get
     (This        : Editor_Buffer_Factory;
      File        : Virtual_File := No_File;
      Force       : Boolean := False;
      Open_Buffer : Boolean := False;
      Open_View   : Boolean := True;
      Focus       : Boolean := True) return Editor_Buffer'Class is abstract;
   --  If file is not specified, the current editor is returned, ie the last
   --  one that had the keyboard focus.
   --
   --  If no buffer exists for the file:
   --      - if Open_View is True, open a buffer and a view
   --      - if Open_View is False:
   --           - if Open_Buffer is True, open a buffer but not a view
   --           - if Open_Buffer is False, open nothing
   --
   --  When a new editor is opened and Focus = True, it receives the focus.
   --  Otherwise, it is not raised explicitly, and you need to do it
   --  yourself.
   --
   --  If force is set to true, a reload is forced in case the file is already
   --  open.

   function Get_New
     (This : Editor_Buffer_Factory) return Editor_Buffer'Class is abstract;
   --  Create a new blank editor

   function New_Mark
     (This   : Editor_Buffer_Factory;
      File   : Virtual_File := No_File;
      Line   : Integer;
      Column : Integer) return Editor_Mark'Class is abstract;
   --  Return a new mark without having to create an editor buffer

   function Buffers
     (This   : Editor_Buffer_Factory) return Buffer_Lists.List is abstract;
   --  Return the list of all buffers

   function Buffer_From_Instance
     (This       : Editor_Buffer_Factory;
      Instance   : Class_Instance) return Editor_Buffer'Class is abstract;
   --  Get buffer from class instance or raise exception if no object is
   --  stored in the instance.

   ---------------------
   -- Editor_Listener --
   ---------------------

   procedure Before_Insert_Text
     (This      : in out Editor_Listener;
      Location  : Editor_Location'Class;
      Text      : String := "";
      From_User : Boolean) is abstract;

   procedure Before_Delete_Range
     (This           : in out Editor_Listener;
      Start_Location : Editor_Location'Class;
      End_Location   : Editor_Location'Class;
      Offset         : Integer;
      From_User      : Boolean) is abstract;

   procedure After_Insert_Text
     (This            : in out Editor_Listener;
      Cursor_Location : Editor_Location'Class;
      From_User       : Boolean) is abstract;

   procedure After_Delete_Range
     (This            : in out Editor_Listener;
      Cursor_Location : Editor_Location'Class;
      From_User       : Boolean) is abstract;

   procedure After_Cursor_Moved
     (This            : in out Editor_Listener;
      Cursor_Location : Editor_Location'Class;
      From_User       : Boolean) is abstract;

   ----------------------
   -- Location markers --
   ----------------------

   type Abstract_File_Marker_Data is
      new Location_Marker_Data with null record;
   type Abstract_File_Marker is access all Abstract_File_Marker_Data'Class;
   --  Type should be abstract, but AJIS fails in that case.

   function Get_File
     (Self : not null access Abstract_File_Marker_Data)
      return GNATCOLL.VFS.Virtual_File is (GNATCOLL.VFS.No_File);
   function Get_Line
     (Self : not null access Abstract_File_Marker_Data)
      return Editable_Line_Type is (0);
   function Get_Column
     (Self : not null access Abstract_File_Marker_Data)
      return Visible_Column_Type is (0);
   --  Return the current location of the marker

   function Get_File (Self : Location_Marker) return GNATCOLL.VFS.Virtual_File
     is (if Self.Is_Null
         or else Self.Unchecked_Get.all not in Abstract_File_Marker_Data'Class
         then GNATCOLL.VFS.No_File
         else Get_File (Abstract_File_Marker (Self.Unchecked_Get)));
   function Get_Line (Self : Location_Marker) return Editable_Line_Type
     is (if Self.Is_Null
         or else Self.Unchecked_Get.all not in Abstract_File_Marker_Data'Class
         then 0
         else Get_Line (Abstract_File_Marker (Self.Unchecked_Get)));
   function Get_Column (Self : Location_Marker) return Visible_Column_Type
     is (if Self.Is_Null
         or else Self.Unchecked_Get.all not in Abstract_File_Marker_Data'Class
         then 0
         else Get_Column (Abstract_File_Marker (Self.Unchecked_Get)));

   function Create_Marker
     (This    : Editor_Buffer_Factory;
      File    : GNATCOLL.VFS.Virtual_File;
      Project : GNATCOLL.Projects.Project_Type := GNATCOLL.Projects.No_Project;
      Line    : Editable_Line_Type;
      Column  : Visible_Column_Type;
      Length  : Natural := 0) return Location_Marker is abstract;
   --  Create a mark at a specific location in a file.
   --  Whenever the file is edited, the mark will move to keep pointing to the
   --  same position (hooks are set up even if the file is not currently
   --  edited).

   package Editor_Mark_Holders is
     new Ada.Containers.Indefinite_Holders (Editor_Mark'Class);
   --  Holder to store editor mark.

private

   overriding function Go_To
     (Self  : not null access Abstract_File_Marker_Data) return Boolean
     is (False);
   overriding function To_String
     (Self : not null access Abstract_File_Marker_Data) return String is ("");
   overriding function Save
     (Self : not null access Abstract_File_Marker_Data)
     return XML_Utils.Node_Ptr is (null);
   --  Dummy functions for the sake of AJIS. Will be removed when we can make
   --  Abstract_File_Marker_Data abstract

   -------------------------
   -- Nil_Editor_Location --
   -------------------------

   type Dummy_Editor_Location is new Editor_Location with null record;

   overriding function "=" (Left, Right : Dummy_Editor_Location) return Boolean
     is (True);

   overriding function Beginning_Of_Line
     (This : Dummy_Editor_Location) return Editor_Location'Class;

   overriding function End_Of_Line
     (This : Dummy_Editor_Location) return Editor_Location'Class;

   overriding function Block_Start
     (This        : Dummy_Editor_Location;
      Update_Tree : Boolean := True) return Editor_Location'Class;
   overriding function Block_End
     (This        : Dummy_Editor_Location;
      Update_Tree : Boolean := True) return Editor_Location'Class;
   overriding function Block_Name
     (This        : Dummy_Editor_Location;
      Subprogram  : Boolean;
      Update_Tree : Boolean := True) return String;
   overriding function Block_Level
     (This : Dummy_Editor_Location) return Natural;
   overriding procedure Block_Fold (This : Dummy_Editor_Location) is null;
   overriding procedure Block_Unfold (This : Dummy_Editor_Location) is null;
   overriding function Block_Type
     (This        : Dummy_Editor_Location;
      Update_Tree : Boolean := True) return Language_Category;

   overriding function Line (This : Dummy_Editor_Location) return Integer;
   overriding function Column
     (This : Dummy_Editor_Location) return Visible_Column_Type;
   overriding function Line_Offset
     (This : Dummy_Editor_Location) return Natural is (0);
   overriding function Offset (This : Dummy_Editor_Location) return Natural;

   overriding function Buffer
     (This : Dummy_Editor_Location) return Editor_Buffer'Class;

   overriding function Create_Instance
     (This   : Dummy_Editor_Location;
      Script : access Scripting_Language_Record'Class)
      return Class_Instance;

   overriding function Create_Mark
     (This : Dummy_Editor_Location;
      Dummy_Name : String := "";
      Dummy_Left_Gravity : Boolean := True)
      return Editor_Mark'Class is (Nil_Editor_Mark);

   overriding function Forward_Char
     (This : Dummy_Editor_Location;
      Count : Integer) return Editor_Location'Class;
   overriding function Forward_Word
     (This  : Dummy_Editor_Location;
      Count : Integer) return Editor_Location'Class;
   overriding function Forward_Line
     (This  : Dummy_Editor_Location;
      Count : Integer) return Editor_Location'Class;
   overriding function Starts_Word
     (This : Dummy_Editor_Location) return Boolean;
   overriding function Ends_Word
     (This : Dummy_Editor_Location) return Boolean;
   overriding function Inside_Word
     (This : Dummy_Editor_Location) return Boolean;

   overriding function Get_Overlays
     (This    : Dummy_Editor_Location) return Overlay_Lists.List;
   overriding function Has_Overlay
     (This    : Dummy_Editor_Location;
      Overlay : Editor_Overlay'Class) return Boolean;
   overriding function Forward_Overlay
     (This    : Dummy_Editor_Location;
      Overlay : Editor_Overlay'Class) return Editor_Location'Class;
   overriding function Backward_Overlay
     (This    : Dummy_Editor_Location;
      Overlay : Editor_Overlay'Class) return Editor_Location'Class;

   overriding function Get_Char (This : Dummy_Editor_Location) return Integer;

   overriding procedure Search
     (This              : Dummy_Editor_Location;
      Pattern           : String;
      Backward          : Boolean := False;
      Case_Sensitive    : Boolean := False;
      Regexp            : Boolean := False;
      Whole_Word        : Boolean := False;
      Scope             : String := "Whole";
      Dialog_On_Failure : Boolean := True;
      Success           : out Boolean;
      Starts            : out Dummy_Editor_Location;
      Ends              : out Dummy_Editor_Location);

   Nil_Editor_Location : constant Editor_Location'Class :=
     Dummy_Editor_Location'(Controlled with null record);

   ---------------------
   -- Nil_Editor_Mark --
   ---------------------

   type Dummy_Editor_Mark is new Editor_Mark with null record;

   overriding function Line (This : Dummy_Editor_Mark) return Integer;

   overriding function Column
     (This : Dummy_Editor_Mark) return Visible_Column_Type;

   overriding function Location
     (This : Dummy_Editor_Mark;
      Open : Boolean) return Editor_Location'Class;

   overriding function Is_Present (This : Dummy_Editor_Mark) return Boolean;

   overriding procedure Delete (This : in out Dummy_Editor_Mark) is null;

   overriding procedure Move
     (This : Dummy_Editor_Mark; Location : Editor_Location'Class) is null;

   overriding function Name (This : Dummy_Editor_Mark) return String;

   overriding function Create_Instance
     (This   : Dummy_Editor_Mark;
      Script : access Scripting_Language_Record'Class)
      return Class_Instance;

   Nil_Editor_Mark : constant Editor_Mark'Class :=
     Dummy_Editor_Mark'(Controlled with null record);

   -----------------------
   -- Nil_Editor_Buffer --
   -----------------------

   type Dummy_Editor_Buffer is new Editor_Buffer with null record;

   overriding procedure Newline_And_Indent
     (This : Dummy_Editor_Buffer) is null;

   overriding function Version
     (This : Dummy_Editor_Buffer) return Integer is (0);

   overriding function Get_Main_Cursor
     (This : Dummy_Editor_Buffer) return Editor_Cursor'Class
   is (Nil_Editor_Cursor);

   overriding function Has_Slave_Cursors
     (This : Dummy_Editor_Buffer) return Boolean is (False);

   overriding procedure Close
     (This : Dummy_Editor_Buffer; Force : Boolean) is null;

   overriding function New_Location
     (This   : Dummy_Editor_Buffer;
      Line   : Integer;
      Column : Visible_Column_Type) return Editor_Location'Class;

   overriding function New_Location_Offset
     (This   : Dummy_Editor_Buffer;
      Dummy_Line   : Integer;
      Dummy_Column : Character_Offset_Type) return Editor_Location'Class
   is
      (Nil_Editor_Location);

   overriding function New_Location
     (This   : Dummy_Editor_Buffer;
      Dummy_Offset : Natural) return Editor_Location'Class
   is (Nil_Editor_Location);

   overriding function New_View
     (This : Dummy_Editor_Buffer) return Editor_View'Class;

   overriding function Open
     (This : Dummy_Editor_Buffer) return Editor_View'Class;

   overriding function Current_View
     (This : Dummy_Editor_Buffer) return Editor_View'Class;

   overriding function Lines_Count (This : Dummy_Editor_Buffer) return Integer;
   overriding function Characters_Count
     (This : Dummy_Editor_Buffer) return Natural;

   overriding function Is_Modified (This : Dummy_Editor_Buffer) return Boolean;

   overriding procedure Select_Text
     (This : Dummy_Editor_Buffer;
      From : Editor_Location'Class := Nil_Editor_Location;
      To   : Editor_Location'Class := Nil_Editor_Location) is null;
   overriding procedure Unselect (This : Dummy_Editor_Buffer) is null;
   overriding function Selection_Start
     (This : Dummy_Editor_Buffer) return Editor_Location'Class;
   overriding function Selection_End
     (This : Dummy_Editor_Buffer) return Editor_Location'Class;

   overriding function Get_Chars
     (This : Dummy_Editor_Buffer;
      From : Editor_Location'Class := Nil_Editor_Location;
      To   : Editor_Location'Class := Nil_Editor_Location) return String;

   overriding procedure Insert
     (This : Dummy_Editor_Buffer;
      From : Editor_Location'Class;
      Text : String) is null;

   overriding procedure Delete
     (This : Dummy_Editor_Buffer;
      From : Editor_Location'Class := Nil_Editor_Location;
      To   : Editor_Location'Class := Nil_Editor_Location) is null;

   overriding procedure Indent
     (This : Dummy_Editor_Buffer;
      From : Editor_Location'Class := Nil_Editor_Location;
      To   : Editor_Location'Class := Nil_Editor_Location) is null;
   overriding procedure Refill
     (This : Dummy_Editor_Buffer;
      From : Editor_Location'Class := Nil_Editor_Location;
      To   : Editor_Location'Class := Nil_Editor_Location) is null;

   overriding function Beginning_Of_Buffer
     (This : Dummy_Editor_Buffer) return Editor_Location'Class;

   overriding function End_Of_Buffer
     (This : Dummy_Editor_Buffer) return Editor_Location'Class;

   overriding procedure Save
     (This        : Dummy_Editor_Buffer;
      Interactive : Boolean := True;
      File        : Virtual_File := No_File;
      Internal    : Boolean := False) is null;

   overriding function Get_Mark
     (This : Dummy_Editor_Buffer;
      Name : String) return Editor_Mark'Class;

   overriding procedure Start_Undo_Group (This : Dummy_Editor_Buffer) is null;

   overriding procedure Finish_Undo_Group (This : Dummy_Editor_Buffer) is null;

   overriding procedure Undo (This : Dummy_Editor_Buffer) is null;
   overriding procedure Redo (This : Dummy_Editor_Buffer) is null;

   overriding procedure Set_Read_Only
     (This : Dummy_Editor_Buffer; Read_Only : Boolean) is null;
   overriding function Is_Read_Only
     (This : Dummy_Editor_Buffer) return Boolean;

   overriding procedure Apply_Style
     (This  : Dummy_Editor_Buffer;
      Style : String;
      Line  : Integer;
      From_Column, To_Column : Visible_Column_Type := -1) is null;

   overriding procedure Remove_Style
     (This  : Dummy_Editor_Buffer;
      Style : String;
      Line  : Integer;
      From_Column, To_Column : Visible_Column_Type := -1) is null;

   overriding function File (This : Dummy_Editor_Buffer) return Virtual_File;

   overriding function Has_Information_Column
     (This : Dummy_Editor_Buffer;
      Dummy_Id : String) return Boolean is (False);
   overriding procedure Copy
     (This   : Dummy_Editor_Buffer;
      From   : Editor_Location'Class := Nil_Editor_Location;
      To     : Editor_Location'Class := Nil_Editor_Location;
      Append : Boolean := False) is null;
   overriding procedure Cut
     (This   : Dummy_Editor_Buffer;
      From   : Editor_Location'Class := Nil_Editor_Location;
      To     : Editor_Location'Class := Nil_Editor_Location;
      Append : Boolean := False) is null;
   overriding procedure Paste
     (This   : Dummy_Editor_Buffer;
      From   : Editor_Location'Class) is null;
   overriding procedure Blocks_Fold (This : Dummy_Editor_Buffer) is null;
   overriding procedure Blocks_Unfold (This : Dummy_Editor_Buffer) is null;

   overriding function Create_Overlay
     (This : Dummy_Editor_Buffer;
      Name : String := "") return Editor_Overlay'Class;
   overriding procedure Apply_Overlay
     (This    : Dummy_Editor_Buffer;
      Overlay : Editor_Overlay'Class;
      From    : Editor_Location'Class := Nil_Editor_Location;
      To      : Editor_Location'Class := Nil_Editor_Location) is null;
   overriding procedure Remove_Overlay
     (This    : Dummy_Editor_Buffer;
      Overlay : Editor_Overlay'Class;
      From    : Editor_Location'Class := Nil_Editor_Location;
      To      : Editor_Location'Class := Nil_Editor_Location) is null;

   overriding procedure Add_Cursor
     (This : Dummy_Editor_Buffer;
      Location : Editor_Location'Class) is null;

   overriding function Add_Cursor
     (This : Dummy_Editor_Buffer;
      Dummy_Location : Editor_Location'Class) return Editor_Cursor'Class
   is
      (Nil_Editor_Cursor);

   overriding procedure Delete_Cursor
     (This     : Dummy_Editor_Buffer;
      Location : Editor_Location'Class) is null;

   overriding procedure Remove_All_Slave_Cursors
     (This : Dummy_Editor_Buffer) is null;

   overriding procedure Set_Cursors_Auto_Sync
     (This : Dummy_Editor_Buffer) is null;

   overriding function Get_Cursors
     (This : Dummy_Editor_Buffer) return Cursors_Lists.List
   is (Cursors_Lists.Empty_List);

   overriding procedure Update_Cursors_Selection
     (This : Dummy_Editor_Buffer) is null;

   overriding function Views
     (This : Dummy_Editor_Buffer) return View_Lists.List;

   overriding function "="
     (This : Dummy_Editor_Buffer; Buffer : Dummy_Editor_Buffer) return Boolean;

   overriding procedure Set_Extend_Existing_Selection
     (This : Dummy_Editor_Buffer; Extend : Boolean) is null;
   overriding function Extend_Existing_Selection
     (This : Dummy_Editor_Buffer) return Boolean is (False);

   Nil_Editor_Buffer : constant Editor_Buffer'Class :=
     Dummy_Editor_Buffer'(Controlled with null record);

   ---------------------
   -- Nil_Editor_View --
   ---------------------

   type Dummy_Editor_View is new Editor_View with null record;

   overriding function Get_MDI_Child
     (This : Dummy_Editor_View) return System.Address;

   overriding procedure Set_Read_Only
     (This : Dummy_Editor_View; Read_Only : Boolean) is null;
   overriding function Is_Read_Only
     (This : Dummy_Editor_View) return Boolean;

   overriding procedure Center
     (This     : Dummy_Editor_View;
      Location : Editor_Location'Class := Nil_Editor_Location) is null;

   overriding procedure Cursor_Goto
     (This       : Dummy_Editor_View;
      Location   : Editor_Location'Class;
      Raise_View : Boolean := False;
      Centering  : Centering_Type := With_Margin;
      Extend_Selection : Boolean := False) is null;

   overriding function Cursor
     (This : Dummy_Editor_View) return Editor_Location'Class;

   overriding function Title
     (This : Dummy_Editor_View; Short : Boolean) return String;

   overriding function Buffer
     (This : Dummy_Editor_View) return Editor_Buffer'Class;

   Nil_Editor_View : constant Editor_View'Class :=
     Dummy_Editor_View'(Controlled with null record);

   ------------------------
   -- Nil_Editor_Overlay --
   ------------------------

   type Dummy_Editor_Overlay is new Editor_Overlay with null record;

   overriding function Name (This : Dummy_Editor_Overlay) return String;
   overriding function Get_Property
     (This : Dummy_Editor_Overlay; Name : String) return String;
   overriding function Get_Property
     (This : Dummy_Editor_Overlay; Name : String) return Boolean;
   overriding function Get_Property
     (This : Dummy_Editor_Overlay; Dummy_Name : String) return Integer is (0);
   overriding procedure Set_Property
     (This : Dummy_Editor_Overlay; Name : String; Value : String) is null;
   overriding procedure Set_Property
     (This : Dummy_Editor_Overlay; Name : String; Value : Boolean) is null;
   overriding procedure Set_Property
     (This : Dummy_Editor_Overlay; Name : String; Value : Integer) is null;

   Nil_Editor_Overlay : constant Editor_Overlay'Class :=
     Dummy_Editor_Overlay'(Controlled with null record);

   type Dummy_Editor_Cursor is new Editor_Cursor with null record;

   overriding function Get_Insert_Mark
     (This : Dummy_Editor_Cursor) return Editor_Mark'Class
   is (Nil_Editor_Mark);

   overriding procedure Move
     (This : Dummy_Editor_Cursor; Where : Editor_Location'Class;
      Extend_Selection : Boolean) is null;

   overriding function Get_Selection_Mark
     (This : Dummy_Editor_Cursor) return Editor_Mark'Class
   is
     (Nil_Editor_Mark);

   overriding procedure Set_Manual_Sync
     (This : Dummy_Editor_Cursor) is null;

   Nil_Editor_Cursor : constant Editor_Cursor'Class
     := Dummy_Editor_Cursor'(Controlled with null record);

end GPS.Editors;
