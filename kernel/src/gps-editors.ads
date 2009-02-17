-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2008-2009, AdaCore               --
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

with Ada.Finalization; use Ada.Finalization;
with GNATCOLL.VFS;     use GNATCOLL.VFS;
with GNAT.Strings;     use GNAT.Strings;
with Language;         use Language;

package GPS.Editors is

   --  Declarations of types

   --  The following types & subprograms are used as an abstraction to the
   --  editor facilities by the GPS code. This avoid relying directly on Gtk
   --  internals, and open the door to possible alternate implementation, such
   --  as Eclipse for GNATbench.

   type Editor_Buffer_Factory is abstract new Controlled with null record;

   type Editor_Location is abstract new Controlled with null record;
   Nil_Editor_Location : constant Editor_Location'Class;

   type Editor_Mark is abstract new Controlled with null record;
   Nil_Editor_Mark : constant Editor_Mark'Class;

   type Editor_Buffer is abstract new Controlled with null record;
   Nil_Editor_Buffer : constant Editor_Buffer'Class;

   type Editor_View is abstract new Controlled with null record;
   Nil_Editor_View : constant Editor_View'Class;

   ---------------------------
   -- Editor_Buffer_Factory --
   ---------------------------

   function Get
     (This  : Editor_Buffer_Factory;
      File  : Virtual_File := No_File;
      Force : Boolean := False;
      Open  : Boolean := True) return Editor_Buffer'Class is abstract;
   --  If file is not specified, the current editor is returned, ie the last
   --  one that had the keyboard focus.
   --
   --  If the file is not currently open, the behavior depends on the open
   --  parameter: if true, a new editor is created for that file, otherwise
   --  None is returned.
   --
   --  When a new file is open, it has received the focus. But if the editor
   --  already existed, it is not raised explicitly, and you need to do it
   --  yourself.
   --
   --  If force is set to true, a reload is forced in case the file is already
   --  open.

   function New_Mark
     (This   : Editor_Buffer_Factory;
      File   : Virtual_File := No_File;
      Line   : Integer;
      Column : Integer) return Editor_Mark'Class is abstract;
   --  Return a new mark without having to create an editor buffer

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

   ------------
   -- Styles --
   ------------
   --  See description for Editor_Overlay

   type Simple_Style_Record is tagged private;
   type Simple_Style_Access is access all Simple_Style_Record'Class;
   pragma No_Strict_Aliasing (Simple_Style_Access);

   procedure Set_Foreground
     (Style : not null access Simple_Style_Record; Color : String);
   procedure Set_Background
     (Style : not null access Simple_Style_Record; Color : String);
   --  Set the foreground or background color for Style. Color must be a
   --  recognized color. (Either a simple color, or "#RRGGBB");

   function Get_Foreground
     (Style : not null access Simple_Style_Record) return String;
   function Get_Background
     (Style : not null access Simple_Style_Record) return String;
   --  Return the background color used for the style

   procedure Set_In_Speedbar
     (Style       : not null access Simple_Style_Record;
      In_Speedbar : Boolean);
   function In_Speedbar
     (Style       : not null access Simple_Style_Record) return Boolean;
   --  Set or get whether a mark should be put in the speedbar when this style
   --  is used within the line

   procedure Free (Style : in out Simple_Style_Record);
   --  Free style.

   ---------------------
   -- Editor_Location --
   ---------------------

   function Beginning_Of_Line
     (This : Editor_Location) return Editor_Location'Class is abstract;
   --  Return a location located at the beginning of the line on which This is

   function End_Of_Line
     (This : Editor_Location) return Editor_Location'Class is abstract;
   --  Return a location located at the end of the line on which self is

   function Block_Start
     (This : Editor_Location) return Editor_Location'Class is abstract;
   --  Return the location of the beginning of the current block

   function Block_End
     (This : Editor_Location) return Editor_Location'Class is abstract;
   --  Return the location of the end of the current block

   function Block_Type
     (This : Editor_Location) return Language_Category is abstract;
   --  Return the type of the block surrounding the location. This type
   --  indicates whether the block is a subprogram, an if statement,...

   function Line (This : Editor_Location) return Integer is abstract;
   --  Return the line of the location

   function Column (This : Editor_Location) return Integer is abstract;
   --  Return the column of the location

   function Buffer
     (This : Editor_Location) return Editor_Buffer'Class is abstract;
   --  Return the editor in which the location is found

   function Create_Mark
     (This : Editor_Location; Name : String := "")
      return Editor_Mark'Class is abstract;
   --  Create a mark at that location in the buffer. The mark will stay
   --  permanently at that location, and follows if the buffer is modified. If
   --  the name is specified, this creates a named mark, which can be retrieved
   --  through a call to GPS.EditorBuffer.get_mark. If a mark with the same
   --  name already exists, it is moved to the new location, and then returned

   function Forward_Char
     (This  : Editor_Location;
      Count : Integer) return Editor_Location'Class is abstract;
   --  Return a new location located count characters after self. If count is
   --  negative, the location is moved backward instead
   --  ??? Is this in byte offset or character offset?

   -----------------
   -- Editor_Mark --
   -----------------

   function Line (This : Editor_Mark) return Integer is abstract;
   --  Return the current line of the mark, without opening the buffer if not
   --  open.

   function Column (This : Editor_Mark) return Integer is abstract;
   --  Return the current column of the mark, without opening the buffer if not
   --  open.

   function Location
     (This : Editor_Mark) return Editor_Location'Class is abstract;
   --  Returns the current location of the mark. This location will vary
   --  depending on the changes that take place in the buffer.

   function Is_Present (This : Editor_Mark) return Boolean is abstract;
   --  Returns True if mark's location is still present in the buffer

   procedure Delete (This : Editor_Mark) is abstract;
   --  Deletes the physical mark from the buffer

   -------------------
   -- Editor_Buffer --
   -------------------

   function New_Location
     (This   : Editor_Buffer;
      Line   : Integer;
      Column : Integer) return Editor_Location'Class is abstract;
   --  Return a new location

   function New_View
     (This : Editor_Buffer) return Editor_View'Class is abstract;
   --  Creates a new view for the given buffer, and insert it in the MDI

   function Open (This : Editor_Buffer) return Editor_View'Class is abstract;
   --  Opens a view for the given buffer. If the view is already exists, it
   --  will get the focus, otherwise a new view will be opened.

   function Add_Special_Line
     (This       : Editor_Buffer;
      Start_Line : Integer;
      Text       : String;
      Category   : String := "";
      Name       : String := "") return Editor_Mark'Class is abstract;
   --  Adds one non-editable line to the buffer, starting at line start_line
   --  and contains string text. If category is specified, use it for
   --  highlighting. Create a mark at beginning of block and return it. If name
   --  is specified, retuned mark will have this name

   procedure Add_Special_Line
     (This       : Editor_Buffer'Class;
      Start_Line : Integer;
      Text       : String;
      Category   : String := "";
      Name       : String := "");
   --  Same as above, but doesn't return mark

   procedure Remove_Special_Lines
     (This  : Editor_Buffer;
      Mark  : Editor_Mark'Class;
      Lines : Integer) is abstract;
   --  Removes specified number of special lines at the specified mark. It
   --  doesn't delete the mark

   function Current_View
     (This : Editor_Buffer) return Editor_View'Class is abstract;
   --  Returns the last view used for this buffer, ie the last view that had
   --  the focus and through which the user might have edited the buffer's
   --  contents

   function Lines_Count (This : Editor_Buffer) return Integer is abstract;
   --  Returns the total number of lines in the buffer

   procedure Select_Text
     (This : Editor_Buffer;
      From : Editor_Location'Class := Nil_Editor_Location;
      To   : Editor_Location'Class := Nil_Editor_Location) is abstract;
   --  Selects an area in the buffer. The boundaries are included in the
   --  selection. The order of the boundaries is irrelevant, but the cursor
   --  will be left on to. By default, From is set to the beginning of the
   --  buffer, and to to the end.

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

   function Beginning_Of_Buffer
     (This : Editor_Buffer) return Editor_Location'Class is abstract;
   --  Returns a location pointing to the first character in the buffer

   function End_Of_Buffer
     (This : Editor_Buffer) return Editor_Location'Class is abstract;
   --  Returns a location pointing to the last character in the buffer

   procedure Save
     (This        : Editor_Buffer;
      Interactive : Boolean := True;
      File        : Virtual_File := No_File) is abstract;
   --  Saves the buffer to the given file. If interactive is true, a dialog is
   --  open to ask for confirmation from the user first, which gives him a
   --  chance to cancel the saving. "interactive" is ignored if file is
   --  specified. When no file is specified, then the buffer will be saved
   --  in the same file as it's currently edited.

   function Get_Mark
     (This : Editor_Buffer;
      Name : String) return Editor_Mark'Class is abstract;
   --  Check whether there is a mark with that name in the buffer, and return
   --  it. A Nil_Editor_Mark is returned if there is no such mark

   procedure Start_Undo_Group (This : Editor_Buffer) is abstract;
   --  Starts grouping commands on the editor. All future editions will be
   --  considered as belonging to the same group. finish_undo_group should be
   --  called once for every call to start_undo_group.

   procedure Finish_Undo_Group (This : Editor_Buffer) is abstract;
   --  ancels the grouping of commands on the editor. See
   --  GPS.EditorBuffer.start_undo_group

   procedure Undo (This : Editor_Buffer) is abstract;
   --  Undo the last command on the editor

   procedure Set_Read_Only
     (This : Editor_Buffer; Read_Only : Boolean) is abstract;
   --  Indicates whether the user should be able to edit the buffer
   --  interactively (through any view).

   procedure Get_Constructs
     (This       : Editor_Buffer;
      Constructs : out Language.Construct_List;
      Timestamp  : out Natural) is abstract;
   --  Return the constructs in the current state of the buffer.
   --  This should be cached in the editor; as a result the caller should not
   --  free the result.
   --  Timestamp is a Natural which is increased every time the constructs are
   --  increased. As a result, if two calls to Get_Constructs return the same
   --  Timestamp, the caller can assume that the constructs have not changed
   --  in the meantime.

   procedure Apply_Style
     (This  : Editor_Buffer;
      Style : not null access Simple_Style_Record'Class;
      Line  : Integer;
      From_Column, To_Column : Integer := -1) is abstract;
   --  Apply a specific style to part of a buffer.
   --  If From_Column and To_Column are equal, the highlighting is drawn so
   --  that the whole line including the trailing spaces appear selected.
   --  Otherwise only the actual characters will be styled.

   procedure Remove_Style
     (This  : Editor_Buffer;
      Style : not null access Simple_Style_Record'Class;
      Line  : Integer;
      From_Column, To_Column : Integer := -1) is abstract;
   --  Remove highlighting from a specific part of the text.
   --  If Line is 0, the removal is done on the whole buffer.

   -----------------
   -- Editor_View --
   -----------------

   procedure Set_Read_Only
     (This : Editor_View; Read_Only : Boolean) is abstract;
   --  Indicates whether the user should be able to edit interactively through
   --  this view. Setting a view Writable/Read Only will also modify the status
   --  of the other views of the same buffer.xx

   procedure Center
     (This     : Editor_View;
      Location : Editor_Location'Class := Nil_Editor_Location) is abstract;
   --  Scrolls the view so that the location is centered. By default, the
   --  editor is centered around the location of the cursor.

   procedure Cursor_Goto
     (This       : Editor_View;
      Location   : Editor_Location'Class;
      Raise_View : Boolean := False) is abstract;
   --  Moves the cursor at the given location. Each view of a particular buffer
   --  has its own cursor position, which is where characters typed by the user
   --  will be inserted.

   function Cursor
     (This : Editor_View) return Editor_Location'Class is abstract;
   --  Return the current location of the cursor in this view

private

   type Simple_Style_Record is tagged record
      Foreground : GNAT.Strings.String_Access;
      Background : GNAT.Strings.String_Access;
      Speedbar   : Boolean := False;
   end record;

   -------------------------
   -- Nil_Editor_Location --
   -------------------------

   type Dummy_Editor_Location is new Editor_Location with null record;

   overriding function Beginning_Of_Line
     (This : Dummy_Editor_Location) return Editor_Location'Class;

   overriding function End_Of_Line
     (This : Dummy_Editor_Location) return Editor_Location'Class;

   overriding function Block_Start
     (This : Dummy_Editor_Location) return Editor_Location'Class;

   overriding function Block_End
     (This : Dummy_Editor_Location) return Editor_Location'Class;

   overriding function Block_Type
     (This : Dummy_Editor_Location) return Language_Category;

   overriding function Line (This : Dummy_Editor_Location) return Integer;

   overriding function Column (This : Dummy_Editor_Location) return Integer;

   overriding function Buffer
     (This : Dummy_Editor_Location) return Editor_Buffer'Class;

   overriding function Create_Mark
     (This : Dummy_Editor_Location; Name : String := "")
      return Editor_Mark'Class;

   overriding function Forward_Char
     (This : Dummy_Editor_Location;
      Count : Integer) return Editor_Location'Class;

   Nil_Editor_Location : constant Editor_Location'Class :=
     Dummy_Editor_Location'(Controlled with others => <>);

   ---------------------
   -- Nil_Editor_Mark --
   ---------------------

   type Dummy_Editor_Mark is new Editor_Mark with null record;

   overriding function Line (This : Dummy_Editor_Mark) return Integer;

   overriding function Column (This : Dummy_Editor_Mark) return Integer;

   overriding function Location
     (This : Dummy_Editor_Mark) return Editor_Location'Class;

   overriding function Is_Present (This : Dummy_Editor_Mark) return Boolean;

   overriding procedure Delete (This : Dummy_Editor_Mark) is null;

   Nil_Editor_Mark : constant Editor_Mark'Class :=
     Dummy_Editor_Mark'(Controlled with others => <>);

   -----------------------
   -- Nil_Editor_Buffer --
   -----------------------

   type Dummy_Editor_Buffer is new Editor_Buffer with null record;

   overriding function New_Location
     (This   : Dummy_Editor_Buffer;
      Line   : Integer;
      Column : Integer) return Editor_Location'Class;

   overriding function New_View
     (This : Dummy_Editor_Buffer) return Editor_View'Class;

   overriding function Open
     (This : Dummy_Editor_Buffer) return Editor_View'Class;

   overriding function Add_Special_Line
     (This       : Dummy_Editor_Buffer;
      Start_Line : Integer;
      Text       : String;
      Category   : String := "";
      Name       : String := "") return Editor_Mark'Class;

   overriding procedure Remove_Special_Lines
     (This  : Dummy_Editor_Buffer;
      Mark  : Editor_Mark'Class;
      Lines : Integer) is null;

   overriding function Current_View
     (This : Dummy_Editor_Buffer) return Editor_View'Class;

   overriding function Lines_Count (This : Dummy_Editor_Buffer) return Integer;

   overriding procedure Select_Text
     (This : Dummy_Editor_Buffer;
      From : Editor_Location'Class := Nil_Editor_Location;
      To   : Editor_Location'Class := Nil_Editor_Location) is null;

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

   overriding function Beginning_Of_Buffer
     (This : Dummy_Editor_Buffer) return Editor_Location'Class;

   overriding function End_Of_Buffer
     (This : Dummy_Editor_Buffer) return Editor_Location'Class;

   overriding procedure Save
     (This        : Dummy_Editor_Buffer;
      Interactive : Boolean := True;
      File        : Virtual_File := No_File) is null;

   overriding function Get_Mark
     (This : Dummy_Editor_Buffer;
      Name : String) return Editor_Mark'Class;

   overriding procedure Start_Undo_Group (This : Dummy_Editor_Buffer) is null;

   overriding procedure Finish_Undo_Group (This : Dummy_Editor_Buffer) is null;

   overriding procedure Undo (This : Dummy_Editor_Buffer) is null;

   overriding procedure Set_Read_Only
     (This : Dummy_Editor_Buffer; Read_Only : Boolean) is null;

   overriding procedure Get_Constructs
     (This       : Dummy_Editor_Buffer;
      Constructs : out Language.Construct_List;
      Timestamp  : out Natural);

   overriding procedure Apply_Style
     (This  : Dummy_Editor_Buffer;
      Style : not null access Simple_Style_Record'Class;
      Line  : Integer;
      From_Column, To_Column : Integer := -1) is null;

   overriding procedure Remove_Style
     (This  : Dummy_Editor_Buffer;
      Style : not null access Simple_Style_Record'Class;
      Line  : Integer;
      From_Column, To_Column : Integer := -1) is null;

   Nil_Editor_Buffer : constant Editor_Buffer'Class :=
     Dummy_Editor_Buffer'(Controlled with others => <>);

   ---------------------
   -- Nil_Editor_View --
   ---------------------

   type Dummy_Editor_View is new Editor_View with null record;

   overriding procedure Set_Read_Only
     (This : Dummy_Editor_View; Read_Only : Boolean) is null;

   overriding procedure Center
     (This     : Dummy_Editor_View;
      Location : Editor_Location'Class := Nil_Editor_Location) is null;

   overriding procedure Cursor_Goto
     (This       : Dummy_Editor_View;
      Location   : Editor_Location'Class;
      Raise_View : Boolean := False) is null;

   overriding function Cursor
     (This : Dummy_Editor_View) return Editor_Location'Class;

   Nil_Editor_View : constant Editor_View'Class :=
     Dummy_Editor_View'(Controlled with others => <>);

end GPS.Editors;
