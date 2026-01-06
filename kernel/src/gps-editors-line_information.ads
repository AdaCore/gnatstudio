------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2010-2026, AdaCore                     --
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

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with Commands;
with GPS.Kernel;                     use GPS.Kernel;
with GPS.Kernel.Messages.References;
with GPS.Kernel.Style_Manager;       use GPS.Kernel.Style_Manager;

package GPS.Editors.Line_Information is

   type Cursor_Movement_Controller is
     abstract new Limited_Controlled with null record;
   --  Stops cursor(s) movement while object exists.

   type GPS_Editor_Buffer is abstract new Editor_Buffer with null record;

   function Freeze_Cursor
     (This : in out GPS_Editor_Buffer)
      return Cursor_Movement_Controller'Class is abstract;
   --  Returns an object which blocks cursor movement while exists.

   ----------------
   -- Line types --
   ----------------

   type Buffer_Line_Type is new Natural;
   --  Buffer lines correspond to lines actually in the buffer, ie all lines
   --  that are visible on the screen.

   type File_Line_Type is new Natural;
   --  File lines identify lines that were in the file the last time that the
   --  buffer was saved.

   ----------------------
   -- Line information --
   ----------------------

   type Line_Information_Record is record
      Text               : Ada.Strings.Unbounded.Unbounded_String :=
        Null_Unbounded_String;
      Tooltip_Text       : Ada.Strings.Unbounded.Unbounded_String :=
        Null_Unbounded_String;
      --  A text to be displayed in a tooltip

      Image              : Ada.Strings.Unbounded.Unbounded_String :=
        Null_Unbounded_String;

      Category           : Ada.Strings.Unbounded.Unbounded_String :=
        Null_Unbounded_String;
      --  When set, the multiactions popup will gather all the clickable
      --  actions belonging to same category, with the specified label above
      --  the whole group.
      --  This has no effect for line infos that don't have associated comands
      --  or that are not clickable from the left-side of the editor.

      Message            : GPS.Kernel.Messages.References.Message_Reference;
      --  Reference to the message that will be put into context of execution
      --  of associated command.

      Associated_Command : Commands.Command_Access := null;

      Display_Popup_When_Alone : Boolean := False;
      --  Should be True if the multiactions popup that lists all the available
      --  actions should still be shown even if it's the only one action
      --  available.
   end record;
   --  Text must be a valid UTF8 string, which may contain markups in the pango
   --  markup format.

   Empty_Line_Information : constant Line_Information_Record;

   type Line_Information_Access is access all Line_Information_Record;
   procedure Free (Info : in out Line_Information_Access);
   --  Free memory associated with Info
   function To_Line_Information_Access is new Ada.Unchecked_Conversion
      (System.Address, Line_Information_Access);
   function To_Address is new Ada.Unchecked_Conversion
     (Line_Information_Access, System.Address);

   type Line_Information_Display_Type is
     (No_Display, On_Line_Number, On_Side_Area);
   --  The display type of line information data.
   --  Line information can be displayed directly on the editor's line numbers
   --  (On_Line_Number) or on the editor's left side area (On_Side_Area).

   function Get_Display_Type
     (Line_Info : Line_Information_Record)
      return Line_Information_Display_Type;
   --  Return the display type of the given line information data.

   type Line_Information_Array is array (Editable_Line_Type range <>)
     of Line_Information_Record;

   function Sort_By_Category (A, B : Line_Information_Record) return Boolean
   is
     (if A.Category = Null_Unbounded_String then False
      else A.Category < B.Category);
   --  Used to sort line information by category

   package Line_Information_Vectors is
     new Ada.Containers.Vectors (Positive, Line_Information_Record);
   package Line_Information_Vectors_Sorting is
     new Line_Information_Vectors.Generic_Sorting (Sort_By_Category);

   type Line_Information_Data is access Line_Information_Array;
   for Line_Information_Data'Size use Standard'Address_Size;
   pragma No_Strict_Aliasing (Line_Information_Data);

   procedure Free (X : in out Line_Information_Record);
   --  Free memory associated with X

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Line_Information_Array, Line_Information_Data);

   procedure Add_File_Information
     (This       : GPS_Editor_Buffer;
      Identifier : String;
      Info       : Line_Information_Data)
   is abstract;
   --  Add Info to Buffer.

   function Add_Special_Line
     (This       : GPS_Editor_Buffer;
      Start_Line : Integer;
      Text       : String;
      Style      : Style_Access := null;
      Name       : String := "";
      Column_Id  : String := "";
      Info       : Line_Information_Data := null)
      return Editor_Mark'Class is abstract;
   --  Adds one non-editable line to the buffer, starting at line start_line
   --  and contains string text. If Style is specified, use it for
   --  highlighting. Create a mark at beginning of block and return it.
   --  If Name is specified, the retuned mark will have this name.
   --  Column_Id and Info, if not empty and null, indicate the Side information
   --  to add to the buffer lines that we are inserting.

   procedure Add_Special_Line
     (This       : GPS_Editor_Buffer'Class;
      Start_Line : Integer;
      Text       : String;
      Style      : Style_Access := null;
      Name       : String := "";
      Column_Id  : String := "";
      Info       : Line_Information_Data := null);
   --  Same as above, but doesn't return mark

   procedure Remove_Special_Lines
     (This  : GPS_Editor_Buffer;
      Mark  : Editor_Mark'Class;
      Lines : Integer) is abstract;
   --  Removes specified number of special lines at the specified mark. It
   --  doesn't delete the mark

   function Flatten_Area
     (This      : GPS_Editor_Buffer;
      From_Line : Editable_Line_Type;
      To_Line   : Editable_Line_Type) return Boolean is abstract;
   --  Remove all special lines and unfold all blocks in the given range.
   --  Return True if there was actual unfolding or removal of special
   --  lines.

   function Click_On_Side_Column
     (This      : GPS_Editor_Buffer;
      Line      : Integer;
      Column    : Positive;
      Icon_Name : String := "") return Boolean is abstract;
   --  Simulate a click on the editor's side icon identified with the
   --  given Icon_Name and present at the given Line and in the given side
   --  information Column.
   --  The default side information Column (i.e: the one that displays block
   --  folding of codefix icons) starts at 1.
   --  If no icon name is given, the default action for the given column will
   --  be executed.

   procedure Click_On_Line_Number
     (This       : GPS_Editor_Buffer;
      Line       : Integer;
      Click_Type : Line_Click_Type) is abstract;
   --  Simulate a click on the line number on the side of the editor.
   --  Click_Type is used to determine if it is a simple click or a click
   --  occurring while the editor is in hyper mode (i.e: when the ctrl key
   --  is being pressed at the same time).

private

   Empty_Line_Information : constant Line_Information_Record :=
     (Text                     => Ada.Strings.Unbounded.Null_Unbounded_String,
      Tooltip_Text             => Ada.Strings.Unbounded.Null_Unbounded_String,
      Image                    => Ada.Strings.Unbounded.Null_Unbounded_String,
      Message                  => <>,
      Category                 => <>,
      Associated_Command       => null,
      Display_Popup_When_Alone => False);

end GPS.Editors.Line_Information;
