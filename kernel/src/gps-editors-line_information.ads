------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2018, AdaCore                     --
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

with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with Commands;
with GPS.Kernel.Messages.References;
with GPS.Kernel.Style_Manager;       use GPS.Kernel.Style_Manager;

package GPS.Editors.Line_Information is

   type GPS_Editor_Buffer is abstract new Editor_Buffer with null record;

   ----------------------
   -- Line information --
   ----------------------

   type Line_Information_Record is record
      Text               : Ada.Strings.Unbounded.Unbounded_String;
      Tooltip_Text       : Ada.Strings.Unbounded.Unbounded_String;
      --  A text to be displayed in a tooltip
      Image              : Ada.Strings.Unbounded.Unbounded_String;
      Message            : GPS.Kernel.Messages.References.Message_Reference;
      --  Reference to the message that will be put into context of execution
      --  of associated command.
      Associated_Command : Commands.Command_Access := null;
   end record;
   --  Text must be a valid UTF8 string, which may contain markups in the pango
   --  markup format.

   Empty_Line_Information : constant Line_Information_Record;

   type Line_Information_Display_Type is
     (No_Display, On_Line_Number, On_Side_Area);
   --  The display type of line information data.
   --  Line information can be displayed directly on the editor's line numbers
   --  (On_Line_Number) or on the editor's left side area (On_Side_Area).

   function Get_Display_Type
     (Line_Info : Line_Information_Record)
      return Line_Information_Display_Type;
   --  Return the display type of the given line information data.

   type Line_Information_Array is array (Integer range <>)
     of Line_Information_Record;

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
   --  and contains string text. If Styleis specified, use it for
   --  highlighting. Create a mark at beginning of block and return it. If name
   --  is specified, retuned mark will have this name
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

private

   Empty_Line_Information : constant Line_Information_Record :=
     (Text               => Ada.Strings.Unbounded.Null_Unbounded_String,
      Tooltip_Text       => Ada.Strings.Unbounded.Null_Unbounded_String,
      Image              => Ada.Strings.Unbounded.Null_Unbounded_String,
      Message            => <>,
      Associated_Command => null);

end GPS.Editors.Line_Information;
