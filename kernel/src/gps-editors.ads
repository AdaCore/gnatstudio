-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2008, AdaCore                    --
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

with GNATCOLL.VFS;     use GNATCOLL.VFS;
with Ada.Finalization; use Ada.Finalization;

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

   --------------------
   -- Editor_Overlay --
   --------------------

   ---------------------
   -- Editor_Location --
   ---------------------

   function Beginning_Of_Line
     (This : Editor_Location) return Editor_Location'Class is abstract;
   --  Return a location located at the beginning of the line on which This is.

   function End_Of_Line
     (This : Editor_Location) return Editor_Location'Class is abstract;
   --  Return a location located at the end of the line on which self is.

   function Forward_Char
     (This : Editor_Location;
      Count : Integer) return Editor_Location'Class is abstract;
   --  Return a new location located count characters after self. If count is
   --  negative, the location is moved backward instead
   --  ??? Is this in byte offset or character offset?

   -----------------
   -- Editor_Mark --
   -----------------

   -------------------
   -- Editor_Buffer --
   -------------------

   function New_Location
     (This   : Editor_Buffer;
      Line   : Integer;
      Column : Integer) return Editor_Location'Class is abstract;
   --  Return a new location.

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

   procedure Remove_Special_Lines
     (This  : Editor_Buffer;
      Mark  : Editor_Mark'Class;
      Lines : Integer) is abstract;
   --  Removes specified number of special lines at the specified mark. It
   --  doesn't delete the mark

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
   --  language-dependent

   function Beginning_Of_Buffer
     (This : Editor_Buffer) return Editor_Location'Class is abstract;
   --  Returns a location pointing to the first character in the buffer

   function End_Of_Buffer
     (This : Editor_Buffer) return Editor_Location'Class is abstract;
   --  Returns a location pointing to the last character in the buffer

   function Get_Mark
     (This : Editor_Buffer;
      Name : String) return Editor_Mark'Class is abstract;
   --  Check whether there is a mark with that name in the buffer, and return
   --  it. An exception is raised if there is no such mark

private

   -------------------------
   -- Nil_Editor_Location --
   -------------------------

   type Dummy_Editor_Location is new Editor_Location with null record;

   overriding function Beginning_Of_Line
     (This : Dummy_Editor_Location) return Editor_Location'Class;

   overriding function End_Of_Line
     (This : Dummy_Editor_Location) return Editor_Location'Class;

   overriding function Forward_Char
     (This : Dummy_Editor_Location;
      Count : Integer) return Editor_Location'Class;

   Nil_Editor_Location : constant Editor_Location'Class :=
     Dummy_Editor_Location'(Controlled with others => <>);

   ---------------------
   -- Nil_Editor_Mark --
   ---------------------

   type Dummy_Editor_Mark is new Editor_Mark with null record;

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

   overriding function Get_Mark
     (This : Dummy_Editor_Buffer;
      Name : String) return Editor_Mark'Class;

   Nil_Editor_Buffer : constant Editor_Buffer'Class :=
     Dummy_Editor_Buffer'(Controlled with others => <>);

end GPS.Editors;
