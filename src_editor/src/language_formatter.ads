------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2025, AdaCore                          --
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

with GPS.Kernel;  use GPS.Kernel;
with GPS.Editors; use GPS.Editors;

package Language_Formatter is

   type Language_Formatting_Provider is
     new GPS.Editors.Editor_Formatting_Provider
   with record
      Kernel : Kernel_Handle;
   end record;

private

   overriding
   function On_Range_Formatting
     (Self        : in out Language_Formatting_Provider;
      From, To    : Editor_Location'Class;
      Cursor_Line : Natural;
      Cursor_Move : in out Integer) return Boolean;
   --  Called for formatting code section. Return False when provider
   --  can't be used on this editor.
   --  Cursor_Line is the location of the cursor when rangeFormatting was asked
   --  Cursor_Move indicates the number of characters that the cursor should
   --  move (Negative numbers are allowed to move backward).

   overriding
   function On_Type_Formatting
     (Self        : in out Language_Formatting_Provider;
      From, To    : Editor_Location'Class;
      Cursor_Line : Natural) return Boolean;

end Language_Formatter;
