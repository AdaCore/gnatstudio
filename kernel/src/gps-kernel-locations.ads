-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2009, AdaCore                   --
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

--  An old style interface to the locations view.

with GPS.Kernel.Styles;

package GPS.Kernel.Locations is

   procedure Insert_Location
     (Kernel             : access Kernel_Handle_Record'Class;
      Category           : Glib.UTF8_String;
      File               : GNATCOLL.VFS.Virtual_File;
      Text               : Glib.UTF8_String;
      Line               : Positive;
      Column             : Basic_Types.Visible_Column_Type;
      Length             : Natural := 0;
      Highlight          : Boolean := False;
      Highlight_Category : GPS.Kernel.Styles.Style_Access := null;
      Quiet              : Boolean := False;
      Remove_Duplicates  : Boolean := True;
      Has_Markups        : Boolean := False;
      Sort_In_File       : Boolean := False;
      Look_For_Secondary : Boolean := False);
   --  Insert a new location in the result view.
   --  This is similar to Insert, except it creates the result view if
   --  necessary.
   --  If Quiet is True, the locations window will not be raised, and the
   --  cursor will not jump to the first location.
   --  If Remove_Duplicates is True, remove the duplicates while inserting
   --  the items.
   --  If Has_Markups is True, then Text should be in Pango Markup language;
   --  in this case, the markups will be interpreted and displayed in the
   --  Locations view.
   --  If Sort_In_File is True, then the new entry will be inserted before the
   --  first entry with a higher line number. This should be avoided if you
   --  know that you are already inserting entries sorted, since it is slower.
   --  If Look_For_Secondary is True, Text will be scanned for additional
   --  references of the form file:line[:column].

   procedure Remove_Location_Category
     (Kernel   : access Kernel_Handle_Record'Class;
      Category : String;
      File     : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Line     : Natural := 0);
   --  Remove Category from the results view, if it exists.
   --  If File /= No_File, remove only the node corresponding to File in
   --  the category.
   --  If also Line /= 0, removes only the corresponding line in the
   --  corresponding File.

   function Category_Count
     (Kernel   : access Kernel_Handle_Record'Class;
      Category : String) return Natural;
   --  Return the number of entries for a given category

   procedure Next_Item
     (Kernel    : access Kernel_Handle_Record'Class;
      Backwards : Boolean := False);
   --  If an item is selected, jump to the location pointed to by the iter
   --  immediately following it in the same category. If there is none, jump
   --  to the first item in the category.

   procedure Parse_File_Locations
     (Kernel                  : access Kernel_Handle_Record'Class;
      Text                    : String;
      Category                : Glib.UTF8_String;
      Highlight               : Boolean := False;
      Highlight_Category      : GPS.Kernel.Styles.Style_Access := null;
      Style_Category          : GPS.Kernel.Styles.Style_Access := null;
      Warning_Category        : GPS.Kernel.Styles.Style_Access := null;
      File_Location_Regexp    : String := "";
      File_Index_In_Regexp    : Integer := -1;
      Line_Index_In_Regexp    : Integer := -1;
      Col_Index_In_Regexp     : Integer := -1;
      Msg_Index_In_Regexp     : Integer := -1;
      Style_Index_In_Regexp   : Integer := -1;
      Warning_Index_In_Regexp : Integer := -1;
      Quiet                   : Boolean := False;
      Remove_Duplicates       : Boolean := False);
   --  Perform a basic parsing on Text, and add any found file locations
   --  to the results view in Category.
   --  If Highlighting is True, attempt to highlight the corresponding
   --  locations using Highlight_Category, Style_Category or Warning_Category
   --  as highlighting identifier.
   --  File_Location_Regexp indicates how file locations should be recognized.
   --  The default blank value will matches locations reported by gcc or GNAT,
   --  ie "file:line:column message". The various index parameters indicate the
   --  relevant parenthesis pair in the regexp.
   --  Remove_Duplicates indicates whether duplicated entries should be
   --  filtered out.

   procedure Register (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register hooks.

   procedure Highlight_Line
     (Kernel             : access GPS.Kernel.Kernel_Handle_Record'Class;
      Filename           : GNATCOLL.VFS.Virtual_File;
      Line               : Natural;
      Column             : Basic_Types.Visible_Column_Type;
      Length             : Natural;
      Highlight_Category : GPS.Kernel.Styles.Style_Access;
      Highlight          : Boolean := True);
   --  Highlight the line with the corresponding category.
   --  If Highlight is set to False, remove the highlighting.
   --  If Line = 0, highlight / unhighlight all lines in file.
   --  If Length = 0, highlight the whole line, otherwise use highlight_range.

end GPS.Kernel.Locations;
