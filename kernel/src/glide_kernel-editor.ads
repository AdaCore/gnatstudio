-----------------------------------------------------------------------
--                                                                   --
--                     Copyright (C) 2001                            --
--                          ACT-Europe                               --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Src_Info.Queries;

package Glide_Kernel.Editor is

   procedure New_Editor
     (Kernel : access Kernel_Handle_Record'Class);
   --  Create a new editor and add it in the MDI.

   procedure New_View
     (Kernel : access Kernel_Handle_Record'Class);
   --  Create a new view for the current editor and add it in the MDI.
   --  The current editor is the focus child in the MDI. If the focus child
   --  is not an editor, nothing happens.

   procedure Open_File
     (Kernel  : access Kernel_Handle_Record'Class;
      File    : String;
      Success : out Boolean);
   --  Open a given file. Print an error message if we failed to open or read
   --  the file.
   --  Depending on the preferences, a new editor will be opened each time
   --  this function is called, or the same editor will be used, or a
   --  new editor will be opened only for different files.

   procedure Open_Or_Create
     (Kernel : access Kernel_Handle_Record'Class;
      File   : String);
   --  Open a given file, or open an empty editor if the file does not exist.
   --  Depending on the preferences, a new editor will be opened each time
   --  this function is called, or the same editor will be used, or a
   --  new editor will be opened only for different files.

   procedure Go_To
     (Kernel    : access Kernel_Handle_Record'Class;
      File      : String;
      Line      : Natural := 0;
      Column    : Natural := 0;
      Highlight : Boolean := True;
      Success   : out Boolean);
   --  Go to the specified file at Line:Column
   --  Depending on the preferences, this may or may not open a new editor.
   --  If Highlight is True, Line will be highlighted.

   procedure Save_To_File
     (Kernel  : access Kernel_Handle_Record'Class;
      Name    : String := "";
      Success : out Boolean);
   --  Save the current editor to Name, or its associated filename if Name is
   --  null.

   procedure Cut_Clipboard (Kernel : access Kernel_Handle_Record'Class);
   --  Copy the currently-selected text to the clipboard and then delete it.

   procedure Copy_Clipboard (Kernel : access Kernel_Handle_Record'Class);
   --  Copy the currently-selected text to the clipboard.

   procedure Paste_Clipboard (Kernel : access Kernel_Handle_Record'Class);
   --  Paste the contents of the clipboard.

   procedure Select_All (Kernel : access Kernel_Handle_Record'Class);
   --  Set the selection bounds from the begining to the end of the buffer.

   procedure Set_Editor_Child (Kernel : access Kernel_Handle_Record'Class);
   --  Set the editor child to the focus widget.

   function Get_Editor_Child
     (Kernel : access Kernel_Handle_Record'Class) return Gtkada.MDI.MDI_Child;
   --  Return the last editor child that had the focus, null if none.

   function Get_Editor_Filename
     (Kernel : access Kernel_Handle_Record'Class) return String;
   --  Return the filename of the last editor window that had the focus,
   --  "" if none.

   ------------------
   -- Xref support --
   ------------------

   procedure Goto_Declaration_Or_Body
     (Kernel : access Kernel_Handle_Record'Class);
   --  Find the location of the declaration for the entity below the
   --  insert cursor (see Src_Info.Queries.Find_Declaration_Or_Body
   --  for more details on the behavior of this query) and highlight
   --  the entity found, opening a new editor if needed (this may depend
   --  on the user preferences).

   procedure Find_Dependencies
     (Kernel       : access Kernel_Handle_Record'Class;
      Source_Name  : String;
      Dependencies : out Src_Info.Queries.Dependency_List;
      Status       : out Src_Info.Queries.Dependencies_Query_Status);
   --  Return the list of units on which the unit associated to the given
   --  filename depends. An Internal_Error status is returned if the
   --  LI information of the given file can not be found or if the associated
   --  LI file can not be found.

   --  ??? Shouldn't this be moved to another package, like
   --  ??? Glide_Kernel.Browsers

end Glide_Kernel.Editor;
