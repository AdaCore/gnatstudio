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
     (Kernel : access Kernel_Handle_Record'Class;
      File   : String);
   --  Open a given file.
   --  Depending on the preferences, a new editor will be opened each time
   --  this function is called, or the same editor will be used, or a
   --  new editor will be opened only for different files.

   procedure Go_To
     (Kernel    : access Kernel_Handle_Record'Class;
      File      : String;
      Line      : Natural := 0;
      Column    : Natural := 0;
      Highlight : Boolean := True);
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

   function Focus_Is_Editor
     (Kernel : access Kernel_Handle_Record'Class) return Boolean;
   --  Return True if the focus window in Glide is an editor.

   function Get_Focus_Title
     (Kernel : access Kernel_Handle_Record'Class) return String;
   --  Return the Title of the focus window.

end Glide_Kernel.Editor;
