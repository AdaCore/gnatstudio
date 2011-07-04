-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2010-2011, AdaCore                  --
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

--  This package provides subprogram to parse output of the tools and
--  add messages for each recognized tool's message. Duplicate messages
--  are removed automatically. Secondary locations are parsed and
--  corresponding secondary messages are added.

package GPS.Kernel.Messages.Tools_Output is

   procedure Parse_File_Locations
     (Kernel            : access Kernel_Handle_Record'Class;
      Text              : UTF8_String;
      Category          : Glib.UTF8_String;
      Highlight         : Boolean := False;
      Styles            : GPS.Styles.UI.Builder_Message_Styles :=
        (others => null);
      Show_In_Locations : Boolean := True);
   --  Perform a basic parsing on Text, and add any found file locations
   --  to the results view in Category.
   --  If Highlighting is True, attempt to highlight the corresponding
   --  locations using Highlight_Category, Style_Category or Warning_Category
   --  as highlighting identifier.

   procedure Parse_File_Locations
     (Kernel                  : access Kernel_Handle_Record'Class;
      Text                    : String;
      Category                : String;
      Highlight               : Boolean := False;
      Styles                  : GPS.Styles.UI.Builder_Message_Styles :=
        (others => null);
      File_Location_Regexp    : String;
      File_Index_In_Regexp    : Integer;
      Line_Index_In_Regexp    : Integer;
      Col_Index_In_Regexp     : Integer;
      Msg_Index_In_Regexp     : Integer;
      Style_Index_In_Regexp   : Integer;
      Warning_Index_In_Regexp : Integer;
      Info_Index_In_Regexp    : Integer;
      Show_In_Locations       : Boolean);
   --  Perform a basic parsing on Text, and add any found file locations
   --  to the results view in Category.
   --  If Highlighting is True, attempt to highlight the corresponding
   --  locations using Highlight_Category, Style_Category or Warning_Category
   --  as highlighting identifier.
   --  File_Location_Regexp indicates how file locations should be recognized.
   --  The default blank value will matches locations reported by gcc or GNAT,
   --  ie "file:line:column message". The various index parameters indicate the
   --  relevant parenthesis pair in the regexp.
   --  If Show_In_Locations is True, show the messages in the Locations view,
   --  otherwise show messages only in the editors.
   --  Subprograms below is intended to be used only by scripting engine

   function Add_Tool_Message
     (Container          : not null access Messages_Container'Class;
      Category           : String;
      File               : GNATCOLL.VFS.Virtual_File;
      Line               : Positive;
      Column             : Basic_Types.Visible_Column_Type;
      Text               : String;
      Weight             : Natural;
      Highlight_Category : GPS.Styles.UI.Style_Access;
      Length             : Natural;
      Look_For_Secondary : Boolean;
      Show_In_Locations  : Boolean) return Message_Access;
   --  Looking for same message in the messages container and add it into the
   --  container when it is not exists. If secondary locations is detected in
   --  the message when add messages for all detected locations.
   --  If we have added messages, return the primary message inserted.

end GPS.Kernel.Messages.Tools_Output;
