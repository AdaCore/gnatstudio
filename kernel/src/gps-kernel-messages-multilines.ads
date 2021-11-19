------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2021, AdaCore                          --
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
--  This package provides simple implementation of Abstract_Message which
--  can be displayed accross multiple lines.

with GPS.Kernel.Messages.Simple; use GPS.Kernel.Messages.Simple;

package GPS.Kernel.Messages.Multilines is

   type Multiline_Message (Level : Message_Levels) is
     new Simple_Message with private;

   type Multiline_Message_Access is access all Multiline_Message'Class;

   function Create_Message
     (Container                : not null Messages_Container_Access;
      Category                 : String;
      File                     : GNATCOLL.VFS.Virtual_File;
      Line                     : Natural;
      Column                   : Basic_Types.Visible_Column_Type;
      End_Line                 : Natural;
      End_Column               : Basic_Types.Visible_Column_Type;
      Text                     : String;
      Highlight_Category       : GPS.Kernel.Style_Manager.Style_Access;
      Importance               : Message_Importance_Type;
      Show_In_Locations        : Boolean;
      Allow_Auto_Jump_To_First : Boolean := True)
      return not null Multiline_Message_Access;
   --  Creates new instance of Multilines_Message.

   procedure Register (Container : not null access Messages_Container'Class);
   --  Registers load/save procedures for the multilines message

private

   type Multiline_Message (Level : Message_Levels) is
     new Simple_Message (Level) with record
      End_Line   : Natural;
      End_Column : Basic_Types.Visible_Column_Type;
   end record;

   overriding function Has_Multiline_Highlighting
     (Self : not null access constant Multiline_Message)
      return Boolean;

   overriding procedure Get_Multiline_Highlighting_Range
     (Self         : not null access constant Multiline_Message;
      Start_Line   : out Natural;
      Start_Column : out Basic_Types.Visible_Column_Type;
      End_Line     : out Natural;
      End_Column   : out Basic_Types.Visible_Column_Type);

end GPS.Kernel.Messages.Multilines;
