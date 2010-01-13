-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2009-2010, AdaCore                  --
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
--  This package provides simple implementation of Abstract_Message
--  which is suitable for general purpose. Message's text is escaped
--  to protect from interpretation as markup by Pango; secondary level
--  messages can mark specified portion of text as "hyperlink".

package GPS.Kernel.Messages.Simple is

   type Simple_Message (Level : Message_Levels) is
     new Abstract_Message with private;

   type Simple_Message_Access is access all Simple_Message'Class;

   function Create_Simple_Message
     (Container     : not null Messages_Container_Access;
      Category      : String;
      File          : GNATCOLL.VFS.Virtual_File;
      Line          : Natural;
      Column        : Basic_Types.Visible_Column_Type;
      Text          : String;
      Actual_Line   : Integer;
      Actual_Column : Integer)
      return not null Simple_Message_Access;
   --  Creates new instance of primary Simple_Message.

   procedure Create_Simple_Message
     (Parent        : not null Message_Access;
      File          : GNATCOLL.VFS.Virtual_File;
      Line          : Natural;
      Column        : Basic_Types.Visible_Column_Type;
      Text          : String;
      First         : Positive;
      Last          : Natural;
      Actual_Line   : Integer;
      Actual_Column : Integer);
   --  Creates new instance of secondary Simple_Message.

   procedure Register (Container : not null access Messages_Container'Class);
   --  Registers load/save procedures for the simple message

private

   type Simple_Message (Level : Message_Levels) is
     new Abstract_Message (Level) with record
      Text : Ada.Strings.Unbounded.Unbounded_String;

      case Level is
         when Primary =>
            null;

         when Secondary =>
            First : Positive := 1;
            Last  : Natural  := 0;
            --  Range of the slice of the secondary location information to be
            --  highlighted.
      end case;
   end record;

   overriding function Get_Text
     (Self : not null access constant Simple_Message)
      return Ada.Strings.Unbounded.Unbounded_String;

   overriding function Get_Markup
     (Self : not null access constant Simple_Message)
      return Ada.Strings.Unbounded.Unbounded_String;

end GPS.Kernel.Messages.Simple;
