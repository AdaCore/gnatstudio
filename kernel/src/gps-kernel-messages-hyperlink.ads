-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2010, AdaCore                    --
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
--  This package provides implementation of Abstract_Message which is
--  suitable for use when portion of text must be highlighted as
--  hyperlink. Message's text is escaped to protect from interpretation
--  as markup by Pango. Only secondary messages can be hyperlinked.

package GPS.Kernel.Messages.Hyperlink is

   type Hyperlink_Message is new Abstract_Message (Secondary) with private;

   type Hyperlink_Message_Access is access all Hyperlink_Message'Class;

   procedure Create_Hyperlink_Message
     (Parent : not null Message_Access;
      File   : GNATCOLL.VFS.Virtual_File;
      Line   : Natural;
      Column : Basic_Types.Visible_Column_Type;
      Text   : String;
      First  : Positive;
      Last   : Natural;
      Flags  : Message_Flags);
   --  Creates new instance of secondary Simple_Message.

   procedure Register (Container : not null access Messages_Container'Class);
   --  Registers load/save procedures for the simple message

private

   type Hyperlink_Message is new Abstract_Message (Secondary) with record
      Text  : Ada.Strings.Unbounded.Unbounded_String;
      First : Positive := 1;
      Last  : Natural  := 0;
      --  Range of the slice of the secondary location information to be
      --  highlighted.
   end record;

   overriding function Get_Text
     (Self : not null access constant Hyperlink_Message)
      return Ada.Strings.Unbounded.Unbounded_String;

   overriding function Get_Markup
     (Self : not null access constant Hyperlink_Message)
      return Ada.Strings.Unbounded.Unbounded_String;

end GPS.Kernel.Messages.Hyperlink;
