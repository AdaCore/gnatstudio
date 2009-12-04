-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2009, AdaCore                    --
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
--  which is suitable for general purpose.

package GPS.Kernel.Messages.Simple is

   type Simple_Message is new Abstract_Message with private;

   type Simple_Message_Access is access all Simple_Message'Class;

   function Create_Simple_Message
     (Container : not null Messages_Container_Access;
      Category  : String;
      File      : GNATCOLL.VFS.Virtual_File;
      Line      : Natural;
      Column    : Basic_Types.Visible_Column_Type;
      Text      : String)
      return not null Simple_Message_Access;
   --  Creates new instance of primary Simple_Message.

   procedure Create_Simple_Message
     (Parent : not null Message_Access;
      File   : GNATCOLL.VFS.Virtual_File;
      Line   : Natural;
      Column : Basic_Types.Visible_Column_Type;
      Text   : String;
      First  : Positive;
      Last   : Natural);
   --  Creates new instance of secondary Simple_Message.

private

   type Simple_Message is new Abstract_Message with record
      Text : Ada.Strings.Unbounded.Unbounded_String;

      First : Positive := 1;
      Last  : Natural  := 0;
      --  Range of the slice of the secondary location information to be
      --  highlighted.
      --
      --  GNAT GPL 2009/GNAT Pro 6.3.1 Note: First and Last members must be
      --  protected by the Level discriminant but it is impossible for now
      --  because of compiler's error (see IC04-014).
   end record;

   overriding function Get_Text
     (Self : not null access constant Simple_Message)
      return Ada.Strings.Unbounded.Unbounded_String;

   overriding function Get_Markup
     (Self : not null access constant Simple_Message)
      return Ada.Strings.Unbounded.Unbounded_String;

end GPS.Kernel.Messages.Simple;
