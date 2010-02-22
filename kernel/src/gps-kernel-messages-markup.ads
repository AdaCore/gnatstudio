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
--  suitable for use when message text includes Pango markup. Support
--  for secondary messages are not provided.

package GPS.Kernel.Messages.Markup is

   type Markup_Message is new Abstract_Message (Primary) with private;

   type Markup_Message_Access is access all Markup_Message'Class;

   function Create_Markup_Message
     (Container : not null Messages_Container_Access;
      Category  : String;
      File      : GNATCOLL.VFS.Virtual_File;
      Line      : Natural;
      Column    : Basic_Types.Visible_Column_Type;
      Text      : String;
      Weight    : Natural)
      return not null Markup_Message_Access;
   --  Creates new instance of Markup_Message.

   procedure Register (Container : not null access Messages_Container'Class);
   --  Registers load/save procedures for the markup message

private

   type Markup_Message is new Abstract_Message (Primary) with record
      Text : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   overriding function Get_Text
     (Self : not null access constant Markup_Message)
      return Ada.Strings.Unbounded.Unbounded_String;

   overriding function Get_Markup
     (Self : not null access constant Markup_Message)
      return Ada.Strings.Unbounded.Unbounded_String;

end GPS.Kernel.Messages.Markup;
