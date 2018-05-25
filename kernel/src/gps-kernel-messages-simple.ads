------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2009-2018, AdaCore                     --
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
--  This package provides simple implementation of Abstract_Message
--  which is suitable for general purpose. Message's text is escaped
--  to protect from interpretation as markup by Pango.

package GPS.Kernel.Messages.Simple is

   type Simple_Message (Level : Message_Levels) is
     new Abstract_Message with private;

   type Simple_Message_Access is access all Simple_Message'Class;

   function Create_Simple_Message
     (Container  : not null Messages_Container_Access;
      Category   : String;
      File       : GNATCOLL.VFS.Virtual_File;
      Line       : Natural;
      Column     : Basic_Types.Visible_Column_Type;
      Text       : String;
      Importance : Message_Importance_Type;
      Flags      : Message_Flags;
      Allow_Auto_Jump_To_First : Boolean := True)
      return not null Simple_Message_Access;
   --  Creates new instance of primary Simple_Message.

   procedure Create_Simple_Message
     (Container  : not null Messages_Container_Access;
      Category   : String;
      File       : GNATCOLL.VFS.Virtual_File;
      Line       : Natural;
      Column     : Basic_Types.Visible_Column_Type;
      Text       : String;
      Importance : Message_Importance_Type;
      Flags      : Message_Flags;
      Allow_Auto_Jump_To_First : Boolean := True);
   --  Creates new instance of primary Simple_Message.

   function Create_Simple_Message
     (Parent : not null Message_Access;
      File   : GNATCOLL.VFS.Virtual_File;
      Line   : Natural;
      Column : Basic_Types.Visible_Column_Type;
      Text   : String;
      Flags  : Message_Flags) return Simple_Message_Access;
   --  Creates new instance of secondary Simple_Message.

   procedure Create_Simple_Message
     (Parent : not null Message_Access;
      File   : GNATCOLL.VFS.Virtual_File;
      Line   : Natural;
      Column : Basic_Types.Visible_Column_Type;
      Text   : String;
      Flags  : Message_Flags);
   --  Creates new instance of secondary Simple_Message.

   procedure Register (Container : not null access Messages_Container'Class);
   --  Registers load/save procedures for the simple message

private

   type Simple_Message (Level : Message_Levels) is
     new Abstract_Message (Level) with record
      Text : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   overriding function Get_Text
     (Self : not null access constant Simple_Message)
      return Ada.Strings.Unbounded.Unbounded_String;

   overriding function Get_Markup
     (Self : not null access constant Simple_Message)
      return Ada.Strings.Unbounded.Unbounded_String;

end GPS.Kernel.Messages.Simple;
