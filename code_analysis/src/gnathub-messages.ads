------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2016, AdaCore                        --
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
--  Provides subclass of message to be used to represent GNAThub's messages

with Ada.Strings.Unbounded;

with Gdk.RGBA;
with GNATCOLL.VFS;

with Basic_Types;
with GPS.Kernel.Messages;

package GNAThub.Messages is

   type Message is
     new GPS.Kernel.Messages.Primary_Abstract_Message with private;

   type Message_Access is access all Message'Class;

   procedure Initialize
     (Self      : not null access Message'Class;
      Container : not null GPS.Kernel.Messages_Container_Access;
      Severity  : Severity_Access;
      Rule      : Rule_Access;
      Text      : Ada.Strings.Unbounded.Unbounded_String;
      File      : GNATCOLL.VFS.Virtual_File;
      Line      : Natural;
      Column    : Basic_Types.Visible_Column_Type);
   --  Initialize instance of GNAThub's message.

   function Get_Severity (Self : Message) return Severity_Access;
   --  Returns severity of the message

   function Get_Tool (Self : Message) return Tool_Access;
   --  Returns tool that generates message

   function Get_Rule (Self : Message) return Rule_Access;
   --  Returns rule of the message

   overriding function Get_Background_Color
     (Self : not null access Message)
      return Gdk.RGBA.Gdk_RGBA;

   Category : constant String := "GNAThub Analysis Data";
   --  Category for GNAThub messages

private

   type Message is
     new GPS.Kernel.Messages.Primary_Abstract_Message with record
      Severity : Severity_Access;
      Rule     : Rule_Access;
      Text     : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   overriding function Get_Text
     (Self : not null access constant Message)
      return Ada.Strings.Unbounded.Unbounded_String;

end GNAThub.Messages;
