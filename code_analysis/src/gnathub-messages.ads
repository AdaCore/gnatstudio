------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2016-2018, AdaCore                   --
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

   type GNAThub_Message is
     new GPS.Kernel.Messages.Primary_Abstract_Message with private;

   type GNAThub_Message_Access is access all GNAThub_Message'Class;

   procedure Initialize
     (Self      : not null access GNAThub_Message'Class;
      Container : not null GPS.Kernel.Messages_Container_Access;
      Severity  : not null Severity_Access;
      Rule      : not null Rule_Access;
      Text      : Ada.Strings.Unbounded.Unbounded_String;
      File      : GNATCOLL.VFS.Virtual_File;
      Line      : Natural;
      Column    : Basic_Types.Visible_Column_Type;
      Entity    : Entity_Data := No_Entity_Data);
   --  Initialize instance of GNAThub's message.

   function Get_Severity (Self : GNAThub_Message) return Severity_Access;
   --  Returns severity of the message

   function Get_Tool (Self : GNAThub_Message) return Tool_Access;
   --  Returns tool that generates message

   function Get_Rule (Self : GNAThub_Message) return Rule_Access;
   --  Returns rule of the message

   function Get_Entity (Self : GNAThub_Message) return Entity_Data;
   --  Returns the entity associated to the message

   procedure Increment_Current_Counters (Self : GNAThub_Message);
   --  Increment the current counters of the severity/rule/tool associated to
   --  this message.

   procedure Decrement_Current_Counters (Self : GNAThub_Message);
   --  Decrement the current counters of the severity/rule/tool associated to
   --  this message.

   procedure Increment_Total_Counters (Self : GNAThub_Message);
   --  Increment the total counters of the severity/rule/tool associated to
   --  this message.

   overriding function Get_Background_Color
     (Self : not null access GNAThub_Message)
      return Gdk.RGBA.Gdk_RGBA;
   --  Return the message's background color

private

   type GNAThub_Message is
     new GPS.Kernel.Messages.Primary_Abstract_Message with record
      Severity   : Severity_Access;
      Rule       : Rule_Access;
      Text       : Ada.Strings.Unbounded.Unbounded_String;
      Entity     : Entity_Data;
   end record;

   overriding function Get_Text
     (Self : not null access constant GNAThub_Message)
      return Ada.Strings.Unbounded.Unbounded_String;

end GNAThub.Messages;
