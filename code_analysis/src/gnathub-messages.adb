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

package body GNAThub.Messages is

   --------------------------
   -- Get_Background_Color --
   --------------------------

   overriding function Get_Background_Color
     (Self : not null access Message)
      return Gdk.RGBA.Gdk_RGBA is
   begin
      return Self.Severity.Color.Get_Pref;
   end Get_Background_Color;

   --------------
   -- Get_Rule --
   --------------

   function Get_Rule (Self : Message) return Rule_Access is
   begin
      return Self.Rule;
   end Get_Rule;

   ------------------
   -- Get_Severity --
   ------------------

   function Get_Severity (Self : Message) return Severity_Access is
   begin
      return Self.Severity;
   end Get_Severity;

   --------------
   -- Get_Text --
   --------------

   overriding function Get_Text
     (Self : not null access constant Message)
      return Ada.Strings.Unbounded.Unbounded_String
   is
      use Ada.Strings.Unbounded;
   begin
      return Self.Rule.Tool.Name & ": " & Self.Text;
   end Get_Text;

   --------------
   -- Get_Tool --
   --------------

   function Get_Tool (Self : Message) return Tool_Access is
   begin
      return Self.Rule.Tool;
   end Get_Tool;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self      : not null access Message'Class;
      Container : not null GPS.Kernel.Messages_Container_Access;
      Severity  : Severity_Access;
      Rule      : Rule_Access;
      Text      : Ada.Strings.Unbounded.Unbounded_String;
      File      : GNATCOLL.VFS.Virtual_File;
      Line      : Natural;
      Column    : Basic_Types.Visible_Column_Type) is
   begin
      Self.Rule     := Rule;
      Self.Severity := Severity;
      Self.Text     := Text;

      GPS.Kernel.Messages.Initialize
        (Self          => Self,
         Container     => Container,
         Category      => Category,
         File          => File,
         Line          => Line,
         Column        => Column,
         Weight        => 0,  --  ??? It is possible to set different weight
                              --  for different severities of the message.
                              --  It allows to group message in Locations view
         Actual_Line   => Line,
         Actual_Column => Integer (Column));
   end Initialize;

end GNAThub.Messages;
