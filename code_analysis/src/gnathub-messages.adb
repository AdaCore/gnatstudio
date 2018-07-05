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
     (Self : not null access GNAThub_Message)
      return Gdk.RGBA.Gdk_RGBA is
   begin
      return Background (Self.Severity.Style);
   end Get_Background_Color;

   --------------
   -- Get_Rule --
   --------------

   function Get_Rule (Self : GNAThub_Message) return Rule_Access is
   begin
      return Self.Rule;
   end Get_Rule;

   ------------------
   -- Get_Severity --
   ------------------

   function Get_Severity (Self : GNAThub_Message) return Severity_Access is
   begin
      return Self.Severity;
   end Get_Severity;

   --------------
   -- Get_Text --
   --------------

   overriding function Get_Text
     (Self : not null access constant GNAThub_Message)
      return Ada.Strings.Unbounded.Unbounded_String is
   begin
      return Self.Text;
   end Get_Text;

   --------------
   -- Get_Tool --
   --------------

   function Get_Tool (Self : GNAThub_Message) return Tool_Access is
   begin
      return Self.Rule.Tool;
   end Get_Tool;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self      : not null access GNAThub_Message'Class;
      Container : not null GPS.Kernel.Messages_Container_Access;
      Severity  : not null Severity_Access;
      Rule      : not null Rule_Access;
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
         Category      => To_String (Self.Rule.Tool.Name),
         File          => File,
         Line          => Line,
         Column        => Column,
         Importance    => Severity.Ranking,
         Actual_Line   => Line,
         Actual_Column => Integer (Column));
   end Initialize;

end GNAThub.Messages;
