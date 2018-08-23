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

with Language;                        use Language;
with Language.Abstract_Language_Tree; use Language.Abstract_Language_Tree;
with GNATCOLL.Symbols;

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

   ----------------
   -- Get_Entity --
   ----------------

   function Get_Entity (Self : GNAThub_Message) return Entity_Data is
   begin
      return Self.Entity;
   end Get_Entity;

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

   --------------------------------
   -- Increment_Current_Counters --
   --------------------------------

   procedure Increment_Current_Counters (Self : GNAThub_Message) is
   begin
      Self.Rule.Increment_Current_Count;
      Self.Severity.Increment_Current_Count;
      Self.Rule.Tool.Increment_Current_Count;
   end Increment_Current_Counters;

   --------------------------------
   -- Decrement_Current_Counters --
   --------------------------------

   procedure Decrement_Current_Counters (Self : GNAThub_Message) is
   begin
      Self.Rule.Decrement_Current_Count;
      Self.Severity.Decrement_Current_Count;
      Self.Rule.Tool.Decrement_Current_Count;
   end Decrement_Current_Counters;

   ------------------------------
   -- Increment_Total_Counters --
   ------------------------------

   procedure Increment_Total_Counters (Self : GNAThub_Message) is
   begin
      Self.Rule.Increment_Total_Count;
      Self.Severity.Increment_Total_Count;
      Self.Rule.Tool.Increment_Total_Count;
   end Increment_Total_Counters;

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
      Column    : Basic_Types.Visible_Column_Type;
      Entity    : Entity_Data := No_Entity_Data) is
   begin
      Self.Rule     := Rule;
      Self.Severity := Severity;
      Self.Text     := Text;
      Self.Entity   := Entity;

      --  If no entity has been given, search for the closest one.

      if Self.Entity = No_Entity_Data then
         declare
            Tree        : constant Semantic_Tree'Class :=
              Container.Get_Kernel.Get_Abstract_Tree_For_File
                ("GNATHUB", File);
            Entity_Node : constant Semantic_Node'Class := Tree.Node_At
              (Sloc            => Sloc_T'(Line   => Line,
                                          Column => Column,
                                          Index  => 0),
               Category_Filter => (Cat_Package,
                                   Cat_Procedure,
                                   Cat_Function,
                                   Cat_Task,
                                   Cat_Protected,
                                   Cat_Entry,
                                   Cat_Method,
                                   Cat_Class,
                                   Cat_Constructor,
                                   Cat_Destructor));
         begin
            if Entity_Node /= No_Semantic_Node then
               Self.Entity := Entity_Data'
                 (Name   => To_Unbounded_String
                    (GNATCOLL.Symbols.Get (Entity_Node.Name).all),
                  Line   => Entity_Node.Sloc_Start.Line,
                  Column => Natural (Entity_Node.Sloc_Start.Column));
            end if;
         end;
      end if;

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
