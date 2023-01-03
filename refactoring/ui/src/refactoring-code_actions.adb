------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2021-2023, AdaCore                  --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.Projects;
with GNATCOLL.Traces;    use GNATCOLL.Traces;

with GPS.Kernel.Hooks;             use GPS.Kernel.Hooks;
with GPS.Kernel.Messages;          use GPS.Kernel.Messages;
with GPS.Kernel.Messages.Markup;   use GPS.Kernel.Messages.Markup;

with GPS.Editors.Line_Information; use GPS.Editors.Line_Information;

package body Refactoring.Code_Actions is

   Me : constant Trace_Handle := Create ("Refactoring.Code_Actions");

   --  By design, there should be only one "Code Action" message, at the
   --  place of the cursor. We guarantee in this module that there is only
   --  one such message. This message always belongs to Category below.
   Category : constant String := "_internal_code_actions";

   type On_Location_Changed is new File_Location_Hooks_Function with
     null record;
   overriding procedure Execute
     (Self   : On_Location_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Line, Column : Integer;
      Project      : GNATCOLL.Projects.Project_Type);
   --  Clear code action messages in response to the location changing

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Location_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Line, Column : Integer;
      Project      : GNATCOLL.Projects.Project_Type) is
   begin
      --  The location has changed: remove all code action messages
      Invalidate_Code_Actions (Kernel);
   exception
      when E : others =>
         Trace (Me, E);
   end Execute;

   ---------------------
   -- Add_Code_Action --
   ---------------------

   procedure Add_Code_Action
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File    : Virtual_File;
      Line    : Editable_Line_Type;
      Column  : Visible_Column_Type;
      Markup  : String;
      Command : Command_Access)
   is
      Message : constant Markup_Message_Access
        := Create_Markup_Message
          (Container  => Kernel.Get_Messages_Container,
           Category   => Category,
           File       => File,
           Line       => Natural (Line),
           Column     => Column,
           Text       => Markup,
           Importance => Unspecified,
           Flags      => Sides_Only,
           Allow_Auto_Jump_To_First => False);

      Action : GPS.Editors.Line_Information.Line_Information_Access;
   begin
      --  Defensive programming, shouldn't happen
      if Message = null then
         return;
      end if;

      Action := new Line_Information_Record'
        (Text                     => To_Unbounded_String (Markup),
         Tooltip_Text             => To_Unbounded_String (Markup),
         Image                    => To_Unbounded_String ("gps-light-bulb"),
         Message                  => <>,
         Associated_Command       => Command,
         Display_Popup_When_Alone => True);

      Message.Set_Action (Action);
   end Add_Code_Action;

   -----------------------------
   -- Invalidate_Code_Actions --
   -----------------------------

   procedure Invalidate_Code_Actions
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Kernel.Get_Messages_Container.Remove_Category
        (Category, Empty_Message_Flags);
   end Invalidate_Code_Actions;

   ----------------------
   -- Register_Actions --
   ----------------------

   procedure Register_Actions
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Kernel);

   begin
      Location_Changed_Hook.Add_Debounce (new On_Location_Changed);
   end Register_Actions;

end Refactoring.Code_Actions;
