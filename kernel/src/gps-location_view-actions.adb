------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                        Copyright (C) 2012, AdaCore                       --
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
with Commands.Interactive;
with Gtkada.MDI;
with GPS.Intl; use GPS.Intl;
with GPS.Kernel.Actions;
with GPS.Kernel.MDI;

package body GPS.Location_View.Actions is

   use type Gtkada.MDI.MDI_Child;

   type Clear_Locations_Command is
     new Commands.Interactive.Interactive_Command with null record;

   overriding function Execute
     (Self    : access Clear_Locations_Command;
      Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type;
   --  Removes all messages

   type Remove_Message_Command is
     new Commands.Interactive.Interactive_Command with null record;

   overriding function Execute
     (Self    : access Remove_Message_Command;
      Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type;
   --  Removes selected message

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Clear_Locations_Command;
      Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type
   is
      pragma Unreferenced (Self);

      Child : constant Gtkada.MDI.MDI_Child :=
                Gtkada.MDI.Find_MDI_Child_By_Tag
                  (GPS.Kernel.MDI.Get_MDI
                       (GPS.Kernel.Get_Kernel (Context.Context)),
                   Location_View_Record'Tag);

   begin
      if Child /= null then
         On_Clear_Locations
           (Location_View_Record'Class (Child.Get_Widget.all)'Access);
      end if;

      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Remove_Message_Command;
      Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type
   is
      pragma Unreferenced (Self);

      Child : constant Gtkada.MDI.MDI_Child :=
                Gtkada.MDI.Find_MDI_Child_By_Tag
                  (GPS.Kernel.MDI.Get_MDI
                       (GPS.Kernel.Get_Kernel (Context.Context)),
                   Location_View_Record'Tag);

   begin
      if Child /= null then
         On_Remove_Message
           (Location_View_Record'Class (Child.Get_Widget.all)'Access);
      end if;

      return Commands.Success;
   end Execute;

   ----------------------
   -- Register_Actions --
   ----------------------

   procedure Register_Actions
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Command : Commands.Interactive.Interactive_Command_Access;

   begin
      --  Register 'Remove message' action

      Command :=
         new Remove_Message_Command'
           (Commands.Interactive.Interactive_Command with null record);
      GPS.Kernel.Actions.Register_Action
        (Kernel,
         -"Remove message",
         Command,
         -"Remove selected message",
         null,
         -"Locations view");
      GPS.Kernel.Bind_Default_Key
        (Kernel,
         -"Remove message",
         "alt-Delete");

      --  Register 'Clear locations' action

      Command :=
         new Clear_Locations_Command'
           (Commands.Interactive.Interactive_Command with null record);
      GPS.Kernel.Actions.Register_Action
        (Kernel,
         -"Clear locations",
         Command,
         -"Remove all messages",
         null,
         -"Locations view");
   end Register_Actions;

end GPS.Location_View.Actions;
