----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
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

with GVD_Module;           use GVD_Module;
with Glide_Kernel.Modules; use Glide_Kernel.Modules;
with Debugger_Pixmaps;     use Debugger_Pixmaps;
with GVD.Types;            use GVD.Types;

package body Commands.Debugger is

   ------------
   -- Create --
   ------------

   procedure Create
     (Item           : out Set_Breakpoint_Command_Access;
      Kernel         : Kernel_Handle;
      Debugger       : Debugger_Access;
      Mode           : Breakpoint_Command_Mode;
      File           : String;
      Line           : Positive) is
   begin
      Item := new Set_Breakpoint_Command;
      Item.Kernel := Kernel;
      Item.BMode := Mode;
      Item.File := new String' (File);
      Item.Line := Line;
      Item.Debugger := Debugger;
   end Create;

   -------------
   -- Execute --
   -------------

   function Execute (Command : access Set_Breakpoint_Command) return Boolean
   is
      Other_Command : Set_Breakpoint_Command_Access;
      L             : constant Integer := Command.Line;
      A             : Line_Information_Array (L .. L);

   begin
      case Command.BMode is
         when Set =>
            Command.BP := Break_Source
              (Command.Debugger,
               Command.File.all,
               Command.Line,
               Mode => Visible);
            Create
              (Other_Command,
               Command.Kernel,
               Command.Debugger,
               Unset,
               Command.File.all,
               Command.Line);
            Other_Command.BP := Command.BP;

            A (L).Image := Line_Has_Breakpoint_Pixbuf;
            A (L).Associated_Command := Command_Access (Other_Command);
            Add_Line_Information
              (Command.Kernel,
               Command.File.all,
               GVD_Module_Name & "/Line Information",
               new Line_Information_Array' (A));

         when Unset =>
            Remove_Breakpoint
              (Command.Debugger,
               Command.BP,
               Mode => Visible);
            Create
              (Other_Command,
               Command.Kernel,
               Command.Debugger,
               Set,
               Command.File.all,
               Command.Line);

            A (L).Image := Line_Has_Code_Pixbuf;
            A (L).Associated_Command := Command_Access (Other_Command);
            Add_Line_Information
              (Command.Kernel,
               Command.File.all,
               GVD_Module_Name & "/Line Information",
               new Line_Information_Array' (A));
      end case;

      Command_Finished (Command, True);
      return True;
   end Execute;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Command : access Set_Breakpoint_Command) is
   begin
      Free (Command.File);
   end Destroy;

end Commands.Debugger;
