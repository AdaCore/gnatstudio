-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
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

with Glide_Kernel.Modules; use Glide_Kernel.Modules;
with Glide_Kernel.Scripts; use Glide_Kernel.Scripts;
with VFS;                  use VFS;

package body Commands.Locations is

   ------------------
   -- Set_Location --
   ------------------

   procedure Set_Location
     (Item       : access Source_Location_Command_Type;
      New_Line   : Natural;
      New_Column : Natural) is
   begin
      Item.Line := New_Line;
      Item.Column := New_Column;
   end Set_Location;

   --------------
   -- Get_File --
   --------------

   function Get_File
     (Item : access Source_Location_Command_Type) return VFS.Virtual_File is
   begin
      return Item.Filename;
   end Get_File;

   --------------
   -- Get_Line --
   --------------

   function Get_Line
     (Item : access Source_Location_Command_Type) return Natural is
   begin
      return Item.Line;
   end Get_Line;

   ----------------
   -- Get_Column --
   ----------------

   function Get_Column
     (Item : access Source_Location_Command_Type) return Natural is
   begin
      return Item.Column;
   end Get_Column;

   ------------
   -- Create --
   ------------

   procedure Create
     (Item    : out Generic_Location_Command;
      Kernel  : Kernel_Handle;
      Args    : GNAT.OS_Lib.Argument_List)
   is
      A : String_List (Args'Range);
   begin
      Item := new Generic_Location_Command_Type;
      Item.Kernel := Kernel;

      if Item.Args /= null then
         Free (Item.Args);
      end if;

      for J in Args'Range loop
         A (J) := new String'(Args (J).all);
      end loop;

      Item.Args := new String_List'(A);
   end Create;

   procedure Create
     (Item     : out Html_Location_Command;
      Kernel   : Kernel_Handle;
      Filename : VFS.Virtual_File) is
   begin
      Item := new Html_Location_Command_Type;
      Item.Kernel := Kernel;
      Item.Filename := Filename;
   end Create;

   procedure Create
     (Item           : out Source_Location_Command;
      Kernel         : Kernel_Handle;
      Filename       : VFS.Virtual_File;
      Line           : Natural := 0;
      Column         : Natural := 0;
      Column_End     : Natural := 0) is
   begin
      Item := new Source_Location_Command_Type;
      Item.Kernel := Kernel;
      Item.Filename := Filename;
      Item.Line := Line;
      Item.Column := Column;
      Item.Column_End := Column_End;
   end Create;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Generic_Location_Command_Type) is
   begin
      Free (X.Args);
   end Free;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Generic_Location_Command_Type)
      return Command_Return_Type is
   begin
      if Command.Args /= null then
         if Command.Args'Length > 1 then
            Execute_GPS_Shell_Command
              (Command.Kernel,
               Command.Args (Command.Args'First).all,
               Command.Args (Command.Args'First + 1 .. Command.Args'Last));
         else
            Execute_GPS_Shell_Command
              (Command.Kernel, Command.Args (Command.Args'First).all);
         end if;

         return Success;
      else
         return Failure;
      end if;
   end Execute;

   function Execute
     (Command : access Html_Location_Command_Type) return Command_Return_Type
   is
   begin
      if Command.Filename /= VFS.No_File then
         Open_Html (Command.Kernel, Command.Filename, False);
      end if;

      Command_Finished (Command, True);

      return Success;
   end Execute;

   function Execute
     (Command : access Source_Location_Command_Type) return Command_Return_Type
   is
      Other_Command    : Command_Access;
      Location_Command : Source_Location_Command;
      Open_At_Line     : Boolean := True;
   begin
      case Command.Mode is
         when Normal =>
            Open_At_Line := True;

         when Done =>
            Open_At_Line := False;

            Other_Command := Get_Next_Command (Command.Queue);

            if Other_Command /= null
              and then Other_Command.all in Source_Location_Command_Type'Class
            then
               Location_Command := Source_Location_Command (Other_Command);

               if Location_Command.Filename = Command.Filename then
                  Open_At_Line := True;
               end if;
            end if;

         when Undone =>
            Other_Command := Get_Previous_Command (Command.Queue);

            if Other_Command /= null
              and then Other_Command.all in Source_Location_Command_Type'Class
            then
               Open_At_Line := False;

               Location_Command := Source_Location_Command (Other_Command);

               if Location_Command.Filename = Command.Filename then
                  Open_At_Line := True;
               end if;
            end if;
      end case;

      if Open_At_Line then
         Open_File_Editor
           (Command.Kernel,
            Command.Filename,
            Command.Line,
            Command.Column,
            Command.Column_End,
            False);
      else
         Open_File_Editor
           (Command.Kernel,
            Command.Filename,
            Enable_Navigation => False);
      end if;

      Command_Finished (Command, True);

      return Success;
   end Execute;

end Commands.Locations;
