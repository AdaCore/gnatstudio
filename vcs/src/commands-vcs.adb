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

package body Commands.VCS is

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Commit_Command_Type) is
   begin
      String_List.Free (X.Filenames);
      String_List.Free (X.Logs);
   end Free;

   procedure Free (X : in out Get_Status_Command_Type) is
   begin
      String_List.Free (X.Filenames);
   end Free;

   procedure Free (X : in out Update_Files_Command_Type) is
   begin
      String_List.Free (X.Filenames);
   end Free;

   ------------
   -- Create --
   ------------

   procedure Create
     (Item      : out Commit_Command_Access;
      Rep       : VCS_Access;
      Filenames : String_List.List;
      Logs      : String_List.List) is
   begin
      Item := new Commit_Command_Type;
      Item.Rep       := Rep;
      Item.Filenames := Copy_String_List (Filenames);
      Item.Logs      := Copy_String_List (Logs);
   end Create;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Commit_Command_Type) return Command_Return_Type is
   begin
      Commit (Command.Rep, Command.Filenames, Command.Logs);
      Command_Finished (Command, True);
      return Success;
   end Execute;

   ------------
   -- Create --
   ------------

   procedure Create
     (Item      : out Get_Status_Command_Access;
      Rep       : VCS_Access;
      Filenames : String_List.List) is
   begin
      Item := new Get_Status_Command_Type;
      Item.Rep       := Rep;
      Item.Filenames := Copy_String_List (Filenames);
   end Create;

   procedure Create
     (Item      : out Update_Files_Command_Access;
      Kernel    : Kernel_Handle;
      Filenames : String_List.List) is
   begin
      Item := new Update_Files_Command_Type;
      Item.Kernel    := Kernel;
      Item.Filenames := Copy_String_List (Filenames);
   end Create;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Get_Status_Command_Type) return Command_Return_Type is
   begin
      Get_Status (Command.Rep, Command.Filenames, Clear_Logs => True);
      Command_Finished (Command, True);
      return Success;
   end Execute;

   function Execute
     (Command : access Update_Files_Command_Type) return Command_Return_Type
   is
      use String_List;
      L_Temp : List_Node := First (Command.Filenames);
   begin
      while L_Temp /= Null_Node loop
         File_Changed_On_Disk (Command.Kernel, Data (L_Temp));
         L_Temp := Next (L_Temp);
      end loop;

      Command_Finished (Command, True);
      return Success;
   end Execute;

end Commands.VCS;
