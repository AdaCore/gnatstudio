-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2005                       --
--                              AdaCore                              --
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

with GPS.Intl;    use GPS.Intl;
with VFS;         use VFS;
with Traces;      use Traces;
with Basic_Types; use Basic_Types;

package body Commands.VCS is

   Me : constant Debug_Handle := Create ("Command.VCS");

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Log_Action_Command_Type) is
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

   procedure Free (X : in out Generic_Kernel_Command) is
      pragma Unreferenced (X);
   begin
      null;
   end Free;

   ------------
   -- Create --
   ------------

   procedure Create
     (Item      : out Log_Action_Command_Access;
      Rep       : VCS_Access;
      Action    : VCS_Action;
      Filenames : String_List.List;
      Logs      : String_List.List) is
   begin
      Item := new Log_Action_Command_Type;
      Item.Rep       := Rep;
      Item.Filenames := Copy_String_List (Filenames);
      Item.Logs      := Copy_String_List (Logs);
      Item.Action    := Action;
   end Create;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Log_Action_Command_Type) return Command_Return_Type
   is
      use String_List;

      Node : List_Node;
      Log  : List_Node;

      File : List;
   begin
      --  If we have a single element in Log, then the same log must be used
      --  for all files.

      Log := First (Command.Logs);

      if Length (Command.Logs) = 1 then
         case Command.Action is
            when Commit =>
               Commit (Command.Rep, Command.Filenames, Data (Log));

            when Add =>
               Add (Command.Rep, Command.Filenames, Data (Log));

            when Add_No_Commit =>
               Add (Command.Rep, Command.Filenames, Data (Log),
                    Commit => False);

            when Remove =>
               Remove (Command.Rep, Command.Filenames, Data (Log));

            when Remove_No_Commit =>
               Remove (Command.Rep, Command.Filenames, Data (Log),
                       Commit => False);

            when others =>
               raise Program_Error;
         end case;

      else
         --  Log is not shared, launch one command for each commit

         Node := First (Command.Filenames);

         while Node /= Null_Node loop
            Append (File, Data (Node));

            case Command.Action is
               when Commit =>
                  Commit (Command.Rep, File, Data (Log));

               when Add =>
                  Add (Command.Rep, File, Data (Log));

               when Add_No_Commit =>
                  Add (Command.Rep, File, Data (Log), Commit => False);

               when Remove =>
                  Remove (Command.Rep, File, Data (Log));

               when Remove_No_Commit =>
                  Remove (Command.Rep, File, Data (Log), Commit => False);

               when others =>
                  raise Program_Error;
            end case;

            Free (File);
            Node := Next (Node);
            Log  := Next (Log);
         end loop;
      end if;

      Command_Finished (Command, True);
      return Success;

   exception
      when List_Empty =>
         Trace (Me, "Logs do not correspond to files");
         return Failure;
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

   procedure Create
     (Item      : out Generic_Kernel_Command_Access;
      Kernel    : Kernel_Handle;
      Callback  : Context_Callback.Marshallers.Void_Marshaller.Handler) is
   begin
      Item := new Generic_Kernel_Command;
      Item.Kernel   := Kernel;
      Item.Callback := Callback;
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
         File_Changed_On_Disk
           (Command.Kernel, Create (Full_Filename => Data (L_Temp)));
         L_Temp := Next (L_Temp);
      end loop;

      Command_Finished (Command, True);
      return Success;
   end Execute;

   function Execute
     (Command : access Generic_Kernel_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      The_Context : Selection_Context_Access := Context.Context;
   begin
      if The_Context = null then
         The_Context := Get_Current_Context (Command.Kernel);
      end if;

      Ref (The_Context);
      Command.Callback (Command.Kernel, The_Context);
      Unref (The_Context);

      return Success;
   end Execute;

   ----------
   -- Name --
   ----------

   function Name (X : access Log_Action_Command_Type) return String is
      Action_String : constant Basic_Types.String_Access :=
                        Get_Identified_Actions (X.Rep) (X.Action);
   begin
      if Action_String /= null then
         return Action_String.all;
      else
         return -"Unnamed VCS action";
      end if;
   end Name;

   function Name (X : access Get_Status_Command_Type) return String is
      pragma Unreferenced (X);
   begin
      return -"Getting status";
   end Name;

   function Name (X : access Update_Files_Command_Type) return String is
      pragma Unreferenced (X);
   begin
      return -"Updating files";
   end Name;

   function Name (X : access Generic_Kernel_Command) return String is
      pragma Unreferenced (X);
   begin
      return -"VCS";
   end Name;

end Commands.VCS;
