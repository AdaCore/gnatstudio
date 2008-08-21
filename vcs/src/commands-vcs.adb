-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2001-2008, AdaCore                  --
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

with GNAT.Strings;
with GPS.Intl;            use GPS.Intl;
with GPS.Kernel.Hooks;    use GPS.Kernel.Hooks;
with Traces;              use Traces;
with VCS_Module;          use VCS_Module;
with VCS_Status;          use VCS_Status;
with VCS_View;            use VCS_View;
with VCS_View.Activities; use VCS_View.Activities;
with GNATCOLL.VFS;        use GNATCOLL.VFS;

package body Commands.VCS is

   use type GNAT.Strings.String_Access;

   Me : constant Debug_Handle := Create ("Command.VCS");

   ----------
   -- Free --
   ----------

   overriding procedure Free (X : in out Log_Action_Command_Type) is
   begin
      String_List.Free (X.Filenames);
      String_List.Free (X.Logs);
   end Free;

   overriding procedure Free (X : in out Get_Status_Command_Type) is
   begin
      String_List.Free (X.Filenames);
   end Free;

   overriding procedure Free (X : in out Update_Files_Command_Type) is
   begin
      String_List.Free (X.Filenames);
   end Free;

   overriding procedure Free (X : in out Generic_Kernel_Command) is
      pragma Unreferenced (X);
   begin
      null;
   end Free;

   ------------
   -- Create --
   ------------

   procedure Create
     (Item      : out Log_Action_Command_Access;
      Kernel    : Kernel_Handle;
      Rep       : VCS_Access;
      Action    : VCS_Action;
      Filenames : String_List.List;
      Logs      : String_List.List) is
   begin
      Item := new Log_Action_Command_Type;
      Item.Kernel    := Kernel;
      Item.Rep       := Rep;
      Item.Filenames := Copy_String_List (Filenames);
      Item.Logs      := Copy_String_List (Logs);
      Item.Action    := Action;
   end Create;

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

   procedure Create
     (Item     : out Check_Activity_Command_Access;
      Kernel   : Kernel_Handle;
      Activity : Activity_Id) is
   begin
      Item := new Check_Activity_Command_Type;
      Item.Kernel := Kernel;
      Item.Activity := Activity;
   end Create;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Log_Action_Command_Type) return Command_Return_Type
   is
      use String_List;

      Node  : List_Node;
      Log   : List_Node;

      Files : List;
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

            when Remove =>
               Remove (Command.Rep, Command.Filenames, Data (Log));

            when others =>
               raise Program_Error;
         end case;

      else
         --  Log is not shared, launch one command for each commit having the
         --  same log.

         Node := First (Command.Filenames);

         while Node /= Null_Node loop

            declare
               Log_Content : constant String := Data (Log);
            begin
               loop
                  Append (Files, Data (Node));
                  Node := Next (Node);
                  Log  := Next (Log);

                  exit when Log = Null_Node or else Data (Log) /= Log_Content;
               end loop;

               case Command.Action is
                  when Commit =>
                     Commit (Command.Rep, Files, Log_Content);

                  when Add =>
                     Add (Command.Rep, Files, Log_Content);

                  when Remove =>
                     Remove (Command.Rep, Files, Log_Content);

                  when others =>
                     raise Program_Error;
               end case;

               Free (Files);
            end;
         end loop;
      end if;

      if Command.Action = Commit then
         Run_Hook (Command.Kernel, Commit_Done_Hook);
      end if;

      Command_Finished (Command, True);
      return Success;

   exception
      when List_Empty =>
         Trace (Me, "Logs do not correspond to files");
         return Failure;
   end Execute;

   overriding function Execute
     (Command : access Get_Status_Command_Type) return Command_Return_Type is
   begin
      Get_Status (Command.Rep, Command.Filenames, Clear_Logs => True);
      Command_Finished (Command, True);
      return Success;
   end Execute;

   overriding function Execute
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

   overriding function Execute
     (Command : access Generic_Kernel_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      The_Context : Selection_Context := Context.Context;
   begin
      if The_Context = No_Context then
         The_Context := Get_Current_Context (Command.Kernel);
      end if;

      Command.Callback (Command.Kernel, The_Context);

      return Success;
   end Execute;

   overriding function Execute
     (Command : access Check_Activity_Command_Type)
      return Command_Return_Type
   is
      use type String_List.List_Node;
      Explorer  : constant VCS_Activities_View_Access :=
                    Get_Activities_Explorer (Command.Kernel, False, False);
      VCS_Ref   : constant VCS_Access :=
                    Get_VCS_For_Activity (Command.Kernel, Command.Activity);
      Files     : constant String_List.List :=
                    Get_Files_In_Activity (Command.Activity);
      Files_It  : String_List.List_Node;
      Closed    : Boolean := True;
   begin
      --  Set the committed status if all files are up-to-date or removed

      Files_It := String_List.First (Files);

      while Files_It /= String_List.Null_Node loop
         declare
            File : constant Virtual_File :=
                     Create (Full_Filename => String_List.Data (Files_It));
         begin
            Closed := Closed
              and then
                (Has_Status (Get_Status_Cache, File, VCS_Ref, Up_To_Date_Id)
                 or else
                 Has_Status (Get_Status_Cache, File, VCS_Ref, Removed_Id));
         end;
         Files_It := String_List.Next (Files_It);
      end loop;

      Set_Closed (Command.Kernel, Command.Activity, To => Closed);
      Refresh (Explorer);

      Run_Hook (Command.Kernel, Activity_Checked_Hook);

      Command_Finished (Command, True);
      return Success;
   end Execute;

   ----------
   -- Name --
   ----------

   overriding function Name
     (X : access Log_Action_Command_Type) return String
   is
      Action_String : constant GNAT.Strings.String_Access :=
                        Get_Identified_Actions (X.Rep) (X.Action);
   begin
      if Action_String /= null then
         return Action_String.all;
      else
         return -"Unnamed VCS action";
      end if;
   end Name;

   overriding function Name
     (X : access Get_Status_Command_Type) return String
   is
      pragma Unreferenced (X);
   begin
      return -"Getting status";
   end Name;

   overriding function Name
     (X : access Update_Files_Command_Type) return String
   is
      pragma Unreferenced (X);
   begin
      return -"Updating files";
   end Name;

   overriding function Name
     (X : access Generic_Kernel_Command) return String
   is
      pragma Unreferenced (X);
   begin
      return -"VCS";
   end Name;

   overriding function Name
     (X : access Check_Activity_Command_Type) return String
   is
      pragma Unreferenced (X);
   begin
      return -"Check Activity";
   end Name;

end Commands.VCS;
