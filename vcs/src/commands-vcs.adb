------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2017, AdaCore                     --
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

with GNAT.Strings;
with GPS.Intl;            use GPS.Intl;
with GPS.Kernel.Hooks;    use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;      use GPS.Kernel.MDI;
with GNATCOLL.Traces;              use GNATCOLL.Traces;
with VCS_Module;          use VCS_Module;
with VCS_Status;          use VCS_Status;
with VCS_View;            use VCS_View;
with VCS_View.Activities; use VCS_View.Activities;

package body Commands.VCS is

   use type GNAT.Strings.String_Access;

   Me : constant Trace_Handle := Create ("Command.VCS");

   --------------------
   -- Primitive_Free --
   --------------------

   overriding procedure Primitive_Free (X : in out Log_Action_Command_Type) is
   begin
      Unchecked_Free (X.Filenames);
   end Primitive_Free;

   overriding procedure Primitive_Free (X : in out Get_Status_Command_Type) is
   begin
      Unchecked_Free (X.Filenames);
   end Primitive_Free;

   overriding procedure Primitive_Free
     (X : in out Update_Files_Command_Type) is
   begin
      Unchecked_Free (X.Filenames);
   end Primitive_Free;

   ------------
   -- Create --
   ------------

   procedure Create
     (Item      : out Log_Action_Command_Access;
      Kernel    : not null access Kernel_Handle_Record'Class;
      Rep       : VCS_Access;
      Action    : VCS_Action;
      Filenames : File_Array;
      Logs      : String_List.Vector) is
   begin
      Item := new Log_Action_Command_Type;
      Item.Kernel    := Kernel;
      Item.Rep       := Rep;
      Item.Filenames := new File_Array'(Filenames);
      Item.Logs      := Copy_String_List (Logs);
      Item.Action    := Action;
   end Create;

   procedure Create
     (Item      : out Get_Status_Command_Access;
      Rep       : VCS_Access;
      Filenames : File_Array) is
   begin
      Item := new Get_Status_Command_Type;
      Item.Rep       := Rep;
      Item.Filenames := new File_Array'(Filenames);
   end Create;

   procedure Create
     (Item      : out Update_Files_Command_Access;
      Kernel    : not null access Kernel_Handle_Record'Class;
      Filenames : File_Array) is
   begin
      Item := new Update_Files_Command_Type;
      Item.Kernel    := Kernel;
      Item.Filenames := new File_Array'(Filenames);
   end Create;

   procedure Create
     (Item     : out Generic_Kernel_Command_Access;
      Kernel   : not null access Kernel_Handle_Record'Class;
      Callback : Context_Callback.Marshallers.Void_Marshaller.Handler) is
   begin
      Item := new Generic_Kernel_Command;
      Item.Kernel   := Kernel;
      Item.Callback := Callback;
   end Create;

   procedure Create
     (Item     : out Check_Activity_Command_Access;
      Kernel   : not null access Kernel_Handle_Record'Class;
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
      use type Ada.Containers.Count_Type;

      Log   : Cursor;
      Idx   : Natural;
      Last  : Natural;

   begin
      --  If we have a single element in Log, then the same log must be used
      --  for all files.

      Log := First (Command.Logs);

      if Command.Logs.Is_Empty then
         --  No log, this is the case when no log is required by the external
         --  VCS.
         case Command.Action is
            when Commit =>
               Commit (Command.Rep, Command.Filenames.all, "");

            when Add =>
               Add (Command.Rep, Command.Filenames.all, "");

            when Remove =>
               Remove (Command.Rep, Command.Filenames.all, "");

            when others =>
               raise Program_Error;
         end case;

      elsif Command.Logs.Length = 1 then
         case Command.Action is
            when Commit =>
               Commit (Command.Rep, Command.Filenames.all, Element (Log));

            when Add =>
               Add (Command.Rep, Command.Filenames.all, Element (Log));

            when Remove =>
               Remove (Command.Rep, Command.Filenames.all, Element (Log));

            when others =>
               raise Program_Error;
         end case;

      else
         --  Log is not shared, launch one command for each commit having the
         --  same log.

         Idx := Command.Filenames'First;
         while Idx <= Command.Filenames'Last loop

            declare
               Log_Content : constant String := Element (Log);
            begin
               Last := Idx;
               loop
                  Log  := Next (Log);
                  Last := Last + 1;
                  exit when Last > Command.Filenames'Last
                    or else not Has_Element (Log)
                    or else Element (Log) /= Log_Content;
               end loop;

               case Command.Action is
                  when Commit =>
                     Commit
                       (Command.Rep,
                        Command.Filenames (Idx .. Last - 1),
                        Log_Content);

                  when Add =>
                     Add
                       (Command.Rep,
                        Command.Filenames (Idx .. Last - 1),
                        Log_Content);

                  when Remove =>
                     Remove
                       (Command.Rep,
                        Command.Filenames (Idx .. Last - 1),
                        Log_Content);

                  when others =>
                     raise Program_Error;
               end case;

               Idx := Last;
            end;
         end loop;
      end if;

      Command_Finished (Command, True);
      return Success;

   exception
      when Constraint_Error =>
         Trace (Me, "Logs do not correspond to files");
         return Failure;
   end Execute;

   overriding function Execute
     (Command : access Get_Status_Command_Type) return Command_Return_Type is
   begin
      Get_Status (Command.Rep, Command.Filenames.all, Clear_Logs => True);
      Command_Finished (Command, True);
      return Success;
   end Execute;

   overriding function Execute
     (Command : access Update_Files_Command_Type) return Command_Return_Type is
   begin
      for J in Command.Filenames'Range loop
         File_Changed_On_Disk_Hook.Run (Command.Kernel, Command.Filenames (J));
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

      Command.Callback (null, The_Context);

      return Success;
   end Execute;

   overriding function Execute
     (Command : access Check_Activity_Command_Type) return Command_Return_Type
   is
      Explorer  : constant VCS_Activities_View_Access :=
                    Get_Activities_Explorer (Command.Kernel, False, False);
      VCS_Ref   : constant VCS_Access :=
                    Get_VCS_For_Activity (Command.Kernel, Command.Activity);
      Files     : constant File_Array :=
                    Get_Files_In_Activity (Command.Activity);
      Closed    : Boolean := True;
   begin
      --  Set the committed status if all files are up-to-date or removed

      for J in Files'Range loop
         declare
            File : Virtual_File renames Files (J);
         begin
            Closed := Closed
              and then
                (Has_Status (Get_Status_Cache, File, VCS_Ref, Up_To_Date_Id)
                 or else
                 Has_Status (Get_Status_Cache, File, VCS_Ref, Removed_Id));
         end;
      end loop;

      Set_Closed (Command.Kernel, Command.Activity, To => Closed);
      Refresh (Explorer);

      Activity_Checked_Hook.Run (Command.Kernel);

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
