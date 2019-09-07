------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2003-2019, AdaCore                     --
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

package body GPS.Scripts.Commands is

   procedure Free_When_Python_Owns_Ada (S : in out Scheduled_Command_Access);
   --  Free a command, when it was owned by script instances.

   procedure Free_When_Python_Owns_Ada
      (S : in out Scheduled_Command_Access) is
   begin
      Unref (Command_Access (S));
   end Free_When_Python_Owns_Ada;

   package Command_Script_Proxies is new Script_Proxies
      (Scheduled_Command_Access, Command_Script_Proxy,
       Free => Free_When_Python_Owns_Ada);

   procedure Command_Cmds
     (Data : in out Callback_Data'Class; Command : String);
   --  Handle shell commands.

   function Create_Dead_Command
      (Self : Scheduled_Command_Access) return Scheduled_Command_Access;
   function Transfer_Ownership is new Command_Script_Proxies.Transfer_Ownership
      (Detach => Create_Dead_Command);
   --  Transfer ownership of the command to the script instances associated
   --  with it (if any), instead of having the script instances belong to
   --  the command.

   -----------------
   -- Get_Command --
   -----------------

   function Get_Command
      (Data       : Callback_Data'Class;
       Nth        : Positive;
       Allow_Null : Boolean := False)
      return Scheduled_Command_Access is
   begin
      return Command_Script_Proxies.From_Instance
         (Data.Nth_Arg (Nth, Allow_Null => Allow_Null));
   exception
      when No_Data_Set_For_Instance =>
         return null;
   end Get_Command;

   -----------------
   -- Set_Command --
   -----------------

   procedure Set_Command
      (Inst    : Class_Instance;
       Command : not null access Scheduled_Command'Class) is
   begin
      Command_Script_Proxies.Store_In_Instance
         (Command.Instances, Inst, Scheduled_Command_Access (Command));
   end Set_Command;

   ------------------
   -- Get_Instance --
   ------------------

   function Get_Instance
      (Command : not null access Scheduled_Command'Class;
       Script  : not null access Scripting_Language_Record'Class;
       Class_To_Create : String := "")
      return Class_Instance
   is
   begin
      return Command_Script_Proxies.Get_Or_Create_Instance
         (Command.Instances, Scheduled_Command_Access (Command), Script,
          Class_To_Create => Class_To_Create);
   end Get_Instance;

   ------------------
   -- Command_Cmds --
   ------------------

   procedure Command_Cmds
     (Data : in out Callback_Data'Class; Command : String)
   is
      Cmd              : Scheduled_Command_Access;
   begin
      if Command = "progress" then
         Cmd := Get_Command (Data, 1);
         if Cmd /= null then
            Data.Set_Return_Value (Progress (Cmd).Current);
            Data.Set_Return_Value_Key ("current");
            Data.Set_Return_Value (Progress (Cmd).Total);
            Data.Set_Return_Value_Key ("total");
         end if;

      elsif Command = "name" then
         Cmd := Get_Command (Data, 1);
         if Cmd /= null then
            Data.Set_Return_Value (Name (Cmd));
         end if;

      elsif Command = "get_result" then
         Data.Set_Return_Value
           (String'
              ("Error: this primitive should be implemeted by subclasses"));
      end if;
   end Command_Cmds;

   -----------------
   -- Get_Command --
   -----------------

   function Get_Command (Command : access Scheduled_Command'Class)
      return Command_Access is
   begin
      return Command.Command;
   end Get_Command;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Scheduled_Command) return Command_Return_Type is
   begin
      return Execute (Command.Command);
   end Execute;

   ----------
   -- Name --
   ----------

   overriding function Name
     (Command : access Scheduled_Command) return String is
   begin
      return Name (Command.Command);
   end Name;

   --------------
   -- Progress --
   --------------

   overriding function Progress
     (Command : access Scheduled_Command) return Progress_Record is
   begin
      return Progress (Command.Command);
   end Progress;

   ------------------
   -- Set_Progress --
   ------------------

   overriding procedure Set_Progress (Command : access Scheduled_Command;
                                      Progress : Progress_Record) is
   begin
      Set_Progress (Command.Command, Progress);
   end Set_Progress;

   ---------------
   -- Interrupt --
   ---------------

   overriding procedure Interrupt (Command : in out Scheduled_Command) is
   begin
      Interrupt (Command.Command.all);
   end Interrupt;

   -------------------------
   -- Create_Dead_Command --
   -------------------------

   function Create_Dead_Command
      (Self : Scheduled_Command_Access) return Scheduled_Command_Access
   is
      Result : constant Scheduled_Command_Access := new Scheduled_Command;
   begin
      Result.Command := Self.Command;
      Self.Command := null;

      --  Some commands (like a GPS.Kernel.Timeout.Monitor_Command) keep a
      --  reference to the scheduled command they are encapsulated in. This
      --  pointer should thus be reset.

      return Result;
   end Create_Dead_Command;

   --------------------
   -- Primitive_Free --
   --------------------

   overriding procedure Primitive_Free (Command : in out Scheduled_Command) is
   begin
      --  If some script instance is referencing the command, we need to
      --  keep it in memory, but owned by the instances.

      if Transfer_Ownership (Command.Instances) = Has_No_Instances then
         Unref (Command.Command);
      end if;
   end Primitive_Free;

   ----------
   -- Undo --
   ----------

   overriding function Undo (This : access Scheduled_Command) return Boolean is
   begin
      return Undo (This.Command);
   end Undo;

   --------------------
   -- Create_Wrapper --
   --------------------

   function Create_Wrapper
     (Command : access Root_Command'Class)
      return Scheduled_Command_Access
   is
      C : Scheduled_Command_Access;
   begin
      if Command.all in Scheduled_Command'Class then
         C := Scheduled_Command_Access (Command);
         return C;
      end if;

      C := new Scheduled_Command;
      C.Command := Command_Access (Command);

      return C;
   end Create_Wrapper;

   -----------------------
   -- Register_Commands --
   -----------------------

   procedure Register_Commands
     (Kernel : access GPS.Core_Kernels.Core_Kernel_Record'Class)
   is
      Command_Class : constant Class_Type :=
         Kernel.Scripts.New_Class (Command_Class_Name);
   begin
      Kernel.Scripts.Register_Command
        ("progress", Handler => Command_Cmds'Access, Class => Command_Class);
      Kernel.Scripts.Register_Command
        ("name", Handler => Command_Cmds'Access, Class => Command_Class);
      Kernel.Scripts.Register_Command
        ("get_result", Handler => Command_Cmds'Access, Class => Command_Class);
   end Register_Commands;

end GPS.Scripts.Commands;
