------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2015, AdaCore                     --
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

   Command_Class_Name       : constant String := "Command";

   type Command_Property is new Instance_Property_Record with record
      Command : Scheduled_Command_Access;
   end record;
   type Command_Property_Access is access all Command_Property'Class;

   procedure Command_Cmds
     (Data : in out Callback_Data'Class; Command : String);
   --  Handle shell commands.

   ------------------
   -- Command_Cmds --
   ------------------

   procedure Command_Cmds
     (Data : in out Callback_Data'Class; Command : String)
   is
      Command_Class    : constant Class_Type :=
        Get_Command_Class (Get_Kernel (Data));
      Data_Command     : Scheduled_Command_Access;
      Command_Instance : Class_Instance;
   begin
      if Command = Destructor_Method then
         Command_Instance := Nth_Arg (Data, 1, Command_Class);
         Data_Command := Get_Data (Command_Instance);
         if Data_Command /= null then
            Remove_Instance (Data_Command, Get_Script (Data));
            Unref (Command_Access (Data_Command));
         end if;

      elsif Command = "progress" then
         Command_Instance := Nth_Arg (Data, 1, Command_Class);
         Data_Command := Get_Data (Command_Instance);
         Set_Return_Value (Data, Progress (Data_Command).Current);
         Set_Return_Value_Key (Data, "current");
         Set_Return_Value (Data, Progress (Data_Command).Total);
         Set_Return_Value_Key (Data, "total");

      elsif Command = "name" then
         Command_Instance := Nth_Arg (Data, 1, Command_Class);
         Data_Command := Get_Data (Command_Instance);
         Set_Return_Value (Data, Name (Data_Command));

      elsif Command = "get_result" then
         Set_Return_Value
           (Data,
            String'
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

   -----------------------
   -- Get_Command_Class --
   -----------------------

   function Get_Command_Class
     (Kernel : access GPS.Core_Kernels.Core_Kernel_Record'Class)
      return Class_Type is
   begin
      return New_Class (Kernel.Scripts, Command_Class_Name);
   end Get_Command_Class;

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

   ----------
   -- Free --
   ----------

   overriding procedure Free (Command : in out Scheduled_Command) is
      Instances : constant Instance_Array := Get_Instances (Command.Instances);
   begin
      if Command.Is_Dead then
         for J in Instances'Range loop
            if Instances (J) /= No_Class_Instance then
               return;
            end if;
         end loop;

         if Command.Destroy_On_Exit then
            Unref (Command.Command);
         end if;

         Free (Command.Instances);
      else
         declare
            Dead_Command : Scheduled_Command_Access :=
                             new Scheduled_Command;
            Found        : Boolean := False;
         begin
            Dead_Command.Command := Command.Command;
            Dead_Command.Destroy_On_Exit := Command.Destroy_On_Exit;
            Dead_Command.Is_Dead := True;

            for J in Instances'Range loop
               if Instances (J) /= No_Class_Instance then
                  Found := True;
                  Set_Data
                    (Instances (J), Command_Class_Name, Command_Property'
                       (Command => Dead_Command));
               end if;
            end loop;

            --  If the command is not referenced by any script, then we just
            --  remove it.

            if not Found then
               Unref (Command_Access (Dead_Command));
            end if;

            Free (Command.Instances);
         end;
      end if;
   end Free;

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
     (Command : access Root_Command'Class; Destroy_On_Exit : Boolean)
      return Scheduled_Command_Access
   is
      C : Scheduled_Command_Access;
   begin
      if Command.all in Scheduled_Command'Class then
         C := Scheduled_Command_Access (Command);

         if C.Destroy_On_Exit = Destroy_On_Exit then
            return C;
         end if;
      end if;

      C := new Scheduled_Command;
      C.Command := Command_Access (Command);
      C.Destroy_On_Exit := Destroy_On_Exit;

      return C;
   end Create_Wrapper;

   ------------------
   -- Get_Instance --
   ------------------

   function Get_Instance
     (Command       : access Scheduled_Command'Class;
      Language      : access Scripting_Language_Record'Class;
      Command_Class : Class_Type := No_Class)
      return Class_Instance
   is
      Inst : Class_Instance := Get (Command.Instances, Language);
   begin
      if Inst = No_Class_Instance then

         if Command_Class = No_Class then
            declare
               Root_Command_Class : constant Class_Type :=
                 New_Class (Get_Repository (Language), Command_Class_Name);
            begin
               Inst := New_Instance (Language, Root_Command_Class);
            end;
         else
            Inst := New_Instance (Language, Command_Class);
         end if;

         Set_Instance (Command, Language, Inst);
      end if;

      return Inst;
   end Get_Instance;

   ------------------
   -- Set_Instance --
   ------------------

   procedure Set_Instance
     (Command  : access Scheduled_Command'Class;
      Language : access Scripting_Language_Record'Class;
      Instance : Class_Instance) is
   begin
      Set_Data
        (Instance, Command_Class_Name, Command_Property'
           (Command => Scheduled_Command_Access (Command)));
      Ref (Command_Access (Command));  --  unrefed in GPS.Command.__del__
      Set (Command.Instances, Language, Instance);
   end Set_Instance;

   ---------------------
   -- Remove_Instance --
   ---------------------

   procedure Remove_Instance
     (Command  : access Scheduled_Command'Class;
      Language : access Scripting_Language_Record'Class) is
   begin
      Set (Command.Instances, Language, No_Class_Instance);
   end Remove_Instance;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (Instance : Class_Instance) return Scheduled_Command_Access
   is
      Cmd : Command_Property_Access;
   begin
      Cmd := Command_Property_Access
        (Instance_Property'(Get_Data (Instance, Command_Class_Name)));
      if Cmd = null then
         return null;
      else
         return Cmd.Command;
      end if;
   exception
      when Invalid_Data =>
         return null;
   end Get_Data;

   -----------------------
   -- Register_Commands --
   -----------------------

   procedure Register_Commands
     (Kernel : access GPS.Core_Kernels.Core_Kernel_Record'Class)
   is
      Command_Class    : constant Class_Type := Get_Command_Class (Kernel);
   begin
      Register_Command
        (Kernel.Scripts, Destructor_Method,
         0, 0, Command_Cmds'Access, Command_Class);
      Register_Command
        (Kernel.Scripts, "progress",
         0, 0, Command_Cmds'Access, Command_Class);
      Register_Command
        (Kernel.Scripts, "name",
         0, 0, Command_Cmds'Access, Command_Class);
      Register_Command
        (Kernel.Scripts, "get_result",
         0, 0, Command_Cmds'Access, Command_Class);
   end Register_Commands;

end GPS.Scripts.Commands;
