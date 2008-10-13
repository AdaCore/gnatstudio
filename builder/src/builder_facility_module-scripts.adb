-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2008, AdaCore                    --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GPS.Kernel;         use GPS.Kernel;
with GPS.Kernel.Scripts; use GPS.Kernel.Scripts;
with GNATCOLL.Scripts;   use GNATCOLL.Scripts;

with GPS.Intl;           use GPS.Intl;

with Build_Configurations; use Build_Configurations;

package body Builder_Facility_Module.Scripts is

   --  BuildTarget class
   --

   Target_Name_Cst   : aliased constant String := "target_name";
   Target_Class_Name : constant String := "BuildTarget";

   Constructor_Args : constant Cst_Argument_List :=
     (2 => Target_Name_Cst'Access);

   type Target_Property is new Instance_Property_Record with record
      Target_Name : Unbounded_String;
   end record;
   type Target_Property_Access is access all Target_Property'Class;

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Shell_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Shell command handler.

   function Get_Target_Class (Kernel : access Kernel_Handle_Record'Class)
                              return Class_Type;
   --  Convenience function to get the target class

   function Get_Target_Name (Inst : Class_Instance) return String;
   --  Convenience function to get the target stored in Inst.

   ---------------------
   -- Get_Target_Name --
   ---------------------

   function Get_Target_Name (Inst : Class_Instance) return String is
      T : constant Target_Property_Access := Target_Property_Access
        (Instance_Property'(Get_Data (Inst, Target_Class_Name)));
   begin
      if T = null then
         return "";
      else
         return To_String (T.Target_Name);
      end if;
   end Get_Target_Name;

   ----------------------
   -- Get_Target_Class --
   ----------------------

   function Get_Target_Class (Kernel : access Kernel_Handle_Record'Class)
      return Class_Type is
   begin
      return New_Class (Kernel, Target_Class_Name);
   end Get_Target_Class;

   -------------------
   -- Shell_Handler --
   -------------------

   procedure Shell_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Target_Class : constant Class_Type :=
        Get_Target_Class (Get_Kernel (Data));
   begin
      if Command = Constructor_Method then
         Name_Parameters (Data, Constructor_Args);

         declare
            Inst : constant Class_Instance := Nth_Arg (Data, 1, Target_Class);
            Name : constant String := Nth_Arg (Data, 2);
         begin
            if Name = "" then
               Set_Error_Msg (Data, -"Target name must be specified.");
               return;
            end if;

            --  Verify that the target does exist.

            if Get_Target_From_Name (Registry, Name) = null then
               Set_Error_Msg
                 (Data,
                  (-"No target is registered with the name: '") & Name & "'");
            end if;

            Set_Data (Inst, Target_Class_Name, Target_Property'
                        (Target_Name => To_Unbounded_String (Name)));
         end;

      elsif Command = "remove" then
         declare
            Inst : constant Class_Instance := Nth_Arg (Data, 1, Target_Class);
            Name : constant String := Get_Target_Name (Inst);
         begin
            if Name = "" then
               Set_Error_Msg (Data, -"Invalid target");
               return;
            end if;

            Remove_Target (Registry, Name);

            --  ??? Need to update the IDE items: icon, menu, and build action.
         end;
      end if;
   end Shell_Handler;

   -----------------------
   -- Register_Commands --
   -----------------------

   procedure Register_Commands (Kernel : GPS.Kernel.Kernel_Handle) is
      Target_Class : constant Class_Type := Get_Target_Class (Kernel);
   begin
      Register_Command
        (Kernel, Constructor_Method, 1, 1, Shell_Handler'Access, Target_Class);

      Register_Command
        (Kernel, "remove",
         Minimum_Args => 0,
         Maximum_Args => 0,
         Class        => Target_Class,
         Handler      => Shell_Handler'Access);
   end Register_Commands;

end Builder_Facility_Module.Scripts;
