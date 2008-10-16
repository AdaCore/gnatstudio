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
with Ada.Unchecked_Deallocation;

with GNAT.OS_Lib;

with GNATCOLL.VFS;       use GNATCOLL.VFS;

with GPS.Kernel;         use GPS.Kernel;
with GPS.Kernel.Actions; use GPS.Kernel.Actions;
with GPS.Kernel.Scripts; use GPS.Kernel.Scripts;
with GNATCOLL.Scripts;   use GNATCOLL.Scripts;

with GPS.Intl;           use GPS.Intl;

with Build_Command_Manager; use Build_Command_Manager;
with Build_Configurations;  use Build_Configurations;

with String_List_Utils;  use String_List_Utils;

package body Builder_Facility_Module.Scripts is

   ----------------
   --  Constants --
   ----------------

   --  NOTE: these constants must match the names of the predefined targets
   --  registered in builder_support.py.
   Compile_File_Target : constant String := "Compile File";
   Build_File_Target : constant String := "Build <current file>";
   Check_Syntax_Target : constant String := "Check Syntax";
   Check_Semantic_Target : constant String := "Check Semantic";
   Build_Main_Target : constant String := "Build Main";

   --  BuildTarget class

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

   procedure Free (Ar : in out GNAT.OS_Lib.String_List);
   procedure Free (Ar : in out GNAT.OS_Lib.String_List_Access);
   --  Free the memory associate with Ar

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
      use String_List;
      Kernel     : constant Kernel_Handle := Get_Kernel (Data);
      Node       : List_Node;
      Extra_Args : GNAT.OS_Lib.Argument_List_Access;
      Info       : Virtual_File;
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

      elsif Command = "get_build_output" then
         Node := First (Get_Build_Output);

         Set_Return_Value_As_List (Data);
         while Node /= Null_Node loop
            Set_Return_Value
              (Data, String_List_Utils.String_List.Data (Node));

            Node := Next (Node);
         end loop;

      elsif Command = "compile" then
         Info := Get_Data (Nth_Arg (Data, 1, Get_File_Class (Kernel)));
         Extra_Args := GNAT.OS_Lib.Argument_String_To_List
           (Nth_Arg (Data, 2, ""));

         Launch_Target (Kernel       => Kernel,
                        Registry     => Registry,
                        Target_Name  => Compile_File_Target,
                        Force_File   => Info,
                        Extra_Args   => Extra_Args,
                        Quiet        => False,
                        Synchronous  => True,
                        Force_Dialog => False,
                        Main         => "");

         Free (Extra_Args);

      elsif Command = "make" then
         Info := Get_Data (Nth_Arg (Data, 1, Get_File_Class (Kernel)));
         Extra_Args := GNAT.OS_Lib.Argument_String_To_List
           (Nth_Arg (Data, 2, ""));

         Launch_Target (Kernel       => Kernel,
                        Registry     => Registry,
                        Target_Name  => Build_File_Target,
                        Force_File   => Info,
                        Extra_Args   => Extra_Args,
                        Quiet        => False,
                        Synchronous  => True,
                        Force_Dialog => False,
                        Main         => "");

         Free (Extra_Args);

      elsif Command = "check_syntax" then
         Info := Get_Data (Nth_Arg (Data, 1, Get_File_Class (Kernel)));
         Launch_Target (Kernel       => Kernel,
                        Registry     => Registry,
                        Target_Name  => Check_Syntax_Target,
                        Force_File   => Info,
                        Extra_Args   => null,
                        Quiet        => False,
                        Synchronous  => True,
                        Force_Dialog => False,
                        Main         => "");

      elsif Command = "check_semantic" then
         Info := Get_Data (Nth_Arg (Data, 1, Get_File_Class (Kernel)));
         Launch_Target (Kernel       => Kernel,
                        Registry     => Registry,
                        Target_Name  => Check_Semantic_Target,
                        Force_File   => Info,
                        Extra_Args   => null,
                        Quiet        => False,
                        Synchronous  => True,
                        Force_Dialog => False,
                        Main         => "");

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

      Register_Command
        (Kernel, "get_build_output",
         Handler => Shell_Handler'Access);

      --  File commands

      Register_Command
        (Kernel, "compile",
         Minimum_Args => 0,
         Maximum_Args => 1,
         Class   => Get_File_Class (Kernel),
         Handler      => Shell_Handler'Access);

      Register_Command
        (Kernel, "make",
         Minimum_Args => 0,
         Maximum_Args => 1,
         Class   => Get_File_Class (Kernel),
         Handler      => Shell_Handler'Access);

      --  Register the "build main number x" actions

      for J in 1 .. 4 loop
         declare
            C : Build_Main_Command_Access;
         begin
            Create (C, Kernel, Registry, Build_Main_Target,
                    J, False, False);
            Register_Action
              (Kernel      => Kernel,
               Name        => (-"Build Main Number") & J'Img,
               Command     => C,
               Description => (-"Build the main source number") & J'Img,
               Filter      => null,
               Category    => -"Build",
               Defined_In  => GNATCOLL.VFS.No_File);
         end;
      end loop;

      Bind_Default_Key (Kernel      => Kernel,
                        Action      => (-"Build Main Number 1"),
                        Default_Key => "F4");
   end Register_Commands;

   ----------
   -- Free --
   ----------

   procedure Free (Ar : in out GNAT.OS_Lib.String_List) is
      use GNAT.OS_Lib;
   begin
      for A in Ar'Range loop
         Free (Ar (A));
      end loop;
   end Free;

   procedure Free (Ar : in out GNAT.OS_Lib.String_List_Access) is
      use GNAT.OS_Lib;
      procedure Free is new
        Ada.Unchecked_Deallocation (GNAT.OS_Lib.String_List,
                                    GNAT.OS_Lib.String_List_Access);

   begin
      if Ar /= null then
         Free (Ar.all);
         Free (Ar);
      end if;
   end Free;

end Builder_Facility_Module.Scripts;
