------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2008-2018, AdaCore                     --
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

with Ada.Unchecked_Deallocation;

with GNAT.OS_Lib;

with Build_Configurations;       use Build_Configurations;
with GPS.Intl;                   use GPS.Intl;
with GPS.Scripts;                use GPS.Scripts;
with GPS.Scripts.Files;          use GPS.Scripts.Files;

package body Commands.Builder.Scripts is

   ----------------
   --  Constants --
   ----------------

   --  NOTE: these constants must match the names of the predefined targets
   --  registered in builder_support.py.
   Compile_File_Target   : constant String := "Compile File";
   Build_File_Target     : constant String := "Build <current file>";
   Check_Syntax_Target   : constant String := "Check Syntax";
   Check_Semantic_Target : constant String := "Check Semantic";

   --  BuildTarget class

   Target_Name_Cst   : aliased constant String := "target_name";
   Shadow_Cst        : aliased constant String := "shadow";
   Background_Cst    : aliased constant String := "background";
   As_String_Cst     : aliased constant String := "as_string";

   Target_Class_Name : constant String := "BuildTarget";

   Constructor_Args : constant Cst_Argument_List :=
     (2 => Target_Name_Cst'Access);

   type Target_Property is new Instance_Property_Record with record
      Target_Name : Unbounded_String;
      Builder     : Builder_Context;
   end record;
   type Target_Property_Access is access all Target_Property'Class;

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Shell_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Shell command handler

   function Get_Builder (Inst : Class_Instance) return Builder_Context;
   --  Convenience function to get the builder stored in Inst

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

   -----------------
   -- Get_Builder --
   -----------------

   function Get_Builder (Inst : Class_Instance) return Builder_Context is
      T : constant Target_Property_Access := Target_Property_Access
        (Instance_Property'(Get_Data (Inst, Target_Class_Name)));
   begin
      if T = null then
         return null;
      else
         return T.Builder;
      end if;
   end Get_Builder;

   ----------------------
   -- Get_Target_Class --
   ----------------------

   function Get_Target_Class (Kernel : access Core_Kernel_Record'Class)
      return Class_Type is
   begin
      return New_Class (Kernel.Scripts, Target_Class_Name);
   end Get_Target_Class;

   -------------------
   -- Shell_Handler --
   -------------------

   procedure Shell_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Builder      : Builder_Context;
      Kernel       : constant Core_Kernel := Get_Kernel (Data);
      Target_Class : constant Class_Type := Get_Target_Class (Kernel);
      Extra_Args   : GNAT.OS_Lib.Argument_List_Access;
      Info         : Virtual_File;
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

            Builder := Builder_Context
              (Kernel.Module (Builder_Context_Record'Tag));

            --  Verify that the target does exist

            if Get_Target_From_Name (Builder.Registry, Name) = null then
               Set_Error_Msg
                 (Data,
                  (-"No target is registered with the name: '") & Name & "'");
               return;
            end if;

            Set_Data (Inst, Target_Class_Name, Target_Property'
                        (Target_Name => To_Unbounded_String (Name),
                         Builder     => Builder));
         end;

      elsif Command = "execute" then
         declare
            Inst        : constant Class_Instance :=
                            Nth_Arg (Data, 1, Target_Class);
            Main        : constant String  := Nth_Arg (Data, 2, "");
            Force       : constant Boolean := Nth_Arg (Data, 4, False);
            Name        : constant String  := Get_Target_Name (Inst);
            Mode        : Dialog_Mode      := Default;
            Build_Mode  : constant String  := Nth_Arg (Data, 6, "");
            Synchronous : constant Boolean := Nth_Arg (Data, 7, True);
            Directory   : constant Filesystem_String := Nth_Arg (Data, 8, "");
            Dir         : Virtual_File := No_File;
            Quiet       : constant Boolean := Nth_Arg (Data, 9, False);

            On_Exit     : constant Subprogram_Type := Nth_Arg (Data, 10, null);
         begin
            Info := Get_Data
              (Nth_Arg (Data, 3, Get_File_Class (Kernel), True));

            if Base_Name (Info)'Length = 0 then
               Info := No_File;
            end if;

            if Name = "" then
               Set_Error_Msg (Data, -"Invalid target");
               return;
            end if;

            begin
               Extra_Args := GNAT.OS_Lib.Argument_String_To_List
                 (Nth_Arg (Data, 5, ""));
            exception
               when Invalid_Parameter =>
                  declare
                     List : constant List_Instance'Class :=
                       Nth_Arg (Data, 5);
                     Length : constant Natural := List.Number_Of_Arguments;
                  begin
                     Extra_Args := new Argument_List (1 .. Length);
                     for N in 1 .. Length loop
                        Extra_Args (N) := new String'(List.Nth_Arg (N));
                     end loop;
                  end;
            end;

            if Force then
               Mode := Force_No_Dialog;
            end if;

            if Directory /= "" then
               Dir := GNATCOLL.VFS.Create (Directory);
            end if;

            Launch_Target (Builder      => Get_Builder (Inst),
                           Target_Name  => Name,
                           Mode_Name    => Build_Mode,
                           Force_File   => Info,
                           Extra_Args   => Extra_Args,
                           Quiet        => Quiet,
                           Synchronous  => Synchronous,
                           Dialog       => Mode,
                           Via_Menu     => False,
                           Main         => Create (+Main),
                           Main_Project => No_Project,
                           Background   => False,
                           Directory    => Dir,
                           On_Exit      => On_Exit);
            Free (Extra_Args);
         end;

      elsif Command = "get_build_output" then
         Name_Parameters
           (Data,
            (1 => Target_Name_Cst'Access,
             2 => Shadow_Cst'Access,
             3 => Background_Cst'Access,
             4 => As_String_Cst'Access));

         Builder := Builder_Context
           (Kernel.Module (Builder_Context_Record'Tag));

         declare
            S : constant String := To_String
              (Get_Build_Output
                 (Self       => Builder,
                  Target     => Nth_Arg (Data, 1, ""),
                  Shadow     => Nth_Arg (Data, 2, False),
                  Background => Nth_Arg (Data, 3, False)));
            Prev : Integer := S'First;
         begin
            if not Nth_Arg (Data, 4, False) then
               --  Returning output as a list

               Set_Return_Value_As_List (Data);

               for J in S'Range loop
                  if S (J) = ASCII.LF then
                     Set_Return_Value (Data, S (Prev .. J - 1));
                     Prev := J + 1;
                  end if;
               end loop;
            else
               --  Returning output as a string

               Set_Return_Value (Data, S);
            end if;
         end;

      elsif Command = "compile" then
         Info := Get_Data (Nth_Arg (Data, 1, Get_File_Class (Kernel)));
         Extra_Args := GNAT.OS_Lib.Argument_String_To_List
           (Nth_Arg (Data, 2, ""));

         Builder := Builder_Context
           (Kernel.Module (Builder_Context_Record'Tag));

         Launch_Target (Builder      => Builder,
                        Target_Name  => Compile_File_Target,
                        Mode_Name    => "",
                        Force_File   => Info,
                        Extra_Args   => Extra_Args,
                        Quiet        => False,
                        Synchronous  => True,
                        Dialog       => Default,
                        Via_Menu     => False,
                        Background   => False,
                        Main_Project => No_Project,
                        Main         => No_File);

         Free (Extra_Args);

      elsif Command = "make" then
         Info := Get_Data (Nth_Arg (Data, 1, Get_File_Class (Kernel)));
         Extra_Args := GNAT.OS_Lib.Argument_String_To_List
           (Nth_Arg (Data, 2, ""));

         Builder := Builder_Context
           (Kernel.Module (Builder_Context_Record'Tag));

         Launch_Target (Builder      => Builder,
                        Target_Name  => Build_File_Target,
                        Mode_Name    => "",
                        Force_File   => Info,
                        Extra_Args   => Extra_Args,
                        Quiet        => False,
                        Synchronous  => True,
                        Dialog       => Default,
                        Via_Menu     => False,
                        Background   => False,
                        Main_Project => No_Project,
                        Main         => No_File);

         Free (Extra_Args);

      elsif Command = "check_syntax" then
         Info := Get_Data (Nth_Arg (Data, 1, Get_File_Class (Kernel)));
         Builder := Builder_Context
           (Kernel.Module (Builder_Context_Record'Tag));

         Launch_Target (Builder      => Builder,
                        Target_Name  => Check_Syntax_Target,
                        Force_File   => Info,
                        Mode_Name    => "",
                        Extra_Args   => null,
                        Quiet        => False,
                        Synchronous  => True,
                        Dialog       => Default,
                        Via_Menu     => False,
                        Background   => False,
                        Main_Project => No_Project,
                        Main         => No_File);

      elsif Command = "check_semantic" then
         Info := Get_Data (Nth_Arg (Data, 1, Get_File_Class (Kernel)));
         Builder := Builder_Context
           (Kernel.Module (Builder_Context_Record'Tag));

         Launch_Target (Builder      => Builder,
                        Target_Name  => Check_Semantic_Target,
                        Force_File   => Info,
                        Mode_Name    => "",
                        Extra_Args   => null,
                        Quiet        => False,
                        Synchronous  => True,
                        Dialog       => Default,
                        Via_Menu     => False,
                        Background   => False,
                        Main_Project => No_Project,
                        Main         => No_File);

      elsif Command = "get_build_mode" then
         Set_Return_Value (Data, Kernel.Get_Build_Mode);

      elsif Command = "get_target" then
         Set_Return_Value (Data, Kernel.Get_Target);

      elsif Command = "get_runtime" then
         Set_Return_Value (Data, Kernel.Get_Runtime);
      end if;
   end Shell_Handler;

   -----------------------
   -- Register_Commands --
   -----------------------

   procedure Register_Commands (Kernel : access Core_Kernel_Record'Class) is
      Target_Class : constant Class_Type := Get_Target_Class (Kernel);
   begin
      Register_Command
        (Kernel.Scripts,
         Constructor_Method, 1, 1, Shell_Handler'Access, Target_Class);

      Register_Command
        (Kernel.Scripts, "execute",
         Params => (Param ("main_name",   Optional => True),   --  2
                    Param ("file",        Optional => True),   --  3
                    Param ("force",       Optional => True),   --  4
                    Param ("extra_args",  Optional => True),   --  5
                    Param ("build_mode",  Optional => True),   --  6
                    Param ("synchronous", Optional => True),   --  7
                    Param ("directory",   Optional => True),   --  8
                    Param ("quiet",       Optional => True),   --  9
                    Param ("on_exit",     Optional => True)),  --  10
         Class        => Target_Class,
         Handler      => Shell_Handler'Access);

      Register_Command
        (Kernel.Scripts, "get_build_output",
         Handler => Shell_Handler'Access,
         Minimum_Args => 0,
         Maximum_Args => 4);

      --  File commands

      Register_Command
        (Kernel.Scripts, "compile",
         Minimum_Args => 0,
         Maximum_Args => 1,
         Class   => Get_File_Class (Kernel),
         Handler      => Shell_Handler'Access);

      Register_Command
        (Kernel.Scripts, "make",
         Minimum_Args => 0,
         Maximum_Args => 1,
         Class   => Get_File_Class (Kernel),
         Handler      => Shell_Handler'Access);

      --  Global commands

      Register_Command (Kernel.Scripts,
                        Command       => "get_build_mode",
                        Handler       => Shell_Handler'Access);

      Register_Command (Kernel.Scripts,
                        Command       => "get_target",
                        Handler       => Shell_Handler'Access);

      Register_Command (Kernel.Scripts,
                        Command       => "get_runtime",
                        Handler       => Shell_Handler'Access);
   end Register_Commands;

   ----------
   -- Free --
   ----------

   procedure Free (Ar : in out GNAT.OS_Lib.String_List) is
   begin
      for A in Ar'Range loop
         Free (Ar (A));
      end loop;
   end Free;

   procedure Free (Ar : in out GNAT.OS_Lib.String_List_Access) is
      procedure Free is new Ada.Unchecked_Deallocation
          (GNAT.OS_Lib.String_List, GNAT.OS_Lib.String_List_Access);

   begin
      if Ar /= null then
         Free (Ar.all);
         Free (Ar);
      end if;
   end Free;

end Commands.Builder.Scripts;
