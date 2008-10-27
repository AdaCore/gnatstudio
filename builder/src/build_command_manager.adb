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

with Ada.Unchecked_Deallocation;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;

with GNATCOLL.Templates; use GNATCOLL.Templates;
with GPS.Kernel;             use GPS.Kernel;
with GPS.Kernel.Console;     use GPS.Kernel.Console;
with GPS.Kernel.Contexts;    use GPS.Kernel.Contexts;
with GPS.Kernel.Macros;      use GPS.Kernel.Macros;
with GPS.Kernel.Preferences; use GPS.Kernel.Preferences;
with GPS.Kernel.Project;     use GPS.Kernel.Project;
with GPS.Intl;               use GPS.Intl;

with GPS.Location_View;      use GPS.Location_View;

with Projects.Registry;      use Projects.Registry;

with Commands.Builder; use Commands.Builder;

with Build_Configurations.Gtkada; use Build_Configurations.Gtkada;

with OS_Utils;           use OS_Utils;
with Projects;           use Projects;
with Remote;             use Remote;
with String_Utils;       use String_Utils;
with Traces;             use Traces;

with Builder_Facility_Module; use Builder_Facility_Module;

package body Build_Command_Manager is

   Me : constant Debug_Handle := Create ("Build_Command_Manager");

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Argument_List, Argument_List_Access);

   Invalid_Argument : exception;
   --  Raised by Expand_Arg below.

   function Expand_Command_Line
     (Kernel     : GPS.Kernel.Kernel_Handle;
      CL         : Argument_List;
      Server     : Server_Type;
      Force_File : Virtual_File;
      Main       : String;
      Subdir     : String;
      Simulate   : Boolean := False) return Argument_List_Access;
   --  Expand all macros contained in CL using the GPS macro language.
   --  User must free the result.
   --  CL must contain at least one element.
   --  If Simulate is true, never fail on unknown parameters.

   function Expand_Arg
     (Kernel     : GPS.Kernel.Kernel_Handle;
      Context    : GPS.Kernel.Selection_Context;
      Arg        : String;
      Server     : Server_Type;
      Force_File : Virtual_File;
      Main       : String;
      Subdir     : String;
      Simulate   : Boolean) return Argument_List;
   --  Expand macros contained in Arg.
   --  Caller must free the result.
   --  Will raise Invalid_Argument if an invalid/non existent argument is
   --  found.
   --  If Simulate is true, Invalid_Argument will never be raised, and no
   --  expansion will be done.

   procedure Free (Ar : in out Argument_List);
   --  Free memory associated to Ar.

   ----------
   -- Free --
   ----------

   procedure Free (Ar : in out Argument_List) is
   begin
      for A in Ar'Range loop
         Free (Ar (A));
      end loop;
   end Free;

   ----------------
   -- Expand_Arg --
   ----------------

   function Expand_Arg
     (Kernel     : GPS.Kernel.Kernel_Handle;
      Context    : GPS.Kernel.Selection_Context;
      Arg        : String;
      Server     : Server_Type;
      Force_File : Virtual_File;
      Main       : String;
      Subdir     : String;
      Simulate   : Boolean) return Argument_List
   is
      function Substitution
        (Param  : String; Quoted : Boolean) return String;
      --  Wrapper around GPS.Kernel.Macros.Substitute

      ------------------
      -- Substitution --
      ------------------

      function Substitution
        (Param : String; Quoted : Boolean) return String
      is
         Done   : aliased Boolean := False;
      begin
         if Param = "subdir" then
            return Subdir;
         else
            declare
               Result : constant String := GPS.Kernel.Macros.Substitute
                 (Param, Context, Quoted, Done'Access, Server => Server);
            begin
               if Result = "" then
                  if Simulate then
                     return '%' & Param;
                  else
                     raise Invalid_Argument;
                  end if;
               else
                  return Result;
               end if;
            end;
         end if;
      end Substitution;

   begin
      --  ??? Special case for "%X"
      --  We are implementing a special case here since GPS.Kernel.Macros
      --  does not support returning an Argument_List.
      --  See H926-007.

      if Arg = "%X" then
         declare
            Vars : Argument_List_Access := Argument_String_To_List
              (Scenario_Variables_Cmd_Line (Kernel, "-X"));
            --  ??? Scenario_Variables_Cmd_Line should be modified to
            --  return an Argument_List[_Access].
            Res  : constant Argument_List := Vars.all;
         begin
            Unchecked_Free (Vars);
            return Res;
         end;

      --  ??? Ditto for %vars
      elsif Arg = "%vars" then
         declare
            Vars : Argument_List_Access := Argument_String_To_List
              (Scenario_Variables_Cmd_Line (Kernel, ""));
            Res  : constant Argument_List := Vars.all;
         begin
            Unchecked_Free (Vars);
            return Res;
         end;

      --  ??? Would be nice to support a generic %vars(xxx)
      elsif Arg = "%vars(-D)" then
         declare
            Vars : Argument_List_Access := Argument_String_To_List
              (Scenario_Variables_Cmd_Line (Kernel, "-D"));
            Res  : constant Argument_List := Vars.all;
         begin
            Unchecked_Free (Vars);
            return Res;
         end;

      --  ??? Ditto for %eL
      elsif Arg = "%eL" then
         if Trusted_Mode.Get_Pref then
            return (1 .. 0 => null);
         else
            return (1 => new String'("-eL"));
         end if;

      --  ??? Ditto for %builder, %gprbuild and %gprclean
      elsif Arg = "%builder"
        or else Arg = "%gprbuild"
        or else Arg = "%gprclean"
      then
         declare
            Builder  : constant Boolean := Arg /= "%gprclean";
            Prj      : constant Project_Type :=
                         Get_Project (Get_Kernel (Context));
            Gnatmake : constant String :=
                         Get_Attribute_Value
                           (Prj, Compiler_Command_Attribute,
                            Default => "gnatmake",
                            Index   => "Ada");
            First    : Natural := Gnatmake'First;
            Langs    : Argument_List := Get_Languages (Prj, Recursive => True);

            Multi_Language_Build : Boolean := True;

         begin
            if Arg /= "%gprbuild"
              and then Langs'Length = 1
              and then Langs (Langs'First).all = "ada"
            then
               --  Determine if the project has only Ada set, if so, set
               --  Multi_Language_Build to False.

               Multi_Language_Build := False;
            end if;

            Free (Langs);

            if Multi_Language_Build
              and then Multi_Language_Builder.Get_Pref = Gprbuild
            then
               if Gnatmake'Length > 9
                 and then Gnatmake
                   (Gnatmake'Last - 8 .. Gnatmake'Last) = "-gnatmake"
               then
                  for J in reverse Gnatmake'First .. Gnatmake'Last - 9 loop
                     if Is_Directory_Separator (Gnatmake (J)) then
                        First := J + 1;
                        exit;
                     end if;
                  end loop;

                  if Builder then
                     return (new String'("gprbuild"), new String'("--target="
                       & Gnatmake (First .. Gnatmake'Last - 9)));
                  else
                     return (new String'("gprclean"), new String'("--target="
                       & Gnatmake (First .. Gnatmake'Last - 9)));
                  end if;

               elsif Builder then
                  return (1 => new String'("gprbuild"));
               else
                  return (1 => new String'("gprclean"));
               end if;

            elsif Builder then
               if Multi_Language_Build then
                  return (1 => new String'("gprmake"));
               else
                  return (1 => new String'(Gnatmake));
               end if;
            else
               return
                 (new String'(Get_Attribute_Value
                   (Prj, GNAT_Attribute, Default => "gnat")),
                  new String'("clean"));
            end if;
         end;

      elsif Arg = "%fp" then
         if Force_File /= No_File then
            --  We are launching a compile command involving Force_File:
            --  remove reference to File from the Locations View.
            --  See F830-003.
            Remove_Location_Category (Kernel, Error_Category, Force_File);
            return (1 => new String'(Base_Name (Force_File)));
         end if;

         declare
            File : constant Virtual_File := File_Information (Context);
         begin
            if File = No_File then
               if Simulate then
                  return (1 => new String'("<current-file>"));
               else
                  Console.Insert
                    (Kernel, -"No file selected", Mode => Console.Error);
                  raise Invalid_Argument;
               end if;
            elsif Get_Project_From_File
              (Get_Registry (Kernel).all, File, False) = No_Project
            then
               if Simulate then
                  return (1 => new String'("<current-file>"));
               else
                  Console.Insert
                    (Kernel, -"Could not determine the project for file: "
                     & Full_Name (File).all,
                     Mode => Console.Error);
                  raise Invalid_Argument;
               end if;
            else
               --  We are launching a compile command involving File:
               --  remove reference to File from the Locations View.
               --  See F830-003.
               Remove_Location_Category (Kernel, Error_Category, File);

               return (1 => new String'(Base_Name (File)));
            end if;
         end;

      elsif Arg = "%M" then
         if Main /= "" then
            return (1 => new String'(Main));
         else
            Console.Insert
              (Kernel, -"Could not determine the main to build.",
               Mode => Console.Error);
            raise Invalid_Argument;
         end if;

      else
         return (1 => new String'
                   (GNATCOLL.Templates.Substitute
                    (Str => Arg,
                     Delimiter => GPS.Kernel.Macros.Special_Character,
                     Callback  => Substitution'Unrestricted_Access)));
      end if;
   end Expand_Arg;

   -------------------------
   -- Expand_Command_Line --
   -------------------------

   function Expand_Command_Line
     (Kernel     : GPS.Kernel.Kernel_Handle;
      CL         : Argument_List;
      Server     : Server_Type;
      Force_File : Virtual_File;
      Main       : String;
      Subdir     : String;
      Simulate   : Boolean := False) return Argument_List_Access
   is
      Result : Argument_List_Access := new Argument_List (1 .. CL'Length * 2);
      Index  : Natural := 1;
      --  Index of the next free element in Result.

      Context : constant Selection_Context := Get_Current_Context (Kernel);

   begin
      for J in CL'Range loop
         if CL (J) = null then
            --  This should not happen
            Insert
              (Kernel, (-"Invalid command line"),
               Mode => Error);
            Free (Result);
            return null;
         end if;

         declare
            Expanded : constant Argument_List :=
              Expand_Arg
                (Kernel, Context, CL (J).all, Server,
                 Force_File, Main, Subdir, Simulate);
         begin
            --  Expand the result if needed
            if Result'Last - Index < Expanded'Length then
               declare
                  New_Result : constant Argument_List_Access :=
                    new Argument_List
                      (1 .. (Result'Length + Expanded'Length) * 2);
               begin
                  for K in 1 .. Index - 1 loop
                     New_Result (K) := Result (K);
                  end loop;
                  Unchecked_Free (Result);
                  Result := New_Result;
               end;
            end if;

            Result (Index .. Index + Expanded'Length - 1) := Expanded;

            Index := Index + Expanded'Length;
         end;
      end loop;

      declare
         Real_Result : Argument_List_Access;
      begin
         Real_Result := new Argument_List (1 .. Index - 1);
         Real_Result (1 .. Index - 1) := Result (1 .. Index - 1);
         Unchecked_Free (Result);
         return Real_Result;
      end;

   exception
      when Invalid_Argument =>
         Insert
           (Kernel, (-"Invalid context, cannot build"),
            Mode => Console.Error);
         Free (Result);
         return null;
   end Expand_Command_Line;

   -------------------
   -- Launch_Target --
   -------------------

   procedure Launch_Target
     (Kernel       : GPS.Kernel.Kernel_Handle;
      Registry     : Build_Config_Registry_Access;
      Target_Name  : String;
      Mode_Name    : String;
      Force_File   : Virtual_File;
      Extra_Args   : Argument_List_Access;
      Quiet        : Boolean;
      Synchronous  : Boolean;
      Dialog       : Dialog_Mode;
      Main         : String)
   is
      Prj          : constant Project_Type := Get_Project (Kernel);
      Old_Dir      : constant Dir_Name_Str := Get_Current_Dir;
      T            : Target_Access;
      Full         : Argument_List_Access;
      Command_Line : Argument_List_Access;
      All_Extra_Args : Argument_List_Access;

      procedure Launch_For_Mode
        (Mode   : String;
         Quiet  : Boolean;
         Shadow : Boolean);
      --  Compute and launch the command, for the given mode.

      ---------------------
      -- Launch_For_Mode --
      ---------------------

      procedure Launch_For_Mode
        (Mode   : String;
         Quiet  : Boolean;
         Shadow : Boolean) is

         Subdir : constant String := Get_Mode_Subdir (Mode);
         Server : Server_Type;

         function Expand_Cmd_Line (CL : String) return String;
         --  Callback for Single_Target_Dialog

         function Expand_Cmd_Line (CL : String) return String is
            CL_Args : Argument_List_Access := Argument_String_To_List (CL);
            Mode_Args : Argument_List_Access :=
                          Apply_Mode_Args (Get_Model (T), Mode, CL_Args.all);
            Args    : Argument_List_Access :=
              Expand_Command_Line
                (Kernel, Mode_Args.all & All_Extra_Args.all, Server,
                 Force_File, Main, Subdir, Simulate => True);
            Res     : constant String := Argument_List_To_String (Args.all);

         begin
            Free (CL_Args);
            Free (Mode_Args);
            Free (Args);
            return Res;
         end Expand_Cmd_Line;

      begin
         --  Compute the extra args, taking into account the mode and the
         --  extra args explicitely passed.

         if Extra_Args /= null then
            All_Extra_Args := new Argument_List'(Extra_Args.all);
         else
            All_Extra_Args := new Argument_List (1 .. 0);
         end if;

         if Is_Server_In_Mode (Mode) then
            Server := Get_Mode_Server (Mode);
         else
            Server := Get_Server (T);
         end if;

         if (not Shadow)
           and then
            (Dialog = Force_Dialog
              or else (Dialog = Force_Dialog_Unless_Disabled_By_Target
                        and then Get_Properties (T).Launch_Mode
                        /= Manually_With_No_Dialog)
              or else (Dialog = Default
                        and then Get_Properties (T).Launch_Mode
                        = Manually_With_Dialog))
         then
            --  Use the single target dialog to get the unexpanded command line
            Single_Target_Dialog
              (Registry        => Registry,
               Parent          => Get_Main_Window (Kernel),
               Tooltips        => Get_Tooltips (Kernel),
               Target          => Target_Name,
               History         => Get_History (Kernel),
               Expand_Cmd_Line => Expand_Cmd_Line'Unrestricted_Access,
               Result          => Command_Line);

            if Command_Line = null then
               --  The dialog was cancelled: return
               Unchecked_Free (All_Extra_Args);
               return;
            end if;

            Full := Expand_Command_Line
              (Kernel, Command_Line.all & All_Extra_Args.all,
               Server, Force_File, Main, Subdir);
            Free (Command_Line);

         else
            --  Get the unexpanded command line from the target

            declare
               CL : constant Argument_List :=
                      Get_Command_Line_Unexpanded (Registry, T);
               CL_Mode : Argument_List_Access :=
                           Apply_Mode_Args (Get_Model (T), Mode, CL);
            begin
               --  Sanity check that the command line contains at least one
               --  item (the command itself). It can happen that this is not
               --  the case if the user has modified the command by hand.

               if CL_Mode'Length = 0 then
                  Insert
                    (Kernel,
                     -"Command line is empty for target: " & Target_Name,
                     Mode => Error);
                  Free (CL_Mode);
                  Unchecked_Free (All_Extra_Args);
                  return;
               end if;

               --  Expand the command line

               if All_Extra_Args = null then
                  Full := Expand_Command_Line
                    (Kernel, CL_Mode.all, Server, Force_File, Main, Subdir);
               else
                  Full := Expand_Command_Line
                    (Kernel, CL_Mode.all & All_Extra_Args.all,
                     Server, Force_File, Main, Subdir);
               end if;

               Free (CL_Mode);
            end;
         end if;

         --  Trace the command line, for debug purposes
         if Full = null then
            Trace (Me, "Macro expansion resulted in empty command line");
            Unchecked_Free (All_Extra_Args);
            return;
         end if;

         --  Launch the build command

         Change_Dir (Dir_Name (Project_Path (Prj)).all);
         Launch_Build_Command
           (Kernel, Full, Target_Name, Mode, Server, Quiet, Shadow,
            Synchronous, Uses_Shell (T));
         Change_Dir (Old_Dir);

         Unchecked_Free (All_Extra_Args);
      end Launch_For_Mode;

   begin
      --  Get the target

      T := Get_Target_From_Name (Registry, Target_Name);

      if T = null then
         --  This should never happen
         Insert
           (Kernel,
            (-"Build target not found in registry: ") & Target_Name);
         return;
      end if;

      if Mode_Name = "" then
         declare
            Modes : Argument_List := Get_List_Of_Modes (Get_Model (T));
         begin
            for J in Modes'Range loop
               --  All modes after Modes'First are Ninja modes
               Launch_For_Mode
                 (Modes (J).all,
                  Quiet,
                  J > Modes'First);
            end loop;

            Free (Modes);
         end;
      else
         Launch_For_Mode (Mode_Name, Quiet, False);
      end if;
   end Launch_Target;

   -------------
   -- Execute --
   -------------

   overriding
   function Execute
     (Command : access Build_Command;
      Context : Interactive_Command_Context)
      return Command_Return_Type
   is
      --  ??? We should use the command context
      pragma Unreferenced (Context);
   begin
      Launch_Target (Kernel       => Command.Kernel,
                     Registry     => Command.Registry,
                     Target_Name  => To_String (Command.Target_Name),
                     Mode_Name    => "",
                     Force_File   => No_File,
                     Extra_Args   => null,
                     Quiet        => Command.Quiet,
                     Dialog       => Command.Dialog,
                     Synchronous  => False,
                     Main         => To_String (Command.Main));
      return Success;
   end Execute;

   ------------
   -- Create --
   ------------

   procedure Create
     (Item         : out Build_Command_Access;
      Kernel       : GPS.Kernel.Kernel_Handle;
      Registry     : Build_Config_Registry_Access;
      Target_Name  : String;
      Main         : String;
      Quiet        : Boolean;
      Dialog       : Dialog_Mode) is
   begin
      Item := new Build_Command;
      Item.Kernel := Kernel;
      Item.Registry := Registry;
      Item.Target_Name := To_Unbounded_String (Target_Name);
      Item.Main := To_Unbounded_String (Main);
      Item.Dialog := Dialog;
      Item.Quiet := Quiet;
   end Create;

   -------------
   -- Execute --
   -------------

   overriding
   function Execute
     (Command : access Build_Main_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Context);
      Mains  : Argument_List := Get_Mains (Command.Kernel);

   begin
      if Command.Main not in 1 .. Mains'Length then
         Insert (Command.Kernel,
                 (-"This project does not contain") & Command.Main'Img
                 & (-" main files"), Mode => Error);
         return Failure;
      end if;

      Launch_Target
        (Kernel       => Command.Kernel,
         Registry     => Command.Registry,
         Target_Name  => To_String (Command.Target_Name),
         Mode_Name    => "",
         Force_File   => No_File,
         Extra_Args   => null,
         Quiet        => Command.Quiet,
         Dialog       => Command.Dialog,
         Synchronous  => False,
         Main         => Mains (Mains'First - 1 + Command.Main).all);

      Free (Mains);

      return Success;
   end Execute;

   ------------
   -- Create --
   ------------

   procedure Create
     (Item         : out Build_Main_Command_Access;
      Kernel       : GPS.Kernel.Kernel_Handle;
      Registry     : Build_Config_Registry_Access;
      Target_Name  : String;
      Main         : Natural;
      Quiet        : Boolean;
      Dialog       : Dialog_Mode)
   is
   begin
      Item := new Build_Main_Command;
      Item.Kernel := Kernel;
      Item.Registry := Registry;
      Item.Target_Name := To_Unbounded_String (Target_Name);
      Item.Main := Main;
      Item.Dialog := Dialog;
      Item.Quiet := Quiet;
   end Create;

end Build_Command_Manager;
