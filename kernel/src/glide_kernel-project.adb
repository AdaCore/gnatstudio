-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
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

with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with GNAT.Expect;               use GNAT.Expect;

pragma Warnings (Off);
with GNAT.Expect.TTY;           use GNAT.Expect.TTY;
pragma Warnings (On);

with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.Case_Util;            use GNAT.Case_Util;

with Gtkada.Dialogs; use Gtkada.Dialogs;

with Projects;           use Projects;
with Projects.Registry;  use Projects.Registry;
with Src_Info.Prj_Utils; use Src_Info.Prj_Utils;
with String_Utils;       use String_Utils;
with Basic_Types;

with Glide_Intl;               use Glide_Intl;
with Glide_Kernel.Console;     use Glide_Kernel.Console;
with Glide_Kernel.Timeout;     use Glide_Kernel.Timeout;

package body Glide_Kernel.Project is

   procedure Compute_Predefined_Paths
     (Handle : access Kernel_Handle_Record'Class);
   --  Compute the predefined source and object paths, given the current
   --  project view associated with Handle.

   ------------------------
   -- Change_Project_Dir --
   ------------------------

   procedure Change_Project_Dir
     (Handle : access Kernel_Handle_Record'Class;
      Dir    : String)
   is
      pragma Unreferenced (Handle);
   begin
      Change_Dir (Dir);

      --  ??? We may want to use the following code to have a "dynamic"
      --  default project. But it is unclear whether this is an intuitive
      --  behavior, and this code hasn't been tested properly yet.

      --  if Get_Project_File_Name (Handle) = "" then
      --     Load_Default_Project (Handle, Dir);
      --  end if;
   end Change_Project_Dir;

   ----------------------
   -- Find_Source_File --
   ----------------------

   function Find_Source_File
     (Kernel                     : access Kernel_Handle_Record'Class;
      Short_File_Name            : String;
      Use_Predefined_Source_Path : Boolean := False)
      return String is
   begin
      if Use_Predefined_Source_Path
        and then Get_Predefined_Source_Path (Kernel) /= ""
      then
         return Find_File
           (Short_File_Name,
            Include_Path (Get_Project (Kernel), True),
            Get_Predefined_Source_Path (Kernel));
      else
         return Find_File
           (Short_File_Name, Include_Path (Get_Project (Kernel), True), "");
      end if;
   end Find_Source_File;

   ----------------------
   -- Find_Object_File --
   ----------------------

   function Find_Object_File
     (Kernel                     : access Kernel_Handle_Record'Class;
      Short_File_Name            : String;
      Use_Predefined_Object_Path : Boolean := False)
      return String is
   begin
      if Use_Predefined_Object_Path
        and then Get_Predefined_Object_Path (Kernel) /= ""
      then
         return Find_File
           (Short_File_Name,
            Object_Path (Get_Project (Kernel), True),
            Get_Predefined_Object_Path (Kernel));
      else
         return Find_File
           (Short_File_Name, Object_Path (Get_Project (Kernel), True), "");
      end if;
   end Find_Object_File;

   ------------------------------
   -- Compute_Predefined_Paths --
   ------------------------------

   procedure Compute_Predefined_Paths
     (Handle : access Kernel_Handle_Record'Class)
   is
      Source_Path : Boolean := True;

      procedure Add_Directory (S : String);
      --  Add S to the search path.
      --  If Source_Path is True, the source path is modified.
      --  Otherwise, the object path is modified.

      -------------------
      -- Add_Directory --
      -------------------

      procedure Add_Directory (S : String) is
         Tmp : GNAT.OS_Lib.String_Access;
      begin
         if S = ""
           or else S = "<Current_Directory>"
         then
            --  Do not include "." in the default source paths: when the user
            --  is compiling, it would represent the object directory, when the
            --  user is searching file it would represent whatever the current
            --  directory is at that point, ...
            return;

         elsif Source_Path then
            Tmp := Handle.Predefined_Source_Path;
            Handle.Predefined_Source_Path := new String'
              (Handle.Predefined_Source_Path.all & Path_Separator & S);

         else
            Tmp := Handle.Predefined_Object_Path;
            Handle.Predefined_Object_Path := new String'
              (Handle.Predefined_Object_Path.all & Path_Separator & S);
         end if;

         Free (Tmp);
      end Add_Directory;

      Fd          : TTY_Process_Descriptor;
      Result      : Expect_Match;
      Gnatls      : constant String := Get_Attribute_Value
        (Get_Project (Handle), Gnatlist_Attribute,
         Ide_Package, Default => "gnatls");
      Gnatls_Args : Argument_List_Access :=
        Argument_String_To_List (Gnatls & " -v");
      Path        : String_Access;

   begin
      --  If the gnatls commands hasn't changed, no need to recompute the
      --  predefined paths.

      if Handle.Gnatls_Cache /= null
        and then Handle.Gnatls_Cache.all = Gnatls
      then
         return;
      end if;

      Free (Handle.Gnatls_Cache);
      Handle.Gnatls_Cache := new String'(Gnatls);
      Free (Handle.Predefined_Source_Path);
      Free (Handle.Predefined_Object_Path);
      Basic_Types.Free (Handle.Predefined_Source_Files);
      Handle.Predefined_Source_Path := new String'("");
      Handle.Predefined_Object_Path := new String'("");

      --  ??? Should remove, when possible, the previous predefined project

      Path := Locate_Exec_On_Path (Gnatls_Args (1).all);

      if Path /= null then
         Non_Blocking_Spawn
           (Fd, Path.all,
            Gnatls_Args (2 .. Gnatls_Args'Last),
            Buffer_Size => 0, Err_To_Out => True);

         Free (Path);
         Free (Gnatls_Args);
         Expect (Fd, Result, "GNATLS .+\)", Timeout => -1);
         Free (Handle.GNAT_Version);

         declare
            S : constant String := Expect_Out_Match (Fd);
         begin
            Handle.GNAT_Version := new String'(S (S'First + 7 .. S'Last));
         end;

         Expect (Fd, Result, "Source Search Path:", Timeout => -1);

         loop
            Expect (Fd, Result, "\n", Timeout => -1);

            declare
               S : constant String :=
                 Trim (Strip_CR (Expect_Out (Fd)), Ada.Strings.Left);
            begin
               if S = "Object Search Path:" & ASCII.LF then
                  Source_Path := False;
               else
                  Add_Directory (S (S'First .. S'Last - 1));
               end if;
            end;
         end loop;
      end if;

      Free (Gnatls_Args);

   exception
      when Process_Died =>
         Close (Fd);
   end Compute_Predefined_Paths;

   --------------------------
   -- Load_Default_Project --
   --------------------------

   procedure Load_Default_Project
     (Kernel               : access Kernel_Handle_Record'Class;
      Directory            : String;
      Load_Default_Desktop : Boolean := True)
   is
      Had_Project_Desktop : Boolean;
      pragma Unreferenced (Had_Project_Desktop);
   begin
      --  Save all open children, and close everything. A new desktop will be
      --  open in the end anway

      if not Save_All_MDI_Children (Kernel, Force => False) then
         return;
      end if;

      Load_Default_Project
        (Kernel.Registry.all,
         Normalize_Pathname (Directory, Resolve_Links => False));
      Project_Changed (Kernel);
      Recompute_View (Kernel);

      --  Reload the default desktop

      if Load_Default_Desktop then
         Close_All_Children (Kernel);
         Had_Project_Desktop := Load_Desktop (Kernel);
      end if;
   end Load_Default_Project;

   ------------------
   -- Load_Project --
   ------------------

   procedure Load_Project
     (Kernel : access Kernel_Handle_Record'class; Project : String)
   is
      procedure Report_Error (S : String);
      --  Output error messages from the project parser to the glide console.

      procedure Report_Error (S : String) is
      begin
         Console.Insert (Kernel, S, Mode => Console.Error, Add_LF => False);
      end Report_Error;

      Had_Project_Desktop : Boolean;
      New_Project_Loaded : Boolean;
      pragma Unreferenced (Had_Project_Desktop);

   begin
      if not Save_All_MDI_Children (Kernel, Force => False) then
         return;
      end if;

      Load (Registry           => Kernel.Registry.all,
            Root_Project_Path  => Project,
            Errors             => Report_Error'Unrestricted_Access,
            New_Project_Loaded => New_Project_Loaded);

      if not New_Project_Loaded then
         return;
      end if;

      Project_Changed (Kernel);
      Recompute_View (Kernel);

      --  Reload the desktop, in case there is a project-specific setup already
      Close_All_Children (Kernel);
      Had_Project_Desktop := Load_Desktop (Kernel);
   end Load_Project;

   -----------------
   -- Get_Project --
   -----------------

   function Get_Project (Handle : access Kernel_Handle_Record'Class)
      return Project_Type is
   begin
      return Get_Root_Project (Handle.Registry.all);
   end Get_Project;

   --------------------
   -- Recompute_View --
   --------------------

   procedure Recompute_View (Handle : access Kernel_Handle_Record'Class) is
      procedure Report_Error (S : String);
      --  Handler called when the project parser finds an error.

      procedure Report_Error (S : String) is
      begin
         Console.Insert (Handle, S, Mode => Console.Error);
      end Report_Error;

   begin
      Recompute_View (Handle.Registry.all, Report_Error'Unrestricted_Access);
      Compute_Predefined_Paths (Handle);
      Project_View_Changed (Handle);
   end Recompute_View;

   ---------------------------------
   -- Scenario_Variables_Cmd_Line --
   ---------------------------------

   function Scenario_Variables_Cmd_Line
     (Handle : access Kernel_Handle_Record'Class;
      Syntax : Command_Syntax) return String
   is
      Scenario_Vars : constant Scenario_Variable_Array :=
        Scenario_Variables (Handle);

      function Concat
        (Current : String; Index : Natural; Set_Var : String) return String;
      --  Concat the command line line for the Index-nth variable and the
      --  following ones to Current, and return the result.

      function Concat
        (Current : String; Index : Natural; Set_Var : String) return String is
      begin
         if Index > Scenario_Vars'Last then
            return Current;
         end if;

         return Concat
           (Current
            & Set_Var & External_Reference_Of (Scenario_Vars (Index))
            & "=" & Value_Of (Scenario_Vars (Index)) & " ",
            Index + 1,
            Set_Var);
      end Concat;

   begin
      --  A recursive function is probably not the most efficient way, but this
      --  prevents limits on the command line lengths. This also avoids the use
      --  of unbounded strings.

      case Syntax is
         when GNAT_Syntax =>
            return Concat ("", Scenario_Vars'First, "-X");
         when Make_Syntax =>
            return Concat ("", Scenario_Vars'First, "");
      end case;
   end Scenario_Variables_Cmd_Line;

   ------------------------
   -- Scenario_Variables --
   ------------------------

   function Scenario_Variables (Kernel : access Kernel_Handle_Record'Class)
      return Scenario_Variable_Array is
   begin
      return Scenario_Variables (Kernel.Registry.all);
   end Scenario_Variables;

   ------------------
   -- Save_Project --
   ------------------

   procedure Save_Project
     (Kernel    : access Kernel_Handle_Record'Class;
      Project   : Project_Type;
      Recursive : Boolean := False)
   is
      Iter : Imported_Project_Iterator := Start (Project, Recursive);
   begin
      while Current (Iter) /= No_Project loop
         declare
            Langs : Argument_List := Get_Languages (Current (Iter));
         begin
            Save_Single_Project (Kernel, Current (Iter), Langs);
            Basic_Types.Free (Langs);
         end;

         Next (Iter);
      end loop;

      --  Force a change in the icons in the explorer.
      --  ??? Probably not very efficient, however.

      Project_View_Changed (Kernel);
   end Save_Project;

   -------------------------
   -- Save_Single_Project --
   -------------------------

   procedure Save_Single_Project
     (Kernel    : access Kernel_Handle_Record'Class;
      Project   : Project_Type;
      Langs     : GNAT.OS_Lib.Argument_List)
   is
      procedure Report_Error (Msg : String);
      --  Report errors to the user

      ------------------
      -- Report_Error --
      ------------------

      procedure Report_Error (Msg : String) is
      begin
         Insert (Kernel, Msg, Mode => Glide_Kernel.Console.Error);
      end Report_Error;

      Args    : Argument_List (1 .. 2);
      Success : Boolean;
   begin
      --  A multi-language project ? If yes, we need to generate the Makefile

      if Langs'Length > 1 then
         To_Lower (Langs (Langs'First).all);
      end if;

      if Langs'Length > 1
        or else Langs (Langs'First).all /= "ada"
      then
         if Project_Modified (Project) then
            Save_Project (Project, Report_Error'Unrestricted_Access);
            Args (1) := new String'("-R");

            declare
               Name : constant String := Project_Path (Project);
            begin
               if not Is_Regular_File (Name)
                 or else Is_Writable_File (Name)
               then
                  --  call gpr2make -R Name

                  Free (Args (2));
                  Args (2) := new String'(Name);
                  Launch_Process
                    (Kernel_Handle (Kernel), "gpr2make",
                     Args, "", null, null, "", Success);
               end if;
            end;

            Basic_Types.Free (Args);
         end if;

      else
         Save_Project (Project, Report_Error'Unrestricted_Access);
      end if;
   end Save_Single_Project;

   ------------------------------
   -- Save_Project_Conditional --
   ------------------------------

   function Save_Project_Conditional
     (Kernel : access Kernel_Handle_Record'Class;
      Force  : Boolean) return Save_Return_Value
   is
      Button : Message_Dialog_Buttons;
   begin
      if Force then
         Save_Project (Kernel, Get_Project (Kernel), Recursive => True);

      elsif Project_Modified (Get_Project (Kernel), Recursive => True) then
         Button := Message_Dialog
           (Msg            => -"Do you want to save the projects ?",
            Dialog_Type    => Confirmation,
            Buttons        =>
              Button_Yes or Button_All or Button_No or Button_Cancel,
            Default_Button => Button_Cancel,
            Parent         => Get_Main_Window (Kernel));

         case Button is
            when Button_Yes =>
               Save_Project (Kernel, Get_Project (Kernel), Recursive => True);
               return Saved;

            when Button_No =>
               return Not_Saved;

            when Button_All =>
               Save_Project
                 (Kernel    => Kernel,
                  Project   => Get_Project (Kernel),
                  Recursive => True);
               return Save_All;

            when others =>
               return Cancel;
         end case;
      end if;
      return Saved;
   end Save_Project_Conditional;

   ------------------
   -- Get_Registry --
   ------------------

   function Get_Registry
     (Handle : access Kernel_Handle_Record'Class)
      return Projects.Registry.Project_Registry'Class is
   begin
      return Handle.Registry.all;
   end Get_Registry;

end Glide_Kernel.Project;
