------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2012, AdaCore                          --
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

with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;

with Generic_Stack;

with GNAT.Directory_Operations;
with GNAT.Regpat;               use GNAT.Regpat;

with GNATCOLL.Templates;          use GNATCOLL.Templates;
with GNATCOLL.Utils;              use GNATCOLL.Utils;

with GPS.Intl;                    use GPS.Intl;
with GPS.Kernel.Shared_Macros; use GPS.Kernel.Shared_Macros;
with Traces;                      use Traces;
with GNAT.Strings;

package body Build_Command_Utils is

   Me : constant Debug_Handle := Create ("Build_Command_Manager");

   Max_Number_Of_Mains : constant := 128;
   --  The maximum number of Mains that we accept to display in the Menus
   --  and toolbar.

   package Projects_Stack is new Generic_Stack (Project_Type);

   function Is_Server_In_Mode
     (Registry   : Build_Config_Registry_Access;
      Mode : String) return Boolean;

   function Get_Mode_Server
     (Registry   : Build_Config_Registry_Access;
      Mode : String) return Remote.Server_Type;

   function Scenario_Variables_Cmd_Line
     (Adapter : Abstract_Build_Command_Adapter'Class;
      Prefix : String) return String;

   ---------------------
   -- Apply_Mode_Args --
   ---------------------

   function Apply_Mode_Args
     (Registry : Build_Config_Registry_Access;
      Model : String;
      Mode : String;
      Cmd_Line : GNAT.OS_Lib.Argument_List)
      return GNAT.OS_Lib.Argument_List_Access
   is
      use Model_List;
      M         : Mode_Record;
      Model_Rec : Model_Record;
      C         : Model_List.Cursor;
      Res       : GNAT.OS_Lib.Argument_List_Access;
      Supported : Boolean;

      function Compute_Num_Args
        (Args : GNAT.OS_Lib.Argument_List; Filter : String) return Natural;
      --  Compute number of relevant arguments in Args that match Filter

      function Compute_Num_Args
        (Args : GNAT.OS_Lib.Argument_List; Filter : String) return Natural
      is
         Result  : Natural := 0;
      begin
         if Filter = "" then
            return Args'Length;
         else
            for J in Args'Range loop
               if Match (Filter, Args (J).all) then
                  Result := Result + 1;
               end if;
            end loop;

            return Result;
         end if;
      end Compute_Num_Args;

   begin
      Supported := True;

      if Model = "" then
         Supported := False;
      end if;

      if Mode = "" then
         Supported := False;
      end if;

      if Supported then
         M := Element_Mode
           (Registry, To_Unbounded_String (Mode));

         if (M.Args = null
             or else M.Args'Length = 0)
           and then
             (M.Subst_Src = null
              or else M.Subst_Src'Length = 0)
         then
            Supported := False;
         end if;
      end if;

      if Supported and then not M.Models.Is_Empty then
         C := M.Models.First;

         Supported := False;
         while Has_Element (C) loop
            Model_Rec := Element (C);

            if Model_Rec.Model = Model then
               Supported := True;
               exit;
            end if;

            Next (C);
         end loop;
      end if;

      --  We finished the check to see if the Mode should be active
      --  If unsupported, return a copy of the initial command line.
      if not Supported then
         Res := new GNAT.OS_Lib.Argument_List (Cmd_Line'Range);

         for J in Cmd_Line'Range loop
            Res (J) := new String'(Cmd_Line (J).all);
         end loop;

         return Res;
      end if;

      --  Now let's apply the Mode. First we create the result with enough
      --  room.
      if M.Args /= null then
         Res := new GNAT.OS_Lib.Argument_List
           (1 .. Cmd_Line'Length
                  + Compute_Num_Args
                      (M.Args.all, To_String (Model_Rec.Filter)));
      else
         Res := new GNAT.OS_Lib.Argument_List (1 .. Cmd_Line'Length);
      end if;

      --  Let's apply substitutions if needed
      if M.Subst_Src /= null then
         for J in 1 .. Cmd_Line'Length loop
            declare
               Found : Boolean := False;
            begin
               for K in M.Subst_Src'Range loop
                  if Cmd_Line (Cmd_Line'First + J - 1).all =
                    M.Subst_Src (K).all
                  then
                     Res (J) := new String'(M.Subst_Dest (K).all);
                     Found := True;
                     exit;
                  end if;
               end loop;

               if not Found then
                  Res (J) :=
                    new String'(Cmd_Line (Cmd_Line'First + J - 1).all);
               end if;
            end;
         end loop;

      else
         --  Simple copy of the initial command line
         for J in 1 .. Cmd_Line'Length loop
            Res (J) :=
              new String'(Cmd_Line (Cmd_Line'First + J - 1).all);
         end loop;
      end if;

      if Length (Model_Rec.Filter) = 0 then
         --  Append the extra args
         for J in 1 .. Res'Last - Cmd_Line'Length loop
            Res (J + Cmd_Line'Length) :=
              new String'(M.Args (M.Args'First + J - 1).all);
         end loop;
      else
         declare
            Filter : constant String := To_String (Model_Rec.Filter);
            Index  : Natural := Cmd_Line'Length + 1;
         begin
            for J in M.Args'Range loop
               if Match (Filter, M.Args (J).all) then
                  Res (Index) := new String'(M.Args (J).all);
                  Index := Index + 1;
               end if;
            end loop;
         end;
      end if;

      return Res;
   end Apply_Mode_Args;

   ---------------
   -- Get_Mains --
   ---------------

   function Get_Mains (Registry : Project_Registry_Access)
      return GNATCOLL.VFS.File_Array
   is
      Root_Project : constant Project_Type := Registry.Tree.Root_Project;

      Result       : File_Array (1 .. Max_Number_Of_Mains);
      Index        : Natural := Result'First;
      Projects     : Projects_Stack.Simple_Stack;
      The_Project  : Project_Type;
      Iterator     : Project_Iterator :=
                       Root_Project.Start (Include_Extended => True);
      Mains        : String_List_Access;

      function To_Full_Path (Basename : String) return Virtual_File;
      --  Return the full path of file Basename in project Project

      function Is_Already_In_Mains (F : Virtual_File) return Boolean;
      --  Return True if S is in Result (for instance a project could use its
      --  extended project's Main attribute, in which case we would end up
      --  with duplicates).

      -------------------------
      -- Is_Already_In_Mains --
      -------------------------

      function Is_Already_In_Mains (F : Virtual_File) return Boolean is
      begin
         for J in Result'First .. Index - 1 loop
            if Result (J) = F then
               return True;
            end if;
         end loop;
         return False;
      end Is_Already_In_Mains;

      ------------------
      -- To_Full_Path --
      ------------------

      function To_Full_Path (Basename : String) return Virtual_File is
         File : Virtual_File;
      begin
         if GNAT.Directory_Operations.File_Extension (Basename) = "" then
            --  The project files used to support the form
            --     for Main use ("basename");
            --  If this is the case here, add ".adb" to get the real name of
            --  the source unit.
            File := Registry.Tree.Create
              (Filesystem_String (Basename & ".adb"),
               Use_Object_Path => False);
         else
            File := Registry.Tree.Create
              (Filesystem_String (Basename),
               Use_Object_Path => False);
         end if;

         if File = GNATCOLL.VFS.No_File then
            return Create_From_Base (+Basename);
         end if;

         return File;
      end To_Full_Path;

   begin
      --  The project Iterator starts with the leaf projects and ends with
      --  the root project. Reverse the order to be more user-friendly: in
      --  the majority of cases, users will want to see the mains defined
      --  in the root project first.

      while Current (Iterator) /= No_Project loop
         Projects_Stack.Push (Projects, Current (Iterator));
         Next (Iterator);
      end loop;

      while not Projects_Stack.Is_Empty (Projects)
        and Index <= Result'Last
      loop
         Projects_Stack.Pop (Projects, The_Project);
         Mains := The_Project.Attribute_Value (Main_Attribute);
         if Mains /= null then
            for J in Mains'Range loop
               if Mains (J)'Length > 0 then
                  Result (Index) := To_Full_Path (Mains (J).all);

                  if not Is_Already_In_Mains (Result (Index)) then
                     Index := Index + 1;
                     exit when Index > Result'Last;
                  else
                     Result (Index) := GNATCOLL.VFS.No_File;
                  end if;
               end if;
            end loop;

            Free (Mains);
         end if;
      end loop;

      Projects_Stack.Clear (Projects);

      return Result (1 .. Index - 1);
   end Get_Mains;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Adapter     : in out Abstract_Build_Command_Adapter'Class;
      Kernel_Registry : Project_Registry_Access;
      Context_Project : Project_Type;
      Context_Toolchains_Manager : Toolchain_Manager;
      Context_File_Information : Virtual_File;
      Kernel_Macros_Special_Character : Character;
      Trusted_Mode_Preference : Boolean;
      Execute_Command_Preference : String;
      Multi_Language_Builder : Multi_Language_Builder_Policy) is
   begin
      Adapter.Kernel_Registry := Kernel_Registry;
      Adapter.Context_Project := Context_Project;
      Adapter.Context_Toolchains_Manager := Context_Toolchains_Manager;
      Adapter.Context_File_Information := Context_File_Information;
      Adapter.Kernel_Macros_Special_Character :=
         Kernel_Macros_Special_Character;
      Adapter.Trusted_Mode_Preference := Trusted_Mode_Preference;
      Adapter.Execute_Command_Preference :=
         To_Unbounded_String (Execute_Command_Preference);
      Adapter.Multi_Language_Builder := Multi_Language_Builder;

   end Initialize;

   -------------------------
   -- Get_Kernel_Registry --
   -------------------------

   function Get_Kernel_Registry
     (Adapter : Abstract_Build_Command_Adapter)
        return Project_Registry_Access is
   begin
      return Adapter.Kernel_Registry;
   end Get_Kernel_Registry;

   -------------------------
   -- Get_Context_Project --
   -------------------------

   function Get_Context_Project
     (Adapter : Abstract_Build_Command_Adapter) return Project_Type is
   begin
      return Adapter.Context_Project;
   end Get_Context_Project;

   ------------------------------------
   -- Get_Context_Toolchains_Manager --
   ------------------------------------

   function Get_Context_Toolchains_Manager
     (Adapter : Abstract_Build_Command_Adapter)
      return Toolchain_Manager is
   begin
      return Adapter.Context_Toolchains_Manager;
   end Get_Context_Toolchains_Manager;

   ----------------------------------
   -- Get_Context_File_Information --
   ----------------------------------

   function Get_Context_File_Information
     (Adapter : Abstract_Build_Command_Adapter) return Virtual_File is
   begin
      return Adapter.Context_File_Information;
   end Get_Context_File_Information;

   -----------------------------------------
   -- Get_Kernel_Macros_Special_Character --
   -----------------------------------------

   function Get_Kernel_Macros_Special_Character
     (Adapter : Abstract_Build_Command_Adapter)
      return Character is
   begin
      return Adapter.Kernel_Macros_Special_Character;
   end Get_Kernel_Macros_Special_Character;

   ---------------------------------
   -- Get_Trusted_Mode_Preference --
   ---------------------------------

   function Get_Trusted_Mode_Preference
     (Adapter :  Abstract_Build_Command_Adapter) return Boolean is
   begin
      return Adapter.Trusted_Mode_Preference;
   end Get_Trusted_Mode_Preference;

   ------------------------------------
   -- Get_Execute_Command_Preference --
   ------------------------------------

   function Get_Execute_Command_Preference
     (Adapter :  Abstract_Build_Command_Adapter) return String is
   begin
      return To_String (Adapter.Execute_Command_Preference);
   end Get_Execute_Command_Preference;

   --------------------------------
   -- Get_Multi_Language_Builder --
   --------------------------------

   function Get_Multi_Language_Builder
     (Adapter :  Abstract_Build_Command_Adapter) return
        Multi_Language_Builder_Policy is
   begin
      return Adapter.Multi_Language_Builder;
   end Get_Multi_Language_Builder;

   type Build_Command_Adapter is new Abstract_Build_Command_Adapter with record
      Last_Main_For_Background : Virtual_File := No_File;
      Project_File             : Virtual_File := No_File;
      Built_File               : Virtual_File := No_File;
      Status                   : Unbounded_String := To_Unbounded_String ("");
   end record;
   type Build_Command_Adapter_Access is access all Build_Command_Adapter;

   overriding
   function Get_Last_Main_For_Background_Target
     (Adapter : Build_Command_Adapter;
      Target : Target_Access) return Virtual_File;
   --  Return the Main to use for building Target as a background build.
   --  This is either the last main that was used, if it exists, or the first
   --  main defined for this target, if it exists.
   --  The full path to the target is returned.
   --  If the target is not found, "" is returned.

   overriding
   function Get_Background_Project_Full_Name
     (Adapter : Build_Command_Adapter) return Filesystem_String;

   overriding
   function Substitute
     (Adapter : Build_Command_Adapter;
      Param     : String;
      Quoted    : Boolean;
      Done      : access Boolean;
      Server    : Server_Type := GPS_Server;
      For_Shell : Boolean := False) return String;
   --  Wrapper around GPS.Kernel.Macros.Substitute

   overriding
   procedure Console_Insert
     (Adapter : in out Build_Command_Adapter;
      Text   : String;
      Add_LF : Boolean := True;
      Mode   : Console_Message_Type := Info);

   overriding
   procedure Remove_Error_Builder_Message_From_File
     (Adapter : Build_Command_Adapter;
      File     : Virtual_File);
   --  Removes all messages for specified file in the error category.
   --  Do nothing when there is no such category or file.

   overriding
   function Get_Background_Environment_File
     (Adapter : Build_Command_Adapter) return Virtual_File;

   overriding
   function Get_Scenario_Variables
     (Adapter : Build_Command_Adapter) return Scenario_Variable_Array;

   Invalid_Argument : exception;
   --  Raised by Expand_Arg below

   function Expand_Arg
     (Adapter     : Abstract_Build_Command_Adapter_Access;
      Target     : Target_Access;
      Arg        : String;
      Server     : Server_Type;
      Force_File : Virtual_File;
      Main       : Virtual_File;
      Subdir     : Filesystem_String;
      Background : Boolean;
      Simulate   : Boolean) return Expansion_Result;
   --  Expand macros contained in Arg.
   --  Caller must free the result.
   --  Will raise Invalid_Argument if an invalid/non existent argument is
   --  found.
   --  If Simulate is true, Invalid_Argument will never be raised, and no
   --  expansion will be done.

   procedure Free_Adapter is new Ada.Unchecked_Deallocation
     (Build_Command_Adapter, Build_Command_Adapter_Access);

   procedure Free (Ar : in out Argument_List);
   --  Free memory associated to Ar

   ----------
   -- Free --
   ----------

   procedure Free (Ar : in out Argument_List) is
   begin
      for A in Ar'Range loop
         Free (Ar (A));
      end loop;
   end Free;

   ---------------------------------
   -- Scenario_Variables_Cmd_Line --
   ---------------------------------

   function Scenario_Variables_Cmd_Line
     (Adapter : Abstract_Build_Command_Adapter'Class;
      Prefix : String) return String is
      Scenario_Vars : constant Scenario_Variable_Array :=
         Get_Scenario_Variables (Adapter);

      function Concat
        (Current : String; Index : Natural; Set_Var : String) return String;
      --  Concat the command line line for the Index-nth variable and the
      --  following ones to Current, and return the result.

      ------------
      -- Concat --
      ------------

      function Concat
        (Current : String; Index : Natural; Set_Var : String) return String is
      begin
         if Index > Scenario_Vars'Last then
            return Current;
         end if;

         return Concat
           (Current
            & Set_Var & External_Name (Scenario_Vars (Index))
            & "=" & Value (Scenario_Vars (Index))
            & " ",
            Index + 1,
            Set_Var);
      end Concat;

   begin
      --  A recursive function is probably not the most efficient way, but this
      --  prevents limits on the command line lengths. This also avoids the use
      --  of unbounded strings.
      return Concat ("", Scenario_Vars'First, Prefix);
   end Scenario_Variables_Cmd_Line;

   ----------------
   -- Expand_Arg --
   ----------------

   function Expand_Arg
     (Adapter    : Abstract_Build_Command_Adapter_Access;
      Target     : Target_Access;
      Arg        : String;
      Server     : Server_Type;
      Force_File : Virtual_File;
      Main       : Virtual_File;
      Subdir     : Filesystem_String;
      Background : Boolean;
      Simulate   : Boolean) return Expansion_Result
   is
      Result : Expansion_Result;

      function Substitution
        (Param  : String; Quoted : Boolean) return String;
      --  Wrapper around GPS.Kernel.Macros.Substitute

      function Get_Attr_Value (Arg : String; Skip : Natural) return String;
      --  return the name of the attribute contained in Arg

      function Get_Index (A, B : Natural) return Natural;
      --  Return A if A /= 0, B otherwise

      function Multi_Language_Build (Prj : Project_Type) return Boolean;
      --  Return True if the build is multi-language, False if build is Ada
      --  only.

      function Create_Command
        (Command : String; Tc : Toolchains.Toolchain) return Arg_List;
      --  Create an Arg_List containing the given Command, and possibly
      --  appended with --target=xxx if Tc is a cross toolchain.

      ---------------
      -- Get_Index --
      ---------------

      function Get_Index (A, B : Natural) return Natural is
      begin
         if A = 0 then
            return B;
         else
            return A;
         end if;
      end Get_Index;

      ------------------
      -- Substitution --
      ------------------

      function Substitution
        (Param : String; Quoted : Boolean) return String
      is
         Done : aliased Boolean := False;
      begin
         if Param = "subdir" then
            return +Subdir;

         elsif Background
           and then not Simulate
           and then (Param = "pp" or else Param = "PP")
         then
            return +Get_Background_Project_Full_Name (Adapter.all);

         else
            declare
               Result : constant String := Substitute (Adapter.all, Param,
                  Quoted, Done'Access, Server => Server);
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

      --------------------
      -- Get_Attr_Value --
      --------------------

      function Get_Attr_Value (Arg : String; Skip : Natural) return String is
         J    : constant Natural := Get_Index
           (Ada.Strings.Fixed.Index (Arg, "'"), Arg'First + Skip);
         K    : constant Natural := Get_Index
           (Ada.Strings.Fixed.Index (Arg (J .. Arg'Last), ","), Arg'Last);
         Pkg  : constant String := Arg (Arg'First + Skip + 1 .. J - 1);
         Attr : constant String := Arg (J + 1 .. K - 1);
      begin
         return Get_Context_Project (Adapter.all).Attribute_Value
           (Build (Pkg, Attr), Default => Arg (K + 1 .. Arg'Last - 1));
      end Get_Attr_Value;

      --------------------------
      -- Multi_Language_Build --
      --------------------------

      function Multi_Language_Build (Prj : Project_Type) return Boolean is
         Langs  : Argument_List := Prj.Languages (Recursive => True);
         Result : Boolean;
      begin
         Result := Get_Multi_Language_Builder (Adapter.all) = Gprbuild
           or else (Get_Multi_Language_Builder (Adapter.all) = Auto
                    and then not (Langs'Length = 1
                                  and then Langs (Langs'First).all = "ada"));
         Free (Langs);
         return Result;
      end Multi_Language_Build;

      --------------------
      -- Create_Command --
      --------------------

      function Create_Command
        (Command : String; Tc : Toolchains.Toolchain) return Arg_List
      is
         Result : Arg_List;
      begin
         Result := Create (Command);

         if not Is_Native (Tc) then
            Append_Argument (Result, "--target=" & Get_Name (Tc), One_Arg);
         end if;

         return Result;
      end Create_Command;

   begin
      --  ??? Special case for "%X"
      --  We are implementing a special case here since GPS.Kernel.Macros
      --  does not support returning an Argument_List.
      --  See H926-007.

      if Arg = "%X" then
         Result.Args := Parse_String
           (Scenario_Variables_Cmd_Line (Adapter.all, "-X"), Separate_Args);

      --  ??? Ditto for %vars
      elsif Arg = "%vars" then
         Result.Args := Parse_String
           (Scenario_Variables_Cmd_Line (Adapter.all, ""), Separate_Args);

      --  ??? Would be nice to support a generic %vars(xxx)
      elsif Arg = "%vars(-D)" then
         Result.Args := Parse_String
           (Scenario_Variables_Cmd_Line (Adapter.all, "-D"), Separate_Args);

      --  ??? Ditto for %eL
      elsif Arg = "%eL" then
         if Get_Trusted_Mode_Preference (Adapter.all) then
            return Result;
         else
            Result.Args := Create ("-eL");
         end if;

      --  ??? Ditto for %attr
      elsif Starts_With (Arg, "%attr(") and then Arg (Arg'Last) = ')' then
         Result.Args := Parse_String (Get_Attr_Value (Arg, 5), Separate_Args);

      elsif Starts_With (Arg, "%dirattr(") and then Arg (Arg'Last) = ')' then
         Result.Args := Parse_String (Get_Attr_Value (Arg, 8), Separate_Args);
         Set_Nth_Arg
           (Result.Args, 0,
            GNAT.Directory_Operations.Dir_Name (Nth_Arg (Result.Args, 0)));

      elsif Starts_With (Arg, "%baseattr(") and then Arg (Arg'Last) = ')' then
         Result.Args := Parse_String (Get_Attr_Value (Arg, 9), Separate_Args);
         Set_Nth_Arg
           (Result.Args, 0,
            GNAT.Directory_Operations.Base_Name (Nth_Arg (Result.Args, 0)));

      --  ??? Ditto for %switches
      elsif Starts_With (Arg, "%switches(") and then Arg (Arg'Last) = ')' then
         declare
            List : GNAT.Strings.String_List_Access :=
                    Get_Context_Project (Adapter.all).Attribute_Value
                      (Build ("IDE", "Default_Switches"),
                       Index => Arg (Arg'First + 10 .. Arg'Last - 1));
         begin
            if List /= null and then List'Length /= 0 then
               Result.Args := Create (List (List'First).all);
               for J in List'First + 1 .. List'Last loop
                  Append_Argument (Result.Args, List (J).all, One_Arg);
               end loop;
            end if;

            Free (List);
         end;

      --  ??? Ditto for %builder, %gprbuild and %gprclean
      elsif Arg = "%builder"
        or else Arg = "%gprbuild"
        or else Arg = "%gprclean"
      then
         declare
            Prj   : constant Project_Type := Get_Context_Project (Adapter.all);
            Tc    : constant Toolchains.Toolchain :=
                      Get_Toolchain
                        (Get_Context_Toolchains_Manager (Adapter.all), Prj);
            Res   : Expansion_Result;
            Clean : constant Boolean := Arg = "%gprclean";

         begin
            if Multi_Language_Build (Prj) then
               if Clean then
                  Res.Args := Create_Command ("gprclean", Tc);
               else
                  Res.Args := Create_Command ("gprbuild", Tc);
               end if;
            else
               if Clean then
                  Res.Args := Create
                    (Prj.Attribute_Value
                       (GNAT_Attribute, Default => "gnat"));
                  Append_Argument (Res.Args, "clean", One_Arg);
               else
                  --  Compiler ("Ada") is the gnatmake command
                  Res.Args := Create (Get_Exe (Get_Compiler (Tc, "Ada")));
               end if;
            end if;

            return Res;
         end;

      elsif Arg = "%external" then
         Result.Args := Parse_String
           (Get_Execute_Command_Preference (Adapter.all), Separate_Args);

      elsif Arg = "[exec_dir]" then
         declare
            Prj : constant Project_Type :=
              Get_Context_Project (Adapter.all);
         begin
            Result.Dir := Executables_Directory (Prj);
         end;

      elsif Arg = "%fp" then
         if not Simulate
           and then Force_File /= No_File
         then
            --  We are launching a compile command involving Force_File:
            --  remove reference to File from the Locations View.
            --  See F830-003.
            Remove_Error_Builder_Message_From_File
               (Adapter.all, Force_File);
            Result.Args := Create (+Base_Name (Force_File));
            return Result;
         end if;

         declare
            File : constant Virtual_File :=
               Get_Context_File_Information (Adapter.all);
         begin
            if File = No_File then
               if Simulate then
                  Result.Args := Create ("<current-file>");
                  return Result;

               else
                  Console_Insert
                    (Adapter.all, -"No file selected", Mode => Error);
                  raise Invalid_Argument;
               end if;

            elsif Get_Kernel_Registry (Adapter.all).Tree.Info (File).Project =
               No_Project
            then
               if Simulate then
                  Result.Args := Create ("<current-file>");
                  return Result;

               else
                  Console_Insert
                    (Adapter.all, -"Could not determine the project for file: "
                     & Display_Full_Name (File),
                     Mode => Error);

                  --  Do not normalize through VFS so as to preserve the state
                  --  of the file (since otherwise we would cache the
                  --  normalized value)
                  if File.Display_Full_Name /=
                    Normalize_Pathname
                      (File.Display_Full_Name, Resolve_Links => True)
                    and then Get_Trusted_Mode_Preference (Adapter.all)
                  then
                     Console_Insert
                       (Adapter.all, -("You should"
                        & " disable the preference Fast Project Loading for"
                        & " full support of symbolic links"));
                  end if;

                  raise Invalid_Argument;
               end if;

            else
               if Background then
                  Result.Args := Create
                    (+Base_Name (Get_Background_Environment_File
                       (Adapter.all)));
               else
                  --  We are launching a compile command involving File:
                  --  remove reference to File from the Locations View.
                  --  See F830-003.
                  if not Simulate then
                     Remove_Error_Builder_Message_From_File
                       (Adapter.all, File);
                  end if;

                  Result.Args := Create (+Base_Name (File));
               end if;
            end if;
         end;

      elsif Starts_With (Arg, "%TT") then
         if Main /= No_File then
            Result.Args :=
              Create (+Main.To_Remote (Get_Nickname (Server)).Full_Name &
                      Arg (Arg'First + 3 .. Arg'Last));
         else
            if Background then
               declare
                  M : constant Virtual_File :=
                     Get_Last_Main_For_Background_Target (Adapter.all, Target);
               begin
                  if M = No_File then
                     Console_Insert
                       (Adapter.all,
                        (-"Could not launch background build: no main(s)"
                         & " found for target ") & Get_Name (Target),
                        Mode => Error);
                     raise Invalid_Argument;
                  else
                     Result.Args := Create
                       (+M.To_Remote (Get_Nickname (Server)).Full_Name &
                        Arg (Arg'First + 3 .. Arg'Last));
                  end if;
               end;
            else
               Console_Insert
                 (Adapter.all, -"Could not determine the target to build.",
                  Mode => Error);
               raise Invalid_Argument;
            end if;
         end if;

      elsif Starts_With (Arg, "%T") then
         if Main /= No_File then
            Result.Args := Create
              (+Main.Base_Name
               & Arg (Arg'First + 2 .. Arg'Last));
         else
            if Background then
               declare
                  M : constant Virtual_File :=
                     Get_Last_Main_For_Background_Target (Adapter.all, Target);
               begin
                  if M = No_File then
                     Console_Insert
                       (Adapter.all,
                        (-"Could not launch background build: no main(s)"
                         & " found for target ") & Get_Name (Target),
                        Mode => Error);
                     raise Invalid_Argument;
                  else
                     Result.Args := Create
                       (+M.Base_Name &
                        Arg (Arg'First + 3 .. Arg'Last));
                  end if;
               end;
            else
               Console_Insert
                 (Adapter.all, -"Could not determine the target to build.",
                  Mode => Error);
               raise Invalid_Argument;
            end if;
         end if;

      elsif Starts_With (Arg, "%E") then
         if Main /= No_File then
            Result.Args := Create
              (+To_Remote (Main, Get_Nickname (Server)).Full_Name);
         else
            Console_Insert
              (Adapter.all,
               -"Could not determine the executable name for main.",
               Mode => Error);
            raise Invalid_Argument;
         end if;

      else
         Result.Args :=
           Create (GNATCOLL.Templates.Substitute
                   (Str       => Arg,
                    Delimiter => Get_Kernel_Macros_Special_Character
                       (Adapter.all),
                    Callback  => Substitution'Unrestricted_Access));
      end if;

      return Result;
   end Expand_Arg;

   -------------------------
   -- Expand_Command_Line --
   -------------------------

   function Expand_Command_Line
     (Adapter     : Abstract_Build_Command_Adapter_Access;
      CL         : Argument_List;
      Target     : Target_Access;
      Server     : Server_Type;
      Force_File : Virtual_File;
      Main       : Virtual_File;
      Subdir     : Filesystem_String;
      Background : Boolean;
      Simulate   : Boolean) return Expansion_Result
   is
      Result  : Expansion_Result;
      Final   : Expansion_Result;
      Failed  : Boolean := False;

   begin
      for J in CL'Range loop
         if CL (J) = null then
            --  This should not happen
            Console_Insert (Adapter.all, (-"Invalid command line"),
               Mode => Error);
            return (Empty_Command_Line, No_File, To_Unbounded_String (""));
         end if;

         declare
            Arg : constant String := CL (J).all;
         begin
            Result := Expand_Arg
              (Adapter, Target, CL (J).all, Server,
               Force_File, Main, Subdir, Background, Simulate);
         exception
            when Invalid_Argument =>
               Console_Insert
                 (Adapter.all,
                  (-"Could not expand argument in command line: ") & Arg,
                  Mode => Error);
               Failed := True;
         end;

         if Result.Dir /= No_File then
            Final.Dir := Result.Dir;
         end if;

         for J in 0 .. Args_Length (Result.Args) loop
            Append_Argument (Final.Args, Nth_Arg (Result.Args, J), One_Arg);
         end loop;
      end loop;

      if Failed then
         Console_Insert
           (Adapter.all, (-"Build command not launched."),
            Mode => Error);
         return (Empty_Command_Line, No_File, To_Unbounded_String (""));
      end if;

      return Final;
   end Expand_Command_Line;

   -----------------------------------------
   -- Get_Last_Main_For_Background_Target --
   -----------------------------------------

   overriding
   function Get_Last_Main_For_Background_Target
     (Adapter : Build_Command_Adapter;
      Target : Target_Access) return Virtual_File is
      pragma Unreferenced (Adapter);
      pragma Unreferenced (Target);
   begin
      return No_File;
   end Get_Last_Main_For_Background_Target;

   --------------------------------------
   -- Get_Background_Project_Full_Name --
   --------------------------------------

   overriding
   function Get_Background_Project_Full_Name
     (Adapter : Build_Command_Adapter) return Filesystem_String is
   begin
      return Adapter.Project_File.Full_Name.all;
   end Get_Background_Project_Full_Name;

   ----------------------------
   -- Get_Scenario_Variables --
   ----------------------------

   overriding
   function Get_Scenario_Variables
     (Adapter : Build_Command_Adapter) return Scenario_Variable_Array is
   begin
      return Adapter.Kernel_Registry.Tree.Scenario_Variables;
   end Get_Scenario_Variables;

   ----------------
   -- Substitute --
   ----------------

   overriding
   function Substitute
     (Adapter   : Build_Command_Adapter;
      Param     : String;
      Quoted    : Boolean;
      Done      : access Boolean;
      Server    : Server_Type := GPS_Server;
      For_Shell : Boolean := False) return String
   is
   begin
      return Shared_Macros_Substitute (
         Get_Context_Project (Adapter),
         Get_Context_Project (Adapter),
         Get_Context_File_Information (Adapter),
         Param,
         Quoted,
         Done,
         Server,
         For_Shell);
   end Substitute;

   --------------------
   -- Console_Insert --
   --------------------

   overriding
   procedure Console_Insert
     (Adapter : in out Build_Command_Adapter;
      Text   : String;
      Add_LF : Boolean := True;
      Mode   : Console_Message_Type := Info) is
   begin
      Trace (Me, Mode'Img & " : " & Text);
      if Mode = Error then
         Adapter.Status := Adapter.Status & Text;
         if Add_LF then
            Adapter.Status := Adapter.Status & ASCII.LF;
         end if;
      end if;
   end Console_Insert;

   --------------------------------------------
   -- Remove_Error_Builder_Message_From_File --
   --------------------------------------------

   overriding
   procedure Remove_Error_Builder_Message_From_File
     (Adapter : Build_Command_Adapter;
      File     : Virtual_File) is
   begin
      null;
   end Remove_Error_Builder_Message_From_File;

   -------------------------------------
   -- Get_Background_Environment_File --
   -------------------------------------

   overriding
   function Get_Background_Environment_File
     (Adapter : Build_Command_Adapter) return Virtual_File is
      pragma Unreferenced (Adapter);
   begin
      return No_File;
   end Get_Background_Environment_File;

   -----------------------
   -- Is_Server_In_Mode --
   -----------------------

   function Is_Server_In_Mode
     (Registry   : Build_Config_Registry_Access;
      Mode : String) return Boolean  is
      U : constant Unbounded_String := To_Unbounded_String (Mode);
   begin
      return Element_Mode (Registry, U).Is_Server;
   end Is_Server_In_Mode;

   ---------------------
   -- Get_Mode_Server --
   ---------------------

   function Get_Mode_Server
     (Registry   : Build_Config_Registry_Access;
      Mode : String) return Remote.Server_Type is
      U : constant Unbounded_String := To_Unbounded_String (Mode);
   begin
      return Element_Mode (Registry, U).Server;
   end Get_Mode_Server;

   ----------------
   -- Get_Server --
   ----------------

   function Get_Server
     (Registry   : Build_Config_Registry_Access;
      Mode       : String;
      Target     : Target_Access) return Server_Type is
      Server : Server_Type;
   begin
      if Is_Server_In_Mode (Registry, Mode) then
         Server := Get_Mode_Server (Registry, Mode);
      else
         Server := Get_Server (Target);
      end if;
      return Server;
   end Get_Server;

   ---------------------
   -- Get_Mode_Subdir --
   ---------------------

   function Get_Mode_Subdir
     (Registry : Build_Config_Registry_Access;
      Mode     : String) return Filesystem_String is
   begin
      return +To_String
        (Element_Mode
           (Registry, To_Unbounded_String (Mode)).Subdir);
   end Get_Mode_Subdir;

   -------------------------
   -- Expand_Command_Line --
   -------------------------

   function Expand_Command_Line
     (Build_Registry   : Build_Config_Registry_Access;
      Proj_Registry    : Project_Registry_Access;
      Proj_Type        : Project_Type;
      Toolchains       : Toolchain_Manager;
      Command_Line     : String;
      Target_Name      : String;
      Mode_Name        : String;
      Project_File     : Virtual_File;
      Force_File       : Virtual_File;
      Main_File        : Virtual_File;
      Simulate         : Boolean;
      Trusted_Mode     : Boolean;
      Multi_Language_Builder : Multi_Language_Builder_Policy;
      Execute_Command  : String
      ) return Expansion_Result is
      Adapter   : Build_Command_Adapter_Access := new Build_Command_Adapter;
      T         : constant Target_Access :=
         Get_Target_From_Name (Build_Registry, Target_Name);
      CL_Args   : Argument_List_Access :=
         Argument_String_To_List (Command_Line);
      Mode_Args : Argument_List_Access :=
         Apply_Mode_Args (Build_Registry, Get_Model (T), Mode_Name,
                          CL_Args.all);
      Res       : Expansion_Result;
   begin
      Initialize (Adapter.all, Proj_Registry, Proj_Type, Toolchains,
                  Force_File, '%', Trusted_Mode, Execute_Command,
                  Multi_Language_Builder);
      Adapter.Project_File := Project_File;
      Res := Expand_Command_Line
         (Abstract_Build_Command_Adapter_Access (Adapter), Mode_Args.all, T,
          Get_Server (Build_Registry, Mode_Name, T), Force_File, Main_File,
          Get_Mode_Subdir (Build_Registry, Mode_Name), False, Simulate);
      Res.Status := Adapter.Status;
      Free (CL_Args);
      Free (Mode_Args);
      Free_Adapter (Adapter);
      return Res;
   end Expand_Command_Line;

   -----------------
   -- Args_Length --
   -----------------

   function Args_Length (Result : Expansion_Result) return Integer is
   begin
      return Args_Length (Result.Args);
   end Args_Length;

   -------------
   -- Nth_Arg --
   -------------

   function Nth_Arg (Result : Expansion_Result; N : Natural) return String is
   begin
      return Nth_Arg (Result.Args, N);
   end Nth_Arg;

   ---------
   -- Dir --
   ---------

   function Dir (Result : Expansion_Result) return Virtual_File is
   begin
      return Result.Dir;
   end Dir;

   ---------------
   -- Arguments --
   ---------------

   function Arguments (Result : Expansion_Result) return String is
   begin
      return To_Display_String (Result.Args);
   end Arguments;

   ------------
   -- Status --
   ------------

   function Status (Result : Expansion_Result) return String is
   begin
      return To_String (Result.Status);
   end Status;

end Build_Command_Utils;
