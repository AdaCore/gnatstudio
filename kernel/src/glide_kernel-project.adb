-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
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

with Prj;         use Prj;
with Prj.Part;    use Prj.Part;
with Prj.Proc;    use Prj.Proc;
with Prj.Env;     use Prj.Env;
with Prj.Ext;     use Prj.Ext;
with Prj.Util;    use Prj.Util;
with Prj.Tree;    use Prj.Tree;
with Namet;       use Namet;
with Stringt;     use Stringt;
with Types;       use Types;
with Output;      use Output;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with GNAT.Expect;               use GNAT.Expect;
with GNAT.OS_Lib;               use GNAT.OS_Lib;

with Prj_API;            use Prj_API;
with Src_Info.Prj_Utils; use Src_Info.Prj_Utils;
with Prj_Normalize;      use Prj_Normalize;

with Traces;  use Traces;
with Glide_Intl; use Glide_Intl;
with Glide_Kernel.Console; use Glide_Kernel.Console;
with Language_Handlers.Glide; use Language_Handlers.Glide;

package body Glide_Kernel.Project is

   Me : Debug_Handle := Create ("glide_kernel.project");

   procedure Compute_Predefined_Paths
     (Handle : access Kernel_Handle_Record'Class);
   --  Compute the predefined source and object paths, given the current
   --  project view associated with Handle.

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
            Ada_Include_Path (Kernel.Project_View).all,
            Get_Predefined_Source_Path (Kernel));
      else
         return Find_File
           (Short_File_Name, Ada_Include_Path (Kernel.Project_View).all, "");
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
            Ada_Objects_Path (Kernel.Project_View).all,
            Get_Predefined_Object_Path (Kernel));
      else
         return Find_File
           (Short_File_Name, Ada_Objects_Path (Kernel.Project_View).all, "");
      end if;
   end Find_Object_File;

   ---------------------------
   -- Get_Project_File_Name --
   ---------------------------

   function Get_Project_File_Name
     (Kernel : access Kernel_Handle_Record'Class) return String is
   begin
      if Kernel.Project_Is_Default then
         return "";
      else
         return Get_Name_String (Directory_Of (Kernel.Project))
           & Get_Name_String (Prj.Tree.Name_Of (Kernel.Project));
      end if;
   end Get_Project_File_Name;

   -------------------------
   -- Get_Subproject_Name --
   -------------------------

   function Get_Subproject_Name
     (Handle    : access Kernel_Handle_Record'Class;
      File_Name : String) return String
   is
      Project : Project_Node_Id;
   begin
      if Handle.Project_Is_Default then
         return "";
      else
         Project := Get_Project_From_View
           (Get_Project_From_File (Handle.Project_View, File_Name));
         return Get_Name_String (Directory_Of (Project))
           & Get_Name_String (Prj.Tree.Name_Of (Project));
      end if;
   end Get_Subproject_Name;

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

      procedure Add_Directory (S : String) is
         Tmp : String_Access;
      begin
         if S = "" then
            return;

         elsif S = "<Current_Directory>" then
            if Source_Path then
               Tmp := Handle.Predefined_Source_Path;
               Handle.Predefined_Source_Path :=
                 new String' (Handle.Predefined_Source_Path.all & ":.");

            else
               Tmp := Handle.Predefined_Object_Path;
               Handle.Predefined_Object_Path :=
                 new String' (Handle.Predefined_Object_Path.all & ":.");
            end if;

         elsif Source_Path then
            Tmp := Handle.Predefined_Source_Path;
            Handle.Predefined_Source_Path :=
              new String' (Handle.Predefined_Source_Path.all & ":" & S);

         else
            Tmp := Handle.Predefined_Object_Path;
            Handle.Predefined_Object_Path :=
              new String' (Handle.Predefined_Object_Path.all & ":" & S);
         end if;

         Free (Tmp);
      end Add_Directory;

      Fd     : Process_Descriptor;
      Result : Expect_Match;
      Args   : Argument_List (1 .. 1);
      Gnatls : constant String := Get_Attribute_Value
        (Get_Project_View (Handle), Gnatlist_Attribute,
         Ide_Package, Default => "gnatls");

   begin
      --  If the gnatls commands hasn't changed, no need to recompute the
      --  predefined paths.

      if Handle.Gnatls_Cache /= null
        and then Handle.Gnatls_Cache.all = Gnatls
      then
         return;
      end if;

      Free (Handle.Gnatls_Cache);
      Handle.Gnatls_Cache := new String' (Gnatls);

      Free (Handle.Predefined_Source_Path);
      Free (Handle.Predefined_Object_Path);
      Handle.Predefined_Source_Path := new String' ("");
      Handle.Predefined_Object_Path := new String' ("");

      Args (1) := new String' ("-v");
      Non_Blocking_Spawn
        (Fd, Gnatls, Args, Buffer_Size => 0, Err_To_Out => True);
      Free (Args (1));
      Expect (Fd, Result, "Source Search Path:\n", Timeout => -1);

      loop
         Expect (Fd, Result, "\n", Timeout => -1);

         declare
            S : constant String := Trim (Expect_Out (Fd), Ada.Strings.Left);
         begin
            if S = "Object Search Path:" & ASCII.LF then
               Source_Path := False;
            else
               Add_Directory (S (S'First .. S'Last - 1));
            end if;
         end;
      end loop;

   exception
      when Process_Died =>
         Close (Fd);
   end Compute_Predefined_Paths;

   ------------------
   -- Load_Project --
   ------------------

   procedure Load_Project
     (Kernel : access Kernel_Handle_Record'class; Project : String)
   is
      procedure Report_Error (S : String);
      --  Output error messages from the project parser to the glide console.

      ------------------
      -- Report_Error --
      ------------------

      procedure Report_Error (S : String) is
      begin
         Console.Insert (Kernel, S, Mode => Console.Error, Add_LF => False);
      end Report_Error;

      New_Project : Project_Node_Id;
   begin
      Output.Set_Special_Output (Report_Error'Unrestricted_Access);
      Free (Kernel.Scenario_Variables);
      Kernel.Project_Is_Default := False;
      Prj.Part.Parse (New_Project, Project, True);

      if New_Project /= Empty_Node then
         Kernel.Project := New_Project;
         Kernel.Project_View := No_Project;
         Project_Changed (Kernel);
         Recompute_View (Kernel);
      else
         Trace (Me, "Couldn't load or parse the project " & Project);
         Console.Insert (Kernel, -"Couldn't parse the project " & Project);
      end if;

      Reset_Normalized_Flag (Kernel.Project);
      Output.Set_Special_Output (null);
   end Load_Project;

   -----------------
   -- Get_Project --
   -----------------

   function Get_Project (Handle : access Kernel_Handle_Record'Class)
      return Prj.Tree.Project_Node_Id is
   begin
      return Handle.Project;
   end Get_Project;

   ----------------------
   -- Get_Project_View --
   ----------------------

   function Get_Project_View
     (Handle : access Kernel_Handle_Record'Class) return Prj.Project_Id is
   begin
      return Handle.Project_View;
   end Get_Project_View;

   --------------------
   -- Recompute_View --
   --------------------

   procedure Recompute_View (Handle : access Kernel_Handle_Record'Class) is
      procedure Report_Error
        (S       : String;
         Project : Project_Id);
      --  Handler called when the project parser finds an error.

      ------------------
      -- Report_Error --
      ------------------

      procedure Report_Error
        (S       : String;
         Project : Project_Id) is
      begin
         if not Handle.Project_Is_Default then
            Console.Insert
              (Handle,
               Project_Name (Project) & ": " & S,
               Mode => Console.Error, Add_LF => True);
         end if;
      end Report_Error;

      Scenario_Vars : constant Project_Node_Array :=
        Scenario_Variables (Handle);
      Ext_Ref : String_Id;

   begin
      --  To avoid any problem with invalid variable values, we need to provide
      --  a current value when no default value is provided by the user
      --  ??? Is this really needed, when Glide should always have a value for
      --  the variable, set through the combo boxes.

      for J in Scenario_Vars'Range loop
         if External_Default (Scenario_Vars (J)) = Empty_Node then
            Ext_Ref := External_Reference_Of (Scenario_Vars (J));
            pragma Assert
              (Ext_Ref /= No_String,
               "Scenario variable is not an external reference");
            String_To_Name_Buffer (Ext_Ref);

            declare
               Name : constant String :=
                 Name_Buffer (Name_Buffer'First .. Name_Len);
            begin
               if Prj.Ext.Value_Of (Name_Find) = No_String then
                  String_To_Name_Buffer
                    (String_Value_Of (First_Literal_String
                      (String_Type_Of (Scenario_Vars (J)))));
                  Prj.Ext.Add
                    (Name, Name_Buffer (Name_Buffer'First .. Name_Len));
               end if;
            end;
         end if;
      end loop;

      --  Evaluate the current project

      if Debug_Mode and then Active (Me) then
         Trace (Me, "Recompute_View:");
         Trace_Pretty_Print (Me, Handle.Project);
      end if;

      Prj.Reset;
      Prj.Proc.Process
        (Handle.Project_View, Handle.Project,
         Report_Error'Unrestricted_Access);
      pragma Assert (Handle.Project_View /= No_Project);

      --  Parse the list of source files for languages other than Ada.
      --  At the same time, check that the gnatls attribute is coherent between
      --  all projects and subprojects

      declare
         Iter : Imported_Project_Iterator := Start (Handle.Project, True);
         Gnatls : constant String := Get_Attribute_Value
           (Get_Project_View (Handle), Gnatlist_Attribute, Ide_Package);
      begin
         while Current (Iter) /= No_Project loop
            declare
               Ls : constant String := Get_Attribute_Value
                 (Current (Iter), Gnatlist_Attribute, Ide_Package);
            begin
               if Ls /= "" and then Ls /= Gnatls then
                  Insert (Handle,
                          "gnatls attribute is not the same in the "
                          & "subproject """ & Project_Name (Current (Iter))
                          & """ as in the root project."
                          & " It will be ignored in the subproject.");
               end if;
            end;

            Add_Foreign_Source_Files (Current (Iter));
            Next (Iter);
         end loop;
      end;

      --  Check that all the environment variables have values defined through
      --  Prj.Ext. If this is not the case, then their default value should be
      --  put there.
      --  We need to do this only after evaluation the project view, so that if
      --  the default value is defined through other variables these are
      --  already evaluated.

      for J in Scenario_Vars'Range loop
         Ext_Ref := External_Reference_Of (Scenario_Vars (J));
         String_To_Name_Buffer (Ext_Ref);

         declare
            Name : constant String :=
              Name_Buffer (Name_Buffer'First .. Name_Len);
            Value : Variable_Value;
         begin
            if Prj.Ext.Value_Of (Name_Find) = No_String then
               Value := Prj.Util.Value_Of
                 (Variable_Name => Prj.Tree.Name_Of (Scenario_Vars (J)),
                  In_Variables => Projects.Table
                    (Handle.Project_View).Decl.Variables);
               pragma Assert
                 (Value.Kind = Single,
                  "Scenario variables can only be strings");
               String_To_Name_Buffer (Value.Value);

               Prj.Ext.Add (Name, Name_Buffer (Name_Buffer'First .. Name_Len));
            end if;
         end;
      end loop;

      Compute_Predefined_Paths (Handle);

      Set_Project_View
        (Glide_Language_Handler (Handle.Lang_Handler), Handle.Project_View);

      --  Report the change to every listener
      Project_View_Changed (Handle);
   end Recompute_View;

   ------------------------------
   -- Directory_In_Source_Path --
   ------------------------------

   function Directory_In_Source_Path
     (Handle         : access Kernel_Handle_Record'Class;
      Directory_Name : String) return Boolean
   is
      Dir : String_List_Id := Projects.Table (Handle.Project_View).Source_Dirs;
   begin
      while Dir /= Nil_String loop
         String_To_Name_Buffer (String_Elements.Table (Dir).Value);
         if Directory_Name = Name_Buffer (1 .. Name_Len) then
            return True;
         end if;

         Dir := String_Elements.Table (Dir).Next;
      end loop;
      return False;
   end Directory_In_Source_Path;

   --------------------------
   -- File_In_Project_View --
   --------------------------

   function File_In_Project_View
     (Handle          : access Kernel_Handle_Record'Class;
      Short_File_Name : String) return Boolean
   is
      Src : String_List_Id := Projects.Table (Handle.Project_View).Sources;
   begin
      while Src /= Nil_String loop
         String_To_Name_Buffer (String_Elements.Table (Src).Value);
         if Short_File_Name = Name_Buffer (1 .. Name_Len) then
            return True;
         end if;

         Src := String_Elements.Table (Src).Next;
      end loop;

      return False;
   end File_In_Project_View;

   ---------------------------------
   -- Scenario_Variables_Cmd_Line --
   ---------------------------------

   function Scenario_Variables_Cmd_Line
     (Handle : access Kernel_Handle_Record'Class)
      return String
   is
      Scenario_Vars : constant Project_Node_Array :=
        Scenario_Variables (Handle);

      function Concat (Current : String; Index : Natural) return String;
      --  Concat the command line line for the Index-nth variable and the
      --  following ones to Current, and return the return

      ------------
      -- Concat --
      ------------

      function Concat (Current : String; Index : Natural) return String is
         Ext_Ref : String_Id;
      begin
         if Index > Scenario_Vars'Last then
            return Current;
         end if;

         Ext_Ref := External_Reference_Of (Scenario_Vars (Index));
         String_To_Name_Buffer (Ext_Ref);

         declare
            Name : constant String :=
              Name_Buffer (Name_Buffer'First .. Name_Len);
            Value : String_Id;
         begin
            Value := Prj.Ext.Value_Of (Name_Find);
            String_To_Name_Buffer (Value);

            return Concat
              (Current
               & "-X" & Name
               & "=" & Name_Buffer (Name_Buffer'First .. Name_Len) & " ",
               Index + 1);
         end;
      end Concat;

   begin
      --  A recursive function is probably not the most efficient way, but this
      --  prevents limits on the command line lengths. This also avoids the use
      --  of unbounded strings.
      return Concat ("", Scenario_Vars'First);
   end Scenario_Variables_Cmd_Line;

   ------------------------
   -- Scenario_Variables --
   ------------------------

   function Scenario_Variables (Kernel : access Kernel_Handle_Record'Class)
      return Project_Node_Array is
   begin
      if Kernel.Scenario_Variables = null then
         Kernel.Scenario_Variables := new Project_Node_Array'
           (Find_Scenario_Variables (Get_Project (Kernel)));
      end if;
      return Kernel.Scenario_Variables.all;
   end Scenario_Variables;

end Glide_Kernel.Project;

