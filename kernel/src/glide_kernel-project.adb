-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
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
with Prj.PP;      use Prj.PP;
with Namet;       use Namet;
with Stringt;     use Stringt;
with Types;       use Types;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Output;      use Output;

with Prj_API;            use Prj_API;
with Src_Info.Prj_Utils; use Src_Info.Prj_Utils;
with Prj_Normalize;      use Prj_Normalize;

with Traces;  use Traces;
with Glide_Kernel.Console; use Glide_Kernel.Console;

package body Glide_Kernel.Project is

   Me : Debug_Handle := Create ("glide_kernel.project");

   ----------------------
   -- Find_Source_File --
   ----------------------

   function Find_Source_File
     (Kernel                     : access Kernel_Handle_Record'Class;
      Short_File_Name            : String;
      Use_Predefined_Source_Path : Boolean := False)
      return String
   is
      Path : String_Access;
   begin
      --  First, try on the project source path
      Path := Locate_Regular_File
        (Short_File_Name,
         Ada_Include_Path (Kernel.Project_View).all);

      if Path /= null then
         declare
            Full_Path : constant String := Path.all;
         begin
            Free (Path);
            return Full_Path;
         end;
      end if;

      --  Fallback, try on the Source_Path (only if Use_Source_Path is set)
      if Use_Predefined_Source_Path
        and then Get_Predefined_Source_Path (Kernel) /= ""
      then
         Path :=
           Locate_Regular_File
             (Short_File_Name, Get_Predefined_Source_Path (Kernel));
         if Path /= null then
            declare
               Full_Path : constant String := Path.all;
            begin
               Free (Path);
               return Full_Path;
            end;
         end if;
      end if;

      --  Source file not found anywhere, return the empty string
      return "";
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
         return Find_Object_File
           (Kernel.Project_View, Short_File_Name,
            Get_Predefined_Object_Path (Kernel));
      else
         return Find_Object_File (Kernel.Project_View, Short_File_Name, "");
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
           & Get_Name_String (Name_Of (Kernel.Project));
      end if;
   end Get_Project_File_Name;

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
         Insert (Kernel, S, Mode => Glide_Kernel.Console.Error);
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
         Insert
           (Kernel, Text => "Couldn't parse the project " & Project);
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
      procedure Report_Error (S : String);
      --  Handler called when the project parser finds an error.

      ------------------
      -- Report_Error --
      ------------------

      procedure Report_Error (S : String) is
      begin
         if not Handle.Project_Is_Default then
            Insert (Handle, S, Mode => Glide_Kernel.Console.Error);
         end if;
      end Report_Error;

      Scenario_Vars : constant Project_Node_Array :=
        Scenario_Variables (Handle);
      Ext_Ref : String_Id;

   begin
      --  To avoid any problem with invalid variable values, we need to provide
      --  a current value when no default value is provided by the user
      --  ??? Is this really needed, when Glide should always have a value for
      --  ??? the variable, set through the combo boxes.

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
                 (Variable_Name => Name_Of (Scenario_Vars (J)),
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

      --  Report the change to every listener
      Project_View_Changed (Handle);
   end Recompute_View;

   ----------------------
   -- Get_Source_Files --
   ----------------------

   function Get_Source_Files
     (Handle : access Kernel_Handle_Record'Class)
      return Basic_Types.String_Array_Access
   is
      use Basic_Types;

      Src     : String_List_Id;
      Count   : Natural := 0;
      Sources : String_Array_Access;
      Index   : Natural := 1;

   begin
      Src := Projects.Table (Handle.Project_View).Sources;

      while Src /= Nil_String loop
         Count := Count + 1;
         Src := String_Elements.Table (Src).Next;
      end loop;

      Sources := new String_Array (1 .. Count);
      Src := Projects.Table (Handle.Project_View).Sources;

      while Src /= Nil_String loop
         String_To_Name_Buffer (String_Elements.Table (Src).Value);
         Sources (Index) := new String'
           (Name_Buffer (Name_Buffer'First .. Name_Len));
         Src := String_Elements.Table (Src).Next;
      end loop;

      return Sources;
   end Get_Source_Files;

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

   ----------------------------------
   -- Get_Current_Explorer_Context --
   ----------------------------------

   function Get_Current_Explorer_Context
     (Handle : access Kernel_Handle_Record'Class)
      return Selection_Context_Access is
   begin
      return Handle.Explorer_Context;
   end Get_Current_Explorer_Context;

end Glide_Kernel.Project;

