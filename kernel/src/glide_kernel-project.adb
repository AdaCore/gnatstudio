-----------------------------------------------------------------------
--                                                                   --
--                     Copyright (C) 2001                            --
--                          ACT-Europe                               --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
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
with GNAT.OS_Lib; use GNAT.OS_Lib;

with Project_Viewers;   use Project_Viewers;
with Project_Explorers; use Project_Explorers;
with Glide_Main_Window; use Glide_Main_Window;
with Gtkada.MDI;        use Gtkada.MDI;
with Glide_Page;        use Glide_Page;
with GVD.Process;       use GVD.Process;
with Prj_API;           use Prj_API;

package body Glide_Kernel.Project is

   --  ??? Preferences
   Default_Project_Width  : constant := 400;
   Default_Project_Height : constant := 400;

   Project_Editor_Window_Name : constant String := "Project editor";

   ----------------------
   -- Find_Source_File --
   ----------------------

   function Find_Source_File
     (Kernel          : access Kernel_Handle_Record'Class;
      Short_File_Name : String;
      Use_Source_Path : Boolean := False)
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
      if Use_Source_Path then
         Path := Locate_Regular_File (Short_File_Name, Kernel.Source_Path.all);
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
     (Kernel          : access Kernel_Handle_Record'Class;
      Short_File_Name : String;
      Use_Object_Path : Boolean := False)
      return String
   is
      Path : String_Access;
   begin
      --  First, try on the project object path
      Path := Locate_Regular_File
        (Short_File_Name,
         Ada_Objects_Path (Kernel.Project_View).all);

      if Path /= null then
         declare
            Full_Path : constant String := Path.all;
         begin
            Free (Path);
            return Full_Path;
         end;
      end if;

      --  Fallback, try on the Object_Path (only if Use_Object_Path is set)
      if Use_Object_Path then
         Path := Locate_Regular_File (Short_File_Name, Kernel.Object_Path.all);
         if Path /= null then
            declare
               Full_Path : constant String := Path.all;
            begin
               Free (Path);
               return Full_Path;
            end;
         end if;
      end if;

      --  Object file not found anywhere, return the empty string
      return "";
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
     (Kernel : access Kernel_Handle_Record'class; Project : String) is
   begin
      Kernel.Project_Is_Default := False;
      Prj.Part.Parse (Kernel.Project, Project, True);
      Kernel.Project_View := No_Project;
      Project_Changed (Kernel);
      Recompute_View (Kernel);
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
         --  ??? Errors should be reported in the Glide console or better,
         --  handled interactively.
         null;
      end Report_Error;

      Scenario_Variables : constant Project_Node_Array :=
        Find_Scenario_Variables (Get_Project (Handle));
      Ext_Ref : String_Id;

   begin
      --  To avoid any problem with invalid variable values, we need to provide
      --  a current value when no default value is provided by the user

      for J in Scenario_Variables'Range loop
         if External_Default (Scenario_Variables (J)) = Empty_Node then
            Ext_Ref := External_Reference_Of (Scenario_Variables (J));
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
                      (String_Type_Of (Scenario_Variables (J)))));
                  Prj.Ext.Add
                    (Name, Name_Buffer (Name_Buffer'First .. Name_Len));
               end if;
            end;
         end if;
      end loop;


      --  Evaluate the current project

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

      for J in Scenario_Variables'Range loop
         Ext_Ref := External_Reference_Of (Scenario_Variables (J));
         String_To_Name_Buffer (Ext_Ref);

         declare
            Name : constant String :=
              Name_Buffer (Name_Buffer'First .. Name_Len);
            Value : Variable_Value;
         begin
            if Prj.Ext.Value_Of (Name_Find) = No_String then
               Value := Prj.Util.Value_Of
                 (Variable_Name => Name_Of (Scenario_Variables (J)),
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

      --  ??? In fact, we should also cache the list of scenario variables
      --  ??? here, so that all the prj_editor packages do not need to
      --  ??? recompute them every time.

      --  Report the change to every listener
      Project_View_Changed (Handle);
   end Recompute_View;

   -------------------------
   -- Open_Project_Editor --
   -------------------------

   procedure Open_Project_Editor
     (Handle : access Kernel_Handle_Record'Class)
   is
      Top        : constant Glide_Window := Glide_Window (Handle.Main_Window);
      Page       : Glide_Page.Glide_Page :=
        Glide_Page.Glide_Page (Get_Current_Process (Top));
      MDI        : constant MDI_Window := Page.Process_Mdi;
      Child      : MDI_Child;
      Viewer     : Project_Viewer;
      Iter       : Child_Iterator := First_Child (MDI);
   begin
      if Get_Project (Handle) = Empty_Node then
         return;
      end if;

      loop
         Child := Get (Iter);
         exit when Child = null
           or else Get_Title (Child) = Project_Editor_Window_Name;
         Next (Iter);
      end loop;

      if Child /= null then
         Raise_Child (Child);
         return;
      end if;

      Gtk_New (Viewer, Handle, Get_Tree (Page.Explorer));
      Set_Size_Request (Viewer, Default_Project_Width, Default_Project_Height);
      Child := Put (MDI, Viewer);
      Set_Title (Child, Project_Editor_Window_Name);
   end Open_Project_Editor;

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

   ---------------------------------
   -- Scenario_Variables_Cmd_Line --
   ---------------------------------

   function Scenario_Variables_Cmd_Line
     (Handle : access Kernel_Handle_Record'Class)
      return String
   is
      Scenario_Variables : constant Project_Node_Array :=
        Find_Scenario_Variables (Get_Project (Handle));

      function Concat (Current : String; Index : Natural) return String;
      --  Concat the command line line for the Index-nth variable and the
      --  following ones to Current, and return the return

      ------------
      -- Concat --
      ------------

      function Concat (Current : String; Index : Natural) return String is
         Ext_Ref : String_Id;
      begin
         if Index > Scenario_Variables'Last then
            return Current;
         end if;

         Ext_Ref := External_Reference_Of (Scenario_Variables (Index));
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
      return Concat ("", Scenario_Variables'First);
   end Scenario_Variables_Cmd_Line;

end Glide_Kernel.Project;

