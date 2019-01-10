------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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

with Ada.Strings.Unbounded;        use Ada.Strings.Unbounded;
with Commands.Interactive;         use Commands, Commands.Interactive;
with GPS.Editors;                  use GPS.Editors;
with GPS.Editors.Line_Information; use GPS.Editors.Line_Information;
with GPS.Kernel.Actions;           use GPS.Kernel.Actions;
with GPS.Kernel.Contexts;          use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;             use GPS.Kernel.Hooks;
with GPS.Kernel.Modules.UI;        use GPS.Kernel.Modules.UI;
with GPS.Kernel.Project;           use GPS.Kernel.Project;
with GPS.Intl;                     use GPS.Intl;
with GPS.VCS;                      use GPS.VCS;
with GNAT.Strings;                 use GNAT.Strings;
with GNATCOLL.Projects;            use GNATCOLL.Projects;
with GNATCOLL.VFS;                 use GNATCOLL.VFS;
with VCS2.Engines;                 use VCS2.Engines;
with VCS2.Scripts;                 use VCS2.Scripts;
with VCS2.Branches;
with VCS2.Commits;
with VCS2.Diff;
with VCS2.History;

package body VCS2.Module is

   Annotation_Id : constant String := "Annotate";
   --  Id used when annotations editor lines.

   type On_Project_View_Changed is new Simple_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Project_View_Changed;
      Kernel : not null access Kernel_Handle_Record'Class);
   --  Called when the project view has changed.
   --  This looks for what VCS engine to use for each project. It tries to
   --  reuse existing engines when possible, to benefit from their caches.

   type On_Project_Changed is new Simple_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Project_Changed;
      Kernel : not null access Kernel_Handle_Record'Class);
   --  Called when the user loads a new project.

   type On_File_Saved is new File_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_File_Saved;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File);

   type Annotate is new Interactive_Command with null record;
   overriding function Execute
     (Self   : access Annotate;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Remove_Annotate is new Interactive_Command with null record;
   overriding function Execute
     (Self    : access Remove_Annotate;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Is_Annotated_Filter is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Self    : access Is_Annotated_Filter;
      Context : Selection_Context) return Boolean;
   --  Whether the current file is annotated to show the last modifications on
   --  each line.

   type On_Annotation_Visitor is new Task_Visitor with record
      Kernel  : Kernel_Handle;
   end record;
   overriding procedure On_Annotation
     (Self       : not null access On_Annotation_Visitor;
      File       : Virtual_File;
      First_Line : Positive;
      Ids, Text  : GNAT.Strings.String_List);

   type On_File_Changed_Detected is new Simple_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_File_Changed_Detected;
      Kernel : not null access Kernel_Handle_Record'Class);

   -------------------
   -- On_Annotation --
   -------------------

   overriding procedure On_Annotation
     (Self       : not null access On_Annotation_Visitor;
      File       : Virtual_File;
      First_Line : Positive;
      Ids, Text  : GNAT.Strings.String_List)
   is
      A : Line_Information_Array (First_Line .. First_Line + Text'Length - 1);
      Same_Info : constant Unbounded_String :=
        To_Unbounded_String ("   ...");
      Max_Len   : Natural := Length (Same_Info);
      Non_Null  : Natural := 1;
   begin
      for T in Text'Range loop
         if T > Text'First
           and then (Text (T) = null
                     or else Text (Non_Null).all = Text (T).all)
         then
            A (T - Text'First + A'First).Text := Same_Info;
         else
            A (T - Text'First + A'First).Text := To_Unbounded_String
              ("<span underline='single'>" & Text (T).all & "</span>");
            A (T - Text'First + A'First).Associated_Command :=
              VCS2.History.Create_Show_History_Command
                (Kernel    => Self.Kernel,
                 File      => File,
                 Commit_ID => Ids (T).all);
            Max_Len := Integer'Max (Max_Len, Text (T)'Length);
            Non_Null := T;
         end if;
      end loop;

      Create_Line_Information_Column
        (Self.Kernel,
         File       => File,
         Identifier => Annotation_Id,
         Info       =>
           (Text         => To_Unbounded_String (String'(1 .. Max_Len => ' ')),
            Tooltip_Text  => Null_Unbounded_String,
            Image         => Null_Unbounded_String,
            Message       => <>,
            Associated_Command => null),
         Every_Line => False);
      Add_Line_Information
        (Self.Kernel,
         File       => File,
         Identifier => Annotation_Id,
         Info       => A);
   end On_Annotation;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Annotate;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Self);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      File   : constant Virtual_File := File_Information (Context.Context);
      VCS    : VCS_Engine_Access;
   begin
      if File /= No_File then
         VCS := VCS_Engine_Access
           (Kernel.VCS.Guess_VCS_For_Directory (File.Dir));
         VCS.Queue_Annotations
           (new On_Annotation_Visitor'(Task_Visitor with Kernel => Kernel),
            File => File);
      end if;
      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Remove_Annotate;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Self);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      File   : constant Virtual_File := File_Information (Context.Context);
   begin
      if File /= No_File then
         Remove_Line_Information_Column (Kernel, File, Annotation_Id);
      end if;
      return Success;
   end Execute;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Self    : access Is_Annotated_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Self);
      Kernel : constant Kernel_Handle := Get_Kernel (Context);
      File   : constant Virtual_File := File_Information (Context);
   begin
      if File = No_File then
         return False;
      else
         declare
            Buffer : constant Editor_Buffer'Class :=
              Kernel.Get_Buffer_Factory.Get
                (File => File, Open_View => False);
         begin
            return Buffer.Has_Information_Column (Annotation_Id);
         end;
      end if;
   end Filter_Matches_Primitive;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Project_Changed;
      Kernel : not null access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Self);
   begin
      Reset_VCS_Engines (Kernel);
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Project_View_Changed;
      Kernel : not null access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Self);
   begin
      Compute_VCS_Engines (Kernel);
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_File_Saved;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File)
   is
      pragma Unreferenced (Self);
      Info    : constant File_Info'Class := File_Info'Class
         (Get_Registry (Kernel).Tree.Info_Set (File).First_Element);
      Project : constant Project_Type := Info.Project (True);
      V       : constant VCS_Engine_Access :=
         VCS_Engine_Access (Kernel.VCS.Get_VCS (Project));
   begin
      V.Invalidate_File_Status_Cache (File);
      Vcs_Refresh_Hook.Run (Kernel, Is_File_Saved => True);
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_File_Changed_Detected;
      Kernel : not null access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Self);
   begin
      Kernel.VCS.Invalidate_All_Caches;
      Ensure_Status_For_All_Files_In_All_Engines (Kernel, From_User => False);
      Vcs_Refresh_Hook.Run (Kernel, Is_File_Saved => False);
   end Execute;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      V : constant access VCS_Repository := new VCS_Repository'
        (Abstract_VCS_Repository with Kernel => Kernel);
      Is_Annotated : constant Action_Filter := new Is_Annotated_Filter;
   begin
      Kernel.Set_VCS (V);

      VCS2.Scripts.Register_Scripts (Kernel);

      Project_Changed_Hook.Add (new On_Project_Changed);

      After_File_Changed_Detected_Hook.Add (new On_File_Changed_Detected);
      Project_View_Changed_Hook.Add (new On_Project_View_Changed);
      File_Saved_Hook.Add (new On_File_Saved);

      Register_Action
        (Kernel, "vcs annotate",
         Description =>
           -("For each line of the current file, show when the last"
             & " modification was done"),
         Filter      => Kernel.Lookup_Filter ("Source editor")
             and not Is_Annotated,
         Command     => new Annotate,
         Category    => "VCS2");

      Register_Action
        (Kernel, "vcs remove annotate",
         Description =>
           -("Remove annotations done on each line of the current file that"
             & " show when the last modification was done"),
         Filter      => Kernel.Lookup_Filter ("Source editor")
             and Is_Annotated,
         Command     => new Remove_Annotate,
         Category    => "VCS2");

      Register_Contextual_Menu
        (Kernel,
         Action   => "vcs annotate",
         Label    => "Version Control/Show last modification for lines");
      Register_Contextual_Menu
        (Kernel,
         Action   => "vcs remove annotate",
         Label    => "Version Control/Hide last modification for lines");

      VCS2.Commits.Register_Module (Kernel);
      VCS2.History.Register_Module (Kernel);
      VCS2.Diff.Register_Module (Kernel);
      VCS2.Branches.Register_Module (Kernel);
   end Register_Module;

end VCS2.Module;
