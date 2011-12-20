------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2012, AdaCore                     --
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
with GNAT.Expect;                 use GNAT.Expect;
with GNAT.Regpat;                 use GNAT.Regpat;
with GNATCOLL.Scripts;            use GNATCOLL.Scripts;

with Glib;                        use Glib;
with Glib.Object;                 use Glib.Object;
with Gtkada.File_Selector;        use Gtkada.File_Selector;

with Default_Preferences;         use Default_Preferences;
with Docgen2;                     use Docgen2;
with Docgen2.Scripts;
with Docgen2_Backend;             use Docgen2_Backend;
with Docgen2_Backend.HTML;        use Docgen2_Backend.HTML;
with GPS.Intl;                    use GPS.Intl;
with GPS.Kernel.Contexts;         use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;            use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;              use GPS.Kernel.MDI;
with GPS.Kernel.Modules;          use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;       use GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences;      use GPS.Kernel.Preferences;
with GPS.Kernel.Scripts;          use GPS.Kernel.Scripts;
with Traces;                      use Traces;
with GNATCOLL.Projects;           use GNATCOLL.Projects;
with GNATCOLL.VFS;                use GNATCOLL.VFS;

package body Docgen2_Module is

   Me : constant Debug_Handle := Create ("Docgen");

   Docgen_Module_Id : GPS.Kernel.Modules.Module_ID;

   type Docgen_Module_Record is new Module_ID_Record with record
      --  Docgen preferences

      Generate_Body_Files     : Boolean_Preference;
      --  Create also the body documentation

      Comments_Filter         : String_Preference;
      --  Regexp used to filter comments

      Show_Private_Entities   : Boolean_Preference;
      --  Show also private entities

      Show_References         : Boolean_Preference;
      --  True if the program should search for the references
      --  Adding information like "subprogram called by..."

      Process_Up_To_Date_Only : Boolean_Preference;
      --  True if docgen should document up to date entities only.

      Spawn_Browser           : Boolean_Preference;
      --  True if docgen should spawn a browser after documentation generation.

      Gen_Comment_Xref        : Boolean_Preference;
      --  True if docgen should try to find entity names in comments and
      --  generate cross references in this case

      Options                 : Docgen_Options;
      --  Group all the preferences

      Backend                 : Docgen2_Backend.Backend_Handle;
      --  The backend used to generate documentation
   end record;
   type Docgen_Module is access all Docgen_Module_Record'Class;

   overriding procedure Destroy (Module : in out Docgen_Module_Record);

   procedure On_Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class);
   --  Hook called when Preferences changed

   --------------
   -- Commands --
   --------------

   procedure Register_Commands
     (Kernel : not null access Kernel_Handle_Record'Class);
   --  Register commands specific to the Docgen2 module

   File_Cst      : aliased constant String := "file";
   Project_Cst   : aliased constant String := "project";
   Recursive_Cst : aliased constant String := "recursive";
   --  Names of parameters used by script functions

   Generate_For_File_Parameters    : constant Cst_Argument_List :=
                                       (1 => File_Cst'Access);
   --  List of named parameters for the function that generates documentation
   --  for a single file

   Generate_For_Project_Parameters : constant Cst_Argument_List :=
                                       (1 => Project_Cst'Access,
                                        2 => Recursive_Cst'Access);
   --  List of named parameters for the function that generates documentation
   --  for a project

   procedure File_Commands_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Handler for the Docgen2 commands specific to the File class

   procedure Project_Commands_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Handler for the Docgen2 commands specific to the Project class

   ------------------
   -- For the menu --
   ------------------

   procedure Choose_Menu_Current_File
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Generate the doc for the selected file

   procedure Choose_Menu_Project
      (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  In order to generate the doc of the loaded project
   --  It generates only the direct sources of the project
   --  It calls Generate_Project

   procedure Choose_Menu_Project_Recursive
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  In order to generate the doc of the project loaded
   --  It generates the direct sources of the project and the sources
   --     from imported projects
   --  It calls Generate_Project

   procedure Choose_Menu_File
      (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  In order to choose a file and generate its documentation
   --  It calls Generate_File

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Module : in out Docgen_Module_Record) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Backend_Record'Class, Backend_Handle);
   begin
      Unchecked_Free (Module.Backend);
   end Destroy;

   ----------------------------
   -- On_Preferences_Changed --
   ----------------------------

   procedure On_Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class)
   is
      function Get_Filter (Regx : String) return Pattern_Matcher_Access;
      --  Return an access to a pattern, or null if the string is empty

      ----------------
      -- Get_Filter --
      ----------------

      function Get_Filter (Regx : String) return Pattern_Matcher_Access is
      begin
         if Regx = "" then
            return null;
         else
            return new Pattern_Matcher'
              (Compile (Regx, Single_Line or Multiple_Lines));
         end if;
      end Get_Filter;

      pragma Unreferenced (Kernel);
   begin
      Docgen_Module (Docgen_Module_Id).Options :=
        (Process_Body_Files      =>
           Docgen_Module (Docgen_Module_Id).Generate_Body_Files.Get_Pref,
         Comments_Filter         =>
           Get_Filter
             (Docgen_Module (Docgen_Module_Id).Comments_Filter.Get_Pref),
         Show_Private            =>
           Docgen_Module (Docgen_Module_Id).Show_Private_Entities.Get_Pref,
         References              =>
           Docgen_Module (Docgen_Module_Id).Show_References.Get_Pref,
         Process_Up_To_Date_Only =>
           Docgen_Module (Docgen_Module_Id).Process_Up_To_Date_Only.Get_Pref,
         Spawn_Browser           =>
           Docgen_Module (Docgen_Module_Id).Spawn_Browser.Get_Pref,
         Generate_Comment_Xref   =>
           Docgen_Module (Docgen_Module_Id).Gen_Comment_Xref.Get_Pref);

   exception
      when E : others =>
         Trace (Exception_Handle, E);
   end On_Preferences_Changed;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Generate_Project_Command;
      Context : Interactive_Command_Context) return Command_Return_Type is
   begin
      Generate
        (Get_Kernel (Context.Context),
         Docgen_Module (Docgen_Module_Id).Backend,
         Project_Information (Context.Context),
         Docgen_Module (Docgen_Module_Id).Options,
         Command.Recursive);
      return Commands.Success;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return Commands.Failure;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Generate_File_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
   begin
      Generate
        (Get_Kernel (Context.Context),
         Docgen_Module (Docgen_Module_Id).Backend,
         File_Information (Context.Context),
         Docgen_Module (Docgen_Module_Id).Options);
      return Commands.Success;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return Commands.Failure;
   end Execute;

   ---------------------------
   -- File_Commands_Handler --
   ---------------------------

   procedure File_Commands_Handler
     (Data    : in out Callback_Data'Class;
      Command : String) is
   begin
      if Command = "generate_doc" then
         Name_Parameters (Data, Generate_For_File_Parameters);

         declare
            File : constant Virtual_File := Nth_Arg (Data, 1);
         begin
            Trace (Me, "Generating doc for file " &
                   Display_Full_Name (File));
            Generate
              (Get_Kernel (Data),
               Docgen_Module (Docgen_Module_Id).Backend,
               File,
               Docgen_Module (Docgen_Module_Id).Options);
         end;
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
   end File_Commands_Handler;

   ------------------------------
   -- Project_Commands_Handler --
   ------------------------------

   procedure Project_Commands_Handler
     (Data    : in out Callback_Data'Class;
      Command : String) is
   begin
      if Command = "generate_doc" then
         Name_Parameters (Data, Generate_For_Project_Parameters);

         declare
            Kernel    : constant Kernel_Handle := Get_Kernel (Data);
            Project   : constant Project_Type := Get_Data (Data, 1);
            Recursive : constant Boolean := Nth_Arg (Data, 2, False);
         begin
            Trace (Me, "Generating doc for project " & Project.Name);
            Generate
              (Kernel,
               Docgen_Module (Docgen_Module_Id).Backend,
               Project,
               Docgen_Module (Docgen_Module_Id).Options,
               Recursive);
         end;
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
   end Project_Commands_Handler;

   -----------------------
   -- Register_Commands --
   -----------------------

   procedure Register_Commands
     (Kernel : not null access Kernel_Handle_Record'Class) is
   begin
      Docgen2.Scripts.Register_Commands (Kernel);
      Register_Command
        (Kernel, "generate_doc", 0, 0, File_Commands_Handler'Access,
         Get_File_Class (Kernel));
      Register_Command
        (Kernel, "generate_doc", 0, 1, Project_Commands_Handler'Access,
         Get_Project_Class (Kernel));
   end Register_Commands;

   ------------------------------
   -- Choose_Menu_Current_File --
   ------------------------------

   procedure Choose_Menu_Current_File
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Context : constant Selection_Context :=
                  Get_Current_Context (Kernel);
      File    : aliased Virtual_File;

   begin
      if Has_File_Information (Context) then
         File := File_Information (Context);

         if File /= GNATCOLL.VFS.No_File then
            Generate
              (Kernel,
               Docgen_Module (Docgen_Module_Id).Backend,
               File,
               Docgen_Module (Docgen_Module_Id).Options);
         end if;
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
   end Choose_Menu_Current_File;

   -------------------------
   -- Choose_Menu_Project --
   -------------------------

   procedure Choose_Menu_Project
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      Generate
        (Kernel,
         Docgen_Module (Docgen_Module_Id).Backend,
         No_Project,
         Docgen_Module (Docgen_Module_Id).Options,
         False);

   exception
      when E : others =>
         Trace (Exception_Handle, E);
   end Choose_Menu_Project;

   -----------------------------------
   -- Choose_Menu_Project_Recursive --
   -----------------------------------

   procedure Choose_Menu_Project_Recursive
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      Generate
        (Kernel,
         Docgen_Module (Docgen_Module_Id).Backend,
         No_Project,
         Docgen_Module (Docgen_Module_Id).Options,
         True);

   exception
      when E : others =>
         Trace (Exception_Handle, E);
   end Choose_Menu_Project_Recursive;

   ----------------------
   -- Choose_Menu_File --
   ---------------------

   procedure Choose_Menu_File
    (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      File : aliased constant Virtual_File :=
        Select_File
          (Title             => -"Generate Documentation For",
           Parent            => Get_Current_Window (Kernel),
           Use_Native_Dialog => Get_Pref (Use_Native_Dialogs),
           Kind              => Open_File,
           File_Pattern      => "*;*.ad?;{*.c,*.h,*.cpp,*.cc,*.C}",
           Pattern_Name      => -"All files;Ada files;C/C++ files",
           History           => Get_History (Kernel));
   begin
      if File /= GNATCOLL.VFS.No_File then
         Generate
           (Kernel,
            Docgen_Module (Docgen_Module_Id).Backend,
            File,
            Docgen_Module (Docgen_Module_Id).Options);
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
   end Choose_Menu_File;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Tools    : constant String := '/' & (-"Tools");
      Generate : constant String := '/' & (-"_Documentation");
      Command  : Interactive_Command_Access;
   begin
      Docgen_Module_Id := new Docgen_Module_Record;
      Register_Module
        (Module                => Docgen_Module_Id,
         Kernel                => Kernel,
         Module_Name           => "Docgen",
         Priority              => GPS.Kernel.Modules.Default_Priority);

      --  ??? Only one backend for now. If several backends are created,
      --  a backend selection mechanism will be needed.
      Docgen_Module (Docgen_Module_Id).Backend := new HTML_Backend_Record;

      Docgen_Module (Docgen_Module_Id).Generate_Body_Files := Create
        (Get_Preferences (Kernel),
         Name    => "Doc-Process-Body",
         Default => False,
         Page    => -"Documentation",
         Doc     => -"Whether body files should be processed",
         Label   => -"Process body files");

      Docgen_Module (Docgen_Module_Id).Comments_Filter := Create
        (Get_Preferences (Kernel),
         Name    => "Doc-Filter-Comments",
         Default => "",
         Page    => -"Documentation",
         Doc     =>
             -("Regular expression used to filter comments. Matching parts" &
               " of the comments will be ignored."),
         Label   => -"Comments filter regexp");

      Docgen_Module (Docgen_Module_Id).Show_Private_Entities := Create
        (Get_Preferences (Kernel),
         Name    => "Doc-Show-Private",
         Default => False,
         Page    => -"Documentation",
         Doc     => -"Whether Docgen should show private entities",
         Label   => -"Show private entities");

      Docgen_Module (Docgen_Module_Id).Show_References := Create
        (Get_Preferences (Kernel),
         Name    => "Doc-References",
         Default => False,
         Page    => -"Documentation",
         Doc     =>
            -("Whether Docgen should compute references (e.g. call graph)"),
         Label   => -"Call graph");

      Docgen_Module (Docgen_Module_Id).Process_Up_To_Date_Only := Create
        (Get_Preferences (Kernel),
         Name    => "Doc-Up-To-Date-Only",
         Default => True,
         Page    => -"Documentation",
         Doc     =>
            -("Whether Docgen should only document files with up-to-date"
            & " cross ref informations."),
         Label   => -"Up-to-date files only");

      Docgen_Module (Docgen_Module_Id).Spawn_Browser := Create
        (Get_Preferences (Kernel),
         Name    => "Doc-Spawn-Browser",
         Default => True,
         Page    => -"Documentation",
         Doc     =>
         -("Whether Docgen should spawn a browser after having generated"
           & " the documentation."),
         Label   => -"Spawn a browser");
      Docgen_Module (Docgen_Module_Id).Gen_Comment_Xref := Create
        (Get_Preferences (Kernel),
         Name    => "Doc-Generate-Comment-Xref",
         Default => True,
         Page    => -"Documentation",
         Doc     =>
         -("Whether Docgen should analyze the comments and try to find entity "
           & "names in them."),
         Label   => -"Find xrefs in comments");

      Add_Hook
        (Kernel, Preferences_Changed_Hook,
         Wrapper (On_Preferences_Changed'Access),
         Name => "docgen.on_preferences_changed");
      On_Preferences_Changed (Kernel);

      Command := new Generate_Project_Command;
      Register_Contextual_Menu
        (Kernel, "Generate project documentation",
         Label  => "Documentation/Generate for %p",
         Action => Command,
         Filter => Lookup_Filter (Kernel, "Project only"));

      Command := new Generate_Project_Command;
      Generate_Project_Command (Command.all).Recursive := True;
      Register_Contextual_Menu
        (Kernel, "Generate project documentation recursive",
         Label  => "Documentation/Generate for %p and subprojects",
         Action => Command,
         Filter => Lookup_Filter (Kernel, "Project only"));

      Command := new Generate_File_Command;
      Register_Contextual_Menu
        (Kernel, "Generate file documentation",
         Label  => "Documentation/Generate for %f",
         Action => Command,
         Filter => Lookup_Filter (Kernel, "File")
                     and Create (Language => "ada"));

      Register_Menu
        (Kernel, Tools, "_Documentation", Callback => null,
         Ref_Item => -"Consoles", Add_Before => False);

      Register_Menu
        (Kernel,
         Tools & Generate,
         -"Generate _project",
         Callback => Choose_Menu_Project'Access);

      Register_Menu
        (Kernel,
         Tools & Generate,
         -"Generate project & _subprojects",
         Callback => Choose_Menu_Project_Recursive'Access);

      Register_Menu
        (Kernel,
         Tools & Generate,
         -"Generate _current file",
         Callback => Choose_Menu_Current_File'Access);

      Register_Menu
        (Kernel,
         Tools & Generate,
         -"Generate _for ...",
         Callback => Choose_Menu_File'Access);

      Register_Commands (Kernel);
   end Register_Module;

end Docgen2_Module;
