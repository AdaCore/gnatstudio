-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003-2005                       --
--                              AdaCore                              --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
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

with Glide_Kernel;             use Glide_Kernel;
with Glide_Kernel.Contexts;    use Glide_Kernel.Contexts;
with Glide_Kernel.Preferences; use Glide_Kernel.Preferences;
with Glide_Kernel.Project;     use Glide_Kernel.Project;
with Glide_Kernel.MDI;         use Glide_Kernel.MDI;
with Glide_Kernel.Modules;     use Glide_Kernel.Modules;
with Glide_Kernel.Hooks;       use Glide_Kernel.Hooks;
with Glide_Kernel.Standard_Hooks; use Glide_Kernel.Standard_Hooks;
with Glide_Intl;               use Glide_Intl;
with Glib.Object;              use Glib.Object;
with VFS;                      use VFS;
with Docgen.Work_On_File;      use Docgen.Work_On_File;
with Docgen;                   use Docgen;
with Entities;                 use Entities;
with Traces;                   use Traces;
with Ada.Exceptions;           use Ada.Exceptions;
with Gtkada.File_Selector;     use Gtkada.File_Selector;
with Projects;                 use Projects;
with Glib;                     use Glib;
with Glib.Generic_Properties;
with Glib.Properties.Creation; use Glib.Properties.Creation;
with Docgen;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Projects.Registry;         use Projects.Registry;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Docgen.Backend.HTML;       use Docgen.Backend.HTML;
with Commands.Interactive;      use Commands, Commands.Interactive;

package body Docgen_Module is

   Me : constant Debug_Handle := Create ("Docgen");

   type Docgen_Module_Record is new Module_ID_Record with record
      --  Docgen preferences

      Type_Generated_File   : Param_Spec_Enum;
      --  Type of the generated file (html, texi...)

      Generate_Body_Files   : Param_Spec_Boolean;
      --  Create also the body documentation

      Ignore_Some_Comments  : Param_Spec_Boolean;
      --  Ignore all comments with "--!"

      Comments_Before       : Param_Spec_Boolean;
      --  Find doc comments for entities before the entity itself

      Show_Private_Entities : Param_Spec_Boolean;
      --  Show also private entities

      Show_References       : Param_Spec_Boolean;
      --  True if the program should search for the references
      --  Adding information like "subprogram called by..."

      Link_All_References   : Param_Spec_Boolean;
      --  Should links be created to entities whose declaration files
      --  aren't being processed

      Process_Tagged_Types  : Param_Spec_Boolean;
      --  True to we want to build a list with all tagged
      --  types declared in the list of files we are processing. For each
      --  tagged types we indicate its parent and children (if they exist)

      Options : All_Options;
      --  Group all the preferences

      HTML_Backend : Docgen.Backend.Backend_Handle;
      --  A backend suitable for generating HTML output
   end record;
   type Docgen_Module is access all Docgen_Module_Record'Class;

   package Type_Api_Doc_Properties is new
     Glib.Generic_Properties.Generic_Enumeration_Property
       ("Type_Api_Doc", Type_Api_Doc);

   procedure Set_Options
     (Type_Of_File_P         : Type_Api_Doc := HTML;
      Process_Body_Files_P   : Boolean := False;
      Ignorable_Comments_P   : Boolean := False;
      Comments_Above_P       : Boolean := False;
      Show_Private_P         : Boolean := False;
      References_P           : Boolean := False;
      One_Doc_File_P         : Boolean := False;
      Link_All_P             : Boolean := False;
      Tagged_Types_P         : Boolean := False);
   --  Set new options or reset options
   --
   --  - Type_Of_File_P is the type of the generated file (html, texi...)
   --  - Process_Body_Files_P indicates if we create also documentation
   --    for body files.
   --  - Ignorable_Comments_P indicates if we ignore all comments with "--!"
   --  - Comments_Above_P says if we generate doc comments for entities above
   --    the header.
   --  - Show_Private_P indicates if we show also private entities
   --  - References_P says if we add information like "subprogram called by..."
   --  - One_Doc_File_P says if we create documentation in only one
   --    file (only for texi)
   --  - Link_All_P indicates if links are created for entities whose
   --    declaration files aren't processed
   --  - Tagged_Types indicates if we make a list with all tagged
   --    types declared in the list of files we are processing. For each
   --    tagged types we indicate his parent and his children (if they exist).

   procedure Array2List
     (Kernel     : Kernel_Handle;
      Tab        : VFS.File_Array_Access;
      List       : in out Type_Source_File_Table.HTable;
      Doc_Suffix : String);
   --  Create a list of files with those contained in the array

   procedure On_Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class);
   --  Called when the preferences have changed

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

   procedure Generate_Project
     (Kernel    : Kernel_Handle;
      Project   : Project_Type := No_Project;
      Recursive : Boolean);
   --  Generate the doc for the project
   --  If Recursive is true, it generates the direct sources of
   --  the project and also the sources from imported projects

   procedure Choose_Menu_File
      (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  In order to choose a file and generate its documentation
   --  It calls Generate_File

   procedure Generate_File
     (Kernel : Kernel_Handle;
      File   : Virtual_File);
   --  In order to generate the documentation of one file

   ------------------------------------------
   -- main procedure for the documentation --
   ------------------------------------------

   function Get_Backend return Docgen.Backend.Backend_Handle;
   --  Return the backend to use given the current options

   procedure Generate
     (Kernel : Kernel_Handle;
      List   : in out Type_Source_File_Table.HTable;
      Backend : access Docgen.Backend.Backend'Class);
   --  With the list of source files, it generates the documentation

   --------------
   -- Commands --
   --------------

   type Generate_Project_Command
     is new Interactive_Command with record
      Recursive : Boolean := False;
     end record;
   function Execute
     (Command : access Generate_Project_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Generate_File_Command
     is new Interactive_Command with null record;
   function Execute
     (Command : access Generate_File_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   -----------------
   -- Get_Options --
   -----------------

   procedure Get_Options (My_Options : in out All_Options) is
   begin
      My_Options :=
        (Type_Of_File       =>
           Docgen_Module (Docgen_Module_ID).Options.Type_Of_File,
         Process_Body_Files =>
           Docgen_Module (Docgen_Module_ID).Options.Process_Body_Files,
         Ignorable_Comments =>
           Docgen_Module (Docgen_Module_ID).Options.Ignorable_Comments,
         Comments_Above     =>
           Docgen_Module (Docgen_Module_ID).Options.Comments_Above,
         Show_Private       =>
           Docgen_Module (Docgen_Module_ID).Options.Show_Private,
         References         =>
           Docgen_Module (Docgen_Module_ID).Options.References,
         One_Doc_File       =>
           Docgen_Module (Docgen_Module_ID).Options.One_Doc_File,
         Link_All           =>
           Docgen_Module (Docgen_Module_ID).Options.Link_All,
         Tagged_Types       =>
           Docgen_Module (Docgen_Module_ID).Options.Tagged_Types);
   end Get_Options;

   -----------------
   -- Set_Options --
   -----------------

   procedure Set_Options
     (Type_Of_File_P       : Type_Api_Doc := HTML;
      Process_Body_Files_P : Boolean := False;
      Ignorable_Comments_P : Boolean := False;
      Comments_Above_P     : Boolean := False;
      Show_Private_P       : Boolean := False;
      References_P         : Boolean := False;
      One_Doc_File_P       : Boolean := False;
      Link_All_P           : Boolean := False;
      Tagged_Types_P       : Boolean := False) is
   begin
      Docgen_Module (Docgen_Module_ID).Options.Type_Of_File := Type_Of_File_P;
      Docgen_Module (Docgen_Module_ID).Options.Process_Body_Files :=
        Process_Body_Files_P;
      Docgen_Module (Docgen_Module_ID).Options.Ignorable_Comments :=
        Ignorable_Comments_P;
      Docgen_Module (Docgen_Module_ID).Options.Comments_Above :=
        Comments_Above_P;
      Docgen_Module (Docgen_Module_ID).Options.Show_Private :=
        Show_Private_P;
      Docgen_Module (Docgen_Module_ID).Options.References :=
        References_P;
      Docgen_Module (Docgen_Module_ID).Options.One_Doc_File :=
        One_Doc_File_P;
      Docgen_Module (Docgen_Module_ID).Options.Link_All :=
        Link_All_P;
      Docgen_Module (Docgen_Module_ID).Options.Tagged_Types :=
        Tagged_Types_P;
   end Set_Options;

   ----------------
   -- Array2List --
   ----------------

   procedure Array2List
     (Kernel     : Kernel_Handle;
      Tab        : VFS.File_Array_Access;
      List       : in out Type_Source_File_Table.HTable;
      Doc_Suffix : String)
   is
      File   : aliased Virtual_File;
      Source : Source_File;
      Is_Spec : Boolean;
   begin
      for J in 1 .. Tab'Length loop
         File := Tab (J);

         Is_Spec := Is_Spec_File (Kernel, File);

         if Docgen_Module (Docgen_Module_ID).Options.Process_Body_Files
           or else Is_Spec
         then
            Source := Get_Or_Create
              (Db           => Get_Database (Kernel),
               File         => File,
               Allow_Create => True);
            Type_Source_File_Table.Set
              (List,
               Source,
               (Package_Name  => new String'(Get_Unit_Name (Source)),
                Doc_File_Name => new String'
                  (Get_Doc_File_Name (File, Doc_Suffix)),
                Is_Spec       => Is_Spec));
         end if;
      end loop;
   end Array2List;

   ----------------------------
   -- On_Preferences_Changed --
   ----------------------------

   procedure On_Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class) is
   begin
      Set_Options
        (Type_Api_Doc'Val
           (Get_Pref
              (Kernel, Docgen_Module (Docgen_Module_ID).Type_Generated_File)),
         Get_Pref
           (Kernel, Docgen_Module (Docgen_Module_ID).Generate_Body_Files),
         Get_Pref
           (Kernel, Docgen_Module (Docgen_Module_ID).Ignore_Some_Comments),
         Get_Pref
           (Kernel, Docgen_Module (Docgen_Module_ID).Comments_Before),
         Get_Pref
           (Kernel, Docgen_Module (Docgen_Module_ID).Show_Private_Entities),
         Get_Pref (Kernel, Docgen_Module (Docgen_Module_ID).Show_References),
         Get_Pref
           (Kernel, Docgen_Module (Docgen_Module_ID).Link_All_References),
         Get_Pref
           (Kernel, Docgen_Module (Docgen_Module_ID).Process_Tagged_Types));
   end On_Preferences_Changed;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Generate_Project_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
   begin
      Generate_Project
        (Get_Kernel (Context.Context),
         Project_Information (File_Selection_Context_Access (Context.Context)),
         Command.Recursive);
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Generate_File_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
   begin
      Generate_File
        (Get_Kernel (Context.Context),
         File_Information (File_Selection_Context_Access (Context.Context)));
      return Commands.Success;
   end Execute;

   ------------------------------
   -- Choose_Menu_Current_File --
   ------------------------------

   procedure Choose_Menu_Current_File
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Context : constant Selection_Context_Access :=
        Get_Current_Context (Kernel);
      File    : aliased Virtual_File;

   begin
      if Context.all in File_Selection_Context'Class
        and then Has_File_Information (File_Selection_Context_Access (Context))
      then
         File := File_Information (File_Selection_Context_Access (Context));

         if File /= VFS.No_File then
            Generate_File (Kernel, File);
         end if;
      end if;
   end Choose_Menu_Current_File;

   -------------------------
   -- Choose_Menu_Project --
   -------------------------

   procedure Choose_Menu_Project
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      Generate_Project (Kernel, No_Project, False);
   end Choose_Menu_Project;

   -----------------------------------
   -- Choose_Menu_Project_Recursive --
   -----------------------------------

   procedure Choose_Menu_Project_Recursive
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      Generate_Project (Kernel, No_Project, True);
   end Choose_Menu_Project_Recursive;

   ----------------------
   -- Generate_Project --
   ----------------------

   procedure Generate_Project
     (Kernel    : Kernel_Handle;
      Project   : Project_Type := No_Project;
      Recursive : Boolean)
   is
      Sources : VFS.File_Array_Access;
      Source_File_List : Type_Source_File_Table.HTable;
      B       : constant Docgen.Backend.Backend_Handle := Get_Backend;
      P       : Project_Type := Project;
      Context : Selection_Context_Access;

   begin
      if P = No_Project then
         Context := Get_Current_Context (Kernel);
         if Context.all in File_Selection_Context'Class
           and then Has_Project_Information
             (File_Selection_Context_Access (Context))
         then
            P := Project_Information
              (File_Selection_Context_Access (Context));
         else
            P := Get_Project (Kernel);
         end if;
      end if;

      --  To save time, parse everything that we'll need in advance
      --  ??? Doesn't work, since the call graph for instance will require that
      --  more files be parsed (try generating doc for traces.ads)
      Trace (Me, "Parsing files");
      Parse_All_LI_Information (Kernel, P, Recursive => Recursive);
      Trace (Me, "Generating HTML files");

      Sources := Get_Source_Files (P, Recursive);
      Array2List (Kernel, Sources, Source_File_List,
                  Docgen.Backend.Get_Extension (B));
      Generate (Kernel, Source_File_List, B);
      VFS.Unchecked_Free (Sources);
      Type_Source_File_Table.Reset (Source_File_List);

      Trace (Me, "Done generating for project");

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
   end Generate_Project;

   ----------------------
   -- Choose_Menu_File --
   ---------------------

   procedure Choose_Menu_File
    (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      File  : aliased Virtual_File :=
        Select_File
          (Title             => -"Generate Documentation For",
           Parent            => Get_Current_Window (Kernel),
           Use_Native_Dialog => Get_Pref (Kernel, Use_Native_Dialogs),
           Kind              => Unspecified,
           History           => Get_History (Kernel));

   begin
      if File /= VFS.No_File then
         Generate_File (Kernel, File);
      end if;
   end Choose_Menu_File;

   -------------------
   -- Generate_File --
   -------------------

   procedure Generate_File
     (Kernel     : Kernel_Handle;
      File       : Virtual_File)
   is
      Source_File_List : Type_Source_File_Table.HTable;
      Body_File        : Virtual_File;
      Is_Spec          : constant Boolean := Is_Spec_File (Kernel, File);
      Process_Body     : constant Boolean :=
         Docgen_Module (Docgen_Module_ID).Options.Process_Body_Files;
      Source           : Source_File;
      B         : constant Docgen.Backend.Backend_Handle := Get_Backend;
      Doc_Suffix       : constant String :=
         Docgen.Backend.Get_Extension (B);

   begin
      if not Is_Spec and then not Process_Body then
         return;
      end if;

      Source := Get_Or_Create
        (Db           => Get_Database (Kernel),
         File         => File,
         Allow_Create => True);
      Update_Xref (Source);

      Type_Source_File_Table.Set
        (Source_File_List,
         Source,
         (Package_Name  => new String'(Get_Unit_Name (Source)),
          Doc_File_Name => new String'
            (Get_Doc_File_Name (File, Doc_Suffix)),
          Is_Spec       => Is_Spec_File (Kernel, File)));

      if Is_Spec and then Process_Body then
         Body_File := Create
           (Other_File_Base_Name
              (Get_Project_From_File
                 (Project_Registry (Get_Registry (Kernel).all),
                  File),
               File),
            Kernel,
            Use_Object_Path => False);
         Source := Get_Or_Create
           (Db           => Get_Database (Kernel),
            File         => Body_File,
            Allow_Create => True);
         if Body_File /= No_File then
            Type_Source_File_Table.Set
              (Source_File_List,
               Source,
               (Package_Name => new String'(Get_Unit_Name (Source)),
                Doc_File_Name => new String'
                  (Get_Doc_File_Name (Body_File, Doc_Suffix)),
                Is_Spec      => Is_Spec_File (Kernel, Body_File)));
         end if;
      end if;

      Generate (Kernel, Source_File_List, B);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
   end Generate_File;

   -----------------
   -- Get_Backend --
   -----------------

   function Get_Backend return Docgen.Backend.Backend_Handle is
   begin
      case Docgen_Module (Docgen_Module_ID).Options.Type_Of_File is
         when HTML =>
            return Docgen_Module (Docgen_Module_ID).HTML_Backend;

            --  ???
            --       when TEXI =>
            --          return Docgen_Module (Docgen_Module_ID).TEXI_Backend

         when others =>
            return null;
      end case;
   end Get_Backend;

   --------------
   -- Generate --
   --------------

   procedure Generate
     (Kernel : Kernel_Handle;
      List   : in out Type_Source_File_Table.HTable;
      Backend : access Docgen.Backend.Backend'Class)
   is
      use Docgen.Backend;
   begin
      Push_State (Kernel, Busy);

      --  We override old documentations which has the same format and
      --  which has been already processed.
      --  Documentation for new files is added.

      if not Is_Directory
        (Get_Doc_Directory (Backend, Kernel))
      then
         Make_Dir (Get_Doc_Directory (Backend, Kernel));
      end if;

      Process_Files
        (Backend,
         List,
         Kernel,
         Docgen_Module (Docgen_Module_ID).Options);
      Type_Source_File_Table.Reset (List);

      --  <frameset> is not supported by internal HTML viewer. Users will
      --  have to configure GPS so that it uses an external browser if they
      --  want a smooth docgen integration.

      Open_Html
        (Kernel,
         Filename => Create
           (Full_Filename =>
              Get_Doc_Directory (Backend, Kernel)
            & "index" & Get_Extension (Backend)));

      Pop_State (Kernel);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         Pop_State (Kernel);
   end Generate;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Tools    : constant String := '/' & (-"Tools");
      Generate : constant String := '/' & (-"_Documentation");
      Command  : Interactive_Command_Access;
   begin
      Docgen_Module_ID := new Docgen_Module_Record;
      Docgen_Module (Docgen_Module_ID).HTML_Backend := new Backend_HTML;

      Register_Module
        (Module      => Docgen_Module_ID,
         Kernel      => Kernel,
         Module_Name => "Docgen",
         Priority    => Default_Priority);

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
         Filter => Lookup_Filter (Kernel, "File"));

      Docgen_Module (Docgen_Module_ID).Type_Generated_File
        := Param_Spec_Enum
        (Type_Api_Doc_Properties.Gnew_Enum
           (Name    => "Doc-Output",
            Default => HTML,
            Blurb   => -"Choose the kind of documentation generated",
            Nick    => -"Output"));
      Register_Property
        (Kernel,
         Param_Spec (Docgen_Module (Docgen_Module_ID).Type_Generated_File),
         -"Documentation");

      Docgen_Module (Docgen_Module_ID).Generate_Body_Files
        := Param_Spec_Boolean
        (Gnew_Boolean
          (Name    => "Doc-Process-Body",
           Default => False,
           Blurb   => -"Whether body files should be processed",
           Nick    => -"Process body files"));
      Register_Property
        (Kernel,
         Param_Spec (Docgen_Module (Docgen_Module_ID).Generate_Body_Files),
         -"Documentation");

      Docgen_Module (Docgen_Module_ID).Ignore_Some_Comments
        := Param_Spec_Boolean
        (Gnew_Boolean
          (Name    => "Doc-Ignore-Special-Comments",
           Default => False,
           Blurb   =>
             -("Whether Docgen should ignore all comments with --!"),
           Nick    => -"Ignore comments with --!"));
      Register_Property
        (Kernel,
         Param_Spec (Docgen_Module (Docgen_Module_ID).Ignore_Some_Comments),
         -"Documentation");

      Docgen_Module (Docgen_Module_ID).Comments_Before
        := Param_Spec_Boolean
        (Gnew_Boolean
          (Name    => "Doc-Comments-Before",
           Default => False,
           Blurb   =>
             -("Whether comments are found before corresponding entities"),
           Nick    => -"Comments before entities"));
      Register_Property
        (Kernel,
         Param_Spec (Docgen_Module (Docgen_Module_ID).Comments_Before),
         -"Documentation");

      Docgen_Module (Docgen_Module_ID).Show_Private_Entities
        := Param_Spec_Boolean
        (Gnew_Boolean
          (Name    => "Doc-Show-Private",
           Default => False,
           Blurb   => -"Whether Docgen should show private entities",
           Nick    => -"Show private entities"));
      Register_Property
        (Kernel,
         Param_Spec (Docgen_Module (Docgen_Module_ID).Show_Private_Entities),
         -"Documentation");

      Docgen_Module (Docgen_Module_ID).Show_References := Param_Spec_Boolean
        (Gnew_Boolean
          (Name    => "Doc-References",
           Default => False,
           Blurb   =>
             -("Whether Docgen should compute references (e.g. call graph)"),
           Nick    => -"Compute references"));
      Register_Property
        (Kernel,
         Param_Spec (Docgen_Module (Docgen_Module_ID).Show_References),
         -"Documentation");

      Docgen_Module (Docgen_Module_ID).Link_All_References :=
        Param_Spec_Boolean
          (Gnew_Boolean
            (Name    => "Doc-Xref-All",
             Default => False,
             Blurb   =>
               -"Links for entities declared in files which are not processed",
             Nick    => -"Create all links"));
      Register_Property
        (Kernel,
         Param_Spec (Docgen_Module (Docgen_Module_ID).Link_All_References),
         -"Documentation");

      Docgen_Module (Docgen_Module_ID).Process_Tagged_Types :=
        Param_Spec_Boolean
          (Gnew_Boolean
            (Name    => "Doc-Tagged",
             Default => False,
             Blurb   =>
               -"List of tagged types declared in processed files",
             Nick    => -"List tagged types"));
      Register_Property
        (Kernel,
         Param_Spec (Docgen_Module (Docgen_Module_ID).Process_Tagged_Types),
         -"Documentation");

      Add_Hook
        (Kernel, Preferences_Changed_Hook, On_Preferences_Changed'Access);
      On_Preferences_Changed (Kernel);

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
   end Register_Module;

end Docgen_Module;
