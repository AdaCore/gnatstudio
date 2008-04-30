-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2003-2007                      --
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

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;
with GNAT.Strings;              use GNAT.Strings;

with Glib;                      use Glib;
with Glib.Object;               use Glib.Object;
with Glib.Properties.Creation;  use Glib.Properties.Creation;
with Glib.Xml_Int;              use Glib.Xml_Int;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Dialog;                use Gtk.Dialog;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Radio_Button;          use Gtk.Radio_Button;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Tooltips;              use Gtk.Tooltips;
with Gtk.Widget;                use Gtk.Widget;
with Gtkada.File_Selector;      use Gtkada.File_Selector;
with Gtkada.Dialogs;            use Gtkada.Dialogs;

with Commands.Interactive;      use Commands, Commands.Interactive;
with Docgen_Registry;           use Docgen_Registry;
with Docgen.Backend.Text;       use Docgen.Backend; use Docgen.Backend.Text;
with Docgen.Work_On_File;       use Docgen.Work_On_File;
with Entities;                  use Entities;
with GPS.Kernel.Console;        use GPS.Kernel.Console;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with Language_Handlers;         use Language_Handlers;
with Language.Ada;              use Language, Language.Ada;
with Projects;                  use Projects;
with Projects.Registry;         use Projects.Registry;
with Templates_Parser;          use Templates_Parser;
with Traces;                    use Traces;
with GNATCOLL.VFS;                       use GNATCOLL.VFS;

package body Docgen_Module is

   Me : constant Debug_Handle := Create ("Docgen");

   Docgen_Module_Id : GPS.Kernel.Modules.Module_ID;

   type Supported_Backends is
     array (Natural range <>) of Docgen.Backend.Backend_Handle;
   type Supported_Backends_Access is access Supported_Backends;

   type Docgen_Module_Record is new Module_ID_Record with record
      --  Docgen preferences

      Generate_Body_Files   : Param_Spec_Boolean;
      --  Create also the body documentation

      Ignore_Some_Comments  : Param_Spec_Boolean;
      --  Ignore all comments with "--!"

      Show_Private_Entities : Param_Spec_Boolean;
      --  Show also private entities

      Show_References       : Param_Spec_Boolean;
      --  True if the program should search for the references
      --  Adding information like "subprogram called by..."

      Link_All_References   : Param_Spec_Boolean;
      --  Should links be created to entities whose declaration files
      --  aren't being processed

      Process_Tagged_Types  : Param_Spec_Boolean;
      --  True if we want to build a list with all tagged
      --  types declared in the list of files we are processing. For each
      --  tagged types we indicate its parent and children (if they exist)

      Options : All_Options;
      --  Group all the preferences

      Backends : Supported_Backends_Access;
      --  The backends suitable for generating output
   end record;
   type Docgen_Module is access all Docgen_Module_Record'Class;

   procedure Customize
     (Module : access Docgen_Module_Record;
      File   : GNATCOLL.VFS.Virtual_File;
      Node   : Glib.Xml_Int.Node_Ptr;
      Level  : Customization_Level);
   --  See inherited documentation

   procedure Set_Options
     (Process_Body_Files_P : Boolean := False;
      Ignorable_Comments_P : Boolean := False;
      Show_Private_P       : Boolean := False;
      References_P         : Boolean := False;
      Link_All_P           : Boolean := False;
      Tagged_Types_P       : Boolean := False);
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
   --  - Link_All_P indicates if links are created for entities whose
   --    declaration files aren't processed
   --  - Tagged_Types indicates if we make a list with all tagged
   --    types declared in the list of files we are processing. For each
   --    tagged types we indicate his parent and his children (if they exist).

   procedure Array2List
     (Kernel        : Kernel_Handle;
      Tab           : GNATCOLL.VFS.File_Array_Access;
      List          : in out Type_Source_File_Table.HTable;
      Doc_Suffix    : String;
      Nb_Files      : out Natural);
   --  Create a list of files with those contained in the array.
   --  If Remove_Bodies is true, only specs are put in the list.
   --  The list returned contains Nb_Files.

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
   --  Generate documentation for a project.
   --  If Recursive is true, documentation is generated for the project and
   --  the imported ones.

   procedure Choose_Menu_File
      (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  In order to choose a file and generate its documentation
   --  It calls Generate_File

   procedure Generate_File
     (Kernel : Kernel_Handle;
      File   : Virtual_File);
   --  Generate documentation for a single file

   -------------------------------------------
   -- Main procedures for the documentation --
   -------------------------------------------

   function Get_Backend
     (Kernel : Kernel_Handle) return Docgen.Backend.Backend_Handle;
   --  Return the backend to use given the current options.

   procedure Generate
     (Kernel   : Kernel_Handle;
      List     : in out Type_Source_File_Table.HTable;
      Nb_Files : Natural;
      Backend  : access Docgen.Backend.Backend'Class);
   --  Generate documentation for files in List.

   --------------
   -- Commands --
   --------------

   type Generate_Project_Command is new Interactive_Command with record
      Recursive : Boolean := False;
   end record;

   function Execute
     (Command : access Generate_Project_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Generate_File_Command is new Interactive_Command with null record;

   function Execute
     (Command : access Generate_File_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   -----------------
   -- Get_Options --
   -----------------

   procedure Get_Options (My_Options : in out All_Options) is
   begin
      My_Options :=
        (Process_Body_Files =>
           Docgen_Module (Docgen_Module_Id).Options.Process_Body_Files,
         Ignorable_Comments =>
           Docgen_Module (Docgen_Module_Id).Options.Ignorable_Comments,
         Show_Private       =>
           Docgen_Module (Docgen_Module_Id).Options.Show_Private,
         References         =>
           Docgen_Module (Docgen_Module_Id).Options.References,
         Link_All           =>
           Docgen_Module (Docgen_Module_Id).Options.Link_All,
         Tagged_Types       =>
           Docgen_Module (Docgen_Module_Id).Options.Tagged_Types);
   end Get_Options;

   -----------------
   -- Set_Options --
   -----------------

   procedure Set_Options
     (Process_Body_Files_P : Boolean := False;
      Ignorable_Comments_P : Boolean := False;
      Show_Private_P       : Boolean := False;
      References_P         : Boolean := False;
      Link_All_P           : Boolean := False;
      Tagged_Types_P       : Boolean := False) is
   begin
      Docgen_Module (Docgen_Module_Id).Options.Process_Body_Files :=
        Process_Body_Files_P;
      Docgen_Module (Docgen_Module_Id).Options.Ignorable_Comments :=
        Ignorable_Comments_P;
      Docgen_Module (Docgen_Module_Id).Options.Show_Private :=
        Show_Private_P;
      Docgen_Module (Docgen_Module_Id).Options.References :=
        References_P;
      Docgen_Module (Docgen_Module_Id).Options.Link_All :=
        Link_All_P;
      Docgen_Module (Docgen_Module_Id).Options.Tagged_Types :=
        Tagged_Types_P;
   end Set_Options;

   ----------------
   -- Array2List --
   ----------------

   procedure Array2List
     (Kernel        : Kernel_Handle;
      Tab           : GNATCOLL.VFS.File_Array_Access;
      List          : in out Type_Source_File_Table.HTable;
      Doc_Suffix    : String;
      Nb_Files      : out Natural)
   is
      File    : aliased Virtual_File;
      Source  : Source_File;
      Is_Spec : Boolean;
      Is_Ada  : Boolean;
   begin
      Nb_Files := 0;

      for J in 1 .. Tab'Length loop
         File := Tab (J);

         Is_Spec := Is_Spec_File (Kernel, File);

         --  ??? For now, only generate documentation for Ada files

         Is_Ada :=
           Get_Language_From_File (Get_Language_Handler (Kernel), File) =
           Ada_Lang;

         if not Is_Ada then
            Trace (Me, Base_Name (File) & " is not an Ada file. " &
                   "No documentation will be generated.");

         elsif Docgen_Module (Docgen_Module_Id).Options.Process_Body_Files
           or else Is_Spec
         then
            Nb_Files := Nb_Files + 1;
            Source := Get_Or_Create
              (Db           => Get_Database (Kernel),
               File         => File,
               Allow_Create => True);
            Type_Source_File_Table.Set
              (List,
               Source,
               (Unit_Name     => new String'(Get_Unit_Name (Source)),
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
     (Kernel : access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Kernel);
   begin
      Set_Options
        (Get_Pref (Docgen_Module (Docgen_Module_Id).Generate_Body_Files),
         Get_Pref (Docgen_Module (Docgen_Module_Id).Ignore_Some_Comments),
         Get_Pref (Docgen_Module (Docgen_Module_Id).Show_Private_Entities),
         Get_Pref (Docgen_Module (Docgen_Module_Id).Show_References),
         Get_Pref (Docgen_Module (Docgen_Module_Id).Link_All_References),
         Get_Pref (Docgen_Module (Docgen_Module_Id).Process_Tagged_Types));
   end On_Preferences_Changed;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Generate_Project_Command;
      Context : Interactive_Command_Context) return Command_Return_Type is
   begin
      Generate_Project
        (Get_Kernel (Context.Context),
         Project_Information (Context.Context),
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
         File_Information (Context.Context));
      return Commands.Success;
   end Execute;

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
      B                : constant Docgen.Backend.Backend_Handle :=
        Get_Backend (Kernel);
      Sources          : GNATCOLL.VFS.File_Array_Access;
      Source_File_List : Type_Source_File_Table.HTable;
      Nb_Files         : Natural;
      P                : Project_Type := Project;
      Context          : Selection_Context;

   begin
      if B = null then
         return;
      end if;

      if P = No_Project then
         Context := Get_Current_Context (Kernel);
         if Has_Project_Information (Context) then
            P := Project_Information (Context);
         else
            P := Get_Project (Kernel);
         end if;
      end if;

      --  To save time, parse everything that we'll need in advance
      --  ??? Doesn't work, since the call graph for instance will require that
      --  more files be parsed (try generating doc for traces.ads)
      Trace (Me, "Parsing files");
      Parse_All_LI_Information (Kernel, P, Recursive => Recursive);

      Trace (Me, "Generating files for " & B.Output_Description.Name.all);

      Sources := Get_Source_Files (P, Recursive);

      Array2List (Kernel, Sources, Source_File_List,
                  Docgen.Backend.Get_Extension (B), Nb_Files);

      if Nb_Files > 0 then
         Generate (Kernel, Source_File_List, Nb_Files, B);

         --  ??? The following commented line should probably be moved to
         --  to the function in charge of finalizing the files processing
         --  since it is executed as a background command whose execution
         --  is up to the task manager. It's therefore better not to free
         --  structures passed to it as parameters.

         --  VFS.Unchecked_Free (Sources);

         Trace (Me, "Done generating for project");
      else
         Trace (Me, "No source file to generate documentation for.");
      end if;
   exception
      when E : others => Trace (Exception_Handle, E);
   end Generate_Project;

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
         Generate_File (Kernel, File);
      end if;
   end Choose_Menu_File;

   ---------------
   -- Customize --
   ---------------

   procedure Customize
     (Module : access Docgen_Module_Record;
      File   : GNATCOLL.VFS.Virtual_File;
      Node   : Glib.Xml_Int.Node_Ptr;
      Level  : Customization_Level)
   is
      pragma Unreferenced (Level);
   begin
      if Node.Tag.all = "docgen_backend" then
         --  This is a docgen backend description node

         declare
            Format     : constant UTF8_String :=
                           Get_Attribute (Node, "format", "text");
            Name       : constant UTF8_String :=
                           Get_Attribute (Node, "name", "");
            N          : Node_Ptr := Node.Child;
            Out_Format : Output_Description;
         begin
            if Format = "text" then
               Out_Format.Format := Docgen_Registry.Text;
            else
               Out_Format.Format := Binary;
            end if;

            if Name = "" then
               Console.Insert
                 (Get_Kernel (Module.all),
                  -"DOCGEN: missing backend name in " & Full_Name (File).all,
                  Mode => Error);
            else
               Out_Format.Name := new String'(Name);
            end if;

            while N /= null loop
               if N.Tag.all = "description" then
                  Out_Format.Description := new String'(N.Value.all);
               elsif N.Tag.all = "extension" then
                  Out_Format.Extension := new String'(N.Value.all);
               elsif N.Tag.all = "file_header_template" then
                  Out_Format.Entities_Templates (File_Header_Kind) :=
                    new String'(N.Value.all);
               elsif N.Tag.all = "file_footer_template" then
                  Out_Format.Entities_Templates (File_Footer_Kind) :=
                    new String'(N.Value.all);
               elsif N.Tag.all = "comment_template" then
                  Out_Format.Entities_Templates (Comment_Kind) :=
                    new String'(N.Value.all);
               elsif N.Tag.all = "keyword_template" then
                  Out_Format.Entities_Templates (Keyword_Kind) :=
                    new String'(N.Value.all);
               elsif N.Tag.all = "string_template" then
                  Out_Format.Entities_Templates (String_Kind) :=
                    new String'(N.Value.all);
               elsif N.Tag.all = "char_template" then
                  Out_Format.Entities_Templates (Char_Kind) :=
                    new String'(N.Value.all);
               elsif N.Tag.all = "subtitle_template" then
                  Out_Format.Entities_Templates (Subtitle_Kind) :=
                    new String'(N.Value.all);
               elsif N.Tag.all = "package_desc_template" then
                  Out_Format.Entities_Templates (Package_Desc_Kind) :=
                    new String'(N.Value.all);
               elsif N.Tag.all = "package_template" then
                  Out_Format.Entities_Templates (Package_Kind) :=
                    new String'(N.Value.all);
               elsif N.Tag.all = "with_template" then
                  Out_Format.Entities_Templates (With_Kind) :=
                    new String'(N.Value.all);
               elsif N.Tag.all = "variable_template" then
                  Out_Format.Entities_Templates (Variable_Kind) :=
                    new String'(N.Value.all);
               elsif N.Tag.all = "exception_template" then
                  Out_Format.Entities_Templates (Exception_Kind) :=
                    new String'(N.Value.all);
               elsif N.Tag.all = "type_template" then
                  Out_Format.Entities_Templates (Type_Kind) :=
                    new String'(N.Value.all);
               elsif N.Tag.all = "tagged_type_template" then
                  Out_Format.Entities_Templates (Tagged_Type_Kind) :=
                    new String'(N.Value.all);
               elsif N.Tag.all = "calls_references_template" then
                  Out_Format.Entities_Templates (Calls_References_Kind) :=
                    new String'(N.Value.all);
               elsif N.Tag.all = "caller_references_template" then
                  Out_Format.Entities_Templates (Caller_References_Kind) :=
                    new String'(N.Value.all);
               elsif N.Tag.all = "entity_template" then
                  Out_Format.Entities_Templates (Entity_Kind) :=
                    new String'(N.Value.all);
               elsif N.Tag.all = "subprogram_template" then
                  Out_Format.Entities_Templates (Subprogram_Kind) :=
                    new String'(N.Value.all);
               elsif N.Tag.all = "header_template" then
                  Out_Format.Entities_Templates (Header_Kind) :=
                    new String'(N.Value.all);
               elsif N.Tag.all = "footer_template" then
                  Out_Format.Entities_Templates (Footer_Kind) :=
                    new String'(N.Value.all);
               elsif N.Tag.all = "private_header_template" then
                  Out_Format.Entities_Templates (Private_Header_Kind) :=
                    new String'(N.Value.all);
               elsif N.Tag.all = "main_frame_template" then
                  Out_Format.Entities_Templates (Main_Frame_Kind) :=
                    new String'(N.Value.all);
               elsif N.Tag.all = "unit_index_header_template" then
                  Out_Format.Entities_Templates (Unit_Index_Header_Kind) :=
                    new String'(N.Value.all);
               elsif N.Tag.all = "subprogram_index_header_template" then
                  Out_Format.Entities_Templates
                    (Subprogram_Index_Header_Kind) := new String'(N.Value.all);
               elsif N.Tag.all = "type_index_header_template" then
                  Out_Format.Entities_Templates (Type_Index_Header_Kind) :=
                    new String'(N.Value.all);
               elsif N.Tag.all = "tagged_type_index_header_template" then
                  Out_Format.Entities_Templates
                    (Tagged_Type_Index_Header_Kind) :=
                    new String'(N.Value.all);
               elsif N.Tag.all = "item_index_template" then
                  Out_Format.Entities_Templates (Item_Index_Kind) :=
                    new String'(N.Value.all);
               elsif N.Tag.all = "private_index_header_template" then
                  Out_Format.Entities_Templates (Private_Index_Header_Kind) :=
                    new String'(N.Value.all);
               elsif N.Tag.all = "public_index_header_template" then
                  Out_Format.Entities_Templates (Public_Index_Header_Kind) :=
                    new String'(N.Value.all);
               elsif N.Tag.all = "index_footer_template" then
                  Out_Format.Entities_Templates (Index_Footer_Kind) :=
                    new String'(N.Value.all);
               elsif N.Tag.all = "tagged_type_index_template" then
                  Out_Format.Entities_Templates (Tagged_Type_Index_Kind) :=
                    new String'(N.Value.all);
               elsif N.Tag.all = "block_template" then
                  Out_Format.Entities_Templates (Block_Kind) :=
                    new String'(N.Value.all);
               elsif N.Tag.all = "link_template" then
                  Out_Format.Entities_Templates (Link_Kind) :=
                    new String'(N.Value.all);
               elsif N.Tag.all = "description_template" then
                  Out_Format.Entities_Templates (Description_Kind) :=
                    new String'(N.Value.all);
               end if;

               N := N.Next;
            end loop;

            Insert (Out_Format);
         end;
      end if;
   end Customize;

   -------------------
   -- Generate_File --
   -------------------

   procedure Generate_File
     (Kernel : Kernel_Handle;
      File   : Virtual_File)
   is
      B                   : constant Docgen.Backend.Backend_Handle :=
                              Get_Backend (Kernel);
      Is_Spec             : constant Boolean := Is_Spec_File (Kernel, File);
      Process_Body        : constant Boolean :=
                              Docgen_Module
                                (Docgen_Module_Id).Options.Process_Body_Files;
      Source_File_List    : Type_Source_File_Table.HTable;
      Other_File          : Virtual_File;
      Other_File_Basename : GNAT.Strings.String_Access;
      Source              : Source_File;
      Nb_Files            : Natural := 1;

      Reporter            : constant File_Error_Reporter :=
                              new Docgen_Error_Reporter_Record'
                                (Kernel, False);

   begin
      if Get_Language_From_File (Get_Language_Handler (Kernel), File) /=
        Ada_Lang
      then
         Insert
           (Kernel,
            Display_Base_Name (File) &
            (-" is not an Ada file, cannot generate documentation"),
            Add_LF => True,
            Mode   => Error);

         return;
      end if;

      if B = null then
         Insert
           (Kernel,
            -"No backend selected for the documentation generator",
            Add_LF => True,
            Mode   => Error);

         return;
      end if;

      Source := Get_Or_Create
        (Db           => Get_Database (Kernel),
         File         => File,
         Allow_Create => True);

      Update_Xref (Source, Reporter);

      if Docgen_Error_Reporter_Record (Reporter.all).Called then
         return;
      end if;

      Type_Source_File_Table.Set
        (Source_File_List,
         Source,
         (Unit_Name     => new String'(Get_Unit_Name (Source)),
          Doc_File_Name => new String'
            (Get_Doc_File_Name (File, Docgen.Backend.Get_Extension (B))),
          Is_Spec       => Is_Spec_File (Kernel, File)));

      Other_File_Basename := new String'
        (Other_File_Base_Name
           (Get_Project_From_File
              (Project_Registry (Get_Registry (Kernel).all),
               File),
            File));

      if (Is_Spec and then Process_Body)
        or else not Is_Spec
      then
         Other_File := Create
           (Other_File_Basename.all,
            Kernel,
           Use_Object_Path => False);

         if Is_Regular_File (Other_File) then
            Source := Get_Or_Create
              (Db           => Get_Database (Kernel),
               File         => Other_File,
               Allow_Create => True);

            if Other_File /= No_File then
               Nb_Files := 2;
               Type_Source_File_Table.Set
                 (Source_File_List,
                  Source,
                  (Unit_Name     => new String'(Get_Unit_Name (Source)),
                   Doc_File_Name => new String'
                     (Get_Doc_File_Name
                        (Other_File,
                         Docgen.Backend.Get_Extension (B))),
                   Is_Spec       => Is_Spec_File (Kernel, Other_File)));
            end if;
         end if;
      end if;

      Free (Other_File_Basename);

      Generate (Kernel, Source_File_List, Nb_Files, B);

   exception
      when E : others => Trace (Exception_Handle, E);
   end Generate_File;

   -----------------
   -- Get_Backend --
   -----------------

   function Get_Backend
     (Kernel : Kernel_Handle) return Docgen.Backend.Backend_Handle
   is
      Backends : Supported_Backends_Access :=
                   Docgen_Module (Docgen_Module_Id).Backends;
      Button   : Message_Dialog_Buttons;
      pragma Unreferenced (Button);
   begin
      if Length = 0 then
         Button := Message_Dialog
           (Msg         => "There is no document backend configured.",
            Dialog_Type => Warning,
            Title       => -"Documentation backend",
            Buttons     => Button_OK,
            Parent      => Get_Current_Window (Kernel));
         return null;
      end if;

      --  Check if we need to create the backend handles now

      if Backends = null then
         --  Initialize the backends now

         Backends := new Supported_Backends (1 .. Length);

         Docgen_Module (Docgen_Module_Id).Backends := Backends;

         for K in Docgen_Module (Docgen_Module_Id).Backends'Range loop
            --  ??? We have a single textual backend, so we do not check for
            --  binary ones for now.
            Docgen_Module (Docgen_Module_Id).Backends (K) :=
              new Docgen.Backend.Text.Backend (Get (K));
         end loop;
      end if;

      if Backends'Length = 1 then
         --  A single backend is configured, used it
         return Backends (Backends'First);

      else
         --  Open a dialog and let the user select the backend to use

         declare
            Dialog : Gtk_Dialog;
            Label  : Gtk_Label;
            Button : Gtk_Widget;
            Radio  : array (1 .. Length) of Gtk_Radio_Button;
            Tmp    : Gtk_Radio_Button;
            Tips   : Gtk_Tooltips;
            B_Des  : Output_Description_Access;
            pragma Unreferenced (Button);

         begin
            Gtk_New
              (Dialog, -"Select formats",
               Get_Main_Window (Kernel), Modal or Destroy_With_Parent);

            Gtk_New (Label, -"Supported documentation format.");
            Pack_Start (Get_Vbox (Dialog), Label, Expand => False);

            Gtk_New (Tips);

            for K in Radio'Range loop
               B_Des := Get (K);

               if K = 1 then
                  Gtk_New (Radio (K), Label => -"Format " & B_Des.Name.all);
               else
                  Gtk_New (Radio (K), Tmp, -"Format " & B_Des.Name.all);
               end if;

               Set_Tip (Tips, Radio (K), B_Des.Description.all);

               Tmp := Radio (K);

               Set_Active (Radio (K), K = 1);
               Pack_Start (Get_Vbox (Dialog), Radio (K), Expand => False);
            end loop;

            Button := Add_Button (Dialog, Stock_Execute, Gtk_Response_OK);
            Button := Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);

            Show_All (Dialog);

            if Run (Dialog) = Gtk_Response_OK then
               --  Check which radio button has been selected

               for K in Radio'Range loop
                  if Get_Active (Radio (K)) then
                     Destroy (Dialog);
                     return Backends (K);
                  end if;
               end loop;
            end if;

            Destroy (Dialog);
            return null;
         end;
      end if;
   end Get_Backend;

   --------------
   -- Generate --
   --------------

   procedure Generate
     (Kernel   : Kernel_Handle;
      List     : in out Type_Source_File_Table.HTable;
      Nb_Files : Natural;
      Backend  : access Docgen.Backend.Backend'Class) is
   begin
      Push_State (Kernel, Busy);

      --  We override old documentations which has the same format and
      --  which has been already processed.
      --  Documentation for new files is added.

      if not GNAT.OS_Lib.Is_Directory
        (Get_Doc_Directory (Backend, Kernel))
      then
         Make_Dir (Get_Doc_Directory (Backend, Kernel));
      end if;

      Process_Files
        (Backend, Kernel,
         List,
         Nb_Files,
         Docgen_Module (Docgen_Module_Id).Options);

      Release_Cache;

      Pop_State (Kernel);

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         Pop_State (Kernel);
   end Generate;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
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
         Filter => Action_Filter (Lookup_Filter (Kernel, "File") and
                                  Create (Language => "ada")));

      Docgen_Module (Docgen_Module_Id).Generate_Body_Files
        := Param_Spec_Boolean
        (Gnew_Boolean
          (Name    => "Doc-Process-Body",
           Default => False,
           Blurb   => -"Whether body files should be processed",
           Nick    => -"Process body files"));
      Register_Property
        (Kernel,
         Param_Spec (Docgen_Module (Docgen_Module_Id).Generate_Body_Files),
         -"Documentation");

      Docgen_Module (Docgen_Module_Id).Ignore_Some_Comments
        := Param_Spec_Boolean
        (Gnew_Boolean
          (Name    => "Doc-Ignore-Special-Comments",
           Default => False,
           Blurb   =>
             -("Whether Docgen should ignore all comments with --!"),
           Nick    => -"Ignore comments with --!",
           Flags   => Param_Readable)); --  ??? For 3.0.0
      Register_Property
        (Kernel,
         Param_Spec (Docgen_Module (Docgen_Module_Id).Ignore_Some_Comments),
         -"Documentation");

      Docgen_Module (Docgen_Module_Id).Show_Private_Entities :=
        Param_Spec_Boolean
          (Gnew_Boolean
               (Name    => "Doc-Show-Private",
                Default => False,
                Blurb   => -"Whether Docgen should show private entities",
                Nick    => -"Show private entities"));
      Register_Property
        (Kernel,
         Param_Spec (Docgen_Module (Docgen_Module_Id).Show_Private_Entities),
         -"Documentation");

      Docgen_Module (Docgen_Module_Id).Show_References := Param_Spec_Boolean
        (Gnew_Boolean
          (Name    => "Doc-References",
           Default => False,
           Blurb   =>
             -("Whether Docgen should compute references (e.g. call graph)"),
            Nick    => -"Call graph"));
      Register_Property
        (Kernel,
         Param_Spec (Docgen_Module (Docgen_Module_Id).Show_References),
         -"Documentation");

      Docgen_Module (Docgen_Module_Id).Link_All_References :=
        Param_Spec_Boolean
          (Gnew_Boolean
            (Name    => "Doc-Xref-All",
             Default => False,
             Blurb   =>
               -"Links for entities declared in files which are not processed",
             Nick    => -"Create all links"));
      Register_Property
        (Kernel,
         Param_Spec (Docgen_Module (Docgen_Module_Id).Link_All_References),
         -"Documentation");

      Docgen_Module (Docgen_Module_Id).Process_Tagged_Types :=
        Param_Spec_Boolean
          (Gnew_Boolean
            (Name    => "Doc-Tagged",
             Default => False,
             Blurb   =>
               -"List of tagged types declared in processed files",
             Nick    => -"List tagged types"));
      Register_Property
        (Kernel,
         Param_Spec (Docgen_Module (Docgen_Module_Id).Process_Tagged_Types),
         -"Documentation");

      Add_Hook
        (Kernel, Preferences_Changed_Hook,
         Wrapper (On_Preferences_Changed'Access),
         Name => "docgen.on_preferences_changed");
      On_Preferences_Changed (Kernel);

      Register_Menu
        (Kernel, Tools, "_Documentation", Callback => null,
         Ref_Item => -"Compare", Add_Before => False);

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
