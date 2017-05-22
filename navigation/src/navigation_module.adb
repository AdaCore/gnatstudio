------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2002-2017, AdaCore                     --
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
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Calendar;               use Ada.Calendar;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Commands.Interactive;       use Commands, Commands.Interactive;
with GNAT.Strings;               use GNAT.Strings;
with GNATCOLL.Arg_Lists;         use GNATCOLL.Arg_Lists;
with GNATCOLL.Projects;          use GNATCOLL.Projects;
with GNATCOLL.Symbols;           use GNATCOLL.Symbols;
with GNATCOLL.Scripts;           use GNATCOLL.Scripts;
with GNATCOLL.Traces;            use GNATCOLL.Traces;
with GNATCOLL.Utils;             use GNATCOLL.Utils;
with GNATCOLL.VFS;               use GNATCOLL.VFS;
with GPS.Editors;                use GPS.Editors;
with GPS.Intl;                   use GPS.Intl;
with GPS.Kernel.Actions;         use GPS.Kernel.Actions;
with GPS.Kernel.Contexts;        use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;           use GPS.Kernel.Hooks;
with GPS.Kernel.Locations;       use GPS.Kernel.Locations;
with GPS.Kernel.Modules.UI;      use GPS.Kernel.Modules.UI;
with GPS.Kernel.Modules;         use GPS.Kernel.Modules;
with GPS.Kernel.Project;         use GPS.Kernel.Project;
with GPS.Kernel.Scripts;         use GPS.Kernel.Scripts;
with GPS.Kernel.Task_Manager;    use GPS.Kernel.Task_Manager;
with GPS.Markers;                use GPS.Markers;
with Glib.Object;                use Glib.Object;
with Glib;                       use Glib;
with Gtk.Widget;                 use Gtk.Widget;
with Language;                   use Language;
with XML_Parsers;                use XML_Parsers;
with XML_Utils;                  use XML_Utils;
with Language.Abstract_Language_Tree; use Language.Abstract_Language_Tree;

package body Navigation_Module is
   Me : constant Trace_Handle := Create ("Navigation");

   Max_Locations_In_History : constant := 200;
   --  Maximum number of locations stored in the history

   Navigation_Module_Name : constant String := "Navigation";
   Navigation_Module_ID : Module_ID;

   type Location_Marker_Array is array (Positive range <>) of Location_Marker;
   type Location_Marker_Array_Access is access Location_Marker_Array;

   type Action_And_Path is record
      Action : Action_Record_Access;
      Path   : Unbounded_String;
   end record;

   package File_To_Action is new Ada.Containers.Hashed_Maps
     (Key_Type        => Virtual_File,
      Element_Type    => Action_And_Path,
      Hash            => Full_Name_Hash,
      Equivalent_Keys => "=");
   use File_To_Action;

   package File_List is new Ada.Containers.Doubly_Linked_Lists (Virtual_File);
   use File_List;

   package Action_List is new Ada.Containers.Doubly_Linked_Lists
     (Action_And_Path);
   use Action_List;

   function "<" (Left, Right : Action_And_Path) return Boolean is
      (Left.Path < Right.Path);
   --  Compare based on Path

   package Alpha_Sort is new Action_List.Generic_Sorting;

   type Navigation_Module_Record is new Module_ID_Record with record
      Markers        : Location_Marker_Array_Access;
      --  The list of markers from the history of locations

      Current_Marker : Natural := 0;
      --  The current position in Markers

      Last_Marker    : Natural := 0;
      --  The last marker set in Markers

      Previous_Project : Virtual_File := No_File;
      Markers_File     : Virtual_File := No_File;
      --  File where history markers were loaded from

      ----------------------------------
      -- Handling of the Runtime menu --
      ----------------------------------

      Actions_With_A_Menu : Action_List.List;
      --  All the actions which currently provide a Runtime menu

      Runtime_Actions : File_To_Action.Map;
      --  A store of all the "Runtime menu" actions that have been created
   end record;
   type Navigation_Module is access all Navigation_Module_Record'Class;

   type Shell_Marker_Data is new Location_Marker_Data with record
      Script  : Scripting_Language;
      Command : GNAT.Strings.String_Access;
   end record;
   overriding function Go_To
     (Marker : not null access Shell_Marker_Data) return Boolean;
   overriding function To_String
     (Marker : not null access Shell_Marker_Data) return String;
   overriding function Save
     (Marker : not null access Shell_Marker_Data) return XML_Utils.Node_Ptr;
   overriding function Similar
     (Left  : not null access Shell_Marker_Data;
      Right : not null access Location_Marker_Data'Class) return Boolean
     is (False);
   overriding function Distance
     (Left  : not null access Shell_Marker_Data;
      Right : not null access Location_Marker_Data'Class) return Integer
     is (Integer'Last);

   function Create_Shell_Marker
     (Script  : access Scripting_Language_Record'Class;
      Command : String) return Location_Marker;
   --  Create a new marker associated with a shell command

   procedure Extract_Unit_Info
     (Kernel          : Kernel_Handle;
      File            : Virtual_File;
      Unit_Name       : out Symbol;
      Unit_Visibility : out Construct_Visibility);
   --  Extract unit name and visibility from a given file.
   --  This will return the first subprogram or package found in the file, so
   --  is mostly interesting on Ada sources.
   --  If the info cannot be extracted Unit_Name will be set to No_Symbol and
   --  Unit_Visibility to Visibility_Private.

   --------------------------------------------
   -- Commands for handling the Runtime menu --
   --------------------------------------------

   type Runtime_Processor_Command is new Interactive_Command with record
      Total_Files : Integer;
      --  Total number of runtime files

      Files_To_Process : File_List.List;
      --  The runtime files left to process

      Menus_To_Create : Action_List.List;
      --  The actions which have been created from the above files, for
      --  which menus still need to be created.
   end record;
   overriding function Execute
     (Command : access Runtime_Processor_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Open_File_Command is new Interactive_Command with record
      File : Virtual_File;
   end record;
   type Open_File_Command_Access is access all Open_File_Command'Class;
   overriding function Execute
     (Command : access Open_File_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Location_Marker_Array, Location_Marker_Array_Access);

   overriding procedure Destroy (Id : in out Navigation_Module_Record);
   --  Free memory associated to Id

   type Back_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Back_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Callbacks for the back buttons

   type Forward_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Forward_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Callbacks for the forward buttons

   type Goto_Other_File_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Goto_Other_File_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Open the spec if a body or separate is currently selected, and the spec
   --  otherwise.

   type Previous_Tag_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Previous_Tag_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Callback for the "previous result" action

   type Next_Tag_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Next_Tag_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Callback for the "next result" action

   type Start_Statement_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Start_Statement_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Callback for the "start statement" action

   type End_Statement_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access End_Statement_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Callback for the "end statement" action

   type Next_Subprogram_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Next_Subprogram_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Callback for the "next subprogram" action

   type Previous_Subprogram_Command
      is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Previous_Subprogram_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Callback for the "previous subprogram" action

   type Has_Forward_Navigation is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Has_Forward_Navigation;
      Context : Selection_Context) return Boolean;

   type Has_Back_Navigation is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Has_Back_Navigation;
      Context : Selection_Context) return Boolean;

   type Has_Other_File_Filter is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Has_Other_File_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean;
   --  True if the current file has a spec/body

   procedure Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Interactive command handler for the navigation module

   --  Interfaces to GPS commands used by some navigation procedures

   function Get_Current_Line
     (Kernel : Kernel_Handle;
      File   : Virtual_File) return Natural;
   --  Returns current line in File

   procedure Set_Current_Line
     (Kernel : Kernel_Handle;
      File   : Virtual_File;
      Line   : Natural;
      Center : Boolean := False);
   --  Set current File's line, if Center is True the cursor is centered
   --  on the view.

   function Get_Last_Line
     (Kernel : Kernel_Handle;
      File   : Virtual_File) return Natural;
   --  Returns last line index

   function Get_Block_Start
     (Kernel : Kernel_Handle;
      File   : Virtual_File;
      Line   : Natural) return Natural;
   --  Returns first line for block enclosing Line

   function Get_Block_End
     (Kernel : Kernel_Handle;
      File   : Virtual_File;
      Line   : Natural) return Natural;
   --  Returns last line for block enclosing Line

   function Get_Block_Type
     (Kernel : Kernel_Handle;
      File   : Virtual_File;
      Line   : Natural) return Language_Category;
   --  Returns type for block enclosing Line

   type On_Marker_Added_In_History is new Marker_Hooks_Function
      with null record;
   overriding procedure Execute
     (Self     : On_Marker_Added_In_History;
      Kernel   : not null access Kernel_Handle_Record'Class;
      Marker  : Location_Marker);
   --  Called when a new marker is added in the history

   procedure Move_In_Marker_History
     (Kernel    : access Kernel_Handle_Record'Class;
      Move_Back : Boolean);
   --  Move backward or forward in the list of markers. The effect is
   --  immediately visible in the GPS interface.

   procedure Go_To_Current_Marker;
   --  Go to the location pointed to by the current marker in the history

   procedure Save_History_Markers
     (Kernel      : access Kernel_Handle_Record'Class;
      Old_Project : Boolean := False);
   --  Save all locations to an XML file
   --  If Old_Project is True, save the locations from the previous project
   --  rather than from the current project.

   procedure Load_History_Markers
     (Kernel : access Kernel_Handle_Record'Class);
   --  Load all locations from an XML file

   type On_Project_Loaded is new Simple_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Project_Loaded;
      Kernel : not null access Kernel_Handle_Record'Class);
   --  Called when a project is loaded.

   type On_Desktop_Loaded is new Simple_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Desktop_Loaded;
      Kernel : not null access Kernel_Handle_Record'Class);
   --  Called when the desktop is loaded

   procedure Free (Markers : in out Location_Marker_Array_Access);
   --  Free allocated memory.

   procedure Regenerate_Runtime_Menu
     (Kernel : not null access Kernel_Handle_Record'Class);
   --  Regenerate the Runtime menu

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self     : On_Marker_Added_In_History;
      Kernel   : not null access Kernel_Handle_Record'Class;
      Marker   : Location_Marker)
   is
      pragma Unreferenced (Self, Kernel);
      Module : constant Navigation_Module :=
        Navigation_Module (Navigation_Module_ID);
   begin
      if Module.Markers = null then
         Module.Markers := new Location_Marker_Array
           (1 .. Max_Locations_In_History);
         Module.Current_Marker := 0;
         Module.Last_Marker := 0;
      end if;

      if Module.Current_Marker = 0
        or else not Similar (Marker, Module.Markers (Module.Current_Marker))
      then
         Module.Current_Marker := Module.Current_Marker + 1;

         if Module.Current_Marker > Module.Markers'Last then
            Module.Markers (Module.Markers'First .. Module.Markers'Last - 1) :=
              Module.Markers (Module.Markers'First + 1 .. Module.Markers'Last);
            Module.Current_Marker := Module.Markers'Last;
         end if;

         Module.Markers (Module.Current_Marker) := Marker;
         Module.Last_Marker := Module.Current_Marker;
      end if;
   end Execute;

   --------------------------
   -- Save_History_Markers --
   --------------------------

   procedure Save_History_Markers
     (Kernel      : access Kernel_Handle_Record'Class;
      Old_Project : Boolean := False)
   is
      M           : constant Navigation_Module :=
                      Navigation_Module (Navigation_Module_ID);
      Filename    : constant Virtual_File := M.Markers_File;
      File, Child, Project, Prev, Free_Me, Project_Nodes : Node_Ptr;

      Success     : Boolean;

      Project_File : Virtual_File;

      Error             : GNAT.Strings.String_Access;

      Current_Project : constant Virtual_File :=
        Get_Registry (Kernel).Tree.Root_Project.Project_Path;

   begin
      if Old_Project then
         Project_File := M.Previous_Project;
      else
         Project_File := Current_Project;
      end if;

      if M.Markers /= null
        and then Filename /= No_File
        and then Filename.Dir.Is_Directory
      then
         Trace (Me, "Saving " & Filename.Display_Full_Name);

         if Filename.Is_Regular_File then
            Parse (Filename, File, Error);
            Free (Error);
         end if;

         if File = null then
            File     := new Node;
            File.Tag := new String'("Locations");
         end if;

         --  Cleanup: remove all <Project> nodes which do not specify the
         --  project file.

         Child := File.Child;

         while Child /= null loop
            if Get_File_Child (Child, "file") = No_File then
               if Prev = null then
                  File.Child := Child.Next;
               else
                  Prev.Next := Child.Next;
               end if;
               Free_Me := Child;
            else
               Prev := Child;
            end if;
            Child := Child.Next;
            if Free_Me /= null then
               Free (Free_Me);
               Free_Me := null;
            end if;
         end loop;

         --  Find the right <Project> node

         Child := File.Child;

         while Child /= null loop
            if Child.Tag = null
              or else Child.Tag.all /= "Project"
            then
               if Prev = null then
                  File.Child := Child.Next;
               else
                  Prev.Next := Child.Next;
               end if;
               Free_Me := Child;
            else
               Prev := Child;

               if Get_File_Child (Child, "file") = Project_File then
                  Project := Child;
                  Project_Nodes := Project.Child;
                  Project_Nodes.Parent := null;
                  Project.Child := null;
                  exit;
               end if;
            end if;

            Child := Child.Next;
            if Free_Me /= null then
               Free (Free_Me);
               Free_Me := null;
            end if;
         end loop;

         if Project = null then
            Project := new Node;
            Project.Tag := new String'("Project");
            Add_File_Child (Project, "file", Project_File);
            Add_Child (File, Project);
         end if;

         for Index in M.Markers'First .. M.Last_Marker loop
            Child := Save (M.Markers (Index));
            if Child /= null then
               if Index = M.Current_Marker then
                  Set_Attribute (Child, "current", "true");
               end if;
               Add_Child (Project, Child, Append => True);
            end if;
         end loop;

         Print (File, Filename, Success);
         Free (File);
         Free (Project_Nodes);

         if not Success then
            Report_Preference_File_Error (Kernel, Filename);
         end if;
      end if;
   end Save_History_Markers;

   --------------------------
   -- Load_History_Markers --
   --------------------------

   procedure Load_History_Markers
     (Kernel : access Kernel_Handle_Record'Class)
   is
      M             : constant Navigation_Module :=
                        Navigation_Module (Navigation_Module_ID);
      Project_Tree  : constant GNATCOLL.Projects.Project_Tree_Access :=
                        Get_Registry (Kernel).Tree;

      Root_Project  : constant Project_Type := Project_Tree.Root_Project;
      Project_File  : constant Virtual_File := Root_Project.Project_Path;
      Base_Name     : constant Filesystem_String :=
                        Project_File.Base_Name (".gpr") & "-loc.xml";
      Filename      : Virtual_File;
      File, Child, Project : Node_Ptr;
      Marker        : Location_Marker;
      Err           : GNAT.Strings.String_Access;
   begin
      --  Keep markers only for ordinary projects (not empty nor default)

      if Get_Registry (Kernel).Tree.Status /= From_File then
         M.Markers_File := No_File;
         return;
      end if;

      Filename := Root_Project.Artifacts_Dir / Base_Name;
      M.Markers_File := Filename;

      if Is_Regular_File (Filename) then
         Trace (Me, "Loading " & Filename.Display_Full_Name);
         XML_Parsers.Parse (Filename, File, Err);

         if File = null then
            Insert (Kernel, Err.all, Mode => Error);
            Free (Err);

         else
            Child := File.Child;

            while Child /= null loop
               if Child.Tag.all = "Project"
                 and then Get_File_Child (Child, "file") = Project_File
               then
                  Project := Child;
                  exit;
               end if;

               Child := Child.Next;
            end loop;

            if Project /= null then
               Child := Project.Child;

               Free (M.Markers);

               M.Markers := new Location_Marker_Array
                 (1 .. Max_Locations_In_History);
               M.Current_Marker := 0;
               M.Last_Marker := 0;

               while Child /= null loop
                  Marker := Create_Marker (Kernel, Child);

                  if not Marker.Is_Null then
                     M.Last_Marker := M.Last_Marker + 1;
                     M.Markers (M.Last_Marker) := Marker;

                     if Get_Attribute (Child, "current", "false") = "true" then
                        M.Current_Marker := M.Last_Marker;
                     end if;

                     exit when M.Last_Marker = Max_Locations_In_History;
                  end if;

                  Child := Child.Next;
               end loop;
            end if;

            Free (File);
         end if;
      end if;
   end Load_History_Markers;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Project_Loaded;
      Kernel : not null access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Self);
   begin
      --  Save the previous history
      Save_History_Markers (Kernel, Old_Project => True);

      --  Load the history for the new project
      Load_History_Markers (Kernel);

      --  Create the Help -> Runtime menu for the new project
      Regenerate_Runtime_Menu (Kernel);
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Desktop_Loaded;
      Kernel : not null access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Self);
   begin
      Load_History_Markers (Kernel);
   end Execute;

   ----------------------------
   -- Move_In_Marker_History --
   ----------------------------

   procedure Move_In_Marker_History
     (Kernel    : access Kernel_Handle_Record'Class;
      Move_Back : Boolean)
   is
      pragma Unreferenced (Kernel);
      Module : constant Navigation_Module :=
                 Navigation_Module (Navigation_Module_ID);
   begin
      if Move_Back then
         if Module.Markers /= null
           and then Module.Current_Marker > Module.Markers'First
         then
            Module.Current_Marker := Module.Current_Marker - 1;
         end if;
      else
         if Module.Markers /= null
           and then Module.Current_Marker < Module.Last_Marker
         then
            Module.Current_Marker := Module.Current_Marker + 1;
         end if;
      end if;
   end Move_In_Marker_History;

   --------------------------
   -- Go_To_Current_Marker --
   --------------------------

   procedure Go_To_Current_Marker is
      Module : constant Navigation_Module :=
                 Navigation_Module (Navigation_Module_ID);
   begin
      if Module.Markers /= null
        and then Module.Current_Marker >= Module.Markers'First
        and then Module.Current_Marker <= Module.Last_Marker
      then
         if not Go_To (Module.Markers (Module.Current_Marker)) then
            Module.Markers (Module.Current_Marker .. Module.Last_Marker - 1) :=
              Module.Markers (Module.Current_Marker + 1 .. Module.Last_Marker);
            Module.Last_Marker := Module.Last_Marker - 1;
         end if;
      end if;
   end Go_To_Current_Marker;

   ----------------------
   -- Get_Current_Line --
   ----------------------

   function Get_Current_Line
     (Kernel : Kernel_Handle;
      File   : Virtual_File) return Natural
   is
      Editor : constant Editor_Buffer'Class :=
                 Kernel.Get_Buffer_Factory.Get (File);
      Location : constant Editor_Location'Class :=
                   GPS.Editors.Cursor (Current_View (Editor));

   begin
      return Line (Location);
   end Get_Current_Line;

   ----------------------
   -- Set_Current_Line --
   ----------------------

   procedure Set_Current_Line
     (Kernel : Kernel_Handle;
      File   : Virtual_File;
      Line   : Natural;
      Center : Boolean := False)
   is
      Editor   : constant Editor_Buffer'Class :=
                   Kernel.Get_Buffer_Factory.Get (File);
      Location : constant Editor_Location'Class :=
                   New_Location_At_Line (Editor, Line);
      Centering : Centering_Type := With_Margin;
   begin
      if Center then
         Centering := GPS.Editors.Center;
      end if;

      Cursor_Goto
        (Current_View (Editor),
         Location,
         Centering => Centering);
   end Set_Current_Line;

   -------------------
   -- Get_Last_Line --
   -------------------

   function Get_Last_Line
     (Kernel : Kernel_Handle;
      File   : Virtual_File) return Natural
   is
      Editor : constant Editor_Buffer'Class :=
                 Kernel.Get_Buffer_Factory.Get (File);
      Location : constant Editor_Location'Class :=
                   End_Of_Buffer (Editor);

   begin
      return Line (Location);
   end Get_Last_Line;

   -------------------
   -- Get_Block_End --
   -------------------

   function Get_Block_End
     (Kernel : Kernel_Handle;
      File   : Virtual_File;
      Line   : Natural) return Natural
   is
      Editor : constant Editor_Buffer'Class :=
                 Kernel.Get_Buffer_Factory.Get (File);
      Loc : constant Editor_Location'Class :=
         Editor.New_Location_At_Line (Line);
   begin
      return Loc.Block_End.Line;
   exception
      when Constraint_Error =>
         return 0;
   end Get_Block_End;

   ---------------------
   -- Get_Block_Start --
   ---------------------

   function Get_Block_Start
     (Kernel : Kernel_Handle;
      File   : Virtual_File;
      Line   : Natural) return Natural
   is
      Editor : constant Editor_Buffer'Class :=
        Kernel.Get_Buffer_Factory.Get (File);
      Loc : constant Editor_Location'Class :=
        Editor.New_Location_At_Line (Line);
   begin
      return Loc.Block_Start (Update_Tree => False).Line;
   exception
      when Constraint_Error =>
         return 0;
   end Get_Block_Start;

   --------------------
   -- Get_Block_Type --
   --------------------

   function Get_Block_Type
     (Kernel : Kernel_Handle;
      File   : Virtual_File;
      Line   : Natural) return Language_Category
   is
      Editor : constant Editor_Buffer'Class :=
        Kernel.Get_Buffer_Factory.Get (File);
      Loc : constant Editor_Location'Class :=
        Editor.New_Location_At_Line (Line);
   begin
      return Loc.Block_Type (Update_Tree => False);
   exception
      when Constraint_Error =>
         return Cat_Unknown;
   end Get_Block_Type;

   -----------
   -- Go_To --
   -----------

   overriding function Go_To
     (Marker : not null access Shell_Marker_Data) return Boolean
   is
      Errors : Boolean;
   begin
      Execute_Command
        (Script       => Marker.Script,
         CL           => Parse_String (Marker.Command.all,
           Command_Line_Treatment (Marker.Script)),
         Hide_Output  => True,
         Show_Command => False,
         Errors       => Errors);
      return not Errors;
   end Go_To;

   ---------------
   -- To_String --
   ---------------

   overriding function To_String
     (Marker : not null access Shell_Marker_Data) return String is
   begin
      return Marker.Command.all;
   end To_String;

   ----------
   -- Save --
   ----------

   overriding function Save
     (Marker : not null access Shell_Marker_Data) return XML_Utils.Node_Ptr
   is
      N : constant Node_Ptr := new Node;
   begin
      N.Tag   := new String'("shell_mark");
      N.Value := new String'(Marker.Command.all);
      return N;
   end Save;

   -------------------------
   -- Create_Shell_Marker --
   -------------------------

   function Create_Shell_Marker
     (Script  : access Scripting_Language_Record'Class;
      Command : String) return Location_Marker is
   begin
      return L : Location_Marker do
         L.Set (Shell_Marker_Data'
                  (Script  => Scripting_Language (Script),
                   Command => new String'(Command)));
      end return;
   end Create_Shell_Marker;

   ---------------------
   -- Command_Handler --
   ---------------------

   procedure Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      pragma Unreferenced (Command);
   begin
      Push_Marker_In_History
        (Get_Kernel (Data),
         Create_Shell_Marker
           (Script => Get_Script (Data),
            Command => Nth_Arg (Data, 1)));
   end Command_Handler;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Has_Other_File_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
      Kernel : constant Kernel_Handle := Get_Kernel (Context);
   begin
      if Has_File_Information (Context) then
         declare
            File       : constant Virtual_File := File_Information (Context);
            Other_File : constant Virtual_File :=
              Get_Registry (Kernel).Tree.Other_File (File);
         begin
            return Other_File /= No_File
              and then Other_File /= File;
         end;
      end if;
      return False;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Has_Back_Navigation;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter, Context);
      Module : constant Navigation_Module :=
                 Navigation_Module (Navigation_Module_ID);
   begin
      return Module.Markers /= null
        and then Module.Current_Marker > Module.Markers'First;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Has_Forward_Navigation;
      Context : Selection_Context) return Boolean
   is
      Module : constant Navigation_Module :=
                 Navigation_Module (Navigation_Module_ID);
      pragma Unreferenced (Filter, Context);
   begin
      return Module.Markers /= null
        and then Module.Current_Marker < Module.Last_Marker;
   end Filter_Matches_Primitive;

   ----------
   -- Free --
   ----------

   procedure Free (Markers : in out Location_Marker_Array_Access) is
   begin
      Unchecked_Free (Markers);
   end Free;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Back_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel  : constant Kernel_Handle := Get_Kernel (Context.Context);
   begin
      Move_In_Marker_History (Kernel, Move_Back => True);
      Go_To_Current_Marker;
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Forward_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel  : constant Kernel_Handle := Get_Kernel (Context.Context);
   begin
      Move_In_Marker_History (Kernel, Move_Back => False);
      Go_To_Current_Marker;
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Start_Statement_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel  : constant Kernel_Handle := Get_Kernel (Context.Context);
      File    : Virtual_File;
      Line    : Natural;           -- Current line being processed
      B_Start : Natural;           -- Block's first line
      B_Type  : Language_Category; -- Block's category
   begin
      if Has_File_Information (Context.Context)
        and then Has_Directory_Information (Context.Context)
      then
         File := File_Information (Context.Context);

         Line   := Get_Current_Line (Kernel, File);
         B_Type := Get_Block_Type (Kernel, File, Line);

         if B_Type in Construct_Category
           or else B_Type in Enclosing_Entity_Category
         then
            B_Start := Get_Block_Start (Kernel, File, Line);

            if B_Start /= 0 then
               if B_Start /= Line then
                  Set_Current_Line (Kernel, File, B_Start);

               else
                  --  We are already at the start of the block, look for
                  --  enclosing block.
                  declare
                     EB_Start : Natural;
                  begin
                     EB_Start := Get_Block_Start (Kernel, File, Line - 1);

                     if EB_Start /= 0 then
                        Set_Current_Line (Kernel, File, EB_Start);
                     end if;
                  end;
               end if;
            end if;
         end if;
      end if;
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access End_Statement_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel  : constant Kernel_Handle := Get_Kernel (Context.Context);
      File    : Virtual_File;
      Line    : Natural;           -- Current line being processed
      B_End   : Natural;           -- Block's first line
      B_Type  : Language_Category; -- Block's category
   begin
      if Has_File_Information (Context.Context)
        and then Has_Directory_Information (Context.Context)
      then
         File := File_Information (Context.Context);

         Line   := Get_Current_Line (Kernel, File);
         B_Type := Get_Block_Type (Kernel, File, Line);

         if B_Type in Construct_Category
           or else B_Type in Enclosing_Entity_Category
         then
            B_End := Get_Block_End (Kernel, File, Line);

            if B_End /= 0 then
               if B_End /= Line then
                  Set_Current_Line (Kernel, File, B_End);
               else
                  --  We are already at the end of the block, look for
                  --  enclosing block.
                  declare
                     EB_End : Natural;
                  begin
                     EB_End := Get_Block_End (Kernel, File, Line + 1);

                     if EB_End /= 0 then
                        Set_Current_Line (Kernel, File, EB_End);
                     end if;
                  end;
               end if;
            end if;
         end if;
      end if;
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Next_Subprogram_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel  : constant Kernel_Handle := Get_Kernel (Context.Context);
      File      : Virtual_File;
      Line      : Natural;           -- Current line being processed
      Last_Line : Natural;           -- Last line in the buffer
      B_Start   : Natural;           -- Block's first line
      B_Type    : Language_Category; -- Block's category
   begin
      if Has_File_Information (Context.Context)
        and then Has_Directory_Information (Context.Context)
      then
         File := File_Information (Context.Context);

         Line      := Get_Current_Line (Kernel, File);
         Last_Line := Get_Last_Line (Kernel, File);

         while Line < Last_Line loop
            Line := Line + 1;

            B_Start := Get_Block_Start (Kernel, File, Line);
            B_Type  := Get_Block_Type (Kernel, File, Line);

            exit when B_Start = Line and then B_Type in Subprogram_Category;
         end loop;

         if Line < Last_Line then
            --  A block has been found, set cursor to its first line
            Set_Current_Line (Kernel, File, Line, Center => True);
         end if;
      end if;
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Next_Tag_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel  : constant Kernel_Handle := Get_Kernel (Context.Context);
   begin
      Next_Item (Kernel, False);
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Previous_Subprogram_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel  : constant Kernel_Handle := Get_Kernel (Context.Context);
      File    : Virtual_File;
      Line    : Natural;           -- Current line being processed
      B_Start : Natural;           -- Block's first line
      B_Type  : Language_Category; -- Block's category
   begin
      if Has_File_Information (Context.Context)
        and then Has_Directory_Information (Context.Context)
      then
         File := File_Information (Context.Context);

         Line := Get_Current_Line (Kernel, File);

         while Line > 1 loop
            Line := Line - 1;

            B_Type  := Get_Block_Type (Kernel, File, Line);
            B_Start := Get_Block_Start (Kernel, File, Line);

            exit when B_Start = Line and then B_Type in Subprogram_Category;
         end loop;

         if Line > 1 then
            --  A block has been found, set cursor to its first line
            Set_Current_Line (Kernel, File, Line, Center => True);
         end if;
      end if;
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Previous_Tag_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel  : constant Kernel_Handle := Get_Kernel (Context.Context);
   begin
      Next_Item (Kernel, True);
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Goto_Other_File_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel  : constant Kernel_Handle := Get_Kernel (Context.Context);
   begin
      if Has_File_Information (Context.Context) then
         declare
            File : constant Virtual_File := File_Information (Context.Context);
            Other_File : constant Virtual_File :=
              Get_Registry (Kernel).Tree.Other_File (File);
         begin
            if Dir_Name (Other_File) /= "" then
               Open_File_Action_Hook.Run
                  (Kernel, File => Other_File,
                   Project => Project_Information (Context.Context),
                   Line => 0);
            else
               Trace (Me, "Other file not found for "
                      & File.Display_Full_Name);
            end if;
         end;
      else
         Insert (Kernel, -"There is no selected file", Mode => Error);
      end if;

      return Commands.Success;
   end Execute;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Src_Action_Context : constant Action_Filter :=
        Lookup_Filter (Kernel, "Source editor");
      Filter : Action_Filter;
   begin
      Navigation_Module_ID := new Navigation_Module_Record;

      Register_Module
        (Module       => Navigation_Module_ID,
         Kernel       => Kernel,
         Module_Name  => Navigation_Module_Name,
         Priority     => High_Priority);

      Register_Command
        (Kernel, "add_location_command",
         Minimum_Args => 1,
         Maximum_Args => Natural'Last,
         Handler      => Command_Handler'Access);

      Filter := new Has_Other_File_Filter;
      Register_Action
        (Kernel, "goto other file",
         Command     => new Goto_Other_File_Command,
         Description => -"Open the corresponding spec or body file",
         Filter      => Filter and Src_Action_Context);

      Register_Action
        (Kernel, "start of statement", new Start_Statement_Command,
         -"Move to the beginning of the current statement",
         Category   => -"Editor",
         Filter     => Src_Action_Context);

      Register_Action
        (Kernel, "end of statement", new End_Statement_Command,
         -"Move to the end of the current statement",
         Category   => -"Editor",
         Filter     => Src_Action_Context);

      Register_Action
        (Kernel, "previous subprogram", new Previous_Subprogram_Command,
         -"Move to the previous subprogram",
         Category   => -"Editor",
         Filter     => Src_Action_Context);

      Register_Action
        (Kernel, "next subprogram", new Next_Subprogram_Command,
         -"Move to the next subprogram",
         Category   => -"Editor",
         Filter     => Src_Action_Context);

      Register_Action
        (Kernel, "previous tag", new Previous_Tag_Command,
         -"Move to the previous message from the Locations window",
         Category   => -"Locations");

      Register_Action
        (Kernel, "next tag", new Next_Tag_Command,
         -"Move to the next message from the Locations window",
         Category   => -"Locations");

      Filter := new Has_Back_Navigation;
      Kernel.Register_Filter (Filter, "has back navigation");
      Register_Action
        (Kernel, "backward locations history", new Back_Command,
         Description => -"Goto previous location",
         Filter      => Filter,
         Category    => -"Editor",
         Icon_Name   => "gps-backward-symbolic");

      Filter := new Has_Forward_Navigation;
      Kernel.Register_Filter (Filter, "has forward navigation");
      Register_Action
        (Kernel, "forward locations history", new Forward_Command,
         Description => -"Goto next location",
         Filter      => Filter,
         Category    => -"Editor",
         Icon_Name   => "gps-forward-symbolic");

      Marker_Added_To_History_Hook.Add (new On_Marker_Added_In_History);
      Project_View_Changed_Hook.Add (new On_Project_Loaded);
      Desktop_Loaded_Hook.Add (new On_Desktop_Loaded);
   end Register_Module;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Id : in out Navigation_Module_Record) is
   begin
      Save_History_Markers (Get_Kernel (Id));
      Free (Id.Markers);
   end Destroy;

   -----------------------
   -- Extract_Unit_Info --
   -----------------------

   procedure Extract_Unit_Info
     (Kernel          : Kernel_Handle;
      File            : Virtual_File;
      Unit_Name       : out Symbol;
      Unit_Visibility : out Construct_Visibility)
   is
      Tree   : constant Semantic_Tree'Class :=
        Kernel.Get_Abstract_Tree_For_File ("INFO", File);
      Root   : constant Semantic_Node_Array'Class := Tree.Root_Nodes;
   begin
      Unit_Name       := No_Symbol;
      Unit_Visibility := Visibility_Private;

      for J in 1 .. Root.Length loop
         if Root.Get (J).Category in
           Cat_Package | Cat_Procedure | Cat_Function
         then
            Unit_Name       := Root.Get (J).Name;
            Unit_Visibility := Root.Get (J).Visibility;
            return;
         end if;
      end loop;
   end Extract_Unit_Info;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Runtime_Processor_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Context);
      Module : constant Navigation_Module :=
        Navigation_Module (Navigation_Module_ID);
      Kernel : constant Kernel_Handle := Module.Get_Kernel;

      procedure Process_File (File : Virtual_File);
      --  Process one file: look for its corresponding menu, creating the
      --  action if needed.

      ------------------
      -- Process_File --
      ------------------

      procedure Process_File (File : Virtual_File) is
         Base : constant Filesystem_String := File.Base_Name;
         AP   : Action_And_Path;
      begin
         --  We are not interested in bodies
         if Base'Length < 4
           or else Base (Base'Last - 3 .. Base'Last) /= ".ads"
         then
            return;
         end if;

         --  Do we have an action in store for this file?

         if Module.Runtime_Actions.Contains (File) then
            --  If so, use the stored action
            AP := Module.Runtime_Actions.Element (File);
         else
            --  Otherwise, create the action now

            declare
               Command    : Open_File_Command_Access;
               Name       : Symbol := No_Symbol;
               Visibility : Construct_Visibility;
            begin
               Extract_Unit_Info (Kernel, File, Name, Visibility);

               if Name = No_Symbol
                 or else Visibility /= Visibility_Public
               then
                  return;
               end if;

               Command := new Open_File_Command;
               Command.File := File;

               declare
                  Unique_Name : constant String :=
                    "open runtime file " & (+File.Full_Name.all);
               begin
                  Register_Action
                    (Kernel      => Kernel,
                     Name        => Unique_Name,
                     Command     => Command,
                     Description => Unique_Name,
                     Category    => "Runtime Menu");

                  AP.Action := Lookup_Action (Kernel, Unique_Name);
                  AP.Path   := To_Unbounded_String (Get (Name).all);

                  Replace (AP.Path, ".", "/");
                  Replace (AP.Path, "_", "__");

                  --  Add the action we have just created to the store
                  Module.Runtime_Actions.Insert (Key      => File,
                                                 New_Item => AP);
               end;
            end;
         end if;

         Command.Menus_To_Create.Append (AP);
      end Process_File;

      Start  : constant Time := Clock;
      Max_Idle_Duration : constant Duration := 0.05;

      File : Virtual_File;
      C    : Action_List.Cursor;
      Pre  : Unbounded_String;
      Ele  : Action_And_Path;
   begin
      if Command.Files_To_Process.Is_Empty then
         --  If we're seeing this at the beginning of Execute, that means we
         --  are running for the first time: fill the list of files.
         declare
            Files : constant File_Array :=
              Get_Registry (Kernel).Environment.Predefined_Source_Files;
         begin
            for F of Files loop
               Command.Files_To_Process.Append (F);
            end loop;
            Command.Total_Files := Integer (Command.Files_To_Process.Length);
         end;
      end if;

      while not Command.Files_To_Process.Is_Empty loop
         if Clock - Start > Max_Idle_Duration then
            Command.Set_Progress
              ((Activity => Running,
                Current  => Command.Total_Files -
                  Integer (Command.Files_To_Process.Length),
                Total    => Command.Total_Files));

            --  We spent too much time already: come back at another time.
            return Execute_Again;
         end if;

         --  Process the first file
         File := Command.Files_To_Process.First_Element;
         Command.Files_To_Process.Delete_First;
         Process_File (File);
      end loop;

      --  If we reach this, this means we didn't timeout on the loop above,
      --  and therfore that all files have been processed: create the menus
      --  now.

      Alpha_Sort.Sort (Command.Menus_To_Create);

      --  Reverse iterate on the sorted list to filter out and change
      --  some paths

      Pre := Null_Unbounded_String;
      C := Command.Menus_To_Create.Last;
      while Has_Element (C) loop
         Ele := Element (C);
         declare
            Path : constant String := To_String (Ele.Path);
            Has_Slash : Boolean;
         begin
            --  Filter out "/Ada" for instance
            if not (Starts_With (Path, "Ada")
                      or else Starts_With (Path, "GNAT")
                      or else Starts_With (Path, "System")
                      or else Starts_With (Path, "Interfaces"))
            then
               Ele.Path := Null_Unbounded_String;
               Command.Menus_To_Create.Replace_Element (C, Ele);
            else
               --  Transform items of the form /Ada/Containers
               --  into /Ada/Containers/<Containers>

               if Pre /= Null_Unbounded_String
                 and then Starts_With (To_String (Pre), Path)
               then
                  Has_Slash := False;
                  for J in reverse Path'Range loop
                     if Path (J) = '/' then
                        Has_Slash := True;
                        Ele.Path := To_Unbounded_String
                            (Path & "/<" & Path (J + 1 .. Path'Last) & ">");

                        Command.Menus_To_Create.Replace_Element (C, Ele);
                        exit;
                     end if;
                  end loop;

                  --  Transform elements of the form "Ada" into "Ada/<Ada>"
                  if not Has_Slash then
                     Ele.Path := To_Unbounded_String
                       (Path & "/<" & Path & ">");

                     Command.Menus_To_Create.Replace_Element (C, Ele);
                  end if;
               end if;
            end if;
         end;
         Pre := Ele.Path;
         Previous (C);
      end loop;

      declare
         Runtime : constant String := Kernel.Get_Runtime;
         Root    : constant String :=
           (if Runtime = "" then
               "/Help/GNAT Runtime/"
            else
               "/Help/GNAT Runtime (" & Runtime & ")/");
      begin
         for AP of Command.Menus_To_Create loop
            if AP.Path /= Null_Unbounded_String then
               Register_Menu
                 (Kernel     => Kernel,
                  Path       => Root & To_String (AP.Path),
                  Action     => Get_Name (AP.Action));
               Module.Actions_With_A_Menu.Append (AP);
            end if;
         end loop;
      end;

      return Success;
   end Execute;

   -----------------------------
   -- Regenerate_Runtime_Menu --
   -----------------------------

   procedure Regenerate_Runtime_Menu
     (Kernel : not null access Kernel_Handle_Record'Class)
   is
      Module : constant Navigation_Module :=
        Navigation_Module (Navigation_Module_ID);
      Task_Name : constant String := "refreshing Runtime menu";
      C : Interactive_Command_Access;
   begin
      --  Interrupt running task, if needed
      Interrupt_Queue (Kernel   => Kernel,
                       Queue_Id => Task_Name);

      --  Remove old runtime menu
      for AP of Module.Actions_With_A_Menu loop
         Remove_UI_For_Action (Kernel => Kernel,
                               Action => Get_Name (AP.Action));
      end loop;
      Module.Actions_With_A_Menu.Clear;

      C := new Runtime_Processor_Command;

      --  Schedule computation of new runtime menu
      Launch_Background_Command
        (Kernel            => Kernel,
         Command           => C,
         Active            => False,
         Show_Bar          => True,
         Queue_Id          => Task_Name,
         Block_Exit        => False,
         Start_Immediately => False);
   end Regenerate_Runtime_Menu;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Open_File_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Module : constant Navigation_Module :=
        Navigation_Module (Navigation_Module_ID);
      Kernel : constant Kernel_Handle := Module.Get_Kernel;
      pragma Unreferenced (Context);
   begin
      Open_File_Action_Hook.Run
        (Kernel,
         File     => Command.File,
         Project  => No_Project,
         New_File => False);
      return Success;
   end Execute;

end Navigation_Module;
