------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2002-2016, AdaCore                     --
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
with Commands.Interactive;       use Commands, Commands.Interactive;
with GNAT.Strings;               use GNAT.Strings;
with GNATCOLL.Arg_Lists; use GNATCOLL.Arg_Lists;
with GNATCOLL.Projects;          use GNATCOLL.Projects;
with GNATCOLL.Scripts;           use GNATCOLL.Scripts;
with GNATCOLL.Traces;            use GNATCOLL.Traces;
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
with Glib.Object;                use Glib.Object;
with Glib;                       use Glib;
with Gtk.Widget;                 use Gtk.Widget;
with Language;                   use Language;
with XML_Parsers;                use XML_Parsers;
with XML_Utils;                  use XML_Utils;

package body Navigation_Module is
   Me : constant Trace_Handle := Create ("Navigation");

   Max_Locations_In_History : constant := 200;
   --  Maximum number of locations stored in the history

   Navigation_Module_Name : constant String := "Navigation";
   Navigation_Module_ID : Module_ID;

   type Location_Marker_Array is array (Positive range <>) of Location_Marker;
   type Location_Marker_Array_Access is access Location_Marker_Array;

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
   end record;
   type Navigation_Module is access all Navigation_Module_Record'Class;

   type Shell_Marker_Record is new Location_Marker_Record with record
      Script  : Scripting_Language;
      Command : GNAT.Strings.String_Access;
   end record;
   type Shell_Marker is access all Shell_Marker_Record'Class;

   function Create_Shell_Marker
     (Script  : access Scripting_Language_Record'Class;
      Command : String) return Shell_Marker;
   --  Create a new marker associated with a shell command

   overriding function Go_To
     (Marker : access Shell_Marker_Record;
      Kernel : access Kernel_Handle_Record'Class) return Boolean;
   overriding procedure Destroy (Marker : in out Shell_Marker_Record);
   overriding function Clone
     (Marker : access Shell_Marker_Record)
      return Location_Marker;
   overriding function To_String
     (Marker : access Shell_Marker_Record) return String;
   overriding function Save
     (Marker : access Shell_Marker_Record) return XML_Utils.Node_Ptr;
   overriding function Similar
     (Left  : access Shell_Marker_Record;
      Right : access Location_Marker_Record'Class) return Boolean;
   overriding function Distance
     (Left  : access Shell_Marker_Record;
      Right : access Location_Marker_Record'Class) return Integer;
   --  See inherited documentation

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Location_Marker_Record'Class, Location_Marker);
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
   --  Callback for Navigate->Previous Result menu

   type Next_Tag_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Next_Tag_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Callback for Navigate->Next Result menu

   type Start_Statement_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Start_Statement_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Callback for Navigate->Start Statement menu

   type End_Statement_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access End_Statement_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Callback for Navigate->End Statement menu

   type Next_Subprogram_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Next_Subprogram_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Callback for Navigate->Next Subprogram menu

   type Previous_Subprogram_Command
      is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Previous_Subprogram_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Callback for Navigate->Previous Subprogram menu

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

   procedure Go_To_Current_Marker (Kernel : access Kernel_Handle_Record'Class);
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

      Local : Location_Marker;
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
            Destroy (Module.Markers (Module.Markers'First).all);
            Unchecked_Free (Module.Markers (Module.Markers'First));
            Module.Markers (Module.Markers'First .. Module.Markers'Last - 1) :=
              Module.Markers (Module.Markers'First + 1 .. Module.Markers'Last);
            Module.Current_Marker := Module.Markers'Last;
         end if;

         Module.Markers (Module.Current_Marker) := Marker;
         Module.Last_Marker := Module.Current_Marker;
      else
         --  We are not storing marker: release memory now.
         Marker.Destroy;
         Local := Marker;
         Unchecked_Free (Local);
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
      M            : constant Navigation_Module :=
                      Navigation_Module (Navigation_Module_ID);
      Project_Tree : constant GNATCOLL.Projects.Project_Tree_Access :=
        Get_Registry (Kernel).Tree;
      Root_Project : constant Project_Type := Project_Tree.Root_Project;
      Project_File : constant Virtual_File := Root_Project.Project_Path;
      Directory    : Virtual_File;
      Filename     : Virtual_File;
      File, Child, Project : Node_Ptr;
      Marker       : Location_Marker;
      Err          : String_Access;
   begin
      --  Keep markers only for ordinary projects (not empty nor default)

      if Get_Registry (Kernel).Tree.Status /= From_File then
         M.Markers_File := No_File;
         return;
      end if;

      Directory := Object_Dir (Root_Project);

      if Directory = No_File then
         Directory := Project_File.Dir;
      end if;

      Filename := Create_From_Dir
        (Directory, Project_File.Base_Name (".gpr") & "-loc.xml");
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

                  if Marker /= null then
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

   procedure Go_To_Current_Marker
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Module : constant Navigation_Module :=
                 Navigation_Module (Navigation_Module_ID);
   begin
      if Module.Markers /= null
        and then Module.Current_Marker >= Module.Markers'First
        and then Module.Current_Marker <= Module.Last_Marker
      then
         if not Go_To (Module.Markers (Module.Current_Marker), Kernel) then
            Destroy (Module.Markers (Module.Current_Marker).all);
            Unchecked_Free (Module.Markers (Module.Current_Marker));
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
                   Cursor (Current_View (Editor));

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
      return Loc.Block_Start.Line;
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
      return Loc.Block_Type;
   exception
      when Constraint_Error =>
         return Cat_Unknown;
   end Get_Block_Type;

   -----------
   -- Go_To --
   -----------

   overriding function Go_To
     (Marker : access Shell_Marker_Record;
      Kernel : access Kernel_Handle_Record'Class) return Boolean
   is
      pragma Unreferenced (Kernel);
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

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Marker : in out Shell_Marker_Record) is
   begin
      Free (Marker.Command);
      Destroy (Location_Marker_Record (Marker));
   end Destroy;

   ---------------
   -- To_String --
   ---------------

   overriding function To_String
     (Marker : access Shell_Marker_Record) return String is
   begin
      return Marker.Command.all;
   end To_String;

   -----------
   -- Clone --
   -----------

   overriding function Clone
     (Marker : access Shell_Marker_Record)
      return Location_Marker is
   begin
      return Location_Marker
        (Create_Shell_Marker (Marker.Script, Marker.Command.all));
   end Clone;

   ----------
   -- Save --
   ----------

   overriding function Save
     (Marker : access Shell_Marker_Record) return XML_Utils.Node_Ptr
   is
      N : constant Node_Ptr := new Node;
   begin
      N.Tag   := new String'("shell_mark");
      N.Value := new String'(Marker.Command.all);
      return N;
   end Save;

   -------------
   -- Similar --
   -------------

   overriding function Similar
     (Left  : access Shell_Marker_Record;
      Right : access Location_Marker_Record'Class) return Boolean
   is
      pragma Unreferenced (Left, Right);
   begin
      return False;
   end Similar;

   --------------
   -- Distance --
   --------------

   overriding function Distance
     (Left  : access Shell_Marker_Record;
      Right : access Location_Marker_Record'Class) return Integer
   is
      pragma Unreferenced (Left, Right);
   begin
      return Integer'Last;
   end Distance;

   -------------------------
   -- Create_Shell_Marker --
   -------------------------

   function Create_Shell_Marker
     (Script  : access Scripting_Language_Record'Class;
      Command : String) return Shell_Marker is
   begin
      return new Shell_Marker_Record'
        (Location_Marker_Record with
         Script  => Scripting_Language (Script),
         Command => new String'(Command));
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
      if Markers /= null then
         for M in Markers'Range loop
            if Markers (M) /= null then
               Markers (M).Destroy;
               Unchecked_Free (Markers (M));
            end if;
         end loop;
         Unchecked_Free (Markers);
      end if;
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
      Go_To_Current_Marker (Kernel);
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
      Go_To_Current_Marker (Kernel);
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

end Navigation_Module;
