-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free software; you can redistribute it and/or modify  it   --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Glib;                      use Glib;
with Glib.Xml_Int;              use Glib.Xml_Int;
with Glib.Object;               use Glib.Object;
with Gdk.Window;                use Gdk.Window;
with Gdk.Event;                 use Gdk.Event;
with Gdk.Pixbuf;                use Gdk.Pixbuf;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Dialog;                use Gtk.Dialog;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Handlers;              use Gtk.Handlers;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Main;                  use Gtk.Main;
with Gtk.Cell_Renderer_Text;    use Gtk.Cell_Renderer_Text;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Tree_Selection;        use Gtk.Tree_Selection;
with Gtk.Tree_Store;            use Gtk.Tree_Store;
with Gtk.Tree_View;             use Gtk.Tree_View;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Window;                use Gtk.Window;
with Gtk.Tooltips;              use Gtk.Tooltips;
with Gtkada.Handlers;           use Gtkada.Handlers;
with Gtkada.MDI;                use Gtkada.MDI;
with System;                    use System;

with String_Utils;              use String_Utils;
with String_List_Utils;         use String_List_Utils;
with Glide_Intl;                use Glide_Intl;
with Glide_Main_Window;         use Glide_Main_Window;
with Interactive_Consoles;      use Interactive_Consoles;
with Default_Preferences;       use Default_Preferences;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with Glide_Kernel.Preferences;  use Glide_Kernel.Preferences;
with Glide_Kernel.Project;      use Glide_Kernel.Project;
with Glide_Kernel.Console;      use Glide_Kernel.Console;
with GVD.Preferences;           use GVD.Preferences;
with GVD.Main_Window;           use GVD.Main_Window;
with Interfaces.C.Strings;      use Interfaces.C.Strings;
with Interfaces.C;              use Interfaces.C;
with GUI_Utils;                 use GUI_Utils;
with File_Utils;                use File_Utils;
with Src_Info;                  use Src_Info;
with Src_Info.Queries;          use Src_Info.Queries;
with Basic_Mapper;              use Basic_Mapper;
with Basic_Types;               use Basic_Types;
with Histories;                 use Histories;

with Projects.Registry;         use Projects, Projects.Registry;

with Glide_Kernel.Timeout;      use Glide_Kernel.Timeout;
with Generic_List;

with Language_Handlers;         use Language_Handlers;
with Language_Handlers.Glide;   use Language_Handlers.Glide;

with Traces;                    use Traces;

with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Tags;                  use Ada.Tags;
with Ada.Text_IO;               use Ada.Text_IO;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Ada.Unchecked_Deallocation;

package body Glide_Kernel is

   Signals : constant chars_ptr_array :=
     (1  => New_String (Project_Changed_Signal),
      2  => New_String (Project_View_Changed_Signal),
      3  => New_String (Context_Changed_Signal),
      4  => New_String (Variable_Changed_Signal),
      5  => New_String (Source_Lines_Revealed_Signal),

      6  => New_String (File_Edited_Signal),
      7  => New_String (File_Saved_Signal),
      8  => New_String (File_Closed_Signal),
      9  => New_String (File_Changed_On_Disk_Signal),
      10  => New_String (Compilation_Finished_Signal),

      11 => New_String (Preferences_Changed_Signal),
      12 => New_String (Search_Regexps_Changed_Signal),
      13 => New_String (Search_Reset_Signal),
      14 => New_String (Search_Functions_Changed_Signal));
   --  The list of signals defined for this object

   Kernel_Class : GObject_Class := Uninitialized_Class;
   --  The class structure for this object

   Me : constant Debug_Handle := Create ("glide_kernel");

   History_Max_Length : constant Positive := 10;
   --  <preferences> Maximum number of entries to store in each history

   package Object_Callback is new Gtk.Handlers.Callback
     (Glib.Object.GObject_Record);

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Project_Registry'Class, Project_Registry_Access);

   procedure Reset_Source_Info_List
     (Handle : access Kernel_Handle_Record'Class);
   --  Re-initialize the Source Info structure.
   --  ??? Needs more comments.

   function Process_Anim (Data : Process_Data) return Boolean;
   --  Process_Timeout callback to handle image animations.

   procedure Row_Activated (Widget : access Gtk_Widget_Record'Class);
   --  Called when a specific entity declaration has been selected in the
   --  overloaded entities dialog.

   procedure Select_Entity_Declaration
     (Kernel        : access Kernel_Handle_Record'Class;
      Lib_Info      : LI_File_Ptr;
      Entity_Name   : String;
      Decl          : out Entity_Information;
      Status        : out Src_Info.Queries.Find_Decl_Or_Body_Query_Status);
   --  Open a dialog to ask the user to select among multiple declaration for
   --  the entity with name Entity_Name.
   --  Decl is set to No_Entity_Information and Status to Entity_Not_Found if
   --  the user didn't select any declaration.

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Kernel_Module_Data_Record'Class, Kernel_Module_Data);

   --------------------------
   -- Get_Language_Handler --
   --------------------------

   function Get_Language_Handler
     (Handle : access Kernel_Handle_Record)
      return Language_Handlers.Language_Handler is
   begin
      return Handle.Lang_Handler;
   end Get_Language_Handler;

   ------------------
   -- GNAT_Version --
   ------------------

   function GNAT_Version
     (Handle : access Kernel_Handle_Record) return String is
   begin
      if Handle.GNAT_Version = null then
         return -"<unknown version>";
      else
         return Handle.GNAT_Version.all;
      end if;
   end GNAT_Version;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Handle      : out Kernel_Handle;
      Main_Window : Gtk.Window.Gtk_Window;
      Home_Dir    : String)
   is
      Signal_Parameters : constant Signal_Parameter_Types :=
        (1 .. 2 | 4 | 11 .. 14 => (1 => GType_None),
         3      | 5            => (1 => GType_Pointer),
         6 .. 10               => (1 => GType_String));
      Handler : Glide_Language_Handler;
      Dir     : constant String := String_Utils.Name_As_Directory (Home_Dir);

   begin
      Handle := new Kernel_Handle_Record;
      Glib.Object.Initialize (Handle);
      Initialize_Class_Record
        (Handle, Signals, Kernel_Class, "GlideKernel", Signal_Parameters);

      Glide_Kernel.Modules.Initialize (Handle);
      Handle.Main_Window  := Main_Window;
      Handle.Home_Dir     := new String'(Home_Dir);

      --  Create the language handler. It is also set for the gvd main window,
      --  so that the embedded gvd uses the same mechanism as the rest of glide
      --  to guess the language for a file name.

      Gtk_New (Handler);
      Handle.Lang_Handler := Language_Handler (Handler);
      Glide_Window (Handle.Main_Window).Lang_Handler :=
        Handle.Lang_Handler;

      Handle.Registry := new Project_Registry;
      Load_Default_Project (Handle.Registry.all, Get_Current_Dir);

      Set_Registry
        (Glide_Language_Handler (Handle.Lang_Handler), Handle.Registry);

      Handle.Gnatls_Cache           := null;

      --  Note: we do not compute the view of this project yet. This will be
      --  done only if no other project was loaded from the command line, which
      --  is more efficient in case the current directory has lots of source
      --  files.

      Reset_Source_Info_List (Handle);

      Gtk_New (Handle.Tooltips);
      Ref (Handle.Tooltips);
      Sink (Handle.Tooltips);

      Handle.Preferences := new GPS_Preferences_Record;
      GVD.Preferences.GVD_Prefs := GVD_Preferences (Handle.Preferences);
      Register_Global_Preferences (Handle);
      Load_Preferences (Handle.Preferences, Dir & "preferences");

      Handle.History := new History_Record;
      Load (Handle.History.all, Dir & "history");
      Set_Max_Length (Handle.History.all, History_Max_Length);
   end Gtk_New;

   ------------------------------
   -- Get_Default_Accelerators --
   ------------------------------

   function Get_Default_Accelerators
     (Handle : access Kernel_Handle_Record)
      return Gtk.Accel_Group.Gtk_Accel_Group is
   begin
      return GVD_Main_Window (Handle.Main_Window).Main_Accel_Group;
   end Get_Default_Accelerators;

   --------------------------------
   -- Get_Predefined_Source_Path --
   --------------------------------

   function Get_Predefined_Source_Path
     (Handle : access Kernel_Handle_Record) return String is
   begin
      if Handle.Predefined_Source_Path = null then
         return ".";
      else
         return Handle.Predefined_Source_Path.all;
      end if;
   end Get_Predefined_Source_Path;

   --------------------------------
   -- Get_Predefined_Object_Path --
   --------------------------------

   function Get_Predefined_Object_Path
     (Handle : access Kernel_Handle_Record) return String is
   begin
      if Handle.Predefined_Object_Path = null then
         return ".";
      else
         return Handle.Predefined_Object_Path.all;
      end if;
   end Get_Predefined_Object_Path;

   ---------------------------------
   -- Get_Predefined_Source_Files --
   ---------------------------------

   function Get_Predefined_Source_Files
     (Handle : access Kernel_Handle_Record) return String_Array_Access
   is
      Result : String_Array_Access;
   begin
      --  ??? A nicer way would be to implement this with a predefined project,
      --  and rely on the project parser to return the source
      --  files. Unfortunately, this doesn't work with the current
      --  implementation of this parser, since one cannot have two separate
      --  project hierarchies at the same time.

      if Handle.Predefined_Source_Files = null then
         Handle.Predefined_Source_Files := Read_Files_From_Dirs
           (Handle.Predefined_Source_Path.all);
      end if;

      --  Make a copy of the result, so that we can keep a cache in the kernel

      Result := new String_Array (Handle.Predefined_Source_Files'Range);

      for S in Handle.Predefined_Source_Files'Range loop
         Result (S) := new String'(Handle.Predefined_Source_Files (S).all);
      end loop;

      return Result;
   end Get_Predefined_Source_Files;

   -----------
   -- Setup --
   -----------

   procedure Setup
     (Data : Glib.Object.GObject; Id : Gtk.Handlers.Handler_Id) is
   begin
      Add_Watch (Id, Data);
   end Setup;

   ----------------
   -- Save_Child --
   ----------------

   function Save_Child
     (Handle : access Kernel_Handle_Record;
      Child  : Gtkada.MDI.MDI_Child;
      Force  : Boolean := True) return Save_Return_Value
   is
      Module : Module_ID;
   begin
      Module := Get_Module_From_Child (Handle, Child);

      if Module /= null
        and then Module.Info.Save_Function /= null
      then
         return
           Module.Info.Save_Function
             (Handle,
              Get_Widget (Child),
              Force);
      end if;

      return Saved;
   end Save_Child;

   ---------------------
   -- Get_File_Editor --
   ---------------------

   function Get_File_Editor
     (Handle : access Kernel_Handle_Record;
      File   : String) return Gtkada.MDI.MDI_Child
   is
      MDI : constant MDI_Window := Get_MDI (Handle);
   begin
      --  ??? the following implementation assumes that the file editors
      --  are MDI children that have corresponding file names for title, and
      --  that they are the only MDI childs that do so.
      --  ??? We might improve a little by checking the Tag of the child
      --  against that of the source editor module. The ID for that module
      --  needs to be moved to glide_kernel.ads.

      return Find_MDI_Child_By_Name (MDI, File);
   end Get_File_Editor;

   ---------------------------
   -- Get_Module_From_Child --
   ---------------------------

   function Get_Module_From_Child
     (Handle : access Kernel_Handle_Record;
      Child  : Gtkada.MDI.MDI_Child) return Module_ID
   is
      use type Module_List.List_Node;
      Module : Module_List.List_Node;

   begin
      Module := Module_List.First (Handle.Modules_List);

      while Module /= Module_List.Null_Node loop
         if Module_List.Data (Module).Info.Child_Tag =
           Get_Widget (Child)'Tag
         then
            return Module_List.Data (Module);
         end if;

         Module := Module_List.Next (Module);
      end loop;

      return null;
   end Get_Module_From_Child;

   ---------------------------
   -- Save_All_MDI_Children --
   ---------------------------

   function Save_All_MDI_Children
     (Handle : access Kernel_Handle_Record;
      Force  : Boolean := False) return Boolean
   is
      MDI       : constant MDI_Window := Get_MDI (Handle);
      Iter      : Child_Iterator;
      Child     : MDI_Child;
      Save_Type : Save_Return_Value;
      F         : Boolean := Force;

   begin
      Iter := First_Child (MDI);
      Child := Get (Iter);

      Save_Type := Save_Project_Conditional (Handle, F);

      if Save_Type = Cancel then
         return False;
      elsif Save_Type = Save_All then
         F := True;
      end if;

      --  Browse through all MDI children.

      while Child /= null loop
         Save_Type := Save_Child (Handle, Child, F);

         if Save_Type = Cancel then
            return False;
         elsif Save_Type = Save_All then
            F := True;
         end if;

         Next (Iter);
         Child := Get (Iter);
      end loop;

      return True;
   end Save_All_MDI_Children;

   ------------------------
   -- Close_All_Children --
   ------------------------

   procedure Close_All_Children (Handle : access Kernel_Handle_Record) is
      MDI   : constant MDI_Window := Get_MDI (Handle);
      Iter  : Child_Iterator := First_Child (MDI);
      Child : MDI_Child;
   begin
      while Get (Iter) /= null loop
         Child := Get (Iter);
         Next (Iter);
         Close_Child (Child);
      end loop;
   end Close_All_Children;

   ----------------------
   -- Save_All_Editors --
   ----------------------

   function Save_All_Editors
     (Handle : access Kernel_Handle_Record;
      Force  : Boolean) return Boolean
   is
      pragma Unreferenced (Force);
      pragma Unreferenced (Handle);
   begin
      --  ??? not implemented yet.
      raise Program_Error;
      return False;
   end Save_All_Editors;

   -------------------------------------
   -- Locate_From_Source_And_Complete --
   -------------------------------------

   function Locate_From_Source_And_Complete
     (Handle          : access Kernel_Handle_Record;
      Source_Filename : String) return Src_Info.LI_File_Ptr
   is
      File : LI_File_Ptr;
      Project : constant Project_Type := Get_Project_From_File
        (Handle.Registry.all, Base_Name (Source_Filename));
   begin
      Create_Or_Complete_LI
        (Handler                => Get_LI_Handler_From_File
           (Glide_Language_Handler (Handle.Lang_Handler),
            Source_Filename),
         File                   => File,
         Source_Filename        => Source_Filename,
         List                   => Handle.Source_Info_List,
         Project                => Project,
         Predefined_Source_Path => Get_Predefined_Source_Path (Handle),
         Predefined_Object_Path => Get_Predefined_Object_Path (Handle));
      return File;

   exception
      when Unsupported_Language =>
         Trace (Me,
                "Locate_From_Source_And_Complete: Unsupported_Language for "
                & Source_Filename);
         return No_LI_File;
   end Locate_From_Source_And_Complete;

   -------------------------
   -- Find_All_References --
   -------------------------

   procedure Find_All_References
     (Kernel       : access Kernel_Handle_Record;
      Entity       : Src_Info.Queries.Entity_Information;
      Iterator     : out Src_Info.Queries.Entity_Reference_Iterator;
      In_File      : String := "";
      LI_Once      : Boolean := False;
      Project      : Project_Type := No_Project) is
   begin
      Find_All_References
        (Get_Project (Kernel),
         Get_Language_Handler (Kernel),
         Entity, Kernel.Source_Info_List,
         Iterator, Project, LI_Once,
         In_File,
         Get_Predefined_Source_Path (Kernel),
         Get_Predefined_Object_Path (Kernel));
   end Find_All_References;

   ----------
   -- Next --
   ----------

   procedure Next
     (Kernel : access Kernel_Handle_Record;
      Iterator : in out Entity_Reference_Iterator) is
   begin
      Next (Get_Language_Handler (Kernel), Iterator, Kernel.Source_Info_List);
   end Next;

   --------------------------------
   -- Find_Ancestor_Dependencies --
   --------------------------------

   procedure Find_Ancestor_Dependencies
     (Kernel          : access Kernel_Handle_Record;
      Source_Filename : String;
      Iterator        : out Dependency_Iterator;
      Project         : Projects.Project_Type := No_Project) is
   begin
      Find_Ancestor_Dependencies
        (Get_Project (Kernel),
         Get_Language_Handler (Kernel),
         Source_Filename,
         Kernel.Source_Info_List, Iterator, Project,
         Include_Self => False,
         Predefined_Source_Path => Get_Predefined_Source_Path (Kernel),
         Predefined_Object_Path => Get_Predefined_Object_Path (Kernel));
   end Find_Ancestor_Dependencies;

   ----------
   -- Next --
   ----------

   procedure Next
     (Kernel   : access Kernel_Handle_Record;
      Iterator : in out Src_Info.Queries.Dependency_Iterator) is
   begin
      Next (Get_Language_Handler (Kernel), Iterator, Kernel.Source_Info_List);
   end Next;

   -----------------
   -- Renaming_Of --
   -----------------

   procedure Renaming_Of
     (Kernel         : access Kernel_Handle_Record;
      Entity         : Entity_Information;
      Is_Renaming    : out Boolean;
      Renamed_Entity : out Entity_Information) is
   begin
      Renaming_Of
        (Kernel.Source_Info_List, Entity, Is_Renaming, Renamed_Entity);
   end Renaming_Of;

   ----------------------------
   -- Reset_Source_Info_List --
   ----------------------------

   procedure Reset_Source_Info_List
     (Handle : access Kernel_Handle_Record'Class) is
   begin
      Src_Info.Reset (Handle.Source_Info_List);
   end Reset_Source_Info_List;

   ---------------------
   -- Project_Changed --
   ---------------------

   procedure Project_Changed (Handle : access Kernel_Handle_Record) is
   begin
      Object_Callback.Emit_By_Name (Handle, Project_Changed_Signal);
   end Project_Changed;

   --------------------------
   -- Project_View_Changed --
   --------------------------

   procedure Project_View_Changed (Handle : access Kernel_Handle_Record) is
   begin
      Object_Callback.Emit_By_Name (Handle, Project_View_Changed_Signal);
   end Project_View_Changed;

   -------------------------
   -- Preferences_Changed --
   -------------------------

   procedure Preferences_Changed (Handle : access Kernel_Handle_Record) is
   begin
      Object_Callback.Emit_By_Name (Handle, Preferences_Changed_Signal);
   end Preferences_Changed;

   ----------------------------
   -- Search_Regexps_Changed --
   ----------------------------

   procedure Search_Regexps_Changed (Handle : access Kernel_Handle_Record) is
   begin
      Object_Callback.Emit_By_Name (Handle, Search_Regexps_Changed_Signal);
   end Search_Regexps_Changed;

   ------------------
   -- Search_Reset --
   ------------------

   procedure Search_Reset (Handle : access Kernel_Handle_Record) is
   begin
      Object_Callback.Emit_By_Name (Handle, Search_Reset_Signal);
   end Search_Reset;

   ------------------------------
   -- Search_Functions_Changed --
   ------------------------------

   procedure Search_Functions_Changed (Handle : access Kernel_Handle_Record) is
   begin
      Object_Callback.Emit_By_Name (Handle, Search_Functions_Changed_Signal);
   end Search_Functions_Changed;

   ----------------------
   -- Variable_Changed --
   ----------------------

   procedure Variable_Changed (Handle : access Kernel_Handle_Record) is
   begin
      Object_Callback.Emit_By_Name (Handle, Variable_Changed_Signal);
   end Variable_Changed;

   ---------------------------
   -- Source_Lines_Revealed --
   ---------------------------

   procedure Source_Lines_Revealed
     (Handle  : access Kernel_Handle_Record;
      Context : access Selection_Context'Class)
   is
      procedure Internal
        (Handle  : System.Address;
         Signal  : String;
         Context : Selection_Context_Access);
      pragma Import (C, Internal, "g_signal_emit_by_name");

   begin
      --  ??? code duplication from Context_Changed, see below.
      Internal
        (Get_Object (Handle),
         Source_Lines_Revealed_Signal & ASCII.NUL,
         Selection_Context_Access (Context));
   end Source_Lines_Revealed;

   -----------------
   -- File_Edited --
   -----------------

   procedure File_Edited
     (Handle : access Kernel_Handle_Record;
      File   : String)
   is
      procedure Internal
        (Handle : System.Address;
         Signal : String;
         File   : String);
      pragma Import (C, Internal, "g_signal_emit_by_name");

   begin
      Internal
        (Get_Object (Handle),
         File_Edited_Signal & ASCII.NUL,
         File & ASCII.NUL);

      if not Is_Open (Handle, File) then
         String_List_Utils.String_List.Append (Handle.Open_Files, File);
      end if;
   end File_Edited;

   ----------------
   -- File_Saved --
   ----------------

   procedure File_Saved
     (Handle  : access Kernel_Handle_Record;
      File    : String)
   is
      procedure Internal
        (Handle : System.Address;
         Signal : String;
         File   : String);
      pragma Import (C, Internal, "g_signal_emit_by_name");

   begin
      Internal
        (Get_Object (Handle),
         File_Saved_Signal & ASCII.NUL,
         File & ASCII.NUL);
   end File_Saved;

   -----------------
   -- File_Closed --
   -----------------

   procedure File_Closed
     (Handle  : access Kernel_Handle_Record;
      File    : String)
   is
      procedure Internal
        (Handle : System.Address;
         Signal : String;
         File   : String);
      pragma Import (C, Internal, "g_signal_emit_by_name");

      use String_List_Utils.String_List;

   begin
      Internal
        (Get_Object (Handle),
         File_Closed_Signal & ASCII.NUL,
         File & ASCII.NUL);

      Remove_From_List (Handle.Open_Files, File);
   end File_Closed;

   --------------------------
   -- File_Changed_On_Disk --
   --------------------------

   procedure File_Changed_On_Disk
     (Handle  : access Kernel_Handle_Record;
      File    : String)
   is
      procedure Internal
        (Handle : System.Address;
         Signal : String;
         File   : String);
      pragma Import (C, Internal, "g_signal_emit_by_name");
   begin
      Internal
        (Get_Object (Handle),
         File_Changed_On_Disk_Signal & ASCII.NUL,
         File & ASCII.NUL);
   end File_Changed_On_Disk;

   --------------------------
   -- Compilation_Finished --
   --------------------------

   procedure Compilation_Finished
     (Handle  : access Kernel_Handle_Record;
      File    : String)
   is
      procedure Internal
        (Handle : System.Address;
         Signal : String;
         File   : String);
      pragma Import (C, Internal, "g_signal_emit_by_name");

   begin
      Internal
        (Get_Object (Handle),
         Compilation_Finished_Signal & ASCII.NUL,
         File & ASCII.NUL);
   end Compilation_Finished;

   -------------
   -- Is_Open --
   -------------

   function Is_Open
     (Kernel   : access Kernel_Handle_Record;
      Filename : String) return Boolean
   is
      use String_List_Utils.String_List;

      Node : List_Node;
   begin
      Node := First (Kernel.Open_Files);

      while Node /= Null_Node loop
         if Data (Node) = Filename then
            return True;
         end if;

         Node := Next (Node);
      end loop;

      return False;
   end Is_Open;

   ---------------------
   -- Context_Changed --
   ---------------------

   procedure Context_Changed
     (Handle  : access Kernel_Handle_Record;
      Context : access Selection_Context'Class)
   is
      procedure Internal
        (Handle  : System.Address;
         Signal  : String;
         Context : Selection_Context_Access);
      pragma Import (C, Internal, "g_signal_emit_by_name");

   begin
      Internal
        (Get_Object (Handle),
         Context_Changed_Signal & ASCII.NUL,
         Selection_Context_Access (Context));
   end Context_Changed;

   ------------------------
   -- Get_Current_Module --
   ------------------------

   function Get_Current_Module
     (Kernel : access Kernel_Handle_Record) return Module_ID
   is
      use type Module_List.List_Node;

      Module : Module_List.List_Node :=
        Module_List.First (Kernel.Modules_List);
      C : constant MDI_Child := Get_Focus_Child (Get_MDI (Kernel));

   begin
      if C = null then
         return null;
      end if;

      while Module /= Module_List.Null_Node loop
         if Module_List.Data (Module).Info.Child_Tag = Get_Widget (C)'Tag then
            return Module_List.Data (Module);
         end if;

         Module := Module_List.Next (Module);
      end loop;

      Trace (Me, "Get_Current_Context: No module associated with tag "
             & External_Tag (Get_Widget (C)'Tag));

      return null;
   end Get_Current_Module;

   -------------------------
   -- Get_Current_Context --
   -------------------------

   function Get_Current_Context
     (Kernel : access Kernel_Handle_Record) return Selection_Context_Access
   is
      Module : Module_ID;
   begin
      if Kernel.Current_Context /= null then
         Free (Kernel.Current_Context);
      end if;

      Module := Get_Current_Module (Kernel);
      if Module /= null
        and then Module.Info.Default_Factory /= null
      then
         Kernel.Current_Context := Module.Info.Default_Factory
           (Kernel, Get_Widget (Get_Focus_Child (Get_MDI (Kernel))));

         if Kernel.Current_Context /= null then
            Set_Context_Information (Kernel.Current_Context, Kernel, Module);
         end if;

         if Kernel.Current_Context = null then
            Trace (Me, "Null context returned by the module");
         end if;
      end if;

      return Kernel.Current_Context;
   end Get_Current_Context;

   ------------------
   -- Save_Desktop --
   ------------------

   procedure Save_Desktop
     (Handle : access Kernel_Handle_Record)
   is
      MDI   : constant MDI_Window := Get_MDI (Handle);
      File_Name    : constant String :=
        String_Utils.Name_As_Directory (Handle.Home_Dir.all) & "desktop";
      Project_Name : constant String :=
        Project_Directory (Get_Project (Handle));
      File  : File_Type;
      N     : Node_Ptr;
      M     : Node_Ptr;
      Old   : Node_Ptr;
      State : Gdk_Window_State;

   begin
      --  Read the previous contents of the file, to save the desktops for
      --  other projects

      Trace (Me, "saving desktop file " & File_Name);

      if Is_Regular_File (File_Name) then
         Old := Parse (File_Name);
      end if;

      Create (File, Mode => Out_File, Name => File_Name);
      Set_Output (File);

      N := new Node'
        (Tag => new String'("GPS_Desktop"),
         Child => null,
         Parent => null,
         Value  => null,
         Attributes => null,
         Next => null,
         Specific_Data => 0);

      --  Merge the old contents of the file

      if Old /= null then
         M := Old.Child;

         while M /= null loop
            if M.Tag /= null
              and then M.Tag.all = "MDI"
              and then Get_Attribute (M, "project") /= Project_Name
            then
               Add_Child (N, M);
            end if;

            M := M.Next;
         end loop;
      end if;

      --  Add the current content, indexed on the current project

      M := Glide_Kernel.Kernel_Desktop.Save_Desktop (MDI);
      Set_Attribute (M, "project", Project_Name);
      Add_Child (N, M);

      --  Add GPS-specific nodes

      State := Get_State (Get_Window (Handle.Main_Window));

      if (State and Window_State_Maximized) = 0 then
         M := new Node'
           (Tag           => new String'("GPS_Width"),
            Value         => new String'
              (Allocation_Int'Image
                 (Get_Allocation_Width (Gtk_Widget ((Handle.Main_Window))))),
            Child         => null,
            Parent        => null,
            Next          => null,
            Specific_Data => 0,
            Attributes    => null);
         Add_Child (N, M);

         M := new Node'
           (Tag           => new String'("GPS_Height"),
            Value         => new String'
              (Allocation_Int'Image
                 (Get_Allocation_Height ((Handle.Main_Window)))),
            Child         => null,
            Parent        => null,
            Next          => null,
            Specific_Data => 0,
            Attributes    => null);
         Add_Child (N, M);
      end if;

      M := new Node'
        (Tag           => new String'("GPS_State"),
         Value         => new String'(Gdk_Window_State'Image (State)),
         Child         => null,
         Parent        => null,
         Next          => null,
         Specific_Data => 0,
         Attributes    => null);
      Add_Child (N, M);

      Print (N);

      Free (N);

      Set_Output (Standard_Output);
      Close (File);
   end Save_Desktop;

   ----------------------
   -- Has_User_Desktop --
   ----------------------

   function Has_User_Desktop
     (Handle : access Kernel_Handle_Record) return Boolean
   is
      File : constant String :=
        String_Utils.Name_As_Directory (Handle.Home_Dir.all) & "desktop";
   begin
      return Is_Regular_File (File);
   end Has_User_Desktop;

   ------------------
   -- Load_Desktop --
   ------------------

   function Load_Desktop
     (Handle : access Kernel_Handle_Record) return Boolean
   is
      MDI    : constant MDI_Window := Get_MDI (Handle);
      Node   : Node_Ptr;
      File   : constant String :=
        String_Utils.Name_As_Directory (Handle.Home_Dir.all) & "desktop";
      Project_Name : constant String :=
        Project_Directory (Get_Project (Handle));
      Child  : Node_Ptr;
      Desktop_Node : Node_Ptr;
      Width  : Gint := 640;
      Height : Gint := 480;
      State  : Gdk_Window_State := 0;

   begin
      if Is_Regular_File (File) then
         Trace (Me, "loading desktop file " & File);
         Node := Parse (File);

         if Node /= null then
            Child := Node.Child;
         end if;

         while Child /= null loop
            if Child.Tag /= null then
               if Child.Tag.all = "MDI"
                 and then Get_Attribute (Child, "project") = Project_Name
               then
                  Desktop_Node := Child;

               elsif Child.Tag.all = "GPS_Height" then
                  Height := Gint'Value (Child.Value.all);
               elsif Child.Tag.all = "GPS_Width" then
                  Width := Gint'Value (Child.Value.all);
               elsif Child.Tag.all = "GPS_State" then
                  State := Gdk_Window_State'Value (Child.Value.all);
               end if;
            end if;

            Child := Child.Next;
         end loop;

         Set_Default_Size (Handle.Main_Window, Width, Height);

         if (State and Window_State_Maximized) /= 0 then
            Maximize (Handle.Main_Window);
         end if;

         --  Call Show_All before restoring the desktop, in case some
         --  children stored in the desktop have something to hide.
         Show_All (Handle.Main_Window);

         if Desktop_Node /= null then
            Kernel_Desktop.Restore_Desktop
              (MDI, Desktop_Node, Kernel_Handle (Handle));
         else
            Kernel_Desktop.Restore_Desktop
              (MDI, Handle.Default_Desktop, Kernel_Handle (Handle));
         end if;

         Free (Node);
         return True;

      else
         Trace (Me, "loading default desktop");
         Show_All (Handle.Main_Window);
         Kernel_Desktop.Restore_Desktop
           (MDI, Handle.Default_Desktop, Kernel_Handle (Handle));

         return False;
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         return False;
   end Load_Desktop;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Command_Information) is
   begin
      Free (X.Command);
      Free (X.Help);
   end Free;

   procedure Free (Module : in out Module_ID) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Module_ID_Record'Class, Module_ID);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Module_ID_Information, Module_ID_Information_Access);

   begin
      Destroy (Module.all);
      Unchecked_Free (Module.Info);
      Unchecked_Free (Module);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Context : in out Selection_Context_Access) is
      procedure Internal is new Ada.Unchecked_Deallocation
        (Selection_Context'Class, Selection_Context_Access);

   begin
      Destroy (Context.all);
      Internal (Context);
   end Free;

   ----------------
   -- Get_Kernel --
   ----------------

   function Get_Kernel
     (Context : access Selection_Context) return Kernel_Handle is
   begin
      return Context.Kernel;
   end Get_Kernel;

   -----------------
   -- Get_Creator --
   -----------------

   function Get_Creator (Context : access Selection_Context)
      return Module_ID is
   begin
      return Context.Creator;
   end Get_Creator;

   -----------------------------
   -- Set_Context_Information --
   -----------------------------

   procedure Set_Context_Information
     (Context : access Selection_Context;
      Kernel  : access Kernel_Handle_Record'Class;
      Creator : Module_ID) is
   begin
      Context.Kernel := Kernel_Handle (Kernel);
      Context.Creator := Creator;
   end Set_Context_Information;

   -------------
   -- Get_MDI --
   -------------

   function Get_MDI
     (Handle : access Kernel_Handle_Record) return Gtkada.MDI.MDI_Window
   is
      Top : constant Glide_Window := Glide_Window (Handle.Main_Window);
   begin
      return Top.Process_Mdi;
   end Get_MDI;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Context : in out Selection_Context) is
      pragma Unreferenced (Context);
   begin
      null;
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Id : in out Module_ID_Record) is
      pragma Unreferenced (Id);
   begin
      null;
   end Destroy;

   ---------------------
   -- Get_Main_Window --
   ---------------------

   function Get_Main_Window
     (Handle : access Kernel_Handle_Record) return Gtk.Window.Gtk_Window is
   begin
      return Handle.Main_Window;
   end Get_Main_Window;

   ------------------
   -- Get_Tooltips --
   ------------------

   function Get_Tooltips
     (Handle : access Kernel_Handle_Record) return Gtk.Tooltips.Gtk_Tooltips is
   begin
      return Handle.Tooltips;
   end Get_Tooltips;

   -----------------
   -- Get_Toolbar --
   -----------------

   function Get_Toolbar
     (Handle : access Kernel_Handle_Record) return Gtk.Toolbar.Gtk_Toolbar is
   begin
      return Glide_Window (Handle.Main_Window).Toolbar;
   end Get_Toolbar;

   ------------------
   -- Process_Anim --
   ------------------

   function Process_Anim (Data : Process_Data) return Boolean is
      Window : constant Glide_Window := Glide_Window (Data.Kernel.Main_Window);
   begin
      if Anim_Cb (Data.Kernel) then
         Window.Timeout_Id := Process_Timeout.Add
           (Guint32 (Get_Delay_Time (Window.Animation_Iter)),
            Process_Anim'Access, Data);
      end if;

      return False;
   end Process_Anim;

   ----------------
   -- Push_State --
   ----------------

   procedure Push_State
     (Handle : Kernel_Handle;
      State  : Action_Kernel_State)
   is
      Window : Glide_Window;
   begin
      if Handle = null then
         return;
      end if;

      Window := Glide_Window (Handle.Main_Window);

      if State = Busy then
         Set_Busy_Cursor (Get_Window (Window), True, True);
         Window.Busy_Level := Window.Busy_Level + 1;
      end if;

      if Window.State_Level = 0
        and then Window.Timeout_Id = 0
        and then Window.Animation_Iter /= null
      then
         Window.Timeout_Id := Process_Timeout.Add
           (Guint32 (Get_Delay_Time (Window.Animation_Iter)),
            Process_Anim'Access, (Handle, null, null, null, null));
      end if;

      Window.State_Level := Window.State_Level + 1;
   end Push_State;

   ---------------
   -- Pop_State --
   ---------------

   procedure Pop_State (Handle : Kernel_Handle) is
      Window  : Glide_Window;
      Console : Interactive_Console;
   begin
      if Handle = null then
         return;
      end if;

      Window := Glide_Window (Handle.Main_Window);

      if Window.State_Level > 0 then
         Window.State_Level := Window.State_Level - 1;

         if Window.Busy_Level > 0 then
            Window.Busy_Level := Window.Busy_Level - 1;

            if Window.Busy_Level = 0 then
               Set_Busy_Cursor (Get_Window (Window), False, False);
            end if;
         end if;

         if Window.State_Level = 0 then
            Console := Get_Interactive_Console (Handle);

            --  If console is null, it means we're exiting, so avoid accessing
            --  fields that may have been deleted already

            if Console /= null then
               if Window.Timeout_Id /= 0 then
                  Timeout_Remove (Window.Timeout_Id);
                  Window.Timeout_Id := 0;
                  Display_Default_Image (Handle);
               end if;
            end if;
         end if;
      end if;
   end Pop_State;

   ------------------
   -- Get_Home_Dir --
   ------------------

   function Get_Home_Dir
     (Handle : access Kernel_Handle_Record) return String is
   begin
      return Handle.Home_Dir.all;
   end Get_Home_Dir;

   ---------------------
   -- Get_Logs_Mapper --
   ---------------------

   function Get_Logs_Mapper
     (Handle : access Kernel_Handle_Record)
      return Basic_Mapper.File_Mapper_Access is
   begin
      return Handle.Logs_Mapper;
   end Get_Logs_Mapper;

   ---------------------
   -- Set_Logs_Mapper --
   ---------------------

   procedure Set_Logs_Mapper
     (Handle : access Kernel_Handle_Record;
      Mapper : Basic_Mapper.File_Mapper_Access) is
   begin
      Handle.Logs_Mapper := Mapper;
   end Set_Logs_Mapper;

   ---------------------
   -- Other_File_Name --
   ---------------------

   function Other_File_Name
     (Kernel          : access Kernel_Handle_Record;
      Source_Filename : String;
      Full_Name       : Boolean := True) return String
   is
      Project : constant Project_Type := Get_Project_From_File
        (Kernel.Registry.all, Source_Filename);
      Other : constant String := Other_File_Name (Project, Source_Filename);
   begin
      if Full_Name then
         return Find_On_Path (Project, Other, False);
      else
         return Other;
      end if;
   end Other_File_Name;

   ------------------------------
   -- Parse_All_LI_Information --
   ------------------------------

   procedure Parse_All_LI_Information
     (Kernel       : access Kernel_Handle_Record;
      In_Directory : String)
   is
      Handler : constant Glide_Language_Handler :=
        Glide_Language_Handler (Get_Language_Handler (Kernel));
      Num     : constant Natural := LI_Handlers_Count (Handler);
      LI      : LI_Handler;

   begin
      for L in 1 .. Num loop
         LI := Get_Nth_Handler (Handler, L);

         if LI /= null then
            Parse_All_LI_Information
              (LI,
               Kernel.Source_Info_List,
               In_Directory,
               Get_Project (Kernel),
               Get_Predefined_Source_Path (Kernel),
               Get_Predefined_Object_Path (Kernel));
         end if;
      end loop;
   end Parse_All_LI_Information;

   --------------------
   -- Find_Next_Body --
   --------------------

   procedure Find_Next_Body
     (Kernel      : access Kernel_Handle_Record;
      Lib_Info    : LI_File_Ptr;
      File_Name   : String;
      Entity_Name : String;
      Line        : Positive;
      Column      : Positive;
      Location    : out Src_Info.File_Location;
      Status      : out Find_Decl_Or_Body_Query_Status)
   is
      Project : constant Project_Type := Get_Project_From_File
        (Kernel.Registry.all, File_Name);
      Entity : Entity_Information;

   begin
      Find_Next_Body
        (Lib_Info, File_Name, Entity_Name, Line, Column,
         Get_LI_Handler_From_File
           (Glide_Language_Handler (Kernel.Lang_Handler), File_Name),
         Kernel.Source_Info_List,
         Project,
         Get_Predefined_Source_Path (Kernel),
         Get_Predefined_Object_Path (Kernel),
         Location, Status);

      if Status = Overloaded_Entity_Found then
         --  Ask the user what entity he is speaking about
         Find_Declaration_Or_Overloaded
           (Kernel      => Kernel,
            Lib_Info    => Lib_Info,
            File_Name   => File_Name,
            Entity_Name => Entity_Name,
            Line        => Line,
            Column      => Column,
            Entity      => Entity,
            Status      => Status);

         --  And search for the body of that one
         if Status = Success or else Status = Fuzzy_Match then
            Find_Next_Body
              (Kernel      => Kernel,
               Lib_Info    => Lib_Info,
               File_Name   => Get_Declaration_File_Of (Entity),
               Entity_Name => Entity_Name,
               Line        => Get_Declaration_Line_Of (Entity),
               Column      => Get_Declaration_Column_Of (Entity),
               Location    => Location,
               Status      => Status);
            Destroy (Entity);
         end if;
      end if;
   end Find_Next_Body;

   -------------------
   -- Row_Activated --
   -------------------

   procedure Row_Activated (Widget : access Gtk_Widget_Record'Class) is
   begin
      Response (Gtk_Dialog (Widget), Gtk_Response_OK);
   end Row_Activated;

   -------------------------------
   -- Select_Entity_Declaration --
   -------------------------------

   procedure Select_Entity_Declaration
     (Kernel        : access Kernel_Handle_Record'Class;
      Lib_Info      : LI_File_Ptr;
      Entity_Name   : String;
      Decl          : out Entity_Information;
      Status        : out Src_Info.Queries.Find_Decl_Or_Body_Query_Status)
   is
      procedure Set
        (Tree, Iter : System.Address;
         Col1 : Gint; Value1 : String;
         Col2, Value2, Col3, Value3 : Gint;
         Col4 : Gint; Value4 : String;
         Col5 : Gint; Value5 : Gint;
         Final : Gint := -1);
      pragma Import (C, Set, "gtk_tree_store_set");

      Column_Types : constant GType_Array :=
        (0 => GType_String,
         1 => GType_Int,
         2 => GType_Int,
         3 => GType_String,
         4 => GType_Int);

      Iter      : Entity_Declaration_Iterator;
      Candidate : Entity_Information;
      Button    : Gtk_Widget;
      pragma Unreferenced (Button);

      Count     : Natural := 0;

      Label     : Gtk_Label;
      Model     : Gtk_Tree_Store;
      Dialog    : Gtk_Dialog;
      It        : Gtk_Tree_Iter;
      Renderer  : Gtk_Cell_Renderer_Text;
      View      : Gtk_Tree_View;
      Col       : Gtk_Tree_View_Column;
      Col_Num   : Gint;
      Index     : Gint := 1;
      pragma Unreferenced (Col_Num);

   begin
      Iter := Find_All_Possible_Declarations
        (Lib_Info    => Lib_Info,
         Entity_Name => Entity_Name);

      while not At_End (Iter) loop
         Count := Count + 1;
         Candidate := Get (Iter);

         if Count = 1 then
            Gtk_New (Dialog,
                     Title  => -"Select the declaration",
                     Parent => Get_Main_Window (Kernel),
                     Flags  => Modal or Destroy_With_Parent);

            Gtk_New (Label, -"This entity is overloaded.");
            Pack_Start (Get_Vbox (Dialog), Label, Expand => False);

            Gtk_New (Label, -"Please select the appropriate declaration.");
            Pack_Start (Get_Vbox (Dialog), Label, Expand => False);

            Gtk_New (View);
            Set_Mode (Get_Selection (View), Selection_Single);
            Pack_Start (Get_Vbox (Dialog), View);

            Gtk_New (Model, Column_Types);
            Set_Model (View, Gtk_Tree_Model (Model));

            Gtk_New (Renderer);

            Gtk_New (Col);
            Col_Num := Append_Column (View, Col);
            Set_Title (Col, -"File");
            Pack_Start (Col, Renderer, False);
            Add_Attribute (Col, Renderer, "text", 0);

            Gtk_New (Col);
            Col_Num := Append_Column (View, Col);
            Set_Title (Col, -"Line");
            Pack_Start (Col, Renderer, False);
            Add_Attribute (Col, Renderer, "text", 1);

            Gtk_New (Col);
            Col_Num := Append_Column (View, Col);
            Set_Title (Col, -"Column");
            Pack_Start (Col, Renderer, False);
            Add_Attribute (Col, Renderer, "text", 2);

            Gtk_New (Col);
            Col_Num := Append_Column (View, Col);
            Set_Title (Col, -"Entity name");
            Pack_Start (Col, Renderer, False);
            Add_Attribute (Col, Renderer, "text", 3);

            Widget_Callback.Object_Connect
              (View, "row_activated",
               Widget_Callback.To_Marshaller (Row_Activated'Access),
               Dialog);

            Button := Add_Button (Dialog, Stock_Ok, Gtk_Response_OK);
            Button := Add_Button
              (Dialog, Stock_Cancel, Gtk_Response_Cancel);
         end if;

         Append (Model, It, Null_Iter);
         Set (Get_Object (Model), It'Address,
              0, Get_Declaration_File_Of (Candidate) & ASCII.NUL,
              1, Gint (Get_Declaration_Line_Of (Candidate)),
              2, Gint (Get_Declaration_Column_Of (Candidate)),
              3, Entity_Name & ASCII.NUL,
              4, Index);

         Index := Index + 1;
         Destroy (Candidate);

         Next (Iter);
      end loop;

      Destroy (Iter);

      Decl := No_Entity_Information;
      Status := Entity_Not_Found;

      if Count > 0 then
         Show_All (Dialog);

         if Run (Dialog) = Gtk_Response_OK then
            Status := Success;
            Get_Selected (Get_Selection (View), Gtk_Tree_Model (Model), It);

            Index := Get_Int (Model, It, 4);

            Iter := Find_All_Possible_Declarations
              (Lib_Info    => Lib_Info,
               Entity_Name => Entity_Name);
            while Index /= 1 loop
               Next  (Iter);
            end loop;

            Decl := Get (Iter);
            Destroy (Iter);
         end if;

         Destroy (Dialog);
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception " & Exception_Information (E));

         if Dialog /= null then
            Destroy (Dialog);
         end if;

         Destroy (Iter);
         raise;
   end Select_Entity_Declaration;

   ------------------------------------
   -- Find_Declaration_Or_Overloaded --
   ------------------------------------

   procedure Find_Declaration_Or_Overloaded
     (Kernel        : access Kernel_Handle_Record;
      Lib_Info      : Src_Info.LI_File_Ptr;
      File_Name     : String;
      Entity_Name   : String;
      Line          : Positive;
      Column        : Positive;
      Entity        : out Src_Info.Queries.Entity_Information;
      Status        : out Src_Info.Queries.Find_Decl_Or_Body_Query_Status) is
   begin
      Find_Declaration
        (Lib_Info, File_Name, Entity_Name, Line, Column, Entity, Status);

      --  ??? Should have the preference for the handling of fuzzy matches:
      --   - consider it as a no match: set Status to Entity_Not_Found;
      --   - consider it as overloaded entity: same as below;
      --   - use the closest match: nothing to do.

      if Status = Overloaded_Entity_Found then
         Select_Entity_Declaration
           (Kernel, Lib_Info, Entity_Name, Entity, Status);
      end if;
   end Find_Declaration_Or_Overloaded;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (Handle : access Kernel_Handle_Record; Home_Dir : String)
   is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (History_Record, History);
      Dir : constant String := String_Utils.Name_As_Directory (Home_Dir);
   begin
      Trace (Me, "Saving preferences in " & Dir & "preferences");
      Save_Preferences (Handle, Dir & "preferences");

      Save (Handle.History.all, Dir & "history");
      Free (Handle.History.all);
      Unchecked_Free (Handle.History);

      Destroy (Handle.Preferences);
      Free (Handle.Gnatls_Cache);
      Free (Handle.Home_Dir);

      Destroy (Handle.Registry.all);
      Unchecked_Free (Handle.Registry);

      if Handle.Current_Context /= null then
         Free (Handle.Current_Context);
      end if;

      if Handle.Last_Context_For_Contextual /= null then
         Free (Handle.Last_Context_For_Contextual);
      end if;

      Command_List.Free (Handle.Commands_List);

      Destroy (Handle.Modules_Data.all);
      Unchecked_Free (Handle.Modules_Data);

      Destroy (Glide_Language_Handler (Handle.Lang_Handler));
      Reset (Handle.Source_Info_List);
      Free (Handle.Logs_Mapper);
      Free_Modules (Handle);
      Unref (Handle.Tooltips);

      --  Free the memory allocated by gtk+, and disconnect all the callbacks,
      --  reclaiming the associated memory.
      Unref (Handle);

      Kernel_Desktop.Free_Registered_Desktop_Functions;
   end Destroy;

   -----------------
   -- Get_History --
   -----------------

   function Get_History
     (Handle : access Kernel_Handle_Record) return Histories.History is
   begin
      return Handle.History;
   end Get_History;

   --------------------
   -- Add_To_History --
   --------------------

   procedure Add_To_History
     (Handle    : access Kernel_Handle_Record;
      Key       : Histories.History_Key;
      New_Entry : String) is
   begin
      Add_To_History (Handle.History.all, Key, New_Entry);
   end Add_To_History;

   ---------------------
   -- Scope_To_String --
   ---------------------

   function Scope_To_String (Scope : Src_Info.E_Scope) return String is
   begin
      case Scope is
         when Global_Scope => return -"global";
         when Local_Scope  => return -"local";
         when Class_Static => return -"static";
         when Static_Local => return -"static";
      end case;
   end Scope_To_String;

   --------------------
   -- Get_Scope_Tree --
   --------------------

   procedure Get_Scope_Tree
     (Kernel : access Kernel_Handle_Record;
      Entity : Entity_Information;
      Tree   : out Src_Info.Queries.Scope_Tree;
      Node   : out Src_Info.Queries.Scope_Tree_Node)
   is
      Lib_Info : LI_File_Ptr;
      Location : File_Location;
      Status   : Find_Decl_Or_Body_Query_Status;
   begin
      Lib_Info := Locate_From_Source_And_Complete
        (Kernel, Get_Declaration_File_Of (Entity));

      if Lib_Info /= No_LI_File then
         --  We need to find the body of the entity in fact. In Ada, this
         --  will always be the same LI as the spec, but this is no
         --  longer true for C or C++.
         Find_Next_Body
           (Kernel             => Kernel,
            Lib_Info           => Lib_Info,
            File_Name          => Get_Declaration_File_Of (Entity),
            Entity_Name        => Get_Name (Entity),
            Line               => Get_Declaration_Line_Of (Entity),
            Column             => Get_Declaration_Column_Of (Entity),
            Location           => Location,
            Status             => Status);

         --  In case there is no body, do nothing.
         if Location /= Null_File_Location then
            Lib_Info := Locate_From_Source_And_Complete
              (Kernel, Get_File (Location));
         end if;
      end if;

      if Lib_Info = No_LI_File then
         Insert (Kernel,
                 -"LI file not found for " & Get_Declaration_File_Of (Entity));
         Tree := Null_Scope_Tree;
         Node := Null_Scope_Tree_Node;
         return;
      end if;

      Tree := Create_Tree (Lib_Info);

      if Tree = Null_Scope_Tree then
         Trace (Me, "Couldn't create scope tree for "
                & Get_LI_Filename (Lib_Info));
         Node := Null_Scope_Tree_Node;
         return;
      end if;

      Node := Find_Entity_Scope (Tree, Entity);

      if Node = Null_Scope_Tree_Node then
         Insert (Kernel,
                 -"Couldn't find the scope tree for " & Get_Name (Entity));
         Trace (Me, "Couldn't find entity "
                & Get_Name (Entity) & " in "
                & Get_LI_Filename (Lib_Info)
                & " at line" & Get_Declaration_Line_Of (Entity)'Img
                & " column"  & Get_Declaration_Column_Of (Entity)'Img);
         Free (Tree);
         Node := Null_Scope_Tree_Node;
      end if;
   end Get_Scope_Tree;

end Glide_Kernel;
