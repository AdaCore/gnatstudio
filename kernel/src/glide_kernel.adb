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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Glib;                      use Glib;
with Glib.Xml_Int;              use Glib.Xml_Int;
with Glib.Object;               use Glib.Object;
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
with Gtk.Tooltips;              use Gtk.Tooltips;
with Gtkada.Handlers;           use Gtkada.Handlers;
with Gtkada.MDI;                use Gtkada.MDI;
with System;                    use System;

with String_Utils;              use String_Utils;
with String_List_Utils;         use String_List_Utils;
with Glide_Intl;                use Glide_Intl;
with Glide_Main_Window;         use Glide_Main_Window;
with Glide_Interactive_Consoles; use Glide_Interactive_Consoles;
with Default_Preferences;       use Default_Preferences;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with Glide_Kernel.Preferences;  use Glide_Kernel.Preferences;
with Glide_Kernel.Project;      use Glide_Kernel.Project;
with Glide_Kernel.Console;      use Glide_Kernel.Console;
with Glide_Page;                use Glide_Page;
with GVD.Preferences;           use GVD.Preferences;
with GVD.Process;               use GVD.Process;
with GVD.Main_Window;           use GVD.Main_Window;
with Interfaces.C.Strings;      use Interfaces.C.Strings;
with Interfaces.C;              use Interfaces.C;
with GUI_Utils;                 use GUI_Utils;
with File_Utils;                use File_Utils;
with Src_Info;                  use Src_Info;
with Src_Info.Queries;          use Src_Info.Queries;
with Basic_Mapper;              use Basic_Mapper;
with Basic_Types;               use Basic_Types;

with Glide_Kernel.Timeout;      use Glide_Kernel.Timeout;
with Prj_API;                   use Prj_API;
with Generic_List;

with Language_Handlers;         use Language_Handlers;
with Language_Handlers.Glide;   use Language_Handlers.Glide;

with Prj;                       use Prj;
with Prj.Tree;                  use Prj.Tree;

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

      9  => New_String (Preferences_Changed_Signal),
      10  => New_String (Search_Regexps_Changed_Signal),
      11 => New_String (Search_Reset_Signal),
      12 => New_String (Search_Functions_Changed_Signal));
   --  The list of signals defined for this object

   Kernel_Class : GObject_Class := Uninitialized_Class;
   --  The class structure for this object

   Me : constant Debug_Handle := Create ("glide_kernel");

   package Object_Callback is new Gtk.Handlers.Callback
     (Glib.Object.GObject_Record);

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

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Handle      : out Kernel_Handle;
      Main_Window : Gtk.Window.Gtk_Window;
      Home_Dir    : String)
   is
      Signal_Parameters : constant Signal_Parameter_Types :=
        (1 .. 2 | 4 | 9 .. 12 => (1 => GType_None),
         3      | 5           => (1 => GType_Pointer),
         6 .. 8               => (1 => GType_String));
      Handler : Glide_Language_Handler;
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

      Handle.Project := Create_Default_Project ("default", Get_Current_Dir);
      Handle.Project_Is_Default     := True;
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
      Load_Preferences
        (Handle.Preferences,
         String_Utils.Name_As_Directory (Home_Dir) & "preferences");
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
      MDI   : constant MDI_Window := Get_MDI (Handle);
   begin
      --  ??? the following implementation assumes that the file editors
      --  are MDI childs that have corresponding file names for title, and
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
      MDI    : constant MDI_Window := Get_MDI (Handle);
      Iter   : Child_Iterator;
      Child  : MDI_Child;
      Module : Module_ID;
      Save_Type : Save_Return_Value;
      F      : Boolean := Force;
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
         --  Find the module associated to Child.
         Module := Get_Module_From_Child (Handle, Child);

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
      use type Prj.Project_Id;
      File : LI_File_Ptr;
      Project : Prj.Project_Id := Get_Project_From_File
        (Get_Project_View (Handle), Base_Name (Source_Filename));
   begin
      if Project = Prj.No_Project then
         Project := Get_Project_View (Handle);
      end if;

      Create_Or_Complete_LI
        (Handler                => Get_LI_Handler_From_File
           (Glide_Language_Handler (Handle.Lang_Handler),
            Source_Filename,
            Project),
         File                   => File,
         Source_Filename        => Source_Filename,
         List                   => Handle.Source_Info_List,
         Project                => Project,
         Predefined_Source_Path => Get_Predefined_Source_Path (Handle),
         Predefined_Object_Path => Get_Predefined_Object_Path (Handle));
      return File;
   end Locate_From_Source_And_Complete;

   -------------------------
   -- Find_All_References --
   -------------------------

   procedure Find_All_References
     (Kernel       : access Kernel_Handle_Record;
      Entity       : Src_Info.Queries.Entity_Information;
      Iterator     : out Src_Info.Queries.Entity_Reference_Iterator;
      Project      : Prj.Project_Id := Prj.No_Project;
      In_File      : String := "";
      LI_Once      : Boolean := False) is
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
      Project         : Prj.Project_Id := Prj.No_Project) is
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
      Free (Handle.Scenario_Variables);
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

   -------------------------
   -- Get_Current_Context --
   -------------------------

   function Get_Current_Context (Kernel : access Kernel_Handle_Record)
      return Selection_Context_Access
   is
      use type Module_List.List_Node;
      C : MDI_Child;
      Module : Module_List.List_Node :=
        Module_List.First (Kernel.Modules_List);
   begin
      if Kernel.Current_Context /= null then
         Free (Kernel.Current_Context);
      end if;

      C := Get_Focus_Child (Get_MDI (Kernel));
      if C = null then
         return null;
      end if;

      --  ??? Should we fall back on the explorer if no factory was defined for
      --  the current child. However, it might have some unexpected effect
      --  for the user.

      while Module /= Module_List.Null_Node loop
         if Module_List.Data (Module).Info.Child_Tag = Get_Widget (C)'Tag then
            if Module_List.Data (Module).Info.Default_Factory /= null then
               Kernel.Current_Context :=
                 Module_List.Data (Module).Info.Default_Factory
                 (Kernel, Get_Widget (C));

               if Kernel.Current_Context /= null then
                  Set_Context_Information
                    (Kernel.Current_Context,
                     Kernel,
                     Module_List.Data (Module));
               end if;
            end if;
            exit;
         end if;

         Module := Module_List.Next (Module);
      end loop;

      if Module = Module_List.Null_Node then
         Trace (Me, "Get_Current_Context: No module associated with tag "
                & External_Tag (Get_Widget (C)'Tag));
      elsif Kernel.Current_Context = null then
         Trace (Me, "Null context returned by the module");
      end if;

      return Kernel.Current_Context;
   end Get_Current_Context;

   ------------------
   -- Save_Desktop --
   ------------------

   procedure Save_Desktop
     (Handle : access Kernel_Handle_Record)
   is
      MDI  : constant MDI_Window :=
        Get_Current_Process (Handle.Main_Window).Process_Mdi;
      File : File_Type;

   begin
      Create
        (File,
         Mode => Out_File,
         Name =>
           String_Utils.Name_As_Directory (Handle.Home_Dir.all) & "desktop");
      Set_Output (File);

      Print (Glide_Kernel.Kernel_Desktop.Save_Desktop (MDI));

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
      MDI  : constant MDI_Window := Glide_Page.Glide_Page
        (Get_Current_Process (Handle.Main_Window)).Process_Mdi;
      Node : Node_Ptr;
      File : constant String :=
        String_Utils.Name_As_Directory (Handle.Home_Dir.all) & "desktop";

   begin
      if Is_Regular_File (File) then
         Trace (Me, "loading desktop file " & File);
         Node := Parse (File);
         Kernel_Desktop.Restore_Desktop (MDI, Node, Kernel_Handle (Handle));
         Free (Handle.Default_Desktop);
         return True;
      else
         Trace (Me, "loading default desktop");
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

   function Get_Kernel (Context : access Selection_Context)
      return Kernel_Handle is
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

   function Get_MDI (Handle : access Kernel_Handle_Record)
      return Gtkada.MDI.MDI_Window
   is
      Top        : constant Glide_Window := Glide_Window (Handle.Main_Window);
      Page       : constant Glide_Page.Glide_Page :=
        Glide_Page.Glide_Page (Get_Current_Process (Top));
   begin
      return Page.Process_Mdi;
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

   function Get_Main_Window (Handle : access Kernel_Handle_Record)
      return Gtk.Window.Gtk_Window is
   begin
      return Handle.Main_Window;
   end Get_Main_Window;

   ------------------
   -- Get_Tooltips --
   ------------------

   function Get_Tooltips (Handle : access Kernel_Handle_Record)
      return Gtk.Tooltips.Gtk_Tooltips is
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
      Console : Glide_Interactive_Console;
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

   -----------------------
   -- Set_Search_Module --
   -----------------------

   procedure Set_Search_Module
     (Handle : access Kernel_Handle_Record;
      Search : access Gtk_Widget_Record'Class) is
   begin
      Handle.Search := Gtk_Widget (Search);
   end Set_Search_Module;

   -----------------------
   -- Get_Search_Module --
   -----------------------

   function Get_Search_Module
     (Handle : access Kernel_Handle_Record) return Gtk_Widget is
   begin
      return Handle.Search;
   end Get_Search_Module;

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

   -----------------------
   -- Get_Other_File_Of --
   -----------------------

   function Get_Other_File_Of
     (Kernel          : access Kernel_Handle_Record;
      Source_Filename : String;
      Full_Name       : Boolean := True) return String
   is
      Lib_Info : LI_File_Ptr;
   begin
      Lib_Info := Locate_From_Source_And_Complete (Kernel, Source_Filename);

      if Lib_Info /= No_LI_File then
         declare
            Other_File : constant String := Get_Other_File_Of
              (Lib_Info, Source_Filename);
         begin
            if Other_File /= "" then
               if Full_Name then
                  declare
                     Full_Name : constant String :=
                       Find_Source_File (Kernel, Other_File, True);
                  begin
                     if Full_Name /= "" then
                        return Full_Name;
                     else
                        Console.Insert
                          (Kernel,
                           (-"Path for ") & Other_File & (-" not found"),
                           Mode => Error);
                     end if;
                  end;
               else
                  return Other_File;
               end if;
            else
               Console.Insert (Kernel, -"No other file found", Mode => Error);
            end if;
         end;
      end if;

      return "";
   end Get_Other_File_Of;

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
               Get_Project_View (Kernel),
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
      Project : Prj.Project_Id := Get_Project_From_File
        (Get_Project_View (Kernel), File_Name);
      Entity : Entity_Information;
   begin
      if Project = Prj.No_Project then
         Project := Get_Project_View (Kernel);
      end if;

      Find_Next_Body
        (Lib_Info, File_Name, Entity_Name, Line, Column,
         Get_LI_Handler_From_File
           (Glide_Language_Handler (Kernel.Lang_Handler),
            File_Name, Project),
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
         Col6 : Gint; Value6 : Gint;
         Final : Gint := -1);
      pragma Import (C, Set, "gtk_tree_store_set");

      Column_Types : constant GType_Array :=
        (0 => GType_String,
         1 => GType_Int,
         2 => GType_Int,
         3 => GType_String,
         4 => GType_Int,
         5 => GType_Int);

      Iter      : Entity_Declaration_Iterator;
      Candidate : Entity_Information;
      Button    : Gtk_Widget;
      Count     : Natural := 0;

      Label     : Gtk_Label;
      Model     : Gtk_Tree_Store;
      Dialog    : Gtk_Dialog;
      It        : Gtk_Tree_Iter;
      Renderer  : Gtk_Cell_Renderer_Text;
      View      : Gtk_Tree_View;
      Col       : Gtk_Tree_View_Column;
      Col_Num   : Gint;

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
              4, E_Scope'Pos (Get_Scope (Candidate)),
              5, E_Kind'Pos (Get_Kind (Candidate)));

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
            Decl := Create
              (Name   => Entity_Name,
               Line   => Positive (Get_Int (Model, It, 1)),
               Column => Natural (Get_Int (Model, It, 2)),
               Scope  => E_Scope'Val (Get_Int (Model, It, 4)),
               Kind   => E_Kind'Val (Get_Int (Model, It, 5)),
               File   => Get_String (Model, It, 0));
         end if;
         Destroy (Dialog);
      end if;

   exception
      when E : others =>
         Trace (Me, "Select_Entity_Declaration: Unexpected exception "
                & Exception_Information (E));
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

   procedure Destroy (Handle : access Kernel_Handle_Record) is
   begin
      Destroy (Handle.Preferences);
      Project_Hash.Project_Htable.Reset (Handle.Projects_Data);
      Free (Handle.Gnatls_Cache);
      Free (Handle.Home_Dir);
      Free (Handle.Scenario_Variables);

      if Handle.Current_Context /= null then
         Free (Handle.Current_Context);
      end if;

      if Handle.Last_Context_For_Contextual /= null then
         Free (Handle.Last_Context_For_Contextual);
      end if;

      --  Free the register search functions
      if Handle.Search /= null then
         Destroy (Handle.Search);
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

   ---------------------
   -- Scope_To_String --
   ---------------------

   function Scope_To_String (Scope : Src_Info.E_Scope) return String is
   begin
      case Scope is
         when Global_Scope => return -"global";
         when Local_Scope  => return -"local";
         when Class_Static => return -"static";
         when Static_Local => return -"global in file (static)";
      end case;
   end Scope_To_String;

   --------------------
   -- Kind_To_String --
   --------------------

   function Kind_To_String (Kind : Src_Info.E_Kind) return String is
   begin
      --  ??? Would be nice to do it as a primitive subprogram of the
      --  LI_Handlers, unfortunately they currently don't have access to
      --  Glide_Intl for proper translations.

      case Kind is
         when Overloaded_Entity            => return "???";
         when Unresolved_Entity            => return -"unknown";
         when Access_Object                =>
            return -"access variable / pointer";
         when Access_Type                  => return -"access type / pointer";
         when Array_Object                 => return -"array";
         when Array_Type                   => return -"array type";
         when Boolean_Object               => return -"boolean";
         when Boolean_Type                 => return -"boolean type";
         when Class_Wide_Object            => return -"class wide";
         when Class_Wide_Type              => return -"class wide type";
         when Decimal_Fixed_Point_Object   => return -"decimal fixed point";
         when Decimal_Fixed_Point_Type     =>
            return -"decimal fixed point type";
         when Entry_Or_Entry_Family        => return -"entry or entry family";
         when Enumeration_Literal          => return -"enumeration literal";
         when Enumeration_Object           => return -"enumeration";
         when Enumeration_Type             => return -"enumeration type";
         when Exception_Entity             => return -"exception";
         when Floating_Point_Object        => return -"floating point";
         when Floating_Point_Type          => return -"floating point type";
         when Generic_Class                => return -"generic class";
         when Generic_Function_Or_Operator => return -"generic function";
         when Generic_Package              => return -"generic package";
         when Generic_Procedure            => return -"generic procedure";
         when Label_On_Block               => return -"label on block";
         when Label_On_Loop                => return -"label on loop";
         when Label_On_Statement           => return -"label on statement";
         when Modular_Integer_Object       => return -"modular integer";
         when Modular_Integer_Type         => return -"modular integer type";
         when Named_Number                 => return -"named number";
         when Non_Generic_Function_Or_Operator => return -"function";
         when Non_Generic_Package          => return -"package";
         when Non_Generic_Procedure        => return -"procedure";
         when Ordinary_Fixed_Point_Object  => return -"fixed point";
         when Ordinary_Fixed_Point_Type    => return -"fixed point type";
         when Private_Type                 => return -"private type";
         when Protected_Object             => return -"protected object";
         when Protected_Type               => return -"protected type";
         when Record_Object                => return -"record / struct";
         when Record_Type                  => return -"record type / struct";
         when Signed_Integer_Object        => return -"signed integer";
         when Signed_Integer_Type          => return -"signed integer type";
         when String_Object                => return -"string";
         when String_Type                  => return -"string type";
         when Task_Object                  => return -"task";
         when Task_Type                    => return -"task type";
      end case;
   end Kind_To_String;

end Glide_Kernel;
