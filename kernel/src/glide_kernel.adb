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
with Glib.Object;               use Glib.Object;
with Gdk.Pixbuf;                use Gdk.Pixbuf;
with Gtk.Handlers;              use Gtk.Handlers;
with Gtk.Main;                  use Gtk.Main;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Tooltips;              use Gtk.Tooltips;
with Gtkada.MDI;                use Gtkada.MDI;
with System;                    use System;

with Ada.Tags;                  use Ada.Tags;
with Ada.Text_IO;               use Ada.Text_IO;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Ada.Unchecked_Deallocation;
with String_Utils;              use String_Utils;
with Gint_Xml;                  use Gint_Xml;
with Glide_Main_Window;         use Glide_Main_Window;
with Glide_Kernel.Preferences;  use Glide_Kernel.Preferences;
with Glide_Kernel.Project;      use Glide_Kernel.Project;
with Glide_Kernel.Console;      use Glide_Kernel.Console;
with Glide_Page;                use Glide_Page;
with GVD.Process;               use GVD.Process;
with GVD.Main_Window;           use GVD.Main_Window;
with Interfaces.C.Strings;      use Interfaces.C.Strings;
with Interfaces.C;              use Interfaces.C;
with GUI_Utils;                 use GUI_Utils;
with Src_Info;                  use Src_Info;
with Src_Info.Queries;          use Src_Info.Queries;

with Glide_Kernel.Timeout;      use Glide_Kernel.Timeout;
with Prj_API;                   use Prj_API;
with Generic_List;

with Language_Handlers;         use Language_Handlers;
with Language_Handlers.Glide;   use Language_Handlers.Glide;

with Prj.Tree;                  use Prj.Tree;

with Traces;                    use Traces;

package body Glide_Kernel is

   Signals : constant chars_ptr_array :=
     (1 => New_String (Project_Changed_Signal),
      2 => New_String (Project_View_Changed_Signal),
      3 => New_String (Context_Changed_Signal),
      4 => New_String (Variable_Changed_Signal));
   --  The list of signals defined for this object

   Kernel_Class : GObject_Class := Uninitialized_Class;
   --  The class structure for this object

   Me : Debug_Handle := Create ("glide_kernel");

   package Object_Callback is new Gtk.Handlers.Callback
     (Glib.Object.GObject_Record);

   procedure Reset_Source_Info_List
     (Handle : access Kernel_Handle_Record'Class);
   --  Re-initialize the Source Info structure.
   --  ??? Needs more comments.

   function Process_Anim (Data : Process_Data) return Boolean;
   --  Process_Timeout callback to handle image animations.

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
        (1 .. 2 | 4 => (1 => GType_None),
         3          => (1 => GType_Pointer));
      Handler : Glide_Language_Handler;
   begin
      Handle := new Kernel_Handle_Record;
      Handle.Main_Window := Main_Window;
      Handle.Home_Dir := new String' (Home_Dir);
      Glib.Object.Initialize (Handle);
      Initialize_Class_Record
        (Handle, Signals, Kernel_Class, "GlideKernel", Signal_Parameters);

      --  Create the language handler. It is also set for the gvd main window,
      --  so that the embedded gvd uses the same mechanism as the rest of glide
      --  to guess the language for a file name.
      Gtk_New (Handler);
      Handle.Lang_Handler := Language_Handler (Handler);
      Glide_Window (Handle.Main_Window).Lang_Handler :=
        Handle.Lang_Handler;

      Handle.Project := Create_Default_Project ("default", Get_Current_Dir);
      Handle.Project_Is_Default := True;
      Handle.Predefined_Source_Path := null;
      Handle.Predefined_Object_Path := null;
      Handle.Gnatls_Cache := null;
      Recompute_View (Handle);
      Reset_Source_Info_List (Handle);

      Gtk_New (Handle.Tooltips);
      Load_Preferences
        (Handle, String_Utils.Name_As_Directory (Home_Dir) & "preferences");
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

   ---------------------------
   -- Save_All_MDI_Children --
   ---------------------------

   function Save_All_MDI_Children
     (Handle : access Kernel_Handle_Record) return Boolean
   is
      MDI   : MDI_Window := Get_MDI (Handle);
      Iter  : Child_Iterator;
      Child : MDI_Child;

      use type Module_List.List_Node;
      Module : Module_List.List_Node;

   begin
      Iter := First_Child (MDI);
      Child := Get (Iter);

      --  Browse through all MDI children.

      while Child /= null loop
         --  Find the module associated to Child.
         Module := Module_List.First (Handle.Modules_List);

         while Module /= Module_List.Null_Node loop
            if Module_List.Data (Module).Child_Tag =
              Get_Widget (Child)'Tag
            then
               if Module_List.Data (Module).Save_Function /= null
                 and then not Module_List.Data (Module).Save_Function
                                (Handle,
                                 Get_Widget (Child),
                                 False)
               then
                  return False;
               end if;

               exit;
            end if;

            Module := Module_List.Next (Module);
         end loop;

         Next (Iter);
         Child := Get (Iter);
      end loop;

      return True;
   end Save_All_MDI_Children;

   -------------------------------------
   -- Locate_From_Source_And_Complete --
   -------------------------------------

   function Locate_From_Source_And_Complete
     (Handle          : access Kernel_Handle_Record;
      Source_Filename : String) return Src_Info.LI_File_Ptr
   is
      use type Prj.Project_Id;
      File : LI_File_Ptr := Locate_From_Source
        (Handle.Source_Info_List, Source_Filename);
      Project : Prj.Project_Id := Get_Project_From_File
        (Get_Project_View (Handle), Base_Name (Source_Filename));
   begin
      pragma Assert (Project /= Prj.No_Project);
      Trace (Me, "Locate_From_Source_And_Complete: " & Source_Filename);

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
      LI_Once      : Boolean := False) is
   begin
      Find_All_References
        (Get_Project (Kernel),
         Get_Language_Handler (Kernel),
         Entity, Kernel.Source_Info_List,
         Iterator, Project, LI_Once);
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

   ----------------------
   -- Variable_Changed --
   ----------------------

   procedure Variable_Changed (Handle : access Kernel_Handle_Record) is
   begin
      Free (Handle.Scenario_Variables);
      Object_Callback.Emit_By_Name (Handle, Variable_Changed_Signal);
   end Variable_Changed;

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
      --  ??? the current child. However, it might have some unexpected effect
      --  ??? for the user.

      while Module /= Module_List.Null_Node loop
         if Module_List.Data (Module).Child_Tag = Get_Widget (C)'Tag then
            if Module_List.Data (Module).Default_Factory /= null then
               Kernel.Current_Context :=
                 Module_List.Data (Module).Default_Factory
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
      MDI  : constant MDI_Window := Glide_Page.Glide_Page
        (Get_Current_Process (Handle.Main_Window)).Process_Mdi;
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
         Node := Parse (File);
         pragma Assert (Node.Tag.all = "MDI");

         Kernel_Desktop.Restore_Desktop (MDI, Node, Kernel_Handle (Handle));
         return True;
      else
         return False;
      end if;
   end Load_Desktop;

   ----------
   -- Free --
   ----------

   procedure Free (Module : in out Module_ID) is
      pragma Unreferenced (Module);
   begin
      null;
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
      Page       : Glide_Page.Glide_Page :=
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
   begin
      return Anim_Cb (Data.Kernel);
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
            Process_Anim'Access, (Kernel_Handle (Handle), null, null));
      end if;

      Window.State_Level := Window.State_Level + 1;
   end Push_State;

   ---------------
   -- Pop_State --
   ---------------

   procedure Pop_State (Handle : Kernel_Handle) is
      Window : Glide_Window;
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

         if Window.State_Level = 0
           and then Window.Timeout_Id /= 0
         then
            Timeout_Remove (Window.Timeout_Id);
            Window.Timeout_Id := 0;
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
     return Basic_Mapper.File_Mapper_Access
   is
   begin
      return Handle.Logs_Mapper;
   end Get_Logs_Mapper;

   ---------------------
   -- Set_Logs_Mapper --
   ---------------------

   procedure Set_Logs_Mapper
     (Handle : access Kernel_Handle_Record;
      Mapper : Basic_Mapper.File_Mapper_Access)
   is
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
                        Insert
                          (Kernel, "Path for " & Other_File & " not found");
                     end if;
                  end;
               else
                  return Other_File;
               end if;
            else
               Insert (Kernel, "No other file found", Mode => Error);
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
      Handler : Glide_Language_Handler :=
        Glide_Language_Handler (Get_Language_Handler (Kernel));
      Num : constant Natural := Languages_Count (Handler);
      LI  : LI_Handler;
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
   begin
      Find_Next_Body (Lib_Info, File_Name, Entity_Name, Line, Column,
                      Get_LI_Handler_From_File
                        (Glide_Language_Handler (Kernel.Lang_Handler),
                         File_Name, Project),
                      Kernel.Source_Info_List,
                      Project,
                      Get_Predefined_Source_Path (Kernel),
                      Get_Predefined_Object_Path (Kernel),
                      Location, Status);
   end Find_Next_Body;

end Glide_Kernel;
