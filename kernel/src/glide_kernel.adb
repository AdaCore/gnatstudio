-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
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

with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.Expect;               use GNAT.Expect;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Ada.Unchecked_Deallocation;
with String_Utils;              use String_Utils;
with Gint_Xml;                  use Gint_Xml;
with Glide_Main_Window;         use Glide_Main_Window;
with Glide_Kernel.Preferences;  use Glide_Kernel.Preferences;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with Glide_Kernel.Project;      use Glide_Kernel.Project;
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
with Namet;                     use Namet;
with Generic_List;

with Language;                  use Language;
with Language.Ada;              use Language.Ada;
with Language.C;                use Language.C;
with Language.Cpp;              use Language.Cpp;

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
   begin
      Handle := new Kernel_Handle_Record;
      Handle.Main_Window := Main_Window;
      Handle.Home_Dir := new String' (Home_Dir);
      Glib.Object.Initialize (Handle);
      Initialize_Class_Record
        (Handle, Signals, Kernel_Class, "GlideKernel", Signal_Parameters);

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

      --  ??? Should use naming schemes instead. This duplicates the
      --  information uselessly.
      Reset_File_Extensions;
      Add_File_Extensions (Ada_Lang, Get_Pref (Handle, Ada_Extensions));
      Add_File_Extensions (C_Lang,   Get_Pref (Handle, C_Extensions));
      Add_File_Extensions (Cpp_Lang, Get_Pref (Handle, Cpp_Extensions));
      Register_Default_Naming_Schemes;

      Handle.Explorer_Context := new File_Selection_Context;
      Set_Context_Information (Handle.Explorer_Context, Handle, null);
      Set_File_Information
        (File_Selection_Context_Access (Handle.Explorer_Context));
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

   ----------------------------
   -- Initialize_All_Modules --
   ----------------------------

   procedure Initialize_All_Modules (Handle : access Kernel_Handle_Record) is
      Module : Module_List.List_Node :=
        Module_List.First (Handle.Modules_List);

      use type Module_List.List_Node;
   begin
      while Module /= Module_List.Null_Node loop
         if not Module_List.Data (Module).Was_Initialized then
            Trace (Me, "Initializing module "
                   & Module_List.Data (Module).Name);

            if Module_List.Data (Module).Initializer /= null then
               Module_List.Data (Module).Initializer (Handle);
            end if;

            Module_List.Data (Module).Was_Initialized := True;
         end if;

         Module := Module_List.Next (Module);
      end loop;
   end Initialize_All_Modules;

   ------------------------------
   -- Compute_Predefined_Paths --
   ------------------------------

   procedure Compute_Predefined_Paths (Handle : access Kernel_Handle_Record) is
      Source_Path : Boolean := True;

      procedure Add_Directory (S : String);
      --  Add S to the search path.
      --  If Source_Path is True, the source path is modified.
      --  Otherwise, the object path is modified.

      procedure Add_Directory (S : String) is
         Tmp : String_Access;
      begin
         if S = "" then
            return;

         elsif S = "<Current_Directory>" then
            if Source_Path then
               Tmp := Handle.Predefined_Source_Path;
               Handle.Predefined_Source_Path :=
                 new String' (Handle.Predefined_Source_Path.all & ":.");

            else
               Tmp := Handle.Predefined_Object_Path;
               Handle.Predefined_Object_Path :=
                 new String' (Handle.Predefined_Object_Path.all & ":.");
            end if;

         elsif Source_Path then
            Tmp := Handle.Predefined_Source_Path;
            Handle.Predefined_Source_Path :=
              new String' (Handle.Predefined_Source_Path.all & ":" & S);

         else
            Tmp := Handle.Predefined_Object_Path;
            Handle.Predefined_Object_Path :=
              new String' (Handle.Predefined_Object_Path.all & ":" & S);
         end if;

         Free (Tmp);
      end Add_Directory;

      Fd     : Process_Descriptor;
      Result : Expect_Match;
      Args   : Argument_List (1 .. 1);
      Gnatls : constant String := Get_Attribute_Value
        (Get_Project_View (Handle), Gnatlist_Attribute,
         Ide_Package, Default => "gnatls");

   begin
      --  If the gnatls commands hasn't changed, no need to recompute the
      --  predefined paths.

      if Handle.Gnatls_Cache /= null
        and then Handle.Gnatls_Cache.all = Gnatls
      then
         return;
      end if;

      Free (Handle.Gnatls_Cache);
      Handle.Gnatls_Cache := new String' (Gnatls);

      Free (Handle.Predefined_Source_Path);
      Free (Handle.Predefined_Object_Path);
      Handle.Predefined_Source_Path := new String' ("");
      Handle.Predefined_Object_Path := new String' ("");

      Args (1) := new String' ("-v");
      Non_Blocking_Spawn
        (Fd, Gnatls, Args, Buffer_Size => 0, Err_To_Out => True);
      Free (Args (1));
      Expect (Fd, Result, "Source Search Path:\n", Timeout => -1);

      loop
         Expect (Fd, Result, "\n", Timeout => -1);

         declare
            S : constant String := Trim (Expect_Out (Fd), Ada.Strings.Left);
         begin
            if S = "Object Search Path:" & ASCII.LF then
               Source_Path := False;
            else
               Add_Directory (S (S'First .. S'Last - 1));
            end if;
         end;
      end loop;

   exception
      when Process_Died =>
         Close (Fd);
   end Compute_Predefined_Paths;

   --------------------------------
   -- Get_Predefined_Source_Path --
   --------------------------------

   function Get_Predefined_Source_Path
     (Handle : access Kernel_Handle_Record) return String
   is
      Gnatlist : String_Access;
   begin
      if Handle.Predefined_Source_Path = null then
         --  Gnatlist := new String' (Get_Gnatlist (Handle.Project));
         Gnatlist := new String' ("");

         if Gnatlist.all = "" then
            Free (Gnatlist);
            Gnatlist := new String' ("gnatls");
         end if;

         --  Parse_Gnatlist (Gnatlist);
         Free (Gnatlist);

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
      Trace (Me, "Locate_From_Source_And_Complete: "
             & Source_Filename
             & " "
             & Get_Name_String (Prj.Projects.Table (Project).Name));

      --  ??? Optimization: we could use only the direct object path from
      --  Project, since we know for sure that the file belongs to it.

      Create_Or_Complete_LI
        (Handler                =>
           Handler_From_Filename (Project, Source_Filename),
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
        (Get_Project (Kernel), Entity, Kernel.Source_Info_List,
         Iterator, Project, LI_Once);
   end Find_All_References;

   ----------
   -- Next --
   ----------

   procedure Next
     (Kernel : access Kernel_Handle_Record;
      Iterator : in out Entity_Reference_Iterator) is
   begin
      Next (Iterator, Kernel.Source_Info_List);
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
        (Get_Project (Kernel), Source_Filename,
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
      Next (Iterator, Kernel.Source_Info_List);
   end Next;

   ----------------------------
   -- Reset_Source_Info_List --
   ----------------------------

   procedure Reset_Source_Info_List
     (Handle : access Kernel_Handle_Record'Class) is
   begin
      Src_Info.Reset (Handle.Source_Info_List);
   end Reset_Source_Info_List;

   --------------------------
   -- Get_Source_Info_List --
   --------------------------

   function Get_Source_Info_List
     (Handle : access Kernel_Handle_Record) return Src_Info.LI_File_List is
   begin
      return Handle.Source_Info_List;
   end Get_Source_Info_List;

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

      File : File_Selection_Context_Access;
   begin
      if Module_Name (Get_Creator (Context)) = Explorer_Module_Name then
         Free (Handle.Explorer_Context);
         Handle.Explorer_Context := new File_Selection_Context;
         Set_Context_Information
           (Handle.Explorer_Context, Handle, Get_Creator (Context));

         File := File_Selection_Context_Access (Context);
         Set_File_Name_Information
           (File_Selection_Context_Access (Handle.Explorer_Context),
            Directory_Information (File),
            File_Information (File));
         Set_File_Information
           (File_Selection_Context_Access (Handle.Explorer_Context),
            Project_Information (File));
      end if;

      Internal
        (Get_Object (Handle),
         Context_Changed_Signal & ASCII.NUL,
         Selection_Context_Access (Context));
   end Context_Changed;

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

   ----------------------------------
   -- Get_Current_Explorer_Context --
   ----------------------------------

   function Get_Current_Explorer_Context
     (Handle : access Kernel_Handle_Record'Class)
      return Selection_Context_Access is
   begin
      return Handle.Explorer_Context;
   end Get_Current_Explorer_Context;

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

      if Window.State_Level = 0 and then Window.Timeout_Id = 0 then
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

         if Window.State_Level = 0 then
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

end Glide_Kernel;
