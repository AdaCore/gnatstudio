-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
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
with Gtk.Handlers;              use Gtk.Handlers;
with Gtk.Tooltips;              use Gtk.Tooltips;
with Gtkada.MDI;                use Gtkada.MDI;
with System;                    use System;

with Ada.Text_IO;               use Ada.Text_IO;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Unchecked_Deallocation;
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
with OS_Utils;                  use OS_Utils;
with Src_Info;                  use Src_Info;
with Src_Info.ALI;

with Prj_API;                  use Prj_API;
with Generic_List;

with Language;                 use Language;
with Language.Ada;             use Language.Ada;
with Language.C;               use Language.C;
with Language.Cpp;             use Language.Cpp;

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

   function Get_Home_Directory return String;
   --  Return the home directory in which glide's user files should be stored
   --  (preferences, log, ...)

   procedure Reset_Source_Info_List
     (Handle : access Kernel_Handle_Record'Class);
   --  Re-initialize the Source Info structure.
   --  ??? Needs more comments.

   ------------------------
   -- Get_Home_Directory --
   ------------------------

   function Get_Home_Directory return String is
      Home, Dir : String_Access;
   begin
      Home := Getenv ("GLIDE_HOME");

      if Home.all = "" then
         Free (Home);
         Home := Getenv ("HOME");
      end if;

      if Home.all /= "" then
         if Is_Directory_Separator (Home (Home'Last)) then
            Dir := new String' (Home (Home'First .. Home'Last - 1) &
              Directory_Separator & ".glide");
         else
            Dir := new String' (Home.all & Directory_Separator & ".glide");
         end if;

      else
         --  Default to /
         Dir := new String'(Directory_Separator & ".glide");
      end if;

      declare
         D : constant String := Dir.all;
      begin
         Free (Home);
         Free (Dir);
         return D;
      end;
   end Get_Home_Directory;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Handle      : out Kernel_Handle;
      Main_Window : Gtk.Window.Gtk_Window)
   is
      Signal_Parameters : constant Signal_Parameter_Types :=
        (1 .. 2 | 4 => (1 => GType_None),
         3          => (1 => GType_Pointer));
   begin
      Handle := new Kernel_Handle_Record;
      Handle.Main_Window := Main_Window;
      Glib.Object.Initialize (Handle);
      Initialize_Class_Record
        (Handle, Signals, Kernel_Class, "GlideKernel", Signal_Parameters);

      Handle.Project := Create_Default_Project ("default", Get_Current_Dir);
      Handle.Project_Is_Default := True;
      Recompute_View (Handle);
      Reset_Source_Info_List (Handle);

      Gtk_New (Handle.Tooltips);

      Set_Predefined_Source_Path
        (Handle, ".:" &
         "/usr/local/gnat/lib/gcc-lib/i686-pc-linux-gnu/2.8.1/adainclude");
      Set_Predefined_Object_Path
        (Handle, ".:" &
         "/usr/local/gnat/lib/gcc-lib/i686-pc-linux-gnu/2.8.1/adalib");
      --  ??? This is a temporary hack for the demo. We should really compute
      --  these values from the output of gnatls -v...

      Load_Preferences
        (Handle, Get_Home_Directory & Directory_Separator & "preferences");

      --  ??? Shouldn't use naming schemes instead ? This duplicates the
      --  ??? information uselessly.
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
      Module : Module_List.List := Global_Modules_List;
   begin
      while not Module_List.Is_Empty (Module) loop
         if not Module_List.Head (Module).Was_Initialized then
            Trace (Me, "Initializing module "
                   & Module_List.Head (Module).Name);

            if Module_List.Head (Module).Initializer /= null then
               Module_List.Head (Module).Initializer (Handle);
            end if;

            Module_List.Head (Module).Was_Initialized := True;
         end if;

         Module := Module_List.Next (Module);
      end loop;
   end Initialize_All_Modules;

   --------------------------------
   -- Set_Predefined_Source_Path --
   --------------------------------

   procedure Set_Predefined_Source_Path
     (Handle : access Kernel_Handle_Record;
      Path   : String) is
   begin
      GNAT.OS_Lib.Free (Handle.Predefined_Source_Path);
      Handle.Predefined_Source_Path := new String'(Path);
   end Set_Predefined_Source_Path;

   --------------------------------
   -- Get_Predefined_Source_Path --
   --------------------------------

   function Get_Predefined_Source_Path
     (Handle : access Kernel_Handle_Record) return String is
   begin
      if Handle.Predefined_Source_Path = null then
         return "";
      end if;

      return Handle.Predefined_Source_Path.all;
   end Get_Predefined_Source_Path;

   --------------------------------
   -- Set_Predefined_Object_Path --
   --------------------------------

   procedure Set_Predefined_Object_Path
     (Handle : access Kernel_Handle_Record;
      Path   : String) is
   begin
      GNAT.OS_Lib.Free (Handle.Predefined_Object_Path);
      Handle.Predefined_Object_Path := new String'(Path);
   end Set_Predefined_Object_Path;

   --------------------------------
   -- Get_Predefined_Object_Path --
   --------------------------------

   function Get_Predefined_Object_Path
     (Handle : access Kernel_Handle_Record) return String is
   begin
      if Handle.Predefined_Object_Path = null then
         return "";
      end if;

      return Handle.Predefined_Object_Path.all;
   end Get_Predefined_Object_Path;

   --------------------
   -- Parse_ALI_File --
   --------------------

   procedure Parse_ALI_File
     (Handle       : access Kernel_Handle_Record;
      ALI_Filename : String;
      Unit         : out Src_Info.LI_File_Ptr;
      Success      : out Boolean) is
   begin
      Src_Info.ALI.Parse_ALI_File
        (ALI_Filename           => ALI_Filename,
         Project                => Get_Project_View (Handle),
         Predefined_Source_Path => Get_Predefined_Source_Path (Handle),
         Predefined_Object_Path => Get_Predefined_Object_Path (Handle),
         List                   => Handle.Source_Info_List,
         Unit                   => Unit,
         Success                => Success);
   end Parse_ALI_File;

   ------------------------
   -- Locate_From_Source --
   ------------------------

   function Locate_From_Source
     (Handle          : access Kernel_Handle_Record;
      Source_Filename : String) return Src_Info.LI_File_Ptr
   is
      File : Src_Info.LI_File_Ptr;
   begin
      Src_Info.ALI.Locate_From_Source
        (List                   => Handle.Source_Info_List,
         Source_Filename        => Source_Filename,
         Project                => Get_Project_View (Handle),
         Predefined_Source_Path => Get_Predefined_Source_Path (Handle),
         Predefined_Object_Path => Get_Predefined_Object_Path (Handle),
         File                   => File);
      return File;
   end Locate_From_Source;

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
         Set_File_Information
           (File_Selection_Context_Access (Handle.Explorer_Context),
            Project_Information (File),
            Directory_Information (File),
            File_Information (File));
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
         Name => Get_Home_Directory & Directory_Separator & "desktop");
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
        Get_Home_Directory & Directory_Separator & "desktop";

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

   -------------------
   -- Get_Unit_Name --
   -------------------

   procedure Get_Unit_Name
     (Handle    : access Kernel_Handle_Record;
      File      : in out Internal_File;
      Unit_Name : out String_Access) is
   begin
      Get_Unit_Name
        (File                   => File,
         Source_Info_List       => Handle.Source_Info_List,
         Project                => Get_Project_View (Handle),
         Predefined_Source_Path => Get_Predefined_Source_Path (Handle),
         Predefined_Object_Path => Get_Predefined_Object_Path (Handle),
         Unit_Name              => Unit_Name);
   end Get_Unit_Name;

   ---------------------------------
   -- Complete_Ali_File_If_Needed --
   ---------------------------------

   procedure Complete_ALI_File_If_Needed
     (Handle      : access Kernel_Handle_Record;
      LI_File     : in out Src_Info.LI_File_Ptr)
   is
      Unit          : Src_Info.LI_File_Ptr;
      Parse_Success : Boolean;

   begin
      if Is_Incomplete (LI_File) then
         declare
            LI_Name : constant String :=
              Find_Object_File
                (Handle, Get_LI_Filename (LI_File),
                 Use_Predefined_Object_Path => True);
         begin
            --  ??? Should we have another version of Parse_ALI_File that takes
            --  ??? directly a LI_File_Ptr that needs to be completed.

            Parse_ALI_File
              (Handle       => Handle,
               ALI_Filename => LI_Name,
               Unit         => Unit,
               Success      => Parse_Success);

            if Parse_Success then
               LI_File := Unit;
            else
               LI_File := No_LI_File;

               --  ??? Should be printed in the status bar
               Put_Line
                 ("Complete_Ali_File_If_Needed: couldn't parse ALI file "
                  & LI_Name);
            end if;
         end;
      end if;
   end Complete_ALI_File_If_Needed;

   ----------
   -- Free --
   ----------

   procedure Free (Module : in out Module_ID) is
   begin
      null;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Context : in out Selection_Context_Access) is
      procedure Internal is new Unchecked_Deallocation
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

end Glide_Kernel;
