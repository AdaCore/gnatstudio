-----------------------------------------------------------------------
--                                                                   --
--                     Copyright (C) 2001                            --
--                          ACT-Europe                               --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Glib;                      use Glib;
with Glib.Object;               use Glib.Object;
with Gtk.Handlers;              use Gtk.Handlers;
with Gtkada.MDI;                use Gtkada.MDI;

with Ada.Text_IO;               use Ada.Text_IO;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Gint_Xml;                  use Gint_Xml;
with Glide_Kernel.Preferences;  use Glide_Kernel.Preferences;
with Glide_Kernel.Project;      use Glide_Kernel.Project;
with Glide_Page;                use Glide_Page;
with GVD.Process;               use GVD.Process;
with Interfaces.C.Strings;      use Interfaces.C.Strings;
with Interfaces.C;              use Interfaces.C;
with OS_Utils;                  use OS_Utils;
with Src_Info;                  use Src_Info;
with Src_Info.ALI;

with Prj_API;                  use Prj_API;

with Language;                 use Language;
with Language.Ada;             use Language.Ada;
with Language.C;               use Language.C;
with Language.Cpp;             use Language.Cpp;

with Namet;                     use Namet;
with Prj.Tree;                  use Prj.Tree;
with Snames;                    use Snames;
with Stringt;                   use Stringt;
with Types;                     use Types;

package body Glide_Kernel is

   Project_Changed_Signal      : constant String := "project_changed";
   Project_View_Changed_Signal : constant String := "project_view_changed";

   Signals : constant chars_ptr_array :=
     (1 => New_String (Project_Changed_Signal),
      2 => New_String (Project_View_Changed_Signal));
   --  The list of signals defined for this object

   Kernel_Class : GObject_Class := Uninitialized_Class;
   --  The class structure for this object

   package Object_Callback is new Gtk.Handlers.Callback
     (Glib.Object.GObject_Record);

   procedure Create_Default_Project
     (Kernel : access Kernel_Handle_Record'Class);
   --  Create a default project file.
   --  ??? This should actually be read from an external file when we have a
   --  ??? full installation procedure for Glide

   function Get_Home_Directory return String;
   --  Return the home directory in which glide's user files should be stored
   --  (preferences, log, ...)

   procedure Reset_Source_Info_List
     (Handle : access Kernel_Handle_Record'Class);
   --  Re-initialize the Source Info structure.
   --  ??? Needs more comments.

   ----------------------------
   -- Create_Default_Project --
   ----------------------------

   procedure Create_Default_Project
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Values : Argument_List (1 .. 1);
   begin
      Kernel.Project_Is_Default := True;
      Kernel.Project := Create_Project ("default", Get_Current_Dir);

      Values := (1 => new String' ("."));
      Update_Attribute_Value_In_Scenario
        (Kernel.project,
         Scenario_Variables => Scenario_Variables (Kernel),
         Attribute_Name     => "source_dirs",
         Values             => Values);
      Free (Values (1));

      Update_Attribute_Value_In_Scenario
        (Kernel.project,
         Scenario_Variables => Scenario_Variables (Kernel),
         Attribute_Name     => "object_dir",
         Value              => ".");
      Recompute_View (Kernel);
   end Create_Default_Project;

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
        (1 => (1 => GType_None), 2 => (1 => GType_None));

   begin
      Handle := new Kernel_Handle_Record;
      Handle.Main_Window := Main_Window;
      Glib.Object.Initialize (Handle);
      Initialize_Class_Record
        (Handle, Signals, Kernel_Class, "GlideKernel", Signal_Parameters);

      Create_Default_Project (Handle);
      Reset_Source_Info_List (Handle);

      Set_Source_Path
        (Handle, ".:" &
         "/usr/local/gnat/lib/gcc-lib/i686-pc-linux-gnu/2.8.1/adainclude");
      Set_Object_Path
        (Handle, ".:" &
         "/usr/local/gnat/lib/gcc-lib/i686-pc-linux-gnu/2.8.1/adalib");
      --  ??? This is a temporary hack for the demo. We should really compute
      --  ??? these values from the output of gnatls -v...

      Load_Preferences
        (Handle, Get_Home_Directory & Directory_Separator & "preferences");

      Reset_File_Extensions;
      Add_File_Extensions (Ada_Lang, Get_Pref (Handle, Ada_Extensions));
      Add_File_Extensions (C_Lang,   Get_Pref (Handle, C_Extensions));
      Add_File_Extensions (Cpp_Lang, Get_Pref (Handle, Cpp_Extensions));
   end Gtk_New;

   ---------------------
   -- Set_Source_Path --
   ---------------------

   procedure Set_Source_Path
     (Handle : access Kernel_Handle_Record;
      Path   : String) is
   begin
      GNAT.OS_Lib.Free (Handle.Source_Path);
      Handle.Source_Path := new String'(Path);
   end Set_Source_Path;

   ---------------------
   -- Get_Source_Path --
   ---------------------

   function Get_Source_Path
     (Handle : access Kernel_Handle_Record) return String is
   begin
      if Handle.Source_Path = null then
         return "";
      end if;
      return Handle.Source_Path.all;
   end Get_Source_Path;

   ---------------------
   -- Set_Object_Path --
   ---------------------

   procedure Set_Object_Path
     (Handle : access Kernel_Handle_Record;
      Path   : String) is
   begin
      GNAT.OS_Lib.Free (Handle.Object_Path);
      Handle.Object_Path := new String'(Path);
   end Set_Object_Path;

   ---------------------
   -- Get_Object_Path --
   ---------------------

   function Get_Object_Path
     (Handle : access Kernel_Handle_Record) return String is
   begin
      if Handle.Object_Path = null then
         return "";
      end if;
      return Handle.Object_Path.all;
   end Get_Object_Path;

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
        (ALI_Filename => ALI_Filename,
         Project      => Get_Project_View (Handle),
         Source_Path  => Get_Source_Path (Handle),
         List         => Handle.Source_Info_List,
         Unit         => Unit,
         Success      => Success);
   end Parse_ALI_File;

   ------------------------
   -- Locate_From_Source --
   ------------------------

   function Locate_From_Source
     (Handle            : access Kernel_Handle_Record;
      Source_Filename   : String)
      return Src_Info.LI_File_Ptr
   is
      File : Src_Info.LI_File_Ptr;
   begin
      Src_Info.ALI.Locate_From_Source
        (List              => Handle.Source_Info_List,
         Source_Filename   => Source_Filename,
         Project           => Get_Project_View (Handle),
         Extra_Source_Path => Get_Source_Path (Handle),
         Extra_Object_Path => Get_Object_Path (Handle),
         File              => File);
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
        (File              => File,
         Source_Info_List  => Handle.Source_Info_List,
         Project           => Get_Project_View (Handle),
         Extra_Source_Path => Get_Source_Path (Handle),
         Extra_Object_Path => Get_Object_Path (Handle),
         Unit_Name         => Unit_Name);
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
              Find_Object_File (Handle, Get_LI_Filename (LI_File));

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

end Glide_Kernel;
