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

--  This package is the root of the glide's kernel API.

with Ada.Tags;
with GNAT.OS_Lib;
with Generic_List;
with Gint_Xml;
with Glib.Object;
with Gtk.Handlers;
with Gtk.Menu;
with Gtk.Window;
with Gtkada.MDI;
with Prj.Tree;
with Prj;
with Prj_API;
with Src_Info;
with System;
with Unchecked_Conversion;

package Glide_Kernel is

   type Kernel_Handle_Record is new Glib.Object.GObject_Record with private;
   type Kernel_Handle is access all Kernel_Handle_Record'Class;
   --  A kernel handle used to share information throughout Glide.

   package Kernel_Desktop is new Gtkada.MDI.Desktop (Kernel_Handle);

   procedure Gtk_New
     (Handle      : out Kernel_Handle;
      Main_Window : Gtk.Window.Gtk_Window);
   --  Create a new Glide kernel.
   --  By default, it isn't associated with any project, nor any source editor.

   procedure Initialize_All_Modules (Handle : access Kernel_Handle_Record);
   --  Initialize all the modules that are registered. This should be called
   --  only after the main window and the MDI have been initialized, so that
   --  the modules can add entries in the menus and the MDI.

   procedure Set_Source_Path
     (Handle : access Kernel_Handle_Record;
      Path   : String);
   --  Set the Source_Path for the given Kernel Handle.
   --  ??? Should this be renamed to something like Set_Extra_Source_Path

   function Get_Source_Path
     (Handle : access Kernel_Handle_Record) return String;
   --  Return the Source_Path associated to the given Kernel Handle.
   --  Return the empty string if no source path has been set yet.
   --  ??? Needs more comments.

   procedure Set_Object_Path
     (Handle : access Kernel_Handle_Record;
      Path   : String);
   --  Set the Object_Path for the given Kernel Handle.
   --  ??? Needs more comments.
   --  ??? Should this be renamed to something like Set_Extra_Object_Path

   function Get_Object_Path
     (Handle : access Kernel_Handle_Record) return String;
   --  Return the Object_Path associated to the given Kernel Handle.
   --  Return the empty string if no object path has been set yet.
   --  ??? Needs more comments.

   procedure Parse_ALI_File
     (Handle       : access Kernel_Handle_Record;
      ALI_Filename : String;
      Unit         : out Src_Info.LI_File_Ptr;
      Success      : out Boolean);
   --  Parse the given ALI file and return the new LI_File_Ptr created if
   --  the parsing was successful.

   procedure Complete_ALI_File_If_Needed
     (Handle      : access Kernel_Handle_Record;
      LI_File     : in out Src_Info.LI_File_Ptr);
   --  Parse the ALI file, but only if needed and it hasn't been done yet.

   function Locate_From_Source
     (Handle            : access Kernel_Handle_Record;
      Source_Filename   : String)
      return Src_Info.LI_File_Ptr;
   --  Find the ALI file for Source_Filename, and return a handle to it.

   procedure Get_Unit_Name
     (Handle    : access Kernel_Handle_Record;
      File      : in out Src_Info.Internal_File;
      Unit_Name : out GNAT.OS_Lib.String_Access);
   --  Return the unit name for the given file

   procedure Save_Desktop
     (Handle : access Kernel_Handle_Record);
   --  Save the current desktop.

   function Load_Desktop (Handle : access Kernel_Handle_Record) return Boolean;
   --  Reload a saved desktop.
   --  Return False if no desktop could be loaded.

   function Get_MDI (Handle : access Kernel_Handle_Record)
      return Gtkada.MDI.MDI_Window;
   --  Return the MDI associated with Handle

   function Find_MDI_Child_By_Tag
     (Handle : access Kernel_Handle_Record; Tag : Ada.Tags.Tag)
      return Gtkada.MDI.MDI_Child;
   --  Return the first child matching Tag

   --------------
   -- Contexts --
   --------------

   type Module_ID_Information (<>) is private;
   type Module_ID is access Module_ID_Information;

   type Selection_Context is abstract tagged private;
   type Selection_Context_Access is access all Selection_Context'Class;
   --  This type contains all the information about the selection in any
   --  module. Note that this is a tagged type, so that it can easily be
   --  extended for modules external to Glide

   function To_Selection_Context_Access is new Unchecked_Conversion
     (System.Address, Selection_Context_Access);

   procedure Set_Context_Information
     (Context : access Selection_Context;
      Kernel  : access Kernel_Handle_Record'Class;
      Creator : Module_ID);
   --  Set the information in the context

   function Get_Kernel (Context : access Selection_Context)
      return Kernel_Handle;
   --  Return the kernel associated with the context

   function Get_Creator (Context : access Selection_Context) return Module_ID;
   --  Return the module ID for the module that created the context

   procedure Destroy (Context : in out Selection_Context) is abstract;
   --  Destroy the information contained in the context, and free the memory

   procedure Free (Context : in out Selection_Context_Access);
   --  Free the memory occupied by Context. It automatically calls the
   --  primitive subprogram Destroy as well

   -------------
   -- Modules --
   -------------
   --  See documentation in Glide_Kernel.Modules

   Kernel_Module : constant Module_ID;
   --  The kernel itself.
   --  ??? Should be a Main_Window module instead

   type Module_Priority is new Natural;
   Low_Priority     : constant Module_Priority := Module_Priority'First;
   Default_Priority : constant Module_Priority := 500;
   High_Priority    : constant Module_Priority := Module_Priority'Last;
   --  The priority of the module.
   --  Modules with a higher priority are always called before modules with
   --  lower priority, for instance when computing the contents of a contextual
   --  menu.

   type Module_Menu_Handler is access procedure
     (Context   : access Selection_Context'Class;
      Menu      : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  Callback used every time some contextual menu event happens in Glide.
   --  The module that initiated the event (ie the one that is currently
   --  displaying the contextual menu) can be found by reading Get_Creator for
   --  the context.
   --  Context contains all the information about the current selection.
   --
   --  The callback should add the relevant items to Menu. It is recommended to
   --  use Glide_Kernel.Modules.Context_Callback below to connect signals to
   --  the items.

   type Module_Initializer is access procedure
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  General type for the module initialization subprograms.

   ---------------------
   -- Signal emission --
   ---------------------

   package Object_User_Callback is new Gtk.Handlers.User_Callback
     (Glib.Object.GObject_Record, Glib.Object.GObject);
   --  Generic callback that can be used to connect a signal to a kernel.

   package Kernel_Callback is new Gtk.Handlers.User_Callback
     (Glib.Object.GObject_Record, Kernel_Handle);
   --  Generic callback that can be used to connect a signal to a kernel.

   procedure Project_Changed (Handle : access Kernel_Handle_Record);
   --  Emits the "project_changed" signal

   procedure Project_View_Changed (Handle : access Kernel_Handle_Record);
   --  Emits the "project_view_changed" signal

   procedure Context_Changed
     (Handle  : access Kernel_Handle_Record;
      Context : access Selection_Context'Class);
   --  Emits the "context_changed" signal

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "project_changed"
   --    procedure Handler (Handle : access Kernel_Handle_Record'Class);
   --
   --    Emitted when the project has changed. This means that a new project
   --    has been loaded in Glide, and that all the previous settings and
   --    caches are now obsolete.
   --    Note: when this signal is emitted, the project view hasn't necessarily
   --    been created yet.
   --
   --  - "project_view_changed"
   --    procedure Handler (Handle : access Kernel_Handle_Record'Class);
   --
   --    Emitted when the project view has been changed (for instance because
   --    one of the environment variables has changed). This means that the
   --    list of directories, files or switches might now be different).
   --
   --  - "context_changed"
   --    procedure Handler (Handle  : access Kernel_Handle_Record'Class;
   --                       Context : Selection_Context_Access);
   --
   --    Emitted when a context has changed, like a new file/directory/project
   --    selection.
   --
   --  </signals>

   Project_Changed_Signal      : constant String := "project_changed";
   Project_View_Changed_Signal : constant String := "project_view_changed";
   Context_Changed_Signal      : constant String := "context_changed";

private

   type Module_ID_Information (Name_Length : Natural) is record
      Name            : String (1 .. Name_Length);
      Priority        : Module_Priority;
      Initializer     : Module_Initializer;
      Contextual_Menu : Module_Menu_Handler;
   end record;

   type Selection_Context is abstract tagged record
      Kernel  : Kernel_Handle;
      Creator : Module_ID;
   end record;

   Kernel_Module : constant Module_ID := new Module_ID_Information'
     (Name_Length     => 6,
      Name            => "Kernel",
      Priority        => High_Priority,
      Initializer     => null,
      Contextual_Menu => null);

   package Module_List is new Generic_List (Module_ID);
   Global_Modules_List : Module_List.List;
   --  The list of all the modules that have been loaded in Glide. This has to
   --  be a global variable for elaboration reasons: when the modules are
   --  registered, the kernel hasn't necessarily been created yet.

   function Get_Source_Info_List
     (Handle : access Kernel_Handle_Record) return Src_Info.LI_File_List;
   --  Return the Source Information List for the given Kernel Handle

   type Kernel_Handle_Record is new Glib.Object.GObject_Record with record
      Project : Prj.Tree.Project_Node_Id := Prj.Tree.Empty_Node;
      --  The project currently loaded. This is the tree form, independent of
      --  the current value of the environment variables.

      Project_Is_Default : Boolean;
      --  True when the current project is still the default project. This is
      --  set to False as soon as the user loads a new project

      Project_View : Prj.Project_Id := Prj.No_Project;
      --  The current project view. This is the same Project, after it has been
      --  evaluated based on the current value of the environment variables.

      Main_Window : Gtk.Window.Gtk_Window;
      --  The main glide window

      Source_Path : GNAT.OS_Lib.String_Access;
      --  The path of the sources used to compile the project which are not
      --  directly part of the project (eg the Ada run-time files).

      Object_Path : GNAT.OS_Lib.String_Access;
      --  The path for the object files associated the sources in the
      --  Source Path above.

      Source_Info_List : Src_Info.LI_File_List;
      --  The semantic information associated to the files for the current
      --  project.

      Preferences : Gint_Xml.Node_Ptr;
      --  The XML tree that contains the current preferences

      Current_Editor : Gtkada.MDI.MDI_Child;
      --  The last editor that had the focus

      Scenario_Variables : Prj_API.Project_Node_Array_Access := null;
      --  The (cached) list of scenario variables for the current project. Note
      --  that this list depends on which project was loaded in Glide, and
      --  might change when new dependencies are added.

      Last_Context_For_Contextual : Selection_Context_Access := null;
      --  The context used in the last selection.
   end record;

end Glide_Kernel;
