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

with Prj;
with Prj.Tree;
with Glib.Object;
with GNAT.OS_Lib;
with Gtk.Handlers;
with Gtk.Window;
with Src_Info;
with Gint_Xml;
with Gtkada.MDI;

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

   procedure Set_Source_Path
     (Handle : access Kernel_Handle_Record;
      Path   : String);
   --  Set the Source_Path for the given Kernel Handle.

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

   function Get_Object_Path
     (Handle : access Kernel_Handle_Record) return String;
   --  Return the Object_Path associated to the given Kernel Handle.
   --  Return the empty string if no object path has been set yet.
   --  ??? Needs more comments.

   procedure Reset_Source_Info_List
     (Handle : access Kernel_Handle_Record);
   --  Re-initialize the Source Info structure.
   --  ??? Needs more comments.

   function Get_Source_Info_List
     (Handle : access Kernel_Handle_Record) return Src_Info.LI_File_List;
   --  Return the Source Information List for the given Kernel Handle

   procedure Parse_ALI_File
     (Handle       : access Kernel_Handle_Record;
      ALI_Filename : String;
      Unit         : out Src_Info.LI_File_Ptr;
      Success      : out Boolean);
   --  Parse the given ALI file and return the new LI_File_Ptr created if
   --  the parsing was successful.

   procedure Save_Desktop
     (Handle : access Kernel_Handle_Record);
   --  Save the current desktop.

   function Load_Desktop (Handle : access Kernel_Handle_Record) return Boolean;
   --  Reload a saved desktop.
   --  Return False if no desktop could be loaded.

   ---------------------
   -- Signal emission --
   ---------------------

   package Object_User_Callback is new Gtk.Handlers.User_Callback
     (Glib.Object.GObject_Record, Glib.Object.GObject);
   --  Generic callback that can be used to connect a signal to a kernel.

   procedure Project_Changed (Handle : access Kernel_Handle_Record);
   --  Emits the "project_changed" signal

   procedure Project_View_Changed (Handle : access Kernel_Handle_Record);
   --  Emits the "project_view_changed" signal

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
   --  </signals>

private

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
   end record;

end Glide_Kernel;
