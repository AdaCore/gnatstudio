------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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

--  General description of modules
--  ==============================
--
--  This package contains all the subprograms needed to register new modules in
--  GPS.
--
--  All the functionalities provided in GPS are organized into modules. Each
--  module can extensively modify the standard behavior of GPS (see below)
--
--  The modules should only interact with each other through the kernel,
--  never directly. This provides more flexibility, as well as room for future
--  extensions like dynamic modules.
--
--  The default modules provided in GPS (source editor, project editor,...)
--  are more closely integrated into the kernel than other external
--  modules. However, even these should ideally be fully replaceable with minor
--  source modification (for instance if one wants to override the default
--  source editor).
--
--  Each module is associated with a unique name. The names for some of the
--  default GPS modules are provided as constants in this package, so that it
--  is easy to check whether an action was initiated by one module or another.
--
--  Registering modules
--  ===================
--
--  All the modules must be registered with the kernel before they can do
--  anything. Register_Module should be called from gps.adb, and this
--  subprogram can then register new behaviors in GPS (see below "Registering
--  New Features")
--
--  This mechanism allows the kernel to be completely independent of the
--  specific modules, since it doesn't need to know in advance the exact list
--  of modules.
--
--  It is possible to dynamically register a module that hasn't been linked
--  with the GPS executable using the procedure Dynamic_Register_Module.
--  In order to register modules dynamically, the following conditions need
--  to be met:
--
--  - compile the kernel as a shared library, using project files.
--  - create the dynamic module, as a SAL, including elaboration code that
--    will be called by Dynamic_Register_Module
--
--   To load a module during GPS execution, use the command "insmod":
--
--   GPS> insmod vcs_cvs vcs__cvs
--
--  See also GPS.Kernel.Modules.UI for contextual menus, toolbar items, etc...
--
--  All these changes can be done locally in the module, and do not need any
--  modification to the rest of GPS itself (apart from registering the module
--  itself. This means that a user might choose not to load some of the
--  modules to simplify the GUI or to use less memory.

with GNAT.Strings;
with Glib.Object;
with Gtk.Widget;
with XML_Utils;
with GPS.Customizable_Modules;  use GPS.Customizable_Modules;

package GPS.Kernel.Modules is

   Explorer_Module_Name        : constant String := "Project_Explorer_Project";
   Project_Editor_Module_Name     : constant String := "Project_Editor";
   Dependency_Browser_Module_Name : constant String := "Dependency_Browser";
   Project_Browser_Module_Name    : constant String := "Project_Browser";
   Revision_View_Module_Name      : constant String := "Revision_Views";
   Entity_Browser_Module_Name : constant String := "Entity_Browser";
   --  Names for the internal modules.
   --  Changing these might also impact the contents of the saved perspectives
   --  files.#

   ------------------
   -- Module types --
   ------------------

   type Module_ID_Record is new Customizable_Module_Record with private;
   type Module_ID is access all Module_ID_Record'Class;

   function Get_Name (Module : Module_ID) return String;
   --  Return the name of the module

   function Get_Kernel (ID : Module_ID_Record'Class) return Kernel_Handle;
   --  Return the kernel associated with Module

   type Save_Function_Mode is (Query, Action);
   --  The two types of use for Module_Save_Function.
   --  If Query, then the save_function should return whether the corresponding
   --  child has been modified, and not saved yet, and thus whether we should
   --  ask the user whether to save it.
   --  If Action, the save_function *must* save the child. The return value
   --  then indicates whether the save was successful (True), or failed (False)

   function Save_Function
     (Module       : access Module_ID_Record;
      Child        : Glib.Object.GObject;
      Mode         : Save_Function_Mode;
      Single_Child : Boolean;
      Force        : Boolean) return Boolean;
   --  A function called when the kernel asks a MDI child to save itself.
   --  See the description of Mode for the description of the modes.
   --  Child is the widget that put directly in the MDI.
   --  Single_Child is True if only one child is being saved, False otherwise.
   --  This is needed to know whether a source editor box should be saved.
   --  Should return True if the Child needs to be saved (in Query mode), or
   --  if the child could be saved with success (in Action mode)

   function Tooltip_Handler
     (Module  : access Module_ID_Record;
      Context : Selection_Context) return Gtk.Widget.Gtk_Widget;
   --  Callback used every time some tooltip event happens in GPS.
   --  Context contains all the information about the context of the tooltip.
   --
   --  The first callback that will decide to handle the tooltip will set
   --  pixmap, which will stop the
   --  propagation of the tooltip message (since only one module can display
   --  a tooltip at a time).
   --
   --  Since only one module will handle the tooltip, putting proper priorities
   --  when registering the modules is very important.
   --
   --  See the function GUI_Utils.Create_Pixmap_From_Text for an easy way to
   --  create a tooltip that only contains text

   function Bookmark_Handler
     (Module     : access Module_ID_Record;
      Dummy_Load : XML_Utils.Node_Ptr := null) return Location_Marker
     is (No_Marker);
   --  Create bookmark for either the bookmark described in Load, or
   --  the current context in the module. Load is used when reloading the
   --  bookmarks when GPS is started, and is the same XML node created by
   --  Location_Marker.Save.
   --
   --  null should be returned if we can't create a marker at that position

   overriding procedure Customize
     (Module : access Module_ID_Record;
      File   : GNATCOLL.VFS.Virtual_File;
      Node   : XML_Utils.Node_Ptr;
      Level  : Customization_Level) is null;
   --  Subprogram called when a new customization has been parsed.
   --  It is initially called just after all modules have been registered,
   --  and gets passed a single XML node.
   --  File is the XML file that is currently being parsed.

   -------------------------
   -- Module manipulation --
   -------------------------

   type Module_Priority is new Natural;
   Low_Priority     : constant Module_Priority := Module_Priority'First;
   Default_Priority : constant Module_Priority := 500;
   High_Priority    : constant Module_Priority := Module_Priority'Last;
   --  The priority of the module.
   --  Modules with a higher priority are always called before modules with
   --  lower priority, for instance when computing the contents of a contextual
   --  menu.

   procedure Register_Module
     (Module      : access Module_ID_Record;
      Kernel      : access Kernel_Handle_Record'Class;
      Module_Name : String;
      Priority    : Module_Priority := Default_Priority);
   --  Register a new module into GPS.
   --  If Module is null, a new module_id is created. Otherwise, the internal
   --  information stored in Module is changed. This allows you to store user
   --  data specific to each module, instead of using global variables.
   --
   --  Module_Name can be used by other modules to check whether they want to
   --  interact with this module.

   procedure Dynamic_Register_Module
     (Kernel      : access Kernel_Handle_Record'Class;
      Shared_Lib  : String;
      Module_Name : String;
      Success     : out Boolean);
   --  Register a module dynamically.
   --  Shared_Lib is the name of the shared library containing the module.
   --  It can either be a full name, or a short name, e.g. "vfs" for
   --  "libvfs.so".
   --  Module_Name is the name of the module, e.g. "vfs_module".
   --  This procedure assumes that Shared_Lib provides two routines called
   --  Module_Name & "_init" and Module_Name & "__register_module" with the
   --  following profiles:
   --
   --  type Module_Init is access procedure;
   --
   --  type Register_Module is access procedure
   --    (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --
   --  Success is set to True if the module could be successfully registered.

   function Module_Name (ID : access Module_ID_Record'Class) return String;
   --  Return the name of the module registered as ID

   procedure Free_Modules (Kernel : access Kernel_Handle_Record'Class);
   --  Free all the registered modules, and call Destroy for each of these

   function Get_Priority
     (ID : access Module_ID_Record'Class) return Module_Priority;
   --  Return the current priority of ID

private

   type Module_ID_Record is new Customizable_Module_Record with record
      Kernel   : Kernel_Handle;
      Priority : Module_Priority;
      Name     : GNAT.Strings.String_Access;
   end record;

end GPS.Kernel.Modules;
