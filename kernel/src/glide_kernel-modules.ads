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

--  General description of modules
--  ==============================
--
--  This package contains all the subprograms needed to register new modules in
--  Glide.
--
--  All the functionalities provided in Glide are organized into modules. Each
--  module can do the following:
--     - Add new menus to the standard Glide menu bar
--     - Add new icons in the Glide toolbar
--     - Add new entries in the contextual menus.
--     - Associate any of the previous with specific callbacks, and insert new
--       widgets in the Glide MDI
--
--  The modules should only interact with each other through the Glide kernel,
--  never directly. This provides more flexibility, as well as room for future
--  extensions like dynamic modules.
--
--  The default modules provided in Glide (source editor, project editor,...)
--  are more closely integrated into the kernel than other external
--  modules. However, even these should ideally be fully replaceable with minor
--  source modification (for instance if one wants to override the default
--  source editor).
--
--  Each module is associated with a unique name. The names for the default
--  Glide modules are provided as constants in this package, so that it is easy
--  to check whether an action was initiated by one module or another.
--
--  Registering modules
--  ===================
--
--  All the modules must be registered with the kernel before they can do
--  anything. This is typically in the elaboration part of a package, by
--  calling Register_Module.
--
--  Once the kernel has been created, it will call any initialization
--  function you have provided. This function might for instance be used to
--  register new menu for the menu bar, or new icons in the toolbar.
--
--  This mechanism allows the kernel to be completly independeny of the
--  specific modules, since it doesn't need to do in advance the exact list of
--  modules.
--
--  Contextual menus
--  ================
--
--   Here is a description of the sequence of events used to display contextual
--   menus in Glide:
--      - Each object that should have a contextual menu should call
--        Register_Contextual_Menu. The kernel will automatically setup
--        appropriate gtk callbacks.
--      - Whenever the user presses the right mouse button, the kernel will ask
--        the object to report the context in which the event occured (name of
--        selected file, selected project,...).
--      - Each of the registered module then has the opportunity to add entries
--        in the contextual menu, based on this context.
--      - The menu is displayed, and the callback for the selected menu item
--        will be called as usual.
--      - The menu is automatically destroyed, and the context freed, when the
--        action has finished executing.

with Gdk.Event;
with Glib.Object;
with Gdk.Types;
with Gtk.Handlers;
with Gtk.Menu_Item;
with Gtk.Widget;
with Prj;

package Glide_Kernel.Modules is

   Explorer_Module_Name           : constant String := "Explorer";
   Project_Editor_Module_Name     : constant String := "Project_Editor";
   Aunit_Module_Name              : constant String := "Unit_Testing";
   VCS_Module_Name                : constant String := "VCS_Interface";
   VCS_CVS_Module_Name            : constant String := "CVS_Connectivity";
   Dependency_Browser_Module_Name : constant String := "Dependency_Browser";
   Project_Browser_Module_Name    : constant String := "Project_Browser";
   --  Names for the internal modules

   -----------
   -- Types --
   -----------
   --  See also the types defined in glide_kernel.ads

   package Context_Callback is new Gtk.Handlers.User_Callback
     (Glib.Object.GObject_Record, Selection_Context_Access);

   -------------------------
   -- Module manipulation --
   -------------------------

   function Register_Module
     (Module_Name             : String;
      Priority                : Module_Priority := Default_Priority;
      Initializer             : Module_Initializer  := null;
      Contextual_Menu_Handler : Module_Menu_Handler := null;
      Mime_Handler            : Module_Mime_Handler := null) return Module_ID;
   --  Register a new module into Glide.
   --  Module_Name can be used by other modules to check whether they want to
   --  interact with this module.
   --  See the general description for this package for explanation on
   --  Initializer and Contextual_Menu_Handler.

   function Module_Name (ID : Module_ID) return String;
   --  Return the name of the module registered as ID.

   ----------------------
   -- Contextual menus --
   ----------------------

   type Context_Factory is access function
     (Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu) return Selection_Context_Access;
   --  This function should return the context associated with the contextual
   --  menu, when the mouse event Event happened on Widget.
   --  The mouse event occured in Event_Widget, and the contextual menu was
   --  registered for Object
   --  The object should also add its default entries into the menu, so that
   --  they always appear first in the menu. Note that the module will not be
   --  asked in the second step whether new entries should be added.
   --
   --  If null is returned, no contextual menu will be displayed.
   --
   --  The kernel is automatically set in the context.

   procedure Register_Contextual_Menu
     (Kernel          : access Kernel_Handle_Record'Class;
      Event_On_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object          : access Glib.Object.GObject_Record'Class;
      ID              : Module_ID;
      Context_Func    : Context_Factory);
   --  Register that Widget should be associated with a contextual menu.
   --  Whenever a right-button click happens inside Widget_On_Widget, then the
   --  following will happen:
   --     - the kernel detects the event, and creates an empty menu.
   --     - it asks Widget, through Context_Func, the exact context for the
   --       menu (selected file, ....)
   --     - it then asks each of the registered modules whether it wants to
   --       add new items to the menu, and let is do so (through the
   --       Contextual_Menu_Handler provided in Register_Module)
   --     - it then displays the menu
   --     - it finally cleans up the memory when the menu is hidden

   -----------
   -- Menus --
   -----------

   procedure Register_Menu
     (Kernel      : access Kernel_Handle_Record'Class;
      Parent_Path : String;
      Item        : Gtk.Menu_Item.Gtk_Menu_Item := null;
      Ref_Item    : String := "";
      Add_Before  : Boolean := True);
   --  Add new menu items to the menu bar, as a child of Parent_Path.
   --  Parent_Path should have a form like "/main_main/submenu".
   --  Menus will be created if they don't exist.
   --  This must be an absolute path, starting with '/'.
   --
   --  Item might be null, in which case only the parent menu items are
   --  created, and Add_Before applies to the deepest one instead of Item.
   --
   --  The new item is inserted either:
   --    - before Ref_Item if the latter is not the empty string and Add_Before
   --      is true
   --    - after Ref_Item if the latter is not the empty string and Add_Before
   --      is false
   --    - at the end of the menu

   procedure Register_Menu
     (Kernel      : access Kernel_Handle_Record'Class;
      Parent_Path : String;
      Text        : String;
      Stock_Image : String := "";
      Callback    : Kernel_Callback.Marshallers.Void_Marshaller.Handler;
      Accel_Key   : Gdk.Types.Gdk_Key_Type := 0;
      Accel_Mods  : Gdk.Types.Gdk_Modifier_Type := 0;
      Ref_Item    : String := "";
      Add_Before  : Boolean := True);
   --  Same as the above, but creates the menu item directly, and connects the
   --  appropriate callback.

   --------------------
   -- Mime callbacks --
   --------------------
   --  The following subprograms are provided so that the kernel can request
   --  each of its module whether it can display some specific data.
   --  The type of data is indicated through standard MIME types.

   Mime_Source_File : constant String := "glide/source";
   --  There are multiple data associated with this type:
   --     first  : full name of the source file to open (use Get_String)
   --     second : line to display initially (use Get_Int). Ignored if 0
   --     third  : column to display initially (use Get_Int). Ignored if 0
   --     fourth : True if the line should be highlighted (use Get_Boolean)
   --  See also the function Open_File_Editor.

   Mime_Html_File : constant String := "glide/html";
   --  Request to display a html file
   --  There are multiple data associated with this type:
   --     first  : full name of the html file to open (use Get_String)

   Mime_Diff_File : constant String := "application/diff";
   --  There are multiple data associated with this type:
   --     first  : full name of the original file (use Get_String). Built
   --              from second and third arguments if null.
   --     second : full name of the new file (use Get_String). Built from
   --              first and third arguments if null. First and second cannot
   --              be both null.
   --     third  : full name of the diff file (use Get_String).
   --  See also the function Display_Differences.

   function Mime_Action
     (Kernel    : access Kernel_Handle_Record'Class;
      Mime_Type : String;
      Data      : GValue_Array;
      Mode      : Mime_Mode := Read_Write) return Boolean;
   --  This function calls each of the registered module, and check whether it
   --  can handle the data.
   --  If any of the module was able to, True is returned.

   procedure Open_File_Editor
     (Kernel         : access Kernel_Handle_Record'Class;
      Filename       : String;
      Line           : Natural := 0;
      Column         : Natural := 0;
      Highlight_Line : Boolean := True);
   --  Open, or create, an editor that edits Filename (Mime_Source_File type)

   procedure Open_Html
     (Kernel         : access Kernel_Handle_Record'Class;
      Filename       : String);
   --  Open, or create, an html viewer for Filename (Mime_Html_File type)

   procedure Display_Differences
     (Kernel         : access Kernel_Handle_Record'Class;
      Orig_File      : String := "";
      New_File       : String := "";
      Diff_File      : String);
   --  Display differences between Orig_File and New_File (Mime_Diff_File type)
   --  Either Orig_File or New_File can be null (but not both), in which
   --  case, the contents of the file is computed from the other file and the
   --  diff file.

   ------------------------
   -- File_Name contexts --
   ------------------------

   type File_Name_Selection_Context is new Selection_Context with private;
   type File_Name_Selection_Context_Access is access all
     File_Name_Selection_Context'Class;

   procedure Set_File_Name_Information
     (Context : access File_Name_Selection_Context;
      Directory : String := "";
      File_Name : String := "");
   --  Set the information in this context.

   function Has_Directory_Information
     (Context : access File_Name_Selection_Context) return Boolean;
   --  True if the context has information about a selected directory.

   function Directory_Information
     (Context : access File_Name_Selection_Context) return String;
   --  Return the information about the selected project. This is only relevant
   --  if Has_Directory_Information is True.
   --  This directory name always ends with a '/'

   function Has_File_Information
     (Context : access File_Name_Selection_Context) return Boolean;
   --  True if the context has information about a selected file.

   function File_Information
     (Context : access File_Name_Selection_Context) return String;
   --  Return the information about the selected project. This is only relevant
   --  if Has_File_Information is True.
   --  This is the base file name for the file

   procedure Destroy (Context : in out File_Name_Selection_Context);
   --  Free the memory associated with the context

   -------------------
   -- File Contexts --
   -------------------

   type File_Selection_Context is new File_Name_Selection_Context with private;
   type File_Selection_Context_Access is access all File_Selection_Context;

   procedure Set_File_Information
     (Context           : access File_Selection_Context;
      Project_View      : Prj.Project_Id := Prj.No_Project;
      Importing_Project : Prj.Project_Id := Prj.No_Project);
   --  Set the information in Context.
   --  File_Name should always be the base file name (no directory
   --  information), and Directory should always end with a path separator.

   function Has_Project_Information
     (Context : access File_Selection_Context) return Boolean;
   --  True if the context has information about a selected project.

   function Project_Information
     (Context : access File_Selection_Context) return Prj.Project_Id;
   --  Return the id of the selected project. This can be No_Project is there
   --  wasn't any information about a specific project

   function Has_Importing_Project_Information
     (Context : access File_Selection_Context) return Boolean;
   --  True if the context contains the name of the project importing the
   --  current one.

   function Importing_Project_Information
     (Context : access File_Selection_Context) return Prj.Project_Id;
   --  Return the project that imports the one returned by Project_Information.

   ---------------------
   -- Entity Contexts --
   ---------------------

   type Entity_Selection_Context is new File_Name_Selection_Context
     with private;
   type Entity_Selection_Context_Access is access all Entity_Selection_Context;

   procedure Set_Entity_Information
     (Context     : access Entity_Selection_Context;
      Entity_Name : String := "";
      Line        : Integer := 0;
      Column      : Integer := 0);
   --  Set the information in the context

   function Has_Entity_Name_Information
     (Context : access Entity_Selection_Context) return Boolean;
   function Entity_Name_Information
     (Context : access Entity_Selection_Context) return String;
   --  Check whether there is some entity information, and return it.

   function Has_Line_Information
     (Context : access Entity_Selection_Context) return Boolean;
   function Line_Information
     (Context : access Entity_Selection_Context) return Integer;
   --  Check whether there is some line information, and return it.

   function Has_Column_Information
     (Context : access Entity_Selection_Context) return Boolean;
   function Column_Information
     (Context : access Entity_Selection_Context) return Integer;
   --  Check whether there is some column information, and return it.


private

   type File_Name_Selection_Context is new Selection_Context with record
      Directory : GNAT.OS_Lib.String_Access := null;
      File_Name : GNAT.OS_Lib.String_Access := null;
   end record;

   type File_Selection_Context is new File_Name_Selection_Context with record
      Project_View : Prj.Project_Id            := Prj.No_Project;
      Importing_Project : Prj.Project_Id       := Prj.No_Project;
   end record;

   type Entity_Selection_Context is new File_Name_Selection_Context with record
      Entity_Name : GNAT.OS_Lib.String_Access := null;
      Line, Column : Integer := 0;
   end record;

   pragma Inline (Has_Project_Information,
                  Has_Directory_Information,
                  Has_Importing_Project_Information,
                  Importing_Project_Information,
                  Project_Information,
                  Has_File_Information,
                  Has_Entity_Name_Information,
                  Entity_Name_Information,
                  Has_Line_Information,
                  Line_Information,
                  Has_Column_Information,
                  Column_Information);
end Glide_Kernel.Modules;
