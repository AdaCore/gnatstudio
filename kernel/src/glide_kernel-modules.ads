-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
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
--  - compile GPS sources with -fPIC when required by the platform
--  - bind GPS with -shared so that the shared libgnat is used: this is
--    needed so that the same run time is used by all modules, and in
--    particular for proper dispatching (access to a global table).
--  - create libkernel.so and build gps with -lkernel by using the special
--    target 'shared' in the glide directory: make -C glide shared
--    As for libgnat, this step is needed so that global variables (e.g.
--    the debug handlers, the module ids) are shared between modules.
--  - create the dynamic module, including elaboration code that will be
--    called by Dynamic_Register_Module, e.g:
--
--    module/obj> gnat bind -E -P../$module -C -L${module}_module_ \
--                  -o ${module}_init.c *.ali
--    > gcc -c -fPIC ${module}_init.c
--    > gcc -shared -o lib${module}_module.so *.o
--
--   To load a module during GPS execution, use the command "insmod":
--
--   GPS> insmod vcs_cvs vcs__cvs
--
--  Contextual menus
--  ================
--
--   Here is a description of the sequence of events used to display contextual
--   menus in GPS:
--      - Each object that should have a contextual menu calls
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
--
--  Registering features
--  ====================
--
--   The behavior of the kernel and GPS itself can be modified extensively by
--   the modules, through a set of Register_* subprograms. This includes:
--      - Inserting new widgets in the MDI (either at startup or upon user
--        request)
--      - Adding new menus and toolbar icons
--      - Adding new contextual menu and new entries in existing menus
--      - Changing the default behavior of several standard functions, like
--        file edition, help file display, ... through Mime callbacks
--      - Adding new search contexts (see find_utils.ads in the vsearch module)
--      - Adding new predefined regular expressions in the search dialog
--      - Changing the way the current desktop is saved to disk and restored
--      - Changing what is displayed in tooltips in the editors
--      - Adding new attributes to projects, and the corresponding pages in the
--        project creation wizard or the project properties dialog.
--      - Adding new user-modifiable preferences (see glide_preferences.ads)
--      - Adding new supported languages (see language_handlers-glide.ads)
--        and the corresponding cross-referencing subprograms (same file)
--      - Each module can register new commands for the shell interpreter
--      - Adding key handlers, which have priority over other shortcuts
--
--   All these changes can be done locally in the module, and do not need any
--   modification to the rest of GPS itself (apart from registering the module
--   itself. This means that a user might choose not to load some of the
--   modules to simplify the GUI or to use less memory.

with Gdk.Event;
with Glib.Object;
with Glib.Values;
with Gdk.Types;
with Gdk.Pixbuf;
with Gtk.Image;
with Gtk.Handlers;
with Gtk.Menu_Item;
with Gtk.Selection;
with Gtk.Widget;
with Gtkada.MDI;
with Src_Info;
with Basic_Types; use Basic_Types;
with Commands; use Commands;
with Src_Info.Queries;  use Src_Info.Queries;
with VFS;

with Interfaces.C.Strings;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

package Glide_Kernel.Modules is

   Explorer_Module_Name           : constant String := "Explorer";
   Project_Editor_Module_Name     : constant String := "Project_Editor";
   Dependency_Browser_Module_Name : constant String := "Dependency_Browser";
   Project_Browser_Module_Name    : constant String := "Project_Browser";
   --  Names for the internal modules

   -----------
   -- Types --
   -----------
   --  See also the types defined in glide_kernel.ads

   package Context_Callback is new Gtk.Handlers.User_Callback
     (Glib.Object.GObject_Record, Selection_Context_Access);

   package Command_Callback is new Gtk.Handlers.User_Callback
     (Glib.Object.GObject_Record, Command_Access);

   -------------------------
   -- Module manipulation --
   -------------------------

   procedure Register_Module
     (Module                  : in out Module_ID;
      Kernel                  : access Kernel_Handle_Record'Class;
      Module_Name             : String;
      Priority                : Module_Priority := Default_Priority;
      Contextual_Menu_Handler : Module_Menu_Handler := null;
      Mime_Handler            : Module_Mime_Handler := null;
      Default_Context_Factory : Module_Default_Context_Factory := null;
      Save_Function           : Module_Save_Function := null;
      Tooltip_Handler         : Module_Tooltip_Handler := null);
   --  Register a new module into GPS.
   --  If Module is null, a new module_id is created. Otherwise, the internal
   --  information stored in Module is changed. This allows you to store user
   --  data specific to each module, instead of using global variables.
   --
   --  Module_Name can be used by other modules to check whether they want to
   --  interact with this module.
   --  See the general description for this package for explanation on
   --  Initializer and Contextual_Menu_Handler.
   --
   --  Save_Function is an optional callback that will handle the saving of
   --  the given module.
   --
   --  Tooltip_Handler is an optional callback used to display tooltips.
   --  See description of Module_Tooltip_Handler in Glide_Kernel and procedure
   --  Compute_Tooltip below for more details.

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
   --    (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --
   --  Success is set to True if the module could be successfully registered.

   function List_Of_Modules (Kernel : access Kernel_Handle_Record'Class)
      return Glide_Kernel.Module_List.List;
   --  Return the list of currently loaded modules.

   function Module_Name (ID : access Module_ID_Record'Class) return String;
   --  Return the name of the module registered as ID.

   procedure Free_Modules (Kernel : access Kernel_Handle_Record'Class);
   --  Free all the registered modules, and call Destroy for each of these.

   function Get_Priority
     (ID : access Module_ID_Record'Class) return Module_Priority;
   --  Return the current priority of ID

   ----------------------
   -- Desktop handling --
   ----------------------

   procedure Add_Default_Desktop_Item
     (Kernel      : access Kernel_Handle_Record'Class;
      Node        : Glib.Xml_Int.Node_Ptr;
      X           : Integer := 100;
      Y           : Integer := 100;
      Width       : Integer := 100;
      Height      : Integer := 100;
      Short_Title : String := "";
      Title       : String := "";
      State       : Gtkada.MDI.State_Type := Gtkada.MDI.Normal;
      Dock        : Gtkada.MDI.Dock_Side := Gtkada.MDI.None;
      Focus       : Boolean := False;
      Raised      : Boolean := False);
   --  Add an item to the default desktop.
   --  Node is a node that will generate the desired widget when
   --  passed to the Kernel.Desktop.Load_Desktop_Function.
   --  If Focus is True, then the widget will be given the focus, unless
   --  another widget is also registered later on with Focus set to True.
   --  If Raised is True and the child is docked, then this widget will appear
   --  on top unless another widget is also registered later on with Raised set
   --  to True and in the same Dock.

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
   --  Whenever a right-button click happens inside Event_On_Widget, then the
   --  following will happen:
   --     - the kernel detects the event, and creates an empty menu.
   --     - it asks Object, through Context_Func, the exact context for the
   --       menu (selected file, ....)
   --     - it then asks each of the registered modules whether it wants to
   --       add new items to the menu, and let it do so (through the
   --       Contextual_Menu_Handler provided in Register_Module)
   --     - it then displays the menu
   --     - it finally cleans up the memory when the menu is hidden

   --------------
   -- Tooltips --
   --------------

   procedure Compute_Tooltip
     (Kernel  : access Kernel_Handle_Record'Class;
      Context : Selection_Context_Access;
      Pixmap  : out Gdk.Gdk_Pixmap;
      Width   : out Glib.Gint;
      Height  : out Glib.Gint);
   --  Given a context, pointing to e.g an entity, the kernel will ask
   --  each of the registered modules whether it wants to display a tooltip.
   --  The first module to set Pixmap will stop the process.
   --  If no module wants to display a tooltip, Pixmap is set to null, and
   --  Width and Height are set to 0.

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
      Command     : Command_Access := null;
      Accel_Key   : Gdk.Types.Gdk_Key_Type := 0;
      Accel_Mods  : Gdk.Types.Gdk_Modifier_Type := 0;
      Ref_Item    : String := "";
      Add_Before  : Boolean := True;
      Sensitive   : Boolean := True);
   --  Same as the above, but creates the menu item directly, and connects the
   --  appropriate callback.
   --  If Command is not null, then a callback will be created to launch
   --  this command when the menu is activated. In this case, both Callback
   --  and Command will be called.
   --  Sensitive indicates whether the menu item is created sensitive or not.

   function Register_Menu
     (Kernel      : access Kernel_Handle_Record'Class;
      Parent_Path : String;
      Text        : String;
      Stock_Image : String := "";
      Callback    : Kernel_Callback.Marshallers.Void_Marshaller.Handler;
      Command     : Command_Access := null;
      Accel_Key   : Gdk.Types.Gdk_Key_Type := 0;
      Accel_Mods  : Gdk.Types.Gdk_Modifier_Type := 0;
      Ref_Item    : String := "";
      Add_Before  : Boolean := True;
      Sensitive   : Boolean := True) return Gtk.Menu_Item.Gtk_Menu_Item;
   --  Same as above, but returns the menu item that was created.

   function Find_Menu_Item
     (Kernel : access Kernel_Handle_Record'Class;
      Path   : String) return Gtk.Menu_Item.Gtk_Menu_Item;
   --  Given an absolute path (see Register_Menu) for a menu item, return
   --  the underlying gtk menu item. Useful in particular to check or change
   --  the state of a menu item.

   ---------------------
   -- Toolbar buttons --
   ---------------------

   procedure Register_Button
     (Kernel  : access Kernel_Handle_Record'Class;
      Text    : String;
      Command : Command_Access := null;
      Image   : Gtk.Image.Gtk_Image := null;
      Tooltip : String := "");
   --  Add a button at the end of the toolbar.

   procedure Register_Button
     (Kernel   : access Kernel_Handle_Record'Class;
      Stock_Id : String;
      Command  : Command_Access := null;
      Tooltip  : String := "");
   --  Same as above but with a stock button

   --------------------
   -- Mime callbacks --
   --------------------
   --  The following subprograms are provided so that the kernel can request
   --  each of its module whether it can display some specific data.
   --  The type of data is indicated through standard MIME types.

   Mime_Source_File : constant String := "gps/source";
   --  There are multiple data associated with this type:
   --     first  : full name of the source file to open (use Get_Address, and
   --              convert to Virtual_File)
   --     second : line to display initially (use Get_Int). Ignored if 0 (in
   --              this case, any existing editor should be left at its current
   --              line; if there is no existing editor, open the file on line
   --              1).
   --              if set to -1, close all file editors that correspond
   --              to this file.
   --     third  : column to display initially (use Get_Int). Ignored if line
   --              is 0
   --     fourth : If non 0, the area between third and fourth arguments should
   --              be selected. (use Get_Int).
   --     fifth  : True if the location should be stored for navigation
   --              with Back/Forward.
   --     sixth  : True if a new file should be created if needed.
   --  See also the function Open_File_Editor.

   Mime_File_Line_Info : constant String := "gps/file_info";
   --  There are multiple data associated with this type:
   --     first  : full name of the source file to open (use Get_String)
   --     second : identifier of the emitter (use Get_String)
   --     third  : source_line_info data (use Get_Address)
   --     fourth : line info sticks to data (instead of sticking to
   --              line numbers (use Get_Boolean)
   --     fifth  : True if every line needs to have info for this
   --              identifier (use Get_Boolean)
   --  See also the procedure Create_Line_Information_Column.

   Mime_Location_Action : constant String := "gps/location_info";
   --  There are multiple data associated with this type:
   --     first   : identifier of the emitter (use Get_String)
   --     second  : name of the category (use Get_String)
   --     third   : name of the file (use Get_String)
   --     fourth  : line (use Get_Int)
   --     fifth   : column (use Get_Int)
   --     sixth   : message (use Get_String)
   --     seventh : action item (use Get_Addess)
   --  See also the procedure Add_Location_Action

   Mime_Html_File : constant String := "gps/html";
   --  Request to display a html file
   --  There are multiple data associated with this type:
   --     first  : full name of the html file to open (use Get_String),
   --     second : True if the location should be stored for navigation
   --              with Back/Forward.
   --     third  : anchor name

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
      Mode      : Mime_Mode := Read_Write;
      Set_Busy  : Boolean := True) return Boolean;
   --  This function calls each of the registered module, and check whether it
   --  can handle the data.
   --  If any of the module was able to, True is returned.
   --  Set_Busy tells whether the busy cursor should be set while processing
   --  the query.

   procedure Open_File_Editor
     (Kernel            : access Kernel_Handle_Record'Class;
      Filename          : VFS.Virtual_File;
      Line              : Natural := 1;
      Column            : Natural := 1;
      Column_End        : Natural := 0;
      Enable_Navigation : Boolean := True;
      New_File          : Boolean := True);
   --  Open, or create, an editor that edits Filename (Mime_Source_File type)
   --  If Enable_Navigation is True, then the location visited will be
   --  stored in the history for Back/Forward navigation.
   --
   --  Consider using Glide_Kernel.Create if you do not know the full path
   --  name of the file you want to open
   --
   --  If not found and New_File is True, a new file is edited.

   procedure Clear_Highlighting
     (Kernel   : access Kernel_Handle_Record'Class;
      Filename : VFS.Virtual_File);
   --  If Filename is currently open, clear all highlighting currently
   --  associated to it.

   procedure Close_File_Editors
     (Kernel   : access Kernel_Handle_Record'Class;
      Filename : VFS.Virtual_File);
   --  Close all file editors that edit Filename.
   --  Filename must be an absolute file name.

   procedure Open_Html
     (Kernel            : access Kernel_Handle_Record'Class;
      Filename          : VFS.Virtual_File;
      Enable_Navigation : Boolean := True);
   --  Open, or create, an html viewer for Filename (Mime_Html_File type)
   --  If Enable_Navigation is True, then the location visited will be
   --  stored in the history for Back/Forward navigation.
   --  Create Filename with Glide_Kernel.Create_Html.

   procedure Display_Differences
     (Kernel         : access Kernel_Handle_Record'Class;
      Orig_File      : VFS.Virtual_File := VFS.No_File;
      New_File       : VFS.Virtual_File := VFS.No_File;
      Diff_File      : VFS.Virtual_File);
   --  Display differences between Orig_File and New_File (Mime_Diff_File type)
   --  Either Orig_File or New_File can be null (but not both), in which
   --  case, the contents of the file is computed from the other file and the
   --  diff file.

   type Line_Information_Record is record
      Text               : String_Access := null;
      Image              : Gdk.Pixbuf.Gdk_Pixbuf := Gdk.Pixbuf.Null_Pixbuf;
      Associated_Command : Command_Access := null;
   end record;
   --  Text must be a valid Utf8 string.

   type Action_Item is access Line_Information_Record;

   function To_Action_Item is new Ada.Unchecked_Conversion
     (System.Address, Action_Item);
   function To_Address is new Ada.Unchecked_Conversion
     (Action_Item, System.Address);

   procedure Free (X : in out Action_Item);
   --  Free memory associated to X.

   procedure Free (X : in out Line_Information_Record);
   --  Free memory associated with X.

   type Line_Information_Array is array (Integer range <>)
     of Line_Information_Record;

   type Line_Information_Data is access Line_Information_Array;
   for Line_Information_Data'Size use Standard'Address_Size;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Line_Information_Array, Line_Information_Data);

   function To_Line_Information is new Ada.Unchecked_Conversion
     (System.Address, Line_Information_Data);
   function To_Address is new Ada.Unchecked_Conversion
     (Line_Information_Data, System.Address);

   procedure Add_Editor_Label
     (Kernel     : access Kernel_Handle_Record'Class;
      File       : VFS.Virtual_File;
      Identifier : String;
      Label      : String);
   --  Add a label in the editors for File.

   procedure Create_Line_Information_Column
     (Kernel         : access Kernel_Handle_Record'Class;
      File           : VFS.Virtual_File;
      Identifier     : String;
      Stick_To_Data  : Boolean := True;
      Every_Line     : Boolean := True;
      Normalize      : Boolean := True);
   --  Create a column on the side of editors for File.
   --  If Stick_To_Data is set to True, then the line information is relative
   --  to the original data in the file, otherwise it is relative to the lines
   --  in the view.
   --  If Every_Line is set to True, then the editor will emit a line_revealed
   --  signal until all lines for this column are filled.
   --  If File is empty, then the column will be created for all open files.
   --  If Normalize is True, the file name will be normalized.

   procedure Remove_Line_Information_Column
     (Kernel         : access Kernel_Handle_Record'Class;
      File           : VFS.Virtual_File;
      Identifier     : String);
   --  Remove the column identified by Identifier for the editors of File.
   --  If File is empty, then the column will be removed for all open files.

   procedure Add_Line_Information
     (Kernel         : access Kernel_Handle_Record'Class;
      File           : VFS.Virtual_File;
      Identifier     : String;
      Info           : Line_Information_Data;
      Normalize      : Boolean := True);
   --  Add line information to File.
   --  The range of Info must correspond to the range of line numbers
   --  that are to be modified.
   --  If Normalize is True, the file name will be normalized.

   procedure Add_Location_Action
     (Kernel        : access Kernel_Handle_Record'Class;
      Identifier    : String;
      Category      : String;
      File          : VFS.Virtual_File;
      Line          : Integer;
      Column        : Integer;
      Message       : String;
      Action        : Action_Item);
   --  Add an action to the location specified. This will show up on the left
   --  side of the result view.

   procedure Remove_Location_Action
     (Kernel        : access Kernel_Handle_Record'Class;
      Identifier    : String;
      Category      : String;
      File          : VFS.Virtual_File;
      Line          : Integer;
      Column        : Integer;
      Message       : String);
   --  Remove action corresponding to Identifier at specified location.

   ------------------------
   -- File_Name contexts --
   ------------------------

   type File_Selection_Context is new Selection_Context with private;
   type File_Selection_Context_Access is access all
     File_Selection_Context'Class;

   procedure Set_File_Information
     (Context           : access File_Selection_Context;
      File              : VFS.Virtual_File := VFS.No_File;
      Project           : Projects.Project_Type := Projects.No_Project;
      Importing_Project : Projects.Project_Type := Projects.No_Project;
      Line              : Integer := 0;
      Column            : Integer := 0);
   --  Set the information in this context.
   --  File_Name must be UTF8-encoded.

   function Has_Directory_Information
     (Context : access File_Selection_Context) return Boolean;
   --  True if the context has information about a selected directory.

   function Directory_Information
     (Context : access File_Selection_Context) return String;
   --  Return the information about the selected project. This is only relevant
   --  if Has_Directory_Information is True.
   --  This directory name always ends with a directory separator.

   function Has_File_Information
     (Context : access File_Selection_Context) return Boolean;
   --  True if the context has information about a selected file.

   function File_Information
     (Context : access File_Selection_Context) return VFS.Virtual_File;
   --  Return the information about the selected file. This is only relevant
   --  if Has_File_Information is True.
   --  This is the base file name for the file. This name is UTF8-encoded.

   function Has_Line_Information
     (Context : access File_Selection_Context) return Boolean;
   function Line_Information
     (Context : access File_Selection_Context) return Integer;
   --  Check whether there is some line information, and return it. This is the
   --  location of the cursor in the file, when in an editor, or the location
   --  in the file from the messages window or the explorer for instance.
   --  This information will not be set if multiple lines are selected.

   function Has_Column_Information
     (Context : access File_Selection_Context) return Boolean;
   function Column_Information
     (Context : access File_Selection_Context) return Integer;
   --  Check whether there is some column information, and return it. Same
   --  comment as for Line_Information.
   --  Column is the index of the character in the string representing the
   --  line. This means that tabs only count as one, and are not expanded.

   function Has_Project_Information
     (Context : access File_Selection_Context) return Boolean;
   --  True if the creator of the context provided information about the
   --  project.

   function Project_Information
     (Context : access File_Selection_Context) return Projects.Project_Type;
   --  Return the id of the project to which the file belongs. Note that this
   --  is computed automatically and cached otherwise.
   --  This function will return No_Project if the file stored in the context
   --  doesn't belong to any project.

   function Has_Importing_Project_Information
     (Context : access File_Selection_Context) return Boolean;
   --  True if the context contains the name of the project importing the
   --  current one.

   function Importing_Project_Information
     (Context : access File_Selection_Context) return Projects.Project_Type;
   --  Return the project that imports the one returned by Project_Information.
   --  This is never computed automatically, and unless provided by the creator
   --  of the project, this will be left empty.

   procedure Destroy (Context : in out File_Selection_Context);
   --  Free the memory associated with the context

   ------------------------
   -- File_Area contexts --
   ------------------------
   --  This context is used when multiple lines are selected in an editor.

   type File_Area_Context is new File_Selection_Context with private;
   type File_Area_Context_Access is access all File_Area_Context'Class;

   procedure Set_Area_Information
     (Context    : access File_Area_Context;
      Start_Line : Integer := 0;
      End_Line   : Integer := 0);
   --  Set the area information in Context.

   procedure Get_Area
     (Context    : access File_Area_Context;
      Start_Line : out Integer;
      End_Line   : out Integer);
   --  Return the area information in Context.

   ---------------------
   -- Message_Context --
   ---------------------
   --  This context is emitted when the user clicks in the location. It is
   --  mostly used for error messages. The line and column information are
   --  the references in the error message.

   type Message_Context is new File_Selection_Context with private;
   type Message_Context_Access is access all Message_Context;

   procedure Set_Message_Information
     (Context     : access Message_Context;
      Category    : String := "";
      Message     : String := "");
   --  Set the information in the context

   function Has_Category_Information
     (Context : access Message_Context) return Boolean;
   function Category_Information
     (Context : access Message_Context) return String;
   --  Check whether there is some category information, and return it.

   function Has_Message_Information
     (Context : access Message_Context) return Boolean;
   function Message_Information
     (Context : access Message_Context) return String;
   --  Check whether there is some message information, and return it.

   ---------------------
   -- Entity Contexts --
   ---------------------

   type Entity_Selection_Context is new File_Selection_Context
     with private;
   type Entity_Selection_Context_Access is access all Entity_Selection_Context;

   procedure Set_Entity_Information
     (Context     : access Entity_Selection_Context;
      Entity_Name : String := "";
      Entity_Column : Integer := 0);
   --  Set the information in the context.
   --  Entity_Column should be the column on which the entity starts, not the
   --  current location of the cursor.
   --  The line at which the entity starts is the line set in
   --  Set_File_Information

   function Has_Entity_Name_Information
     (Context : access Entity_Selection_Context) return Boolean;
   function Entity_Name_Information
     (Context : access Entity_Selection_Context) return String;
   --  Check whether there is some entity information, and return it. This is
   --  a UTF8-encoded string.

   function Has_Entity_Column_Information
     (Context : access Entity_Selection_Context) return Boolean;
   function Entity_Column_Information
     (Context : access Entity_Selection_Context) return Integer;
   --  Check whether there is some column information, and return it.
   --  The column returned is the column on which the entity starts, not the
   --  column on which the cursor currently is.

   function Get_Entity
     (Context : access Entity_Selection_Context)
      return Src_Info.Queries.Entity_Information;
   --  Return the location of the declaration for the entity in Context.
   --  This information is automatically cached in the context, in case several
   --  modules need to compute it;
   --  No_Entity_Information is returned if the information could not be found.
   --  No also that in most cases you should set the busy cursor before calling
   --  this function, since it might take some time.
   --  You do not need to free the memory, since it will automatically be freed
   --  when the context is destroyed.

   procedure Destroy (Context : in out Entity_Selection_Context);
   --  Destroy the memory associated with the entity

   -------------------------
   -- Drag'n'drop support --
   -------------------------

   My_Target_Url    : constant Guint := 0;
   Target_Table_Url : constant Gtk.Selection.Target_Entry_Array :=
     (1 => (Interfaces.C.Strings.New_String ("text/uri-list"),
            Gtk.Selection.Target_No_Constraint, My_Target_Url));

   procedure Drag_Data_Received
     (Object : access Glib.Object.GObject_Record'Class;
      Args   : Glib.Values.GValues;
      Kernel : Glide_Kernel.Kernel_Handle);
   --  Handle text/uri-list drop events by loading the corresponding projects
   --  or files. Assume the selection data contains a string representing a LF
   --  or CR/LF separated list of files.

private

   type File_Selection_Context is new Selection_Context with record
      File              : VFS.Virtual_File          := VFS.No_File;
      Project           : Projects.Project_Type     := Projects.No_Project;
      Importing_Project : Projects.Project_Type     := Projects.No_Project;
      Line, Column      : Integer := 0;

      Creator_Provided_Project : Boolean := False;
      --  Set to True if the project_view was given by the creator, instead of
      --  being computed automatically
   end record;

   type Message_Context is new File_Selection_Context with record
      Category_Name : GNAT.OS_Lib.String_Access := null;
      Message       : GNAT.OS_Lib.String_Access := null;
   end record;

   type File_Area_Context is new File_Selection_Context with record
      Start_Line : Integer;
      End_Line   : Integer;
   end record;

   type Entity_Selection_Context is new File_Selection_Context with record
      Entity_Name   : GNAT.OS_Lib.String_Access := null;
      Entity_Column : Integer := 0;
      Entity        : Src_Info.Queries.Entity_Information :=
        Src_Info.Queries.No_Entity_Information;
   end record;

   pragma Inline (Has_Project_Information);
   pragma Inline (Has_Directory_Information);
   pragma Inline (Has_Importing_Project_Information);
   pragma Inline (Importing_Project_Information);
   pragma Inline (Project_Information);
   pragma Inline (Has_File_Information);
   pragma Inline (Has_Entity_Name_Information);
   pragma Inline (Entity_Name_Information);
   pragma Inline (Has_Line_Information);
   pragma Inline (Line_Information);
   pragma Inline (Has_Column_Information);
   pragma Inline (Column_Information);
   pragma Inline (Has_Entity_Column_Information);
   pragma Inline (Entity_Column_Information);
end Glide_Kernel.Modules;
