-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
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
--  anything, by calling Register_Module.
--
--  Once the kernel has been created, it will call any initialization
--  function you have provided. This function might for instance be used to
--  register new menu for the menu bar, or new icons in the toolbar.
--
--  This mechanism allows the kernel to be completely independent of the
--  specific modules, since it doesn't need to know in advance the exact list
--  of modules.
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
with Gdk.Pixbuf;
with Gtk.Handlers;
with Gtk.Menu_Item;
with Gtk.Widget;
with Gtkada.MDI;
with Prj;
with Prj.Tree;
with Src_Info;
with Language;
with Basic_Types; use Basic_Types;
with Commands; use Commands;

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

   procedure Initialize (Kernel : access Kernel_Handle_Record'Class);
   --  Initialize the internal data for this package.

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
      MDI_Child_Tag           : Ada.Tags.Tag := Kernel_Handle_Record'Tag;
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
   --  MDI_Child_Tag is used to associated a given MDI child with a specific
   --  module. It should be the name of the widget inserted directly in the
   --  MDI. It is then used in conjunction with Default_Context_Factory to
   --  generate a selection context that can be used for the menubar items.
   --  Note that Kernel_Handle_Record'Tag is used as a default, non-significant
   --  value for MDI_Child_Tag.
   --
   --  Save_Function is an optional callback that will handle the saving of
   --  the given module.
   --
   --  Tooltip_Handler is an optional callback used to display tooltips.
   --  See description of Module_Tooltip_Handler in Glide_Kernel and procedure
   --  Compute_Tooltip below for more details.

   function Module_Name (ID : access Module_ID_Record'Class) return String;
   --  Return the name of t he module registered as ID.

   procedure Free_Modules (Kernel : access Kernel_Handle_Record'Class);
   --  Free all the registered modules, and call Destroy for each of these.

   function Get_Priority (ID : access Module_ID_Record'Class)
      return Module_Priority;
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
      Focus       : Boolean := False);
   --  Add an item to the default desktop.
   --  Node is a node that will generate the desired widget when
   --  passed to the Kernel.Desktop.Load_Desktop_Function.

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
   --  this command when the menu is activated.
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

   ------------
   -- Search --
   ------------

   procedure Register_Search_Pattern
     (Kernel         : access Kernel_Handle_Record'Class;
      Name           : String;
      Regexp         : String;
      Case_Sensitive : Boolean := False;
      Is_Regexp : Boolean := True);
   --  Register a new template regular expression in the search engine.
   --  Name will appear in the popdown menu of the combo box, but this will be
   --  associated with the regular expression Regexp.
   --  This emits the "search_regexps_changed" signal on Kernel.

   function Search_Regexps_Count
     (Kernel : access Kernel_Handle_Record'Class) return Natural;
   --  Return the number of registered predefined patterns

   procedure Get_Nth_Search_Regexp_Options
     (Kernel         : access Kernel_Handle_Record'Class;
      Num            : Natural;
      Case_Sensitive : out Boolean;
      Is_Regexp      : out Boolean);
   --  Return the options for the Num-th predefined search regexp

   function Get_Nth_Search_Regexp_Name
     (Kernel : access Kernel_Handle_Record'Class; Num : Natural)
      return String;
   --  Return the name, as it appears in the combo box, for the Num-th regexp.
   --  The first regexp is number 1.

   function Get_Nth_Search_Regexp
     (Kernel : access Kernel_Handle_Record'Class; Num : Natural)
      return String;
   --  Return the Num-th regular expression

   ----------------------
   -- Projects edition --
   ----------------------

   type Project_Editor_Page_Record is abstract tagged private;
   type Project_Editor_Page is access all Project_Editor_Page_Record'Class;
   --  A page that should be inserted in the project creation wizard and the
   --  project properties editor.

   procedure Destroy (Page : in out Project_Editor_Page_Record);
   --  Free the memory allocated for the page. Inherited subprograms should
   --  always call the parent's Destroy.

   function Widget_Factory
     (Page         : access Project_Editor_Page_Record;
      Project_View : Prj.Project_Id;
      Kernel       : access Kernel_Handle_Record'Class)
      return Gtk.Widget.Gtk_Widget is abstract;
   --  Return a new widget to display in the project properties editor or the
   --  project creation wizard. This can be used to store extra information
   --  closely associated with each projects (either in the project file itself
   --  or in some external files).
   --  This function should expect Project_View to be No_Project in some cases,
   --  when called from the project wizard.
   --  This subprogram should call Show_All on the returned widget. This allows
   --  it to hide some of the components when necessary. The caller should not
   --  force a Show_All on the widget.
   --
   --  Refresh is always called just after Widget_Factory.

   function Project_Editor
     (Page          : access Project_Editor_Page_Record;
      Project       : Prj.Tree.Project_Node_Id;
      Project_View  : Prj.Project_Id;
      Kernel        : access Kernel_Handle_Record'Class;
      Widget        : access Gtk.Widget.Gtk_Widget_Record'Class;
      Scenario_Variables : Prj_API.Project_Node_Array;
      Ref_Project   : Prj.Tree.Project_Node_Id)
      return Prj_API.Project_Node_Array is abstract;
   --  Modifies Project given the data in Widget. Widget is the same that was
   --  created through a Project_Editor_Page_Factor.
   --
   --  Should return the list of modified projects, if any, or an empty array
   --  if no project was modified. The returned value will be different from
   --  Project only if the modified package is a renaming of another package.
   --
   --  This subprogram should not recompute the project view itself,
   --  since this is already done once after all the modifications have been
   --  done.
   --  This function should expect Project_View to be No_Project in some cases.
   --
   --  Ref_Project is the project whose properties the user decided to edit
   --  initially (through the contextual menu). In some cases, an editor might
   --  decide that is cannot modify projects other than this one (for instance,
   --  the object directory editor only modifies ref_project). This function
   --  will not be called with Project /= Ref_Project if the flags do not
   --  include Multiple_Projects in Register_Project_Editor_Page.
   --
   --  This function might be called several times with the same project, but a
   --  different scenario if the user has decided to modify several
   --  scenarios. Project_View, if different from No_Project, will always be
   --  the processed version of Project for the current scenario.

   procedure Refresh
     (Page         : access Project_Editor_Page_Record;
      Widget       : access Gtk.Widget.Gtk_Widget_Record'Class;
      Project_View : Prj.Project_Id := Prj.No_Project;
      Languages    : GNAT.OS_Lib.Argument_List);
   --  Refresh the contents of Widget, that was created by Widget_Factory.
   --  Since Project_View is still the one when the project creation wizard or
   --  the project properties editor were initially displayed, the list of
   --  supported languages should be read from languages.
   --  By default, it does nothing.

   function Get_Label (Page : access Project_Editor_Page_Record'Class)
      return String;
   --  Return the label that should be used to identify the page in the project
   --  properties editor.

   function Get_Toc (Page : access Project_Editor_Page_Record'Class)
      return String;
   --  Return the table-of-contents label to be used in the project creation
   --  wizard.

   function Get_Title (Page : access Project_Editor_Page_Record'Class)
      return String;
   --  Return the title that should be used for this page in the project
   --  creation wizard.

   type Selector_Flags is mod 4;
   Multiple_Projects  : constant Selector_Flags := 2 ** 0;
   Multiple_Scenarios : constant Selector_Flags := 2 ** 1;
   --  The projects or scenarios the project editor applies to.
   --  Multiple_Project should be set if multiple projects can be modified by
   --  the editor.
   --  Multiple_Scenarios should be set if multiple scenarios can be modified
   --  at the same time by the editor.
   --  This flags is used to desactivate the selector widgets in the project
   --  properties dialog.

   function Get_Flags (Page : access Project_Editor_Page_Record'Class)
      return Selector_Flags;
   --  Return the list of selectors recognized by this editor

   procedure Register_Project_Editor_Page
     (Kernel    : access Kernel_Handle_Record'Class;
      Page      : Project_Editor_Page;
      Label     : String;
      Toc       : String;
      Title     : String;
      Flags     : Selector_Flags := Multiple_Projects or Multiple_Scenarios;
      Ref_Page  : String := "";
      Add_After : Boolean := True);
   --  Register a page that should be displayed both in the project wizard and
   --  the project properties editor.
   --  The new page will be put after or before the page whose label is
   --  Ref_Page, or after all the pages if Ref_Page is the empty string.

   function Project_Editor_Pages_Count
      (Kernel : access Kernel_Handle_Record'Class) return Natural;
   --  Return the number of registered project editor pages

   function Get_Nth_Project_Editor_Page
     (Kernel : access Kernel_Handle_Record'Class; Num : Natural)
      return Project_Editor_Page;
   --  Return the Num-th registered project editor page.
   --  First page is number 1.

   --------------------
   -- Mime callbacks --
   --------------------
   --  The following subprograms are provided so that the kernel can request
   --  each of its module whether it can display some specific data.
   --  The type of data is indicated through standard MIME types.

   Mime_Source_File : constant String := "gps/source";
   --  There are multiple data associated with this type:
   --     first  : full name of the source file to open (use Get_String)
   --     second : line to display initially (use Get_Int). Ignored if 0
   --     third  : column to display initially (use Get_Int). Ignored if 0
   --     fourth : True if the line should be highlighted (use Get_Boolean)
   --     fifth  : True if the location should be stored for navigation
   --              with Back/Forward.
   --     sixth  : True if a new file should be created if needed.
   --     seventh: True if file should be search from the project source path
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

   Mime_Html_File : constant String := "gps/html";
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
      Mode      : Mime_Mode := Read_Write;
      Set_Busy  : Boolean := True) return Boolean;
   --  This function calls each of the registered module, and check whether it
   --  can handle the data.
   --  If any of the module was able to, True is returned.
   --  Set_Busy tells whether the busy cursor should be set while processing
   --  the query.

   procedure Open_File_Editor
     (Kernel            : access Kernel_Handle_Record'Class;
      Filename          : String;
      Line              : Natural := 0;
      Column            : Natural := 0;
      Highlight_Line    : Boolean := True;
      Enable_Navigation : Boolean := True;
      New_File          : Boolean := True;
      From_Path         : Boolean := False);
   --  Open, or create, an editor that edits Filename (Mime_Source_File type)
   --  If Enable_Navigation is True, then the location visited will be
   --  stored in the history for Back/Forward navigation.
   --
   --  If Filename contains a relative path, the editor will open it as is. It
   --  thus depends on the current directory, and should only be used for files
   --  opened from the command line. As a result, Filename might be found even
   --  if it doesn't directly belong to a project.
   --
   --  If not found and New_File is True, a new file is edited.
   --
   --  If From_Path is True and the file doesn't contain any directory, then it
   --  is search on the source path for the current project.

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

   type Line_Information_Record is record
      Text               : String_Access := null;
      Image              : Gdk.Pixbuf.Gdk_Pixbuf := Gdk.Pixbuf.Null_Pixbuf;
      Associated_Command : Command_Access := null;
   end record;

   procedure Free (X : in out Line_Information_Record);
   --  Free memory associated with X.

   type Line_Information_Array is array (Natural range <>)
     of Line_Information_Record;

   type Line_Information_Data is access Line_Information_Array;
   for Line_Information_Data'Size use Standard'Address_Size;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Line_Information_Array, Line_Information_Data);

   function To_Line_Information is new Ada.Unchecked_Conversion
     (System.Address, Line_Information_Data);
   function To_Address is new Ada.Unchecked_Conversion
     (Line_Information_Data, System.Address);

   procedure Create_Line_Information_Column
     (Kernel         : access Kernel_Handle_Record'Class;
      File           : String;
      Identifier     : String;
      Stick_To_Data  : Boolean := True;
      Every_Line     : Boolean := True);
   --  Create a column on the side of editors for File.
   --  If Stick_To_Data is set to True, then the line information is relative
   --  to the original data in the file, otherwise it is relative to the lines
   --  in the view.
   --  If Every_Line is set to True, then the editor will emit a line_revealed
   --  signal until all lines for this column are filled.
   --  If File is empty, then the column will be created for all open files.

   procedure Remove_Line_Information_Column
     (Kernel         : access Kernel_Handle_Record'Class;
      File           : String;
      Identifier     : String);
   --  Remove the column identified by Identifier for the editors of File.
   --  If File is empty, then the column will be removed for all open files.

   procedure Add_Line_Information
     (Kernel         : access Kernel_Handle_Record'Class;
      File           : String;
      Identifier     : String;
      Info           : Line_Information_Data);
   --  Add line information to File.
   --  The range of Info must correspond to the range of line numbers
   --  that are to be modified.

   ------------------------
   -- File_Name contexts --
   ------------------------

   type File_Selection_Context is new Selection_Context with private;
   type File_Selection_Context_Access is access all
     File_Selection_Context'Class;

   procedure Set_File_Information
     (Context           : access File_Selection_Context;
      Directory         : String := "";
      File_Name         : String := "";
      Project_View      : Prj.Project_Id := Prj.No_Project;
      Importing_Project : Prj.Project_Id := Prj.No_Project);
   --  Set the information in this context.

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
     (Context : access File_Selection_Context) return String;
   --  Return the information about the selected file. This is only relevant
   --  if Has_File_Information is True.
   --  This is the base file name for the file

   function Has_Project_Information
     (Context : access File_Selection_Context) return Boolean;
   --  True if the creator of the context provided information about the
   --  project.

   function Project_Information
     (Context : access File_Selection_Context) return Prj.Project_Id;
   --  Return the id of the project to which the file belongs. Note that this
   --  is computed automatically and cached otherwise.
   --  This function will return No_Project if the file stored in the context
   --  doesn't belong to any project.

   function Has_Importing_Project_Information
     (Context : access File_Selection_Context) return Boolean;
   --  True if the context contains the name of the project importing the
   --  current one.

   function Importing_Project_Information
     (Context : access File_Selection_Context) return Prj.Project_Id;
   --  Return the project that imports the one returned by Project_Information.
   --  This is never computed automatically, and unless provided by the creator
   --  of the project, this will be left empty.

   procedure Destroy (Context : in out File_Selection_Context);
   --  Free the memory associated with the context

   ------------------------
   -- File_Area contexts --
   ------------------------

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
   -- Entity Contexts --
   ---------------------

   type Entity_Selection_Context is new File_Selection_Context
     with private;
   type Entity_Selection_Context_Access is access all Entity_Selection_Context;

   procedure Set_Entity_Information
     (Context     : access Entity_Selection_Context;
      Entity_Name : String := "";
      Line        : Integer := 0;
      Column      : Integer := 0;
      Category    : Language.Language_Category := Language.Cat_Unknown);
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

   function Has_Category_Information
     (Context : access Entity_Selection_Context) return Boolean;
   function Category_Information
     (Context : access Entity_Selection_Context)
      return Language.Language_Category;
   --  Return the category for the entity

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

private

   type File_Selection_Context is new Selection_Context with record
      Directory         : GNAT.OS_Lib.String_Access := null;
      File_Name         : GNAT.OS_Lib.String_Access := null;
      Project_View      : Prj.Project_Id            := Prj.No_Project;
      Importing_Project : Prj.Project_Id            := Prj.No_Project;

      Creator_Provided_Project : Boolean := False;
      --  Set to True if the project_view was given by the creator, instead of
      --  being computed automatically
   end record;

   type File_Area_Context is new File_Selection_Context with record
      Start_Line : Integer;
      End_Line   : Integer;
   end record;

   type Entity_Selection_Context is new File_Selection_Context with record
      Category      : Language.Language_Category := Language.Cat_Unknown;
      Entity_Name   : GNAT.OS_Lib.String_Access := null;
      Line, Column  : Integer := 0;
      Entity        : Src_Info.Queries.Entity_Information :=
        Src_Info.Queries.No_Entity_Information;
   end record;

   type Project_Editor_Page_Record is abstract tagged record
      Label, Toc, Title : GNAT.OS_Lib.String_Access;
      Flags : Selector_Flags;
   end record;

   type Project_Editor_Page_Array is array (Natural range <>)
     of Project_Editor_Page;
   type Project_Editor_Page_Array_Access is access Project_Editor_Page_Array;

   type Search_Regexp is record
      Name           : String_Access;
      Regexp         : String_Access;
      Case_Sensitive : Boolean;
      Is_Regexp      : Boolean;
   end record;

   type Search_Regexps_Array is array (Natural range <>) of Search_Regexp;
   type Search_Regexps_Array_Access is access Search_Regexps_Array;

   type Real_Kernel_Module_Data_Record is new Kernel_Module_Data_Record
   with record
      Project_Editor_Pages : Project_Editor_Page_Array_Access;
      --  The pages to be added in the project properties editor and the
      --  project creation wizard.

      Search_Regexps : Search_Regexps_Array_Access;
      --  The list of predefined regexps for the search module.
   end record;
   type Real_Module_Data is access all Real_Kernel_Module_Data_Record'Class;

   procedure Destroy (Data : in out Real_Kernel_Module_Data_Record);
   --  Free the memory associated with Data.

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
end Glide_Kernel.Modules;
