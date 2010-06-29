-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2001-2010, AdaCore                  --
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

--  UI extensions to GPS.Kernel.Modules
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
--      - Adding new user-modifiable preferences (see gps-preferences.ads)
--      - Adding new supported languages (see language_handlers-gps.ads)
--        and the corresponding cross-referencing subprograms (same file)
--      - Each module can register new commands for the shell interpreter
--      - Adding key handlers, which have priority over other shortcuts

with GNAT.Strings;
with Gdk.Event;
with Glib.Object;
with Glib.Values;
with Gdk.Types;
with Gtk.Image;
with Gtk.Handlers;
with Gtk.Menu;
with Gtk.Menu_Item;
with Gtk.Selection;
with Gtk.Widget;
with Commands;             use Commands;
with Commands.Interactive; use Commands.Interactive;
with Interfaces.C.Strings;
with GPS.Kernel.Actions;   use GPS.Kernel.Actions;
with GPS.Kernel.MDI;       use GPS.Kernel.MDI;
with XML_Utils;

package GPS.Kernel.Modules.UI is

   function Get_Current_Module
     (Kernel : access Kernel_Handle_Record'Class) return Module_ID;
   --  Return the module the currently selected MDI child belongs to.
   --  null might be returned if there is either no selected child or GPS
   --  couldn't find its module

   -----------
   -- Types --
   -----------
   --  See also the types defined in gps-kernel.ads

   package Context_Callback is new Gtk.Handlers.User_Callback
     (Glib.Object.GObject_Record, Selection_Context);

   ----------------------
   -- Contextual menus --
   ----------------------

   type Context_Factory is access procedure
     (Context      : in out Selection_Context;
      Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu);
   --  This function should set the context associated with the contextual
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

   type Contextual_Menu_Label_Creator_Record is abstract tagged null record;
   type Contextual_Menu_Label_Creator is
     access all Contextual_Menu_Label_Creator_Record'Class;
   function Get_Label
     (Creator   : access Contextual_Menu_Label_Creator_Record;
      Context   : Selection_Context) return String is abstract;
   --  Create the name to use for a contextual menu.
   --  If this function returns the empty string, the menu will be filtered out

   type Custom_Expansion is access function
     (Context : Selection_Context) return String;
   --  Provide the custom expansion for %C when expanding a label. If the
   --  empty string is returned, the contextual entry will not be displayed.

   Default_Contextual_Group : constant := 0;
   procedure Register_Contextual_Menu
     (Kernel      : access Kernel_Handle_Record'Class;
      Name        : String;
      Action      : Action_Record_Access;
      Label       : String := "";
      Custom      : Custom_Expansion := null;
      Stock_Image : String := "";
      Ref_Item    : String := "";
      Add_Before  : Boolean := True;
      Group       : Integer := Default_Contextual_Group);
   --  Register a new contextual menu entry to be displayed.
   --  This menu will only be shown when the filter associated with the Action
   --  matches. The name used in the menu will be Label (or Name if label isn't
   --  specified), interpreted with the usual parameter substitution:
   --     %f => current file basename
   --     %d => current directory
   --     %p => current project name
   --     %l => current line
   --     %c => current columns
   --     %a => current category
   --     %e => current entity name
   --     %i => current importing project
   --     %s => current single-line selection (nothing if multiples lines are
   --           selected)
   --     %S => selection or expression info or var name
   --     %C => value returned by Custom (the menu will not appear if this
   --           returns the empty string or Custom is undefined)
   --  The label might contain a path to indicate submenus.
   --  Image will be added to the left of the contextual menu entry.
   --  Ref_Item is the name of another contextual menu (not a label), relative
   --  to which the menu should be placed. There is no garantee that the new
   --  entry will appear just before or just after that item, in particular if
   --  other entries had the same requirement.
   --
   --  Separators:
   --  If Action is null, then a separator will be added to the contextual
   --  menu instead. It is added in a submenu if Label is not the empty string.
   --  It is good policy to specify a Ref_Item for a separator, since the
   --  separator will automatically be hidden if the Ref_Item itself is hidden
   --
   --  Groups:
   --  Group indicates the group of the entry. If Ref_Item is specified, this
   --  parameter is ignored. Otherwise, it groups items so that all items of
   --  the same group appear before all items with a greater group number.

   procedure Register_Contextual_Menu
     (Kernel      : access Kernel_Handle_Record'Class;
      Name        : String;
      Action      : Action_Record_Access;
      Label       : access Contextual_Menu_Label_Creator_Record'Class;
      Stock_Image : String := "";
      Ref_Item    : String := "";
      Add_Before  : Boolean := True;
      Group       : Integer := Default_Contextual_Group);
   --  Same as above, except the label of the menu is computed dynamically

   procedure Register_Contextual_Menu
     (Kernel            : access Kernel_Handle_Record'Class;
      Name              : String;
      Action            : Commands.Interactive.Interactive_Command_Access;
      Filter            : GPS.Kernel.Action_Filter := null;
      Visibility_Filter : Boolean := True;
      Label             : access Contextual_Menu_Label_Creator_Record'Class;
      Stock_Image       : String := "";
      Ref_Item          : String := "";
      Add_Before        : Boolean := True;
      Group             : Integer := Default_Contextual_Group);
   --  Same as above, except the action to execute is defined internally.
   --  When the command is executed, the Context.Context field will be set to
   --  the current selection context, and Context.Event to the event that
   --  triggered the menu.
   --  Action doesn't need to Push_State/Pop_State, nor handle unexpected
   --  exceptions, since this is already done by its caller. This keeps the
   --  code shorter.
   --  If Visibility_Filter is True, Filter will act on the menu's visibility.
   --  Otherwise, it will act on its sensitivity.

   procedure Register_Contextual_Menu
     (Kernel            : access Kernel_Handle_Record'Class;
      Name              : String;
      Action         : Commands.Interactive.Interactive_Command_Access := null;
      Filter            : GPS.Kernel.Action_Filter := null;
      Visibility_Filter : Boolean := True;
      Label             : String := "";
      Custom            : Custom_Expansion := null;
      Stock_Image       : String := "";
      Ref_Item          : String := "";
      Add_Before        : Boolean := True;
      Group             : Integer := Default_Contextual_Group);
   --  Same as above, but the menu title is a string where %p, %f,... are
   --  substituted.
   --  A separator is inserted if Action is null and the Filter matches.

   type Submenu_Factory_Record is abstract tagged null record;
   type Submenu_Factory is access all Submenu_Factory_Record'Class;
   procedure Append_To_Menu
     (Factory : access Submenu_Factory_Record;
      Object  : access Glib.Object.GObject_Record'Class;
      Context : Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class) is abstract;
   --  Object is the object on which the contextual menu is displayed.
   --  New entries should be appended to Menu.

   procedure Register_Contextual_Submenu
     (Kernel            : access Kernel_Handle_Record'Class;
      Name              : String;
      Label             : String := "";
      Filter            : GPS.Kernel.Action_Filter := null;
      Visibility_Filter : Boolean := True;
      Submenu           : Submenu_Factory := null;
      Ref_Item          : String := "";
      Add_Before        : Boolean := True;
      Group             : Integer := Default_Contextual_Group);
   --  Register a new submenu. Its contents can be computed dynamically by
   --  providing a Submenu callback. This can be left to null if all entries
   --  are added through Register_Contextual_Menu (in which case the call to
   --  Register_Contextual_Submenu can be used to position the parent menu
   --  where appropriate.
   --  Submenu is passed the submenu created for the item, so it doesn't need
   --  to create the submenu itself.

   function Emphasize (Name : String) return String;
   --  Parts of a contextual menu entry can be emphasized (name of entities
   --  for instance). This procedure should be used in this case, to provide
   --  consistent font and color for all emphasized words

   procedure Set_Contextual_Menu_Visible
     (Kernel  : access Kernel_Handle_Record'Class;
      Name    : String;
      Visible : Boolean);
   --  This procedure can be used to toggle the visibility of contextual menus.
   --  When a contextual menu was set as invisible, it will no longer appear.

   procedure Set_Contextual_Menu_Sensitivity
     (Kernel    : access Kernel_Handle_Record'Class;
      Name      : String;
      Sensitive : Boolean);
   --  Control whether the contextual menu entry is sensitive (ie "grayed-out")

   function Get_Registered_Contextual_Menus
     (Kernel : access Kernel_Handle_Record'Class)
      return GNAT.Strings.String_List_Access;
   --  Return the list of registered contextual menus. The returned array must
   --  be freed by the caller.

   procedure Create_Contextual_Menu
     (Kernel  : Kernel_Handle;
      Object  : Glib.Object.GObject;
      Context : Selection_Context;
      Menu    : in out Gtk.Menu.Gtk_Menu);
   --  Creates a menu from context and object.
   --  The Gtk_Menu must be created before calling this procedure.

   --------------
   -- Tooltips --
   --------------

   procedure Compute_Tooltip
     (Kernel  : access Kernel_Handle_Record'Class;
      Context : Selection_Context;
      Pixmap  : out Gdk.Gdk_Pixmap);
   --  Given a context, pointing to e.g an entity, the kernel will ask
   --  each of the registered modules whether it wants to display a tooltip.
   --  The first module to set Pixmap will stop the process.
   --  If no module wants to display a tooltip, Pixmap is set to null.

   -----------
   -- Menus --
   -----------

   type Dynamic_Menu_Factory is access procedure
     (Kernel  : access Kernel_Handle_Record'Class;
      Context : Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  Callback that fills Menu according to Context

   procedure Register_Menu
     (Kernel      : access Kernel_Handle_Record'Class;
      Parent_Path : String;
      Item        : Gtk.Menu_Item.Gtk_Menu_Item := null;
      Ref_Item    : String := "";
      Add_Before  : Boolean := True;
      Filter      : Action_Filter  := null);
   --  Add new menu items to the menu bar, as a child of Parent_Path.
   --  Parent_Path should have a form like "/main_main/submenu".
   --  Menus will be created if they don't exist.
   --  This is considered as an absolute path, as if it always started with
   --  a '/'.
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
   --
   --  To register a separator, do the following:
   --      Mitem : Gtk_Menu_Item;
   --      Gtk_New (Mitem);
   --      Register_Menu (Kernel, "/Parent_Path", Mitem);
   --
   --  The menu item will be active if Filter matches.

   procedure Register_Menu
     (Kernel      : access Kernel_Handle_Record'Class;
      Parent_Path : String;
      Text        : String;
      Stock_Image : String := "";
      Callback    : Kernel_Callback.Marshallers.Void_Marshaller.Handler;
      Command     : Interactive_Command_Access := null;
      Accel_Key   : Gdk.Types.Gdk_Key_Type := 0;
      Accel_Mods  : Gdk.Types.Gdk_Modifier_Type := 0;
      Ref_Item    : String := "";
      Add_Before  : Boolean := True;
      Sensitive   : Boolean := True;
      Action      : Action_Record_Access := null;
      Filter      : Action_Filter  := null;
      Mnemonics   : Boolean := True);
   --  Same as the above, but creates the menu item directly, and connects the
   --  appropriate callback. The latter is all of:
   --     - Callback parameter if the Filter parameter matches
   --     - Action parameter, if the Action's filter matches
   --     - Command, if the Filter parameter matches
   --  The menu item will be active if both of the Action's filter and the
   --  Filter parameter matches. If none of these are defined, the menu item is
   --  always visible.
   --  Sensitive indicates whether the menu item is created sensitive or not.
   --  Do not bind a default key to the action if you are already binding one
   --  to the menu. This would result in executing the action twice otherwise.
   --  Mnemonics indicates whether the menu should be created with mnemonics.

   function Register_Menu
     (Kernel      : access Kernel_Handle_Record'Class;
      Parent_Path : String;
      Text        : String;
      Stock_Image : String := "";
      Callback    : Kernel_Callback.Marshallers.Void_Marshaller.Handler;
      Command     : Interactive_Command_Access := null;
      Accel_Key   : Gdk.Types.Gdk_Key_Type := 0;
      Accel_Mods  : Gdk.Types.Gdk_Modifier_Type := 0;
      Ref_Item    : String := "";
      Add_Before  : Boolean := True;
      Sensitive   : Boolean := True;
      Action      : Action_Record_Access := null;
      Filter      : Action_Filter  := null;
      Mnemonics   : Boolean := True) return Gtk.Menu_Item.Gtk_Menu_Item;
   --  Same as above, but returns the menu item that was created

   procedure Register_Dynamic_Menu
     (Kernel      : access Kernel_Handle_Record'Class;
      Parent_Path : String;
      Text        : String;
      Stock_Image : String := "";
      Ref_Item    : String := "";
      Add_Before  : Boolean := True;
      Factory     : Dynamic_Menu_Factory);
   --  Register a menu that will be generated using Factory

   function Find_Menu_Item
     (Kernel : access Kernel_Handle_Record'Class;
      Path   : String) return Gtk.Menu_Item.Gtk_Menu_Item;
   --  Given an absolute path (see Register_Menu) for a menu item, return
   --  the underlying gtk menu item. Useful in particular to check or change
   --  the state of a menu item. Path is case insensitive

   ---------------------
   -- Toolbar buttons --
   ---------------------

   procedure Register_Button
     (Kernel  : access Kernel_Handle_Record'Class;
      Text    : String;
      Command : Interactive_Command_Access := null;
      Image   : Gtk.Image.Gtk_Image := null;
      Tooltip : String := "");
   --  Add a button at the end of the toolbar

   procedure Register_Button
     (Kernel   : access Kernel_Handle_Record'Class;
      Stock_Id : String;
      Command  : Interactive_Command_Access := null;
      Tooltip  : String := "");
   --  Same as above but with a stock button

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
      Kernel : GPS.Kernel.Kernel_Handle);
   --  Handle text/uri-list drop events by loading the corresponding projects
   --  or files. Assume the selection data contains a string representing a LF
   --  or CR/LF separated list of files.

   -------------
   -- Markers --
   -------------

   function Create_Marker
     (Kernel : access Kernel_Handle_Record'Class;
      Load   : XML_Utils.Node_Ptr := null) return Location_Marker;
   --  Create a marker for the current module at the current location.
   --  Load is an XML node created through a call to Save
   --  (for a Location_Marker and is used to restore a marker from a previous
   --  session.
   --  null is returned if no Location_Marker could be created.

end GPS.Kernel.Modules.UI;
