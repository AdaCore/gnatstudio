------------------------------------------------------------------------------
--                                  G P S                                   --
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

--  UI extensions to GPS.Kernel.Modules
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
--
--  Handling of GUI items that are associated with actions
--  ======================================================
--
--  Some permanent GUI items are associated with actions, and therefore with
--  context-based filters. This is the case for now for global menus,
--  and toolbar buttons. These items need to be refreshed when the context
--  changes, to be made insensitive when the action they trigger does not
--  apply to the context.
--
--  There are two mechanisms for handling this at the moment, depending on
--  whether we are using system menus.
--
--    * When we are not using system menus:
--        - the states of toolbar buttons are recomputed immediately
--          when the context changes,
--        - the states of each menu item is computed at the moment when
--          the menu is mapped
--
--    * When we are using system menus: we do not have access to the 'mapped'
--      signal on the menus. So we have to recompute the state of the menus
--      every time the context changes. In order to protect ourselves from
--      performance issues, we try to do this in a limited amount of time,
--      and, failing this, schedule an idle timeout to continue the processing
--      without blocking the main loop.
--
--  The entry point of this is the body of Update_Menus_And_Buttons.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Strings;
with Glib.Object;
with Glib.Values;
with Gtk.Handlers;
with Gtk.Menu;
with Gtk.Menu_Bar;         use Gtk.Menu_Bar;
with Gtk.Menu_Item;        use Gtk.Menu_Item;
with Gtk.Target_List;
with Gtk.Toolbar;
with Gtk.Widget;
with Gtkada.Types;
with GPS.Kernel.Actions;   use GPS.Kernel.Actions;
with GPS.Kernel.Project;   use GPS.Kernel.Project;
with XML_Utils;

package GPS.Kernel.Modules.UI is

   package Context_Callback is new Gtk.Handlers.User_Callback
     (Glib.Object.GObject_Record, Selection_Context);

   ----------------------
   -- Contextual menus --
   ----------------------

   type Contextual_Menu_Factory is access procedure
     (Context : Selection_Context;
      Menu    : Gtk.Menu.Gtk_Menu);
   --  This function can be used to add custom entries to the contextual menu.
   --  It is recommended that all contextual menu items be GPS actions
   --  nowadays, and this API is only kept (hopefully briefly) for backward
   --  compatibility.

   procedure Setup_Contextual_Menu
     (Kernel          : access Kernel_Handle_Record'Class;
      Event_On_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Context_Func    : Contextual_Menu_Factory := null);
   --  Register that Event_On_Widget should create a contextual menu when the
   --  user right-clicks anywhere inside its area.
   --  Upon right-click, the following happens:
   --     - GPS computes the selection_context via a call to the MDI Child's
   --       Build_Context primitive operation.
   --     - If Context_Func is specified, it is called to add hard-coded
   --       entries to the menu. Historically, these have been used for local
   --       configurations, but it is now preferred to use the local config
   --       menu instead (via the Generic_Views package).
   --     - For all registered contextual menu actions, check whether they
   --       apply, and add them to the menu.
   --     - display the menu.

   type Contextual_Menu_Label_Creator_Record is abstract tagged null record;
   type Contextual_Menu_Label_Creator is
     access all Contextual_Menu_Label_Creator_Record'Class;
   function Get_Label
     (Creator   : access Contextual_Menu_Label_Creator_Record;
      Context   : Selection_Context) return String is abstract;
   --  Create the name to use for a contextual menu.
   --  If this function returns the empty string, the menu will be filtered out

   function Get_Path
     (Creator : access Contextual_Menu_Label_Creator_Record)
      return String is ("");
   --  Returns the full path and name of the item.

   type Custom_Expansion is access function
     (Context : Selection_Context) return String;
   --  Provide the custom expansion for %C when expanding a label. If the
   --  empty string is returned, the contextual entry will not be displayed.

   function Substitute_Label
     (Text    : String;
      Context : Selection_Context;
      Custom  : Custom_Expansion := null) return String;
   --  Substitute patterns like %e, %p,.. in Text.
   --  If some of the patterns could not be substituted, this function returns
   --  an empty string (so that the associated contextual menu does not appear)

   Default_Contextual_Group : constant := 0;
   procedure Register_Contextual_Menu
     (Kernel      : access Kernel_Handle_Record'Class;
      Action      : String;
      Name        : String := "";   --  defaults to Action
      Label       : String := "";
      Custom      : Custom_Expansion := null;
      Ref_Item    : String := "";
      Add_Before  : Boolean := True;
      Filter      : Action_Filter := null;
      Group       : Integer := Default_Contextual_Group)
      with Pre => Action /= "";
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
   --
   --  Ref_Item is the name of another contextual menu (not a label), relative
   --  to which the menu should be placed. There is no garantee that the new
   --  entry will appear just before or just after that item, in particular if
   --  other entries had the same requirement.
   --
   --  Filter:
   --  Acts as an extra filter (in addition to the action's own filter, and
   --  automatic filter computed from the label) to decide whether to show the
   --  contextual menu.
   --
   --  Groups:
   --  Group indicates the group of the entry. If Ref_Item is specified, this
   --  parameter is ignored. Otherwise, it groups items so that all items of
   --  the same group appear before all items with a greater group number.

   procedure Register_Contextual_Menu
     (Kernel      : access Kernel_Handle_Record'Class;
      Action      : String;
      Name        : String := "";   --  defaults to action
      Label       : access Contextual_Menu_Label_Creator_Record'Class;
      Ref_Item    : String := "";
      Add_Before  : Boolean := True;
      Group       : Integer := Default_Contextual_Group)
      with Pre => Action /= "";
   --  Same as above, except the label of the menu is computed dynamically

   procedure Register_Contextual_Separator
     (Kernel      : access Kernel_Handle_Record'Class;
      Action      : String := "";   --  filter
      In_Submenu  : String := "";
      Ref_Item    : String := "";
      Add_Before  : Boolean := True;
      Group       : Integer := Default_Contextual_Group);
   --  Separators:
   --  If Action is "" then a separator will be added
   --  to the contextual menu instead. It is added in a submenu if In_Submenu
   --  is specified. If an action was specified, the separator will be
   --  hidden when the action does not apply to the current context.

   type Submenu_Factory_Record is abstract tagged null record;
   type Submenu_Factory is access all Submenu_Factory_Record'Class;
   procedure Append_To_Menu
     (Factory : access Submenu_Factory_Record;
      Context : Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class) is abstract;
   --  Object is the object on which the contextual menu is displayed.
   --  New entries should be appended to Menu.

   procedure Register_Contextual_Submenu
     (Kernel            : access Kernel_Handle_Record'Class;
      Name              : String;
      Label             : String := "";
      Filter            : access Action_Filter_Record'Class := null;
      Enable_Filter     : access Action_Filter_Record'Class := null;
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

   procedure Add_Actions_To_Contextual_Menu
     (Context : Selection_Context;
      Menu    : in out Gtk.Menu.Gtk_Menu);
   --  Creates a menu from context and object.
   --  The Gtk_Menu must be created before calling this procedure.

   --------------
   -- Tooltips --
   --------------

   function Compute_Tooltip
     (Kernel  : access Kernel_Handle_Record'Class;
      Context : Selection_Context) return Gtk.Widget.Gtk_Widget;
   --  Given a context, pointing to e.g an entity, the kernel will ask
   --  each of the registered modules whether it wants to display a tooltip.
   --  The first module to return non-null will stop the process.
   --  If no module wants to display a tooltip, returns null.

   -----------
   -- Menus --
   -----------

   procedure Install_Menus
     (Kernel    : not null access Kernel_Handle_Record'Class;
      Menubar   : out Gtk.Menu_Bar.Gtk_Menu_Bar);
   --  Load an XML description of the menubar, and create it.

   procedure Start_Monitoring_Menus
     (Kernel      : not null access Kernel_Handle_Record'Class);
   --  Start monitoring the context changes to update the menu sensitivity.

   procedure Update_Menus_And_Buttons
     (Kernel  : not null access Kernel_Handle_Record'Class;
      Context : GPS.Kernel.Selection_Context := No_Context);
   --  Recompute the visibility and sensitivity of menus and toolbar buttons.
   --  This computation is asynchronous so that it doesn't block the user.

   procedure Action_Status_Changed
     (Kernel  : not null access Kernel_Handle_Record'Class;
      Name    : String);
   --  This procedure should be called when an action is overridden or
   --  disabled/enabled by the user. This ensures all associated menus and
   --  buttons will be put on the list of things to check when the context
   --  changes.

   procedure Register_Menu
     (Kernel          : not null access Kernel_Handle_Record'Class;
      Path            : String;
      Action          : String;
      Ref_Item        : String  := "";
      Before_Ref_Item : Boolean := True;
      Prepend         : Boolean := False);
   --  Append a menu binding a GPS action. The action need not exist when the
   --  menu is created (but the menu will always be greyd out if the action
   --  does not exist).
   --  Accel_Key, Accel_Mods are looked up from the action.
   --  Filter is looked up from the action.
   --  The image is also looked up from the action.
   --
   --  If Prepend then the menu will be added at the beginning,
   --  Before_Ref_Item is ignored in this case.
   --
   --  When a menu is optional, it is hidden if its action does not exist.
   --  Otherwise, the menu is simply greyed out, but the menu is still visible.

   procedure Remove_UI_For_Action
     (Kernel : not null access Kernel_Handle_Record'Class;
      Action : String);
   --  Remove all menu and toolbar items associated with the given action,
   --  in all windows that have a menubar.

   function Action_From_Menu
     (Kernel : not null access Kernel_Handle_Record'Class;
      Path   : String) return String;
   --  Return the name of the action executed by a menu (or "" if there is no
   --  such menu or it is not associated with an action).
   --  If Path is not a menu path (starting with /), it is returned as is. So
   --  this function can be given the name of any action to retrieve either the
   --  action itself or the action that would be executed by selecting the menu

   function Menu_List_For_Action (Action : String) return Unbounded_String;
   --  Return a newline-separated list of menus associated with this action.

   procedure Update_Shortcuts_For_Action
     (Kernel : not null access Kernel_Handle_Record'Class;
      Action : String);
   --  Update the shortcuts for all menus associated with the action.

   procedure Execute_Menu
     (Kernel    : Kernel_Handle;
      Menu_Name : String);
   --  Execute immediately a menu. Menu_Name is the full path to the menu.

   procedure Append_Menu
     (Kernel    : not null access Kernel_Handle_Record'Class;
      Menu      : not null access Gtk.Menu.Gtk_Menu_Record'Class;
      Label     : String;
      Action    : String);
   function Append_Menu
     (Kernel    : not null access Kernel_Handle_Record'Class;
      Menu      : not null access Gtk.Menu.Gtk_Menu_Record'Class;
      Label     : String;
      Action    : String)
      return Gtk_Menu_Item;
   --  Append a new entry to the menu, that will execute the action.
   --  This is meant for local config menus.

   function Group_Mains_Into_Projects
     (Kernel      : not null access Kernel_Handle_Record'Class;
      Mains_Count : Natural)
      return Boolean
     is (Mains_Count > 15
         or else Get_Project (Kernel).Is_Aggregate_Project);
   --  Whether the menus that display a list of main units should group them
   --  into projects, or display a flat list.

   ---------------------
   -- Toolbar buttons --
   ---------------------

   procedure Register_Button
     (Kernel          : not null access Kernel_Handle_Record'Class;
      Action          : String;
      Icon_Name       : String := "";
      Label           : String := "";
      Toolbar         : String := "main";
      Section         : String := "";
      Group           : String := "";
      Hide            : Boolean := False);
   --  Register a button based on an action.
   --  The action need not be registered yet.
   --  Icon_Name overrides the action's default image, if specified.
   --  The button will be grayed out automatically whenever the action's filter
   --  indicate the action is not available in the current context.
   --
   --  If Hide is true, the button is hidden when the action does not apply
   --  to the context, instead of being grayed out.
   --
   --  Label is used to override the label on the button, which by default
   --  is the name of the action.
   --
   --  Toolbar defaults to the main toolbar, but you can use some other names
   --  like 'Locations', 'Messages', 'Browser',... to indicate other toolbars.
   --  See the file share/menus.xml and the view names in generic_views.ads.
   --
   --  Section is the position within the toolbar where the new button will be
   --  added.
   --
   --  If Group is specified, a button will be created that can be long-pressed
   --  to popup a menu with all actions associated with the group. Clicking on
   --  the button executes the last action.

   function Get_Toolbar_Section
     (Toolbar : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class;
      Section : String;
      Last    : Boolean := True) return Glib.Gint;
   --  Return the first or last item position in a given section of the
   --  toolbar. A toolbar starts on the first item after the separator with
   --  the name of the section, and ends just before the next separator.

   procedure Create_Toolbar
     (Kernel          : not null access Kernel_Handle_Record'Class;
      Toolbar         : in out Gtk.Toolbar.Gtk_Toolbar;
      Id              : String);
   --  Populate Toolbar with the predefined actions defined from the XML in
   --  Install_Menus.
   --  Toolbar is created if null, or emptied otherwise and then reused.
   --  Any button registered for it later on will be dynamically added to
   --  the toolbar.

   procedure Declare_Toolbar
     (Kernel        : not null access Kernel_Handle_Record'Class;
      Id            : String;
      Inherits      : String := "");
   --  Declares a new toolbar (as would be done by modifying menus.xml),
   --  empty unless it inherits from another toolbar.
   --  This has no effect if the toolbar already exists, for instance because
   --  it was defined in menus.xml.

   -------------------------
   -- Drag'n'drop support --
   -------------------------

   My_Target_Url    : constant Guint := 0;
   Target_Table_Url : constant Gtk.Target_List.Target_Entry_Array :=
     (1 => (Gtkada.Types.New_String ("text/uri-list"),
            0, My_Target_Url));

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

   procedure Initialize (Kernel : access Kernel_Handle_Record'Class);
   --  Initialize the contextual menu by creating root menu item.

private

   type Action_Proxy is abstract tagged record
      Kernel   : access Kernel_Handle_Record'Class;
      Action   : GNAT.Strings.String_Access;

      Optional : Boolean;
      --  If True and the action is not found, the widget will be hidden.

      Hide     : Boolean;
      --  If true, the widget is hidden when the filter does not match.

      Looked_Up : access Action_Record;
      --  A field that must be used only to compare the current action with the
      --  one we previously looked up. Do not use to access the action itself,
      --  since this might be a dangling pointer if the action was
      --  unregistered. Use Lookup_Action instead.
   end record;
   --  Data required for all widgets that encapsulate an action.
   --  A widget must never store a direct Action_Record_Access, since the
   --  action might be unregistered at any point.
   --  This type also provides support for setting various properties of the
   --  widget based on the contents of the action.

   procedure Set_Active
     (Self   : in out Action_Proxy;
      Active : Boolean;
      Object : not null access Glib.Object.GObject_Record'Class) is null;
   --  Called whenever we recompute the status (enabled/disabled) of the
   --  action in the background.

end GPS.Kernel.Modules.UI;
