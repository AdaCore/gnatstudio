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
with Prj;
with Src_Info;
with Language;
with Basic_Types; use Basic_Types;
with Commands; use Commands;

with Unchecked_Conversion;

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
      Accel_Key   : Gdk.Types.Gdk_Key_Type := 0;
      Accel_Mods  : Gdk.Types.Gdk_Modifier_Type := 0;
      Ref_Item    : String := "";
      Add_Before  : Boolean := True;
      Sensitive   : Boolean := True);
   --  Same as the above, but creates the menu item directly, and connects the
   --  appropriate callback.
   --  Sensitive indicates whether the menu item is created sensitive or not.

   function Register_Menu
     (Kernel      : access Kernel_Handle_Record'Class;
      Parent_Path : String;
      Text        : String;
      Stock_Image : String := "";
      Callback    : Kernel_Callback.Marshallers.Void_Marshaller.Handler;
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
   --  See also the function Open_File_Editor.

   Mime_File_Line_Info : constant String := "gps/file_info";
   --  There are multiple data associated with this type:
   --     first  : full name of the source file to open (use Get_String)
   --     second : identifier of the emitter (use Get_String)
   --     third  : source_line_info data (use Get_Address)
   --     fourth : line info sticks to data (instead of sticking to
   --              line numbers (use Get_Boolean)

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
      Mode      : Mime_Mode := Read_Write) return Boolean;
   --  This function calls each of the registered module, and check whether it
   --  can handle the data.
   --  If any of the module was able to, True is returned.

   procedure Open_File_Editor
     (Kernel            : access Kernel_Handle_Record'Class;
      Filename          : String;
      Line              : Natural := 0;
      Column            : Natural := 0;
      Highlight_Line    : Boolean := True;
      Enable_Navigation : Boolean := True;
      New_File          : Boolean := True);
   --  Open, or create, an editor that edits Filename (Mime_Source_File type)
   --  If Enable_Navigation is True, then the location visited will be
   --  stored in the history for Back/Forward navigation.
   --
   --  If Filename contains a relative path, the editor will open it as is. It
   --  thus depends on the current directory, and should only be used for files
   --  opened from the command line. As a result, Filenamemight be found even
   --  if it doesn't directly belong to a project.
   --
   --  If not found and New_File is True, a new file is edited.

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
      Line               : Integer;
      Text               : String_Access := null;
      Image              : Gdk.Pixbuf.Gdk_Pixbuf := Gdk.Pixbuf.Null_Pixbuf;
      Associated_Command : Command_Access := null;
   end record;

   procedu***************
*** 440,449 ****
     is
        pragma Unreferenced (Lang, Indent_Params);

!       Index  : Natural := Buffer'Last - 1;
        Blanks : Natural;

     begin
        while Index > 1 and then Buffer (Index - 1) /= ASCII.LF loop
           Index := Index - 1;
        end loop;
--- 440,457 ----
     is
        pragma Unreferenced (Lang, Indent_Params);

!       Index  : Natural;
        Blanks : Natural;

     begin
+       if Buffer'Length = 0 then
+          Indent := 0;
+          Next_Indent := 0;
+          return;
+       end if;
+
+       Index := Buffer'Last - 1;
+
        while Index > 1 and then Buffer (Index - 1) /= ASCII.LF loop
           Index := Index - 1;
        end loop;
Index: gvd/design/TODO
===================================================================
RCS file: /paris.a/cvs/Dev/gvd/design/TODO,v
retrieving revision 1.230
diff -c -r1.230 TODO
*** gvd/design/TODO	2002/01/14 16:32:22	1.230
--- gvd/design/TODO	2002/03/18 11:44:30
***************
*** 251,261 ****
      Ie pressing these buttons once keeps executing "step" or "next" until
      interrupted by the user. The delay between each should be configurable

! 7 * [Nico] Hide/Show system files in explorer   OB Glide
      - Would be nice to hide system files by default and have the menu flip
        between hide and show system files.

! 7 * [Arno] Code editor   OB Glide
      Should display a warning when the source file is more recent than the
      executable, since the information displayed might be wrong (lines with
      code, current line, ...)
--- 251,261 ----
      Ie pressing these buttons once keeps executing "step" or "next" until
      interrupted by the user. The delay between each should be configurable

! 7 * [Nico] Hide/Show system files in explorer   OB GPS
      - Would be nice to hide system files by default and have the menu flip
        between hide and show system files.

! 7 * [Arno] Code editor   OB GPS
      Should display a warning when the source file is more recent than the
      executable, since the information displayed might be wrong (lines with
      code, current line, ...)
***************
*** 321,327 ****
      wrong. We should either not display any "=>", or display the real enum
      values (might be costly)

! 8 * [Joel] Select next entry on Breakpoint List after deleting one.
      After removing a breakpoint from the list in the Breakpoint Editor,
      the next entry should automatically be selected. This will ease the
      deletion of several breakpoints in a row.
--- 321,327 ----
      wrong. We should either not display any "=>", or display the real enum
      values (might be costly)

! 7 * [Joel] Select next entry on Breakpoint List after deleting one.
      After removing a breakpoint from the list in the Breakpoint Editor,
      the next entry should automatically be selected. This will ease the
      deletion of several breakpoints in a row.
***************
*** 346,352 ****
      again after a few next, then there is a local variable with a different
      type, which should not be detected as an alias.

! 7 * [Arno] Language is sometimes incorrectly detected
      Try the following scenario: "gvd ../tests/parse", "run", "break exception",
      "run", "up", "up", "up", "up": the file opened is still in C mode,
      although this is an Ada file.
--- 346,352 ----
      again after a few next, then there is a local variable with a different
      type, which should not be detected as an alias.

! 7 * [Arno] Language is sometimes incorrectly detected   OB GPS
      Try the following scenario: "gvd ../tests/parse", "run", "break exception",
      "run", "up", "up", "up", "up": the file opened is still in C mode,
      although this is an Ada file.
Index: gvd/glide/gvd_module.adb
===================================================================
RCS file: /paris.a/cvs/Dev/gvd/glide/gvd_module.adb,v
retrieving revision 1.20
diff -c -r1.20 gvd_module.adb
*** gvd/glide/gvd_module.adb	2002/03/15 14:57:41	1.20
--- gvd/glide/gvd_module.adb	2002/03/18 11:44:30
***************
*** 776,791 ****
        use Debugger;

     begin
        --  Initial the debugger if necessary
        if Page.Debugger = null then
-          Push_State (K, Busy);
           Configure (Page, Gdb_Type, "", (1 .. 0 => null), "");
           Set_Sensitive (K, True);
           Page.Destroy_Id := Widget_Callback.Object_Connect
             (Top, "destroy",
              Widget_Callback.To_Marshaller (On_Destroy_Window'Access),
              Page);
-          Pop_State (K);
        end if;

        --  Load a file if necessary
--- 776,791 ----
        use Debugger;

     begin
+       Push_State (K, Busy);
+
        --  Initial the debugger if necessary
        if Page.Debugger = null then
           Configure (Page, Gdb_Type, "", (1 .. 0 => null), "");
           Set_Sensitive (K, True);
           Page.Destroy_Id := Widget_Callback.Object_Connect
             (Top, "destroy",
              Widget_Callback.To_Marshaller (On_Destroy_Window'Access),
              Page);
        end if;

        --  Load a file if necessary
***************
*** 799,804 ****
--- 799,806 ----
                 Insert (K, "File not found: " & Full & Data.File);
           end;
        end if;
+
+       Pop_State (K);

     exception
        when E : others =>
Index: gvd/gvd/debugger-gdb-ada.adb
===================================================================
RCS file: /paris.a/cvs/Dev/gvd/gvd/debugger-gdb-ada.adb,v
retrieving revision 1.51
diff -c -r1.51 debugger-gdb-ada.adb
*** gvd/gvd/debugger-gdb-ada.adb	2002/03/14 15:03:23	1.51
--- gvd/gvd/debugger-gdb-ada.adb	2002/03/18 11:44:30
***************
*** 234,240 ****
     begin
        case Type_Str (Index) is
           when '<' =>
-
              --  A union type

              if Looking_At (Type_Str, Index, "<union ") then
--- 234,239 ----
***************
*** 262,268 ****
              end if;

           when 'a' =>
-
              --  Arrays, as in "array (1 .. 4, 3 .. 5) of character"

              if Looking_At (Type_Str, Index, "array ") then
--- 261,266 ----
***************
*** 285,291 ****
              end if;

           when 'd' =>
-
              --  A delta type, as for "Duration" types (delta 1e-09)

              if Looking_At (Type_Str, Index, "delta ") then
--- 283,288 ----
***************
*** 297,303 ****
              end if;

           when 'm' =>
-
              --  Modular types

              if Looking_At (Type_Str, Index, "mod ") then
--- 294,299 ----
***************
*** 315,321 ****
              end if;

           when 'n' =>
-
              --  A tagged record type, as in
              --  new tagged_type with record c : float; end record;

--- 311,316 ----
***************
*** 366,372 ****
              end if;

           when 'r' =>
-
              --  A record type, as in 'record field1: integer; end record'

              if Looking_At (Type_Str, Index, "record") then
--- 361,366 ----
***************
*** 395,402 ****
                 raise Unexpected_Type;
              end if;

!          when 't' =>

              --  A tagged type

              if Looking_At (Type_Str, Index, "tagged record") then
--- 389,398 ----
                 raise Unexpected_Type;
              end if;

!          --  ??? We could handle "string" as well as a standard type
!          --  when 's' =>

+          when 't' =>
              --  A tagged type

              if Looking_At (Type_Str, Index, "tagged record") then
***************
*** 418,425 ****
              end if;

           when '(' =>
-
              --  Enumeration type
              Skip_To_Char (Type_Str, Index, ')');
              Result := New_Enum_Type;

--- 414,421 ----
              end if;

           when '(' =>
              --  Enumeration type
+
              Skip_To_Char (Type_Str, Index, ')');
              Result := New_Enum_Type;

Index: gvd/gvd/gvd-process.adb
===================================================================
RCS file: /paris.a/cvs/Dev/gvd/gvd/gvd-process.a