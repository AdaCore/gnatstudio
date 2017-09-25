------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2017, AdaCore                     --
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

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash_Case_Insensitive;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Browsers.Canvas;          use Browsers, Browsers.Canvas;
with Commands.Interactive;     use Commands, Commands.Interactive;
with Debugger;                 use Debugger;
with Default_Preferences;      use Default_Preferences;
with Gdk.RGBA;                 use Gdk.RGBA;
with Gdk.Window;               use Gdk.Window;
with Gdk;                      use Gdk;
with Generic_Views;            use Generic_Views;
with Glib.Object;              use Glib.Object;
with Glib;                     use Glib;
with GNAT.Regpat;              use GNAT.Regpat;
with GNAT.Strings;             use GNAT.Strings;
with GNATCOLL.JSON;
with GNATCOLL.Traces;          use GNATCOLL.Traces;
with GNATCOLL.Utils;           use GNATCOLL.Utils;
with GNATCOLL.VFS;             use GNATCOLL.VFS;
with GPS.Intl;                 use GPS.Intl;
with GPS.Debuggers;            use GPS.Debuggers;
with GPS.Kernel.Actions;       use GPS.Kernel.Actions;
with GPS.Kernel.Contexts;      use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;         use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;           use GPS.Kernel.MDI;
with GPS.Kernel.Modules.UI;    use GPS.Kernel.Modules.UI;
with GPS.Kernel.Modules;       use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;   use GPS.Kernel.Preferences;
with GPS.Kernel.Properties;    use GPS.Kernel.Properties;
with GPS.Properties;           use GPS.Properties;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Check_Menu_Item;      use Gtk.Check_Menu_Item;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Handlers;             use Gtk.Handlers;
with Gtk.Menu;                 use Gtk.Menu;
with Gtk.Menu_Item;            use Gtk.Menu_Item;
with Gtk.Radio_Menu_Item;      use Gtk.Radio_Menu_Item;
with Gtk.Separator_Menu_Item;  use Gtk.Separator_Menu_Item;
with Gtk.Widget;               use Gtk.Widget;
with Gtkada.Canvas_View.Views; use Gtkada.Canvas_View.Views;
with Gtkada.Handlers;          use Gtkada.Handlers;
with Gtkada.MDI;               use Gtkada.MDI;
with Gtkada.Style;             use Gtkada.Style;
with GVD.Contexts;             use GVD.Contexts;
with GVD.Generic_View;         use GVD.Generic_View;
with GVD.Items;                use GVD.Items;
with GVD.Memory_View;
with GVD.Menu;                 use GVD.Menu;
with GVD.Preferences;          use GVD.Preferences;
with GVD.Process;              use GVD.Process;
with Process_Proxies;
with GVD.Trace;
with GVD.Types;                use GVD.Types;
with GVD_Module;               use GVD_Module;
with Histories;                use Histories;
with Items.Simples;            use Items.Simples;
with Language;                 use Language;
with Pango.Font;               use Pango.Font;
with GPS.Dialogs;              use GPS.Dialogs;
with String_Utils;             use String_Utils;
with Xref;                     use Xref;

package body GVD.Canvas is

   Me : constant Trace_Handle := Create ("Canvas");

   Detect_Aliases : Boolean_Preference;

   GVD_Canvas_Contextual_Menus_Group : constant Integer := 2;
   --  The GVD.Canvas actions' contextual menus group

   Float_Re : constant String := "([+-]?\d+(?:\.\d+E[+-]\d+))";
   --  regexp for a float number

   Graph_Cmd_Format : constant Pattern_Matcher := Compile
     ("("                  --  paren 1: whole command except dynamic attributes
      & "graph\s+(print|display)"         --  paren 2: type of command
      & "\s+("                            --  paren 3: what to display
      & "`([^`]+)`"                       --  paren 4: a general expression ?
      & "|""([^""]+)"""                   --  paren 5: or a quoted expression ?
      & "|(\S+)"                          --  paren 6: or a standard name ?
      & ")"                               --  end of paren 3
      & "(\s+dependent\s+on\s+(\d+))?"    --  parenthesis 7 .. 8
      & "(\s+link_name\s+(\S+))?"         --  parenthesis 9 .. 10
      & ")"                               --  end of paren 1
      & "(\s+at\s+" & Float_Re & ",\s*" & Float_Re & ")?" --  parens 11 .. 13
      & "(\s+num\s+(\d+))?"               --  parenthesis 14 .. 15
      & "(\s+alias_of\s+\d+)?",           --  parenthesis 16
      Case_Insensitive);
   --  Format of the graph print commands, and how to parse them

   Graph_Cmd_Max_Paren           : constant := 15;
   Graph_Cmd_Type_Paren          : constant := 2;
   Graph_Cmd_Expression_Paren    : constant := 4;
   Graph_Cmd_Quoted_Paren        : constant := 5;
   Graph_Cmd_Variable_Paren      : constant := 6;
   Graph_Cmd_Dependent_Paren     : constant := 8;
   Graph_Cmd_Link_Paren          : constant := 10;
   Graph_Cmd_At_Paren            : constant := 11;
   Graph_Cmd_X_Paren             : constant := 12;
   Graph_Cmd_Y_Paren             : constant := 13;
   Graph_Cmd_Num_Paren           : constant := 15;
   --  Indexes of the parentheses pairs in Graph_Cmd_Format for each of the
   --  relevant fields.

   Graph_Cmd_Format2 : constant Pattern_Matcher := Compile
     ("graph\s+(enable|disable)\s+display\s+(.*)", Case_Insensitive);
   --  Second possible set of commands

   Graph_Cmd_Format3 : constant Pattern_Matcher := Compile
     ("graph\s+undisplay\s+(.*)", Case_Insensitive);
   --  Third possible set of commands

   Typed_Aliases : constant Boolean := True;
   --  If True, then two items are aliases only if they have the same address
   --  *and* they are structurally equivalent. If False, only the addresses
   --  are checked.

   --  Aliases detection
   --  ==================
   --
   --  This package provides a complete aliases detection, ie when some items
   --  are found at the same location in memory. Each item has a uniq id,
   --  which most often is an address in memory, but can also be different for
   --  instance on the Java Virtual Machine.
   --  Every time a new item is inserted in the canvas, either as a result of
   --  a "graph print" or "graph display" command, or when the user clicks on
   --  an access type to dereference it, this package will test that there is
   --  not already an item on the canvas with the same Id.
   --  In every case, the new item is created (with possibly a link to it if it
   --  was a dereference). However, if there was already an item with the same
   --  id then the new item is set to be hidden (ie will not be displayed,
   --  nor any link to or from it).
   --  The links to and from an alias (hidden item) are automatically
   --  duplicated to reference the visible item, so that they are correctly
   --  visible and moved on the canvas. These temporary links have a special
   --  attribute Alias_Link set.
   --
   --  Just before the next update of the canvas, all these temporary links are
   --  removed, all aliases are cancelled and all items are made visible. Then
   --  we recompute the list of aliases before redrawing the canvas. It is
   --  worth noting that when we have an hidden item, we do not waste time
   --  reparsing its value.
   --
   --  Note also that for simplicity we do not create chains of aliases, ie
   --  an item is an alias to a second, which in turn in an alias to a third.
   --  Instead, both the first and the second will refer the same third. It is
   --  thus much easier to deal with aliases.
   --
   --  To improve the support for strings in Ada, an extra rule is added:
   --  X.all can not be an alias of X. It is always considered to be a
   --  different object. This is needed, otherwise it is mostly impossible to
   --  properly display a String parameter correctly.

   type GVD_Canvas_Record is
     new Debugger_Data_View_Record and View_With_Process with
      record
         Process  : access Base_Visual_Debugger'Class;
         Item_Num : Integer := 0;
      end record;
   type GVD_Canvas is access all GVD_Canvas_Record'Class;

   overriding procedure Set_Process
     (Self    : not null access GVD_Canvas_Record;
      Process : access Base_Visual_Debugger'Class);
   overriding function Get_Process
     (Self : not null access GVD_Canvas_Record)
      return access Base_Visual_Debugger'Class is (Self.Process);
   overriding procedure Create_Menu
     (View    : not null access GVD_Canvas_Record;
      Menu    : not null access Gtk.Menu.Gtk_Menu_Record'Class);
   overriding procedure On_Attach
     (Canvas  : not null access GVD_Canvas_Record;
      Process : not null access Base_Visual_Debugger'Class);
   overriding procedure Update (Canvas : not null access GVD_Canvas_Record);

   type On_Command is new Debugger_String_Hooks_Function with null record;
   overriding function Execute
     (Self    : On_Command;
      Kernel  : not null access Kernel_Handle_Record'Class;
      Process : access GPS.Debuggers.Base_Visual_Debugger'Class;
      Command : String) return String;
   --  Handles the "graph display", "graph enable", ... custom commands

   type Local_Vars_Command is new Dbg_Command.Debugger_Command
   with null record;
   overriding function Execute_Dbg
     (Command : access Local_Vars_Command;
      Process : Visual_Debugger) return Command_Return_Type;
   --  Debug->Data->Display Local Variables

   type Arguments_Command is new Dbg_Command.Debugger_Command with null record;
   overriding function Execute_Dbg
     (Command : access Arguments_Command;
      Process : Visual_Debugger) return Command_Return_Type;
   --  Debug->Data->Display Arguments

   type Registers_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Registers_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Debug->Data->Display Registers

   type Expression_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Expression_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Debug->Data->Display Any Expression

   type On_Pref_Changed is new Preferences_Hooks_Function with record
      Canvas : GVD_Canvas;
   end record;
   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference);
   --  Called when the preferences have changed, to refresh the GVD canvas
   --  appropriately.

   function Initialize
     (Self   : access GVD_Canvas_Record'Class) return Gtk_Widget;
   procedure Set_Canvas
     (Process : not null access Base_Visual_Debugger'Class;
      Canvas  : access GVD_Canvas_Record'Class := null);
   function Get_Canvas
     (Process : not null access Base_Visual_Debugger'Class)
      return access GVD_Canvas_Record'Class;
   --  See the doc in GVD.Generic_View

   package Canvas_MDI_Views is new Generic_Views.Simple_Views
     (Module_Name        => "Debugger_Data",
      View_Name          => "Debugger Data",
      Formal_View_Record => GVD_Canvas_Record,
      Formal_MDI_Child   => Browser_Child_Record,
      Reuse_If_Exist     => False,
      Commands_Category  => "",
      Areas              => Gtkada.MDI.Sides_Only,
      Group              => Group_Graphs,
      Position           => Position_Top,
      Local_Config       => True,
      Local_Toolbar      => True,
      Initialize         => Initialize);
   package Canvas_Views is new GVD.Generic_View.Simple_Views
     (Views              => Canvas_MDI_Views,
      Formal_View_Record => GVD_Canvas_Record,
      Formal_MDI_Child   => Browser_Child_Record,
      Get_View           => Get_Canvas,
      Set_View           => Set_Canvas);

   function Get_Next_Item_Num
     (Debugger  : access GVD.Process.Visual_Debugger_Record'Class;
      Candidate : Integer := -1) return Integer;
   --  Return the number that should be used for the next item inserted into
   --  the canvas.
   --  Two successive calls to that function will not return the same value.
   --  Candidate is a suggested candidate, and will be returned if no
   --  other item matches that number. It is ignored if unspecified

   procedure Refresh_Data_Window
     (Debugger : access GVD.Process.Visual_Debugger_Record'Class);
   --  Refresh the contents of the data window (if any) associated with
   --  Debugger

   procedure Process_Graph_Cmd
     (Process : not null access GVD.Process.Visual_Debugger_Record'Class;
      Cmd     : String);
   --  Parse and process a "graph print" or "graph display" command

   function Get_Detect_Aliases
     (Process : access GVD.Process.Visual_Debugger_Record'Class)
      return Boolean;
   --  Return True if aliases detection has been activated.

   type GVD_Link_Record is new GPS_Link_Record with record
      Alias_Link : Boolean := False;
      --  True if this Link was created as a result of an aliasing operation.
      --  Such links are always deleted before each update, and recreated
      --  whenever an aliasing is detected.

      Name : Unbounded_String;
   end record;
   type GVD_Link is access all GVD_Link_Record'Class;

   procedure Canvas_Contextual_Factory
     (Context : GPS.Kernel.Selection_Context;
      Menu    : Gtk.Menu.Gtk_Menu);
   --  Build the context and contextual menu when right clicking in the canvas

   type Display_Item_Record;
   type Display_Item is access all Display_Item_Record;
   type Display_Item_Record is new GPS_Item_Record with record
      Num          : Integer;

      Info         : Item_Info;

      Depend_On    : Integer := -1;
      --  Set if there was a "dependent on..." when creating the item

      Debugger     : GVD.Process.Visual_Debugger;

      Id           : GNAT.Strings.String_Access := null;
      --  Uniq ID used for the variable.
      --  This Id is returned by the debugger, and can be the address of a
      --  variable (in Ada or C), or simply the name of the variable (in
      --  Java) when no overloading exists and addresses don't have any
      --  meaning. This is used to detect aliases.

      Is_Alias_Of  : Display_Item := null;
      --  Item for which we are an alias.

      Is_Dereference : Boolean := False;
      --  True if the item was created as a result of a derefence of an
      --  access type. Such items can be hidden as a result of aliases
      --  detection, whereas items explicitly displayed by the user are
      --  never hidden.

      Was_Alias      : Boolean := False;
      --  Memorize whether the item was an alias in the previous display, so
      --  that we can compute a new position for it.

   end record;

   overriding procedure Destroy
     (Self     : not null access Display_Item_Record;
      In_Model : not null access Canvas_Model_Record'Class);

   package String_To_Items is new Ada.Containers.Indefinite_Hashed_Maps
     (String, Display_Item, Ada.Strings.Hash_Case_Insensitive, "=", "=");
   use String_To_Items;

   function Search_Item
     (Process : access Visual_Debugger_Record'Class;
      Id      : String;
      Name    : String) return Display_Item;
   --  Search for an item whose Id is Id in the canvas.
   --  If Name is not the empty string, the name must also match

   function Is_Alias_Of
     (Item : access Display_Item_Record'Class;
      Id   : String;
      Name : String;
      Deref_Name : String) return Boolean;
   --  Return True if Item is an alias of the entity with name Name and
   --  whose Id is Id.
   --  Deref_Name should be the dereferenced version of Name

   procedure Update_Display (Item : not null access Display_Item_Record'Class);
   --  Recompute the GUI rendering for the item.
   --  This does not refresh the value read from the debugger, only its
   --  display.

   procedure Create_Link
     (Browser    : not null access Debugger_Data_View_Record'Class;
      From, To   : access Display_Item_Record'Class;
      Name       : String;
      Alias_Link : Boolean := False);
   --  Add a new link between two items.
   --  The link is not created if there is already a similar one.

   function Get_Graph_Cmd
     (Item : access Display_Item_Record'Class) return String;
   --  Return the "graph display..." command used to create the item

   function Find_Item
     (Canvas : not null access Debugger_Data_View_Record'Class;
      Num    : Integer) return Display_Item;
   --  Return the item whose identifier is Num, or null if there is none

   procedure Set_Auto_Refresh
     (Item          : access Display_Item_Record'Class;
      Auto_Refresh  : Boolean;
      Update_Value  : Boolean := False);
   --  Change the auto refresh status of the item, and update its pixmap.
   --  If Update_Value is True, then the value of the item is recomputed
   --  if necessary.

   procedure Recompute_All_Aliases
     (Process          : access GVD.Process.Visual_Debugger_Record'Class;
      Recompute_Values : Boolean := True);
   --  Recompute all the aliases, and reparse the values for all the
   --  displayed items if Recompute_Values is True

   procedure Update (Item : not null access Display_Item_Record'Class);
   --  Unconditionally update the value of Item after parsing the new value.
   --  This does not redraw the canvas or the item on the canvas, unless
   --  Redisplay_Canvas is True

   procedure Gtk_New
     (Item           : out Display_Item;
      Browser        : not null access Debugger_Data_View_Record'Class;
      Info           : Item_Info;
      Num            : Integer;
      Debugger       : access Visual_Debugger_Record'Class;
      Auto_Refresh   : Boolean := True;
      Is_Dereference : Boolean := False);
   --  Create a new item to display the value of Variable_Name (or return an
   --  existing item if one matches).
   --
   --  Auto_Refresh should be set to True if the value of Variable should
   --  be parsed again whenever the debugger stops. This is the default
   --  behavior, that can be changed by the user.
   --
   --  Graph_Cmd is the "graph" command that was used to create the item. This
   --  is the command that is saved across GVD sessions so that we can restore
   --  the displayed variables the next time the debugger is started.
   --
   --  If Variable_Name is "", then no parsing is done to get the type and
   --  or value of the variable.
   --  Default_Entity can be used to initialize the entity associated with the
   --  item. This will be used instead of Variable_Name if not null.
   --
   --  Debugger can be null. In this case, the item will never be computed
   --
   --  Num must be specified, and is the number of the item

   function Custom_Label_Expansion
     (Context : Selection_Context) return String;
   --  Expand "%C" to the dereferenced name for the current variable.

   type Access_Variable_Filter is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Access_Variable_Filter;
      Context : Selection_Context) return Boolean;

   type Print_Variable_Command is new Interactive_Command with record
      Display     : Boolean := False;
      Dereference : Boolean := False;
   end record;
   overriding function Execute
     (Command : access Print_Variable_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Access_Variable_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      if Has_Entity_Name_Information (Context) then
         declare
            Entity : constant Root_Entity'Class := Get_Entity (Context);
         begin
            return Is_Fuzzy (Entity)

              --  ??? Should also include array variables
              or else (not Is_Type (Entity)
                       and then Is_Access (Entity));
         end;

      elsif Has_Area_Information (Context) then
         return True;
      end if;
      return False;
   end Filter_Matches_Primitive;

   -----------------
   -- Execute_Dbg --
   -----------------

   overriding function Execute_Dbg
     (Command : access Local_Vars_Command;
      Process : Visual_Debugger) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Block : Process_Proxies.Parse_File_Switch
        (Process.Debugger.Get_Process) with Unreferenced;
   begin
      Process_User_Command
        (Process,
         "graph display `" & Info_Locals (Process.Debugger) & '`',
         Output_Command => True);
      return Commands.Success;
   end Execute_Dbg;

   -----------------
   -- Execute_Dbg --
   -----------------

   overriding function Execute_Dbg
     (Command : access Arguments_Command;
      Process : Visual_Debugger) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Block : Process_Proxies.Parse_File_Switch
        (Process.Debugger.Get_Process) with Unreferenced;
   begin
      Process_User_Command
        (Process,
         "graph display `" & Info_Args (Process.Debugger) & '`',
         Output_Command => True);
      return Commands.Success;
   end Execute_Dbg;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Registers_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel  : constant Kernel_Handle := Get_Kernel (Context.Context);
      Process : constant Visual_Debugger :=
        Visual_Debugger (Get_Current_Debugger (Kernel));
      Result : Boolean with Unreferenced;

   begin
      if Process = null or else Process.Debugger = null then
         return Commands.Failure;
      end if;

      if GVD.Preferences.Debugger_Kind.Get_Pref = Gdb_MI then
         Result := Execute_Action
           (Kernel, "open registers view", Context.Context);

      else
         Process_User_Command
           (Process,
            "graph display `" & Info_Registers (Process.Debugger) & '`',
            Output_Command => True);
      end if;

      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Expression_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel  : constant Kernel_Handle := Get_Kernel (Context.Context);
      Process : constant Visual_Debugger :=
        Visual_Debugger (Get_Current_Debugger (Kernel));
   begin
      if Process = null or else Process.Debugger = null then
         return Commands.Failure;
      end if;

      Display_Expression (Process);
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Print_Variable_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Process  : constant Visual_Debugger :=
        Visual_Debugger (Get_Current_Debugger (Get_Kernel (Context.Context)));
      Debugger : constant Debugger_Access := Process.Debugger;
      Name     : constant String :=
        Get_Variable_Name (Context.Context, Command.Dereference);
   begin
      if Name /= "" then
         if Command.Display then
            Process_User_Command
              (Process, "graph display " & Name, Output_Command => True);
         else
            Send (Debugger, Print_Value_Cmd (Debugger, Name),
                  Mode => GVD.Types.Visible);
         end if;
      end if;
      return Commands.Success;
   end Execute;

   ----------------------------
   -- Custom_Label_Expansion --
   ----------------------------

   function Custom_Label_Expansion
     (Context : Selection_Context) return String is
   begin
      return Emphasize (Get_Variable_Name (Context, True));
   end Custom_Label_Expansion;

   ----------------
   -- Properties --
   ----------------

   type GVD_Items_Property_Record is new Property_Record with record
      Items : String_List_Access;
   end record;
   type GVD_Items_Property is access all GVD_Items_Property_Record'Class;

   overriding procedure Save
     (Property : access GVD_Items_Property_Record;
      Value    : in out GNATCOLL.JSON.JSON_Value);
   overriding procedure Load
     (Property : in out GVD_Items_Property_Record;
      Value    : GNATCOLL.JSON.JSON_Value);
   overriding procedure Destroy (Property : in out GVD_Items_Property_Record);
   --  See inherited documentation

   procedure Load_Items_From_Property
     (Process  : access Visual_Debugger_Record'Class;
      Property : GVD_Items_Property_Record'Class);
   --  Restore the items stored in the property

   -----------------
   -- Local Types --
   -----------------

   type Item_Record is record
      Canvas         : GVD_Canvas;
      Item           : Display_Item;
      Mode           : Display_Mode := Value;
      Format         : Value_Format := Default_Format;
      Zoom           : Guint := 100;
      Component      : Component_Item;
   end record;

   --------------------
   -- Local Packages --
   --------------------

   package Item_Handler is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record, Item_Record);

   ----------------------
   -- Local Procedures --
   ----------------------

   procedure Change_Detect_Aliases (Canvas : access GVD_Canvas_Record'Class);
   --  Callback for the "detect aliases" contextual menu item

   procedure Change_Display_Mode
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record);
   --  Change the mode of a specific item to indicate whether the value of the
   --  item should be displayed.

   procedure Change_Format
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record);
   --  Change the display format of a specific item

   procedure Clone_Component
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record);
   --  Clone the item or its selected component

   procedure Display_Expression (Canvas : access Gtk_Widget_Record'Class);
   --  Popup a dialog to display any expression in the canvas

   procedure Hide_All
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record);
   --  Hide all the subcomponents of the selected item

   procedure Set_Value
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record);
   --  Set the value for a specific component

   procedure Show_All
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record);
   --  Show all the subcomponents of the selected item

   procedure Dereference_All
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record);
   --  Dereference all pointers visible in the box

   procedure View_Into_Memory
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record);
   --  Bring up the memory view if needed, and view the memory at the address
   --  corresponding to Item.

   procedure Update_Variable
     (Widget : access Gtk_Widget_Record'Class;
      Item   : Item_Record);
   --  Callback for the "update value" contextual menu item

   procedure Toggle_Refresh_Mode
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record);
   --  Toggle between "auto_refresh" and "frozen" modes

   procedure On_Data_Refresh (Canvas : access Gtk_Widget_Record'Class);
   --  "Refresh" contextual menu

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference)
   is
      pragma Unreferenced (Kernel);
      Canvas : constant access GVD_Canvas_Record'Class := Self.Canvas;
      Styles : constant access Browser_Styles :=
        Get_Styles (Canvas.Get_View);

      procedure On_Item (Item : not null access Abstract_Item_Record'Class);
      procedure On_Item (Item : not null access Abstract_Item_Record'Class) is
      begin
         Display_Item (Item).Update_Display;
      end On_Item;

      procedure Resize_Item
         (Item : not null access Abstract_Item_Record'Class);
      procedure Resize_Item
         (Item : not null access Abstract_Item_Record'Class)
      is
      begin
         if Item.all in Display_Item_Record'Class then
            Display_Item (Item).Set_Width_Range
              (Max => (Unit_Pixels, Gdouble (Max_Item_Width.Get_Pref)));
            Display_Item (Item).Set_Height_Range
              (Max => (Unit_Pixels, Gdouble (Max_Item_Height.Get_Pref)));
         end if;
      end Resize_Item;

   begin
      Canvas.Modified := Gtk_New
        (Stroke  => Null_RGBA,
         Font    =>
           (Name   => Copy (Styles.Text_Font.Get_Font.Name),
            Color  => Change_Color.Get_Pref,
            others => <>));

      Canvas.Freeze := Gtk_New
        (Stroke  => Styles.Item.Get_Stroke,
         Shadow  => Styles.Item.Get_Shadow,
         Fill    => Create_Rgba_Pattern (Freeze_Bg_Color.Get_Pref));

      Canvas.Get_View.Model.For_Each_Item
        (On_Item'Access, Filter => Kind_Item);
      Canvas.Get_View.Model.Refresh_Layout;  --  resize items and links

      if Pref = null or else Pref = Preference (Detect_Aliases) then
         if Get_Process (Canvas) /= null then
            Change_Detect_Aliases (Canvas);
         end if;
      end if;

      if Pref = null
         or else Pref = Preference (Max_Item_Width)
         or else Pref = Preference (Max_Item_Height)
      then
         Canvas.Get_View.Model.For_Each_Item
           (Resize_Item'Access, Filter => Kind_Item);
      end if;
   end Execute;

   ----------
   -- Save --
   ----------

   overriding procedure Save
     (Property : access GVD_Items_Property_Record;
      Value    : in out GNATCOLL.JSON.JSON_Value)
   is
      use GNATCOLL.JSON;

      Values : JSON_Array;
   begin
      if Property.Items /= null then
         for Item in Property.Items'Range loop
            declare
               Value : constant JSON_Value := Create_Object;
            begin
               Value.Set_Field ("value", Property.Items (Item).all);
               Append (Values, Value);
            end;
         end loop;
      end if;
      Value.Set_Field ("items", Values);
   end Save;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Property : in out GVD_Items_Property_Record;
      Value    : GNATCOLL.JSON.JSON_Value)
   is
      use GNATCOLL.JSON;

      Values : constant JSON_Array := Value.Get ("items");
   begin
      Property.Items := new GNAT.Strings.String_List (1 .. Length (Values));
      for Index in 1 .. Length (Values) loop
         declare
            V : constant JSON_Value := Get (Values, Index);
         begin
            Property.Items (Index) := new String'(V.Get ("value"));
         end;
      end loop;
   end Load;

   ------------------------------
   -- Load_Items_From_Property --
   ------------------------------

   procedure Load_Items_From_Property
     (Process  : access Visual_Debugger_Record'Class;
      Property : GVD_Items_Property_Record'Class) is
   begin
      if Property.Items /= null then
         for Item in reverse Property.Items'Range loop
            Process_User_Command
              (Visual_Debugger (Process), Property.Items (Item).all,
               Mode => GVD.Types.Internal);
         end loop;

         --   ??? Should have saved position in the desktop
         Terminate_Animation (Get_Canvas (Process).Get_View);
         Get_Canvas (Process).Get_View.Scale_To_Fit
           (Duration => 0.3, Max_Scale => 1.0);
      end if;
   end Load_Items_From_Property;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy
     (Property : in out GVD_Items_Property_Record) is
   begin
      Free (Property.Items);
   end Destroy;

   ---------------
   -- On_Attach --
   ---------------

   overriding procedure On_Attach
     (Canvas  : not null access GVD_Canvas_Record;
      Process : not null access Base_Visual_Debugger'Class)
   is
      V        : constant Visual_Debugger := Visual_Debugger (Process);
      Property : GVD_Items_Property_Record;
      Found    : Boolean;
   begin
      Browser_Model (Canvas.Get_View.Model).Clear;
      Canvas.Item_Num := 0;

      if Preserve_State_On_Exit.Get_Pref then
         Get_Property
           (Property,
            Get_Executable (V.Debugger),
            Name  => "debugger_items",
            Found => Found);
         if Found then
            Load_Items_From_Property (V, Property);
         end if;
      end if;
   end On_Attach;

   ------------
   -- Update --
   ------------

   overriding procedure Update (Canvas : not null access GVD_Canvas_Record) is
      V : constant Visual_Debugger := Visual_Debugger (Get_Process (Canvas));
   begin
      if V /= null then
         Recompute_All_Aliases (V);
         Refresh_Data_Window (V);
      end if;
   end Update;

   ----------------
   -- Get_Canvas --
   ----------------

   function Get_Canvas
     (Process : not null access Base_Visual_Debugger'Class)
      return access GVD_Canvas_Record'Class is
   begin
      return GVD_Canvas (Visual_Debugger (Process).Data);
   end Get_Canvas;

   ----------------
   -- Set_Canvas --
   ----------------

   procedure Set_Canvas
     (Process : not null access Base_Visual_Debugger'Class;
      Canvas  : access GVD_Canvas_Record'Class := null)
   is
      V   : constant Visual_Debugger := Visual_Debugger (Process);
      Old : constant GVD_Canvas := Get_Canvas (Process);
      Property : GVD_Items_Property;

      procedure On_Item (Item : not null access Abstract_Item_Record'Class);
      procedure On_Item (Item : not null access Abstract_Item_Record'Class) is
         Cmd : constant String := Get_Graph_Cmd (Display_Item (Item));
      begin
         if Cmd /= "" then
            if Property = null then
               Property := new GVD_Items_Property_Record;
            end if;
            Append (Property.Items, Cmd);
         end if;
      end On_Item;

   begin
      --  Save the currently displayed items, if any

      if Old /= null and then V.Debugger /= null then
         if Preserve_State_On_Exit.Get_Pref then
            Old.Get_View.Model.For_Each_Item
              (On_Item'Access, Filter => Kind_Item);

            if Property = null then
               Remove_Property
                 (Kernel => Old.Kernel,
                  File  => Get_Executable (V.Debugger),
                  Name   => "debugger_items");
            else
               GNATCOLL.Traces.Trace (Me, "Saving debugger canvas properties");
               Set_Property
                 (Kernel     => Old.Kernel,
                  File  => Get_Executable (V.Debugger),
                  Name       => "debugger_items",
                  Property   => Property,
                  Persistent => True);
            end if;
         end if;
      end if;

      --  If we are detaching, clear the old view
      --  ??? Should use On_Detach
      if Old /= null then
         Browser_Model (Old.Get_View.Model).Clear;
      end if;

      V.Data := Abstract_View_Access (Canvas);
   end Set_Canvas;

   -----------------------
   -- Process_Graph_Cmd --
   -----------------------

   procedure Process_Graph_Cmd
     (Process : not null access Visual_Debugger_Record'Class;
      Cmd     : String)
   is
      Matched   : Match_Array (0 .. Graph_Cmd_Max_Paren);
      Item      : Display_Item;
      Index,
      Last      : Positive;
      Enable    : Boolean;
      First     : Natural;
      Link_Name : GNAT.Strings.String_Access;
      Link_From : Display_Item;
      Num       : Integer := -1;
      Link_Num  : Integer := -1;
      Rect      : Model_Rectangle;
      Pos       : Point := No_Position;
      Info      : Item_Info;

   begin
      --  graph (print|display) expression [dependent on display_num]
      --        [link_name name] [at x, y] [num \d+]
      --  graph (print|display) `command` [at x, y] [num \d+]
      --  graph enable display display_num [display_num ...]
      --  graph disable display display_num [display_num ...]
      --  graph undisplay display_num

      Match (Graph_Cmd_Format, Cmd, Matched);

      if Matched (0) /= No_Match then
         Canvas_Views.Attach_To_View
           (Process, Process.Kernel, Create_If_Necessary => True);
         Enable := Cmd (Matched (Graph_Cmd_Type_Paren).First) = 'd'
           or else Cmd (Matched (Graph_Cmd_Type_Paren).First) = 'D';

         --  Do we have any 'dependent on' expression ?
         if Matched (Graph_Cmd_Dependent_Paren) /= No_Match then
            Link_Num := Safe_Value
              (Cmd (Matched (Graph_Cmd_Dependent_Paren).First
                    .. Matched (Graph_Cmd_Dependent_Paren).Last),
               -1);
            if Link_Num /= -1 then
               Link_From := Find_Item (Get_Canvas (Process), Link_Num);
            end if;
         end if;

         --  Do we have any 'link name' expression
         if Matched (Graph_Cmd_Link_Paren) /= No_Match then
            Link_Name := new String'
              (Cmd (Matched (Graph_Cmd_Link_Paren).First
                    .. Matched (Graph_Cmd_Link_Paren).Last));
         end if;

         --  Do we have any 'at' expression
         if Matched (Graph_Cmd_At_Paren) /= No_Match then
            begin
               Pos.X := Gdouble'Value
                 (Cmd (Matched (Graph_Cmd_X_Paren).First
                  .. Matched (Graph_Cmd_X_Paren).Last));
            exception
               when Constraint_Error =>
                  Pos := No_Position;
            end;

            begin
               Pos.Y := Gdouble'Value
                 (Cmd (Matched (Graph_Cmd_Y_Paren).First
                  .. Matched (Graph_Cmd_Y_Paren).Last));
            exception
               when Constraint_Error =>
                  Pos := No_Position;
            end;
         end if;

         --  Do we have any 'num' expression
         if Matched (Graph_Cmd_Num_Paren) /= No_Match then
            Num := Safe_Value
                (Cmd (Matched (Graph_Cmd_Num_Paren).First
                      .. Matched (Graph_Cmd_Num_Paren).Last), -1);
         else
            Num := Get_Next_Item_Num (Process);
         end if;

         --  What do we want to display ?

         if Matched (Graph_Cmd_Expression_Paren) /= No_Match then
            First  := Matched (Graph_Cmd_Expression_Paren).First;
            Last   := Matched (Graph_Cmd_Expression_Paren).Last;
            Info := Wrap_Debugger_Command (Cmd (First .. Last));
         else
            if Matched (Graph_Cmd_Quoted_Paren) /= No_Match then
               First := Matched (Graph_Cmd_Quoted_Paren).First;
               Last  := Matched (Graph_Cmd_Quoted_Paren).Last;
            else
               First := Matched (Graph_Cmd_Variable_Paren).First;
               Last  := Matched (Graph_Cmd_Variable_Paren).Last;
            end if;

            Info := Wrap_Variable (Cmd (First .. Last));
         end if;

         if Link_Name = null then
            Link_Name := new String'(Cmd (First .. Last));
         end if;

         Gtk_New
           (Item,
            Browser        => Get_Canvas (Process),
            Info           => Info,
            Debugger       => Visual_Debugger (Process),
            Auto_Refresh   => Enable,
            Is_Dereference => Link_From /= null,
            Num            => Num);
         Item.Depend_On := Link_Num;

         if Link_From /= null then
            Create_Link (Get_Canvas (Process), Link_From, Item, Link_Name.all);
            Recompute_All_Aliases (Process, Recompute_Values => False);
         end if;

         Free (Link_Name);

         if Item.Position = No_Position then
            if Pos.X /= No_Position.X and then Pos.Y /= No_Position.Y then
               Item.Set_Position (Pos);

            elsif Link_From /= null then
               --  make space for it to the right
               declare
                  L : Items_Lists.List;
               begin
                  L.Append (Abstract_Item (Item));
                  Insert_And_Layout_Items
                    (Get_Canvas (Process).Get_View,
                     Ref       => Link_From,
                     Items     => L,
                     Direction => Right,
                     Duration  => 0.3);
               end;
            else
               Rect := Get_Canvas (Process).Get_View.Model.Bounding_Box;
               Pos.X := Rect.X + Rect.Width + Default_Space_Between_Items;
               Pos.Y := Rect.Y;
               Item.Set_Position (Pos);
            end if;
         end if;

         --  Recompute links
         Get_Canvas (Process).Get_View.Model.Refresh_Layout;
         Get_Canvas (Process).Get_View.Scroll_Into_View
           (Item, Duration => 0.3);

      else
         --  Is this an enable/disable command ?

         Match (Graph_Cmd_Format2, Cmd, Matched);

         if Matched (2) /= No_Match then
            Canvas_Views.Attach_To_View
              (Process, Process.Kernel, Create_If_Necessary => True);

            Index := Matched (2).First;
            Enable := Cmd (Matched (1).First) = 'e'
              or else Cmd (Matched (1).First) = 'E';

            while Index <= Cmd'Last loop
               Last := Index;
               Skip_To_Blank (Cmd, Last);
               Num := Safe_Value (Cmd (Index .. Last - 1), -1);

               if Num /= -1 then
                  Item := Find_Item (Get_Canvas (Process), Num);
                  if Item /= null then
                     --  We update the value when changing from disable to
                     --  enable. No need to do so otherwise since the value is
                     --  already up-to-date.
                     Set_Auto_Refresh
                       (Item,
                        Auto_Refresh => Enable,
                        Update_Value => Enable);
                  end if;
               end if;

               Index := Last + 1;
               Skip_Blanks (Cmd, Index);
            end loop;

         --  Third possible set of commands

         else
            Match (Graph_Cmd_Format3, Cmd, Matched);

            if Matched (1) /= No_Match then
               Canvas_Views.Attach_To_View
                 (Process, Process.Kernel, Create_If_Necessary => True);
               Index := Matched (1).First;

               while Index <= Cmd'Last loop
                  Last := Index;
                  Skip_To_Blank (Cmd, Last);
                  Num := Safe_Value (Cmd (Index .. Last - 1), -1);

                  if Num /= -1 then
                     Item := Find_Item (Get_Canvas (Process), Num);
                     if Item /= null then
                        Get_Canvas (Process).Get_View.Model.Remove
                          (Item);
                     end if;
                  end if;

                  Index := Last + 1;
                  Skip_Blanks (Cmd, Index);
               end loop;
            end if;
         end if;
      end if;

   exception
      when E : Constraint_Error =>
         GNATCOLL.Traces.Trace (Me, E);
         --  Usually because Find_Item returned a null value
         GVD.Trace.Output_Error
           (Process.Kernel, (-" Error while processing: ") & Cmd);
   end Process_Graph_Cmd;

   -------------------------
   -- Refresh_Data_Window --
   -------------------------

   procedure Refresh_Data_Window
     (Debugger : access GVD.Process.Visual_Debugger_Record'Class) is
   begin
      if Debugger.Data /= null then
         Get_Canvas (Debugger).Get_View.Model.Refresh_Layout;
      end if;
   end Refresh_Data_Window;

   ---------------------
   -- On_Data_Refresh --
   ---------------------

   procedure On_Data_Refresh (Canvas : access Gtk_Widget_Record'Class) is
      C    : constant GVD_Canvas := GVD_Canvas (Canvas);

      procedure On_Item (Item : not null access Abstract_Item_Record'Class);
      procedure On_Item (Item : not null access Abstract_Item_Record'Class) is
      begin
         Display_Item (Item).Update;
      end On_Item;

   begin
      C.Get_View.Model.For_Each_Item (On_Item'Access, Filter => Kind_Item);
      C.Get_View.Model.Refresh_Layout;  --  for links
   end On_Data_Refresh;

   -----------------
   -- Create_Menu --
   -----------------

   overriding procedure Create_Menu
     (View    : not null access GVD_Canvas_Record;
      Menu    : not null access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      Sep   : Gtk_Separator_Menu_Item;
   begin
      General_Browser_Record (View.all).Create_Menu (Menu);
      --  inherited

      Gtk_New (Sep);
      Menu.Append (Sep);

      Append_Menu (Menu, View.Kernel, Detect_Aliases);
   end Create_Menu;

   ---------------------------
   -- Change_Detect_Aliases --
   ---------------------------

   procedure Change_Detect_Aliases (Canvas : access GVD_Canvas_Record'Class) is
      V : constant Visual_Debugger := Visual_Debugger (Get_Process (Canvas));
   begin
      Recompute_All_Aliases (V, Recompute_Values => True);
      Canvas.Get_View.Model.Refresh_Layout;  --  for links
   end Change_Detect_Aliases;

   ------------------------
   -- Display_Expression --
   ------------------------

   procedure Display_Expression (Canvas : access Gtk_Widget_Record'Class) is
   begin
      Display_Expression (Get_Process (GVD_Canvas (Canvas)));
   end Display_Expression;

   ------------------------
   -- Get_Detect_Aliases --
   ------------------------

   function Get_Detect_Aliases
     (Process : access GVD.Process.Visual_Debugger_Record'Class)
      return Boolean
   is
      pragma Unreferenced (Process);
   begin
      return Detect_Aliases.Get_Pref;
   end Get_Detect_Aliases;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (Self   : access GVD_Canvas_Record'Class) return Gtk_Widget
   is
      Hook            : access On_Pref_Changed;
   begin
      Browsers.Canvas.Initialize (Self);
      Self.Get_View.Model.Set_Selection_Mode (Selection_Single);

      Setup_Contextual_Menu
        (Kernel          => Self.Kernel,
         Event_On_Widget => Self,
         Context_Func    => Canvas_Contextual_Factory'Access);

      Hook := new On_Pref_Changed;
      Hook.Canvas := GVD_Canvas (Self);
      Preferences_Changed_Hook.Add (Hook, Watch => Self);
      Hook.Execute (Self.Kernel, null);

      return Gtk_Widget (Self.Get_View);
   end Initialize;

   -----------------------
   -- Get_Next_Item_Num --
   -----------------------

   function Get_Next_Item_Num
     (Debugger  : access GVD.Process.Visual_Debugger_Record'Class;
      Candidate : Integer := -1) return Integer is
   begin
      if Candidate > GVD_Canvas (Debugger.Data).Item_Num then
         GVD_Canvas (Debugger.Data).Item_Num := Candidate;
      else
         GVD_Canvas (Debugger.Data).Item_Num :=
           GVD_Canvas (Debugger.Data).Item_Num + 1;
      end if;
      return GVD_Canvas (Debugger.Data).Item_Num;
   end Get_Next_Item_Num;

   -------------------------
   -- Change_Display_Mode --
   -------------------------

   procedure Change_Display_Mode
     (Widget : access Gtk_Widget_Record'Class;
      Item   : Item_Record) is
   begin
      if Get_Active (Gtk_Radio_Menu_Item (Widget))
        and then Item.Item.Info.Mode /= Item.Mode
      then
         Item.Item.Info.Mode := Item.Mode;
         Update (Item.Item);
         Item.Item.Browser.Get_View.Model.Refresh_Layout;
      end if;
   end Change_Display_Mode;

   -------------------
   -- Change_Format --
   -------------------

   procedure Change_Format
     (Widget : access Gtk_Widget_Record'Class;
      Item   : Item_Record) is
   begin
      if Get_Active (Gtk_Radio_Menu_Item (Widget))
        and then Item.Item.Info.Format /= Item.Format
      then
         Item.Item.Info.Format := Item.Format;
         Update (Item.Item);
         Item.Item.Browser.Get_View.Model.Refresh_Layout;
      end if;
   end Change_Format;

   ---------------------
   -- Clone_Component --
   ---------------------

   procedure Clone_Component
     (Widget : access Gtk_Widget_Record'Class;
      Item   : Item_Record)
   is
      pragma Unreferenced (Widget);
   begin
      if Item.Item.Info.Is_A_Variable then
         Process_User_Command
           (Item.Item.Debugger,
            "graph display " & To_String (Item.Component.Name),
            Output_Command => True);
      else
         Process_User_Command
           (Item.Item.Debugger,
            "graph display `" & Item.Item.Info.Name & "`",
            Output_Command => True);
      end if;
   end Clone_Component;

   -------------------------------
   -- Canvas_Contextual_Factory --
   -------------------------------

   procedure Canvas_Contextual_Factory
     (Context : GPS.Kernel.Selection_Context;
      Menu    : Gtk.Menu.Gtk_Menu)
   is
      Canvas  : GVD_Canvas;
      Mitem   : Gtk_Menu_Item;
      Sep     : Gtk_Separator_Menu_Item;
      Check   : Gtk_Check_Menu_Item;
      Details : Canvas_Event_Details;
      Radio   : Gtk_Radio_Menu_Item;
      Submenu : Gtk_Menu;
      Item    : Display_Item;
      Base      : Item_Record :=
        Item_Record'(Canvas         => null,
                     Item           => null,
                     Component      => null,
                     Mode           => Value,
                     Format         => Default_Format,
                     Zoom           => 100);
   begin
      --  ??? Should use actions
      --  ??? Already computed in Build_Context. We should have our own type
      --  of context to cache this information.

      Canvas := GVD_Canvas
        (GPS_MDI_Child
           (Get_MDI (Get_Kernel (Context)).Get_Focus_Child).Get_Actual_Widget);

      Base.Canvas := Canvas;

      if not Has_Browser_Information (Context) then
         return;
      end if;

      Details := Browser_Information (Context);

      if Details.Toplevel_Item = null then
         Gtk_New (Sep);
         Append (Menu, Sep);

         Gtk_New (Mitem, Label => -"Display Expression...");
         Append (Menu, Mitem);
         Widget_Callback.Object_Connect
           (Mitem, Signal_Activate, Display_Expression'Access, Canvas);

         Gtk_New (Mitem, Label => -"Recompute");
         Append (Menu, Mitem);
         Widget_Callback.Object_Connect
           (Mitem, Signal_Activate, On_Data_Refresh'Access, Canvas);

      else
         while Details.Item /= null
           and then Details.Item.all not in Component_Item_Record'Class
         loop
            Details.Item := Details.Item.Parent;
         end loop;

         Item           := Display_Item (Details.Toplevel_Item);
         Base.Item      := Item;
         Base.Component := Component_Item (Details.Item);

         if Item.Info.Is_A_Variable then
            declare
               Component_Name : constant String :=
                 (if Base.Component = null
                  then Item.Info.Name
                  else To_String (Base.Component.Name));
            begin
               Gtk_New (Sep);
               Append (Menu, Sep);

               Gtk_New (Mitem, Label => -"Hide all " & Component_Name);
               Item_Handler.Connect
                 (Mitem, Signal_Activate,
                  Item_Handler.To_Marshaller (Hide_All'Access), Base);
               Append (Menu, Mitem);

               Gtk_New (Mitem, Label => -"Show all " & Component_Name);
               Item_Handler.Connect
                 (Mitem, Signal_Activate,
                  Item_Handler.To_Marshaller (Show_All'Access), Base);
               Append (Menu, Mitem);

               Gtk_New (Mitem, Label => -"Dereference all pointers");
               Item_Handler.Connect
                 (Mitem, Signal_Activate,
                  Item_Handler.To_Marshaller (Dereference_All'Access), Base);
               Append (Menu, Mitem);

               Gtk_New (Sep);
               Append (Menu, Sep);

               --  We can't clone an auto-refreshed item, since it would reuse
               --  the same box.
               if not Item.Info.Auto_Refresh then
                  Gtk_New (Mitem, Label => -"Clone" & " " & Component_Name);
                  Item_Handler.Connect
                    (Mitem, Signal_Activate,
                     Item_Handler.To_Marshaller (Clone_Component'Access),
                     Base);
                  Append (Menu, Mitem);
               end if;

               Gtk_New (Mitem, Label => -"View memory at address of "
                        & Krunch (Component_Name));
               Item_Handler.Connect
                 (Mitem, Signal_Activate,
                  Item_Handler.To_Marshaller (View_Into_Memory'Access), Base);
               Append (Menu, Mitem);

               if Base.Component /= null then
                  Gtk_New (Mitem, -"Set Value of " & Krunch (Component_Name));
                  Item_Handler.Connect
                    (Mitem, Signal_Activate,
                     Item_Handler.To_Marshaller (Set_Value'Access), Base);
                  Append (Menu, Mitem);
               end if;
            end;
         end if;

         --  Updating the value is only interesting when the item is not
         --  auto-refreshed, otherwise the value is already up-to-date.
         if not Item.Info.Auto_Refresh then
            Gtk_New (Mitem, Label => -"Update Value");
            Item_Handler.Connect
              (Mitem, Signal_Activate,
               Item_Handler.To_Marshaller (Update_Variable'Access), Base);
            Append (Menu, Mitem);
         end if;

         if Item.Info.Is_A_Variable then
            Gtk_New (Sep);
            Append (Menu, Sep);

            Gtk_New (Submenu);
            Gtk_New (Mitem, Label => -"Display");
            Set_Submenu (Mitem, Gtk_Widget (Submenu));
            Append (Menu, Mitem);

            Gtk_New (Radio, Widget_SList.Null_List, -"Show Value");
            Set_Active (Radio, Item.Info.Mode = Value);
            Item_Handler.Connect
              (Radio, Signal_Activate,
               Item_Handler.To_Marshaller (Change_Display_Mode'Access),
               Item_Record'(Canvas         => Canvas,
                            Item           => Base.Item,
                            Component      => Base.Component,
                            others         => <>));
            Append (Submenu, Radio);

            Gtk_New (Radio, Get_Group (Radio), -"Show Type");
            Set_Active (Radio, Item.Info.Mode = Type_Only);
            Item_Handler.Connect
              (Radio, Signal_Activate,
               Item_Handler.To_Marshaller (Change_Display_Mode'Access),
               Item_Record'(Canvas         => Canvas,
                            Item           => Base.Item,
                            Component      => Base.Component,
                            Mode           => Type_Only,
                            others         => <>));
            Append (Submenu, Radio);

            Gtk_New (Radio, Get_Group (Radio), -"Show Value + Type");
            Set_Active (Radio, Item.Info.Mode = Type_Value);
            Item_Handler.Connect
              (Radio, Signal_Activate,
               Item_Handler.To_Marshaller (Change_Display_Mode'Access),
               Item_Record'(Canvas         => Canvas,
                            Item           => Base.Item,
                            Component      => Base.Component,
                            Mode           => Type_Value,
                            others         => <>));
            Append (Submenu, Radio);

            Gtk_New (Sep);
            Append (Submenu, Sep);

            Gtk_New (Radio, Widget_SList.Null_List, -"Default");
            Set_Active (Radio, Item.Info.Format = Default_Format);
            Item_Handler.Connect
              (Radio, Signal_Activate,
               Item_Handler.To_Marshaller (Change_Format'Access),
               Item_Record'(Canvas         => Canvas,
                            Item           => Base.Item,
                            Component      => Base.Component,
                            others         => <>));
            Append (Submenu, Radio);

            Gtk_New (Radio, Get_Group (Radio), -"Decimal");
            Set_Active (Radio, Item.Info.Format = Decimal);
            Item_Handler.Connect
              (Radio, Signal_Activate,
               Item_Handler.To_Marshaller (Change_Format'Access),
               Item_Record'(Canvas         => Canvas,
                            Item           => Base.Item,
                            Component      => Base.Component,
                            Format         => Decimal,
                            others         => <>));
            Append (Submenu, Radio);

            Gtk_New (Radio, Get_Group (Radio), -"Hexadecimal");
            Set_Active (Radio, Item.Info.Format = Hexadecimal);
            Item_Handler.Connect
              (Radio, Signal_Activate,
               Item_Handler.To_Marshaller (Change_Format'Access),
               Item_Record'(Canvas         => Canvas,
                            Item           => Base.Item,
                            Component      => Base.Component,
                            Format         => Hexadecimal,
                            others         => <>));
            Append (Submenu, Radio);

            Gtk_New (Radio, Get_Group (Radio), -"Octal");
            Set_Active (Radio, Item.Info.Format = Octal);
            Item_Handler.Connect
              (Radio, Signal_Activate,
               Item_Handler.To_Marshaller (Change_Format'Access),
               Item_Record'(Canvas         => Canvas,
                            Item           => Base.Item,
                            Component      => Base.Component,
                            Format         => Octal,
                            others         => <>));
            Append (Submenu, Radio);

            Gtk_New (Radio, Get_Group (Radio), -"Binary");
            Set_Active (Radio, Item.Info.Format = Binary);
            Item_Handler.Connect
              (Radio, Signal_Activate,
               Item_Handler.To_Marshaller (Change_Format'Access),
               Item_Record'(Canvas         => Canvas,
                            Item           => Base.Item,
                            Component      => Base.Component,
                            Format         => Binary,
                            others         => <>));
            Append (Submenu, Radio);
         end if;

         --  Display a separator

         Gtk_New (Sep);
         Append (Menu, Sep);

         --  Display "Toggle auto-refresh" option

         Gtk_New (Check, "Auto refresh");
         Set_Active (Check, Base.Item.Info.Auto_Refresh);
         Item_Handler.Connect
           (Check, Signal_Activate,
            Item_Handler.To_Marshaller (Toggle_Refresh_Mode'Access), Base);
         Append (Menu, Check);
      end if;
   end Canvas_Contextual_Factory;

   --------------
   -- Hide_All --
   --------------

   procedure Hide_All
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record)
   is
      pragma Unreferenced (Widget);
      procedure On_Child (C : not null access Container_Item_Record'Class);
      procedure On_Child (C : not null access Container_Item_Record'Class) is
      begin
         if C.all in Component_Item_Record'Class then
            Set_Visibility
              (Component_Item (C).Component, False, Recursive => True);
         end if;
      end On_Child;
   begin
      if Item.Component = null then
         Item.Item.For_Each_Child (On_Child'Access);
      else
         Set_Visibility (Item.Component.Component, False, Recursive => True);
      end if;
      Update_Display (Item.Item);
      Item.Canvas.Get_View.Model.Refresh_Layout;
      --  resize items and links
   end Hide_All;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record)
   is
      Name : constant String := To_String (Item.Component.Name);
      S : constant String :=
        Display_Text_Input_Dialog
                 (Kernel   => Item.Item.Debugger.Kernel,
                  Title    => -"Setting value of " & Name,
                  Message  => -"Setting value of " & Name & ':',
                  Key      => "gvd_set_value_dialog");

   begin
      if S /= "" and then S (S'First) /= ASCII.NUL then
         Set_Variable (Item.Item.Debugger.Debugger, Name, S);
         Update_Variable (Widget, Item);
      end if;
   end Set_Value;

   --------------
   -- Show_All --
   --------------

   procedure Show_All
     (Widget : access Gtk_Widget_Record'Class;
      Item   : Item_Record)
   is
      pragma Unreferenced (Widget);
      procedure On_Child (C : not null access Container_Item_Record'Class);
      procedure On_Child (C : not null access Container_Item_Record'Class) is
      begin
         if C.all in Component_Item_Record'Class then
            Set_Visibility
              (Component_Item (C).Component, True, Recursive => True);
         end if;
      end On_Child;
   begin
      if Item.Component = null then
         Item.Item.For_Each_Child (On_Child'Access);
      else
         Set_Visibility (Item.Component.Component, True, Recursive => True);
      end if;
      Item.Item.Update_Display;

      --  resize items and links
      Item.Canvas.Get_View.Model.Refresh_Layout;
   end Show_All;

   ---------------------
   -- Dereference_All --
   ---------------------

   procedure Dereference_All
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record)
   is
      pragma Unreferenced (Widget);

      procedure On_Child (Child : not null access Container_Item_Record'Class);
      procedure On_Child
        (Child : not null access Container_Item_Record'Class)
      is
      begin
         if Child.all in Component_Item_Record'Class
           and then Component_Item (Child).Component.all in Access_Type'Class
         then
            Dereference_Item (Component => Component_Item (Child));
         end if;
      end On_Child;

   begin
      Item.Item.For_Each_Child (On_Child'Access, Recursive => True);
   end Dereference_All;

   ----------------------
   -- View_Into_Memory --
   ----------------------

   procedure View_Into_Memory
     (Widget : access Gtk_Widget_Record'Class;
      Item   : Item_Record)
   is
      pragma Unreferenced (Widget);
   begin
      GVD.Memory_View.Display_Memory
        (Kernel  => Item.Canvas.Kernel,
         Address =>
           (if Item.Component = null
            then Item.Item.Info.Name
            else To_String (Item.Component.Name)));
   end View_Into_Memory;

   ---------------------
   -- Update_Variable --
   ---------------------

   procedure Update_Variable
     (Widget : access Gtk_Widget_Record'Class;
      Item   : Item_Record)
   is
      pragma Unreferenced (Widget);
   begin
      Item.Item.Update;
   end Update_Variable;

   -------------------------
   -- Toggle_Refresh_Mode --
   -------------------------

   procedure Toggle_Refresh_Mode
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record)
   is
      pragma Unreferenced (Widget);
   begin
      Set_Auto_Refresh
        (Item.Item,
         not Item.Item.Info.Auto_Refresh,
         True);
   end Toggle_Refresh_Mode;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : On_Command;
      Kernel  : not null access Kernel_Handle_Record'Class;
      Process : access GPS.Debuggers.Base_Visual_Debugger'Class;
      Command : String) return String
   is
      pragma Unreferenced (Self, Kernel);
   begin
      if Process /= null and then Starts_With (Command, "graph ") then
         Process_Graph_Cmd (Visual_Debugger (Process), Command);
         return Command_Intercepted;  --  command was processed
      else
         return "";  --  command does not apply to the canvas
      end if;
   end Execute;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Access_Filter : Action_Filter;
      Filter        : Action_Filter;
      Command       : Interactive_Command_Access;
   begin
      Detect_Aliases := Kernel.Get_Preferences.Create_Invisible_Pref
        ("gvd-detect-aliases", True,
         Label => -"Detect aliases",
         Doc =>
           -("When two variables are at the same location in memory, a single"
           & " it , a single box is displayed if alias detection is enabled.")
        );

      Debugger_Command_Action_Hook.Add (new On_Command);

      Canvas_Views.Register_Module (Kernel);
      Canvas_Views.Register_Open_View_Action
        (Kernel,
         Action_Name => "open debugger data window",
         Description => -"Open the Data Window for the debugger");

      Access_Filter     := new Access_Variable_Filter;

      Filter := Kernel.Lookup_Filter ("Debugger stopped")
        and Kernel.Lookup_Filter ("Debugger printable variable");

      Command := new Print_Variable_Command;
      Print_Variable_Command (Command.all).Display := True;
      Register_Action
        (Kernel, "debug display variable",
         Command     => Command,
         Description =>
           "Display the value of the variable in the debugger data window,"
         & " so that it is displayed again every time the debugger stops",
         Filter      => Filter,
         Category    => -"Debug");
      Register_Contextual_Menu
        (Kernel => Kernel,
         Label  => -"Debug/Display %S in Graph",
         Action => "debug display variable",
         Group  => GVD_Canvas_Contextual_Menus_Group);

      Command := new Print_Variable_Command;
      Print_Variable_Command (Command.all).Display := True;
      Print_Variable_Command (Command.all).Dereference := True;
      Register_Action
        (Kernel, "debug display dereferenced variable",
         Command     => Command,
         Description =>
           "Display the value pointed to by the variable in the debugger"
         & " data window, so that it is displayed again every time the"
         & "  debugger stops",
         Filter      => Filter and Access_Filter,
         Category    => -"Debug");
      Register_Contextual_Menu
        (Kernel => Kernel,
         Label  => -"Debug/Display %C in Graph",
         Custom => Custom_Label_Expansion'Access,
         Action => "debug display dereferenced variable",
         Group  => GVD_Canvas_Contextual_Menus_Group);

      Command := new Print_Variable_Command;
      Register_Action
        (Kernel, "debug print variable",
         Command     => Command,
         Description =>
           "Print the value of the variable in the debugger console",
         Filter      => Filter,
         Category    => -"Debug");
      Register_Contextual_Menu
        (Kernel => Kernel,
         Label  => -"Debug/Print %S",
         Action => "debug print variable",
         Group  => GVD_Canvas_Contextual_Menus_Group);

      Command := new Print_Variable_Command;
      Print_Variable_Command (Command.all).Dereference := True;
      Register_Action
        (Kernel, "debug print dereferenced variable",
         Command     => Command,
         Description =>
           "Print the value pointed to by the variable in the debugger"
           & " console",
         Filter    => Filter,
         Category  => "Debug");
      Register_Contextual_Menu
        (Kernel => Kernel,
         Label  => "Debug/Print %C",
         Custom => Custom_Label_Expansion'Access,
         Action => "debug print dereferenced variable",
         Group  => GVD_Canvas_Contextual_Menus_Group);

      Filter := Kernel.Lookup_Filter ("Debugger stopped");

      Register_Action
        (Kernel, "debug display local variables", new Local_Vars_Command,
         Filter      => Filter,
         Description => -"Display local variables in the data window",
         Category    => -"Debug");

      Register_Action
        (Kernel, "debug display arguments", new Arguments_Command,
         Filter      => Filter,
         Description => -"Display arguments to the current subprogram",
         Category    => -"Debug");

      Register_Action
        (Kernel, "debug display registers", new Registers_Command,
         Description => -"Display the contents of registers in data window",
         Filter      => Filter,
         Category    => -"Debug");

      Register_Action
        (Kernel, "Debug display any expression", new Expression_Command,
         Description => -"Opens a dialog to choose an expression to display",
         Filter      => Filter,
         Category    => -"Debug");
   end Register_Module;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Item           : out Display_Item;
      Browser        : not null access Debugger_Data_View_Record'Class;
      Info           : Item_Info;
      Num            : Integer;
      Debugger       : access Visual_Debugger_Record'Class;
      Auto_Refresh   : Boolean := True;
      Is_Dereference : Boolean := False)
   is
      Styles : constant access Browser_Styles := Browser.Get_View.Get_Styles;
      Alias_Item : Display_Item;
   begin
      Item                := new Display_Item_Record;
      Item.Browser        := General_Browser (Browser);
      Item.Info           := Info;
      Item.Num            := Num;
      Item.Debugger       := Visual_Debugger (Debugger);
      Item.Info.Auto_Refresh := Auto_Refresh;
      Item.Is_Dereference := Is_Dereference;

      --  We need the information on the type, so that we detect aliases only
      --  for structurally equivalent types. If we have an error at this level,
      --  the variable might not be known yet, and we will simply try to
      --  refresh over and over again until we can parse the type

      Item.Info.Update (Debugger);

      --  If an auto-updated similar item is on the canvas, we simply show
      --  and select it.

      if Item.Info.Entity /= null
        and then Item.Info.Is_A_Variable
      then
         Item.Id := new String'
           (Get_Uniq_Id (Debugger.Debugger, To_String (Item.Info.Varname)));

         if Auto_Refresh then
            --  Avoid creating the same item twice if it already exists in
            --  the canvas

            Alias_Item := Search_Item
              (Debugger, Item.Id.all, To_String (Item.Info.Varname));

            --  Two structures are aliased only if they have the same
            --  address and the same structure. The latter is to handle
            --  cases like "struct A {struct B {int field}} where A, B and
            --  field have the same address and would be considered as
            --  aliases otherwise.
            if Alias_Item /= null
              and then
                (not Typed_Aliases
                 or else Structurally_Equivalent
                   (Alias_Item.Info.Entity, Item.Info.Entity))
            then
               Browser.Get_View.Model.Add_To_Selection (Alias_Item);
               Browser.Get_View.Scroll_Into_View
                 (Alias_Item, Duration => 0.3);
               Destroy (Item, In_Model => Browser.Get_View.Model);
               Item := Alias_Item;
               return;
            end if;
         end if;
      end if;

      Item.Initialize_Rect (Styles.Item);
      Browser_Model (Browser.Get_View.Model).Add (Item);
      Item.Set_Position (No_Position);

      Item.Update_Display;

      Item.Set_Width_Range
        (Max => (Unit_Pixels, Gdouble (Max_Item_Width.Get_Pref)));
      Item.Set_Height_Range
        (Max => (Unit_Pixels, Gdouble (Max_Item_Height.Get_Pref)));

      if Get_Detect_Aliases (Debugger) then
         Recompute_All_Aliases (Debugger, False);
      end if;
   end Gtk_New;

   --------------------
   -- Update_Display --
   --------------------

   procedure Update_Display
     (Item : not null access Display_Item_Record'Class)
   is
      Close   : Close_Button;
   begin
      Item.Clear (In_Model => Item.Browser.Get_View.Model);

      if Item.Info.Auto_Refresh then
         Item.Set_Style (Item.Browser.Get_View.Get_Styles.Item);
      else
         Item.Set_Style (Debugger_Data_View (Item.Browser).Freeze);
      end if;

      Gtk_New (Close);
      Item.Setup_Titlebar
        (Browser => Item.Browser,
         Name    => Integer'Image (Item.Num) & ": " & Item.Info.Name,
         Buttons => (1  => Close));

      if Item.Info.Entity /= null
         and then Item.Info.Entity.Is_Valid
      then
         Item.Add_Child
           (Item.Info.Entity.Build_Display
              (Item.Info.Name,
               Debugger_Data_View (Item.Browser),
               Get_Language (Item.Debugger.Debugger), Item.Info.Mode));
      end if;
   end Update_Display;

   -------------------
   -- Get_Graph_Cmd --
   -------------------

   function Get_Graph_Cmd
     (Item : access Display_Item_Record'Class) return String
   is
      Rect   : constant Point := Item.Position;
      Prefix : constant String :=
        "graph " & (if Item.Info.Auto_Refresh then "display " else "print ");
      Suffix : constant String :=
        (if Item.Depend_On /= -1 then " dependent on" & Item.Depend_On'Img
         else "")
        & " at" & Gdouble'Image (Rect.X)
        & "," & Gdouble'Image (Rect.Y)
        & " num" & Integer'Image (Item.Num);
   begin
      if Item.Info.Cmd /= "" then
         return Prefix & "`" & To_String (Item.Info.Cmd) & "`" & Suffix;
      else
         return Prefix & To_String (Item.Info.Varname) & Suffix;
      end if;
   end Get_Graph_Cmd;

   -----------------
   -- Is_Alias_Of --
   -----------------

   function Is_Alias_Of
     (Item       : access Display_Item_Record'Class;
      Id         : String;
      Name       : String;
      Deref_Name : String) return Boolean is
   begin
      --  Do not detect aliases that are already aliases, so as to
      --  avoid chains of aliases.
      --  Note also that X.all can not be an alias of X, so as to properly
      --  display string parameters in Ada (they appear otherwise as access
      --  types, which, once dereferenced, would point to themselves).

      return Item.Id /= null
        and then Item.Info.Auto_Refresh
        and then Item.Id.all = Id
        and then Item.Info.Name /= Deref_Name
        and then Name /= Dereference_Name
          (Get_Language (Item.Debugger.Debugger), Item.Info.Name);
   end Is_Alias_Of;

   ---------------
   -- Find_Item --
   ---------------

   function Find_Item
     (Canvas : not null access Debugger_Data_View_Record'Class;
      Num    : Integer) return Display_Item
   is
      Found : Display_Item;

      procedure On_Item (Item : not null access Abstract_Item_Record'Class);
      procedure On_Item (Item : not null access Abstract_Item_Record'Class) is
      begin
         if Display_Item (Item).Num = Num then
            Found := Display_Item (Item);
         end if;
      end On_Item;
   begin
      Canvas.Get_View.Model.For_Each_Item
        (On_Item'Access, Filter => Kind_Item);
      return Found;
   end Find_Item;

   -----------------
   -- Search_Item --
   -----------------

   function Search_Item
     (Process : access Visual_Debugger_Record'Class;
      Id      : String;
      Name    : String) return Display_Item
   is
      Alias_Item : Display_Item := null;
      Deref_Name : constant String := Dereference_Name
        (Get_Language (Process.Debugger), Name);

      procedure On_Item (Item : not null access Abstract_Item_Record'Class);
      procedure On_Item (Item : not null access Abstract_Item_Record'Class) is
         It : constant Display_Item := Display_Item (Item);
      begin
         if Alias_Item = null  --  not found yet
           and then (Name = "" or else It.Info.Name = Name)
           and then Is_Alias_Of (It, Id, Name, Deref_Name)
         then
            if It.Is_Alias_Of /= null then
               Alias_Item := It.Is_Alias_Of;
            else
               Alias_Item := It;
            end if;
         end if;
      end On_Item;

   begin
      --  Always search if we have a special name to look for, so as to avoid
      --  creating the same item multiple times
      if Name /= "" or else Get_Detect_Aliases (Process) then
         Get_Canvas (Process).Get_View.Model.For_Each_Item
           (On_Item'Access, Filter => Kind_Item);
      end if;
      return Alias_Item;
   end Search_Item;

   ------------
   -- Update --
   ------------

   procedure Update (Item : not null access Display_Item_Record'Class) is
   begin
      Item.Info.Update (Item.Debugger);
      Item.Update_Display;
   end Update;

   -----------------
   -- Create_Link --
   -----------------

   procedure Create_Link
     (Browser    : not null access Debugger_Data_View_Record'Class;
      From, To   : access Display_Item_Record'Class;
      Name       : String;
      Alias_Link : Boolean := False)
   is
      Styles : constant access Browser_Styles := Get_Styles (Browser.Get_View);
      L : GVD_Link;
   begin
      if not Browser.Has_Link (From, To) then
         L := new GVD_Link_Record;
         L.Alias_Link := Alias_Link;
         L.Default_Style := (if Alias_Link then Styles.Link2 else Styles.Link);
         L.Name := To_Unbounded_String (Name);
         L.Initialize
           (From     => From,
            To       => To,
            Style    => L.Default_Style,
            Routing  => Curve,
            Label    => Gtk_New_Text (Styles.Label, Name));
         Browser_Model (Browser.Get_View.Model).Add (L);
      end if;
   end Create_Link;

   ----------------------
   -- Dereference_Item --
   ----------------------

   procedure Dereference_Item
     (Component : not null access Component_Item_Record'Class)
   is
      Item : constant Display_Item :=
        Display_Item (Component.Get_Toplevel_Item);
      Link_Name : constant String := To_String (Component.Name);
      New_Name  : constant String :=
        Dereference_Name
          (Get_Language (Item.Debugger.Debugger),
           To_String (Component.Name));
      Link      : constant String :=
        Dereference_Name
          (Get_Language (Item.Debugger.Debugger), Link_Name);

   begin
      --  The newly created item should have the same auto-refresh state as
      --  the one we are dereferencing

      if Item.Info.Auto_Refresh then
         Process_User_Command
           (Item.Debugger,
            "graph display """ & New_Name & """ dependent on"
            & Integer'Image (Item.Num) & " link_name " & Link,
            Output_Command => True);
      else
         Process_User_Command
           (Item.Debugger,
            "graph print """ & New_Name & """ dependent on"
            & Integer'Image (Item.Num) & " link_name " & Link,
            Output_Command => True);
      end if;
   end Dereference_Item;

   -----------------------
   -- Change_Visibility --
   -----------------------

   procedure Change_Visibility
     (Item      : not null access Gtkada.Canvas_View.Canvas_Item_Record'Class;
      Component : not null access Generic_Type'Class)
   is
      It : constant Display_Item := Display_Item (Item.Get_Toplevel_Item);
   begin
      Component.Set_Visibility (not Component.Get_Visibility);
      It.Update_Display;
      Get_Canvas (It.Debugger).Get_View.Model.Refresh_Layout;  --  links
   end Change_Visibility;

   ----------------------
   -- Set_Auto_Refresh --
   ----------------------

   procedure Set_Auto_Refresh
     (Item         : access Display_Item_Record'Class;
      Auto_Refresh : Boolean;
      Update_Value : Boolean := False)
   is
   begin
      Item.Info.Auto_Refresh := Auto_Refresh;

      if Update_Value then
         --  If we moved back to the auto-refresh state, force an
         --  update of the value.

         Item.Info.Mark_As_Up_To_Date;
         Update (Item);
      end if;

      Item.Browser.Get_View.Model.Refresh_Layout;  --  for links
   end Set_Auto_Refresh;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy
     (Self     : not null access Display_Item_Record;
      In_Model : not null access Canvas_Model_Record'Class)
   is
      To_Remove : Item_Sets.Set;

      procedure On_Item (Item : not null access Abstract_Item_Record'Class);
      procedure On_Item (Item : not null access Abstract_Item_Record'Class) is
         It : constant Display_Item := Display_Item (Item);
      begin
         if It.Is_Alias_Of = Display_Item (Self) then
            It.Is_Alias_Of := null;
            Include_Related_Items (In_Model, It, To_Remove);
         end if;
      end On_Item;

   begin
      --  Remove all items that are aliases of Self (since they are currently
      --  invisible, the user means to close them at the same time).

      In_Model.For_Each_Item (On_Item'Access, Filter => Kind_Item);
      In_Model.Remove (To_Remove);
      Self.Info.Free;

      GNAT.Strings.Free (Self.Id);

      GPS_Item_Record (Self.all).Destroy (In_Model);  --  inherited
   end Destroy;

   ---------------------------
   -- Recompute_All_Aliases --
   ---------------------------

   procedure Recompute_All_Aliases
     (Process          : access Visual_Debugger_Record'Class;
      Recompute_Values : Boolean := True)
   is
      C : constant Debugger_Data_View := Get_Canvas (Process);

      procedure Remove_Temporary
        (Item : not null access Abstract_Item_Record'Class);
      --  Remove all temporary links from the canvas

      procedure Recompute_Address
        (Item : not null access Abstract_Item_Record'Class);
      --  Recompute the address of the item, and identify which ones should be
      --  displayed and which ones should be hidden as aliases

      procedure Build_Aliases
        (Item : not null access Abstract_Item_Record'Class);
      --  Using the result of Recompute_Address, compute which items should be
      --  aliases

      procedure Make_Visible
        (Item : not null access Abstract_Item_Record'Class);
      --  Make the item visible

      procedure Update_Value
        (Item : not null access Abstract_Item_Record'Class);
      --  Update the value of a specific item in the canvas. The new value is
      --  read from the debugger, parsed, and redisplayed.
      --  Do nothing if the auto-refresh status of Item is set to false.

      To_Remove : Item_Sets.Set;
      To_Hide   : Item_Sets.Set;

      Addresses : String_To_Items.Map;
      --  Maps addresses to items, to identify aliases

      ------------------
      -- Make_Visible --
      ------------------

      procedure Make_Visible
        (Item : not null access Abstract_Item_Record'Class) is
      begin
         Item.Show;
      end Make_Visible;

      ----------------------
      -- Remove_Temporary --
      ----------------------

      procedure Remove_Temporary
        (Item : not null access Abstract_Item_Record'Class)
      is
         Link : constant GVD_Link := GVD_Link (Item);
      begin
         if Link.Alias_Link then
            To_Remove.Include (Abstract_Item (Item));
         else
            Link.Show;  --  in case it was hidden before
         end if;
      end Remove_Temporary;

      -----------------------
      -- Recompute_Address --
      -----------------------

      procedure Recompute_Address
        (Item : not null access Abstract_Item_Record'Class)
      is
         It            : constant Display_Item := Display_Item (Item);
         Keeper : Display_Item;
      begin
         It.Show;
         It.Was_Alias := It.Is_Alias_Of /= null;
         It.Is_Alias_Of := null;

         --  If this is not an item associated with a variable, ignore it
         --  Only detect aliases if we have an auto_refresh item
         if not It.Info.Has_Address then
            return;
         end if;

         --  Else recompute the id for the item
         GNAT.Strings.Free (It.Id);

         declare
            Id : constant String :=
              Get_Uniq_Id (It.Debugger.Debugger, To_String (It.Info.Varname));
         begin
            if Id /= "" then
               It.Id := new String'(Id);

               if Addresses.Contains (Id) then
                  --  We want to hide dereferences when possible, and keep all
                  --  items explicitly display
                  Keeper := Addresses.Element (Id);
                  if Keeper.Is_Dereference and then not It.Is_Dereference then
                     Addresses.Include (Id, It);
                  end if;
               else
                  Addresses.Include (Id, It);
               end if;
            end if;
         end;
      end Recompute_Address;

      -------------------
      -- Build_Aliases --
      -------------------

      procedure Build_Aliases
        (Item : not null access Abstract_Item_Record'Class)
      is
         It : constant Display_Item := Display_Item (Item);
         Keeper : Display_Item;
         S      : Item_Sets.Set;

         procedure Duplicate_Links
           (Link : not null access Abstract_Item_Record'Class);
         --  Duplicate the links that go to an item that is being hidden

         procedure Duplicate_Links
           (Link : not null access Abstract_Item_Record'Class)
         is
            Src, Dest : Display_Item;
            Replace   : Boolean;
         begin
            if not GVD_Link (Link).Alias_Link then
               Src := Display_Item (GVD_Link (Link).Get_From);
               Dest := Display_Item (GVD_Link (Link).Get_To);
               Replace := False;

               if Src = It then
                  Src := It.Is_Alias_Of;
                  Replace := True;
               elsif Dest = It then
                  Dest := It.Is_Alias_Of;
                  Replace := True;
               end if;

               if Replace then
                  Create_Link
                    (C, Src, Dest,
                     Name       => To_String (GVD_Link (Link).Name),
                     Alias_Link => True);
               end if;
            end if;
         end Duplicate_Links;

         List : Items_Lists.List;
      begin
         if It.Id /= null and then Addresses.Contains (It.Id.all) then
            Keeper := Addresses.Element (It.Id.all);
            if It = Keeper then
               null;  --  nothing to do

            elsif It.Is_Dereference then
               C.Get_View.Model.Include_Related_Items (It, To_Hide);
               It.Is_Alias_Of := Keeper;

               S.Include (Abstract_Item (It));
               C.Get_View.Model.For_Each_Link
                 (Duplicate_Links'Access,
                  From_Or_To => S);

            else
               --  not a dereference, so Keeper is not a dereference either
               Create_Link (C, It, Keeper, "<=>", Alias_Link => True);
            end if;
         end if;

         --  If we broke the alias, move the item back to some new coordinates
         if It.Is_Alias_Of = null
           and then It.Was_Alias
         then
            List.Append (Abstract_Item (It));
            Insert_And_Layout_Items
              (C.Get_View,
               Ref       => It,
               Items     => List,
               Direction => Right,
               Duration  => 0.3);
         end if;
      end Build_Aliases;

      ------------------
      -- Update_Value --
      ------------------

      procedure Update_Value
        (Item : not null access Abstract_Item_Record'Class)
      is
         It : constant Display_Item := Display_Item (Item);
      begin
         if It.Info.Auto_Refresh and then It.Is_Alias_Of = null then
            Update (It);
         end if;
      end Update_Value;

   begin
      C.Get_View.Model.For_Each_Item
        (Remove_Temporary'Access, Filter => Kind_Link);
      C.Get_View.Model.Remove (To_Remove);
      To_Remove.Clear;

      --  First: Recompile all the addresses, and detect the aliases
      if Get_Detect_Aliases (Process) then
         C.Get_View.Model.For_Each_Item
           (Recompute_Address'Access, Filter => Kind_Item);
         C.Get_View.Model.For_Each_Item
           (Build_Aliases'Access, Filter => Kind_Item);
      else
         C.Get_View.Model.For_Each_Item
           (Make_Visible'Access, Filter => Kind_Item);
      end if;

      for L of To_Hide loop
         L.Hide;
      end loop;

      --  Then re-parse the value of each item and display them again
      if Recompute_Values then
         C.Get_View.Model.For_Each_Item
           (Update_Value'Access, Filter => Kind_Item);
      end if;
   end Recompute_All_Aliases;

   -----------------
   -- Set_Process --
   -----------------

   overriding procedure Set_Process
     (Self    : not null access GVD_Canvas_Record;
      Process : access Base_Visual_Debugger'Class) is
   begin
      Self.Process := Process;
   end Set_Process;

end GVD.Canvas;
