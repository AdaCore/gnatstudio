------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2012, AdaCore                     --
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

with Gdk.Pixbuf;              use Gdk.Pixbuf;
with Gdk.Color;               use Gdk.Color;
with Gdk.Event;               use Gdk.Event;
with Gdk.Window;              use Gdk.Window;
with Gdk;                     use Gdk;
with Glib;                    use Glib;
with Glib.Object;             use Glib.Object;
with Gtk.Check_Menu_Item;     use Gtk.Check_Menu_Item;
with Gtk.Enums;               use Gtk.Enums;
with Gtk.Handlers;            use Gtk.Handlers;
with Gtk.Menu;                use Gtk.Menu;
with Gtk.Menu_Item;           use Gtk.Menu_Item;
with Gtk.Radio_Menu_Item;     use Gtk.Radio_Menu_Item;
with Gtk.Separator_Menu_Item; use Gtk.Separator_Menu_Item;
with Gtk.Widget;              use Gtk.Widget;
with Gtkada.Canvas;           use Gtkada.Canvas;
with Gtkada.Handlers;         use Gtkada.Handlers;
with Gtkada.MDI;              use Gtkada.MDI;
with Pango.Enums;             use Pango.Enums;
with Pango.Font;              use Pango.Font;
with Pango.Layout;            use Pango.Layout;

with Browsers.Canvas;        use Browsers.Canvas;
with Debugger;               use Debugger;
with Display_Items;          use Display_Items;
with GNAT.Regpat;            use GNAT.Regpat;
with GNAT.Strings;           use GNAT.Strings;
with GNATCOLL.Utils;         use GNATCOLL.Utils;
with GPS.Intl;               use GPS.Intl;
with GPS.Properties;         use GPS.Properties;
with GPS.Kernel;             use GPS.Kernel;
with GPS.Kernel.Hooks;       use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;         use GPS.Kernel.MDI;
with GPS.Kernel.Modules;     use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;  use GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences; use GPS.Kernel.Preferences;
with GPS.Kernel.Properties;  use GPS.Kernel.Properties;
with GPS.Main_Window;        use GPS.Main_Window;
with GVD.Generic_View;
with GVD.Memory_View;        use GVD.Memory_View;
with GVD.Menu;               use GVD.Menu;
with GVD_Module;             use GVD_Module;
with GVD.Preferences;        use GVD.Preferences;
with GVD.Process;            use GVD.Process;
with GVD.Scripts;            use GVD.Scripts;
with GVD.Trace;
with GVD.Types;
with Items;                  use Items;
with Items.Simples;          use Items.Simples;
with Language;               use Language;
with Pixmaps_IDE;            use Pixmaps_IDE;
with Std_Dialogs;            use Std_Dialogs;
with String_Utils;           use String_Utils;
with Traces;                 use Traces;
with XML_Utils;              use XML_Utils;

package body GVD.Canvas is
   Me : constant Debug_Handle := Create ("Canvas");

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
      & "(\s+at\s+(\d+),\s*(\d+))?"       --  parenthesis 11 .. 13
      & "(\s+num\s+(\d+))?"               --  parenthesis 14 .. 15
      & "(\s+alias_of\s+\d+)?",           --  parenthesis 16
      Case_Insensitive);
   --  Format of the graph print commands, and how to parse them

   Graph_Cmd_Max_Paren           : constant := 15;
   Graph_Cmd_Cmd_Paren           : constant := 1;
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

   package Check_Canvas_Handler is new Gtk.Handlers.User_Callback
     (Gtk_Check_Menu_Item_Record, General_Browser);

   package Browser_Views is new GVD.Generic_View
     (Base_Type                     => General_Browser_Record,
      Base_Type_Access              => General_Browser,
      Visual_Debugger_Record        => GVD.Process.Visual_Debugger_Record,
      Visual_Debugger               => GVD.Process.Visual_Debugger);

   type GVD_Canvas_Record is new Browser_Views.Process_View_Record with
      record
         Detect_Aliases : Boolean;
         Item_Num       : Integer := 0;

         --  The graphic contexts used to draw the canvas and its items
         Item_Context    : Items.Drawing_Context;
         Box_Context     : Box_Drawing_Context;
         Tooltip_Context : Items.Drawing_Context;

         Selected_Item           : Browser_Item := null;
         Selected_Component      : Items.Generic_Type_Access := null;
      end record;
   type GVD_Canvas is access all GVD_Canvas_Record'Class;

   type Preferences_Hook_Record is new Function_No_Args with record
      Canvas : GVD_Canvas;
   end record;
   type Preferences_Hook is access all Preferences_Hook_Record'Class;
   overriding procedure Execute
     (Hook   : Preferences_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class);
   --  Called when the preferences have changed, to refresh the GVD canvas
   --  appropriately.

   overriding procedure On_Attach
     (Canvas  : access GVD_Canvas_Record;
      Process : access Visual_Debugger_Record'Class);
   overriding procedure Update (Canvas : access GVD_Canvas_Record);
   --  Called when the canvas needs refreshing

   function Initialize
     (Canvas : access GVD_Canvas_Record'Class;
      Kernel : access Kernel_Handle_Record'Class) return Gtk_Widget;
   --  Create a new data window in the MDI

   function Get_Canvas
     (Process : access Visual_Debugger_Record'Class)
      return General_Browser;
   procedure Set_Canvas
     (Process : access Visual_Debugger_Record'Class;
      Canvas  : General_Browser);
   --  Get or set the data window associated with Process

   package Canvas_Views is new Browser_Views.Simple_Views
     (Module_Name        => "Debugger_Data",
      View_Name          => "Debugger Data",
      Formal_View_Record => GVD_Canvas_Record,
      Get_View           => Get_Canvas,
      Set_View           => Set_Canvas,
      Group              => Group_Graphs,
      Position           => Position_Top,
      Initialize         => Initialize);

   procedure On_Realize (Canvas : access Gtk_Widget_Record'Class);
   --  Initializes all the internal graphic contexts needed for the canvas.
   --  The canvas should have been realized before calling this procedure.

   procedure Set_Detect_Aliases
     (Canvas   : access GVD_Canvas_Record'Class;
      Activate : Boolean);
   --  Change the status of aliases detection in the canvas

   procedure On_Data_Window
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Data->Data Window

   procedure Preferences_Changed
     (Canvas  : access GVD_Canvas_Record'Class);
   --  Called when the preferences have changed, and the canvas should be
   --  redisplayed with the new setup.

   procedure Initialize_GC (Canvas : access GVD_Canvas_Record'Class);
   --  Initialize the graphic contexts based on the preferences

   function Get_Next_Item_Num
     (Debugger  : access GVD.Process.Visual_Debugger_Record'Class;
      Candidate : Integer := -1) return Integer;
   --  Return the number that should be used for the next item inserted into
   --  the canvas.
   --  Two successive calls to that function will not return the same value.
   --  Candidate is a suggested candidate, and will be returned if no
   --  other item matches that number. It is ignored if unspecified

   procedure GVD_Canvas_Context_Factory
     (Context      : in out GPS.Kernel.Selection_Context;
      Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu);
   --  Build the context and contextual menu when right clicking in the canvas

   procedure Item_Contextual_Menu
     (Menu           : Gtk.Menu.Gtk_Menu;
      Debugger       : access GVD.Process.Visual_Debugger_Record'Class;
      Item           : access Display_Items.Display_Item_Record'Class;
      Component      : Items.Generic_Type_Access;
      Component_Name : String);
   --  Fill the contextual menu when clicking on an item.
   --  Note that Component can be null if the user has clicked for instance
   --  on the title bar.

   procedure Unselect_All
     (Canvas  : access GVD_Canvas_Record'Class);
   --  Unselect all selected elements

   procedure On_Background_Click
     (Canvas : access Glib.Object.GObject_Record'Class;
      Event  : Gdk.Event.Gdk_Event);
   --  Handles left-clicking in the background of the canvas

   ----------------
   -- Properties --
   ----------------

   type GVD_Items_Property_Record is new Property_Record with record
      Items : String_List_Access;
   end record;
   type GVD_Items_Property is access all GVD_Items_Property_Record'Class;

   overriding procedure Save
     (Property : access GVD_Items_Property_Record;
      Node     : in out XML_Utils.Node_Ptr);
   overriding procedure Load
     (Property : in out GVD_Items_Property_Record;
      From     : XML_Utils.Node_Ptr);
   overriding procedure Destroy (Property : in out GVD_Items_Property_Record);
   --  See inherited documentation

   procedure Load_Items_From_Property
     (Process  : access Visual_Debugger_Record'Class;
      Property : GVD_Items_Property_Record'Class);
   --  Restore the items stored in the property

   -----------------
   -- Local Types --
   -----------------

   type Item_Record (Name_Length : Natural) is record
      Canvas         : GVD_Canvas;
      Item           : Display_Item;
      Mode           : Display_Mode;
      Format         : Value_Format;
      Zoom           : Guint;
      Component      : Items.Generic_Type_Access;
      Component_Name : String (1 .. Name_Length);
   end record;

   --------------------
   -- Local Packages --
   --------------------

   package Item_Handler is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record, Item_Record);

   ----------------------
   -- Local Procedures --
   ----------------------

   procedure Change_Detect_Aliases
     (Item   : access Gtk_Check_Menu_Item_Record'Class;
      Canvas : General_Browser);
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

   procedure Undisplay_Item
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record);
   --  Hide all the subcomponents of the selected item

   procedure Toggle_Refresh_Mode
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record);
   --  Toggle between "auto_refresh" and "frozen" modes

   procedure Allocate_Fonts (Canvas : access GVD_Canvas_Record'Class);
   --  Reallocate all the fonts, with the appropriate size given the current
   --  zoom

   procedure On_Data_Refresh (Canvas : access Gtk_Widget_Record'Class);
   --  "Refresh" contextual menu

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Hook   : Preferences_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Kernel);
   begin
      Preferences_Changed (Hook.Canvas);
   end Execute;

   ----------
   -- Save --
   ----------

   overriding procedure Save
     (Property : access GVD_Items_Property_Record;
      Node     : in out XML_Utils.Node_Ptr)
   is
      Tmp : XML_Utils.Node_Ptr;
   begin
      if Property.Items /= null then
         for Item in reverse Property.Items'Range loop
            Tmp := new XML_Utils.Node;
            Add_Child (Node, Tmp);
            Tmp.Tag   := new String'("item");
            Tmp.Value := new String'(Property.Items (Item).all);
         end loop;
      end if;
   end Save;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Property : in out GVD_Items_Property_Record;
      From     : XML_Utils.Node_Ptr)
   is
      Tmp   : XML_Utils.Node_Ptr := From.Child;
      Count : Natural := 0;
   begin
      while Tmp /= null loop
         Count := Count + 1;
         Tmp   := Tmp.Next;
      end loop;

      Property.Items := new GNAT.Strings.String_List (1 .. Count);
      Count := Property.Items'First;

      Tmp := From.Child;
      while Tmp /= null loop
         Property.Items (Count) := new String'(Tmp.Value.all);
         Count := Count + 1;
         Tmp   := Tmp.Next;
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
     (Canvas  : access GVD_Canvas_Record;
      Process : access Visual_Debugger_Record'Class)
   is
      Property : GVD_Items_Property_Record;
      Found    : Boolean;
   begin
      Clear (Get_Canvas (Canvas));
      Canvas.Item_Num := 0;

      if Preserve_State_On_Exit.Get_Pref then
         Get_Property
           (Property,
            Get_Executable (Process.Debugger),
            Name  => "debugger_items",
            Found => Found);
         if Found then
            Load_Items_From_Property (Process, Property);
         end if;
      end if;
   end On_Attach;

   ------------
   -- Update --
   ------------

   overriding procedure Update (Canvas : access GVD_Canvas_Record) is
   begin
      if Get_Process (Canvas) /= null then
         Recompute_All_Aliases (Get_Process (Canvas));
         Refresh_Data_Window (Get_Process (Canvas));
      end if;
   end Update;

   ----------------
   -- Get_Canvas --
   ----------------

   function Get_Canvas
     (Process : access Visual_Debugger_Record'Class)
      return General_Browser is
   begin
      return General_Browser (Process.Data);
   end Get_Canvas;

   ----------------
   -- Set_Canvas --
   ----------------

   procedure Set_Canvas
     (Process : access Visual_Debugger_Record'Class;
      Canvas  : General_Browser)
   is
      Old      : constant GVD_Canvas := GVD_Canvas (Process.Data);
      Iter     : Item_Iterator;
      Property : GVD_Items_Property;
      Count    : Natural := 0;
   begin
      --  Save the currently displayed items, if any

      if Old /= null and then Process.Debugger /= null then
         if Preserve_State_On_Exit.Get_Pref then
            Iter := Start (Get_Canvas (Old));

            while Get (Iter) /= null loop
               if Get_Graph_Cmd (Display_Item (Get (Iter))) /= "" then
                  Count := Count + 1;
               end if;

               Next (Iter);
            end loop;

            if Count = 0 then
               Remove_Property
                 (Kernel => Get_Kernel (Old),
                  File   => Get_Executable (Process.Debugger),
                  Name   => "debugger_items");

            else
               Property := new GVD_Items_Property_Record;
               Property.Items := new GNAT.Strings.String_List (1 .. Count);
               Count := Property.Items'First;

               Iter := Start (Get_Canvas (Old));

               while Get (Iter) /= null loop
                  declare
                     S : constant String :=
                           Get_Graph_Cmd (Display_Item (Get (Iter)));
                  begin
                     if S /= "" then
                        Property.Items (Count) := new String'(S);
                        Count := Count + 1;
                     end if;
                  end;

                  Next (Iter);
               end loop;

               Traces.Trace (Me, "Saving debugger canvas properties");
               Set_Property
                 (Kernel     => Get_Kernel (Old),
                  File       => Get_Executable (Process.Debugger),
                  Name       => "debugger_items",
                  Property   => Property,
                  Persistent => True);
            end if;
         end if;

         Old.Selected_Item := null;
         Old.Selected_Component := null;
      end if;

      --  If we are detaching, clear the old view
      if Canvas = null
        and then Process.Data /= null
      then
         Clear (Get_Canvas (GVD_Canvas (Process.Data)));
      end if;

      Process.Data := Gtk_Widget (Canvas);
   end Set_Canvas;

   ----------------
   -- Get_Canvas --
   ----------------

   function Get_Canvas
     (Process : access GVD.Process.Visual_Debugger_Record'Class)
      return Gtkada.Canvas.Interactive_Canvas is
   begin
      if Process.Data = null then
         return null;
      else
         return Get_Canvas (GVD_Canvas (Process.Data));
      end if;
   end Get_Canvas;

   -----------------------
   -- Process_Graph_Cmd --
   -----------------------

   procedure Process_Graph_Cmd
     (Process : access Visual_Debugger_Record'Class;
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
      X, Y      : Gint := Gint'First;
      Num       : Integer := -1;
      Entity    : Generic_Type_Access;

   begin
      --  graph (print|display) expression [dependent on display_num]
      --        [link_name name] [at x, y] [num \d+]
      --  graph (print|display) `command` [at x, y] [num \d+]
      --  graph enable display display_num [display_num ...]
      --  graph disable display display_num [display_num ...]
      --  graph undisplay display_num

      Match (Graph_Cmd_Format, Cmd, Matched);

      if Matched (0) /= No_Match then
         Attach_To_Data_Window (Process, Create_If_Necessary => True);
         Enable := Cmd (Matched (Graph_Cmd_Type_Paren).First) = 'd'
           or else Cmd (Matched (Graph_Cmd_Type_Paren).First) = 'D';

         --  Do we have any 'dependent on' expression ?
         if Matched (Graph_Cmd_Dependent_Paren) /= No_Match then
            Num := Safe_Value
              (Cmd (Matched (Graph_Cmd_Dependent_Paren).First
                    .. Matched (Graph_Cmd_Dependent_Paren).Last),
               -1);
            if Num /= -1 then
               Link_From := Find_Item
                 (Get_Canvas (GVD_Canvas (Process.Data)), Num);
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
            X := Gint (Safe_Value
               (Cmd (Matched (Graph_Cmd_X_Paren).First
                     .. Matched (Graph_Cmd_X_Paren).Last), Integer'First));
            Y := Gint (Safe_Value
               (Cmd (Matched (Graph_Cmd_Y_Paren).First
                     .. Matched (Graph_Cmd_Y_Paren).Last), Integer'First));
         end if;

         --  Do we have any 'num' expression
         if Matched (Graph_Cmd_Num_Paren) /= No_Match then
            Num := Safe_Value
                (Cmd (Matched (Graph_Cmd_Num_Paren).First
                      .. Matched (Graph_Cmd_Num_Paren).Last), -1);
            Num := Get_Next_Item_Num (Process, Num);
         else
            Num := Get_Next_Item_Num (Process);
         end if;

         --  What do we want to display ?

         if Matched (Graph_Cmd_Expression_Paren) /= No_Match then
            First  := Matched (Graph_Cmd_Expression_Paren).First;
            Last   := Matched (Graph_Cmd_Expression_Paren).Last;
            Entity := New_Debugger_Type (Cmd (First .. Last));
            Set_Value
              (Debugger_Output_Type (Entity.all),
               Process_User_Command
                 (Visual_Debugger (Process),
                  Refresh_Command (Debugger_Output_Type (Entity.all)),
                  Mode => GVD.Types.Internal));

         elsif Matched (Graph_Cmd_Quoted_Paren) /= No_Match then
            First := Matched (Graph_Cmd_Quoted_Paren).First;
            Last  := Matched (Graph_Cmd_Quoted_Paren).Last;
         else
            First := Matched (Graph_Cmd_Variable_Paren).First;
            Last  := Matched (Graph_Cmd_Variable_Paren).Last;
         end if;

         --  No link ?

         if Link_From = null then
            Gtk_New
              (Item,
               Browser        => GVD_Canvas (Process.Data),
               Graph_Cmd      => Cmd (Matched
                 (Graph_Cmd_Cmd_Paren).First
                 .. Matched (Graph_Cmd_Cmd_Paren).Last),
               Variable_Name  => Cmd (First .. Last),
               Debugger       => Visual_Debugger (Process),
               Auto_Refresh   => Enable,
               Num            => Num,
               Default_Entity => Entity);
         else
            if Link_Name = null then
               Link_Name := new String'(Cmd (First .. Last));
            end if;

            Gtk_New
              (Item,
               Browser        => GVD_Canvas (Process.Data),
               Graph_Cmd      => Cmd (Matched
                 (Graph_Cmd_Cmd_Paren).First
                 .. Matched (Graph_Cmd_Cmd_Paren).Last),
               Variable_Name  => Cmd (First .. Last),
               Debugger       => Visual_Debugger (Process),
               Auto_Refresh   => Enable,
               Default_Entity => Entity,
               Num            => Num,
               Link_From      => Link_From,
               Link_Name      => Link_Name.all);
         end if;

         --  Might be null if it was already on the canvas
         if Item /= null then
            Put (Get_Canvas (GVD_Canvas (Process.Data)), Item, X, Y);

            if Link_From /= null then
               Recompute_All_Aliases (Process, Recompute_Values => False);
            end if;

            --  We need to do a layout in all cases, so that the newly added
            --  item is put at a correct place.
            Layout (GVD_Canvas (Process.Data), Force => False);
            Refresh_Canvas (Get_Canvas (GVD_Canvas (Process.Data)));
            Align_Item
              (Get_Canvas (GVD_Canvas (Process.Data)), Item, 0.4, 0.4);

            --  Show the item last, one the scrollbars have been computed
            Show_Item (Get_Canvas (GVD_Canvas (Process.Data)), Item);
         end if;

         Free (Link_Name);

      else
         --  Is this an enable/disable command ?

         Match (Graph_Cmd_Format2, Cmd, Matched);

         if Matched (2) /= No_Match then
            Attach_To_Data_Window (Process, Create_If_Necessary => True);

            Index := Matched (2).First;
            Enable := Cmd (Matched (1).First) = 'e'
              or else Cmd (Matched (1).First) = 'E';

            while Index <= Cmd'Last loop
               Last := Index;
               Skip_To_Blank (Cmd, Last);
               Num := Safe_Value (Cmd (Index .. Last - 1), -1);

               if Num /= -1 then
                  Item := Find_Item
                    (Get_Canvas (GVD_Canvas (Process.Data)), Num);

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
               Attach_To_Data_Window (Process, Create_If_Necessary => True);
               Index := Matched (1).First;

               while Index <= Cmd'Last loop
                  Last := Index;
                  Skip_To_Blank (Cmd, Last);
                  Num := Safe_Value (Cmd (Index .. Last - 1), -1);

                  if Num /= -1 then
                     Item :=
                       Find_Item
                         (Get_Canvas (GVD_Canvas (Process.Data)), Num);

                     if Item /= null then
                        Free (Item);
                     end if;
                  end if;

                  Index := Last + 1;
                  Skip_Blanks (Cmd, Index);
               end loop;
            end if;
         end if;
      end if;

   exception
      when E : Constraint_Error => Traces.Trace (Exception_Handle, E);
         --  Usually because Find_Item returned a null value
         GVD.Trace.Output_Error
           (Process.Window.Kernel, (-" Error while processing: ") & Cmd);
   end Process_Graph_Cmd;

   -------------------------
   -- Refresh_Data_Window --
   -------------------------

   procedure Refresh_Data_Window
     (Debugger : access GVD.Process.Visual_Debugger_Record'Class) is
   begin
      if Debugger.Data /= null then
         Refresh_Canvas (Get_Canvas (GVD_Canvas (Debugger.Data)));
      end if;
   end Refresh_Data_Window;

   ---------------------
   -- On_Data_Refresh --
   ---------------------

   procedure On_Data_Refresh (Canvas : access Gtk_Widget_Record'Class) is
      C    : constant GVD_Canvas := GVD_Canvas (Canvas);
      Iter : Item_Iterator;
      Item : Canvas_Item;

   begin
      Iter := Start (Get_Canvas (C));
      loop
         Item := Get (Iter);
         exit when Item = null;

         Display_Items.Update (Display_Item (Item), Redisplay_Canvas => False);

         Next (Iter);
      end loop;

      Refresh_Canvas (Get_Canvas (C));

   exception
      when E : others => Traces.Trace (Exception_Handle, E);
   end On_Data_Refresh;

   -----------------
   -- Select_Item --
   -----------------

   procedure Select_Item
     (Process   : access Visual_Debugger_Record'Class;
      Item      : access Display_Item_Record'Class;
      Component : Generic_Type_Access)
   is
      Canvas            : constant GVD_Canvas := GVD_Canvas (Process.Data);
      Has_New_Selection : constant Boolean :=
                            Canvas.Selected_Item /= Browser_Item (Item)
                              or else Canvas.Selected_Component /= Component;
   begin
      --  Unselect the current selection

      if Has_New_Selection then
         if Canvas.Selected_Component /= null
           and then Canvas.Selected_Item /= null
         then
            Set_Selected (Canvas.Selected_Component, False);
            Update_Component
              (Display_Item (Canvas.Selected_Item), Canvas.Selected_Component);

            --  Avoid refreshing the same item twice, if we're going to do it
            --  in the second part of this procedure anyway.
            if Canvas.Selected_Item /= Browser_Item (Item)
              or else Component = null
            then
               Item_Updated (Get_Canvas (Canvas), Canvas.Selected_Item);
            end if;
         end if;

         if Component /= null then
            --  Select the new one
            Set_Selected (Component, not Get_Selected (Component));

            Update_Component (Item, Component);
            Item_Updated (Get_Canvas (Canvas), Item);

            if Get_Selected (Component) then
               Canvas.Selected_Item := Browser_Item (Item);
               Canvas.Selected_Component := Component;
            else
               Canvas.Selected_Item := null;
            end if;
         else
            Canvas.Selected_Item := null;
         end if;
      end if;
   end Select_Item;

   --------------
   -- Unselect --
   --------------

   procedure Unselect
     (Process : access GVD.Process.Visual_Debugger_Record'Class;
      Item    : access Display_Items.Display_Item_Record'Class)
   is
      Canvas : constant GVD_Canvas := GVD_Canvas (Process.Data);
   begin
      if Canvas.Selected_Item = Browser_Item (Item) then
         Canvas.Selected_Item := null;
      end if;
   end Unselect;

   ---------------------------
   -- Change_Detect_Aliases --
   ---------------------------

   procedure Change_Detect_Aliases
     (Item   : access Gtk_Check_Menu_Item_Record'Class;
      Canvas : General_Browser)
   is
      pragma Unreferenced (Item);
      C : constant GVD_Canvas := GVD_Canvas (Canvas);
   begin
      Set_Detect_Aliases (C, not C.Detect_Aliases);

      --  Recompute all the aliases
      Recompute_All_Aliases (Get_Process (C));

      Refresh_Data_Window (Get_Process (C));

   exception
      when E : others => Traces.Trace (Exception_Handle, E);
   end Change_Detect_Aliases;

   ------------------------
   -- Display_Expression --
   ------------------------

   procedure Display_Expression (Canvas : access Gtk_Widget_Record'Class) is
   begin
      Display_Expression (Get_Process (GVD_Canvas (Canvas)));
   exception
      when E : others => Traces.Trace (Exception_Handle, E);
   end Display_Expression;

   ------------------------
   -- Get_Detect_Aliases --
   ------------------------

   function Get_Detect_Aliases
     (Process : access GVD.Process.Visual_Debugger_Record'Class)
      return Boolean is
   begin
      return GVD_Canvas (Process.Data).Detect_Aliases;
   end Get_Detect_Aliases;

   ------------------------
   -- Set_Detect_Aliases --
   ------------------------

   procedure Set_Detect_Aliases
     (Canvas   : access GVD_Canvas_Record'Class;
      Activate : Boolean) is
   begin
      --  ??? We should modify the items displayed so as to remove currently
      --  detected aliases. This is part of the whole aliases detection
      --  implementation.
      Canvas.Detect_Aliases := Activate;
   end Set_Detect_Aliases;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (Canvas : access GVD_Canvas_Record'Class;
      Kernel : access Kernel_Handle_Record'Class) return Gtk_Widget
   is
      Annotation_Font : Pango_Font_Description;
      Hook            : Preferences_Hook;
   begin
      Browsers.Canvas.Initialize
        (Canvas, Kernel, Create_Toolbar => False);
      Canvas.Detect_Aliases := Default_Detect_Aliases.Get_Pref;

      Register_Contextual_Menu
        (Kernel          => Kernel,
         Event_On_Widget => Canvas,
         Object          => Canvas,
         ID              => Debugger_Module_ID,
         Context_Func    => GVD_Canvas_Context_Factory'Access);

      Widget_Callback.Object_Connect
        (Get_Canvas (Canvas), Signal_Realize, On_Realize'Access, Canvas);
      Object_Callback.Object_Connect
        (Get_Canvas (Canvas), Signal_Background_Click,
         Object_Callback.To_Marshaller (On_Background_Click'Access),
         Canvas);

      Hook := new Preferences_Hook_Record'
        (Function_No_Args with Canvas => GVD_Canvas (Canvas));
      Add_Hook
        (Kernel, Preferences_Changed_Hook, Hook,
         Name  => "canvas.preferences_changed",
         Watch => GObject (Canvas));

      --  Initialize the canvas

      Annotation_Font :=
        Copy (GPS.Kernel.Preferences.Default_Font.Get_Pref_Font);
      Set_Size
        (Annotation_Font,
         Gint'Max (Pango_Scale,
           Get_Size (Annotation_Font) - 2 * Pango_Scale));
      Configure (Get_Canvas (Canvas), Annotation_Font => Annotation_Font);
      Free (Annotation_Font);

      return Gtk_Widget (Canvas);
   end Initialize;

   -------------------
   -- Initialize_GC --
   -------------------

   procedure Initialize_GC (Canvas : access GVD_Canvas_Record'Class) is
   begin
      Canvas.Item_Context.Foreground := Black (Get_Default_Colormap);
      Canvas.Tooltip_Context.Foreground := Black (Get_Default_Colormap);

      Canvas.Item_Context.Xref_Color := Xref_Color.Get_Pref;
      Canvas.Tooltip_Context.Xref_Color := Xref_Color.Get_Pref;

      Canvas.Item_Context.Modified_Color := Change_Color.Get_Pref;
      Canvas.Tooltip_Context.Modified_Color := Change_Color.Get_Pref;

      Canvas.Item_Context.Selection_Color := Selected_Item_Color.Get_Pref;
      Canvas.Tooltip_Context.Selection_Color := Selected_Item_Color.Get_Pref;

      Canvas.Box_Context.Black_Color := Black (Get_Default_Colormap);
      Canvas.Box_Context.Grey_Color := Title_Color.Get_Pref;
      Canvas.Box_Context.Thaw_Bg_Color := Thaw_Bg_Color.Get_Pref;
      Canvas.Box_Context.Freeze_Bg_Color := Freeze_Bg_Color.Get_Pref;
   end Initialize_GC;

   ----------------
   -- On_Realize --
   ----------------

   procedure On_Realize (Canvas : access Gtk_Widget_Record'Class) is
      C   : constant GVD_Canvas := GVD_Canvas (Canvas);
      Win : constant Gdk.Window.Gdk_Window := Get_Window (Get_Canvas (C));
   begin
      pragma Assert (Win /= null);
      if C.Box_Context.Close_Pixmap = null then
         C.Box_Context.Close_Pixmap :=
           Gdk.Pixbuf.Gdk_New_From_Xpm_Data (cancel_xpm);
         C.Box_Context.Locked_Pixmap :=
           Gdk.Pixbuf.Gdk_New_From_Xpm_Data (lock_xpm);
         C.Box_Context.Auto_Display_Pixmap :=
           Gdk.Pixbuf.Gdk_New_From_Xpm_Data (display_small_xpm);
         C.Item_Context.Hidden_Pixmap :=
           Gdk.Pixbuf.Gdk_New_From_Xpm_Data (box_xpm);
         C.Item_Context.Unknown_Pixmap :=
           Gdk.Pixbuf.Gdk_New_From_Xpm_Data (trash_xpm);
         Preferences_Changed (C);
      end if;

      --  Create graphic contexts

      Initialize_GC (C);

   exception
      when E : others => Traces.Trace (Exception_Handle, E);
   end On_Realize;

   ---------------------------
   -- Attach_To_Data_Window --
   ---------------------------

   procedure Attach_To_Data_Window
     (Debugger : access GVD.Process.Visual_Debugger_Record'Class;
      Create_If_Necessary : Boolean)
      renames Canvas_Views.Attach_To_View;

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

   --------------------
   -- Allocate_Fonts --
   --------------------

   procedure Allocate_Fonts (Canvas : access GVD_Canvas_Record'Class) is
      Iter : Item_Iterator := Start (Get_Canvas (Canvas));
      Item : Canvas_Item;
      Hide : constant Boolean := Hide_Big_Items.Get_Pref;
   begin
      loop
         Item := Get (Iter);
         exit when Item = null;

         Update_Resize_Display
           (Display_Item (Item), True, Hide, Redisplay_Canvas => False);
         Next (Iter);
      end loop;
   end Allocate_Fonts;

   -------------------------
   -- Preferences_Changed --
   -------------------------

   procedure Preferences_Changed
     (Canvas : access GVD_Canvas_Record'Class)
   is
      Item : Canvas_Item;
      Iter : Item_Iterator;
      Hide : constant Boolean := Hide_Big_Items.Get_Pref;

   begin
      Set_Detect_Aliases (Canvas, Default_Detect_Aliases.Get_Pref);

      --  If we are not attached to a process, this means the canvas is empty
      --  and nothing needs to be done anyway
      if Get_Process (Canvas) /= null then
         Recompute_All_Aliases (Get_Process (Canvas));
      end if;

      Initialize_GC (Canvas);

      --  The drawing context for the items

      Items.Set_Max_Height (Max_Item_Height.Get_Pref);
      Items.Set_Max_Width  (Max_Item_Width.Get_Pref);

      if Canvas.Item_Context.Text_Layout /= null then
         Unref (Canvas.Item_Context.Text_Layout);
         Unref (Canvas.Item_Context.Type_Layout);
      end if;

      Canvas.Item_Context.Line_Height := To_Pixels
        (Get_Size (View_Fixed_Font.Get_Pref));

      Canvas.Item_Context.Big_Item_Height := Gint (Big_Item_Height.Get_Pref);

      Canvas.Item_Context.Text_Layout :=
        Create_Pango_Layout (Get_Canvas (Canvas));
      Set_Font_Description
        (Canvas.Item_Context.Text_Layout, View_Fixed_Font.Get_Pref);

      Canvas.Item_Context.Type_Layout :=
        Create_Pango_Layout (Get_Canvas (Canvas));
      Set_Font_Description
        (Canvas.Item_Context.Type_Layout, Type_Font.Get_Pref);

      Allocate_Fonts (Canvas);

      Iter := Start (Get_Canvas (Canvas));
      loop
         Item := Get (Iter);
         exit when Item = null;

         Update_Resize_Display
           (Display_Item (Item), True, Hide, Redisplay_Canvas => False);
         Next (Iter);
      end loop;

      Refresh_Canvas (Get_Canvas (Canvas));
   end Preferences_Changed;

   -------------------------
   -- Change_Display_Mode --
   -------------------------

   procedure Change_Display_Mode
     (Widget : access Gtk_Widget_Record'Class;
      Item   : Item_Record) is
   begin
      if Get_Active (Gtk_Radio_Menu_Item (Widget))
        and then Get_Display_Mode (Item.Item) /= Item.Mode
      then
         Set_Display_Mode (Item.Item, Item.Mode);
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
        and then Get_Format (Item.Item) /= Item.Format
      then
         Set_Format (Item.Item, Item.Format);
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
      if Is_A_Variable (Item.Item) then
         Process_User_Command
           (Get_Debugger (Item.Item),
            "graph display " & Item.Component_Name,
            Output_Command => True);
      else
         Process_User_Command
           (Get_Debugger (Item.Item),
            "graph display `" & Get_Name (Item.Item) & "`",
            Output_Command => True);
      end if;
   end Clone_Component;

   ------------------
   -- Unselect_All --
   ------------------

   procedure Unselect_All
     (Canvas : access GVD_Canvas_Record'Class) is
   begin
      if Canvas.Selected_Component /= null
        and then Canvas.Selected_Item /= null
      then
         Set_Selected (Canvas.Selected_Component, False);
         Update_Component
           (Display_Item (Canvas.Selected_Item), Canvas.Selected_Component);
         Item_Updated (Get_Canvas (Canvas), Canvas.Selected_Item);
         Canvas.Selected_Component := null;
         Canvas.Selected_Item := null;
         Traces.Trace (Me, "Unselect_All");
      end if;
   end Unselect_All;

   -------------------------
   -- On_Background_Click --
   -------------------------

   procedure On_Background_Click
     (Canvas : access Glib.Object.GObject_Record'Class;
      Event  : Gdk.Event.Gdk_Event)
   is
      C : constant GVD_Canvas := GVD_Canvas (Canvas);
   begin
      if Get_Button (Event) = 1 then
         if Get (Start (Get_Canvas (C))) /= null then
            Unselect_All (C);
         end if;
      end if;

      --  third mouse button handled by GVD_Canvas_Context_Factory as part of
      --  the standard contextual menu handling
   end On_Background_Click;

   --------------------------------
   -- GVD_Canvas_Context_Factory --
   --------------------------------

   procedure GVD_Canvas_Context_Factory
     (Context      : in out GPS.Kernel.Selection_Context;
      Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu)
   is
      Canvas  : constant GVD_Canvas := GVD_Canvas (Object);
      Mitem   : Gtk_Menu_Item;
      Sep     : Gtk_Separator_Menu_Item;
      Check   : Gtk_Check_Menu_Item;
      Xr, Yr  : Gint;
      CItem   : Canvas_Item;
      Item    : Display_Item;
   begin
      Default_Browser_Context_Factory
        (Context, Kernel, Event_Widget, Object, Event, Menu);

      if Get_Event_Type (Event) in Button_Press .. Button_Release then
         Item_At_Coordinates (Get_Canvas (Canvas), Event, CItem, Xr, Yr);
         Item := Display_Item (CItem);
      end if;

      if Item = null then
         Gtk_New (Sep);
         Append (Menu, Sep);

         Gtk_New (Mitem, Label => -"Display Expression...");
         Append (Menu, Mitem);
         Widget_Callback.Object_Connect
           (Mitem, Signal_Activate, Display_Expression'Access, Canvas);

         Gtk_New (Check, Label => -"Detect Aliases");
         Set_Active (Check, Canvas.Detect_Aliases);
         Append (Menu, Check);
         Check_Canvas_Handler.Connect
           (Check, Signal_Activate,
            Check_Canvas_Handler.To_Marshaller (Change_Detect_Aliases'Access),
            General_Browser (Canvas));

         Gtk_New (Mitem, Label => -"Recompute");
         Append (Menu, Mitem);
         Widget_Callback.Object_Connect
           (Mitem, Signal_Activate, On_Data_Refresh'Access, Canvas);

      else
         declare
            Component : aliased Generic_Type_Access;
            Name      : constant String := Get_Component
              (Item, X => Xr, Y => Yr, Component => Component'Access);
         begin
            if Component /= null then
               Item_Contextual_Menu
                 (Menu,
                  Debugger       => Get_Process (Canvas),
                  Item           => Item,
                  Component      => Component,
                  Component_Name => Name);
            end if;
         end;
      end if;
   end GVD_Canvas_Context_Factory;

   --------------------------
   -- Item_Contextual_Menu --
   --------------------------

   procedure Item_Contextual_Menu
     (Menu           : Gtk_Menu;
      Debugger       : access GVD.Process.Visual_Debugger_Record'Class;
      Item           : access Display_Items.Display_Item_Record'Class;
      Component      : Items.Generic_Type_Access;
      Component_Name : String)
   is
      Canvas  : constant GVD_Canvas := GVD_Canvas (Debugger.Data);
      Mitem   : Gtk_Menu_Item;
      Sep     : Gtk_Separator_Menu_Item;
      Radio   : Gtk_Radio_Menu_Item;
      Check   : Gtk_Check_Menu_Item;
      Submenu : Gtk_Menu;

   begin
      --  Display "Close" option

      Gtk_New (Mitem, Label => -"Close" & " " & Get_Name (Item));
      Item_Handler.Connect
        (Mitem, Signal_Activate,
         Item_Handler.To_Marshaller (Undisplay_Item'Access),
         Item_Record'(Name_Length    => Component_Name'Length,
                      Canvas         => Canvas,
                      Item           => Display_Item (Item),
                      Component      => Component,
                      Component_Name => Component_Name,
                      Mode           => Value,
                      Format         => Default_Format,
                      Zoom           => 100));
      Append (Menu, Mitem);

      if Is_A_Variable (Item) then
         --  Display a separator

         Gtk_New (Sep);
         Append (Menu, Sep);

         Gtk_New (Mitem, Label => -"Hide all " & Component_Name);
         Item_Handler.Connect
           (Mitem, Signal_Activate,
            Item_Handler.To_Marshaller (Hide_All'Access),
            Item_Record'(Name_Length    => Component_Name'Length,
                         Canvas         => Canvas,
                         Item           => Display_Item (Item),
                         Component      => Component,
                         Component_Name => Component_Name,
                         Mode           => Value,
                         Format         => Default_Format,
                         Zoom           => 100));
         Append (Menu, Mitem);

         Gtk_New (Mitem, Label => -"Show all " & Component_Name);
         Item_Handler.Connect
           (Mitem, Signal_Activate,
            Item_Handler.To_Marshaller (Show_All'Access),
            Item_Record'(Name_Length    => Component_Name'Length,
                         Canvas         => Canvas,
                         Item           => Display_Item (Item),
                         Component      => Component,
                         Component_Name => Component_Name,
                         Mode           => Value,
                         Format         => Default_Format,
                         Zoom           => 100));
         Append (Menu, Mitem);

         Gtk_New (Mitem, Label => -"Dereference all pointers");
         Item_Handler.Connect
           (Mitem, Signal_Activate,
            Item_Handler.To_Marshaller (Dereference_All'Access),
            Item_Record'(Name_Length    => Component_Name'Length,
                         Canvas         => Canvas,
                         Item           => Display_Item (Item),
                         Component      => Component,
                         Component_Name => Component_Name,
                         Mode           => Value,
                         Format         => Default_Format,
                         Zoom           => 100));
         Append (Menu, Mitem);

         --  Display a separator

         Gtk_New (Sep);
         Append (Menu, Sep);

         Gtk_New (Mitem, Label => -"Clone" & " " & Component_Name);
         Item_Handler.Connect
           (Mitem, Signal_Activate,
            Item_Handler.To_Marshaller (Clone_Component'Access),
            Item_Record'(Name_Length    => Component_Name'Length,
                         Canvas         => Canvas,
                         Item           => Display_Item (Item),
                         Component      => Component,
                         Component_Name => Component_Name,
                         Mode           => Value,
                         Format         => Default_Format,
                         Zoom           => 100));
         Append (Menu, Mitem);

         Gtk_New (Mitem, Label => -"View memory at address of "
                  & Krunch (Component_Name));
         Item_Handler.Connect
           (Mitem, Signal_Activate,
            Item_Handler.To_Marshaller (View_Into_Memory'Access),
            Item_Record'(Name_Length    => Component_Name'Length,
                         Canvas         => Canvas,
                         Item           => Display_Item (Item),
                         Component      => Component,
                         Component_Name => Component_Name,
                         Mode           => Value,
                         Format         => Default_Format,
                         Zoom           => 100));
         Append (Menu, Mitem);

         Gtk_New (Mitem, Label => -"Set Value of " & Krunch (Component_Name));
         Item_Handler.Connect
           (Mitem, Signal_Activate,
            Item_Handler.To_Marshaller (Set_Value'Access),
            Item_Record'(Name_Length    => Component_Name'Length,
                         Canvas         => Canvas,
                         Item           => Display_Item (Item),
                         Component      => Component,
                         Component_Name => Component_Name,
                         Mode           => Value,
                         Format         => Default_Format,
                         Zoom           => 100));
         Append (Menu, Mitem);
      end if;

      Gtk_New (Mitem, Label => -"Update Value");
      Item_Handler.Connect
        (Mitem, Signal_Activate,
         Item_Handler.To_Marshaller (Update_Variable'Access),
         Item_Record'(Name_Length    => Component_Name'Length,
                      Canvas         => Canvas,
                      Item           => Display_Item (Item),
                      Component      => Component,
                      Component_Name => Component_Name,
                      Mode           => Value,
                      Format         => Default_Format,
                      Zoom           => 100));
      Append (Menu, Mitem);

      if Is_A_Variable (Item) then
         --  Display a separator
         Gtk_New (Sep);
         Append (Menu, Sep);

         Gtk_New (Submenu);
         Gtk_New (Mitem, Label => -"Display");
         Set_Submenu (Mitem, Gtk_Widget (Submenu));
         Append (Menu, Mitem);

         Gtk_New (Radio, Widget_SList.Null_List, -"Show Value");
         Set_Active (Radio, Get_Display_Mode (Item) = Value);
         Item_Handler.Connect
           (Radio, Signal_Activate,
            Item_Handler.To_Marshaller (Change_Display_Mode'Access),
            Item_Record'(Name_Length    => Component_Name'Length,
                         Canvas         => Canvas,
                         Item           => Display_Item (Item),
                         Component      => Component,
                         Component_Name => Component_Name,
                         Mode           => Value,
                         Format         => Default_Format,
                         Zoom           => 100));
         Append (Submenu, Radio);

         Gtk_New (Radio, Group (Radio), -"Show Type");
         Set_Active (Radio, Get_Display_Mode (Item) = Type_Only);
         Item_Handler.Connect
           (Radio, Signal_Activate,
            Item_Handler.To_Marshaller (Change_Display_Mode'Access),
            Item_Record'(Name_Length    => Component_Name'Length,
                         Canvas         => Canvas,
                         Item           => Display_Item (Item),
                         Component      => Component,
                         Component_Name => Component_Name,
                         Mode           => Type_Only,
                         Format         => Default_Format,
                         Zoom           => 100));
         Append (Submenu, Radio);

         Gtk_New (Radio, Group (Radio), -"Show Value + Type");
         Set_Active (Radio, Get_Display_Mode (Item) = Type_Value);
         Item_Handler.Connect
           (Radio, Signal_Activate,
            Item_Handler.To_Marshaller (Change_Display_Mode'Access),
            Item_Record'(Name_Length    => Component_Name'Length,
                         Canvas         => Canvas,
                         Item           => Display_Item (Item),
                         Component      => Component,
                         Component_Name => Component_Name,
                         Mode           => Type_Value,
                         Format         => Default_Format,
                         Zoom           => 100));
         Append (Submenu, Radio);

         --  Display a separator
         Gtk_New (Sep);
         Append (Submenu, Sep);

         Gtk_New (Radio, Widget_SList.Null_List, -"Default");
         Set_Active (Radio, Get_Format (Item) = Default_Format);
         Item_Handler.Connect
           (Radio, Signal_Activate,
            Item_Handler.To_Marshaller (Change_Format'Access),
            Item_Record'(Name_Length    => Component_Name'Length,
                         Canvas         => Canvas,
                         Item           => Display_Item (Item),
                         Component      => Component,
                         Component_Name => Component_Name,
                         Mode           => Value,
                         Format         => Default_Format,
                         Zoom           => 100));
         Append (Submenu, Radio);

         Gtk_New (Radio, Group (Radio), -"Decimal");
         Set_Active (Radio, Get_Format (Item) = Decimal);
         Item_Handler.Connect
           (Radio, Signal_Activate,
            Item_Handler.To_Marshaller (Change_Format'Access),
            Item_Record'(Name_Length    => Component_Name'Length,
                         Canvas         => Canvas,
                         Item           => Display_Item (Item),
                         Component      => Component,
                         Component_Name => Component_Name,
                         Mode           => Value,
                         Format         => Decimal,
                         Zoom           => 100));
         Append (Submenu, Radio);

         Gtk_New (Radio, Group (Radio), -"Hexadecimal");
         Set_Active (Radio, Get_Format (Item) = Hexadecimal);
         Item_Handler.Connect
           (Radio, Signal_Activate,
            Item_Handler.To_Marshaller (Change_Format'Access),
            Item_Record'(Name_Length    => Component_Name'Length,
                         Canvas         => Canvas,
                         Item           => Display_Item (Item),
                         Component      => Component,
                         Component_Name => Component_Name,
                         Mode           => Value,
                         Format         => Hexadecimal,
                         Zoom           => 100));
         Append (Submenu, Radio);

         Gtk_New (Radio, Group (Radio), -"Octal");
         Set_Active (Radio, Get_Format (Item) = Octal);
         Item_Handler.Connect
           (Radio, Signal_Activate,
            Item_Handler.To_Marshaller (Change_Format'Access),
            Item_Record'(Name_Length    => Component_Name'Length,
                         Canvas         => Canvas,
                         Item           => Display_Item (Item),
                         Component      => Component,
                         Component_Name => Component_Name,
                         Mode           => Value,
                         Format         => Octal,
                         Zoom           => 100));
         Append (Submenu, Radio);

         Gtk_New (Radio, Group (Radio), -"Binary");
         Set_Active (Radio, Get_Format (Item) = Binary);
         Item_Handler.Connect
           (Radio, Signal_Activate,
            Item_Handler.To_Marshaller (Change_Format'Access),
            Item_Record'(Name_Length    => Component_Name'Length,
                         Canvas         => Canvas,
                         Item           => Display_Item (Item),
                         Component      => Component,
                         Component_Name => Component_Name,
                         Mode           => Value,
                         Format         => Binary,
                         Zoom           => 100));
         Append (Submenu, Radio);
      end if;

      --  Display a separator

      Gtk_New (Sep);
      Append (Menu, Sep);

      --  Display "Toggle auto-refresh" option

      Gtk_New (Check, "Auto refresh");
      Set_Active (Check, Get_Auto_Refresh (Display_Item (Item)));
      Item_Handler.Connect
        (Check, Signal_Activate,
         Item_Handler.To_Marshaller (Toggle_Refresh_Mode'Access),
         Item_Record'(Name_Length    => Component_Name'Length,
                      Canvas         => Canvas,
                      Item           => Display_Item (Item),
                      Component      => Component,
                      Component_Name => Component_Name,
                      Mode           => Value,
                      Format         => Default_Format,
                      Zoom           => 100));
      Append (Menu, Check);
   end Item_Contextual_Menu;

   ----------------------
   -- Get_Item_Context --
   ----------------------

   function Get_Item_Context
     (Debugger : access Visual_Debugger_Record'Class)
      return Items.Drawing_Context is
   begin
      return GVD_Canvas (Debugger.Data).Item_Context;
   end Get_Item_Context;

   ---------------------
   -- Get_Box_Context --
   ---------------------

   function Get_Box_Context
     (Debugger : access Visual_Debugger_Record'Class)
      return Box_Drawing_Context is
   begin
      return GVD_Canvas (Debugger.Data).Box_Context;
   end Get_Box_Context;

   --------------
   -- Hide_All --
   --------------

   procedure Hide_All
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record)
   is
      pragma Unreferenced (Widget);
   begin
      Set_Visibility (Item.Component, False, Recursive => True);
      Update_Resize_Display (Item.Item, True);
   end Hide_All;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record)
   is
      S : constant String :=
        Simple_Entry_Dialog
        (Parent   => Get_Debugger (Item.Item).Window,
         Title    => -"Setting value of " & Item.Component_Name,
         Message  => -"Setting value of " & Item.Component_Name & ':',
         Position => Win_Pos_Mouse,
         History  => Get_History (Get_Kernel (Item.Canvas)),
         Key      => "gvd_set_value_dialog");

   begin
      if S /= "" and then S (S'First) /= ASCII.NUL then
         Set_Variable
           (Get_Debugger (Item.Item).Debugger, Item.Component_Name, S);
         Update_Variable (Widget, Item);
      end if;

   exception
      when E : others => Traces.Trace (Exception_Handle, E);
   end Set_Value;

   --------------
   -- Show_All --
   --------------

   procedure Show_All
     (Widget : access Gtk_Widget_Record'Class;
      Item   : Item_Record)
   is
      pragma Unreferenced (Widget);
   begin
      Set_Visibility (Item.Component, True, Recursive => True);
      Update_Resize_Display (Item.Item, True);

   exception
      when E : others => Traces.Trace (Exception_Handle, E);
   end Show_All;

   ---------------------
   -- Dereference_All --
   ---------------------

   procedure Dereference_All
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record)
   is
      pragma Unreferenced (Widget);
      Lang : constant Language_Access := Get_Language (Item.Item);

      procedure Dereference_In_Comp
        (Component : Generic_Type_Access; Name : String);
      --  Dereference recursively all pointers found in Component

      procedure Dereference_In_Comp
        (Component : Generic_Type_Access; Name : String)
      is
         Iter : Generic_Iterator'Class := Start (Component);
         Comp : Generic_Type_Access;
      begin
         while not At_End (Iter) loop
            Comp := Data (Iter);
            if Comp.all in Access_Type'Class then
               Dereference_Item
                 (Item            => Item.Item,
                  Deref_Component => Comp,
                  Component_Name  => Get_Component_Name
                    (Component, Lang, Name, Comp),
                  Link_Name       => Get_Component_Name
                    (Component, Lang, Item_Name_In_Link, Comp));
            else
               Dereference_In_Comp
                 (Comp, Get_Component_Name (Component, Lang, Name, Comp));
            end if;

            Next (Iter);
         end loop;
      end Dereference_In_Comp;

   begin
      Dereference_In_Comp (Get_Entity (Item.Item), Get_Name (Item.Item));
   exception
      when E : others =>
         Traces.Trace (Exception_Handle, E);
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
      Display_Memory
        (Kernel => Get_Kernel (Item.Canvas),
         Address => Item.Component_Name);
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
      Display_Items.Update (Item.Item, Redisplay_Canvas => True);

   exception
      when E : others => Traces.Trace (Exception_Handle, E);
   end Update_Variable;

   --------------------
   -- Undisplay_Item --
   --------------------

   procedure Undisplay_Item
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record)
   is
      pragma Unreferenced (Widget);
   begin
      Process_User_Command
        (Get_Debugger (Item.Item),
         "graph undisplay" & Integer'Image (Get_Num (Item.Item)),
         Output_Command => True);

   exception
      when E : others => Traces.Trace (Exception_Handle, E);
   end Undisplay_Item;

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
         not Get_Auto_Refresh (Item.Item),
         True);

   exception
      when E : others => Traces.Trace (Exception_Handle, E);
   end Toggle_Refresh_Mode;

   --------------------
   -- On_Data_Window --
   --------------------

   procedure On_Data_Window
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Process : constant Visual_Debugger := Get_Current_Process
        (GPS_Window (Get_Main_Window (Kernel)));
   begin
      if Process /= null and then Process.Debugger /= null then
         Attach_To_Data_Window (Process, Create_If_Necessary => True);
      end if;
   exception
      when E : others => Traces.Trace (Exception_Handle, E);
   end On_Data_Window;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Debug          : constant String := '/' & (-"_Debug") & '/';
      Data_Sub       : constant String := Debug & (-"D_ata") & '/';
   begin
      Register_Menu (Kernel, Data_Sub, -"_Data Window", "",
                     On_Data_Window'Access, Ref_Item => -"Protection Domains");
      Canvas_Views.Register_Desktop_Functions (Kernel);
   end Register_Module;

end GVD.Canvas;
