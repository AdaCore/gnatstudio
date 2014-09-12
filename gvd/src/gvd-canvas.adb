------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2014, AdaCore                     --
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
with Gdk.Event;               use Gdk.Event;
with Gdk.RGBA;                use Gdk.RGBA;
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
with Gtkada.Canvas_View;      use Gtkada.Canvas_View;
with Gtkada.Canvas_View.Views; use Gtkada.Canvas_View.Views;
with Gtkada.Handlers;         use Gtkada.Handlers;
with Gtkada.MDI;              use Gtkada.MDI;
with Gtkada.Style;            use Gtkada.Style;
with Pango.Font;              use Pango.Font;

with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Browsers.Canvas;        use Browsers, Browsers.Canvas;
with Commands.Interactive;   use Commands, Commands.Interactive;
with Debugger;               use Debugger;
with Display_Items;          use Display_Items;
with GNAT.Regpat;            use GNAT.Regpat;
with GNAT.Strings;           use GNAT.Strings;
with GNATCOLL.Utils;         use GNATCOLL.Utils;
with GPS.Intl;               use GPS.Intl;
with GPS.Properties;         use GPS.Properties;
with GPS.Kernel;             use GPS.Kernel;
with GPS.Kernel.Actions;     use GPS.Kernel.Actions;
with GPS.Kernel.Hooks;       use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;         use GPS.Kernel.MDI;
with GPS.Kernel.Modules;     use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;  use GPS.Kernel.Modules.UI;
with GPS.Kernel.Properties;  use GPS.Kernel.Properties;
with GPS.Main_Window;        use GPS.Main_Window;
with GVD.Generic_View;
with GVD.Memory_View;        use GVD.Memory_View;
with GVD.Menu;               use GVD.Menu;
with GVD_Module;             use GVD_Module;
with GVD.Preferences;        use GVD.Preferences;
with GVD.Scripts;            use GVD.Scripts;
with GVD.Trace;
with GVD.Types;
with Histories;              use Histories;
with Items;                  use Items;
with Items.Simples;          use Items.Simples;
with Pixmaps_IDE;            use Pixmaps_IDE;
with Std_Dialogs;            use Std_Dialogs;
with String_Utils;           use String_Utils;
with GNATCOLL.Traces;        use GNATCOLL.Traces;
with XML_Utils;              use XML_Utils;

package body GVD.Canvas is
   Me : constant Trace_Handle := Create ("Canvas");

   Detect_Aliases : constant History_Key := "gvd-detect-aliases";

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

   package Browser_Views is new GVD.Generic_View
     (Base_Type                     => Debugger_Data_View_Record,
      Base_Type_Access              => Debugger_Data_View,
      Visual_Debugger_Record        => GVD.Process.Visual_Debugger_Record,
      Visual_Debugger               => GVD.Process.Visual_Debugger);

   type GVD_Canvas_Record is new Browser_Views.Process_View_Record with
      record
         Item_Num           : Integer := 0;
      end record;
   type GVD_Canvas is access all GVD_Canvas_Record'Class;

   overriding procedure Create_Menu
     (View    : not null access GVD_Canvas_Record;
      Menu    : not null access Gtk.Menu.Gtk_Menu_Record'Class);

   type Preferences_Hook_Record is new Function_With_Args with record
      Canvas : GVD_Canvas;
   end record;
   type Preferences_Hook is access all Preferences_Hook_Record'Class;
   overriding procedure Execute
     (Hook   : Preferences_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
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

   procedure Set_Canvas
     (Process : access Visual_Debugger_Record'Class;
      Canvas  : Debugger_Data_View);
   function Get_Canvas
     (Process : access Visual_Debugger_Record'Class)
      return Debugger_Data_View;
   --  Return the canvas on which the drawing is done

   package Canvas_Views is new Browser_Views.Simple_Views
     (Module_Name        => "Debugger_Data",
      View_Name          => "Debugger Data",
      Formal_View_Record => GVD_Canvas_Record,
      Get_View           => Get_Canvas,
      Set_View           => Set_Canvas,
      Group              => Group_Graphs,
      Position           => Position_Top,
      Local_Config       => True,
      Local_Toolbar      => True,
      Initialize         => Initialize);

   type Data_Window_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Data_Window_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Debug->Data->Data Window

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

   procedure Change_Detect_Aliases (Browser : access Gtk_Widget_Record'Class);
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
     (Hook   : Preferences_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      pragma Unreferenced (Kernel, Data);
      Canvas : constant access GVD_Canvas_Record'Class := Hook.Canvas;
      Styles : constant access Browser_Styles := Get_Styles (Canvas.Get_View);

      procedure On_Item (Item : not null access Abstract_Item_Record'Class);
      procedure On_Item (Item : not null access Abstract_Item_Record'Class) is
      begin
         Display_Item (Item).Update_Display;
      end On_Item;

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

         --   ??? Should have saved position in the desktop
         Terminate_Animation (Process.Data.Get_View);
         Process.Data.Get_View.Scale_To_Fit
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
     (Canvas  : access GVD_Canvas_Record;
      Process : access Visual_Debugger_Record'Class)
   is
      Property : GVD_Items_Property_Record;
      Found    : Boolean;
   begin
      Browser_Model (Canvas.Get_View.Model).Clear;
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
      return Debugger_Data_View is
   begin
      return Process.Data;
   end Get_Canvas;

   ----------------
   -- Set_Canvas --
   ----------------

   procedure Set_Canvas
     (Process : access Visual_Debugger_Record'Class;
      Canvas  : Debugger_Data_View)
   is
      Old      : constant GVD_Canvas := GVD_Canvas (Process.Data);
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

      if Old /= null and then Process.Debugger /= null then
         if Preserve_State_On_Exit.Get_Pref then
            Old.Get_View.Model.For_Each_Item
              (On_Item'Access, Filter => Kind_Item);

            if Property = null then
               Remove_Property
                 (Kernel => Get_Kernel (Old),
                  File   => Get_Executable (Process.Debugger),
                  Name   => "debugger_items");
            else
               GNATCOLL.Traces.Trace (Me, "Saving debugger canvas properties");
               Set_Property
                 (Kernel     => Get_Kernel (Old),
                  File       => Get_Executable (Process.Debugger),
                  Name       => "debugger_items",
                  Property   => Property,
                  Persistent => True);
            end if;
         end if;
      end if;

      --  If we are detaching, clear the old view
      if Canvas = null
        and then Process.Data /= null
      then
         Browser_Model (Process.Data.Get_View.Model).Clear;
      end if;

      Process.Data := Canvas;
   end Set_Canvas;

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
      Num       : Integer := -1;
      Entity    : Generic_Type_Access;
      Rect      : Model_Rectangle;
      Pos       : Point := No_Position;

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
               Link_From := Find_Item (Process.Data, Num);
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
               when Constraint_Error => null;
            end;

            begin
               Pos.Y := Gdouble'Value
                 (Cmd (Matched (Graph_Cmd_Y_Paren).First
                  .. Matched (Graph_Cmd_Y_Paren).Last));
            exception
               when Constraint_Error => null;
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
            Is_Dereference => Link_From /= null,
            Num            => Num);

         if Link_From /= null then
            Create_Link (Process.Data, Link_From, Item, Link_Name.all);
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
                    (Process.Data.Get_View,
                     Ref       => Link_From,
                     Items     => L,
                     Direction => Right,
                     Duration  => 0.3);
               end;
            else
               Rect := Process.Data.Get_View.Model.Bounding_Box;
               Pos.X := Rect.X + Rect.Width + Default_Space_Between_Items;
               Pos.Y := Rect.Y;
               Item.Set_Position (Pos);
            end if;
         end if;

         Process.Data.Get_View.Model.Refresh_Layout;  --  recompute links
         Process.Data.Get_View.Scroll_Into_View (Item, Duration => 0.3);

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
                  Item := Find_Item (Process.Data, Num);
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
                     Item := Find_Item (Process.Data, Num);
                     if Item /= null then
                        Process.Data.Get_View.Model.Remove (Item);
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
           (Process.Window.Kernel, (-" Error while processing: ") & Cmd);
   end Process_Graph_Cmd;

   -------------------------
   -- Refresh_Data_Window --
   -------------------------

   procedure Refresh_Data_Window
     (Debugger : access GVD.Process.Visual_Debugger_Record'Class) is
   begin
      if Debugger.Data /= null then
         Debugger.Data.Get_View.Model.Refresh_Layout;
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
      Check : Gtk_Check_Menu_Item;
   begin
      General_Browser_Record (View.all).Create_Menu (Menu);  --  inherited

      Gtk_New (Check, Label => -"Detect aliases");
      Check.Set_Tooltip_Text
        (-("When two variables are at the same location in memory, a single it"
           & ", a single box is displayed if alias detection is enabled."));
      Associate (Get_History (View.Kernel).all, Detect_Aliases, Check);
      Append (Menu, Check);
      Widget_Callback.Object_Connect
        (Check, Signal_Toggled, Change_Detect_Aliases'Access, View);
   end Create_Menu;

   ---------------------------
   -- Change_Detect_Aliases --
   ---------------------------

   procedure Change_Detect_Aliases
     (Browser : access Gtk_Widget_Record'Class)
   is
      View : constant GVD_Canvas := GVD_Canvas (Browser);
   begin
      Recompute_All_Aliases (Get_Process (View), Recompute_Values => True);
      View.Get_View.Model.Refresh_Layout;  --  for links
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
      return Boolean is
   begin
      return Get_History
        (Get_History (Get_Kernel (Process)).all, Detect_Aliases);
   end Get_Detect_Aliases;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (Canvas : access GVD_Canvas_Record'Class;
      Kernel : access Kernel_Handle_Record'Class) return Gtk_Widget
   is
      Hook            : Preferences_Hook;
   begin
      Assert (Me, Canvas.Kernel /= null,
              "Canvas' kernel not initialized");
      Browsers.Canvas.Initialize (Canvas);
      Canvas.Get_View.Model.Set_Selection_Mode (Selection_Single);

      Register_Contextual_Menu
        (Kernel          => Canvas.Kernel,
         Event_On_Widget => Canvas,
         Object          => Canvas,
         ID              => Debugger_Module_ID,
         Context_Func    => GVD_Canvas_Context_Factory'Access);

      Canvas.Hidden_Pixmap  := Gdk.Pixbuf.Gdk_New_From_Xpm_Data (box_xpm);
      Canvas.Unknown_Pixmap := Gdk.Pixbuf.Gdk_New_From_Xpm_Data (trash_xpm);

      Hook := new Preferences_Hook_Record'
        (Function_With_Args with Canvas => GVD_Canvas (Canvas));
      Add_Hook
        (Canvas.Kernel, Preference_Changed_Hook, Hook,
         Name  => "canvas.preferences_changed",
         Watch => GObject (Canvas));

      Hook.Execute (Kernel, null);

      return Gtk_Widget (Canvas);
   end Initialize;

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
            "graph display " & To_String (Item.Component.Name),
            Output_Command => True);
      else
         Process_User_Command
           (Get_Debugger (Item.Item),
            "graph display `" & Get_Name (Item.Item) & "`",
            Output_Command => True);
      end if;
   end Clone_Component;

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
      Details : Canvas_Event_Details;
      Radio   : Gtk_Radio_Menu_Item;
      Submenu : Gtk_Menu;
      Item    : Display_Item;
      Base      : Item_Record :=
        Item_Record'(Canvas         => Canvas,
                     Item           => null,
                     Component      => null,
                     Mode           => Value,
                     Format         => Default_Format,
                     Zoom           => 100);
   begin
      Default_Browser_Context_Factory
        (Context, Kernel, Event_Widget, Object, Event, Menu);

      --   ??? Already computed in Default_Browser_Context_Factory
      Canvas.Get_View.Set_Details (Details, Event.Button);

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

         if Base.Component /= null and then Is_A_Variable (Item) then
            declare
               Component_Name : constant String :=
                 To_String (Base.Component.Name);
            begin
               --  Display a separator

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

               --  Display a separator

               Gtk_New (Sep);
               Append (Menu, Sep);

               --  We can't clone an auto-refreshed item, since it would reuse
               --  the same box.
               if not Item.Get_Auto_Refresh then
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

               Gtk_New (Mitem, -"Set Value of " & Krunch (Component_Name));
               Item_Handler.Connect
                 (Mitem, Signal_Activate,
                  Item_Handler.To_Marshaller (Set_Value'Access), Base);
               Append (Menu, Mitem);
            end;
         end if;

         Gtk_New (Mitem, Label => -"Update Value");
         Item_Handler.Connect
           (Mitem, Signal_Activate,
            Item_Handler.To_Marshaller (Update_Variable'Access), Base);
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
               Item_Record'(Canvas         => Canvas,
                            Item           => Base.Item,
                            Component      => Base.Component,
                            others         => <>));
            Append (Submenu, Radio);

            Gtk_New (Radio, Get_Group (Radio), -"Show Type");
            Set_Active (Radio, Get_Display_Mode (Item) = Type_Only);
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
            Set_Active (Radio, Get_Display_Mode (Item) = Type_Value);
            Item_Handler.Connect
              (Radio, Signal_Activate,
               Item_Handler.To_Marshaller (Change_Display_Mode'Access),
               Item_Record'(Canvas         => Canvas,
                            Item           => Base.Item,
                            Component      => Base.Component,
                            Mode           => Type_Value,
                            others         => <>));
            Append (Submenu, Radio);

            --  Display a separator
            Gtk_New (Sep);
            Append (Submenu, Sep);

            Gtk_New (Radio, Widget_SList.Null_List, -"Default");
            Set_Active (Radio, Get_Format (Item) = Default_Format);
            Item_Handler.Connect
              (Radio, Signal_Activate,
               Item_Handler.To_Marshaller (Change_Format'Access),
               Item_Record'(Canvas         => Canvas,
                            Item           => Base.Item,
                            Component      => Base.Component,
                            others         => <>));
            Append (Submenu, Radio);

            Gtk_New (Radio, Get_Group (Radio), -"Decimal");
            Set_Active (Radio, Get_Format (Item) = Decimal);
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
            Set_Active (Radio, Get_Format (Item) = Hexadecimal);
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
            Set_Active (Radio, Get_Format (Item) = Octal);
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
            Set_Active (Radio, Get_Format (Item) = Binary);
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
         Set_Active (Check, Get_Auto_Refresh (Base.Item));
         Item_Handler.Connect
           (Check, Signal_Activate,
            Item_Handler.To_Marshaller (Toggle_Refresh_Mode'Access), Base);
         Append (Menu, Check);
      end if;
   end GVD_Canvas_Context_Factory;

   --------------
   -- Hide_All --
   --------------

   procedure Hide_All
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record)
   is
      pragma Unreferenced (Widget);
   begin
      Set_Visibility (Item.Component.Component, False, Recursive => True);
      Update_Display (Item.Item);
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
        Simple_Entry_Dialog
        (Parent   => Get_Debugger (Item.Item).Window,
         Title    => -"Setting value of " & Name,
         Message  => -"Setting value of " & Name & ':',
         Position => Win_Pos_Mouse,
         History  => Get_History (Get_Kernel (Item.Canvas)),
         Key      => "gvd_set_value_dialog");

   begin
      if S /= "" and then S (S'First) /= ASCII.NUL then
         Set_Variable (Get_Debugger (Item.Item).Debugger, Name, S);
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
   begin
      Set_Visibility (Item.Component.Component, True, Recursive => True);
      Item.Item.Update_Display;
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
            Dereference_Item
              (Item      => Item.Item,
               Component => Component_Item (Child));
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
      Display_Memory
        (Kernel  => Get_Kernel (Item.Canvas),
         Address => To_String (Item.Component.Name));
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
         not Get_Auto_Refresh (Item.Item),
         True);
   end Toggle_Refresh_Mode;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Data_Window_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      Process : constant Visual_Debugger := Get_Current_Process
        (GPS_Window (Get_Main_Window (Kernel)));
   begin
      if Process /= null and then Process.Debugger /= null then
         Attach_To_Data_Window (Process, Create_If_Necessary => True);
      end if;
      return Commands.Success;
   end Execute;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Register_Action
        (Kernel, "open debugger data window", new Data_Window_Command,
         Description => -"Open the Data Window for the debugger",
         Category => -"Views");

      Create_New_Boolean_Key_If_Necessary
        (Get_History (Kernel).all, Detect_Aliases, Default_Value => True);

      Canvas_Views.Register_Desktop_Functions (Kernel);
   end Register_Module;

end GVD.Canvas;
