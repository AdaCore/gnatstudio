-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2000-2005                      --
--                               AdaCore                             --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Gdk.Bitmap;          use Gdk.Bitmap;
with Gdk.Color;           use Gdk.Color;
with Gdk.Event;           use Gdk.Event;
with Gdk.GC;              use Gdk.GC;
with Gdk.Pixmap;          use Gdk.Pixmap;
with Gdk.Types.Keysyms;   use Gdk.Types.Keysyms;
with Gdk.Window;          use Gdk.Window;
with Gdk;                 use Gdk;
with Glib;                use Glib;
with Glib.Object;         use Glib.Object;
with Glib.Xml_Int;        use Glib.Xml_Int;
with Gtk.Accel_Group;     use Gtk.Accel_Group;
with Gtk.Check_Menu_Item; use Gtk.Check_Menu_Item;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Handlers;        use Gtk.Handlers;
with Gtk.Menu;            use Gtk.Menu;
with Gtk.Menu_Item;       use Gtk.Menu_Item;
with Gtk.Radio_Menu_Item; use Gtk.Radio_Menu_Item;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Widget;          use Gtk.Widget;
with Gtkada.Canvas;       use Gtkada.Canvas;
with Gtkada.Handlers;     use Gtkada.Handlers;
with Gtkada.MDI;          use Gtkada.MDI;
with Pango.Enums;         use Pango.Enums;
with Pango.Font;          use Pango.Font;
with Pango.Layout;        use Pango.Layout;

with Ada.Exceptions;         use Ada.Exceptions;
with Basic_Types;            use Basic_Types;
with Debugger;               use Debugger;
with Display_Items;          use Display_Items;
with GNAT.Regpat;            use GNAT.Regpat;
with GPS.Intl;               use GPS.Intl;
with GPS.Kernel.MDI;         use GPS.Kernel.MDI;
with GPS.Kernel.Modules;     use GPS.Kernel.Modules;
with GPS.Kernel.Preferences; use GPS.Kernel.Preferences;
with GPS.Kernel;             use GPS.Kernel;
with GPS.Main_Window;        use GPS.Main_Window;
with GVD.Memory_View;        use GVD.Memory_View;
with GVD.Menu;               use GVD.Menu;
with GVD.Preferences;        use GVD.Preferences;
with GVD.Process;            use GVD.Process;
with GVD.Trace;
with GVD.Types;
with GVD_Module;             use GVD_Module;
with Items;                  use Items;
with Items.Simples;          use Items.Simples;
with Pixmaps_IDE;            use Pixmaps_IDE;
with Std_Dialogs;            use Std_Dialogs;
with String_Utils;           use String_Utils;
with Traces;                 use Traces;

package body GVD.Canvas is
   Me : constant Debug_Handle := Create ("Canvas");

   Graph_Cmd_Format : constant Pattern_Matcher := Compile
     ("graph\s+(print|display)\s+(`([^`]+)`|""([^""]+)"")?(.*)",
      Case_Insensitive);
   --  Format of the graph print commands, and how to parse them

   Graph_Cmd_Type_Paren          : constant := 1;
   Graph_Cmd_Expression_Paren    : constant := 3;
   Graph_Cmd_Quoted_Paren        : constant := 4;
   Graph_Cmd_Rest_Paren          : constant := 5;
   --  Indexes of the parentheses pairs in Graph_Cmd_Format for each of the
   --  relevant fields.

   Graph_Cmd_Dependent_Format : constant Pattern_Matcher := Compile
     ("\s+dependent\s+on\s+(\d+)\s*", Case_Insensitive);
   --  Partial analyses of the last part of a graph command

   Graph_Cmd_Link_Format : constant Pattern_Matcher := Compile
     ("\s+link_name\s+(.+)", Case_Insensitive);
   --  Partial analyses of the last part of a graph command

   Graph_Cmd_Format2 : constant Pattern_Matcher := Compile
     ("graph\s+(enable|disable)\s+display\s+(.*)", Case_Insensitive);
   --  Second possible set of commands.

   Graph_Cmd_Format3 : constant Pattern_Matcher := Compile
     ("graph\s+undisplay\s+(.*)", Case_Insensitive);
   --  Third possible set of commands

   type GVD_Canvas_Record is new Gtk_Scrolled_Window_Record with record
      Kernel         : Kernel_Handle;
      Canvas         : Interactive_Canvas;
      Detect_Aliases : Boolean;
      Item_Num       : Integer := 0;
      Process        : Visual_Debugger;
      --  The process tab that contains the canvas

      --  The graphic contexts used to draw the canvas and its items
      Item_Context    : Items.Drawing_Context;
      Box_Context     : Box_Drawing_Context;
      Tooltip_Context : Items.Drawing_Context;

      Contextual_Background_Menu : Gtk.Menu.Gtk_Menu;
      Item_Contextual_Menu       : Gtk.Menu.Gtk_Menu;
   end record;
   type GVD_Canvas is access all GVD_Canvas_Record'Class;

   function Create_Data_Window
     (Kernel : access Kernel_Handle_Record'Class)
      return MDI_Child;
   --  Create a new data window in the MDI

   procedure Init_Graphics
     (Process : access Visual_Debugger_Record'Class);
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
     (Process : access Visual_Debugger_Record'Class);
   --  Called when the preferences have changed, and the canvas should be
   --  redisplayed with the new setup.

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Kernel : Kernel_Handle) return Glib.Xml_Int.Node_Ptr;
   function Load_Desktop
     (MDI    : MDI_Window;
      Node   : Glib.Xml_Int.Node_Ptr;
      Kernel : Kernel_Handle) return MDI_Child;
   --  Handling of desktops

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

   Zoom_Levels : constant array (Positive range <>) of Guint :=
     (15, 25, 50, 75, 100, 150, 200, 300, 400);
   --  All the possible zoom levels. We have to use such an array, instead
   --  of doing the computation directly, so as to avoid rounding errors that
   --  would appear in the computation and make zoom_in not the reverse of
   --  zoom_out.

   Zoom_Steps : constant := 3;
   --  Number of steps while zooming in or out.

   --------------------
   -- Local Packages --
   --------------------

   package Check_Canvas_Handler is new Gtk.Handlers.User_Callback
     (Gtk_Check_Menu_Item_Record, GVD_Canvas);

   package Item_Handler is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record, Item_Record);

   ----------------------
   -- Local Procedures --
   ----------------------

   procedure Change_Align_On_Grid
     (Item   : access Gtk_Check_Menu_Item_Record'Class;
      Canvas : GVD_Canvas);
   --  Callback for the "align on grid" contextual menu item.

   procedure Change_Detect_Aliases
     (Item   : access Gtk_Check_Menu_Item_Record'Class;
      Canvas : GVD_Canvas);
   --  Callback for the "detect aliases" contextual menu item.

   procedure Change_Display_Mode
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record);
   --  Change the mode of a specific item to indicate whether the value of the
   --  item should be displayed.

   procedure Change_Format
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record);
   --  Change the display format of a specific item.

   procedure Clone_Component
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record);
   --  Clone the item or its selected component.

   procedure Display_Expression (Canvas : access Gtk_Widget_Record'Class);
   --  Popup a dialog to display any expression in the canvas

   procedure Zoom_In (Canvas : access Gtk_Widget_Record'Class);
   --  Zoom in to the previous zoom level, if any

   procedure Zoom_Out (Canvas : access Gtk_Widget_Record'Class);
   --  Zoom out to the next zoom level, if any

   procedure Zoom_Level
     (Mitem  : access Gtk_Widget_Record'Class;
      Item   : Item_Record);
   --  Zoom directly to a specific level (Item.Zoom)

   procedure Hide_All
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record);
   --  Hide all the subcomponents of the selected item.

   procedure Set_Value
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record);
   --  Set the value for a specific component

   procedure Show_All
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record);
   --  Show all the subcomponents of the selected item.

   procedure View_Into_Memory
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record);
   --  Bring up the memory view if needed, and view the memory at the address
   --  corresponding to Item.

   procedure Update_Variable
     (Widget : access Gtk_Widget_Record'Class;
      Item   : Item_Record);
   --  Callback for the "update value" contextual menu item.

   procedure Undisplay_Item
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record);
   --  Hide all the subcomponents of the selected item.

   procedure Toggle_Refresh_Mode
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record);
   --  Toggle between "auto_refresh" and "frozen" modes.

   procedure Allocate_Fonts (Canvas : access GVD_Canvas_Record'Class);
   --  Reallocate all the fonts, with the appropriate size given the current
   --  zoom

   function Key_Press
     (Canvas : access Gtk_Widget_Record'Class; Event : Gdk_Event)
      return Boolean;
   --  Handle key release events

   function Contextual_Background_Menu_Destroy
     (Widget : access Gtk_Widget_Record'Class) return Boolean;
   --  Called when the contexual menu is destroyed.

   procedure On_Data_Canvas_Destroy
     (Canvas : access Gtk_Widget_Record'Class);
   --  Called when the data canvas is destroyed

   procedure On_Debugger_Terminate
     (Process : access GObject_Record'Class);
   --  Called when the debugger is terminated

   procedure On_Data_Refresh (Canvas : access Gtk_Widget_Record'Class);
   --  "Refresh" contextual menu

   procedure On_Background_Click
     (Canvas : access Glib.Object.GObject_Record'Class;
      Event  : Gdk.Event.Gdk_Event);
   --  Called for clicks in the background of the canvas.

   ----------------
   -- Get_Canvas --
   ----------------

   function Get_Canvas
     (Process : access GVD.Process.Visual_Debugger_Record'Class)
      return Gtkada.Canvas.Interactive_Canvas
   is
   begin
      if Process.Data = null then
         return null;
      else
         return GVD_Canvas (Process.Data).Canvas;
      end if;
   end Get_Canvas;

   -----------------------
   -- Process_Graph_Cmd --
   -----------------------

   procedure Process_Graph_Cmd
     (Process : access Visual_Debugger_Record'Class;
      Cmd     : String)
   is
      Matched   : Match_Array (0 .. 10);
      Matched2  : Match_Array (0 .. 10);
      Item      : Display_Item;
      Index,
      Last      : Positive;
      Enable    : Boolean;
      First     : Natural;
      Link_Name : Basic_Types.String_Access;
      Link_From : Display_Item;
      Dependent_On_First : Natural := Natural'Last;
      Link_Name_First    : Natural := Natural'Last;

   begin
      --  graph (print|display) expression [dependent on display_num]
      --        [link_name name]
      --  graph (print|display) `command`
      --  graph enable display display_num [display_num ...]
      --  graph disable display display_num [display_num ...]
      --  graph undisplay display_num

      Match (Graph_Cmd_Format, Cmd, Matched);

      if Matched (0) /= No_Match then
         Attach_To_Data_Window (Process, Create_If_Necessary => True);
         Enable := Cmd (Matched (Graph_Cmd_Type_Paren).First) = 'd'
           or else Cmd (Matched (Graph_Cmd_Type_Paren).First) = 'D';

         --  Do we have any 'dependent on' expression ?

         if Matched (Graph_Cmd_Rest_Paren).First >= Cmd'First then
            Match (Graph_Cmd_Dependent_Format,
                   Cmd (Matched (Graph_Cmd_Rest_Paren).First
                        .. Matched (Graph_Cmd_Rest_Paren).Last),
                   Matched2);

            if Matched2 (1) /= No_Match then
               Dependent_On_First := Matched2 (0).First;
               Link_From := Find_Item
                 (GVD_Canvas (Process.Data).Canvas,
                  Integer'Value
                  (Cmd (Matched2 (1).First .. Matched2 (1).Last)));
            end if;
         end if;

         --  Do we have any 'link name' expression ?

         if Matched (Graph_Cmd_Rest_Paren).First >= Cmd'First then
            Match (Graph_Cmd_Link_Format,
                   Cmd (Matched (Graph_Cmd_Rest_Paren).First
                        .. Matched (Graph_Cmd_Rest_Paren).Last),
                   Matched2);

            if Matched2 (0) /= No_Match then
               Link_Name_First := Matched2 (0).First;
               Link_Name := new String'
                 (Cmd (Matched2 (1).First .. Matched2 (1).Last));
            end if;
         end if;

         --  A general expression (graph print `cmd`)
         if Matched (Graph_Cmd_Expression_Paren) /= No_Match then
            declare
               Expr : constant String := Cmd
                 (Matched (Graph_Cmd_Expression_Paren).First ..
                  Matched (Graph_Cmd_Expression_Paren).Last);
               Entity : constant Items.Generic_Type_Access :=
                 New_Debugger_Type (Expr);

            begin
               Set_Value
                 (Debugger_Output_Type (Entity.all),
                  Send
                    (Process.Debugger,
                     Refresh_Command (Debugger_Output_Type (Entity.all)),
                     Mode => GVD.Types.Internal));

               --  No link ?

               if Dependent_On_First = Natural'Last then
                  Gtk_New
                    (Item,
                     Variable_Name  => Expr,
                     Debugger       => Process,
                     Auto_Refresh   => Enable,
                     Default_Entity => Entity);
                  Put (GVD_Canvas (Process.Data).Canvas, Item);

               else
                  Gtk_New
                    (Item,
                     Variable_Name  => Expr,
                     Debugger       => Process,
                     Auto_Refresh   => Enable,
                     Default_Entity => Entity,
                     Link_From      => Link_From,
                     Link_Name      => Link_Name.all);
               end if;

               if Item /= null then
                  Show_Item (GVD_Canvas (Process.Data).Canvas, Item);
               end if;
            end;

         --  A quoted name or standard name

         else
            --  Quoted

            if Matched (Graph_Cmd_Quoted_Paren) /= No_Match then
               First := Matched (Graph_Cmd_Quoted_Paren).First;
               Last  := Matched (Graph_Cmd_Quoted_Paren).Last;

            --  Standard

            else
               First := Matched (Graph_Cmd_Rest_Paren).First;
               Last  := Natural'Min (Link_Name_First, Dependent_On_First);

               if Last = Natural'Last then
                  Last := Matched (Graph_Cmd_Rest_Paren).Last;
               else
                  Last := Last - 1;
               end if;
            end if;

            --  If we don't want any link:

            if Dependent_On_First = Natural'Last then
               Gtk_New
                 (Item,
                  Variable_Name => Cmd (First .. Last),
                  Debugger      => Process,
                  Auto_Refresh  =>
                    Cmd (Matched (Graph_Cmd_Type_Paren).First) = 'd');

               if Item /= null then
                  Put (GVD_Canvas (Process.Data).Canvas, Item);
                  Show_Item (GVD_Canvas (Process.Data).Canvas, Item);
                  Recompute_All_Aliases
                    (Process,
                     Recompute_Values => False);
               end if;

            --  Else if we have a link

            else
               if Link_Name = null then
                  Link_Name := new String'(Cmd (First .. Last));
               end if;

               Gtk_New
                 (Item,
                  Variable_Name => Cmd (First .. Last),
                  Debugger      => Process,
                  Auto_Refresh  => Enable,
                  Link_From     => Link_From,
                  Link_Name     => Link_Name.all);

               if Item /= null then
                  Show_Item (GVD_Canvas (Process.Data).Canvas, Item);
               end if;
            end if;
         end if;

         Free (Link_Name);

      else
         --  Is this an enable/disable command ?

         Match (Graph_Cmd_Format2, Cmd, Matched);

         if Matched (2) /= No_Match then
            Attach_To_Data_Window (Process, Create_If_Necessary => True);

            Index := Matched (2).First;
            Enable := Cmd (Matched (Graph_Cmd_Type_Paren).First) = 'e'
              or else Cmd (Matched (Graph_Cmd_Type_Paren).First) = 'E';

            while Index <= Cmd'Last loop
               Last := Index;
               Skip_To_Blank (Cmd, Last);
               Set_Auto_Refresh
                 (Find_Item
                    (GVD_Canvas (Process.Data).Canvas,
                     Integer'Value (Cmd (Index .. Last - 1))),
                  Enable,
                  Update_Value => True);
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
                  Free
                    (Find_Item
                      (GVD_Canvas (Process.Data).Canvas,
                       Integer'Value (Cmd (Index .. Last - 1))));
                  Index := Last + 1;
                  Skip_Blanks (Cmd, Index);
               end loop;
            end if;
         end if;
      end if;

   exception
      when Constraint_Error =>
         --  Usually because Find_Item returned a null value.
         GVD.Trace.Output_Error
           (GPS_Window (Process.Window).Kernel,
            (-" Error while processing: ") & Cmd);
   end Process_Graph_Cmd;

   -------------------------
   -- Refresh_Data_Window --
   -------------------------

   procedure Refresh_Data_Window
     (Debugger : access GVD.Process.Visual_Debugger_Record'Class) is
   begin
      Refresh_Canvas (GVD_Canvas (Debugger.Data).Canvas);
   end Refresh_Data_Window;

   ---------------------
   -- On_Data_Refresh --
   ---------------------

   procedure On_Data_Refresh (Canvas : access Gtk_Widget_Record'Class) is
      C : constant GVD_Canvas := GVD_Canvas (Canvas);
      Iter    : Item_Iterator;
      Item    : Canvas_Item;

   begin
      Iter := Start (C.Canvas);
      loop
         Item := Get (Iter);
         exit when Item = null;

         Display_Items.Update (Display_Item (Item), Redisplay_Canvas => False);

         Next (Iter);
      end loop;

      Refresh_Canvas (C.Canvas);

   exception
      when E : others =>
         Traces.Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Data_Refresh;

   ---------------------------
   -- On_Debugger_Terminate --
   ---------------------------

   procedure On_Debugger_Terminate
     (Process : access GObject_Record'Class)
   is
      P : constant Visual_Debugger := Visual_Debugger (Process);
   begin
      --  Are we killing the last debugger ? If yes, update the preference to
      --  automatically restart the call stack if appropriate next time

--        if Get_Debugger_List (GPS_Window (P.Window).Kernel).Next = null then
--           Set_Pref (GPS_Window (P.Window).Kernel,
--                     Show_Call_Stack, P.Stack /= null);
--        end if;

      if P.Data /= null then
         Close (GPS_Window (P.Window).MDI, P.Data);
      end if;
   end On_Debugger_Terminate;

   ----------------------------
   -- On_Data_Canvas_Destroy --
   ----------------------------

   procedure On_Data_Canvas_Destroy
     (Canvas : access Gtk_Widget_Record'Class) is
   begin
      if GVD_Canvas (Canvas).Process /= null then
         GVD_Canvas (Canvas).Process.Data := null;
         GVD_Canvas (Canvas).Process.Selected_Item := null;
         GVD_Canvas (Canvas).Process.Selected_Component := null;
         GVD_Canvas (Canvas).Process := null;
      end if;
   end On_Data_Canvas_Destroy;

   --------------------------
   -- Change_Align_On_Grid --
   --------------------------

   procedure Change_Align_On_Grid
     (Item   : access Gtk_Check_Menu_Item_Record'Class;
      Canvas : GVD_Canvas) is
   begin
      Align_On_Grid (Canvas.Canvas, Get_Active (Item));

   exception
      when E : others =>
         Traces.Trace (Exception_Handle,
                       "Unexpected exception: " & Exception_Information (E));
   end Change_Align_On_Grid;

   ---------------------------
   -- Change_Detect_Aliases --
   ---------------------------

   procedure Change_Detect_Aliases
     (Item    : access Gtk_Check_Menu_Item_Record'Class;
      Canvas  : GVD_Canvas)
   is
      pragma Unreferenced (Item);
   begin
      Set_Detect_Aliases (Canvas, not Canvas.Detect_Aliases);

      --  Recompute all the aliases
      Recompute_All_Aliases (Canvas.Process);

      Refresh_Data_Window (Canvas.Process);

   exception
      when E : others =>
         Traces.Trace (Exception_Handle,
                       "Unexpected exception: " & Exception_Information (E));
   end Change_Detect_Aliases;

   ------------------------
   -- Display_Expression --
   ------------------------

   procedure Display_Expression (Canvas : access Gtk_Widget_Record'Class) is
   begin
      Display_Expression (GVD_Canvas (Canvas).Process);
   exception
      when E : others =>
         Traces.Trace (Exception_Handle,
                       "Unexpected exception: " & Exception_Information (E));
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

   ---------------
   -- Key_Press --
   ---------------

   function Key_Press
     (Canvas : access Gtk_Widget_Record'Class; Event : Gdk_Event)
      return Boolean is
   begin
      case Get_Key_Val (Event) is
         when GDK_equal => Zoom_In (Canvas);
         when GDK_minus => Zoom_Out (Canvas);
         when others    => null;
      end case;

      return False;
   end Key_Press;

   -------------------------
   -- On_Background_Click --
   -------------------------

   procedure On_Background_Click
     (Canvas : access Glib.Object.GObject_Record'Class;
      Event  : Gdk.Event.Gdk_Event)
   is
      C      : constant GVD_Canvas := GVD_Canvas (Canvas);
      Iter   : Item_Iterator;
   begin
      if Get_Button (Event) = 1 then
         Iter := Start (C.Canvas);
         if Get (Iter) /= null then
            Unselect_All (C.Process);
--            Select_Item (Display_Item (Get (Iter)), null);
         end if;

      elsif Get_Button (Event) = 3
        and then Get_Event_Type (Event) = Button_Press
      then
         Popup
           (Contextual_Background_Menu (C.Process),
            Button            => Get_Button (Event),
            Activate_Time     => Get_Time (Event));
      end if;

   exception
      when E : others =>
         Traces.Trace (Exception_Handle,
                       "Unexpected exception: " & Exception_Information (E));
   end On_Background_Click;

   ------------------------
   -- Create_Data_Window --
   ------------------------

   function Create_Data_Window
     (Kernel : access Kernel_Handle_Record'Class)
      return MDI_Child
   is
      Child           : GPS_MDI_Child;
      Data            : GVD_Canvas;
      Annotation_Font : Pango_Font_Description;
   begin
      Data := new GVD_Canvas_Record;
      Gtk.Scrolled_Window.Initialize (Data);
      Set_Policy (Data, Policy_Automatic, Policy_Automatic);

      Data.Kernel         := Kernel_Handle (Kernel);
      Data.Detect_Aliases := Get_Pref (Default_Detect_Aliases);

      Gtk_New (Data.Canvas);
      Align_On_Grid (Data.Canvas, True);
      Add (Data, Data.Canvas);
      Add_Events (Data.Canvas, Key_Press_Mask);

      Gtkada.Handlers.Return_Callback.Connect
        (Data.Canvas, "key_press_event",
         Gtkada.Handlers.Return_Callback.To_Marshaller (Key_Press'Access));
      Object_Callback.Object_Connect
        (Data.Canvas, "background_click",
         Object_Callback.To_Marshaller (On_Background_Click'Access),
         Data);
      Widget_Callback.Object_Connect
        (Data.Canvas, "destroy",
         On_Data_Canvas_Destroy'Access, Data);

      --  Initialize the canvas

      Annotation_Font := Copy (Get_Pref (GPS.Kernel.Preferences.Default_Font));
      Set_Size
        (Annotation_Font,
         Gint'Max (Pango_Scale,
           Get_Size (Annotation_Font) - 2 * Pango_Scale));
      Configure (Data.Canvas, Annotation_Font => Annotation_Font);
      Free (Annotation_Font);

      Gtk_New (Child, Data,
               Group          => Group_Graphs,
               Default_Height => 200,
               Module         => Debugger_Module_ID);
      Put (Get_MDI (Kernel), Child, Initial_Position => Position_Top);
      Set_Focus_Child (Child);

      return MDI_Child (Child);
   end Create_Data_Window;

   -------------------
   -- Init_Graphics --
   -------------------

   procedure Init_Graphics
     (Process : access Visual_Debugger_Record'Class)
   is
      Canvas : constant GVD_Canvas := GVD_Canvas (Process.Data);
      Win : constant Gdk.Window.Gdk_Window := Get_Window (Canvas);
   begin
      pragma Assert (Win /= null);
      if Canvas.Box_Context.Close_Pixmap = null then
         Create_From_Xpm_D
           (Canvas.Box_Context.Close_Pixmap, Win,
            Canvas.Box_Context.Close_Mask, Null_Color, cancel_xpm);
         Create_From_Xpm_D
           (Canvas.Box_Context.Locked_Pixmap, Win,
            Canvas.Box_Context.Locked_Mask, Null_Color, lock_xpm);
         Create_From_Xpm_D
           (Canvas.Box_Context.Auto_Display_Pixmap, Win,
            Canvas.Box_Context.Auto_Display_Mask, Null_Color,
            display_small_xpm);
         Create_From_Xpm_D
           (Canvas.Item_Context.Hidden_Pixmap, Win,
            Canvas.Item_Context.Hidden_Mask, Null_Color, box_xpm);
         Create_From_Xpm_D
           (Canvas.Item_Context.Unknown_Pixmap, Win,
            Canvas.Item_Context.Unknown_Mask, Null_Color, trash_xpm);

         GVD.Canvas.Preferences_Changed (Process);
      end if;
   end Init_Graphics;

   ---------------------------
   -- Attach_To_Data_Window --
   ---------------------------

   procedure Attach_To_Data_Window
     (Debugger : access GVD.Process.Visual_Debugger_Record'Class;
      Create_If_Necessary : Boolean)
   is
      MDI  : constant MDI_Window :=
        Get_MDI (Get_Kernel (Debugger_Module_ID.all));
      Iter : Child_Iterator;
      Child : MDI_Child;
      Data  : GVD_Canvas;
   begin
      if Debugger.Data = null then
         --  Do we have an existing unattached data window ?
         Iter := First_Child (MDI);
         loop
            Child := Get (Iter);
            exit when Child = null;

            if Get_Widget (Child).all in GVD_Canvas_Record'Class then
               Data := GVD_Canvas (Get_Widget (Child));
               if Data.Process = null then
                  Traces.Trace
                    (Me, "Debugger attached to existing data window");
                  Data.Process := Visual_Debugger (Debugger);
                  Debugger.Data := Gtk_Widget (Data);
                  exit;
               end if;
               Data := null;
            end if;
            Next (Iter);
         end loop;

         --  If none was found, create one

         if Child = null and then Create_If_Necessary then
            Traces.Trace (Me, "Creating new data window");
            Child := Create_Data_Window (Get_Kernel (Debugger_Module_ID.all));
            Data  := GVD_Canvas (Get_Widget (Child));
            Data.Process := Visual_Debugger (Debugger);
            Debugger.Data := Gtk_Widget (Data);
         end if;

         --  Initialize the pixmaps and colors for the canvas
         Realize (Data.Canvas);
         Init_Graphics (Debugger);

         if Child /= null then
            if Debugger.Debugger_Num = 1 then
               Set_Title (Child, -"Debugger Data");
            else
               Set_Title (Child, -"Debugger Data" & " <" &
                          Image (Debugger.Debugger_Num) & ">");
            end if;
         end if;

         if Debugger.Data /= null then
--              Gtkada.Handlers.Object_Callback.Object_Connect
--                (Debugger, "process_stopped",
--                 Update_Call_Stack'Access,
--                 Slot_Object => Debugger.Stack);
--              Object_Callback.Object_Connect
--                (Debugger, "context_changed",
--                 Update_Call_Stack'Access,
--                 Slot_Object => Debugger.Stack);

            Object_Callback.Object_Connect
              (Debugger.Debugger_Text, "destroy",
               On_Debugger_Terminate'Access,
               Slot_Object => Debugger);
         end if;

      else
         Child := Find_MDI_Child (MDI, Debugger.Data);
         if Child /= null then
            Raise_Child (Child);
         else
            Destroy (Debugger.Data);
            Debugger.Data := null;
         end if;
      end if;
   end Attach_To_Data_Window;

   -----------------------
   -- Get_Next_Item_Num --
   -----------------------

   function Get_Next_Item_Num
     (Debugger : access GVD.Process.Visual_Debugger_Record'Class)
      return Integer
   is
   begin
      GVD_Canvas (Debugger.Data).Item_Num :=
        GVD_Canvas (Debugger.Data).Item_Num + 1;
      return GVD_Canvas (Debugger.Data).Item_Num;
   end Get_Next_Item_Num;

   --------------------
   -- Allocate_Fonts --
   --------------------

   procedure Allocate_Fonts (Canvas : access GVD_Canvas_Record'Class) is
      Iter : Item_Iterator := Start (Canvas.Canvas);
      Item : Canvas_Item;
      Hide : constant Boolean := Get_Pref (Hide_Big_Items);
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
     (Process : access Visual_Debugger_Record'Class)
   is
      C   : constant GVD_Canvas := GVD_Canvas (Process.Data);
      Win : Gdk.Window.Gdk_Window;
      Item : Canvas_Item;
      Iter : Item_Iterator;
      Hide : constant Boolean := Get_Pref (Hide_Big_Items);

   begin
      Realize (C);
      Win := Get_Window (C);
      Set_Detect_Aliases
        (C, Get_Pref (Default_Detect_Aliases));
      Recompute_All_Aliases (Process);

      --  The drawing context for the items

      if C.Item_Context.GC /= null then
         Destroy (C.Item_Context.GC);
      end if;

      Gdk_New (C.Item_Context.GC, Win);
      Set_Foreground (C.Item_Context.GC, Black (Get_Default_Colormap));
      C.Tooltip_Context.GC := C.Item_Context.GC;

      if C.Item_Context.Xref_GC /= null then
         Destroy (C.Item_Context.Xref_GC);
      end if;

      Gdk_New (C.Item_Context.Xref_GC, Win);
      Set_Foreground
        (C.Item_Context.Xref_GC, Get_Pref (Xref_Color));
      C.Tooltip_Context.Xref_GC := C.Item_Context.Xref_GC;

      if C.Item_Context.Modified_GC /= null then
         Destroy (C.Item_Context.Modified_GC);
      end if;

      Gdk_New (C.Item_Context.Modified_GC, Win);
      Set_Foreground
        (C.Item_Context.Modified_GC, Get_Pref (Change_Color));
      C.Tooltip_Context.Modified_GC := C.Item_Context.Modified_GC;

      if C.Item_Context.Selection_GC /= null then
         Destroy (C.Item_Context.Selection_GC);
      end if;

      Gdk_New (C.Item_Context.Selection_GC, Win);
      Set_Foreground (C.Item_Context.Selection_GC,
                      Get_Pref (Selected_Item_Color));
      C.Tooltip_Context.Selection_GC := C.Item_Context.Selection_GC;

      if C.Item_Context.Text_Layout /= null then
         Unref (C.Item_Context.Text_Layout);
         Unref (C.Item_Context.Type_Layout);
      end if;

      C.Item_Context.Line_Height := To_Pixels
        (Get_Size (Get_Pref (Default_Font)));

      C.Item_Context.Big_Item_Height := Get_Pref (Big_Item_Height);

      C.Item_Context.Text_Layout := Create_Pango_Layout (C.Canvas);
      Set_Font_Description
        (C.Item_Context.Text_Layout, Get_Pref (Default_Font));

      C.Item_Context.Type_Layout := Create_Pango_Layout (C.Canvas);
      Set_Font_Description
        (C.Item_Context.Type_Layout, Get_Pref (Type_Font));

      --  The drawing context for the boxes

      if C.Box_Context.Grey_GC /= null then
         Destroy (C.Box_Context.Grey_GC);
      end if;

      Gdk_New (C.Box_Context.Grey_GC, Win);
      Set_Foreground
        (C.Box_Context.Grey_GC, Get_Pref (Title_Color));

      if C.Box_Context.Black_GC /= null then
         Destroy (C.Box_Context.Black_GC);
      end if;

      Gdk_New (C.Box_Context.Black_GC, Win);
      Set_Foreground (C.Box_Context.Black_GC, Black (Get_Default_Colormap));

      if C.Box_Context.Refresh_Button_GC /= null then
         Destroy (C.Box_Context.Refresh_Button_GC);
      end if;

      Gdk_New (C.Box_Context.Refresh_Button_GC, Win);

      if C.Box_Context.Thaw_Bg_GC /= null then
         Destroy (C.Box_Context.Thaw_Bg_GC);
      end if;

      Gdk_New (C.Box_Context.Thaw_Bg_GC, Win);
      Set_Foreground
        (C.Box_Context.Thaw_Bg_GC, Get_Pref (Thaw_Bg_Color));

      if C.Box_Context.Freeze_Bg_GC /= null then
         Destroy (C.Box_Context.Freeze_Bg_GC);
      end if;

      Gdk_New (C.Box_Context.Freeze_Bg_GC, Win);
      Set_Foreground
        (C.Box_Context.Freeze_Bg_GC,
         Get_Pref (Freeze_Bg_Color));

      Allocate_Fonts (C);

      Iter := Start (C.Canvas);
      loop
         Item := Get (Iter);
         exit when Item = null;

         Update_Resize_Display
           (Display_Item (Item), True, Hide, Redisplay_Canvas => False);
         Next (Iter);
      end loop;

      Refresh_Canvas (C.Canvas);
   end Preferences_Changed;

   -------------------------
   -- Change_Display_Mode --
   -------------------------

   procedure Change_Display_Mode
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record) is
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
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record) is
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
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record)
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

   ----------------------------------------
   -- Contextual_Background_Menu_Destroy --
   ----------------------------------------

   function Contextual_Background_Menu_Destroy
     (Widget : access Gtk_Widget_Record'Class) return Boolean is
   begin
      Destroy (GVD_Canvas (Widget).Contextual_Background_Menu);
      GVD_Canvas (Widget).Contextual_Background_Menu := null;
      return False;
   end Contextual_Background_Menu_Destroy;

   --------------------------------
   -- Contextual_Background_Menu --
   --------------------------------

   function Contextual_Background_Menu
     (Debugger : access GVD.Process.Visual_Debugger_Record'Class)
      return Gtk_Menu
   is
      Canvas : constant GVD_Canvas := GVD_Canvas (Debugger.Data);
      Accel_Group : constant Gtk_Accel_Group :=
        Debugger.Window.Main_Accel_Group;
      Check : Gtk_Check_Menu_Item;
      Mitem : Gtk_Menu_Item;
      Zooms_Menu : Gtk_Menu;

   begin
      if Canvas.Contextual_Background_Menu /= null then
         return Canvas.Contextual_Background_Menu;
      end if;

      Gtk_New (Canvas.Contextual_Background_Menu);

      Gtkada.Handlers.Return_Callback.Object_Connect
        (Canvas.Contextual_Background_Menu, "unmap_event",
         Gtkada.Handlers.Return_Callback.To_Marshaller
         (Contextual_Background_Menu_Destroy'Access),
         Canvas);

      Gtk_New (Mitem, Label => -"Display Expression...");
      Append (Canvas.Contextual_Background_Menu, Mitem);
      Widget_Callback.Object_Connect
        (Mitem, "activate", Display_Expression'Access, Canvas);

      Gtk_New (Mitem);
      Append (Canvas.Contextual_Background_Menu, Mitem);

      Gtk_New (Check, Label => -"Align On Grid");
      Set_Active (Check, Get_Align_On_Grid (Canvas.Canvas));
      Append (Canvas.Contextual_Background_Menu, Check);
      Check_Canvas_Handler.Connect
        (Check, "activate",
         Check_Canvas_Handler.To_Marshaller (Change_Align_On_Grid'Access),
         Canvas);

      Gtk_New (Check, Label => -"Detect Aliases");
      Set_Active (Check, Canvas.Detect_Aliases);
      Append (Canvas.Contextual_Background_Menu, Check);
      Check_Canvas_Handler.Connect
        (Check, "activate",
         Check_Canvas_Handler.To_Marshaller (Change_Detect_Aliases'Access),
         Canvas);

      Gtk_New (Mitem);
      Append (Canvas.Contextual_Background_Menu, Mitem);

      Gtk_New (Mitem, Label => -"Zoom in");
      Append (Canvas.Contextual_Background_Menu, Mitem);
      Widget_Callback.Object_Connect
        (Mitem, "activate",
         Widget_Callback.To_Marshaller (Zoom_In'Access), Canvas);
      Add_Accelerator
        (Mitem, "activate",
         Accel_Group, GDK_equal, 0, Accel_Visible);

      Gtk_New (Mitem, Label => -"Zoom out");
      Append (Canvas.Contextual_Background_Menu, Mitem);
      Widget_Callback.Object_Connect
        (Mitem, "activate",
         Widget_Callback.To_Marshaller (Zoom_Out'Access), Canvas);
      Add_Accelerator
        (Mitem, "activate",
         Accel_Group, GDK_minus, 0, Accel_Visible);

      Gtk_New (Zooms_Menu);

      for J in Zoom_Levels'Range loop
         Gtk_New (Mitem, Label => Guint'Image (Zoom_Levels (J)) & '%');
         Append (Zooms_Menu, Mitem);
         Item_Handler.Connect
           (Mitem, "activate",
            Item_Handler.To_Marshaller (Zoom_Level'Access),
            (Name_Length    => 0,
             Canvas         => Canvas,
             Item           => null,
             Component      => null,
             Component_Name => "",
             Mode           => Value,
             Format         => Default_Format,
             Zoom           => Zoom_Levels (J)));
      end loop;

      Gtk_New (Mitem, Label => -"Zoom");
      Append (Canvas.Contextual_Background_Menu, Mitem);
      Set_Submenu (Mitem, Zooms_Menu);

      Gtk_New (Mitem);
      Append (Canvas.Contextual_Background_Menu, Mitem);

      Gtk_New (Mitem, Label => -"Refresh");
      Append (Canvas.Contextual_Background_Menu, Mitem);
      Widget_Callback.Object_Connect
        (Mitem, "activate", On_Data_Refresh'Access, Canvas);

      Show_All (Canvas.Contextual_Background_Menu);

      return Canvas.Contextual_Background_Menu;
   end Contextual_Background_Menu;

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

   --------------------------
   -- Item_Contextual_Menu --
   --------------------------

   function Item_Contextual_Menu
     (Debugger       : access GVD.Process.Visual_Debugger_Record'Class;
      Item           : access Display_Items.Display_Item_Record'Class;
      Component      : Items.Generic_Type_Access;
      Component_Name : String) return Gtk.Menu.Gtk_Menu
   is
      Canvas : constant GVD_Canvas := GVD_Canvas (Debugger.Data);
      Mitem   : Gtk_Menu_Item;
      Radio   : Gtk_Radio_Menu_Item;
      Check   : Gtk_Check_Menu_Item;
      Submenu : Gtk_Menu;

   begin
      if Canvas.Item_Contextual_Menu /= null then
         Destroy (Canvas.Item_Contextual_Menu);
      end if;

      Gtk_New (Canvas.Item_Contextual_Menu);

      --  Display "Close" option.

      Gtk_New (Mitem, Label => -"Close" & " " & Get_Name (Item));
      Item_Handler.Connect
        (Mitem, "activate",
         Item_Handler.To_Marshaller (Undisplay_Item'Access),
         Item_Record'(Name_Length    => Component_Name'Length,
                      Canvas         => Canvas,
                      Item           => Display_Item (Item),
                      Component      => Component,
                      Component_Name => Component_Name,
                      Mode           => Value,
                      Format         => Default_Format,
                      Zoom           => 100));
      Append (Canvas.Item_Contextual_Menu, Mitem);

      if Is_A_Variable (Item) then
         --  Display a separator

         Gtk_New (Mitem);
         Append (Canvas.Item_Contextual_Menu, Mitem);

         Gtk_New (Mitem, Label => -"Hide all " & Component_Name);
         Item_Handler.Connect
           (Mitem, "activate",
            Item_Handler.To_Marshaller (Hide_All'Access),
            Item_Record'(Name_Length    => Component_Name'Length,
                         Canvas         => Canvas,
                         Item           => Display_Item (Item),
                         Component      => Component,
                         Component_Name => Component_Name,
                         Mode           => Value,
                         Format         => Default_Format,
                         Zoom           => 100));
         Append (Canvas.Item_Contextual_Menu, Mitem);

         Gtk_New (Mitem, Label => -"Show all " & Component_Name);
         Item_Handler.Connect
           (Mitem, "activate",
            Item_Handler.To_Marshaller (Show_All'Access),
            Item_Record'(Name_Length    => Component_Name'Length,
                         Canvas         => Canvas,
                         Item           => Display_Item (Item),
                         Component      => Component,
                         Component_Name => Component_Name,
                         Mode           => Value,
                         Format         => Default_Format,
                         Zoom           => 100));
         Append (Canvas.Item_Contextual_Menu, Mitem);

         --  Display a separator

         Gtk_New (Mitem);
         Append (Canvas.Item_Contextual_Menu, Mitem);

         Gtk_New (Mitem, Label => -"Clone" & " " & Component_Name);
         Item_Handler.Connect
           (Mitem, "activate",
            Item_Handler.To_Marshaller (Clone_Component'Access),
            Item_Record'(Name_Length    => Component_Name'Length,
                         Canvas         => Canvas,
                         Item           => Display_Item (Item),
                         Component      => Component,
                         Component_Name => Component_Name,
                         Mode           => Value,
                         Format         => Default_Format,
                         Zoom           => 100));
         Append (Canvas.Item_Contextual_Menu, Mitem);

         Gtk_New (Mitem, Label => -"View memory at address of "
                  & Krunch (Component_Name));
         Item_Handler.Connect
           (Mitem, "activate",
            Item_Handler.To_Marshaller (View_Into_Memory'Access),
            Item_Record'(Name_Length    => Component_Name'Length,
                         Canvas         => Canvas,
                         Item           => Display_Item (Item),
                         Component      => Component,
                         Component_Name => Component_Name,
                         Mode           => Value,
                         Format         => Default_Format,
                         Zoom           => 100));
         Append (Canvas.Item_Contextual_Menu, Mitem);

         Gtk_New (Mitem, Label => -"Set Value of " & Krunch (Component_Name));
         Item_Handler.Connect
           (Mitem, "activate",
            Item_Handler.To_Marshaller (Set_Value'Access),
            Item_Record'(Name_Length    => Component_Name'Length,
                         Canvas         => Canvas,
                         Item           => Display_Item (Item),
                         Component      => Component,
                         Component_Name => Component_Name,
                         Mode           => Value,
                         Format         => Default_Format,
                         Zoom           => 100));
         Append (Canvas.Item_Contextual_Menu, Mitem);
      end if;

      Gtk_New (Mitem, Label => -"Update Value");
      Item_Handler.Connect
        (Mitem, "activate",
         Item_Handler.To_Marshaller (Update_Variable'Access),
         Item_Record'(Name_Length    => Component_Name'Length,
                      Canvas         => Canvas,
                      Item           => Display_Item (Item),
                      Component      => Component,
                      Component_Name => Component_Name,
                      Mode           => Value,
                      Format         => Default_Format,
                      Zoom           => 100));
      Append (Canvas.Item_Contextual_Menu, Mitem);

      if Is_A_Variable (Item) then
         --  Display a separator
         Gtk_New (Mitem);
         Append (Canvas.Item_Contextual_Menu, Mitem);

         Gtk_New (Submenu);
         Gtk_New (Mitem, Label => -"Display");
         Set_Submenu (Mitem, Gtk_Widget (Submenu));
         Append (Canvas.Item_Contextual_Menu, Mitem);

         Gtk_New (Radio, Widget_SList.Null_List, -"Show Value");
         Set_Active (Radio, Get_Display_Mode (Item) = Value);
         Item_Handler.Connect
           (Radio, "activate",
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
           (Radio, "activate",
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
           (Radio, "activate",
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
         Gtk_New (Mitem);
         Append (Submenu, Mitem);

         Gtk_New (Radio, Widget_SList.Null_List, -"Default");
         Set_Active (Radio, Get_Format (Item) = Default_Format);
         Item_Handler.Connect
           (Radio, "activate",
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
           (Radio, "activate",
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
           (Radio, "activate",
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
           (Radio, "activate",
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
           (Radio, "activate",
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

      Gtk_New (Mitem);
      Append (Canvas.Item_Contextual_Menu, Mitem);

      --  Display "Toggle auto-refresh" option.

      Gtk_New (Check, "Auto refresh");
      Set_Active (Check, Get_Auto_Refresh (Display_Item (Item)));
      Item_Handler.Connect
        (Check, "activate",
         Item_Handler.To_Marshaller (Toggle_Refresh_Mode'Access),
         Item_Record'(Name_Length    => Component_Name'Length,
                      Canvas         => Canvas,
                      Item           => Display_Item (Item),
                      Component      => Component,
                      Component_Name => Component_Name,
                      Mode           => Value,
                      Format         => Default_Format,
                      Zoom           => 100));
      Append (Canvas.Item_Contextual_Menu, Check);

      Show_All (Canvas.Item_Contextual_Menu);
      return Canvas.Item_Contextual_Menu;
   end Item_Contextual_Menu;

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
         History  => Get_History (Item.Canvas.Kernel),
         Key      => "gvd_set_value_dialog");

   begin
      if S /= "" and then S (S'First) /= ASCII.NUL then
         Set_Variable
           (Get_Debugger (Item.Item).Debugger, Item.Component_Name, S);
         Update_Variable (Widget, Item);
      end if;

   exception
      when E : others =>
         Traces.Trace (Exception_Handle,
                       "Unexpected exception: " & Exception_Information (E));
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
      when E : others =>
         Traces.Trace (Exception_Handle,
                       "Unexpected exception: " & Exception_Information (E));
   end Show_All;

   ----------------------
   -- View_Into_Memory --
   ----------------------

   procedure View_Into_Memory
     (Widget : access Gtk_Widget_Record'Class;
      Item   : Item_Record)
   is
      pragma Unreferenced (Widget);

      Top  : constant GPS_Window :=
        GPS_Window (Visual_Debugger (Item.Canvas.Process).Window);
      View : GVD_Memory_View;

   begin
      Gtk_New (View, Gtk_Widget (Top));
      Show_All (View);
      Display_Memory (View, Item.Component_Name);

   exception
      when E : others =>
         Traces.Trace (Exception_Handle,
                       "Unexpected exception: " & Exception_Information (E));
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
      when E : others =>
         Traces.Trace (Exception_Handle,
                       "Unexpected exception: " & Exception_Information (E));
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
      when E : others =>
         Traces.Trace (Exception_Handle,
                       "Unexpected exception: " & Exception_Information (E));
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
      when E : others =>
         Traces.Trace (Exception_Handle,
                       "Unexpected exception: " & Exception_Information (E));
   end Toggle_Refresh_Mode;

   -------------
   -- Zoom_In --
   -------------

   procedure Zoom_In (Canvas : access Gtk_Widget_Record'Class) is
      Z : constant Guint := Get_Zoom (GVD_Canvas (Canvas).Canvas);
   begin
      for J in Zoom_Levels'Range loop
         if Zoom_Levels (J) = Z then
            if J /= Zoom_Levels'Last then
               Zoom (GVD_Canvas (Canvas).Canvas,
                     Zoom_Levels (J + 1), Zoom_Steps);
            end if;
         end if;
      end loop;
   end Zoom_In;

   --------------
   -- Zoom_Out --
   --------------

   procedure Zoom_Out (Canvas : access Gtk_Widget_Record'Class) is
      Z : constant Guint := Get_Zoom (GVD_Canvas (Canvas).Canvas);
   begin
      for J in Zoom_Levels'Range loop
         if Zoom_Levels (J) = Z then
            if J /= Zoom_Levels'First then
               Zoom (GVD_Canvas (Canvas).Canvas,
                     Zoom_Levels (J - 1), Zoom_Steps);
            end if;
         end if;
      end loop;
   end Zoom_Out;

   ----------------
   -- Zoom_Level --
   ----------------

   procedure Zoom_Level
     (Mitem : access Gtk_Widget_Record'Class;
      Item  : Item_Record)
   is
      pragma Unreferenced (Mitem);
   begin
      Zoom (Item.Canvas.Canvas, Item.Zoom, 1);

   exception
      when E : others =>
         Traces.Trace (Exception_Handle,
                       "Unexpected exception: " & Exception_Information (E));
   end Zoom_Level;

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
      when E : others =>
         Traces.Trace (Exception_Handle,
                       "Unexpected exception: " & Exception_Information (E));
   end On_Data_Window;

   ------------------
   -- Load_Desktop --
   ------------------

   function Load_Desktop
     (MDI    : MDI_Window;
      Node   : Glib.Xml_Int.Node_Ptr;
      Kernel : Kernel_Handle) return MDI_Child
   is
      pragma Unreferenced (MDI);
   begin
      if Node.Tag.all = "Data_Window" then
         return Create_Data_Window (Kernel);
      end if;
      return null;
   end Load_Desktop;

   ------------------
   -- Save_Desktop --
   ------------------

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Kernel : Kernel_Handle) return Glib.Xml_Int.Node_Ptr
   is
      pragma Unreferenced (Kernel);
      N : Glib.Xml_Int.Node_Ptr;
   begin
      if Widget.all in GVD_Canvas_Record'Class then
         N     := new Glib.Xml_Int.Node;
         N.Tag := new String'("Data_Window");
         return N;
      end if;
      return null;
   end Save_Desktop;

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
      GPS.Kernel.Kernel_Desktop.Register_Desktop_Functions
        (Save_Desktop'Access, Load_Desktop'Access);
   end Register_Module;

end GVD.Canvas;
