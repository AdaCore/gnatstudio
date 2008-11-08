-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2003-2008, AdaCore                  --
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

with Gdk.Color;                use Gdk.Color;
with Gdk.Drawable;             use Gdk.Drawable;
with Gdk.Event;                use Gdk.Event;
with Gdk.Pixbuf;               use Gdk.Pixbuf;

with Glib.Object;              use Glib.Object;
with Glib.Values;              use Glib.Values;

with Gtk.Button;               use Gtk.Button;
with Gtk.Cell_Renderer_Pixbuf; use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Handlers;
with Gtk.Image;                use Gtk.Image;
with Gtk.Menu;                 use Gtk.Menu;
with Gtk.Menu_Item;            use Gtk.Menu_Item;
with Gtk.Object;               use Gtk.Object;
with Gtk.Progress_Bar;         use Gtk.Progress_Bar;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Stock;                use Gtk.Stock;
with Gtk.Tree_Selection;       use Gtk.Tree_Selection;
with Gtk.Tree_Store;           use Gtk.Tree_Store;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;

with Gtkada.Handlers;          use Gtkada.Handlers;

with GPS.Intl;                 use GPS.Intl;
with GPS.Kernel.Preferences;   use GPS.Kernel.Preferences;
with GUI_Utils;                use GUI_Utils;
with String_Utils;             use String_Utils;
with Traces;                   use Traces;
with GNAT.Strings;

package body Task_Manager.GUI is

   ---------------------
   -- Local constants --
   ---------------------

   function Columns_Types return GType_Array;
   --  Returns the types for the columns in the Model.
   --  This is not implemented as
   --       Columns_Types : constant GType_Array ...
   --  because Gdk.Pixbuf.Get_Type cannot be called before
   --  Gtk.Main.Init.

   --  The following list must be synchronized with the array of types
   --  in Columns_Types.

   Icon_Column             : constant := 0;
   Command_Name_Column     : constant := 1;
   Command_Progress_Column : constant := 2;

   -----------------
   -- Local types --
   -----------------

   type Manager_Index_Record is record
      Manager : Task_Manager_Access;
      Index   : Integer;
   end record;

   -----------------------
   -- Local subprograms --
   -----------------------

   package Manager_Contextual_Menus is new User_Contextual_Menus
     (Manager_Index_Record);

   package Task_Manager_Handler is new Gtk.Handlers.User_Callback
     (GObject_Record, Manager_Index_Record);

   procedure Set_Column_Types
     (View : access Task_Manager_Interface_Record'Class);
   --  Sets the types of columns to be displayed in the tree_view

   procedure On_View_Destroy
     (Object : access GObject_Record'Class;
      Params : GValues);
   --  Callback for a "destroy" signal

   procedure On_View_Realize
     (Object : access GObject_Record'Class;
      Params : GValues);
   --  Callback for a "realize" signal. Initializes the graphical components

   function On_Button_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean;
   --  Callback for a "button_press_event"

   procedure On_Progress_Bar_Destroy
     (Object  : access GObject_Record'Class;
      Manager : Manager_Index_Record);
   --  Called when a progress bar is destroyed

   function On_Progress_Bar_Button_Pressed
     (Object  : access Gtk_Widget_Record'Class;
      Params  : GValues;
      Manager : Manager_Contextual_Menus.Callback_User_Data) return Boolean;
   --  Callback for a "button_press_event" on a progress bar

   procedure Interrupt_Task
     (Object    : access GObject_Record'Class;
      User_Data : Manager_Index_Record);
   --  Callback to interrupt a task

   procedure Refresh_Command
     (Manager : Task_Manager_Access;
      Index   : Integer);
   --  Refresh only one command line.
   --  Index corresponds to the index of the command in View.Manager.Queues

   procedure Pause_Command
     (Manager : Task_Manager_Access;
      Index   : Integer);
   --  Pause command referenced by Index

   procedure Resume_Command
     (Manager : Task_Manager_Access;
      Index   : Integer);
   --  Resume command referenced by Index

   function Menu_Create
     (View   : Manager_Index_Record;
      Event  : Gdk.Event.Gdk_Event) return Gtk.Menu.Gtk_Menu;
   --  Create the task manager contextual menu

   procedure Menu_Destroy
     (Manager : Manager_Index_Record;
      Menu    : Gtk.Menu.Gtk_Menu);
   --  Destroy the task manager contextual menu

   procedure On_Pause_Command
     (Object  : access GObject_Record'Class;
      Params  : GValues;
      Manager : Manager_Index_Record);
   --  Pause the referenced command in the task manager

   procedure On_Resume_Command
     (Object  : access GObject_Record'Class;
      Params  : GValues;
      Manager : Manager_Index_Record);
   --  Resume the referenced command in the task manager

   procedure On_Interrupt_Command
     (Object  : access GObject_Record'Class;
      Params  : GValues;
      Manager : Manager_Index_Record);
   --  Resume the referenced command in the task manager

   -----------------------------
   -- On_Progress_Bar_Destroy --
   -----------------------------

   procedure On_Progress_Bar_Destroy
     (Object  : access GObject_Record'Class;
      Manager : Manager_Index_Record)
   is
      pragma Unreferenced (Object);
   begin
      Pop_State (Manager.Manager);
   end On_Progress_Bar_Destroy;

   ------------------------------------
   -- On_Progress_Bar_Button_Pressed --
   ------------------------------------

   function On_Progress_Bar_Button_Pressed
     (Object  : access Gtk_Widget_Record'Class;
      Params  : GValues;
      Manager : Manager_Contextual_Menus.Callback_User_Data) return Boolean
   is
      pragma Unreferenced (Object, Params);
   begin
      Manager.User.Manager.Referenced_Command := Manager.User.Index;

      return False;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return False;
   end On_Progress_Bar_Button_Pressed;

   ----------------------
   -- On_Pause_Command --
   ----------------------

   procedure On_Pause_Command
     (Object  : access GObject_Record'Class;
      Params  : GValues;
      Manager : Manager_Index_Record)
   is
      pragma Unreferenced (Object, Params);
   begin
      if Manager.Manager.Referenced_Command = Manager.Index then
         Pause_Command (Manager.Manager, Manager.Index);
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Pause_Command;

   -----------------------
   -- On_Resume_Command --
   -----------------------

   procedure On_Resume_Command
     (Object  : access GObject_Record'Class;
      Params  : GValues;
      Manager : Manager_Index_Record)
   is
      pragma Unreferenced (Object, Params);
   begin
      if Manager.Manager.Referenced_Command = Manager.Index then
         Resume_Command (Manager.Manager, Manager.Index);
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Resume_Command;

   --------------------
   -- Interrupt_Task --
   --------------------

   procedure Interrupt_Task
     (Object    : access GObject_Record'Class;
      User_Data : Manager_Index_Record)
   is
      pragma Unreferenced (Object);
   begin
      Interrupt_Command (User_Data.Manager, User_Data.Index);
   end Interrupt_Task;

   --------------------------
   -- On_Interrupt_Command --
   --------------------------

   procedure On_Interrupt_Command
     (Object  : access GObject_Record'Class;
      Params  : GValues;
      Manager : Manager_Index_Record)
   is
      pragma Unreferenced (Object, Params);
   begin
      if Manager.Manager.Referenced_Command = Manager.Index then
         Interrupt_Command (Manager.Manager, Manager.Index);
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Interrupt_Command;

   -----------------
   -- Menu_Create --
   -----------------

   function Menu_Create
     (View   : Manager_Index_Record;
      Event  : Gdk.Event.Gdk_Event) return Gtk.Menu.Gtk_Menu
   is
      pragma Unreferenced (Event);
      Menu : Gtk_Menu;
      Item : Gtk_Menu_Item;
   begin
      Gtk_New (Menu);

      Gtk_New (Item, -"Pause");
      Task_Manager_Handler.Connect
        (Item, Gtk.Menu_Item.Signal_Activate, On_Pause_Command'Access,
          (View.Manager, View.Manager.Referenced_Command));
      Append (Menu, Item);

      Gtk_New (Item, -"Resume");
      Task_Manager_Handler.Connect
        (Item, Gtk.Menu_Item.Signal_Activate, On_Resume_Command'Access,
         (View.Manager, View.Manager.Referenced_Command));
      Append (Menu, Item);

      Gtk_New (Item);
      Append (Menu, Item);

      Gtk_New (Item, -"Interrupt");
      Task_Manager_Handler.Connect
        (Item, Gtk.Menu_Item.Signal_Activate, On_Interrupt_Command'Access,
         (View.Manager, View.Manager.Referenced_Command));
      Append (Menu, Item);

      return Menu;
   end Menu_Create;

   ------------------
   -- Menu_Destroy --
   ------------------

   procedure Menu_Destroy
     (Manager : Manager_Index_Record;
      Menu    : Gtk.Menu.Gtk_Menu)
   is
      pragma Unreferenced (Menu);

      Iface : constant Task_Manager_Interface :=
        Task_Manager_Interface (Manager.Manager.GUI);
      Iter  : Gtk_Tree_Iter;
      Model : Gtk_Tree_Model;
      Path  : Gtk_Tree_Path;
   begin
      Manager.Manager.Referenced_Command := -1;

      if Iface /= null then
         Get_Selected (Get_Selection (Iface.Tree), Model, Iter);

         if Iter = Null_Iter then
            Iface.Manager.Referenced_Command := -1;
         else
            Path := Get_Path (Model, Iter);

            declare
               A : constant Gint_Array := Get_Indices (Path);
            begin
               Manager.Manager.Referenced_Command := Integer (A (A'First)) + 1;
            end;

            Path_Free (Path);
         end if;
      end if;
   end Menu_Destroy;

   -------------------
   -- Pause_Command --
   -------------------

   procedure Pause_Command
     (Manager : Task_Manager_Access;
      Index   : Integer)
   is
      One_Running : Boolean := False;
      Result      : Command_Return_Type;
      pragma Unreferenced (Result);

   begin
      if Manager.Queues = null then
         return;
      end if;

      if Index in Manager.Queues'Range then
         Manager.Queues (Index).Status := Paused;
         Manager.Queues (Index).Need_Refresh := True;
         Refresh_Command (Manager, Index);
      end if;

      for J in Manager.Queues'Range loop
         if Manager.Queues (J).Status = Running then
            One_Running := True;
            exit;
         end if;
      end loop;

      if not One_Running then
         Result := Execute (Manager.Pop_Command);
      end if;
   end Pause_Command;

   --------------------
   -- Resume_Command --
   --------------------

   procedure Resume_Command
     (Manager : Task_Manager_Access;
      Index   : Integer)
   is
      One_Running : Boolean := False;
      Result      : Command_Return_Type;
      pragma Unreferenced (Result);

   begin
      if Manager.Queues = null then
         return;
      end if;

      for J in Manager.Queues'Range loop
         if Manager.Queues (J).Status = Running then
            One_Running := True;
            exit;
         end if;
      end loop;

      if not One_Running then
         Result := Execute (Manager.Push_Command);
      end if;

      if Index in Manager.Queues'Range then
         Manager.Queues (Index).Status := Running;
         Manager.Queues (Index).Need_Refresh := True;
         Refresh_Command (Manager, Index);

         Run (Manager, Active => Index < Manager.Passive_Index);
      end if;
   end Resume_Command;

   -----------------------
   -- Interrupt_Command --
   -----------------------

   procedure Interrupt_Command
     (Manager : Task_Manager_Access;
      Index   : Integer) is
   begin
      if Manager.Queues = null then
         return;
      end if;

      if Index in Manager.Queues'Range then
         Interrupt (Command_Queues.Head (Manager.Queues (Index).Queue).all);

         Manager.Queues (Index).Status := Interrupted;
         Manager.Queues (Index).Need_Refresh := True;
         Refresh_Command (Manager, Index);
         Run (Manager, Active => Index < Manager.Passive_Index);
      end if;
   end Interrupt_Command;

   ---------------------
   -- On_View_Realize --
   ---------------------

   procedure On_View_Realize
     (Object : access GObject_Record'Class;
      Params : GValues)
   is
      pragma Unreferenced (Params);
      use type Gdk_Drawable;

      Color   : Gdk_Color;
      Iface   : constant Task_Manager_Interface :=
        Task_Manager_Interface (Object);
      Success : Boolean;

   begin
      if Iface.Progress_Template /= null then
         --  Already realized
         return;
      end if;

      Iface.Progress_Width := 100;
      Iface.Progress_Height := 15;
      --  ??? should find a better computation for these constants

      Gdk_New (Iface.Progress_Background_GC, Get_Window (Iface));
      Gdk_New (Iface.Progress_Foreground_GC, Get_Window (Iface));
      Gdk_New (Iface.Progress_Text_GC, Get_Window (Iface));
      Set_Foreground (Iface.Progress_Text_GC, Black (Get_Default_Colormap));

      Color := Parse ("#cccccc");
      Alloc_Color (Get_Default_Colormap, Color, False, True, Success);

      if Success then
         Set_Foreground (Iface.Progress_Background_GC, Color);
      else
         Set_Foreground
           (Iface.Progress_Background_GC, Black (Get_Default_Colormap));
      end if;

      Color := Parse ("#aaaaff");
      Alloc_Color (Get_Default_Colormap, Color, False, True, Success);

      if Success then
         Set_Foreground (Iface.Progress_Foreground_GC, Color);
      else
         Set_Foreground
           (Iface.Progress_Foreground_GC, White (Get_Default_Colormap));
      end if;

      Iface.Progress_Layout := Create_Pango_Layout (Iface);
      Set_Font_Description
        (Iface.Progress_Layout,
         View_Fixed_Font.Get_Pref);

      Gdk_New
        (Iface.Progress_Template,
         Get_Window (Iface),
         Iface.Progress_Width,
         Iface.Progress_Height);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_View_Realize;

   ---------------------
   -- On_View_Destroy --
   ---------------------

   procedure On_View_Destroy
     (Object : access GObject_Record'Class;
      Params : GValues)
   is
      pragma Unreferenced (Params);
      Iface : constant Task_Manager_Interface :=
        Task_Manager_Interface (Object);
   begin
      Iface.Manager.GUI := null;

      --  If the graphics constants have been initialized, free them
      if Iface.Progress_Layout /= null then
         Unref (Iface.Progress_Foreground_GC);
         Unref (Iface.Progress_Background_GC);
         Unref (Iface.Progress_Text_GC);
         Gdk.Drawable.Unref (Iface.Progress_Template);
         Unref (Iface.Progress_Layout);
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_View_Destroy;

   ---------------------------
   -- On_Button_Press_Event --
   ---------------------------

   function On_Button_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      Iface : constant Task_Manager_Interface :=
        Task_Manager_Interface (Object);
      Iter  : Gtk_Tree_Iter;
      Model : Gtk_Tree_Model;
      Path  : Gtk_Tree_Path;

   begin
      Model := Get_Model (Iface.Tree);
      Iter := Find_Iter_For_Event (Iface.Tree, Model, Event);

      if Iter = Null_Iter then
         Iface.Manager.Referenced_Command := -1;
      else
         Path := Get_Path (Model, Iter);

         declare
            A : constant Gint_Array := Get_Indices (Path);
         begin
            Iface.Manager.Referenced_Command := Integer (A (A'First)) + 1;
         end;

         Path_Free (Path);
      end if;

      return False;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return False;
   end On_Button_Press_Event;

   -------------
   -- Refresh --
   -------------

   procedure Refresh (Manager : Task_Manager_Access) is
      View : Task_Manager_Interface;
      Need_GUI_Refresh : Boolean;
      Box : Gtk_Box;
      Button : Gtk_Button;
      Image  : Gtk_Image;
      Pixbuf : Gdk_Pixbuf;

   begin
      if Manager.GUI /= null then
         View := Task_Manager_Interface (Manager.GUI);
      end if;

      --  Clear the GUIs if there is need for a global refresh

      Need_GUI_Refresh := View /= null
        and then (Manager.Need_Global_Refresh or else View.Lines = null);

      if Need_GUI_Refresh then
         Clear (View.Tree.Model);
         Unchecked_Free (View.Lines);
      end if;

      if Manager.Queues = null
        or else Manager.Queues'Length = 0
      then
         return;
      end if;

      --  Clear the progress bars

      if Manager.Need_Global_Refresh
        and then Manager.Progress_Area /= null
      then
         for J in Manager.Queues'Range loop
            if Manager.Queues (J).Bar /= null then
               Remove (Manager.Progress_Area,
                       Get_Parent (Manager.Queues (J).Bar));
            end if;

            if Manager.Queues (J).Show_Bar then
               Gtk_New (Manager.Queues (J).Bar);
            end if;
         end loop;
      end if;

      if View /= null then
         if View.Lines /= null
           and then (View.Lines'First /= View.Manager.Queues'First
                     or else View.Lines'Last /= View.Manager.Queues'Last)
         then
            Unchecked_Free (View.Lines);
            View.Lines := null;
         end if;

         if View.Lines = null then
            View.Lines := new Iter_Array (View.Manager.Queues'Range);
         end if;
      end if;

      for J in Manager.Queues'Range loop
         if Need_GUI_Refresh then
            Append (View.Tree.Model, View.Lines (J), Null_Iter);
         end if;

         if Manager.Need_Global_Refresh or else Need_GUI_Refresh then
            Manager.Queues (J).Need_Refresh := True;
         end if;

         if Manager.Need_Global_Refresh
           and then Manager.Progress_Area /= null
         then
            if Manager.Queues (J).Show_Bar then
               Gtk_New_Hbox (Box, Homogeneous => False);
               Pack_Start (Box, Manager.Queues (J).Bar,
                           Expand => True, Fill => True);

               Gtk_New (Button);
               Pixbuf := Render_Icon
                 (Button, Stock_Close, Icon_Size_Menu);
               Gtk_New (Image, Pixbuf);
               Add (Button, Image);
               Set_Relief (Button, Relief_None);
               Pack_Start (Box, Button, Expand => False);

               Pack_End
                 (Manager.Progress_Area,
                  Box,
                  Expand  => False,
                  Fill    => True,
                  Padding => 0);

               Push_State (Manager);

               Manager_Contextual_Menus.Contextual_Callback.Connect
                 (Manager.Queues (J).Bar,
                  Signal_Button_Press_Event,
                  On_Progress_Bar_Button_Pressed'Access,
                  (null, null, (Manager, J)));

               Task_Manager_Handler.Connect
                 (Manager.Queues (J).Bar,
                  Signal_Destroy,
                  Task_Manager_Handler.To_Marshaller
                    (On_Progress_Bar_Destroy'Access),
                  (Manager, J));

               Task_Manager_Handler.Connect
                 (Button, Gtk.Button.Signal_Clicked,
                  Interrupt_Task'Access,
                  User_Data => (Manager, J));

               Manager_Contextual_Menus.Register_Contextual_Menu
                 (Manager.Queues (J).Bar,
                  (Manager, J), Menu_Create'Access, Menu_Destroy'Access);

               Show_All (Box);
            end if;
         end if;

         Refresh_Command (Manager, J);
      end loop;

      Manager.Need_Global_Refresh := False;
   end Refresh;

   ---------------------
   -- Refresh_Command --
   ---------------------

   procedure Refresh_Command
     (Manager : Task_Manager_Access;
      Index   : Integer)
   is
      Command         : Command_Access;
      Progress        : Progress_Record;
      Length          : Natural;
      View            : Task_Manager_Interface;
      Progress_String : GNAT.Strings.String_Access;
      Name_String     : GNAT.Strings.String_Access;
      Fraction        : Gdouble;

   begin
      if Manager.Queues = null then
         return;
      end if;

      if not (Index in Manager.Queues'Range)
        or else not Manager.Queues (Index).Need_Refresh
      then
         return;
      end if;

      if Manager.GUI /= null then
         View := Task_Manager_Interface (Manager.GUI);
      end if;

      Length := Command_Queues.Length (Manager.Queues (Index).Queue);

      if Length /= 0 then
         Command := Command_Queues.Head (Manager.Queues (Index).Queue);

         Progress := Commands.Progress (Command);

         if Progress.Total <= 1 then
            Progress_String := new String'("");
         else
            Progress_String := new String'
              (Image (Progress.Current) & "/" & Image (Progress.Total));
         end if;

         if Length > 1 then
            declare
               New_String : constant String := Progress_String.all
                 & " (" & Image (Length) & (-" queued)");
            begin
               GNAT.Strings.Free (Progress_String);
               Progress_String := new String'(New_String);
            end;
         end if;

         Name_String := new String'(Name (Command));

         if Progress.Total = 0 then
            Fraction := 0.0;
         else
            Fraction := Gdouble (Progress.Current) / Gdouble (Progress.Total);
         end if;

         if Manager.Queues (Index).Total > 1 then
            Fraction := (Fraction + Gdouble (Manager.Queues (Index).Done))
              / Gdouble (Manager.Queues (Index).Total);
         end if;

         if View /= null then
            Set
              (View.Tree.Model, View.Lines (Index), Command_Name_Column,
               Name_String.all);

            --  Create the pixbuf showing the progress

            if View.Progress_Layout /= null then
               declare
                  Pix           : Gdk_Pixbuf;
                  Layout_Width,
                  Layout_Height : Gint;
               begin
                  Draw_Rectangle
                    (View.Progress_Template,
                     View.Progress_Background_GC,
                     True, 0, 0,
                     View.Progress_Width,
                     View.Progress_Height);

                  Draw_Rectangle
                    (View.Progress_Template,
                     View.Progress_Foreground_GC,
                     True, 0, 0,
                     Gint (Gdouble (View.Progress_Width) * Fraction),
                     View.Progress_Height);

                  Set_Text (View.Progress_Layout, Progress_String.all);

                  Get_Pixel_Size
                    (View.Progress_Layout, Layout_Width, Layout_Height);

                  Draw_Layout
                    (Drawable => View.Progress_Template,
                     GC       => View.Progress_Text_GC,
                     X        => (View.Progress_Width - Layout_Width) / 2,
                     Y        => 0,
                     Layout   => View.Progress_Layout);

                  Pix := Get_From_Drawable
                    (Pix, View.Progress_Template,
                     Get_Default_Colormap, 0, 0, 0, 0,
                     View.Progress_Width,
                     View.Progress_Height);

                  Set
                    (View.Tree.Model, View.Lines (Index),
                     Command_Progress_Column,
                     C_Proxy (Pix));
                  Unref (Pix);
               end;
            end if;
         end if;

         if Manager.Queues (Index).Bar /= null then
            Set_Text
              (Manager.Queues (Index).Bar,
               Name_String.all & " " & Progress_String.all);

            if Progress.Total <= 0 then
               Progress.Total := 1;
            end if;

            Set_Fraction (Manager.Queues (Index).Bar, Fraction);
         end if;

         GNAT.Strings.Free (Name_String);
         GNAT.Strings.Free (Progress_String);
      end if;

      Manager.Queues (Index).Need_Refresh := False;
   end Refresh_Command;

   ----------------------
   -- Set_Column_Types --
   ----------------------

   procedure Set_Column_Types
     (View : access Task_Manager_Interface_Record'Class)
   is
      Tree        : constant Tree_View := View.Tree;
      Col         : Gtk_Tree_View_Column;
      Text_Rend   : Gtk_Cell_Renderer_Text;
      Pixbuf_Rend : Gtk_Cell_Renderer_Pixbuf;
      Dummy       : Gint;
      pragma Unreferenced (Dummy);

   begin
      Set_Rules_Hint (Tree, False);

      Gtk_New (Col);
      Set_Resizable (Col, True);
      Set_Title (Col, -"Task");
      Gtk_New (Pixbuf_Rend);
      Gtk_New (Text_Rend);
      Pack_Start (Col, Pixbuf_Rend, False);
      Pack_Start (Col, Text_Rend, True);
      Add_Attribute (Col, Pixbuf_Rend, "pixbuf", Icon_Column);
      Add_Attribute (Col, Text_Rend, "text", Command_Name_Column);
      Dummy := Append_Column (Tree, Col);

      Gtk_New (Col);
      Set_Resizable (Col, True);
      Set_Title (Col, -"Progress");
      Gtk_New (Pixbuf_Rend);
      Pack_Start (Col, Pixbuf_Rend, False);
      Add_Attribute (Col, Pixbuf_Rend, "pixbuf", Command_Progress_Column);
      Dummy := Append_Column (Tree, Col);
   end Set_Column_Types;

   -------------------
   -- Columns_Types --
   -------------------

   function Columns_Types return GType_Array is
   begin
      return GType_Array'
        (Icon_Column             => Gdk.Pixbuf.Get_Type,
         Command_Name_Column     => GType_String,
         Command_Progress_Column => Gdk.Pixbuf.Get_Type);
   end Columns_Types;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (View    : out Task_Manager_Interface;
      Manager : Task_Manager_Access;
      Dialog  : Gtk_Widget := null) is
   begin
      View := new Task_Manager_Interface_Record;
      Initialize (View, Manager, Dialog);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (View    : access Task_Manager_Interface_Record'Class;
      Manager : Task_Manager_Access;
      Dialog  : Gtk_Widget := null)
   is
      Scrolled : Gtk_Scrolled_Window;
   begin
      Initialize_Hbox (View);

      View.Manager := Manager;
      View.Dialog := Dialog;

      --  Initialize the tree

      Gtk_New (View.Tree, Columns_Types);

      Set_Name (View.Tree, "Task Manager Tree");  --  For testsuite

      Set_Column_Types (View);
      Set_Headers_Visible (View.Tree, True);

      Gtk_New (Scrolled);
      Set_Policy
        (Scrolled, Gtk.Enums.Policy_Automatic, Gtk.Enums.Policy_Automatic);
      Add (Scrolled, View.Tree);

      Add (View, Scrolled);

      Modify_Font (View.Tree, View_Fixed_Font.Get_Pref);

      View.Manager.GUI := Gtk_Widget (View);

      Refresh (Manager);

      Object_Callback.Object_Connect
        (View,
         Signal_Destroy,
         On_View_Destroy'Access,
         GObject (View),
         After => False);

      Object_Callback.Object_Connect
        (View,
         Signal_Realize,
         On_View_Realize'Access,
         GObject (View),
         After => False);

      Return_Callback.Object_Connect
        (View.Tree,
         Signal_Button_Press_Event,
         Return_Callback.To_Marshaller (On_Button_Press_Event'Access),
         View,
         After => False);

      Manager_Contextual_Menus.Register_Contextual_Menu
        (View.Tree, (View.Manager, -1),
         Menu_Create'Access, Menu_Destroy'Access);
   end Initialize;

   ---------------
   -- Pop_State --
   ---------------

   procedure Pop_State (Manager : Task_Manager_Access) is
      Dummy : Command_Return_Type;
      pragma Unreferenced (Dummy);
   begin
      if Manager.Pop_Command /= null then
         Dummy := Execute (Manager.Pop_Command);
      end if;
   end Pop_State;

   ----------------
   -- Push_State --
   ----------------

   procedure Push_State (Manager : Task_Manager_Access) is
      Dummy : Command_Return_Type;
      pragma Unreferenced (Dummy);
   begin
      if Manager.Push_Command /= null then
         Dummy := Execute (Manager.Push_Command);
      end if;
   end Push_State;

end Task_Manager.GUI;
