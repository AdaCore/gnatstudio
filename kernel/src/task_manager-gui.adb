-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003 - 2005                     --
--                              AdaCore                              --
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

with Glib;                     use Glib;
with Glib.Object;              use Glib.Object;
with Glib.Values;              use Glib.Values;
with Gdk.Event;                use Gdk.Event;
with Gdk.Pixbuf;               use Gdk.Pixbuf;
with Gtk.Button;               use Gtk.Button;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Image;                use Gtk.Image;
with Gtk.Progress_Bar;         use Gtk.Progress_Bar;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Pixbuf; use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Handlers;
with Gtk.Menu;                 use Gtk.Menu;
with Gtk.Menu_Item;            use Gtk.Menu_Item;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Stock;                use Gtk.Stock;
with Gtk.Tree_Store;           use Gtk.Tree_Store;
with Gtk.Tree_Selection;       use Gtk.Tree_Selection;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Enums;                use Gtk.Enums;

with Gtkada.Handlers;          use Gtkada.Handlers;
with Glide_Intl;               use Glide_Intl;

with GUI_Utils;                use GUI_Utils;
with String_Utils;             use String_Utils;
with Commands;                 use Commands;
with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.Exceptions;           use Ada.Exceptions;
with Traces;                   use Traces;

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
   Command_Status_Column   : constant := 2;
   Command_Progress_Column : constant := 3;

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
   --  Sets the types of columns to be displayed in the tree_view.

   procedure On_View_Destroy
     (Object : access GObject_Record'Class;
      Params : GValues);
   --  Callback for a "destroy" signal.

   procedure On_View_Selection_Changed
     (Object : access GObject_Record'Class;
      Params : GValues);
   --  Callback for a "changed" signal.

   function On_Progress_Bar_Button_Pressed
     (Object  : access Gtk_Widget_Record'Class;
      Params  : GValues;
      Manager : Manager_Contextual_Menus.Callback_User_Data) return Boolean;
   --  Callback for a "button_press_event" on a progress bar.

   procedure Interrupt_Build
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
   --  Pause command referenced by Index.

   procedure Resume_Command
     (Manager : Task_Manager_Access;
      Index   : Integer);
   --  Resume command referenced by Index.

   function Menu_Create
     (View   : Manager_Index_Record;
      Event  : Gdk.Event.Gdk_Event) return Gtk.Menu.Gtk_Menu;
   --  Create the task manager contextual menu.

   procedure Menu_Destroy
     (Manager : Manager_Index_Record;
      Menu    : Gtk.Menu.Gtk_Menu);
   --  Destroy the task manager contextual menu.

   procedure On_Pause_Command
     (Object  : access GObject_Record'Class;
      Params  : GValues;
      Manager : Manager_Index_Record);
   --  Pause the referenced command in the task manager.

   procedure On_Resume_Command
     (Object  : access GObject_Record'Class;
      Params  : GValues;
      Manager : Manager_Index_Record);
   --  Resume the referenced command in the task manager.

   procedure On_Interrupt_Command
     (Object  : access GObject_Record'Class;
      Params  : GValues;
      Manager : Manager_Index_Record);
   --  Resume the referenced command in the task manager.

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
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
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
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
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
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Resume_Command;

   ---------------------
   -- Interrupt_Build --
   ---------------------

   procedure Interrupt_Build
     (Object    : access GObject_Record'Class;
      User_Data : Manager_Index_Record)
   is
      pragma Unreferenced (Object);
   begin
      Interrupt_Command (User_Data.Manager, User_Data.Index);
   end Interrupt_Build;

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
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
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
        (Item, "activate", On_Pause_Command'Access,
          (View.Manager, View.Manager.Referenced_Command));
      Append (Menu, Item);

      Gtk_New (Item, -"Resume");
      Task_Manager_Handler.Connect
        (Item, "activate", On_Resume_Command'Access,
         (View.Manager, View.Manager.Referenced_Command));
      Append (Menu, Item);

      Gtk_New (Item);
      Append (Menu, Item);

      Gtk_New (Item, -"Interrupt");
      Task_Manager_Handler.Connect
        (Item, "activate", On_Interrupt_Command'Access,
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
         Manager.Queues (Index).Status := Interrupted;
         Manager.Queues (Index).Need_Refresh := True;
         Refresh_Command (Manager, Index);

         Run (Manager, Active => Index < Manager.Passive_Index);
      end if;
   end Interrupt_Command;

   ---------------------
   -- On_View_Destroy --
   ---------------------

   procedure On_View_Destroy
     (Object : access GObject_Record'Class;
      Params : GValues)
   is
      pragma Unreferenced (Params);
   begin
      Task_Manager_Interface (Object).Manager.GUI := null;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_View_Destroy;

   -------------------------------
   -- On_View_Selection_Changed --
   -------------------------------

   procedure On_View_Selection_Changed
     (Object : access GObject_Record'Class;
      Params : GValues)
   is
      pragma Unreferenced (Params);
      Iface : constant Task_Manager_Interface :=
        Task_Manager_Interface (Object);
      Iter  : Gtk_Tree_Iter;
      Model : Gtk_Tree_Model;
      Path  : Gtk_Tree_Path;

   begin
      Get_Selected (Get_Selection (Iface.Tree), Model, Iter);

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

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_View_Selection_Changed;

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

      if View /= null and then View.Lines = null then
         View.Lines := new Iter_Array (View.Manager.Queues'Range);
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

               Manager_Contextual_Menus.Contextual_Callback.Connect
                 (Manager.Queues (J).Bar,
                  "button_press_event",
                  On_Progress_Bar_Button_Pressed'Access,
                  (null, null, (Manager, J)));

               Task_Manager_Handler.Connect
                 (Button, "clicked",
                  Interrupt_Build'Access,
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
      Progress_String : String_Access;
      Name_String     : String_Access;
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
               Free (Progress_String);
               Progress_String := new String'(New_String);
            end;
         end if;

         Name_String := new String'(Name (Command));

         if View /= null then
            Set
              (View.Tree.Model, View.Lines (Index), Command_Name_Column,
               Name_String.all);

            case Manager.Queues (Index).Status is
               when Running =>
                  Set
                    (View.Tree.Model,
                     View.Lines (Index),
                     Command_Status_Column,
                       -(To_Lower (Progress.Activity'Img)));

               when Not_Started | Paused | Completed | Interrupted =>
                  Set
                    (View.Tree.Model,
                     View.Lines (Index),
                     Command_Status_Column,
                       -(To_Lower (Manager.Queues (Index).Status'Img)));
            end case;

            Set
              (View.Tree.Model, View.Lines (Index), Command_Progress_Column,
               Progress_String.all);
         end if;

         if Manager.Queues (Index).Bar /= null then
            Set_Text
              (Manager.Queues (Index).Bar,
               Name_String.all & " " & Progress_String.all);

            if Progress.Total <= 0 then
               Progress.Total := 1;
            end if;

            Fraction := Gdouble (Progress.Current) / Gdouble (Progress.Total);

            if Manager.Queues (Index).Total > 1 then
               Fraction := (Fraction + Gdouble (Manager.Queues (Index).Done))
                 / Gdouble (Manager.Queues (Index).Total);
            end if;

            Set_Fraction (Manager.Queues (Index).Bar, Fraction);
         end if;

         Free (Name_String);
         Free (Progress_String);
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
      Set_Title (Col, -"Task");
      Gtk_New (Pixbuf_Rend);
      Gtk_New (Text_Rend);
      Pack_Start (Col, Pixbuf_Rend, False);
      Pack_Start (Col, Text_Rend, True);
      Add_Attribute (Col, Pixbuf_Rend, "pixbuf", Icon_Column);
      Add_Attribute (Col, Text_Rend, "text", Command_Name_Column);
      Dummy := Append_Column (Tree, Col);

      Gtk_New (Col);
      Set_Title (Col, -"Status");
      Gtk_New (Text_Rend);
      Pack_Start (Col, Text_Rend, True);
      Add_Attribute (Col, Text_Rend, "text", Command_Status_Column);
      Dummy := Append_Column (Tree, Col);

      Gtk_New (Col);
      Set_Title (Col, -"Progress");
      Gtk_New (Text_Rend);
      Pack_Start (Col, Text_Rend, True);
      Add_Attribute (Col, Text_Rend, "text", Command_Progress_Column);
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
         Command_Status_Column   => GType_String,
         Command_Progress_Column => GType_String);
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

      --  Initialize the tree.

      Gtk_New (View.Tree, Columns_Types);
      Set_Column_Types (View);
      Set_Headers_Visible (View.Tree, True);

      Gtk_New (Scrolled);
      Set_Policy
        (Scrolled, Gtk.Enums.Policy_Automatic, Gtk.Enums.Policy_Automatic);
      Add (Scrolled, View.Tree);

      Add (View, Scrolled);

      View.Manager.GUI := Gtk_Widget (View);

      Refresh (Manager);

      Object_Callback.Object_Connect
        (View,
         "destroy",
         On_View_Destroy'Access,
         GObject (View),
         After => False);

      Object_Callback.Object_Connect
        (Get_Selection (View.Tree),
         "changed",
         On_View_Selection_Changed'Access,
         GObject (View),
         After => True);

      Manager_Contextual_Menus.Register_Contextual_Menu
        (View.Tree, (View.Manager, -1),
         Menu_Create'Access, Menu_Destroy'Access);
   end Initialize;

end Task_Manager.GUI;
