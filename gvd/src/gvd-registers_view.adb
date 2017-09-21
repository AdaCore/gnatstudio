------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2017, AdaCore                          --
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

with Ada.Containers.Indefinite_Ordered_Maps;

with Glib;                      use Glib;
with Glib.Object;
with Glib.Values;               use Glib.Values;

with Gtk.Box;                   use Gtk.Box;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Menu;
with Gtk.Menu_Item;
with Gtk.Cell_Renderer_Text;    use Gtk.Cell_Renderer_Text;
with Gdk.RGBA;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Tree_View;             use Gtk.Tree_View;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;
with Gtk.Tree_Store;            use Gtk.Tree_Store;
with Gtk.Widget;                use Gtk.Widget;

with Gtkada.MDI;                use Gtkada.MDI;
with Gtkada.Style;

with GPS.Debuggers;             use GPS.Debuggers;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Actions;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;

with Debugger;                  use Debugger;
with GVD.Generic_View;          use GVD.Generic_View;
with GVD_Module;                use GVD_Module;
with GVD.Preferences;           use GVD.Preferences;
with GVD.Process;               use GVD.Process;
with GVD.Types;

with Default_Preferences;       use Default_Preferences;
with Generic_Views;             use Generic_Views;
with Glib_Values_Utils;         use Glib_Values_Utils;

with Commands;                  use Commands;
with Commands.Interactive;      use Commands.Interactive;

package body GVD.Registers_View is

   package Registers_Preferences_Map is
     new Ada.Containers.Indefinite_Ordered_Maps (String, Boolean_Preference);

   Registers_Preferences : Registers_Preferences_Map.Map;

   type Registers_View_Record is new Process_View_Record with
      record
         Tree       : Gtk.Tree_View.Gtk_Tree_View;
         Model      : Gtk.Tree_Store.Gtk_Tree_Store;
         --  The actual contents of the viewer
         Old_Values : GVD.Types.Strings_Vectors.Vector;
         Locked     : Boolean := False;
         Resize     : Boolean := False;
      end record;
   type Registers_View is access all Registers_View_Record'Class;

   overriding procedure Create_Menu
     (View : not null access Registers_View_Record;
      Menu : not null access Gtk.Menu.Gtk_Menu_Record'Class);

   overriding procedure Update (View : not null access Registers_View_Record);

   overriding procedure On_Process_Terminated
     (View : not null access Registers_View_Record);

   function Initialize
     (Widget : access Registers_View_Record'Class) return Gtk_Widget;
   --  Internal initialization function

   function Get_View
     (Process : not null access Base_Visual_Debugger'Class)
      return access Registers_View_Record'Class;
   procedure Set_View
     (Process : not null access Base_Visual_Debugger'Class;
      View    : access Registers_View_Record'Class := null);
   --  Store or retrieve the view from the process

   procedure Create_Register_Preferences
     (View  : not null Registers_View;
      Names : GVD.Types.Strings_Vectors.Vector);
   --  Create preferences for them

   package Registers_MDI_Views is new Generic_Views.Simple_Views
     (Module_Name        => "Registers_View",
      View_Name          => "Registers",
      Formal_View_Record => Registers_View_Record,
      Formal_MDI_Child   => GPS_MDI_Child_Record,
      Reuse_If_Exist     => True,
      Commands_Category  => "",
      Group              => Group_Debugger_Stack,
      Position           => Position_Right,
      Areas              => Gtkada.MDI.Both,
      Initialize         => Initialize,
      Local_Config       => True,
      Local_Toolbar      => True);

   package Simple_Views is new GVD.Generic_View.Simple_Views
     (Views              => Registers_MDI_Views,
      Formal_View_Record => Registers_View_Record,
      Formal_MDI_Child   => GPS_MDI_Child_Record,
      Get_View           => Get_View,
      Set_View           => Set_View);

   type On_Pref_Changed is new Preferences_Hooks_Function with record
      View : Registers_View;
   end record;
   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference);
   --  Called when the preferences have changed, to refresh the editor
   --  appropriately.

   type Selection_Command is new Interactive_Command with record
      Selection : Boolean := True;
   end record;
   overriding function Execute
     (Command : access Selection_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Select/deselect all registers

   procedure On_Edit
     (Self     : access Glib.Object.GObject_Record'Class;
      Path     : UTF8_String;
      New_Text : UTF8_String);
   --  Edit a register's value callback

   Name_Column           : constant := 0;
   Hexadecimal_Column    : constant := 1;
   Octal_Column          : constant := 2;
   Binary_Column         : constant := 3;
   Decimal_Column        : constant := 4;
   Raw_Column            : constant := 5;
   Naturals_Column       : constant := 6;
   FG_Color_Column       : constant := 7;
   BG_Name_Color_Column  : constant := 8;
   BG_Value_Color_Column : constant := 9;
   Editable_Column       : constant := 10;

   -----------------
   -- Create_Menu --
   -----------------

   overriding procedure Create_Menu
     (View : not null access Registers_View_Record;
      Menu : not null access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      K       : constant Kernel_Handle := View.Kernel;
      Process : Visual_Debugger;
      Names   : GVD.Types.Strings_Vectors.Vector;
   begin
      Process := Get_Process (View);
      Names   := Process.Debugger.Get_Register_Names;

      Create_Register_Preferences (View, Names);

      Append_Menu (Menu, K, Registers_Hexadecimal);
      Append_Menu (Menu, K, Registers_Octal);
      Append_Menu (Menu, K, Registers_Binary);
      Append_Menu (Menu, K, Registers_Decimal);
      Append_Menu (Menu, K, Registers_Raw);
      Append_Menu (Menu, K, Registers_Natural);
      Menu.Append (Gtk.Menu_Item.Gtk_Menu_Item_New);

      for Item of Names loop
         Append_Menu (Menu, K, Registers_Preferences.Element (Item));
      end loop;
   end Create_Menu;

   ---------------------------------
   -- Create_Register_Preferences --
   ---------------------------------

   procedure Create_Register_Preferences
     (View  : not null Registers_View;
      Names : GVD.Types.Strings_Vectors.Vector)
   is
      Manager : constant Preferences_Manager := View.Kernel.Get_Preferences;
   begin
      for Item of Names loop
         if not Registers_Preferences.Contains (Item) then
            Registers_Preferences.Insert
              (Item,
               Create_Invisible_Pref (Manager, Item, True, Item));
         end if;
      end loop;
   end Create_Register_Preferences;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Selection_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Manager : constant Preferences_Manager :=
        Get_Kernel (Context.Context).Get_Preferences;
      View    : constant Registers_View := Registers_View
        (Registers_MDI_Views.Get_Or_Create_View
           (Get_Kernel (Context.Context)));
   begin
      View.Locked := True;
      for Item of Registers_Preferences loop
         Set_Pref (Item, Manager, Command.Selection);
      end loop;

      View.Locked := False;
      View.Resize := True;
      View.Update;

      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference)
   is
      use Default_Preferences;
      pragma Unreferenced (Kernel);

      function Is_Register_Pref return Boolean;

      ----------------------
      -- Is_Register_Pref --
      ----------------------

      function Is_Register_Pref return Boolean is
      begin
         for Item of Registers_Preferences loop
            if Preference (Item) = Pref then
               return True;
            end if;
         end loop;

         return False;
      end Is_Register_Pref;

      Changed : Boolean := False;
   begin
      if Pref = null then
         Update (Self.View);
         return;

      elsif Pref = Preference (GVD.Preferences.Registers_Hexadecimal) then
         Self.View.Tree.Get_Column (Hexadecimal_Column).Set_Visible
           (GVD.Preferences.Registers_Hexadecimal.Get_Pref);
         Changed := True;

      elsif Pref = Preference (GVD.Preferences.Registers_Octal) then
         Self.View.Tree.Get_Column (Octal_Column).Set_Visible
           (GVD.Preferences.Registers_Octal.Get_Pref);
         Changed := True;

      elsif Pref = Preference (GVD.Preferences.Registers_Binary) then
         Self.View.Tree.Get_Column (Binary_Column).Set_Visible
           (GVD.Preferences.Registers_Binary.Get_Pref);
         Changed := True;

      elsif Pref = Preference (GVD.Preferences.Registers_Decimal) then
         Self.View.Tree.Get_Column (Decimal_Column).Set_Visible
           (GVD.Preferences.Registers_Decimal.Get_Pref);
         Changed := True;

      elsif Pref = Preference (GVD.Preferences.Registers_Raw) then
         Self.View.Tree.Get_Column (Raw_Column).Set_Visible
           (GVD.Preferences.Registers_Raw.Get_Pref);
         Changed := True;

      elsif Pref = Preference (GVD.Preferences.Registers_Natural) then
         Self.View.Tree.Get_Column (Naturals_Column).Set_Visible
           (GVD.Preferences.Registers_Natural.Get_Pref);
         Changed := True;
      end if;

      if Changed
        or else Is_Register_Pref
      then
         Self.View.Old_Values.Clear;
         Self.View.Resize := True;
         Update (Self.View);
      end if;
   end Execute;

   --------------
   -- Get_View --
   --------------

   function Get_View
     (Process : not null access Base_Visual_Debugger'Class)
      return access Registers_View_Record'Class is
   begin
      return Registers_View (Visual_Debugger (Process).Registers_View);
   end Get_View;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (Widget : access Registers_View_Record'Class) return Gtk_Widget
   is
      Hook     : access On_Pref_Changed;
      Scrolled : Gtk_Scrolled_Window;

      Column_Types : constant GType_Array :=
        (Name_Column           => GType_String,
         Hexadecimal_Column    => GType_String,
         Octal_Column          => GType_String,
         Binary_Column         => GType_String,
         Decimal_Column        => GType_String,
         Raw_Column            => GType_String,
         Naturals_Column       => GType_String,
         FG_Color_Column       => Gdk.RGBA.Get_Type,
         BG_Name_Color_Column  => Gdk.RGBA.Get_Type,
         BG_Value_Color_Column => Gdk.RGBA.Get_Type,
         Editable_Column       => GType_Boolean);

      Col        : Gtk_Tree_View_Column;
      Render     : Gtk_Cell_Renderer_Text;
      Col_Number : Gint with Unreferenced;

      procedure Create (Column : Glib.Gint; Allowed : Boolean; Name : String);
      --  Create column for registers values

      ------------
      -- Create --
      ------------

      procedure Create
        (Column : Glib.Gint; Allowed : Boolean; Name : String) is
      begin
         Gtk_New (Col);
         Col_Number := Widget.Tree.Append_Column (Col);
         Col.Set_Title (Name);
         Col.Set_Resizable (True);
         Col.Set_Reorderable (True);
         Col.Set_Clickable (False);
         Gtk_New (Render);
         Col.Pack_Start (Render, False);
         Col.Add_Attribute (Render, "text", Column);
         Col.Add_Attribute (Render, "foreground-rgba", FG_Color_Column);
         Col.Add_Attribute (Render, "background-rgba", BG_Value_Color_Column);
         Col.Add_Attribute (Render, "editable", Editable_Column);
         Col.Set_Visible (Allowed);
         Render.On_Edited (On_Edit'Access, Widget, True);
      end Create;

   begin
      Initialize_Vbox (Widget, Homogeneous => False);

      Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);
      Widget.Pack_Start (Scrolled, Expand => True, Fill => True);

      Gtk_New (Widget.Model, Column_Types);
      Gtk_New (Widget.Tree,  Widget.Model);
      Widget.Tree.Get_Selection.Set_Mode (Selection_Single);
      Widget.Tree.Set_Grid_Lines (Grid_Lines_Both);
      Add (Scrolled, Widget.Tree);

      Gtk_New (Col);
      Col_Number := Widget.Tree.Append_Column (Col);
      Col.Set_Title ("Name");
      Col.Set_Resizable (True);
      Col.Set_Reorderable (False);
      Col.Set_Clickable (False);
      Gtk_New (Render);
      Col.Pack_Start (Render, False);
      Col.Add_Attribute (Render, "text", Name_Column);
      Col.Add_Attribute (Render, "background-rgba", BG_Name_Color_Column);

      Create
        (Hexadecimal_Column,
         GVD.Preferences.Registers_Hexadecimal.Get_Pref,
        "Hexadecimal");
      Create
        (Octal_Column,
         GVD.Preferences.Registers_Octal.Get_Pref,
         "Octal");
      Create
        (Binary_Column,
         GVD.Preferences.Registers_Binary.Get_Pref,
         "Binary");
      Create
        (Decimal_Column,
         GVD.Preferences.Registers_Decimal.Get_Pref,
         "Decimal");
      Create
        (Raw_Column,
         GVD.Preferences.Registers_Raw.Get_Pref,
        "Raw");
      Create
        (Naturals_Column,
         GVD.Preferences.Registers_Natural.Get_Pref,
         "Natural");

      Widget.Modify_Font (Default_Style.Get_Pref_Font);

      Hook      := new On_Pref_Changed;
      Hook.View := Registers_View (Widget);
      Preferences_Changed_Hook.Add (Hook, Watch => Widget);

      return Gtk_Widget (Widget.Tree);
   end Initialize;

   -------------
   -- On_Edit --
   -------------

   procedure On_Edit
     (Self     : access Glib.Object.GObject_Record'Class;
      Path     : UTF8_String;
      New_Text : UTF8_String)
   is
      Widget   : constant Registers_View := Registers_View (Self);
      Process  : Visual_Debugger;
      Instance : Debugger.Debugger_Access;

   begin
      Process := Get_Process (Widget);

      if Process = null
        or else Process.Command_In_Process
      then
         return;
      end if;

      Instance := Process.Debugger;
      if Instance /= null then
         Instance.Set_Register
           (Widget.Model.Get_String
              (Widget.Model.Get_Iter_From_String (Path),
               Name_Column), New_Text);

         Widget.Update;
      end if;
   end On_Edit;

   ---------------------------
   -- On_Process_Terminated --
   ---------------------------

   overriding procedure On_Process_Terminated
     (View : not null access Registers_View_Record) is
   begin
      View.Old_Values.Clear;
   end On_Process_Terminated;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Debugger_Stopped : Action_Filter;
   begin
      Simple_Views.Register_Module (Kernel);
      Simple_Views.Register_Open_View_Action
        (Kernel,
         Action_Name => "open registers view",
         Description => "Open the Registers view for the debugger");

      Debugger_Stopped := Kernel.Lookup_Filter
        ("Debugger stopped");

      GPS.Kernel.Actions.Register_Action
        (Kernel, "register_view select all",
         Command     => new Selection_Command'
           (Interactive_Command with Selection => True),
         Description => "Select all registers",
         Icon_Name   => "gps-add-symbolic",
         Category    => "Debug",
         Filter      => Debugger_Stopped);

      GPS.Kernel.Actions.Register_Action
        (Kernel, "register_view deselect all",
         Command     => new Selection_Command'
           (Interactive_Command with Selection => False),
         Description => "Deselect all registers",
         Icon_Name   => "gps-remove-symbolic",
         Category    => "Debug",
         Filter      => Debugger_Stopped);
   end Register_Module;

   --------------
   -- Set_View --
   --------------

   procedure Set_View
     (Process : not null access Base_Visual_Debugger'Class;
      View    : access Registers_View_Record'Class := null)
   is
      V   : constant Visual_Debugger := Visual_Debugger (Process);
      Old : constant Registers_View  := Get_View (Process);
   begin
      --  If we are detaching, clear the old view
      if Old /= null then
         Old.Model.Clear;
      end if;

      V.Registers_View := Abstract_View_Access (View);
   end Set_View;

   ------------
   -- Update --
   ------------

   overriding procedure Update
     (View : not null access Registers_View_Record)
   is
      use type Ada.Containers.Count_Type;

      Model      : Gtk.Tree_Store.Gtk_Tree_Store renames View.Model;
      Detached   : Gtk.Tree_Model.Gtk_Tree_Model;

      Process    : Visual_Debugger;
      Instance   : Debugger.Debugger_Access;
      Names      : GVD.Types.Strings_Vectors.Vector;
      Selected   : GVD.Types.Strings_Vectors.Vector;
      First_Pass : Boolean := True;

      Row        : Gtk_Tree_Iter;
      Current    : Gtk_Tree_Path := Null_Gtk_Tree_Path;
      Values     : Glib.Values.GValue_Array (1 .. 6);
      Columns    : Columns_Array (Values'Range);
      Last       : Gint := 0;

      Bg_Name       : Gdk.RGBA.Gdk_RGBA;
      Bg_Name_Dark  : Gdk.RGBA.Gdk_RGBA;
      Bg_Value      : Gdk.RGBA.Gdk_RGBA;
      Bg_Value_Dark : Gdk.RGBA.Gdk_RGBA;
      Fg            : Gdk.RGBA.Gdk_RGBA;

      procedure Get_Values (Fmt : GVD.Types.Registers_Format);
      --  Retrive values in selected format

      procedure Get_Values (Fmt : GVD.Types.Registers_Format) is
         Allowed : Boolean;
         Result  : GVD.Types.Strings_Vectors.Vector;
         Column  : Glib.Gint;
      begin
         case Fmt is
            when GVD.Types.Hexadecimal =>
               Allowed := GVD.Preferences.Registers_Hexadecimal.Get_Pref;
               Column  := Hexadecimal_Column;
            when GVD.Types.Octal =>
               Allowed := GVD.Preferences.Registers_Octal.Get_Pref;
               Column  := Octal_Column;
            when GVD.Types.Binary =>
               Allowed := GVD.Preferences.Registers_Binary.Get_Pref;
               Column  := Binary_Column;
            when GVD.Types.Decimal =>
               Allowed := GVD.Preferences.Registers_Decimal.Get_Pref;
               Column  := Decimal_Column;
            when GVD.Types.Raw =>
               Allowed := GVD.Preferences.Registers_Raw.Get_Pref;
               Column  := Raw_Column;
            when GVD.Types.Naturals =>
               Allowed := GVD.Preferences.Registers_Natural.Get_Pref;
               Column  := Naturals_Column;
         end case;

         if not Allowed then
            return;
         end if;

         if Selected.Length = Names.Length then
            Result := Instance.Get_Registers_Values
              (GVD.Types.Strings_Vectors.Empty_Vector, Fmt);
         else
            Result := Instance.Get_Registers_Values (Selected, Fmt);
         end if;

         if Selected.Length /= Result.Length then
            View.Old_Values.Clear;
            return;
         end if;

         if not First_Pass then
            Row := Model.Get_Iter_First;
         end if;

         for Index in 1 .. Natural (Selected.Length) loop
            Last := 0;

            if First_Pass then
               Model.Append (Row, Null_Iter);
               Columns (1) := Name_Column;
               Columns (2) := BG_Name_Color_Column;
               Columns (3) := BG_Value_Color_Column;

               Values  (1) := As_String (Selected.Element (Index));

               if Index rem 2 = 0 then
                  Gdk.RGBA.Set_Value (Values (2), Bg_Name);
                  Gdk.RGBA.Set_Value (Values (3), Bg_Value);
               else
                  Gdk.RGBA.Set_Value (Values (2), Bg_Name_Dark);
                  Gdk.RGBA.Set_Value (Values (3), Bg_Value_Dark);
               end if;

               Last := 3;

               if not View.Old_Values.Is_Empty
                 and then View.Old_Values.Element
                   (Index) /= Result.Element (Index)
               then
                  Last := 4;
                  Columns (4) := FG_Color_Column;
                  Gdk.RGBA.Set_Value (Values (4), Fg);
               end if;
            end if;

            Last := Last + 1;
            Columns (Last) := Column;
            Values  (Last) := As_String (Result.Element (Index));

            Last := Last + 1;
            Columns (Last) := Editable_Column;
            Values  (Last) := As_Boolean (True);

            Model.Set
              (Row,
               Glib.Gint_Array (Columns (1 .. Last)),
               Values (1 .. Last));
            Unset (Values (1 .. Last));

            if not First_Pass then
               Model.Next (Row);
            end if;
         end loop;

         if First_Pass then
            View.Old_Values := Result;
         end if;

         First_Pass := False;
      end Get_Values;

   begin
      if View.Locked then
         return;
      end if;

      declare
         M : Gtk_Tree_Model with Unreferenced;
         C : Gtk_Tree_Iter;
      begin
         View.Tree.Get_Selection.Get_Selected (M, C);
         if C /= Null_Iter then
            Current := Model.Get_Path (C);
         end if;
      end;

      Detached := View.Tree.Get_Model;
      View.Tree.Set_Model (Null_Gtk_Tree_Model);
      Model.Clear;

      Process := Get_Process (View);

      if Process = null then
         View.Tree.Set_Model (Detached);
         return;
      end if;

      Instance := Process.Debugger;

      if Instance /= null then
         Names := Instance.Get_Register_Names;

         Create_Register_Preferences (View, Names);

         Bg_Value      := Default_Style.Get_Pref_Bg;
         Bg_Value_Dark := Gtkada.Style.Shade_Or_Lighten (Bg_Value, 0.05);
         Bg_Name       := Gtkada.Style.Shade_Or_Lighten (Bg_Value, 0.1);
         Bg_Name_Dark  := Gtkada.Style.Shade_Or_Lighten (Bg_Value, 0.15);

         Fg := GPS.Kernel.Preferences.Error_Src_Highlight.Get_Pref;

         --  Get names of selected registers
         for Item of Names loop
            if Registers_Preferences.Element (Item).Get_Pref then
               Selected.Append (Item);
            end if;
         end loop;

         for Index in GVD.Types.Registers_Format loop
            Get_Values (Index);
         end loop;
      end if;

      View.Tree.Set_Model (Detached);

      if View.Resize then
         for Index in Name_Column .. Naturals_Column loop
            View.Tree.Get_Column (Gint (Index)).Queue_Resize;
         end loop;

         View.Resize := False;
      end if;

      if Current /= Null_Gtk_Tree_Path then
         View.Tree.Scroll_To_Cell
           (Current, View.Tree.Get_Column (Name_Column), False, 0.5, 0.0);
         View.Tree.Get_Selection.Select_Path (Current);
         Path_Free (Current);
      end if;
   end Update;

end GVD.Registers_View;
