------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2017-2023, AdaCore                     --
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

with Ada.Containers;
with Ada.Containers.Indefinite_Ordered_Sets;

with Commands;                  use Commands;
with Commands.Interactive;      use Commands.Interactive;

with Default_Preferences;       use Default_Preferences;
with Debugger;                  use Debugger;
with Generic_Views;             use Generic_Views;

with Glib;                      use Glib;
with Glib.Object;
with Glib.Values;               use Glib.Values;
with Glib_Values_Utils;         use Glib_Values_Utils;

with GNATCOLL.JSON;
with GNATCOLL.Traces;           use GNATCOLL.Traces;

with Gtk.Box;                   use Gtk.Box;
with Gtk.Cell_Renderer_Text;    use Gtk.Cell_Renderer_Text;
with Gtk.Check_Button;          use Gtk.Check_Button;
with Gtk.Dialog;                use Gtk.Dialog;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Flow_Box;              use Gtk.Flow_Box;
with Gtk.Flow_Box_Child;        use Gtk.Flow_Box_Child;
with Gtk.Menu;
with Gtk.Menu_Item;
with Gdk.RGBA;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Tree_View;             use Gtk.Tree_View;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;
with Gtk.Tree_Store;            use Gtk.Tree_Store;
with Gtk.Widget;                use Gtk.Widget;

with Gtkada.MDI;                use Gtkada.MDI;
with Gtkada.Style;
with Gtkada.Stock_Labels;       use Gtkada.Stock_Labels;

with GPS.Debuggers;             use GPS.Debuggers;
with GPS.Dialogs;               use GPS.Dialogs;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Actions;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Properties;
with GPS.Properties;            use GPS.Properties;

with GVD.Generic_View;          use GVD.Generic_View;
with GVD_Module;                use GVD_Module;
with GVD.Preferences;           use GVD.Preferences;
with GVD.Process;               use GVD.Process;
with GVD.Types;

with String_Utils;              use String_Utils;
with GUI_Utils;                 use GUI_Utils;

package body GVD.Registers_View is
   Me : constant Trace_Handle := Create ("GPS.DEBUGGING.REGISTERS_VIEW");

   package Registers_Set is
      new Ada.Containers.Indefinite_Ordered_Sets (String);

   type Registers_View_Record is new Process_View_Record with
      record
         Tree       : Gtk.Tree_View.Gtk_Tree_View;

         Model      : Gtk.Tree_Store.Gtk_Tree_Store;
         --  The actual contents of the viewer

         Old_Values : GVD.Types.String_To_String_Maps.Map;
         --  Register name => value

         Registers  : Registers_Set.Set;
         --  Set of all the displayed registers

         Locked     : Boolean := False;
         --  If true, disable updates

         Resize     : Boolean := False;
         --  If true, the column will be resized at the next update

      end record;
   type Registers_View is access all Registers_View_Record'Class;

   overriding procedure Create_Menu
     (View : not null access Registers_View_Record;
      Menu : not null access Gtk.Menu.Gtk_Menu_Record'Class);

   overriding procedure Update (View : not null access Registers_View_Record);

   overriding procedure On_Attach
     (Self    : not null access Registers_View_Record;
      Process : not null access Base_Visual_Debugger'Class);

   overriding procedure On_Detach
     (Self    : not null access Registers_View_Record;
      Process : not null access Base_Visual_Debugger'Class);

   type Registers_Property_Record is new Property_Record with record
      Items : Registers_Set.Set;
   end record;
   --  This type is used to preverse the visible registers accross sessions by
   --  saving them in the property database.
   overriding procedure Save
     (Self  : access Registers_Property_Record;
      Value : in out GNATCOLL.JSON.JSON_Value);
   overriding procedure Load
     (Self  : in out Registers_Property_Record;
      Value : GNATCOLL.JSON.JSON_Value);

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

   package Registers_MDI_Views is new Generic_Views.Simple_Views
     (Module_Name                     => "Registers_View",
      View_Name                       => "Registers",
      Formal_View_Record              => Registers_View_Record,
      Formal_MDI_Child                => GPS_MDI_Child_Record,
      Reuse_If_Exist                  => True,
      Save_Duplicates_In_Perspectives => False,
      Commands_Category               => "",
      Group                           => Group_Debugger_Stack,
      Position                        => Position_Right,
      Areas                           => Gtkada.MDI.Sides_Only,
      Initialize                      => Initialize,
      Local_Config                    => True,
      Local_Toolbar                   => True);

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

   type Add_All_Registers_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Add_All_Registers_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Add all the available registers in the view.

   type Add_Registers_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Add_Registers_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Open a dialog to let the user choose the visible registers

   type Remove_Selected_Registers_Command is
     new Interactive_Command with null record;
   overriding function Execute
     (Command : access Remove_Selected_Registers_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Remove the selected registers

   procedure On_Edit
     (Self     : access Glib.Object.GObject_Record'Class;
      Path     : UTF8_String;
      New_Text : UTF8_String);
   --  Edit a register's value callback

   function Sort_Func
     (Model : Gtk_Tree_Model;
      A     : Gtk.Tree_Model.Gtk_Tree_Iter;
      B     : Gtk.Tree_Model.Gtk_Tree_Iter) return Gint;
   --  Smart sort for the registers' names

   Name_Column           : constant := 0;
   Hexadecimal_Column    : constant := 1;
   Naturals_Column       : constant := 2;
   Octal_Column          : constant := 3;
   Binary_Column         : constant := 4;
   Decimal_Column        : constant := 5;
   Raw_Column            : constant := 6;
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
      use type GVD.Types.Debugger_Type;

      K : constant Kernel_Handle := View.Kernel;
   begin
      Append_Menu (Menu, K, Registers_Hexadecimal);
      Append_Menu (Menu, K, Registers_Natural);
      if Debugger_Kind.Get_Pref /= GVD.Types.Gdb then
         Append_Menu (Menu, K, Registers_Octal);
         Append_Menu (Menu, K, Registers_Binary);
         Append_Menu (Menu, K, Registers_Decimal);
         Append_Menu (Menu, K, Registers_Raw);
      end if;
      Menu.Append (Gtk.Menu_Item.Gtk_Menu_Item_New);
   end Create_Menu;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference)
   is
      use Default_Preferences;
      use type GVD.Types.Debugger_Type;

      pragma Unreferenced (Kernel);
      Changed : Boolean := False;
   begin
      if Pref = null then
         Update (Self.View);
         return;

      elsif Pref = Preference (GVD.Preferences.Registers_Hexadecimal) then
         Self.View.Tree.Get_Column (Hexadecimal_Column).Set_Visible
           (GVD.Preferences.Registers_Hexadecimal.Get_Pref);
         Changed := True;

      elsif Pref = Preference (GVD.Preferences.Registers_Natural) then
         Self.View.Tree.Get_Column (Naturals_Column).Set_Visible
           (GVD.Preferences.Registers_Natural.Get_Pref);
         Changed := True;

      elsif Debugger_Kind.Get_Pref /= GVD.Types.Gdb then
         if Pref = Preference (GVD.Preferences.Registers_Octal) then
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
         end if;
      end if;

      if Changed then
         Self.View.Old_Values.Clear;
         Self.View.Resize := True;
         Update (Self.View);
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Add_All_Registers_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      View    : constant Registers_View :=
        Registers_View
          (Registers_MDI_Views.Get_Or_Create_View
             (Get_Kernel (Context.Context)));
      Process : Visual_Debugger;
      Names   : GVD.Types.Strings_Vectors.Vector;
   begin
      Process := Visual_Debugger (Get_Process (View));
      View.Locked := True;
      Names := Process.Debugger.Get_Register_Names;

      for Item of Names loop
         View.Registers.Include (Item);
      end loop;

      View.Locked := False;
      View.Old_Values.Clear;
      View.Update;
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Add_Registers_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      View     : constant Registers_View :=
        Registers_View
          (Registers_MDI_Views.Get_Or_Create_View
             (Get_Kernel (Context.Context)));
      Process  : Visual_Debugger;
      Names    : GVD.Types.Strings_Vectors.Vector;

      Scrolled : Gtk_Scrolled_Window;
      Dialog   : GPS_Dialog;
      Button   : Gtk_Widget;
      Ignore   : Gtk_Widget;
      Flow_Box : Gtk_Flow_Box;
      Check    : Gtk_Check_Button;

      procedure Is_Selected_Register
        (Widget : not null access Gtk_Widget_Record'Class);

      --------------------------
      -- Is_Selected_Register --
      --------------------------

      procedure Is_Selected_Register
        (Widget : not null access Gtk_Widget_Record'Class) is
      begin
         if Gtk_Check_Button (Gtk_Flow_Box_Child (Widget).Get_Child).Get_Active
         then
            View.Registers.Include
              (Gtk_Check_Button
                 (Gtk_Flow_Box_Child (Widget).Get_Child).Get_Label);
         end if;
      end Is_Selected_Register;
   begin
      Process := Visual_Debugger (Get_Process (View));
      View.Locked := True;
      Names := Process.Debugger.Get_Register_Names;

      Gtk_New (Dialog,
               Title          => "Registers Selector",
               Kernel         => View.Kernel,
               Flags          => Destroy_With_Parent,
               Default_Width  => 500,
               Default_Length => 400);

      Gtk_New (Scrolled);
      Gtk_New (Flow_Box);
      Flow_Box.Set_Homogeneous (True);
      Scrolled.Add (Flow_Box);
      Pack_Start
        (Get_Content_Area (Dialog), Scrolled, Expand => True, Fill => True);

      for Name of Names loop
         Gtk_New (Check, Name);
         if View.Registers.Contains (Name) then
            Check.Set_Active (True);
         end if;
         Check.Set_Name (Name);
         Flow_Box.Add (Check);
      end loop;

      Button := Add_Button (Dialog, Stock_Ok, Gtk_Response_OK);
      Grab_Default (Button);
      Ignore := Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);

      Show_All (Dialog);

      case Run (Dialog) is
         when Gtk_Response_OK =>
            --  Update the list of visible child
            View.Registers.Clear;
            View.Old_Values.Clear;
            Flow_Box.Foreach
              (Is_Selected_Register'Unrestricted_Access);
            Destroy (Dialog);
         when others =>
            Destroy (Dialog);
      end case;

      View.Locked := False;
      View.Update;
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Remove_Selected_Registers_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      View   : constant Registers_View :=
        Registers_View
          (Registers_MDI_Views.Get_Or_Create_View
             (Get_Kernel (Context.Context)));
      List   : Gtk_Tree_Path_List.Glist;
      G_Iter : Gtk_Tree_Path_List.Glist;
      Path   : Gtk_Tree_Path;
      Model  : Gtk_Tree_Model;

      use Gtk_Tree_Path_List;
   begin
      pragma Assert (View /= null);
      View.Locked := True;
      View.Tree.Get_Selection.Get_Selected_Rows (Model, List);

      if Model /= Null_Gtk_Tree_Model and then List /= Null_List then
         --  The children must be modified before their fathers
         G_Iter := Gtk_Tree_Path_List.Last (List);

         while G_Iter /= Gtk_Tree_Path_List.Null_List loop
            Path := Gtk_Tree_Path (Gtk_Tree_Path_List.Get_Data (G_Iter));

            if Path /= Null_Gtk_Tree_Path then
               declare
                  Name : constant String :=
                    View.Model.Get_String
                      (View.Model.Get_Iter (Path), Name_Column);
               begin
                  View.Registers.Delete (Name);
               end;
            end if;

            G_Iter := Gtk_Tree_Path_List.Prev (G_Iter);
         end loop;
      end if;

      Free_Path_List (List);
      View.Locked := False;
      View.Update;
      return Commands.Success;
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
      use type GVD.Types.Debugger_Type;

      Scrolled : Gtk_Scrolled_Window;

      Column_Types : constant GType_Array :=
        (Name_Column           => GType_String,
         Hexadecimal_Column    => GType_String,
         Naturals_Column       => GType_String,
         Octal_Column          => GType_String,
         Binary_Column         => GType_String,
         Decimal_Column        => GType_String,
         Raw_Column            => GType_String,
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
         Col.Set_Clickable (True);
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
      Set_Name (Widget.Tree, "Registers Tree");  --  For testsuite

      Widget.Tree.Get_Selection.Set_Mode (Selection_Multiple);
      Widget.Tree.Set_Search_Column (Name_Column);
      Widget.Tree.Set_Grid_Lines (Grid_Lines_Both);
      Add (Scrolled, Widget.Tree);

      Gtk_New (Col);
      Col_Number := Widget.Tree.Append_Column (Col);
      Col.Set_Title ("Name");
      Col.Set_Resizable (True);
      Col.Set_Sort_Column_Id (Name_Column);
      Col.Set_Clickable (True);
      Gtk_New (Render);
      Col.Pack_Start (Render, False);
      Col.Add_Attribute (Render, "text", Name_Column);
      Col.Add_Attribute (Render, "background-rgba", BG_Name_Color_Column);

      Create
        (Hexadecimal_Column,
         GVD.Preferences.Registers_Hexadecimal.Get_Pref,
         "Hexadecimal");
      Create
        (Naturals_Column,
         GVD.Preferences.Registers_Natural.Get_Pref,
         "Natural");

      Create
        (Octal_Column,
         Debugger_Kind.Get_Pref /= GVD.Types.Gdb and then
         GVD.Preferences.Registers_Octal.Get_Pref,
         "Octal");

      Create
        (Binary_Column,
         Debugger_Kind.Get_Pref /= GVD.Types.Gdb and then
         GVD.Preferences.Registers_Binary.Get_Pref,
         "Binary");

      Create
        (Decimal_Column,
         Debugger_Kind.Get_Pref /= GVD.Types.Gdb and then
         GVD.Preferences.Registers_Decimal.Get_Pref,
         "Decimal");

      Create
        (Raw_Column,
         Debugger_Kind.Get_Pref /= GVD.Types.Gdb and then
         GVD.Preferences.Registers_Raw.Get_Pref,
         "Raw");

      Widget.Model.Set_Sort_Func (Name_Column, Sort_Func'Access);
      Widget.Model.Set_Sort_Column_Id (Name_Column, Sort_Descending);

      Widget.Modify_Font (Default_Style.Get_Pref_Font);

      Preferences_Changed_Hook.Add
        (Obj   =>
            new On_Pref_Changed'
           (Hook_Function with View => Registers_View (Widget)),
         Watch => Widget);

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
      Process := Visual_Debugger (Get_Process (Widget));

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
      end if;
   end On_Edit;

   ---------------
   -- Sort_Func --
   ---------------

   function Sort_Func
     (Model : Gtk_Tree_Model;
      A     : Gtk.Tree_Model.Gtk_Tree_Iter;
      B     : Gtk.Tree_Model.Gtk_Tree_Iter) return Gint
   is
      S_A : constant String := Get_String (Model, A, Name_Column);
      S_B : constant String := Get_String (Model, B, Name_Column);
   begin
      if Smart_Sort (S_A, S_B) then
         return 1;
      else
         return -1;
      end if;
   end Sort_Func;

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
      Debugger_Stopped := Kernel.Lookup_Filter ("Debugger stopped");

      Simple_Views.Register_Module (Kernel);
      Simple_Views.Register_Open_View_Action
        (Kernel,
         Action_Name => "open registers view",
         Description => "Open the Registers view for the debugger",
         Filter      => Debugger_Stopped);

      GPS.Kernel.Actions.Register_Action
        (Kernel, "registers add all",
         Command     => new Add_All_Registers_Command,
         Description => "Add all registers",
         Icon_Name   => "gps-add-symbolic",
         Category    => "Debug",
         Filter      => Debugger_Stopped);

      GPS.Kernel.Actions.Register_Action
        (Kernel, "registers add dialog",
         Command     => new Add_Registers_Command,
         Description => "Open a dialog to select the registers",
         Icon_Name   => "gps-add-symbolic",
         Category    => "Debug",
         Filter      => Debugger_Stopped);

      GPS.Kernel.Actions.Register_Action
        (Kernel, "registers delete selected",
         Command     => new Remove_Selected_Registers_Command,
         Description => "Remove the selected registers",
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
      Selected   : GVD.Types.Strings_Vectors.Vector;
      First_Pass : Boolean := True;

      Row        : Gtk_Tree_Iter;
      Current    : Gtk_Tree_Path := Null_Gtk_Tree_Path;
      Values     : Glib.Values.GValue_Array (1 .. 6);
      Columns    : Columns_Array (Values'Range);
      Last       : Gint := 0;

      Index         : Integer := 0;
      Bg_Name       : Gdk.RGBA.Gdk_RGBA;
      Bg_Name_Dark  : Gdk.RGBA.Gdk_RGBA;
      Bg_Value      : Gdk.RGBA.Gdk_RGBA;
      Bg_Value_Dark : Gdk.RGBA.Gdk_RGBA;
      Modified_Fg   : Gdk.RGBA.Gdk_RGBA;

      procedure Get_Values (Fmt : GVD.Types.Registers_Format);
      --  Retrieve values in selected format

      procedure Get_Values (Fmt : GVD.Types.Registers_Format) is
         use type GVD.Types.Debugger_Type;
         use type GVD.Types.Registers_Format;

         Allowed : Boolean;
         Result  : GVD.Types.String_To_String_Maps.Map;
         Column  : Glib.Gint;

      begin
         if Debugger_Kind.Get_Pref /= GVD.Types.Gdb then
            case Fmt is
               when GVD.Types.Hexadecimal =>
                  Allowed := GVD.Preferences.Registers_Hexadecimal.Get_Pref;
                  Column  := Hexadecimal_Column;

               when GVD.Types.Naturals =>
                  Allowed := GVD.Preferences.Registers_Natural.Get_Pref;
                  Column  := Naturals_Column;

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
            end case;

         else
            case Fmt is
               when GVD.Types.Hexadecimal =>
                  Allowed := GVD.Preferences.Registers_Hexadecimal.Get_Pref;
                  Column  := Hexadecimal_Column;

               when GVD.Types.Naturals =>
                  Allowed := GVD.Preferences.Registers_Natural.Get_Pref;
                  Column  := Naturals_Column;

               when others =>
                  Allowed := False;
                  Column  := Raw_Column;
            end case;
         end if;

         if not Allowed then
            return;
         end if;

         Result := Instance.Get_Registers_Values (Selected, Fmt);

         if Selected.Length /= Result.Length then
            View.Old_Values.Clear;
            return;
         end if;

         if not First_Pass then
            Row := Model.Get_Iter_First;
         end if;

         for Item of Selected loop
            Last := 0;

            if First_Pass then
               Model.Append (Row, Null_Iter);
               Columns (1) := Name_Column;
               Columns (2) := BG_Name_Color_Column;
               Columns (3) := BG_Value_Color_Column;

               Values  (1) := As_String (Item);

               if Index rem 2 = 0 then
                  Gdk.RGBA.Set_Value (Values (2), Bg_Name);
                  Gdk.RGBA.Set_Value (Values (3), Bg_Value);
               else
                  Gdk.RGBA.Set_Value (Values (2), Bg_Name_Dark);
                  Gdk.RGBA.Set_Value (Values (3), Bg_Value_Dark);
               end if;

               Last := 3;

               if not View.Old_Values.Is_Empty
                 and then
                   View.Old_Values.Element (Item) /= Result.Element (Item)
               then
                  Last := 4;
                  Columns (4) := FG_Color_Column;
                  Gdk.RGBA.Set_Value (Values (4), Modified_Fg);
               end if;
            end if;

            Last := Last + 1;
            Columns (Last) := Column;
            Values  (Last) := As_String (Result.Element (Item));

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

            Index := Index + 1;
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
         M : Gtk_Tree_Model;
         C : Gtk_Tree_Iter;
      begin
         Get_First_Selected (View.Tree.Get_Selection, M, C);
         if C /= Null_Iter then
            Current := Model.Get_Path (C);
         end if;
      end;

      Detached := View.Tree.Get_Model;
      View.Tree.Set_Model (Null_Gtk_Tree_Model);
      Model.Clear;

      Process := Visual_Debugger (Get_Process (View));

      if Process = null then
         View.Tree.Set_Model (Detached);
         return;
      end if;

      Instance := Process.Debugger;

      if Instance /= null then
         Bg_Value      := Default_Style.Get_Pref_Bg;
         Bg_Value_Dark := Gtkada.Style.Shade_Or_Lighten (Bg_Value, 0.05);
         Bg_Name       := Gtkada.Style.Shade_Or_Lighten (Bg_Value, 0.1);
         Bg_Name_Dark  := Gtkada.Style.Shade_Or_Lighten (Bg_Value, 0.15);

         Modified_Fg := Numbers_Style.Get_Pref_Fg;

         --  Get names of selected registers
         for Item of View.Registers loop
            Selected.Append (Item);
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

   ---------------
   -- On_Attach --
   ---------------

   overriding procedure On_Attach
     (Self    : not null access Registers_View_Record;
      Process : not null access Base_Visual_Debugger'Class)
   is
      V        : constant Visual_Debugger := Visual_Debugger (Process);
      Found    : Boolean;
      Property : Registers_Property_Record;
   begin
      if V.Debugger /= null and then Preserve_State_On_Exit.Get_Pref then
         Get_Property
           (Property,
            Get_Executable (V.Debugger),
            Name  => "debugger_registers",
            Found => Found);
         if Found then
            Self.Registers := Property.Items;
            Self.Old_Values.Clear;
            Self.Update;
         end if;
      end if;
   end On_Attach;

   ---------------
   -- On_Detach --
   ---------------

   overriding procedure On_Detach
     (Self    : not null access Registers_View_Record;
      Process : not null access Base_Visual_Debugger'Class)
   is
      V : constant Visual_Debugger := Visual_Debugger (Process);

      function Deep_Copy
        (Registers : Registers_Set.Set) return Registers_Set.Set;

      ---------------
      -- Deep_Copy --
      ---------------

      function Deep_Copy
        (Registers : Registers_Set.Set) return Registers_Set.Set
      is
         Result : Registers_Set.Set;
      begin
         for Name of Registers loop
            Result.Include (Name);
         end loop;

         return Result;
      end Deep_Copy;

   begin
      if V.Debugger /= null and then Preserve_State_On_Exit.Get_Pref then
         GPS.Kernel.Properties.Set_Property
           (Kernel     => Self.Kernel,
            File       => Get_Executable (Visual_Debugger (Process).Debugger),
            Name       => "debugger_registers",
            Property   =>
               new Registers_Property_Record'
                 (Items => Deep_Copy (Self.Registers)),
            Persistent => True);
      end if;
   end On_Detach;

   ----------
   -- Save --
   ----------

   overriding procedure Save
     (Self  : access Registers_Property_Record;
      Value : in out GNATCOLL.JSON.JSON_Value)
   is
      use GNATCOLL.JSON;

      Values : JSON_Array;
   begin
      Trace (Me, "Saving registers view to JSON, has items ?"
             & Self.Items.Length'Img);

      for Item of Self.Items loop
         declare
            Register_Value : constant JSON_Value := Create_Object;
         begin
            Register_Value.Set_Field ("name", Item);
            Append (Values, Register_Value);
         end;
      end loop;
      Value.Set_Field ("value", Values);
   end Save;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Self  : in out Registers_Property_Record;
      Value : GNATCOLL.JSON.JSON_Value)
   is
      use GNATCOLL.JSON;

      Values : constant JSON_Array := Value.Get ("value");
   begin
      Trace (Me, "Loading variable view from JSON, has items ?"
             &  Boolean'Image (Length (Values) > 0));

      for Index in 1 .. Length (Values) loop
         declare
            V : constant JSON_Value := Get (Values, Index);
         begin
            Self.Items.Include (String'(V.Get ("name")));
         end;
      end loop;
   end Load;

   -----------------------
   -- Refresh_Registers --
   -----------------------

   procedure Refresh_Registers
     (Kernel   : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Debugger : access GPS.Debuggers.Base_Visual_Debugger'Class)
   is
      pragma Unreferenced (Kernel);
      Process  : constant Visual_Debugger := Visual_Debugger (Debugger);
   begin
      if Process /= null
        and then Process.Debugger /= null
        and then not Process.Command_In_Process
      then
         declare
            View : constant Registers_View := Get_View (Debugger);
         begin
            if View /= null then
               View.Update;
            end if;
         end;
      end if;
   end Refresh_Registers;

end GVD.Registers_View;
