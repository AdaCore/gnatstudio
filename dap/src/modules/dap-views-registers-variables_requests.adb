------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2023, AdaCore                          --
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

with VSS.Strings;               use VSS.Strings;
with VSS.Strings.Conversions;

with Glib;                      use Glib;
with Glib.Values;               use Glib.Values;
with Glib_Values_Utils;         use Glib_Values_Utils;

with Gdk.RGBA;

with Gtk.Box;                   use Gtk.Box;
with Gtk.Check_Button;          use Gtk.Check_Button;
with Gtk.Dialog;                use Gtk.Dialog;
with Gtk.Flow_Box;              use Gtk.Flow_Box;
with Gtk.Flow_Box_Child;        use Gtk.Flow_Box_Child;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Tree_Model;            use Gtk.Tree_Model;

with Gtkada.Style;

with GPS.Dialogs;               use GPS.Dialogs;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;

with DAP.Tools;                 use DAP.Tools;
with GUI_Utils;                 use GUI_Utils;

package body DAP.Views.Registers.Variables_Requests is

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self        : in out Variables_Request;
      Result      : in out DAP.Tools.VariablesResponse;
      New_Request : in out DAP.Requests.DAP_Request_Access)
   is
      use Registers_MDI_Views;

      View : constant DAP_Registers_View :=
        Registers_MDI_Views.Retrieve_View
          (Self.Kernel,
           Visible_Only => False);

   begin
      New_Request := null;

      if View = null then
         return;
      end if;

      case Self.Kind is
         when Select_Names =>
            Self.Select_Names (Result);
         when Select_All_Names =>
            Self.Select_All_Names (Result);
         when Update_Registers =>
            Self.Update_Registers (Result);
      end case;
   end On_Result_Message;

   ------------------
   -- Select_Names --
   ------------------

   procedure Select_Names
     (Self   : in out Variables_Request;
      Result : in out DAP.Tools.VariablesResponse)
   is
      use Registers_MDI_Views;

      View : constant DAP_Registers_View :=
        Registers_MDI_Views.Retrieve_View
          (Self.Kernel,
           Visible_Only => False);

      Names    : DAP.Types.Strings_Vectors.Vector;

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
         if Gtk_Check_Button
           (Gtk_Flow_Box_Child (Widget).Get_Child).Get_Active
         then
            View.Registers.Include
              (Gtk_Check_Button
                 (Gtk_Flow_Box_Child (Widget).Get_Child).Get_Label);
         end if;
      end Is_Selected_Register;

   begin
      for Index in 1 .. Length (Result.a_body.variables) loop
         Names.Append
           (VSS.Strings.Conversions.To_UTF_8_String
              (Result.a_body.variables (Index).name));
      end loop;

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
        (In_Box => Get_Content_Area (Dialog),
         Child  => Scrolled,
         Expand => True,
         Fill   => True);

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
   end Select_Names;

   ----------------------
   -- Select_All_Names --
   ----------------------

   procedure Select_All_Names
     (Self   : in out Variables_Request;
      Result : in out DAP.Tools.VariablesResponse)
   is
      View : constant DAP_Registers_View :=
        Registers_MDI_Views.Retrieve_View
          (Self.Kernel,
           Visible_Only => False);
   begin
      for Index in 1 .. Length (Result.a_body.variables) loop
         View.Registers.Include
           (VSS.Strings.Conversions.To_UTF_8_String
              (Result.a_body.variables (Index).name));
      end loop;

      View.Locked := False;
      View.Old_Values.Clear;
      View.Update;
   end Select_All_Names;

   ----------------------
   -- Update_Registers --
   ----------------------

   procedure Update_Registers
     (Self   : in out Variables_Request;
      Result : in out DAP.Tools.VariablesResponse)
   is
      use Registers_MDI_Views;
      use DAP.Types.String_To_String_Maps;

      View : constant DAP_Registers_View :=
        Registers_MDI_Views.Retrieve_View
          (Self.Kernel,
           Visible_Only => False);

      Model      : Gtk.Tree_Store.Gtk_Tree_Store renames View.Model;
      Detached   : Gtk.Tree_Model.Gtk_Tree_Model;

      Row        : Gtk_Tree_Iter;
      Current    : Gtk_Tree_Path := Null_Gtk_Tree_Path;
      Values     : Glib.Values.GValue_Array (1 .. 8);
      Columns    : Columns_Array (Values'Range);
      Last       : Gint := 0;

      Index         : Integer := 0;
      Bg_Name       : Gdk.RGBA.Gdk_RGBA;
      Bg_Name_Dark  : Gdk.RGBA.Gdk_RGBA;
      Bg_Value      : Gdk.RGBA.Gdk_RGBA;
      Bg_Value_Dark : Gdk.RGBA.Gdk_RGBA;
      Modified_Fg   : Gdk.RGBA.Gdk_RGBA;
      Var           : Variable;
      Cursor        : DAP.Types.String_To_String_Maps.Cursor;

   begin
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

      Bg_Value      := Default_Style.Get_Pref_Bg;
      Bg_Value_Dark := Gtkada.Style.Shade_Or_Lighten (Bg_Value, 0.05);
      Bg_Name       := Gtkada.Style.Shade_Or_Lighten (Bg_Value, 0.1);
      Bg_Name_Dark  := Gtkada.Style.Shade_Or_Lighten (Bg_Value, 0.15);
      Modified_Fg   := Numbers_Style.Get_Pref_Fg;

      Row := Model.Get_Iter_First;

      for Item of View.Registers loop
         for Index in 1 .. Length (Result.a_body.variables) loop
            if Result.a_body.variables (Index).name =
              VSS.Strings.Conversions.To_Virtual_String (Item)
            then
               Var := Result.a_body.variables (Index);
               exit;
            end if;
         end loop;

         Last := 0;

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

         Cursor := View.Old_Values.Find (Item);
         if Has_Element (Cursor)
           and then VSS.Strings.Conversions.To_Virtual_String
             (Element (Cursor)) /= Var.value
         then
            Last := 4;
            Columns (4) := FG_Color_Column;
            Gdk.RGBA.Set_Value (Values (4), Modified_Fg);
         end if;

         Last := Last + 1;
         Columns (Last) := Raw_Column;
         Values  (Last) := As_String
           (VSS.Strings.Conversions.To_UTF_8_String (Var.value));

         Last := Last + 1;
         Columns (Last) := Type_Column;
         Values  (Last) := As_String
           (VSS.Strings.Conversions.To_UTF_8_String (Var.a_type));

         Last := Last + 1;
         Columns (Last) := Editable_Column;
         Values  (Last) := As_Boolean (True);

         Last := Last + 1;
         Columns (Last) := Id_Column;
         Values  (Last) := As_Int (Gint (Var.variablesReference));

         Model.Set
           (Row,
            Glib.Gint_Array (Columns (1 .. Last)),
            Values (1 .. Last));
         Unset (Values (1 .. Last));

         View.Old_Values.Include
           (Item, VSS.Strings.Conversions.To_UTF_8_String (Var.value));

         Model.Next (Row);

         Index := Index + 1;
      end loop;

      View.Tree.Set_Model (Detached);

      if View.Resize then
         for Index in Name_Column .. Type_Column loop
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
   end Update_Registers;

end DAP.Views.Registers.Variables_Requests;
