------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2012-2013, AdaCore                   --
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

with Ada.Strings.Fixed;
with GNAT.Regpat;

with Basic_Types;
with GPS.Editors.GtkAda;          use GPS.Editors.GtkAda;
with GPS.Location_View.Listener;
with GNATCOLL.Utils;
with GNATCOLL.VFS.GtkAda;         use GNATCOLL.VFS.GtkAda;
with Gdk.RGBA;                    use Gdk.RGBA;
with Glib;                        use Glib;
with Glib.Generic_Properties;
with Glib.Values;                 use Glib.Values;
with Gtk.Tree_Model;              use Gtk.Tree_Model;
with Gtk.Tree_Model_Filter;       use Gtk.Tree_Model_Filter;

package body GPS.Location_View_Filter is

   use type Glib.Gint;
   use type Gtk.Tree_Model.Gtk_Tree_Iter;

   package Set_Visible_Funcs is new Set_Visible_Func_User_Data
     (User_Data_Type => Location_View_Filter_Model);

   function Is_Visible
     (Child_Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter        : Gtk.Tree_Model.Gtk_Tree_Iter;
      Self        : Location_View_Filter_Model) return Boolean;
   --  Computes whether a row should be visible.

   procedure Get_Value
     (Filter_Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter         : Gtk.Tree_Model.Gtk_Tree_Iter;
      Value        : in out Glib.Values.GValue;
      Column       : Gint);
   --  Return the value to use for that cell.

   ---------------
   -- Get_Value --
   ---------------

   procedure Get_Value
     (Filter_Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter         : Gtk.Tree_Model.Gtk_Tree_Iter;
      Value        : in out Glib.Values.GValue;
      Column       : Gint)
   is
      Self        : constant Location_View_Filter_Model :=
        Location_View_Filter_Model (Gtk_Tree_Model_Filter'(-Filter_Model));
      Child_Model : constant Gtk_Tree_Model := Self.Get_Model;
      It          : Gtk_Tree_Iter;  --  in child model
      Proxy_Path  : Gtk.Tree_Model.Gtk_Tree_Path;
      Proxy_Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
      V           : Glib.Values.GValue;

   begin
      Self.Convert_Iter_To_Child_Iter (Child_Iter => It, Filter_Iter => Iter);

      --  Obtain value from the source model.

      Gtk.Tree_Model.Get_Value (Child_Model, It, Column, V);

      --  Copy (modified if necessary) value into destination GValue object

      case Column is
         when GPS.Location_View.Listener.Category_Column =>
            Set_String (Value, Get_String (V));

         when GPS.Location_View.Listener.Weight_Column =>
            Set_Int (Value, Get_Int (V));

         when GPS.Location_View.Listener.File_Column =>
            Set_File (Value, Get_File (V));

         when GPS.Location_View.Listener.Line_Column =>
            Set_Int (Value, Get_Int (V));

         when GPS.Location_View.Listener.Column_Column =>
            Set_Int (Value, Get_Int (V));

         when GPS.Location_View.Listener.Text_Column =>
            Set_String (Value, Get_String (V));

         when GPS.Location_View.Listener.Node_Icon_Column =>
            Set_Object (Value, Get_Object (V));

         when GPS.Location_View.Listener.Node_Markup_Column =>
            --  Modify markup text for categories and files level rows.

            Proxy_Path := Self.Get_Path (Iter);

            if Gtk.Tree_Model.Get_Depth (Proxy_Path) < 3 then
               declare
                  Text        : constant String := Glib.Values.Get_String (V);
                  Total       : constant Gint :=
                    Get_Int
                      (Child_Model,
                       It,
                       GPS.Location_View.Listener.Number_Of_Children_Column);
                  Total_Image : constant String :=
                    GNATCOLL.Utils.Image (Natural (Total), 1);
                  Visible     : Natural;

               begin
                  --  Compute number of visible messages.

                  if Gtk.Tree_Model.Get_Depth (Proxy_Path) = 1 then
                     --  For category row go through all children files rows

                     Visible := 0;
                     Proxy_Iter := Self.Children (Iter);

                     while Proxy_Iter /= Gtk.Tree_Model.Null_Iter loop
                        Visible :=
                          Visible + Natural (Self.N_Children (Proxy_Iter));
                        Self.Next (Proxy_Iter);
                     end loop;

                  else
                     --  For file row obtain number of visible rows directly.

                     Visible := Natural (Self.N_Children (Iter));
                  end if;

                  if Total = 1 then
                     Glib.Values.Set_String
                       (Value, Text & " (" & Total_Image & " item)");

                  else
                     if Visible = Natural (Total) then
                        Glib.Values.Set_String
                          (Value, Text & " (" & Total_Image & " items)");

                     else
                        Glib.Values.Set_String
                          (Value,
                           Text
                           & " ("
                           & GNATCOLL.Utils.Image (Visible, 1)
                           & " of "
                           & Total_Image
                           & " items)");
                     end if;
                  end if;
               end;

            else
               Set_String (Value, Get_String (V));
            end if;

         when GPS.Location_View.Listener.Node_Foreground_Column =>
            begin
               Set_Value (Value, Get_Value (V));

            exception
               when Glib.Generic_Properties.Unset_Value =>
                  Set_Value (Value, Null_RGBA);
            end;

         when GPS.Location_View.Listener.Node_Tooltip_Column =>
            Set_String (Value, Get_String (V));

         when GPS.Location_View.Listener.Node_Mark_Column =>
            Set_Mark (Value, Get_Mark (V));

         when GPS.Location_View.Listener.Action_Pixbuf_Column =>
            Set_Object (Value, Get_Object (V));

         when GPS.Location_View.Listener.Action_Command_Column =>
            Set_Address (Value, Get_Address (V));

         when GPS.Location_View.Listener.Action_Tooltip_Column =>
            Set_String (Value, Get_String (V));

         when GPS.Location_View.Listener.Number_Of_Children_Column =>
            Set_Int (Value, Get_Int (V));

         when GPS.Location_View.Listener.Sort_Order_Hint_Column =>
            Set_Int (Value, Get_Int (V));

         when GPS.Location_View.Listener.Message_Column =>
            Set_Address (Value, Get_Address (V));

         when others =>
            null;
      end case;

      Unset (V);
      Gtk.Tree_Model.Path_Free (Proxy_Path);
   end Get_Value;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Model       : out Location_View_Filter_Model;
      Child_Model : Gtk.Tree_Model.Gtk_Tree_Model)
   is
      Cols : constant Guint := Guint (Get_N_Columns (Child_Model));
      Types : GType_Array (0 .. Cols - 1);
   begin
      Model := new Location_View_Filter_Model_Record;
      Gtk.Tree_Model_Filter.Initialize (Model, Child_Model => Child_Model);

      for T in Types'Range loop
         Types (T) := Get_Column_Type (Child_Model, Gint (T));
      end loop;

      Set_Visible_Funcs.Set_Visible_Func
        (Model, Is_Visible'Access, Data => Model);
      Model.Set_Modify_Func (Types, Get_Value'Access);
   end Gtk_New;

   ----------------
   -- Is_Visible --
   ----------------

   function Is_Visible
     (Child_Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter        : Gtk.Tree_Model.Gtk_Tree_Iter;
      Self        : Location_View_Filter_Model) return Boolean
   is
      use type GNAT.Expect.Pattern_Matcher_Access;
      use type GNAT.Strings.String_Access;

      P     : constant Gtk_Tree_Iter := Parent (Child_Model, Iter);
      P2    : Gtk_Tree_Iter;
      Child : Gtk.Tree_Model.Gtk_Tree_Iter;

   begin
      if P = Null_Iter then
         --  Category rows are displayed always, otherwise view doesn't
         --  display any rows at all when model is filled from empty state.

         return True;
      end if;

      P2 := Parent (Child_Model, P);
      if P2 = Null_Iter then
         --  File rows are displayed only when they have visible messages.

         Child := Children (Child_Model, Iter);
         while Child /= Null_Iter loop
            if Is_Visible (Child_Model, Child, Self) then
               return True;
            end if;
            Next (Child_Model, Child);
         end loop;
         return False;

      else
         --  Messages rows are displayed when they match filter.

         declare
            Text  : constant String := Get_String
              (Child_Model, Iter, GPS.Location_View.Listener.Text_Column);
            Found : Boolean := False;
         begin
            if Self.Regexp /= null then
               Found := GNAT.Regpat.Match (Self.Regexp.all, Text);
            elsif Self.Text /= null then
               Found := Ada.Strings.Fixed.Index (Text, Self.Text.all) /= 0;
            else
               return True;
            end if;

            if Self.Is_Hide then
               Found := not Found;
            end if;

            return Found;
         end;
      end if;
   end Is_Visible;

   -----------------
   -- Set_Pattern --
   -----------------

   procedure Set_Pattern
     (Self         : not null access Location_View_Filter_Model_Record;
      Pattern      : String;
      Is_Regexp    : Boolean;
      Hide_Matched : Boolean)
   is
      New_Regexp : GNAT.Expect.Pattern_Matcher_Access;
      New_Text   : GNAT.Strings.String_Access;

   begin
      Basic_Types.Unchecked_Free (Self.Regexp);
      GNAT.Strings.Free (Self.Text);

      if Pattern /= "" then
         if Is_Regexp then
            begin
               New_Regexp := new GNAT.Regpat.Pattern_Matcher'
                 (GNAT.Regpat.Compile (Pattern));

            exception
               when GNAT.Regpat.Expression_Error =>
                  New_Regexp := null;
                  New_Text   := new String'(Pattern);
            end;

         else
            New_Text := new String'(Pattern);
         end if;
      end if;

      Self.Regexp  := New_Regexp;
      Self.Text    := New_Text;
      Self.Is_Hide := Hide_Matched;

      Self.Refilter;
   end Set_Pattern;

end GPS.Location_View_Filter;
