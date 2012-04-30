------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2012, AdaCore                        --
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
with GNATCOLL.Utils;
with GPS.Location_View.Listener;

package body GPS.Location_View_Filter is

   use type Glib.Gint;
   use type Gtk.Tree_Model.Gtk_Tree_Iter;

   ---------------
   -- Get_Value --
   ---------------

   overriding procedure Get_Value
     (Self   : access Location_View_Filter_Model_Record;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Glib.Gint;
      Value  : out Glib.Values.GValue)
   is
      Proxy_Path  : Gtk.Tree_Model.Gtk_Tree_Path;
      Proxy_Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
      Source_Iter : Gtk.Tree_Model.Gtk_Tree_Iter;

   begin
      --  Obtain value from the source model.

      Gtkada.Abstract_Filter_Model.Gtk_Abstract_Filter_Model_Record
        (Self.all).Get_Value (Iter, Column, Value);

      --  Modify markup text for categories and files level rows.

      if Column = GPS.Location_View.Listener.Node_Markup_Column then
         Proxy_Path := Self.Get_Path (Iter);
         Source_Iter := Self.Map_To_Source (Iter);

         if Gtk.Tree_Model.Get_Depth (Proxy_Path) < 3 then
            declare
               Text        : constant String := Glib.Values.Get_String (Value);
               Total       : constant Natural :=
                 Natural
                   (Self.Get_Source_Model.Get_Int
                      (Source_Iter,
                       GPS.Location_View.Listener.Number_Of_Children_Column));
               Total_Image : constant String :=
                 GNATCOLL.Utils.Image (Total, 1);
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
                  if Visible = Total then
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
         end if;
      end if;

      Gtk.Tree_Model.Path_Free (Proxy_Path);
   end Get_Value;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Model : out Location_View_Filter_Model) is
   begin
      Model := new Location_View_Filter_Model_Record;
      Initialize (Model);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : not null access Location_View_Filter_Model_Record'Class) is
   begin
      Gtkada.Abstract_Filter_Model.Initialize (Self);
   end Initialize;

   ----------------
   -- Is_Visible --
   ----------------

   overriding function Is_Visible
     (Self        : not null access Location_View_Filter_Model_Record;
      Source_Path : Gtk.Tree_Model.Gtk_Tree_Path;
      Source_Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Boolean
   is
      use type GNAT.Expect.Pattern_Matcher_Access;
      use type GNAT.Strings.String_Access;

      Depth : constant Glib.Gint := Gtk.Tree_Model.Get_Depth (Source_Path);

   begin
      if Depth = 1 then
         --  Category rows are displayed always, otherwise view doesn't
         --  display any rows at all when model is filled from empty state.

         return True;

      elsif Depth = 2 then
         --  File rows are displayed only when they have visible messages.

         declare
            Child_Iter : Gtk.Tree_Model.Gtk_Tree_Iter;
            Child_Path : Gtk.Tree_Model.Gtk_Tree_Path;
            Found      : Boolean := False;

         begin
            Child_Iter := Self.Get_Source_Model.Children (Source_Iter);
            Child_Path := Gtk.Tree_Model.Copy (Source_Path);
            Gtk.Tree_Model.Down (Child_Path);

            while Child_Iter /= Gtk.Tree_Model.Null_Iter loop
               if Self.Is_Visible (Child_Path, Child_Iter) then
                  Found := True;

                  exit;
               end if;

               Self.Get_Source_Model.Next (Child_Iter);
               Gtk.Tree_Model.Next (Child_Path);
            end loop;

            Gtk.Tree_Model.Path_Free (Child_Path);

            return Found;
         end;

      else
         --  Messages rows are displayed when match filter.

         declare
            Text  : constant String :=
              Self.Get_Source_Model.Get_String
                (Source_Iter, GPS.Location_View.Listener.Text_Column);
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

      Self.Invalidate;
   end Set_Pattern;

end GPS.Location_View_Filter;
