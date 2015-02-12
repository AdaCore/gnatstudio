------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2015, AdaCore                     --
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

with Glib;            use Glib;
with Gtk.Icon_Theme;  use Gtk.Icon_Theme;
with GNAT.IO;         use GNAT.IO;
with Items.Records;   use Items.Records;
with Language;        use Language;

package body Items.Classes is

   --------------------
   -- New_Class_Type --
   --------------------

   function New_Class_Type
     (Num_Ancestors : Natural) return Generic_Type_Access is
   begin
      return new Class_Type (Num_Ancestors);
   end New_Class_Type;

   ------------------
   -- Add_Ancestor --
   ------------------

   procedure Add_Ancestor
     (Item     : in out Class_Type;
      Num      : Positive;
      Ancestor : Class_Type_Access) is
   begin
      pragma Assert (Num <= Item.Num_Ancestors);

      if Item.Ancestors (Num) /= null then
         Free (Item.Ancestors (Num), Only_Value => False);
      end if;

      Item.Ancestors (Num) := Ancestor;
      Draw_Border (Ancestor, False);
      Item.Valid := True;
      Item.Ancestors (Num).Valid := True;
   end Add_Ancestor;

   ---------------
   -- Set_Child --
   ---------------

   procedure Set_Child
     (Item  : in out Class_Type;
      Child : Record_Type_Access) is
   begin
      pragma Assert (Item.Child = null);

      if Item.Child /= null then
         Free (Item.Child, Only_Value => False);
      end if;

      Item.Child := Child;
      Draw_Border (Child, False);
   end Set_Child;

   ---------------
   -- Get_Child --
   ---------------

   function Get_Child (Item : Class_Type) return Generic_Type_Access is
   begin
      return Generic_Type_Access (Item.Child);
   end Get_Child;

   ------------------
   -- Get_Ancestor --
   ------------------

   function Get_Ancestor
     (Item : Class_Type;
      Num  : Positive) return Generic_Type_Access is
   begin
      pragma Assert (Num <= Item.Num_Ancestors);
      return Generic_Type_Access (Item.Ancestors (Num));
   end Get_Ancestor;

   -----------------------
   -- Get_Num_Ancestors --
   -----------------------

   function Get_Num_Ancestors (Item : Class_Type) return Natural is
   begin
      return Item.Num_Ancestors;
   end Get_Num_Ancestors;

   -----------
   -- Print --
   -----------

   overriding procedure Print (Value : Class_Type; Indent : Natural := 0) is
   begin
      Put ("{Class ");
      for A in Value.Ancestors'Range loop
         New_Line;
         Put (String'(1 .. Indent + 3 => ' '));
         Put ("Ancestor" & A'Img & " => ");

         if Value.Ancestors (A) = null then
            Put (" <unknown>");
         else
            Print (Value.Ancestors (A).all, Indent + 3);
         end if;
      end loop;

      New_Line;
      Put (String'(1 .. Indent + 3 => ' '));
      Put ("Child => ");

      Print (Value.Child.all, Indent + 3);
      Put ("}");
   end Print;

   ----------
   -- Free --
   ----------

   overriding procedure Free
     (Item : access Class_Type;
      Only_Value : Boolean := False) is
   begin
      for A in Item.Ancestors'Range loop
         Free (Item.Ancestors (A), Only_Value);
      end loop;

      Free (Item.Child, Only_Value);
      Free (Generic_Type (Item.all)'Access, Only_Value);
   end Free;

   -----------------------
   -- Clone_Dispatching --
   -----------------------

   overriding procedure Clone_Dispatching
     (Item  : Class_Type;
      Clone : in out Generic_Type_Access)
   is
      R : Class_Type_Access;
   begin
      Clone_Dispatching (Generic_Type (Item), Clone);
      R := Class_Type_Access (Clone);

      for A in Item.Ancestors'Range loop
         R.Ancestors (A) :=
           Class_Type_Access (Items.Clone (Item.Ancestors (A).all));
      end loop;

      R.Child := Record_Type_Access (Items.Clone (Item.Child.all));
   end Clone_Dispatching;

   -------------------
   -- Build_Display --
   -------------------

   overriding function Build_Display
     (Self   : not null access Class_Type;
      Name   : String;
      View   : not null access Debugger_Data_View_Record'Class;
      Lang   : Language.Language_Access;
      Mode   : Display_Mode) return Component_Item
   is
      Styles : constant access Browser_Styles := View.Get_View.Get_Styles;
      Rect   : constant Component_Item :=
        New_Component_Item (Styles, Self, Name);
      R      : Rect_Item;
   begin
      if not Self.Valid
        or else (Self.Ancestors'Length = 0
                 and then not Is_Valid (Self.Child))
      then
         Rect.Add_Child
           (Gtk_New_Image
              (Styles.Invisible,
               Gtk.Icon_Theme.Get_Default.Load_Icon
                  ("gps-unknown-item-symbolic", 16, 0, null),
               Allow_Rescale => False, Width => 16.0, Height => 16.0));

      elsif not Self.Visible then
         Rect.Add_Child (View.Item_Hidden);

      else
         for A in Self.Ancestors'Range loop

            --  Draw the ancestor if it isn't a null record.

            if Self.Ancestors (A) /= null then
               R := Gtk_New_Rect (Styles.Invisible);
               R.Set_Child_Layout (Horizontal_Stack);
               Rect.Add_Child (R);

               R.Add_Child
                 (Self.Ancestors (A).Build_Display (Name, View, Lang, Mode));

               if Self.Ancestors (A).Child /= null
                 and then Num_Fields (Self.Ancestors (A).Child.all) > 0
               then
                  Rect.Add_Child (Gtk_New_Hr (Styles.Item));
               end if;
            end if;
         end loop;

         if Self.Child /= null then
            Rect.Add_Child (Self.Child.Build_Display (Name, View, Lang, Mode));
         end if;
      end if;

      return Rect;
   end Build_Display;

   -------------
   -- Replace --
   -------------

   overriding function Replace
     (Parent       : access Class_Type;
      Current      : access Generic_Type'Class;
      Replace_With : access Generic_Type'Class) return Generic_Type_Access is
   begin
      for A in Parent.Ancestors'Range loop
         if Parent.Ancestors (A) = Class_Type_Access (Current) then
            Free (Parent.Ancestors (A), Only_Value => False);
            Parent.Ancestors (A) := Class_Type_Access (Replace_With);
            return Generic_Type_Access (Replace_With);
         end if;
      end loop;

      if Parent.Child = Record_Type_Access (Current) then
         Free (Parent.Child, Only_Value => False);
         Parent.Child := Record_Type_Access (Replace_With);
         return Generic_Type_Access (Replace_With);
      end if;

      return null;
   end Replace;

   -----------
   -- Start --
   -----------

   overriding function Start
     (Item : access Class_Type) return Generic_Iterator'Class
   is
      Iter : Class_Iterator;
   begin
      Iter.Item := Class_Type_Access (Item);
      if Item.Ancestors'Length /= 0 then
         Iter.Ancestor := Item.Ancestors'Last + 1;
      else
         Iter.Ancestor := Item.Ancestors'First;
      end if;
      return Iter;
   end Start;

   ----------
   -- Next --
   ----------

   overriding procedure Next (Iter : in out Class_Iterator) is
   begin
      Iter.Ancestor := Iter.Ancestor + 1;
   end Next;

   ------------
   -- At_End --
   ------------

   overriding function At_End (Iter : Class_Iterator) return Boolean is
   begin
      return Iter.Ancestor > Iter.Item.Ancestors'Last + 1;
   end At_End;

   ----------
   -- Data --
   ----------

   overriding function Data
     (Iter : Class_Iterator) return Generic_Type_Access is
   begin
      if Iter.Ancestor <= Iter.Item.Ancestors'Last then
         return Generic_Type_Access (Iter.Item.Ancestors (Iter.Ancestor));
      else
         return Generic_Type_Access (Iter.Item.Child);
      end if;
   end Data;

   -----------------
   -- Draw_Border --
   -----------------

   procedure Draw_Border
     (Item : access Class_Type;
      Draw : Boolean := True) is
   begin
      if Draw then
         Item.Border_Spacing := Border_Spacing;
      else
         Item.Border_Spacing := 0;
      end if;
   end Draw_Border;

   -------------------
   -- Set_Type_Name --
   -------------------

   overriding procedure Set_Type_Name
     (Item : access Class_Type;
      Name : String)
   is
   begin
      if Item.Child /= null then
         Set_Type_Name (Item.Child, Name);
      end if;
   end Set_Type_Name;

   -----------------------------
   -- Structurally_Equivalent --
   -----------------------------

   overriding function Structurally_Equivalent
     (Item1 : access Class_Type; Item2 : access Generic_Type'Class)
      return Boolean
   is
      Result : Boolean;
   begin
      if Item2.all not in Class_Type'Class then
         return False;
      end if;

      Result := Item1.Num_Ancestors = Class_Type_Access (Item2).Num_Ancestors
        and then Structurally_Equivalent
        (Item1.Child, Class_Type_Access (Item2).Child);

      if Result then
         for A in Item1.Ancestors'Range loop
            Result := Result
              and then Structurally_Equivalent
              (Item1.Ancestors (A), Class_Type_Access (Item2).Ancestors (A));
         end loop;
      end if;

      --  We should consider two classes to be structurally equivalent if one
      --  of them is an ancestor for the other, to handle the following case:
      --    - type A is access Root'Class;
      --  If a structure contains a field of type A, and there is already an
      --  item of type Child (extending Root), it is possible that the type
      --  pointed two is the same.
      --
      --  We only need to test the first parent, since this is the only one
      --  whose data will have the same address.

      if not Result then
         if Item1.Num_Ancestors /= 0 then
            Result := Result or else Structurally_Equivalent
              (Item1.Ancestors (1), Item2);
         end if;

         if Class_Type_Access (Item2).Num_Ancestors /= 0 then
            Result := Result or else Structurally_Equivalent
              (Item1, Class_Type_Access (Item2).Ancestors (1));
         end if;
      end if;

      return Result;
   end Structurally_Equivalent;

end Items.Classes;
