------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2012, AdaCore                     --
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

with GNAT.IO;         use GNAT.IO;

with Cairo;           use Cairo;
with Glib;            use Glib;

with Gtkada.Style;    use Gtkada.Style;

with Language;        use Language;
with Items.Records;   use Items.Records;

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

   -----------
   -- Paint --
   -----------

   overriding procedure Paint
     (Item    : in out Class_Type;
      Context : Drawing_Context;
      Cr      : Cairo.Cairo_Context;
      Lang    : Language.Language_Access;
      Mode    : Display_Mode;
      X, Y    : Glib.Gint := 0)
   is
      Current_Y : Gint := Y + Item.Border_Spacing;
   begin
      Item.X := X;
      Item.Y := Y;

      if not Item.Valid
        or else (Item.Ancestors'Length = 0
                 and then not Is_Valid (Item.Child))
      then
         Draw_Pixbuf
           (Cr, Context.Unknown_Pixmap,
            X + Left_Border, Y + Item.Border_Spacing);
         return;
      end if;

      if not Item.Visible then
         Draw_Pixbuf (Cr, Context.Hidden_Pixmap, X + Left_Border, Current_Y);
         return;
      end if;

      if Item.Selected then
         Draw_Rectangle
           (Cr, Context.Selection_Color,
            Filled => True,
            X      => X,
            Y      => Y,
            Width  => Item.Width,
            Height => Item.Height);
      end if;

      for A in Item.Ancestors'Range loop

         --  Draw the ancestor if it isn't a null record.

         if Item.Ancestors (A) /= null
           and then Item.Ancestors (A).Height > 0
         then
            --  Do not add Left_Border to X, since each of the ancestor is
            --  itself a Class_Type and will already draw it.
            Paint (Item.Ancestors (A).all, Context, Cr, Lang, Mode,
                   X + Item.Border_Spacing, Current_Y);
            Current_Y := Current_Y + Item.Ancestors (A).Height + Line_Spacing;

            if Item.Ancestors (A).Child /= null
              and then Num_Fields (Item.Ancestors (A).Child.all) > 0
            then
               Save (Cr);
               Set_Dash (Cr, (1 .. 2 => 2.0), 0.0);
               Set_Line_Cap (Cr, Cairo_Line_Cap_Butt);
               Set_Line_Join (Cr, Cairo_Line_Join_Miter);
               Draw_Line (Cr, Context.Foreground,
                          X + Item.Border_Spacing,
                          Current_Y,
                          X + Item.Width - Item.Border_Spacing,
                          Current_Y);
               Restore (Cr);
               Current_Y := Current_Y + 2;
            end if;

         end if;
      end loop;

      if Get_Height (Item.Child.all) > 2 * Item.Border_Spacing then
         Paint (Item.Child.all, Context, Cr, Lang, Mode, X + Left_Border
                + Item.Border_Spacing, Current_Y);
      end if;

      --  Draw a border
      if Item.Border_Spacing /= 0 then
         Draw_Rectangle
           (Cr, Context.Foreground,
            Filled => False,
            X      => X,
            Y      => Y,
            Width  => Item.Width - 1,
            Height => Item.Height - 1);
      end if;
   end Paint;

   ------------------
   -- Size_Request --
   ------------------

   overriding procedure Size_Request
     (Item           : in out Class_Type;
      Context        : Drawing_Context;
      Lang           : Language.Language_Access;
      Mode           : Display_Mode;
      Hide_Big_Items : Boolean := False)
   is
      Total_Height, Total_Width : Gint := 0;
   begin
      if not Item.Valid then
         Item.Width := Get_Width (Context.Unknown_Pixmap);
         Item.Height := Get_Height (Context.Unknown_Pixmap);
         return;
      end if;

      if Item.Visible then
         for A in Item.Ancestors'Range loop
            if Item.Ancestors (A) /= null then
               Size_Request
                 (Item.Ancestors (A).all, Context, Lang, Mode,
                  Hide_Big_Items);

               --  If we don't have an null record
               if Item.Ancestors (A).Height /= 0 then
                  Total_Height := Total_Height + Item.Ancestors (A).Height +
                    Line_Spacing;

                  --  Keep some space for the dashed line
                  if Item.Ancestors (A).Child /= null
                    and then Num_Fields (Item.Ancestors (A).Child.all) > 0
                  then
                     Total_Height := Total_Height + 2;
                  end if;

               end if;

               Total_Width := Gint'Max (Total_Width, Item.Ancestors (A).Width);
            end if;
         end loop;

         Size_Request (Item.Child.all, Context, Lang, Mode, Hide_Big_Items);

         Total_Width :=
           Gint'Max (Total_Width, Get_Width (Item.Child.all)) + Left_Border;
         Propagate_Width (Item.Child.all, Total_Width);

         --  Dont print an extra border around, since each ancestors and child
         --  are records and already have their own borders.
         Item.Width  := Total_Width + 2 * Item.Border_Spacing;
         Item.Height := Total_Height + Get_Height (Item.Child.all)
           + 2 * Item.Border_Spacing;

         if Hide_Big_Items
           and then Item.Height > Context.Big_Item_Height
         then
            Item.Visible := False;
         end if;
      end if;

      if not Item.Visible then
         Item.Width := Left_Border + 2 * Item.Border_Spacing +
           Get_Width (Context.Hidden_Pixmap);
         Item.Height := 2 * Item.Border_Spacing +
           Get_Height (Context.Hidden_Pixmap);
      end if;
   end Size_Request;

   ------------------------
   -- Get_Component_Name --
   ------------------------

   overriding function Get_Component_Name
     (Item : access Class_Type;
      Lang : access Language.Language_Root'Class;
      Name : String;
      Comp : Generic_Type_Access) return String
   is
      pragma Unreferenced (Item, Lang, Comp);
   begin
      --  Comp is either one of the ancestors or the child, but in any case
      --  it doesn't have its own name.

      return Name;
   end Get_Component_Name;

   ------------------------
   -- Get_Component_Name --
   ------------------------

   overriding function Get_Component_Name
     (Item : access Class_Type;
      Lang : access Language_Root'Class;
      Name : String;
      X, Y : Glib.Gint) return String
   is
      Total_Height : Gint := 0;
   begin
      if not Item.Visible then
         return Name;
      end if;

      --  Click in the left column ? => Select the whole item

      if X <= Left_Border then
         return Name;
      end if;

      for A in Item.Ancestors'Range loop
         if Y <= Total_Height + Item.Ancestors (A).Height then
            --  Do not substract Left_Border from X, since the ancestor is
            --  a Class_Type and already has it.
            return Get_Component_Name
              (Item.Ancestors (A), Lang, Name, X, Y - Total_Height);
         end if;

         Total_Height := Total_Height + Item.Ancestors (A).Height;
      end loop;

      return Get_Component_Name
        (Item.Child, Lang, Name, X - Left_Border, Y - Total_Height);
   end Get_Component_Name;

   -------------------
   -- Get_Component --
   -------------------

   overriding function Get_Component
     (Item : access Class_Type;
      X, Y : Glib.Gint) return Generic_Type_Access
   is
      Total_Height : Gint := 0;
   begin
      if not Item.Valid or else not Item.Visible then
         return Generic_Type_Access (Item);
      end if;

      --  Click in the left column ? => Select the whole item

      if X < Left_Border then
         return Generic_Type_Access (Item);
      end if;

      for A in Item.Ancestors'Range loop
         if Y <= Total_Height + Item.Ancestors (A).Height then
            return Get_Component (Item.Ancestors (A), X, Y - Total_Height);
         end if;
         Total_Height := Total_Height + Item.Ancestors (A).Height;
      end loop;

      return Get_Component (Item.Child, X - Left_Border, Y - Total_Height);
   end Get_Component;

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

   ---------------------
   -- Propagate_Width --
   ---------------------

   overriding procedure Propagate_Width
     (Item  : in out Class_Type;
      Width : Glib.Gint) is
   begin
      Item.Width := Width;

      if Item.Visible then
         for A in Item.Ancestors'Range loop
            Propagate_Width
              (Item.Ancestors (A).all, Width - 2 * Item.Border_Spacing);
         end loop;
         Propagate_Width
           (Item.Child.all, Width - Left_Border - 2 * Item.Border_Spacing);
      end if;
   end Propagate_Width;

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
