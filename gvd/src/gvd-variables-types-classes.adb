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

with Glib;                        use Glib;
with Gtkada.Canvas_View;          use Gtkada.Canvas_View;
with Browsers;                    use Browsers;
with GVD.Canvas;
with GVD.Variables.Types.Records; use GVD.Variables.Types.Records;

package body GVD.Variables.Types.Classes is

   type Class_Iterator is new Generic_Iterator with record
      Item     : GVD_Class_Type_Access;
      Ancestor : Natural;
   end record;
   overriding procedure Next (Iter : in out Class_Iterator);
   overriding function At_End (Iter : Class_Iterator) return Boolean;
   overriding function Data
     (Iter : Class_Iterator)
      return GVD_Type_Holder'Class;
   overriding function Field_Name
     (Iter : Class_Iterator;
      Lang : not null access Language_Root'Class;
      Base : String := "") return String;

   ------------------
   -- Add_Ancestor --
   ------------------

   procedure Add_Ancestor
     (Self     : not null access GVD_Class_Type;
      Num      : Positive;
      Ancestor : GVD_Type_Holder) is
   begin
      pragma Assert (Num <= Self.Num_Ancestors);

      Self.Ancestors (Num) := Ancestor;
      GVD_Class_Type_Access (Ancestor.Get_Type).Draw_Border (False);
      Self.Valid := True;
      Self.Ancestors (Num).Get_Type.Valid := True;
   end Add_Ancestor;

   ------------
   -- At_End --
   ------------

   overriding function At_End (Iter : Class_Iterator) return Boolean is
   begin
      return Iter.Ancestor > Iter.Item.Ancestors'Last + 1
        or else (Iter.Ancestor = Iter.Item.Ancestors'Last + 1
                 and then Iter.Item.Child.Data = null);
   end At_End;

   -------------------
   -- Build_Display --
   -------------------

   overriding function Build_Display
     (Self   : not null access GVD_Class_Type;
      Holder : GVD_Type_Holder'Class;
      Name   : String;
      View   : not null access GVD.Canvas.Debugger_Data_View_Record'Class;
      Lang   : Language.Language_Access;
      Mode   : GVD.Canvas.Display_Mode) return GVD.Canvas.Component_Item
   is
      Styles : constant access Browser_Styles := View.Get_View.Get_Styles;
      Rect   : constant GVD.Canvas.Component_Item :=
        GVD.Canvas.New_Component_Item (Styles, GVD_Type_Holder (Holder), Name);
      R      : Rect_Item;
   begin
      if not Self.Valid
        or else (Self.Ancestors'Length = 0
                 and then not Self.Child.Get_Type.Is_Valid)
      then
         Rect.Add_Child
           (Gtk_New_Image
              (Styles.Invisible,
               Icon_Name => "gps-unknown-item-symbolic",
               Allow_Rescale => False, Width => 16.0, Height => 16.0));

      elsif not Self.Visible then
         Rect.Add_Child (View.Item_Hidden);

      else
         for A in Self.Ancestors'Range loop

            --  Draw the ancestor if it isn't a null record.

            if Self.Ancestors (A) /= Empty_GVD_Type_Holder then
               R := Gtk_New_Rect (Styles.Invisible);
               R.Set_Child_Layout (Horizontal_Stack);
               Rect.Add_Child (R);

               R.Add_Child
                 (Self.Ancestors (A).Get_Type.Build_Display
                  (Self.Ancestors (A), Name, View, Lang, Mode));

               if GVD_Class_Type_Access
                 (Self.Ancestors (A).Get_Type).Child /= Empty_GVD_Type_Holder
                 and then GVD_Record_Type_Access
                   (GVD_Class_Type_Access
                      (Self.Ancestors (A).Get_Type).
                        Child.Get_Type).Num_Fields > 0
               then
                  Rect.Add_Child (Gtk_New_Hr (Styles.Item));
               end if;
            end if;
         end loop;

         if Self.Child /= Empty_GVD_Type_Holder then
            Rect.Add_Child
              (Self.Child.Get_Type.Build_Display
                 (Self.Child, Name, View, Lang, Mode));
         end if;
      end if;

      return Rect;
   end Build_Display;

   -----------
   -- Clear --
   -----------

   overriding procedure Clear (Self : not null access GVD_Class_Type) is
   begin
      for A in Self.Ancestors'Range loop
         Self.Ancestors (A).Get_Type.Clear;
      end loop;

      Self.Child.Get_Type.Clear;
   end Clear;

   -----------
   -- Clone --
   -----------

   overriding procedure Clone
     (Self : not null access GVD_Class_Type;
      Item : not null GVD_Generic_Type_Access)
   is
      Src : constant GVD_Class_Type_Access := GVD_Class_Type_Access (Item);
   begin
      GVD_Generic_Type (Self.all).Clone (Item);

      for A in Src.Ancestors'Range loop
         Self.Ancestors (A) := Src.Ancestors (A).Clone;
      end loop;

      Self.Child := Src.Child.Clone;
   end Clone;

   ----------
   -- Data --
   ----------

   overriding function Data
     (Iter : Class_Iterator) return GVD_Type_Holder'Class is
   begin
      if Iter.Ancestor <= Iter.Item.Ancestors'Last then
         return Iter.Item.Ancestors (Iter.Ancestor);
      else
         return Iter.Item.Child;
      end if;
   end Data;

   -----------------
   -- Draw_Border --
   -----------------

   procedure Draw_Border
     (Self : not null access GVD_Class_Type;
      Draw : Boolean := True) is
   begin
      if Draw then
         Self.Border_Spacing := Border_Spacing;
      else
         Self.Border_Spacing := 0;
      end if;
   end Draw_Border;

   ----------------
   -- Field_Name --
   ----------------

   overriding function Field_Name
     (Iter : Class_Iterator;
      Lang : not null access Language_Root'Class;
      Base : String := "") return String
   is
      pragma Unreferenced (Lang);
   begin
      if Iter.Ancestor <= Iter.Item.Ancestors'Last then
         return "<parent class>";

      elsif Base = "" then
         return "<record>";

      else
         return Base;
      end if;
   end Field_Name;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : not null access GVD_Class_Type) is
   begin
      for A in Self.Ancestors'Range loop
         Self.Ancestors (A) := Empty_GVD_Type_Holder;
      end loop;

      Self.Child := Empty_GVD_Type_Holder;
      GVD_Generic_Type (Self.all).Free;
   end Free;

   ------------------
   -- Get_Ancestor --
   ------------------

   function Get_Ancestor
     (Self : not null access GVD_Class_Type;
      Num  : Positive)
      return GVD_Type_Holder is
   begin
      pragma Assert (Num <= Self.Num_Ancestors);
      return Self.Ancestors (Num);
   end Get_Ancestor;

   ---------------
   -- Get_Child --
   ---------------

   function Get_Child
     (Self : not null access GVD_Class_Type)
      return GVD_Type_Holder is
   begin
      return Self.Child;
   end Get_Child;

   -----------------------
   -- Get_Num_Ancestors --
   -----------------------

   function Get_Num_Ancestors
     (Self : not null access GVD_Class_Type)
      return Natural is
   begin
      return Self.Num_Ancestors;
   end Get_Num_Ancestors;

   --------------------
   -- New_Class_Type --
   --------------------

   function New_Class_Type
     (Num_Ancestors : Natural) return GVD_Type_Holder
   is
      Data : constant GVD_Type_Holder_Data_Access :=
        new GVD_Type_Holder_Data'
          (Count    => 1,
           Instance => new GVD_Class_Type (Num_Ancestors));
   begin
      return GVD_Type_Holder'(Ada.Finalization.Controlled with Data);
   end New_Class_Type;

   ----------
   -- Next --
   ----------

   overriding procedure Next (Iter : in out Class_Iterator) is
   begin
      Iter.Ancestor := Iter.Ancestor + 1;
   end Next;

   -------------
   -- Replace --
   -------------

   overriding function Replace
     (Self         : not null access GVD_Class_Type;
      Current      : GVD_Type_Holder'Class;
      Replace_With : GVD_Type_Holder'Class) return GVD_Type_Holder'Class is
   begin
      for A in Self.Ancestors'Range loop
         if Self.Ancestors (A).Data = Current.Data then
            Self.Ancestors (A) := GVD_Type_Holder (Replace_With);
            return Replace_With;
         end if;
      end loop;

      if Self.Child.Data = Current.Data then
         Self.Child := GVD_Type_Holder (Replace_With);
         return Replace_With;
      end if;

      return Empty_GVD_Type_Holder;
   end Replace;

   ---------------
   -- Set_Child --
   ---------------

   procedure Set_Child
     (Self  : not null access GVD_Class_Type;
      Child : GVD_Type_Holder) is
   begin
      pragma Assert (Self.Child.Data = null);

      Self.Child := Child;
      GVD_Record_Type_Access (Child.Get_Type).Draw_Border (False);
   end Set_Child;

   -------------------
   -- Set_Type_Name --
   -------------------

   overriding procedure Set_Type_Name
     (Self : not null access GVD_Class_Type;
      Name : String)
   is
   begin
      if Self.Child.Data /= null then
         Self.Child.Get_Type.Set_Type_Name (Name);
      end if;
   end Set_Type_Name;

   -----------
   -- Start --
   -----------

   overriding function Start
     (Self : not null access GVD_Class_Type) return Generic_Iterator'Class
   is
      Iter : Class_Iterator;
   begin
      Iter.Item := GVD_Class_Type_Access (Self);
      if Self.Ancestors'Length = 0 then
         Iter.Ancestor := Self.Ancestors'Last + 1;
      else
         Iter.Ancestor := Self.Ancestors'First;
      end if;

      return Iter;
   end Start;

   -----------------------------
   -- Structurally_Equivalent --
   -----------------------------

   overriding function Structurally_Equivalent
     (Self : not null access GVD_Class_Type;
      Item : GVD_Type_Holder'Class)
      return Boolean
   is
      Result : Boolean;
   begin
      if Item.Data = null
        or else Item.Data.Instance = null
        or else Item.Data.Instance.all not in GVD_Class_Type'Class
      then
         return False;
      end if;

      Result := Self.Num_Ancestors = GVD_Class_Type_Access
        (Item.Get_Type).Num_Ancestors
        and then Self.Child.Get_Type.Structurally_Equivalent
          (GVD_Class_Type_Access (Item.Get_Type).Child);

      if Result then
         for A in Self.Ancestors'Range loop
            Result := Result
              and then Self.Ancestors (A).Get_Type.Structurally_Equivalent
              (GVD_Class_Type_Access (Item.Get_Type).Ancestors (A));
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
         if Self.Num_Ancestors /= 0 then
            Result := Self.Ancestors (1).Get_Type.Structurally_Equivalent
              (Item);
         end if;

         if GVD_Class_Type_Access (Item.Get_Type).Num_Ancestors /= 0 then
            Result := Result or else Self.Structurally_Equivalent
              (GVD_Class_Type_Access (Item.Get_Type).Ancestors (1));
         end if;
      end if;

      return Result;
   end Structurally_Equivalent;

end GVD.Variables.Types.Classes;
