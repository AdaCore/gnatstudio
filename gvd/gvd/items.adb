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

with Glib;              use Glib;
with Language;          use Language;
with Debugger;          use Debugger;
with Language.Debugger; use Language.Debugger;

package body Items is

   use type GNAT.Strings.String_Access;

   Max_Item_Width : Positive;
   --  Maximal width (in pixels) an item can have

   Max_Item_Height : Positive;
   --  Maximal height (in pixels) an item can have

   ---------------
   -- Get_Width --
   ---------------

   function Get_Width (Item : Generic_Type) return Glib.Gint is
   begin
      return Item.Width;
   end Get_Width;

   ----------------
   -- Get_Height --
   ----------------

   function Get_Height (Item : Generic_Type) return Glib.Gint is
   begin
      return Item.Height;
   end Get_Height;

   -----------
   -- Get_X --
   -----------

   function Get_X (Item : Generic_Type) return Glib.Gint is
   begin
      return Item.X;
   end Get_X;

   -----------
   -- Get_Y --
   -----------

   function Get_Y (Item : Generic_Type) return Glib.Gint is
   begin
      return Item.Y;
   end Get_Y;

   ---------------
   -- Set_Valid --
   ---------------

   procedure Set_Valid
     (Item  : access Generic_Type; Valid : Boolean := True) is
   begin
      Item.Valid := Valid;
   end Set_Valid;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Item : access Generic_Type) return Boolean is
   begin
      return Item.Valid;
   end Is_Valid;

   --------------------
   -- Set_Visibility --
   --------------------

   procedure Set_Visibility
     (Item      : access Generic_Type;
      Visible   : Boolean;
      Recursive : Boolean := False)
   is
      Iter : Generic_Iterator'Class := Start (Generic_Type_Access (Item));
   begin
      Item.Visible := Visible;

      if Recursive then
         while not At_End (Iter) loop
            Set_Visibility (Data (Iter), Visible, Recursive);
            Next (Iter);
         end loop;
      end if;
   end Set_Visibility;

   --------------------
   -- Get_Visibility --
   --------------------

   function Get_Visibility (Item : Generic_Type) return Boolean is
   begin
      return Item.Visible;
   end Get_Visibility;

   --------------------------
   -- Component_Is_Visible --
   --------------------------

   procedure Component_Is_Visible
     (Item       : access Generic_Type;
      Component  : access Generic_Type'Class;
      Is_Visible : out Boolean;
      Found      : out Boolean)
   is
      Iter : Generic_Iterator'Class := Start (Generic_Type_Access (Item));
      F    : Boolean;
   begin
      if Generic_Type_Access (Component) = Generic_Type_Access (Item) then
         Is_Visible := Item.Visible;
         Found := True;
         return;
      end if;

      while not At_End (Iter) loop
         Component_Is_Visible
           (Data (Iter), Component, Is_Visible, F);
         if F then
            Is_Visible := Is_Visible and then Item.Visible;
            Found := True;
            return;
         end if;
         Next (Iter);
      end loop;

      Found := False;
   end Component_Is_Visible;

--     --------------------
--     -- Display_Pixmap --
--     --------------------
--
--     procedure Display_Pixmap
--       (On_Pixmap : Gdk_Pixmap;
--        GC        : Gdk_GC;
--        Pixmap    : Gdk_Pixmap;
--        Mask      : Gdk_Bitmap;
--        X, Y      : Gint) is
--     begin
--        Set_Clip_Mask (GC, Mask);
--        Set_Clip_Origin (GC, X, Y);
--        Draw_Pixmap (On_Pixmap, GC, Pixmap, 0, 0, X, Y);
--        Set_Clip_Mask (GC, Null_Pixmap);
--        Set_Clip_Origin (GC, 0, 0);
--     end Display_Pixmap;

   ---------------------
   -- Propagate_Width --
   ---------------------

   procedure Propagate_Width
     (Item  : in out Generic_Type;
      Width : Glib.Gint) is
   begin
      Item.Width := Width;
   end Propagate_Width;

   ------------------
   -- Set_Selected --
   ------------------

   procedure Set_Selected
     (Item     : access Generic_Type;
      Selected : Boolean := True) is
   begin
      Item.Selected := Selected;
   end Set_Selected;

   ------------------
   -- Get_Selected --
   ------------------

   function Get_Selected (Item : access Generic_Type) return Boolean is
   begin
      return Item.Selected;
   end Get_Selected;

   -------------------
   -- Set_Type_Name --
   -------------------

   procedure Set_Type_Name
     (Item : access Generic_Type;
      Name : String) is
   begin
      GNAT.Strings.Free (Item.Type_Name);
      Item.Type_Name := new String'(Name);
   end Set_Type_Name;

   -------------------
   -- Get_Type_Name --
   -------------------

   function Get_Type_Name
     (Item    : access Generic_Type;
      Lang    : Language.Language_Access)
     return String is
   begin
      if Item.Type_Name = null then
         return "";

      --  Lazy evaluation ?
      elsif Item.Type_Name'Length > Unknown_Type_Prefix'Length
        and then Item.Type_Name
        (Item.Type_Name'First
         .. Item.Type_Name'First + Unknown_Type_Prefix'Length - 1) =
        Unknown_Type_Prefix
      then
         declare
            Entity_Start, Default_Start : Positive;
            Debugger : constant Debugger_Access :=
              Get_Debugger (Language_Debugger_Access (Lang));
         begin
            Entity_Start := Item.Type_Name'First + Unknown_Type_Prefix'Length;

            Default_Start := Entity_Start;
            while Default_Start < Item.Type_Name'Last
              and then Item.Type_Name (Default_Start) /= ASCII.LF
            loop
               Default_Start := Default_Start + 1;
            end loop;

            Set_Type_Name
              (Item,
               Get_Type_Info
               (Debugger,
                Item.Type_Name (Entity_Start .. Default_Start - 1),
                Item.Type_Name (Default_Start + 1 .. Item.Type_Name'Last)));
            return Item.Type_Name.all;
         end;
      else
         return Item.Type_Name.all;
      end if;
   end Get_Type_Name;

   ----------------
   -- Show_Value --
   ----------------

   function Show_Value (Mode : Display_Mode) return Boolean is
   begin
      return Mode = Value or else Mode = Type_Value;
   end Show_Value;

   ---------------
   -- Show_Type --
   ---------------

   function Show_Type (Mode : Display_Mode) return Boolean is
   begin
      return Mode = Type_Only or else Mode = Type_Value;
   end Show_Type;

   -----------------------
   -- Clone_Dispatching --
   -----------------------

   procedure Clone_Dispatching
     (Item  : Generic_Type;
      Clone : in out Generic_Type_Access) is
   begin
      if Item.Type_Name /= null then
         Clone.Type_Name := new String'(Item.Type_Name.all);
      end if;
   end Clone_Dispatching;

   -----------
   -- Clone --
   -----------

   function Clone (Item : Generic_Type'Class) return Generic_Type_Access is
      R : Generic_Type_Access := new Generic_Type'Class'(Item);
   begin
      Clone_Dispatching (Item, R);
      return R;
   end Clone;

   ----------
   -- Free --
   ----------

   procedure Free
     (Item : access Generic_Type;
      Only_Value : Boolean := False)
   is
      J : Generic_Type_Access := Generic_Type_Access (Item);
   begin
      if not Only_Value then
         GNAT.Strings.Free (Item.Type_Name);
         Free_Internal (J);
      end if;
   end Free;

   -----------
   -- Start --
   -----------

   function Start (Item : access Generic_Type) return Generic_Iterator'Class is
      pragma Unreferenced (Item);

      Iter : Generic_Iterator;
   begin
      return Iter;
   end Start;

   ------------
   -- At_End --
   ------------

   function At_End (Iter : Generic_Iterator) return Boolean is
      pragma Unreferenced (Iter);
   begin
      return True;
   end At_End;

   ----------
   -- Next --
   ----------

   procedure Next (Iter : in out Generic_Iterator) is
      pragma Unreferenced (Iter);
   begin
      null;
   end Next;

   ----------
   -- Data --
   ----------

   function Data (Iter : Generic_Iterator) return Generic_Type_Access is
      pragma Unreferenced (Iter);
   begin
      return null;
   end Data;

   ---------------------
   -- Reset_Recursive --
   ---------------------

   procedure Reset_Recursive (Item : access Generic_Type) is
      Iter : Generic_Iterator'Class := Start (Generic_Type_Access (Item));
      It   : Generic_Type_Access;

   begin
      while not At_End (Iter) loop
         It := Data (Iter);

         if It /= null then
            Reset_Recursive (It);
         end if;

         Next (Iter);
      end loop;
   end Reset_Recursive;

   ---------------------
   -- Constraint_Size --
   ---------------------

   procedure Constraint_Size (Item : in out Generic_Type) is
   begin
      if Item.Width > Gint (Max_Item_Width) then
         Item.Width := Gint (Max_Item_Width);
      end if;

      if Item.Height > Gint (Max_Item_Height) then
         Item.Height := Gint (Max_Item_Height);
      end if;
   end Constraint_Size;

   --------------------
   -- Set_Max_Height --
   --------------------

   procedure Set_Max_Height (Height : Positive) is
   begin
      Max_Item_Height := Height;
   end Set_Max_Height;

   -------------------
   -- Set_Max_Width --
   -------------------

   procedure Set_Max_Width  (Width : Positive) is
   begin
      Max_Item_Width := Width;
   end Set_Max_Width;

end Items;
