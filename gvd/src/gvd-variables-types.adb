------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2017-2019, AdaCore                     --
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

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

package body GVD.Variables.Types is

   GVD_Type_Holder_GType : Glib.GType := Glib.GType_None;
   --  Initialized only the first time this is needed, since we need glib
   --  initialized for this.

   function GVD_Type_Holder_Boxed_Copy
     (Boxed : System.Address)
      return System.Address;
   pragma Convention (C, GVD_Type_Holder_Boxed_Copy);
   procedure GVD_Type_Holder_Boxed_Free (Boxed : System.Address);
   pragma Convention (C, GVD_Type_Holder_Boxed_Free);
   function To_GVD_Type_Holder_Data_Access is new Ada.Unchecked_Conversion
     (System.Address, GVD_Type_Holder_Data_Access);
   --  Subprograms required for the support of GValue

   procedure Free is new Ada.Unchecked_Deallocation
     (GVD_Type_Holder_Data, GVD_Type_Holder_Data_Access);

   procedure Internal_Free is new Ada.Unchecked_Deallocation
     (GVD_Generic_Type'Class, GVD_Generic_Type_Access);

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Self : in out GVD_Type_Holder) is
   begin
      if Self.Data /= null then
         Self.Data.Count := Self.Data.Count + 1;
      end if;
   end Adjust;

   ------------------------
   -- As_GVD_Type_Holder --
   ------------------------

   function As_GVD_Type_Holder
     (Self : GVD_Type_Holder)
      return Glib.Values.GValue is
   begin
      return Result : Glib.Values.GValue do
         Glib.Values.Init (Result, Get_GVD_Type_Holder_GType);
         Set_Value        (Result, Self);
      end return;
   end As_GVD_Type_Holder;

   -----------
   -- Clone --
   -----------

   function Clone (Self : GVD_Type_Holder) return GVD_Type_Holder is
   begin
      if Self.Data /= null
        and then Self.Data.Instance /= null
      then
         declare
            Data : constant GVD_Type_Holder_Data_Access :=
              new GVD_Type_Holder_Data'
                (Count    => 1,
                 Instance => new GVD_Generic_Type'Class'
                   (Self.Data.Instance.all));
         begin
            Data.Instance.Clone (Self.Data.Instance);
            return GVD_Type_Holder'(Ada.Finalization.Controlled with Data);
         end;
      else
         return Empty_GVD_Type_Holder;
      end if;
   end Clone;

   -----------
   -- Clone --
   -----------

   procedure Clone
     (Self : not null access GVD_Generic_Type;
      Item : not null GVD_Generic_Type_Access) is
   begin
      if Item.Type_Name /= Null_Unbounded_String then
         Self.Type_Name := Item.Type_Name;
      end if;
   end Clone;

   ---------------------------
   -- Create_Empty_Iterator --
   ---------------------------

   function Create_Empty_Iterator return Generic_Iterator'Class is
   begin
      return Empty_Iterator'(Generic_Iterator with null record);
   end Create_Empty_Iterator;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out GVD_Type_Holder) is
   begin
      if Self.Data /= null then
         if Self.Data.Count = 1 then
            if Self.Data.Instance /= null then
               Self.Data.Instance.Free;
               Internal_Free (Self.Data.Instance);
            end if;
            Free (Self.Data);

         else
            Self.Data.Count := Self.Data.Count - 1;
            Self.Data       := null;
         end if;
      end if;
   end Finalize;

   ----------
   -- Free --
   ----------

   procedure Free (Self : not null access GVD_Generic_Type) is
   begin
      Self.Type_Name := Null_Unbounded_String;
   end Free;

   -------------------------------
   -- Get_GVD_Type_Holder_GType --
   -------------------------------

   function Get_GVD_Type_Holder_GType return Glib.GType is
      use Glib;
   begin
      if GVD_Type_Holder_GType = Glib.GType_None then
         GVD_Type_Holder_GType := Glib.Boxed_Type_Register_Static
           ("GVD_Variable_Type",
            GVD_Type_Holder_Boxed_Copy'Access,
            GVD_Type_Holder_Boxed_Free'Access);
      end if;

      return GVD_Type_Holder_GType;
   end Get_GVD_Type_Holder_GType;

   --------------
   -- Get_Type --
   --------------

   function Get_Type
     (Self : GVD_Type_Holder) return GVD_Generic_Type_Access is
   begin
      if Self.Data /= null then
         return Self.Data.Instance;
      else
         return null;
      end if;
   end Get_Type;

   -------------------
   -- Get_Type_Name --
   -------------------

   function Get_Type_Name
     (Self    : not null access GVD_Generic_Type)
      return String is
   begin
      return To_String (Self.Type_Name);
   end Get_Type_Name;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
     (Value : Glib.Values.GValue)
      return GVD_Type_Holder
   is
      Data : constant GVD_Type_Holder_Data_Access :=
        To_GVD_Type_Holder_Data_Access (Glib.Values.Get_Boxed (Value));
   begin
      if Data /= null then
         Data.Count := Data.Count + 1;
         return GVD_Type_Holder'(Ada.Finalization.Controlled with Data);
      else
         return Empty_GVD_Type_Holder;
      end if;
   end Get_Value;

   --------------------
   -- Get_Visibility --
   --------------------

   function Get_Visibility
     (Self : not null access GVD_Generic_Type) return Boolean is
   begin
      return Self.Visible;
   end Get_Visibility;

   --------------------------------
   -- GVD_Type_Holder_Boxed_Copy --
   --------------------------------

   function GVD_Type_Holder_Boxed_Copy
     (Boxed : System.Address) return System.Address
   is
      Value : constant GVD_Type_Holder_Data_Access :=
        To_GVD_Type_Holder_Data_Access (Boxed);
   begin
      if Value /= null then
         Value.Count := Value.Count + 1;
      end if;

      return Boxed;
   end GVD_Type_Holder_Boxed_Copy;

   --------------------------------
   -- GVD_Type_Holder_Boxed_Free --
   --------------------------------

   procedure GVD_Type_Holder_Boxed_Free (Boxed : System.Address) is
      Value : GVD_Type_Holder_Data_Access :=
        To_GVD_Type_Holder_Data_Access (Boxed);
   begin
      --  Release the reference we owned
      if Value /= null then
         if Value.Count = 1 then
            if Value.Instance /= null then
               Value.Instance.Free;
               Internal_Free (Value.Instance);
            end if;
            Free (Value);

         else
            Value.Count := Value.Count - 1;
         end if;
      end if;
   end GVD_Type_Holder_Boxed_Free;

   --------
   -- Id --
   --------

   function Id
     (Self : GVD_Type_Holder)
      return System.Storage_Elements.Integer_Address is
   begin
      if Self.Data = null then
         return System.Storage_Elements.To_Integer (System.Null_Address);
      else
         return System.Storage_Elements.To_Integer (Self.Data.all'Address);
      end if;
   end Id;

   ----------------
   -- Is_Changed --
   ----------------

   function Is_Changed
     (Self : not null access GVD_Generic_Type) return Boolean
   is
      Iter : Generic_Iterator'Class := GVD_Generic_Type'Class (Self.all).Start;
   begin
      while not Iter.At_End loop
         if GVD_Type_Holder (Iter.Data) /= Empty_GVD_Type_Holder
           and then Iter.Data.Get_Type.Is_Changed
         then
            return True;
         end if;
         Iter.Next;
      end loop;
      return False;
   end Is_Changed;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid
     (Self : not null access GVD_Generic_Type) return Boolean is
   begin
      return Self.Valid;
   end Is_Valid;

   ---------------------
   -- Reset_Recursive --
   ---------------------

   procedure Reset_Recursive (Self : not null access GVD_Generic_Type) is
      Iter : Generic_Iterator'Class :=
        GVD_Generic_Type'Class (Self.all).Start;
   begin
      while not Iter.At_End loop
         if GVD_Type_Holder (Iter.Data) /= Empty_GVD_Type_Holder then
            Iter.Data.Get_Type.Reset_Recursive;
         end if;
         Iter.Next;
      end loop;
   end Reset_Recursive;

   -------------------
   -- Set_Type_Name --
   -------------------

   procedure Set_Type_Name
     (Self : not null access GVD_Generic_Type;
      Name : String) is
   begin
      Self.Type_Name := To_Unbounded_String (Name);
   end Set_Type_Name;

   ---------------
   -- Set_Valid --
   ---------------

   procedure Set_Valid
     (Self  : not null access GVD_Generic_Type; Valid : Boolean := True) is
   begin
      Self.Valid := Valid;
   end Set_Valid;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Value : in out Glib.Values.GValue;
      Holder : GVD_Type_Holder) is
   begin
      if Holder.Data = null then
         Glib.Values.Set_Boxed (Value, System.Null_Address);
      else
         --  This results in a call to GVD_Variable_Type_Boxed_Copy, so
         --  increases the refcount of Item.Data.Count (which is expected
         --  since we now own one).
         Glib.Values.Set_Boxed (Value, Holder.Data.all'Address);
      end if;
   end Set_Value;

   --------------------
   -- Set_Visibility --
   --------------------

   procedure Set_Visibility
     (Self      : not null access GVD_Generic_Type;
      Visible   : Boolean;
      Recursive : Boolean := False)
   is
      Iter : Generic_Iterator'Class := Self.Start;
   begin
      Self.Visible := Visible;

      if Recursive then
         while not Iter.At_End loop
            Iter.Data.Get_Type.Set_Visibility (Visible, Recursive);
            Iter.Next;
         end loop;
      end if;
   end Set_Visibility;

   -----------
   -- Start --
   -----------

   function Start
     (Self : not null access GVD_Generic_Type) return Generic_Iterator'Class
   is
      pragma Unreferenced (Self);
      Iter : Empty_Iterator;
   begin
      return Iter;
   end Start;

end GVD.Variables.Types;
