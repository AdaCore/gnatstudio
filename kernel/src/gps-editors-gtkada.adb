------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2009-2019, AdaCore                     --
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
with Glib;             use Glib;
with Glib.Object;      use Glib.Object;
with System;

package body GPS.Editors.GtkAda is

   Editor_Mark_Type : Glib.GType := Glib.GType_None;
   --  Initialized only the first time it is needed.

   type Editor_Mark_Access is access all Editor_Mark'Class;

   function Editor_Mark_Copy (Boxed : System.Address) return System.Address;
   pragma Convention (C, Editor_Mark_Copy);
   procedure Editor_Mark_Free (Boxed : System.Address);
   pragma Convention (C, Editor_Mark_Free);
   --  Subprograms required for the support of GValue

   pragma Warnings (Off);
   --  This UC is safe aliasing-wise, so kill warning
   function To_Editor_Mark is new Ada.Unchecked_Conversion
      (System.Address, Editor_Mark_Access);
   function To_Address is new Ada.Unchecked_Conversion
     (Editor_Mark_Access, System.Address);
   pragma Warnings (On);

   ----------------------
   -- Editor_Mark_Copy --
   ----------------------

   function Editor_Mark_Copy (Boxed : System.Address) return System.Address is
      Value : constant Editor_Mark_Access := To_Editor_Mark (Boxed);
      Value2 : Editor_Mark_Access;
   begin
      if Value = null then
         return System.Null_Address;
      end if;

      Value2 := new Editor_Mark'Class'(Value.all);
      return Value2.all'Address;
   end Editor_Mark_Copy;

   ----------------------
   -- Editor_Mark_Free --
   ----------------------

   procedure Editor_Mark_Free (Boxed : System.Address) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Editor_Mark'Class, Editor_Mark_Access);
      Value : Editor_Mark_Access := To_Editor_Mark (Boxed);
   begin
      Unchecked_Free (Value);
   end Editor_Mark_Free;

   --------------------------
   -- Get_Editor_Mark_Type --
   --------------------------

   function Get_Editor_Mark_Type return Glib.GType is
   begin
      if Editor_Mark_Type = Glib.GType_None then
         Editor_Mark_Type := Glib.Boxed_Type_Register_Static
           ("Editor_Mark",
            Editor_Mark_Copy'Access,
            Editor_Mark_Free'Access);
      end if;
      return Editor_Mark_Type;
   end Get_Editor_Mark_Type;

   --------------
   -- Set_Mark --
   --------------

   procedure Set_Mark (Value : in out Glib.Values.GValue;
                       Mark  : Editor_Mark'Class)
   is
      --  Val : Editor_Mark_Access;
   begin
      if Mark = Nil_Editor_Mark then
         Set_Boxed (Value, System.Null_Address);
      else
         --  This results in a call to Editor_Mark_Copy, which already
         --  allocates a new pointer. We can safely take 'Access since tagged
         --  types are passed by ref anyway, and the access is only needed
         --  while Set_Boxes executes

         Set_Boxed (Value, To_Address (Mark'Unrestricted_Access));
      end if;
   end Set_Mark;

   --------------
   -- Get_Mark --
   --------------

   function Get_Mark (Value : Glib.Values.GValue) return Editor_Mark'Class is
      Val : constant Editor_Mark_Access := To_Editor_Mark (Get_Boxed (Value));
   begin
      if Val = null then
         return Nil_Editor_Mark;
      else
         return Val.all;
      end if;
   end Get_Mark;

   --------------
   -- Get_Mark --
   --------------

   function Get_Mark
     (Model  : not null
          access Gtk.Tree_Model.Gtk_Root_Tree_Model_Record'Class;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Glib.Gint) return Editor_Mark'Class
   is
      Value : GValue;
   begin
      Get_Value (To_Interface (Model), Iter, Column, Value);
      declare
         Mark : constant Editor_Mark'Class := Get_Mark (Value);
      begin
         Unset (Value);
         return Mark;
      end;
   end Get_Mark;

   -------------------
   -- Get_MDI_Child --
   -------------------

   function Get_MDI_Child
     (This : Editor_View'Class) return Standard.Gtkada.MDI.MDI_Child
   is
      Stub : Standard.Gtkada.MDI.MDI_Child_Record;
      pragma Warnings (Off, Stub);
      use type System.Address;
      A : System.Address;
   begin
      A := GPS.Editors.Get_MDI_Child (This);
      if A = System.Null_Address then
         return null;
      else
         return Standard.Gtkada.MDI.MDI_Child (Get_User_Data (A, Stub));
      end if;
   end Get_MDI_Child;

end GPS.Editors.GtkAda;
