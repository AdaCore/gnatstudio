-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2003-2007, AdaCore             --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Glib.Object;  use Glib.Object;
with Gtkada.Types; use Gtkada.Types;
with Scripts.Impl; use Scripts.Impl;

package body Scripts.Gtkada is

   type GObject_Properties_Record is new Instance_Property_Record with record
      Obj : Glib.Object.GObject;
   end record;
   type GObject_Properties is access all GObject_Properties_Record'Class;
   overriding procedure Destroy (Prop : in out GObject_Properties_Record);

   type CIR_Data_Type (Length : Natural) is record
      CIR           : Class_Instance_Record_Access;
      Property_Name : String (1 .. Length);
   end record;
   package CIR_User_Data is new Glib.Object.User_Data
     (Data_Type => CIR_Data_Type);

   procedure On_Widget_Data_Destroyed (CIR : CIR_Data_Type);
   --  Called when the widget associated with CIR is destroyed

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Prop : in out GObject_Properties_Record) is
   begin
      --  Nothing to do; the object holds a reference to the
      --  instance, the opposite is not true. The instance will
      --  never be destroyed while the object exists in this case.
      --
      --  We shouldn't be able to free Obj while the widget's user
      --  data still exists, since it holds a reference to the
      --  class_instance. Therefore Free should only be called when
      --  Obj has already been reset to null when
      --  On_Widget_Data_Destroyed has been called.

      if Prop.Obj /= null then
         Prop.Obj := null;
      end if;
   end Destroy;

   ------------------------------
   -- On_Widget_Data_Destroyed --
   ------------------------------

   procedure On_Widget_Data_Destroyed (CIR : CIR_Data_Type) is
      Data : User_Data_List := CIR.CIR.User_Data;
   begin
      --  Warning: it is possible that the Ada handle to the widget has already
      --  been deallocated, through a call to Glib.Object.Free_Data. The order
      --  of calls between Free_Data and On_Widget_Data_Destroyed is undefined,
      --  since they are both associated with user data stored in the C widget.
      --  As a result, we shouldn't use the Ada handle here!

      while Data /= null loop
         if Data.Name = CIR.Property_Name then
            GObject_Properties (Data.Prop).Obj := null;
            exit;
         end if;
         Data := Data.Next;
      end loop;

      Decref (CIR.CIR); --  Might free CIR, do not reuse afterward!
   end On_Widget_Data_Destroyed;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Instance : Class_Instance;
      Widget   : Glib.Object.GObject;
      Name     : String := GUI_Data_Name) is
   begin
      Set_Data
        (Instance, Name, GObject_Properties_Record'(Obj => Widget));

      --  The widget will hold a reference to the Instance, so that the
      --  instance is not destroyed while the widget is in use
      Incref (Get_CIR (Instance));

      --  Use a name specific to the scripting language, so that the same
      --  widget can have corresponding instances in several languages
      CIR_User_Data.Set
        (Widget, CIR_Data_Type'
           (CIR           => Get_CIR (Instance),
            Length        => Name'Length,
            Property_Name => Name),
         "GPS-Instance-" & Get_Name (Instance.Data.Data.Script),
         On_Destroyed => On_Widget_Data_Destroyed'Access);
   end Set_Data;

   ------------------
   -- Get_Instance --
   ------------------

   function Get_Instance
     (Script : access Scripting_Language_Record'Class;
      Widget : access Glib.Object.GObject_Record'Class)
      return Class_Instance is
   begin
      return From_Instance
        (Script, CIR_User_Data.Get
           (Widget, "GPS-Instance-" & Get_Name (Script)).CIR);
   exception
      when Data_Error =>
         return No_Class_Instance;
   end Get_Instance;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (Instance : Class_Instance;
      Name     : String := GUI_Data_Name) return Glib.Object.GObject
   is
      Prop : constant Instance_Property_Record'Class :=
        Get_Data (Instance, Name);
   begin
      return GObject_Properties_Record (Prop).Obj;
   end Get_Data;

end Scripts.Gtkada;
