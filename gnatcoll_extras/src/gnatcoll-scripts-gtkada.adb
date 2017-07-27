------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2003-2017, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Glib.Object;           use Glib.Object;
with Gtkada.Types;          use Gtkada.Types;

package body GNATCOLL.Scripts.Gtkada is

   type GObject_Properties_Record is new Instance_Property_Record with record
      Obj : Glib.Object.GObject;
   end record;
   type GObject_Properties is access all GObject_Properties_Record'Class;
   overriding procedure Destroy (Prop : in out GObject_Properties_Record);

   type CIR_Data_Type (Length : Natural) is record
      Inst          : Class_Instance;
      Property_Name : String (1 .. Length);
   end record;
   package CIR_User_Data is new Glib.Object.User_Data
     (Data_Type => CIR_Data_Type);

   procedure On_Widget_Data_Destroyed (CIR : CIR_Data_Type);
   --  Called when the widget associated with CIR is destroyed

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Prop : in out GObject_Properties_Record) is
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

      Prop.Obj := null;
   end Destroy;

   ------------------------------
   -- On_Widget_Data_Destroyed --
   ------------------------------

   procedure On_Widget_Data_Destroyed (CIR : CIR_Data_Type) is
      U : constant access User_Data_List := Get_CIR (CIR.Inst).Get_User_Data;
      Data : User_Data_List;
   begin
      --  Warning: it is possible that the Ada handle to the widget has already
      --  been deallocated, through a call to Glib.Object.Free_Data. The order
      --  of calls between Free_Data and On_Widget_Data_Destroyed is undefined,
      --  since they are both associated with user data stored in the C widget.
      --  As a result, we shouldn't use the Ada handle here!

      if U /= null then
         Data := U.all;
         while Data /= null loop
            if Data.Name = CIR.Property_Name then
               GObject_Properties (Data.Prop).Obj := null;
               exit;
            end if;
            Data := Data.Next;
         end loop;
      end if;
   end On_Widget_Data_Destroyed;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Instance : Class_Instance;
      Widget   : Glib.Object.GObject;
      Name     : String := GUI_Data_Name) is
   begin
      --  The widget will hold a reference to the Instance, so that the
      --  instance is not destroyed while the widget is in use

      --  Use a name specific to the scripting language, so that the same
      --  widget can have corresponding instances in several languages
      CIR_User_Data.Set
        (Widget, CIR_Data_Type'
           (Inst          => Instance,
            Length        => Name'Length,
            Property_Name => Name),
         "GPS-Instance-" & Get_Name (Instance.Ref.Get.Script),
         On_Destroyed => On_Widget_Data_Destroyed'Access);

      --  Do this after we have called CIR_User_Data.Set above, since the
      --  following will remove existing user_data associated with Name
      Set_Data
        (Instance, Name, GObject_Properties_Record'(Obj => Widget));
   end Set_Data;

   ------------------
   -- Get_Instance --
   ------------------

   function Get_Instance
     (Script : access Scripting_Language_Record'Class;
      Widget : access Glib.Object.GObject_Record'Class)
      return Class_Instance
   is
      Data_Name : constant String :=  "GPS-Instance-" & Get_Name (Script);
   begin
      if CIR_User_Data.Is_Set (Widget, Data_Name) then
         return CIR_User_Data.Get (Widget, Data_Name).Inst;
      else
         return No_Class_Instance;
      end if;
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
      Prop : constant Instance_Property := Get_Data (Instance, Name);
   begin
      if Prop = null then
         return null;
      else
         return GObject_Properties_Record (Prop.all).Obj;
      end if;
   end Get_Data;

end GNATCOLL.Scripts.Gtkada;
