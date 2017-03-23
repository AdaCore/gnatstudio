------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                        Copyright (C) 2013-2017, AdaCore                  --
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

package body GPS.Scripts is

   type Ownership_Mode is (Ada_Owns_Python, Python_Owns_Ada);

   ------------
   -- Create --
   ------------

   function Create
     (Kernel : GPS.Core_Kernels.Core_Kernel)
      return Kernel_Scripts_Repository is
   begin
      return (Scripts_Repository_Record with Kernel => Kernel);
   end Create;

   ----------------
   -- Get_Kernel --
   ----------------

   function Get_Kernel
     (Data : GNATCOLL.Scripts.Callback_Data'Class)
      return GPS.Core_Kernels.Core_Kernel is
   begin
      return Kernel_Scripts_Repository (Get_Repository (Data).all).Kernel;
   end Get_Kernel;

   ----------------
   -- Get_Kernel --
   ----------------

   function Get_Kernel
     (Script : access GNATCOLL.Scripts.Scripting_Language_Record'Class)
      return GPS.Core_Kernels.Core_Kernel is
   begin
      return Kernel_Scripts_Repository (Get_Repository (Script).all).Kernel;
   end Get_Kernel;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Script_Proxy) is
      C : Inst_Cursor := First (Self.Instances);
   begin
      --  Severe the connection between instances and Ada object
      while Has_Element (C) loop
         Unset_Data
            (Element (Self.Instances, C),
             Script_Proxy'Class (Self).Class_Name);
         Next (Self.Instances, C);
      end loop;

      --  Unref all instances (and possibly destroy them). Also
      --  release memory on the Ada side.
      Free (Self.Instances);
   end Free;

   --------------------
   -- Script_Proxies --
   --------------------

   package body Script_Proxies is

      type Element_Properties_Record is new Instance_Property_Record with
         record
            Element   : Element_Type;
            Ownership : Ownership_Mode;
         end record;
      overriding procedure Destroy (Prop : in out Element_Properties_Record);

      -------------
      -- Destroy --
      -------------

      overriding procedure Destroy
         (Prop : in out Element_Properties_Record) is
      begin
         if Prop.Ownership = Python_Owns_Ada then
            Free (Prop.Element);
         end if;
      end Destroy;

      ----------------------------
      -- Get_Or_Create_Instance --
      ----------------------------

      function Get_Or_Create_Instance
         (Self   : in out Proxy'Class;
          Obj    : Element_Type;
          Script : not null access Scripting_Language_Record'Class;
          Class_To_Create : String := "")
         return Class_Instance
      is
         C : Class_Instance := Get (Self.Instances, Script);
      begin
         if C = No_Class_Instance then
            --  This assumes the class has already been declared with its
            --  proper base classes.
            C := New_Instance
               (Script, New_Class (Script.Get_Repository,
                (if Class_To_Create = "" then Self.Class_Name
                 else Class_To_Create)));
            Store_In_Instance (Self, C, Obj);
         end if;
         return C;
      end Get_Or_Create_Instance;

      -----------------------
      -- Store_In_Instance --
      -----------------------

      procedure Store_In_Instance
         (Self   : in out Proxy'Class;
          Inst   : Class_Instance;
          Obj    : Element_Type) is
      begin
         Set_Data
            (Inst, Self.Class_Name,
             Element_Properties_Record'
                (Element   => Obj,
                 Ownership => Ada_Owns_Python));
         Set (Self.Instances, Inst);
      end Store_In_Instance;

      -----------------
      -- Has_Element --
      -----------------

      function Has_Element (Inst : Class_Instance) return Boolean is
         P : Proxy;  --  Only to retrieve class name
         Data : constant Instance_Property := Get_Data (Inst, P.Class_Name);
      begin
         return Data /= null;
      end Has_Element;

      -------------------
      -- From_Instance --
      -------------------

      function From_Instance (Inst : Class_Instance) return Element_Type is
         P : Proxy;  --  Only to retrieve class name
         Data : constant Instance_Property := Get_Data (Inst, P.Class_Name);
      begin
         if Data = null then
            raise No_Data_Set_For_Instance with
                "No Ada object associated with python " & P.Class_Name
                & " instance";
         end if;
         return Element_Properties_Record (Data.all).Element;
      end From_Instance;

      ------------------------
      -- Transfer_Ownership --
      ------------------------

      function Transfer_Ownership
         (Self : in out Proxy'Class) return Instances_Status
      is
         Found : Boolean := False;
         C : Inst_Cursor := First (Self.Instances);
         Detached : Element_Type;
      begin
         while Has_Element (C) loop
            if not Found then
               Detached := Detach
                  (From_Instance (Element (Self.Instances, C)));
               Found := True;
            end if;
            Set_Data
               (Element (Self.Instances, C),
                Self.Class_Name,
                Element_Properties_Record'
                   (Element   => Detached,
                    Ownership => Python_Owns_Ada));
            Next (Self.Instances, C);
         end loop;

         Free (Self.Instances);

         if Found then
            return Has_Instances;
         else
            return Has_No_Instances;
         end if;
      end Transfer_Ownership;

   end Script_Proxies;

end GPS.Scripts;
