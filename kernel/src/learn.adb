------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2017-2018, AdaCore                     --
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

package body Learn is

   Learn_Module : Learn_Module_Access;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : not null access Learn_Item_Group_Type;
      Name : String) is
   begin
      Self.Name := To_Unbounded_String (Name);
   end Initialize;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (Self : not null access Learn_Item_Group_Type) return String is
   begin
      return To_String (Self.Name);
   end Get_Name;

   --------------------
   -- Add_Learn_Item --
   --------------------

   procedure Add_Learn_Item
     (Self : not null access Learn_Item_Group_Type;
      Item : not null access Learn_Item_Type'Class) is
   begin
      Self.Items.Append (Item);
   end Add_Learn_Item;

   -----------------------
   -- Register_Provider --
   -----------------------

   procedure Register_Provider
     (Provider : not null access Learn_Provider_Type'Class) is
   begin
      Learn_Module.Providers.Insert
        (Key      => Provider.Get_Name,
         New_Item => Provider);
   end Register_Provider;

   -----------------------
   -- Register_Listener --
   -----------------------

   procedure Register_Listener
     (Listener : not null access Learn_Listener_Type'Class) is
   begin
      Learn_Module.Listeners.Append (Learn_Listener (Listener));
   end Register_Listener;

   -------------------------
   -- Unregister_Listener --
   -------------------------

   procedure Unregister_Listener
     (Listener : not null access Learn_Listener_Type'Class)
   is
      Position : Learn_Listener_Vectors.Cursor :=
                   Learn_Module.Listeners.Find (Listener);
   begin
      if Learn_Listener_Vectors.Has_Element (Position) then
         Learn_Module.Listeners.Delete (Position);
      end if;
   end Unregister_Listener;

   ---------------------------------------------
   -- Notify_Listeners_About_Provider_Changed --
   ---------------------------------------------

   procedure Notify_Listeners_About_Provider_Changed
     (Provider : not null access Learn_Provider_Type'Class) is
   begin
      for Listener of Learn_Module.Listeners loop
         Listener.On_Provider_Changed (Provider);
      end loop;
   end Notify_Listeners_About_Provider_Changed;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Learn_Module := new Learn_Module_Type;
      Register_Module (Learn_Module, Kernel, "Learn Manager");
   end Register_Module;

   ------------------------------
   -- Get_Registered_Providers --
   ------------------------------

   function Get_Registered_Providers return Learn_Provider_Maps.Map is
   begin
      return Learn_Module.Providers;
   end Get_Registered_Providers;

end Learn;
