------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2012, AdaCore                     --
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

with Gtkada.MDI;
with GPS.Kernel.MDI;
with GPS.Kernel.Preferences;
with GPS.Location_View;

package body GPS.Kernel.Messages.View is

   use Glib.Main;
   use Gtkada.MDI;
   use GPS.Kernel.MDI;
   use GPS.Kernel.Preferences;
   use GPS.Location_View;

   function To_View_Manager is
     new Ada.Unchecked_Conversion (System.Address, View_Manager_Access);

   package Idles is new Glib.Main.Generic_Sources (View_Manager_Access);

   function On_Idle (Self : View_Manager_Access) return Boolean;
   --  Resets Goto_First_Location flag.

   --------------------
   -- Category_Added --
   --------------------

   overriding procedure Category_Added
     (Self     : not null access View_Manager;
      Category : Ada.Strings.Unbounded.Unbounded_String)
   is
   begin
      Get_Or_Create_Location_View (Self.Kernel).Expand_Category
        (Category,
         Self.Goto_First_Location and then Auto_Jump_To_First.Get_Pref);
   end Category_Added;

   --------------------------------
   -- Do_Not_Goto_First_Location --
   --------------------------------

   procedure Do_Not_Goto_First_Location
     (Kernel : not null access Kernel_Handle_Record'Class)
   is
      Manager : constant View_Manager_Access :=
                  To_View_Manager (Kernel.Locations_View_Manager);

   begin
      Manager.Goto_First_Location := False;

      if Manager.Idle_Handler = No_Source_Id then
         Manager.Idle_Handler := Idles.Idle_Add (On_Idle'Access, Manager);
      end if;
   end Do_Not_Goto_First_Location;

   ---------------------
   -- Expand_Category --
   ---------------------

   procedure Expand_Category
     (Kernel   : not null access Kernel_Handle_Record'Class;
      Category : String) is
   begin
      Get_Or_Create_Location_View (Kernel).Expand_Category
        (Ada.Strings.Unbounded.To_Unbounded_String (Category), False);
   end Expand_Category;

   -----------------
   -- Expand_File --
   -----------------

   procedure Expand_File
     (Kernel   : not null access Kernel_Handle_Record'Class;
      Category : String;
      File     : GNATCOLL.VFS.Virtual_File) is
   begin
      Get_Or_Create_Location_View (Kernel).Expand_File
        (Ada.Strings.Unbounded.To_Unbounded_String (Category), File, False);
   end Expand_File;

   -------------------
   -- Message_Added --
   -------------------

   overriding procedure Message_Added
     (Self    : not null access View_Manager;
      Message : not null access Abstract_Message'Class)
   is
      pragma Unreferenced (Message);

   begin
      Highlight_Child
        (Find_MDI_Child
           (Get_MDI (Kernel_Handle (Self.Kernel)),
            Get_Or_Create_Location_View (Self.Kernel)));
   end Message_Added;

   -------------
   -- On_Idle --
   -------------

   function On_Idle (Self : View_Manager_Access) return Boolean is
   begin
      Self.Goto_First_Location := True;
      Self.Idle_Handler := No_Source_Id;

      return False;
   end On_Idle;

   --------------
   -- Register --
   --------------

   procedure Register (Kernel : not null access Kernel_Handle_Record'Class) is

      function To_Address is
        new Ada.Unchecked_Conversion (View_Manager_Access, System.Address);

      Manager : constant View_Manager_Access := new View_Manager (Kernel);

   begin
      Get_Messages_Container (Kernel).Register_Listener
        (Listener_Access (Manager),
         (Editor_Side => False, Locations => True));
      Kernel.Locations_View_Manager := To_Address (Manager);
   end Register;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister
     (Kernel : not null access Kernel_Handle_Record'Class)
   is
      procedure Free is
        new Ada.Unchecked_Deallocation
          (View_Manager'Class, View_Manager_Access);

      Manager : View_Manager_Access :=
                  To_View_Manager (Kernel.Locations_View_Manager);

   begin
      Get_Messages_Container (Kernel).Unregister_Listener
        (Listener_Access (Manager));
      Free (Manager);
   end Unregister;

end GPS.Kernel.Messages.View;
