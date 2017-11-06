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

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash_Case_Insensitive;

with Gtk.Flow_Box_Child; use Gtk.Flow_Box_Child;

with GPS.Kernel;         use GPS.Kernel;
with GPS.Kernel.Modules; use GPS.Kernel.Modules;

package Learn is

   type Learn_Module_Type is new Module_ID_Record with private;
   type Learn_Module_Access is access all Learn_Module_Type;

   type Learn_Item_Type is abstract new Gtk_Flow_Box_Child_Record
   with private;
   type Learn_Item is access all Learn_Item_Type'Class;
   --  This represents the GUI elements that will be displayed in the Learn
   --  view.

   function Is_Visible
     (Item        : not null access Learn_Item_Type;
      Context     : Selection_Context;
      Filter_Text : String) return Boolean is abstract;
   --  Return True if the given learn item should be displayed in the given
   --  context, False otherwise.

   package Learn_Item_Type_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Learn_Item,
      "="          => "=");

   type Learn_Provider_Type is interface;
   type Learn_Provider is access all Learn_Provider_Type'Class;
   --  An interface used to provide the learn items of a given kind
   --  (e.g : Actions).

   function Get_Name
     (Provider : not null access Learn_Provider_Type) return String
      is abstract;
   --  Return the name of the learn provider.

   function Get_Learn_Items
     (Provider : not null access Learn_Provider_Type)
      return Learn_Item_Type_Lists.List is abstract;
   --  Return a list of all the learn items that should be displayed for this
   --  provider.

   procedure Register_Provider
     (Provider : not null access Learn_Provider_Type'Class);
   --  Register a learn provider.

   procedure Register_Module
     (Kernel : not null access Kernel_Handle_Record'Class);
   --  Register the learn module.

private

   type Learn_Item_Type is abstract new Gtk_Flow_Box_Child_Record with
     null record;

   package Learn_Provider_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Learn_Provider,
        Hash            => Ada.Strings.Hash_Case_Insensitive,
        Equivalent_Keys => "=",
        "="             => "=");

   function Get_Registered_Providers return Learn_Provider_Maps.Map;

   type Learn_Module_Type is new Module_ID_Record with record
      Providers : Learn_Provider_Maps.Map;
   end record;

end Learn;
