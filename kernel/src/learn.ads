------------------------------------------------------------------------------
--                                  G P S                                   --
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

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Hash_Case_Insensitive;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Gtk.Widget;            use Gtk.Widget;

with GPS.Kernel;            use GPS.Kernel;
with GPS.Kernel.Modules;    use GPS.Kernel.Modules;

package Learn is

   type Learn_Module_Type is new Module_ID_Record with private;
   type Learn_Module_Access is access all Learn_Module_Type;

   -----------------
   -- Learn Items --
   -----------------

   type Learn_Item_Type is abstract tagged private;
   type Learn_Item is access all Learn_Item_Type'Class;
   --  This represents the GUI elements that will be displayed in the Learn
   --  view.

   procedure Initialize
     (Item       : not null access Learn_Item_Type;
      Group_Name : String);
   --  Initialize the given item, associating it to a group.

   function Get_Group_Name
     (Item : not null access Learn_Item_Type) return String;
   --  Return the item's group name.

   function Get_ID
     (Item : not null access Learn_Item_Type) return String is abstract;
   --  Return a unique ID for the given learn item.
   --  Can be used by some listeners to identify easily learn items.

   function Get_Widget
     (Item : not null access Learn_Item_Type)
      return Gtk_Widget is abstract;
   --  Return the widget corresponding to the given item and that will be
   --  displayed in the Learn view.

   function Get_Help
     (Item : not null access Learn_Item_Type) return String
   is
     ("");
   --  Return an help string associated with the given item.

   function Is_Visible
     (Item        : not null access Learn_Item_Type;
      Context     : Selection_Context;
      Filter_Text : String) return Boolean is abstract;
   --  Return True if the given learn item should be displayed in the given
   --  context, False otherwise.

   procedure On_Double_Click
     (Item    : not null access Learn_Item_Type;
      Context : Selection_Context) is null;
   --  Called each time the user double-clicks on the given learn item.
   --  Override this function if you want to react to this event.

   ---------------------
   -- Learn Providers --
   ---------------------

   type Learn_Provider_Type is abstract tagged private;
   type Learn_Provider is access all Learn_Provider_Type'Class;
   --  An interface used to provide the learn items of a given kind
   --  (e.g : Actions).

   function Get_Name
     (Provider : not null access Learn_Provider_Type) return String
      is abstract;
   --  Return the name of the learn provider.

   procedure Add_Item
     (Provider : not null access Learn_Provider_Type'Class;
      Item     : not null access Learn_Item_Type'Class;
      ID       : String);
   --  Add an item to the learn provider.
   --  If an item with the same ID has already been added, replace it.

   procedure Delete_Item
     (Provider : not null access Learn_Provider_Type'Class;
      ID       : String);
   --  Delete an item from the learn provider.

   procedure Register_Provider
     (Provider : not null access Learn_Provider_Type'Class);
   --  Register a learn provider.

   ---------------------
   -- Learn Listeners --
   ---------------------

   type Learn_Listener_Type is interface;
   type Learn_Listener is access all Learn_Listener_Type'Class;
   --  A listener interfaced used to react to changes made regarding providers
   --  (e.g: when adding learn items to a provider).

   procedure On_Item_Added
     (Self     : not null access Learn_Listener_Type;
      Provider : not null access Learn_Provider_Type'Class;
      Item     : not null access Learn_Item_Type'Class) is abstract;
   --  Called when the given item is added to the learn provider.

   procedure On_Item_Deleted
     (Self     : not null access Learn_Listener_Type;
      Provider : not null access Learn_Provider_Type'Class;
      Item     : not null access Learn_Item_Type'Class) is abstract;
   --  Called when the given item is deleted from the learn provider.

   procedure Register_Listener
     (Listener : not null access Learn_Listener_Type'Class);
   --  Register a learn listener.

   procedure Unregister_Listener
     (Listener : not null access Learn_Listener_Type'Class);
   --  Unregister the given learn provider.

   ------------------
   -- Learn Module --
   ------------------

   procedure Register_Module
     (Kernel : not null access Kernel_Handle_Record'Class);
   --  Register the learn module.

private

   type Learn_Item_Type is abstract tagged record
      Group_Name : Unbounded_String;
   end record;

   package Learn_Item_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Learn_Item,
      Hash            => Ada.Strings.Hash_Case_Insensitive,
      Equivalent_Keys => "=",
      "="             => "=");

   type Learn_Provider_Type is abstract tagged record
      Items : Learn_Item_Maps.Map;
   end record;

   package Learn_Provider_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Learn_Provider,
        Hash            => Ada.Strings.Hash_Case_Insensitive,
        Equivalent_Keys => "=",
        "="             => "=");

   package Learn_Listener_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Learn_Listener,
      "="          => "=");

   function Get_Registered_Providers return Learn_Provider_Maps.Map;

   type Learn_Module_Type is new Module_ID_Record with record
      Providers : Learn_Provider_Maps.Map;
      Listeners : Learn_Listener_Vectors.Vector;
   end record;

end Learn;
