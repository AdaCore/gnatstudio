------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2023, AdaCore                          --
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

--  This package implements a tree view target to the display of registers.

with Ada.Containers.Indefinite_Ordered_Sets;

with GNATCOLL.JSON;

with Gtk.Menu;                    use Gtk.Menu;
with Gtk.Tree_Store;              use Gtk.Tree_Store;
with Gtk.Tree_View;               use Gtk.Tree_View;
with Gtk.Widget;                  use Gtk.Widget;

with Gtkada.MDI;

with GPS.Kernel;
with GPS.Kernel.MDI;              use GPS.Kernel.MDI;
with GPS.Properties;              use GPS.Properties;

with DAP.Types;

package DAP.Views.Registers is

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register menus and other functions to support the registers view

   procedure Update (Client : not null access DAP.Clients.DAP_Client'Class);

private

   type Command_Kind is (Update_Registers, Select_Names, Select_All_Names);

   package Registers_Set is
      new Ada.Containers.Indefinite_Ordered_Sets (String);

   type Registers_View_Record is new View_Record with
      record
         Tree         : Gtk.Tree_View.Gtk_Tree_View;

         Model        : Gtk.Tree_Store.Gtk_Tree_Store;
         --  The actual contents of the viewer

         Old_Values   : DAP.Types.String_To_String_Maps.Map;
         --  Register name => value

         Registers    : Registers_Set.Set;
         --  Set of all the displayed registers

         Locked       : Boolean := False;
         --  If true, disable updates

         Resize       : Boolean := False;
         --  If true, the column will be resized at the next update

         Registers_Id : Integer := 0;
      end record;
   type Registers_View is access all Registers_View_Record'Class;

   function Initialize
     (Self : access Registers_View_Record'Class) return Gtk_Widget;
   --  Internal initialization function

   overriding procedure Create_Menu
     (Self : not null access Registers_View_Record;
      Menu : not null access Gtk.Menu.Gtk_Menu_Record'Class);

   overriding procedure Update (Self : not null access Registers_View_Record);

   overriding procedure On_Attach
     (Self   : not null access Registers_View_Record;
      Client : not null access DAP.Clients.DAP_Client'Class);

   overriding procedure On_Detach
     (Self   : not null access Registers_View_Record;
      Client : not null access DAP.Clients.DAP_Client'Class);

   overriding procedure On_Status_Changed
     (Self : not null access Registers_View_Record;
      Status : GPS.Debuggers.Debugger_State);

   overriding procedure On_Process_Terminated
     (View : not null access Registers_View_Record);

   procedure Send_Request
     (Self : not null access Registers_View_Record;
      Kind : Command_Kind);

   type Registers_Property_Record is new Property_Record with record
      Items : Registers_Set.Set;
   end record;
   --  This type is used to preverse the visible registers accross sessions by
   --  saving them in the property database.

   overriding procedure Save
     (Self  : access Registers_Property_Record;
      Value : in out GNATCOLL.JSON.JSON_Value);
   overriding procedure Load
     (Self  : in out Registers_Property_Record;
      Value : GNATCOLL.JSON.JSON_Value);

   package Registers_MDI_Views is new Generic_Views.Simple_Views
     (Module_Name                     => "Registers_View",
      View_Name                       => "Registers",
      Formal_View_Record              => Registers_View_Record,
      Formal_MDI_Child                => GPS_MDI_Child_Record,
      Reuse_If_Exist                  => True,
      Save_Duplicates_In_Perspectives => False,
      Commands_Category               => "",
      Group                           => Group_Debugger_Stack,
      Position                        => Gtkada.MDI.Position_Right,
      Areas                           => Gtkada.MDI.Sides_Only,
      Initialize                      => Initialize,
      Local_Config                    => True,
      Local_Toolbar                   => True);
   subtype DAP_Registers_View is Registers_MDI_Views.View_Access;

   function Get_View
     (Client : not null access DAP.Clients.DAP_Client'Class)
      return access Registers_View_Record'Class;

   procedure Set_View
     (Client : not null access DAP.Clients.DAP_Client'Class;
      View   : access Registers_View_Record'Class := null);

   package Simple_Views is new DAP.Views.Simple_Views
     (Formal_Views       => Registers_MDI_Views,
      Formal_View_Record => Registers_View_Record,
      Formal_MDI_Child   => GPS_MDI_Child_Record,
      Get_View           => Get_View,
      Set_View           => Set_View);

   Name_Column           : constant := 0;
   Raw_Column            : constant := 1;
   Type_Column           : constant := 2;
   FG_Color_Column       : constant := 3;
   BG_Name_Color_Column  : constant := 4;
   BG_Value_Color_Column : constant := 5;
   Editable_Column       : constant := 6;
   Id_Column             : constant := 7;

end DAP.Views.Registers;
