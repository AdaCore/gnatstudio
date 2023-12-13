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

with Ada.Containers.Multiway_Trees;

with Gdk.Event;                   use Gdk.Event;

with Gtk.Cell_Renderer_Text;      use Gtk.Cell_Renderer_Text;
with Gtk.Menu;                    use Gtk.Menu;
with Gtk.Toolbar;                 use Gtk.Toolbar;
with Gtk.Tree_Model;              use Gtk.Tree_Model;
with Gtk.Tree_View_Column;        use Gtk.Tree_View_Column;
with Gtk.Widget;                  use Gtk.Widget;

with Gtkada.MDI;
with Gtkada.Tree_View;            use Gtkada.Tree_View;

with VSS.Strings;                 use VSS.Strings;
with VSS.Strings.Hash;

with GPS.Kernel;
with GPS.Kernel.MDI;              use GPS.Kernel.MDI;
with GPS.Search;                  use GPS.Search;

with DAP.Tools;                   use DAP.Tools;
with DAP.Modules.Variables.Items; use DAP.Modules.Variables.Items;
with DAP.Requests;

package DAP.Views.Variables is

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register menus and other functions

   procedure Update (Client : not null access DAP.Clients.DAP_Client'Class);

private

   use DAP.Modules.Variables.Items.Item_Info_Vectors;

   package Variables_References_Trees is
     new Ada.Containers.Multiway_Trees (DAP.Tools.Variable);
   use Variables_References_Trees;

   function Full_Name
     (Cursor : Variables_References_Trees.Cursor)
      return Virtual_String;

   function Full_Name
     (Cursor : Variables_References_Trees.Cursor)
      return String;

   procedure Find_Best_Ref
     (Name   : Virtual_String;
      Cursor : in out Variables_References_Trees.Cursor;
      Found  : out Boolean);

   procedure Find_Cmd_Ref
     (Name   : Virtual_String;
      Cursor : in out Variables_References_Trees.Cursor;
      Found  : out Boolean);

   type Variables_Tree_View_Record is
     new Gtkada.Tree_View.Tree_View_Record with
      record
         View         : View_Access;
         Pattern      : Search_Pattern_Access;
         Ids          : Item_ID := Unknown_Id; --  to compute
         --  unique ids for items
         Items        : Item_Info_Vectors.Vector;
         Types_Column : Gtk_Tree_View_Column;
         Text         : Gtk_Cell_Renderer_Text;
      end record;
   type Variables_Tree_View is access all Variables_Tree_View_Record'Class;
   overriding function Is_Visible
     (Self   : not null access Variables_Tree_View_Record;
      Iter   : Gtk_Tree_Iter) return Boolean;
   overriding procedure Add_Children
     (Self       : not null access Variables_Tree_View_Record;
      Store_Iter : Gtk_Tree_Iter);
   overriding procedure On_Edited
     (Self        : not null access Variables_Tree_View_Record;
      Store_Iter  : Gtk_Tree_Iter;
      View_Column : Edited_Column_Id;
      Text        : String);

   function Get_Id
     (Self : not null access Variables_Tree_View_Record'Class;
      Iter : Gtk_Tree_Iter) return Virtual_String;
   package Expansions is new Expansion_Support
     (Variables_Tree_View_Record, Virtual_String, Get_Id, VSS.Strings.Hash);
   --  An Id that uniquely identifies each row of the tree view

   procedure Add_Row
     (Self   : not null access Variables_Tree_View_Record'Class;
      Item   : Item_Info;
      Cursor : Variables_References_Trees.Cursor;
      Parent : Gtk_Tree_Iter);

   function Item_From_Iter
     (Self       : not null access Variables_Tree_View_Record'Class;
      Store_Iter : Gtk_Tree_Iter)
      return Item_Info;
   --  Return a row in the variables view converted into an item

   procedure Item_From_Iter
     (Self        : not null access Variables_Tree_View_Record'Class;
      Filter_Iter : in out Gtk_Tree_Iter;
      It          : out Item_Info);

   function Find_Info
     (Self : not null access Variables_Tree_View_Record'Class;
      Id   : Item_ID)
      return Item_Info;

   procedure Find_Best_Info
     (Self   : access Variables_Tree_View_Record'Class;
      Name   : Virtual_String;
      Cursor : out Item_Info_Vectors.Cursor;
      Found  : out Boolean);

   function Get_Item_Info
     (Self : not null access Variables_Tree_View_Record'Class;
      Name : String)
      return Item_Info;

   --  DAP_Variables_View_Record --

   type DAP_Variables_View_Record is new View_Record with record
      Tree               : Variables_Tree_View;
      --  Represents variables in GUI

      Expansion          : Expansions.Expansion_Status;
      --  Used to restore expansion and selection

      Locals_Scope_Id    : Integer := 0;
      --  Current 'Locals' scope Id on debugger side.

      Arguments_Scope_Id : Integer := 0;
      --  Current 'Arguments' scope Id on debugger side.

      Scopes             : Variables_References_Trees.Tree := Empty_Tree;
      --  Contains the different scopes that are returned by the DAP
      --  ScopesRequest (e.g: "Locals", "Arguments")

      Old_Scopes         : Variables_References_Trees.Tree := Empty_Tree;
      --  Contains old scopes' values. Used to detect changes in values
      --  in order to highlight them in the view
   end record;

   function Initialize
     (Self : access DAP_Variables_View_Record'Class) return Gtk_Widget;

   procedure Display
     (Self : access DAP_Variables_View_Record'Class;
      Name : String);

   procedure Display
     (Self : access DAP_Variables_View_Record'Class;
      Item : in out Item_Info);

   procedure Undisplay
     (Self : access DAP_Variables_View_Record'Class;
      Name : Virtual_String);

   procedure Undisplay
     (Self : access DAP_Variables_View_Record'Class;
      Item : Item_Info);

   procedure Publish_Or_Request
     (Self     : access DAP_Variables_View_Record'Class;
      Item     : Item_Info;
      Position : Natural;
      Childs   : Boolean;
      Path     : Gtk.Tree_Model.Gtk_Tree_Path;
      Request  : out DAP.Requests.DAP_Request_Access);

   function Find_Ref
     (Self : access DAP_Variables_View_Record'Class;
      Id   : Integer)
      return Variables_References_Trees.Cursor;

   procedure Continue_Update
     (Self     : not null access DAP_Variables_View_Record'Class;
      Position : Natural;
      Request  : out DAP.Requests.DAP_Request_Access);

   procedure Update
     (Self     : access DAP_Variables_View_Record'Class;
      Item     : Item_Info;
      Position : Natural;
      Path     : Gtk.Tree_Model.Gtk_Tree_Path;
      Childs   : Boolean := False);

   function Update
     (Self     : access DAP_Variables_View_Record'Class;
      Item     : Item_Info;
      Position : Natural;
      Path     : Gtk.Tree_Model.Gtk_Tree_Path;
      Childs   : Boolean := False)
      return DAP.Requests.DAP_Request_Access;

   function Is_Changed
     (Self   : access DAP_Variables_View_Record'Class;
      Cursor : Variables_References_Trees.Cursor)
      return Boolean;

   procedure Clear (Self : not null access DAP_Variables_View_Record'Class);
   --  Clear the contents of Self

   procedure Set_Variable_Value
     (Self  : access DAP_Variables_View_Record'Class;
      Name  : String;
      Value : String;
      Path  : Gtk.Tree_Model.Gtk_Tree_Path);

   overriding procedure Create_Menu
     (View    : not null access DAP_Variables_View_Record;
      Menu    : not null access Gtk.Menu.Gtk_Menu_Record'Class);

   overriding procedure Create_Toolbar
     (Self    : not null access DAP_Variables_View_Record;
      Toolbar : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class);

   overriding procedure On_Process_Terminated
     (Self : not null access DAP_Variables_View_Record);

   overriding procedure On_Status_Changed
     (Self : not null access DAP_Variables_View_Record;
      Status : GPS.Debuggers.Debugger_State);

   overriding procedure Update
     (Self : not null access DAP_Variables_View_Record);

   overriding procedure Filter_Changed
     (Self    : not null access DAP_Variables_View_Record;
      Pattern : in out Search_Pattern_Access);

   overriding procedure On_Location_Changed
     (Self : not null access DAP_Variables_View_Record);

   overriding procedure On_Attach
     (Self   : not null access DAP_Variables_View_Record;
      Client : not null access DAP.Clients.DAP_Client'Class);

   overriding procedure On_Detach
     (Self   : not null access DAP_Variables_View_Record;
      Client : not null access DAP.Clients.DAP_Client'Class);

   type Variables_MDI_Child_Record is
     new GPS_MDI_Child_Record with null record;
   overriding function Build_Context
     (Self  : not null access Variables_MDI_Child_Record;
      Event : Gdk.Event.Gdk_Event := null)
      return GPS.Kernel.Selection_Context;

   package Variables_MDI_Views is new Generic_Views.Simple_Views
     (Module_Name                     => "Debugger_Variables",
      View_Name                       => "Variables",
      Formal_View_Record              => DAP_Variables_View_Record,
      Formal_MDI_Child                => Variables_MDI_Child_Record,
      Reuse_If_Exist                  => False,
      Save_Duplicates_In_Perspectives => False,
      Commands_Category               => "",
      Local_Toolbar                   => True,
      Local_Config                    => True,
      Areas                           => Gtkada.MDI.Sides_Only,
      Position                        => Gtkada.MDI.Position_Right,
      Initialize                      => Initialize);

   function Get_View
     (Client : not null access DAP.Clients.DAP_Client'Class)
      return access DAP_Variables_View_Record'Class;

   procedure Set_View
     (Client : not null access DAP.Clients.DAP_Client'Class;
      View   : access DAP_Variables_View_Record'Class := null);

   package Variables_Views is new DAP.Views.Simple_Views
     (Formal_View_Record => DAP_Variables_View_Record,
      Formal_MDI_Child   => Variables_MDI_Child_Record,
      Formal_Views       => Variables_MDI_Views,
      Get_View           => Get_View,
      Set_View           => Set_View);
   use type Variables_MDI_Views.View_Access;
   subtype DAP_Variables_View is Variables_MDI_Views.View_Access;

   --  Tree columns
   Column_Name      : constant := 0;
   Column_Value     : constant := 1;
   Column_Type      : constant := 2;
   Column_Icon      : constant := 3;
   Column_Id        : constant := 4;   --  integer id for the variable
   Column_Name_Fg   : constant := 5;
   Column_Value_Fg  : constant := 6;
   Column_Type_Fg   : constant := 7;
   Column_Full_Name : constant := 8;

end DAP.Views.Variables;
