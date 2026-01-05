------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2023-2026, AdaCore                     --
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

with DAP.Clients.Variables;       use DAP.Clients.Variables;
with DAP.Modules.Variables.Items; use DAP.Modules.Variables.Items;
with DAP.Types;                   use DAP.Types;
with DAP.Tools;

package DAP.Views.Variables is

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register menus and other functions

   procedure Update (Client : not null access DAP.Clients.DAP_Client'Class);

   procedure On_Variable_Not_Found
     (Client : not null access DAP.Clients.DAP_Client'Class;
      Params : Request_Parameters);
   --  Calback called when the variable is not found

   procedure On_Variable_Loaded
     (Client : not null access DAP.Clients.DAP_Client'Class;
      Params : Request_Parameters;
      C      : Variables_References_Trees.Cursor);
   --  Calback called when the variable is loaded

   procedure On_Children_Loaded
     (Client : not null access DAP.Clients.DAP_Client'Class;
      Params : Request_Parameters;
      C      : Variables_References_Trees.Cursor);
   --  Calback called when the variable's children is loaded

   procedure On_Variable_Set
     (Client   : not null access DAP.Clients.DAP_Client'Class;
      Params   : Request_Parameters;
      Variable : DAP.Tools.Variable);
   --  Calback called when the variable's value is set

private

   use DAP.Modules.Variables.Items.Item_Info_Vectors;
   use DAP.Types.Variables_References_Trees;

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
      Item   : Item_Info'Class;
      Cursor : Variables_References_Trees.Cursor;
      Parent : Gtk_Tree_Iter);

   function Item_From_Store_Iter
     (Self       : not null access Variables_Tree_View_Record'Class;
      Store_Iter : Gtk_Tree_Iter)
      return Item_Info'Class;
   --  Return a row in the variables view converted into an item

   function Item_From_Filter_Iter
     (Self        : not null access Variables_Tree_View_Record'Class;
      Filter_Iter : in out Gtk_Tree_Iter)
      return Item_Info'Class;

   function Find_Info
     (Self : not null access Variables_Tree_View_Record'Class;
      Id   : Item_ID)
      return Item_Info'Class;

   procedure Find_Best_Info
     (Self   : access Variables_Tree_View_Record'Class;
      Name   : Virtual_String;
      Cursor : out Item_Info_Vectors.Cursor;
      Found  : out Boolean);

   function Get_Item_Info
     (Self : not null access Variables_Tree_View_Record'Class;
      Name : String)
      return Item_Info'Class;

   procedure Set_Item_Full_Name
     (Self : not null access Variables_Tree_View_Record'Class;
      Item : Item_Info'Class;
      Name : VSS.Strings.Virtual_String);
   --  Set full name for the Item_Info

   --  DAP_Variables_View_Record --

   type DAP_Variables_View_Record is new View_Record with record
      Tree       : Variables_Tree_View;
      --  Represents variables in GUI

      Expansion  : Expansions.Expansion_Status := Expansions.No_Expansion;
      --  Used to restore expansion and selection

      Collapse_All_First : Boolean := False;
      --  To collaps nodes only once after update

      Expansion_Restoring_Count : Natural := 0;
      --  Counting how many times Restore_Expansion is called

      Old_Scopes : Variables_References_Trees.Tree :=
        Variables_References_Trees.Empty_Tree;
      --  Contains old scopes' values. Used to detect changes in values
      --  in order to highlight them in the view
   end record;

   function Initialize
     (Self : access DAP_Variables_View_Record'Class) return Gtk_Widget;

   procedure Display
     (Self : access DAP_Variables_View_Record'Class;
      Name : String);
   --  Displays the variable by Name

   procedure Display
     (Self : access DAP_Variables_View_Record'Class;
      Item : in out Item_Info'Class);
   --  Displays the variable by Item

   procedure Undisplay
     (Self : access DAP_Variables_View_Record'Class;
      Name : Virtual_String);
   --  Removes the variable from the view by Name

   procedure Undisplay
     (Self : access DAP_Variables_View_Record'Class;
      Item : Item_Info'Class);
   --  Removes the variable from the view by Item

   overriding procedure Update
     (Self : not null access DAP_Variables_View_Record);
   --  Update all displayed variables

   procedure Update
     (Self     : access DAP_Variables_View_Record'Class;
      Position : Natural);
   --  Update the next after Position variable.

   procedure Update
     (Self     : access DAP_Variables_View_Record'Class;
      Item     : Item_Info'Class;
      Position : Natural;
      Path     : Gtk.Tree_Model.Gtk_Tree_Path;
      Children : Boolean := False);
   --  Update the variable by Item. Position points the Item in the internal
   --  list. Path points to the view location. Children controls whether
   --  children should be loaded or not.

   function Is_Changed
     (Self   : access DAP_Variables_View_Record'Class;
      Cursor : Variables_References_Trees.Cursor)
      return Boolean;
   --  Returns true if the new value of the item has been changed

   procedure Clear (Self : not null access DAP_Variables_View_Record'Class);
   --  Clear the contents of Self

   procedure Set_Variable_Value
     (Self      : access DAP_Variables_View_Record'Class;
      Full_Name : VSS.Strings.Virtual_String;
      Value     : String;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path);

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

   procedure Restore_Expansion
     (Self : access DAP_Variables_View_Record'Class);
   --  Restores expansion if any is stored

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
      Reuse_If_Exist                  => True,
      Save_Duplicates_In_Perspectives => False,
      Commands_Category               => "",
      Local_Toolbar                   => True,
      Local_Config                    => True,
      Areas                           => Gtkada.MDI.Sides_Only,
      Position                        => Gtkada.MDI.Position_Right,
      Initialize                      => Initialize);

   package Variables_Views is new DAP.Views.Simple_Views
     (Formal_View_Record => DAP_Variables_View_Record,
      Formal_MDI_Child   => Variables_MDI_Child_Record,
      Formal_Views       => Variables_MDI_Views);
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
