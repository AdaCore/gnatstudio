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

--  Module that incapsulate Variables information

with Ada.Containers.Multiway_Trees;

with Gtk.Tree_Model;               use Gtk.Tree_Model;

with VSS.Strings;                  use VSS.Strings;

with DAP.Modules.Variables.Items;
with DAP.Tools;                    use DAP.Tools;

package DAP.Clients.Variables is

   type Variable_Kind is (Locals, Arguments, Non_Specified);
   --  Kind is used to determine from where we get the variable

   type Variable_Data (Kind : Variable_Kind := Non_Specified) is record
      Data : DAP.Tools.Variable;
   end record;
   --  Holds DAP variable data

   package Variables_References_Trees is
     new Ada.Containers.Multiway_Trees (Variable_Data);

   type Request_Params_Kind is (View, Python_API, Set_Variable);
   --  To determine where the variable needed:
   --   - View : when the request is initiated from the view
   --   - Python_API: when the request is initiated from the Python API
   --   - Set_Variable: when the request for setting the variable value

   type Request_Parameters (Kind : Request_Params_Kind) is record
      Item     : DAP.Modules.Variables.Items.Item_Info;
      Children : Boolean := False;

      case Kind is
         when View =>
            --  Variables view data
            Position : Natural;
            --  To know the variable's number we updating, to continue
            --  updating from the next one when we have the response.
            Path     : Gtk.Tree_Model.Gtk_Tree_Path;
            --  Points to the tree node we are updating, to understand where
            --  to place the data from the response.

         when Python_API =>
            --  Python API data, to call corresponding subprogram when
            --  we have the response
            On_Result   : GNATCOLL.Scripts.Subprogram_Type;
            On_Error    : GNATCOLL.Scripts.Subprogram_Type;
            On_Rejected : GNATCOLL.Scripts.Subprogram_Type;

         when Set_Variable =>
            --  Variables view data used when the view set the variable's value
            Name     : VSS.Strings.Virtual_String;
            Value    : VSS.Strings.Virtual_String;
            Set_Path : Gtk.Tree_Model.Gtk_Tree_Path;
      end case;
   end record;
   --  To store requested side data that will be needed when
   --  the response arrives.

   procedure Free (Src : in out Request_Parameters);
   --  Free parameter resources

   -- Variables_Holder --

   type Variables_Holder
     (Client : not null access DAP.Clients.DAP_Client'Class) is tagged private;
   type Variables_Holder_Access is access all Variables_Holder'Class;

   function Get_Scopes
     (Self : in out Variables_Holder)
      return Variables_References_Trees.Tree;
   --  Returns current scopes

   procedure Clear (Self : in out Variables_Holder);
   --  Clear internal data

   procedure Get_Variable
     (Self   : in out Variables_Holder;
      Params : in out Request_Parameters);
   --  Get variable and update the view or call Python API.

   procedure Set_Variable
     (Self   : in out Variables_Holder;
      Params : Request_Parameters);
   --  Set variable's valuie and update the view

   -- Utils --

   procedure Find_Name_Or_Parent
     (Name   : Virtual_String;
      Cursor : in out Variables_References_Trees.Cursor;
      Found  : out Boolean);
   --  Find the variable or its closest parent. Found is set to True only
   --  when the exact variable is found.
   --  When the closest parent is found instead, Found is set to False but
   --  the returned cursor will be set on the parent's element.
   --  Returns No_Element only when both the variable and its parent are
   --  not found.

   function Full_Name
     (Cursor : Variables_References_Trees.Cursor)
      return VSS.Strings.Virtual_String;
   --  Returns the variable full name

   procedure Register_Module;
   --  Register the functions needed to work properly

private

   use Variables_References_Trees;

   type Variables_Holder
     (Client : not null access DAP.Clients.DAP_Client'Class)
   is tagged record
      Has_Scopes_Ids     : Boolean := False;

      Locals_Scope_Id    : Integer := 0;
      --  Current 'Locals' scope Id on debugger side.

      Arguments_Scope_Id : Integer := 0;
      --  Current 'Arguments' scope Id on debugger side.

      Scopes             : Variables_References_Trees.Tree := Empty_Tree;
      --  Contains the different scopes that are returned by the DAP
      --  ScopesRequest (e.g: "Locals", "Arguments")
   end record;

   procedure On_Scopes_Result
     (Self   : in out Variables_Holder;
      Params : in out Request_Parameters);
   --  Callback when the `scopes` response is arrived

   function Find_By_Id
     (Self : Variables_Holder;
      Id   : Integer)
      return Variables_References_Trees.Cursor;
   --  Find variable with the given Id and return cursor to it.
   --  Returns No_Element if Id is not found.

   procedure On_Variables_Response
     (Self   : in out Variables_Holder;
      Params : in out Request_Parameters);
   --  Callback when the `variables` response has arrived

   procedure On_Variable_Set
     (Self     : in out Variables_Holder;
      Params   : Request_Parameters;
      Variable : DAP.Tools.Variable);
   --  Callback when the variable's value is set

   procedure Find_By_Name
     (Name   : Virtual_String;
      Cursor : in out Variables_References_Trees.Cursor);
   --  Find the variable with the given Name and return cursor to it.
   --  Returns No_Element if not found.

   procedure On_Variable_Not_Found
     (Self   : in out Variables_Holder;
      Params : in out Request_Parameters);
   --  Called when the requested variable is not found.
   --  Informs View or Python side.

   Empty_Variable : constant DAP.Tools.Variable :=
     (name               => <>,
      value              => <>,
      a_type             => <>,
      presentationHint   => (Is_Set => False),
      evaluateName       => <>,
      variablesReference => 0,
      namedVariables     => (Is_Set => False),
      indexedVariables   => (Is_Set => False),
      memoryReference    => <>);
   --  Is used to mark variable that does not have children

   Empty_Variable_Data : constant Variable_Data :=
     (Non_Specified, Empty_Variable);

end DAP.Clients.Variables;
