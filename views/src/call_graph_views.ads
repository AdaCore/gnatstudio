------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2005-2022, AdaCore                     --
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

with Basic_Types;  use Basic_Types;
with GPS.Editors;
with GPS.Kernel;   use GPS.Kernel;
with GNATCOLL.VFS; use GNATCOLL.VFS;
with Language;

package Call_Graph_Views is

   procedure Register_Module
     (Kernel : access Kernel_Handle_Record'Class);
   --  Register the module into the list

   -------------------------
   -- Call Graph Provider --
   -------------------------

   type Call_Graph_Provider is interface;
   type Call_Graph_Provider_Access is access all Call_Graph_Provider'Class;

   function Supports_Language
     (Self : access Call_Graph_Provider;
      Lang : Language.Language_Access)
      return Boolean is abstract;
   --  Return True if the Provider can give results for Lang

   type View_Type is (View_Calls, View_Called_By);

   procedure Prepare_Call_Hierarchy
     (Self     : access Call_Graph_Provider;
      ID       : String;
      File     : Virtual_File;
      Location : GPS.Editors.Editor_Location'Class;
      Kind     : View_Type) is abstract;
   --  Resolve the proper node for File/Location

   procedure Is_Called_By
     (Self     : access Call_Graph_Provider;
      ID       : String;
      File     : Virtual_File;
      Location : GPS.Editors.Editor_Location'Class) is abstract;
   --  Request Is_Called_By references of the entity defined in File
   --  at (Line, Column)
   --  ID is unique and must be sent back when calling Add_Row
   --  and Finished_Computing.

   procedure Calls
     (Self     : access Call_Graph_Provider;
      ID       : String;
      File     : Virtual_File;
      Location : GPS.Editors.Editor_Location'Class) is abstract;
   --  Request Calls references of the entity defined in File
   --  at (Line, Column)
   --  ID is unique and must be sent back when calling Add_Row
   --  and Finished_Computing.

   procedure Finished_Prepare_Call_Hierarchy
     (Kernel  : Kernel_Handle;
      Name    : String;
      Line    : Editable_Line_Type;
      Column  : Visible_Column_Type;
      File    : Virtual_File;
      Project : Virtual_File;
      ID      : String;
      Kind    : View_Type);

   procedure Add_Row
     (Kernel       : Kernel_Handle;
      ID           : String;
      Decl_Name    : String;
      Decl_Line    : Integer;
      Decl_Column  : Integer;
      Decl_File    : Virtual_File;
      Decl_Project : Virtual_File;
      Ref_Line     : Integer;
      Ref_Column   : Integer;
      Ref_File     : Virtual_File;
      Dispatching  : Boolean);
   --  ID must match the ID given by Is_Called_By.
   --  The others arguments are the data to display in the Call Graph.

   procedure Finished_Computing (Kernel : Kernel_Handle; ID : String);
   --  Must always be called by the providers in response to any request.
   --  It will clean the dummy nodes, stored data and stop the pulse bar.

   procedure Set_LSP_Provider (Provider : Call_Graph_Provider_Access);
   --  When set, the data will be queried via the LSP if possible.
   --  Set to null, to always use the default provider.
   --  Note: for now, it will only query LSP results for Is_Called_By on
   --  Ada files.

end Call_Graph_Views;
