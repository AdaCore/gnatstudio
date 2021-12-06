------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2020-2021, AdaCore                  --
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

with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;

with VSS.Strings.Conversions;
with VSS.Unicode;

with Call_Graph_Views;                  use Call_Graph_Views;
with GPS.Editors; use GPS.Editors;
with GPS.Kernel.Project;                use GPS.Kernel.Project;
with GPS.LSP_Client.Language_Servers;   use GPS.LSP_Client.Language_Servers;
with GPS.LSP_Client.Requests;           use GPS.LSP_Client.Requests;
with GPS.LSP_Client.Requests.Called_By; use GPS.LSP_Client.Requests.Called_By;
with GPS.LSP_Client.Utilities;          use GPS.LSP_Client.Utilities;
with GPS.LSP_Module;                    use GPS.LSP_Module;
with GNATCOLL.JSON;
with GNATCOLL.VFS;                      use GNATCOLL.VFS;
with Language;                          use Language;
with LSP.Messages;                      use LSP.Messages;
with LSP.Types;                         use LSP.Types;
with GNATCOLL.Projects; use GNATCOLL.Projects;
with Basic_Types; use Basic_Types;

package body GPS.LSP_Client.Call_Tree is

   type LSP_Provider is new Call_Graph_Provider with record
      Kernel : Kernel_Handle;
   end record;
   type LSP_Provider_Access is access all LSP_Provider;

   overriding function Supports_Language
     (Self : access LSP_Provider;
      Lang : Language.Language_Access)
      return Boolean;

   overriding procedure Prepare_Call_Hierarchy
     (Self     : access LSP_Provider;
      ID       : String;
      File     : Virtual_File;
      Location : GPS.Editors.Editor_Location'Class);

   overriding procedure Is_Called_By
     (Self     : access LSP_Provider;
      ID       : String;
      File     : Virtual_File;
      Location : GPS.Editors.Editor_Location'Class);

   overriding procedure Calls
     (Self     : access LSP_Provider;
      ID       : String;
      File     : Virtual_File;
      Location : GPS.Editors.Editor_Location'Class);
   -------------------------------------
   -- Prepare_Call_Hierarchy_Request  --
   -------------------------------------

   type Prepare_Call_Hierarchy_Request
   is new Abstract_Prepare_Call_Hierarchy_Request with record
      ID : Unbounded_String;
   end record;
   type Prepare_Call_Hierarchy_Request_Access is
     access all Prepare_Call_Hierarchy_Request'Class;

   overriding procedure On_Result_Message
     (Self   : in out Prepare_Call_Hierarchy_Request;
      Result : LSP.Messages.CallHierarchyItem_Vector);

   overriding procedure On_Rejected
     (Self : in out Prepare_Call_Hierarchy_Request);

   overriding procedure On_Error_Message
     (Self    : in out Prepare_Call_Hierarchy_Request;
      Code    : LSP.Messages.ErrorCodes;
      Message : String;
      Data    : GNATCOLL.JSON.JSON_Value);

   overriding function Get_Task_Label
     (Self : Prepare_Call_Hierarchy_Request) return String
   is
      ("preparing call hierarchy");

   ------------------------
   -- Called_By_Request  --
   ------------------------

   type Called_By_Request is new Abstract_Called_By_Request with record
      ID : Unbounded_String;
   end record;
   type Called_By_Request_Access is access all Called_By_Request'Class;

   overriding procedure On_Result_Message
     (Self   : in out Called_By_Request;
      Result : LSP.Messages.CallHierarchyIncomingCall_Vector);

   overriding procedure On_Rejected (Self : in out Called_By_Request);

   overriding function Get_Task_Label (Self : Called_By_Request) return String
   is
      ("is called by");

   overriding procedure On_Error_Message
     (Self    : in out Called_By_Request;
      Code    : LSP.Messages.ErrorCodes;
      Message : String;
      Data    : GNATCOLL.JSON.JSON_Value);

   --------------------
   -- Calls_Request  --
   --------------------

   type Calls_Request is new Abstract_Calls_Request with record
      ID : Unbounded_String;
   end record;
   type Calls_Request_Access is access all Calls_Request'Class;

   overriding procedure On_Result_Message
     (Self   : in out Calls_Request;
      Result : LSP.Messages.CallHierarchyOutgoingCall_Vector);

   overriding procedure On_Rejected (Self : in out Calls_Request);

   overriding procedure On_Error_Message
     (Self    : in out Calls_Request;
      Code    : LSP.Messages.ErrorCodes;
      Message : String;
      Data    : GNATCOLL.JSON.JSON_Value);

   procedure Results_Received
     (Kernel   : Kernel_Handle;
      ID       : Unbounded_String;
      Item     : LSP.Messages.CallHierarchyItem;
      Spans    : LSP.Messages.Span_Vector;
      Kinds    : LSP.Messages.AlsReferenceKind_Vector;
      Ref_File : Virtual_File);
   --  Common function to process the results from "Calls" and "Called By"
   --  requests.
   --  Kinds is empty or as the same size than Spans.
   --  Ref_File is the relative file where Spans are located. If No_File,
   --  then use Item.uri.

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self   : in out Prepare_Call_Hierarchy_Request;
      Result : LSP.Messages.CallHierarchyItem_Vector) is
   begin
      for Item of Result loop
         declare
            File     : constant Virtual_File := To_Virtual_File (Item.uri);
            Holder   : constant GPS.Editors.Controlled_Editor_Buffer_Holder :=
              Self.Kernel.Get_Buffer_Factory.Get_Holder (File => File);
            Location : constant Editor_Location'Class :=
              LSP_Position_To_Location
                (Editor   => Holder.Editor,
                 Position => Item.selectionRange.first);
         begin
            Call_Graph_Views.Finished_Prepare_Call_Hierarchy
              (Kernel       => Self.Kernel,
               Name         =>
                 VSS.Strings.Conversions.To_UTF_8_String (Item.name),
               Line         => Editable_Line_Type (Location.Line),
               Column       => Location.Column,
               File         => File,
               Project      => Lookup_Project
                 (Self.Kernel, File).Project_Path,
               Kind         => View_Called_By);
         end;
      end loop;
   end On_Result_Message;

   -----------------
   -- On_Rejected --
   -----------------

   overriding procedure On_Rejected
     (Self : in out Prepare_Call_Hierarchy_Request) is
   begin
      Call_Graph_Views.Finished_Computing (Self.Kernel, To_String (Self.ID));
   end On_Rejected;

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out Prepare_Call_Hierarchy_Request;
      Code    : LSP.Messages.ErrorCodes;
      Message : String;
      Data    : GNATCOLL.JSON.JSON_Value) is
   begin
      Call_Graph_Views.Finished_Computing (Self.Kernel, To_String (Self.ID));
   end On_Error_Message;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self   : in out Called_By_Request;
      Result : LSP.Messages.CallHierarchyIncomingCall_Vector) is
   begin
      for Item of Result loop
         --  According to documentation: "This is the range relative to the
         --  caller denoted by [`this.from`](#CallHierarchyIncomingCall.from)"
         --  thus set Ref_File to No_File.
         Results_Received
           (Kernel   => Self.Kernel,
            ID       => Self.ID,
            Item     => Item.from,
            Spans    => Item.fromRanges,
            Kinds    => Item.kinds,
            Ref_File => No_File);
      end loop;

      Call_Graph_Views.Finished_Computing (Self.Kernel, To_String (Self.ID));
   end On_Result_Message;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self   : in out Calls_Request;
      Result : LSP.Messages.CallHierarchyOutgoingCall_Vector)
   is
      --  According to documentation: "This is the range relative to
      --  the caller, e.g the item passed to `callHierarchy/outgoingCalls`
      --  request"
      Ref_File : constant Virtual_File := To_Virtual_File (Self.Item.uri);
   begin
      for Item of Result loop
         Results_Received
           (Kernel   => Self.Kernel,
            ID       => Self.ID,
            Item     => Item.to,
            Spans    => Item.fromRanges,
            Kinds    => Item.kinds,
            Ref_File => Ref_File);
      end loop;

      Call_Graph_Views.Finished_Computing (Self.Kernel, To_String (Self.ID));
   end On_Result_Message;

   ----------------------
   -- Results_Received --
   ----------------------

   procedure Results_Received
     (Kernel   : Kernel_Handle;
      ID       : Unbounded_String;
      Item     : LSP.Messages.CallHierarchyItem;
      Spans    : LSP.Messages.Span_Vector;
      Kinds    : LSP.Messages.AlsReferenceKind_Vector;
      Ref_File : Virtual_File)
   is
      Decl_Name      : Unbounded_String;
      Decl_Line      : Integer;
      Decl_Column    : Integer;
      Decl_File      : Virtual_File;
      Decl_Project   : Virtual_File;
      Is_Dispatching : Boolean := False;
      Ref_Line       : Integer;
      Ref_Column     : Integer;
      Kind_Index     : Integer := Kinds.First_Index;

      procedure Get_Decl (X : LSP.Messages.CallHierarchyItem);
      --  Retrieve declaration data and store them in local variables

      procedure Get_Reference_Record (X : LSP.Messages.Span);
      --  Retrieve the reference data and stotr them in local variables

      --------------
      -- Get_Decl --
      --------------

      procedure Get_Decl (X : LSP.Messages.CallHierarchyItem) is
      begin
         Decl_File := To_Virtual_File (X.uri);
         Decl_Name :=
           VSS.Strings.Conversions.To_Unbounded_UTF_8_String (X.name);

         declare
            Holder   : constant GPS.Editors.Controlled_Editor_Buffer_Holder :=
              Kernel.Get_Buffer_Factory.Get_Holder (File => Decl_File);
            Location : constant GPS.Editors.Editor_Location'Class :=
              GPS.LSP_Client.Utilities.LSP_Position_To_Location
                (Holder.Editor, X.span.first);

         begin
            Decl_Line   := Location.Line;
            Decl_Column := Integer (Location.Column);
         end;

         Decl_Project := Lookup_Project (Kernel, Decl_File).Project_Path;
      end Get_Decl;

      --------------------------
      -- Get_Reference_Record --
      --------------------------

      procedure Get_Reference_Record (X : LSP.Messages.Span) is
         use type VSS.Unicode.UTF16_Code_Unit_Count;

      begin
         if Kind_Index <= Kinds.Last_Index then
            Is_Dispatching :=
              Kinds (Kind_Index) = LSP.Messages.Dispatching_Call;
            Kind_Index := Kind_Index + 1;
         end if;
         Ref_Line := Integer (X.first.line + 1);
         Ref_Column := Integer (X.first.character + 1);
      end Get_Reference_Record;
   begin
      --  Add a child for each of the references found.

      Get_Decl (Item);
      for Span of Spans loop
         Get_Reference_Record (Span);
         Call_Graph_Views.Add_Row
           (Kernel       => Kernel,
            ID           => To_String (ID),
            Decl_Name    => To_String (Decl_Name),
            Decl_Line    => Decl_Line,
            Decl_Column  => Decl_Column,
            Decl_File    => Decl_File,
            Decl_Project => Decl_Project,
            Ref_Line     => Ref_Line,
            Ref_Column   => Ref_Column,
            Ref_File     =>
              (if Ref_File /= No_File then Ref_File else Decl_File),
            Dispatching  => Is_Dispatching);
      end loop;
   end Results_Received;

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out Called_By_Request;
      Code    : LSP.Messages.ErrorCodes;
      Message : String;
      Data    : GNATCOLL.JSON.JSON_Value) is
   begin
      Call_Graph_Views.Finished_Computing (Self.Kernel, To_String (Self.ID));
   end On_Error_Message;

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out Calls_Request;
      Code    : LSP.Messages.ErrorCodes;
      Message : String;
      Data    : GNATCOLL.JSON.JSON_Value) is
   begin
      Call_Graph_Views.Finished_Computing (Self.Kernel, To_String (Self.ID));
   end On_Error_Message;

   -----------------
   -- On_Rejected --
   -----------------

   overriding procedure On_Rejected (Self : in out Called_By_Request) is
   begin
      Call_Graph_Views.Finished_Computing (Self.Kernel, To_String (Self.ID));
   end On_Rejected;

   -----------------
   -- On_Rejected --
   -----------------

   overriding procedure On_Rejected (Self : in out Calls_Request) is
   begin
      Call_Graph_Views.Finished_Computing (Self.Kernel, To_String (Self.ID));
   end On_Rejected;

   -----------------------
   -- Supports_Language --
   -----------------------

   overriding function Supports_Language
     (Self : access LSP_Provider;
      Lang : Language.Language_Access)
      return Boolean is
   begin
      return Get_Language_Server (Lang) /= null;
   end Supports_Language;

   ----------------------------
   -- Prepare_Call_Hierarchy --
   ----------------------------

   overriding procedure Prepare_Call_Hierarchy
     (Self     : access LSP_Provider;
      ID       : String;
      File     : Virtual_File;
      Location : GPS.Editors.Editor_Location'Class)
   is
      R : Prepare_Call_Hierarchy_Request_Access;
   begin
      R := new Prepare_Call_Hierarchy_Request'
        (LSP_Request with
           Kernel   => Self.Kernel,
           File     => File,
           Position => Location_To_LSP_Position (Location),
           ID       => To_Unbounded_String (ID));
      GPS.LSP_Client.Requests.Execute
        (Self.Kernel.Get_Language_Handler.Get_Language_From_File (File),
         Request_Access (R));
   end Prepare_Call_Hierarchy;

   ------------------
   -- Is_Called_By --
   ------------------

   overriding procedure Is_Called_By
     (Self     : access LSP_Provider;
      ID       : String;
      File     : Virtual_File;
      Location : GPS.Editors.Editor_Location'Class)
   is
      R        : Called_By_Request_Access;
      Position : constant LSP.Messages.Position :=
        Location_To_LSP_Position (Location);
   begin
      R := new Called_By_Request'
        (LSP_Request with
           Kernel => Self.Kernel,
         Item   => LSP.Messages.CallHierarchyItem'
           (uri            => To_URI (File),
            span           => LSP.Messages.Span'
              (first => Position,
               last  => Position),
            selectionRange => LSP.Messages.Span'
              (first => Position,
               last  => Position),
            kind           => A_Function,
            name           => <>,
            detail         => <>,
            tags           => (Is_Set => False)),
           ID     => To_Unbounded_String (ID));

      GPS.LSP_Client.Requests.Execute
        (Self.Kernel.Get_Language_Handler.Get_Language_From_File (File),
         Request_Access (R));
   end Is_Called_By;

   -----------
   -- Calls --
   -----------

   overriding procedure Calls
     (Self     : access LSP_Provider;
      ID       : String;
      File     : Virtual_File;
      Location : GPS.Editors.Editor_Location'Class)
   is
      R        : Calls_Request_Access;
      Position : constant LSP.Messages.Position :=
        Location_To_LSP_Position (Location);
   begin
      R := new Calls_Request'
        (LSP_Request with
           Kernel => Self.Kernel,
         Item   => LSP.Messages.CallHierarchyItem'
           (uri            => To_URI (File),
            span           => LSP.Messages.Span'
              (first => Position,
               last  => Position),
            selectionRange => LSP.Messages.Span'
              (first => Position,
               last  => Position),
            kind           => A_Function,
            name           => <>,
            detail         => <>,
            tags           => (Is_Set => False)),
         ID     => To_Unbounded_String (ID));

      GPS.LSP_Client.Requests.Execute
        (Self.Kernel.Get_Language_Handler.Get_Language_From_File (File),
         Request_Access (R));
   end Calls;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module (Kernel : Kernel_Handle)
   is
      Provider : constant LSP_Provider_Access :=
        new LSP_Provider'(Kernel => Kernel);
   begin
      Call_Graph_Views.Set_LSP_Provider
        (Call_Graph_Provider_Access (Provider));
   end Register_Module;

end GPS.LSP_Client.Call_Tree;
