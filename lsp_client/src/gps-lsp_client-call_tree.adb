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
with Basic_Types;                       use Basic_Types;
with Call_Graph_Views;                  use Call_Graph_Views;
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

package body GPS.LSP_Client.Call_Tree is

   type LSP_Provider is new Call_Graph_Provider with record
      Kernel : Kernel_Handle;
   end record;
   type LSP_Provider_Access is access all LSP_Provider;

   overriding function Supports_Language
     (Self : access LSP_Provider;
      Lang : Language.Language_Access)
      return Boolean;

   overriding procedure Is_Called_By
     (Self   : access LSP_Provider;
      ID     : String;
      File   : Virtual_File;
      Line   : Integer;
      Column : Integer);

   overriding procedure Calls
     (Self   : access LSP_Provider;
      ID     : String;
      File   : Virtual_File;
      Line   : Integer;
      Column : Integer);

   type Called_By_Request is new Abstract_Called_By_Request with record
      ID : Unbounded_String;
   end record;
   type Called_By_Request_Access is access all Called_By_Request'Class;

   overriding procedure On_Result_Message
     (Self   : in out Called_By_Request;
      Result : LSP.Messages.ALS_Subprogram_And_References_Vector);

   overriding procedure On_Rejected (Self : in out Called_By_Request);

   overriding function Get_Task_Label (Self : Called_By_Request) return String
   is
      ("is called by");

   overriding procedure On_Error_Message
     (Self    : in out Called_By_Request;
      Code    : LSP.Messages.ErrorCodes;
      Message : String;
      Data    : GNATCOLL.JSON.JSON_Value);

   type Calls_Request is new Abstract_Calls_Request with record
      ID : Unbounded_String;
   end record;
   type Calls_Request_Access is access all Calls_Request'Class;

   overriding procedure On_Result_Message
     (Self   : in out Calls_Request;
      Result : LSP.Messages.ALS_Subprogram_And_References_Vector);

   overriding procedure On_Rejected (Self : in out Calls_Request);

   overriding procedure On_Error_Message
     (Self    : in out Calls_Request;
      Code    : LSP.Messages.ErrorCodes;
      Message : String;
      Data    : GNATCOLL.JSON.JSON_Value);

   procedure Results_Received
     (Kernel : Kernel_Handle;
      ID     : Unbounded_String;
      Result : LSP.Messages.ALS_Subprogram_And_References_Vector);
   --  Common function to process the results from "Calls" and "Called By"
   --  requests.

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self   : in out Called_By_Request;
      Result : LSP.Messages.ALS_Subprogram_And_References_Vector) is
   begin
      Results_Received (Self.Kernel, Self.ID, Result);
   end On_Result_Message;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self   : in out Calls_Request;
      Result : LSP.Messages.ALS_Subprogram_And_References_Vector) is
   begin
      Results_Received (Self.Kernel, Self.ID, Result);
   end On_Result_Message;

   ----------------------
   -- Results_Received --
   ----------------------

   procedure Results_Received
     (Kernel : Kernel_Handle;
      ID     : Unbounded_String;
      Result : LSP.Messages.ALS_Subprogram_And_References_Vector)
   is
      Decl_Name      : Unbounded_String;
      Decl_Line      : Integer;
      Decl_Column    : Integer;
      Decl_File      : Virtual_File;
      Decl_Project   : Virtual_File;
      Is_Dispatching : Boolean;
      Ref_Line       : Integer;
      Ref_Column     : Integer;
      Ref_File       : Virtual_File;

      procedure Get_Decl (X : LSP.Messages.ALS_Subprogram_And_References);
      --  Retrieve declaration data and store them in local variables

      procedure Get_Reference_Record (X : LSP.Messages.Location);
      --  Retrieve the reference data and stotr them in local variables

      --------------
      -- Get_Decl --
      --------------

      procedure Get_Decl (X : LSP.Messages.ALS_Subprogram_And_References) is
      begin
         Decl_Name    := To_UTF_8_Unbounded_String (X.name);
         Decl_Line    := Integer (X.loc.span.first.line + 1);
         Decl_Column  :=
           Integer
             (UTF_16_Offset_To_Visible_Column (X.loc.span.first.character));
         Decl_File    := To_Virtual_File (X.loc.uri);
         Decl_Project := Lookup_Project (Kernel, Decl_File).Project_Path;
      end Get_Decl;

      --------------------------
      -- Get_Reference_Record --
      --------------------------

      procedure Get_Reference_Record (X : LSP.Messages.Location) is
      begin
         Is_Dispatching := False;
         for K of X.alsKind.As_Strings loop
            if K = "dispatching call" then
               Is_Dispatching := True;
               exit;
            end if;
         end loop;

         Ref_Line := Integer (X.span.first.line + 1);
         Ref_Column := Integer (X.span.first.character + 1);
         Ref_File  := To_Virtual_File (X.uri);
      end Get_Reference_Record;
   begin
      --  Add a child for each of the references found.

      for Reference of Result loop
         Get_Decl (Reference);
         for R of Reference.refs loop
            Get_Reference_Record (R);
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
               Ref_File     => Ref_File,
               Dispatching  => Is_Dispatching);
         end loop;
      end loop;

      Call_Graph_Views.Finished_Computing (Kernel, To_String (ID));
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

   ------------------
   -- Is_Called_By --
   ------------------

   overriding procedure Is_Called_By
     (Self   : access LSP_Provider;
      ID     : String;
      File   : Virtual_File;
      Line   : Integer;
      Column : Integer)
   is
      R : Called_By_Request_Access;
   begin
      R := new Called_By_Request'
        (LSP_Request with
           Kernel => Self.Kernel,
           File   => File,
           Line   => Positive (Line),
           Column => Visible_Column_Type (Column),
           ID     => To_Unbounded_String (ID));

      GPS.LSP_Client.Requests.Execute
        (Self.Kernel.Get_Language_Handler.Get_Language_From_File (File),
         Request_Access (R));
   end Is_Called_By;

   -----------
   -- Calls --
   -----------

   overriding procedure Calls
     (Self   : access LSP_Provider;
      ID     : String;
      File   : Virtual_File;
      Line   : Integer;
      Column : Integer)
   is
      R : Calls_Request_Access;
   begin
      R := new Calls_Request'
        (LSP_Request with
           Kernel => Self.Kernel,
           File   => File,
           Line   => Positive (Line),
           Column => Visible_Column_Type (Column),
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
