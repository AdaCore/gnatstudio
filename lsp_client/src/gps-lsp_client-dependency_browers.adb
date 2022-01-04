------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2020-2022, AdaCore                  --
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

with GNATCOLL.JSON;
with GNATCOLL.Projects;               use GNATCOLL.Projects;
with GNATCOLL.Traces;                 use GNATCOLL.Traces;
with GNATCOLL.VFS;                    use GNATCOLL.VFS;

with VSS.Strings.Conversions;

with Browsers.Dependency_Items;       use Browsers.Dependency_Items;

with GPS.Editors;                     use GPS.Editors;
with GPS.LSP_Client.Language_Servers; use GPS.LSP_Client.Language_Servers;
with GPS.LSP_Client.Requests.Show_Dependencies;
use GPS.LSP_Client.Requests.Show_Dependencies;
with GPS.LSP_Client.Requests;         use GPS.LSP_Client.Requests;
with GPS.LSP_Client.Utilities;        use GPS.LSP_Client.Utilities;
with GPS.LSP_Module;                  use GPS.LSP_Module;
with Language;                        use Language;
with Language.Ada;                    use Language.Ada;
with LSP.Messages;

package body GPS.LSP_Client.Dependency_Browers is

   Me : constant Trace_Handle := Create ("GPS.LSP.BROWSERS");

   type LSP_Dependency_Browser_Provider_Type is
     new Dependency_Browser_Provider_Interface with null record;

   overriding procedure Compute_Dependencies
     (Provider      : LSP_Dependency_Browser_Provider_Type;
      Kernel        : not null access Kernel_Handle_Record'Class;
      File          : Virtual_File;
      Project       : GNATCOLL.Projects.Project_Type;
      Kind          : Dependency_Kind_Type;
      Show_Implicit : Boolean);

   type Show_Dependencies_Request is new Abstract_Show_Dependencies_Request
   with record
      Close_Document_On_Finish : Boolean := False;
   end record;
   type Show_Dependencies_Request_Access is
     access all Show_Dependencies_Request;

   overriding procedure On_Result_Message
     (Self   : in out Show_Dependencies_Request;
      Result : LSP.Messages.ALS_Unit_Description_Vector);

   overriding procedure On_Rejected
     (Self : in out Show_Dependencies_Request);

   overriding procedure On_Error_Message
     (Self    : in out Show_Dependencies_Request;
      Code    : LSP.Messages.ErrorCodes;
      Message : String;
      Data    : GNATCOLL.JSON.JSON_Value);

   overriding function Get_Task_Label
     (Self : Show_Dependencies_Request) return String
   is
     ("querying dependencies");

   function To_LSP_Kind
     (Kind : Dependency_Kind_Type)
      return LSP.Messages.ALS_ShowDependenciesKind;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self   : in out Show_Dependencies_Request;
      Result : LSP.Messages.ALS_Unit_Description_Vector)
   is
      Dependencies : Dependency_Description_Vectors.Vector;
      Lang         : constant Language_Access :=
        Self.Kernel.Get_Language_Handler.Get_Language_From_File (Self.File);
      Server       : constant Language_Server_Access := Get_Language_Server
        (Lang);
   begin
      if Self.Close_Document_On_Finish and then Server /= null then
         Server.Get_Client.Send_Text_Document_Did_Close (Self.File);
      end if;

      for Dependency of Result loop
         Dependencies.Append
           (Dependency_Description_Type'
              (File         => To_Virtual_File (Dependency.uri),
               Project_Path => To_Virtual_File (Dependency.projectUri)));
      end loop;

      Browsers.Dependency_Items.Show_Dependencies
        (Kernel       => Self.Kernel,
         File         => Self.File,
         Project      => No_Project,
         Dependencies => Dependencies);
   end On_Result_Message;

   -----------------
   -- On_Rejected --
   -----------------

   overriding procedure On_Rejected
     (Self : in out Show_Dependencies_Request)
   is
      Lang   : constant Language_Access :=
        Self.Kernel.Get_Language_Handler.Get_Language_From_File (Self.File);
      Server : constant Language_Server_Access := Get_Language_Server (Lang);
   begin
      if Self.Close_Document_On_Finish and then Server /= null then
         Server.Get_Client.Send_Text_Document_Did_Close (Self.File);
      end if;

      Trace
        (Me,
         VSS.Strings.Conversions.To_UTF_8_String (Self.Method)
         & " has been rejected");
   end On_Rejected;

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out Show_Dependencies_Request;
      Code    : LSP.Messages.ErrorCodes;
      Message : String;
      Data    : GNATCOLL.JSON.JSON_Value)
   is
      Lang   : constant Language_Access :=
        Self.Kernel.Get_Language_Handler.Get_Language_From_File (Self.File);
      Server : constant Language_Server_Access := Get_Language_Server
        (Lang);
   begin
      if Self.Close_Document_On_Finish and then Server /= null then
         Server.Get_Client.Send_Text_Document_Did_Close (Self.File);
      end if;

      Trace
        (Me,
         "Error received after sending "
         & VSS.Strings.Conversions.To_UTF_8_String (Self.Method));
   end On_Error_Message;

   -----------------
   -- To_LSP_Kind --
   -----------------

   function To_LSP_Kind
     (Kind : Dependency_Kind_Type)
      return LSP.Messages.ALS_ShowDependenciesKind is
   begin
      case Kind is
         when Show_Imported  => return LSP.Messages.Show_Imported;
         when Show_Importing => return LSP.Messages.Show_Importing;
      end case;
   end To_LSP_Kind;

   --------------------------
   -- Compute_Dependencies --
   --------------------------

   overriding procedure Compute_Dependencies
     (Provider      : LSP_Dependency_Browser_Provider_Type;
      Kernel        : not null access Kernel_Handle_Record'Class;
      File          : Virtual_File;
      Project       : GNATCOLL.Projects.Project_Type;
      Kind          : Dependency_Kind_Type;
      Show_Implicit : Boolean)
   is
      Request                  : Show_Dependencies_Request_Access;
      Buffer                   : constant Editor_Buffer'Class :=
        Kernel.Get_Buffer_Factory.Get
          (File        => File,
           Force       => False,
           Open_Buffer => False,
           Open_View   => False);
      Lang                     : constant Language_Access :=
        Kernel.Get_Language_Handler.Get_Language_From_File (File);
      Server                   : constant Language_Server_Access :=
        Get_Language_Server (Lang);
      Close_Document_On_Finish : Boolean := False;
   begin

      --  If there is no opened editor for the queried file, make sure to
      --  open the document on the server side first.

      if Buffer = Nil_Editor_Buffer and then Server /= null then
         declare
            Buffer : constant Editor_Buffer'Class :=
              Kernel.Get_Buffer_Factory.Get
                (File        => File,
                 Open_Buffer => True,
                 Open_View   => False);
         begin
            if Buffer.Get_Language /= null then
               Server.Get_Client.Send_Text_Document_Did_Open (File);
               Close_Document_On_Finish := True;
            end if;

            Buffer.Close;
         end;
      end if;

      Request := new Show_Dependencies_Request (Kernel_Handle (Kernel));
      Request.File := File;
      Request.Kind := To_LSP_Kind (Kind);
      Request.Close_Document_On_Finish := Close_Document_On_Finish;
      Request.Show_Implicit := Show_Implicit;

      GPS.LSP_Client.Requests.Execute
        (Ada_Lang, GPS.LSP_Client.Requests.Request_Access (Request));
   end Compute_Dependencies;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : not null access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Kernel);
      Provider : constant Dependency_Browser_Provider_Access :=
        new LSP_Dependency_Browser_Provider_Type;
   begin
      if LSP_Ada_Support_Trace_Is_Active then
         Browsers.Dependency_Items.Set_Dependency_Browser_Provider
           (Provider);
      end if;
   end Register_Module;

end GPS.LSP_Client.Dependency_Browers;
