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

with VSS.Strings.Conversions;

with Glib;                            use Glib;
with Glib.Main;                       use Glib.Main;
with GNATCOLL.JSON;
with GNATCOLL.Traces;                 use GNATCOLL.Traces;
with GNATCOLL.VFS;                    use GNATCOLL.VFS;

with GPS.Editors;                     use GPS.Editors;
with GPS.LSP_Client.Requests;         use GPS.LSP_Client.Requests;
with GPS.LSP_Client.Requests.Document_Symbols;
use GPS.LSP_Client.Requests.Document_Symbols;
with GPS.LSP_Client.Language_Servers; use GPS.LSP_Client.Language_Servers;
with GPS.LSP_Client.Utilities;        use GPS.LSP_Client.Utilities;
with GPS.LSP_Module;                  use GPS.LSP_Module;

with Basic_Types;
with Language;                        use Language;
with LSP.Messages;                    use LSP.Messages;
with LSP.Types;                       use LSP.Types;
with Outline_View;                    use Outline_View;

package body GPS.LSP_Client.Outline is

   Me        : constant Trace_Handle :=
     Create ("GPS.LSP.OUTLINE.ADVANCED", Off);
   Me_Debug  : constant Trace_Handle :=
     Create ("GPS.LSP.OUTLINE.DEBUG", Off);
   Me_Active : constant Trace_Handle :=
     Create ("GPS.LSP.OUTLINE", On);
   Me_Use_Timeout : constant Trace_Handle :=
     Create ("GPS.LSP.OUTLINE.USE_TIMEOUT", Off);

   ----------------------
   -- Outline Provider --
   ----------------------

   type Result_Access is access LSP.Messages.Symbol_Vector;

   type Outline_LSP_Provider is new Outline_View.Outline_Provider with record
      Kernel        : Kernel_Handle;
      File          : Virtual_File := No_File;
      Model         : Outline_Model_Access := null;
      Loader_Id     : Glib.Main.G_Source_Id := No_Source_Id;
      Tree_Cursor   : DocumentSymbol_Trees.Cursor :=
        DocumentSymbol_Trees.No_Element;
      Vector_Cursor : SymbolInformation_Vectors.Element_Vectors.Cursor :=
        SymbolInformation_Vectors.Element_Vectors.No_Element;
      Result        : Result_Access := null;
   end record;
   type Outline_LSP_Provider_Access is access all Outline_LSP_Provider;

   overriding procedure Start_Fill
     (Self : access Outline_LSP_Provider; File : Virtual_File);

   overriding procedure Stop_Fill (Self : access Outline_LSP_Provider);
   --  Stop the async_load if necessary and clean the Outline model

   overriding function Support_Language
     (Self : access Outline_LSP_Provider;
      Lang : Language_Access)
      return Boolean;

   -----------------
   -- LSP Request --
   -----------------

   type GPS_LSP_Outline_Request is
     new Document_Symbols_Request with record
      Provider                 : Outline_LSP_Provider_Access;

      Close_Document_On_Finish : Boolean := False;
      --  Set to True if we should send a didClose notification to the server
      --  after finishing the request.
      --  This is needed when there is no opened editor for the file queried by
      --  the Outline: in that case, we should open the document on the server
      --  side via a didOpen notification before sending the
      --  textDocument/documentSymbols request and then close the document
      --  once the request finished.
   end record;
   type GPS_LSP_Outline_Request_Access is access all
     GPS_LSP_Outline_Request'Class;

   overriding procedure On_Result_Message
     (Self   : in out GPS_LSP_Outline_Request;
      Result : LSP.Messages.Symbol_Vector);

   overriding procedure On_Error_Message
     (Self    : in out GPS_LSP_Outline_Request;
      Code    : LSP.Messages.ErrorCodes;
      Message : String;
      Data    : GNATCOLL.JSON.JSON_Value);

   overriding procedure On_Rejected (Self : in out GPS_LSP_Outline_Request);

   function Get_Optional_String (S : Optional_String) return String;

   function Get_Optional_Boolean (B : Optional_Boolean) return Boolean;

   function Get_Optional_Visibility
     (V : Optional_Als_Visibility) return Construct_Visibility;

   ----------------
   -- Async Load --
   ----------------

   package Async_Load is new Glib.Main.Generic_Sources
     (Outline_LSP_Provider_Access);
   function On_Idle_Load_Tree
     (Self : Outline_LSP_Provider_Access) return Boolean;
   function On_Idle_Load_Vector
     (Self : Outline_LSP_Provider_Access) return Boolean;
   procedure Free_Idle
     (Self    : Outline_LSP_Provider_Access;
      Stopped : Boolean);

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self   : in out GPS_LSP_Outline_Request;
      Result : LSP.Messages.Symbol_Vector)
   is
      Lang   : constant Language_Access :=
        Self.Kernel.Get_Language_Handler.Get_Language_From_File (Self.File);
      Server : constant Language_Server_Access := Get_Language_Server
        (Lang);

      procedure Register (Callback : Async_Load.G_Source_Func);

      procedure Register (Callback : Async_Load.G_Source_Func) is
      begin
         if Me_Use_Timeout.Active then
            Self.Provider.Loader_Id :=
              Async_Load.Timeout_Add (100, Callback, Self.Provider);

         else
            Self.Provider.Loader_Id := Async_Load.Idle_Add
              (Callback, Self.Provider);
         end if;
      end Register;

   begin
      Trace (Me_Debug, "On_Result_Message");

      if Self.Close_Document_On_Finish and then Server /= null then
         Server.Get_Client.Send_Text_Document_Did_Close (Self.File);
      end if;

      if Self.Provider.Loader_Id /= No_Source_Id then
         Remove (Self.Provider.Loader_Id);
         Free_Idle (Self.Provider, True);
      end if;

      Trace
        (Me,
         "Results received for "
         & VSS.Strings.Conversions.To_UTF_8_String (Self.Method));

      begin
         Self.Provider.Model :=
           Outline_View.Get_Outline_Model
             (Self.Provider.Kernel, Self.Provider.File);
      exception
         when Outline_View.Outline_Error =>
            Trace (Me, "The Outline view was closed");
            return;
      end;

      if Self.Provider.Model = null then
         return;
      else
         Trace (Me_Debug, "Clear_Outline_Model");
         Outline_View.Clear_Outline_Model (Self.Provider.Model);
      end if;

      Self.Provider.Result := new LSP.Messages.Symbol_Vector'(Result);

      if Result.Is_Tree then
         Trace (Me_Debug, "Process result tree");
         Self.Provider.Tree_Cursor := Self.Provider.Result.Tree.Root;
         Register (On_Idle_Load_Tree'Access);

      else
         Trace (Me_Debug, "Process result vector");
         Self.Provider.Vector_Cursor := Self.Provider.Result.Vector.First;
         Register (On_Idle_Load_Vector'Access);
      end if;
      Trace (Me_Debug, "On_Result_Message done");
   end On_Result_Message;

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out GPS_LSP_Outline_Request;
      Code    : LSP.Messages.ErrorCodes;
      Message : String;
      Data    : GNATCOLL.JSON.JSON_Value)
   is
      Lang   : constant Language_Access :=
        Self.Kernel.Get_Language_Handler.Get_Language_From_File (Self.File);
      Server : constant Language_Server_Access := Get_Language_Server
        (Lang);
   begin
      Trace (Me_Debug, "On_Error_Message");
      if Self.Close_Document_On_Finish and then Server /= null then
         Server.Get_Client.Send_Text_Document_Did_Close (Self.File);
      end if;

      Trace
        (Me,
         "Error received after sending "
         & VSS.Strings.Conversions.To_UTF_8_String (Self.Method));
      Outline_View.Finished_Computing
        (Self.Provider.Kernel, Status => Outline_View.Failed);
      Trace (Me_Debug, "On_Error_Message done");
   end On_Error_Message;

   -----------------
   -- On_Rejected --
   -----------------

   overriding procedure On_Rejected
     (Self : in out GPS_LSP_Outline_Request) is
   begin
      Trace
        (Me,
         VSS.Strings.Conversions.To_UTF_8_String (Self.Method)
         & " has been rejected");
      Outline_View.Finished_Computing
        (Self.Provider.Kernel, Status => Outline_View.Failed);
      Trace (Me_Debug, "On_Rejected done");
   end On_Rejected;

   ----------------
   -- Start_Fill --
   ----------------

   overriding procedure Start_Fill
     (Self : access Outline_LSP_Provider; File : Virtual_File)
   is
      R                        : GPS_LSP_Outline_Request_Access;
      Buffer                   : constant Editor_Buffer'Class :=
        Self.Kernel.Get_Buffer_Factory.Get
          (File        => File,
           Force       => False,
           Open_Buffer => False,
           Open_View   => False);
      Lang                     : constant Language_Access :=
        Self.Kernel.Get_Language_Handler.Get_Language_From_File (File);
      Server                   : constant Language_Server_Access :=
        Get_Language_Server (Lang);
      Close_Document_On_Finish : Boolean := False;
   begin
      Trace (Me, "Sending documentSymbols Request");
      Self.File := File;

      --  If there is no opened editor for the queried file, make sure to
      --  open the document on the server side first.

      if Buffer = Nil_Editor_Buffer and then Server /= null then
         declare
            Buffer : constant Editor_Buffer'Class :=
              Self.Kernel.Get_Buffer_Factory.Get
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

      R :=
        new GPS_LSP_Outline_Request'
          (LSP_Request
           with
             Provider                 => Outline_LSP_Provider_Access (Self),
             Close_Document_On_Finish => Close_Document_On_Finish,
             File                     => File,
             Kernel                   => Self.Kernel);

      GPS.LSP_Client.Requests.Execute
        (Self.Kernel.Get_Language_Handler.Get_Language_From_File (File),
         Request_Access (R));
      Trace (Me_Debug, "Start_Fill done");
   end Start_Fill;

   ---------------
   -- Stop_Fill --
   ---------------

   overriding procedure Stop_Fill (Self : access Outline_LSP_Provider) is
   begin
      Trace (Me_Debug, "Stop_Fill");
      if Self.Loader_Id /= No_Source_Id then
         Trace (Me_Debug, "Stop_Fill Loader_Id /= No_Source_Id");
         if Self.Model /= null then
            Trace (Me_Debug, "Stop_Fill Model /= null");
            Outline_View.Clear_Outline_Model (Self.Model);
            Outline_View.Free (Self.Model);
            Trace (Me_Debug, "Stop_Fill Model is freed");
         end if;
         Trace (Me_Debug, "Stop_Fill Model remove Loader_Id");
         Remove (Self.Loader_Id);
         Free_Idle (Outline_LSP_Provider_Access (Self), True);
      end if;
      Trace (Me_Debug, "Stop_Fill done");
   end Stop_Fill;

   ----------------------
   -- Support_Language --
   ----------------------

   overriding function Support_Language
     (Self : access Outline_LSP_Provider;
      Lang : Language_Access)
      return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return Get_Language_Server (Lang) /= null;
   end Support_Language;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module (Kernel : Kernel_Handle) is
   begin
      if Me_Active.Active then
         declare
            Provider : constant Outline_LSP_Provider_Access :=
              new Outline_LSP_Provider'(Kernel => Kernel, others => <>);
         begin
            Outline_View.Set_LSP_Provider
              (Outline_View.Outline_Provider_Access (Provider));
         end;
      end if;
   end Register_Module;

   -----------------------
   -- On_Idle_Load_Tree --
   -----------------------

   function On_Idle_Load_Tree
     (Self : Outline_LSP_Provider_Access) return Boolean
   is
      use DocumentSymbol_Trees;
      Nb_Added_Rows : Integer := 0;
      Prev_Depth    : Integer;
      Tree_Iter     : Tree_Iterator_Interfaces.Forward_Iterator'Class :=
        Iterate (Self.Result.Tree);
      Holder : constant GPS.Editors.Controlled_Editor_Buffer_Holder :=
        Self.Kernel.Get_Buffer_Factory.Get_Holder (File => Self.File);

   begin
      Trace (Me_Debug, "On_Idle_Load_Tree");
      if Is_Root (Self.Tree_Cursor) then
         Self.Tree_Cursor := Tree_Iter.Next (Self.Tree_Cursor);
      end if;

      Prev_Depth := Integer (Depth (Self.Tree_Cursor));

      while Self.Tree_Cursor /= No_Element loop
         declare
            Symbol         : constant DocumentSymbol :=
              Element (Self.Tree_Cursor);
            First_Location : constant GPS.Editors.Editor_Location'Class :=
              GPS.LSP_Client.Utilities.LSP_Position_To_Location
                (Holder.Editor, Symbol.selectionRange.first);
            Last_Location : constant GPS.Editors.Editor_Location'Class :=
              GPS.LSP_Client.Utilities.LSP_Position_To_Location
                (Holder.Editor, Symbol.selectionRange.last);

            Visible   : Boolean;
            Cur_Depth : Integer;
         begin
            Trace (Me_Debug, "On_Idle_Load_Tree Outline_View.Add_Row");
            Outline_View.Add_Row
              (Self           => Self.Model,
               Name           => Symbol.name,
               Profile        => Get_Optional_String (Symbol.detail),
               Category       =>
                 To_Language_Category
                   (Symbol.kind,
                    Get_Optional_Boolean (Symbol.alsIsAdaProcedure)),
               Is_Declaration =>
                 Get_Optional_Boolean (Symbol.alsIsDeclaration),
               Visibility     =>
                 Get_Optional_Visibility (Symbol.alsVisibility),
               Def_Line       => First_Location.Line,
               Def_Col        => First_Location.Column,
               Def_End_Line   => Last_Location.Line,
               Def_End_Col    => Last_Location.Column,
               End_Line       => Integer (Symbol.span.last.line + 1),
               Id             => "",
               Visible        => Visible);

            Nb_Added_Rows := Nb_Added_Rows + 1;

            Trace
              (Me_Debug,
               "On_Idle_Load_Tree Nb_Added_Rows:" & Nb_Added_Rows'Img);

            Self.Tree_Cursor := Tree_Iter.Next (Self.Tree_Cursor);
            Cur_Depth := Integer (Depth (Self.Tree_Cursor));

            Trace
              (Me_Debug,
               "On_Idle_Load_Tree set visibility, Cur_Depth:" &
                 Cur_Depth'Img & " Prev_Depth:" & Prev_Depth'Img);
            if Visible then
               for I in Cur_Depth .. Prev_Depth loop
                  Outline_View.Move_Cursor (Self.Model, Outline_View.Up);
               end loop;
            end if;
            Trace (Me_Debug, "On_Idle_Load_Tree visibility is set");
            Prev_Depth := Cur_Depth;
         end;

         if Nb_Added_Rows = 100 then
            --  Stop here and restart later
            Trace (Me_Debug, "On_Idle_Load_Tree restart later");
            return True;
         end if;
      end loop;

      Free_Idle (Self, False);
      Trace (Me_Debug, "On_Idle_Load_Tree done");
      return False;
   end On_Idle_Load_Tree;

   -------------------------
   -- On_Idle_Load_Vector --
   -------------------------

   function On_Idle_Load_Vector
     (Self : Outline_LSP_Provider_Access) return Boolean
   is
      use SymbolInformation_Vectors.Element_Vectors;
      use type Basic_Types.Visible_Column_Type;
      Dummy         : Boolean;
      Nb_Added_Rows : Integer := 0;
      Holder : constant GPS.Editors.Controlled_Editor_Buffer_Holder :=
        Self.Kernel.Get_Buffer_Factory.Get_Holder (File => Self.File);

   begin
      Trace (Me_Debug, "On_Idle_Load_Vector");
      while Self.Vector_Cursor /= No_Element loop
         declare
            Symbol   : constant SymbolInformation :=
              Self.Result.Vector.Reference (Self.Vector_Cursor);
            Location : constant GPS.Editors.Editor_Location'Class :=
              GPS.LSP_Client.Utilities.LSP_Position_To_Location
                (Holder.Editor, Symbol.location.span.first);

         begin
            Outline_View.Add_Row
              (Self           => Self.Model,
               Name           => Symbol.name,
               Profile        => "",
               Category       => To_Language_Category (Symbol.kind),
               Is_Declaration => False,
               Visibility     => Visibility_Public,
               Def_Line       => Location.Line,
               Def_Col        => Location.Column,
               Def_End_Line   => -1,
               Def_End_Col    => -1,
               End_Line       =>
                 Integer (Symbol.location.span.last.line + 1),
               Id             => "",
               Visible        => Dummy);
            Outline_View.Move_Cursor (Self.Model, Outline_View.Up);
         end;

         Nb_Added_Rows := Nb_Added_Rows + 1;
         Next (Self.Vector_Cursor);
         if Nb_Added_Rows = 100 then
            --  Stop here and restart later
            Trace (Me_Debug, "On_Idle_Load_Vector restart later");
            return True;
         end if;
      end loop;

      Free_Idle (Self, False);
      Trace (Me_Debug, "On_Idle_Load_Vector done");
      return False;
   end On_Idle_Load_Vector;

   ---------------
   -- Free_Idle --
   ---------------

   procedure Free_Idle
     (Self    : Outline_LSP_Provider_Access;
      Stopped : Boolean) is
   begin
      Trace (Me_Debug, "Free_Idle");
      if Self.Model /= null then
         Trace (Me_Debug, "Free_Idle free model");
         Outline_View.Free (Self.Model);
      end if;

      Self.Loader_Id := No_Source_Id;
      if Stopped then
         Outline_View.Finished_Computing
           (Self.Kernel, Status => Outline_View.Stopped);
      else
         Outline_View.Finished_Computing
           (Self.Kernel, Status => Outline_View.Succeeded);
      end if;
      Trace (Me_Debug, "Free_Idle done");
   end Free_Idle;

   -------------------------
   -- Get_Optional_String --
   -------------------------

   function Get_Optional_String (S : Optional_String) return String is
   begin
      if S.Is_Set then
         return To_UTF_8_String (S.Value);
      else
         return "";
      end if;
   end Get_Optional_String;

   --------------------------
   -- Get_Optional_Boolean --
   --------------------------

   function Get_Optional_Boolean (B : Optional_Boolean) return Boolean is
   begin
      if B.Is_Set then
         return B.Value;
      else
         return False;
      end if;
   end Get_Optional_Boolean;

   -----------------------------
   -- Get_Optional_Visibility --
   -----------------------------

   function Get_Optional_Visibility
     (V : Optional_Als_Visibility) return Construct_Visibility is
   begin
      if V.Is_Set then
         return To_Construct_Visibility (V.Value);
      else
         return Visibility_Public;
      end if;
   end Get_Optional_Visibility;

end GPS.LSP_Client.Outline;
