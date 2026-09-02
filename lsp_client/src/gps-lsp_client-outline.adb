------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2020-2026, AdaCore                  --
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

with Ada.Containers.Vectors;
with Ada.Unchecked_Deallocation;
with VSS.Strings.Conversions;

with VSS.Strings;

with Glib;            use Glib;
with Glib.Main;       use Glib.Main;
with GNATCOLL.Traces; use GNATCOLL.Traces;
with GNATCOLL.VFS;    use GNATCOLL.VFS;

with GPS.Editors;                              use GPS.Editors;
with GPS.LSP_Client.Requests;                  use GPS.LSP_Client.Requests;
with GPS.LSP_Client.Requests.Document_Symbols;
use GPS.LSP_Client.Requests.Document_Symbols;
with GPS.LSP_Client.Language_Servers;
use GPS.LSP_Client.Language_Servers;
with GPS.LSP_Client.Utilities;                 use GPS.LSP_Client.Utilities;
with GPS.LSP_Module;                           use GPS.LSP_Module;

with Basic_Types;
with Language;         use Language;
with LSP.Enumerations; use LSP.Enumerations;
with LSP.Messages;
with LSP.Structures;   use LSP.Structures;
with Outline_View;     use Outline_View;

package body GPS.LSP_Client.Outline is

   Me             : constant Trace_Handle :=
     Create ("GPS.LSP.OUTLINE.ADVANCED", Off);
   Me_Debug       : constant Trace_Handle :=
     Create ("GPS.LSP.OUTLINE.DEBUG", Off);
   Me_Active      : constant Trace_Handle := Create ("GPS.LSP.OUTLINE", On);
   Me_Use_Timeout : constant Trace_Handle :=
     Create ("GPS.LSP.OUTLINE.USE_TIMEOUT", Off);

   ----------------------
   -- Outline Provider --
   ----------------------

   type Result_Access is access LSP.Structures.DocumentSymbol_Result;

   procedure Free is new
     Ada.Unchecked_Deallocation
       (LSP.Structures.DocumentSymbol_Result,
        Result_Access);

   --  3.17's DocumentSymbol_Vector represents the hierarchical result as a
   --  vector of DocumentSymbol, each carrying its own nested "children"
   --  vector, rather than the multiway-tree-with-cursor used by 3.16. We
   --  walk it depth-first using an explicit stack of (vector, next index)
   --  frames so that we can resume the walk across idle callbacks exactly
   --  as the previous cursor-based code did. Each frame holds its own COPY
   --  of the sibling vector (DocumentSymbol_Vector copies are cheap deep
   --  copies via its controlled Adjust) rather than a pointer into the
   --  parent, since taking 'Access of a vector element/component is not
   --  legal here (the public indexing view is not an aliased view).

   type Stack_Frame is record
      Vec   : LSP.Structures.DocumentSymbol_Vector;
      Index : Positive := 1;
   end record;

   package Stack_Vectors is new Ada.Containers.Vectors (Positive, Stack_Frame);

   type Outline_LSP_Provider is new Outline_View.Outline_Provider with record
      Kernel       : Kernel_Handle;
      File         : Virtual_File := No_File;
      Model        : Outline_Model_Access := null;
      Loader_Id    : Glib.Main.G_Source_Id := No_Source_Id;
      Tree_Stack   : Stack_Vectors.Vector;
      Vector_Index : Positive := 1;
      Result       : Result_Access := null;
   end record;
   type Outline_LSP_Provider_Access is access all Outline_LSP_Provider;

   overriding
   procedure Start_Fill
     (Self : access Outline_LSP_Provider; File : Virtual_File);

   overriding
   procedure Stop_Fill (Self : access Outline_LSP_Provider);
   --  Stop the async_load if necessary and clean the Outline model

   overriding
   function Support_Language
     (Self : access Outline_LSP_Provider; Lang : Language_Access)
      return Boolean;

   overriding
   function Get_Last_Result
     (Self : access Outline_LSP_Provider; File : Virtual_File)
      return LSP.Messages.Symbol_Vector;
   --  Outline_View.Outline_Provider (a separate, out-of-scope project) still
   --  declares this abstract function in terms of the 3.16
   --  LSP.Messages.Symbol_Vector type. Rather than reintroduce a real 3.16
   --  dependency here to build a faithful conversion, this always returns
   --  the "no result" value: it disables the cross-feature reuse
   --  optimization documented on Outline_Provider.Get_Last_Result (used by
   --  GPS.LSP_Client.Search.Entities to skip a redundant documentSymbol
   --  request), which now always re-queries instead. Properly restoring the
   --  optimization requires migrating Outline_View itself (and
   --  code_analysis's Outline_Provider implementation) to LSP.Structures.

   -----------------
   -- LSP Request --
   -----------------

   type GPS_LSP_Outline_Request is new Document_Symbols_Request with record
      Provider : Outline_LSP_Provider_Access;

      Close_Document_On_Finish : Boolean := False;
      --  Set to True if we should send a didClose notification to the server
      --  after finishing the request.
      --  This is needed when there is no opened editor for the file queried by
      --  the Outline: in that case, we should open the document on the server
      --  side via a didOpen notification before sending the
      --  textDocument/documentSymbols request and then close the document
      --  once the request finished.
   end record;
   type GPS_LSP_Outline_Request_Access is
     access all GPS_LSP_Outline_Request'Class;

   overriding
   procedure On_Result_Message
     (Self   : in out GPS_LSP_Outline_Request;
      Result : LSP.Structures.DocumentSymbol_Result);

   overriding
   procedure On_Error_Message
     (Self    : in out GPS_LSP_Outline_Request;
      Code    : LSP.Enumerations.ErrorCodes;
      Message : VSS.Strings.Virtual_String);

   overriding
   function Auto_Cancel
     (Self : in out GPS_LSP_Outline_Request; Next_Request : Request_Access)
      return Boolean;

   overriding
   procedure On_Rejected
     (Self : in out GPS_LSP_Outline_Request; Reason : Reject_Reason);

   function Get_Optional_Boolean (B : Boolean_Optional) return Boolean;

   function Get_Optional_Visibility
     (V : AlsVisibility_Optional) return Construct_Visibility;

   -----------------
   -- Auto_Cancel --
   -----------------

   overriding
   function Auto_Cancel
     (Self : in out GPS_LSP_Outline_Request; Next_Request : Request_Access)
      return Boolean is
   begin
      if Next_Request /= null
        and then Next_Request.all in GPS_LSP_Outline_Request'Class
      then
         return Self.File = GPS_LSP_Outline_Request_Access (Next_Request).File;
      else
         return False;
      end if;
   end Auto_Cancel;

   ----------------
   -- Async Load --
   ----------------

   package Async_Load is new
     Glib.Main.Generic_Sources (Outline_LSP_Provider_Access);
   function On_Idle_Load_Tree
     (Self : Outline_LSP_Provider_Access) return Boolean;
   function On_Idle_Load_Vector
     (Self : Outline_LSP_Provider_Access) return Boolean;
   procedure Free_Idle (Self : Outline_LSP_Provider_Access; Stopped : Boolean);

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding
   procedure On_Result_Message
     (Self   : in out GPS_LSP_Outline_Request;
      Result : LSP.Structures.DocumentSymbol_Result)
   is
      Lang   : constant Language_Access :=
        Self.Kernel.Get_Language_Handler.Get_Language_From_File (Self.File);
      Server : constant Language_Server_Access := Get_Language_Server (Lang);

      procedure Register (Callback : Async_Load.G_Source_Func);

      procedure Register (Callback : Async_Load.G_Source_Func) is
      begin
         if Me_Use_Timeout.Active then
            Self.Provider.Loader_Id :=
              Async_Load.Timeout_Add (100, Callback, Self.Provider);

         else
            Self.Provider.Loader_Id :=
              Async_Load.Idle_Add (Callback, Self.Provider);
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

      Free (Self.Provider.Result);
      Self.Provider.Result :=
        new LSP.Structures.DocumentSymbol_Result'(Result);

      case Result.Kind is
         when Variant_2 =>
            Trace (Me_Debug, "Process result tree");
            Self.Provider.Tree_Stack.Clear;
            if Self.Provider.Result.Variant_2.Length > 0 then
               Self.Provider.Tree_Stack.Append
                 (Stack_Frame'
                    (Vec => Self.Provider.Result.Variant_2, Index => 1));
            end if;
            Register (On_Idle_Load_Tree'Access);

         when others    =>
            Trace (Me_Debug, "Process result vector");
            Self.Provider.Vector_Index := 1;
            Register (On_Idle_Load_Vector'Access);
      end case;
      Trace (Me_Debug, "On_Result_Message done");
   end On_Result_Message;

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding
   procedure On_Error_Message
     (Self    : in out GPS_LSP_Outline_Request;
      Code    : LSP.Enumerations.ErrorCodes;
      Message : VSS.Strings.Virtual_String)
   is
      Lang   : constant Language_Access :=
        Self.Kernel.Get_Language_Handler.Get_Language_From_File (Self.File);
      Server : constant Language_Server_Access := Get_Language_Server (Lang);
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

   overriding
   procedure On_Rejected
     (Self : in out GPS_LSP_Outline_Request; Reason : Reject_Reason) is
   begin
      Trace
        (Me,
         VSS.Strings.Conversions.To_UTF_8_String (Self.Method)
         & " has been rejected");
      if Reason = Server_Not_Ready then
         Outline_View.Finished_Computing
           (Self.Provider.Kernel, Status => Outline_View.Failed);
      else
         Outline_View.Finished_Computing
           (Self.Provider.Kernel, Status => Outline_View.Stopped);
      end if;
      Trace (Me_Debug, "On_Rejected done");
   end On_Rejected;

   ----------------
   -- Start_Fill --
   ----------------

   overriding
   procedure Start_Fill
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
                (File => File, Open_Buffer => True, Open_View => False);
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
             Kernel                   => Self.Kernel,
             others                   => <>);

      GPS.LSP_Client.Requests.Execute
        (Self.Kernel.Get_Language_Handler.Get_Language_From_File (File),
         Request_Access (R));
      Trace (Me_Debug, "Start_Fill done");
   end Start_Fill;

   ---------------
   -- Stop_Fill --
   ---------------

   overriding
   procedure Stop_Fill (Self : access Outline_LSP_Provider) is
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

   overriding
   function Support_Language
     (Self : access Outline_LSP_Provider; Lang : Language_Access)
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
      Nb_Added_Rows : Integer := 0;
      Holder        : constant GPS.Editors.Controlled_Editor_Buffer_Holder :=
        Self.Kernel.Get_Buffer_Factory.Get_Holder (File => Self.File);
   begin
      Trace (Me_Debug, "On_Idle_Load_Tree");

      while not Self.Tree_Stack.Is_Empty loop
         declare
            Top : Stack_Frame := Self.Tree_Stack.Last_Element;
         begin
            if Top.Index > Top.Vec.Length then
               --  Finished this vector: pop back to the parent level.
               Self.Tree_Stack.Delete_Last;
               if not Self.Tree_Stack.Is_Empty then
                  Outline_View.Move_Cursor (Self.Model, Outline_View.Up);
               end if;

            else
               declare
                  Symbol         : constant DocumentSymbol :=
                    Top.Vec (Top.Index);
                  First_Location :
                    constant GPS.Editors.Editor_Location'Class :=
                      GPS.LSP_Client.Utilities.LSP_Position_To_Location
                        (Holder.Editor, Symbol.selectionRange.start);
                  Last_Location  :
                    constant GPS.Editors.Editor_Location'Class :=
                      GPS.LSP_Client.Utilities.LSP_Position_To_Location
                        (Holder.Editor, Symbol.selectionRange.an_end);

                  Visible : Boolean;

               begin
                  --  Advance to the next sibling before we possibly descend
                  --  into this symbol's children.
                  Top.Index := Top.Index + 1;
                  Self.Tree_Stack.Replace_Element (Self.Tree_Stack.Last, Top);

                  Trace (Me_Debug, "On_Idle_Load_Tree Outline_View.Add_Row");
                  Outline_View.Add_Row
                    (Self           => Self.Model,
                     Name           => Symbol.name,
                     Profile        =>
                       (if not Symbol.detail.Is_Empty
                        then
                          VSS.Strings.Conversions.To_UTF_8_String
                            (Symbol.detail)
                        else ""),
                     Category       =>
                       To_Language_Category
                         (Symbol.kind,
                          Get_Optional_Boolean (Symbol.alsIsAdaProcedure)),
                     Is_Declaration =>
                       Get_Optional_Boolean (Symbol.alsIsDeclaration),
                     Visibility     =>
                       Get_Optional_Visibility (Symbol.alsVisibility),
                     Start_Line     => Integer (Symbol.a_range.start.line + 1),
                     Def_Line       => First_Location.Line,
                     Def_Col        => First_Location.Column,
                     Def_End_Line   => Last_Location.Line,
                     Def_End_Col    => Last_Location.Column,
                     End_Line       =>
                       Integer (Symbol.a_range.an_end.line + 1),
                     Id             => "",
                     Visible        => Visible);

                  Nb_Added_Rows := Nb_Added_Rows + 1;

                  Trace
                    (Me_Debug,
                     "On_Idle_Load_Tree Nb_Added_Rows:" & Nb_Added_Rows'Img);

                  if Visible and then Symbol.children.Length > 0 then
                     --  Descend: Add_Row already advanced the cursor to
                     --  this row, which is exactly the parent the first
                     --  child needs.
                     Self.Tree_Stack.Append
                       (Stack_Frame'(Vec => Symbol.children, Index => 1));
                  elsif Visible then
                     --  No children to descend into: Add_Row still
                     --  advanced the cursor to this row, so pop back to
                     --  its parent now or the next sibling would be
                     --  nested underneath it instead of alongside it.
                     Outline_View.Move_Cursor (Self.Model, Outline_View.Up);
                  end if;
                  --  else: the symbol was filtered out, so Add_Row left
                  --  the cursor untouched at the real parent. Moving it
                  --  up here would incorrectly pop past that parent and
                  --  cause the next sibling to be added one level too
                  --  high (or at the top level).
               end;
            end if;
         end;

         if Nb_Added_Rows >= 100 then
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
      use type Basic_Types.Visible_Column_Type;
      Dummy         : Boolean;
      Nb_Added_Rows : Integer := 0;
      Holder        : constant GPS.Editors.Controlled_Editor_Buffer_Holder :=
        Self.Kernel.Get_Buffer_Factory.Get_Holder (File => Self.File);

   begin
      Trace (Me_Debug, "On_Idle_Load_Vector");
      while Self.Vector_Index <= Natural (Self.Result.Variant_1.Length) loop
         declare
            Symbol   : constant SymbolInformation :=
              Self.Result.Variant_1 (Self.Vector_Index);
            Location : constant GPS.Editors.Editor_Location'Class :=
              GPS.LSP_Client.Utilities.LSP_Position_To_Location
                (Holder.Editor, Symbol.location.a_range.start);

         begin
            Outline_View.Add_Row
              (Self           => Self.Model,
               Name           => Symbol.name,
               Profile        => "",
               Category       => To_Language_Category (Symbol.kind),
               Is_Declaration => False,
               Visibility     => Visibility_Public,
               Start_Line     =>
                 Integer (Symbol.location.a_range.start.line + 1),
               Def_Line       => Location.Line,
               Def_Col        => Location.Column,
               Def_End_Line   => -1,
               Def_End_Col    => -1,
               End_Line       =>
                 Integer (Symbol.location.a_range.an_end.line + 1),
               Id             => "",
               Visible        => Dummy);
            Outline_View.Move_Cursor (Self.Model, Outline_View.Up);
         end;

         Nb_Added_Rows := Nb_Added_Rows + 1;
         Self.Vector_Index := Self.Vector_Index + 1;
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

   procedure Free_Idle (Self : Outline_LSP_Provider_Access; Stopped : Boolean)
   is
   begin
      Self.Loader_Id := No_Source_Id;

      Trace (Me_Debug, "Free_Idle");
      if Self.Model /= null then
         Trace (Me_Debug, "Free_Idle free model");
         if Stopped then
            --  Clear the model, we stop to restart
            Outline_View.Clear_Outline_Model (Self.Model);
         end if;
         Outline_View.Free (Self.Model);
      end if;

      if Stopped then
         Outline_View.Finished_Computing
           (Self.Kernel, Status => Outline_View.Stopped);
      else
         Outline_View.Finished_Computing
           (Self.Kernel, Status => Outline_View.Succeeded);
      end if;

      Trace (Me_Debug, "Free_Idle done");
   end Free_Idle;

   ---------------------
   -- Get_Last_Result --
   ---------------------

   overriding
   function Get_Last_Result
     (Self : access Outline_LSP_Provider; File : Virtual_File)
      return LSP.Messages.Symbol_Vector
   is
      pragma Unreferenced (Self, File);
   begin
      return (Is_Tree => False, Vector => <>);
   end Get_Last_Result;

   --------------------------
   -- Get_Optional_Boolean --
   --------------------------

   function Get_Optional_Boolean (B : Boolean_Optional) return Boolean is
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
     (V : AlsVisibility_Optional) return Construct_Visibility is
   begin
      if V.Is_Set then
         return To_Construct_Visibility (V.Value);
      else
         return Visibility_Public;
      end if;
   end Get_Optional_Visibility;

end GPS.LSP_Client.Outline;
