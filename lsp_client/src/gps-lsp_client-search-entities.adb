------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2020-2026, AdaCore                     --
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

with Ada.Characters.Handling;
with Ada.Containers.Vectors;
with Ada.Strings.UTF_Encoding;

with GNAT.OS_Lib;                     use GNAT.OS_Lib;
with GNAT.Strings;

with GNATCOLL.JSON;
with GNATCOLL.Projects;               use GNATCOLL.Projects;
with GNATCOLL.Traces;                 use GNATCOLL.Traces;
with GNATCOLL.Utils;
with GNATCOLL.VFS;                    use GNATCOLL.VFS;
with GNATCOLL.Xref;

with VSS.Strings;                    use VSS.Strings;
with VSS.Strings.Conversions;

with Gtk.Widget;

with GPS.Editors;
with GPS.Kernel.Hooks;
with GPS.Kernel.Preferences;
with GPS.Kernel.Search;               use GPS.Kernel.Search;
with GPS.Search;                      use GPS.Search;
with GPS.Search.GUI;                  use GPS.Search.GUI;

with Language;
with Language_Handlers;
with Completion.Search;

with LSP.Types;                       use LSP.Types;
with LSP.Messages;                    use LSP.Messages;
with GPS.LSP_Module;
with GPS.LSP_Client.Utilities;

with GPS.LSP_Client.Requests;           use GPS.LSP_Client.Requests;
with GPS.LSP_Client.Requests.Document_Symbols;
with GPS.LSP_Client.Requests.Symbols;
with GPS.LSP_Client.Editors.Tooltips;

with Outline_View; use Outline_View;

package body GPS.LSP_Client.Search.Entities is

   Me_Search_Entities_Support : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create
       ("GPS.LSP.SEARCH_ENTITIES_SUPPORT", GNATCOLL.Traces.On);

   Me : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create
       ("GPS.LSP.SEARCH_ENTITIES_SUPPORT.ADVANCED", GNATCOLL.Traces.On);
   --  For logging

   package Reference_Vectors is new Ada.Containers.Vectors
     (Positive, GPS.LSP_Client.Requests.Reference,
      "=" => GPS.LSP_Client.Requests."=");

   ------------------------------
   -- Entities_Search_Provider --
   ------------------------------

   type Entities_Search_Provider is new Kernel_Search_Provider with record
      Pattern     : Search_Pattern_Access; --  Do not free
      Request_Num : Integer := 0;
      References  : Reference_Vectors.Vector;

      Results     : LSP.Messages.SymbolInformation_Vector;
      --  Results received from all the servers

      Waiting     : Integer := 0;
      --  We are waiting for response when not 0

      Position    : Integer := 1;
      --  The current element to process in the received results
   end record;

   type Entities_Search_Provider_Access is access all Entities_Search_Provider;

   overriding function Documentation
     (Self    : not null access Entities_Search_Provider) return String;
   overriding procedure Set_Pattern
     (Self    : not null access Entities_Search_Provider;
      Pattern : not null access GPS.Search.Search_Pattern'Class;
      Limit   : Natural := Natural'Last);
   overriding procedure Next
     (Self     : not null access Entities_Search_Provider;
      Result   : out GPS.Search.Search_Result_Access;
      Has_Next : out Boolean);
   overriding function Display_Name
     (Self     : not null access Entities_Search_Provider) return String
   is
     (Provider_Entities);
   overriding function Is_Result_Ready
     (Self : not null access Entities_Search_Provider) return Boolean;

   procedure On_Response
     (Self : not null access Entities_Search_Provider'Class;
      Num  : Integer);

   procedure Send_Request
     (Self : not null access Entities_Search_Provider'Class);
   --  Send requests to LSP servers

   -------------------------------------------
   -- Current_File_Entities_Search_Provider --
   -------------------------------------------

   type Current_File_Entities_Search_Provider is
     new Kernel_Search_Provider with record
      Pattern       : Search_Pattern_Access; --  Do not free
      --  Pattern to search
      File          : Virtual_File;
      --  File that we are interested
      Request       : GPS.LSP_Client.Requests.Reference;
      --  Reference for the sent request if any
      Request_Num   : Integer := 0;
      --  Number of the current request
      Waiting       : Boolean := False;
      --  Are we waiting for the response?
      Result        : LSP.Messages.Symbol_Vector;
      --  For holding response information
      --  Cursors in the result for stepping over it
      Tree_Cursor   : DocumentSymbol_Trees.Cursor :=
        DocumentSymbol_Trees.No_Element;
      Vector_Cursor : SymbolInformation_Vectors.Element_Vectors.Cursor :=
        SymbolInformation_Vectors.Element_Vectors.No_Element;
   end record;

   type Current_File_Entities_Search_Provider_Access is
     access all Current_File_Entities_Search_Provider;

   overriding function Display_Name
     (Self : not null access Current_File_Entities_Search_Provider)
      return String
   is
     (Provider_Entities & " for current file");
   overriding function Documentation
     (Self : not null access Current_File_Entities_Search_Provider)
      return String;
   overriding procedure Set_Pattern
     (Self    : not null access Current_File_Entities_Search_Provider;
      Pattern : not null access GPS.Search.Search_Pattern'Class;
      Limit   : Natural := Natural'Last);
   overriding procedure Next
     (Self     : not null access Current_File_Entities_Search_Provider;
      Result   : out GPS.Search.Search_Result_Access;
      Has_Next : out Boolean);
   overriding function Is_Result_Ready
     (Self : not null access Current_File_Entities_Search_Provider)
      return Boolean;
   procedure On_Response
     (Self : not null access Current_File_Entities_Search_Provider'Class;
      Num  : Integer);

   --------------------------
   -- Entity_Search_Result --
   --------------------------

   type Entity_Search_Result is new Kernel_Search_Result with record
      Position : LSP.Messages.Position;
      File     : Virtual_File;
      Line     : Integer := 0;
      Column   : GNATCOLL.Xref.Visible_Column := 0;
   end record;
   procedure Adjust_Location
     (Self : not null access Entity_Search_Result'Class);
   --  Converts LSP position to the buffer's position. Opening a buffer
   --  for a file takes a while so we do this only when it is really needed.

   overriding procedure Execute
     (Self       : not null access Entity_Search_Result;
      Give_Focus : Boolean);
   overriding function Full
     (Self : not null access Entity_Search_Result)
      return Gtk.Widget.Gtk_Widget;

   --------------------
   -- Symbol_Request --
   --------------------

   type Symbol_Request is
     new GPS.LSP_Client.Requests.Symbols.Abstract_Symbol_Request
   with record
      Provider : Entities_Search_Provider_Access;
      Num      : Integer := 0;
   end record;
   overriding procedure On_Partial_Result_Message
     (Self   : in out Symbol_Request;
      Result : LSP.Messages.SymbolInformation_Vector);
   overriding procedure On_Result_Message
     (Self   : in out Symbol_Request;
      Result : LSP.Messages.SymbolInformation_Vector);
   overriding procedure On_Error_Message
     (Self    : in out Symbol_Request;
      Code    : LSP.Messages.ErrorCodes;
      Message : VSS.Strings.Virtual_String;
      Data    : GNATCOLL.JSON.JSON_Value);
   overriding procedure On_Rejected
     (Self : in out Symbol_Request; Reason : Reject_Reason);

   ----------------------
   -- Document_Request --
   ----------------------

   type Document_Request is
     new GPS.LSP_Client.Requests.Document_Symbols.
       Document_Symbols_Request
   with record
      Provider : Current_File_Entities_Search_Provider_Access;
      Num      : Integer := 0;
   end record;
   overriding procedure On_Result_Message
     (Self   : in out Document_Request;
      Result : LSP.Messages.Symbol_Vector);
   overriding procedure On_Error_Message
     (Self    : in out Document_Request;
      Code    : LSP.Messages.ErrorCodes;
      Message : VSS.Strings.Virtual_String;
      Data    : GNATCOLL.JSON.JSON_Value);
   overriding procedure On_Rejected
     (Self : in out Document_Request; Reason : Reject_Reason);
   overriding function Auto_Cancel
     (Self         : in out Document_Request;
      Next_Request : Request_Access) return Boolean is (True);

   -------------------
   -- Documentation --
   -------------------

   overriding function Documentation
     (Self : not null access Entities_Search_Provider) return String
   is
      pragma Unreferenced (Self);
   begin
      return "Searches amongst entities defined in the project.";
   end Documentation;

   -------------------
   -- Documentation --
   -------------------

   overriding function Documentation
     (Self : not null access Current_File_Entities_Search_Provider)
      return String
   is
      pragma Unreferenced (Self);
   begin
      return "Searches amongst entities defined in the current file.";
   end Documentation;

   ---------------------
   -- Adjust_Location --
   ---------------------

   procedure Adjust_Location
     (Self : not null access Entity_Search_Result'Class) is
   begin
      if Self.Line /= 0 then
         return;
      end if;

      declare
         Holder  : constant GPS.Editors.
           Controlled_Editor_Buffer_Holder :=
             Self.Kernel.Get_Buffer_Factory.Get_Holder
               (File => Self.File);
         Location : constant GPS.Editors.Editor_Location'Class :=
           GPS.LSP_Client.Utilities.LSP_Position_To_Location
             (Holder.Editor, Self.Position);

      begin
         Self.Line   := Location.Line;
         Self.Column := Location.Column;
      end;
   end Adjust_Location;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self       : not null access Entity_Search_Result;
      Give_Focus : Boolean) is
   begin
      Self.Adjust_Location;

      GPS.Kernel.Hooks.Open_File_Action_Hook.Run
        (Self.Kernel,
         File    => Self.File,
         Project => GNATCOLL.Projects.No_Project,   --  ??? unknown
         Line    => Self.Line,
         Column  => Self.Column,
         Focus   => Give_Focus);
   end Execute;

   ----------
   -- Full --
   ----------

   overriding function Full
     (Self : not null access Entity_Search_Result)
      return Gtk.Widget.Gtk_Widget is
   begin
      Self.Adjust_Location;

      return GPS.LSP_Client.Editors.Tooltips.Query_Tooltip_For_Entity
        (Kernel              => Self.Kernel,
         File                => Self.File,
         Line                => Self.Line,
         Column              => Self.Column,
         For_Global_Tooltips => False,
         Xalign              => 0.0,
         Yalign              => 0.0,
         Font                =>
           GPS.Kernel.Preferences.View_Fixed_Font.Get_Pref,
         Separator_Expand => False,
         Separator_Padding => 2);
   end Full;

   ---------------------
   -- Is_Result_Ready --
   ---------------------

   overriding function Is_Result_Ready
     (Self : not null access Entities_Search_Provider) return Boolean is
   begin
      return Self.Waiting = 0;
   end Is_Result_Ready;

   ---------------------
   -- Is_Result_Ready --
   ---------------------

   overriding function Is_Result_Ready
     (Self : not null access Current_File_Entities_Search_Provider)
      return Boolean is
   begin
      return not Self.Waiting;
   end Is_Result_Ready;

   ----------
   -- Next --
   ----------

   overriding procedure Next
     (Self     : not null access Entities_Search_Provider;
      Result   : out GPS.Search.Search_Result_Access;
      Has_Next : out Boolean)
   is
      Context  : GPS.Search.Search_Context;
      Long     : GNAT.Strings.String_Access;
      File     : Virtual_File;
      Info     : LSP.Messages.SymbolInformation;
   begin
      Result   := null;
      Has_Next := True;

      if Self.Waiting /= 0 then
         return;
      end if;

      Context.Score := 100;
      --  default value when Highlights is not allowed

      if Self.Position <= Integer (Self.Results.Length) then
         Info := Self.Results.Element (Self.Position);
         File := GPS.LSP_Client.Utilities.To_Virtual_File (Info.location.uri);
         Self.Position := Self.Position + 1;

         declare
            Short : constant Ada.Strings.UTF_Encoding.UTF_8_String :=
              VSS.Strings.Conversions.To_UTF_8_String (Info.name);
         begin
            if Self.Pattern.Get_Allow_Highlights then
               Context := Self.Pattern.Start
                 (Short,
                  Tab_Width =>
                   Self.Kernel.Get_Language_Handler.Get_Language_From_File
                     (File).Get_Indentation_Level);
            end if;

            if not Self.Pattern.Get_Allow_Highlights
              or else Context /= GPS.Search.No_Match
            then
               Long := new String'
                 (File.Display_Base_Name
                  & ":" & GNATCOLL.Utils.Image
                    (Integer (Info.location.span.first.line) + 1,
                     Min_Width => 0)
                  & ":"
                  & GNATCOLL.Utils.Image
                    (Integer (Info.location.span.first.character) + 1,
                     Min_Width => 0));

               Result := new Entity_Search_Result'
                 (Kernel   => Self.Kernel,
                  Provider => Self,
                  Score    => Context.Score,
                  Short    =>
                    (if Self.Pattern.Get_Allow_Highlights
                     then new String'
                       (Self.Pattern.Highlight_Match
                            (Short, Context => Context))
                     else new String'(Short)),
                  Long     => Long,
                  Id       =>
                    VSS.Strings.Conversions.To_Virtual_String
                      (Short & ":" & Long.all),
                  Position => Info.location.span.first,
                  File     => File,
                  others   => <>);

               --  Matches in runtime files should get a lower score, so
               --  that we first list those matches in user code. "10" is
               --  similar to what is done for filenames, so that in fuzzy
               --  matching this correpsonds to having characters separated
               --  by 9 others.

               declare
                  use GNATCOLL.Projects;

                  Inf : constant File_Info'Class :=
                    File_Info'Class
                      (Get_Project_Tree (Self.Kernel.all).Info_Set
                       (File).First_Element);
               begin
                  if Inf.Project
                    (Root_If_Not_Found => False) = No_Project
                  then
                     Result.Score := Result.Score - 10;
                  end if;
               end;

               Self.Adjust_Score (Result);
            end if;
         end;

      else
         --  we processed all response's records
         Has_Next := False;
      end if;
   end Next;

   ----------
   -- Next --
   ----------

   overriding procedure Next
     (Self     : not null access Current_File_Entities_Search_Provider;
      Result   : out GPS.Search.Search_Result_Access;
      Has_Next : out Boolean)
   is
      use DocumentSymbol_Trees;
      use SymbolInformation_Vectors.Element_Vectors;

      Context  : GPS.Search.Search_Context;
      Long     : GNAT.Strings.String_Access;

   begin
      Result   := null;
      Has_Next := True;

      if Self.Waiting then
         return;
      end if;

      Context.Score := 100;
      --  default value when Highlights is not allowed

      if Self.Tree_Cursor /= DocumentSymbol_Trees.No_Element then
         declare
            Tree_Iter : Tree_Iterator_Interfaces.Forward_Iterator'Class :=
              Iterate (Self.Result.Tree);
         begin
            if Is_Root (Self.Tree_Cursor) then
               Self.Tree_Cursor := Tree_Iter.Next (Self.Tree_Cursor);
            end if;

            declare
               Symbol : constant DocumentSymbol :=
                 Element (Self.Tree_Cursor);
               Short  : constant Ada.Strings.UTF_Encoding.UTF_8_String :=
                 VSS.Strings.Conversions.To_UTF_8_String (Symbol.name);
            begin
               if Self.Pattern.Get_Allow_Highlights then
                  Context := Self.Pattern.Start (Short);
               end if;

               if not Self.Pattern.Get_Allow_Highlights
                 or else Context /= GPS.Search.No_Match
               then
                  Long := new String'
                    (Self.File.Display_Base_Name
                     & ":" & GNATCOLL.Utils.Image
                       (Integer (Symbol.selectionRange.first.line) + 1,
                        Min_Width => 0)
                     & ":"
                     & GNATCOLL.Utils.Image
                       (Integer (Symbol.selectionRange.first.character) + 1,
                        Min_Width => 0));

                  Result := new Entity_Search_Result'
                    (Kernel   => Self.Kernel,
                     Provider => Self,
                     Score    => Context.Score,
                     Short    =>
                       (if Self.Pattern.Get_Allow_Highlights
                        then new String'
                          (Self.Pattern.Highlight_Match
                               (Short, Context => Context))
                        else new String'(Short)),
                     Long     => Long,
                     Id       =>
                       VSS.Strings.Conversions.To_Virtual_String
                         (Short & ":" & Long.all),
                     Position => Symbol.selectionRange.first,
                     File     => Self.File,
                     others   => <>);

                  Self.Adjust_Score (Result);
               end if;
            end;

            Self.Tree_Cursor := Tree_Iter.Next (Self.Tree_Cursor);
         end;

      elsif Self.Vector_Cursor /= SymbolInformation_Vectors.Element_Vectors.
        No_Element
      then
         declare
            Info  : constant SymbolInformation :=
              Self.Result.Vector.Reference (Self.Vector_Cursor);
            Short : constant Ada.Strings.UTF_Encoding.UTF_8_String :=
              VSS.Strings.Conversions.To_UTF_8_String (Info.name);
         begin
            Next (Self.Vector_Cursor);

            if Self.Pattern.Get_Allow_Highlights then
               Context := Self.Pattern.Start (Short);
            end if;

            if not Self.Pattern.Get_Allow_Highlights
              or else Context /= GPS.Search.No_Match
            then
               Long := new String'
                 (Self.File.Display_Base_Name
                  & ":" & GNATCOLL.Utils.Image
                    (Integer (Info.location.span.first.line) + 1,
                     Min_Width => 0)
                  & ":"
                  & GNATCOLL.Utils.Image
                    (Integer (Info.location.span.first.character) + 1,
                     Min_Width => 0));

               Result := new Entity_Search_Result'
                 (Kernel   => Self.Kernel,
                  Provider => Self,
                  Score    => Context.Score,
                  Short    =>
                    (if Self.Pattern.Get_Allow_Highlights
                     then new String'
                       (Self.Pattern.Highlight_Match
                            (Short, Context => Context))
                     else new String'(Short)),
                  Long     => Long,
                  Id       =>
                    VSS.Strings.Conversions.To_Virtual_String
                      (Short & ":" & Long.all),
                  Position => Info.location.span.first,
                  File     => Self.File,
                  others   => <>);

               Self.Adjust_Score (Result);
            end if;
         end;

      else
         --  we processed all response's records
         Has_Next := False;
      end if;
   end Next;

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out Symbol_Request;
      Code    : LSP.Messages.ErrorCodes;
      Message : VSS.Strings.Virtual_String;
      Data    : GNATCOLL.JSON.JSON_Value) is
   begin
      Self.Provider.On_Response (Self.Num);
   end On_Error_Message;

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out Document_Request;
      Code    : LSP.Messages.ErrorCodes;
      Message : VSS.Strings.Virtual_String;
      Data    : GNATCOLL.JSON.JSON_Value) is
   begin
      Self.Provider.On_Response (Self.Num);
   end On_Error_Message;

   -----------------
   -- On_Rejected --
   -----------------

   overriding procedure On_Rejected
     (Self : in out Symbol_Request; Reason : Reject_Reason)
   is
      pragma Unreferenced (Reason);
   begin
      Self.Provider.On_Response (Self.Num);
   end On_Rejected;

   -----------------
   -- On_Rejected --
   -----------------

   overriding procedure On_Rejected
     (Self : in out Document_Request; Reason : Reject_Reason)
   is
      pragma Unreferenced (Reason);
   begin
      Self.Provider.On_Response (Self.Num);
   end On_Rejected;

   -------------------------------
   -- On_Partial_Result_Message --
   -------------------------------

   overriding procedure On_Partial_Result_Message
     (Self   : in out Symbol_Request;
      Result : LSP.Messages.SymbolInformation_Vector) is
   begin
      if Self.Provider.Request_Num = Self.Num then
         Self.Provider.Results.Append (Result);
      end if;
   end On_Partial_Result_Message;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self   : in out Symbol_Request;
      Result : LSP.Messages.SymbolInformation_Vector) is
   begin
      Self.Provider.On_Response (Self.Num);

      if Self.Provider.Request_Num = Self.Num then
         Self.Provider.Results.Append (Result);
      end if;
   end On_Result_Message;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self   : in out Document_Request;
      Result : LSP.Messages.Symbol_Vector) is
   begin
      Self.Provider.On_Response (Self.Num);
      if Self.Provider.Request_Num = Self.Num then
         Self.Provider.Result := Result;
         if Result.Is_Tree then
            Self.Provider.Tree_Cursor := Self.Provider.Result.Tree.Root;
         else
            Self.Provider.Vector_Cursor := Self.Provider.Result.Vector.First;
         end if;
      end if;
   end On_Result_Message;

   -----------------
   -- On_Response --
   -----------------

   procedure On_Response
     (Self : not null access Entities_Search_Provider'Class;
      Num  : Integer) is
   begin
      if Self.Request_Num = Num then
         Self.Waiting := Self.Waiting - 1;

         if Self.Waiting = 0 then
            --  we have all responses, clear requests references because
            --  we do not need to cancel them anymore
            Self.References.Clear;
         end if;
      end if;
   end On_Response;

   -----------------
   -- On_Response --
   -----------------

   procedure On_Response
     (Self : not null access Current_File_Entities_Search_Provider'Class;
      Num  : Integer) is
   begin
      if Self.Request_Num = Num then
         Self.Waiting := False;
      end if;
   end On_Response;

   -----------------
   -- Set_Pattern --
   -----------------

   overriding procedure Set_Pattern
     (Self    : not null access Entities_Search_Provider;
      Pattern : not null access GPS.Search.Search_Pattern'Class;
      Limit   : Natural := Natural'Last)
   is
      pragma Unreferenced (Limit);
      Ref  : GPS.LSP_Client.Requests.Reference;
   begin
      if Self.Request_Num < Integer'Last then
         Self.Request_Num := Self.Request_Num + 1;
      else
         Self.Request_Num := 1;
      end if;

      --  cancel previous requests if any
      while not Self.References.Is_Empty loop
         Ref := Self.References.First_Element;
         Self.References.Delete_First;
         Ref.Cancel;
      end loop;

      Self.Results.Clear;

      Self.Position := 1;
      Self.Waiting  := 0;

      if Pattern.Get_Text = "" then
         return;
      end if;

      Self.Pattern := Search_Pattern_Access (Pattern);

      Self.Send_Request;
   end Set_Pattern;

   -----------------
   -- Set_Pattern --
   -----------------

   overriding procedure Set_Pattern
     (Self    : not null access Current_File_Entities_Search_Provider;
      Pattern : not null access GPS.Search.Search_Pattern'Class;
      Limit   : Natural := Natural'Last) is
   begin
      Self.File := Self.Kernel.Get_Buffer_Factory.Get
        (Open_Buffer => False,
         Open_View   => False).File;

      if Self.Request_Num < Integer'Last then
         Self.Request_Num := Self.Request_Num + 1;
      else
         Self.Request_Num := 1;
      end if;

      --  cancel previous request if any
      if Self.Waiting then
         Self.Request.Cancel;
      end if;

      Self.Result        := (Is_Tree => False, Vector  => <>);
      Self.Tree_Cursor   := DocumentSymbol_Trees.No_Element;
      Self.Vector_Cursor := SymbolInformation_Vectors.Element_Vectors.
        No_Element;
      Self.Waiting       := False;

      if Pattern.Get_Text = "" then
         return;
      end if;

      Self.Pattern := Search_Pattern_Access (Pattern);

      --  Try to reuse the outline view result
      if Outline_View.Get_LSP_Provider /= null then
         Self.Result :=
           Outline_View.Get_LSP_Provider.Get_Last_Result (Self.File);
      end if;

      if Self.Result /= (Is_Tree => False, Vector  => <>) then
         if Self.Result.Is_Tree then
            Self.Tree_Cursor := Self.Result.Tree.Root;
         else
            Self.Vector_Cursor := Self.Result.Vector.First;
         end if;

         Me.Trace ("Reuse the result from the outline view");
         return;
      end if;

      declare
         Lang : constant Language.Language_Access :=
           Self.Kernel.Get_Language_Handler.Get_Language_From_File (Self.File);
      begin
         if GPS.LSP_Module.LSP_Is_Enabled (Lang) then
            declare
               Request : GPS.LSP_Client.Requests.Request_Access;
            begin
               if Ada.Characters.Handling.To_Lower
                 (Lang.Get_Name) = "ada"
               then
                  Request := new Document_Request'
                    (GPS.LSP_Client.Requests.LSP_Request with
                     Provider        =>
                       Current_File_Entities_Search_Provider_Access (Self),
                     Num      => Self.Request_Num,
                     Kernel   => Self.Kernel,
                     File     => Self.File,
                     Query    =>
                       VSS.Strings.Conversions.To_Virtual_String
                         (Self.Pattern.Get_Text),
                     Case_Sensitive =>
                       (Is_Set => True,
                        Value  => Self.Pattern.Get_Case_Sensitive),
                     Whole_Word =>
                       (Is_Set => True,
                        Value  => Self.Pattern.Get_Whole_Word),
                     Negate =>
                       (Is_Set => True,
                        Value  => Self.Pattern.Get_Negate),
                     Kind =>
                       (Is_Set => True,
                        Value  => LSP.Messages.Search_Kind'Val
                          (GPS.Search.Search_Kind'Pos
                               (Self.Pattern.Get_Kind))));

               elsif Self.Pattern.Get_Kind = Full_Text
                 and then not Self.Pattern.Get_Negate
               then
                  --  Start only full text search on servers without
                  --  filtration. In other case the result may have
                  --  a huge ammount of records (all known entities)
                  --  and this will cause freeze of the search engine.

                  Request := new Document_Request'
                    (GPS.LSP_Client.Requests.LSP_Request with
                     Provider =>
                       Current_File_Entities_Search_Provider_Access (Self),
                     Num      => Self.Request_Num,
                     Kernel   => Self.Kernel,
                     File     => Self.File,
                     others => <>);
               end if;

               if Request /= null then
                  Self.Request :=
                    GPS.LSP_Client.Requests.Execute (Lang, Request);
                  Self.Waiting := True;
               end if;
            end;
         end if;
      end;
   end Set_Pattern;

   ------------------
   -- Send_Request --
   ------------------

   procedure Send_Request
     (Self : not null access Entities_Search_Provider'Class)
   is
      Languages : GNAT.Strings.String_List :=
        Root_Project (Self.Kernel.Get_Project_Tree.all).Languages (True);
      Lang      : Language.Language_Access;

   begin
      for Index in Languages'Range loop
         Lang := Self.Kernel.Get_Language_Handler.Get_Language_By_Name
           (Languages (Index).all);

         if GPS.LSP_Module.LSP_Is_Enabled (Lang) then
            declare
               Request : GPS.LSP_Client.Requests.Request_Access;

            begin
               if Ada.Characters.Handling.To_Lower
                 (Lang.Get_Name) = "ada"
               then
                  Request := new Symbol_Request'
                    (GPS.LSP_Client.Requests.LSP_Request with
                     Provider           =>
                       Entities_Search_Provider_Access (Self),
                     Num                => Self.Request_Num,
                     Kernel             => Self.Kernel,
                     Query              =>
                       VSS.Strings.Conversions.To_Virtual_String
                         (Self.Pattern.Get_Text),
                     Case_Sensitive     =>
                       (Is_Set => True,
                        Value  => Self.Pattern.Get_Case_Sensitive),
                     Whole_Word         =>
                       (Is_Set => True,
                        Value  => Self.Pattern.Get_Whole_Word),
                     Negate             =>
                       (Is_Set => True,
                        Value  => Self.Pattern.Get_Negate),
                     Kind               =>
                       (Is_Set => True,
                        Value  => LSP.Messages.Search_Kind'Val
                          (GPS.Search.Search_Kind'Pos
                               (Self.Pattern.Get_Kind))),
                     partialResultToken => <>);

               elsif Self.Pattern.Get_Kind = Full_Text
                 and then not Self.Pattern.Get_Negate
               then
                  --  Start only full text search on servers without
                  --  filtration. In other case the result may have
                  --  a huge ammount of records (all known entities)
                  --  and this will cause freeze of the search engine.

                  Request := new Symbol_Request'
                    (GPS.LSP_Client.Requests.LSP_Request with
                     Provider =>
                       Entities_Search_Provider_Access (Self),
                     Num      => Self.Request_Num,
                     Kernel   => Self.Kernel,
                     Query    =>
                       VSS.Strings.Conversions.To_Virtual_String
                         (Self.Pattern.Get_Text),
                     others   => <>);
               end if;

               if Request /= null then
                  Self.References.Append
                    (GPS.LSP_Client.Requests.Execute (Lang, Request));
                  Self.Waiting := Self.Waiting + 1;
               end if;
            end;
         end if;
         Free (Languages (Index));
      end loop;
   end Send_Request;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module (Kernel : Kernel_Handle) is
      P : Kernel_Search_Provider_Access;
   begin
      if Me_Search_Entities_Support.Active then
         P := new Entities_Search_Provider;
         Register_Provider_And_Action (Kernel, P);

         P := new Current_File_Entities_Search_Provider;
         Register_Provider_And_Action (Kernel, P);

      else
         --  Old implementation
         P := new Completion.Search.Entities_Search_Provider;
         Register_Provider_And_Action (Kernel, P);

         P := new Completion.Search.Current_File_Entities_Search_Provider;
         Register_Provider_And_Action (Kernel, P);
      end if;
   end Register_Module;

end GPS.LSP_Client.Search.Entities;
