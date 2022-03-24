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

with Ada.Characters.Handling;         use Ada.Characters.Handling;
with Ada.Strings;                     use Ada.Strings;
with Ada_Semantic_Tree;               use Ada_Semantic_Tree;

with GNAT.Regpat;                     use GNAT.Regpat;
with GNATCOLL.Traces;                 use GNATCOLL.Traces;
with GNATCOLL.VFS;                    use GNATCOLL.VFS;
with GNATCOLL.JSON;
with GNATCOLL.Scripts;                use GNATCOLL.Scripts;
with GNATCOLL.Scripts.Python;         use GNATCOLL.Scripts.Python;
with GNATCOLL.Projects;               use GNATCOLL.Projects;

with VSS.Strings.Conversions;

with Glib;
with Glib.Convert;                    use Glib.Convert;
with Gtkada.Style;

with LSP.Types;             use LSP.Types;

with Completion_Module;               use Completion_Module;
with GPS.Kernel.Contexts;             use GPS.Kernel.Contexts;
with GPS.Kernel.Style_Manager;        use GPS.Kernel.Style_Manager;
with GPS.LSP_Client.Utilities;
with GPS.LSP_Client.Requests.Completion;
with GPS.LSP_Client.Requests;         use GPS.LSP_Client.Requests;
with GPS.LSP_Client.Language_Servers; use GPS.LSP_Client.Language_Servers;
with GPS.LSP_Module;                  use GPS.LSP_Module;

with LAL.Core_Module;
with LAL.Highlighters;
with LAL.Module;
with Language.Ada;                    use Language.Ada;
with Language.Cpp;                    use Language.Cpp;
with Language.C;                      use Language.C;
with Langkit_Support.Text;
with Libadalang.Analysis;
with Libadalang.Common;

package body GPS.LSP_Client.Completion is

   Me : constant Trace_Handle := Create ("GPS.LSP.COMPLETION", On);

   Advanced_Me : constant Trace_Handle :=
     Create ("GPS.LSP.COMPLETION.ADVANCED", Off);

   LSP_Resolver_ID_Prefix : constant String := "LSP_CMP_";

   Location_Pattern       : constant Pattern_Matcher :=
     Compile ("at\s([\w\-]*?\.\w*)\s\((\d*)\:(\d*)\)");
   --  Pattern used to detect a file location in the completion items'
   --  documentation.
   --  This location can then be used to display a link button that jumps to
   --  the completion item's declaration.
   --  This format is only used by the ALS now. Here is an example of what
   --  this regexp matches:
   ---
   ---  at gps-kernel.ads (360:4)

   function To_LSP_Completion_Trigger_Kind
     (Trigger_Kind : Completion_Trigger_Kind)
      return LSP.Messages.CompletionTriggerKind is
     (case Trigger_Kind is
         when Invoked =>
            LSP.Messages.Invoked,
         when TriggerCharacter =>
            LSP.Messages.TriggerCharacter,
         when TriggerForIncompleteCompletions =>
            LSP.Messages.TriggerForIncompleteCompletions);

   ----------------------------
   -- LSP Completion Request --
   ----------------------------

   type LSP_Completion_Request is
     new GPS.LSP_Client.Requests.Completion.Abstract_Completion_Request with
      record
         Resolver : LSP_Completion_Resolver_Access;
         Result   : Completion_List;
      end record;
   type LSP_Completion_Request_Access is access all LSP_Completion_Request;

   overriding procedure On_Result_Message
     (Self   : in out LSP_Completion_Request;
      Result : LSP.Messages.CompletionList);

   overriding procedure On_Rejected (Self : in out LSP_Completion_Request);

   overriding procedure On_Error_Message
     (Self    : in out LSP_Completion_Request;
      Code    : LSP.Messages.ErrorCodes;
      Message : String;
      Data    : GNATCOLL.JSON.JSON_Value);

   overriding procedure Finalize (Self : in out LSP_Completion_Request)
   is null;

   ----------------------------------------
   -- LSP CompletionItem Resolve Request --
   ----------------------------------------

   type LSP_CompletionItem_Resolve_Request is
     new GPS.LSP_Client.Requests.Completion.
       Abstract_CompletionItem_Resolve_Request with
      record
         Resolver : LSP_Completion_Resolver_Access;
         Item_Idx : Positive;
   end record;
   type LSP_CompletionItem_Resolve_Request_Access
   is access all LSP_CompletionItem_Resolve_Request;

   overriding procedure On_Result_Message
     (Self   : in out LSP_CompletionItem_Resolve_Request;
      Result : LSP.Messages.CompletionItem);

   ----------------------
   -- Lazy Computation --
   ----------------------

   type LSP_Completion_Component is
     new Completion_List_Pckg.Virtual_List_Component
   with record
      Resolver : LSP_Completion_Resolver_Access;
   end record;

   type LSP_Completion_Iterator is
     new Completion_List_Pckg.Virtual_List_Component_Iterator
   with record
      Resolver : LSP_Completion_Resolver_Access;
      Index    : Positive;
   end record;

   overriding function First (List : LSP_Completion_Component)
      return Completion_List_Pckg.Virtual_List_Component_Iterator'Class;

   overriding function At_End (It : LSP_Completion_Iterator) return Boolean;

   overriding procedure Next (It : in out LSP_Completion_Iterator);

   overriding function Get
     (It : in out LSP_Completion_Iterator) return Completion_Proposal'Class;

   -----------
   -- Utils --
   -----------

   function To_Language_Category
     (Kind : CompletionItemKind) return Language_Category
   is
     (case Kind is
         when Text                  => Cat_Unknown,
         when Method .. Constructor => Cat_Function,
         when Field | Property      => Cat_Field,
         when Variable              => Cat_Variable,
         when Class .. An_Interface => Cat_Class,
         when Module | Unit         => Cat_Package,
         when Enum | EnumMember     => Cat_Custom,
         when Struct                => Cat_Type,
         when TypeParameter         => Cat_Parameter,
         when Snippet               => Cat_Snippet,
         when others                => Cat_Unknown);

   type LSP_Completion_Detail_Highlighter is
     new LAL.Highlighters.Highlightable_Interface with record
      Kernel : Kernel_Handle;
      Detail : Unbounded_String;
   end record;
   --  Used to highlight the completion item's detail.

   overriding procedure Highlight_Token
     (Self  : in out LSP_Completion_Detail_Highlighter;
      Token : Libadalang.Common.Token_Reference;
      Style : String);

   overriding procedure Remove_Highlighting
     (Self  : in out LSP_Completion_Detail_Highlighter;
      Style : String;
      From  : Integer;
      To    : Integer) is null;

   function Default_Completion_Trigger_Chars_Func
     (Editor : Editor_Buffer'Class; C : Character) return Boolean;

         function Get_Detail (Item : CompletionItem) return Unbounded_String;
      --  Get the detail field of the given completion item, if any.

      function Get_Documentation
        (Item : CompletionItem) return VSS.Strings.Virtual_String;
      --  Get the documentation associated to the given completion item.

   -----------------------------
   -- LSP Completion Resolver --
   -----------------------------

   overriding function Get_Id
     (Resolver : LSP_Completion_Resolver) return String
   is
     (LSP_Resolver_ID_Prefix & To_String (Resolver.Lang_Name));

   --------------------
   -- Get_Completion --
   --------------------

   overriding function Get_Completion
     (Proposal : LSP_Completion_Proposal;
      Db       : access Xref.General_Xref_Database_Record'Class)
      return UTF8_String
   is
     (VSS.Strings.Conversions.To_UTF_8_String (Proposal.Text));

   ---------------
   -- Get_Label --
   ---------------

   overriding function Get_Label
     (Proposal : LSP_Completion_Proposal;
      Db       : access Xref.General_Xref_Database_Record'Class)
      return UTF8_String
   is
      pragma Unreferenced (Db);
   begin
      return VSS.Strings.Conversions.To_UTF_8_String (Proposal.Label);
   end Get_Label;

   -------------------
   -- Get_Sort_Text --
   -------------------

   overriding function Get_Sort_Text
     (Proposal : LSP_Completion_Proposal;
      Db       : access Xref.General_Xref_Database_Record'Class)
      return UTF8_String
   is
      pragma Unreferenced (Db);
   begin
      return VSS.Strings.Conversions.To_UTF_8_String (Proposal.Sort_Text);
   end Get_Sort_Text;

   ---------------------
   -- Get_Filter_Text --
   ---------------------

   overriding function Get_Filter_Text
     (Proposal : LSP_Completion_Proposal;
      Db       : access Xref.General_Xref_Database_Record'Class)
      return UTF8_String is
   begin
      return VSS.Strings.Conversions.To_UTF_8_String (Proposal.Filter_Text);
   end Get_Filter_Text;

   ------------------
   -- Get_Category --
   ------------------

   overriding function Get_Category
     (Proposal : LSP_Completion_Proposal) return Language_Category
   is
     (Proposal.Category);

   --------------------
   -- Get_Visibility --
   --------------------

   overriding function Get_Visibility
     (Proposal : LSP_Completion_Proposal) return Construct_Visibility
   is
     (Visibility_Public);

   ------------------
   -- Get_Location --
   ------------------

   overriding function Get_Location
     (Proposal : LSP_Completion_Proposal;
      Db       : access Xref.General_Xref_Database_Record'Class)
      return File_Location
   is
      pragma Unreferenced (Db);
      Resolver : constant LSP_Completion_Resolver_Access :=
        LSP_Completion_Resolver_Access
          (Proposal.Resolver);
      Item     : constant CompletionItem :=
        Resolver.Completions.items (Proposal.ID);
      Doc      : constant String :=
        VSS.Strings.Conversions.To_UTF_8_String (Get_Documentation (Item));
   begin
      if Doc = "" then
         return Null_File_Location;
      end if;

      --  Try to match the file location pattern in the proposal's
      --  documentation.

      declare
         Kernel   : constant Kernel_Handle := Resolver.Kernel;
         Matched  : Match_Array (0 .. 3);
      begin
         Match (Location_Pattern, Doc, Matched);

         if Matched (0) = No_Match then
            return Null_File_Location;
         end if;

         declare
            Filename : constant String := Doc
              (Matched (1).First .. Matched (1).Last);
            File     : constant Virtual_File :=
              Kernel.Get_Project_Tree.Create
                (Base_Name (Create_From_Base (+Filename)));
            Line     : constant Integer := Integer'Value
              (Doc (Matched (2).First .. Matched (2).Last));
            Column   : constant Integer := Integer'Value
              (Doc (Matched (3).First .. Matched (3).Last));
         begin
            if File = No_File then
               return Null_File_Location;
            end if;

            return File_Location'
              (File_Path => File,
               Line      => Line,
               Column    => Visible_Column_Type (Column));
         end;

      end;
   end Get_Location;

   -----------------------
   -- Get_Documentation --
   -----------------------

   overriding function Get_Documentation
     (Proposal : LSP_Completion_Proposal)
      return String
   is
      use Libadalang.Analysis;
      use Libadalang.Common;
      use LAL.Core_Module;

      Resolver       : constant LSP_Completion_Resolver_Access :=
        LSP_Completion_Resolver_Access (Proposal.Get_Resolver);
      Item           : constant CompletionItem :=
        Resolver.Completions.items (Proposal.ID);
      Detail         : Unbounded_String;
      Documentation  : VSS.Strings.Virtual_String;
      Is_Highlighted : Boolean := False;
   begin
      Documentation := Get_Documentation (Item);
      Detail := Get_Detail (Item);

      --  Try to highlight the completion item's detail, if any.

      if Proposal.Highlightable_Detail
        and then Detail /= Null_Unbounded_String
      then
         declare
            Highlighter     : LSP_Completion_Detail_Highlighter :=
              (Kernel => LSP_Completion_Resolver_Access
                 (Proposal.Resolver).Kernel,
               Detail => <>);
            LAL_Module      : constant LAL.Core_Module.LAL_Module_Id :=
              LAL.Module.Get_LAL_Core_Module;
            Unit            : constant Analysis_Unit :=
              Get_From_Buffer
                (Context  =>
                   LAL_Module.Get_Current_Analysis_Context,
                 Filename => "",
                 Charset  => "UTF-8",
                 Buffer   => To_String (Detail),
                 Rule     => Basic_Decl_Rule);
         begin
            Is_Highlighted := Highlighter.Highlight_Using_Tree
              (Unit => Unit);

            Detail :=
              (if Is_Highlighted then Highlighter.Detail else Detail);
         end;
      end if;

      if Detail /= Null_Unbounded_String then
         return (if Is_Highlighted then To_String (Detail)
                 else Escape_Text (To_String (Detail)))
           & ASCII.LF
           & ASCII.LF
           & Escape_Text
           (VSS.Strings.Conversions.To_UTF_8_String (Documentation));

      else
         return Escape_Text
           (VSS.Strings.Conversions.To_UTF_8_String (Documentation));
      end if;
   end Get_Documentation;

   -----------
   -- Match --
   -----------

   overriding function Match
     (Proposal   : LSP_Completion_Proposal;
      Context    : Completion_Context;
      Offset     : String_Index_Type) return Boolean
   is
     (True);

   -----------------------------
   -- Insert_Text_On_Selected --
   -----------------------------

   overriding function Insert_Text_On_Selected
     (Proposal : LSP_Completion_Proposal) return Boolean is
   begin
      if not Proposal.Is_Snippet then
         return True;
      end if;

      --  If the proposal's text does not contain the '$' sign
      --  (i.e: the sign used in the LSP to introduce completion snippet
      --  parameters), return True so that the text gets automatically
      --  inserted.

      return Index (LSP_String'(To_LSP_String (Proposal.Text)), "$") = 0;
   end Insert_Text_On_Selected;

   ----------------
   -- Get_Detail --
   ----------------

   function Get_Detail (Item : CompletionItem) return Unbounded_String is
   begin
      if Item.detail.Is_Set then
         return To_Unbounded_String (To_UTF_8_String (Item.detail.Value));
      else
         return Null_Unbounded_String;
      end if;
   end Get_Detail;

   -----------------------
   -- Get_Documentation --
   -----------------------

   function Get_Documentation
     (Item : CompletionItem) return VSS.Strings.Virtual_String is
   begin
      --  When set, extract the documentation, either in plain text or
      --  markdown format.
      if Item.documentation.Is_Set then
         if Item.documentation.Value.Is_String then
            return Item.documentation.Value.String;
         else
            return
              LSP.Types.To_Virtual_String
                (Item.documentation.Value.Content.value);
         end if;
      end if;

      return VSS.Strings.Empty_Virtual_String;
   end Get_Documentation;

   -----------------
   -- On_Selected --
   -----------------

   overriding procedure On_Selected
     (Proposal : LSP_Completion_Proposal;
      Kernel   : not null Kernel_Handle)
   is
      Python : constant Scripting_Language :=
        Kernel.Scripts.Lookup_Scripting_Language ("Python");
      Args   : Callback_Data'Class := Python.Create (1);
   begin
      Python_Callback_Data'Class (Args).Set_Nth_Arg
        (1, VSS.Strings.Conversions.To_UTF_8_String (Proposal.Text));

      --  Call the Python function that will expand the snippet
      Args.Execute_Command ("aliases.expand_lsp_snippet");
   end On_Selected;

   ----------------------------
   -- On_Documentation_Query --
   ----------------------------

   overriding function On_Documentation_Query
     (Proposal : LSP_Completion_Proposal) return Boolean
   is
      Resolver : constant LSP_Completion_Resolver_Access :=
        LSP_Completion_Resolver_Access
             (Proposal.Resolver);
      Kernel   : constant Kernel_Handle := Resolver.Kernel;
      Lang     : constant Language_Access :=
        Kernel.Get_Language_Handler.Get_Language_By_Name
          (To_String (Resolver.Lang_Name));
      Request  : LSP_CompletionItem_Resolve_Request_Access :=
        new LSP_CompletionItem_Resolve_Request'
          (GPS.LSP_Client.Requests.LSP_Request with
           Kernel   => Kernel,
           Resolver => Resolver,
           Item     => Resolver.Completions.items (Proposal.ID),
           Item_Idx => Proposal.ID);
   begin
      return GPS.LSP_Client.Requests.Execute
        (Lang,
         GPS.LSP_Client.Requests.Request_Access (Request));
   end On_Documentation_Query;

   ----------------------
   -- To_Completion_Id --
   ----------------------

   overriding function To_Completion_Id
     (Proposal : LSP_Completion_Proposal)
      return Completion_Id
   is
      ID : constant String := Integer'Image (Proposal.ID);
   begin
      return Completion_Id'
        (Id_Length   => ID'Length,
         Resolver_Id => LSP_Resolver_ID_Prefix,
         Id          => ID,
         File        => No_File,
         Line        => 0,
         Column      => 0);
   end To_Completion_Id;

   ---------------------
   -- Highlight_Token --
   ---------------------

   overriding procedure Highlight_Token
     (Self  : in out LSP_Completion_Detail_Highlighter;
      Token : Libadalang.Common.Token_Reference;
      Style : String)
   is
      use Langkit_Support.Text;
      use Libadalang.Common;

      Highlight_Style : constant Style_Access :=
        Get_Style_Manager (Self.Kernel).Get
        (Key        => Style,
         Allow_Null => True);
   begin
      if Highlight_Style = null then
         Self.Detail := Self.Detail
           & Escape_Text (To_UTF8 (Text (Token)));
      else
         Self.Detail := Self.Detail & "<span foreground="""
           & Gtkada.Style.To_Hex (Get_Foreground (Highlight_Style))
           & """>"
           & Escape_Text (To_UTF8 (Text (Token)))
           & "</span>";
      end if;
   end Highlight_Token;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self   : in out LSP_Completion_Request;
      Result : LSP.Messages.CompletionList)
   is
      Component : constant LSP_Completion_Component :=
        LSP_Completion_Component'
                          (Resolver => Self.Resolver);
   begin
      --  If there are no completion items, return immediately and display
      --  any results already computed by other providers.
      if Result.items.Is_Empty then
         declare
            Window : constant Completion_Display_Interface_Access :=
                       Get_Completion_Display;
         begin
            if Window /= null then
               Window.Display_Proposals
                 (List          => Self.Result,
                  Is_Incomplete => False);
            end if;

            return;
         end;
      end if;

      Trace
        (Advanced_Me,
         "completions received, ID "
         & VSS.Strings.Conversions.To_UTF_8_String
           (To_Virtual_String (Self.Id))
         & ": " & Integer (Result.items.Length)'Img);
      Trace
        (Advanced_Me, "Is list incomplete: " & Result.isIncomplete'Img);

      Self.Resolver.Completions :=
        CompletionList'(isIncomplete => Result.isIncomplete,
                        items        => Result.items.Copy);

      Append (Self.Result, Component);

      declare
         Window : constant Completion_Display_Interface_Access :=
                      Get_Completion_Display;
      begin
         if Window /= null then
            Window.Display_Proposals
              (List          => Self.Result,
               Is_Incomplete => Result.isIncomplete);
         end if;
      end;
   end On_Result_Message;

   -----------------
   -- On_Rejected --
   -----------------

   overriding procedure On_Rejected (Self : in out LSP_Completion_Request) is
      pragma Unreferenced (Self);

      Window : constant Completion_Display_Interface_Access :=
                  Get_Completion_Display;
   begin
      Trace (Advanced_Me, "On_Rejected is called");

      if Window /= null then
         Window.Display_Proposals (Null_Completion_List);
      end if;
   end On_Rejected;

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out LSP_Completion_Request;
      Code    : LSP.Messages.ErrorCodes;
      Message : String;
      Data    : GNATCOLL.JSON.JSON_Value)
   is
      pragma Unreferenced (Self);

      Window : constant Completion_Display_Interface_Access :=
                  Get_Completion_Display;
   begin
      Trace (Advanced_Me, "Error received: " & Message);

      if Window /= null then
         Window.Display_Proposals (Null_Completion_List);
      end if;
   end On_Error_Message;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self   : in out LSP_CompletionItem_Resolve_Request;
      Result : LSP.Messages.CompletionItem) is
   begin
      --  Replace the completion item returned on textDocument/completion
      --  with the new one, that has all its fields computed.
      Self.Resolver.Completions.items.Replace_Element
        (Self.Item_Idx, Result);

      Get_Completion_Display.Display_Documentation;
   end On_Result_Message;

   -----------
   -- First --
   -----------

   overriding function First (List : LSP_Completion_Component)
      return Completion_List_Pckg.Virtual_List_Component_Iterator'Class
   is
      Iterator : LSP_Completion_Iterator;
   begin
      Iterator.Resolver := List.Resolver;
      Iterator.Index := 1;

      return Iterator;
   end First;

   ------------
   -- At_End --
   ------------

   overriding function At_End (It : LSP_Completion_Iterator) return Boolean is
   begin
      return It.Index > Integer (It.Resolver.Completions.items.Length);
   end At_End;

   ----------
   -- Next --
   ----------

   overriding procedure Next (It : in out LSP_Completion_Iterator) is
   begin
      It.Index := It.Index + 1;
   end Next;

   ---------
   -- Get --
   ---------

   overriding function Get
     (It : in out LSP_Completion_Iterator) return Completion_Proposal'Class is
   begin
      if It.Resolver.Completions.items.Is_Empty then
         return LSP_Completion_Proposal'
           (Resolver => It.Resolver,
            others   => <>);
      end if;

      declare
         Item     : constant CompletionItem := It.Resolver.Completions.items
           (It.Index);
         Proposal : constant LSP_Completion_Proposal :=
           LSP_Completion_Proposal'
             (Resolver             => It.Resolver,
              Text                 =>
                (if Item.insertText.Is_Set then
                    LSP.Types.To_Virtual_String (Item.insertText.Value)
                 else
                    Item.label),
              Label                => Item.label,
              Sort_Text            =>
                (if Item.sortText.Is_Set then
                    LSP.Types.To_Virtual_String (Item.sortText.Value)
                 else
                    Item.label),
              Filter_Text          =>
                (if Item.filterText.Is_Set then
                    LSP.Types.To_Virtual_String (Item.filterText.Value)
                 else
                    Item.label),
              Detail               => Get_Detail (Item),
              Highlightable_Detail =>
                To_String (It.Resolver.Lang_Name) = "ada",
              Documentation        => Get_Documentation (Item),
              Category             =>
                (if Item.kind.Is_Set then
                    To_Language_Category (Item.kind.Value)
                 else
                    Cat_Unknown),
              Is_Snippet           =>
                (Item.insertTextFormat.Is_Set
                 and then Item.insertTextFormat.Value = Snippet),
              ID                   => It.Index);
      begin
         return Proposal;
      end;
   end Get;

   ---------------------------------
   -- Get_Initial_Completion_List --
   ---------------------------------

   overriding function Get_Initial_Completion_List
     (Manager : access LSP_Completion_Manager;
      Context : Completion_Context) return Completion_List
   is
      Result : Completion_List := Null_Completion_List;
   begin
      --  Iterate over all the non-LSP completion resolvers that might
      --  have been registered (e.g: aliases completion resolver) and
      --  return their results.

      for Resolver of Manager.Get_Resolvers loop
         if Resolver.all not in LSP_Completion_Resolver'Class then
            Resolver.Get_Completion_Root
              (Offset  => Get_Completion_Start_Offset (Context),
               Context => Context,
               Result  => Result);
         end if;
      end loop;

      return Result;
   end Get_Initial_Completion_List;

   ---------------------------
   -- Query_Completion_List --
   ---------------------------

   overriding procedure Query_Completion_List
     (Manager      : access LSP_Completion_Manager;
      Context      : Completion_Context;
      Initial_List : in out Completion_List)
   is
      Kernel         : Kernel_Handle renames Manager.Kernel;
      Editor_Context : constant Selection_Context :=
                         Kernel.Get_Current_Context;
      File           : constant Virtual_File := File_Information
        (Editor_Context);
      Lang           : constant Language_Access :=
                         Kernel.Get_Language_Handler.Get_Language_From_File
                           (File);
      Resolver       : constant LSP_Completion_Resolver_Access :=
                         LSP_Completion_Resolver_Access
                           (Manager.Get_Resolver
                              (LSP_Resolver_ID_Prefix
                               & To_Lower (Lang.Get_Name)));
      Holder         : constant Controlled_Editor_Buffer_Holder :=
        Kernel.Get_Buffer_Factory.Get_Holder (File);
      Location       : constant Editor_Location'Class :=
        Holder.Editor.New_Location
          (Line_Information (Editor_Context),
           Column_Information (Editor_Context));
      Request        : LSP_Completion_Request_Access := new
        LSP_Completion_Request'
          (GPS.LSP_Client.Requests.LSP_Request with
           Kernel        => Resolver.Kernel,
           Resolver      => Resolver,
           File          => File,
           Result        => Initial_List,
           Position      =>
             GPS.LSP_Client.Utilities.Location_To_LSP_Position (Location),
           Context       => (To_LSP_Completion_Trigger_Kind
                             (Get_Trigger_Kind (Context)), others => <>));

   begin
      Resolver.Completions.items.Clear;

      Trace (Advanced_Me, "queriying completions...");

      GPS.LSP_Client.Requests.Execute
        (Lang,
         GPS.LSP_Client.Requests.Request_Access (Request));

      if Request /= null then
         Trace (Advanced_Me, "queriying completions with ID "
                & VSS.Strings.Conversions.To_UTF_8_String
                  (To_Virtual_String (Request.Id)));
      end if;
   end Query_Completion_List;

   -------------------------
   -- Get_Completion_Root --
   -------------------------

   overriding procedure Get_Completion_Root
     (Resolver   : access LSP_Completion_Resolver;
      Offset     : String_Index_Type;
      Context    : Completion_Context;
      Result     : in out Completion_List) is null;

   ------------------------------------
   -- LSP_Completion_Manager_Factory --
   ------------------------------------

   function LSP_Completion_Manager_Factory
     (Kernel : not null GPS.Kernel.Kernel_Handle;
      File   : GNATCOLL.VFS.Virtual_File;
      Lang   : Language.Language_Access) return Completion_Manager_Access
   is
      pragma Unreferenced (File);
      Manager   : Completion_Manager_Access;
      Resolver  : Completion_Resolver_Access;
      Lang_Name : constant String := To_Lower (Lang.Get_Name);
   begin
      --  Enable LSP-based completion for Ada only if the LSP.COMPLETION
      --  trace is active.

      if (Lang_Name /= "ada" or else Me.Is_Active)
        and then LSP_Is_Enabled (Lang)
      then
         Manager := new LSP_Completion_Manager'
           (Asynchronous_Completion_Manager with
            Kernel => Kernel);

         Resolver := new LSP_Completion_Resolver'
           (Completion_Resolver with
            Kernel      => Kernel,
            Lang_Name   => To_Unbounded_String (To_Lower (Lang.Get_Name)),
            Completions => <>);

         Register_Resolver
           (Manager,
            Resolver);
      end if;

      return Manager;
   end LSP_Completion_Manager_Factory;

   -------------------------------------------
   -- Default_Completion_Trigger_Chars_Func --
   -------------------------------------------

   function Default_Completion_Trigger_Chars_Func
     (Editor : Editor_Buffer'Class; C : Character) return Boolean
   is
      Lang   : constant Language.Language_Access
        := (if Editor /= Nil_Editor_Buffer then Editor.Get_Language
            else null);

      --  Return true if the cursor is at a location where an Ada keyword
      --  should open an auto-completion, false otherwise

   begin

      --  ??? this whole test is too language-specific for the moment.
      --  Should probably be moved to some new language primitive in order
      --  to support other auto-completion triggers for other languages.

      if Lang = null then
         return False;
      elsif Lang = Ada_Lang then
         --  We want to complete only when certain specific tokens that
         --  indicate certain language constructs precede the current cursor.

         if C = ' ' then
            declare
               Insert_Mark_Loc : constant Editor_Location'Class :=
                 Editor.Get_Main_Cursor.Get_Insert_Mark.Location;
               Exp             : Parsed_Expression;
               The_Text        : String_Access;
               Ret             : Boolean;
            begin
               The_Text := new String'(Editor.Get_Chars
                 (From                 => Insert_Mark_Loc,
                  To                   => Insert_Mark_Loc.Beginning_Of_Line,
                  Include_Hidden_Chars => False));

               Exp := Parse_Expression_Backward (The_Text);

               Ret := Integer (Exp.Tokens.Length) = 1
                 and then
                   Exp.Tokens.First_Element.Tok_Type in
                     Tok_With | Tok_Use | Tok_Pragma | Tok_Accept
                       | Tok_Raise | Tok_Aspect;

               Free (Exp);

               return Ret;
            end;
         end if;

         return C in '.' | ',' | '(' | ''';

      elsif Lang in Cpp_Lang | C_Lang then
         return C in '.' | '(' | '>';
      else
         return C not in ' ' | ASCII.HT;
      end if;
   end Default_Completion_Trigger_Chars_Func;

   ---------------------------------------
   -- LSP_Completion_Trigger_Chars_Func --
   ---------------------------------------

   function LSP_Completion_Trigger_Chars_Func
     (Editor : Editor_Buffer'Class;
      C      : Character) return Boolean
   is
      Lang   : constant Language.Language_Access :=
        Editor.Get_Language;
      Server : constant Language_Server_Access := Get_Language_Server
        (Lang);
   begin
      --  If there is no server for the given language, fallback to the default
      --  function, based on the old engine.
      if Server = null then
         return Default_Completion_Trigger_Chars_Func
           (Editor => Editor,
            C      => C);
      end if;

      --  Check if the entered character is present in the server's
      --  triggerCharacters list, if any.

      declare
         Capabilities : constant LSP.Messages.ServerCapabilities :=
           Server.Get_Client.Capabilities;
      begin
         if Capabilities.completionProvider.Is_Set then
            declare
               Completion_Options : LSP.Messages.CompletionOptions renames
                 Capabilities.completionProvider.Value;
            begin
               return Completion_Options.triggerCharacters.Is_Set and then
                 Completion_Options.triggerCharacters.Value.Contains
                   (To_LSP_String ("" & C));
            end;
         end if;
      end;

      return False;
   end LSP_Completion_Trigger_Chars_Func;

   --------------
   -- Register --
   --------------

   procedure Register (Kernel : Kernel_Handle) is
      pragma Unreferenced (Kernel);
   begin
      Completion_Module.Set_Completion_Manager_Factory
        (Factory => LSP_Completion_Manager_Factory'Access);
      Completion_Module.Set_Completion_Trigger_Chars_Func
        (Func => LSP_Completion_Trigger_Chars_Func'Access);
   end Register;

end GPS.LSP_Client.Completion;
