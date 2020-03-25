------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2020, AdaCore                       --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;

with GNATCOLL.Traces;         use GNATCOLL.Traces;
with GNATCOLL.VFS;            use GNATCOLL.VFS;
with GNATCOLL.JSON;

with Glib.Unicode;            use Glib.Unicode;
with Glib;
with Glib.Convert;            use Glib.Convert;

with Completion_Module;       use Completion_Module;
with GPS.Editors;             use GPS.Editors;
with GPS.Kernel.Contexts;     use GPS.Kernel.Contexts;
with GPS.LSP_Client.Requests.Completion;
with GPS.LSP_Client.Requests; use GPS.LSP_Client.Requests;
with GPS.LSP_Module;          use GPS.LSP_Module;

package body GPS.LSP_Client.Completion is

   Me : constant Trace_Handle :=
     Create ("GPS.LSP.COMPLETION", Off);

   Advanced_Me : constant Trace_Handle :=
     Create ("GPS.LSP.COMPLETION.ADVANCED", Off);

   LSP_Resolver_ID_Prefix : constant String := "LSP_CMP_";

   ----------------------------
   -- LSP Completion Request --
   ----------------------------

   type LSP_Completion_Request is
     new GPS.LSP_Client.Requests.Completion.Abstract_Completion_Request with
      record
         Kernel   : Kernel_Handle;
         Resolver : LSP_Completion_Resolver_Access;
         Context  : Completion_Context;
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
         when Text => Cat_Unknown,
         when Method .. Constructor => Cat_Function,
         when Field | Property      => Cat_Field,
         when Variable              => Cat_Variable,
         when Class .. An_Interface => Cat_Class,
         when Module | Unit         => Cat_Package,
         when Enum | EnumMember     => Cat_Custom,
         when Struct                => Cat_Type,
         when TypeParameter         => Cat_Parameter,
         when others                => Cat_Unknown);

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
      (To_UTF_8_String (Proposal.Text));

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

   -----------------------
   -- Get_Documentation --
   -----------------------

   overriding function Get_Documentation
     (Proposal : LSP_Completion_Proposal)
      return String
   is
     (To_UTF_8_String (Proposal.Documentation));

   -----------
   -- Match --
   -----------

   overriding function Match
     (Proposal   : LSP_Completion_Proposal;
      Context    : Completion_Context;
      Offset     : String_Index_Type) return Boolean
   is
     (True);

   ----------------------
   -- To_Completion_Id --
   ----------------------

   overriding function To_Completion_Id
     (Proposal : LSP_Completion_Proposal)
      return Completion_Id
   is
     (Completion_Id'
        (Id_Length   => Length (Proposal.Text),
         Resolver_Id => LSP_Resolver_ID_Prefix,
         Id          => To_UTF_8_String (Proposal.Text),
         File        => No_File,
         Line        => 0,
         Column      => 0));

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self   : in out LSP_Completion_Request;
      Result : LSP.Messages.CompletionList)
   is
      List      : Completion_List;
      Component : constant LSP_Completion_Component :=
                        LSP_Completion_Component'
                          (Resolver => Self.Resolver);
   begin
      --  If there are no completion items, close the completion window.
      if Result.items.Is_Empty then
         declare
            Window : constant Completion_Display_Interface_Access :=
                       Get_Completion_Display;
         begin
            if Window /= null then
               Window.Display_Proposals (Null_Completion_List);
            end if;

            return;
         end;
      end if;

      Trace
        (Advanced_Me,
         "completions received: " & Integer (Result.items.Length)'Img);

      Self.Resolver.Completions :=
        CompletionList'(isIncomplete => Result.isIncomplete,
                        items        => Result.items.Copy);

      Append (List, Component);

      Self.Resolver.Get_Completion_Root
        (Offset  => 0,
         Context => Self.Context,
         Result  => List);

      Free (Self.Context);

      declare
         Window : constant Completion_Display_Interface_Access :=
                      Get_Completion_Display;
      begin
         if Window /= null then
            Window.Display_Proposals (List);
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

      function Get_Documentation
        (Item : CompletionItem) return LSP_String;
      --  Get the documentation associated to the given completion item.

      -----------------------
      -- Get_Documentation --
      -----------------------

      function Get_Documentation
        (Item : CompletionItem) return LSP_String
      is
         Doc      : LSP_String;
         New_Line : constant LSP_String := To_LSP_String (ASCII.LF & ASCII.LF);
      begin
         --  First, get the completion item's detail if any.
         if Item.detail.Is_Set then
            Doc := Doc
              & To_LSP_String
              (Escape_Text
                 (To_UTF_8_String (Item.detail.Value))
               & ASCII.LF);
         end if;

         --  When set, extract the documentation, either in plain text or
         --  markdown format.
         if Item.documentation.Is_Set then
            if Item.documentation.Value.Is_String then
               Doc := Doc & New_Line & Item.documentation.Value.String;
            else
               Doc := Doc & New_Line & Item.documentation.Value.Content.value;
            end if;
         end if;

         return Doc;
      end Get_Documentation;

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
                        (Resolver      => It.Resolver,
                         Text          =>
                           (if Item.insertText.Is_Set then
                               Item.insertText.Value
                            else
                               Item.label),
                         Label         => Item.label,
                         Documentation => Get_Documentation (Item),
                         Category      =>
                           (if Item.kind.Is_Set then
                               To_Language_Category (Item.kind.Value)
                            else
                               Cat_Unknown));
      begin
         return Proposal;
      end;
   end Get;

   ---------------------------------
   -- Get_Initial_Completion_List --
   ---------------------------------

   overriding function Get_Initial_Completion_List
     (Manager : access LSP_Completion_Manager;
      Context : Completion_Context) return Completion_List is
   begin
      return Null_Completion_List;
   end Get_Initial_Completion_List;

   ---------------------------
   -- Query_Completion_List --
   ---------------------------

   overriding procedure Query_Completion_List
     (Manager : access LSP_Completion_Manager;
      Context : Completion_Context;
      Win     : access Completion_Display_Interface'Class)
   is
      pragma Unreferenced (Win);

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
      Request        : LSP_Completion_Request_Access := new
        LSP_Completion_Request'(LSP_Request with
                                Resolver      => Resolver,
                                Text_Document => File,
                                Line          => Line_Information
                                  (Editor_Context),
                                Column        => Column_Information
                                  (Editor_Context),
                                Kernel        => Resolver.Kernel,
                                Context       => Deep_Copy (Context));
   begin
      Resolver.Completions.items.Clear;

      Trace (Advanced_Me, "queriying completions...");

      GPS.LSP_Client.Requests.Execute
        (Lang,
         GPS.LSP_Client.Requests.Request_Access (Request));
   end Query_Completion_List;

   -------------------------
   -- Get_Completion_Root --
   -------------------------

   overriding procedure Get_Completion_Root
     (Resolver   : access LSP_Completion_Resolver;
      Offset     : String_Index_Type;
      Context    : Completion_Context;
      Result     : in out Completion_List)
   is
      pragma Unreferenced (Offset);
      use Glib;

      File : constant Virtual_File := Get_File (Context);
      Loc  : Editor_Location'Class :=
                   Get_Current_Location (Resolver.Kernel, File);

      function Unichar_To_UTF8 (Char : Glib.Gunichar) return String;
      function Unichar_To_UTF8 (Char : Glib.Gunichar) return String is
         The_Char   : String (1 .. 6);
         Last       : Natural;
      begin
         Unichar_To_UTF8 (Char, The_Char, Last);
         return The_Char (1 .. Last);
      end Unichar_To_UTF8;

   begin
      --  Find the prefix of the word

      declare
         Unichar    : Glib.Gunichar;
         Prefix     : Unbounded_String;
      begin
         loop
            Loc := Loc.Forward_Char (-1);
            Unichar := Glib.Gunichar (Loc.Get_Char);

            --  Exit when we are out of an identifier, eg. the current char is
            --  neither an alphanumeric character, neither an underscore

            exit when not
              (Is_Alnum (Unichar) or else Unichar = Character'Pos ('_'));

            Insert (Prefix, 1, Unichar_To_UTF8 (Unichar));

            --  Exit here if we are on the beginning of the buffer

            exit when Loc.Offset = 0;
         end loop;
      end;
   end Get_Completion_Root;

   ------------------------------------
   -- LSP_Completion_Manager_Factory --
   ------------------------------------

   function LSP_Completion_Manager_Factory
     (Kernel : not null GPS.Kernel.Kernel_Handle;
      File   : GNATCOLL.VFS.Virtual_File;
      Lang   : Language.Language_Access) return Completion_Manager_Access
   is
      pragma Unreferenced (File);
      Manager  : Completion_Manager_Access;
      Resolver : Completion_Resolver_Access;
   begin
      if LSP_Is_Enabled (Lang) then
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

   --------------
   -- Register --
   --------------

   procedure Register (Kernel : Kernel_Handle) is
      pragma Unreferenced (Kernel);
   begin
      if Me.Is_Active then
         Completion_Module.Set_Completion_Manager_Factory
           (Factory => LSP_Completion_Manager_Factory'Access);
      end if;
   end Register;

end GPS.LSP_Client.Completion;
