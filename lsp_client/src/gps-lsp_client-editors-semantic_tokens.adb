------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2019-2023, AdaCore                  --
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

with Ada.Characters;
with Ada.Characters.Handling;
with Ada.Containers.Indefinite_Vectors;

with Ada.Strings.Unbounded;
with GNATCOLL.Utils;
with Interfaces;
with VSS.Strings.Conversions;

with Glib.Main;               use Glib.Main;

with GNATCOLL.Traces;         use GNATCOLL.Traces;
with GNATCOLL.VFS;            use GNATCOLL.VFS;
with GNATCOLL.Xref;

with Basic_Types;
with GPS.Kernel.Hooks;        use GPS.Kernel.Hooks;
with GPS.Kernel.Modules;      use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;
with GPS.Kernel.Style_Manager;
with GPS.Editors;             use GPS.Editors;

with GPS.LSP_Module;
with GPS.LSP_Client.Language_Servers;
with GPS.LSP_Client.Requests; use GPS.LSP_Client.Requests;

with GPS.LSP_Client.Requests.SemanticTokens_Full;
with GPS.LSP_Client.Requests.SemanticTokens_Range;

with LSP.Types;

package body GPS.LSP_Client.Editors.Semantic_Tokens is

   Me : constant Trace_Handle := Create ("GPS.LSP.SEMANTIC_TOKENS", On);

   LSP_Deprecated_Style_Name : constant String := "deprecated";
   --  Used as a depricated style name

   --------------------
   -- Hooks_Function --
   --------------------

   type On_File_Edited_Or_Reloaded is new File_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_File_Edited_Or_Reloaded;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File);
   --  Used to highlight code.

   type On_Highlight_Range is new Highlight_Hooks_Function with null record;
   overriding procedure Execute
     (Self      : On_Highlight_Range;
      Kernel    : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      File      : GNATCOLL.VFS.Virtual_File;
      From_Line : Integer;
      To_Line   : Integer);
   --  Highlight piece of code between From_Line and To_Line in a buffer
   --  corresponding to given File.

   type On_Clear_Highlighting is new File_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Clear_Highlighting;
      Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File);
   --  Clear highlighting styles for the given file

   --------------------------------------
   -- Semantic_Tokens_Module_ID_Record --
   --------------------------------------

   type Request_Data is record
      File : GNATCOLL.VFS.Virtual_File;
      From : Integer := 0;
      To   : Integer := 0;
   end record;

   type Result_Type is record
      Request : Request_Data;
      Data    : LSP.Messages.uinteger_Vector;
   end record;
   --  Store result data

   package Request_Data_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, Request_Data);

   package Result_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, Result_Type);

   type Semantic_Tokens_Module_ID_Record is
     new Module_ID_Record
   with record
      Postponed : Request_Data_Vectors.Vector;
      --  Store requests data that did not send

      Data      : Result_Vectors.Vector;
      --  To store results

      Idle_ID   : Glib.Main.G_Source_Id := Glib.Main.No_Source_Id;
      --  To store Idle id

      --  Indexes that are used for parsing results
      Index     : Positive := 1;
      Prev_Line : Positive := 1;
      Prev_Char : Basic_Types.Visible_Column_Type := 1;
   end record;

   overriding procedure Destroy (Id : in out Semantic_Tokens_Module_ID_Record);

   type Semantic_Tokens_Module_ID is
     access all Semantic_Tokens_Module_ID_Record'Class;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy
     (Id : in out Semantic_Tokens_Module_ID_Record) is
   begin
      Id.Postponed.Clear;
      Id.Data.Clear;

      if Id.Idle_ID /= Glib.Main.No_Source_Id then
         Remove (Id.Idle_ID);
         Id.Idle_ID := Glib.Main.No_Source_Id;
      end if;
   end Destroy;

   ---------------------------------
   -- SemanticTokens_Full_Request --
   ---------------------------------

   type SemanticTokens_Full_Request is
     new GPS.LSP_Client.Requests.SemanticTokens_Full.
       Abstract_SemanticTokens_Full_Request with null record;

   overriding procedure On_Result_Message
     (Self   : in out SemanticTokens_Full_Request;
      Result : LSP.Messages.SemanticTokens);

   ----------------------------------
   -- SemanticTokens_Range_Request --
   ----------------------------------

   type SemanticTokens_Range_Request is
     new GPS.LSP_Client.Requests.SemanticTokens_Range.
       Abstract_SemanticTokens_Range_Request with null record;

   overriding procedure On_Result_Message
     (Self   : in out SemanticTokens_Range_Request;
      Result : LSP.Messages.SemanticTokens);

   procedure Clear_Indexes;
   --  Clear module indexes to start parsing results from the beginning

   function On_Idle return Boolean;
   --  Called on Idle to parse results

   procedure Remove_Highlighting_On_Lines
     (Buffer : GPS.Editors.Editor_Buffer'Class;
      From   : Integer;
      To     : Integer);
   --  Remove highlighting from the buffer

   function Get_Style_Name (T : LSP.Messages.SemanticTokenTypes) return String;
   --  Convert SemanticTokenTypes to the style name. Returns empty string if
   --  corresponding style name does not exist.

   function Get_Style
     (Token_Type      : LSP.Messages.uinteger;
      Token_Modifiers : LSP.Messages.uinteger;
      Legend          : LSP.Messages.SemanticTokensLegend)
      return String;
   --  Convert Token_Type to the style name. Returns empty string if
   --  corresponding style name does not exist.

   type Modifiers_Array is array (SemanticTokenModifiers) of Boolean;
   Empty_Modifiers_Array : constant Modifiers_Array := (others => False);

   function Check_Style_Name
     (Name      : String;
      Modifiers : Modifiers_Array := Empty_Modifiers_Array)
      return String;
   --  Returns empty string if style name does not exist.

   function Send_Request
     (Kernel : not null access Kernel_Handle_Record'Class;
      Data   : Request_Data)
      return Boolean;
   --  Send full or range request based on the Data

   procedure Store_Request (Data : Request_Data);
   --  Store postponed request for resending in future

   Module : Semantic_Tokens_Module_ID;

   Max_Count_On_Idle : constant := 50000; -- 1000(items) * 5(length)
   --  Maximum tokens parsed in one Idle cycle

   Styles_To_Clear : constant array (1 .. 13) of SemanticTokenTypes :=
     (1  => namespace,
      2  => class,
      3  => enum,
      4  => an_interface,
      5  => struct,
      6  => typeParameter,
      7  => parameter,
      8  => variable,
      9  => property,
      10 => enumMember,
      11 => a_function,
      12 => modifier,
      13 => operator);
   --  Styles that should be removed before applying new highliting

   ----------------------
   -- Check_Style_Name --
   ----------------------

   function Check_Style_Name
     (Name      : String;
      Modifiers : Modifiers_Array := Empty_Modifiers_Array)
      return String
   is
      use GPS.Kernel.Style_Manager;

      function Correct_Name (Value : String) return String;
      function Check (Value : String) return String;
      function Get_Modifiers (Modifiers : Modifiers_Array) return String;

      Manager : constant Style_Manager_Access :=
        Get_Style_Manager (Module.Get_Kernel);

      -----------
      -- Check --
      -----------

      function Check (Value : String) return String is
      begin
         if Manager.Get (Value, True) /= null then
            return Value;
         else
            return "";
         end if;
      end Check;

      ------------------
      -- Correct_Name --
      ------------------

      function Correct_Name (Value : String) return String is
         N : constant String := Ada.Characters.Handling.To_Lower (Value);
      begin
         if N'Length > 2
           and then N (N'First .. N'First + 1) = "a_"
         then
            return N (N'First + 2 .. N'Last);

         elsif N'Length > 3
           and then N (N'First .. N'First + 2) = "an_"
         then
            return N (N'First + 3 .. N'Last);

         else
            return N;
         end if;
      end Correct_Name;

      -------------------
      -- Get_Modifiers --
      -------------------

      function Get_Modifiers (Modifiers : Modifiers_Array) return String
      is
         use Ada.Strings.Unbounded;

         M : Unbounded_String;
      begin
         for Index in SemanticTokenModifiers'Range loop
            if Modifiers (Index) then
               M.Append
                 ("-" & Correct_Name (SemanticTokenModifiers'Image (Index)));
            end if;
         end loop;

         return To_String (M);
      end Get_Modifiers;

      Type_Name           : constant String := Correct_Name (Name);
      Type_Modifiers_Name : constant String :=
        Type_Name & Get_Modifiers (Modifiers);

   begin
      if Check (Type_Modifiers_Name) /= "" then
         --  style has been found
         return Type_Modifiers_Name;

      else
         --  we don't have type style with modificators,
         --  try to reduce modificators that are used
         declare
            M : Modifiers_Array := Modifiers;
         begin
            for Index in reverse SemanticTokenModifiers'Range loop
               if M (Index) then
                  M (Index) := False;

                  declare
                     Curr : constant String :=
                       Type_Name & Get_Modifiers (M);

                  begin
                     if Check (Curr) /= "" then
                        return Curr;
                     end if;
                  end;
               end if;
            end loop;
         end;

         return Check (Type_Name);
      end if;
   end Check_Style_Name;

   -------------------
   -- Clear_Indexes --
   -------------------

   procedure Clear_Indexes is
   begin
      Module.Index     := 1;
      Module.Prev_Line := 1;
      Module.Prev_Char := 1;
   end Clear_Indexes;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_File_Edited_Or_Reloaded;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File)
   is
      use GPS.Kernel.Preferences;
   begin
      if Use_External_Highlighting.Get_Pref = GPS.Kernel.Preferences.LSP then
         if not Send_Request (Kernel, (File, 0, 0)) then
            Store_Request ((File, 0, 0));
         end if;
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self      : On_Highlight_Range;
      Kernel    : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      File      : GNATCOLL.VFS.Virtual_File;
      From_Line : Integer;
      To_Line   : Integer)
   is
      pragma Unreferenced (Self);
      use GPS.Kernel.Preferences;

   begin
      if Use_External_Highlighting.Get_Pref = GPS.Kernel.Preferences.LSP then
         if not Send_Request (Kernel, (File, From_Line, To_Line)) then
            Store_Request ((File, From_Line, To_Line));
         end if;
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Clear_Highlighting;
      Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File)
   is
      Buffer : constant Editor_Buffer'Class :=
        Kernel.Get_Buffer_Factory.Get
          (File, Open_Buffer => False, Open_View => False);
   begin
      if Buffer /= Nil_Editor_Buffer then
         Remove_Highlighting_On_Lines (Buffer, 0, 0);
      end if;
   end Execute;

   ---------------------------
   -- Get_Supported_Options --
   ---------------------------

   function Get_Supported_Options
     return Optional_SemanticTokensClientCapabilities
   is

      function Get_Supported_Token_Types
        return LSP.Messages.SemanticTokenTypes_Vector;

      function Get_Supported_Token_Modifiers
        return LSP.Messages.SemanticTokenModifiers_Vector;

      -------------------------------
      -- Get_Supported_Token_Types --
      -------------------------------

      function Get_Supported_Token_Types
        return LSP.Messages.SemanticTokenTypes_Vector
      is
         use LSP.Messages;

         Result : SemanticTokenTypes_Vector;
      begin
         for Item in SemanticTokenTypes'Range loop
            if Item /= event  --  skip unsupported
              and then Item /= method
              and then Item /= macro
              and then Item /= regexp
            then
               Result.Append (Item);
            end if;
         end loop;

         return Result;
      end Get_Supported_Token_Types;

      -----------------------------------
      -- Get_Supported_Token_Modifiers --
      -----------------------------------

      function Get_Supported_Token_Modifiers
        return LSP.Messages.SemanticTokenModifiers_Vector
      is
         use LSP.Messages;

         Result : SemanticTokenModifiers_Vector;

      begin
         for Item in SemanticTokenModifiers'Range loop
            --  skip unsupported
            if Item /= async then
               Result.Append (Item);
            end if;
         end loop;

         return Result;
      end Get_Supported_Token_Modifiers;

      Formats : constant LSP.Messages.TokenFormatSet :=
        LSP.Messages.TokenFormatSet
          (LSP.Messages.TokenFormatSets.To_Set
             (LSP.Messages.relative, LSP.Messages.relative));

   begin
      return
        (Is_Set => True,
         Value  =>
           (requests       =>
                (span => (Is_Set => True, Value => True),
                 full =>
                   (Is_Set => True,
                    Value  =>
                      (diff => (Is_Set => False)))),
            tokenTypes     => Get_Supported_Token_Types,
            tokenModifiers => Get_Supported_Token_Modifiers,
            formats        => Formats,
            others => <>));
   end Get_Supported_Options;

   --------------------
   -- Get_Style_Name --
   --------------------

   function Get_Style_Name (T : LSP.Messages.SemanticTokenTypes) return String
   is
   begin
      return Check_Style_Name (LSP.Messages.SemanticTokenTypes'Image (T));
   end Get_Style_Name;

   ---------------
   -- Get_Style --
   ---------------

   function Get_Style
     (Token_Type      : LSP.Messages.uinteger;
      Token_Modifiers : LSP.Messages.uinteger;
      Legend          : LSP.Messages.SemanticTokensLegend)
      return String
   is
      use Interfaces;

      Modifiers : Modifiers_Array := Empty_Modifiers_Array;
      UInt      : Unsigned_32 := Unsigned_32 (Token_Modifiers);
      Index     : Positive := 1;
   begin
      while UInt /= 0 loop
         if (UInt and 1) = 1 then
            declare
               N : constant String :=
                 VSS.Strings.Conversions.To_UTF_8_String
                   (Legend.tokenModifiers.Element (Index));
            begin
               if N = "abstract" then
                  Modifiers (an_abstract) := True;
               else
                  Modifiers (SemanticTokenModifiers'Value (N)) := True;
               end if;
            end;
         end if;

         UInt  := Shift_Right (UInt, 1);
         Index := Index + 1;
      end loop;

      if Modifiers (deprecated) then
         return LSP_Deprecated_Style_Name;

      else
         return Check_Style_Name
           (VSS.Strings.Conversions.To_UTF_8_String
              (Legend.tokenTypes.Element (Natural (Token_Type) + 1)),
            Modifiers);
      end if;
   end Get_Style;

   -------------
   -- On_Idle --
   -------------

   function On_Idle return Boolean
   is
      use LSP.Types;
      use LSP.Messages;
      use Result_Vectors;
      use type GPS.LSP_Client.Language_Servers.Language_Server_Access;

      Server : GPS.LSP_Client.Language_Servers.Language_Server_Access;
      Max    : Positive;
      R      : Result_Type;
      Legend : SemanticTokensLegend;

   begin
      if Module.Get_Kernel.Is_In_Destruction then
         Module.Idle_ID := Glib.Main.No_Source_Id;
         return False;
      end if;

      while not Module.Postponed.Is_Empty loop
         if not Send_Request
           (Module.Get_Kernel, Module.Postponed.First_Element)
         then
            return True;
         else
            Module.Postponed.Delete_First;
         end if;
      end loop;

      if Module.Data.Is_Empty then
         Clear_Indexes;
         Module.Idle_ID := Glib.Main.No_Source_Id;
         return False;
      end if;

      R := Module.Data.First_Element;

      Server := GPS.LSP_Module.Get_Language_Server
        (Module.Get_Kernel.Get_Language_Handler.
           Get_Language_From_File (R.Request.File));

      if Server = null then
         Module.Data.Clear;
         Clear_Indexes;
         Module.Idle_ID := Glib.Main.No_Source_Id;

         return False;
      end if;

      Legend := Server.Get_Client.Capabilities.
        semanticTokensProvider.Value.legend;

      declare
         Buffer : constant Editor_Buffer'Class :=
           Module.Get_Kernel.Get_Buffer_Factory.Get
             (R.Request.File, Open_Buffer => False, Open_View => False);

         -------------
         -- Process --
         -------------

         procedure Process
           (Delta_Line      : uinteger;
            Delta_Start     : uinteger;
            Length          : uinteger;
            Token_Type      : uinteger;
            Token_Modifiers : uinteger);

         procedure Process
           (Delta_Line      : uinteger;
            Delta_Start     : uinteger;
            Length          : uinteger;
            Token_Type      : uinteger;
            Token_Modifiers : uinteger)
         is
            use Basic_Types;
            use type GNATCOLL.Xref.Visible_Column;

            Line : constant Integer := Module.Prev_Line + Natural (Delta_Line);
            Char : Visible_Column_Type;
            Len  : Visible_Column_Type;
         begin
            if Line = Module.Prev_Line then
               --  relative char position
               Char := Module.Prev_Char + Visible_Column_Type (Delta_Start);
            else
               Char := Visible_Column_Type (Delta_Start + 1);
            end if;

            declare
               Name : constant String :=
                 Get_Style (Token_Type, Token_Modifiers, Legend);
            begin
               if Name /= "" then
                  Len := Char + Visible_Column_Type (Length);
                  Buffer.Apply_Style (Name, Line, Char, Len);
               end if;
            end;

            Module.Prev_Line := Line;
            Module.Prev_Char := Char;

         exception
            when E : others =>
               Trace (Me, E);
         end Process;

      begin
         if Buffer = Nil_Editor_Buffer then
            Module.Data.Delete_First;
            Clear_Indexes;
            return True;
         end if;

         if Module.Index = 1 then
            Remove_Highlighting_On_Lines
              (Buffer, R.Request.From, R.Request.To);
         end if;

         Max := Module.Index + Max_Count_On_Idle;
         while Module.Index <= Natural'Min (Max, Natural (R.Data.Length)) loop
            Process
              (Delta_Line      => R.Data.Element (Module.Index),
               Delta_Start     => R.Data.Element (Module.Index + 1),
               Length          => R.Data.Element (Module.Index + 2),
               Token_Type      => R.Data.Element (Module.Index + 3),
               Token_Modifiers => R.Data.Element (Module.Index + 4));

            Module.Index := Module.Index + 5;
         end loop;
      end;

      if Module.Index > Natural (R.Data.Length) then
         Module.Data.Delete_First;
         Clear_Indexes;
      end if;

      if Module.Data.Is_Empty then
         Module.Idle_ID := Glib.Main.No_Source_Id;
         return False;

      else
         return True;
      end if;
   end On_Idle;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self   : in out SemanticTokens_Full_Request;
      Result : LSP.Messages.SemanticTokens)
   is
      use Result_Vectors;
      Idx : Positive := 1;
   begin
      --  delete previouse results for the same file
      while Idx <= Natural (Module.Data.Length) loop
         if Module.Data.Element (Idx).Request.File = Self.File then
            Module.Data.Delete (Idx);
            if Idx = 1 then
               Clear_Indexes;
            end if;
         else
            Idx := Idx + 1;
         end if;
      end loop;

      --  store new result
      Module.Data.Append (Result_Type'((Self.File, 0, 0), Result.data));

      if Module.Idle_ID = Glib.Main.No_Source_Id then
         --  register Idle
         Module.Idle_ID := Glib.Main.Idle_Add (On_Idle'Access);
      end if;
   end On_Result_Message;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self   : in out SemanticTokens_Range_Request;
      Result : LSP.Messages.SemanticTokens) is
   begin
      Module.Data.Append
        (Result_Type'((Self.File, Self.From, Self.To), Result.data));

      if Module.Idle_ID = Glib.Main.No_Source_Id then
         --  register Idle
         Module.Idle_ID := Glib.Main.Idle_Add (On_Idle'Access);
      end if;
   end On_Result_Message;

   ----------------------------------
   -- Remove_Highlighting_On_Lines --
   ----------------------------------

   procedure Remove_Highlighting_On_Lines
     (Buffer : GPS.Editors.Editor_Buffer'Class;
      From   : Integer;
      To     : Integer)
   is
      function In_Clear (Token : SemanticTokenTypes) return Boolean;
      --  SemanticTokenType is in the Styles_To_Clear list, so we should
      --  clear the buffer from this style

      procedure Remove (Name : String);
      --  Call Remove_Style_On_Lines for the given style name

      --------------
      -- In_Clear --
      --------------

      function In_Clear (Token : SemanticTokenTypes) return Boolean is
      begin
         for Index in Styles_To_Clear'Range loop
            if Token = Styles_To_Clear (Index) then
               return True;
            end if;
         end loop;

         return False;
      end In_Clear;

      ------------
      -- Remove --
      ------------

      procedure Remove (Name : String) is
      begin
         if From = 0 then
            Buffer.Remove_Style (Name, 0);

         else
            Buffer.Remove_Style_On_Lines
              (Name,
               Basic_Types.Editable_Line_Type (From),
               Basic_Types.Editable_Line_Type (To));
         end if;
      end Remove;

      Styles : constant GPS.Kernel.Style_Manager.Style_Vector.Vector :=
        GPS.Kernel.Style_Manager.Get_Style_Manager
          (Module.Get_Kernel).List_Styles;

   begin
      Remove (LSP_Deprecated_Style_Name);

      for J in SemanticTokenTypes'Range loop
         declare
            N : constant String := Get_Style_Name (J);
         begin
            if N /= "" then
               for Style of Styles loop
                  if GNATCOLL.Utils.Starts_With
                    (Style.Get_Name,
                     N & (if In_Clear (J) then "" else "-"))
                  then
                     Remove (Style.Get_Name);
                  end if;
               end loop;
            end if;
         end;
      end loop;
   end Remove_Highlighting_On_Lines;

   ------------------
   -- Send_Request --
   ------------------

   function Send_Request
     (Kernel : not null access Kernel_Handle_Record'Class;
      Data   : Request_Data)
      return Boolean
   is
      use type GPS.LSP_Client.Language_Servers.Language_Server_Access;

      Server : GPS.LSP_Client.Language_Servers.Language_Server_Access;

      function Send_Full_Request
        (Kernel : not null access Kernel_Handle_Record'Class;
         File   : Virtual_File)
      return Boolean;
      --  Send textDocument/semanticTokens/full request

      function Send_Range_Request
        (Kernel    : not null access Kernel_Handle_Record'Class;
         File      : Virtual_File;
         From_Line : Integer;
         To_Line   : Integer)
      return Boolean;
      --  Send textDocument/semanticTokens/range request

      -----------------------
      -- Send_Full_Request --
      -----------------------

      function Send_Full_Request
        (Kernel : not null access Kernel_Handle_Record'Class;
         File   : Virtual_File)
         return Boolean
      is
         Request : GPS.LSP_Client.Requests.Request_Access :=
           new SemanticTokens_Full_Request'
             (LSP_Request with
              Kernel => Kernel_Handle (Kernel),
              File   => File);
      begin
         if Request.Is_Request_Supported (Server.Get_Client.Capabilities) then
            return GPS.LSP_Client.Requests.Execute
              (Kernel.Get_Language_Handler.Get_Language_From_File (File),
               Request_Access (Request));
         else
            --  Don't supported, do not resend
            return True;
         end if;
      end Send_Full_Request;

      ------------------------
      -- Send_Range_Request --
      ------------------------

      function Send_Range_Request
        (Kernel    : not null access Kernel_Handle_Record'Class;
         File      : Virtual_File;
         From_Line : Integer;
         To_Line   : Integer)
         return Boolean
      is
         Request : GPS.LSP_Client.Requests.Request_Access :=
           new SemanticTokens_Range_Request'
             (LSP_Request with
              Kernel => Kernel_Handle (Kernel),
              File   => File,
              From   => From_Line,
              To     => To_Line);
      begin
         if Request.Is_Request_Supported (Server.Get_Client.Capabilities) then
            return GPS.LSP_Client.Requests.Execute
              (Kernel.Get_Language_Handler.Get_Language_From_File (File),
               Request_Access (Request));
         else
            --  Don't supported, do not resend
            return True;
         end if;
      end Send_Range_Request;

   begin
      Server := GPS.LSP_Module.Get_Language_Server
        (Module.Get_Kernel.Get_Language_Handler.
           Get_Language_From_File (Data.File));

      if Server /= null then
         if Server.Get_Client.Is_Ready then
            if Data.From = 0 then
               return Send_Full_Request (Kernel, Data.File);
            else
               return Send_Range_Request
                 (Kernel, Data.File, Data.From, Data.To);
            end if;

         else
            return False;
         end if;

      else
         return True;
      end if;
   end Send_Request;

   -------------------
   -- Store_Request --
   -------------------

   procedure Store_Request (Data : Request_Data) is
   begin
      Module.Postponed.Append (Data);

      if Module.Idle_ID = Glib.Main.No_Source_Id then
         Module.Idle_ID := Glib.Main.Idle_Add (On_Idle'Access);
      end if;
   end Store_Request;

   --------------
   -- Register --
   --------------

   procedure Register (Kernel : Kernel_Handle) is
   begin
      Module := new Semantic_Tokens_Module_ID_Record;

      Register_Module
        (Module      => Module,
         Kernel      => Kernel,
         Module_Name => "LSP_Semantic_Tokens");

      File_Edited_Hook.Add        (new On_File_Edited_Or_Reloaded);
      File_Reloaded_Hook.Add      (new On_File_Edited_Or_Reloaded);
      Highlight_Range_Hook.Add    (new On_Highlight_Range);
      Clear_Highlighting_Hook.Add (new On_Clear_Highlighting);
   end Register;

end GPS.LSP_Client.Editors.Semantic_Tokens;
