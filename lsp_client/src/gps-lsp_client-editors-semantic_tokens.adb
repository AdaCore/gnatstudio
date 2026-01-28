------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2019-2026, AdaCore                  --
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

with Interfaces;

with GNATCOLL.Traces;         use GNATCOLL.Traces;
with GNATCOLL.JSON;
with GNATCOLL.Xref;
with GNATCOLL.Utils;

with Glib.Main;               use Glib.Main;
with Glib.Values;

with Gtk.Handlers;
with Gtk.Label;
with Gtk.Separator;

with VSS.String_Vectors;
with VSS.Strings.Conversions;

with GPS.Kernel.Hooks;        use GPS.Kernel.Hooks;
with GPS.Kernel.Modules;      use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;
with GPS.Kernel.Style_Manager;
with GPS.Editors;             use GPS.Editors;

with LSP.Types;

with GPS.LSP_Module;
with GPS.LSP_Client.Language_Servers;
with GPS.LSP_Client.Requests; use GPS.LSP_Client.Requests;

with GPS.LSP_Client.Requests.SemanticTokens_Full;
with GPS.LSP_Client.Requests.SemanticTokens_Range;

package body GPS.LSP_Client.Editors.Semantic_Tokens is

   Me : constant Trace_Handle := Create ("GPS.LSP.SEMANTIC_TOKENS", On);

   LSP_Deprecated_Style_Name : constant String := "deprecated";
   --  Used as a depricated style name

   Semantic_Tooltip_Box_Name : constant String := "Semantic_Box_Tooltip";
   --  Used as a name for semantic box in tooltip

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
       Abstract_SemanticTokens_Range_Request
   with record
      Vbox_Destroyed_Handler_ID : Gtk.Handlers.Handler_Id;
      --  Handler ID waiting for tooltip's box destruction
      Vbox                      : Gtk_Vbox;
      --  Vbox in tooltip that displays semantic information
      Column                    : Basic_Types.Visible_Column_Type := 0;
      --  Column in editor where tooltip is triggered
   end record;
   type SemanticTokens_Range_Request_Access is
     access all SemanticTokens_Range_Request'Class;

   overriding procedure On_Result_Message
     (Self   : in out SemanticTokens_Range_Request;
      Result : LSP.Messages.SemanticTokens);

   overriding procedure On_Error_Message
     (Self    : in out SemanticTokens_Range_Request;
      Code    : LSP.Messages.ErrorCodes;
      Message : VSS.Strings.Virtual_String;
      Data    : GNATCOLL.JSON.JSON_Value);

   overriding procedure On_Rejected
     (Self : in out SemanticTokens_Range_Request; Reason : Reject_Reason);

   package Tooltip_Destroyed_Callback is new Gtk.Handlers.User_Callback
     (Widget_Type  => Gtk_Widget_Record,
      User_Type    => SemanticTokens_Range_Request_Access);

   procedure On_Tooltip_Destroyed
     (Widget    : access Gtk_Widget_Record'Class;
      Params    : Glib.Values.GValues;
      User_Data : SemanticTokens_Range_Request_Access);
   pragma Warnings (Off, On_Tooltip_Destroyed);
   --  Called when the tooltip that shows the current semantic token gets
   --  detroyed while waiting for the request result.

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

   procedure Send_Request
     (Kernel  : not null access Kernel_Handle_Record'Class;
      Data    : Request_Data;
      Request : out GPS.LSP_Client.Requests.Request_Access;
      Is_Sent : out Boolean);
   --  Send full or range request based on the Data
   --  Is_Sent set to true when the request is successfully sent
   --  Request is the created request when Is_Set=True

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
               if Index = localVariable
                 or else Index = globalVariable
               then
                  M := "-" & Correct_Name
                    (SemanticTokenModifiers'Image (Index)) & M;
               else
                  M.Append
                    ("-" & Correct_Name
                       (SemanticTokenModifiers'Image (Index)));
               end if;
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
               if M (Index)
                 and then Index /= localVariable
                 and then Index /= globalVariable
               then
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
      Request : GPS.LSP_Client.Requests.Request_Access;
      Result  : Boolean;
   begin
      if LSP_Semantic_Highlighting.Get_Pref then
         Send_Request
           (Kernel  => Kernel,
            Data    => (File, 0, 0),
            Request => Request,
            Is_Sent => Result);

         if not Result then
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

      Request : GPS.LSP_Client.Requests.Request_Access;
      Result  : Boolean;

   begin
      if LSP_Semantic_Highlighting.Get_Pref then
         Send_Request (Kernel, (File, From_Line, To_Line), Request, Result);
         if not Result then
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

      Server  : GPS.LSP_Client.Language_Servers.Language_Server_Access;
      Max     : Positive;
      R       : Result_Type;
      Legend  : SemanticTokensLegend;
      Request : GPS.LSP_Client.Requests.Request_Access;
      Result  : Boolean;

   begin
      if Module.Get_Kernel.Is_In_Destruction then
         Module.Idle_ID := Glib.Main.No_Source_Id;
         return False;
      end if;

      while not Module.Postponed.Is_Empty loop
         Send_Request
           (Kernel  => Module.Get_Kernel,
            Data    => Module.Postponed.First_Element,
            Request => Request,
            Is_Sent => Result);

         if not Result then
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
      Result : LSP.Messages.SemanticTokens)
   is
      use type Basic_Types.Visible_Column_Type;

      procedure Fill_Semantic_Tooltip;
      --  Fills semantic tooltip with data

      ---------------------------
      -- Fill_Semantic_Tooltip --
      ---------------------------

      procedure Fill_Semantic_Tooltip
      is
         use VSS.Strings;

         use type GPS.LSP_Client.Language_Servers.Language_Server_Access;
         use type uinteger;
         use type Interfaces.Unsigned_32;
         use type Gtk.Label.Gtk_Label;

         Server          : GPS.LSP_Client.Language_Servers.
           Language_Server_Access;
         Legend          : SemanticTokensLegend;
         Index, M_Index  : Integer  := 1;
         Line, Prev_Line : Positive := 1;
         Char, Prev_Char : Basic_Types.Visible_Column_Type := 1;
         UInt            : Interfaces.Unsigned_32;
         Txt             : VSS.String_Vectors.Virtual_String_Vector;
      begin
         Server := GPS.LSP_Module.Get_Language_Server
           (Module.Get_Kernel.Get_Language_Handler.
              Get_Language_From_File (Self.File));

         if Server = null then
            return;
         end if;

         declare
            Buffer : constant Editor_Buffer'Class :=
              Module.Get_Kernel.Get_Buffer_Factory.Get
                (Self.File, Open_Buffer => False, Open_View => False);
         begin
            if Buffer = Nil_Editor_Buffer then
               return;
            end if;

            Legend := Server.Get_Client.Capabilities.
              semanticTokensProvider.Value.legend;

            while Index < Natural (Result.data.Length) loop
               --  iterate over response lines to find the line we need
               --   (line for which the tooltip is triggered)

               Line := Prev_Line + Natural (Result.data.Element (Index));

               if Line = Self.From + 1 then
                  --  Tooltip's line has been found: add +1 since 
                  --  LSP lines are 0-based.

                  if Line = Prev_Line then
                     --  relative char position
                     Char := Prev_Char + Visible_Column_Type
                       (Result.data.Element (Index + 1));
                  else
                     Char := Visible_Column_Type
                       (Result.data.Element (Index + 1) + 1);
                  end if;

                  --  Check that the current tocken is for needed position
                  --  e.g. starts before the needed column and snds after
                  if Char <= Self.Column
                    and then Char + Visible_Column_Type
                      (Result.data.Element (Index + 2) - 1) >= Self.Column
                  then
                     --  Found the corresponding to the location tocken

                     --  Add Semantic type information
                     Txt.Append
                       (VSS.Strings.Conversions.To_Virtual_String
                          ("Semantic type: ") &
                          Legend.tokenTypes.Element
                          (Natural (Result.data.Element (Index + 3)) + 1));

                     UInt := Interfaces.Unsigned_32
                       (Result.data.Element (Index + 4));

                     --  Test if we have modifiers
                     if UInt /= 0 then
                        Txt.Append ("Modifiers:");

                        --  Iterate over modifiers
                        while UInt /= 0 loop
                           if (UInt and 1) = 1 then
                              --  Modifier bit is set, add information about
                              --  this modifier
                              Txt.Append
                                (VSS.Strings.Conversions.To_Virtual_String
                                   ("          ") &
                                   Legend.tokenModifiers.Element (M_Index));
                           end if;

                           --  shift (discard) current bit to handle next one
                           UInt := Interfaces.Shift_Right (UInt, 1);
                           exit when UInt = 0; --  no more modifiers

                           M_Index := M_Index + 1;
                        end loop;
                     end if;
                  end if;
               end if;

               exit when Line > Self.From + 1; --  we passsed over the line

               Prev_Line := Line;
               Prev_Char := Char;
               Index     := Index + 5;
            end loop;
         end;

         if not Txt.Is_Empty then
            --  We collect some information, add it to the tooltip box
            declare
               L    : Gtk.Label.Gtk_Label;
               Hsep : Gtk.Separator.Gtk_Hseparator;
            begin
               Gtk.Label.Gtk_New (L);
               L.Set_Use_Markup (False);
               L.Set_Alignment (0.0, 0.0);
               L.Set_Single_Line_Mode (False);
               L.Set_Text
                 (VSS.Strings.Conversions.To_UTF_8_String
                    (Txt.Join_Lines (CR, False)));
               Self.Vbox.Pack_Start (L, False, False);

               Gtk.Separator.Gtk_New_Hseparator (Hsep);
               Self.Vbox.Pack_End (Hsep, False, False);
               Self.Vbox.Show_All;
            end;
         end if;
      end Fill_Semantic_Tooltip;

   begin
      if Self.Column = 0 then
         --  Column is 0 so it is highlight request

         Module.Data.Append
           (Result_Type'((Self.File, Self.From, Self.To), Result.data));

         if Module.Idle_ID = Glib.Main.No_Source_Id then
            --  register Idle
            Module.Idle_ID := Glib.Main.Idle_Add (On_Idle'Access);
         end if;

      elsif Self.Vbox /= null then
         --  Column is not 0 so it is semantic tooltip request and we still
         --  have Vbox (tooltip was not closed)

         Gtk.Handlers.Disconnect
           (Object => Self.Vbox,
            Id     => Self.Vbox_Destroyed_Handler_ID);

         Fill_Semantic_Tooltip;
      end if;
   end On_Result_Message;

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out SemanticTokens_Range_Request;
      Code    : LSP.Messages.ErrorCodes;
      Message : VSS.Strings.Virtual_String;
      Data    : GNATCOLL.JSON.JSON_Value) is
   begin
      if Self.Vbox /= null then
         Gtk.Handlers.Disconnect
           (Object => Self.Vbox,
            Id     => Self.Vbox_Destroyed_Handler_ID);
      end if;

      GPS.LSP_Client.Requests.SemanticTokens_Range.On_Error_Message
        (Self    => GPS.LSP_Client.Requests.SemanticTokens_Range.
           Abstract_SemanticTokens_Range_Request (Self),
         Code    => Code,
         Message => Message,
         Data    => Data);
   end On_Error_Message;

   -----------------
   -- On_Rejected --
   -----------------

   overriding procedure On_Rejected
     (Self : in out SemanticTokens_Range_Request; Reason : Reject_Reason) is
   begin
      if Self.Vbox /= null then
         Gtk.Handlers.Disconnect
           (Object => Self.Vbox,
            Id     => Self.Vbox_Destroyed_Handler_ID);
      end if;

      GPS.LSP_Client.Requests.SemanticTokens_Range.On_Rejected
        (Self   => GPS.LSP_Client.Requests.SemanticTokens_Range.
           Abstract_SemanticTokens_Range_Request (Self),
         Reason => Reason);
   end On_Rejected;

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

   procedure Send_Request
     (Kernel  : not null access Kernel_Handle_Record'Class;
      Data    : Request_Data;
      Request : out GPS.LSP_Client.Requests.Request_Access;
      Is_Sent : out Boolean)
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
         return Boolean is
      begin
         Request := new SemanticTokens_Full_Request'
           (LSP_Request with
              Kernel => Kernel_Handle (Kernel),
            File   => File);

         if Request.Is_Request_Supported (Server.Get_Client.Capabilities) then
            return GPS.LSP_Client.Requests.Execute
              (Kernel.Get_Language_Handler.Get_Language_From_File (File),
               Request_Access (Request));
         else
            --  Not supported, do not send
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
         return Boolean is
      begin
         Request := new SemanticTokens_Range_Request'
           (LSP_Request with
              Kernel => Kernel_Handle (Kernel),
            File   => File,
            From   => From_Line,
            To     => To_Line,
            others => <>);

         if Request.Is_Request_Supported (Server.Get_Client.Capabilities) then
            return GPS.LSP_Client.Requests.Execute
              (Kernel.Get_Language_Handler.Get_Language_From_File (File),
               Request_Access (Request));
         else
            --  Not supported, do not send
            return True;
         end if;
      end Send_Range_Request;

   begin
      Request := null;
      Is_Sent := False;
      Server  := GPS.LSP_Module.Get_Language_Server
        (Module.Get_Kernel.Get_Language_Handler.
           Get_Language_From_File (Data.File));

      if Server /= null then
         if Server.Get_Client.Is_Ready then
            if Data.From = 0 then
               Is_Sent := Send_Full_Request (Kernel, Data.File);
            else
               Is_Sent := Send_Range_Request
                 (Kernel, Data.File, Data.From, Data.To);
            end if;

         else
            Is_Sent := False;
         end if;

      else
         Is_Sent := True;
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

   -----------------------------------
   -- Create_Semantic_Token_Tooltip --
   -----------------------------------

   procedure Create_Semantic_Token_Tooltip
     (Tooltip_Hbox : Gtk_Hbox;
      File         : GNATCOLL.VFS.Virtual_File;
      Line         : Integer;
      Column       : Visible_Column_Type)
   is
      Request : GPS.LSP_Client.Requests.Request_Access;
      Result  : Boolean;
   begin
      if GPS.Kernel.Preferences.LSP_Show_Semantic_Tooltip.Get_Pref then
         Send_Request
           (Kernel  => Module.Get_Kernel,
            Data    => (File, Line - 1, Line - 1),
            Request => Request,
            Is_Sent => Result);

         if Result
           and then Request /= null
         then
            declare
               R : constant SemanticTokens_Range_Request_Access :=
                 SemanticTokens_Range_Request_Access (Request);
            begin
               R.Column := Column;

               Gtk_New_Vbox (R.Vbox, Homogeneous => False);
               R.Vbox.Set_Name (Semantic_Tooltip_Box_Name);
               R.Vbox.Set_Hexpand (False);
               R.Vbox.Set_Spacing (0);

               Tooltip_Hbox.Pack_Start (R.Vbox, False, False);

               R.Vbox_Destroyed_Handler_ID :=
                 Tooltip_Destroyed_Callback.Object_Connect
                   (Widget      => R.Vbox,
                    Name        => Signal_Destroy,
                    Cb          => On_Tooltip_Destroyed'Access,
                    Slot_Object => R.Vbox,
                    User_Data   => R);
            end;
         end if;
      end if;
   end Create_Semantic_Token_Tooltip;

   --------------------------
   -- On_Tooltip_Destroyed --
   --------------------------

   procedure On_Tooltip_Destroyed
     (Widget    : access Gtk_Widget_Record'Class;
      Params    : Glib.Values.GValues;
      User_Data : SemanticTokens_Range_Request_Access)
   is
      pragma Unreferenced (Widget, Params);
   begin
      if User_Data /= null then
         User_Data.Vbox := null;
      end if;
   end On_Tooltip_Destroyed;

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
