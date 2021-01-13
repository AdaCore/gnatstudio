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

with Ada.Strings.Unbounded;           use Ada.Strings.Unbounded;

with GNAT.OS_Lib;                     use GNAT.OS_Lib;
with GNAT.Strings;

with GNATCOLL.JSON;
with GNATCOLL.Projects;               use GNATCOLL.Projects;
with GNATCOLL.Traces;                 use GNATCOLL.Traces;
with GNATCOLL.Utils;
with GNATCOLL.VFS;                    use GNATCOLL.VFS;
with GNATCOLL.Xref;

with Gtk.Widget;

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

with GPS.LSP_Client.Requests.Symbols;
with GPS.LSP_Client.Editors.Tooltips;

package body GPS.LSP_Client.Search.Entities is

   Me_Search_Entities_Support : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create
       ("GPS.LSP.SEARCH_ENTITIES_SUPPORT", GNATCOLL.Traces.Off);

   type Entities_Search_Provider is new Kernel_Search_Provider with record
      Pattern : Search_Pattern_Access; --  Do not free
      File    : Virtual_File := No_File;

      Results : LSP.Messages.SymbolInformation_Vector;
      --  Results received from all the servers
      Waiting : Integer;
      --  Count of the responses we are waiting for

      Position : Integer := 1;
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

   type Current_File_Entities_Search_Provider is
     new Entities_Search_Provider with null record;

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

   type Entity_Search_Result is new Kernel_Search_Result with record
      File   : Virtual_File;
      Line   : Integer;
      Column : GNATCOLL.Xref.Visible_Column;
   end record;
   overriding procedure Execute
     (Self       : not null access Entity_Search_Result;
      Give_Focus : Boolean);
   overriding function Full
     (Self : not null access Entity_Search_Result)
      return Gtk.Widget.Gtk_Widget;

   -- Symbol_Request --

   type Symbol_Request is
     new GPS.LSP_Client.Requests.Symbols.Abstract_Symbol_Request
   with record
      Provider : Entities_Search_Provider_Access;
      Id       : Integer;
   end record;

   overriding procedure On_Result_Message
     (Self   : in out Symbol_Request;
      Result : LSP.Messages.SymbolInformation_Vector);

   overriding procedure On_Error_Message
     (Self    : in out Symbol_Request;
      Code    : LSP.Messages.ErrorCodes;
      Message : String;
      Data    : GNATCOLL.JSON.JSON_Value);

   overriding procedure On_Rejected (Self : in out Symbol_Request);

   Request_Id : Integer := 0;

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

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self       : not null access Entity_Search_Result;
      Give_Focus : Boolean) is
   begin
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
      return Self.Waiting < 1;
   end Is_Result_Ready;

   ----------
   -- Next --
   ----------

   overriding procedure Next
     (Self     : not null access Entities_Search_Provider;
      Result   : out GPS.Search.Search_Result_Access;
      Has_Next : out Boolean)
   is
      use Ada.Strings.Unbounded;

      Context : GPS.Search.Search_Context;
      Short   : Unbounded_String;
      Long    : GNAT.Strings.String_Access;
      File    : Virtual_File;
      Column  : GNATCOLL.Xref.Visible_Column;
   begin
      Result   := null;
      Has_Next := True;

      if Self.Position <= Integer (Self.Results.Length) then
         declare
            Info : constant LSP.Messages.SymbolInformation :=
              Self.Results.Element (Self.Position);
         begin
            File := GPS.LSP_Client.Utilities.To_Virtual_File
              (Info.location.uri);

            if Self.File = No_File
              or else Self.File = File
            then
               Short   := To_Unbounded_String (To_UTF_8_String (Info.name));
               Context := Self.Pattern.Start (To_String (Short));

               if Context /= GPS.Search.No_Match then
                  Column := GPS.LSP_Client.Utilities.
                    UTF_16_Offset_To_Visible_Column
                      (Info.location.span.first.character);

                  Long := new String'
                    (File.Display_Base_Name
                     & ":" & GNATCOLL.Utils.Image
                       (Integer (Info.location.span.first.line) + 1,
                        Min_Width => 0)
                     & ":"
                     & GNATCOLL.Utils.Image
                       (Integer (Column), Min_Width => 0));

                  Result := new Entity_Search_Result'
                    (Kernel   => Self.Kernel,
                     Provider => Self,
                     Score    => Context.Score,
                     Short    => new String'
                       (Self.Pattern.Highlight_Match
                            (To_String (Short), Context => Context)),
                     Long     => Long,
                     Id       => new String'
                       (To_String (Short) & ":" & Long.all),
                     File     => File,
                     Line     => Integer (Info.location.span.first.line) + 1,
                     Column   => Column);

                  --  Matches in runtime files should get a lower score, so
                  --  that we first list those matches in user code. "10" is
                  --  similar to what is done for filenames, so that in fuzzy
                  --  matching this correpsonds to having characters separated
                  --  by 9 others

                  declare
                     use GNATCOLL.Projects;

                     Inf : constant File_Info'Class :=
                       File_Info'Class
                         (Get_Project_Tree (Self.Kernel.all).Info_Set
                          (File).First_Element);
                  begin
                     if Inf.Project (Root_If_Not_Found => False) =
                       No_Project
                     then
                        Result.Score := Result.Score - 10;
                     end if;
                  end;

                  Self.Adjust_Score (Result);
               end if;
            end if;
         end;

         Self.Position := Self.Position + 1;

      elsif Self.Waiting < 1 then
         --  we processed all responses
         Has_Next := False;
      end if;
   end Next;

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out Symbol_Request;
      Code    : LSP.Messages.ErrorCodes;
      Message : String;
      Data    : GNATCOLL.JSON.JSON_Value) is
   begin
      if Request_Id = Self.Id then
         Self.Provider.Waiting := Self.Provider.Waiting - 1;
      end if;
   end On_Error_Message;

   -----------------
   -- On_Rejected --
   -----------------

   overriding procedure On_Rejected (Self : in out Symbol_Request) is
   begin
      if Request_Id = Self.Id then
         Self.Provider.Waiting := Self.Provider.Waiting - 1;
      end if;
   end On_Rejected;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self   : in out Symbol_Request;
      Result : LSP.Messages.SymbolInformation_Vector) is
   begin
      if Request_Id = Self.Id then
         Self.Provider.Waiting := Self.Provider.Waiting - 1;
         Self.Provider.Results.Append (Result);
      end if;
   end On_Result_Message;

   -----------------
   -- Set_Pattern --
   -----------------

   overriding procedure Set_Pattern
     (Self    : not null access Entities_Search_Provider;
      Pattern : not null access GPS.Search.Search_Pattern'Class;
      Limit   : Natural := Natural'Last)
   is
      pragma Unreferenced (Limit);

      Lang : Language.Language_Access;
   begin
      if Request_Id < Integer'Last then
         Request_Id := Request_Id + 1;
      else
         Request_Id := 1;
      end if;

      Self.Results.Clear;
      Self.Waiting  := 0;
      Self.Position := 1;

      if Pattern.Get_Text = "" then
         return;
      end if;

      Self.Pattern := Search_Pattern_Access (Pattern);
      declare
         Languages : GNAT.Strings.String_List :=
           Root_Project (Self.Kernel.Get_Project_Tree.all).Languages (True);
      begin
         for Index in Languages'Range loop
            Lang := Self.Kernel.Get_Language_Handler.Get_Language_By_Name
              (Languages (Index).all);
            if GPS.LSP_Module.LSP_Is_Enabled (Lang) then
               declare
                  Request : GPS.LSP_Client.Requests.Request_Access;
               begin
                  Request := new Symbol_Request'
                    (GPS.LSP_Client.Requests.LSP_Request with
                     Provider => Entities_Search_Provider_Access (Self),
                     Id       => Request_Id,
                     Kernel   => Self.Kernel,
                     Query    =>
                       (if Self.Pattern.Get_Kind = Full_Text
                        then To_LSP_String (Pattern.Get_Text)
                        else Empty_LSP_String));

                  if GPS.LSP_Client.Requests.Execute (Lang, Request) then
                     Self.Waiting := Self.Waiting + 1;
                  end if;
               end;
            end if;
            Free (Languages (Index));
         end loop;
      end;
   end Set_Pattern;

   -----------------
   -- Set_Pattern --
   -----------------

   overriding procedure Set_Pattern
     (Self    : not null access Current_File_Entities_Search_Provider;
      Pattern : not null access GPS.Search.Search_Pattern'Class;
      Limit   : Natural := Natural'Last) is
   begin
      Entities_Search_Provider (Self.all).Set_Pattern
        (Pattern => Pattern,
         Limit   => Limit);

      Self.File := Self.Kernel.Get_Buffer_Factory.Get
        (Open_Buffer => False,
         Open_View   => False).File;
   end Set_Pattern;

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
