------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2019-2023, AdaCore                   --
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

with GNATCOLL.JSON;
with GNATCOLL.Scripts;           use GNATCOLL.Scripts;
with GNATCOLL.Scripts.VSS_Utils; use GNATCOLL.Scripts.VSS_Utils;
with GNATCOLL.VFS;

with Language;
with GPS.Kernel.Scripts;
with GPS.LSP_Client.Language_Servers;
with GPS.LSP_Client.Requests;
with GPS.LSP_Client.Requests.Shell;
with GPS.LSP_Module;

with VSS.Strings.Conversions;

package body GPS.LSP_Client.Shell is

   procedure Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Implementation of LSPRequest scripting class.

   LanguageServer_Class_Name   : constant String := "LanguageServer";
   Get_By_Language_Name_Method : constant String := "get_by_language_name";
   Get_By_Language_Info_Method : constant String := "get_by_language_info";
   Get_By_File_Method          : constant String := "get_by_file";
   Restart_Method              : constant String := "restart";
   Request_Low_Level_Method    : constant String := "request_low_level";
   Get_Log_File_Method         : constant String := "get_log_file";
   Get_Requests_Method         : constant String := "get_requests";

   Is_Enabled_Name_Method      : constant String :=
     "is_enabled_for_language_name";

   LanguageServer_Class : GNATCOLL.Scripts.Class_Type;

   type LanguageServer_Properties_Record is
     new GNATCOLL.Scripts.Instance_Property_Record with record
      Language : VSS.Strings.Virtual_String;
   end record;
   type LanguageServer_Properties_Access is
     access all LanguageServer_Properties_Record'Class;

   procedure Set_Data (Instance : Class_Instance; Language : String);
   --  Set data for scripting object

   function Get_Language
     (Instance : Class_Instance) return Language.Language_Access;
   --  Resolve and return language information for given class instance

   ---------------------
   -- Command_Handler --
   ---------------------

   procedure Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Instance : Class_Instance;

   begin
      if Command = Constructor_Method then
         Data.Set_Error_Msg ("Cannot build instances of LanguageServer.");

      elsif Command = Get_By_File_Method then
         declare
            File : constant GNATCOLL.VFS.Virtual_File :=
                     GPS.Kernel.Scripts.Get_Data (Nth_Arg (Data, 1));

         begin
            Instance := New_Instance (Data.Get_Script, LanguageServer_Class);
            Set_Data
              (Instance,
               GPS.Kernel.Scripts.Get_Kernel
                 (Data).Get_Language_Handler.Get_Language_From_File
                   (File).Get_Name);
            Set_Return_Value (Data, Instance);
         end;

      elsif Command = Get_By_Language_Info_Method then
         declare
            Info : constant Language.Language_Access :=
                     GPS.Kernel.Scripts.Get_Language_Info (Nth_Arg (Data, 1));

         begin
            Instance := New_Instance (Data.Get_Script, LanguageServer_Class);
            Set_Data (Instance, Info.Get_Name);
            Set_Return_Value (Data, Instance);
         end;

      elsif Command = Get_By_Language_Name_Method then
         declare
            Name : constant String := Nth_Arg (Data, 1);

         begin
            Instance := New_Instance (Data.Get_Script, LanguageServer_Class);
            Set_Data (Instance, Name);
            Set_Return_Value (Data, Instance);
         end;

      elsif Command = Restart_Method then
         declare
            Instance : constant Class_Instance := Nth_Arg (Data, 1);
            Language : constant Standard.Language.Language_Access :=
              Get_Language (Instance);
            use type GPS.LSP_Client.Language_Servers.Language_Server_Access;
            Server   : constant
              GPS.LSP_Client.Language_Servers.Language_Server_Access :=
                GPS.LSP_Module.Get_Language_Server (Language);
         begin
            if Server = null then
               return;
            end if;
            GPS.LSP_Module.Restart_Server (Server);
         end;

      elsif Command = Get_Log_File_Method then
         declare
            Instance : constant Class_Instance := Nth_Arg (Data, 1);
            Language : constant Standard.Language.Language_Access :=
              Get_Language (Instance);
            use type GPS.LSP_Client.Language_Servers.Language_Server_Access;
            Server   : constant
              GPS.LSP_Client.Language_Servers.Language_Server_Access :=
                GPS.LSP_Module.Get_Language_Server (Language);
         begin
            if Server = null then
               return;
            end if;

            Data.Set_Return_Value
              (GPS.Kernel.Scripts.Create_File
                 (Data.Get_Script,
                  Server.Get_Client.Get_Standard_Errors_File));
         end;

      elsif Command = Request_Low_Level_Method then
         declare
            use type Language.Language_Access;

            Instance    : constant Class_Instance := Nth_Arg (Data, 1);
            Language    : constant Standard.Language.Language_Access :=
                          Get_Language (Instance);
            Method      : constant VSS.Strings.Virtual_String :=
              Nth_Arg (Data, 2);
            Params      : constant VSS.Strings.Virtual_String :=
              Nth_Arg (Data, 3);
            On_Result   : constant Subprogram_Type := Nth_Arg (Data, 4);
            Auto_Cancel : constant Boolean := Nth_Arg (Data, 7, False);
            On_Error    : Subprogram_Type;
            On_Reject   : Subprogram_Type;

            Aux         : GPS.LSP_Client.Requests.Request_Access;

         begin
            begin
               On_Error := Nth_Arg (Data, 5);

            exception
               when No_Such_Parameter =>
                  On_Error := null;
            end;

            begin
               On_Reject := Nth_Arg (Data, 6);

            exception
               when No_Such_Parameter =>
                  On_Reject := null;
            end;

            Aux :=
              new GPS.LSP_Client.Requests.Shell.Shell_Request'
                (GPS.LSP_Client.Requests.LSP_Request with
                 Kernel            => null,
                 Method            => Method,
                 Params            =>
                   GNATCOLL.JSON.Read
                     (VSS.Strings.Conversions.To_Unbounded_UTF_8_String
                        (Params)),
                 On_Result_Message => On_Result,
                 On_Error_Message  => On_Error,
                 On_Rejected       => On_Reject,
                 Auto_Canceled     => Auto_Cancel);

            if Language /= null then
               GPS.LSP_Client.Requests.Execute (Language, Aux);

            else
               Aux.On_Rejected (GPS.LSP_Client.Requests.Server_Not_Ready);
               GPS.LSP_Client.Requests.Destroy (Aux);
            end if;
         end;

      elsif Command = Is_Enabled_Name_Method then
         declare
            use type Language.Language_Access;

            Name     : constant String := Nth_Arg (Data, 1);
            Language : constant Standard.Language.Language_Access :=
              GPS.Kernel.Scripts.Get_Kernel (Data).
              Get_Language_Handler.Get_Language_By_Name
                (Ada.Characters.Handling.To_Lower (Name));
            Result : Boolean := False;

         begin
            if Language /= null then
               Result := GPS.LSP_Module.LSP_Is_Enabled (Language);
            end if;
            Set_Return_Value (Data, Result);
         end;
      elsif Command = Get_Requests_Method then
         declare
            Instance : constant Class_Instance := Nth_Arg (Data, 1);
            Language : constant Standard.Language.Language_Access :=
              Get_Language (Instance);
            use type GPS.LSP_Client.Language_Servers.Language_Server_Access;
            use type GPS.LSP_Client.Requests.Request_Access;
            Server   : constant
              GPS.LSP_Client.Language_Servers.Language_Server_Access :=
                GPS.LSP_Module.Get_Language_Server (Language);
         begin
            if Server = null then
               return;
            end if;
            Set_Return_Value_As_List (Data);
            for R of Server.Get_Client.Get_Requests loop
               if R /= null then
                  declare
                     Name : constant String :=
                       VSS.Strings.Conversions.To_UTF_8_String (R.Method);
                  begin
                     Set_Return_Value (Data, Name);
                  end;
               end if;
            end loop;
         end;
      end if;
   end Command_Handler;

   ------------------
   -- Get_Language --
   ------------------

   function Get_Language
     (Instance : Class_Instance) return Language.Language_Access
   is
      Properties : LanguageServer_Properties_Access;

   begin
      if Instance /= No_Class_Instance then
         Properties :=
           LanguageServer_Properties_Access
             (Instance_Property'
                (Get_Data (Instance, LanguageServer_Class_Name)));

         if Properties /= null then
            return
              GPS.Kernel.Scripts.Get_Kernel (Get_Script (Instance))
                .Get_Language_Handler.Get_Language_By_Name
                  (VSS.Strings.Conversions.To_UTF_8_String
                     (Properties.Language));
         end if;
      end if;

      return null;
   end Get_Language;

   -----------------------
   -- Register_Commands --
   -----------------------

   procedure Register_Commands
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      LanguageServer_Class :=
        GPS.Kernel.Scripts.New_Class (Kernel, LanguageServer_Class_Name);

      Kernel.Scripts.Register_Command
        (Command => Constructor_Method,
         Params  => No_Params,
         Handler => Command_Handler'Access,
         Class   => LanguageServer_Class);

      Kernel.Scripts.Register_Command
        (Command       => Get_By_File_Method,
         Params        => (1 .. 1 => Param ("file")),
         Handler       => Command_Handler'Access,
         Class         => LanguageServer_Class,
         Static_Method => True);

      Kernel.Scripts.Register_Command
        (Command       => Get_By_Language_Info_Method,
         Params        => (1 .. 1 => Param ("language")),
         Handler       => Command_Handler'Access,
         Class         => LanguageServer_Class,
         Static_Method => True);

      Kernel.Scripts.Register_Command
        (Command       => Get_By_Language_Name_Method,
         Params        => (1 .. 1 => Param ("language")),
         Handler       => Command_Handler'Access,
         Class         => LanguageServer_Class,
         Static_Method => True);

      Kernel.Scripts.Register_Command
        (Command => Restart_Method,
         Handler => Command_Handler'Access,
         Class   => LanguageServer_Class);

      Kernel.Scripts.Register_Command
        (Command => Request_Low_Level_Method,
         Params  =>
           (Param ("method"),
            Param ("params"),
            Param ("on_result_message"),
            Param ("on_error_message", True),
            Param ("on_rejected", True),
            Param ("auto_cancel", True)),
         Handler => Command_Handler'Access,
         Class   => LanguageServer_Class);

      Kernel.Scripts.Register_Command
        (Command => Get_Log_File_Method,
         Params  => No_Params,
         Handler => Command_Handler'Access,
         Class   => LanguageServer_Class);

      Kernel.Scripts.Register_Command
        (Command       => Get_Requests_Method,
         Params        => No_Params,
         Handler       => Command_Handler'Access,
         Class         => LanguageServer_Class);

      Kernel.Scripts.Register_Command
        (Command       => Is_Enabled_Name_Method,
         Params        => (1 .. 1 => Param ("language")),
         Handler       => Command_Handler'Access,
         Class         => LanguageServer_Class,
         Static_Method => True);
   end Register_Commands;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data (Instance : Class_Instance; Language : String) is
   begin
      Set_Data
        (Instance => Instance,
         Name     => LanguageServer_Class_Name,
         Property =>
           LanguageServer_Properties_Record'
             (Language =>
                  VSS.Strings.Conversions.To_Virtual_String (Language)));
   end Set_Data;

end GPS.LSP_Client.Shell;
