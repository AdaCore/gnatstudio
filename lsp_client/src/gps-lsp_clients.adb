------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2018-2019, AdaCore                     --
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

with Ada.Strings.UTF_Encoding;
with GNAT.OS_Lib;

with GNATCOLL.Arg_Lists; use GNATCOLL.Arg_Lists;
with GNATCOLL.JSON;
with GNATCOLL.Traces;    use GNATCOLL.Traces;

with GPS.Editors;
with GPS.Kernel.Project;
with Language;

with Spawn.String_Vectors;

package body GPS.LSP_Clients is

   Me : constant Trace_Handle := Create ("GPS.LSP_CLIENT");

   function "+" (Text : Ada.Strings.UTF_Encoding.UTF_8_String)
     return LSP.Types.LSP_String renames
       LSP.Types.To_LSP_String;

   procedure Process_Command_Queue (Self : in out LSP_Client'Class);

   ------------------------------
   -- Did_Change_Text_Document --
   ------------------------------

   not overriding procedure Did_Change_Text_Document
     (Self : in out LSP_Client;
      File : GNATCOLL.VFS.Virtual_File) is
      pragma Unreferenced (Self, File);
   begin
      null;
   end Did_Change_Text_Document;

   -----------------------------
   -- Did_Close_Text_Document --
   -----------------------------

   not overriding procedure Did_Close_Text_Document
     (Self : in out LSP_Client;
      File : GNATCOLL.VFS.Virtual_File) is
      pragma Unreferenced (Self, File);
   begin
      null;
   end Did_Close_Text_Document;

   ----------------------------
   -- Did_Open_Text_Document --
   ----------------------------

   not overriding procedure Did_Open_Text_Document
     (Self    : in out LSP_Client;
      Handler : not null
        GPS.LSP_Client.Text_Document_Handlers.Text_Document_Handler_Access)
   is
   begin
      Self.Text_Document_Handlers.Insert (Handler.File, Handler);
      Self.Open_File (Handler.File);
   end Did_Open_Text_Document;

   -------------------------
   -- Initialize_Response --
   -------------------------

   overriding procedure Initialize_Response
     (Self     : not null access Response_Handler;
      Request  : LSP.Types.LSP_Number;
      Response : LSP.Messages.Initialize_Response)
   is
      pragma Unreferenced (Request);
   begin
      Self.Client.Server_Capabilities := Response.result.capabilities;
      Self.Client.Is_Ready := True;
      Self.Client.Initialized;
      Process_Command_Queue (Self.Client.all);
   end Initialize_Response;

   --------------
   -- On_Error --
   --------------

   overriding procedure On_Error
     (Self  : in out LSP_Client;
      Error : String) is
   begin
      Me.Trace ("On_Error:" & Error);
      Self.Is_Ready := False;
   end On_Error;

   ----------------
   -- On_Started --
   ----------------

   overriding procedure On_Started (Self : in out LSP_Client) is
      Root    : constant String :=
                  String
                    (GNATCOLL.VFS.Filesystem_String'
                       (GPS.Kernel.Project.Get_Project
                          (Self.Kernel).Project_Path.Dir_Name));
      --  ??? Root directory of the project is directoy where
      --  project file is stored.
      Id      : LSP.Types.LSP_Number;
      My_PID  : constant Integer :=
                  GNAT.OS_Lib.Pid_To_Integer (GNAT.OS_Lib.Current_Process_Id);
      Request : constant LSP.Messages.InitializeParams :=
                  (processId    => (True, My_PID),
                   rootPath     => +Root,
                   rootUri      => +("file://" & Root),
                   capabilities =>
                     (workspace => (applyEdit => LSP.Types.False,
                                       others    => <>),
                      textDocument => <>),
                   trace        => LSP.Types.Unspecified);

   begin
      Self.Initialize_Request (Id, Request);
   end On_Started;

   ---------------
   -- Open_File --
   ---------------

   not overriding procedure Open_File
     (Self : in out LSP_Client;
      File : GNATCOLL.VFS.Virtual_File) is
   begin
      if Self.Is_Ready then
         declare
            Factory : constant GPS.Editors.Editor_Buffer_Factory_Access :=
              Self.Kernel.Get_Buffer_Factory;
            Buffer : constant GPS.Editors.Editor_Buffer'Class := Factory.Get
              (File        => File,
               Open_Buffer => True,
               Open_View   => False);
            Lang : constant not null Language.Language_Access :=
              Buffer.Get_Language;
            Name  : constant GNATCOLL.VFS.Filesystem_String := File.Full_Name;
            Value : constant LSP.Messages.DidOpenTextDocumentParams :=
              (textDocument =>
                 (uri        => +("file://" & String (Name)),
                  languageId => +Lang.Get_Name,
                  version    => 0,
                  text       => +Buffer.Get_Chars));
         begin
            Self.Text_Document_Did_Open (Value);
         end;
      else
         Self.Commands.Append
           ((Open_File, Self.Text_Document_Handlers (File)));
      end if;
   end Open_File;

   ---------------------------
   -- Process_Command_Queue --
   ---------------------------

   procedure Process_Command_Queue (Self : in out LSP_Client'Class) is
      Next : Command;
   begin
      while not Self.Commands.Is_Empty loop
         Next := Self.Commands.First_Element;

         case Next.Kind is
            when Open_File =>
               Self.Open_File (Next.Handler.File);
         end case;

         Self.Commands.Delete_First;
      end loop;
   end Process_Command_Queue;

   -----------
   -- Start --
   -----------

   not overriding procedure Start
     (Self : aliased in out LSP_Client;
      Cmd  : GNATCOLL.Arg_Lists.Arg_List)
   is
      Args : Spawn.String_Vectors.UTF_8_String_Vector;
   begin
      Self.Set_Response_Handler (Self.Response_Handler'Unchecked_Access);

      for J in 1 .. Args_Length (Cmd) loop
         Args.Append (Nth_Arg (Cmd, J));
      end loop;

      --  TODO: Use Find_In_Path
      Self.Set_Program (Nth_Arg (Cmd, 0));
      Self.Set_Arguments (Args);
      --  TODO: Self.Set_Environment
      --  TODO: Self.Set_Working_Directory
      Me.Trace ("Start: " & To_Display_String (Cmd));
      Self.Start;
   end Start;

end GPS.LSP_Clients;
