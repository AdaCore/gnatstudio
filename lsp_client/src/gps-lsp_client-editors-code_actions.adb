------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2021-2022, AdaCore                  --
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

with Glib.Convert; use Glib.Convert;

with Basic_Types;  use Basic_Types;
with Commands;     use Commands;

with Language;                      use Language;
with Language_Handlers;             use Language_Handlers;
with GPS.Editors; use GPS.Editors;
with GPS.LSP_Client.Requests.Code_Action;
with GPS.LSP_Client.Utilities;      use GPS.LSP_Client.Utilities;

with Refactoring.Code_Actions;
with GPS.LSP_Client.Editors.Code_Actions.Dialog;
use GPS.LSP_Client.Editors.Code_Actions.Dialog;

package body GPS.LSP_Client.Editors.Code_Actions is

   -------------------------
   -- Code_Action_Request --
   -------------------------

   --  This part handles the emission of the textDocument/codeAction request
   --  and the processing of its results.

   type Code_Action_Request is
     new GPS.LSP_Client.Requests.Code_Action.Abstract_Code_Action_Request with
    record
       Lang : Language_Access;
    end record;

   type Code_Action_Request_Access is access all Code_Action_Request'Class;

   overriding function Is_Request_Supported
     (Self    : Code_Action_Request;
      Options : LSP.Messages.ServerCapabilities)
      return Boolean is (Options.codeActionProvider.Is_Set);

   overriding procedure On_Result_Message
     (Self   : in out Code_Action_Request;
      Result : LSP.Messages.CodeAction_Vector);

   -------------------------
   -- Code_Action_Command --
   -------------------------

   --  This part defines the Code_Action_Commands, which get executed when
   --  an user activates the menu for a code action at a given location.

   type Code_Action_Command is new Root_Command with record
      Kernel  : Kernel_Handle;
      Lang    : Language_Access;
      Command : LSP.Messages.Command (Is_Unknown => True);
   end record;
   overriding function Execute
     (Command : access Code_Action_Command) return Command_Return_Type;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Code_Action_Command) return Command_Return_Type is
      Request : Execute_Command_Request_Access;
   begin
      Request :=
        new Execute_Command_Request'
          (LSP_Request with Kernel => Command.Kernel,
           Params =>
             (Is_Unknown => True, Base => (workDoneToken => (Is_Set => False)),
              command => Command.Command.command,
              arguments => Command.Command.arguments));

      Execute_Request_Via_Dialog
        (Kernel  => Command.Kernel,
         Lang    => Command.Lang,
         Request => Request);
      return Success;
   end Execute;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self   : in out Code_Action_Request;
      Result : LSP.Messages.CodeAction_Vector)
   is
      Buffer : constant Editor_Buffer'Class :=
        Self.Kernel.Get_Buffer_Factory.Get
          (File            => Self.Text_Document,
           Force           => False,
           Open_Buffer     => False,
           Open_View       => False,
           Focus           => False);
      Command : Command_Access;
   begin
      Refactoring.Code_Actions.Invalidate_Code_Actions (Self.Kernel);

      if Result.Is_Empty
        or else Buffer = Nil_Editor_Buffer
        or else Buffer.Version /= Self.Document_Version
      then
         return;
      end if;

      for Code_Action of Result loop
         if Code_Action.command.Is_Set then
            declare
               Start_Location : constant GPS.Editors.Editor_Location'Class :=
                 LSP_Position_To_Location (Buffer, Self.Start_Position);
            begin
               Command := new Code_Action_Command'
                 (Root_Command with Self.Kernel,
                  Self.Lang, Code_Action.command.Value);

               Refactoring.Code_Actions.Add_Code_Action
                 (Kernel => Self.Kernel,
                  File   => Self.Text_Document,
                  Line   => Editable_Line_Type (Start_Location.Line),
                  Column => Start_Location.Column,
                  Markup => Escape_Text
                    (VSS.Strings.Conversions.To_UTF_8_String (
                     if Code_Action.command.Value.title.Is_Empty
                     then Code_Action.title
                     else Code_Action.command.Value.title)),
                  Command => Command);
            end;
         end if;
      end loop;

   end On_Result_Message;

   -------------------------
   -- Request_Code_Action --
   -------------------------

   procedure Request_Code_Action
     (Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File)
   is
      Buffer : constant Editor_Buffer'Class :=
        Kernel.Get_Buffer_Factory.Get
          (File            => File,
           Force           => False,
           Open_Buffer     => False,
           Open_View       => False,
           Focus           => False,
           Only_If_Focused => True);
      Lang               : constant Language.Language_Access :=
        Get_Language_From_File
          (Kernel.Get_Language_Handler,
           File);
      Request  : Code_Action_Request_Access;
   begin
      --  Sanity check
      if Buffer = Nil_Editor_Buffer then
         return;
      end if;

      --  Craft the request
      declare
         Loc_Start : constant Editor_Location'Class := Buffer.Selection_Start;
         Loc_End   : constant Editor_Location'Class := Buffer.Selection_End;
      begin
         --  The cursor or the selection bound might be in the special lines:
         --  in this case, it will have a Line of 0; in this case, do not
         --  emit a request for code actions.
         if Loc_Start.Line = 0
           or else Loc_End.Line = 0
         then
            return;
         end if;

         declare
            Start_Position : constant LSP.Messages.Position :=
              Location_To_LSP_Position (Loc_Start);
            End_Position   : constant LSP.Messages.Position :=
              Location_To_LSP_Position (Loc_End);
         begin
            Request := new Code_Action_Request'
              (LSP_Request with
               Kernel           => Kernel_Handle (Kernel),
               Lang             => Lang,
               Text_Document    => File,
               Start_Position   => Start_Position,
               End_Position     => End_Position,
               Document_Version => Buffer.Version);

            GPS.LSP_Client.Requests.Execute (Lang, Request_Access (Request));
         end;
      end;
   end Request_Code_Action;

end GPS.LSP_Client.Editors.Code_Actions;
