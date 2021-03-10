------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2021, AdaCore                       --
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

with LSP.Types;    use LSP.Types;

with Basic_Types;  use Basic_Types;
with Commands;     use Commands;

with Language;                      use Language;
with Language_Handlers;             use Language_Handlers;
with GPS.Editors; use GPS.Editors;
with GPS.LSP_Client.Requests.Code_Action;
with GPS.LSP_Client.Requests.Execute_Command;
with GPS.LSP_Client.Requests;       use GPS.LSP_Client.Requests;

with Refactoring.Code_Actions;

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

   -----------------------------
   -- Execute_Command_Request --
   -----------------------------

   --  This part handles the emission of the textDocument/codeAction request
   --  and the processing of its results.

   type Execute_Command_Request is new
     GPS.LSP_Client.Requests.Execute_Command.Abstract_Execute_Command_Request
   with record
      Params : LSP.Messages.ExecuteCommandParams (True);
   end record;
   type Execute_Command_Request_Access is
     access all Execute_Command_Request'Class;

   overriding function Params
     (Self : Execute_Command_Request)
      return LSP.Messages.ExecuteCommandParams is (Self.Params);

   overriding procedure On_Result_Message
     (Self : in out Execute_Command_Request) is null;

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
     (Command : access Code_Action_Command)
      return Command_Return_Type
   is
      Request : Execute_Command_Request_Access;
   begin
      Request := new Execute_Command_Request'
        (LSP_Request with
           Kernel => Command.Kernel,
         Params => (Is_Unknown => True,
                    Base       => (workDoneToken => (Is_Set => False)),
                    command    => Command.Command.command,
                    arguments  => Command.Command.arguments));

      GPS.LSP_Client.Requests.Execute (Command.Lang, Request_Access (Request));
      return Success;
   end Execute;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self   : in out Code_Action_Request;
      Result : LSP.Messages.CodeAction_Vector)
   is
      Command : Command_Access;
   begin
      if Result.Is_Empty then
         return;
      end if;

      for Code_Action of Result loop
         if Code_Action.command.Is_Set then
            Command := new Code_Action_Command'
              (Root_Command with Self.Kernel,
               Self.Lang, Code_Action.command.Value);

            Refactoring.Code_Actions.Add_Code_Action
              (Kernel => Self.Kernel,
               File   => Self.Text_Document,
               Line   => Editable_Line_Type (Self.Line_Start),
               Column => Self.Column_Start,
               Markup => Escape_Text
                 (To_UTF_8_String (
                  if Code_Action.command.Value.title /= Empty_LSP_String then
                     Code_Action.command.Value.title
                  else
                     Code_Action.title)),
               Command => Command);
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

         Request := new Code_Action_Request'
           (LSP_Request with
            Kernel        => Kernel_Handle (Kernel),
            Lang          => Lang,
            Text_Document => File,
            Line_Start    => Loc_Start.Line,
            Column_Start  => Loc_Start.Column,
            Line_End      => Loc_End.Line,
            Column_End    => Loc_End.Column);

         GPS.LSP_Client.Requests.Execute (Lang, Request_Access (Request));
      end;
   end Request_Code_Action;

end GPS.LSP_Client.Editors.Code_Actions;
