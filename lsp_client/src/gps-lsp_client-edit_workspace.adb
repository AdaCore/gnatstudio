------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2019-2021, AdaCore                   --
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

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Strings.UTF_Encoding;

with GNATCOLL.VFS;               use GNATCOLL.VFS;

with VSS.Unicode;

with Gtk.Stock;

with GPS.Editors;                use GPS.Editors;
with GPS.Kernel.Messages;
with GPS.Kernel.Messages.Markup;
with GPS.Kernel.Messages.Simple;
with GPS.LSP_Client.Utilities;   use GPS.LSP_Client.Utilities;

with Basic_Types;                use Basic_Types;
with Commands;                   use Commands;
with Commands.Interactive;       use Commands.Interactive;
with LSP.Types;                  use LSP.Types;

with Refactoring.Services;
with Refactoring.UI;
with Src_Editor_Module;

package body GPS.LSP_Client.Edit_Workspace is

   function "<" (Left, Right : LSP.Messages.Span) return Boolean;

   package Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (LSP.Messages.Span, Ada.Strings.UTF_Encoding.UTF_8_String);

   type Edit_Workspace_Command is new Interactive_Command with
      record
         Kernel                   : Kernel_Handle;
         Workspace_Edit           : LSP.Messages.WorkspaceEdit;
         Title                    : Unbounded_String;
         Make_Writable            : Boolean;
         Auto_Save                : Boolean;
         Locations_Message_Markup : Unbounded_String;
      end record;
   overriding function Execute
     (Command : access Edit_Workspace_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   overriding function Undo
     (Command : access Edit_Workspace_Command) return Boolean;

   -------
   -- < --
   -------

   function "<" (Left, Right : LSP.Messages.Span) return Boolean is
      use type VSS.Unicode.UTF16_Code_Unit_Count;

   begin
      if Left.first.line = Right.first.line then
         return Left.first.character < Right.first.character;
      else
         return Left.first.line < Right.first.line;
      end if;
   end "<";

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Edit_Workspace_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Buffer_Factory : constant Editor_Buffer_Factory_Access :=
        Get_Buffer_Factory (Command.Kernel);

      Error  : Boolean := False;
      Errors : Refactoring.UI.Source_File_Set;

      procedure Process_File
        (File    : Virtual_File;
         Map     : Maps.Map);
      --  Apply changes in the Map to the file

      ------------------
      -- Process_File --
      ------------------

      procedure Process_File
        (File    : Virtual_File;
         Map     : Maps.Map)
      is
         Buffer   : constant Editor_Buffer'Class :=
                      Buffer_Factory.Get
                        (File,
                         Open_View   => not Command.Auto_Save,
                         Open_Buffer => True);
         --  This group is necessary to undo all modification is a buffer
         --  at once.
         G        : constant Group_Block := Buffer.New_Undo_Group;
         C        : Maps.Cursor;
         Writable : Boolean := False;
         Ignored  : GPS.Kernel.Messages.Markup.Markup_Message_Access;
      begin
         if Command.Make_Writable
           and then Buffer.Is_Read_Only
         then
            Buffer.Set_Read_Only (False);
         end if;

         Writable := File.Is_Writable;

         --  Sort changes for applying them in reverse direction
         --  from the last to the first line

         C := Map.Last;
         while Maps.Has_Element (C) loop
            declare
               use type Visible_Column_Type;

               From : constant GPS.Editors.Editor_Location'Class :=
                 GPS.LSP_Client.Utilities.LSP_Position_To_Location
                   (Buffer, Maps.Key (C).first);
               To   : constant GPS.Editors.Editor_Location'Class :=
                 GPS.LSP_Client.Utilities.LSP_Position_To_Location
                   (Buffer, Maps.Key (C).last);

            begin
               if not Writable then
                  GPS.Kernel.Messages.Simple.Create_Simple_Message
                    (Container  => Get_Messages_Container (Command.Kernel),
                     Category   => To_String (Command.Title),
                     File       => File,
                     Line       => From.Line,
                     Column     => From.Column,
                     Text       => "error, file is not writable",
                     Importance => GPS.Kernel.Messages.Unspecified,
                     Flags      => GPS.Kernel.Messages.Side_And_Locations);

                  Errors.Include (File);

               elsif not Refactoring.Services.Insert_Text
                 (Command.Kernel.Refactoring_Context,
                  File,
                  From.Line, From.Column, To.Line, To.Column,
                  Text => Maps.Element (C))
               then
                  GPS.Kernel.Messages.Simple.Create_Simple_Message
                    (Container  => Get_Messages_Container (Command.Kernel),
                     Category   => To_String (Command.Title),
                     File       => File,
                     Line       => From.Line,
                     Column     => From.Column,
                     Text       => "error, failed to process entity",
                     Importance => GPS.Kernel.Messages.Unspecified,
                     Flags      => GPS.Kernel.Messages.Side_And_Locations);
                  Errors.Include (File);

               elsif To_String (Command.Locations_Message_Markup) /= "" then
                  --  Edit done, insert entry into locations view, don't auto
                  --  jump: it will keep the initial file focused

                  Ignored := GPS.Kernel.Messages.Markup.Create_Markup_Message
                    (Container  => Get_Messages_Container (Command.Kernel),
                     Category   => To_String (Command.Title),
                     File       => File,
                     Line       => From.Line,
                     Column     => From.Column,
                     Text       =>
                       To_String (Command.Locations_Message_Markup),
                     Importance => GPS.Kernel.Messages.Unspecified,
                     Flags      => GPS.Kernel.Messages.Side_And_Locations,
                     Allow_Auto_Jump_To_First => False);
               end if;

               Maps.Previous (C);
            end;
         end loop;

         if Command.Auto_Save then
            Buffer.Save (Interactive => False);
         end if;
      end Process_File;

   begin
      declare
         use LSP.Messages.TextDocumentEdit_Maps;

         Map : Maps.Map;
         C   : LSP.Messages.TextDocumentEdit_Maps.Cursor :=
           Command.Workspace_Edit.changes.First;
      begin
         while Has_Element (C) loop
            for Change of Element (C) loop
               Map.Insert
                 (Change.span, To_UTF_8_String (Change.newText));
            end loop;

            Process_File
              (GPS.LSP_Client.Utilities.To_Virtual_File (Key (C)),
               Map);

            Map.Clear;
            Next (C);
         end loop;
      end;

      declare
         use LSP.Messages.Document_Change_Vectors.Element_Vectors;

         C : LSP.Messages.Document_Change_Vectors.Element_Vectors.Cursor :=
           Command.Workspace_Edit.documentChanges.First;
      begin
         while Has_Element (C) loop
            declare
               Item : constant LSP.Messages.Document_Change := Element (C);
               Map  : Maps.Map;
            begin
               case Item.Kind is
                  when LSP.Messages.Text_Document_Edit =>
                     for Change of Item.Text_Document_Edit.edits loop
                        Map.Insert
                          (Change.span, To_UTF_8_String (Change.newText));
                     end loop;

                     Process_File
                       (GPS.LSP_Client.Utilities.To_Virtual_File
                          (Item.Text_Document_Edit.textDocument.uri),
                        Map);

                  when LSP.Messages.Create_File |
                       LSP.Messages.Rename_File |
                       LSP.Messages.Delete_File =>
                     --  Not supported yet
                     Error := True;
                     exit;
               end case;
            end;

            Next (C);
         end loop;
      end;

      --  The calls to Process_File above might have generated entries
      --  in the Errors list. Process this now.

      if Error or not Errors.Is_Empty then
         Error := True;

         if not Refactoring.UI.Dialog
           (Command.Kernel,
            Title         => To_String (Command.Title) & " raises errors",
            Msg           =>
              "Some references could not be processed because one or more" &
              " files were already modified or non writable",
            Files         => Errors,
            Execute_Label => Gtk.Stock.Stock_Ok,
            Cancel_Label  => Gtk.Stock.Stock_Undo)
         then
            if Command.Undo then
               return Success;
            else
               return Failure;
            end if;
         end if;
      end if;

      if Error then
         return Failure;
      else
         return Success;
      end if;
   end Execute;

   ----------
   -- Undo --
   ----------

   overriding function Undo
     (Command : access Edit_Workspace_Command) return Boolean
   is
      Buffer_Factory : constant Editor_Buffer_Factory_Access :=
        Get_Buffer_Factory (Command.Kernel);
   begin
      --  Remove the messages related to the worskspaceEdit being undone
      Get_Messages_Container (Command.Kernel).Remove_Category
        (To_String (Command.Title), GPS.Kernel.Messages.Side_And_Locations);

      --  Loop through the list of TextDocumentEdit and undo each one
      declare
         use LSP.Messages.TextDocumentEdit_Maps;

         C : LSP.Messages.TextDocumentEdit_Maps.Cursor :=
           Command.Workspace_Edit.changes.First;
      begin
         while Has_Element (C) loop
            declare
               Buffer : constant Editor_Buffer'Class :=
                 Buffer_Factory.Get
                   (GPS.LSP_Client.Utilities.To_Virtual_File (Key (C)),
                    Open_View   => not Command.Auto_Save,
                    Focus       => False,
                    Open_Buffer => True);
            begin
               --  Only one undo is necessary because all the Edits done
               --  in one buffer were applied under the same undo group.
               Buffer.Undo;
               if Command.Auto_Save then
                  Buffer.Save (Interactive => False);
               end if;
            end;

            Next (C);
         end loop;
      end;

      --  Loop through the list of documentChanges and undo each one
      declare
         use LSP.Messages.Document_Change_Vectors.Element_Vectors;

         C : LSP.Messages.Document_Change_Vectors.Element_Vectors.
           Cursor := Command.Workspace_Edit.documentChanges.First;
      begin
         while Has_Element (C) loop
            declare
               Item : constant LSP.Messages.Document_Change :=
                 Element (C);
            begin
               case Item.Kind is
               when LSP.Messages.Text_Document_Edit =>
                  declare
                     Buffer : constant Editor_Buffer'Class :=
                       Buffer_Factory.Get
                         (GPS.LSP_Client.Utilities.To_Virtual_File
                            (Item.Text_Document_Edit.textDocument.uri),
                          Open_View   => not Command.Auto_Save,
                          Focus       => False,
                          Open_Buffer => True);
                  begin
                     --  Only one undo is necessary because all the Edits done
                     --  in one buffer were applied under the same undo group.
                     Buffer.Undo;
                     if Command.Auto_Save then
                        Buffer.Save (Interactive => False);
                     end if;
                  end;

               when LSP.Messages.Create_File |
                    LSP.Messages.Rename_File |
                    LSP.Messages.Delete_File =>
                  --  not supported yet (need to be first supported by Execute)
                  null;
               end case;
            end;

            Next (C);
         end loop;
      end;
      return True;
   end Undo;

   ----------
   -- Edit --
   ----------

   procedure Edit
     (Kernel                   : Kernel_Handle;
      Workspace_Edit           : LSP.Messages.WorkspaceEdit;
      Title                    : String;
      Make_Writable            : Boolean;
      Auto_Save                : Boolean;
      Locations_Message_Markup : String;
      Error                    : out Boolean)
   is
      Command : Command_Access := new Edit_Workspace_Command'
        (Root_Command with
         Kernel                   => Kernel,
         Workspace_Edit           => Workspace_Edit,
         Title                    => To_Unbounded_String (Title),
         Make_Writable            => Make_Writable,
         Auto_Save                => Auto_Save,
         Locations_Message_Markup =>
           To_Unbounded_String (Locations_Message_Markup));
   begin
      Src_Editor_Module.Set_Global_Command (Command);
      --  Give up the local ownership of this command, it will be
      --  automatically freed when the Global_command is invalidated.
      Unref (Command);

      Error := Src_Editor_Module.Execute_Global_Command /= Success;
   end Edit;

end GPS.LSP_Client.Edit_Workspace;
