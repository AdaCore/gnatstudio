------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2019-2022, AdaCore                   --
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
         Reverse_Edit             : LSP.Messages.TextDocumentEdit_Maps.Map;
         --  A map containing all the edits to reverse Workspace_Edit, the
         --  map is filled after each execution of the command.
         --  Careful, to prevent unecessary conversion: the map is using
         --  "spans" which are already converted to Editor_Location.

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
        (File : Virtual_File;
         Map  : Maps.Map);
      --  Apply changes in the Map to the file

      procedure Internal_Process_File
        (File   : Virtual_File;
         Editor : GPS.Editors.Editor_Buffer'Class;
         Map    : Maps.Map);

      ------------------
      -- Process_File --
      ------------------

      procedure Process_File
        (File : Virtual_File;
         Map  : Maps.Map) is
      begin
         if Command.Auto_Save then
            declare
               Holder : constant GPS.Editors.
                 Controlled_Editor_Buffer_Holder :=
                   Buffer_Factory.Get_Holder (File);
            begin
               Internal_Process_File (File, Holder.Editor, Map);
            end;
         else
            declare
               Editor : constant GPS.Editors.Editor_Buffer'Class :=
                 Buffer_Factory.Get
                   (File,
                    Open_View   => True,
                    Open_Buffer => True);
            begin
               Internal_Process_File (File, Editor, Map);
            end;
         end if;
      end Process_File;

      ---------------------------
      -- Internal_Process_File --
      ---------------------------

      procedure Internal_Process_File
        (File   : Virtual_File;
         Editor : GPS.Editors.Editor_Buffer'Class;
         Map    : Maps.Map)
      is
         G        : constant Group_Block := Editor.New_Undo_Group;
         C        : Maps.Cursor;
         Writable : Boolean := False;
         Ignored  : GPS.Kernel.Messages.Markup.Markup_Message_Access;
         URI      : constant LSP.Messages.DocumentUri :=
           GPS.LSP_Client.Utilities.To_URI (File);
      begin
         if Command.Make_Writable
           and then Editor.Is_Read_Only
         then
            Editor.Set_Read_Only (False);
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
                   (Editor, Maps.Key (C).first);
               To   : constant GPS.Editors.Editor_Location'Class :=
                 GPS.LSP_Client.Utilities.LSP_Position_To_Location
                   (Editor, Maps.Key (C).last);

               --  Set to -1 to detect if an edit was done
               Rev_To_Line     : Integer := -1;
               Rev_To_Column   : Visible_Column_Type;
               Rev_Text        : Unbounded_String;
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

               elsif not Refactoring.Services.Insert_Text_With_Reverse
                 (Command.Kernel.Refactoring_Context,
                  File,
                  From.Line, From.Column, To.Line, To.Column,
                  Text          => Maps.Element (C),
                  Rev_To_Line   => Rev_To_Line,
                  Rev_To_Column => Rev_To_Column,
                  Rev_Text      => Rev_Text)
               then
                  --  The previous value can be crushed by
                  --  Insert_Text_With_Reverse, however the insert was not
                  --  applied so restore it.
                  Rev_To_Line := -1;
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

               if Rev_To_Line /= -1 then
                  if not Command.Reverse_Edit.Contains (URI) then
                     declare
                        Empty_Vect : LSP.Messages.TextEdit_Vector;
                     begin
                        Command.Reverse_Edit.Insert (URI, Empty_Vect);
                     end;
                  end if;
                  --  The edit to undo the Insert_Text_With_Reverse
                  declare
                     Item : constant LSP.Messages.TextEdit :=
                       (span    =>
                          (first =>
                             (line      => Line_Number (From.Line),
                              character => UTF_16_Index (From.Column)),
                           last  =>
                             (line      => Line_Number (Rev_To_Line),
                              character => UTF_16_Index (Rev_To_Column))),
                        newText => To_LSP_String (To_String (Rev_Text)));
                  begin
                     Command.Reverse_Edit (URI).Append (Item);
                  end;
               end if;
            end;
         end loop;

         if Command.Auto_Save then
            Editor.Save (Interactive => False);
         end if;

      end Internal_Process_File;

   begin
      --  Clear the previous changes
      Command.Reverse_Edit.Clear;

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

      procedure Process_File
        (File : Virtual_File;
         Map  : Maps.Map);
      --  Apply changes in the Map to the file

      procedure Internal_Process_File
        (File   : Virtual_File;
         Editor : GPS.Editors.Editor_Buffer'Class;
         Map    : Maps.Map);

      ------------------
      -- Process_File --
      ------------------

      procedure Process_File
        (File : Virtual_File;
         Map  : Maps.Map) is
      begin
         if Command.Auto_Save then
            declare
               Holder : constant GPS.Editors.
                 Controlled_Editor_Buffer_Holder :=
                   Buffer_Factory.Get_Holder (File);
            begin
               Internal_Process_File (File, Holder.Editor, Map);
            end;
         else
            declare
               Editor : constant GPS.Editors.Editor_Buffer'Class :=
                 Buffer_Factory.Get
                   (File,
                    Open_View   => True,
                    Open_Buffer => True);
            begin
               Internal_Process_File (File, Editor, Map);
            end;
         end if;
      end Process_File;

      ---------------------------
      -- Internal_Process_File --
      ---------------------------

      procedure Internal_Process_File
        (File   : Virtual_File;
         Editor : GPS.Editors.Editor_Buffer'Class;
         Map    : Maps.Map) is
      begin
         if Editor.Can_Undo then
            --  At this point we are focused on an Editor for File with an
            --  non empty Undo_Redo queue: use it to undo the last group.
            Editor.Undo;
         else
            declare
               G : constant Group_Block := Editor.New_Undo_Group;
               C : Maps.Cursor;
            begin
               C := Map.First;
               while Maps.Has_Element (C) loop
                  declare
                     --  S is a fake span which is already converted to an
                     --  Editor Location
                     S     : constant LSP.Messages.Span := Maps.Key (C);
                     Dummy : constant Boolean :=
                       Refactoring.Services.Insert_Text
                         (Context     => Command.Kernel.Refactoring_Context,
                          In_File     => File,
                          From_Line   => Integer (S.first.line),
                          From_Column =>
                            Visible_Column_Type (S.first.character),
                          To_Line     => Integer (S.last.line),
                          To_Column   =>
                            Visible_Column_Type (S.last.character),
                          Text        => Maps.Element (C));
                  begin
                     Maps.Next (C);
                  end;
               end loop;
            end;
         end if;

         if Command.Auto_Save then
            Editor.Save (Interactive => False);
         end if;
      end Internal_Process_File;

   begin
      --  Remove the messages related to the worskspaceEdit being undone
      Get_Messages_Container (Command.Kernel).Remove_Category
        (To_String (Command.Title), GPS.Kernel.Messages.Side_And_Locations);

      --  Loop through the generated edit to undo WorkspaceEdit
      declare
         use LSP.Messages.TextDocumentEdit_Maps;

         Map : Maps.Map;
         C   : LSP.Messages.TextDocumentEdit_Maps.Cursor :=
           Command.Reverse_Edit.First;
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
         Reverse_Edit             => <>,
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
