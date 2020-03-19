------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2019-2020, AdaCore                   --
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
with Ada.Strings.UTF_Encoding;

with Gtk.Stock;
with GNATCOLL.VFS;               use GNATCOLL.VFS;

with Basic_Types;                use Basic_Types;
with Commands;                   use Commands;
with GPS.Editors;                use GPS.Editors;
with GPS.Kernel.Messages.Simple;
with LSP.Types;                  use LSP.Types;
with GPS.LSP_Client.Utilities;   use GPS.LSP_Client.Utilities;

with Refactoring.Services;
with Refactoring.UI;

package body GPS.LSP_Client.Edit_Workspace is

   function "<" (Left, Right : LSP.Messages.Span) return Boolean;

   package Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (LSP.Messages.Span, Ada.Strings.UTF_Encoding.UTF_8_String);

   ----------
   -- Edit --
   ----------

   procedure Edit
     (Kernel         : Kernel_Handle;
      Workspace_Edit : LSP.Messages.WorkspaceEdit;
      Old_Name       : String;
      Title          : String;
      Make_Writable  : Boolean;
      Auto_Save      : Boolean;
      Error          : out Boolean)
   is
      Buffer_Factory : constant Editor_Buffer_Factory_Access :=
        Get_Buffer_Factory (Kernel);

      Errors : Refactoring.UI.Source_File_Set;

      procedure Process_File
        (File    : Virtual_File;
         Changes : LSP.Messages.TextEdit_Vector);
      --  Apply changes to the file

      ------------------
      -- Process_File --
      ------------------

      procedure Process_File
        (File    : Virtual_File;
         Changes : LSP.Messages.TextEdit_Vector)
      is
         use LSP.Messages;

         Buffer   : constant Editor_Buffer'Class :=
                      Buffer_Factory.Get
                        (File,
                         Open_View   => not Auto_Save,
                         Open_Buffer => Auto_Save);
         G        : constant Group_Block := Buffer.New_Undo_Group;
         Map      : Maps.Map;
         C        : Maps.Cursor;
         Writable : Boolean := False;
      begin
         if Make_Writable
           and then Buffer.Is_Read_Only
         then
            Buffer.Set_Read_Only (False);
         end if;

         Writable := File.Is_Writable;

         --  Sort changes for ply them in reverse direction
         --  from the last to the first line

         for Change of Changes loop
            Map.Insert (Change.span, To_UTF_8_String (Change.newText));
         end loop;

         C := Map.Last;
         while Maps.Has_Element (C) loop
            declare
               use type Visible_Column_Type;

               Line   : constant Integer := Integer
                 (Maps.Key (C).first.line) + 1;
               Column : constant Visible_Column_Type :=
                 UTF_16_Offset_To_Visible_Column
                   (Maps.Key (C).first.character);
            begin
               if not Writable then
                  GPS.Kernel.Messages.Simple.Create_Simple_Message
                    (Container  => Get_Messages_Container (Kernel),
                     Category   =>
                       Title & " " & Old_Name & " to " & Maps.Element (C),
                     File       => File,
                     Line       => Line,
                     Column     => Column,
                     Text       => "error, file is not writable",
                     Importance => GPS.Kernel.Messages.Unspecified,
                     Flags      => GPS.Kernel.Messages.Side_And_Locations);

                  Errors.Include (File);

               elsif not Refactoring.Services.Insert_Text
                 (Kernel.Refactoring_Context,
                  File,
                  Line,
                  Column,
                  Maps.Element (C),
                  Indent            => False,
                  Replaced_Length   => Integer
                    (UTF_16_Offset_To_Visible_Column
                         (Maps.Key (C).last.character) - Column))
               then
                  GPS.Kernel.Messages.Simple.Create_Simple_Message
                    (Container  => Get_Messages_Container (Kernel),
                     Category   =>
                       Title & " " & Old_Name & " to " & Maps.Element (C),
                     File       => File,
                     Line       => Line,
                     Column     => Column,
                     Text       => "error, failed to process entity",
                     Importance => GPS.Kernel.Messages.Unspecified,
                     Flags      => GPS.Kernel.Messages.Side_And_Locations);
                  Errors.Include (File);

               else
                  --  Renaming done, insert entry into locations view

                  GPS.Kernel.Messages.Simple.Create_Simple_Message
                    (Container  => Get_Messages_Container (Kernel),
                     Category   =>
                       Title & " " & Old_Name & " to " & Maps.Element (C),
                     File       => File,
                     Line       => Line,
                     Column     => Column,
                     Text       => "entity processed",
                     Importance => GPS.Kernel.Messages.Unspecified,
                     Flags      => GPS.Kernel.Messages.Side_And_Locations);
               end if;

               Maps.Previous (C);
            end;
         end loop;

         if Auto_Save then
            Buffer.Save (Interactive => False);
         end if;
      end Process_File;

   begin
      Error := False;

      declare
         use LSP.Messages.TextDocumentEdit_Maps;

         C : LSP.Messages.TextDocumentEdit_Maps.Cursor :=
           Workspace_Edit.changes.First;
      begin
         while Has_Element (C) loop
            Process_File
              (GPS.LSP_Client.Utilities.To_Virtual_File (Key (C)),
               Element (C));

            Next (C);
         end loop;
      end;

      declare
         use LSP.Messages.Document_Change_Vectors.Element_Vectors;

         C : LSP.Messages.Document_Change_Vectors.Element_Vectors.Cursor :=
           Workspace_Edit.documentChanges.First;
      begin
         while Has_Element (C) loop
            declare
               Item : constant LSP.Messages.Document_Change := Element (C);
            begin
               case Item.Kind is
                  when LSP.Messages.Text_Document_Edit =>
                     Process_File
                       (GPS.LSP_Client.Utilities.To_Virtual_File
                          (Item.Text_Document_Edit.textDocument.uri),
                        Item.Text_Document_Edit.edits);

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
           (Kernel,
            Title         => Title & " raises errors",
            Msg           =>
              "Some references could not be processed because one or more" &
              " files were already modified or non writable",
            Files         => Errors,
            Execute_Label => Gtk.Stock.Stock_Ok,
            Cancel_Label  => Gtk.Stock.Stock_Undo)
         then
            declare
               use LSP.Messages.TextDocumentEdit_Maps;

               C : LSP.Messages.TextDocumentEdit_Maps.Cursor :=
                 Workspace_Edit.changes.First;
            begin
               while Has_Element (C) loop
                  Buffer_Factory.Get
                    (GPS.LSP_Client.Utilities.To_Virtual_File (Key (C))).Undo;

                  Next (C);
               end loop;
            end;

            declare
               use LSP.Messages.Document_Change_Vectors.Element_Vectors;

               C : LSP.Messages.Document_Change_Vectors.Element_Vectors.
                 Cursor := Workspace_Edit.documentChanges.First;
            begin
               while Has_Element (C) loop
                  declare
                     Item : constant LSP.Messages.Document_Change :=
                       Element (C);
                  begin
                     case Item.Kind is
                        when LSP.Messages.Text_Document_Edit =>
                           Buffer_Factory.Get
                             (GPS.LSP_Client.Utilities.To_Virtual_File
                                (Item.Text_Document_Edit.textDocument.uri)).
                               Undo;

                        when LSP.Messages.Create_File |
                             LSP.Messages.Rename_File |
                             LSP.Messages.Delete_File =>
                           null;
                     end case;
                  end;

                  Next (C);
               end loop;
            end;
         end if;
      end if;
   end Edit;

   -------
   -- < --
   -------

   function "<" (Left, Right : LSP.Messages.Span) return Boolean is
   begin
      if Left.first.line = Right.first.line then
         return Left.first.character < Right.first.character;
      else
         return Left.first.line < Right.first.line;
      end if;
   end "<";

end GPS.LSP_Client.Edit_Workspace;
