------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
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

   type Loc is record
      Line   : Natural;
      Column : UTF_16_Index;
   end record;

   function "<" (Left, Right : Loc) return Boolean;

   package Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Loc, Ada.Strings.UTF_Encoding.UTF_8_String);

   ----------
   -- Edit --
   ----------

   procedure Edit
     (Kernel         : Kernel_Handle;
      Workspace_Edit : LSP.Messages.WorkspaceEdit;
      Old_Name       : String;
      Title          : String;
      Make_Writable  : Boolean;
      Auto_Save      : Boolean)
   is
      use LSP.Messages.TextDocumentEdit_Maps;

      Buffer_Factory : constant Editor_Buffer_Factory_Access :=
        Get_Buffer_Factory (Kernel);
      C              : LSP.Messages.TextDocumentEdit_Maps.Cursor :=
        Workspace_Edit.changes.First;

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
         Was_Open : constant Boolean := Buffer_Factory.Get
           (File      => File,
            Force     => False,
            Open_View => False) /= Nil_Editor_Buffer;
         Buffer   : constant Editor_Buffer'Class := Buffer_Factory.Get (File);
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
            declare
               New_Name : constant Ada.Strings.UTF_Encoding.UTF_8_String :=
                 To_UTF_8_String (Change.newText);
               Line   : constant Natural :=
                 (if Change.span.first.line <= 0
                  then 1
                  else Integer (Change.span.first.line) + 1);
            begin
               Map.Insert ((Line, Change.span.first.character), New_Name);
            end;
         end loop;

         C := Map.Last;
         while Maps.Has_Element (C) loop
            declare
               Line   : constant Natural := Maps.Key (C).Line;
               Column : constant Visible_Column_Type :=
                 UTF_16_Offset_To_Visible_Column
                   (Maps.Key (C).Column);
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
                  Replaced_Length   => Old_Name'Length,
                  Only_If_Replacing => Old_Name)
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
            if not Was_Open then
               Buffer.Close;
            end if;
         end if;
      end Process_File;

   begin
      while Has_Element (C) loop
         Process_File
           (GPS.LSP_Client.Utilities.To_Virtual_File (Key (C)),
            Element (C));

         Next (C);
      end loop;

      --  The calls to Process_File above might have generated entries
      --  in the Errors list. Process this now.

      if not Errors.Is_Empty then
         if not Refactoring.UI.Dialog
           (Kernel,
            Title         => Title & " raises errors",
            Msg           =>
              "Some references could not be processed because one or more" &
              " files were already modified",
            Files         => Errors,
            Execute_Label => Gtk.Stock.Stock_Ok,
            Cancel_Label  => Gtk.Stock.Stock_Undo)
         then
            C := Workspace_Edit.changes.First;

            while Has_Element (C) loop
               Buffer_Factory.Get
                 (GPS.LSP_Client.Utilities.To_Virtual_File (Key (C))).Undo;

               Next (C);
            end loop;
         end if;
      end if;
   end Edit;

   -------
   -- < --
   -------

   function "<" (Left, Right : Loc) return Boolean is
   begin
      if Left.Line = Right.Line then
         return Left.Column < Right.Column;
      else
         return Left.Line < Right.Line;
      end if;
   end "<";

end GPS.LSP_Client.Edit_Workspace;
