------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2019-2026, AdaCore                     --
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

with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Unbounded;

with GNAT.Strings;

with Gtkada.Stock_Labels;

with GNATCOLL.Traces;                 use GNATCOLL.Traces;
with GNATCOLL.Scripts;                use GNATCOLL.Scripts;
with GNATCOLL.Scripts.Python;         use GNATCOLL.Scripts.Python;
with GNATCOLL.Scripts.VSS_Utils;      use GNATCOLL.Scripts.VSS_Utils;
with GNATCOLL.VFS;                    use GNATCOLL.VFS;

with VSS.Strings.Character_Iterators;
with VSS.Strings.Conversions;

with GPS.Editors;                     use GPS.Editors;
with GPS.Kernel.Messages;
with GPS.Kernel.Messages.Markup;
with GPS.Kernel.Messages.Simple;
with GPS.Kernel.Project;
with GPS.LSP_Module;                  use GPS.LSP_Module;
with GPS.LSP_Client.Language_Servers; use GPS.LSP_Client.Language_Servers;
with GPS.LSP_Client.Utilities;        use GPS.LSP_Client.Utilities;

with Basic_Types;                     use Basic_Types;
with Commands;                        use Commands;
with Commands.Interactive;            use Commands.Interactive;
with LSP.Types;                       use LSP.Types;

with Refactoring.Services;
with Refactoring.UI;
with String_Utils;
with Src_Editor_Module;
with VFS_Module;
with VSS.Strings.Cursors;
with VSS.Strings.Cursors.Iterators;
with VSS.Strings.Cursors.Iterators.Lines;
with VSS.Unicode;
with GPS.Editors.Line_Information; use GPS.Editors.Line_Information;

package body GPS.LSP_Client.Edit_Workspace is

   Me : constant Trace_Handle :=
     Create ("GPS.LSP_Client.Edit_Workspace.Debug", Off);

   type Text_Edit is record
      Span  : LSP.Messages.Span;
      Text  : VSS.Strings.Virtual_String;
   end record;

   package Vectors is new Ada.Containers.Indefinite_Vectors
     (Natural, Text_Edit);

   function "<" (L, R : Text_Edit) return Boolean;

   package Ascending is new Vectors.Generic_Sorting ("<");

   procedure Insert_Change
     (Vector : in out Vectors.Vector;
      Span   : LSP.Messages.Span;
      Change : VSS.Strings.Virtual_String);
   --  Add to Map the contents of Change, converted to fit the needs
   --  of editor buffers.

   procedure Debug_Print_Changes
     (Editor  : GPS.Editors.Editor_Buffer'Class;
      Msg     : String;
      Changes : Vectors.Vector);
   --  Trace the given edit changes.

   function Get_Minimal_Changes
     (Kernel   : not null access Kernel_Handle_Record'Class;
      Editor   : GPS.Editors.Editor_Buffer'Class;
      Changes  : Vectors.Vector) return Vectors.Vector;
   --  Reduce the given changes to more minimal ones, to allow keeping track of
   --  the cursor when applying TextEdits.
   --
   --  For example:
   --     * Original text: "This is some text"
   --     * New text: "This is the new text"
   --
   --  Instead of removing the whole original text and insert the new one, the
   --  newly computed changes will be:
   --
   --     * Delete "some"
   --     * Insert "the new"

   procedure Get_Diff_Changes
     (Kernel        : not null access Kernel_Handle_Record'Class;
      Editor        : GPS.Editors.Editor_Buffer'Class;
      Original_Span : LSP.Messages.Span;
      Index         : in out Integer;
      Old_Text      : VSS.Strings.Virtual_String;
      New_Text      : VSS.Strings.Virtual_String;
      Changes       : out Vectors.Vector);
   --  Compute the diff between Old_Text and New_Text using a Python
   --  implementation of the Myers diff algorithm.

   function Has_Cursor
     (Editor : GPS.Editors.Editor_Buffer'Class;
      Edit   : Text_Edit) return Boolean;
   --  Return True if Edit is affecting a cursor inside Editor

   type Edit_Workspace_Command is new Interactive_Command with
      record
         Kernel                   : Kernel_Handle;
         Limit_Span               : LSP.Messages.Span;
         --  Only apply edit affecting this Span, this is used to limit
         --  overzealous formatting.

         Workspace_Edit           : LSP.Messages.WorkspaceEdit;
         Reverse_Edit             : LSP.Messages.TextDocumentEdit_Maps.Map;
         --  A map containing all the edits to reverse Workspace_Edit, the
         --  map is filled after each execution of the command.
         --  Careful, to prevent unecessary conversion: the map is using
         --  "spans" which are already converted to Editor_Location.

         Reverse_Document_Changes : LSP.Messages.Document_Change_Vector;
         --  A vector containing all the document changes in the reverse order.
         --  This is used for instance to revert file renamings before undoing
         --  all the workspaceEdits individually.

         Title                    : VSS.Strings.Virtual_String;
         Make_Writable            : Boolean;
         Auto_Save                : Boolean;
         Allow_File_Renaming      : Boolean;
         Locations_Message_Markup : VSS.Strings.Virtual_String;

         Compute_Minimal_Edits    : Boolean := False;
         --  Compute_Minimal_Edits controls whether we'll try to split the
         --  given Edits into smaller ones, allowing to preserve the current
         --  cursor's position: thus, this should only be used in particular
         --  contexts (e.g: formatting). The computation is done through
         --  an implementation of the Myers diff algorithm.

         Avoid_Cursor_Move        : Boolean := True;
         --  If Avoid_Cursor_Move is True, the cursor won't be moved to the
         --  location of the last change being applied: it will be kept at the
         --  current location.
      end record;
   overriding function Execute
     (Command : access Edit_Workspace_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   overriding function Undo
     (Command : access Edit_Workspace_Command) return Boolean;

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Text_Edit) return Boolean
   is
      use LSP.Messages;
      use type VSS.Unicode.UTF16_Code_Unit_Offset;
   begin
      --  According to the documentation textEdit range should not overlap so
      --  we only need to compare the first location. Also we need to preserve
      --  the order of textEdit which are inserting at the exact same span:
      --  they are reversed later so reverse them here also.
      return L.Span.first = R.Span.first
        or else L.Span.first.line < R.Span.first.line
        or else (L.Span.first.line = R.Span.first.line
                  and then L.Span.first.character < R.Span.first.character);
   end "<";

   -------------------------
   -- Get_Minimal_Changes --
   -------------------------

   function Get_Minimal_Changes
     (Kernel  : not null access Kernel_Handle_Record'Class;
      Editor  : GPS.Editors.Editor_Buffer'Class;
      Changes : Vectors.Vector) return Vectors.Vector
   is
      New_Changes : Vectors.Vector;
      C           : Vectors.Cursor;
   begin
      C := Changes.Last;

      Trace (Me, "Nb changes: " & Integer'Image (Integer ((Changes.Length))));

      Debug_Print_Changes
        (Editor => Editor, Msg => "Original edits:", Changes => Changes);

      while Vectors.Has_Element (C) loop
         if Has_Cursor (Editor, Vectors.Element (C)) then
            declare
               Span     : constant LSP.Messages.Span :=
                 Vectors.Element (C).Span;
               From     : constant GPS.Editors.Editor_Location'Class :=
                 GPS.LSP_Client.Utilities.LSP_Position_To_Location
                   (Editor, Span.first);
               To       : constant GPS.Editors.Editor_Location'Class :=
                 GPS.LSP_Client.Utilities.LSP_Position_To_Location
                   (Editor, Span.last);
               --  We are using Get_Text below which includes the character
               --  after To. TextEdits should ignore this character so move
               --  To backward now to compensate.
               --  Also handle the special case where From = To, we should not
               --  move To backward or we will get the characters between
               --  From - 1 and To + 1.
               Old_Text : constant VSS.Strings.Virtual_String :=
                 (if From = To
                  then VSS.Strings.Empty_Virtual_String
                  else
                    Editor.Get_Text
                      (From                 => From,
                       To                   => To.Forward_Char (-1),
                       Include_Hidden_Chars => False));
               New_Text : constant VSS.Strings.Virtual_String :=
                 Vectors.Element (C).Text;
               Index    : Integer := 1;
            begin

               if Me.Is_Active then
                  declare
                     Cur_Change : Vectors.Vector;
                  begin
                     Cur_Change.Append (Vectors.Element (C));
                     Debug_Print_Changes
                       (Editor  => Editor,
                        Msg     => "Computing minimal diff:",
                        Changes => Cur_Change);
                  end;
               end if;

               Get_Diff_Changes
                 (Kernel        => Kernel,
                  Editor        => Editor,
                  Original_Span => Span,
                  Index         => Index,
                  Old_Text      => Old_Text,
                  New_Text      => New_Text,
                  Changes       => New_Changes);
            end;
         else
            New_Changes.Prepend (Vectors.Element (C));
         end if;
         Vectors.Previous (C);
      end loop;

      Debug_Print_Changes
        (Editor => Editor, Msg => "New edits:", Changes => New_Changes);

      return New_Changes;
   end Get_Minimal_Changes;

   ----------------------
   -- Get_Diff_Changes --
   ----------------------

   procedure Get_Diff_Changes
     (Kernel        : not null access Kernel_Handle_Record'Class;
      Editor        : GPS.Editors.Editor_Buffer'Class;
      Original_Span : LSP.Messages.Span;
      Index         : in out Integer;
      Old_Text      : VSS.Strings.Virtual_String;
      New_Text      : VSS.Strings.Virtual_String;
      Changes       : out Vectors.Vector)
   is
      use LSP.Types;
      use VSS.Strings.Cursors.Iterators.Lines;

      Script : constant GNATCOLL.Scripts.Scripting_Language :=
        Kernel.Scripts.Lookup_Scripting_Language (Python_Name);
      Data   : Callback_Data'Class := Create (Script, 2);

      Cur_Cursor  : LSP.Messages.Position := Original_Span.last;
      Next_Cursor : LSP.Messages.Position := Original_Span.last;

      type Diff_Operation_Type is (Keep, Insert, Delete);
      --  The type of diff operation.

      function Print_Cursor
        (Cursor : LSP.Messages.Position;
         Name   : String) return String
      is
        (Name
         & " ("
         & Cursor.line'Img
         & ", "
         & Cursor.character'Img
         & ")"
         & ASCII.LF);

      function Compute_New_Cursor
        (Cursor : LSP.Messages.Position;
         Str    : VSS.Strings.Virtual_String) return LSP.Messages.Position;
      --  Compute the the cursor's position according to the current diff
      --  operation.

      -----------------------
      -- To_Diff_Operation --
      -----------------------

      function To_Diff_Operation (Num : Integer) return Diff_Operation_Type
      is
        (case Num is
            when -1 => Delete,
            when 0  => Keep,
            when 1  => Insert,
            when others => raise Constraint_Error);

      ------------------------
      -- Compute_New_Cursor --
      ------------------------

      function Compute_New_Cursor
        (Cursor : LSP.Messages.Position;
         Str    : VSS.Strings.Virtual_String) return LSP.Messages.Position
      is
         Cur_Loc  : constant GPS.Editors.Editor_Location'Class :=
           LSP_Position_To_Location (Editor, Cursor);
         Iterator : constant VSS.Strings.Character_Iterators.Character_Iterator
           := Str.After_Last_Character;
         Offset   : constant Integer := Integer (Iterator.First_UTF8_Offset);
         New_Loc  : constant GPS.Editors.Editor_Location'Class :=
           Editor.New_Location
             (Offset => VSS.Strings.Character_Count (Cur_Loc.Offset - Offset));
      begin
         return Location_To_LSP_Position (New_Loc);
      end Compute_New_Cursor;

   begin
      --  Call the Myers diff Python implementation
      Set_Nth_Arg (Data, 1, Old_Text);
      Set_Nth_Arg (Data, 2, New_Text);
      Execute_Command (Data, "diff_match_patch.compute_diff");

      --  Iterate over the results and create the new edit changes.
      declare
         use VSS.Strings;
         Result    : constant List_Instance'Class := Return_Value (Data);
         Total_Arg : constant Integer := Number_Of_Arguments (Result);
      begin
         for J in reverse 1 .. Total_Arg loop
            Index := Index + 1;
            declare
               Item      : constant List_Instance'Class :=
                 Result.Nth_Arg (J);
               Operation : constant Diff_Operation_Type :=
                 To_Diff_Operation (Item.Nth_Arg (1));
               V_Str     : constant VSS.Strings.Virtual_String :=
                 Nth_Arg (Item, 2);
               Safe_Str : constant VSS.Strings.Virtual_String :=
                 (if V_Str /= Empty_Virtual_String
                  then
                    V_Str.Split_Lines.Join_Lines
                      (Terminator     => VSS.Strings.LF,
                       Terminate_Last =>
                         V_Str.At_Line (V_Str.At_Last_Character)
                           .Has_Line_Terminator)
                  else V_Str);
               --  Fancy way to remove all CR characters which could corrupt
               --  the offset.
            begin
               case Operation is
                  when Keep =>
                     Next_Cursor :=
                       Compute_New_Cursor (Cur_Cursor, Safe_Str);

                  when Delete =>
                     Next_Cursor :=
                       Compute_New_Cursor (Cur_Cursor, Safe_Str);
                     Changes.Prepend
                       (Text_Edit'(Span => (first => Next_Cursor,
                                            last  => Cur_Cursor),
                                   Text  => ""));

                  when Insert =>
                     --  The cursor doesn't move when inserting
                     Changes.Prepend
                       (Text_Edit'(Span => (first => Cur_Cursor,
                                            last  => Next_Cursor),
                                   Text => Safe_Str));
               end case;

               Trace
                 (Me,
                  "===" & ASCII.LF
                  & Operation'Img & ": '"
                  & VSS.Strings.Conversions.To_UTF_8_String (Safe_Str)
                  & "'" & ASCII.LF
                  & Print_Cursor (Next_Cursor, "Next_Cursor")
                  & Print_Cursor (Cur_Cursor, "Cur_Cursor")
                  & "===" & ASCII.LF);

               Cur_Cursor := Next_Cursor;
            end;
         end loop;
      end;
   end Get_Diff_Changes;

   ----------------
   -- Has_Cursor --
   ----------------

   function Has_Cursor
     (Editor : GPS.Editors.Editor_Buffer'Class; Edit : Text_Edit)
      return Boolean is
   begin
      for Cursor of Editor.Get_Cursors loop
         declare
            Cursor_Line : constant Integer := Cursor.Get_Insert_Mark.Line;
         begin
            --  Only compare the line, the column are too fine grain
            if Integer (Edit.Span.first.line) <= Cursor_Line
              and then Cursor_Line <= Integer (Edit.Span.last.line)
            then
               return True;
            end if;
         end;
      end loop;
      return False;
   end Has_Cursor;

   -------------------
   -- Insert_Change --
   -------------------

   procedure Insert_Change
     (Vector : in out Vectors.Vector;
      Span   : LSP.Messages.Span;
      Change : VSS.Strings.Virtual_String)
   is
      Contents : GNAT.Strings.String_Access;
      --  We use a String_Access here to avoid allocating a potentially
      --  large string on the stack.
      Last     : Integer;
      Ignored  : Boolean;
   begin
      --  Convert the contents to UTF8
      Contents := new String'
        (VSS.Strings.Conversions.To_UTF_8_String (Change));

      --  Strip any CRs from the text
      String_Utils.Strip_CR (Contents.all, Last, Ignored);
      Vector.Append
        (Text_Edit'(Span => Span,
                    Text =>
                      VSS.Strings.Conversions.To_Virtual_String
                        (Contents (Contents'First .. Last))));
      GNAT.Strings.Free (Contents);
   end Insert_Change;

   -------------------------
   -- Debug_Print_Changes --
   -------------------------

   procedure Debug_Print_Changes
     (Editor  : GPS.Editors.Editor_Buffer'Class;
      Msg     : String;
      Changes : Vectors.Vector)
   is
      C : Vectors.Cursor;
   begin
      --  Return immediately if the debug trace is not active, since
      --  we are doing a loop with some position conversions, which can
      --  be costly if we have a lot of edits.
      if not Me.Is_Active then
         return;
      end if;

      Trace (Me, Msg);
      C := Changes.Last;

      while Vectors.Has_Element (C) loop
         declare
            Span     : constant LSP.Messages.Span :=
              Vectors.Element (C).Span;
            New_Text : constant VSS.Strings.Virtual_String :=
              Vectors.Element (C).Text;
            From     : constant GPS.Editors.Editor_Location'Class :=
              GPS.LSP_Client.Utilities.LSP_Position_To_Location
                (Editor, Span.first);
            To       : constant GPS.Editors.Editor_Location'Class :=
              GPS.LSP_Client.Utilities.LSP_Position_To_Location
                (Editor, Span.last);
         begin
            Trace
              (Me,
               ((if New_Text.Is_Empty then "* Delete " else "* Insert ")
                & "from ("
                & Integer'Image (From.Line)
                & ","
                & Visible_Column_Type'Image (From.Column)
                & ") to ("
                & Integer'Image (To.Line)
                & ","
                & Visible_Column_Type'Image (To.Column)
                & ") new text:"""
                & VSS.Strings.Conversions.To_UTF_8_String (New_Text)
                & """"));
            Vectors.Previous (C);
         end;
      end loop;
   end Debug_Print_Changes;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Edit_Workspace_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel : Kernel_Handle renames Command.Kernel;
      Buffer_Factory : constant Editor_Buffer_Factory_Access :=
        Get_Buffer_Factory (Kernel);

      Error                : Boolean := False;
      Need_Project_Refresh : Boolean := False;
      Errors               : Refactoring.UI.Source_File_Set;

      function Edit_Affect_Span
        (From : LSP.Messages.Position;
         To   : LSP.Messages.Position)
         return Boolean;

      procedure Process_File
        (File   : Virtual_File;
         Vector : Vectors.Vector);
      --  Apply changes in the Map to the file

      procedure Internal_Process_File
        (File   : Virtual_File;
         Editor : in out GPS.Editors.Editor_Buffer'Class;
         Vector : Vectors.Vector);

      function Get_Limited_Changes
        (Changes : Vectors.Vector) return Vectors.Vector;
      --  Limit the changes to the initial span if required

      -------------------------
      -- Get_Limited_Changes --
      -------------------------

      function Get_Limited_Changes
        (Changes : Vectors.Vector) return Vectors.Vector
      is
         use type LSP.Messages.Span;
         Result : Vectors.Vector;
         C      : Vectors.Cursor;
      begin
         if Command.Limit_Span = LSP.Messages.Empty_Span then
            return Changes;
         else
            C := Changes.First;

            while Vectors.Has_Element (C) loop
               if Edit_Affect_Span
                 (Vectors.Element (C).Span.first,
                  Vectors.Element (C).Span.last)
               then
                  Result.Append (Vectors.Element (C));
               end if;
               C.Next;
            end loop;

            return Result;
         end if;
      end Get_Limited_Changes;

      ----------------------
      -- Edit_Affect_Span --
      ----------------------

      function Edit_Affect_Span
        (From : LSP.Messages.Position;
         To   : LSP.Messages.Position)
         return Boolean
      is
         use type LSP.Messages.Span;
         --  Use Position as parameter to prevent costly convertions using the
         --  editor.
      begin
         if Command.Limit_Span = LSP.Messages.Empty_Span then
            return True;
         else
            if Me.Is_Active then
               Trace
                 (Me,
                  "From.line: "
                  & Line_Number'Image (From.line)
                  & ASCII.LF
                  & "To.line: "
                  & Line_Number'Image (To.line)
                  & ASCII.LF);
            end if;
            --  Check if Limit_Span is partially include in the textEdit.
            return
              (Command.Limit_Span.first.line <= From.line
               and then From.line <= Command.Limit_Span.last.line)
              or else (Command.Limit_Span.first.line <= To.line
                       and then To.line <= Command.Limit_Span.last.line)
              or else (From.line <= Command.Limit_Span.first.line
                       and then Command.Limit_Span.first.line <= To.line)
              or else (From.line <= Command.Limit_Span.last.line
                       and then Command.Limit_Span.last.line <= To.line);
         end if;
      end Edit_Affect_Span;

      ------------------
      -- Process_File --
      ------------------

      procedure Process_File
        (File   : Virtual_File;
         Vector : Vectors.Vector) is
      begin
         if Command.Auto_Save then
            declare
               Holder : constant GPS.Editors.Controlled_Editor_Buffer_Holder :=
                 Buffer_Factory.Get_Holder (File);
               Editor : GPS.Editors.Editor_Buffer'Class := Holder.Editor;
               Server : constant Language_Server_Access :=
                 Get_Language_Server
                   (Command.Kernel.Get_Language_Handler.
                      Get_Language_From_File (File));
            begin
               --  First open the file on the server side if it's not already
               --  the case.
               if Server /= null
                 and then not Holder.Editor.Is_Opened_On_LSP_Server
               then
                  Server.Get_Client.Send_Text_Document_Did_Open (File);
                  Holder.Editor.Set_Opened_On_LSP_Server (True);
               end if;

               Internal_Process_File (File, Editor, Vector);
            end;
         else
            declare
               Editor : GPS.Editors.Editor_Buffer'Class :=
                 Buffer_Factory.Get
                   (File,
                    Open_View   => True,
                    Open_Buffer => True);
            begin
               Internal_Process_File (File, Editor, Vector);
            end;
         end if;
      end Process_File;

      ---------------------------
      -- Internal_Process_File --
      ---------------------------

      procedure Internal_Process_File
        (File   : Virtual_File;
         Editor : in out GPS.Editors.Editor_Buffer'Class;
         Vector : Vectors.Vector)
      is
         Controller : constant Cursor_Movement_Controller'Class :=
           GPS_Editor_Buffer'Class (Editor).Freeze_Cursor;
         pragma Unreferenced (Controller);
         G               : constant Group_Block := Editor.New_Undo_Group;
         C               : Vectors.Cursor;
         Writable        : Boolean := False;
         Ignored         : GPS.Kernel.Messages.Markup.Markup_Message_Access;
         URI             : constant LSP.Messages.DocumentUri :=
           GPS.LSP_Client.Utilities.To_URI (File);
         Limited_Changes : constant Vectors.Vector :=
           Get_Limited_Changes (Vector);
         Changes         : constant Vectors.Vector :=
           (if Command.Compute_Minimal_Edits then
               Get_Minimal_Changes (Kernel, Editor, Limited_Changes)
            else Limited_Changes);
      begin
         if Command.Make_Writable
           and then Editor.Is_Read_Only
         then
            Editor.Set_Read_Only (False);
         end if;

         --  In case the file is not created yet on the disk yet and is
         --  opened in a buffer then consider it as writable, we have
         --  sole ownership of it.
         Writable := not File.Is_Regular_File or else File.Is_Writable;

         --  Sort changes for applying them in reverse direction
         --  from the last to the first line

         C := Changes.Last;
         while Vectors.Has_Element (C) loop
            declare
               use type Visible_Column_Type;

               From : constant GPS.Editors.Editor_Location'Class :=
                 GPS.LSP_Client.Utilities.LSP_Position_To_Location
                   (Editor, Vectors.Element (C).Span.first);
               To   : constant GPS.Editors.Editor_Location'Class :=
                 GPS.LSP_Client.Utilities.LSP_Position_To_Location
                   (Editor, Vectors.Element (C).Span.last);

               --  Set to -1 to detect if an edit was done
               Rev_To_Line     : Integer := -1;
               Rev_To_Column   : Visible_Column_Type;
               Rev_Text        : Ada.Strings.Unbounded.Unbounded_String;
            begin
               if not Writable then
                  GPS.Kernel.Messages.Simple.Create_Simple_Message
                    (Container  => Get_Messages_Container (Command.Kernel),
                     Category   => Command.Title,
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
                  Text          =>
                    VSS.Strings.Conversions.To_UTF_8_String
                      (Vectors.Element (C).Text),
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
                     Category   => Command.Title,
                     File       => File,
                     Line       => From.Line,
                     Column     => From.Column,
                     Text       => "error, failed to process entity",
                     Importance => GPS.Kernel.Messages.Unspecified,
                     Flags      => GPS.Kernel.Messages.Side_And_Locations);
                  Errors.Include (File);

               elsif not Command.Locations_Message_Markup.Is_Empty then
                  --  Edit done, insert entry into locations view, don't auto
                  --  jump: it will keep the initial file focused

                  Ignored := GPS.Kernel.Messages.Markup.Create_Markup_Message
                    (Container  => Get_Messages_Container (Command.Kernel),
                     Category   => Command.Title,
                     File       => File,
                     Line       => From.Line,
                     Column     => From.Column,
                     Text       =>
                       VSS.Strings.Conversions.To_UTF_8_String
                         (Command.Locations_Message_Markup),
                     Importance => GPS.Kernel.Messages.Unspecified,
                     Flags      => GPS.Kernel.Messages.Side_And_Locations,
                     Allow_Auto_Jump_To_First => False);
               end if;

               Vectors.Previous (C);

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
                        newText =>
                          VSS.Strings.Conversions.To_Virtual_String
                            (Rev_Text));
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
      Trace (Me, "Limit.first: " & Line_Number'Image
             (Command.Limit_Span.first.line)
             & ASCII.LF
             & "Limit.last: " & Line_Number'Image
               (Command.Limit_Span.last.line));
      --  Clear the previous changes
      Command.Reverse_Edit.Clear;
      Command.Reverse_Document_Changes.Clear;

      declare
         use LSP.Messages.TextDocumentEdit_Maps;

         Vector : Vectors.Vector;
         C      : LSP.Messages.TextDocumentEdit_Maps.Cursor :=
           Command.Workspace_Edit.changes.First;
      begin
         while Has_Element (C) loop
            for Change of Element (C) loop
               Insert_Change (Vector, Change.span, Change.newText);
            end loop;

            --  Guarantee the order of textEdit
            Ascending.Sort (Vector);

            Process_File
              (GPS.LSP_Client.Utilities.To_Virtual_File (Key (C)),
               Vector);

            Vector.Clear;
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
               Item   : constant LSP.Messages.Document_Change := Element (C);
               Vector : Vectors.Vector;
            begin
               case Item.Kind is
                  when LSP.Messages.Text_Document_Edit =>
                     for Change of Item.Text_Document_Edit.edits loop
                        Insert_Change (Vector, Change.span, Change.newText);
                     end loop;

                     --  Guarantee the order of textEdit
                     Ascending.Sort (Vector);

                     Process_File
                       (GPS.LSP_Client.Utilities.To_Virtual_File
                          (Item.Text_Document_Edit.textDocument.uri),
                        Vector);

                  when LSP.Messages.Create_File =>
                     Error := True;
                     exit;

                  when LSP.Messages.Delete_File =>
                     Error := True;
                     exit;

                  when LSP.Messages.Rename_File =>

                     --  Return immediately if the user did not allow file
                     --  renaming.
                     if not Command.Allow_File_Renaming then
                        exit;
                     end if;

                     declare
                        Success     : Boolean;
                        Prj_Changed : Boolean;
                        File        : constant Virtual_File :=
                          GPS.LSP_Client.Utilities.To_Virtual_File
                            (Item.Rename_File.oldUri);
                        New_File    : constant Virtual_File :=
                          GPS.LSP_Client.Utilities.To_Virtual_File
                            (Item.Rename_File.newUri);
                        Reverse_Item : LSP.Messages.Document_Change :=
                          Item;
                     begin
                        --  Rename the file
                        VFS_Module.Rename_File
                          (Kernel                  => Command.Kernel,
                           File                    => File,
                           New_File                => New_File,
                           Success                 => Success,
                           Prj_Changed             => Prj_Changed,
                           Display_Confirm_Dialogs => False);

                        Need_Project_Refresh :=
                          Need_Project_Refresh or else Prj_Changed;

                        --  Now send a 'didClose' notification for the old file
                        --  and a 'didOpen' one for the new file
                        declare
                           Server : constant Language_Server_Access :=
                             Get_Language_Server
                               (Command.Kernel.Get_Language_Handler.
                                  Get_Language_From_File (File));
                        begin
                           if Server /= null then
                              --  See LSP documentation:
                              --  Document renames should be signaled to a
                              --  server sending a document close notification
                              --  with the document's old name followed by an
                              --  open notification using the document's new
                              --  name. Major reason is that besides the name
                              --  other attributes can change as well like the
                              --  language that is associated with the
                              --  document. In addition the new document could
                              --  not be of interest for the server anymore.
                              Server.Get_Client.Send_Text_Document_Did_Close
                                (File);
                              Server.Get_Client.Send_Text_Document_Did_Open
                                (New_File);
                              Server.Get_Client.Send_Did_Rename_File
                                (File, New_File);
                           end if;
                        end;

                        --  Append a reverse renameFile in case the user wants
                        --  to undo the last workspaceEdit command.
                        Reverse_Item.Rename_File.newUri :=
                          Item.Rename_File.oldUri;
                        Reverse_Item.Rename_File.oldUri :=
                          Item.Rename_File.newUri;
                        Command.Reverse_Document_Changes.Append (Reverse_Item);
                     end;
               end case;
            end;

            Next (C);
         end loop;
      end;

      --  Reload the project if needed
      if Need_Project_Refresh then
         GPS.Kernel.Project.Recompute_View (Command.Kernel);
      end if;

      --  The calls to Process_File above might have generated entries
      --  in the Errors list. Process this now.

      if Error or not Errors.Is_Empty then
         Error := True;

         if not Refactoring.UI.Dialog
           (Command.Kernel,
            Title         =>
              VSS.Strings.Conversions.To_UTF_8_String (Command.Title)
                & " raises errors",
            Msg           =>
              "Some references could not be processed because one or more" &
              " files were already modified or non writable",
            Files         => Errors,
            Execute_Label => Gtkada.Stock_Labels.Stock_Ok,
            Cancel_Label  => Gtkada.Stock_Labels.Stock_Undo)
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
      Buffer_Factory       : constant Editor_Buffer_Factory_Access :=
        Get_Buffer_Factory (Command.Kernel);
      Need_Project_Refresh : Boolean := False;

      procedure Process_File
        (File   : Virtual_File;
         Vector : Vectors.Vector);
      --  Apply changes in the Vector to the file

      procedure Internal_Process_File
        (File   : Virtual_File;
         Editor : in out GPS.Editors.Editor_Buffer'Class;
         Vector : Vectors.Vector);

      ------------------
      -- Process_File --
      ------------------

      procedure Process_File
        (File   : Virtual_File;
         Vector : Vectors.Vector) is
      begin
         if Command.Auto_Save then
            declare
               Holder : constant GPS.Editors.
                 Controlled_Editor_Buffer_Holder :=
                   Buffer_Factory.Get_Holder (File);
               Editor : GPS.Editors.Editor_Buffer'Class := Holder.Editor;
            begin
               Internal_Process_File (File, Editor, Vector);
            end;
         else
            declare
               Editor : GPS.Editors.Editor_Buffer'Class :=
                 Buffer_Factory.Get
                   (File,
                    Open_View   => True,
                    Open_Buffer => True);
            begin
               Internal_Process_File (File, Editor, Vector);
            end;
         end if;
      end Process_File;

      ---------------------------
      -- Internal_Process_File --
      ---------------------------

      procedure Internal_Process_File
        (File   : Virtual_File;
         Editor : in out GPS.Editors.Editor_Buffer'Class;
         Vector : Vectors.Vector)
      is
         Controller : constant Cursor_Movement_Controller'Class :=
           GPS_Editor_Buffer'Class (Editor).Freeze_Cursor;
         pragma Unreferenced (Controller);
      begin
         if Editor.Can_Undo then
            --  At this point we are focused on an Editor for File with an
            --  non empty Undo_Redo queue: use it to undo the last group.
            Editor.Undo;
         else
            declare
               G : constant Group_Block := Editor.New_Undo_Group;
               C : Vectors.Cursor;
            begin
               C := Vector.First;
               while Vectors.Has_Element (C) loop
                  declare
                     --  S is a fake span which is already converted to an
                     --  Editor Location
                     S     : constant LSP.Messages.Span :=
                       Vectors.Element (C).Span;
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
                          Text        =>
                            VSS.Strings.Conversions.To_UTF_8_String
                              (Vectors.Element (C).Text));
                  begin
                     Vectors.Next (C);
                  end;
               end loop;
            end;
         end if;

         if Command.Auto_Save then
            Editor.Save (Interactive => False);
         end if;
      end Internal_Process_File;

   begin

      --  Undo all the documentChanges firts.
      --  We only support file renamings currently.
      declare
         use LSP.Messages.Document_Change_Vectors.Element_Vectors;

         C : LSP.Messages.Document_Change_Vectors.Element_Vectors.Cursor :=
           Command.Reverse_Document_Changes.First;
      begin
         while Has_Element (C) loop
            declare
               Item : constant LSP.Messages.Document_Change := Element (C);
            begin
               case Item.Kind is
                  when LSP.Messages.Rename_File =>
                     declare
                        Success     : Boolean;
                        Prj_Changed : Boolean;
                        File        : constant Virtual_File :=
                          GPS.LSP_Client.Utilities.To_Virtual_File
                            (Item.Rename_File.oldUri);
                        New_File    : constant Virtual_File :=
                          GPS.LSP_Client.Utilities.To_Virtual_File
                            (Item.Rename_File.newUri);
                     begin
                        VFS_Module.Rename_File
                          (Kernel                  => Command.Kernel,
                           File                    => File,
                           New_File                => New_File,
                           Success                 => Success,
                           Prj_Changed             => Prj_Changed,
                           Display_Confirm_Dialogs => False);

                        Need_Project_Refresh :=
                          Need_Project_Refresh or else Prj_Changed;

                        --  Now send a 'didClose' notification for the old file
                        --  and a 'didOpen' one for the new file
                        declare
                           Server : constant Language_Server_Access :=
                             Get_Language_Server
                               (Command.Kernel.Get_Language_Handler.
                                  Get_Language_From_File (File));
                        begin
                           if Server /= null then
                              --  See LSP documentation:
                              --  Document renames should be signaled to a
                              --  server sending a document close notification
                              --  with the document's old name followed by an
                              --  open notification using the document's new
                              --  name. Major reason is that besides the name
                              --  other attributes can change as well like the
                              --  language that is associated with the
                              --  document. In addition the new document could
                              --  not be of interest for the server anymore.
                              Server.Get_Client.Send_Text_Document_Did_Close
                                (File);
                              Server.Get_Client.Send_Text_Document_Did_Open
                                (New_File);
                              Server.Get_Client.Send_Did_Rename_File
                                (File, New_File);
                           end if;
                        end;
                     end;

                  when others =>
                     --  Not supported yet
                     null;
               end case;
            end;

            Next (C);
         end loop;
      end;

      if Need_Project_Refresh then
         --  Some project files were affected: update the views
         GPS.Kernel.Project.Recompute_View (Command.Kernel);
      end if;

      --  Remove the messages related to the worskspaceEdit being undone
      Get_Messages_Container (Command.Kernel).Remove_Category
        (Command.Title, GPS.Kernel.Messages.Side_And_Locations);

      --  Loop through the generated edit to undo WorkspaceEdit
      declare
         use LSP.Messages.TextDocumentEdit_Maps;

         Vector : Vectors.Vector;
         C      : LSP.Messages.TextDocumentEdit_Maps.Cursor :=
           Command.Reverse_Edit.First;
      begin
         while Has_Element (C) loop
            for Change of Element (C) loop
               Insert_Change (Vector, Change.span, Change.newText);
            end loop;

            Process_File
              (GPS.LSP_Client.Utilities.To_Virtual_File (Key (C)),
               Vector);

            Vector.Clear;
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
      Title                    : VSS.Strings.Virtual_String;
      Make_Writable            : Boolean;
      Auto_Save                : Boolean;
      Allow_File_Renaming      : Boolean;
      Locations_Message_Markup : VSS.Strings.Virtual_String;
      Error                    : out Boolean;
      Limit_Span               : LSP.Messages.Span := LSP.Messages.Empty_Span;
      Compute_Minimal_Edits    : Boolean := False;
      Avoid_Cursor_Move        : Boolean := True)
   is
      Command : Command_Access := new Edit_Workspace_Command'
        (Root_Command with
         Kernel                   => Kernel,
         Limit_Span               => Limit_Span,
         Workspace_Edit           => Workspace_Edit,
         Reverse_Edit             => <>,
         Title                    => Title,
         Reverse_Document_Changes => <>,
         Make_Writable            => Make_Writable,
         Auto_Save                => Auto_Save,
         Allow_File_Renaming      => Allow_File_Renaming,
         Locations_Message_Markup => Locations_Message_Markup,
         Compute_Minimal_Edits    => Compute_Minimal_Edits,
         Avoid_Cursor_Move        => Avoid_Cursor_Move);
   begin
      Src_Editor_Module.Set_Global_Command (Command);
      --  Give up the local ownership of this command, it will be
      --  automatically freed when the Global_command is invalidated.
      Unref (Command);
      Error := Src_Editor_Module.Execute_Global_Command /= Success;
   end Edit;

end GPS.LSP_Client.Edit_Workspace;
